(* sv_gui.ml — lablgtk3 shell where Lua scripts are first-class menu items.

   Each *.lua under ./scripts and $XDG_CONFIG_HOME/sv_suite/scripts is
   dofile'd into a single persistent interpreter at startup. While running it
   can call gui.add_menu / gui.add_item / gui.set_text / … to wire itself
   into the menubar; the registered handler name is invoked as `<name>()` on
   click. Re-running "View → Reload scripts" rips the script-added items and
   re-loads, so editing a script is fast-feedback.

   Hooks into Sv_lua are plain function refs (string ↔ unit), so the rest of
   the codebase keeps no lablgtk3 dependency.                               *)

(* ---------- shared interpreter state ---------- *)

let state = Sv_lua.I.mk ()

(* Last-loaded BIR program — fed by Decompile → Parse * and consumed by
   Decompile → Optimise.  None until the user has parsed something. *)
let current_prog : (string * Behavioral_ir.bprogram) option ref = ref None

let run_lua s =
  try ignore (Sv_lua.I.dostring state s); None
  with e -> Some (Printexc.to_string e)

(* ---------- GTK widget refs (filled in main) ---------- *)

let window_ref      : GWindow.window option            ref = ref None
let buffer_ref      : GText.buffer option              ref = ref None
let menubar_ref     : GMenu.menu_shell option          ref = ref None
let accel_group_ref : Gtk.accel_group option           ref = ref None
let status_ctx_ref  : GMisc.statusbar_context option   ref = ref None

(* Top-level menus by display name (stable across reloads). *)
let menus : (string, GMenu.menu) Hashtbl.t = Hashtbl.create 16

(* Items added by Lua scripts — destroyed on reload. *)
let dynamic_items : GObj.widget list ref = ref []

let need_window () = match !window_ref with
  | Some w -> w
  | None -> failwith "GUI not initialised"

(* ---------- dialog helpers ---------- *)

let info_dialog msg =
  let d = GWindow.message_dialog
    ~message:msg ~message_type:`INFO
    ~buttons:GWindow.Buttons.ok
    ~parent:(need_window ()) ~modal:true () in
  ignore (d#run ()); d#destroy ()

let error_dialog msg =
  let d = GWindow.message_dialog
    ~message:msg ~message_type:`ERROR
    ~buttons:GWindow.Buttons.ok
    ~parent:(need_window ()) ~modal:true () in
  ignore (d#run ()); d#destroy ()

(* Sticky directory across all file choosers — opening any picker
   returns to the directory of the last successfully picked file.
   Persisted across sessions in
   $XDG_CONFIG_HOME/sv_suite/last_dir.txt; loaded at startup
   and rewritten on every successful pick.                         *)
let last_chooser_dir : string ref =
  ref (try Sys.getenv "HOME" with Not_found -> "")

let last_dir_file () =
  let xdg =
    try Sys.getenv "XDG_CONFIG_HOME"
    with Not_found ->
      try (Sys.getenv "HOME") ^ "/.config"
      with Not_found -> "/tmp"
  in
  Filename.concat xdg "sv_suite/last_dir.txt"

let load_last_chooser_dir () =
  let p = last_dir_file () in
  if Sys.file_exists p then
    try
      let ic = open_in p in
      (try
         let l = String.trim (input_line ic) in
         if l <> "" && Sys.file_exists l && Sys.is_directory l
         then last_chooser_dir := l
       with End_of_file -> ());
      close_in ic
    with _ -> ()

let save_last_chooser_dir () =
  let p = last_dir_file () in
  let dir = Filename.dirname p in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  try
    let oc = open_out p in
    output_string oc (!last_chooser_dir ^ "\n");
    close_out oc
  with _ -> ()

let chooser_dialog action title =
  let d = GWindow.file_chooser_dialog
    ~action ~title ~parent:(need_window ()) ~modal:true () in
  d#add_button "Cancel" `CANCEL;
  d#add_button (match action with `OPEN -> "Open" | `SAVE -> "Save" | _ -> "OK")
    `OK;
  (* Double-click (or Enter on a selected file) → same as clicking Open. *)
  ignore (d#connect#file_activated ~callback:(fun () -> d#response `OK));
  if !last_chooser_dir <> ""
     && Sys.file_exists !last_chooser_dir
     && Sys.is_directory !last_chooser_dir
  then ignore (d#set_current_folder !last_chooser_dir);
  let result = match d#run () with
    | `OK -> (match d#filename with Some f -> f | None -> "")
    | _   -> ""
  in
  d#destroy ();
  if result <> "" then begin
    last_chooser_dir := Filename.dirname result;
    save_last_chooser_dir ()
  end;
  result

let open_file_dialog () = chooser_dialog `OPEN "Open file"
let save_file_dialog () = chooser_dialog `SAVE "Save file"

(* ---------- text buffer accessors ---------- *)

let set_text s =
  match !buffer_ref with
  | Some b -> b#set_text s
  | None   -> ()

let get_text () =
  match !buffer_ref with
  | Some b -> b#get_text ~start:b#start_iter ~stop:b#end_iter ()
  | None   -> ""

let append_text s =
  match !buffer_ref with
  | Some b -> b#insert ~iter:b#end_iter s
  | None   -> ()

(* ---------- status bar ---------- *)

let set_status msg =
  match !status_ctx_ref with
  | Some c -> ignore (c#push msg)
  | None   -> ()

(* ---------- menu construction ---------- *)

let need_menubar () = match !menubar_ref with
  | Some b -> b
  | None   -> failwith "menubar not initialised"

let need_accel () = match !accel_group_ref with
  | Some g -> g
  | None   -> failwith "accel group not initialised"

let make_or_get_menu name =
  match Hashtbl.find_opt menus name with
  | Some m -> m
  | None ->
      let factory = new GMenu.factory ~accel_group:(need_accel ())
        (need_menubar ()) in
      let m = factory#add_submenu name in
      Hashtbl.add menus name m; m

let add_lua_menu name = ignore (make_or_get_menu name)

let add_lua_item menu_name label handler =
  let m = make_or_get_menu menu_name in
  let f = new GMenu.factory ~accel_group:(need_accel ()) m in
  let item = f#add_item label
    ~callback:(fun () ->
      match run_lua (handler ^ "()") with
      | None     -> ()
      | Some err ->
          error_dialog (Printf.sprintf "Lua error in %s:\n%s" handler err))
  in
  dynamic_items := (item :> GObj.widget) :: !dynamic_items

(* ---------- dependency discovery for "Parse Verible" ----------
   When the user opens a single .sv file we pull in the files that
   define modules it instantiates so the BIR view shows the full
   design.  Strategy: scan sibling directories + persisted search
   paths for *.sv/*.v, build a module-name → file-path map (via
   Sv_preproc.find_module_names so macros / `\`include`s are honoured),
   parse the seed file, walk the BIR for unresolved
   instance.module_name references, add the matching files, and
   re-parse.  When a name still can't be found we ask the user to
   locate it; the chosen file's directory becomes a permanent search
   path persisted under $XDG_CONFIG_HOME/sv_suite/.            *)

let walk_sv_dir_into ?(max_depth = 4) tbl root =
  let rec walk depth dir =
    if depth > max_depth then ()
    else if Sys.file_exists dir && Sys.is_directory dir then begin
      let entries =
        try Array.to_list (Sys.readdir dir) with _ -> [] in
      List.iter (fun e ->
        if e = "" || e.[0] = '.' then ()    (* skip hidden / .git *)
        else begin
          let p = Filename.concat dir e in
          if (try Sys.is_directory p with _ -> false)
          then walk (depth + 1) p
          else if Filename.check_suffix p ".sv"
               || Filename.check_suffix p ".v" then begin
            try
              let text = Sv_preproc.preprocess_file p in
              let names = Sv_preproc.find_module_names text in
              List.iter (fun n ->
                if not (Hashtbl.mem tbl n) then Hashtbl.add tbl n p
              ) names
            with _ -> ()
          end
        end
      ) entries
    end
  in
  walk 0 root

let walk_sv_dir ?(max_depth = 4) root =
  let tbl = Hashtbl.create 256 in
  walk_sv_dir_into ~max_depth tbl root;
  tbl

(* Persisted search paths — populated on demand by the user when a
   dependency can't be located, kept across sessions in a plain text
   file (one absolute path per line).                              *)
let search_paths : string list ref = ref []

let search_paths_file () =
  let xdg =
    try Sys.getenv "XDG_CONFIG_HOME"
    with Not_found ->
      try (Sys.getenv "HOME") ^ "/.config"
      with Not_found -> "/tmp"
  in
  Filename.concat xdg "sv_suite/search_paths.txt"

let load_search_paths () =
  let p = search_paths_file () in
  if Sys.file_exists p then begin
    try
      let ic = open_in p in
      let lines = ref [] in
      (try while true do
         let l = input_line ic in
         let l = String.trim l in
         if l <> "" && l.[0] <> '#' then lines := l :: !lines
       done with End_of_file -> ());
      close_in ic;
      search_paths := List.rev !lines
    with _ -> ()
  end

let save_search_paths () =
  let p = search_paths_file () in
  let dir = Filename.dirname p in
  if not (Sys.file_exists dir) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)));
  try
    let oc = open_out p in
    output_string oc
      "# sv_suite search paths (one absolute dir per line)\n";
    List.iter (fun d -> output_string oc (d ^ "\n")) !search_paths;
    close_out oc
  with _ -> ()

let add_search_path d =
  let d = if Filename.is_relative d
          then Filename.concat (Sys.getcwd ()) d else d in
  if not (List.mem d !search_paths) then begin
    search_paths := !search_paths @ [d];
    save_search_paths ()
  end

(* Cache the merged seed-dir + search-path index keyed by its full
   composition so changing search_paths invalidates the cache.       *)
let sv_dir_index : (string * (string, string) Hashtbl.t) ref =
  ref ("", Hashtbl.create 0)

let module_name_index_for path =
  let seed_root = Filename.dirname path in
  let key = String.concat "|" (seed_root :: !search_paths) in
  let cached_key, cached_tbl = !sv_dir_index in
  if cached_key = key then cached_tbl
  else begin
    let tbl = Hashtbl.create 256 in
    walk_sv_dir_into tbl seed_root;
    List.iter (fun d ->
      if d <> seed_root then walk_sv_dir_into tbl d
    ) !search_paths;
    sv_dir_index := (key, tbl);
    tbl
  end

(* The Verible converter's extract_instances occasionally surfaces
   non-instantiation parse-tree nodes (variable decls of typedef'd
   types, $clog2-bounded array dimensions, …) as binstances.  We
   silently drop those at closure time so the user isn't prompted
   to "locate" a typedef or system function.  Real module names
   don't start with $ and don't end with _t in any project we've
   seen — if a user really has a module named `foo_t`, they can
   still rename or reach it by passing the file to convert_files
   directly.                                                       *)
let looks_like_module_name n =
  let l = String.length n in
  l > 0
  && n.[0] <> '$'
  && not (l >= 2 && String.sub n (l - 2) 2 = "_t")

(* Closure walker — `on_missing` is called once per never-seen
   unresolved module name.  Return Some path to register that file
   (its dir is also indexed for sibling deps); return None to skip
   this name (remembered, so we don't re-prompt in the same call). *)
let close_verible_dependencies ~seed ~on_missing name_map =
  let files = ref [seed] in
  let prog  = ref (Verible_to_behavioral.convert_files_all !files) in
  let max_iters = 8 in
  let skipped : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let iter = ref 0 in
  let changed = ref true in
  while !changed && !iter < max_iters do
    incr iter;
    changed := false;
    let defined = List.fold_left (fun acc (m : Behavioral_ir.bmodule) ->
      m.name :: acc) [] (!prog).modules in
    let referenced =
      List.concat_map (fun (m : Behavioral_ir.bmodule) ->
        List.map (fun (i : Behavioral_ir.binstance) -> i.module_name)
          m.instances) (!prog).modules in
    let unresolved = List.filter (fun n ->
      not (List.mem n defined)
      && not (Hashtbl.mem skipped n)
      && looks_like_module_name n)
      referenced in
    let any_added = ref false in
    List.iter (fun n ->
      let pick =
        match Hashtbl.find_opt name_map n with
        | Some p -> Some p
        | None ->
            match on_missing n with
            | Some p ->
                Hashtbl.replace name_map n p;
                walk_sv_dir_into name_map (Filename.dirname p);
                Some p
            | None ->
                Hashtbl.add skipped n ();
                None
      in
      match pick with
      | Some p when not (List.mem p !files) ->
          files := p :: !files;
          changed := true;
          any_added := true
      | _ -> ()
    ) unresolved;
    if !any_added then
      prog := Verible_to_behavioral.convert_files_all !files
  done;
  (List.rev !files, !prog)

(* Modal dialog: ask the user to locate a missing module's source.
   Shared between Decompile→Parse Verible and Topology→Run ORFS so both
   paths apply the same dependency closure with the same UX.            *)
let prompt_locate_module module_name =
  let parent = need_window () in
  let d = GWindow.dialog
    ~title:("Module not found: " ^ module_name)
    ~parent ~modal:true ~width:480 () in
  let lbl = GMisc.label
    ~text:(Printf.sprintf
      "Module '%s' is instantiated but no source file was found in \
       the current search paths.\n\nLocate the .sv/.v file — its \
       directory will be added to the persisted search paths so future \
       runs find sibling modules automatically."
      module_name)
    ~xalign:0.0 ~justify:`LEFT
    ~packing:(d#vbox#pack ~padding:8) () in
  lbl#set_line_wrap true;
  d#add_button "Locate file…" `OK;
  d#add_button "Skip"         `CANCEL;
  d#vbox#misc#show_all ();
  let action = d#run () in
  d#destroy ();
  match action with
  | `OK ->
      let f = open_file_dialog () in
      if f = "" then None
      else begin
        add_search_path (Filename.dirname f);
        Some f
      end
  | _ -> None

(* ---------- hardwired actions: parse / optim / Z3 miter ---------- *)

let with_errors label f =
  try f ()
  with e -> error_dialog (Printf.sprintf "%s: %s" label (Printexc.to_string e))

let dump_prog ?(banner = "") p =
  let s = Behavioral_ir.string_of_bprogram p in
  set_text (banner ^ s)

(* Verible and slang both elaborate top-down from a named root module —
   passing the literal "auto" gets you zero modules because no source
   file declares a `module auto`.  For SV we route the file through
   sv_preproc (handles macros / `\`include`) and pull out the first
   declared module name.  For VHDL we do a small inline scan for the
   first `entity <name>`.  Falls back to the file's basename.         *)
let entity_re =
  Str.regexp
    "\\(^\\|[^A-Za-z0-9_]\\)entity[ \t\n\r]+\\([A-Za-z_][A-Za-z0-9_]*\\)"

let slurp path =
  let ic = open_in path in
  let n  = in_channel_length ic in
  let s  = really_input_string ic n in
  close_in ic; s

let derive_top ~frontend path =
  let fallback () = Filename.chop_extension (Filename.basename path) in
  match frontend with
  | "verilator" -> fallback ()                   (* JSON, not source *)
  | "vhdl" ->
      (try
         let s = slurp path in
         let _ = Str.search_forward entity_re s 0 in
         Str.matched_group 2 s
       with _ -> fallback ())
  | _ ->
      (try
         let text = Sv_preproc.preprocess_file path in
         match Sv_preproc.find_module_names text with
         | n :: _ -> n
         | []     -> fallback ()
       with _ -> fallback ())

let do_parse fe () =
  let path = open_file_dialog () in
  if path <> "" then with_errors ("parse " ^ fe) (fun () ->
    set_status (Printf.sprintf "%s: parsing %s …" fe path);
    (* "Parse" reads every module in the file at default params; pick a
       top only later (Verify / hardcaml).  For frontends whose driver
       elaborates from a hard-coded top we still seed it from the source
       so the driver is happy, but no specialisation walk happens.     *)
    let p, file_count, extra_banner =
      match fe with
      | "verible" ->
          let name_map = module_name_index_for path in
          let files, prog =
            close_verible_dependencies
              ~seed:path ~on_missing:prompt_locate_module name_map in
          let extra =
            if List.length files > 1 then
              Printf.sprintf "// dependencies pulled in (%d files):\n%s\n"
                (List.length files)
                (String.concat ""
                   (List.map (fun f -> "//   " ^ f ^ "\n") files))
            else ""
          in
          (prog, List.length files, extra)
      | "vhdl" | "verilator" ->
          (Sv_lua.load_frontend ~frontend:fe ~top:"" ~files:[path], 1, "")
      | _ ->
          (* slang / yosys driver requires --top; seed it from the source
             so the driver elaborates SOMETHING, then dump every module
             the driver emitted (slang's JSON contains the full design;
             yosys hierarchy is rooted but other modules survive when
             present in the file). *)
          let top = derive_top ~frontend:fe path in
          (Sv_lua.load_frontend ~frontend:fe ~top ~files:[path], 1, "")
    in
    current_prog := Some (path, p);
    let names = List.map (fun (m : Behavioral_ir.bmodule) -> m.name)
                  p.modules in
    dump_prog ~banner:(Printf.sprintf
      "// %s — %d module(s) from %s%s\n// modules: %s\n%s\n"
      fe (List.length p.modules) path
      (if file_count > 1 then Printf.sprintf
        " (+%d dependency files)" (file_count - 1)
       else "")
      (String.concat ", " names)
      extra_banner) p;
    set_status (Printf.sprintf "%s: %d module(s) from %d file(s) loaded" fe
                  (List.length p.modules) file_count))

let do_optim () =
  match !current_prog with
  | None ->
      info_dialog
        "No BIR loaded yet — use Decompile → Parse <frontend> first."
  | Some (path, p) ->
      with_errors "optimise" (fun () ->
        set_status "Optimising …";
        let p' = Behavioral_optimize.optimize_quick p in
        current_prog := Some (path, p');
        dump_prog ~banner:(Printf.sprintf
          "// optimised — %d module(s) from %s\n\n"
          (List.length p'.modules) path) p';
        set_status "Optimisation done")

let pick_first_module label (p : Behavioral_ir.bprogram) =
  match p.modules with
  | m :: _ -> m
  | []     -> failwith (label ^ ": no modules in program")

let do_miter fe_a fe_b () =
  with_errors (Printf.sprintf "miter %s vs %s" fe_a fe_b) (fun () ->
    set_status (Printf.sprintf "Pick file A (%s)…" fe_a);
    let fa = open_file_dialog () in
    if fa = "" then () else begin
      set_status (Printf.sprintf "Pick file B (%s)…" fe_b);
      let fb = open_file_dialog () in
      if fb = "" then () else begin
        let top_a = derive_top ~frontend:fe_a fa in
        let top_b = derive_top ~frontend:fe_b fb in
        set_status (Printf.sprintf "Parsing %s with %s (top=%s) …"
                      fa fe_a top_a);
        let pa = Sv_lua.load_frontend ~frontend:fe_a ~top:top_a ~files:[fa] in
        set_status (Printf.sprintf "Parsing %s with %s (top=%s) …"
                      fb fe_b top_b);
        let pb = Sv_lua.load_frontend ~frontend:fe_b ~top:top_b ~files:[fb] in
        let ma = pick_first_module fe_a pa in
        let mb = pick_first_module fe_b pb in
        let ma' = Sv_lua.prep_for_z3 ma pa in
        let mb' = Sv_lua.prep_for_z3 mb pb in
        set_status "Running Z3 miter …";
        let verdict =
          if Z3_miter.check_miter_equivalence ma' mb'
          then "EQUIVALENT" else "DIFFER"
        in
        set_text (Printf.sprintf
          "Z3 miter\n\
           ────────\n\
           A : %s   (%s, module %s)\n\
           B : %s   (%s, module %s)\n\n\
           Verdict : %s\n"
          fa fe_a ma.name fb fe_b mb.name verdict);
        set_status ("Z3 miter: " ^ verdict)
      end
    end)

(* ---------- Hardcaml simulation + waveform window (Stage 1) ----------
   Loads the BIR via the existing file-open dialog, lowers the top module
   through Behavioral_to_hardcaml, runs Cyclesim for 128 cycles with
   default stimulus (clock from Cyclesim, reset asserted for the first
   4 cycles auto-detected from port names, all other inputs zero), and
   pops a new GtkWindow with a Cairo waveform canvas.  Gated by
   SV_DECOMP_GUI_SIM=1 so the default Verify menu doesn't surface a
   half-polished feature.                                              *)

let open_waveform_window (sr : Gui_sim.sim_result) =
  let w, h = Gui_waveform.extents sr in
  let win = GWindow.window
    ~title:(Printf.sprintf "Waveforms: %s (%d cycles)"
              sr.sr_module_name sr.sr_cycles)
    ~width:(min 1400 (int_of_float w + 30))
    ~height:(min 900 (int_of_float h + 30))
    () in
  let sw = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:win#add () in
  let da = GMisc.drawing_area
    ~packing:sw#add_with_viewport () in
  da#misc#set_size_request
    ~width:(int_of_float w) ~height:(int_of_float h) ();
  ignore (da#misc#connect#draw ~callback:(fun cr ->
    Gui_waveform.render cr sr;
    true));
  win#show ()

let do_simulate () =
  with_errors "simulate" (fun () ->
    match !current_prog with
    | None ->
        info_dialog "Load a SystemVerilog file first (File → Open … or \
                     Decompile → Parse with verible …)."
    | Some (_label, p) ->
        (* Inline picker — pick_default_top is defined further down in
           this file; we just want a synthesisable module with at least
           one process or a non-empty IO surface. *)
        let synth_candidate (m : Behavioral_ir.bmodule) =
          m.processes <> [] || m.signals <> [] in
        let m = match List.filter synth_candidate p.modules with
          | [] -> failwith "no synthesisable top module in loaded BIR"
          | [m] -> m
          | ms ->
              (* Pick the module whose name is not instantiated by any
                 other module in this program — that's the topmost. *)
              let instantiated =
                List.concat_map (fun (m : Behavioral_ir.bmodule) ->
                  List.map (fun (i : Behavioral_ir.binstance) -> i.module_name)
                    m.instances
                ) p.modules in
              (match List.filter (fun (m : Behavioral_ir.bmodule) ->
                       not (List.mem m.name instantiated)) ms with
               | [t] -> t
               | t :: _ -> t   (* tie-break: first one *)
               | [] -> List.hd ms) in
        set_status (Printf.sprintf "Simulating %s @ 128 cycles…" m.name);
        let sr = Gui_sim.run ~n_cycles:128 m in
        set_status (Printf.sprintf "Simulation OK: %s, %d cycles, %d ports"
                      m.name sr.sr_cycles
                      (List.length sr.sr_inputs + List.length sr.sr_outputs));
        open_waveform_window sr)

(* Path of the last synth output the GUI produced, used by "Show BSDL". *)
let last_synth_out_path : string option ref = ref None

(* ATPG runner — reuses Synth_pipeline (so the DFT env toggles apply)
   and Fault_sim, then renders per-module coverage into the text view.
   Source files come from the currently-loaded program plus its
   Verible dependency closure.                                          *)
let do_atpg () =
  with_errors "atpg" (fun () ->
    match !current_prog with
    | None ->
        info_dialog "Load a SystemVerilog file first (Decompile → Parse Verible…)."
    | Some (path, p) ->
        let pick_top () =
          let instantiated =
            List.concat_map (fun (m : Behavioral_ir.bmodule) ->
              List.map (fun (i : Behavioral_ir.binstance) -> i.module_name)
                m.instances) p.modules in
          match List.filter (fun (m : Behavioral_ir.bmodule) ->
                  not (List.mem m.name instantiated)) p.modules with
          | [t] -> t.name
          | t :: _ -> t.name
          | [] -> (List.hd p.modules).name in
        let top = pick_top () in
        let name_map = module_name_index_for path in
        let files, _ =
          close_verible_dependencies
            ~seed:path ~on_missing:prompt_locate_module name_map in
        Unix.putenv "SV_DECOMP_SCAN" "1";
        let out_path =
          Filename.concat (Filename.get_temp_dir_name ())
            (Printf.sprintf "sv_gui_atpg_%s.v" top) in
        set_status (Printf.sprintf "ATPG synth: top=%s, %d file(s)…"
                      top (List.length files));
        let netlists, _ =
          Synth_pipeline.run ~emit_verilog:true ~top ~out_path ~files () in
        last_synth_out_path := Some out_path;
        let buf = Buffer.create 4096 in
        Buffer.add_string buf
          (Printf.sprintf "ATPG coverage — top=%s, %d module(s)\n\n"
             top (List.length netlists));
        List.iter (fun (mn : Hier_synth.module_netlist) ->
          Buffer.add_string buf
            (Printf.sprintf "=== %s: %d cells ===\n"
               mn.mn_name (List.length mn.mn_netlist.insts));
          if mn.mn_netlist.insts = [] then
            Buffer.add_string buf "  (empty netlist — skipped)\n\n"
          else begin
            let r = Fault_sim.run_atpg ~module_name:mn.mn_name
                      mn.mn_netlist in
            Buffer.add_string buf (Fault_sim.render_report r);
            Buffer.add_string buf "\n"
          end
        ) netlists;
        set_text (Buffer.contents buf);
        set_status "ATPG complete")

let do_show_bsdl () =
  with_errors "show-bsdl" (fun () ->
    let candidate =
      match !last_synth_out_path with
      | Some p when Sys.file_exists (p ^ ".bsd") -> Some (p ^ ".bsd")
      | _ -> None in
    let path = match candidate with
      | Some p -> p
      | None -> open_file_dialog ()
    in
    if path = "" then ()
    else begin
      let ic = open_in path in
      let buf = Buffer.create 4096 in
      (try while true do Buffer.add_channel buf ic 4096 done
       with End_of_file -> ());
      close_in ic;
      set_text (Buffer.contents buf);
      set_status (Printf.sprintf "Loaded %s" path)
    end)

(* ---------- ORFS handholding (Phase 1: launch + log streaming) ----------
   The GUI doesn't reimplement OpenROAD; it just writes a config.mk + sdc
   with sensible defaults and shells out to ORFS's make.  Output streams
   into the centre text view via a Glib IO watch so the GUI stays
   responsive while ORFS runs (5–30 min for nangate45 designs).        *)

type mem_backend = Mem_fakeram | Mem_openram | Mem_bitblast

type orfs_cfg = {
  o_top        : string;
  o_files      : string list;   (* full closed-over fileset for VERILOG_FILES *)
  o_platform   : string;
  o_freq_ghz   : float;
  o_util       : int;
  o_workdir    : string;
  o_mem_bits   : int;           (* SYNTH_MEMORY_MAX_BITS in config.mk    *)
  o_use_decomp : bool;          (* USE_DECOMP_SYNTH=1 to make            *)
  o_mem_back   : mem_backend;   (* memory lowering strategy              *)
}

(* Total bits of a btype, recursing into arrays/structs.  Used to size
   yosys's SYNTH_MEMORY_MAX_BITS so a `reg [W-1:0] mem [0:D-1]` doesn't
   trip the platform's default 4096-bit ceiling.                       *)
let rec btype_bits = function
  | Behavioral_ir.BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * btype_bits element
  | BStruct { fields } ->
      List.fold_left (fun acc (_, t) -> acc + btype_bits t) 0 fields

let max_array_bits (p : Behavioral_ir.bprogram) =
  List.fold_left (fun acc (m : Behavioral_ir.bmodule) ->
    List.fold_left (fun acc (s : Behavioral_ir.bsignal) ->
      match s.stype with
      | BArray _ -> max acc (btype_bits s.stype)
      | _ -> acc) acc m.signals) 0 p.modules

(* Round up to the next power of two, with a sane minimum. *)
let pow2_ceiling ?(floor = 4096) n =
  let n = max n floor in
  let p = ref 1 in
  while !p < n do p := !p * 2 done;
  !p

(* Heuristic: the most likely top is a module that nobody instantiates
   AND has the largest *recursive* subtree.  Local-only score caused
   picosoc_regs (fat register file, no instances) to beat picosoc
   (4 child instances, each itself substantial).  We now compute
   subtree score = local + sum of subtree(child) for every child
   instance, with cycle protection.  Unresolved instances are given a
   nominal weight so a pure-instance top doesn't lose to a leaf with a
   bigger array.                                                      *)
let pick_default_top (p : Behavioral_ir.bprogram) =
  let by_name = Hashtbl.create 16 in
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Hashtbl.replace by_name m.name m) p.modules;
  let local_score (m : Behavioral_ir.bmodule) =
    let sig_bits = List.fold_left (fun acc (s : Behavioral_ir.bsignal) ->
      acc + btype_bits s.stype) 0 m.signals in
    sig_bits + 10 * List.length m.processes
  in
  let memo : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let rec subtree visiting (m : Behavioral_ir.bmodule) =
    if Hashtbl.mem visiting m.name then 0
    else match Hashtbl.find_opt memo m.name with
    | Some s -> s
    | None ->
        Hashtbl.add visiting m.name ();
        let kids = List.fold_left (fun acc (i : Behavioral_ir.binstance) ->
          match Hashtbl.find_opt by_name i.module_name with
          | Some child -> acc + subtree visiting child
          | None -> acc + 500   (* unresolved — assume substantial *)
        ) 0 m.instances in
        Hashtbl.remove visiting m.name;
        let s = local_score m + kids in
        Hashtbl.replace memo m.name s; s
  in
  let instantiated =
    List.fold_left (fun acc (m : Behavioral_ir.bmodule) ->
      List.fold_left (fun acc (i : Behavioral_ir.binstance) ->
        i.module_name :: acc) acc m.instances) [] p.modules in
  let candidates =
    List.filter (fun (m : Behavioral_ir.bmodule) ->
      not (List.mem m.name instantiated)) p.modules in
  let pool = if candidates = [] then p.modules else candidates in
  let scored = List.map (fun m ->
    (m, subtree (Hashtbl.create 4) m)) pool in
  match List.sort (fun (_, a) (_, b) -> compare b a) scored with
  | (m, _) :: _ -> m.name
  | []          -> ""

let orfs_dir () =
  try Sys.getenv "ORFS_DIR"
  with Not_found ->
    (try Sys.getenv "HOME" ^ "/OpenROAD-flow-scripts"
     with Not_found -> "/opt/OpenROAD-flow-scripts")

let mkdir_p path =
  if not (Sys.file_exists path) then
    ignore (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote path)))

let write_file path s =
  let oc = open_out path in
  output_string oc s;
  close_out oc

let write_orfs_files cfg =
  mkdir_p cfg.o_workdir;
  let period_ns = 1.0 /. cfg.o_freq_ghz in
  let io_delay  = period_ns *. 0.1 in
  let sdc =
    (* OpenSTA's TCL doesn't include `remove_from_collection`; use a
       foreach + name-filter loop instead.  Applying set_input_delay to
       the clock port itself works in most STA tools but is semantically
       redundant once create_clock is in place — explicitly exclude it
       to avoid double-constraining the clock pin.                    *)
    Printf.sprintf
      "current_design %s\n\
       create_clock -name clk -period %.3f [get_ports clk]\n\
       foreach p [get_ports *] {\n\
         \  if {[get_property $p direction] eq \"input\" && [get_property $p name] ne \"clk\"} {\n\
         \    set_input_delay %.3f -clock clk $p\n\
         \  }\n\
       }\n\
       set_output_delay %.3f -clock clk [all_outputs]\n"
      cfg.o_top period_ns io_delay io_delay
  in
  let sdc_path = Filename.concat cfg.o_workdir "constraint.sdc" in
  write_file sdc_path sdc;
  let verilog_files = String.concat " " cfg.o_files in
  let mk =
    Printf.sprintf
      "export DESIGN_NICKNAME = %s\n\
       export DESIGN_NAME     = %s\n\
       export PLATFORM        = %s\n\
       export VERILOG_FILES   = %s\n\
       export SDC_FILE        = %s\n\
       export CORE_UTILIZATION       = %d\n\
       export CORE_ASPECT_RATIO      = 1\n\
       export CORE_MARGIN            = 2\n\
       export PLACE_DENSITY_LB_ADDON = 0.20\n\
       export TNS_END_PERCENT        = 100\n\
       export SYNTH_MEMORY_MAX_BITS  = %d\n"
      cfg.o_top cfg.o_top cfg.o_platform verilog_files sdc_path cfg.o_util
      cfg.o_mem_bits
  in
  let mk_path = Filename.concat cfg.o_workdir "config.mk" in
  write_file mk_path mk;
  (sdc_path, mk_path)

(* Hijacking the ORFS Makefile so USE_DECOMP_SYNTH=1 routes 1_2_yosys.v
   through synth_orfs_shim instead of yosys+ABC.  test/orfs/install.sh
   applies the patch idempotently; we run it on demand if the patch
   isn't there yet.                                                   *)
let repo_root () =
  (* Walk up from the executable's directory looking for test/orfs/install.sh
     so the GUI works whether launched from the repo root or _build/. *)
  let exe = try Sys.executable_name with _ -> Sys.argv.(0) in
  let exe_abs =
    if Filename.is_relative exe
    then Filename.concat (Sys.getcwd ()) exe else exe in
  let rec walk dir n =
    let probe = Filename.concat dir "test/orfs/install.sh" in
    if Sys.file_exists probe then Some dir
    else if n = 0 then None
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else walk parent (n - 1)
  in
  match walk (Filename.dirname exe_abs) 6 with
  | Some d -> d
  | None -> Filename.dirname exe_abs    (* best-effort *)

let decomp_synth_patch_applied () =
  let mk = Filename.concat (orfs_dir ()) "flow/Makefile" in
  if not (Sys.file_exists mk) then false
  else
    try
      let ic = open_in mk in
      let found = ref false in
      (try while not !found do
         let l = input_line ic in
         if (try ignore (Str.search_forward
                           (Str.regexp_string "USE_DECOMP_SYNTH") l 0); true
             with Not_found -> false)
         then found := true
       done with End_of_file -> ());
      close_in ic;
      !found
    with _ -> false

let ensure_decomp_synth_patch () =
  if decomp_synth_patch_applied () then true
  else begin
    let install_sh =
      Filename.concat (repo_root ()) "test/orfs/install.sh" in
    if not (Sys.file_exists install_sh) then begin
      error_dialog (Printf.sprintf
        "ORFS Makefile patch not applied and install script not found at:\n\
         %s\n\nCannot enable USE_DECOMP_SYNTH automatically." install_sh);
      false
    end else begin
      append_text (Printf.sprintf
        "[orfs] applying Makefile patch via %s …\n" install_sh);
      let rc = Sys.command (Filename.quote install_sh) in
      if rc <> 0 then begin
        error_dialog (Printf.sprintf
          "%s failed (rc=%d).  See terminal for details." install_sh rc);
        false
      end else begin
        append_text "[orfs] Makefile patch installed\n";
        true
      end
    end
  end

let decomp_shim_exe () =
  Filename.concat (repo_root ())
    "_build/default/synth_orfs_shim.exe"

let orfs_prep_exe () =
  Filename.concat (repo_root ())
    "_build/default/orfs_prep.exe"

(* Date stamp used as FLOW_VARIANT — keeps each run in its own
   results subdir under <flow>/results/<plat>/<top>/<stamp>/.       *)
let stamp_now () =
  let t = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d_%02d%02d%02d"
    (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
    t.tm_hour t.tm_min t.tm_sec

(* OpenRAM technology to match an ORFS platform.  Hardcaml memlower
   reads MEM_MACRO_TECH from the environment to decide which OpenRAM
   tech directory to invoke.  scn4m_subm is OpenRAM's bundled
   educational PDK and won't physically align with nangate45's metal
   stack — using it produces a "valid but wrong" SRAM macro.        *)
let openram_tech_for_platform = function
  | "nangate45" -> "freepdk45"
  | "sky130hd"  -> "sky130"
  | "gf180mcu"  -> "gf180mcu"
  | _           -> "scn4m_subm"   (* asap7 has no OpenRAM tech — fall back *)

let spawn_orfs cfg =
  (* If the user wants our hardcaml synth, ensure the Makefile patch
     and the shim binary are both ready before launching. *)
  let use_decomp = cfg.o_use_decomp in
  let proceed =
    if not use_decomp then true
    else if not (Sys.file_exists (decomp_shim_exe ())) then begin
      error_dialog (Printf.sprintf
        "synth_orfs_shim.exe not found at:\n%s\n\nBuild it with:\n\
         dune build _build/default/synth_orfs_shim.exe"
        (decomp_shim_exe ()));
      false
    end else
      ensure_decomp_synth_patch ()
  in
  if not proceed then raise Exit;

  let sdc_path, base_mk_path = write_orfs_files cfg in
  let flow_dir = Filename.concat (orfs_dir ()) "flow" in
  let mem_tech = openram_tech_for_platform cfg.o_platform in
  let plat_dir =
    Filename.concat (orfs_dir ())
      (Filename.concat "flow"
         (Filename.concat "platforms" cfg.o_platform))
  in
  (* When using our hardcaml synth, run orfs_prep first.  It picks a
     date-stamped FLOW_VARIANT, runs the same pipeline as the make-
     time shim, and writes a self-contained per-variant config.mk
     with ADDITIONAL_LEFS / ADDITIONAL_LIBS for any memory macros it
     emitted.  Without that, the GUI's bare config.mk leaves OpenROAD
     unable to find the macro LEF and floorplan rejects the design
     (ORD-2013 "LEF master not found").  The bare config.mk we just
     wrote is still useful as a fallback when use_decomp=false. *)
  let variant, mk_path =
    if use_decomp && Sys.file_exists (orfs_prep_exe ()) then begin
      let v = stamp_now () in
      let prep_argv = [|
        orfs_prep_exe ();
        "--top"; cfg.o_top;
        "--orfs-flow"; flow_dir;
        "--platform"; cfg.o_platform;
        "--design-cfg-dir"; cfg.o_workdir;
        "--sdc"; sdc_path;
        "--variant"; v;
        "--";
      |] in
      let argv = Array.append prep_argv (Array.of_list cfg.o_files) in
      (* orfs_prep needs the same memory-backend env as the make-time
         shim — re-export them so its catalogue + wrapper-emit pick
         FakeRAM / OpenRAM as the user selected.                     *)
      let prefix p e =
        let pl = String.length p in
        String.length e >= pl && String.sub e 0 pl = p in
      let parent = Array.to_list (Unix.environment ()) in
      let parent = List.filter (fun e ->
        not (prefix "MEM_MACRO_TECH=" e
             || prefix "MEMLOWER=" e
             || prefix "MEM_USE_FAKERAM=" e
             || prefix "FAKERAM_PLATFORM_DIR=" e)) parent in
      let extras =
        ("MEM_MACRO_TECH=" ^ mem_tech)
        :: (match cfg.o_mem_back with
            | Mem_bitblast -> [ "MEMLOWER=0" ]
            | Mem_fakeram  -> [ "MEM_USE_FAKERAM=1"
                              ; "FAKERAM_PLATFORM_DIR=" ^ plat_dir ]
            | Mem_openram  -> []) in
      let env = Array.of_list (parent @ extras) in
      append_text (Printf.sprintf "[orfs] preflight: orfs_prep variant=%s\n" v);
      let pid = Unix.create_process_env (orfs_prep_exe ()) argv env
                  Unix.stdin Unix.stderr Unix.stderr in
      let _, status = Unix.waitpid [] pid in
      (match status with
       | Unix.WEXITED 0 ->
           let prep_mk = Filename.concat
             (Filename.concat cfg.o_workdir v) "config.mk" in
           if Sys.file_exists prep_mk then v, prep_mk
           else begin
             append_text "[orfs] orfs_prep produced no config; using bare config.mk\n";
             "base", base_mk_path
           end
       | _ ->
           append_text "[orfs] orfs_prep failed; using bare config.mk\n";
           "base", base_mk_path)
    end else
      "base", base_mk_path
  in
  set_text "";
  append_text (Printf.sprintf
    "[orfs] workdir=%s\n[orfs] variant=%s\n[orfs] config.mk=%s\n[orfs] flow=%s\n"
    cfg.o_workdir variant mk_path flow_dir);
  let extra_args =
    if use_decomp
    then [| "USE_DECOMP_SYNTH=1";
            "DECOMP_SHIM=" ^ decomp_shim_exe ();
            "FLOW_VARIANT=" ^ variant |]
    else [| "FLOW_VARIANT=" ^ variant |]
  in
  let make_cmd_str =
    Printf.sprintf "make -C %s DESIGN_CONFIG=%s FLOW_VARIANT=%s%s"
      flow_dir mk_path variant
      (if use_decomp
       then " USE_DECOMP_SYNTH=1 DECOMP_SHIM=" ^ decomp_shim_exe ()
       else "")
  in
  append_text (Printf.sprintf "[orfs] running: %s\n" make_cmd_str);
  if use_decomp then
    append_text "[orfs] synth backend: hardcaml (synth_orfs_shim) — yosys/ABC bypassed\n"
  else
    append_text "[orfs] synth backend: yosys + ABC (stock ORFS)\n";
  append_text "\n";
  set_status (Printf.sprintf "ORFS: %s on %s — running" cfg.o_top cfg.o_platform);

  if use_decomp then begin
    let label = match cfg.o_mem_back with
      | Mem_fakeram  -> "FakeRAM (pre-built tables)"
      | Mem_openram  -> Printf.sprintf "OpenRAM (tech=%s)" mem_tech
      | Mem_bitblast -> "bit-blast (MEMLOWER=0, all flops)"
    in
    append_text (Printf.sprintf "[orfs] memory backend: %s\n" label);
    if cfg.o_mem_back = Mem_fakeram then
      append_text (Printf.sprintf
        "[orfs] FakeRAM platform dir: %s\n" plat_dir);
    append_text
      "[orfs] HIER_SYNTH_STUB_ON_FAIL=1 (failing modules become \
       port-only stubs — temporary)\n"
  end;
  let r, w = Unix.pipe () in
  let pid =
    try
      let base = [| "make"; "-C"; flow_dir; "DESIGN_CONFIG=" ^ mk_path |] in
      let argv = Array.append base extra_args in
      (* Inherit parent env; layer in memory-backend selection per cfg:
         FakeRAM → MEM_USE_FAKERAM=1 + FAKERAM_PLATFORM_DIR pointing at
                   the ORFS platform tree (where pre-built fakeram45_*
                   .lef/.lib live).
         OpenRAM → just MEM_MACRO_TECH (matched to platform).
         Bit-blast → MEMLOWER=0 in the shim's env.

         HIER_SYNTH_STUB_ON_FAIL=1 is forced ON for now: if a single
         module's Hardcaml lowering fails, the synth shim emits a
         port-only stub for that module instead of aborting the whole
         flow.  Lets the user push large designs (e.g. picosoc with
         spimemio's partial-slice-write chain) through layout while
         specific modules are debugged.  Temporary — once the underlying
         lowering bugs are fixed this default should flip back to
         strict.                                                      *)
      let prefix p e =
        let pl = String.length p in
        String.length e >= pl && String.sub e 0 pl = p
      in
      let env =
        let parent = Array.to_list (Unix.environment ()) in
        let parent = List.filter (fun e ->
          not (prefix "MEM_MACRO_TECH=" e
               || prefix "MEMLOWER=" e
               || prefix "MEM_USE_FAKERAM=" e
               || prefix "FAKERAM_PLATFORM_DIR=" e
               || prefix "HIER_SYNTH_STUB_ON_FAIL=" e)) parent in
        let extras =
          ("MEM_MACRO_TECH=" ^ mem_tech)
          :: "HIER_SYNTH_STUB_ON_FAIL=1"
          :: (match cfg.o_mem_back with
              | Mem_bitblast -> [ "MEMLOWER=0" ]
              | Mem_fakeram  -> [ "MEM_USE_FAKERAM=1"
                                ; "FAKERAM_PLATFORM_DIR=" ^ plat_dir ]
              | Mem_openram  -> [])
        in
        Array.of_list (parent @ extras)
      in
      Unix.create_process_env "make" argv env Unix.stdin w w
    with e ->
      Unix.close r; Unix.close w;
      error_dialog ("Failed to launch make: " ^ Printexc.to_string e);
      raise Exit
  in
  Unix.close w;
  let chan = GMain.Io.channel_of_descr r in
  let bytes = Bytes.create 4096 in
  let _watch =
    GMain.Io.add_watch chan ~prio:0 ~cond:[`IN; `HUP; `ERR]
      ~callback:(fun _conds ->
        let n = try Unix.read r bytes 0 (Bytes.length bytes)
                with Unix.Unix_error _ -> 0 in
        if n > 0 then begin
          append_text (Bytes.sub_string bytes 0 n);
          true
        end else begin
          (try Unix.close r with _ -> ());
          let msg =
            try
              let _, status = Unix.waitpid [Unix.WNOHANG] pid in
              match status with
              | Unix.WEXITED 0    -> "ORFS run complete"
              | Unix.WEXITED rc   -> Printf.sprintf "ORFS exited %d" rc
              | Unix.WSIGNALED s  -> Printf.sprintf "ORFS killed by sig %d" s
              | Unix.WSTOPPED s   -> Printf.sprintf "ORFS stopped %d" s
            with _ ->
              "ORFS finished (status unknown)"
          in
          append_text ("\n[orfs] " ^ msg ^ "\n");
          set_status msg;
          (* Suggest next step: open the layout view (Phase 2). *)
          let result_dir =
            Filename.concat
              (Filename.concat flow_dir
                 (Filename.concat "results" cfg.o_platform))
              (Filename.concat cfg.o_top "base") in
          if Sys.file_exists (Filename.concat result_dir "6_final.def")
          then append_text (Printf.sprintf
            "[orfs] artifacts: %s\n  6_final.def, 6_final.v, 6_final.sdc, 6_final.spef\n"
            result_dir);
          false
        end)
  in
  ()

(* Modal dialog form.  Pre-fills from current_prog when available so the
   common case is "click Run". *)
let show_orfs_run_dialog () =
  let parent = need_window () in
  let d = GWindow.dialog
    ~title:"Run ORFS layout"
    ~parent ~modal:true ~width:560 ~height:300 () in
  d#add_button "Cancel" `CANCEL;
  d#add_button "Run"    `OK;

  let pack_row ?(label_w = 140) label widget =
    let row = GPack.hbox ~spacing:8 ~border_width:4
      ~packing:d#vbox#pack () in
    let _ = GMisc.label ~text:label ~width:label_w ~xalign:0.0
              ~packing:row#pack () in
    row#pack ~expand:true ~fill:true (widget : #GObj.widget :> GObj.widget)
  in

  let default_top, default_file = match !current_prog with
    | Some (path, p) ->
        (* Common case: the user picked foo.sv and there's a module
           named foo in it — that's almost always the intended top.
           Fall back to the recursive subtree heuristic only if no
           module matches the seed file's basename, which catches
           generated/auto-named files but never overrules the
           obvious intent.                                          *)
        let stem = Filename.chop_extension (Filename.basename path) in
        let by_basename = List.exists
          (fun (m : Behavioral_ir.bmodule) -> m.name = stem) p.modules in
        let top =
          if by_basename then stem
          else match pick_default_top p with
            | "" -> stem
            | n  -> n
        in (top, path)
    | None -> ("", "")
  in
  let default_workdir =
    let home = try Sys.getenv "HOME" with Not_found -> "." in
    let stem = if default_top = "" then "design" else default_top in
    Filename.concat home (Filename.concat "sv_suite_orfs" stem)
  in

  let top_e   = GEdit.entry ~text:default_top ()  in
  let file_e  = GEdit.entry ~text:default_file () in

  pack_row "Top module:" top_e;

  let file_box = GPack.hbox ~spacing:4 () in
  file_box#pack ~expand:true ~fill:true file_e#coerce;
  let browse = GButton.button ~label:"Browse…" ~packing:file_box#pack () in
  ignore (browse#connect#clicked ~callback:(fun () ->
    let p = open_file_dialog () in
    if p <> "" then file_e#set_text p));
  pack_row "Verilog file:" file_box;

  let platforms = ["nangate45"; "sky130hd"; "asap7"] in
  let combo, _ = GEdit.combo_box_text
    ~strings:platforms ~active:0 () in
  pack_row "Platform:" combo;

  let freq_e = GEdit.entry ~text:"1.0" () in
  pack_row "Target freq (GHz):" freq_e;

  let util_e = GEdit.entry ~text:"30" () in
  pack_row "Core utilisation (%):" util_e;

  let workdir_e = GEdit.entry ~text:default_workdir () in
  pack_row "Workdir:" workdir_e;
  (* Auto-update workdir as the user edits the Top field — but only if
     the user hasn't manually edited the workdir away from the
     auto-derived path.  Tracked via a simple flag flipped by the
     workdir entry's `changed` signal.                              *)
  let workdir_user_edited = ref false in
  let workdir_pre = ref default_workdir in
  ignore (workdir_e#connect#changed ~callback:(fun () ->
    if workdir_e#text <> !workdir_pre then workdir_user_edited := true));
  ignore (top_e#connect#changed ~callback:(fun () ->
    if not !workdir_user_edited then begin
      let home = try Sys.getenv "HOME" with Not_found -> "." in
      let stem = let t = top_e#text in if t = "" then "design" else t in
      let p = Filename.concat home
        (Filename.concat "sv_suite_orfs" stem) in
      workdir_pre := p;
      workdir_e#set_text p
    end));

  let decomp_chk = GButton.check_button
    ~label:"Use decompiler synth (hardcaml gate-level netlist instead of yosys+ABC)"
    ~active:true
    ~packing:(d#vbox#pack ~padding:4) () in

  let mem_label = GMisc.label
    ~text:"Memory backend:" ~xalign:0.0
    ~packing:(d#vbox#pack ~padding:4) () in
  ignore mem_label;
  let mem_box = GPack.hbox ~spacing:8 ~packing:d#vbox#pack () in
  let fakeram_rb = GButton.radio_button
    ~label:"FakeRAM (pre-built)"
    ~active:true ~packing:mem_box#pack () in
  let openram_rb = GButton.radio_button
    ~group:fakeram_rb#group
    ~label:"OpenRAM (generate)"
    ~packing:mem_box#pack () in
  let bitblast_rb = GButton.radio_button
    ~group:fakeram_rb#group
    ~label:"Bit-blast (flops)"
    ~packing:mem_box#pack () in
  ignore openram_rb; ignore bitblast_rb;

  let hint = GMisc.label
    ~text:(Printf.sprintf
      "ORFS install: %s\nResults will land in <ORFS>/flow/results/<platform>/<top>/base/"
      (orfs_dir ()))
    ~xalign:0.0 ~justify:`LEFT ~packing:d#vbox#pack () in
  hint#set_line_wrap true;

  d#vbox#misc#show_all ();

  let result =
    match d#run () with
    | `OK ->
        (try Some {
           o_top        = top_e#text;
           o_files      = [file_e#text];   (* expanded by do_orfs_run *)
           o_platform   = (try List.nth platforms combo#active
                           with _ -> "nangate45");
           o_freq_ghz   = (try float_of_string freq_e#text with _ -> 1.0);
           o_util       = (try int_of_string util_e#text with _ -> 30);
           o_workdir    = workdir_e#text;
           o_mem_bits   = 4096;     (* refined by do_orfs_run from BIR *)
           o_use_decomp = decomp_chk#active;
           o_mem_back   =
             if      bitblast_rb#active then Mem_bitblast
             else if openram_rb#active  then Mem_openram
             else                            Mem_fakeram;
         }
         with _ -> None)
    | _ -> None
  in
  d#destroy ();
  result

let do_orfs_run () =
  match show_orfs_run_dialog () with
  | None -> ()
  | Some cfg ->
      let seed = match cfg.o_files with f :: _ -> f | [] -> "" in
      if cfg.o_top = "" || seed = "" then
        error_dialog "Top and Verilog file are required."
      else if not (Sys.file_exists seed) then
        error_dialog ("Verilog file not found:\n" ^ seed)
      else if not (Sys.file_exists (orfs_dir ())) then
        error_dialog (Printf.sprintf
          "ORFS install not found at %s.\nSet $ORFS_DIR or install at \
           $HOME/OpenROAD-flow-scripts." (orfs_dir ()))
      else
        try
          (* Same dependency closure as Decompile→Parse Verible.  Yosys's
             read_verilog needs every module that gets instantiated (the
             "Module \\X referenced … is not part of the design" error
             from a single-file submission).                            *)
          set_status "ORFS: discovering Verilog dependencies …";
          let name_map = module_name_index_for seed in
          let files, prog =
            close_verible_dependencies
              ~seed ~on_missing:prompt_locate_module name_map in
          let names = List.map (fun (m : Behavioral_ir.bmodule) -> m.name)
                        prog.modules in
          if not (List.mem cfg.o_top names) then begin
            error_dialog (Printf.sprintf
              "Top module '%s' not found among discovered modules.\n\n\
               Discovered: %s\n\n\
               Edit the Top field or pick a different seed file."
              cfg.o_top
              (String.concat ", " (List.sort compare names)))
          end else begin
            set_text "";
            append_text (Printf.sprintf
              "[orfs] dependency closure: %d files, %d modules\n"
              (List.length files) (List.length names));
            List.iter (fun f ->
              append_text ("  " ^ f ^ "\n")) files;
            (* Auto-size SYNTH_MEMORY_MAX_BITS from the largest array we
               discovered.  Yosys's platform default (4096) trips on
               anything bigger — picosoc_mem at 256×32 = 8192 bits is
               typical.  Round up to the next pow2 with a 4096 floor.  *)
            let max_bits = max_array_bits prog in
            let mem_cap = pow2_ceiling (max max_bits 4096) in
            if max_bits > 0 then
              append_text (Printf.sprintf
                "[orfs] largest array: %d bits → SYNTH_MEMORY_MAX_BITS = %d\n"
                max_bits mem_cap)
            else
              append_text (Printf.sprintf
                "[orfs] no arrays detected → SYNTH_MEMORY_MAX_BITS = %d\n"
                mem_cap);
            append_text "\n";
            spawn_orfs { cfg with o_files = files; o_mem_bits = mem_cap }
          end
        with Exit -> ()

(* ---------- Phase 2: DEF/LEF reader + Cairo layout viewer ----------
   ORFS produces 6_final.def in results/<platform>/<design>/base/.  We
   read it line-wise (DIEAREA + UNITS + COMPONENTS PLACED) and walk the
   platform's LEF directory for MACRO SIZE entries, then render with
   Cairo on a GtkDrawingArea fitted to the window.  No grammar changes
   needed — DEF/LEF top-level keywords are stable enough that line
   regexes are simpler than extending the menhir parser.              *)

(* ---- hierarchy colour-coding -------------------------------------------
   A cell's hierarchy is its first path component.  yosys's `flatten` tags a
   module's internal cells as "$flatten\\eth.$abc$..." -- they carry the
   hierarchy WITHOUT starting with it -- so the marker is stripped first;
   matching on a plain prefix finds only a fraction of a subsystem. *)
let hier_of (name : string) : string =
  let m = "$flatten\\" in
  let lm = String.length m in
  let s =
    if String.length name > lm && String.sub name 0 lm = m
    then String.sub name lm (String.length name - lm) else name in
  match String.index_opt s '.' with
  | Some i when i > 0 -> String.sub s 0 i
  | _ -> "(top)"

(* stable colour per hierarchy: hash -> hue, fixed saturation/value so the
   groups stay distinguishable and the same subsystem keeps its colour between
   runs. *)
(* Colour per FUNCTION CLASS, for a DEF written at BEL resolution: these are
   the macro names opendcp_xml emits for what is packed into a site.  Returning
   an option lets the caller tell "this is a known function" from "colour it by
   hierarchy instead", so the site-level DEF is unaffected. *)
let func_colour (m : string) : (float * float * float) option =
  match m with
  | "LUT6"   -> Some (0.20, 0.40, 0.85)
  | "LUT5"   -> Some (0.40, 0.60, 0.95)
  | "FF"     -> Some (0.85, 0.20, 0.20)
  | "FF5"    -> Some (0.95, 0.45, 0.45)
  | "CARRY4" -> Some (0.95, 0.60, 0.10)
  | "MUXF7"  -> Some (0.65, 0.35, 0.80)
  | "MUXF8"  -> Some (0.50, 0.20, 0.70)
  | "DRAM"   -> Some (0.15, 0.65, 0.45)
  | "SRL"    -> Some (0.10, 0.55, 0.55)
  | "RAMB18" -> Some (0.20, 0.70, 0.30)
  | "RAMB36" -> Some (0.10, 0.55, 0.20)
  | "DSP48"  -> Some (0.80, 0.75, 0.10)
  | "IOB"    -> Some (0.55, 0.35, 0.20)
  | "BUFG"   -> Some (0.90, 0.30, 0.65)
  | "MMCM"   -> Some (0.75, 0.45, 0.85)
  | "GT"     -> Some (0.30, 0.30, 0.35)
  | "SLICE_OTHER" | "OTHER" -> Some (0.60, 0.60, 0.60)
  | _ -> None

let hier_colour (k : string) : float * float * float =
  if k = "(top)" then (0.30, 0.50, 0.80)
  else begin
    let h = float_of_int (abs (Hashtbl.hash k) mod 360) /. 60.0 in
    let s = 0.70 and v = 0.95 in
    let i = int_of_float (Float.of_int (int_of_float h)) mod 6 in
    let f = h -. Float.of_int (int_of_float h) in
    let p = v *. (1.0 -. s) in
    let q = v *. (1.0 -. s *. f) in
    let t' = v *. (1.0 -. s *. (1.0 -. f)) in
    match i with
    | 0 -> (v, t', p) | 1 -> (q, v, p) | 2 -> (p, v, t')
    | 3 -> (p, q, v)  | 4 -> (t', p, v) | _ -> (v, p, q)
  end

type placed = {
  p_inst   : string;
  p_cell   : string;
  p_x      : int;       (* DBU *)
  p_y      : int;
  p_orient : string;
}

(* a net for the nextpnr-JSON routing overlay: driver + sink positions
   pre-resolved to placement coords (empty for DEF layouts). *)
type lnet = {
  ln_name   : string;
  ln_drv    : int * int;
  ln_snks   : (int * int) list;
  (* Sites the net actually reaches, read from the routed net's pip list rather
     than from connectivity: the LOAD PIPS.  For a global buffer the root is one
     BUFGCTRL in a corner and the driver->sink star says nothing -- what matters
     is where the clock spine taps down into fabric, and which loads it never
     reached.  A skipped arc leaves its load with no tap pip, so (snks - taps)
     IS the unrouted set. *)
  ln_taps   : (int * int) list;
  ln_tracks : string list;                (* distinct GCLK_* tracks used *)
}

(* One hop of nextpnr's critical path (NEXTPNR_CRIT_PATH_REPORT=1).  The report
   prints tile coordinates, but the viewer indexes placement by SLICE_XnYm, so
   hops carry CELL NAMES and are resolved against the placement table at draw
   time -- the two coordinate systems do not correspond. *)
type chop = {
  ch_src : string;                      (* driving cell *)
  ch_dst : string;                      (* sink cell *)
  ch_net : string;
  ch_ns  : float;                       (* net delay of this hop *)
}

type layout = {
  l_design     : string;
  l_units      : int;                                 (* DBU per μm *)
  l_die        : int * int * int * int;               (* x1 y1 x2 y2 in DBU *)
  l_components : placed list;
  l_macro_um   : (string, float * float) Hashtbl.t;   (* cell → (w,h) μm *)
  (* LEF PIN geometry per macro: (pin, direction, rect in μm relative to the
     macro origin).  Without it a cell can only be drawn as a blank box. *)
  l_macro_pins : (string, (string * string * (float * float * float * float)) list)
                   Hashtbl.t;
  l_nets       : lnet list;                           (* nextpnr routing overlay *)
  (* DEF routing, in DBU.  A replayed vendor layout carries its whole routing in
     the NETS section -- for ethmin that is 4770 nets, ~72k segments and ~62k
     vias -- and it is the routing, not the 1022 site rectangles, that makes the
     picture look like a chip.  Segments carry their layer so wire CLASS
     (LOCAL/SINGLE/DOUBLE/QUAD/HEX/LONG) can be colour-coded. *)
  l_segs       : (int * int * int * int * string) list;  (* x1 y1 x2 y2 layer *)
  l_vias       : (int * int) list;                       (* pip sites *)
  l_hi         : (string, unit) Hashtbl.t;            (* net names to highlight *)
  l_crit       : chop list;                           (* worst critical path *)
  l_crit_clk   : string;                              (* which clock it belongs to *)
  l_crit_ns    : float;                               (* its total *)
}

let layout_lines path =
  let ic = open_in path in
  let rec loop acc =
    match try Some (input_line ic) with End_of_file -> None with
    | Some l -> loop (l :: acc)
    | None   -> List.rev acc
  in
  let lines = loop [] in
  close_in ic;
  lines

let def_design_re  = Str.regexp "DESIGN[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)"
let def_units_re   = Str.regexp "UNITS DISTANCE MICRONS[ \t]+\\([0-9]+\\)"
let def_die_re     =
  Str.regexp
    "DIEAREA[ \t]+([ \t]*\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*)[ \t]+\
     ([ \t]*\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*)"
(* DEF orientations: N, S, E, W, FN, FS, FE, FW.  The flipped
   variants START WITH F, so the leading character class must
   include F too — earlier `[NSEW][NSEWF]*` rejected `FS` etc. and
   silently dropped half the placements (every flipped row of
   clkbufs / DFFs in particular).                                  *)
(* PLACED is not the only placement status.  A DEF that REPLAYS a vendor layout
   marks its components FIXED (they are being shown, not seeded), and a hard
   macro may be COVER.  Matching PLACED alone silently dropped every component of
   such a file -- the window opened with 0 instances and no hint why. *)
let def_comp_re    =
  Str.regexp
    "[ \t]*-[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\).*\\(PLACED\\|FIXED\\|COVER\\)[ \t]+\
     ([ \t]*\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*)[ \t]+\
     \\([NSEWF][NSEWF]*\\)"

let parse_def path =
  let lines = layout_lines path in
  let design = ref "" and units = ref 2000 in
  let die    = ref (0, 0, 0, 0) in
  let comps  = ref [] in
  List.iter (fun line ->
    if Str.string_match def_design_re line 0
    then design := Str.matched_group 1 line;
    if Str.string_match def_units_re line 0
    then units := int_of_string (Str.matched_group 1 line);
    (try
       let _ = Str.search_forward def_die_re line 0 in
       die := ( int_of_string (Str.matched_group 1 line)
              , int_of_string (Str.matched_group 2 line)
              , int_of_string (Str.matched_group 3 line)
              , int_of_string (Str.matched_group 4 line))
     with Not_found -> ());
    if Str.string_match def_comp_re line 0 then
      (* groups: 1 inst, 2 cell, 3 status (PLACED|FIXED|COVER), 4 x, 5 y, 6 orient.
         Str has no non-capturing group, so adding the status alternation shifted
         every index after it. *)
      comps := { p_inst   = Str.matched_group 1 line
               ; p_cell   = Str.matched_group 2 line
               ; p_x      = int_of_string (Str.matched_group 4 line)
               ; p_y      = int_of_string (Str.matched_group 5 line)
               ; p_orient = Str.matched_group 6 line } :: !comps
  ) lines;
  (* Routing comes from the DEF LEXER in lef_def, not from another hand-rolled
     tokeniser: it already handles STAR-repeated coordinates, comments and
     quoted names, and it is the reader placement.ml uses. *)
  let rt = try Lef_def.Placement.routing path
           with _ -> { Lef_def.Placement.segs = []; vias = []; die = (0,0,0,0);
                       units = 2000; design = "" } in
  {
    l_design     = !design;
    l_units      = !units;
    l_die        = !die;
    l_components = List.rev !comps;
    l_macro_um   = Hashtbl.create 256;
    l_macro_pins = Hashtbl.create 256;
    l_nets       = [];
    l_segs       = rt.Lef_def.Placement.segs;
    l_vias       = rt.Lef_def.Placement.vias;
    l_hi         = Hashtbl.create 1;
    l_crit       = []; l_crit_clk = ""; l_crit_ns = 0.0;
  }

let lef_macro_re = Str.regexp "[ \t]*MACRO[ \t]+\\([^ \t]+\\)"
let lef_size_re  =
  Str.regexp "[ \t]*SIZE[ \t]+\\([0-9.]+\\)[ \t]+BY[ \t]+\\([0-9.]+\\)"

let lef_pin_re = Str.regexp "[ \t]*PIN[ \t]+\\([^ \t]+\\)"
let lef_dir_re = Str.regexp "[ \t]*DIRECTION[ \t]+\\([A-Z]+\\)"
let lef_rect_re =
  Str.regexp
    "[ \t]*RECT[ \t]+\\(-?[0-9.]+\\)[ \t]+\\(-?[0-9.]+\\)[ \t]+\
     \\(-?[0-9.]+\\)[ \t]+\\(-?[0-9.]+\\)"

let parse_lef_into tbl pintbl path =
  let lines = layout_lines path in
  let cur = ref "" and pin = ref "" and dir = ref "" in
  List.iter (fun line ->
    if Str.string_match lef_macro_re line 0
    then (cur := Str.matched_group 1 line; pin := ""; dir := "")
    else if !cur <> "" && Str.string_match lef_size_re line 0 then begin
      let w = float_of_string (Str.matched_group 1 line) in
      let h = float_of_string (Str.matched_group 2 line) in
      Hashtbl.replace tbl !cur (w, h)
    end
    else if !cur <> "" && Str.string_match lef_pin_re line 0 then
      (pin := Str.matched_group 1 line; dir := "")
    else if !pin <> "" && Str.string_match lef_dir_re line 0 then
      dir := Str.matched_group 1 line
    else if !pin <> "" && Str.string_match lef_rect_re line 0 then begin
      let f k = float_of_string (Str.matched_group k line) in
      let r = (f 1, f 2, f 3, f 4) in
      let prev = try Hashtbl.find pintbl !cur with Not_found -> [] in
      Hashtbl.replace pintbl !cur ((!pin, !dir, r) :: prev);
      (* one rect per pin is all the viewer needs *)
      pin := ""
    end
  ) lines

let load_layout def_path lef_paths =
  let l = parse_def def_path in
  List.iter (fun p ->
    try parse_lef_into l.l_macro_um l.l_macro_pins p
    with _ -> ()) lef_paths;
  l

(* ── Phase 3: timing report parser + critical-path overlay ── *)

type timing_hop = {
  t_inst    : string;
  t_cell    : string;
  t_arrival : float;     (* ns *)
}

type timing_path = {
  tp_startpoint : string;
  tp_endpoint   : string;
  tp_hops       : timing_hop list;
  tp_text       : string;            (* full block text *)
}

(* ---- nextpnr critical-path overlay -------------------------------------
   A SECOND source for the path overlay, and for this flow the only usable
   one: OpenSTA's setup numbers here are advisory nonsense (arnoldi cascades
   slew down long LUT chains and inflates whole paths into the microseconds,
   e.g. -2299 ns), so nextpnr's own engine is what we optimise against.

   It also sidesteps the failure mode the OpenSTA overlay suffers from.  That
   one matches hops to placement by INSTANCE NAME, and a name-space mismatch
   silently yields "0/N hops matched".  nextpnr prints SITE COORDINATES in the
   report itself:

     Info:  7.1  7.4    Net <name> budget 1.773000 ns (154,333) -> (192,160)

   so the segment endpoints need no lookup at all and cannot mis-join.

   Feed it with:  NEXTPNR_CRIT_PATH_REPORT=1 (and NEXTPNR_POST_ROUTE_TIMING=1
   for routed rather than post-placement numbers), then point
   SV_GUI_NPNR_PATH at the log.  Set SV_GUI_NPNR_CLOCK to pick one clock
   domain by substring; otherwise the WORST (longest total) path is drawn. *)
type npnr_seg = {
  ns_x1 : int; ns_y1 : int;
  ns_x2 : int; ns_y2 : int;
  ns_delay : float;                    (* ns for this hop *)
  ns_net : string;
}

let npnr_seg_re =
  Str.regexp
    "Net \\(.*\\) budget [-0-9.]+ ns (\\([0-9]+\\),\\([0-9]+\\)) -> (\\([0-9]+\\),\\([0-9]+\\))"
let npnr_delay_re = Str.regexp "^Info: +\\([0-9.]+\\) +\\([0-9.]+\\) +Net "
let npnr_clock_re = Str.regexp "Critical path report for clock '\\([^']*\\)'"
let npnr_total_re = Str.regexp "\\([0-9.]+\\) ns logic, \\([0-9.]+\\) ns routing"

(* Returns (clock, segments, logic_ns, routing_ns) for the selected path. *)
let parse_nextpnr_critpath file : (string * npnr_seg list * float * float) option =
  let want = try Some (Sys.getenv "SV_GUI_NPNR_CLOCK") with Not_found -> None in
  let best = ref None in
  (try
     let ic = open_in file in
     let cur_clk = ref "" and cur = ref [] and cur_delay = ref 0.0 in
     let finish () =
       if !cur <> [] then begin
         let segs = List.rev !cur in
         let tot = List.fold_left (fun a s -> a +. s.ns_delay) 0.0 segs in
         let keep = match want with
           | Some w ->
             (* substring match on the clock name *)
             let re = Str.regexp_string w in
             (try ignore (Str.search_forward re !cur_clk 0); true
              with Not_found -> false)
           | None -> (match !best with None -> true | Some (_, _, t, _) -> tot > t) in
         if keep then best := Some (!cur_clk, segs, tot, !cur_delay)
       end;
       cur := []; cur_delay := 0.0 in
     (try
        while true do
          let l = input_line ic in
          if Str.string_match npnr_clock_re l 0
             || (try ignore (Str.search_forward npnr_clock_re l 0); true
                 with Not_found -> false)
          then begin finish (); cur_clk := Str.matched_group 1 l end
          else if (try ignore (Str.search_forward npnr_total_re l 0); true
                   with Not_found -> false)
          then cur_delay := float_of_string (Str.matched_group 2 l)
          else if (try ignore (Str.search_forward npnr_seg_re l 0); true
                   with Not_found -> false)
          then begin
            let net = Str.matched_group 1 l in
            let x1 = int_of_string (Str.matched_group 2 l) in
            let y1 = int_of_string (Str.matched_group 3 l) in
            let x2 = int_of_string (Str.matched_group 4 l) in
            let y2 = int_of_string (Str.matched_group 5 l) in
            (* the hop delay sits in the same line's leading columns *)
            let d = if Str.string_match npnr_delay_re l 0
                    then (try float_of_string (Str.matched_group 1 l) with _ -> 0.0)
                    else 0.0 in
            cur := { ns_x1 = x1; ns_y1 = y1; ns_x2 = x2; ns_y2 = y2;
                     ns_delay = d; ns_net = net } :: !cur
          end
        done
      with End_of_file -> ());
     finish ();
     close_in ic
   with Sys_error e -> Printf.eprintf "[npnr-path] %s\n%!" e);
  match !best with
  | Some (c, s, _, r) -> Some (c, s, 0.0, r)
  | None -> None

let npnr_path : (string * npnr_seg list * float * float) option ref = ref None

let path_re_start = Str.regexp "^Startpoint: \\([^ \t]+\\)"
let path_re_end   = Str.regexp "^Endpoint: \\([^ \t]+\\)"
let path_re_max   = Str.regexp "^Path Type: max"
let hop_re =
  (* Two columns of floats, then ^ or v, then inst/pin (cell).
     Group 3 (inst) is greedy any-non-WS, so on hierarchical names
     like cpu/_T_xxx/Q it backtracks to the LAST "/" — splitting
     "cpu/_T_xxx" (inst) from "Q" (pin).  Old pattern stopped at the
     first "/", which made every hop's t_inst the bare top-level
     prefix ("cpu") and broke the path-overlay lookup. *)
  Str.regexp
    "^[ \t]*\\([0-9.]+\\)[ \t]+\\([0-9.]+\\)[ \t]+[v^][ \t]+\
     \\([^ \t]+\\)/\\([^ \t/]+\\)[ \t]+(\\([^ \t)]+\\))"

let parse_path_block lines =
  let sp = ref "" and ep = ref "" and is_max = ref false in
  let hops = ref [] in
  List.iter (fun line ->
    if Str.string_match path_re_start line 0
    then sp := Str.matched_group 1 line;
    if Str.string_match path_re_end line 0
    then ep := Str.matched_group 1 line;
    if Str.string_match path_re_max line 0
    then is_max := true;
    if Str.string_match hop_re line 0 then
      hops := { t_inst = Str.matched_group 3 line
              ; t_cell = Str.matched_group 5 line
              ; t_arrival = float_of_string (Str.matched_group 2 line)
              } :: !hops
  ) lines;
  if !is_max && !sp <> "" && !hops <> [] then
    Some { tp_startpoint = !sp
         ; tp_endpoint   = !ep
         ; tp_hops       = List.rev !hops
         ; tp_text       = String.concat "\n" lines }
  else None

let parse_timing_report path =
  if not (Sys.file_exists path) then []
  else
    let lines = layout_lines path in
    let blocks = ref [] in
    let cur = ref [] in
    List.iter (fun line ->
      if Str.string_match path_re_start line 0 && !cur <> [] then begin
        blocks := List.rev !cur :: !blocks;
        cur := [line]
      end else
        cur := line :: !cur
    ) lines;
    if !cur <> [] then blocks := List.rev !cur :: !blocks;
    List.filter_map parse_path_block (List.rev !blocks)

let worst_max_path paths =
  match paths with
  | p :: _ -> Some p     (* OpenROAD orders by ascending slack *)
  | []     -> None

(* Build a [timing_path] from a [Lef_def.Placement_timing.report]
   when no 6_finish.rpt exists for an intermediate stage.  Uses our
   own placement-aware critical-path estimator (DEF + LEF + Liberty)
   to produce arrival times along the longest path back from the
   worst-arrival cell.  Numbers are unit-less ps from the report;
   t_arrival in our overlay struct is "ns" by convention so we
   divide by 1000.                                                 *)
let timing_path_of_report
      ~lib_path ~lef_path ~def_path ~design : timing_path option =
  let pin_dir =
    try
      let entries = Lef_def.Lef_pins.parse lef_path in
      Some (Lef_def.Lef_pins.table_of_entries entries)
    with _ -> None in
  let placements =
    try Lef_def.Placement.parse def_path with _ -> [] in
  let nets =
    try Lef_def.Nets.parse def_path with _ -> [] in
  if placements = [] then None
  else
    let delay_tbl =
      try Some (Cell_delay.load lib_path) with _ -> None in
    let r =
      match delay_tbl with
      | Some tbl ->
          Lef_def.Placement_timing.report ?pin_dir
            ~delay_of:(Cell_delay.lookup tbl)
            placements nets
      | None ->
          Lef_def.Placement_timing.report ?pin_dir
            placements nets in
    if r.worst_inst = "" then None
    else begin
      let hops =
        List.map (fun (inst, cell, arr_ps) ->
          { t_inst = inst; t_cell = cell;
            t_arrival = arr_ps /. 1000.0 }) r.path_hops in
      (* Build a per-hop table showing the incremental delay each
         cell contributes — that's what makes the report actionable.
         Δ for hop i = arr(i) - arr(i-1); for the first hop it's
         the cell's own delay (arr(0) itself).                      *)
      let hop_lines =
        let buf = Buffer.create 256 in
        Buffer.add_string buf
          "  arr(ns)  Δ(ps)  cell           inst\n";
        Buffer.add_string buf
          "  -------  -----  -------------  --------------------\n";
        let prev_arr = ref 0.0 in
        List.iter (fun h ->
          let delta_ps = (h.t_arrival -. !prev_arr) *. 1000.0 in
          Buffer.add_string buf
            (Printf.sprintf "  %7.3f  %5.1f  %-13s  %s\n"
               h.t_arrival delta_ps h.t_cell h.t_inst);
          prev_arr := h.t_arrival
        ) hops;
        Buffer.contents buf in
      let summary =
        Printf.sprintf
          "Estimated critical path (placement-aware, no STA report):\n\
           design = %s\n\
           worst inst = %s (%s)\n\
           worst arrival = %.3f ns (= %.1f ps)\n\
           total wire delay across signal nets = %.1f ps\n\
           path hops = %d\n\n\
           %s"
          design r.worst_inst r.worst_cell
          (r.worst_arr_ps /. 1000.0) r.worst_arr_ps
          r.total_wire_ps (List.length hops) hop_lines in
      Some { tp_startpoint = (match hops with
                              | [] -> "?"
                              | h :: _ -> h.t_inst);
             tp_endpoint   = r.worst_inst;
             tp_hops       = hops;
             tp_text       = summary }
    end

(* Index instances by name for fast lookup during overlay rendering. *)
let inst_index layout =
  let h = Hashtbl.create (List.length layout.l_components) in
  List.iter (fun p -> Hashtbl.replace h p.p_inst p) layout.l_components;
  h

(* Cairo rendering — fit-to-window, no zoom/pan yet.
   Optional [path] is overlaid as a thick red polyline through the centres
   of each hop's placed instance.  Hops whose instance isn't in DEF
   (clock buffers, port-side hops) are silently skipped.               *)
(* ---------- nextpnr placement/routing JSON reader ----------
   nextpnr-xilinx emits a yosys-json superset: each cell carries a
   NEXTPNR_BEL="SLICE_XxYy/slot" placement attribute, netnames map to bit
   ids, and connections+port_directions give driver/sinks.  We render cells
   at their BEL grid coords and overlay HIGHLIGHTED nets (driver->sinks) for
   placement/routing debug -- e.g. the unroutable clock loads a route.log's
   SKIP_FAILED_ARCS lines name.  Coord note: SLICE X/Y dominate; BRAM/DSP/BUFG
   sit at their own (coarser) site X/Y, so those few are only approximate. *)
let jmem k = function `Assoc a -> (try List.assoc k a with Not_found -> `Null) | _ -> `Null
let jassoc = function `Assoc a -> a | _ -> []
let jlist  = function `List l -> l | _ -> []
let jstr   = function `String s -> s | `Int i -> string_of_int i | _ -> ""

let bel_xy_re = Str.regexp "X\\([0-9]+\\)Y\\([0-9]+\\)"
let bel_xy s =
  try ignore (Str.search_forward bel_xy_re s 0);
      Some (int_of_string (Str.matched_group 1 s),
            int_of_string (Str.matched_group 2 s))
  with Not_found -> None

(* place_lef/carry_stamp always emit a bels.txt sibling (cellname<TAB>BEL);
   pull placement from it when the JSON carries no NEXTPNR_BEL -- an aborted or
   incomplete route never writes the routed JSON.  bels.txt names may carry a
   place_lef packing suffix ($carry/$mux/...) absent from the JSON connectivity,
   so index each position under BOTH the full and the $-stripped name. *)
let read_bels_txt dir pos =
  let f = Filename.concat dir "bels.txt" in
  if Sys.file_exists f then
    (try
       let ic = open_in f in
       (try while true do
          match String.split_on_char '\t' (input_line ic) with
          | cn :: bel :: _ ->
              (match bel_xy bel with
               | Some xy ->
                   Hashtbl.replace pos cn xy;
                   (match String.rindex_opt cn '$' with
                    | Some i when i > 0 -> Hashtbl.replace pos (String.sub cn 0 i) xy
                    | _ -> ())
               | None -> ())
          | _ -> ()
        done with End_of_file -> ());
       close_in ic
     with _ -> ())

let parse_nextpnr_json path =
  let j = Yojson.Safe.from_file path in
  let modules = jassoc (jmem "modules" j) in
  let is_top (_, mv) =
    match jmem "top" (jmem "attributes" mv) with `Null -> false | _ -> true in
  let (_, tm) =
    try List.find is_top modules
    with Not_found -> (match modules with x :: _ -> x | [] -> ("", `Null)) in
  let cells = jassoc (jmem "cells" tm) in
  let pos   = Hashtbl.create 8192 in
  let ctype = Hashtbl.create 8192 in
  let drv   = Hashtbl.create 16384 and snk = Hashtbl.create 16384 in
  List.iter (fun (cn, cv) ->
    Hashtbl.replace ctype cn (jstr (jmem "type" cv));
    let attrs = jmem "attributes" cv in
    let bel   = jstr (jmem "NEXTPNR_BEL" attrs) in
    let bel   = if bel = "" then jstr (jmem "BEL" attrs) else bel in
    (match bel_xy bel with Some xy -> Hashtbl.replace pos cn xy | None -> ());
    let conns = jassoc (jmem "connections" cv) in
    let dirs  = jassoc (jmem "port_directions" cv) in
    List.iter (fun (port, bits) ->
      let dir = jstr (try List.assoc port dirs with Not_found -> `Null) in
      List.iter (function
        | `Int b ->
            if dir = "output" then Hashtbl.replace drv b cn
            else Hashtbl.replace snk b
                   (cn :: (try Hashtbl.find snk b with Not_found -> []))
        | _ -> ()) (jlist bits)) conns) cells;
  if Hashtbl.length pos = 0 then read_bels_txt (Filename.dirname path) pos;
  let comps = ref [] in
  let minx = ref max_int and maxx = ref min_int
  and miny = ref max_int and maxy = ref min_int in
  Hashtbl.iter (fun cn (x, y) ->
    if Hashtbl.mem ctype cn then begin
      comps := { p_inst = cn; p_cell = Hashtbl.find ctype cn;
                 p_x = x; p_y = y; p_orient = "N" } :: !comps;
      if x < !minx then minx := x;  if x > !maxx then maxx := x;
      if y < !miny then miny := y;  if y > !maxy then maxy := y
    end) pos;
  let netnames = jassoc (jmem "netnames" tm) in
  (* nextpnr writes each routed net as an ROUTING attribute: repeating triples
     "wire;src->dst;strength".  A pip whose DESTINATION is a site wire
     (SITEWIRE/SLICE_XxYy/PIN) is the last hop into a load, so its coordinates
     are that load's own site -- those are the tap points.  Also collect the
     distinct global tracks -- GCLK_... -- the net rides, which runs out when
     a clock or high-fanout CE fails to reach the far corners. *)
  let parse_routing s =
    if s = "" then ([], []) else begin
      let taps = ref [] and tracks = Hashtbl.create 8 in
      List.iteri (fun i e ->
        if i mod 3 = 1 then
          match String.index_opt e '>' with
          | Some k when k > 0 && e.[k-1] = '-' ->
              let src = String.sub e 0 (k-1)
              and dst = String.sub e (k+1) (String.length e - k - 1) in
              let is_site w =
                String.length w > 9 && String.sub w 0 9 = "SITEWIRE/" in
              if is_site dst && not (is_site src) then
                (match bel_xy dst with Some xy -> taps := xy :: !taps | None -> ());
              List.iter (fun w ->
                match String.index_opt w '/' with
                | Some j ->
                    let base = String.sub w (j+1) (String.length w - j - 1) in
                    let is_gclk =
                      String.length base > 4 && String.sub base 0 4 = "GCLK" in
                    if is_gclk then Hashtbl.replace tracks base ()
                | None -> ()) [ src; dst ]
          | _ -> ()) (String.split_on_char ';' s);
      (!taps, Hashtbl.fold (fun k () a -> k :: a) tracks [])
    end in
  let nets = ref [] in
  List.iter (fun (nn, nv) ->
    match jlist (jmem "bits" nv) with
    | (`Int b) :: _ ->
        (match Hashtbl.find_opt drv b with
         | Some dc when Hashtbl.mem pos dc ->
             let sxys =
               List.filter_map (fun sc -> Hashtbl.find_opt pos sc)
                 (try Hashtbl.find snk b with Not_found -> []) in
             if sxys <> [] then begin
               let (taps, tracks) =
                 parse_routing (jstr (jmem "ROUTING" (jmem "attributes" nv))) in
               nets := { ln_name = nn; ln_drv = Hashtbl.find pos dc;
                         ln_snks = sxys; ln_taps = taps;
                         ln_tracks = List.sort compare tracks } :: !nets
             end
         | _ -> ())
    | _ -> ()) netnames;
  let pad = 4 in
  let die =
    if !minx = max_int then (0, 0, 1, 1)
    else (!minx - pad, !miny - pad, !maxx + pad, !maxy + pad) in
  { l_design     = Filename.basename path;
    l_units      = 1;
    l_die        = die;
    l_components = List.rev !comps;
    l_macro_um   = Hashtbl.create 4;
    l_macro_pins = Hashtbl.create 4;
    l_nets       = List.rev !nets;
    l_segs       = [];                 (* nextpnr routing arrives via l_nets *)
    l_vias       = [];
    l_hi         = Hashtbl.create 256;
    l_crit       = []; l_crit_clk = ""; l_crit_ns = 0.0 }

(* nextpnr's critical path (NEXTPNR_CRIT_PATH_REPORT=1), read from the same
   route log as the skips.  The report reads:

     Info: curr total
     Info:  0.3  0.3  Source <cell>.<port>
     Info:  6.5  6.8    Net <net> budget 7.779000 ns (23,248) -> (192,160)
     Info:               Sink <cell>.<port>
     ...
     Info: 0.9 ns logic, 19.1 ns routing

   One report per clock, and the log holds several (the placer's and the
   post-route pass), so keep the LAST report seen for each clock -- that is the
   post-route one -- and then the clock whose total is largest.  Every domain
   here comes out >85% ROUTING, so what matters visually is WHERE each hop goes,
   which is why this is worth drawing rather than reading. *)
let crit_src_re  = Str.regexp "Source \\([^ \t]+\\)\\."
let crit_sink_re = Str.regexp "Sink \\([^ \t]+\\)\\."
let crit_net_re  =
  Str.regexp "^Info: +\\([0-9.]+\\) +\\([0-9.]+\\) +Net \\([^ ]+\\) "
let crit_hdr_re  = Str.regexp "Critical path report for clock '\\([^']+\\)'"

let load_crit_path log_path =
  let per_clock : (string, chop list * float) Hashtbl.t = Hashtbl.create 8 in
  let clk = ref "" and hops = ref [] and src = ref "" and pend = ref None in
  let total = ref 0.0 in
  let finish () =
    if !clk <> "" && !hops <> [] then
      Hashtbl.replace per_clock !clk (List.rev !hops, !total);
    hops := []; src := ""; pend := None; total := 0.0 in
  (try
     let ic = open_in log_path in
     (try while true do
        let l = input_line ic in
        (try
           ignore (Str.search_forward crit_hdr_re l 0);
           finish (); clk := Str.matched_group 1 l
         with Not_found ->
           (try
              ignore (Str.search_forward crit_src_re l 0);
              src := Str.matched_group 1 l
            with Not_found ->
              (try
                 ignore (Str.search_forward crit_net_re l 0);
                 let ns = float_of_string (Str.matched_group 1 l) in
                 total := (try float_of_string (Str.matched_group 2 l) with _ -> !total);
                 pend := Some (Str.matched_group 3 l, ns)
               with Not_found ->
                 (try
                    ignore (Str.search_forward crit_sink_re l 0);
                    (match !pend with
                     | Some (net, ns) when !src <> "" ->
                       hops := { ch_src = !src; ch_dst = Str.matched_group 1 l;
                                 ch_net = net; ch_ns = ns } :: !hops
                     | _ -> ());
                    pend := None
                  with Not_found -> ()))))
      done with End_of_file -> ());
     close_in ic
   with _ -> ());
  finish ();
  (* Pick the clock with the worst SLACK, not the longest path.  The longest
     path here is the 25 MHz CPU domain at 18.9 ns, which passes comfortably,
     while the eth datapath misses a 125 MHz target -- ranking by absolute
     length points the overlay at the wrong path entirely.  nextpnr prints
       Max frequency for clock 'X': 83.59 MHz (FAIL at 125.00 MHz)
     so rank by achieved/target and let a FAIL outrank any PASS. *)
  let ratio = Hashtbl.create 8 in
  (try
     let ic = open_in log_path in
     let re = Str.regexp
         "Max frequency for clock +'\\([^']+\\)': +\\([0-9.]+\\) MHz +(\\(PASS\\|FAIL\\) at +\\([0-9.]+\\)" in
     (try while true do
        let l = input_line ic in
        (try
           ignore (Str.search_forward re l 0);
           let c = Str.matched_group 1 l in
           let got = float_of_string (Str.matched_group 2 l) in
           let fail = Str.matched_group 3 l = "FAIL" in
           let tgt = float_of_string (Str.matched_group 4 l) in
           if tgt > 0.0 then Hashtbl.replace ratio c (got /. tgt, fail)
         with Not_found -> ())
      done with End_of_file -> ());
     close_in ic
   with _ -> ());
  let score c t =
    match Hashtbl.find_opt ratio c with
    | Some (r, fail) -> (if fail then 0 else 1), r          (* fails first, tightest first *)
    | None -> 2, 1000.0 /. (t +. 1.0) in                    (* unranked: fall back to length *)
  Hashtbl.fold (fun c (h, t) acc ->
      let sc = score c t in
      match acc with
      | Some (_, _, _, bsc) when bsc <= sc -> acc
      | _ -> Some (c, h, t, sc)) per_clock None
  |> Option.map (fun (c, h, t, _) -> (c, h, t))

(* Pre-light the unroutable nets from the route log that produced THIS JSON.
   A build dir accumulates route*.log from earlier experiments (route_sr.log,
   route_bufh.log, ...); unioning them all lit up nets that later runs had
   already fixed.  nextpnr writes the routed JSON at the end of the run, so the
   matching log is the one whose mtime is closest to the JSON's — pick that one
   alone.  Returns the log basename used (for the status line). *)
let skip_re = Str.regexp "SKIP_FAILED_ARCS.*net '\\([^']+\\)'"
let load_skips_into hi json_path =
  let dir = Filename.dirname json_path in
  let files = try Sys.readdir dir with _ -> [||] in
  let mtime p = try (Unix.stat p).Unix.st_mtime with _ -> nan in
  let json_t = mtime json_path in
  (* closest mtime to the JSON; on a tie or missing stat, newest wins *)
  let best = ref None in
  Array.iter (fun f ->
    if String.length f >= 5 && String.sub f 0 5 = "route"
       && Filename.check_suffix f ".log" then begin
      let t = mtime (Filename.concat dir f) in
      let d = if Float.is_nan t || Float.is_nan json_t then infinity
              else Float.abs (t -. json_t) in
      match !best with
      | Some (_, bd, bt) when not (d < bd || (d = bd && t > bt)) -> ()
      | _ -> best := Some (f, d, t)
    end) files;
  match !best with
  | None -> None
  | Some (f, _, _) ->
      (try
         let ic = open_in (Filename.concat dir f) in
         (try while true do
            let l = input_line ic in
            (try ignore (Str.search_forward skip_re l 0);
                 Hashtbl.replace hi (Str.matched_group 1 l) ()
             with Not_found -> ())
          done with End_of_file -> ());
         close_in ic
       with _ -> ());
      Some f

(* The box a component occupies, in DEF units.  Shared by the renderer and by
   click hit-testing so the two cannot drift: a click must select the thing the
   user actually sees under the cursor. *)
let comp_box layout p =
  let w_um, h_um =
    try Hashtbl.find layout.l_macro_um p.p_cell
    with Not_found -> (0.5, 1.4) in
  let rotated = match p.p_orient with
    | "E" | "W" | "FE" | "FW" -> true
    | _ -> false in
  let w_um, h_um = if rotated then (h_um, w_um) else (w_um, h_um) in
  let u = float_of_int layout.l_units in
  (float_of_int p.p_x, float_of_int p.p_y, w_um *. u, h_um *. u)

(* Fit transform of the LAST render: (off_x, off_y, scale, x1, y1, dh).  A click
   arrives in device coordinates and has to be pushed back through it. *)
let last_fit : (float * float * float * int * int * float) option ref = ref None

(* Which component covers a point, in DEF units.  SMALLEST box wins: at BEL
   resolution the cells sit inside the site they were packed into, and the
   specific one is what was clicked.  Kept separate from the event handling so
   it can be exercised without a pointer -- see SV_GUI_HITTEST. *)
let component_at layout dx dy =
  let best = ref None in
  List.iter
    (fun p ->
       let (bx, by, bw, bh) = comp_box layout p in
       if dx >= bx && dx <= bx +. bw && dy >= by && dy <= by +. bh then
         match !best with
         | Some (_, a) when a <= bw *. bh -> ()
         | _ -> best := Some (p, bw *. bh))
    layout.l_components;
  match !best with Some (p, _) -> Some p | None -> None

(* Pin names collapsed to BUSES.  A RAMB36 carries several hundred pins --
   DIADI0..DIADI31 and so on -- and listing them one per name is both unreadable
   and long enough to push the drawing area out of the window.  Split each name
   into <base><digits>, group by base and direction, and print contiguous
   indices as base[hi:lo].  A base with a single member keeps its own name, so
   O6 stays O6 rather than becoming O[6]. *)
let collapse_pins pins =
  let split n =
    let l = String.length n in
    let rec back i = if i > 0 && n.[i-1] >= '0' && n.[i-1] <= '9' then back (i-1) else i in
    let i = back l in
    if i = l || i = 0 then (n, None)
    else (String.sub n 0 i, Some (int_of_string (String.sub n i (l - i)))) in
  let groups = Hashtbl.create 32 in
  let order = ref [] in
  List.iter
    (fun (n, d, _) ->
       let (base, idx) = split n in
       let key = (base, d) in
       if not (Hashtbl.mem groups key) then order := key :: !order;
       let prev = try Hashtbl.find groups key with Not_found -> [] in
       Hashtbl.replace groups key ((idx, n) :: prev))
    pins;
  let ranges idxs =
    let sorted = List.sort_uniq compare idxs in
    let rec go acc lo prev = function
      | [] -> List.rev ((lo, prev) :: acc)
      | x :: tl when x = prev + 1 -> go acc lo x tl
      | x :: tl -> go ((lo, prev) :: acc) x x tl in
    match sorted with [] -> [] | x :: tl -> go [] x x tl in
  let dirtag = function "INPUT" -> "in" | "OUTPUT" -> "out" | _ -> "?" in
  List.rev_map
    (fun (base, d) ->
       let members = Hashtbl.find groups (base, d) in
       match members with
       | [ (_, n) ] -> Printf.sprintf "%s:%s" n (dirtag d)
       | _ ->
         let idxs = List.filter_map (fun (i, _) -> i) members in
         if idxs = [] then Printf.sprintf "%s:%s" base (dirtag d)
         else
           let rs = ranges idxs in
           let spell (lo, hi) =
             if lo = hi then Printf.sprintf "%s[%d]" base lo
             else Printf.sprintf "%s[%d:%d]" base hi lo in
           Printf.sprintf "%s:%s"
             (String.concat "," (List.map spell rs)) (dirtag d))
    (List.sort compare !order)

(* What the selection box shows for a component.  Top level so the headless
   SV_GUI_HITTEST check exercises the SAME text the window displays. *)
let describe_component layout p =
  let u = float_of_int layout.l_units in
  let pins =
    match Hashtbl.find_opt layout.l_macro_pins p.p_cell with
    | None | Some [] -> "(none in the LEF)"
    | Some ps ->
      let cols = collapse_pins ps in
      Printf.sprintf "%d pin(s) in %d group(s)\n  %s"
        (List.length ps) (List.length cols)
        (String.concat "\n  " cols) in
  Printf.sprintf
    "Instance:  %s\nCell:      %s\nOrient:    %s\n\
     Placed at: ( %d %d ) = site ( %.2f %.2f )\n\nPins: %s"
    p.p_inst p.p_cell p.p_orient p.p_x p.p_y
    (float_of_int p.p_x /. u) (float_of_int p.p_y /. u) pins

let render_layout cr ~width ~height ?path layout =
  let (x1, y1, x2, y2) = layout.l_die in
  let dw = float_of_int (x2 - x1) and dh = float_of_int (y2 - y1) in
  Cairo.set_source_rgb cr 1.0 1.0 1.0;
  Cairo.paint cr;
  if dw <= 0.0 || dh <= 0.0 then ()
  else begin
    let margin = 0.05 in
    let avail_w = float_of_int width  *. (1.0 -. 2.0 *. margin) in
    let avail_h = float_of_int height *. (1.0 -. 2.0 *. margin) in
    let scale = min (avail_w /. dw) (avail_h /. dh) in
    let off_x = (float_of_int width  -. dw *. scale) /. 2.0 in
    let off_y = (float_of_int height -. dh *. scale) /. 2.0 in
    (* DEF y grows upward; Cairo y grows downward — flip. *)
    let xform x y =
      (off_x +. float_of_int (x - x1) *. scale,
       off_y +. (dh -. float_of_int (y - y1)) *. scale)
    in
    last_fit := Some (off_x, off_y, scale, x1, y1, dh);
    (* Die outline *)
    let (dx0, dy0) = xform x1 y2 in
    let (dx1, dy1) = xform x2 y1 in
    Cairo.set_source_rgb cr 0.0 0.0 0.0;
    Cairo.set_line_width cr 1.5;
    Cairo.rectangle cr dx0 dy0 ~w:(dx1 -. dx0) ~h:(dy1 -. dy0);
    Cairo.stroke cr;
    (* DEF routing, drawn UNDER the cells.  Colour is by wire class, which is
       what a 7-series route is actually made of: LOCAL taps inside a tile, then
       SINGLE/DOUBLE/QUAD/HEX/LONG as the span grows.  Segments are grouped by
       layer and stroked once per group -- 72k individual strokes would crawl. *)
    if layout.l_segs <> [] then begin
      let layer_colour = function
        | "SITE"   -> (0.55, 0.55, 0.55)
        | "LOCAL"  -> (0.20, 0.60, 0.20)
        | "SINGLE" -> (0.20, 0.40, 0.85)
        | "DOUBLE" -> (0.85, 0.55, 0.10)
        | "QUAD"   -> (0.80, 0.20, 0.20)
        | "HEX"    -> (0.60, 0.20, 0.75)
        | "LONG"   -> (0.10, 0.65, 0.70)
        | _        -> (0.45, 0.45, 0.45)
      in
      let by_layer : (string, (int * int * int * int) list ref) Hashtbl.t =
        Hashtbl.create 16 in
      List.iter (fun (x1s, y1s, x2s, y2s, lay) ->
        let l = (try Hashtbl.find by_layer lay with Not_found ->
                   let r = ref [] in Hashtbl.add by_layer lay r; r) in
        l := (x1s, y1s, x2s, y2s) :: !l) layout.l_segs;
      (* Zoom is a Cairo transform applied OVER this render, so a fixed line
         width is multiplied by it and the routing smears into blobs at exactly
         the magnification where the detail matters.  Divide by the current
         scale to keep hairlines one pixel on screen at any zoom. *)
      let cscale =
        let m = Cairo.get_matrix cr in
        if m.Cairo.xx > 0.0 then m.Cairo.xx else 1.0 in
      Cairo.set_line_width cr (0.6 /. cscale);
      Hashtbl.iter (fun lay ss ->
        let (r, g, b) = layer_colour lay in
        Cairo.set_source_rgba cr r g b 0.65;
        List.iter (fun (xa, ya, xb, yb) ->
          let (px, py) = xform xa ya and (qx, qy) = xform xb yb in
          Cairo.move_to cr px py; Cairo.line_to cr qx qy) !ss;
        Cairo.stroke cr) by_layer;
      (* Pips are NOT drawn by default.  A pip's position is only known to tile
         resolution, so 62k of them land on a regular lattice and read as a
         square grid ruled over the cells -- it hides the thing you are looking
         at and tells you nothing the segments do not already show.  Set
         SV_GUI_PIPS=1 to put them back. *)
      if Sys.getenv_opt "SV_GUI_PIPS" <> None then begin
        Cairo.set_source_rgba cr 0.15 0.15 0.15 0.5;
        let vr = 0.7 /. cscale in
        List.iter (fun (vx, vy) ->
          let (px, py) = xform vx vy in
          Cairo.rectangle cr (px -. vr) (py -. vr) ~w:(2.0 *. vr) ~h:(2.0 *. vr))
          layout.l_vias;
        Cairo.fill cr
      end
    end;
    (* Instances, COLOUR-CODED BY HIERARCHY so a subsystem's placement is
       visible at a glance -- e.g. whether the eth core sits as one compact
       block or is smeared across the die.  Cairo fills every queued path with
       the CURRENT source, so the cells are grouped by hierarchy first and each
       group filled separately.  Fill stays mostly transparent so dense regions
       still read as intensity. *)
    let units_f = float_of_int layout.l_units in
    let groups : (string, (float * float * float * float) list ref) Hashtbl.t =
      Hashtbl.create 32 in
    List.iter (fun p ->
      let (_, _, w_um, h_um) = comp_box layout p in
      let w_um = w_um /. units_f and h_um = h_um /. units_f in
      let w_dbu = w_um *. units_f in
      let h_dbu = h_um *. units_f in
      let (rx, ry) = xform p.p_x (p.p_y + int_of_float h_dbu) in
      (* A BEL-resolution DEF names the cell's FUNCTION as its macro, so colour
         by that -- the point of the finer file is to tell a LUT from a flop
         from carry inside one slice.  A macro the table does not know (the
         site-level DEF's SLICE_LOGIC etc.) keeps the hierarchy colouring. *)
      let k = match func_colour p.p_cell with
        | Some _ -> p.p_cell
        | None   -> hier_of p.p_inst in
      let l = (try Hashtbl.find groups k with Not_found ->
                 let r = ref [] in Hashtbl.add groups k r; r) in
      l := (rx, ry, w_dbu *. scale, h_dbu *. scale) :: !l)
      layout.l_components;
    Hashtbl.iter (fun k rects ->
      let (r, g, b) = match func_colour k with
        | Some c -> c
        | None   -> hier_colour k in
      Cairo.set_source_rgba cr r g b 0.55;
      List.iter (fun (x, y, w, h) -> Cairo.rectangle cr x y ~w ~h) !rects;
      Cairo.fill cr) groups;
    (* PINS.  Only worth drawing once a cell is big enough on screen for them to
       land on distinct pixels; below that they would just darken the box. *)
    if Hashtbl.length layout.l_macro_pins > 0 then begin
      let cscale =
        let m = Cairo.get_matrix cr in
        if m.Cairo.xx > 0.0 then m.Cairo.xx else 1.0 in
      List.iter (fun p ->
        match Hashtbl.find_opt layout.l_macro_pins p.p_cell with
        | None | Some [] -> ()
        | Some pins ->
          let (_, h_um) =
            try Hashtbl.find layout.l_macro_um p.p_cell
            with Not_found -> (0.5, 1.4) in
          let h_dbu = h_um *. units_f in
          if h_dbu *. scale *. cscale > 14.0 then
            List.iter (fun (_, dir, (x1p, y1p, x2p, y2p)) ->
              let (px, py) =
                xform (p.p_x + int_of_float (x1p *. units_f))
                      (p.p_y + int_of_float (y2p *. units_f)) in
              let w = (x2p -. x1p) *. units_f *. scale
              and h = (y2p -. y1p) *. units_f *. scale in
              (match dir with
               | "OUTPUT" -> Cairo.set_source_rgba cr 0.85 0.10 0.10 0.9
               | "INPUT"  -> Cairo.set_source_rgba cr 0.10 0.25 0.85 0.9
               | _        -> Cairo.set_source_rgba cr 0.45 0.45 0.45 0.9);
              Cairo.rectangle cr px py ~w:(max w (0.7 /. cscale))
                                       ~h:(max h (0.7 /. cscale));
              Cairo.fill cr)
              pins)
        layout.l_components
    end;
    (* nextpnr routing overlay.  Small nets keep the driver->sink star.  A
       global buffer's star is useless -- 149 lines from one corner BUFGCTRL
       tell you nothing -- so above WIDE_FANOUT we drop the lines and plot the
       LOAD PIPS instead: where the spine actually taps down.  Loads with no tap
       pip (the skipped arcs) are crossed in magenta, which is the whole point:
       you see WHICH corners the buffer failed to reach. *)
    if Hashtbl.length layout.l_hi > 0 && layout.l_nets <> [] then begin
      let wide_fanout = 32 in
      let n_hi = ref 0 and n_dead = ref 0 and tracks = Hashtbl.create 8 in
      List.iter (fun ln ->
        if Hashtbl.mem layout.l_hi ln.ln_name then begin
          incr n_hi;
          List.iter (fun t -> Hashtbl.replace tracks t ()) ln.ln_tracks;
          let wide = List.length ln.ln_snks > wide_fanout in
          let tapped = Hashtbl.create 64 in
          List.iter (fun xy -> Hashtbl.replace tapped xy ()) ln.ln_taps;
          let (dx, dy) = xform (fst ln.ln_drv) (snd ln.ln_drv) in
          if not wide then begin
            Cairo.set_source_rgba cr 0.95 0.15 0.15 0.80;
            Cairo.set_line_width cr 1.2;
            List.iter (fun (sx, sy) ->
              let (qx, qy) = xform sx sy in
              Cairo.move_to cr dx dy; Cairo.line_to cr qx qy) ln.ln_snks;
            Cairo.stroke cr
          end;
          (* root: filled when it carries the story, hollow when it doesn't *)
          Cairo.set_source_rgb cr 0.95 0.80 0.10;
          Cairo.rectangle cr (dx -. 3.0) (dy -. 3.0) ~w:6.0 ~h:6.0;
          if wide then (Cairo.set_line_width cr 1.5; Cairo.stroke cr)
          else Cairo.fill cr;
          (* reached loads: red dots at the tap pips (fall back to sink sites
             for an unrouted JSON, which carries no ROUTING at all) *)
          Cairo.set_source_rgb cr 0.95 0.15 0.15;
          List.iter (fun (sx, sy) ->
            let (qx, qy) = xform sx sy in
            Cairo.arc cr qx qy ~r:2.5 ~a1:0.0 ~a2:6.2831853; Cairo.fill cr)
            (if ln.ln_taps = [] then ln.ln_snks else ln.ln_taps);
          (* loads the net never reached *)
          if ln.ln_taps <> [] then
            List.iter (fun (sx, sy) ->
              if not (Hashtbl.mem tapped (sx, sy)) then begin
                incr n_dead;
                let (qx, qy) = xform sx sy in
                Cairo.set_source_rgb cr 1.0 0.20 0.90;
                Cairo.set_line_width cr 2.0;
                Cairo.move_to cr (qx -. 4.0) (qy -. 4.0);
                Cairo.line_to cr (qx +. 4.0) (qy +. 4.0);
                Cairo.move_to cr (qx +. 4.0) (qy -. 4.0);
                Cairo.line_to cr (qx -. 4.0) (qy +. 4.0);
                Cairo.stroke cr
              end) ln.ln_snks
        end) layout.l_nets;
      let tl = Hashtbl.fold (fun k () a -> k :: a) tracks [] in
      set_status (Printf.sprintf
        "%s — %d instances · %d highlighted net(s)%s%s"
        layout.l_design (List.length layout.l_components) !n_hi
        (if !n_dead = 0 then "" else Printf.sprintf " · %d UNREACHED load(s)" !n_dead)
        (if tl = [] then ""
         else Printf.sprintf " · tracks %s" (String.concat "," (List.sort compare tl))))
    end;
    (* Critical-path overlay *)
    (match path with
     | None -> ()
     | Some p ->
         let idx = inst_index layout in
         let matched = ref 0 in
         let unmatched = ref 0 in
         let unmatched_samples = ref [] in
         let pts = List.filter_map (fun (h : timing_hop) ->
           match Hashtbl.find_opt idx h.t_inst with
           | None ->
               incr unmatched;
               if List.length !unmatched_samples < 3 then
                 unmatched_samples := h.t_inst :: !unmatched_samples;
               None
           | Some pl ->
               incr matched;
               let w_um, h_um =
                 try Hashtbl.find layout.l_macro_um pl.p_cell
                 with Not_found -> (0.5, 1.4) in
               let cx_dbu = pl.p_x + int_of_float (w_um *. units_f /. 2.0) in
               let cy_dbu = pl.p_y + int_of_float (h_um *. units_f /. 2.0) in
               Some (cx_dbu, cy_dbu, xform cx_dbu cy_dbu)
         ) p.tp_hops in
         (* Telemetry — log to stderr (always visible) AND status bar.
            Helps spot hop-name mismatches vs genuinely clustered paths. *)
         let log msg =
           Printf.eprintf "[path-overlay] %s\n%!" msg;
           set_status msg in
         (match pts with
          | _ :: _ ->
              let xmin = ref max_int and xmax = ref min_int in
              let ymin = ref max_int and ymax = ref min_int in
              List.iter (fun (xd, yd, _) ->
                if xd < !xmin then xmin := xd;
                if xd > !xmax then xmax := xd;
                if yd < !ymin then ymin := yd;
                if yd > !ymax then ymax := yd) pts;
              let dx_um = float_of_int (!xmax - !xmin) /. units_f in
              let dy_um = float_of_int (!ymax - !ymin) /. units_f in
              log (Printf.sprintf
                "Path: %d/%d hops matched · bbox %.1f × %.1f µm%s"
                !matched (!matched + !unmatched) dx_um dy_um
                (if !unmatched > 0
                 then Printf.sprintf "  (sample miss: %s)"
                        (List.hd !unmatched_samples)
                 else ""));
              if !unmatched > 0 then begin
                Printf.eprintf "[path-overlay] more sample misses: %s\n%!"
                  (String.concat ", " !unmatched_samples);
                (* Also dump a few inst names from the index for comparison *)
                let idx_keys =
                  Hashtbl.fold (fun k _ acc ->
                    if List.length acc < 3 then k :: acc else acc) idx [] in
                Printf.eprintf "[path-overlay] sample placement insts: %s\n%!"
                  (String.concat ", " idx_keys)
              end
          | [] when !unmatched > 0 ->
              log (Printf.sprintf
                "Path: 0/%d hops matched (sample miss: %s)"
                !unmatched (List.hd !unmatched_samples));
              let idx_keys =
                Hashtbl.fold (fun k _ acc ->
                  if List.length acc < 3 then k :: acc else acc) idx [] in
              Printf.eprintf "[path-overlay] sample placement insts: %s\n%!"
                (String.concat ", " idx_keys)
          | [] -> ());
         let xy_pts = List.map (fun (_, _, p) -> p) pts in
         (match xy_pts with
          | [] -> ()
          | (x0, y0) :: rest ->
              Cairo.set_source_rgba cr 0.90 0.10 0.10 0.95;
              Cairo.set_line_width cr 2.5;
              Cairo.move_to cr x0 y0;
              List.iter (fun (x, y) -> Cairo.line_to cr x y) rest;
              Cairo.stroke cr;
              (* End-point markers *)
              Cairo.set_source_rgb cr 0.90 0.10 0.10;
              List.iter (fun (x, y) ->
                Cairo.arc cr x y ~r:3.0 ~a1:0.0 ~a2:(2.0 *. 3.14159265);
                Cairo.fill cr
              ) ((x0, y0) :: rest)))
  end

let open_layout_window ?critical_path layout =
  let win = GWindow.window
    ~title:(Printf.sprintf "Layout — %s (%d insts)"
              layout.l_design (List.length layout.l_components))
    ~width:1200 ~height:920 () in
  let vbox = GPack.vbox ~packing:win#add () in
  let hpane = GPack.paned `HORIZONTAL
    ~packing:(vbox#pack ~expand:true ~fill:true) () in
  let path_ref = ref critical_path in
  let da = GMisc.drawing_area () in
  hpane#pack1 ~resize:true ~shrink:true da#coerce;
  (* view transform on top of render_layout's fit-to-window: scroll wheel zooms
     around the cursor, left-drag pans, 'f' resets to fit. *)
  let zoom = ref 1.0 and pan_x = ref 0.0 and pan_y = ref 0.0 in
  let drag = ref None in
  da#misc#set_can_focus true;
  da#event#add [ `SCROLL; `SMOOTH_SCROLL; `BUTTON_PRESS; `BUTTON_RELEASE;
                 `POINTER_MOTION; `BUTTON1_MOTION ];
  ignore (da#misc#connect#draw ~callback:(fun cr ->
    let alloc = da#misc#allocation in
    (try
       Cairo.save cr;
       Cairo.translate cr !pan_x !pan_y;
       Cairo.scale cr !zoom !zoom;
       render_layout cr
         ~width:alloc.Gtk.width ~height:alloc.Gtk.height
         ?path:!path_ref layout;
       Cairo.restore cr
     with e ->
       Printf.eprintf "[layout] render failed: %s\n%!"
         (Printexc.to_string e));
    true));
  (* A TRACKPAD does not send UP/DOWN.  It sends `SMOOTH events carrying dx/dy
     deltas, so a handler that only understands the discrete directions does
     nothing at all under a two-finger gesture -- the view simply refuses to
     move.  Treat the two devices the way the rest of the desktop does:
       two-finger drag / wheel tilt  -> PAN along both axes
       ctrl + either                 -> ZOOM about the pointer
     A mouse wheel keeps its familiar plain-scroll-to-zoom, since that is what
     it has always done here and there is no second axis to pan with. *)
  ignore (da#event#connect#scroll ~callback:(fun ev ->
    let mx = GdkEvent.Scroll.x ev and my = GdkEvent.Scroll.y ev in
    let ctrl = List.mem `CONTROL (Gdk.Convert.modifier (GdkEvent.Scroll.state ev)) in
    let zoom_by f =
      if f <> 1.0 then begin
        zoom := !zoom *. f;
        (* keep the point under the cursor fixed *)
        pan_x := mx -. f *. (mx -. !pan_x);
        pan_y := my -. f *. (my -. !pan_y);
        da#misc#queue_draw ()
      end in
    (match GdkEvent.Scroll.direction ev with
     | `SMOOTH ->
         let dx = GdkEvent.Scroll.delta_x ev and dy = GdkEvent.Scroll.delta_y ev in
         if ctrl then
           (if dy <> 0.0 then zoom_by (exp (-. dy *. 0.12)))
         else if dx <> 0.0 || dy <> 0.0 then begin
           (* natural direction: content follows the fingers *)
           pan_x := !pan_x -. dx *. 40.0;
           pan_y := !pan_y -. dy *. 40.0;
           da#misc#queue_draw ()
         end
     | `UP    -> zoom_by (if ctrl then 1.15 else 1.15)
     | `DOWN  -> zoom_by (if ctrl then 1.0 /. 1.15 else 1.0 /. 1.15)
     | `LEFT  -> pan_x := !pan_x +. 40.0; da#misc#queue_draw ()
     | `RIGHT -> pan_x := !pan_x -. 40.0; da#misc#queue_draw ()
     | _ -> ());
    true));
  (* CLICK TO IDENTIFY.  Button 1 also starts a pan, so what counts as a click
     is decided on RELEASE: if the pointer never really moved, identify what is
     under it instead of treating it as a (zero-length) drag.  The description
     goes to the side panel, which is filled in further down -- hence the
     forward reference rather than a popup, so clicking around never has to be
     dismissed. *)
  let press_at = ref None in
  let show_sel = ref (fun (_ : string) -> ()) in
  let def_of_device ex ey =
    match !last_fit with
    | None -> None
    | Some (off_x, off_y, scale, x1, y1, dh) ->
      (* device -> the view transform the draw callback applied -> DEF units *)
      let ux = (ex -. !pan_x) /. !zoom and uy = (ey -. !pan_y) /. !zoom in
      Some (float_of_int x1 +. (ux -. off_x) /. scale,
            float_of_int y1 +. dh -. (uy -. off_y) /. scale) in
  let describe = describe_component layout in
  let identify_at ex ey =
    match def_of_device ex ey with
    | None -> ()
    | Some (dx, dy) ->
      (match component_at layout dx dy with
       | None -> !show_sel "(nothing here)"
       | Some p -> !show_sel (describe p)) in
  ignore (da#event#connect#button_press ~callback:(fun ev ->
    if GdkEvent.Button.button ev = 1 then begin
      press_at := Some (GdkEvent.Button.x ev, GdkEvent.Button.y ev);
      drag := Some (GdkEvent.Button.x ev -. !pan_x, GdkEvent.Button.y ev -. !pan_y)
    end;
    true));
  ignore (da#event#connect#button_release ~callback:(fun ev ->
    drag := None;
    (match !press_at with
     | Some (px, py) ->
       let ex = GdkEvent.Button.x ev and ey = GdkEvent.Button.y ev in
       if abs_float (ex -. px) < 3.0 && abs_float (ey -. py) < 3.0 then
         identify_at ex ey
     | None -> ());
    press_at := None;
    true));
  (* Hover names the thing under the pointer.  The hit test walks every
     component, so it is only run when the pointer has actually moved to a new
     one -- and never while dragging, where the answer would be discarded. *)
  let hover = ref None in
  ignore (da#event#connect#motion_notify ~callback:(fun ev ->
    (match !drag with
     | Some (ox, oy) ->
         pan_x := GdkEvent.Motion.x ev -. ox;
         pan_y := GdkEvent.Motion.y ev -. oy;
         da#misc#queue_draw ()
     | None ->
       (match def_of_device (GdkEvent.Motion.x ev) (GdkEvent.Motion.y ev) with
        | None -> ()
        | Some (dx, dy) ->
          let who = match component_at layout dx dy with
            | Some p -> Some (p.p_inst, p.p_cell)
            | None -> None in
          if who <> !hover then begin
            hover := who;
            match who with
            | Some (inst, cell) ->
              da#misc#set_tooltip_text (Printf.sprintf "%s\n%s" inst cell)
            | None -> da#misc#set_tooltip_text ""
          end));
    true));
  ignore (win#event#connect#key_press ~callback:(fun ev ->
    if GdkEvent.Key.keyval ev = GdkKeysyms._f then begin
      zoom := 1.0; pan_x := 0.0; pan_y := 0.0; da#misc#queue_draw ()
    end;
    false));

  (* Right pane: critical path side panel. *)
  let side = GPack.vbox ~spacing:4 ~border_width:6 () in
  hpane#pack2 ~resize:false ~shrink:true side#coerce;
  hpane#set_position 800;
  let header_text = match !path_ref with
    | Some p -> Printf.sprintf
        "Critical path (worst max-delay)\n%s → %s\n%d hop(s) shown"
        p.tp_startpoint p.tp_endpoint (List.length p.tp_hops)
    | None when layout.l_nets <> [] ->
        Printf.sprintf
          "nextpnr placement / routing\n%d placed cells · %d nets\n\
           %d net(s) pre-lit from the matching route log.\n\
           Type a net name / substring below to highlight\n\
           its driver (yellow) → sinks (red)."
          (List.length layout.l_components) (List.length layout.l_nets)
          (Hashtbl.length layout.l_hi)
    | None ->
        "No 6_finish.rpt found alongside this DEF\n\
         (or no max-delay path inside it)"
  in
  let header = GMisc.label ~text:header_text
    ~xalign:0.0 ~justify:`LEFT ~packing:side#pack () in
  header#set_line_wrap true;
  (* Selection box, directly under the header: a click fills this in rather
     than raising a dialog, so browsing the layout is uninterrupted.

     It lives in a SCROLLER with a fixed height and a capped line width.  A
     label sizes to its content, so a wide cell (a RAMB36 has several hundred
     pins) makes the side panel demand the whole window and the layout
     disappears -- the widget must be allowed to overflow, not to grow. *)
  let sel_scroll = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC ~packing:side#pack () in
  sel_scroll#misc#set_size_request ~height:240 ();
  let sel = GMisc.label ~text:"Click a component to identify it."
    ~xalign:0.0 ~yalign:0.0 ~justify:`LEFT () in
  sel_scroll#add_with_viewport sel#coerce;
  sel#set_line_wrap true;
  sel#set_max_width_chars 44;
  sel#set_selectable true;
  sel#misc#modify_font_by_name "Monospace 9";
  show_sel := (fun s -> sel#set_text s);
  let scrolled = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(side#pack ~expand:true ~fill:true) () in
  let tv = GText.view ~packing:scrolled#add () in
  tv#misc#modify_font_by_name "Monospace 9";
  tv#set_editable false;
  let body = match !path_ref with
    | Some p -> p.tp_text
    | None   -> ""
  in
  tv#buffer#set_text body;

  (* nextpnr routing debug: highlight nets by WILDCARD, chosen from a list.
     Typing a substring meant knowing a net's name before you could look at it,
     and these names are unusable by hand -- a flattened abc net reads
     `$flatten\eth.\i_mac.$abc$45263$auto$blifparse.cc:557:parse_blif$45311.
      genblk1.genblk1...f0`.  So offer PATTERNS instead: the categories you
     actually want (clocks, resets, enables, carry chains) plus whatever the
     design itself supplies.
     Patterns come from, in order: SV_GUI_NET_PATTERNS (a file, one glob per
     line, '#' comments), then `sv_gui_net_patterns.txt` beside the loaded
     design, then the built-ins below.  The chosen pattern is matched with
     `*`/`?` globbing against every net name; the match count is reported so a
     pattern that selects nothing says so instead of looking like a redraw. *)
  if layout.l_nets <> [] then begin
    let glob_match pat s =
      let np = String.length pat and ns = String.length s in
      let memo = Hashtbl.create 64 in
      let rec go i j =
        match Hashtbl.find_opt memo (i, j) with
        | Some r -> r
        | None ->
          let r =
            if i >= np then j >= ns
            else match pat.[i] with
              | '*' -> go (i + 1) j || (j < ns && go i (j + 1))
              | '?' -> j < ns && go (i + 1) (j + 1)
              | c   -> j < ns && s.[j] = c && go (i + 1) (j + 1) in
          Hashtbl.replace memo (i, j) r; r in
      go 0 0 in
    let builtin = [
      "*";                    (* everything *)
      "*clk*"; "*CLK*";
      "*rst*"; "*reset*"; "*RESET*"; "*SRESET*";
      "*_CE*"; "*cen*"; "*enable*";
      "*carry*"; "*CARRY*";
      "*NotGate*";            (* abc-replicated inverters (the SR class) *)
      "*abc*";
    ] in
    let from_file path =
      if Sys.file_exists path then begin
        let acc = ref [] in
        (try
           let ic = open_in path in
           (try while true do
              let l = input_line ic in
              let l = match String.index_opt l '#' with
                | Some i -> String.sub l 0 i | None -> l in
              let l = String.trim l in
              if l <> "" then acc := l :: !acc
            done with End_of_file -> ());
           close_in ic
         with Sys_error _ -> ());
        List.rev !acc
      end else [] in
    let extra =
      (match Sys.getenv_opt "SV_GUI_NET_PATTERNS" with
       | Some f -> from_file f | None -> [])
      @ from_file (Filename.concat (Sys.getcwd ()) "sv_gui_net_patterns.txt") in
    let pats = extra @ builtin in
    let hb = GPack.hbox ~spacing:4 ~packing:side#pack () in
    let _  = GMisc.label ~text:"net pattern:" ~packing:hb#pack () in
    let combo = GEdit.combo_box_text ~packing:(hb#pack ~expand:true ~fill:true) () in
    let combo_box, _ = combo in
    List.iter (fun p -> GEdit.text_combo_add combo p) pats;
    if pats <> [] then combo_box#set_active 0;
    let stat = GMisc.label ~text:"" ~packing:hb#pack () in
    let apply () =
      let i = combo_box#active in
      if i >= 0 && i < List.length pats then begin
        let q = List.nth pats i in
        let n = List.fold_left (fun n ln ->
          if glob_match q ln.ln_name
          then (Hashtbl.replace layout.l_hi ln.ln_name (); n + 1) else n) 0 layout.l_nets in
        stat#set_text (Printf.sprintf "%d/%d" n (List.length layout.l_nets))
      end;
      da#misc#queue_draw () in
    ignore (combo_box#connect#changed ~callback:apply);
    let addb = GButton.button ~label:"Add"   ~packing:hb#pack () in
    ignore (addb#connect#clicked ~callback:apply);
    let clrb = GButton.button ~label:"Clear" ~packing:hb#pack () in
    ignore (clrb#connect#clicked
      ~callback:(fun () -> Hashtbl.clear layout.l_hi; stat#set_text "";
                  da#misc#queue_draw ()))
  end;

  let (x1, y1, x2, y2) = layout.l_die in
  let info = Printf.sprintf
    "die %d×%d DBU  ·  %.1f×%.1f μm  ·  %d instances  ·  %d units/μm  ·  %d cell shapes"
    (x2 - x1) (y2 - y1)
    (float_of_int (x2 - x1) /. float_of_int layout.l_units)
    (float_of_int (y2 - y1) /. float_of_int layout.l_units)
    (List.length layout.l_components)
    layout.l_units
    (Hashtbl.length layout.l_macro_um)
  in
  let _ = GMisc.label ~text:info ~xalign:0.0 ~packing:vbox#pack () in
  win#show ()

let pick_results_dir () =
  let parent = need_window () in
  let d = GWindow.file_chooser_dialog
    ~action:`SELECT_FOLDER
    ~title:"Pick ORFS results dir (must contain 6_final.def)"
    ~parent ~modal:true () in
  d#add_button "Cancel" `CANCEL;
  d#add_button "Open"   `OK;
  (* Default to the most recent ORFS run if we can guess one. *)
  let guess =
    let flow = Filename.concat (orfs_dir ()) "flow" in
    let results = Filename.concat flow "results" in
    if Sys.file_exists results then Some results else None
  in
  (match guess with
   | Some g -> ignore (d#set_current_folder g)
   | None -> ());
  let path = match d#run () with
    | `OK -> (match d#filename with Some f -> f | None -> "")
    | _   -> ""
  in
  d#destroy ();
  path

(* Available P&R stage outputs.  ORFS writes <stage>.odb after each
   step; if the flow fails partway through, the later .odb files are
   absent.  6_final.def is the only signoff DEF; for intermediate
   stages we dump the .odb to a sibling .def via openroad. *)
let pr_stage_files dir =
  let stages = [
    "1_synth";
    "2_1_floorplan"; "2_2_floorplan_macro"; "2_3_floorplan_tapcell";
    "2_4_floorplan_pdn"; "2_floorplan";
    "3_1_place_gp_skip_io"; "3_2_place_iop"; "3_3_place_gp";
    "3_4_place_resized"; "3_5_place_dp"; "3_place";
    "4_1_cts"; "4_cts";
    "5_1_grt"; "5_2_route"; "5_3_fillcell"; "5_route";
  ] in
  let odbs = List.filter_map (fun s ->
    let p = Filename.concat dir (s ^ ".odb") in
    if Sys.file_exists p then Some (s, p) else None
  ) stages in
  let final_def = Filename.concat dir "6_final.def" in
  let with_final =
    if Sys.file_exists final_def
    then odbs @ [("6_final", final_def)]
    else odbs in
  with_final

(* Convert <stage>.odb → <stage>.def using openroad.  Cached: if the
   .def already exists alongside the .odb with newer mtime, skip.
   The .odb carries its own LEF references, so a bare read_db +
   write_def is enough — adding read_lef calls actually fails because
   openroad rejects duplicate library definitions.                  *)
let ensure_def_for_odb ~odb_path ~def_path =
  let need_run =
    not (Sys.file_exists def_path)
    || (Unix.stat odb_path).st_mtime > (Unix.stat def_path).st_mtime in
  if not need_run then ()
  else begin
    let openroad =
      Filename.concat (orfs_dir ()) "tools/install/OpenROAD/bin/openroad" in
    if not (Sys.file_exists openroad) then
      failwith ("openroad binary missing at " ^ openroad);
    (* Tcl quoting: { … } passes the path literally with no
       interpretation.  Filename.quote here would emit shell-style
       single quotes which Tcl treats as part of the filename. *)
    let tcl =
      Printf.sprintf "read_db {%s}\nwrite_def {%s}\nexit 0\n"
        odb_path def_path in
    let tcl_file = Filename.temp_file "stage_" ".tcl" in
    let oc = open_out tcl_file in
    output_string oc tcl; close_out oc;
    let log_file = Filename.temp_file "stage_or_" ".log" in
    let cmd = Printf.sprintf "%s -exit -no_init -no_splash %s > %s 2>&1"
      (Filename.quote openroad) (Filename.quote tcl_file)
      (Filename.quote log_file) in
    set_status (Printf.sprintf "Converting %s → %s …"
      (Filename.basename odb_path) (Filename.basename def_path));
    let rc = Sys.command cmd in
    (try Sys.remove tcl_file with _ -> ());
    if rc <> 0 then begin
      let snippet =
        try
          let ic = open_in log_file in
          let buf = Buffer.create 1024 in
          (try while true do Buffer.add_channel buf ic 256 done
           with End_of_file -> ());
          close_in ic;
          Buffer.contents buf
        with _ -> "" in
      (try Sys.remove log_file with _ -> ());
      failwith
        (Printf.sprintf "openroad write_def failed (rc=%d)\n%s" rc snippet)
    end else
      (try Sys.remove log_file with _ -> ())
  end

(* Stage picker — modal dialog listing available .odb stages plus
   6_final.def if present.  Returns (stage_label, def_path) or None.
   The latest available stage is pre-selected so the most-likely
   "what we got" view opens by default.                            *)
let pick_pr_stage stages =
  let parent = need_window () in
  let d = GWindow.dialog
    ~title:"Pick P&R stage to view"
    ~parent ~modal:true () in
  let _ = d#vbox#pack
    (GMisc.label ~text:"Layout stage:" ~xalign:0.0 ())#coerce in
  let combo = GEdit.combo_box_text ~packing:d#vbox#pack () in
  let combo_box, _ = combo in
  List.iter (fun (label, _) -> GEdit.text_combo_add combo label) stages;
  if stages <> [] then combo_box#set_active (List.length stages - 1);
  d#add_button "Cancel" `CANCEL;
  d#add_button "Open"   `OK;
  let result = match d#run () with
    | `OK ->
        let i = combo_box#active in
        if i >= 0 && i < List.length stages
        then Some (List.nth stages i)
        else None
    | _ -> None in
  d#destroy ();
  result

let do_open_orfs_run () =
  with_errors "open ORFS run" (fun () ->
    let dir = pick_results_dir () in
    if dir = "" then () else begin
      let stages = pr_stage_files dir in
      if stages = [] then begin
        error_dialog ("No .odb or 6_final.def found in:\n" ^ dir);
        ()
      end else
      match pick_pr_stage stages with
      | None -> ()
      | Some (stage_label, stage_file) ->
        let def_path =
          if Filename.check_suffix stage_file ".def"
          then stage_file
          else Filename.concat dir (stage_label ^ ".def") in
        (* results/<platform>/<design>/base → walk up to find platforms/<platform>/lef. *)
        let base_dir   = dir in
        let design_dir = Filename.dirname base_dir in
        let plat_dir   = Filename.dirname design_dir in
        let platform   = Filename.basename plat_dir in
        let results_p  = Filename.dirname plat_dir in
        let flow       = Filename.dirname results_p in
        let lef_dir    =
          Filename.concat flow
            (Filename.concat "platforms"
               (Filename.concat platform "lef")) in
        let lefs =
          if Sys.file_exists lef_dir && Sys.is_directory lef_dir then
            Array.fold_left (fun acc f ->
              if Filename.check_suffix f ".lef"
              then (Filename.concat lef_dir f) :: acc
              else acc)
              [] (Sys.readdir lef_dir)
          else []
        in
        (* Convert .odb → .def if the picked stage isn't already a
           .def (everything except 6_final). *)
        if not (Filename.check_suffix stage_file ".def") then
          ensure_def_for_odb ~odb_path:stage_file ~def_path;
        set_status (Printf.sprintf "Loading %s/%s (LEF count: %d) …"
                      stage_label (Filename.basename def_path)
                      (List.length lefs));
        let layout = load_layout def_path lefs in
        (* Timing report sits in the parallel reports/ tree, with the
           SAME variant name as the results dir we just opened:
             flow/results/<plat>/<des>/<variant>/  ↔
             flow/reports/<plat>/<des>/<variant>/
           Use the actual variant from the picked path — hard-coding
           "base" was a regression that made the GUI show -6.87
           (the original baseline) when the user opened a fresh
           stamped variant whose own report was at -2.26.            *)
        let variant = Filename.basename base_dir in
        let reports_dir =
          Filename.concat flow
            (Filename.concat "reports"
               (Filename.concat platform
                  (Filename.concat layout.l_design variant))) in
        (* Critical path: prefer 6_finish.rpt when present (final
           signoff has the real STA), otherwise fall back to our
           placement-aware estimator on the staged DEF + platform
           LEF + liberty.  The estimate is approximate (no real CTS,
           no slew propagation through Liberty NLDM) but good enough
           to highlight the worst path on a partial layout.        *)
        let rpt_path = Filename.concat reports_dir "6_finish.rpt" in
        let critical_path =
          if stage_label = "6_final" && Sys.file_exists rpt_path
          then worst_max_path (parse_timing_report rpt_path)
          else begin
            let lib_path =
              Filename.concat flow
                (Filename.concat "platforms"
                   (Filename.concat platform
                      "lib/NangateOpenCellLibrary_typical.lib")) in
            let lef_path =
              Filename.concat flow
                (Filename.concat "platforms"
                   (Filename.concat platform
                      "lef/NangateOpenCellLibrary.macro.mod.lef")) in
            if Sys.file_exists lib_path && Sys.file_exists lef_path
            then
              try
                timing_path_of_report
                  ~lib_path ~lef_path ~def_path ~design:layout.l_design
              with _ -> None
            else None
          end
        in
        open_layout_window ?critical_path layout;
        let extra = match critical_path, stage_label = "6_final" with
          | Some _, true  -> " + critical path"
          | Some _, false -> " + estimated critical path"
          | None,   false -> " (intermediate; no STA, estimator unavailable)"
          | None,   true  -> ""
        in
        set_status (Printf.sprintf
          "Opened layout %s @ %s — %d instances%s"
          layout.l_design stage_label
          (List.length layout.l_components) extra)
    end)

(* ---------- Synthesise to gate-level mapped netlist ----------

   Runs Synth_pipeline on the currently-loaded BIR (re-using the same
   dependency closure as ATPG), then converts the resulting hierarchy
   of Lib_map netlists back into a Behavioral_ir.bprogram and replaces
   current_prog.  The point is to feed Schematic → Show gate-level
   without making the user shuttle through a Verilog file.            *)

let bir_of_synth_netlists
    (netlists : Hier_synth.module_netlist list) : Behavioral_ir.bprogram =
  let mk_signal direction (name, width) : Behavioral_ir.bsignal = {
    name; direction;
    stype = Behavioral_ir.BInt { width; signed = Unsigned };
    initial_value = None;
    attrs = [];
  } in
  let modules = List.map (fun (mn : Hier_synth.module_netlist) ->
    let signals =
      List.map (mk_signal `Input)  mn.mn_real_inputs
      @ List.map (mk_signal `Output) mn.mn_real_outputs
      @ List.map (fun (w, width) -> mk_signal `Internal (w, width))
                 mn.mn_netlist.wires in
    let cell_insts = List.map (fun (i : Lib_map.instance) ->
      { Behavioral_ir.inst_name = i.inst_name;
        module_name = i.cell.cell_name;
        param_values = []; param_strs = [];
        port_connections =
          List.map (fun (pc : Lib_map.pin_conn) ->
            (pc.pin, Behavioral_ir.BVar pc.net)) i.conns }
    ) mn.mn_netlist.insts in
    let child_insts = List.map (fun (ci : Hier_synth.child_inst_emit) ->
      { Behavioral_ir.inst_name = ci.ci_inst;
        module_name = ci.ci_module;
        param_values = []; param_strs = [];
        port_connections =
          List.map (fun (p, n) -> (p, Behavioral_ir.BVar n)) ci.ci_conns }
    ) mn.mn_child_insts in
    { Behavioral_ir.name = mn.mn_name;
      params = []; signals; processes = [];
      instances = cell_insts @ child_insts;
      funcs = []; mems = []; attrs = [] }
  ) netlists in
  let cell_tbl : (string, Behavioral_ir.library_port list) Hashtbl.t =
    Hashtbl.create 64 in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    List.iter (fun (i : Lib_map.instance) ->
      if not (Hashtbl.mem cell_tbl i.cell.cell_name) then
        let ports =
          List.map (fun p ->
            { Behavioral_ir.port_name = p;
              port_direction = `Input;
              port_width = 1 }) i.cell.in_pins
          @ [ { Behavioral_ir.port_name = i.cell.out_pin;
                port_direction = `Output;
                port_width = 1 } ] in
        Hashtbl.add cell_tbl i.cell.cell_name ports
    ) mn.mn_netlist.insts
  ) netlists;
  let library_cells =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) cell_tbl [] in
  { modules; library_cells }

(* Core synthesis path: returns the mapped bprogram + a one-line
   summary string.  Side-effect-free wrt current_prog so callers can
   decide whether to install the result. *)
let synthesise_inner (path, p : string * Behavioral_ir.bprogram)
    : Behavioral_ir.bprogram * string =
  let pick_top () =
    let instantiated =
      List.concat_map (fun (m : Behavioral_ir.bmodule) ->
        List.map (fun (i : Behavioral_ir.binstance) -> i.module_name)
          m.instances) p.modules in
    match List.filter (fun (m : Behavioral_ir.bmodule) ->
            not (List.mem m.name instantiated)) p.modules with
    | [t] -> t.name
    | t :: _ -> t.name
    | [] -> (List.hd p.modules).name in
  let top = pick_top () in
  let name_map = module_name_index_for path in
  let files, _ =
    close_verible_dependencies
      ~seed:path ~on_missing:prompt_locate_module name_map in
  let out_path =
    Filename.concat (Filename.get_temp_dir_name ())
      (Printf.sprintf "sv_gui_synth_%s.v" top) in
  set_status (Printf.sprintf
    "Synthesising: top=%s, %d file(s)…" top (List.length files));
  let netlists, _ =
    Synth_pipeline.run ~emit_verilog:true ~top ~out_path ~files () in
  last_synth_out_path := Some out_path;
  let p' = bir_of_synth_netlists netlists in
  let n_cells = List.fold_left (fun a (m : Behavioral_ir.bmodule) ->
    a + List.length m.instances) 0 p'.modules in
  let summary = Printf.sprintf
    "Synthesis OK: top=%s, %d module(s), %d cell(s), %d cell types — \
     Verilog @ %s"
    top (List.length p'.modules) n_cells
    (List.length p'.library_cells) out_path in
  p', summary

(* Open any LEF/DEF pair -- not only an ORFS results directory.  The DEF names
   its own macros, so the library must come from the file beside it: prefer
   <base>.lef, and only fall back to every *.lef in the directory when there is
   no such file.  Reading them all unconditionally is what makes a second,
   near-identical library (a free-placement variant of the same design, say)
   collide on duplicate macro names. *)
let open_lef_def_path path =
    try
      let dir = Filename.dirname path in
      let base = Filename.remove_extension (Filename.basename path) in
      let sibling = Filename.concat dir (base ^ ".lef") in
      let lefs =
        if Sys.file_exists sibling then [ sibling ]
        else
          List.filter (fun f -> Filename.check_suffix f ".lef")
            (List.map (Filename.concat dir) (Array.to_list (Sys.readdir dir))) in
      let layout = load_layout path lefs in
      if layout.l_components = [] then
        error_dialog
          (Printf.sprintf
             "%s parsed, but no components were placed.\n\n\
              The COMPONENTS section must carry PLACED, FIXED or COVER \
              coordinates."
             (Filename.basename path))
      else begin
        let summary =
          Printf.sprintf
            "%s: %d instance(s), %d macro(s) with %d pin(s), %d LEF(s), \
             %d segment(s), %d pip(s)"
            (Filename.basename path) (List.length layout.l_components)
            (Hashtbl.length layout.l_macro_um)
            (Hashtbl.fold (fun _ ps n -> n + List.length ps)
               layout.l_macro_pins 0)
            (List.length lefs)
            (List.length layout.l_segs) (List.length layout.l_vias) in
        set_status summary;
        (* Also on stderr: the status bar is invisible to a headless run. *)
        prerr_endline summary;
        (* SV_GUI_HITTEST="x,y" (DEF units) runs the same lookup a click does,
           so the selection can be checked without a pointer. *)
        (match Sys.getenv_opt "SV_GUI_HITTEST" with
         | None -> ()
         | Some spec ->
           List.iter
             (fun pt ->
                match String.split_on_char ',' pt with
                | [sx; sy] ->
                  let dx = float_of_string (String.trim sx)
                  and dy = float_of_string (String.trim sy) in
                  (match component_at layout dx dy with
                   | None -> Printf.eprintf "hit (%g,%g): nothing\n%!" dx dy
                   | Some p ->
                     Printf.eprintf "hit (%g,%g):\n%s\n%!" dx dy
                       (describe_component layout p))
                | _ -> ())
             (String.split_on_char ';' spec));
        open_layout_window layout
      end
    with e -> error_dialog (Printexc.to_string e)

let do_open_lef_def () =
  let path = chooser_dialog `OPEN "Open DEF (LEF taken from beside it)" in
  if path <> "" then open_lef_def_path path

let do_open_nextpnr_json () =
  let path = chooser_dialog `OPEN "Open nextpnr placement/routing JSON" in
  if path <> "" then
    try
      let layout = parse_nextpnr_json path in
      let skip_log = load_skips_into layout.l_hi path in
      (* Same route log the skips came from carries the critical path. *)
      let layout =
        match skip_log with
        | None -> layout
        | Some f ->
          (match load_crit_path (Filename.concat (Filename.dirname path) f) with
           | Some (clk, hops, tot) ->
             { layout with l_crit = hops; l_crit_clk = clk; l_crit_ns = tot }
           | None -> layout) in
      if layout.l_components = [] then
        info_dialog
          "No placed cells (NEXTPNR_BEL) in this JSON.\n\
           Open a placed/routed nextpnr JSON — e.g. arp_stamped.json or\n\
           *_routed.json — not the pre-placement synth output."
      else begin
        (* Reuse the existing critical-path overlay rather than drawing a second
           one: it already resolves hops through inst_index and reports the
           match rate, which is the thing that silently breaks when two name
           spaces meet.  Arrival is cumulative so the hop labels read like the
           nextpnr report. *)
        let critical_path =
          if layout.l_crit = [] then None
          else begin
            let acc = ref 0.0 in
            let hops =
              (match layout.l_crit with
               | h :: _ -> [ { t_inst = h.ch_src; t_cell = ""; t_arrival = 0.0 } ]
               | [] -> [])
              @ List.map (fun (c : chop) ->
                  acc := !acc +. c.ch_ns;
                  { t_inst = c.ch_dst; t_cell = ""; t_arrival = !acc })
                layout.l_crit in
            Some { tp_startpoint = (match layout.l_crit with
                                    | h :: _ -> h.ch_src | [] -> "");
                   tp_endpoint   = (match List.rev layout.l_crit with
                                    | h :: _ -> h.ch_dst | [] -> "");
                   tp_hops       = hops;
                   tp_text       =
                     Printf.sprintf "nextpnr critical path, clock %s: %.1f ns\n%s"
                       layout.l_crit_clk layout.l_crit_ns
                       (String.concat "\n"
                          (List.map (fun (c : chop) ->
                               Printf.sprintf "  %6.2f ns  %s -> %s   (%s)"
                                 c.ch_ns c.ch_src c.ch_dst c.ch_net)
                             layout.l_crit)) }
          end in
        open_layout_window ?critical_path layout;
        set_status (Printf.sprintf
          "nextpnr JSON: %d cells · %d nets · %d skip-net(s) from %s%s"
          (List.length layout.l_components) (List.length layout.l_nets)
          (Hashtbl.length layout.l_hi)
          (match skip_log with Some f -> f | None -> "(no route log found)")
          (if layout.l_crit = [] then "  ·  no critical path in the log"
           else Printf.sprintf "  ·  crit path %s = %.1f ns over %d hop(s)"
                  layout.l_crit_clk layout.l_crit_ns (List.length layout.l_crit)))
      end
    with e ->
      error_dialog (Printf.sprintf "Failed to read nextpnr JSON:\n%s"
                      (Printexc.to_string e))

let do_synthesise () =
  with_errors "synthesise" (fun () ->
    match !current_prog with
    | None ->
        info_dialog "Load a SystemVerilog file first (Decompile → Parse…)."
    | Some (path, _ as cp) ->
        let p', summary = synthesise_inner cp in
        current_prog := Some (path ^ " [synthesised]", p');
        dump_prog ~banner:(summary ^ "\n\ncurrent_prog replaced with the \
                                       mapped netlist.\n\n") p';
        set_status summary)

(* ---------- Schematic generation ----------

   Three menu actions feed the new Schematic_layout/Schematic_view
   modules:

     • Load .slib …          — parse a Synopsys-style symbol library
                                file and stash it as the active set of
                                symbols for subsequent renders.
     • Show RTL schematic …  — render every binstance in a picked
                                module, falling back to auto-generated
                                symbols when no .slib entry exists.
     • Show gate-level …     — render only those binstances whose
                                module_name appears in [library_cells]
                                (i.e. post-mapping designs).

   The user picks the target bmodule from a small chooser dialog when
   the loaded BIR has more than one module.                            *)

let active_slib : Symbol_lib.library ref = ref (Symbol_lib.empty ())

let pick_module_dialog (p : Behavioral_ir.bprogram)
    : Behavioral_ir.bmodule option =
  match p.modules with
  | []  -> None
  | [m] -> Some m
  | ms ->
      let parent = need_window () in
      let d = GWindow.dialog ~title:"Select module" ~parent
                ~modal:true ~width:420 () in
      ignore (GMisc.label ~text:"Choose a module to display:"
                ~xalign:0.0
                ~packing:(d#vbox#pack ~padding:6) ());
      let combo, (store, col) =
        GEdit.combo_box_text ~strings:(List.map (fun (m : Behavioral_ir.bmodule) -> m.name) ms)
          ~packing:(d#vbox#pack ~padding:6) () in
      ignore store; ignore col;
      combo#set_active 0;
      d#add_button "Open" `OK;
      d#add_button "Cancel" `CANCEL;
      d#vbox#misc#show_all ();
      let r = d#run () in
      let idx = combo#active in
      d#destroy ();
      match r, idx with
      | `OK, i when i >= 0 && i < List.length ms ->
          Some (List.nth ms i)
      | _ -> None

let do_load_slib () =
  with_errors "load .slib" (fun () ->
    let p = open_file_dialog () in
    if p = "" then ()
    else begin
      let lib = Symbol_lib.parse_file p in
      Symbol_lib.merge ~into:!active_slib lib;
      let n = Hashtbl.fold (fun _ _ a -> a + 1) lib 0 in
      set_status (Printf.sprintf
        "Loaded %d symbols from %s (total active: %d)"
        n p (Hashtbl.fold (fun _ _ a -> a + 1) !active_slib 0))
    end)

(* Auto-fill the active library from the cells referenced by [m]:
   for every instance whose module_name is missing from active_slib,
   synthesise a stub from either library_cells or the referenced
   bmodule's signal list. *)
let auto_fill_missing
    ~(prog : Behavioral_ir.bprogram)
    (m : Behavioral_ir.bmodule) =
  List.iter (fun (i : Behavioral_ir.binstance) ->
    if not (Hashtbl.mem !active_slib i.module_name) then begin
      let pins = match List.assoc_opt i.module_name prog.library_cells with
        | Some lps ->
            List.map (fun (lp : Behavioral_ir.library_port) ->
              (lp.port_name,
               match lp.port_direction with
               | `Input -> "input" | `Output -> "output")) lps
        | None ->
            match List.find_opt (fun (mm : Behavioral_ir.bmodule) ->
                    mm.name = i.module_name) prog.modules with
            | Some mm ->
                List.filter_map (fun (s : Behavioral_ir.bsignal) ->
                  match s.direction with
                  | `Input  -> Some (s.name, "input")
                  | `Inout  -> Some (s.name, "inout")
                  | `Output -> Some (s.name, "output")
                  | `Internal -> None) mm.signals
            | None ->
                List.map (fun (p, _) -> (p, "input")) i.port_connections
      in
      Hashtbl.replace !active_slib i.module_name
        (Symbol_lib.auto_generate ~cell_name:i.module_name ~pins)
    end
  ) m.instances

let confirm_dialog msg =
  let d = GWindow.message_dialog
    ~message:msg ~message_type:`QUESTION
    ~buttons:GWindow.Buttons.yes_no
    ~parent:(need_window ()) ~modal:true () in
  let r = d#run () in
  d#destroy ();
  r = `YES

let do_show_schematic ~gate_only () =
  with_errors "show schematic" (fun () ->
    match !current_prog with
    | None ->
        info_dialog "Load a SystemVerilog file first (File → Open … or \
                     Decompile → Parse with verible …)."
    | Some (_label, p) ->
        (match pick_module_dialog p with
         | None -> ()
         | Some m_chosen ->
             let module_name = m_chosen.name in
             let needs_synth =
               m_chosen.instances = []
               || (gate_only &&
                   let lib_names = List.map fst p.library_cells in
                   not (List.exists (fun (i : Behavioral_ir.binstance) ->
                          List.mem i.module_name lib_names) m_chosen.instances))
             in
             let p, m =
               if needs_synth then begin
                 let prompt =
                   if m_chosen.instances = []
                   then Printf.sprintf
                          "Module '%s' is pure behavioural RTL (no \
                           instances to draw). Synthesise it to gates \
                           now and show the mapped schematic?" module_name
                   else Printf.sprintf
                          "Module '%s' has no library-cell instances. \
                           Synthesise to gates now?" module_name in
                 if not (confirm_dialog prompt) then (p, m_chosen)
                 else begin
                   match !current_prog with
                   | None -> (p, m_chosen)
                   | Some cp ->
                       let p', summary = synthesise_inner cp in
                       current_prog := Some (fst cp ^ " [synthesised]", p');
                       set_status summary;
                       let m' =
                         match List.find_opt (fun (m : Behavioral_ir.bmodule) ->
                                 m.name = module_name) p'.modules with
                         | Some m -> m
                         | None ->
                             (* Fall back to the synth top if name was
                                renamed (shouldn't happen, but be safe). *)
                             List.hd p'.modules in
                       (p', m')
                 end
               end else (p, m_chosen)
             in
             let m =
               if gate_only then
                 let lib_names = List.map fst p.library_cells in
                 let kept = List.filter (fun (i : Behavioral_ir.binstance) ->
                   List.mem i.module_name lib_names) m.instances in
                 { m with instances = kept }
               else m
             in
             if m.instances = [] then
               info_dialog
                 (Printf.sprintf
                   "Module '%s' still has nothing to draw after \
                    synthesis — the design may have optimised away to \
                    constants, or top selection picked the wrong \
                    module." m.name)
             else begin
               auto_fill_missing ~prog:p m;
               let sc = Schematic_layout.build
                          ~slib:!active_slib ~prog:p m in
               let title = if gate_only then "Gate-level schematic"
                                         else "RTL schematic" in
               Schematic_view.open_window ~title sc;
               set_status (Printf.sprintf
                 "%s: %s — %d cells, %d nets" title sc.sc_module
                 (List.length sc.sc_insts) (List.length sc.sc_nets))
             end))

(* ---------- script discovery / loading ---------- *)

let read_file path =
  let ic = open_in path in
  let n  = in_channel_length ic in
  let s  = really_input_string ic n in
  close_in ic; s

(* Walk up from start looking for [child]; return Some absolute-path if found
   within [steps] levels, else None. *)
let find_upwards ?(steps = 6) start child =
  let rec loop dir n =
    let p = Filename.concat dir child in
    if Sys.file_exists p && Sys.is_directory p then Some p
    else if n = 0 then None
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else loop parent (n - 1)
  in
  loop start steps

let script_dirs () =
  let xdg =
    try Sys.getenv "XDG_CONFIG_HOME"
    with Not_found ->
      try (Sys.getenv "HOME") ^ "/.config"
      with Not_found -> "/tmp"
  in
  let env_override =
    try [ Sys.getenv "SV_DECOMPILER_SCRIPTS" ] with Not_found -> []
  in
  let from_cwd =
    let cwd = Sys.getcwd () in
    match find_upwards cwd "gui_scripts" with
    | Some p -> [p]
    | None   -> [ Filename.concat cwd "gui_scripts" ]   (* fallback for log *)
  in
  let from_exe =
    let exe = try Sys.executable_name with _ -> Sys.argv.(0) in
    let exe_abs =
      if Filename.is_relative exe
      then Filename.concat (Sys.getcwd ()) exe else exe in
    match find_upwards (Filename.dirname exe_abs) "gui_scripts" with
    | Some p -> [p]
    | None   -> []
  in
  let user = [ Filename.concat xdg "sv_suite/scripts" ] in
  (* De-dup while preserving order. *)
  let seen = Hashtbl.create 4 in
  List.filter (fun d ->
    if Hashtbl.mem seen d then false
    else (Hashtbl.add seen d (); true))
    (env_override @ from_cwd @ from_exe @ user)

let load_scripts () =
  let report = Buffer.create 1024 in
  let loaded = ref 0 and failed = ref 0 and dirs_seen = ref 0 in
  Buffer.add_string report "sv_gui — script load report\n\n";
  List.iter (fun dir ->
    if Sys.file_exists dir && Sys.is_directory dir then begin
      incr dirs_seen;
      Buffer.add_string report ("[dir] " ^ dir ^ "\n");
      let files = Sys.readdir dir in
      Array.sort compare files;
      Array.iter (fun f ->
        if Filename.check_suffix f ".lua" then begin
          let p = Filename.concat dir f in
          match run_lua (read_file p) with
          | None ->
              incr loaded;
              Buffer.add_string report ("  ok    " ^ f ^ "\n")
          | Some err ->
              incr failed;
              Buffer.add_string report
                ("  FAIL  " ^ f ^ ":  " ^ err ^ "\n");
              Printf.eprintf "[sv_gui] %s: %s\n%!" p err
        end
      ) files
    end else
      Buffer.add_string report ("[skip] " ^ dir ^ " (not present)\n")
  ) (script_dirs ());
  if !dirs_seen = 0 then
    Buffer.add_string report
      "\nNo gui_scripts directory found. Drop *.lua into one of the\n\
       directories listed above (or set $SV_DECOMPILER_SCRIPTS).\n";
  Buffer.add_string report
    (Printf.sprintf "\n%d loaded, %d failed.\n" !loaded !failed);
  set_text (Buffer.contents report);
  set_status (Printf.sprintf "Lua: %d loaded, %d failed" !loaded !failed)

let clear_dynamic () =
  List.iter (fun w -> w#destroy ()) !dynamic_items;
  dynamic_items := []

let reload_scripts () =
  clear_dynamic ();
  load_scripts ()

(* ---------- hook installation ---------- *)

let install_hooks () =
  Sv_lua.gui_add_menu_hook    := add_lua_menu;
  Sv_lua.gui_add_item_hook    := add_lua_item;
  Sv_lua.gui_set_text_hook    := set_text;
  Sv_lua.gui_get_text_hook    := get_text;
  Sv_lua.gui_append_text_hook := append_text;
  Sv_lua.gui_message_hook     := info_dialog;
  Sv_lua.gui_error_hook       := error_dialog;
  Sv_lua.gui_open_file_hook   := open_file_dialog;
  Sv_lua.gui_save_file_hook   := save_file_dialog;
  Sv_lua.gui_set_status_hook  := set_status;
  Sv_lua.gui_quit_hook        := GMain.quit

(* ---------- main ---------- *)

let () =
  let _ = GMain.init () in
  let window = GWindow.window
    ~title:"sv_suite" ~width:1100 ~height:750 () in
  ignore (window#connect#destroy ~callback:GMain.quit);

  let vbox = GPack.vbox ~packing:window#add () in

  (* Menubar. *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let bar_factory = new GMenu.factory menubar in
  let accel_group = bar_factory#accel_group in
  window#add_accel_group accel_group;
  menubar_ref     := Some (menubar :> GMenu.menu_shell);
  accel_group_ref := Some accel_group;
  window_ref      := Some window;

  (* Default top-level menus, in display order. *)
  let mk name = let m = bar_factory#add_submenu name in
                Hashtbl.add menus name m; m in
  let file_menu      = mk "File" in
  let decompile_menu = mk "Decompile" in
  let verify_menu    = mk "Verify" in
  let topology_menu  = mk "Topology" in
  let schematic_menu = mk "Schematic" in
  let _scripts       = mk "Scripts" in   (* default landing for add_item *)
  let view_menu      = mk "View" in
  let help_menu      = mk "Help" in

  (* Centre: a single scrolled text view (BIR / log / script output). *)
  let scrolled = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(vbox#pack ~expand:true) () in
  let view = GText.view ~packing:scrolled#add () in
  view#misc#modify_font_by_name "Monospace 10";
  buffer_ref := Some view#buffer;

  (* Status bar. *)
  let status = GMisc.statusbar ~packing:vbox#pack () in
  let ctx = status#new_context ~name:"main" in
  status_ctx_ref := Some ctx;
  ignore (ctx#push "Ready");

  load_search_paths ();
  load_last_chooser_dir ();
  install_hooks ();

  (* File menu. *)
  let f = new GMenu.factory file_menu ~accel_group in
  (* "Open..." dispatches on WHAT THE FILE IS.  Dropping a DEF into the text
     buffer is not just unhelpful -- a layout DEF here runs to megabytes, so the
     window fills with coordinates and the viewer never appears.  A .def opens in
     the layout viewer; everything else is still text. *)
  ignore (f#add_item "Open..." ~key:GdkKeysyms._O
            ~callback:(fun () ->
              let p = open_file_dialog () in
              if p <> "" then
                let ext = String.lowercase_ascii (Filename.extension p) in
                if ext = ".def" then open_lef_def_path p
                else
                  try set_text (read_file p); set_status ("Loaded " ^ p)
                  with e -> error_dialog (Printexc.to_string e)));
  ignore (f#add_item "Save text..." ~key:GdkKeysyms._S
            ~callback:(fun () ->
              let p = save_file_dialog () in
              if p <> "" then
                try
                  let oc = open_out p in
                  output_string oc (get_text ());
                  close_out oc;
                  set_status ("Wrote " ^ p)
                with e -> error_dialog (Printexc.to_string e)));
  ignore (f#add_separator ());
  ignore (f#add_item "Quit" ~key:GdkKeysyms._Q ~callback:GMain.quit);

  (* Decompile menu — hardwired so the basic flow works without any Lua
     scripts loaded.                                                   *)
  let d = new GMenu.factory decompile_menu ~accel_group in
  ignore (d#add_item "Parse Verible..."        ~callback:(do_parse "verible"));
  ignore (d#add_item "Parse Verilator JSON..." ~callback:(do_parse "verilator"));
  ignore (d#add_item "Parse Slang..."          ~callback:(do_parse "slang"));
  ignore (d#add_item "Parse VHDL..."           ~callback:(do_parse "vhdl"));
  ignore (d#add_item "Parse Yosys/RTLIL..."    ~callback:(do_parse "yosys"));
  ignore (d#add_separator ());
  ignore (d#add_item "Optimise loaded BIR" ~callback:do_optim);
  ignore (d#add_item "Synthesise loaded BIR (gate mapping)"
            ~callback:do_synthesise);

  (* Verify menu. *)
  let m = new GMenu.factory verify_menu ~accel_group in
  ignore (m#add_item "Z3 miter: verible vs verilator..."
            ~callback:(do_miter "verible" "verilator"));
  ignore (m#add_item "Z3 miter: verible vs slang..."
            ~callback:(do_miter "verible" "slang"));
  ignore (m#add_item "Z3 miter: verilator vs slang..."
            ~callback:(do_miter "verilator" "slang"));
  ignore (m#add_item "Z3 miter: verible vs vhdl..."
            ~callback:(do_miter "verible" "vhdl"));
  if Sys.getenv_opt "SV_DECOMP_GUI_SIM" = Some "1" then begin
    ignore (m#add_separator ());
    ignore (m#add_item "Simulate top (Cyclesim, 128 cycles)..."
              ~callback:do_simulate)
  end;

  (* Topology menu. *)
  let t = new GMenu.factory topology_menu ~accel_group in
  ignore (t#add_item "Run ORFS layout..."  ~callback:do_orfs_run);
  ignore (t#add_item "Open ORFS run..."    ~callback:do_open_orfs_run);
  ignore (t#add_item "Open nextpnr placement/routing JSON..." ~callback:do_open_nextpnr_json);
  ignore (t#add_item "Open LEF/DEF layout..." ~callback:do_open_lef_def);

  (* Schematic menu. *)
  let s = new GMenu.factory schematic_menu ~accel_group in
  ignore (s#add_item "Show RTL schematic..."
            ~callback:(do_show_schematic ~gate_only:false));
  ignore (s#add_item "Show gate-level schematic..."
            ~callback:(do_show_schematic ~gate_only:true));
  ignore (s#add_separator ());
  ignore (s#add_item "Load .slib..."       ~callback:do_load_slib);

  (* DFT menu — scan-chain insert, boundary scan around macros + every
     hierarchical child, IEEE 1149.1 JTAG TAP + BSDL pad insertion at
     the chip top.  Toggles set the corresponding env vars at runtime;
     synth_pipeline reads them per-invocation.                          *)
  let dft_menu = bar_factory#add_submenu "DFT" in
  Hashtbl.add menus "DFT" dft_menu;
  let dft = new GMenu.factory dft_menu ~accel_group in
  let mk_env_toggle label var =
    let item = GMenu.check_menu_item ~label ~packing:dft_menu#append () in
    item#set_active (Sys.getenv_opt var = Some "1");
    ignore (item#connect#toggled ~callback:(fun () ->
      Unix.putenv var (if item#active then "1" else "0");
      set_status (Printf.sprintf "%s = %s" var
                    (if item#active then "1" else "0"))));
    item
  in
  ignore (mk_env_toggle "Scan-chain insert (SDFFs + chain)" "SV_DECOMP_SCAN");
  ignore (mk_env_toggle "Memory boundary scan (around macros)"
            "SV_DECOMP_MEM_BSR");
  ignore (mk_env_toggle "Hier boundary scan (around every child)"
            "SV_DECOMP_HIER_BSR");
  ignore (mk_env_toggle "JTAG TAP + BSDL pads at chip top"
            "SV_DECOMP_JTAG");
  ignore (dft#add_separator ());
  ignore (dft#add_item "Run ATPG..." ~callback:do_atpg);
  ignore (dft#add_item "Show BSDL..." ~callback:do_show_bsdl);

  (* View menu. *)
  let v = new GMenu.factory view_menu ~accel_group in
  ignore (v#add_item "Reload scripts" ~key:GdkKeysyms._R
            ~callback:reload_scripts);
  ignore (v#add_item "Clear text"
            ~callback:(fun () -> set_text ""));

  (* Help menu. *)
  let h = new GMenu.factory help_menu ~accel_group in
  ignore (h#add_item "About"
            ~callback:(fun () ->
              info_dialog (
                "sv_suite — GUI shell\n\n\
                 Lua scripts under ./scripts and \
                 $XDG_CONFIG_HOME/sv_suite/scripts/\n\
                 are loaded at startup. They can register menu items by \
                 calling\n\
                 gui.add_menu(name) and gui.add_item(menu, label, handler) \
                 with handler being a Lua global function name.\n\n\
                 View → Reload scripts (Ctrl-R) reloads them.")));

  load_scripts ();
  window#show ();

  (* A .def named on the command line opens straight into the layout viewer.
     This is also the only way to exercise the LEF/DEF reader without a human
     driving the file chooser, so it is what the headless smoke test uses. *)
  List.iter
    (fun a ->
       if Filename.check_suffix a ".def" then open_lef_def_path a)
    (List.tl (Array.to_list Sys.argv));
  if Sys.getenv_opt "SV_GUI_EXIT_AFTER_LOAD" <> None then exit 0;

  GMain.main ()
