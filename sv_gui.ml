(* sv_gui.ml — lablgtk3 shell where Lua scripts are first-class menu items.

   Each *.lua under ./scripts and $XDG_CONFIG_HOME/sv_decompiler/scripts is
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
   $XDG_CONFIG_HOME/sv_decompiler/last_dir.txt; loaded at startup
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
  Filename.concat xdg "sv_decompiler/last_dir.txt"

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
   path persisted under $XDG_CONFIG_HOME/sv_decompiler/.            *)

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
  Filename.concat xdg "sv_decompiler/search_paths.txt"

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
      "# sv_decompiler search paths (one absolute dir per line)\n";
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
    Filename.concat home (Filename.concat "sv_decompiler_orfs" stem)
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
        (Filename.concat "sv_decompiler_orfs" stem) in
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

type placed = {
  p_inst   : string;
  p_cell   : string;
  p_x      : int;       (* DBU *)
  p_y      : int;
  p_orient : string;
}

type layout = {
  l_design     : string;
  l_units      : int;                                 (* DBU per μm *)
  l_die        : int * int * int * int;               (* x1 y1 x2 y2 in DBU *)
  l_components : placed list;
  l_macro_um   : (string, float * float) Hashtbl.t;   (* cell → (w,h) μm *)
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
let def_comp_re    =
  Str.regexp
    "[ \t]*-[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\).*PLACED[ \t]+\
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
      comps := { p_inst   = Str.matched_group 1 line
               ; p_cell   = Str.matched_group 2 line
               ; p_x      = int_of_string (Str.matched_group 3 line)
               ; p_y      = int_of_string (Str.matched_group 4 line)
               ; p_orient = Str.matched_group 5 line } :: !comps
  ) lines;
  {
    l_design     = !design;
    l_units      = !units;
    l_die        = !die;
    l_components = List.rev !comps;
    l_macro_um   = Hashtbl.create 256;
  }

let lef_macro_re = Str.regexp "[ \t]*MACRO[ \t]+\\([^ \t]+\\)"
let lef_size_re  =
  Str.regexp "[ \t]*SIZE[ \t]+\\([0-9.]+\\)[ \t]+BY[ \t]+\\([0-9.]+\\)"

let parse_lef_into tbl path =
  let lines = layout_lines path in
  let cur = ref "" in
  List.iter (fun line ->
    if Str.string_match lef_macro_re line 0
    then cur := Str.matched_group 1 line
    else if !cur <> "" && Str.string_match lef_size_re line 0 then begin
      let w = float_of_string (Str.matched_group 1 line) in
      let h = float_of_string (Str.matched_group 2 line) in
      Hashtbl.replace tbl !cur (w, h)
    end
  ) lines

let load_layout def_path lef_paths =
  let l = parse_def def_path in
  List.iter (fun p ->
    try parse_lef_into l.l_macro_um p
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
    (* Die outline *)
    let (dx0, dy0) = xform x1 y2 in
    let (dx1, dy1) = xform x2 y1 in
    Cairo.set_source_rgb cr 0.0 0.0 0.0;
    Cairo.set_line_width cr 1.5;
    Cairo.rectangle cr dx0 dy0 ~w:(dx1 -. dx0) ~h:(dy1 -. dy0);
    Cairo.stroke cr;
    (* Instances — fill mostly transparent so dense regions read as colour
       intensity. *)
    Cairo.set_source_rgba cr 0.30 0.50 0.80 0.45;
    let units_f = float_of_int layout.l_units in
    List.iter (fun p ->
      let w_um, h_um =
        try Hashtbl.find layout.l_macro_um p.p_cell
        with Not_found -> (0.5, 1.4)         (* unknown cell fallback *)
      in
      let rotated = match p.p_orient with
        | "E" | "W" | "FE" | "FW" -> true
        | _ -> false
      in
      let w_um, h_um = if rotated then (h_um, w_um) else (w_um, h_um) in
      let w_dbu = w_um *. units_f in
      let h_dbu = h_um *. units_f in
      let (rx, ry) = xform p.p_x (p.p_y + int_of_float h_dbu) in
      Cairo.rectangle cr rx ry ~w:(w_dbu *. scale) ~h:(h_dbu *. scale)
    ) layout.l_components;
    Cairo.fill cr;
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
  ignore (da#misc#connect#draw ~callback:(fun cr ->
    let alloc = da#misc#allocation in
    (try
       render_layout cr
         ~width:alloc.Gtk.width ~height:alloc.Gtk.height
         ?path:!path_ref layout
     with e ->
       Printf.eprintf "[layout] render failed: %s\n%!"
         (Printexc.to_string e));
    true));

  (* Right pane: critical path side panel. *)
  let side = GPack.vbox ~spacing:4 ~border_width:6 () in
  hpane#pack2 ~resize:false ~shrink:true side#coerce;
  hpane#set_position 800;
  let header_text = match !path_ref with
    | Some p -> Printf.sprintf
        "Critical path (worst max-delay)\n%s → %s\n%d hop(s) shown"
        p.tp_startpoint p.tp_endpoint (List.length p.tp_hops)
    | None ->
        "No 6_finish.rpt found alongside this DEF\n\
         (or no max-delay path inside it)"
  in
  let header = GMisc.label ~text:header_text
    ~xalign:0.0 ~justify:`LEFT ~packing:side#pack () in
  header#set_line_wrap true;
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
        param_values = [];
        port_connections =
          List.map (fun (pc : Lib_map.pin_conn) ->
            (pc.pin, Behavioral_ir.BVar pc.net)) i.conns }
    ) mn.mn_netlist.insts in
    let child_insts = List.map (fun (ci : Hier_synth.child_inst_emit) ->
      { Behavioral_ir.inst_name = ci.ci_inst;
        module_name = ci.ci_module;
        param_values = [];
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
  let user = [ Filename.concat xdg "sv_decompiler/scripts" ] in
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
    ~title:"sv_decompiler" ~width:1100 ~height:750 () in
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
  ignore (f#add_item "Open..." ~key:GdkKeysyms._O
            ~callback:(fun () ->
              let p = open_file_dialog () in
              if p <> "" then
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
                "sv_decompiler — GUI shell\n\n\
                 Lua scripts under ./scripts and \
                 $XDG_CONFIG_HOME/sv_decompiler/scripts/\n\
                 are loaded at startup. They can register menu items by \
                 calling\n\
                 gui.add_menu(name) and gui.add_item(menu, label, handler) \
                 with handler being a Lua global function name.\n\n\
                 View → Reload scripts (Ctrl-R) reloads them.")));

  load_scripts ();
  window#show ();
  GMain.main ()
