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

let chooser_dialog action title =
  let d = GWindow.file_chooser_dialog
    ~action ~title ~parent:(need_window ()) ~modal:true () in
  d#add_button "Cancel" `CANCEL;
  d#add_button (match action with `OPEN -> "Open" | `SAVE -> "Save" | _ -> "OK")
    `OK;
  let result = match d#run () with
    | `OK -> (match d#filename with Some f -> f | None -> "")
    | _   -> ""
  in
  d#destroy (); result

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
    let p =
      match fe with
      | "verible" -> Verible_to_behavioral.convert_files_all [path]
      | "vhdl" | "verilator" ->
          Sv_lua.load_frontend ~frontend:fe ~top:"" ~files:[path]
      | _ ->
          (* slang / yosys driver requires --top; seed it from the source
             so the driver elaborates SOMETHING, then dump every module
             the driver emitted (slang's JSON contains the full design;
             yosys hierarchy is rooted but other modules survive when
             present in the file). *)
          let top = derive_top ~frontend:fe path in
          Sv_lua.load_frontend ~frontend:fe ~top ~files:[path]
    in
    current_prog := Some (path, p);
    let names = List.map (fun (m : Behavioral_ir.bmodule) -> m.name)
                  p.modules in
    dump_prog ~banner:(Printf.sprintf
      "// %s — %d module(s) from %s\n// modules: %s\n\n"
      fe (List.length p.modules) path
      (String.concat ", " names)) p;
    set_status (Printf.sprintf "%s: %d module(s) loaded" fe
                  (List.length p.modules)))

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

(* ---------- ORFS handholding (Phase 1: launch + log streaming) ----------
   The GUI doesn't reimplement OpenROAD; it just writes a config.mk + sdc
   with sensible defaults and shells out to ORFS's make.  Output streams
   into the centre text view via a Glib IO watch so the GUI stays
   responsive while ORFS runs (5–30 min for nangate45 designs).        *)

type orfs_cfg = {
  o_top      : string;
  o_file     : string;
  o_platform : string;
  o_freq_ghz : float;
  o_util     : int;
  o_workdir  : string;
}

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
    Printf.sprintf
      "current_design %s\n\
       create_clock -name clk -period %.3f [get_ports clk]\n\
       set non_clk [remove_from_collection [all_inputs] [get_ports clk]]\n\
       set_input_delay  %.3f -clock clk $non_clk\n\
       set_output_delay %.3f -clock clk [all_outputs]\n"
      cfg.o_top period_ns io_delay io_delay
  in
  let sdc_path = Filename.concat cfg.o_workdir "constraint.sdc" in
  write_file sdc_path sdc;
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
       export TNS_END_PERCENT        = 100\n"
      cfg.o_top cfg.o_top cfg.o_platform cfg.o_file sdc_path cfg.o_util
  in
  let mk_path = Filename.concat cfg.o_workdir "config.mk" in
  write_file mk_path mk;
  (sdc_path, mk_path)

let spawn_orfs cfg =
  let _, mk_path = write_orfs_files cfg in
  let flow_dir = Filename.concat (orfs_dir ()) "flow" in
  set_text "";
  append_text (Printf.sprintf
    "[orfs] workdir=%s\n[orfs] config.mk=%s\n[orfs] flow=%s\n"
    cfg.o_workdir mk_path flow_dir);
  append_text (Printf.sprintf "[orfs] running: make -C %s DESIGN_CONFIG=%s\n\n"
                 flow_dir mk_path);
  set_status (Printf.sprintf "ORFS: %s on %s — running" cfg.o_top cfg.o_platform);

  let r, w = Unix.pipe () in
  let pid =
    try
      Unix.create_process "make"
        [| "make"; "-C"; flow_dir; "DESIGN_CONFIG=" ^ mk_path |]
        Unix.stdin w w
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
        let top = match p.modules with
          | (m : Behavioral_ir.bmodule) :: _ -> m.name
          | [] -> Filename.chop_extension (Filename.basename path)
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
           o_top      = top_e#text;
           o_file     = file_e#text;
           o_platform = (try List.nth platforms combo#active
                         with _ -> "nangate45");
           o_freq_ghz = (try float_of_string freq_e#text with _ -> 1.0);
           o_util     = (try int_of_string util_e#text with _ -> 30);
           o_workdir  = workdir_e#text;
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
      if cfg.o_top = "" || cfg.o_file = "" then
        error_dialog "Top and Verilog file are required."
      else if not (Sys.file_exists cfg.o_file) then
        error_dialog ("Verilog file not found:\n" ^ cfg.o_file)
      else if not (Sys.file_exists (orfs_dir ())) then
        error_dialog (Printf.sprintf
          "ORFS install not found at %s.\nSet $ORFS_DIR or install at \
           $HOME/OpenROAD-flow-scripts." (orfs_dir ()))
      else
        try spawn_orfs cfg with Exit -> ()

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
let def_comp_re    =
  Str.regexp
    "[ \t]*-[ \t]+\\([^ \t]+\\)[ \t]+\\([^ \t]+\\).*PLACED[ \t]+\
     ([ \t]*\\(-?[0-9]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*)[ \t]+\
     \\([NSEW][NSEWF]*\\)"

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
  (* Two columns of floats, then ^ or v, then inst/pin (cell). *)
  Str.regexp
    "^[ \t]*\\([0-9.]+\\)[ \t]+\\([0-9.]+\\)[ \t]+[v^][ \t]+\
     \\([^ \t/]+\\)/\\([^ \t]+\\)[ \t]+(\\([^ \t)]+\\))"

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
         let pts = List.filter_map (fun (h : timing_hop) ->
           match Hashtbl.find_opt idx h.t_inst with
           | None -> None
           | Some pl ->
               let w_um, h_um =
                 try Hashtbl.find layout.l_macro_um pl.p_cell
                 with Not_found -> (0.5, 1.4) in
               let cx_dbu = pl.p_x + int_of_float (w_um *. units_f /. 2.0) in
               let cy_dbu = pl.p_y + int_of_float (h_um *. units_f /. 2.0) in
               Some (xform cx_dbu cy_dbu)
         ) p.tp_hops in
         (match pts with
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

let do_open_orfs_run () =
  with_errors "open ORFS run" (fun () ->
    let dir = pick_results_dir () in
    if dir = "" then () else begin
      let def_path = Filename.concat dir "6_final.def" in
      if not (Sys.file_exists def_path) then
        error_dialog ("No 6_final.def found in:\n" ^ dir)
      else begin
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
        set_status (Printf.sprintf "Loading %s (LEF count: %d) …"
                      def_path (List.length lefs));
        let layout = load_layout def_path lefs in
        (* Timing report sits in the parallel reports/ tree:
           flow/results/<plat>/<des>/base/  ↔  flow/reports/<plat>/<des>/base/
           ORFS's final STA is "6_finish.rpt".                         *)
        let reports_dir =
          Filename.concat flow
            (Filename.concat "reports"
               (Filename.concat platform
                  (Filename.concat layout.l_design "base"))) in
        let rpt_path = Filename.concat reports_dir "6_finish.rpt" in
        let critical_path =
          if Sys.file_exists rpt_path
          then worst_max_path (parse_timing_report rpt_path)
          else None
        in
        open_layout_window ?critical_path layout;
        let extra = match critical_path with
          | Some _ -> " + critical path"
          | None -> ""
        in
        set_status (Printf.sprintf
          "Opened layout %s — %d instances%s"
          layout.l_design (List.length layout.l_components) extra)
      end
    end)

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

  (* Topology menu. *)
  let t = new GMenu.factory topology_menu ~accel_group in
  ignore (t#add_item "Run ORFS layout..."  ~callback:do_orfs_run);
  ignore (t#add_item "Open ORFS run..."    ~callback:do_open_orfs_run);

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
