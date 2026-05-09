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
