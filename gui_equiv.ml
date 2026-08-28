(* gui_equiv.ml — the Z3 equivalence workbench window.
 *
 * Four pages, in the order the work actually happens:
 *
 *   1 · Designs      load A (spec) and B (impl): frontend, files, top,
 *                    optional lowering pass.
 *   2 · Registers    match the two register spaces.  The miter ties state BY
 *                    NAME, so this page is what makes the tool work against a
 *                    synthesised netlist at all — and its manual overrides are
 *                    persisted, so a matching is done once, not once per run.
 *   3 · Miter        run it; verdict shown NEXT TO the census of what was
 *                    proved over, so a vacuous pass cannot masquerade as a
 *                    real one.
 *   4 · Counterexample   solve one failing cone and walk back through the
 *                    partial netlist to the first divergence.
 *
 * All the thinking is in equiv_core.ml; this file is widgets and glue.  A
 * verdict shown here is reproducible with `sv_suite equiv <project.json>`.
 *)

module E = Equiv_core

type callbacks = {
  cb_choose_files : unit -> string list;   (* multi-select open *)
  cb_choose_open  : unit -> string;
  cb_choose_save  : unit -> string;
  cb_status       : string -> unit;
  cb_error        : string -> unit;
}

type st = {
  mutable a_spec    : E.side_spec;
  mutable b_spec    : E.side_spec;
  mutable a         : E.side option;
  mutable b         : E.side option;
  mutable pairs     : E.pair list;
  mutable b_left    : E.reg list;
  mutable stale     : (string * string) list;
  mutable overrides : E.overrides;
  mutable mode      : E.mode;
  mutable timeout   : int;
  mutable run       : E.run_result option;
  mutable cones     : string list;
  mutable ce        : E.ce option;
  mutable project   : string option;
}

(* Keep the window responsive across a long solve without threads: the engine
   is synchronous, so pump the event loop at each step boundary.  This is
   honest about what it is — the window does freeze inside Z3 — but it means
   progress lines appear as they happen rather than all at once at the end. *)
let pump () = while Glib.Main.iteration false do () done

let hex = E.hex

(* ────────────────────────────────────────────────────────────── *)

(* [show_tools] opens the tool picker straight away — used by the headless
   smoke test (SV_GUI_EQUIV=tools), so the dialog is exercised too rather than
   only the pages behind it. *)
let open_window ?(show_tools = false) (cb : callbacks) () =
  let st = { a_spec = E.default_spec "A"; b_spec = E.default_spec "B";
             a = None; b = None; pairs = []; b_left = []; stale = [];
             overrides = [];
             mode = E.Flat; timeout = 30000; run = None; cones = [];
             ce = None; project = None } in

  let win = GWindow.window ~title:"Equivalence workbench (Z3)"
              ~width:1200 ~height:820 () in
  let outer = GPack.vbox ~packing:win#add () in

  (* toolbar: project load/save *)
  let bar = GPack.hbox ~spacing:6 ~border_width:4 ~packing:outer#pack () in
  let proj_label = GMisc.label ~xalign:0.0 ~text:"project: (unsaved)"
                     ~packing:(bar#pack ~expand:true) () in

  let nb = GPack.notebook ~packing:(outer#pack ~expand:true) () in

  (* log pane, shared by every page — the engine's own report lands here *)
  let log_frame = GBin.frame ~label:"Engine output" ~height:190
                    ~packing:outer#pack () in
  let log_sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                 ~packing:log_frame#add () in
  let log_view = GText.view ~editable:false ~packing:log_sw#add () in
  log_view#misc#modify_font_by_name "Monospace 9";
  let log_buf = log_view#buffer in
  let log s =
    log_buf#insert ~iter:log_buf#end_iter s;
    let m = log_buf#create_mark log_buf#end_iter in
    log_view#scroll_to_mark ~use_align:false (`MARK m);
    pump () in
  let logf fmt = Printf.ksprintf log fmt in

  let status s = cb.cb_status s; logf "%s\n" s in

  (* ══════════ page 1 · Designs ══════════ *)

  let page1 = GPack.vbox ~spacing:8 ~border_width:8 () in
  ignore (nb#append_page ~tab_label:(GMisc.label ~text:"1 · Designs" ())#coerce
            page1#coerce);
  ignore (GMisc.label ~xalign:0.0 ~packing:page1#pack
            ~markup:"<b>A</b> is the reference (RTL / golden); <b>B</b> is the \
                     implementation (post-synthesis netlist, other flow). \
                     Each side gets its own frontend — that is where the \
                     synthesis tool is chosen." ());
  let sides_box = GPack.hbox ~spacing:8 ~packing:(page1#pack ~expand:true) () in

  (* one side panel; returns a refresh function *)
  let side_panel ~title ~(get : unit -> E.side_spec) ~(set : E.side_spec -> unit)
                 ~(loaded : unit -> E.side option) ~(store : E.side option -> unit) =
    let fr = GBin.frame ~label:title ~packing:(sides_box#pack ~expand:true) () in
    let v = GPack.vbox ~spacing:6 ~border_width:6 ~packing:fr#add () in

    let row1 = GPack.hbox ~spacing:6 ~packing:v#pack () in
    ignore (GMisc.label ~text:"Frontend / synthesis:" ~packing:row1#pack ());
    (* Only frontends whose tool was FOUND are offered, and each is labelled
       with the binary that will run — "yosys" and "the yosys in a checkout
       you forgot about" are different tools, and the difference surfaces as a
       mysterious verdict.  [fe_names] tracks the combo rows so the label the
       user sees maps back to the frontend name. *)
    let fe = GEdit.combo_box_text ~strings:[] ~packing:row1#pack () in
    let fe_names : string list ref = ref [] in
    let fill_frontends ?(keep = "") () =
      let sts = Tool_scan.get () in
      let usable = List.filter (Tool_scan.usable sts) sts in
      let (fstore_c, _) = snd fe in
      fstore_c#clear ();
      fe_names := List.map (fun st -> st.Tool_scan.st_spec.Tool_scan.fe) usable;
      List.iter (fun st -> GEdit.text_combo_add fe (Tool_scan.label st)) usable;
      let rec idx i = function
        | [] -> 0
        | x :: t -> if x = keep then i else idx (i + 1) t in
      (fst fe)#set_active (if !fe_names = [] then -1 else idx 0 !fe_names) in
    let selected_frontend () =
      let i = (fst fe)#active in
      if i >= 0 && i < List.length !fe_names then List.nth !fe_names i
      else "verible" in
    fill_frontends ();
    ignore (GMisc.label ~text:"  Lowering:" ~packing:row1#pack ());
    let post = GEdit.combo_box_text ~strings:E.post_passes ~packing:row1#pack () in
    (fst post)#set_active 0;

    let row2 = GPack.hbox ~spacing:6 ~packing:v#pack () in
    ignore (GMisc.label ~text:"Top module:" ~packing:row2#pack ());
    let top_e = GEdit.entry ~packing:(row2#pack ~expand:true) () in

    let cols = new GTree.column_list in
    let c_file = cols#add Gobject.Data.string in
    let fstore = GTree.list_store cols in
    let fsw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                ~height:150 ~packing:(v#pack ~expand:true) () in
    let fview = GTree.view ~model:fstore ~packing:fsw#add () in
    let r = GTree.cell_renderer_text [] in
    ignore (fview#append_column
              (GTree.view_column ~title:"Source files" ~renderer:(r, [ "text", c_file ]) ()));

    let sync_files () =
      let acc = ref [] in
      fstore#foreach (fun _ it -> acc := fstore#get ~row:it ~column:c_file :: !acc; false);
      set { (get ()) with E.s_files = List.rev !acc } in

    let row3 = GPack.hbox ~spacing:6 ~packing:v#pack () in
    let b_add = GButton.button ~label:"Add files…" ~packing:row3#pack () in
    let b_del = GButton.button ~label:"Remove" ~packing:row3#pack () in
    let b_load = GButton.button ~label:"Load" ~packing:row3#pack () in
    let summary = GMisc.label ~xalign:0.0 ~text:"not loaded"
                    ~packing:(v#pack ~expand:false) () in

    ignore (b_add#connect#clicked ~callback:(fun () ->
      List.iter (fun f ->
        let it = fstore#append () in
        fstore#set ~row:it ~column:c_file f) (cb.cb_choose_files ());
      sync_files ();
      (* a bare top is the commonest omission: guess it from the first file *)
      if top_e#text = "" then
        match (get ()).E.s_files with
        | f :: _ -> top_e#set_text (Filename.remove_extension (Filename.basename f))
        | [] -> ()));
    ignore (b_del#connect#clicked ~callback:(fun () ->
      match fview#selection#get_selected_rows with
      | [] -> ()
      | p :: _ -> ignore (fstore#remove (fstore#get_iter p)); sync_files ()));

    let refresh_summary () =
      match loaded () with
      | None -> summary#set_text "not loaded"
      | Some s ->
          let regs = List.length (E.regs_of s.E.ripped) in
          let ins = List.length (Z3_miter.get_input_signals s.E.ripped) in
          let outs = List.length (Z3_miter.get_output_signals s.E.ripped) in
          summary#set_text (Printf.sprintf
            "%s: %d module(s) → top %s — %d registers, %d free inputs, %d compared outputs"
            s.E.sp.E.s_frontend (List.length s.E.prog.Behavioral_ir.modules)
            s.E.picked.Behavioral_ir.name regs ins outs) in

    ignore (b_load#connect#clicked ~callback:(fun () ->
      sync_files ();
      let sp = { (get ()) with
                 E.s_frontend = selected_frontend ();
                 E.s_post = (match GEdit.text_combo_get_active post with
                             | Some s -> s | None -> "none");
                 E.s_top = top_e#text } in
      set sp;
      status (Printf.sprintf "%s: loading %s with %s…" sp.E.s_tag sp.E.s_top
                sp.E.s_frontend);
      let (res, out) = E.capture (fun () -> E.load_side sp) in
      if out <> "" then log out;
      match res with
      | Ok s ->
          store (Some s);
          refresh_summary ();
          status (Printf.sprintf "%s loaded: %s" sp.E.s_tag s.E.picked.Behavioral_ir.name)
      | Error e ->
          store None; refresh_summary ();
          cb.cb_error (Printf.sprintf "%s: %s" sp.E.s_tag (Printexc.to_string e));
          status (Printf.sprintf "%s FAILED to load" sp.E.s_tag)));

    (* push a spec (e.g. loaded from a project file) into the widgets *)
    let set_widgets (sp : E.side_spec) =
      let idx l x = let rec go i = function
        | [] -> 0 | y :: t -> if y = x then i else go (i + 1) t in go 0 l in
      fill_frontends ~keep:sp.E.s_frontend ();
      if not (List.mem sp.E.s_frontend !fe_names) then
        (* the project names a frontend whose tool is missing HERE — say so
           rather than silently loading it with a different one *)
        cb.cb_error (Printf.sprintf
          "This project's %s side uses the '%s' frontend, whose tool is not \
           available on this machine.  Tools… lets you point at the binary."
          sp.E.s_tag sp.E.s_frontend);
      (fst post)#set_active (idx E.post_passes sp.E.s_post);
      top_e#set_text sp.E.s_top;
      fstore#clear ();
      List.iter (fun f ->
        let it = fstore#append () in fstore#set ~row:it ~column:c_file f)
        sp.E.s_files in
    (set_widgets, refresh_summary, fun () -> fill_frontends ~keep:(selected_frontend ()) ())
  in

  let (set_a_widgets, refresh_a, refill_a) =
    side_panel ~title:"A — reference"
      ~get:(fun () -> st.a_spec) ~set:(fun s -> st.a_spec <- s)
      ~loaded:(fun () -> st.a) ~store:(fun s -> st.a <- s) in
  let (set_b_widgets, refresh_b, refill_b) =
    side_panel ~title:"B — implementation"
      ~get:(fun () -> st.b_spec) ~set:(fun s -> st.b_spec <- s)
      ~loaded:(fun () -> st.b) ~store:(fun s -> st.b <- s) in

  (* ══════════ page 2 · Registers ══════════ *)

  let page2 = GPack.vbox ~spacing:8 ~border_width:8 () in
  ignore (nb#append_page ~tab_label:(GMisc.label ~text:"2 · Registers" ())#coerce
            page2#coerce);
  ignore (GMisc.label ~xalign:0.0 ~packing:page2#pack
            ~markup:"The miter lifts every flop to a free state variable and \
                     ties the two sides <b>by name</b>. Unmatched registers are \
                     not compared — they are a hole in the proof, not a detail." ());

  let mrow = GPack.hbox ~spacing:6 ~packing:page2#pack () in
  let b_match = GButton.button ~label:"Match registers" ~packing:mrow#pack () in
  let sim_chk = GButton.check_button ~label:"use simulation signatures (slower, \
                                             survives renaming)" ~packing:mrow#pack () in
  sim_chk#set_active true;
  let match_sum = GMisc.label ~xalign:0.0 ~text:"" ~packing:(mrow#pack ~expand:true) () in

  let pcols = new GTree.column_list in
  let pc_a = pcols#add Gobject.Data.string in
  let pc_aw = pcols#add Gobject.Data.string in
  let pc_b = pcols#add Gobject.Data.string in
  let pc_bw = pcols#add Gobject.Data.string in
  let pc_m = pcols#add Gobject.Data.string in
  (* Empty string = "leave the theme's colour alone".  Binding "foreground"
     straight to the column would make GTK complain "Don't know color ''" for
     every uncoloured row, so the colour is applied by a cell data function
     that clears FOREGROUND_SET instead — which also keeps the default
     readable under a dark theme. *)
  let pc_fg = pcols#add Gobject.Data.string in
  let pstore = GTree.list_store pcols in
  let psw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
              ~packing:(page2#pack ~expand:true) () in
  let pview = GTree.view ~model:pstore ~packing:psw#add () in
  let add_col view title col =
    let r = GTree.cell_renderer_text [] in
    let vc = GTree.view_column ~title ~renderer:(r, [ "text", col ]) () in
    vc#set_cell_data_func r (fun model iter ->
      match model#get ~row:iter ~column:pc_fg with
      | "" -> r#set_properties [ `FOREGROUND_SET false ]
      | c -> r#set_properties [ `FOREGROUND c ]);
    vc#set_resizable true; ignore (view#append_column vc) in
  add_col pview "A register" pc_a;
  add_col pview "w" pc_aw;
  add_col pview "B register" pc_b;
  add_col pview "w" pc_bw;
  add_col pview "matched by" pc_m;

  (* B-side leftovers: the pool a manual override draws from *)
  let brow = GPack.hbox ~spacing:6 ~packing:page2#pack () in
  ignore (GMisc.label ~text:"Manual override — B register:" ~packing:brow#pack ());
  let b_combo = GEdit.combo_box_text ~strings:[] ~packing:brow#pack () in
  let b_pin = GButton.button ~label:"Pin to selected A" ~packing:brow#pack () in
  let b_unmatch = GButton.button ~label:"Force unmatched" ~packing:brow#pack () in
  let b_clear = GButton.button ~label:"Clear overrides" ~packing:brow#pack () in
  let unmatched_lbl = GMisc.label ~xalign:0.0 ~text:"" ~packing:(brow#pack ~expand:true) () in

  let fill_pairs () =
    pstore#clear ();
    List.iter (fun (p : E.pair) ->
      let it = pstore#append () in
      pstore#set ~row:it ~column:pc_a p.E.p_a;
      pstore#set ~row:it ~column:pc_aw (string_of_int p.E.p_a_w);
      pstore#set ~row:it ~column:pc_b
        (match p.E.p_b with Some b -> b | None -> "—");
      pstore#set ~row:it ~column:pc_bw
        (if p.E.p_b = None then "" else string_of_int p.E.p_b_w);
      pstore#set ~row:it ~column:pc_m (E.meth_str p.E.p_meth);
      let colour = match p.E.p_meth with
        | E.Unmatched -> Some "#c02020"
        | E.Forced_unmatched -> Some "#a06000"
        | E.Manual -> Some "#0060c0"
        | E.Sim -> Some "#207020"
        | _ -> None in
      pstore#set ~row:it ~column:pc_fg
        (match colour with Some c -> c | None -> "")) st.pairs;
    (* width mismatch is worth seeing: it usually means the pair is wrong *)
    let (store_b, col_b) = snd b_combo in
    store_b#clear ();
    List.iter (fun (r : E.reg) -> GEdit.text_combo_add b_combo r.E.r_base) st.b_left;
    ignore col_b;
    let n_un = List.length (List.filter (fun p -> p.E.p_b = None) st.pairs) in
    unmatched_lbl#set_text
      (Printf.sprintf "%d A unmatched, %d B unmatched%s" n_un (List.length st.b_left)
         (if st.stale = [] then ""
          else Printf.sprintf "  ⚠ %d saved override(s) no longer apply"
                 (List.length st.stale)));
    let by m = List.length (List.filter (fun (x : E.pair) -> x.E.p_meth = m) st.pairs) in
    match_sum#set_text (Printf.sprintf
      "%d matched of %d A-registers  (name %d, canonical %d, simulation %d, manual %d)"
      (List.length (List.filter (fun (p : E.pair) -> p.E.p_b <> None) st.pairs))
      (List.length st.pairs) (by E.Exact) (by E.Canon) (by E.Sim) (by E.Manual)) in

  let do_match () =
    match st.a, st.b with
    | Some a, Some b ->
        status "matching register spaces…";
        let (res, out) = E.capture (fun () ->
          E.match_registers ~use_sim:sim_chk#active ~overrides:st.overrides a b) in
        if out <> "" then log out;
        (match res with
         | Ok (pairs, left, stale) ->
             st.pairs <- pairs; st.b_left <- left; st.stale <- stale;
             List.iter (fun (an, bn) ->
               logf "⚠ override %s → %s could not be applied (no such register)\n"
                 an bn) stale;
             fill_pairs ();
             status (Printf.sprintf "matched %d of %d registers"
                       (List.length (List.filter (fun (p : E.pair) -> p.E.p_b <> None) pairs))
                       (List.length pairs))
         | Error e -> cb.cb_error (Printexc.to_string e))
    | _ -> cb.cb_error "Load both sides on page 1 first." in

  ignore (b_match#connect#clicked ~callback:do_match);

  let selected_a () =
    match pview#selection#get_selected_rows with
    | [] -> None
    | p :: _ -> Some (pstore#get ~row:(pstore#get_iter p) ~column:pc_a) in

  ignore (b_pin#connect#clicked ~callback:(fun () ->
    match selected_a (), GEdit.text_combo_get_active b_combo with
    | Some an, Some bn ->
        st.overrides <- (an, Some bn) :: List.remove_assoc an st.overrides;
        do_match ()
    | _ -> cb.cb_error "Select an A register in the table and a B register in \
                        the drop-down."));
  ignore (b_unmatch#connect#clicked ~callback:(fun () ->
    match selected_a () with
    | Some an ->
        st.overrides <- (an, None) :: List.remove_assoc an st.overrides;
        do_match ()
    | None -> cb.cb_error "Select an A register first."));
  ignore (b_clear#connect#clicked ~callback:(fun () ->
    st.overrides <- []; do_match ()));

  (* ══════════ page 3 · Miter ══════════ *)

  let page3 = GPack.vbox ~spacing:8 ~border_width:8 () in
  ignore (nb#append_page ~tab_label:(GMisc.label ~text:"3 · Miter" ())#coerce
            page3#coerce);
  let rrow = GPack.hbox ~spacing:6 ~packing:page3#pack () in
  ignore (GMisc.label ~text:"Mode:" ~packing:rrow#pack ());
  let mode_combo = GEdit.combo_box_text
      ~strings:[ "flat (whole module at once)";
                 "per-cone (localise which outputs differ)";
                 "bottom-up hierarchical (children black-boxed)" ]
      ~packing:rrow#pack () in
  (fst mode_combo)#set_active 0;
  ignore (GMisc.label ~text:"  Z3 timeout (ms):" ~packing:rrow#pack ());
  let tmo = GEdit.entry ~text:"30000" ~width:80 ~packing:rrow#pack () in
  let b_run = GButton.button ~label:"Run miter" ~packing:rrow#pack () in
  let b_save_rep = GButton.button ~label:"Save report…" ~packing:rrow#pack () in

  let verdict_lbl = GMisc.label ~xalign:0.0 ~markup:"<b>no run yet</b>"
                      ~packing:page3#pack () in
  let census_sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                    ~packing:(page3#pack ~expand:true) () in
  let census_view = GText.view ~editable:false ~packing:census_sw#add () in
  census_view#misc#modify_font_by_name "Monospace 10";

  (* ══════════ page 4 · Counterexample ══════════ *)

  let page4 = GPack.vbox ~spacing:8 ~border_width:8 () in
  ignore (nb#append_page
            ~tab_label:(GMisc.label ~text:"4 · Counterexample" ())#coerce
            page4#coerce);
  let crow = GPack.hbox ~spacing:6 ~packing:page4#pack () in
  let b_scan = GButton.button ~label:"Find differing cones" ~packing:crow#pack () in
  ignore (GMisc.label ~text:"  Cone:" ~packing:crow#pack ());
  let cone_combo = GEdit.combo_box_text ~strings:[] ~packing:crow#pack () in
  let b_explain = GButton.button ~label:"Explain counterexample"
                    ~packing:crow#pack () in
  let cone_lbl = GMisc.label ~xalign:0.0 ~text:"" ~packing:(crow#pack ~expand:true) () in

  let paned = GPack.paned `VERTICAL ~packing:(page4#pack ~expand:true) () in
  let dcols = new GTree.column_list in
  let dc_sig = dcols#add Gobject.Data.string in
  let dc_a = dcols#add Gobject.Data.string in
  let dc_b = dcols#add Gobject.Data.string in
  let dc_kind = dcols#add Gobject.Data.string in
  let dc_sup = dcols#add Gobject.Data.string in
  let dc_fg = dcols#add Gobject.Data.string in
  let dstore = GTree.list_store dcols in
  let dsw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
  paned#add1 dsw#coerce;
  let dview = GTree.view ~model:dstore ~packing:dsw#add () in
  let add_dcol title col =
    let r = GTree.cell_renderer_text [] in
    let vc = GTree.view_column ~title ~renderer:(r, [ "text", col ]) () in
    vc#set_cell_data_func r (fun model iter ->
      match model#get ~row:iter ~column:dc_fg with
      | "" -> r#set_properties [ `FOREGROUND_SET false ]
      | c -> r#set_properties [ `FOREGROUND c ]);
    vc#set_resizable true; ignore (dview#append_column vc) in
  add_dcol "signal" dc_sig;
  add_dcol "A" dc_a;
  add_dcol "B" dc_b;
  add_dcol "" dc_kind;
  add_dcol "common inputs (all equal)" dc_sup;

  let detail_sw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
  paned#add2 detail_sw#coerce;
  let detail_view = GText.view ~editable:false ~packing:detail_sw#add () in
  detail_view#misc#modify_font_by_name "Monospace 10";

  let show_ce (c : E.ce) =
    st.ce <- Some c;
    dstore#clear ();
    List.iter (fun (d : E.divergence) ->
      let it = dstore#append () in
      dstore#set ~row:it ~column:dc_sig d.E.d_signal;
      dstore#set ~row:it ~column:dc_a (hex d.E.d_a);
      dstore#set ~row:it ~column:dc_b (hex d.E.d_b);
      dstore#set ~row:it ~column:dc_kind
        (if d.E.d_frontier then "FIRST DIVERGENCE" else "downstream");
      dstore#set ~row:it ~column:dc_sup (string_of_int (List.length d.E.d_support));
      dstore#set ~row:it ~column:dc_fg
        (if d.E.d_frontier then "#c02020" else "#707070")) c.E.ce_divs;
    cone_lbl#set_text (Printf.sprintf
      "%s: A=%s B=%s — cone %d/%d signals — %s"
      c.E.ce_cone (hex c.E.ce_a_val) (hex c.E.ce_b_val)
      (fst c.E.ce_cone_size) (snd c.E.ce_cone_size)
      (if c.E.ce_reproduced then "reproduced in simulation"
       else "NOT reproduced in simulation"));
    detail_view#buffer#set_text (E.report_ce c) in

  ignore (b_scan#connect#clicked ~callback:(fun () ->
    match st.a, st.b with
    | Some a, Some b ->
        (* Scanning with no matching would compare an unrenamed B: every
           state cone would "differ" for the wrong reason. *)
        if st.pairs = [] then do_match ();
        status "scanning cones for differences…";
        let (res, out) = E.capture (fun () -> E.differing_cones a b st.pairs) in
        if out <> "" then log out;
        (match res with
         | Ok (diff, unknown) ->
             st.cones <- diff;
             let (s, _) = snd cone_combo in
             s#clear ();
             List.iter (GEdit.text_combo_add cone_combo) diff;
             if diff <> [] then (fst cone_combo)#set_active 0;
             status (Printf.sprintf "%d differing cone(s)%s" (List.length diff)
                       (if unknown = [] then ""
                        else Printf.sprintf ", %d inconclusive" (List.length unknown)))
         | Error e -> cb.cb_error (Printexc.to_string e))
    | _ -> cb.cb_error "Load both sides on page 1 first."));

  ignore (b_explain#connect#clicked ~callback:(fun () ->
    match st.a, st.b, GEdit.text_combo_get_active cone_combo with
    | Some a, Some b, Some cone ->
        if st.pairs = [] then do_match ();
        status (Printf.sprintf "solving cone %s…" cone);
        let (res, out) = E.capture (fun () -> E.explain_cone a b st.pairs cone) in
        if out <> "" then log out;
        (match res with
         | Ok (Ok c) -> show_ce c; status ("counterexample on " ^ cone)
         | Ok (Error why) -> status why; cb.cb_error why
         | Error e -> cb.cb_error (Printexc.to_string e))
    | _ -> cb.cb_error "Pick a cone first (Find differing cones)."));

  (* selecting a divergence shows its two defining expressions *)
  ignore (dview#selection#connect#changed ~callback:(fun () ->
    match st.ce, dview#selection#get_selected_rows with
    | Some c, (p :: _) ->
        let name = dstore#get ~row:(dstore#get_iter p) ~column:dc_sig in
        (match List.find_opt (fun (d : E.divergence) -> d.E.d_signal = name) c.E.ce_divs with
         | None -> ()
         | Some d ->
             let b = Buffer.create 1024 in
             Printf.bprintf b "%s\n  A = %s\n  B = %s\n  %s\n\n"
               d.E.d_signal (hex d.E.d_a) (hex d.E.d_b)
               (if d.E.d_frontier
                then "FIRST DIVERGENCE — every common signal it depends on agrees, \
                      so the fault is in the logic between them"
                else "downstream — at least one of its inputs already differs");
             Printf.bprintf b "Depends on %d common signal(s):\n"
               (List.length d.E.d_support);
             List.iter (fun s -> Printf.bprintf b "  %s\n" s) d.E.d_support;
             detail_view#buffer#set_text (Buffer.contents b))
    | _ -> ()));

  (* ══════════ run / project ══════════ *)

  let refresh_verdict () =
    match st.run with
    | None -> ()
    | Some r ->
        let colour =
          if r.E.rr_verdict = "EQUIVALENT" then "#207020"
          else if String.length r.E.rr_verdict >= 5
               && String.sub r.E.rr_verdict 0 5 = "DIFFE" then "#c02020"
          else "#a06000" in
        verdict_lbl#set_label
          (Printf.sprintf "<span foreground='%s'><b>%s</b></span>  (%.2f s, %s)"
             colour r.E.rr_verdict r.E.rr_seconds (E.mode_str st.mode));
        let body =
          match st.a, st.b with
          | Some a, Some b -> E.report_run a b st.pairs st.b_left r
          | _ -> E.string_of_census r.E.rr_census in
        census_view#buffer#set_text body in

  ignore (b_run#connect#clicked ~callback:(fun () ->
    match st.a, st.b with
    | Some a, Some b ->
        st.mode <- (match (fst mode_combo)#active with
                    | 1 -> E.Per_cone | 2 -> E.Hierarchical | _ -> E.Flat);
        st.timeout <- (try int_of_string (String.trim tmo#text) with _ -> 30000);
        if st.pairs = [] then do_match ();
        status (Printf.sprintf "running %s miter…" (E.mode_str st.mode));
        let r = E.run_miter ~mode:st.mode ~timeout_ms:st.timeout a b st.pairs in
        st.run <- Some r;
        log r.E.rr_log;
        refresh_verdict ();
        if r.E.rr_cones <> [] then begin
          st.cones <- r.E.rr_cones;
          let (s, _) = snd cone_combo in
          s#clear ();
          List.iter (GEdit.text_combo_add cone_combo) r.E.rr_cones;
          (fst cone_combo)#set_active 0
        end;
        status ("verdict: " ^ r.E.rr_verdict)
    | _ -> cb.cb_error "Load both sides on page 1 first."));

  ignore (b_save_rep#connect#clicked ~callback:(fun () ->
    match st.run with
    | None -> cb.cb_error "Nothing to save — run the miter first."
    | Some r ->
        let p = cb.cb_choose_save () in
        if p <> "" then begin
          let oc = open_out p in
          (match st.a, st.b with
           | Some a, Some b -> output_string oc (E.report_run a b st.pairs st.b_left r)
           | _ -> ());
          output_string oc r.E.rr_log;
          (match st.ce with Some c -> output_string oc ("\n" ^ E.report_ce c) | None -> ());
          close_out oc;
          status ("wrote " ^ p)
        end));

  (* ══════════ Tools: scan, and select what the scan could not find ══════════
     The scan looks where each frontend itself looks (same candidate lists), so
     what it reports is what will run.  When a tool lives somewhere nobody
     guessed, this is where you point at it — the choice is written to
     ~/.config/sv_suite/tools.json and exported into the frontend's env
     override, so it holds for the next session too. *)
  let show_tools_dialog () =
    let dlg = GWindow.window ~title:"External tools" ~width:900 ~height:420
                ~modal:true ~position:`CENTER_ON_PARENT () in
    let v = GPack.vbox ~spacing:6 ~border_width:8 ~packing:dlg#add () in
    ignore (GMisc.label ~xalign:0.0 ~packing:v#pack
              ~markup:"Only frontends whose tool was found are offered on page 1.                        Select a binary here to add one — the choice is remembered." ());
    let tcols = new GTree.column_list in
    let t_fe = tcols#add Gobject.Data.string in
    let t_stat = tcols#add Gobject.Data.string in
    let t_path = tcols#add Gobject.Data.string in
    let t_via = tcols#add Gobject.Data.string in
    let t_note = tcols#add Gobject.Data.string in
    let t_fg = tcols#add Gobject.Data.string in
    let tstore = GTree.list_store tcols in
    let tsw = GBin.scrolled_window ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
                ~packing:(v#pack ~expand:true) () in
    let tview = GTree.view ~model:tstore ~packing:tsw#add () in
    let addc title col =
      let r = GTree.cell_renderer_text [] in
      let vc = GTree.view_column ~title ~renderer:(r, [ "text", col ]) () in
      vc#set_cell_data_func r (fun model iter ->
        match model#get ~row:iter ~column:t_fg with
        | "" -> r#set_properties [ `FOREGROUND_SET false ]
        | c -> r#set_properties [ `FOREGROUND c ]);
      vc#set_resizable true; ignore (tview#append_column vc) in
    addc "frontend" t_fe; addc "status" t_stat; addc "binary" t_path;
    addc "found via" t_via; addc "notes" t_note;
    let fill ?(rescan = false) () =
      let sts = Tool_scan.get ~rescan () in
      tstore#clear ();
      List.iter (fun (x : Tool_scan.status) ->
        let it = tstore#append () in
        let usable = Tool_scan.usable sts x in
        tstore#set ~row:it ~column:t_fe x.Tool_scan.st_spec.Tool_scan.fe;
        tstore#set ~row:it ~column:t_stat
          (if not (Tool_scan.available x) then "MISSING"
           else if not usable then "BLOCKED"
           else match x.Tool_scan.st_spec.Tool_scan.kind with
             | Tool_scan.Builtin -> "built-in"
             | Tool_scan.Prepared -> "dump-only"
             | Tool_scan.External -> "ok");
        tstore#set ~row:it ~column:t_path
          (match x.Tool_scan.st_path with Some p -> p | None -> "—");
        tstore#set ~row:it ~column:t_via x.Tool_scan.st_source;
        tstore#set ~row:it ~column:t_note
          (String.concat " — "
             (List.filter (fun s -> s <> "")
                [ (match Tool_scan.blocked_by sts x with Some w -> w | None -> "");
                  (match x.Tool_scan.st_extra with Some e -> e | None -> "");
                  (match x.Tool_scan.st_version with Some v -> v | None -> "");
                  x.Tool_scan.st_spec.Tool_scan.note ]));
        let colour =
          if not (Tool_scan.available x) then Some "#c02020"
          else if not usable then Some "#a06000" else None in
        tstore#set ~row:it ~column:t_fg
          (match colour with Some c -> c | None -> "")) sts in
    fill ();
    let row = GPack.hbox ~spacing:6 ~packing:v#pack () in
    let selected_fe () =
      match tview#selection#get_selected_rows with
      | [] -> None
      | p :: _ -> Some (tstore#get ~row:(tstore#get_iter p) ~column:t_fe) in
    let b_browse = GButton.button ~label:"Select binary…" ~packing:row#pack () in
    let b_clear = GButton.button ~label:"Clear selection" ~packing:row#pack () in
    let b_rescan = GButton.button ~label:"Rescan" ~packing:row#pack () in
    let b_close = GButton.button ~label:"Close" ~packing:row#pack () in
    let msg = GMisc.label ~xalign:0.0 ~text:"" ~packing:(row#pack ~expand:true) () in
    ignore (b_browse#connect#clicked ~callback:(fun () ->
      match selected_fe () with
      | None -> msg#set_text "Select a row first."
      | Some fe ->
          let p = cb.cb_choose_open () in
          if p <> "" then
            (match Tool_scan.select fe p with
             | Ok () ->
                 fill ~rescan:true ();
                 msg#set_text (Printf.sprintf "%s → %s" fe p);
                 logf "tool: %s → %s\n" fe p
             | Error e -> msg#set_text e; cb.cb_error e)));
    ignore (b_clear#connect#clicked ~callback:(fun () ->
      match selected_fe () with
      | None -> msg#set_text "Select a row first."
      | Some fe ->
          Tool_scan.clear_selection fe; fill ~rescan:true ();
          msg#set_text (fe ^ ": back to the automatic search")));
    ignore (b_rescan#connect#clicked ~callback:(fun () ->
      fill ~rescan:true (); msg#set_text "rescanned"));
    ignore (b_close#connect#clicked ~callback:(fun () ->
      refill_a (); refill_b ();
      logf "%s" (Tool_scan.report (Tool_scan.get ()));
      dlg#destroy ()));
    ignore (dlg#connect#destroy ~callback:(fun () -> refill_a (); refill_b ()));
    dlg#show () in

  let b_tools = GButton.button ~label:"Tools…" ~packing:bar#pack () in
  ignore (b_tools#connect#clicked ~callback:show_tools_dialog);
  let b_open_proj = GButton.button ~label:"Open project…" ~packing:bar#pack () in
  let b_save_proj = GButton.button ~label:"Save project…" ~packing:bar#pack () in
  ignore (b_open_proj#connect#clicked ~callback:(fun () ->
    let p = cb.cb_choose_open () in
    if p <> "" then
      try
        let (a, b, ov, mode, tmo_ms) = E.load_project p in
        st.a_spec <- a; st.b_spec <- b; st.overrides <- ov; st.mode <- mode;
        st.timeout <- tmo_ms; st.project <- Some p;
        set_a_widgets a; set_b_widgets b;
        tmo#set_text (string_of_int tmo_ms);
        (fst mode_combo)#set_active
          (match mode with E.Flat -> 0 | E.Per_cone -> 1 | E.Hierarchical -> 2);
        proj_label#set_text ("project: " ^ p);
        status (Printf.sprintf "loaded project %s (%d manual override(s))"
                  p (List.length ov))
      with e -> cb.cb_error (Printexc.to_string e)));
  ignore (b_save_proj#connect#clicked ~callback:(fun () ->
    let p = match st.project with
      | Some p -> p
      | None -> cb.cb_choose_save () in
    if p <> "" then
      try
        E.save_project p st.a_spec st.b_spec st.overrides st.mode st.timeout;
        st.project <- Some p;
        proj_label#set_text ("project: " ^ p);
        status ("wrote " ^ p)
      with e -> cb.cb_error (Printexc.to_string e)));

  ignore (refresh_a); ignore (refresh_b);
  win#show ();
  if show_tools then show_tools_dialog ()
