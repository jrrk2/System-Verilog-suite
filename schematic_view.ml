(* schematic_view.ml — Cairo renderer + GTK window for the placed
   schematic produced by Schematic_layout.build.

   View interactions:
     • scroll wheel               → zoom in / out around cursor
     • left-button drag           → pan
     • hover over an instance     → status bar shows "<inst> : <module>"

   The window is self-contained: caller passes in a schematic record and
   we pop a new top-level GtkWindow. *)

open Symbol_lib
open Schematic_layout

(* ---------------- low-level drawing primitives ---------------- *)

let draw_prim cr (ox, oy) sym_bbox p =
  let (_, sy1, _, sy2) = sym_bbox in
  let yflip y = sy2 -. (y -. sy1) in
  match p with
  | PLine ((x1, y1), (x2, y2)) ->
      Cairo.move_to cr (ox +. x1) (oy +. yflip y1);
      Cairo.line_to cr (ox +. x2) (oy +. yflip y2);
      Cairo.stroke cr
  | PCircle ((cx, cy), r) ->
      Cairo.arc cr (ox +. cx) (oy +. yflip cy) ~r
        ~a1:0.0 ~a2:(2.0 *. 3.14159265358979);
      Cairo.stroke cr
  | PArc ((x1, y1), (x2, y2), (x3, y3)) ->
      (* Three-point arc: passes through start, mid, end.  This matches
         the Synopsys .slib convention.  Compute the circumcircle, work
         out angles, and pick CCW vs CW so the rendered arc actually
         contains the mid-point. *)
      let y1 = yflip y1 and y2 = yflip y2 and y3 = yflip y3 in
      let ax = x2 -. x1 and ay = y2 -. y1 in
      let bx = x3 -. x1 and by = y3 -. y1 in
      let d = 2.0 *. (ax *. by -. ay *. bx) in
      if abs_float d < 1e-9 then begin
        (* Collinear — degenerate to a straight line through all three. *)
        Cairo.move_to cr (ox +. x1) (oy +. y1);
        Cairo.line_to cr (ox +. x3) (oy +. y3);
        Cairo.stroke cr
      end else begin
        let a2 = ax *. ax +. ay *. ay in
        let b2 = bx *. bx +. by *. by in
        let cx = x1 +. (by *. a2 -. ay *. b2) /. d in
        let cy = y1 +. (ax *. b2 -. bx *. a2) /. d in
        let r  = sqrt ((x1 -. cx) ** 2.0 +. (y1 -. cy) ** 2.0) in
        let a1 = atan2 (y1 -. cy) (x1 -. cx) in
        let am = atan2 (y2 -. cy) (x2 -. cx) in
        let ae = atan2 (y3 -. cy) (x3 -. cx) in
        let pi2 = 2.0 *. 3.14159265358979 in
        let pi  = 3.14159265358979 in
        let norm a = let a = mod_float a pi2 in if a < 0.0 then a +. pi2 else a in
        let n1 = norm a1 and nm = norm am and ne = norm ae in
        (* Walk CCW from n1; does we pass nm before reaching ne? *)
        let sweep_ccw =
          let dnm = if nm >= n1 then nm -. n1 else nm -. n1 +. pi2 in
          let dne = if ne >= n1 then ne -. n1 else ne -. n1 +. pi2 in
          dnm < dne in
        ignore pi;
        if sweep_ccw
        then Cairo.arc cr (ox +. cx) (oy +. cy) ~r ~a1 ~a2:ae
        else Cairo.arc_negative cr (ox +. cx) (oy +. cy) ~r ~a1 ~a2:ae;
        Cairo.stroke cr
      end

let draw_label cr ~x ~y ?(size = 9.0) ?(align = `Centre) text =
  Cairo.save cr;
  Cairo.set_font_size cr size;
  let ext = Cairo.text_extents cr text in
  let dx = match align with
    | `Left -> 0.0
    | `Centre -> -. ext.Cairo.width /. 2.0
    | `Right -> -. ext.Cairo.width in
  Cairo.move_to cr (x +. dx) (y +. ext.Cairo.height /. 2.0);
  Cairo.show_text cr text;
  Cairo.restore cr

(* ---------------- main render ---------------- *)

let render cr ~scale ~pan_x ~pan_y
           ?(highlight_inst : string option = None)
           (sc : schematic) =
  Cairo.set_source_rgb cr 1.0 1.0 1.0;
  Cairo.paint cr;
  Cairo.save cr;
  Cairo.translate cr pan_x pan_y;
  Cairo.scale cr scale scale;

  (* Module outline. *)
  Cairo.set_source_rgba cr 0.85 0.85 0.85 1.0;
  Cairo.set_line_width cr (1.0 /. scale);
  Cairo.rectangle cr 0.0 0.0 ~w:sc.sc_width ~h:sc.sc_height;
  Cairo.stroke cr;

  (* Nets first so symbols overpaint them.  Tie-driven nets get a
     little "0"/"1" label at each consumer pin instead of wires. *)
  List.iter (fun n ->
    match n.net_tie_const with
    | Some s ->
        Cairo.set_source_rgb cr 0.25 0.25 0.25;
        List.iter (fun pp ->
          if pp.pp_dir <> PinOut then
            let (x, y) = pp.pp_pos in
            draw_label cr ~x:(x -. 6.0) ~y ~size:9.0 ~align:`Right s
        ) n.net_endpoints
    | None ->
        Cairo.set_source_rgba cr 0.20 0.40 0.80 0.85;
        Cairo.set_line_width cr (1.2 /. scale);
        List.iter (fun poly ->
          match poly with
          | [] | [_] -> ()
          | (x0, y0) :: rest ->
              Cairo.move_to cr x0 y0;
              List.iter (fun (x, y) -> Cairo.line_to cr x y) rest;
              Cairo.stroke cr
        ) n.net_polyline;
        (* Junction dots where 3+ wires meet at a shared trunk x. *)
        let ends = List.map (fun pp -> pp.pp_pos) n.net_endpoints in
        if List.length ends >= 3 then
          List.iter (fun (x, y) ->
            Cairo.arc cr x y ~r:(1.6 /. scale) ~a1:0.0
              ~a2:(2.0 *. 3.14159265);
            Cairo.fill cr) ends
  ) sc.sc_nets;

  (* Port pads.  Primary inputs are drawn as filled circles ("blobs");
     primary outputs as hollow circles for visual contrast.            *)
  Cairo.set_line_width cr (1.4 /. scale);
  List.iter (fun (pp : port_pad) ->
    let (x, y) = pp.pad_pos in
    let r = 4.5 in
    (match pp.pad_dir with
     | `Input ->
         Cairo.set_source_rgba cr 0.10 0.55 0.10 1.0;
         Cairo.arc cr x y ~r ~a1:0.0 ~a2:(2.0 *. 3.14159265358979);
         Cairo.fill cr
     | `Output ->
         Cairo.set_source_rgba cr 0.65 0.10 0.10 1.0;
         Cairo.arc cr x y ~r ~a1:0.0 ~a2:(2.0 *. 3.14159265358979);
         Cairo.stroke cr);
    Cairo.set_source_rgb cr 0.0 0.0 0.0;
    (match pp.pad_dir with
     | `Input  -> draw_label cr ~x:(x -. 8.0) ~y ~align:`Right pp.pad_name
     | `Output -> draw_label cr ~x:(x +. 8.0) ~y ~align:`Left  pp.pad_name)
  ) sc.sc_ports;

  (* Instances. *)
  List.iter (fun (pi : placed_inst) ->
    let (ox, oy) = pi.pi_xy in
    let (sx1, sy1, sx2, sy2) = pi.pi_sym.sym_bbox in
    let bw = sx2 -. sx1 and bh = sy2 -. sy1 in
    let hl = (match highlight_inst with
              | Some n when n = pi.pi_inst -> true
              | _ -> false) in
    if hl then begin
      Cairo.set_source_rgba cr 1.0 0.9 0.6 0.9;
      Cairo.rectangle cr ox oy ~w:bw ~h:bh;
      Cairo.fill cr
    end;
    Cairo.set_source_rgb cr 0.0 0.0 0.0;
    Cairo.set_line_width cr (1.0 /. scale);
    if pi.pi_sym.sym_prims = [] then begin
      Cairo.rectangle cr ox oy ~w:bw ~h:bh;
      Cairo.stroke cr
    end;
    let off = (ox -. sx1, oy) in
    ignore sy1;
    List.iter (fun p -> draw_prim cr off pi.pi_sym.sym_bbox p) pi.pi_sym.sym_prims;
    List.iter (fun p ->
      List.iter (fun pr -> draw_prim cr off pi.pi_sym.sym_bbox pr) p.pin_prims
    ) pi.pi_sym.sym_pins;
    Cairo.set_source_rgb cr 0.20 0.20 0.20;
    draw_label cr ~x:(ox +. bw /. 2.0) ~y:(oy +. bh /. 2.0 -. 6.0)
      ~size:10.0 pi.pi_type;
    draw_label cr ~x:(ox +. bw /. 2.0) ~y:(oy +. bh /. 2.0 +. 7.0)
      ~size:8.0 pi.pi_inst;
    Cairo.set_source_rgb cr 0.35 0.35 0.35;
    List.iter (fun pp ->
      let (x, y) = pp.pp_pos in
      let dx = if pp.pp_dir = PinIn then -2.0 else 2.0 in
      let align = if pp.pp_dir = PinIn then `Right else `Left in
      draw_label cr ~x:(x +. dx) ~y:(y -. 4.0) ~size:7.0 ~align pp.pp_pin
    ) pi.pi_pins
  ) sc.sc_insts;

  Cairo.restore cr

(* ---------------- hit-testing ---------------- *)

let inst_at (sc : schematic) ~x ~y =
  List.find_opt (fun (pi : placed_inst) ->
    let (ox, oy) = pi.pi_xy in
    x >= ox && x <= ox +. pi.pi_w &&
    y >= oy && y <= oy +. pi.pi_h
  ) sc.sc_insts

(* Rasterise the entire schematic (natural coords, scale 1.0) to a
   PNG or SVG file.  Cairo's image-surface limit is 32767 px in either
   dimension; for canvases larger than that we fall back to SVG via
   Cairo's vector surface, which has no fixed bound. *)
let save_png (sc : schematic) (path : string) =
  let margin = 40 in
  let w = int_of_float sc.sc_width  + 2 * margin in
  let h = int_of_float sc.sc_height + 2 * margin in
  let want_svg =
    let lp = String.lowercase_ascii path in
    Filename.check_suffix lp ".svg"
    || w >= 32760 || h >= 32760 in
  if want_svg then begin
    let svg_path =
      if Filename.check_suffix (String.lowercase_ascii path) ".png"
      then Filename.remove_extension path ^ ".svg"
      else path in
    let surf = Cairo.SVG.create svg_path
                 ~w:(float_of_int w) ~h:(float_of_int h) in
    let cr = Cairo.create surf in
    render cr ~scale:1.0
      ~pan_x:(float_of_int margin) ~pan_y:(float_of_int margin) sc;
    Cairo.Surface.finish surf;
    Printf.eprintf "[schematic] canvas %dx%d > Cairo PNG limit; wrote SVG to %s\n%!"
      w h svg_path;
    (* Also produce a downscaled PNG preview so existing tooling that
       expects a rastered output still gets something. *)
    if Filename.check_suffix (String.lowercase_ascii path) ".png" then begin
      let max_d = 4000 in
      let scale = min (float_of_int max_d /. float_of_int w)
                      (float_of_int max_d /. float_of_int h) in
      let scale = min 1.0 scale in
      let pw = int_of_float (float_of_int w *. scale) in
      let ph = int_of_float (float_of_int h *. scale) in
      let surf2 = Cairo.Image.create Cairo.Image.ARGB32 ~w:pw ~h:ph in
      let cr2 = Cairo.create surf2 in
      render cr2 ~scale
        ~pan_x:(float_of_int margin *. scale)
        ~pan_y:(float_of_int margin *. scale) sc;
      Cairo.PNG.write surf2 path;
      Printf.eprintf "[schematic] downscaled %.3fx PNG preview written to %s\n%!"
        scale path
    end
  end else begin
    let surf = Cairo.Image.create Cairo.Image.ARGB32 ~w ~h in
    let cr = Cairo.create surf in
    render cr ~scale:1.0
      ~pan_x:(float_of_int margin) ~pan_y:(float_of_int margin) sc;
    Cairo.PNG.write surf path
  end

(* ---------------- GTK window ---------------- *)

let open_window ?(title = "Schematic") (sc : schematic) =
  let win = GWindow.window
    ~title:(Printf.sprintf "%s — %s" title sc.sc_module)
    ~width:1100 ~height:750 () in
  let vbox = GPack.vbox ~packing:win#add () in
  let toolbar = GPack.hbox ~packing:vbox#pack ~spacing:6 () in
  let save_btn = GButton.button ~label:"Save as PNG..."
                   ~packing:(toolbar#pack ~padding:4) () in
  let scrolled = GBin.scrolled_window
    ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC
    ~packing:(vbox#pack ~expand:true) () in
  let da = GMisc.drawing_area ~packing:scrolled#add_with_viewport () in
  let status = GMisc.statusbar ~packing:vbox#pack () in
  let ctx = status#new_context ~name:"schem" in
  let push s = ignore (ctx#push s) in
  ignore (save_btn#connect#clicked ~callback:(fun () ->
    let d = GWindow.file_chooser_dialog ~action:`SAVE
              ~title:"Save schematic as PNG"
              ~parent:win ~modal:true () in
    d#add_button_stock `CANCEL `CANCEL;
    d#add_button_stock `SAVE   `SAVE;
    d#set_current_name (sc.sc_module ^ ".png");
    let r = d#run () in
    let f = d#filename in
    d#destroy ();
    match r, f with
    | `SAVE, Some path ->
        (try
          save_png sc path;
          push (Printf.sprintf
            "Saved %dx%d PNG to %s"
            (int_of_float sc.sc_width + 80)
            (int_of_float sc.sc_height + 80) path)
         with e ->
           let err = GWindow.message_dialog
             ~message:("Save failed: " ^ Printexc.to_string e)
             ~message_type:`ERROR
             ~buttons:GWindow.Buttons.ok ~parent:win ~modal:true () in
           ignore (err#run ()); err#destroy ())
    | _ -> ()));
  push (Printf.sprintf "%s: %d cells, %d nets, %d ports"
          sc.sc_module
          (List.length sc.sc_insts)
          (List.length sc.sc_nets)
          (List.length sc.sc_ports));
  let init_w = int_of_float (sc.sc_width  +. 60.0) in
  let init_h = int_of_float (sc.sc_height +. 60.0) in
  da#misc#set_size_request ~width:(max 600 init_w) ~height:(max 400 init_h) ();
  da#event#add [`BUTTON_PRESS; `BUTTON_RELEASE; `BUTTON_MOTION;
                `POINTER_MOTION; `SCROLL];
  let scale = ref 1.0 in
  let pan_x = ref 30.0 and pan_y = ref 30.0 in
  let dragging : (float * float * float * float) option ref = ref None in
  let highlight : string option ref = ref None in
  let canvas_coords x y =
    ((x -. !pan_x) /. !scale, (y -. !pan_y) /. !scale) in
  ignore (da#misc#connect#draw ~callback:(fun cr ->
    render cr
           ~scale:!scale ~pan_x:!pan_x ~pan_y:!pan_y
           ~highlight_inst:!highlight sc;
    true));
  ignore (da#event#connect#button_press ~callback:(fun ev ->
    if GdkEvent.Button.button ev = 1 then begin
      dragging := Some (GdkEvent.Button.x ev, GdkEvent.Button.y ev,
                        !pan_x, !pan_y);
      true
    end else false));
  ignore (da#event#connect#button_release ~callback:(fun _ ->
    dragging := None; true));
  ignore (da#event#connect#motion_notify ~callback:(fun ev ->
    let x = GdkEvent.Motion.x ev and y = GdkEvent.Motion.y ev in
    (match !dragging with
     | Some (x0, y0, px0, py0) ->
         pan_x := px0 +. (x -. x0);
         pan_y := py0 +. (y -. y0);
         GtkBase.Widget.queue_draw da#as_widget
     | None ->
         let (cx, cy) = canvas_coords x y in
         let h = inst_at sc ~x:cx ~y:cy in
         (match h with
          | Some pi ->
              push (Printf.sprintf "%s : %s" pi.pi_inst pi.pi_type);
              if !highlight <> Some pi.pi_inst then begin
                highlight := Some pi.pi_inst;
                GtkBase.Widget.queue_draw da#as_widget
              end
          | None ->
              if !highlight <> None then begin
                highlight := None;
                GtkBase.Widget.queue_draw da#as_widget
              end));
    true));
  ignore (da#event#connect#scroll ~callback:(fun ev ->
    let x = GdkEvent.Scroll.x ev and y = GdkEvent.Scroll.y ev in
    let factor = match GdkEvent.Scroll.direction ev with
      | `UP   -> 1.15
      | `DOWN -> 1.0 /. 1.15
      | _     -> 1.0 in
    let (cx, cy) = canvas_coords x y in
    scale := !scale *. factor;
    pan_x := x -. cx *. !scale;
    pan_y := y -. cy *. !scale;
    GtkBase.Widget.queue_draw da#as_widget;
    true));
  win#show ()
