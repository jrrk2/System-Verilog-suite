(* Cairo-based waveform renderer for the GUI's simulation window.

   Layout:

     ┌─────────────────────────────────────────────────────────────┐
     │  module name @ N cycles                                     │   header
     ├─────────────┬───────────────────────────────────────────────┤
     │             │ 0   1   2   3   4   5   6   7   8   9   …    │   time axis
     ├─────────────┼───────────────────────────────────────────────┤
     │  clk     1  │ ╨─╨─╨─╨─╨─╨─╨─╨─╨─╨─                         │   per-signal
     │  rst     1  │ ¯¯¯¯¯¯¯____________________                  │   lanes
     │  data   32  │  0x0000_0000  │ 0xdead_beef │ 0xcafe_…       │
     │  …                                                          │
     └─────────────┴───────────────────────────────────────────────┘

   Bool signals render as a two-level square wave.  Multi-bit busses render
   as bracketed segments with a hex label centred between transitions.
   The label is omitted when the segment is too narrow to fit it.        *)

open Gui_sim

(* ── Geometry constants — tweak here to retune the look ─────────────── *)

let row_h        = 22.0   (* px per signal lane                       *)
let label_w      = 180.0  (* px reserved for signal-name column       *)
let cell_w       = 22.0   (* px per cycle                             *)
let top_pad      = 36.0   (* px from canvas top to first signal lane  *)
let trace_pad_y  = 4.0    (* vertical breathing room inside a lane    *)
let header_h     = 28.0
let axis_h       = 16.0

let bool_lo_y row_y = row_y +. row_h -. trace_pad_y
let bool_hi_y row_y = row_y +. trace_pad_y
let bus_top_y  row_y = row_y +. trace_pad_y
let bus_bot_y  row_y = row_y +. row_h -. trace_pad_y

(* Total dimensions required to draw [sr] without clipping — the caller
   sets the [Cairo.surface] / drawing-area size to at least these.  *)
let extents (sr : sim_result) : float * float =
  let n_sigs = List.length sr.sr_inputs + List.length sr.sr_outputs in
  let h = top_pad +. float_of_int n_sigs *. row_h +. 12.0 in
  let w = label_w +. float_of_int sr.sr_cycles *. cell_w +. 12.0 in
  w, h

let set_rgb cr r g b = Cairo.set_source_rgb cr r g b
let set_rgba cr r g b a = Cairo.set_source_rgba cr r g b a

let draw_header cr (sr : sim_result) =
  Cairo.select_font_face cr "Sans" ~slant:Cairo.Upright ~weight:Cairo.Bold;
  Cairo.set_font_size cr 13.0;
  set_rgb cr 0.0 0.0 0.0;
  let title = Printf.sprintf "Simulation: %s @ %d cycles, %d signals"
    sr.sr_module_name sr.sr_cycles
    (List.length sr.sr_inputs + List.length sr.sr_outputs) in
  Cairo.move_to cr 8.0 18.0;
  Cairo.show_text cr title

let draw_axis cr (sr : sim_result) =
  Cairo.select_font_face cr "Sans" ~slant:Cairo.Upright ~weight:Cairo.Normal;
  Cairo.set_font_size cr 10.0;
  set_rgba cr 0.0 0.0 0.0 0.6;
  let y = header_h +. axis_h -. 2.0 in
  for c = 0 to sr.sr_cycles - 1 do
    let x = label_w +. float_of_int c *. cell_w in
    let tall = c mod 10 = 0 in
    Cairo.set_line_width cr (if tall then 1.0 else 0.5);
    Cairo.move_to cr x (y -. (if tall then 8.0 else 4.0));
    Cairo.line_to cr x y;
    Cairo.stroke cr;
    if tall then begin
      Cairo.move_to cr (x +. 2.0) (y -. 2.0);
      Cairo.show_text cr (string_of_int c)
    end
  done

let draw_lane_label cr ts row_y =
  set_rgb cr 0.0 0.0 0.0;
  Cairo.select_font_face cr "Mono" ~slant:Cairo.Upright ~weight:Cairo.Normal;
  Cairo.set_font_size cr 11.0;
  Cairo.move_to cr 4.0 (row_y +. row_h *. 0.65);
  Cairo.show_text cr (Printf.sprintf "%-20s%2d" ts.ts_name ts.ts_width)

let draw_bool_trace cr ts row_y =
  let n = Array.length ts.ts_values in
  set_rgb cr 0.10 0.40 0.10;
  Cairo.set_line_width cr 1.4;
  let y_for c =
    if Hardcaml.Bits.is_vdd ts.ts_values.(c) then bool_hi_y row_y
    else bool_lo_y row_y in
  Cairo.move_to cr label_w (y_for 0);
  for c = 0 to n - 1 do
    let x0 = label_w +. float_of_int c *. cell_w in
    let x1 = x0 +. cell_w in
    let y = y_for c in
    Cairo.line_to cr x0 y;
    Cairo.line_to cr x1 y
  done;
  Cairo.stroke cr

let draw_bus_segment cr ts row_y c0 c1 =
  let x0 = label_w +. float_of_int c0 *. cell_w in
  let x1 = label_w +. float_of_int (c1 + 1) *. cell_w in
  let yt = bus_top_y row_y in
  let yb = bus_bot_y row_y in
  let yc = (yt +. yb) /. 2.0 in
  (* Hex bracket: rising slash | level | falling slash, drawn with a small
     gap at each transition so adjacent segments look distinct.           *)
  set_rgb cr 0.10 0.20 0.45;
  Cairo.set_line_width cr 1.2;
  let slash = 2.5 in
  Cairo.move_to cr (x0 +. slash) yt;
  Cairo.line_to cr (x1 -. slash) yt;
  Cairo.line_to cr x1 yc;
  Cairo.line_to cr (x1 -. slash) yb;
  Cairo.line_to cr (x0 +. slash) yb;
  Cairo.line_to cr x0 yc;
  Cairo.Path.close cr;
  Cairo.stroke cr;
  (* Hex label centred in the segment — only when it fits.  Approx 7 px
     per character for size 10 Mono. *)
  let lbl = format_value ts c0 in
  let need = float_of_int (String.length lbl) *. 6.5 +. 6.0 in
  if x1 -. x0 -. 6.0 >= need then begin
    set_rgba cr 0.05 0.10 0.30 0.95;
    Cairo.select_font_face cr "Mono" ~slant:Cairo.Upright ~weight:Cairo.Normal;
    Cairo.set_font_size cr 10.0;
    let tx = (x0 +. x1) /. 2.0
             -. float_of_int (String.length lbl) *. 3.0 in
    Cairo.move_to cr tx (yc +. 3.5);
    Cairo.show_text cr lbl
  end

let draw_bus_trace cr ts row_y =
  let n = Array.length ts.ts_values in
  let seg_start = ref 0 in
  for c = 1 to n - 1 do
    if not (same_at ts !seg_start c) then begin
      draw_bus_segment cr ts row_y !seg_start (c - 1);
      seg_start := c
    end
  done;
  draw_bus_segment cr ts row_y !seg_start (n - 1)

let draw_lane cr (ts : trace_signal) idx =
  let row_y = top_pad +. float_of_int idx *. row_h in
  (* Alternating stripe to make rows separable. *)
  if idx mod 2 = 0 then begin
    set_rgba cr 0.95 0.95 0.95 1.0;
    Cairo.rectangle cr 0.0 row_y ~w:9999.0 ~h:row_h;
    Cairo.fill cr
  end;
  draw_lane_label cr ts row_y;
  if ts.ts_width = 1 then draw_bool_trace cr ts row_y
  else draw_bus_trace cr ts row_y

let render cr (sr : sim_result) =
  set_rgb cr 1.0 1.0 1.0;
  Cairo.paint cr;
  draw_header cr sr;
  draw_axis cr sr;
  let all = sr.sr_inputs @ sr.sr_outputs in
  List.iteri (fun i ts -> draw_lane cr ts i) all;
  (* Column separator between label and trace areas. *)
  set_rgba cr 0.0 0.0 0.0 0.35;
  Cairo.set_line_width cr 1.0;
  Cairo.move_to cr label_w (header_h +. axis_h);
  let _, h = extents sr in
  Cairo.line_to cr label_w h;
  Cairo.stroke cr
