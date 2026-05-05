(* Regression: placement-aware critical path is sensitive to
   coordinate spacing.  We synthesise two tiny DEFs in-memory:
   four buffers in a chain, placed (a) tightly clustered and
   (b) spread along x.  The spread version must report a longer
   arrival.  Anything else means placement isn't influencing
   timing. *)

open Lef_def

let make_chain ~spacing =
  let placements = [
    { Placement.inst="b0"; cell="BUF_X1"; x = 0;            y=0; orient=Placement.N };
    { Placement.inst="b1"; cell="BUF_X1"; x = spacing;      y=0; orient=Placement.N };
    { Placement.inst="b2"; cell="BUF_X1"; x = 2 * spacing;  y=0; orient=Placement.N };
    { Placement.inst="b3"; cell="BUF_X1"; x = 3 * spacing;  y=0; orient=Placement.N };
  ] in
  let nets : Nets.net list = [
    { name="n01"; pins=[{inst="b0";pin="Z"}; {inst="b1";pin="A"}] };
    { name="n12"; pins=[{inst="b1";pin="Z"}; {inst="b2";pin="A"}] };
    { name="n23"; pins=[{inst="b2";pin="Z"}; {inst="b3";pin="A"}] };
  ] in
  placements, nets

let run () =
  let pl1, n1 = make_chain ~spacing:1000   in
  let pl2, n2 = make_chain ~spacing:100000 in
  let r1 = Placement_timing.report pl1 n1 in
  let r2 = Placement_timing.report pl2 n2 in
  Printf.printf "tight  spacing=1000   worst=%s  arr=%.3f ps  total wire=%.3f ps\n"
    r1.worst_inst r1.worst_arr_ps r1.total_wire_ps;
  Printf.printf "spread spacing=100000 worst=%s  arr=%.3f ps  total wire=%.3f ps\n"
    r2.worst_inst r2.worst_arr_ps r2.total_wire_ps;
  if r2.worst_arr_ps > r1.worst_arr_ps
  then begin print_endline "OK   placement moves the arrival number"; exit 0 end
  else begin print_endline "FAIL placement-aware timing is flat"; exit 1 end

let () = run ()
