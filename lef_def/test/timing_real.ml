(* Run the placement-aware timing report on a real DEF. *)
open Lef_def

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "real.def" in
  let placements = Placement.parse path in
  let nets       = Nets.parse path in
  let r = Placement_timing.report placements nets in
  Printf.printf "Worst inst        : %s (%s)\n" r.worst_inst r.worst_cell;
  Printf.printf "Worst arrival     : %.3f ps\n" r.worst_arr_ps;
  Printf.printf "Total wire delay  : %.3f ps  (sum over all signal nets)\n"
    r.total_wire_ps
