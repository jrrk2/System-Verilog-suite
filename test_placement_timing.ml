(* End-to-end placement-aware critical path with real Liberty
   per-cell delays.

   Usage: test_placement_timing <design.def> <library.lib>
*)

let () =
  if Array.length Sys.argv < 3 then begin
    prerr_endline "usage: test_placement_timing <design.def> <library.lib>";
    exit 2
  end;
  let def_path = Sys.argv.(1) in
  let lib_path = Sys.argv.(2) in

  Printf.printf "Loading Liberty %s ...\n%!" lib_path;
  let delay_tbl = Cell_delay.load lib_path in
  Printf.printf "  cells with delay arcs : %d\n%!" (Hashtbl.length delay_tbl);

  let mn = ref infinity and mx = ref neg_infinity in
  let sum = ref 0. and n = ref 0 in
  Hashtbl.iter (fun _ v ->
    if v < !mn then mn := v;
    if v > !mx then mx := v;
    sum := !sum +. v; incr n) delay_tbl;
  if !n > 0 then
    Printf.printf "  delay range (ps)      : %.1f .. %.1f  (mean %.1f)\n"
      !mn !mx (!sum /. float_of_int !n);

  Printf.printf "Loading DEF %s ...\n%!" def_path;
  let placements = Lef_def.Placement.parse def_path in
  let nets       = Lef_def.Nets.parse def_path in
  Printf.printf "  placements            : %d\n" (List.length placements);
  Printf.printf "  nets                  : %d\n%!" (List.length nets);

  let r_default = Lef_def.Placement_timing.report placements nets in
  let r_real    = Lef_def.Placement_timing.report
                    ~delay_of:(Cell_delay.lookup delay_tbl)
                    placements nets in

  Printf.printf "\n--- Default 50-ps cell delays ---\n";
  Printf.printf "  worst inst   : %s (%s)\n"
    r_default.worst_inst r_default.worst_cell;
  Printf.printf "  worst arr    : %.3f ps\n" r_default.worst_arr_ps;

  Printf.printf "\n--- Real Liberty cell delays ---\n";
  Printf.printf "  worst inst   : %s (%s)\n"
    r_real.worst_inst r_real.worst_cell;
  Printf.printf "  worst arr    : %.3f ps\n" r_real.worst_arr_ps;
  Printf.printf "  total wire   : %.3f ps  (sum across signal nets)\n"
    r_real.total_wire_ps;

  let speedup = r_default.worst_arr_ps /. r_real.worst_arr_ps in
  Printf.printf "\nReal/default ratio = %.2fx\n" (1. /. speedup)
