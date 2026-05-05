(* MAC architecture sweep: build a synthetic placed netlist for
   each (mul_arch, adder_arch) combination, then run the same
   Liberty + LEF + Placement_timing pipeline as a real placed
   design.  Tabulate analytical depth, total cells, total HPWL,
   and worst arrival.

   Usage:
     test_mac_arch <library.lib> <tech.lef> [width=8]
*)

open Lef_def

let () =
  if Array.length Sys.argv < 3 then begin
    prerr_endline
      "usage: test_mac_arch <library.lib> <tech.lef> [width=8]";
    exit 2
  end;
  let lib_path = Sys.argv.(1) in
  let lef_path = Sys.argv.(2) in
  let width = if Array.length Sys.argv > 3
              then int_of_string Sys.argv.(3) else 8 in

  Printf.printf "Loading Liberty %s ...\n%!" lib_path;
  let delay_tbl = Cell_delay.load lib_path in
  Printf.printf "  cells with delay arcs : %d\n%!"
    (Hashtbl.length delay_tbl);

  Printf.printf "Loading LEF %s ...\n%!" lef_path;
  let lef_entries = Lef_pins.parse lef_path in
  let pin_dir = Lef_pins.table_of_entries lef_entries in
  Printf.printf "  cell pins             : %d\n\n%!"
    (List.length lef_entries);

  let muls = [ Synth_mac.Array_m; Synth_mac.Wallace_m; Synth_mac.Dadda_m ] in
  let adds = [ Synth_mac.Ripple_a; Synth_mac.Brent_kung_a;
               Synth_mac.Sklansky_a; Synth_mac.Kogge_stone_a ] in

  Printf.printf "MAC width = %d  (a,b ∈ unsigned %d-bit, y ∈ %d-bit)\n\n"
    width width (2 * width);

  Printf.printf
    "  %-9s %-12s | %5s %5s | %6s | %10s | %10s\n"
    "mul" "adder" "depth" "cells" "HPWL" "wire ps" "worst ps";
  Printf.printf "  %s\n" (String.make 75 '-');

  let results = ref [] in

  List.iter (fun ma ->
    List.iter (fun aa ->
      let nl = Synth_mac.build ~width ~mul_arch:ma ~add_arch:aa () in
      let r = Placement_timing.report
                ~delay_of:(Cell_delay.lookup delay_tbl)
                ~pin_dir
                nl.cells nl.nets in
      let plc_tbl = Hpwl.placement_table nl.cells in
      let total_hpwl =
        List.fold_left
          (fun acc n -> acc + Hpwl.hpwl_of_net plc_tbl n)
          0 nl.nets in
      let n_cells = List.length nl.cells in
      Printf.printf
        "  %-9s %-12s | %5d %5d | %6d | %10.2f | %10.2f\n"
        (Synth_mac.mul_arch_name ma)
        (Synth_mac.adder_arch_name aa)
        nl.depth n_cells total_hpwl r.total_wire_ps r.worst_arr_ps;
      results := (ma, aa, nl.depth, n_cells, total_hpwl, r) :: !results
    ) adds
  ) muls;

  Printf.printf "\nBest by topological worst-arrival:\n";
  let sorted = List.sort
    (fun (_,_,_,_,_,r1) (_,_,_,_,_,r2) ->
       compare r1.Placement_timing.worst_arr_ps
               r2.Placement_timing.worst_arr_ps)
    !results in
  List.iteri (fun i (ma, aa, _, _, _, r) ->
    if i < 3 then
      Printf.printf "  #%d  %s + %s   worst = %.1f ps\n"
        (i+1) (Synth_mac.mul_arch_name ma)
        (Synth_mac.adder_arch_name aa)
        r.Placement_timing.worst_arr_ps) sorted
