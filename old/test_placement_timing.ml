(* End-to-end placement-aware critical path with real Liberty
   per-cell delays.

   Usage: test_placement_timing <design.def> <library.lib>
*)

let () =
  if Array.length Sys.argv < 3 then begin
    prerr_endline "usage: test_placement_timing <design.def> <library.lib> [tech.lef]";
    exit 2
  end;
  let def_path = Sys.argv.(1) in
  let lib_path = Sys.argv.(2) in
  let lef_path = if Array.length Sys.argv > 3 then Some Sys.argv.(3) else None in

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

  let pin_dir = Option.map (fun p ->
    Printf.printf "Loading LEF %s ...\n%!" p;
    let entries = Lef_def.Lef_pins.parse p in
    Printf.printf "  cell pins             : %d\n%!" (List.length entries);
    Lef_def.Lef_pins.table_of_entries entries) lef_path in

  let r_default = Lef_def.Placement_timing.report ?pin_dir placements nets in
  let r_real    = Lef_def.Placement_timing.report ?pin_dir
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
  Printf.printf "\nReal/default ratio = %.2fx\n" (1. /. speedup);

  (* Per-prefix worst-arrival breakdown — this is where a
     cert-gated arch swap reads its target.  CLI args 4..N are
     BIR instance-prefix probes. *)
  let probes = ref [] in
  for k = 4 to Array.length Sys.argv - 1 do
    probes := Sys.argv.(k) :: !probes
  done;
  if !probes <> [] then begin
    let probes = List.rev !probes in
    let plc_tbl = Lef_def.Hpwl.placement_table placements in
    let cell_of = Hashtbl.create (List.length placements) in
    List.iter (fun (p : Lef_def.Placement.placement) ->
      Hashtbl.replace cell_of p.Lef_def.Placement.inst
        p.Lef_def.Placement.cell) placements;
    let edges = Lef_def.Placement_timing.fanout_edges
                  ?pin_dir ~cell_of plc_tbl nets in
    let arr_tbl = Lef_def.Placement_timing.arrival_table
                    ~delay_of:(Cell_delay.lookup delay_tbl)
                    edges placements in
    let bindings = Lef_def.Bir_def_bind.bind_by_prefix probes placements in
    Printf.printf "\nPer-BIR-prefix worst arrival (Liberty-grounded):\n";
    List.iter (fun b ->
      let arr = Lef_def.Bir_def_bind.subgraph_worst arr_tbl b in
      Printf.printf "  %-20s  %4d cells  %.1f ps\n"
        b.Lef_def.Bir_def_bind.bir_path
        (List.length b.Lef_def.Bir_def_bind.members) arr) bindings
  end
