(* Demonstrate critical-path fanout-cone extraction.

   Two parts:
   1. Synthetic MAC (we know the topology): show how the cone
      shrinks as slack budget tightens.  At budget=0 the cone
      should equal the longest path; loosening the budget pulls
      in side-paths.
   2. Real placed netlist (gcd_nangate45.def): identify the
      cone-of-the-worst-endpoint at sensible budgets and
      report its size and cell-mix. *)

open Lef_def

let load_synth ~lib_path ~lef_path ~width =
  let pair = Cell_delay.load_arc_table lib_path in
  let dsf cell ~slew ~load = Cell_delay.delay_and_slew ~slew ~load pair cell in
  let pin_dir =
    let entries = Lef_pins.parse lef_path in
    Lef_pins.table_of_entries entries in
  let nl = Synth_mac.build ~width
             ~mul_arch:Synth_mac.Wallace_m
             ~add_arch:Synth_mac.Kogge_stone_a () in
  let plc_tbl = Hpwl.placement_table nl.cells in
  let cell_of = Hashtbl.create (List.length nl.cells) in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) nl.cells;
  let edges = Placement_timing.fanout_edges
                ~cell_of ~pin_dir plc_tbl nl.nets in
  let arr =
    Placement_timing.arrival_table_forward
      ~edges ~plc_tbl ~delay_slew_fn:dsf nl.cells in
  (nl, edges, arr)

let load_real ~lib_path ~lef_path ~def_path =
  let pair = Cell_delay.load_arc_table lib_path in
  let dsf cell ~slew ~load = Cell_delay.delay_and_slew ~slew ~load pair cell in
  let pin_dir =
    let entries = Lef_pins.parse lef_path in
    Lef_pins.table_of_entries entries in
  let placements = Placement.parse def_path in
  let nets       = Nets.parse def_path in
  let plc_tbl = Hpwl.placement_table placements in
  let cell_of = Hashtbl.create (List.length placements) in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  let edges = Placement_timing.fanout_edges
                ~cell_of ~pin_dir plc_tbl nets in
  let arr =
    Placement_timing.arrival_table_forward
      ~edges ~plc_tbl ~delay_slew_fn:dsf placements in
  (placements, edges, arr)

let report_cone tag placements edges arr ~budgets =
  let fanin = Fanout_cone.invert_edges edges in
  match Fanout_cone.worst_endpoint arr with
  | None -> Printf.printf "%s: no endpoint\n" tag
  | Some (ep, ep_arr) ->
      Printf.printf "%s: worst endpoint = %s @ %.2f ps\n" tag ep ep_arr;
      List.iter (fun b ->
        let cone = Fanout_cone.cone_of_endpoint
                     ~slack_budget:b ~arrival_tbl:arr ~fanin ~endpoint:ep () in
        let bbox = Fanout_cone.cone_bbox placements cone in
        let hist = Fanout_cone.cone_cell_histogram placements cone in
        Printf.printf "  budget=%.0f ps  cone=%d cells" b (List.length cone);
        (match bbox with
         | None -> ()
         | Some ((xa,ya),(xb,yb)) ->
             Printf.printf "  bbox=[%d..%d]x[%d..%d]" xa xb ya yb);
        Printf.printf "  top-cells: ";
        List.iteri (fun i (c, n) ->
          if i < 3 then Printf.printf "%s=%d " c n) hist;
        print_newline ()) budgets

let () =
  let lib_path = "lef_def/test/nangate.lib" in
  let lef_path = "lef_def/test/nangate.lef" in
  let def_path = "lef_def/test/real.def" in

  let nl, edges_s, arr_s = load_synth ~lib_path ~lef_path ~width:4 in
  Printf.printf "=== Synthetic W=4 wallace+kogge_stone (%d cells, depth %d) ===\n"
    (List.length nl.cells) nl.depth;
  report_cone "synth" nl.cells edges_s arr_s ~budgets:[ 0.0; 30.0; 100.0; 300.0 ];

  Printf.printf "\n=== Real placed netlist (gcd_nangate45.def) ===\n";
  let plcs, edges_r, arr_r = load_real ~lib_path ~lef_path ~def_path in
  report_cone "gcd  " plcs edges_r arr_r ~budgets:[ 0.0; 50.0; 200.0; 1000.0 ]
