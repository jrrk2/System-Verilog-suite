(* Smoke: bind synth-side instance prefixes against the
   gcd_nangate45 placed netlist and report per-binding member
   count, bbox, and worst gate-level arrival. *)

open Lef_def

let () =
  let def_path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "real.def" in
  let lef_path = if Array.length Sys.argv > 2 then Some Sys.argv.(2) else None in

  let placements = Placement.parse def_path in
  let nets       = Nets.parse def_path in
  let pin_dir = Option.map (fun p ->
    let entries = Lef_pins.parse p in
    Lef_pins.table_of_entries entries) lef_path in

  Printf.printf "DEF: %d placements, %d nets\n%!"
    (List.length placements) (List.length nets);

  (* Build the global per-instance arrival table once. *)
  let plc_tbl = Hpwl.placement_table placements in
  let cell_of = Hashtbl.create (List.length placements) in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  let edges = Placement_timing.fanout_edges ?pin_dir ~cell_of plc_tbl nets in
  let arr_tbl = Placement_timing.arrival_table edges placements in

  (* Bind a few synth-emitted prefix groups.  These are what
     OpenROAD/Yosys actually wrote for this design. *)
  let prefixes = [ "rebuffer"; "output"; "clkbuf"; "FILLER"; "clone" ] in
  let bindings = Bir_def_bind.bind_by_prefix prefixes placements in

  let dump label bs =
    Printf.printf "\n%s\n" label;
    Printf.printf "  %-12s  %6s  %12s  %s\n"
      "BIR-prefix" "count" "worst arr ps" "bbox";
    Printf.printf "  %s\n" (String.make 70 '-');
    List.iter (fun b ->
      let arr = Bir_def_bind.subgraph_worst arr_tbl b in
      let bbox = match Bir_def_bind.bbox b with
        | None -> "(empty)"
        | Some ((xa,ya),(xb,yb)) ->
            Printf.sprintf "x=[%d..%d] y=[%d..%d]" xa xb ya yb in
      Printf.printf "  %-12s  %6d  %12.1f  %s\n"
        b.Bir_def_bind.bir_path
        (List.length b.Bir_def_bind.members) arr bbox) bs;
    let u = Bir_def_bind.unbound bs placements in
    Printf.printf "  unbound: %d\n" (List.length u)
  in
  dump "=== prefix-only ===" bindings;

  let bindings_c, iters =
    Bir_def_bind.bind_with_connectivity bindings
      ~edges ~placements in
  dump (Printf.sprintf "=== + connectivity (%d iters) ===" iters) bindings_c;

  let bindings_b, iters_b =
    Bir_def_bind.bind_with_connectivity ~bbox_pad:None bindings
      ~edges ~placements in
  dump (Printf.sprintf "=== + connectivity, no bbox gate (%d iters) ==="
          iters_b) bindings_b
