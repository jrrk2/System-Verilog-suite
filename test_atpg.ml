(* Stage-1 ATPG entry point.

   Reads a SV file, runs the full synth_pipeline (with scan insertion
   forced on so FFs are externally controllable), and runs random-
   pattern fault simulation per module.  Prints a coverage report.

   Usage:
     test_atpg <file.sv> [top] [pattern-words]

   With pattern-words = 16 (default) the simulator runs 16 × 64 = 1024
   random patterns per module.  Increase to push coverage higher on
   designs that random alone can't reach the deep AND-of-many-inputs
   patterns of.                                                         *)

let derive_top path =
  let base = Filename.basename path in
  try Filename.chop_extension base with Invalid_argument _ -> base

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test_atpg <file.sv> [top] [pattern-words]";
    exit 1
  end;
  let path = Sys.argv.(1) in
  let top =
    if Array.length Sys.argv >= 3 then Sys.argv.(2) else derive_top path in
  let n_words =
    if Array.length Sys.argv >= 4 then int_of_string Sys.argv.(3) else 16 in
  Unix.putenv "SV_DECOMP_SCAN" "1";
  (* DCE / kary_merge can prune the small smoke-test designs to empty;
     turn them off here unless the user explicitly overrides. *)
  if Sys.getenv_opt "SV_DECOMP_NO_DCE" = None then
    Unix.putenv "SV_DECOMP_NO_DCE" "1";
  if Sys.getenv_opt "SV_DECOMP_NO_KARY_MERGE" = None then
    Unix.putenv "SV_DECOMP_NO_KARY_MERGE" "1";
  Printf.printf "ATPG smoke test: top=%s, %d × 64 = %d patterns\n%!"
    top n_words (n_words * 64);
  let netlists, _ =
    Synth_pipeline.run ~emit_verilog:false
      ~top ~out_path:"/tmp/_atpg_ignored.v" ~files:[path] () in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    Printf.printf "\n=== %s: %d cells, %d wires, %d inputs, %d outputs, %d assigns ===\n%!"
      mn.mn_name
      (List.length mn.mn_netlist.insts)
      (List.length mn.mn_netlist.wires)
      (List.length mn.mn_netlist.inputs)
      (List.length mn.mn_netlist.outputs)
      (List.length mn.mn_netlist.assigns);
    if mn.mn_netlist.insts = [] then
      Printf.printf "  (empty netlist — skipping fault sim)\n"
    else
      let r =
        Fault_sim.run_atpg ~n_pattern_words:n_words
          ~module_name:mn.mn_name mn.mn_netlist in
      print_endline (Fault_sim.render_report r)
  ) netlists
