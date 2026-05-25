(* Full picosoc through the FPGA flow:
 *   Verible -> flatten -> unroll/inline/iflift/blocking_subst/meminfer
 *   -> memlower(FPGA BRAM) -> behavioral_to_hardcaml ~emit_instances
 *   -> bir_to_aig -> fpga_map ~io -> yosys-JSON.
 * Usage: test_picosoc_fpga <top> <file.sv> [more ...] *)
let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  Printf.eprintf "[picosoc] parsing %s (%d files)\n%!" top (List.length files);
  let prog = Verible_to_behavioral.convert_files ~top files in
  Printf.eprintf "[picosoc] parsed %d modules\n%!" (List.length prog.modules);
  (* flatten_for_z3 fully inlines the submodule hierarchy into one bmodule
     (param-specialised) — what the miter uses; flatten_program only
     flattens generate blocks and leaves instances. *)
  let flat = Behavioral_hier.flatten_for_z3 prog ~top in
  let prog = { Behavioral_ir.modules = [ flat ]; library_cells = prog.library_cells } in
  Printf.eprintf "[picosoc] flattened -> 1 module, %d signals %d processes %d insts\n%!"
    (List.length flat.signals) (List.length flat.processes) (List.length flat.instances);
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  Printf.eprintf "[picosoc] pipeline done\n%!";
  let lowered, _ = Behavioral_memlower.lower_program prog in
  Printf.eprintf "[picosoc] memlower done\n%!";
  let m =
    List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) lowered.modules
  in
  Printf.eprintf "[picosoc] top %s: signals=%d processes=%d instances=[%s] mems=%d\n%!"
    m.name (List.length m.signals) (List.length m.processes)
    (String.concat "," (List.map (fun (i : Behavioral_ir.binstance) -> i.module_name) m.instances))
    (List.length m.mems);
  (let oc = open_out "/tmp/picosoc_flat.txt" in
   output_string oc (Behavioral_ir.string_of_bmodule m);
   close_out oc);
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true m in
  Printf.eprintf "[picosoc] hardcaml circuit built\n%!";
  let l = Fpga_synth.Bir_to_aig.lower_circuit circ in
  Printf.eprintf "[picosoc] AIG: nodes=%d insts=%d regs=%d outputs=%d\n%!"
    (Array.length l.Fpga_synth.Bir_to_aig.graph.Fpga_synth.Lut_cover.nodes)
    (List.length l.Fpga_synth.Bir_to_aig.insts)
    (List.length l.Fpga_synth.Bir_to_aig.regs)
    (List.length l.Fpga_synth.Bir_to_aig.graph.Fpga_synth.Lut_cover.outputs);
  let mapped = Fpga_synth.Fpga_map.map_lowered ~io:true ~k:6 ~name:top l in
  Fpga_synth.Fpga_emit.write_yosys_json ~path:"/tmp/picosoc_fpga.json" mapped;
  Printf.eprintf "[picosoc] wrote /tmp/picosoc_fpga.json\n%!"
