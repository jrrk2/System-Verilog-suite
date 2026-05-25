(* End-to-end: an SV sync RAM -> memlower (FPGA byte-lane RAMB18E1) ->
 * behavioral_to_hardcaml (emit_instances) -> fpga_synth AIG/LUT map ->
 * yosys-JSON, confirming RAMB18E1 cells reach the netlist.
 *
 * Usage: test_mem_fpga <top> <file.sv> [more ...] *)
let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let prog = Verible_to_behavioral.convert_files ~top files in
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  let lowered, _ = Behavioral_memlower.lower_program prog in
  let m =
    List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) lowered.modules
  in
  Printf.printf "lowered instances: [%s]\n"
    (String.concat ", "
       (List.map (fun (i : Behavioral_ir.binstance) -> i.module_name) m.instances));
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true m in
  let l = Fpga_synth.Bir_to_aig.lower_circuit circ in
  Printf.printf "AIG: insts=%d regs=%d outputs=%d\n"
    (List.length l.Fpga_synth.Bir_to_aig.insts)
    (List.length l.Fpga_synth.Bir_to_aig.regs)
    (List.length l.Fpga_synth.Bir_to_aig.graph.Fpga_synth.Lut_cover.outputs);
  let mapped = Fpga_synth.Fpga_map.map_lowered ~k:6 ~name:top l in
  Fpga_synth.Fpga_emit.write_yosys_json ~path:"/tmp/mem_fpga.json" mapped;
  print_endline "wrote /tmp/mem_fpga.json"
