(* Map flattened picosoc to nangate45 gates (loop detection OFF so the
 * Circuit builds despite the combinational loop), for OpenSTA/OpenTimer
 * loop analysis.  Usage: test_picosoc_gates <top> <file.sv> [more ...] *)
open Hardcaml

let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let prog = Verible_to_behavioral.convert_files ~top files in
  let flat = Behavioral_hier.flatten_for_z3 prog ~top in
  let prog = { Behavioral_ir.modules = [ flat ]; library_cells = prog.library_cells } in
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  let lowered, _ = Behavioral_memlower.lower_program prog in
  let m = List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) lowered.modules in
  let circ =
    Behavioral_to_hardcaml.create_circuit ~emit_instances:true ~detect_loops:false m
  in
  Printf.eprintf "[gates] circuit built (loop detection off)\n%!";
  (* Behavioral Verilog: yosys sees native RTL ($mux/$and/...) so scc/check
   * can trace the combinational loop and name the wires. *)
  (try
     let oc = Stdlib.open_out "/tmp/picosoc_beh.v" in
     Stdlib.Fun.protect
       ~finally:(fun () -> Stdlib.close_out oc)
       (fun () -> Rtl.output ~output_mode:(To_channel oc) Verilog circ);
     Printf.eprintf "[gates] wrote /tmp/picosoc_beh.v\n%!"
   with e -> Printf.eprintf "[gates] Rtl.output failed: %s\n%!" (Printexc.to_string e));
  let nl = Lib_map.map_circuit circ in
  ignore (Cell_verilog_emit.emit_to_file ~module_name:top nl "/tmp/picosoc_gates.v");
  Printf.eprintf "[gates] wrote /tmp/picosoc_gates.v\n%!"
