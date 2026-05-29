(* Map flattened picosoc to nangate45 gates (loop detection OFF so the
 * Circuit builds despite the combinational loop), for OpenSTA/OpenTimer
 * loop analysis.  Usage: test_picosoc_gates <top> <file.sv> [more ...] *)
open Hardcaml

let () =
  (* Default to FPGA-mode memlower (the picosoc bring-up wants block-
     RAM inference for the progmem ROM), but allow callers to skip
     it — random_sv_gen's tiny case-as-ROM emits would otherwise
     pick up RAMB18/36E1 primitives that downstream yosys equiv
     can't link without unisims_sim.    *)
  if Sys.getenv_opt "GATE_NO_BRAM" <> Some "1" then
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
  (* SSA after memlower: convert each combinational/sequential body to
     versioned BAssigns so multi-write targets (picorv32 pcpi_mul's
     carry-save next_rd/next_rdt slice-write chain) get separate
     intermediate signals, breaking the structural cycle through
     Always.Variable.value var.  Skippable via NO_SSA=1 — handy when
     the SSA pass is suspected of breaking a downstream lowering
     (e.g. losing reset BIf structure → uninitialised gate FFs). *)
  let lowered =
    if Sys.getenv_opt "NO_SSA" = Some "1" then lowered
    else { lowered with
      modules = List.map Behavioral_ssa.module_to_ssa lowered.modules } in
  let m = List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) lowered.modules in
  let circ =
    Behavioral_to_hardcaml.create_circuit ~emit_instances:true ~detect_loops:false m
  in
  Printf.eprintf "[gates] circuit built (loop detection off)\n%!";
  (* Persistent build dir — survives reboot, unlike /tmp.  Default
     ~/picosoc_build/, overridable via $PICOSOC_BUILD. *)
  let build =
    match Sys.getenv_opt "PICOSOC_BUILD" with
    | Some d -> d
    | None ->
        Filename.concat (Sys.getenv "HOME") "picosoc_build" in
  (try Unix.mkdir build 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let beh_path = Filename.concat build "picosoc_beh.v" in
  let gate_path = Filename.concat build "picosoc_gate.v" in
  let gates_path = Filename.concat build "picosoc_gates.v" in
  (* Behavioral Verilog: yosys sees native RTL ($mux/$and/...) so scc/check
   * can trace the combinational loop and name the wires. *)
  (try
     let oc = Stdlib.open_out beh_path in
     Stdlib.Fun.protect
       ~finally:(fun () -> Stdlib.close_out oc)
       (fun () -> Rtl.output ~output_mode:(To_channel oc) Verilog circ);
     Printf.eprintf "[gates] wrote %s\n%!" beh_path
   with e -> Printf.eprintf "[gates] Rtl.output failed: %s\n%!" (Printexc.to_string e));
  (* xsim miter co-sim needs a renamed module so it can sit alongside
     the gold instance with a distinct name. Emit the same content as
     picosoc_beh.v but with `<top>` renamed to `<top>_gate`. *)
  (try
     let ic = open_in beh_path in
     let oc = Stdlib.open_out gate_path in
     let re = Str.regexp_string ("module " ^ top ^ " ") in
     let replace = "module " ^ top ^ "_gate " in
     Stdlib.Fun.protect
       ~finally:(fun () -> Stdlib.close_out oc; close_in ic)
       (fun () ->
          try
            while true do
              let line = input_line ic in
              output_string oc (Str.global_replace re replace line);
              output_char oc '\n'
            done
          with End_of_file -> ());
     Printf.eprintf "[gates] wrote %s (module renamed to %s_gate)\n%!"
       gate_path top
   with e -> Printf.eprintf "[gates] gate-rename failed: %s\n%!" (Printexc.to_string e));
  let nl = Lib_map.map_circuit circ in
  ignore (Cell_verilog_emit.emit_to_file ~module_name:top nl gates_path);
  Printf.eprintf "[gates] wrote %s\n%!" gates_path
