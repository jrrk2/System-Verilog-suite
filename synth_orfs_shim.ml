(* End-to-end synth driver for the ORFS Makefile hijack (#102).

   Reads SystemVerilog files, runs them through:
       Verible → BIR → elaborate → hardcaml.Circuit → lib_map → cell-Verilog
   and writes the resulting structural netlist to the path ORFS expects.

   Exit codes:
       0 — emitted a usable [1_2_yosys.v]
       1 — failure of any kind (parse, unsupported construct, lowering,
           emission).  Stderr says why.

   IMPORTANT: there is *no* silent fallback to yosys in this driver.
   When this exits non-zero, the ORFS make target also fails — that
   visibility is the point.  yosys remains available for explicit
   oracle/comparison runs (#101), but never as a trapdoor that hides
   bugs in our pipeline.  *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <output.v> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 1

let fail reason =
  Printf.eprintf "[synth_orfs_shim] FAIL: %s\n" reason;
  exit 1

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top      = Sys.argv.(1) in
  let out_path = Sys.argv.(2) in
  let files    = Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in

  Printf.eprintf "[synth_orfs_shim] top=%s, %d input files\n"
    top (List.length files);

  let prog = Verible_to_behavioral.convert_files ~top files in
  if prog.modules = [] then fail "Verible produced no modules";

  (* Full BIR elaboration pipeline — same passes as test_cva6_ff_diff
     uses for its dumps; gives us flat behavioural code closer to the
     gate level.  Note: flatten here does NOT kill hierarchy; it's a
     boundary-preserving wrapper-stripper.  Hierarchy is handled in
     hier_synth.synth_program. *)
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
    |> Behavioral_flatten.flatten_program
  in

  if not (List.exists
            (fun (m : Behavioral_ir.bmodule) -> m.name = top) prog.modules)
  then fail (Printf.sprintf "top %s not found in Verible output" top);

  Printf.eprintf "[synth_orfs_shim] %d module(s) after elaboration\n"
    (List.length prog.modules);

  (* Synth each module to gate level, leaves first.  Fails loudly on
     any leaf the hardcaml lowerer can't handle — surfaces real bugs
     instead of silently omitting cells. *)
  let netlists =
    try Hier_synth.synth_program prog
    with Failure msg -> fail msg
       | e -> fail (Printf.sprintf "hier_synth crashed: %s" (Printexc.to_string e))
  in

  (* Emit one Verilog file with all module blocks in dependency order
     (children first, top last).  ORFS doesn't care about order, but
     readability does. *)
  let oc = open_out out_path in
  let total_cells = ref 0 in
  let total_children = ref 0 in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    let child_insts =
      List.map (fun (c : Hier_synth.child_inst_emit) ->
        { Cell_verilog_emit.ci_module = c.ci_module;
          ci_inst = c.ci_inst;
          ci_conns = c.ci_conns }) mn.mn_child_insts in
    Cell_verilog_emit.emit_module_hier
      ~oc ~module_name:mn.mn_name
      ~real_inputs:mn.mn_real_inputs
      ~real_outputs:mn.mn_real_outputs
      ~child_insts
      mn.mn_netlist;
    total_cells := !total_cells + List.length mn.mn_netlist.insts;
    total_children := !total_children + List.length mn.mn_child_insts
  ) netlists;
  close_out oc;

  Printf.eprintf "[synth_orfs_shim] OK — %d module block(s), %d cells, %d child instances\n"
    (List.length netlists) !total_cells !total_children;
  Printf.eprintf "[synth_orfs_shim] wrote %s\n" out_path
