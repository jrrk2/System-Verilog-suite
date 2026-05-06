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
     gate level. *)
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
    |> Behavioral_flatten.flatten_program
  in

  (* Prefer the named top.  If we got >1 module, refuse for now —
     hierarchical synth lives in the next ticket. *)
  let n_mods = List.length prog.modules in
  if n_mods > 1 then
    fail (Printf.sprintf "%d modules after elab, hierarchical synth not yet wired" n_mods);

  let m =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
            prog.modules with
    | Some m -> m
    | None -> fail (Printf.sprintf "top %s not found in Verible output" top)
  in

  (* If the module has Inst sub-modules in its body, the lib_map walker
     would skip them silently — that's a wrong netlist.  Hierarchical
     synth (#103a) handles this case; for now refuse loudly so we
     notice the gap. *)
  if m.instances <> [] then
    fail (Printf.sprintf "%d submodule instance(s) — needs flatten" (List.length m.instances));

  (try
     let circuit = Behavioral_to_hardcaml.create_circuit m in
     let netlist = Lib_map.map_circuit circuit in
     let n_cells = List.length netlist.insts in
     if n_cells = 0 then fail "no cells emitted";
     let _ = Cell_verilog_emit.emit_to_file ~module_name:m.name netlist out_path in
     Printf.eprintf "[synth_orfs_shim] OK — %d cells, %s\n"
       n_cells (Cell_verilog_emit.summary netlist
                |> String.split_on_char '\n' |> List.hd);
     Printf.eprintf "[synth_orfs_shim] wrote %s\n" out_path;
     exit 0
   with e ->
     fail (Printf.sprintf "lowering failed: %s" (Printexc.to_string e)))
