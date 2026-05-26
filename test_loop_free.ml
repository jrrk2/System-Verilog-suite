(* Loop-detection regression driver.
 *
 * Runs each tests/operators/op_*.v through the same BIR pipeline
 * test_picosoc_gates uses, then builds a hardcaml Circuit with
 * `~detect_loops:true` and reports per-file PASS/FAIL.  Designed to
 * catch a regression in the behavioral_ssa @slice_write /
 * @part_sel_write_* lowering — if those ever stop versioning the
 * target, the unrolled slice-write chain in op_unrolled_slice_chain
 * (and any future seed like it) closes a structural cycle through
 * Always.Variable.value var and hardcaml's loop detector raises.
 *
 * Usage: ./test_loop_free.exe [path/to/op_X.v ...]
 *        defaults to tests/operators/op_*.v *)

let pipeline ~top files =
  let prog = Verible_to_behavioral.convert_files ~top files in
  let prog = { Behavioral_ir.modules = prog.modules;
               library_cells = prog.library_cells } in
  let prog = prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program in
  let lowered, _ = Behavioral_memlower.lower_program prog in
  { lowered with
    modules = List.map Behavioral_ssa.module_to_ssa lowered.modules }

let run_one file =
  let base = Filename.basename file in
  let top = Filename.chop_extension base in
  Printf.printf "── %-30s " top;
  flush stdout;
  try
    let lowered = pipeline ~top [file] in
    let m = List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top)
              lowered.modules in
    let _circ =
      Behavioral_to_hardcaml.create_circuit ~emit_instances:false
        ~detect_loops:true m in
    Printf.printf "PASS\n%!"; true
  with
  | e ->
      Printf.printf "FAIL\n  %s\n%!"
        (Printexc.to_string e |> fun s ->
           String.sub s 0 (min (String.length s) 240));
      false

let () =
  let files =
    let argv = Array.to_list Sys.argv in
    match List.tl argv with
    | [] ->
        let dir = "tests/operators" in
        Sys.readdir dir
        |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".v"
                              && String.length f > 3
                              && String.sub f 0 3 = "op_")
        |> List.sort compare
        |> List.map (fun f -> Filename.concat dir f)
    | xs -> xs
  in
  Printf.printf "═══ Loop-free regression: hardcaml Circuit detect_loops:true ═══\n";
  let results = List.map run_one files in
  let pass = List.length (List.filter (fun b -> b) results) in
  let fail = List.length results - pass in
  Printf.printf "\n── %d passed / %d failed ──\n" pass fail;
  if fail > 0 then exit 1
