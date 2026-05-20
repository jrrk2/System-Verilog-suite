(* Verilator JSON ↔ Verible parse-tree miter.
 *
 * Both paths take SV source as input and produce a Behavioral_ir
 * bmodule via fully software-only frontends — no Yosys synthesis or
 * Vivado in the loop. Useful as a parse-and-elaborate sanity check
 * across two independent SV implementations.
 *
 * Usage:
 *   test_verilator_vs_verible <top> <file.sv> [more.sv ...] *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

let run_verilator ~top ~files =
  let mdir = Filename.concat (Filename.get_temp_dir_name ())
               (Printf.sprintf "miter_vlt_vrb_%s_%d" top (Unix.getpid ())) in
  let _ = Sys.command (Printf.sprintf "rm -rf %s" mdir) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote mdir)) in
  let files_str = String.concat " " (List.map Filename.quote files) in
  let log = Filename.concat mdir "verilator.log" in
  let cmd =
    Printf.sprintf
      "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
       -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING \
       -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
       --top-module %s %s --Mdir %s > %s 2>&1"
      (Filename.quote top) files_str (Filename.quote mdir) (Filename.quote log)
  in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    Printf.eprintf "verilator failed (rc=%d), see %s\n" rc log;
    exit 1
  end;
  Filename.concat mdir (Printf.sprintf "V%s.tree.json" top)

(* Run the same downstream passes both sides need before the miter:
 * loop unrolling, function/task inlining, if-lifting, memory inference.
 * Without these, V↔V on designs that use functions, tasks, or
 * constant-bound for loops mismatches because the raw BIR still
 * contains BCallStmt/BCall and BFor nodes that ffrip can't see
 * through. *)
let apply_passes (p : Behavioral_ir.bprogram) =
  p
  |> Behavioral_unroll.unroll_program
  |> Behavioral_inline.inline_program
  |> Behavioral_iflift.lift_program
  |> Behavioral_meminfer.infer_program

(* Constant-fold through flip-flops. Verilator's optimiser already
 * does this; without it, a constant-driven FF on the Verible side
 * survives into ffrip and produces phantom Q/Q__D ports that the
 * Verilator side doesn't have. *)
let fold_ff_consts (p : Behavioral_ir.bprogram) =
  Behavioral_const.fold_ffs_program p

(* Truncate/zero-extend each BCall actual to match the formal width
 * declared in the function. Verilator's JSON pre-folds this cast;
 * Verible's parse-tree doesn't, so without the pass the two sides
 * mint distinct uninterpreted-function decls for the same call. *)
let normalize_bcall_args (p : Behavioral_ir.bprogram) =
  Behavioral_const.normalize_bcall_args_program p

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Verilator JSON ↔ Verible parse-tree miter: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] Verilator → BIR ...\n%!";
  let json = run_verilator ~top ~files in
  let vlt_prog =
    match Verilator_to_behavioral.convert_verilator_json_to_behavioral json with
    | Some p -> p
    | None ->
        Printf.eprintf "verilator-side BIR conversion failed\n";
        exit 1
  in
  let vlt_prog = vlt_prog |> fold_ff_consts |> normalize_bcall_args in
  Printf.printf "  %d modules\n" (List.length vlt_prog.modules);

  Printf.printf "[2/3] Verible → BIR ...\n%!";
  let vrb_prog = Verible_to_behavioral.convert_files ~top files in
  let vrb_prog = vrb_prog |> fold_ff_consts |> normalize_bcall_args in
  Printf.printf "  %d modules\n" (List.length vrb_prog.modules);
  ignore apply_passes;

  let pick label src =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
            src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s side: no module '%s'. Available: %s\n"
          label top
          (String.concat ", "
             (List.map (fun (m : Behavioral_ir.bmodule) -> m.name) src));
        exit 1
  in
  let vlt_top = pick "verilator" vlt_prog.modules in
  let vrb_top = pick "verible"   vrb_prog.modules in

  Printf.printf "[3/3] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence vlt_top vrb_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (Verilator JSON ≡ Verible parse-tree)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
