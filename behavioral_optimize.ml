(* Behavioral IR Optimization Pipeline
 *
 * Runs all optimization passes in the correct order to maximize benefit.
 *
 * Pipeline:
 *   1. SSA Construction - Transform to Single Static Assignment
 *   2. Constant Propagation - Fold compile-time constants
 *   3. Dead Code Elimination - Remove unused assignments
 *   4. Common Subexpression Elimination - Reuse computed values
 *   5. Register Inference - Identify registers vs wires
 *
 * This pipeline is SHARED by all language frontends (VHDL, SystemVerilog).
 * This is how we permanently fix the register inference bug!
 *)

open Behavioral_ir

(* Optimization configuration *)
type opt_config = {
  enable_ssa: bool;
  enable_const_prop: bool;
  enable_dce: bool;
  enable_cse: bool;
  enable_register_inference: bool;
  max_const_prop_iterations: int;
  verbose: bool;
}

let default_config = {
  enable_ssa = true;
  enable_const_prop = true;
  enable_dce = true;
  enable_cse = true;
  enable_register_inference = true;
  max_const_prop_iterations = 5;
  verbose = true;
}

(* Run complete optimization pipeline *)
let optimize_program ?(config=default_config) prog =
  let log msg =
    if config.verbose then Printf.printf "%s\n" msg
  in

  log "═══════════════════════════════════════════════════════════════";
  log "  Behavioral IR Optimization Pipeline";
  log "═══════════════════════════════════════════════════════════════";
  log "";

  (* Phase 1: SSA Construction *)
  let prog1 = if config.enable_ssa then begin
    log "Phase 1: SSA Construction";
    log "─────────────────────────────────────────────────────────────";
    let prog' = Behavioral_ssa.program_to_ssa prog in
    log "✅ SSA construction complete";
    log "";
    prog'
  end else prog in

  (* Phase 2: Constant Propagation (iterative) *)
  let prog2 = if config.enable_const_prop then begin
    log "Phase 2: Constant Propagation";
    log "─────────────────────────────────────────────────────────────";
    let (prog', iterations) = Behavioral_const.propagate_to_fixpoint prog1 in
    log (Printf.sprintf "✅ Constant propagation converged after %d iterations" iterations);
    log "";
    prog'
  end else prog1 in

  (* Phase 3: Dead Code Elimination *)
  let prog3 = if config.enable_dce then begin
    log "Phase 3: Dead Code Elimination";
    log "─────────────────────────────────────────────────────────────";
    let prog' = Behavioral_dce.eliminate_dead_with_stats prog2 in
    log "✅ Dead code elimination complete";
    log "";
    prog'
  end else prog2 in

  (* Phase 4: Common Subexpression Elimination *)
  let prog4 = if config.enable_cse then begin
    log "Phase 4: Common Subexpression Elimination";
    log "─────────────────────────────────────────────────────────────";
    let prog' = Behavioral_cse.apply_cse_with_stats prog3 in
    log "✅ CSE complete";
    log "";
    prog'
  end else prog3 in

  (* Phase 5: Register Inference *)
  let register_info = if config.enable_register_inference then begin
    log "Phase 5: Register Inference";
    log "─────────────────────────────────────────────────────────────";
    log "THIS IS WHERE THE BUG FIX HAPPENS!";
    log "";

    (* Analyze each module *)
    List.iter (fun bmod ->
      log (Printf.sprintf "Analyzing module: %s" bmod.name);
      let ctx = Behavioral_registers.analyze_module bmod in
      Behavioral_registers.print_register_stats ctx;
      log "";

      (* Show comparison with old buggy approach *)
      Behavioral_registers.compare_with_vhdl_bug bmod.name ctx;
    ) prog4.modules;

    log "✅ Register inference complete";
    log "";
    Some ()
  end else None in

  log "═══════════════════════════════════════════════════════════════";
  log "  Optimization Complete";
  log "═══════════════════════════════════════════════════════════════";
  log "";

  (prog4, register_info)

(* Quick optimization (no register inference, for intermediate passes) *)
let optimize_quick prog =
  let config = { default_config with
    enable_register_inference = false;
    verbose = false;
  } in
  fst (optimize_program ~config prog)

(* Full optimization with verbose output *)
let optimize_full prog =
  optimize_program ~config:default_config prog

(* Custom optimization *)
let optimize_custom config prog =
  optimize_program ~config prog
