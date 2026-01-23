(* Dead Code Elimination for Behavioral IR
 *
 * Removes unused variable assignments and unreachable code.
 *
 * FIXED: Now tracks signal usage across ALL processes to prevent
 * aggressive elimination of signals used in other processes.
 *)

open Behavioral_ir

(* String set module *)
module StringSet = Set.Make(String)

(* Track variable uses and definitions *)
type liveness_info = {
  mutable used: StringSet.t;
  mutable defined: StringSet.t;
  mutable live_in: StringSet.t;
  mutable live_out: StringSet.t;
}

(* Collect used variables in expression *)
let rec collect_uses_expr = function
  | BVar var -> StringSet.singleton var
  | BConst _ -> StringSet.empty
  | BBinOp { lhs; rhs; _ } ->
      StringSet.union (collect_uses_expr lhs) (collect_uses_expr rhs)
  | BUnOp { operand; _ } ->
      collect_uses_expr operand
  | BSelect { array; index } ->
      StringSet.union (collect_uses_expr array) (collect_uses_expr index)
  | BSlice { signal; _ } ->
      collect_uses_expr signal
  | BConcat exprs ->
      List.fold_left (fun acc expr ->
        StringSet.union acc (collect_uses_expr expr)
      ) StringSet.empty exprs
  | BReplicate { value; _ } ->
      collect_uses_expr value
  | BCond { condition; then_val; else_val } ->
      StringSet.union (collect_uses_expr condition)
        (StringSet.union (collect_uses_expr then_val) (collect_uses_expr else_val))
  | BCall { args; _ } ->
      List.fold_left (fun acc expr ->
        StringSet.union acc (collect_uses_expr expr)
      ) StringSet.empty args

(* Collect uses and defs in statement *)
let rec collect_uses_defs_stmt stmt =
  match stmt with
  | BAssign { lhs; rhs } ->
      let uses = collect_uses_expr rhs in
      let defs = StringSet.singleton lhs in
      (uses, defs)

  | BIf { condition; then_stmts; else_stmts } ->
      let cond_uses = collect_uses_expr condition in
      let (then_uses, then_defs) = List.fold_left (fun (u, d) stmt ->
        let (su, sd) = collect_uses_defs_stmt stmt in
        (StringSet.union u su, StringSet.union d sd)
      ) (StringSet.empty, StringSet.empty) then_stmts in

      let (else_uses, else_defs) = List.fold_left (fun (u, d) stmt ->
        let (su, sd) = collect_uses_defs_stmt stmt in
        (StringSet.union u su, StringSet.union d sd)
      ) (StringSet.empty, StringSet.empty) else_stmts in

      let uses = StringSet.union cond_uses (StringSet.union then_uses else_uses) in
      let defs = StringSet.union then_defs else_defs in
      (uses, defs)

  | BCase { selector; cases; default } ->
      let sel_uses = collect_uses_expr selector in

      let (case_uses, case_defs) = List.fold_left (fun (u, d) (value, stmts) ->
        let val_uses = collect_uses_expr value in
        let (stmt_uses, stmt_defs) = List.fold_left (fun (su, sd) stmt ->
          let (ssu, ssd) = collect_uses_defs_stmt stmt in
          (StringSet.union su ssu, StringSet.union sd ssd)
        ) (StringSet.empty, StringSet.empty) stmts in
        (StringSet.union u (StringSet.union val_uses stmt_uses),
         StringSet.union d stmt_defs)
      ) (StringSet.empty, StringSet.empty) cases in

      let (default_uses, default_defs) = List.fold_left (fun (u, d) stmt ->
        let (su, sd) = collect_uses_defs_stmt stmt in
        (StringSet.union u su, StringSet.union d sd)
      ) (StringSet.empty, StringSet.empty) default in

      let uses = StringSet.union sel_uses (StringSet.union case_uses default_uses) in
      let defs = StringSet.union case_defs default_defs in
      (uses, defs)

  | BWhile { condition; body } | BFor { condition; body; _ } ->
      let cond_uses = collect_uses_expr condition in
      let (body_uses, body_defs) = List.fold_left (fun (u, d) stmt ->
        let (su, sd) = collect_uses_defs_stmt stmt in
        (StringSet.union u su, StringSet.union d sd)
      ) (StringSet.empty, StringSet.empty) body in
      (StringSet.union cond_uses body_uses, body_defs)

  | BBlock stmts ->
      List.fold_left (fun (u, d) stmt ->
        let (su, sd) = collect_uses_defs_stmt stmt in
        (StringSet.union u su, StringSet.union d sd)
      ) (StringSet.empty, StringSet.empty) stmts

  | BCallStmt { args; _ } ->
      let uses = List.fold_left (fun acc expr ->
        StringSet.union acc (collect_uses_expr expr)
      ) StringSet.empty args in
      (uses, StringSet.empty)

  | BReturn (Some expr) ->
      (collect_uses_expr expr, StringSet.empty)

  | BReturn None ->
      (StringSet.empty, StringSet.empty)

(* Collect uses and defs in process *)
let collect_uses_defs_process = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      List.fold_left (fun (u, d) stmt ->
        let (su, sd) = collect_uses_defs_stmt stmt in
        (StringSet.union u su, StringSet.union d sd)
      ) (StringSet.empty, StringSet.empty) body

(* NEW: Collect module-level signal usage across ALL processes *)
let collect_module_live_signals bmod =
  (* Collect all uses and defs across all processes *)
  let (all_uses, all_defs) = List.fold_left (fun (u, d) proc ->
    let (pu, pd) = collect_uses_defs_process proc in
    (StringSet.union u pu, StringSet.union d pd)
  ) (StringSet.empty, StringSet.empty) bmod.processes in

  (* Signals that feed outputs are always live *)
  let output_signals = List.fold_left (fun acc signal ->
    match signal.direction with
    | `Output -> StringSet.add signal.name acc
    | _ -> acc
  ) StringSet.empty bmod.signals in

  (* Signals that are used but not defined in current scope are live
     (they come from inputs or other processes) *)
  let cross_process_live = all_uses in

  (* Combine: signals used anywhere + output signals *)
  let module_live = StringSet.union cross_process_live output_signals in

  (* Also include signals that are defined in one process and used in another *)
  (* This catches cases like: process1: iQ := ..., process2: Q := iQ *)
  module_live

(* NEW: Strip SSA suffixes to get original signal names *)
let strip_ssa_suffix name =
  (* Check if it's a CSE temp - keep as-is *)
  if String.length name >= 9 && String.sub name 0 9 = "_cse_temp" then
    name
  else
    (* Try to strip _N suffix *)
    try
      let last_underscore = String.rindex name '_' in
      let suffix = String.sub name (last_underscore + 1)
                              (String.length name - last_underscore - 1) in
      (* Check if suffix is all digits *)
      if String.length suffix > 0 &&
         String.for_all (fun c -> c >= '0' && c <= '9') suffix then
        String.sub name 0 last_underscore
      else
        name
    with Not_found -> name

(* Check if statement has side effects (can't be eliminated) *)
let has_side_effects = function
  | BAssign _ -> false  (* Can be eliminated if unused *)
  | BCallStmt _ -> true  (* Calls might have side effects *)
  | BReturn _ -> true    (* Must keep returns *)
  | _ -> false           (* Control flow: keep for now *)

(* Eliminate dead assignments in statement *)
let rec eliminate_dead_stmt live_vars = function
  | BAssign { lhs; rhs } as stmt ->
      (* Check both the SSA version and the original signal name *)
      let original_name = strip_ssa_suffix lhs in
      if StringSet.mem lhs live_vars || StringSet.mem original_name live_vars then
        Some stmt
      else
        None  (* Dead assignment *)

  | BIf { condition; then_stmts; else_stmts } ->
      let then_stmts' = List.filter_map (eliminate_dead_stmt live_vars) then_stmts in
      let else_stmts' = List.filter_map (eliminate_dead_stmt live_vars) else_stmts in

      (* Eliminate empty if statements *)
      if List.length then_stmts' = 0 && List.length else_stmts' = 0 then
        None
      else
        Some (BIf { condition; then_stmts = then_stmts'; else_stmts = else_stmts' })

  | BCase { selector; cases; default } ->
      let cases' = List.filter_map (fun (value, stmts) ->
        let stmts' = List.filter_map (eliminate_dead_stmt live_vars) stmts in
        if List.length stmts' = 0 then None
        else Some (value, stmts')
      ) cases in

      let default' = List.filter_map (eliminate_dead_stmt live_vars) default in

      if List.length cases' = 0 && List.length default' = 0 then
        None
      else
        Some (BCase { selector; cases = cases'; default = default' })

  | BWhile { condition; body } ->
      let body' = List.filter_map (eliminate_dead_stmt live_vars) body in
      if List.length body' = 0 then None
      else Some (BWhile { condition; body = body' })

  | BFor { init; condition; update; body } ->
      let body' = List.filter_map (eliminate_dead_stmt live_vars) body in
      if List.length body' = 0 then None
      else Some (BFor { init; condition; update; body = body' })

  | BBlock stmts ->
      let stmts' = List.filter_map (eliminate_dead_stmt live_vars) stmts in
      if List.length stmts' = 0 then None
      else Some (BBlock stmts')

  | stmt ->
      Some stmt  (* Keep other statements *)

(* Compute live variables (backwards analysis) within a process *)
let compute_live_vars stmts =
  (* Start with output variables (conservatively assume all could be outputs) *)
  let live = ref StringSet.empty in

  (* Backwards pass *)
  let rec analyze_backwards = function
    | [] -> StringSet.empty
    | stmt :: rest ->
        let live_after = analyze_backwards rest in
        let (uses, defs) = collect_uses_defs_stmt stmt in

        (* live_before = (live_after - defs) ∪ uses *)
        let live_before = StringSet.union
          (StringSet.diff live_after defs)
          uses
        in

        live := StringSet.union !live live_before;
        live_before
  in

  ignore (analyze_backwards (List.rev stmts));
  !live

(* NEW: Eliminate dead code in process with module-level liveness *)
let eliminate_dead_process_with_module_live module_live = function
  | BCombinational { name; sensitivity; body } ->
      (* Combine local liveness with module-level liveness *)
      let local_live = compute_live_vars body in
      let live_vars = StringSet.union local_live module_live in
      let body' = List.filter_map (eliminate_dead_stmt live_vars) body in
      BCombinational { name; sensitivity; body = body' }

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body } ->
      (* Combine local liveness with module-level liveness *)
      let local_live = compute_live_vars body in
      let live_vars = StringSet.union local_live module_live in
      let body' = List.filter_map (eliminate_dead_stmt live_vars) body in
      BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body = body' }

(* NEW: Eliminate dead code in module with cross-process tracking *)
let eliminate_dead_module bmod =
  (* Step 1: Collect module-level live signals *)
  let module_live = collect_module_live_signals bmod in

  (* Step 2: Eliminate dead code in each process, using module-level liveness *)
  let processes' = List.map (eliminate_dead_process_with_module_live module_live) bmod.processes in

  { bmod with processes = processes' }

(* Eliminate dead code in program *)
let eliminate_dead_program prog =
  let modules' = List.map eliminate_dead_module prog.modules in
  { modules = modules' }

(* Count eliminated statements *)
let count_stmts_process = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      let rec count = function
        | BAssign _ -> 1
        | BIf { then_stmts; else_stmts; _ } ->
            1 + List.fold_left (fun acc s -> acc + count s) 0 (then_stmts @ else_stmts)
        | BCase { cases; default; _ } ->
            let case_count = List.fold_left (fun acc (_, stmts) ->
              acc + List.fold_left (fun a s -> a + count s) 0 stmts
            ) 0 cases in
            1 + case_count + List.fold_left (fun acc s -> acc + count s) 0 default
        | BWhile { body; _ } | BFor { body; _ } ->
            1 + List.fold_left (fun acc s -> acc + count s) 0 body
        | BBlock stmts ->
            List.fold_left (fun acc s -> acc + count s) 0 stmts
        | _ -> 1
      in
      List.fold_left (fun acc s -> acc + count s) 0 body

let eliminate_dead_with_stats prog =
  let before_count = List.fold_left (fun acc bmod ->
    acc + List.fold_left (fun a p -> a + count_stmts_process p) 0 bmod.processes
  ) 0 prog.modules in

  let prog' = eliminate_dead_program prog in

  let after_count = List.fold_left (fun acc bmod ->
    acc + List.fold_left (fun a p -> a + count_stmts_process p) 0 bmod.processes
  ) 0 prog'.modules in

  let eliminated = before_count - after_count in
  Printf.printf "Dead code elimination: %d statements removed (%d → %d)\n"
    eliminated before_count after_count;

  prog'
