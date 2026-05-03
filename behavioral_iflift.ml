(* Lift `if` statements inside always blocks to dataflow form.
 *
 * Two rewrites, both bottom-up so nested ifs collapse into a chain
 * of ternary expressions:
 *
 *   if (cond) lhs <= d;            (no else branch — enable FF)
 *      ──→  lhs <= cond ? d : lhs;
 *
 *   if (cond) lhs <= a;
 *   else      lhs <= b;            (both branches assign same lhs)
 *      ──→  lhs <= cond ? a : b;
 *
 * Nested form
 *
 *   if (p) lhs <= x;
 *   else if (q) lhs <= y;
 *   else        lhs <= z;
 *
 * is processed inside-out: the inner `if (q) ...; else ...;` becomes
 * `lhs <= q ? y : z;`, then the outer `if (p) ...; else ...;` matches
 * the second rule because both legs are now single BAssigns to the
 * same lhs, producing `lhs <= p ? x : (q ? y : z);`.
 *
 * The pass also handles the BBlock-around-single-BAssign shape
 * Verilator emits for `begin ... end` arms.
 *
 * Targets: BSequential bodies (the canonical case the user asked for)
 * and BCombinational bodies (same encoding lifts combinational `if`
 * to a ternary; in that context a missing else implies a latch, which
 * `BCond cond d (BVar lhs)` faithfully captures — `lhs` keeps its
 * previous value when `cond` is false). *)

open Behavioral_ir

(* Strip a wrapping BBlock around a singleton statement. Verilator
 * emits `begin <stmt>; end` as BBlock [stmt]; we want to peer through
 * it for the rewrite rules. *)
let rec unwrap_block = function
  | BBlock [s] -> unwrap_block s
  | BBlock [] -> BBlock []
  | s -> s

(* Try to match a single-assign body and return (lhs, rhs). *)
let single_assign s =
  match unwrap_block s with
  | BAssign { lhs; rhs } -> Some (lhs, rhs)
  | _ -> None

let rec lift_stmt s = match s with
  | BIf { condition; then_stmts; else_stmts } ->
      let then' = List.map lift_stmt then_stmts in
      let else' = List.map lift_stmt else_stmts in
      let lifted () = BIf { condition; then_stmts = then'; else_stmts = else' } in
      (* Both branches a single assign to the same lhs ⇒ ternary. *)
      (match then', else' with
       | [t], [e] ->
           (match single_assign t, single_assign e with
            | Some (lt, rt), Some (le, re) when lt = le ->
                BAssign { lhs = lt;
                          rhs = BCond { condition;
                                        then_val = rt;
                                        else_val = re } }
            | _ -> lifted ())
       (* Then branch assigns, no else ⇒ enable FF / latch. *)
       | [t], [] ->
           (match single_assign t with
            | Some (lt, rt) ->
                BAssign { lhs = lt;
                          rhs = BCond { condition;
                                        then_val = rt;
                                        else_val = BVar lt } }
            | None -> lifted ())
       (* Symmetric: empty then with non-empty else. Inverts the
        * condition so the enable-FF encoding still applies. *)
       | [], [e] ->
           (match single_assign e with
            | Some (le, re) ->
                BAssign { lhs = le;
                          rhs = BCond {
                            condition = BUnOp { op = BNot;
                                                operand = condition;
                                                result_type = BBool };
                            then_val = re;
                            else_val = BVar le } }
            | None -> lifted ())
       | _ -> lifted ())
  | BBlock stmts -> BBlock (List.map lift_stmt stmts)
  | BCase { selector; cases; default } ->
      BCase { selector;
              cases = List.map (fun (k, ss) ->
                (k, List.map lift_stmt ss)) cases;
              default = List.map lift_stmt default }
  | BWhile { condition; body } ->
      BWhile { condition; body = List.map lift_stmt body }
  | BFor { init; condition; update; body } ->
      BFor { init = lift_stmt init; condition; update = lift_stmt update;
             body = List.map lift_stmt body }
  | other -> other

let lift_process = function
  | BCombinational c ->
      BCombinational { c with body = List.map lift_stmt c.body }
  | BSequential s ->
      BSequential { s with body = List.map lift_stmt s.body }

let lift_module (m : bmodule) =
  { m with processes = List.map lift_process m.processes }

let lift_program (p : bprogram) =
  { p with modules = List.map lift_module p.modules }
