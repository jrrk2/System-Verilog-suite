(* Loop unrolling pass on Behavioral IR.
 *
 * Targets two shapes:
 *
 *   BFor { init; condition; update; body }
 *
 *   BBlock [
 *     BAssign { lhs = i; rhs = <const init> };
 *     BWhile { condition; body = body @ [BAssign { lhs = i; rhs = <update> }] };
 *   ]
 *
 * The second is what Verilator emits for `for (int i = 0; i < N; i++) ...`
 * (init / update / condition all visible as plain BIR), so we recognise
 * it and treat it as a BFor for unrolling purposes. The induction
 * variable is constant-substituted into the body for each iteration.
 *
 * Constant requirement: every iteration's bound (init value, condition
 * compared against, update step) must evaluate to a known integer
 * literal. If any leg is data-dependent we leave the loop alone.
 *
 * Iteration limit: hard cap of MAX_UNROLL iterations to prevent
 * runaway. Loops exceeding the cap remain as BWhile/BFor — the formal
 * checker downstream will give up cleanly. *)

open Behavioral_ir

let max_unroll = 1024

(* Substitute occurrences of variable `var` with constant `value` in
 * an expression. Used to specialise the loop body for one specific
 * induction-variable value. *)
let rec subst_expr var value = function
  | BVar n when n = var -> BConst { value = Z.of_int value; width = 32 }
  | BVar _ as e -> e
  | BConst _ as e -> e
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op;
               lhs = subst_expr var value lhs;
               rhs = subst_expr var value rhs;
               result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op; operand = subst_expr var value operand; result_type }
  | BSelect { array; index } ->
      BSelect { array = subst_expr var value array;
                index = subst_expr var value index }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = subst_expr var value signal; msb; lsb }
  | BConcat es -> BConcat (List.map (subst_expr var value) es)
  | BReplicate { count; value = v } ->
      BReplicate { count; value = subst_expr var value v }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = subst_expr var value condition;
              then_val = subst_expr var value then_val;
              else_val = subst_expr var value else_val }
  | BCall { func; args } ->
      BCall { func; args = List.map (subst_expr var value) args }

let rec subst_stmt var value = function
  | BAssign { lhs; rhs } ->
      BAssign { lhs; rhs = subst_expr var value rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition = subst_expr var value condition;
            then_stmts = List.map (subst_stmt var value) then_stmts;
            else_stmts = List.map (subst_stmt var value) else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = subst_expr var value selector;
              cases = List.map (fun (e, ss) ->
                (subst_expr var value e,
                 List.map (subst_stmt var value) ss)) cases;
              default = List.map (subst_stmt var value) default }
  | BWhile { condition; body } ->
      BWhile { condition = subst_expr var value condition;
               body = List.map (subst_stmt var value) body }
  | BFor { init; condition; update; body } ->
      BFor { init = subst_stmt var value init;
             condition = subst_expr var value condition;
             update = subst_stmt var value update;
             body = List.map (subst_stmt var value) body }
  | BBlock ss -> BBlock (List.map (subst_stmt var value) ss)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (subst_expr var value) args }
  | BReturn None -> BReturn None
  | BReturn (Some e) -> BReturn (Some (subst_expr var value e))

(* Recover an integer literal from an expression after constant-
 * folding the trivial cases. Returns None for anything dynamic. *)
let rec const_int = function
  | BConst { value; _ } -> Some (Z.to_int value)
  | BBinOp { op; lhs; rhs; _ } ->
      (match const_int lhs, const_int rhs with
       | Some l, Some r ->
           (match op with
            | BAdd -> Some (l + r) | BSub -> Some (l - r)
            | BMul -> Some (l * r)
            | BDiv when r <> 0 -> Some (l / r)
            | BMod when r <> 0 -> Some (l mod r)
            | BAnd -> Some (l land r) | BOr -> Some (l lor r)
            | BXor -> Some (l lxor r)
            | BShl -> Some (l lsl r) | BShr -> Some (l lsr r)
            | BAshr -> Some (l asr r)
            | _ -> None)
       | _ -> None)
  | BUnOp { op = BNot; operand; _ } ->
      Option.map (fun v -> lnot v) (const_int operand)
  | BUnOp { op = BNeg; operand; _ } ->
      Option.map (fun v -> -v) (const_int operand)
  | _ -> None

let eval_cond i_var i_val cond =
  let cond' = subst_expr i_var i_val cond in
  match cond' with
  | BBinOp { op; lhs; rhs; _ } ->
      (match const_int lhs, const_int rhs with
       | Some l, Some r ->
           (match op with
            | BLt -> Some (l < r)  | BLe -> Some (l <= r)
            | BGt -> Some (l > r)  | BGe -> Some (l >= r)
            | BEq -> Some (l = r)  | BNe -> Some (l <> r)
            | _ -> None)
       | _ -> None)
  | BConst { value; _ } -> Some (not (Z.equal value Z.zero))
  | _ -> None

(* If `update` is `i := <const expr in i>`, return a function that
 * advances the induction variable. Handles `i+1`, `i-1`, `i+k`
 * shapes. Returns None for anything more complex. *)
let advance_step i_var = function
  | BAssign { lhs; rhs } when lhs = i_var ->
      let folded = match rhs with
        | BBinOp { op; lhs = a; rhs = b; _ } ->
            (match a, const_int b with
             | BVar n, Some k when n = i_var ->
                 (match op with
                  | BAdd -> Some (`Add k)
                  | BSub -> Some (`Add (-k))
                  | _ -> None)
             | _ ->
                 (match const_int a, b with
                  | Some k, BVar n when n = i_var ->
                      (match op with
                       | BAdd -> Some (`Add k)
                       | _ -> None)
                  | _ -> None))
        | _ -> None
      in
      (match folded with
       | Some (`Add k) -> Some (fun v -> v + k)
       | None -> None)
  | _ -> None

(* Try to unroll a single BFor. Returns Some unrolled list of stmts,
 * or None if any leg is non-constant or the iteration count exceeds
 * max_unroll. *)
let try_unroll_for ~init ~condition ~update ~body =
  match init with
  | BAssign { lhs = i_var; rhs = init_rhs } ->
      (match const_int init_rhs, advance_step i_var update with
       | Some i0, Some step ->
           let rec loop i acc count =
             if count > max_unroll then None
             else match eval_cond i_var i condition with
               | Some false -> Some (List.rev acc)
               | Some true ->
                   let unrolled =
                     List.map (subst_stmt i_var i) body in
                   loop (step i) (List.rev_append unrolled acc) (count + 1)
               | None -> None
           in
           loop i0 [] 0
       | _ -> None)
  | _ -> None

(* Drop empty BBlocks: they appear from variable declarations the
 * Verilator → BIR converter strips, and they confuse the
 * `BAssign :: BWhile` pattern match below. *)
let drop_empty_blocks =
  List.filter (function BBlock [] -> false | _ -> true)

(* Match the `init; while (cond) { body; update }` shape Verilator
 * emits for `for` loops, then attempt unroll. *)
let try_unroll_while_seq stmts =
  match drop_empty_blocks stmts with
  | (BAssign { lhs; _ } as init) :: BWhile { condition; body } :: rest ->
      let i_var = lhs in
      (* Last stmt of the while body must be `i := i + k`. *)
      let body_rev = List.rev body in
      (match body_rev with
       | upd :: body_inner_rev ->
           let body_inner = List.rev body_inner_rev in
           (match try_unroll_for ~init ~condition ~update:upd
                                 ~body:body_inner with
            | Some unrolled ->
                ignore i_var;
                Some (unrolled @ rest)
            | None -> None)
       | [] -> None)
  | _ -> None

(* Walk all statements; recursively unroll where possible. *)
let rec unroll_stmt = function
  | BFor { init; condition; update; body } ->
      let body' = List.map unroll_stmt body in
      (match try_unroll_for ~init ~condition ~update ~body:body' with
       | Some unrolled -> BBlock unrolled
       | None ->
           BFor { init; condition; update; body = body' })
  | BBlock stmts ->
      let stmts' = List.map unroll_stmt stmts in
      (match try_unroll_while_seq stmts' with
       | Some unrolled -> BBlock unrolled
       | None -> BBlock stmts')
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition;
            then_stmts = List.map unroll_stmt then_stmts;
            else_stmts = List.map unroll_stmt else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector;
              cases = List.map (fun (e, ss) ->
                (e, List.map unroll_stmt ss)) cases;
              default = List.map unroll_stmt default }
  | BWhile { condition; body } ->
      (* Bare while without recognisable bounds — leave it. *)
      BWhile { condition; body = List.map unroll_stmt body }
  | other -> other

let unroll_process = function
  | BCombinational c ->
      BCombinational { c with body = List.map unroll_stmt c.body }
  | BSequential s ->
      BSequential { s with body = List.map unroll_stmt s.body }

let unroll_func (f : bfunc) =
  { f with body = List.map unroll_stmt f.body }

let unroll_module (m : bmodule) =
  { m with
    processes = List.map unroll_process m.processes;
    funcs = List.map unroll_func m.funcs }

let unroll_program (p : bprogram) =
  { p with modules = List.map unroll_module p.modules }
