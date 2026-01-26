(* Constant Propagation for Behavioral IR
 *
 * Propagates compile-time constants through the IR and folds constant expressions.
 *
 * Example:
 *   Before:
 *     x := 5
 *     y := x + 3      -- x is known to be 5
 *     z := y * 2      -- y is known to be 8
 *
 *   After:
 *     x := 5
 *     y := 8          -- Folded at compile time
 *     z := 16         -- Folded at compile time
 *)

open Behavioral_ir

(* Constant value tracked during propagation *)
type const_value =
  | CInt of int * int  (* value, width *)
  | CBool of bool
  | CUnknown

(* Propagation context *)
type prop_context = {
  (* Known constant values for variables *)
  constants: (string, const_value) Hashtbl.t;
  mutable changes: int;  (* Track number of optimizations *)
}

let create_prop_context () = {
  constants = Hashtbl.create 50;
  changes = 0;
}

(* Evaluate binary operation on constants *)
let eval_binop op v1 v2 =
  match (op, v1, v2) with
  (* Arithmetic *)
  | (BAdd, CInt (a, w1), CInt (b, w2)) ->
      let width = max w1 w2 in
      CInt (a + b, width)
  | (BSub, CInt (a, w1), CInt (b, w2)) ->
      let width = max w1 w2 in
      CInt (a - b, width)
  | (BMul, CInt (a, w1), CInt (b, w2)) ->
      let width = w1 + w2 in
      CInt (a * b, width)
  | (BDiv, CInt (a, w), CInt (b, _)) when b <> 0 ->
      CInt (a / b, w)
  | (BMod, CInt (a, w), CInt (b, _)) when b <> 0 ->
      CInt (a mod b, w)

  (* Bitwise *)
  | (BAnd, CInt (a, w1), CInt (b, w2)) ->
      let width = max w1 w2 in
      CInt (a land b, width)
  | (BOr, CInt (a, w1), CInt (b, w2)) ->
      let width = max w1 w2 in
      CInt (a lor b, width)
  | (BXor, CInt (a, w1), CInt (b, w2)) ->
      let width = max w1 w2 in
      CInt (a lxor b, width)

  (* Shift *)
  | (BShl, CInt (a, w), CInt (b, _)) ->
      CInt (a lsl b, w)
  | (BShr, CInt (a, w), CInt (b, _)) ->
      CInt (a lsr b, w)
  | (BAshr, CInt (a, w), CInt (b, _)) ->
      CInt (a asr b, w)

  (* Comparison *)
  | (BEq, CInt (a, _), CInt (b, _)) ->
      CBool (a = b)
  | (BNe, CInt (a, _), CInt (b, _)) ->
      CBool (a <> b)
  | (BLt, CInt (a, _), CInt (b, _)) ->
      CBool (a < b)
  | (BLe, CInt (a, _), CInt (b, _)) ->
      CBool (a <= b)
  | (BGt, CInt (a, _), CInt (b, _)) ->
      CBool (a > b)
  | (BGe, CInt (a, _), CInt (b, _)) ->
      CBool (a >= b)

  (* Boolean *)
  | (BAnd, CBool a, CBool b) -> CBool (a && b)
  | (BOr, CBool a, CBool b) -> CBool (a || b)
  | (BXor, CBool a, CBool b) -> CBool (a <> b)

  | _ -> CUnknown

(* Evaluate unary operation on constant *)
let eval_unop op v =
  match (op, v) with
  | (BNot, CBool b) -> CBool (not b)
  | (BNot, CInt (a, w)) -> CInt (lnot a, w)
  | (BNeg, CInt (a, w)) -> CInt (-a, w)
  | (BRedAnd, CInt (a, w)) ->
      (* Reduction AND: all bits set? *)
      CBool (a = (1 lsl w) - 1)
  | (BRedOr, CInt (a, _)) ->
      (* Reduction OR: any bit set? *)
      CBool (a <> 0)
  | (BRedXor, CInt (a, w)) ->
      (* Reduction XOR: odd parity? *)
      let rec count_bits n acc =
        if n = 0 then acc
        else count_bits (n lsr 1) (acc + (n land 1))
      in
      CBool ((count_bits a 0) mod 2 = 1)
  | _ -> CUnknown

(* Convert constant value to expression *)
let const_to_expr = function
  | CInt (value, width) -> BConst { value; width }
  | CBool true -> BConst { value = 1; width = 1 }
  | CBool false -> BConst { value = 0; width = 1 }
  | CUnknown -> failwith "Cannot convert unknown to expression"

(* Convert expression to constant value *)
let expr_to_const ctx = function
  | BConst { value; width } -> CInt (value, width)
  | BVar var ->
      (try Hashtbl.find ctx.constants var with Not_found -> CUnknown)
  | _ -> CUnknown

(* Propagate constants through expression *)
let rec propagate_expr ctx = function
  | BVar var as v ->
      (match try Hashtbl.find ctx.constants var with Not_found -> CUnknown with
       | CUnknown -> v
       | const_val ->
           ctx.changes <- ctx.changes + 1;
           const_to_expr const_val)

  | BConst _ as c -> c

  | BBinOp { op; lhs; rhs; result_type } ->
      let lhs' = propagate_expr ctx lhs in
      let rhs' = propagate_expr ctx rhs in

      (* Try to evaluate if both operands are constant *)
      let lhs_const = expr_to_const ctx lhs' in
      let rhs_const = expr_to_const ctx rhs' in

      (match eval_binop op lhs_const rhs_const with
       | CUnknown ->
           (* Cannot fold, return expression with propagated operands *)
           BBinOp { op; lhs = lhs'; rhs = rhs'; result_type }
       | const_val ->
           ctx.changes <- ctx.changes + 1;
           const_to_expr const_val)

  | BUnOp { op; operand; result_type } ->
      let operand' = propagate_expr ctx operand in
      let operand_const = expr_to_const ctx operand' in

      (match eval_unop op operand_const with
       | CUnknown ->
           BUnOp { op; operand = operand'; result_type }
       | const_val ->
           ctx.changes <- ctx.changes + 1;
           const_to_expr const_val)

  | BSelect { array; index } ->
      BSelect {
        array = propagate_expr ctx array;
        index = propagate_expr ctx index;
      }

  | BSlice { signal; msb; lsb } ->
      BSlice {
        signal = propagate_expr ctx signal;
        msb;
        lsb;
      }

  | BConcat exprs ->
      BConcat (List.map (propagate_expr ctx) exprs)

  | BReplicate { count; value } ->
      BReplicate {
        count;
        value = propagate_expr ctx value;
      }

  | BCond { condition; then_val; else_val } ->
      let condition' = propagate_expr ctx condition in

      (* Try to evaluate condition *)
      (match expr_to_const ctx condition' with
       | CBool true ->
           ctx.changes <- ctx.changes + 1;
           propagate_expr ctx then_val
       | CBool false ->
           ctx.changes <- ctx.changes + 1;
           propagate_expr ctx else_val
       | _ ->
           BCond {
             condition = condition';
             then_val = propagate_expr ctx then_val;
             else_val = propagate_expr ctx else_val;
           })

  | BCall { func; args } ->
      BCall {
        func;
        args = List.map (propagate_expr ctx) args;
      }

(* Propagate constants through statement *)
let rec propagate_stmt ctx = function
  | BAssign { lhs; rhs } ->
      let rhs' = propagate_expr ctx rhs in

      (* Track constant assignments *)
      let const_val = expr_to_const ctx rhs' in
      if const_val <> CUnknown then
        Hashtbl.replace ctx.constants lhs const_val;

      BAssign { lhs; rhs = rhs' }

  | BIf { condition; then_stmts; else_stmts } ->
      let condition' = propagate_expr ctx condition in

      (* Try to eliminate dead branches *)
      (match expr_to_const ctx condition' with
       | CBool true ->
           ctx.changes <- ctx.changes + 1;
           BBlock (List.map (propagate_stmt ctx) then_stmts)
       | CBool false ->
           ctx.changes <- ctx.changes + 1;
           BBlock (List.map (propagate_stmt ctx) else_stmts)
       | _ ->
           (* Cannot eliminate, propagate both branches *)
           let saved_constants = Hashtbl.copy ctx.constants in

           let then_stmts' = List.map (propagate_stmt ctx) then_stmts in

           Hashtbl.clear ctx.constants;
           Hashtbl.iter (fun k v -> Hashtbl.add ctx.constants k v) saved_constants;

           let else_stmts' = List.map (propagate_stmt ctx) else_stmts in

           BIf { condition = condition'; then_stmts = then_stmts'; else_stmts = else_stmts' })

  | BCase { selector; cases; default } ->
      let selector' = propagate_expr ctx selector in

      let cases' = List.map (fun (value, stmts) ->
        let value' = propagate_expr ctx value in
        let stmts' = List.map (propagate_stmt ctx) stmts in
        (value', stmts')
      ) cases in

      let default' = List.map (propagate_stmt ctx) default in

      BCase { selector = selector'; cases = cases'; default = default' }

  | BWhile { condition; body } ->
      let condition' = propagate_expr ctx condition in
      let body' = List.map (propagate_stmt ctx) body in
      BWhile { condition = condition'; body = body' }

  | BFor { init; condition; update; body } ->
      let init' = propagate_stmt ctx init in
      let condition' = propagate_expr ctx condition in
      let body' = List.map (propagate_stmt ctx) body in
      let update' = propagate_stmt ctx update in
      BFor { init = init'; condition = condition'; update = update'; body = body' }

  | BBlock stmts ->
      BBlock (List.map (propagate_stmt ctx) stmts)

  | BCallStmt { func; args } ->
      BCallStmt {
        func;
        args = List.map (propagate_expr ctx) args;
      }

  | BReturn (Some expr) ->
      BReturn (Some (propagate_expr ctx expr))

  | BReturn None -> BReturn None

(* Propagate constants through process *)
let propagate_process = function
  | BCombinational { name; sensitivity; body } ->
      let ctx = create_prop_context () in
      let body' = List.map (propagate_stmt ctx) body in
      (BCombinational { name; sensitivity; body = body' }, ctx.changes)

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body } ->
      let ctx = create_prop_context () in
      let body' = List.map (propagate_stmt ctx) body in
      (BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body = body' }, ctx.changes)

(* Propagate constants through module *)
let propagate_module bmod =
  let total_changes = ref 0 in
  let processes' = List.map (fun proc ->
    let (proc', changes) = propagate_process proc in
    total_changes := !total_changes + changes;
    proc'
  ) bmod.processes in

  ({ bmod with processes = processes' }, !total_changes)

(* Propagate constants through program *)
let propagate_program prog =
  let total_changes = ref 0 in
  let modules' = List.map (fun bmod ->
    let (bmod', changes) = propagate_module bmod in
    total_changes := !total_changes + changes;
    bmod'
  ) prog.modules in

  ({ modules = modules'; library_cells = prog.library_cells }, !total_changes)

(* Iteratively propagate until fixed point *)
let propagate_to_fixpoint prog =
  let rec iterate prog iterations =
    let (prog', changes) = propagate_program prog in
    Printf.printf "Constant propagation iteration %d: %d changes\n" iterations changes;
    if changes = 0 then
      (prog', iterations)
    else
      iterate prog' (iterations + 1)
  in
  iterate prog 1
