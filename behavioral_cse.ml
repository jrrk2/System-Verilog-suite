(* Common Subexpression Elimination for Behavioral IR
 *
 * Identifies and reuses common subexpressions to reduce redundant computation.
 *
 * Example:
 *   Before:
 *     y := a + b
 *     z := a + b    -- Duplicate computation
 *     w := y * z
 *
 *   After:
 *     temp1 := a + b
 *     y := temp1
 *     z := temp1    -- Reuse computed value
 *     w := y * z
 *)

open Behavioral_ir

(* Expression hash for identifying common subexpressions *)
module ExprHash = struct
  type t = bexpr

  let rec hash = function
    | BVar name -> Hashtbl.hash ("var", name)
    | BConst { value; width } -> Hashtbl.hash ("const", value, width)
    | BBinOp { op; lhs; rhs; _ } ->
        Hashtbl.hash ("binop", op, hash lhs, hash rhs)
    | BUnOp { op; operand; _ } ->
        Hashtbl.hash ("unop", op, hash operand)
    | BSelect { array; index } ->
        Hashtbl.hash ("select", hash array, hash index)
    | BSlice { signal; msb; lsb } ->
        Hashtbl.hash ("slice", hash signal, msb, lsb)
    | BConcat exprs ->
        Hashtbl.hash ("concat", List.map hash exprs)
    | BReplicate { count; value } ->
        Hashtbl.hash ("replicate", count, hash value)
    | BCond { condition; then_val; else_val } ->
        Hashtbl.hash ("cond", hash condition, hash then_val, hash else_val)
    | BCall { func; args } ->
        Hashtbl.hash ("call", func, List.map hash args)

  let rec equal e1 e2 =
    match (e1, e2) with
    | (BVar n1, BVar n2) -> n1 = n2
    | (BConst { value = v1; width = w1 }, BConst { value = v2; width = w2 }) ->
        v1 = v2 && w1 = w2
    | (BBinOp b1, BBinOp b2) ->
        b1.op = b2.op && equal b1.lhs b2.lhs && equal b1.rhs b2.rhs
    | (BUnOp u1, BUnOp u2) ->
        u1.op = u2.op && equal u1.operand u2.operand
    | (BSelect s1, BSelect s2) ->
        equal s1.array s2.array && equal s1.index s2.index
    | (BSlice sl1, BSlice sl2) ->
        equal sl1.signal sl2.signal && sl1.msb = sl2.msb && sl1.lsb = sl2.lsb
    | (BConcat es1, BConcat es2) ->
        List.length es1 = List.length es2 &&
        List.for_all2 equal es1 es2
    | (BReplicate r1, BReplicate r2) ->
        r1.count = r2.count && equal r1.value r2.value
    | (BCond c1, BCond c2) ->
        equal c1.condition c2.condition &&
        equal c1.then_val c2.then_val &&
        equal c1.else_val c2.else_val
    | (BCall c1, BCall c2) ->
        c1.func = c2.func &&
        List.length c1.args = List.length c2.args &&
        List.for_all2 equal c1.args c2.args
    | _ -> false
end

module ExprHashtbl = Hashtbl.Make(ExprHash)

(* CSE context *)
type cse_context = {
  (* Map from expression to temp variable *)
  expr_to_temp: string ExprHashtbl.t;
  (* Counter for generating temp names *)
  mutable next_temp: int;
  (* Count of eliminations *)
  mutable eliminations: int;
}

let create_cse_context () = {
  expr_to_temp = ExprHashtbl.create 50;
  next_temp = 0;
  eliminations = 0;
}

let fresh_temp ctx =
  let name = Printf.sprintf "_cse_temp%d" ctx.next_temp in
  ctx.next_temp <- ctx.next_temp + 1;
  name

(* Check if expression is worth extracting *)
let is_worth_extracting = function
  | BVar _ | BConst _ -> false  (* Too simple *)
  | BBinOp { op = BAdd | BSub | BMul | BDiv; _ } -> true  (* Arithmetic *)
  | BBinOp { op = BAnd | BOr | BXor; _ } -> true  (* Bitwise *)
  | BBinOp _ -> true  (* Other binary ops *)
  | BUnOp _ -> false  (* Unary ops usually cheap *)
  | BSelect _ | BSlice _ -> true  (* Memory/slice access *)
  | BConcat _ | BReplicate _ -> true  (* Bit manipulation *)
  | BCond _ -> true  (* Conditional *)
  | BCall _ -> true  (* Function call *)

(* Replace common subexpressions in expression *)
let rec replace_common_expr ctx = function
  | BVar _ as v -> (v, [])
  | BConst _ as c -> (c, [])

  | BBinOp { op; lhs; rhs; result_type } as expr ->
      let (lhs', lhs_pre) = replace_common_expr ctx lhs in
      let (rhs', rhs_pre) = replace_common_expr ctx rhs in

      let expr' = BBinOp { op; lhs = lhs'; rhs = rhs'; result_type } in

      (* Check if we've seen this expression before *)
      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, lhs_pre @ rhs_pre)
        with Not_found ->
          (* New expression, create temp *)
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, lhs_pre @ rhs_pre @ [assign])
      else
        (expr', lhs_pre @ rhs_pre)

  | BUnOp { op; operand; result_type } as expr ->
      let (operand', pre) = replace_common_expr ctx operand in
      let expr' = BUnOp { op; operand = operand'; result_type } in

      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, pre)
        with Not_found ->
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, pre @ [assign])
      else
        (expr', pre)

  | BSelect { array; index } as expr ->
      let (array', array_pre) = replace_common_expr ctx array in
      let (index', index_pre) = replace_common_expr ctx index in
      let expr' = BSelect { array = array'; index = index' } in

      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, array_pre @ index_pre)
        with Not_found ->
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, array_pre @ index_pre @ [assign])
      else
        (expr', array_pre @ index_pre)

  | BSlice { signal; msb; lsb } as expr ->
      let (signal', pre) = replace_common_expr ctx signal in
      let expr' = BSlice { signal = signal'; msb; lsb } in

      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, pre)
        with Not_found ->
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, pre @ [assign])
      else
        (expr', pre)

  | BConcat exprs as expr ->
      let (exprs', all_pre) = List.fold_right (fun e (acc_exprs, acc_pre) ->
        let (e', pre) = replace_common_expr ctx e in
        (e' :: acc_exprs, pre @ acc_pre)
      ) exprs ([], []) in
      let expr' = BConcat exprs' in

      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, all_pre)
        with Not_found ->
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, all_pre @ [assign])
      else
        (expr', all_pre)

  | BReplicate { count; value } as expr ->
      let (value', pre) = replace_common_expr ctx value in
      let expr' = BReplicate { count; value = value' } in

      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, pre)
        with Not_found ->
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, pre @ [assign])
      else
        (expr', pre)

  | BCond { condition; then_val; else_val } as expr ->
      let (cond', cond_pre) = replace_common_expr ctx condition in
      let (then', then_pre) = replace_common_expr ctx then_val in
      let (else', else_pre) = replace_common_expr ctx else_val in
      let expr' = BCond { condition = cond'; then_val = then'; else_val = else' } in

      if is_worth_extracting expr' then
        try
          let temp = ExprHashtbl.find ctx.expr_to_temp expr' in
          ctx.eliminations <- ctx.eliminations + 1;
          (BVar temp, cond_pre @ then_pre @ else_pre)
        with Not_found ->
          let temp = fresh_temp ctx in
          ExprHashtbl.add ctx.expr_to_temp expr' temp;
          let assign = BAssign { lhs = temp; rhs = expr' } in
          (BVar temp, cond_pre @ then_pre @ else_pre @ [assign])
      else
        (expr', cond_pre @ then_pre @ else_pre)

  | BCall { func; args } as expr ->
      let (args', all_pre) = List.fold_right (fun e (acc_exprs, acc_pre) ->
        let (e', pre) = replace_common_expr ctx e in
        (e' :: acc_exprs, pre @ acc_pre)
      ) args ([], []) in
      let expr' = BCall { func; args = args' } in

      (* Function calls might have side effects, so be conservative *)
      (expr', all_pre)

(* Apply CSE to statement *)
let rec apply_cse_stmt ctx = function
  | BAssign { lhs; rhs } ->
      let (rhs', pre_stmts) = replace_common_expr ctx rhs in
      pre_stmts @ [BAssign { lhs; rhs = rhs' }]

  | BIf { condition; then_stmts; else_stmts } ->
      let (condition', cond_pre) = replace_common_expr ctx condition in

      (* Clear expr table for branches (can't share across control flow safely) *)
      let saved_table = ExprHashtbl.copy ctx.expr_to_temp in

      let then_stmts' = List.concat (List.map (apply_cse_stmt ctx) then_stmts) in

      ExprHashtbl.clear ctx.expr_to_temp;
      ExprHashtbl.iter (fun k v -> ExprHashtbl.add ctx.expr_to_temp k v) saved_table;

      let else_stmts' = List.concat (List.map (apply_cse_stmt ctx) else_stmts) in

      cond_pre @ [BIf { condition = condition'; then_stmts = then_stmts'; else_stmts = else_stmts' }]

  | BCase { selector; cases; default } ->
      let (selector', sel_pre) = replace_common_expr ctx selector in

      let cases' = List.map (fun (value, stmts) ->
        let (value', val_pre) = replace_common_expr ctx value in
        let stmts' = val_pre @ List.concat (List.map (apply_cse_stmt ctx) stmts) in
        (value', stmts')
      ) cases in

      let default' = List.concat (List.map (apply_cse_stmt ctx) default) in

      sel_pre @ [BCase { selector = selector'; cases = cases'; default = default' }]

  | BWhile { condition; body } ->
      let (condition', cond_pre) = replace_common_expr ctx condition in
      let body' = List.concat (List.map (apply_cse_stmt ctx) body) in
      cond_pre @ [BWhile { condition = condition'; body = body' }]

  | BFor { init; condition; update; body } ->
      let init' = List.hd (apply_cse_stmt ctx init) in
      let (condition', cond_pre) = replace_common_expr ctx condition in
      let body' = List.concat (List.map (apply_cse_stmt ctx) body) in
      let update' = List.hd (apply_cse_stmt ctx update) in
      cond_pre @ [BFor { init = init'; condition = condition'; update = update'; body = body' }]

  | BBlock stmts ->
      [BBlock (List.concat (List.map (apply_cse_stmt ctx) stmts))]

  | BCallStmt { func; args } ->
      let (args', all_pre) = List.fold_right (fun e (acc_exprs, acc_pre) ->
        let (e', pre) = replace_common_expr ctx e in
        (e' :: acc_exprs, pre @ acc_pre)
      ) args ([], []) in
      all_pre @ [BCallStmt { func; args = args' }]

  | BReturn (Some expr) ->
      let (expr', pre) = replace_common_expr ctx expr in
      pre @ [BReturn (Some expr')]

  | BReturn None -> [BReturn None]

(* Apply CSE to process *)
let apply_cse_process = function
  | BCombinational { name; sensitivity; body } ->
      let ctx = create_cse_context () in
      let body' = List.concat (List.map (apply_cse_stmt ctx) body) in
      (BCombinational { name; sensitivity; body = body' }, ctx.eliminations)

  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body } ->
      let ctx = create_cse_context () in
      let body' = List.concat (List.map (apply_cse_stmt ctx) body) in
      (BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body = body' },
       ctx.eliminations)

(* Apply CSE to module *)
let apply_cse_module bmod =
  let total_elims = ref 0 in
  let processes' = List.map (fun proc ->
    let (proc', elims) = apply_cse_process proc in
    total_elims := !total_elims + elims;
    proc'
  ) bmod.processes in

  ({ bmod with processes = processes' }, !total_elims)

(* Apply CSE to program *)
let apply_cse_program prog =
  let total_elims = ref 0 in
  let modules' = List.map (fun bmod ->
    let (bmod', elims) = apply_cse_module bmod in
    total_elims := !total_elims + elims;
    bmod'
  ) prog.modules in

  ({ modules = modules'; library_cells = prog.library_cells }, !total_elims)

let apply_cse_with_stats prog =
  let (prog', eliminations) = apply_cse_program prog in
  Printf.printf "Common subexpression elimination: %d expressions reused\n" eliminations;
  prog'
