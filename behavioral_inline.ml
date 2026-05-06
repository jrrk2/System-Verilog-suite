(* Function and task inlining pass on Behavioral IR.
 *
 * Replaces every BCall (function call in expression position) and
 * BCallStmt (task call in statement position) with the body of the
 * matching `bfunc`, with formal parameters substituted by the
 * actual arguments.
 *
 * Function semantics: the function name doubles as the return value
 * inside the body — `return a + b` is encoded as `add := a + b` then
 * read back as `add`. We replace BCall (in an expression context) by
 * the *expression* the body assigns to `<fname>`. If the body has
 * complex control flow we fall back to leaving the call alone.
 *
 * Task semantics: a task may write to its output parameters. Inline
 * the body as a BBlock. Output-parameter writes target the *actual*
 * argument's lvalue: when the task writes `vbar = ...` and the call
 * site is `not_val(res, resbar)`, the inlined body assigns to
 * `resbar` directly.
 *
 * Recursive functions are NOT inlined (we'd loop forever); we detect
 * this with a depth bound. *)

open Behavioral_ir

let max_depth = 8

(* Substitute `var` → `replacement_expr` in an expression. Used when
 * replacing formal parameters with actual arguments. *)
let rec subst_expr_expr var repl = function
  | BVar n when n = var -> repl
  | (BVar _ | BConst _) as e -> e
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op;
               lhs = subst_expr_expr var repl lhs;
               rhs = subst_expr_expr var repl rhs;
               result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op; operand = subst_expr_expr var repl operand;
              result_type }
  | BSelect { array; index } ->
      BSelect { array = subst_expr_expr var repl array;
                index = subst_expr_expr var repl index }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = subst_expr_expr var repl signal; msb; lsb }
  | BConcat es ->
      BConcat (List.map (subst_expr_expr var repl) es)
  | BReplicate { count; value } ->
      BReplicate { count; value = subst_expr_expr var repl value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = subst_expr_expr var repl condition;
              then_val = subst_expr_expr var repl then_val;
              else_val = subst_expr_expr var repl else_val }
  | BCall { func; args } ->
      BCall { func; args = List.map (subst_expr_expr var repl) args }

(* Rename a variable on the LHS of an assignment if it matches `from`
 * → `to`. Used when binding output-parameter writes to the actual
 * lvalue at the call site. *)
let rename_lhs from_name to_name lhs =
  if lhs = from_name then to_name else lhs

(* Apply formal→actual substitutions to a stmt. `bindings` maps each
 * formal name to (actual_expr, output_lhs_or_none). For inputs,
 * output_lhs_or_none is None and the variable is replaced with the
 * actual expression on read. For outputs, output_lhs_or_none = Some
 * <lvalue name>: reads of the formal use the formal name (we don't
 * rewrite reads, since outputs are write-only in correct VHDL/SV)
 * and writes get rewritten LHS. *)
let rec subst_stmt bindings = function
  | BAssign { lhs; rhs } ->
      (* Rewrite reads (RHS) by substituting all input formals.
       * Rewrite writes (LHS) by mapping output formals to their
       * actual lvalue. *)
      let rhs' = List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e
      ) rhs bindings in
      let lhs' = List.fold_left (fun n (name, _, out) ->
        match out with
        | Some out_name when n = name -> out_name
        | _ -> n
      ) lhs bindings in
      BAssign { lhs = lhs'; rhs = rhs' }
  | BIf { condition; then_stmts; else_stmts } ->
      let c = List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e) condition bindings in
      BIf { condition = c;
            then_stmts = List.map (subst_stmt bindings) then_stmts;
            else_stmts = List.map (subst_stmt bindings) else_stmts }
  | BCase { selector; cases; default } ->
      let s = List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e) selector bindings in
      BCase { selector = s;
              cases = List.map (fun (k, ss) ->
                let k' = List.fold_left (fun e (name, expr, _) ->
                  subst_expr_expr name expr e) k bindings in
                (k', List.map (subst_stmt bindings) ss)) cases;
              default = List.map (subst_stmt bindings) default }
  | BWhile { condition; body } ->
      let c = List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e) condition bindings in
      BWhile { condition = c;
               body = List.map (subst_stmt bindings) body }
  | BFor { init; condition; update; body } ->
      let c = List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e) condition bindings in
      BFor { init = subst_stmt bindings init;
             condition = c;
             update = subst_stmt bindings update;
             body = List.map (subst_stmt bindings) body }
  | BBlock stmts -> BBlock (List.map (subst_stmt bindings) stmts)
  | BCallStmt { func; args } ->
      let args' = List.map (fun a ->
        List.fold_left (fun e (name, expr, _) ->
          subst_expr_expr name expr e) a bindings
      ) args in
      BCallStmt { func; args = args' }
  | BReturn None -> BReturn None
  | BReturn (Some e) ->
      let e' = List.fold_left (fun ex (name, expr, _) ->
        subst_expr_expr name expr ex) e bindings in
      BReturn (Some e')
  [@@warning "-26"]
let _ = rename_lhs (* silence unused *)

(* Build a binding list from a function's formal params and the
 * actual arguments at the call site. For input params, the actual is
 * an expression; for output params, the actual must be an lvalue
 * (BVar) so we can rewrite assignments. *)
let bind_params params args =
  try
    Some (List.map2 (fun (pname, _ptype, dir) actual ->
      match dir with
      | `Input ->
          (pname, actual, None)
      | `Output | `Inout ->
          (* Output expects a BVar at the call site. *)
          (match actual with
           | BVar n -> (pname, actual, Some n)
           | _ -> raise Exit)
    ) params args)
  with
  | Invalid_argument _ -> None  (* arity mismatch *)
  | Exit -> None  (* output bound to a non-lvalue *)

(* Given a function whose body should reduce to a single expression
 * (the value assigned to `<fname>`), extract that expression with
 * formal-parameter substitution applied. Falls back to None if the
 * body has any control flow we can't represent as a pure expression. *)
let rec body_to_expr fname bindings = function
  | [BAssign { lhs; rhs }] when lhs = fname ->
      Some (List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e) rhs bindings)
  | [BBlock inner] -> body_to_expr fname bindings inner
  | [BReturn (Some rhs)] ->
      Some (List.fold_left (fun e (name, expr, _) ->
        subst_expr_expr name expr e) rhs bindings)
  | stmts when List.for_all (function
        | BCallStmt { func = "@slice_write"; args = [BVar n; _; _; _] }
            when n = fname -> true
        | _ -> false) stmts && stmts <> [] ->
      (* Function body of `fname[hi:lo] = expr;` repeated for
         contiguous slices covering the whole bus.  Coalesce into a
         single BConcat (high-to-low).  Used by AES's mix_col which
         writes [31:24], [23:16], [15:8], [7:0] separately to build
         a 32-bit return value. *)
      let subst e =
        List.fold_left (fun acc (name, expr, _) ->
          subst_expr_expr name expr acc) e bindings in
      let parts = List.filter_map (function
        | BCallStmt { func = "@slice_write";
                      args = [BVar _; BConst { value = msb; _ };
                              BConst { value = lsb; _ }; data] } ->
            let hi = max msb lsb and lo = min msb lsb in
            Some (hi, lo, subst data)
        | _ -> None) stmts in
      let sorted =
        List.sort (fun (a, _, _) (b, _, _) -> compare b a) parts in
      let concat = BConcat (List.map (fun (_, _, d) -> d) sorted) in
      Some concat
  | [BCase { selector; cases; default }] ->
      (* Case statement function body: convert into nested BCond
         on `selector == case_value`, each branch being the case's
         assignment to fname.  Default handles the fall-through. *)
      let subst e =
        List.fold_left (fun acc (name, expr, _) ->
          subst_expr_expr name expr acc) e bindings in
      let default_expr =
        match body_to_expr fname bindings default with
        | Some e -> e
        | None -> BConst { value = 0; width = 1 } in
      let result = List.fold_right (fun (case_val, case_body) acc ->
        match body_to_expr fname bindings case_body with
        | None -> acc  (* skip uninlinable case *)
        | Some case_expr ->
            BCond {
              condition = BBinOp {
                op = BEq;
                lhs = subst selector;
                rhs = subst case_val;
                result_type = BInt { width = 1; signed = Unsigned };
              };
              then_val = case_expr;
              else_val = acc;
            }
      ) cases default_expr in
      Some result
  | _ -> None

(* Lookup table: function name → bfunc. *)
type ftable = (string, bfunc) Hashtbl.t

let build_ftable funcs : ftable =
  let h = Hashtbl.create 16 in
  List.iter (fun (f : bfunc) -> Hashtbl.replace h f.fname f) funcs;
  h

(* Inline pass on expressions. Walk recursively; whenever a BCall
 * matches a function in the table that reduces to a pure expression,
 * substitute. *)
let rec inline_expr ?(depth = 0) ftable e =
  if depth > max_depth then e
  else
    let recurse = inline_expr ~depth ftable in
    match e with
    | BVar _ | BConst _ -> e
    | BBinOp { op; lhs; rhs; result_type } ->
        BBinOp { op; lhs = recurse lhs; rhs = recurse rhs; result_type }
    | BUnOp { op; operand; result_type } ->
        BUnOp { op; operand = recurse operand; result_type }
    | BSelect { array; index } ->
        BSelect { array = recurse array; index = recurse index }
    | BSlice { signal; msb; lsb } ->
        BSlice { signal = recurse signal; msb; lsb }
    | BConcat es -> BConcat (List.map recurse es)
    | BReplicate { count; value } ->
        BReplicate { count; value = recurse value }
    | BCond { condition; then_val; else_val } ->
        BCond { condition = recurse condition;
                then_val = recurse then_val;
                else_val = recurse else_val }
    | BCall { func; args } ->
        let args' = List.map recurse args in
        (match Hashtbl.find_opt ftable func with
         | Some f when not f.is_task ->
             (match bind_params f.params args' with
              | Some bindings ->
                  (match body_to_expr f.fname bindings f.body with
                   | Some inlined ->
                       (* Recursively inline calls that appear inside
                        * the inlined expression. *)
                       inline_expr ~depth:(depth + 1) ftable inlined
                   | None -> BCall { func; args = args' })
              | None -> BCall { func; args = args' })
         | _ -> BCall { func; args = args' })

(* Inline pass on statements. Replaces BCallStmt to known tasks with
 * the inlined body. *)
let rec inline_stmt ?(depth = 0) ftable s =
  if depth > max_depth then s
  else
    let recurse_e = inline_expr ~depth ftable in
    let recurse_s = inline_stmt ~depth ftable in
    match s with
    | BAssign { lhs; rhs } -> BAssign { lhs; rhs = recurse_e rhs }
    | BIf { condition; then_stmts; else_stmts } ->
        BIf { condition = recurse_e condition;
              then_stmts = List.map recurse_s then_stmts;
              else_stmts = List.map recurse_s else_stmts }
    | BCase { selector; cases; default } ->
        BCase { selector = recurse_e selector;
                cases = List.map (fun (k, ss) ->
                  (recurse_e k, List.map recurse_s ss)) cases;
                default = List.map recurse_s default }
    | BWhile { condition; body } ->
        BWhile { condition = recurse_e condition;
                 body = List.map recurse_s body }
    | BFor { init; condition; update; body } ->
        BFor { init = recurse_s init;
               condition = recurse_e condition;
               update = recurse_s update;
               body = List.map recurse_s body }
    | BBlock stmts -> BBlock (List.map recurse_s stmts)
    | BCallStmt { func; args } ->
        let args' = List.map recurse_e args in
        (match Hashtbl.find_opt ftable func with
         | Some f ->
             (match bind_params f.params args' with
              | Some bindings ->
                  let inlined = List.map (fun st ->
                    inline_stmt ~depth:(depth + 1) ftable
                      (subst_stmt bindings st)
                  ) f.body in
                  BBlock inlined
              | None -> BCallStmt { func; args = args' })
         | None -> BCallStmt { func; args = args' })
    | BReturn None -> BReturn None
    | BReturn (Some e) -> BReturn (Some (recurse_e e))

let inline_process ftable = function
  | BCombinational c ->
      BCombinational { c with body = List.map (inline_stmt ftable) c.body }
  | BSequential s ->
      BSequential { s with body = List.map (inline_stmt ftable) s.body }

let inline_module (m : bmodule) =
  let ftable = build_ftable m.funcs in
  { m with processes = List.map (inline_process ftable) m.processes }

let inline_program (p : bprogram) =
  { p with modules = List.map inline_module p.modules }
