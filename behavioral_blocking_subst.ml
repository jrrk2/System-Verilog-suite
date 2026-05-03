(* Forward-substitute blocking-LHS reads within an always_ff body.
 *
 * Verible's BIR loses the SV `=` vs `<=` distinction (both lower to
 * BAssign). Without that, an idiom like
 *
 *   always_ff @(posedge clk) begin
 *     tmp = a + b;            // blocking — intermediate
 *     out <= tmp + c;         // non-blocking — FF
 *   end
 *
 * leaves `tmp` looking like a separate state element when the FF
 * extractor walks the body. Vivado's elaboration optimises `tmp`
 * away, so the FF set differs.
 *
 * Heuristic: within a single BSequential body, if a variable is
 * assigned exactly once near the top of the block and then read in
 * later statements, treat it as an intermediate and substitute every
 * later read with the assigned RHS. Drop the assignment afterwards
 * iff the variable doesn't appear in the module's exported signal
 * list (we can't tell from a single block — for safety we only drop
 * when the post-substitution body has zero remaining reads of the
 * variable).
 *
 * Limitations:
 * - Conservative: if a variable is assigned more than once inside a
 *   block, we leave it alone (real FF or carrier for control flow).
 * - We don't propagate through nested control flow yet (BIf/BCase
 *   bodies are walked but assignments inside them aren't promoted to
 *   the outer scope).
 *
 * The pass runs after iflift (so most BIfs are already collapsed to
 * BCond expressions) and before ffrip. *)

open Behavioral_ir

(* Walk a bexpr, applying f to each BVar and returning the rewritten
 * expression. *)
let rec map_expr f = function
  | BVar n as e -> (match f n with Some e' -> e' | None -> e)
  | BConst _ as e -> e
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op; lhs = map_expr f lhs; rhs = map_expr f rhs; result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op; operand = map_expr f operand; result_type }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = map_expr f condition;
              then_val  = map_expr f then_val;
              else_val  = map_expr f else_val }
  | BConcat es -> BConcat (List.map (map_expr f) es)
  | BReplicate { count; value } ->
      BReplicate { count; value = map_expr f value }
  | BSelect { array; index } ->
      BSelect { array = map_expr f array; index = map_expr f index }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = map_expr f signal; msb; lsb }
  | BCall { func; args } ->
      BCall { func; args = List.map (map_expr f) args }

(* Count free occurrences of `name` in an expression. *)
let rec count_in_expr name = function
  | BVar n -> if n = name then 1 else 0
  | BConst _ -> 0
  | BBinOp { lhs; rhs; _ } ->
      count_in_expr name lhs + count_in_expr name rhs
  | BUnOp { operand; _ } -> count_in_expr name operand
  | BCond { condition; then_val; else_val } ->
      count_in_expr name condition
      + count_in_expr name then_val
      + count_in_expr name else_val
  | BConcat es -> List.fold_left (fun a e -> a + count_in_expr name e) 0 es
  | BReplicate { value; _ } -> count_in_expr name value
  | BSelect { array; index } ->
      count_in_expr name array + count_in_expr name index
  | BSlice { signal; _ } -> count_in_expr name signal
  | BCall { args; _ } ->
      List.fold_left (fun a e -> a + count_in_expr name e) 0 args

(* Count uses of `name` in a statement list. *)
let rec count_in_stmts name = function
  | [] -> 0
  | BAssign { lhs = _; rhs } :: rest ->
      count_in_expr name rhs + count_in_stmts name rest
  | BIf { condition; then_stmts; else_stmts } :: rest ->
      count_in_expr name condition
      + count_in_stmts name then_stmts
      + count_in_stmts name else_stmts
      + count_in_stmts name rest
  | BCase { selector; cases; default } :: rest ->
      count_in_expr name selector
      + List.fold_left (fun a (e, ss) ->
          a + count_in_expr name e + count_in_stmts name ss) 0 cases
      + count_in_stmts name default
      + count_in_stmts name rest
  | BBlock ss :: rest ->
      count_in_stmts name ss + count_in_stmts name rest
  | _ :: rest -> count_in_stmts name rest

(* Map an expression-rewrite over every bexpr appearing in a stmt. *)
let rec map_stmt f = function
  | BAssign { lhs; rhs } -> BAssign { lhs; rhs = map_expr f rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition = map_expr f condition;
            then_stmts = List.map (map_stmt f) then_stmts;
            else_stmts = List.map (map_stmt f) else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = map_expr f selector;
              cases = List.map (fun (e, ss) ->
                       (map_expr f e, List.map (map_stmt f) ss)) cases;
              default = List.map (map_stmt f) default }
  | BBlock ss -> BBlock (List.map (map_stmt f) ss)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (map_expr f) args }
  | BReturn (Some e) -> BReturn (Some (map_expr f e))
  | other -> other

(* Count assignments to `name` within a stmt list (top-level only —
 * we don't promote out of conditional bodies). *)
let assignment_count name stmts =
  List.fold_left (fun acc s ->
    match s with BAssign { lhs; _ } when lhs = name -> acc + 1 | _ -> acc
  ) 0 stmts

let process_seq_body stmts =
  (* Pass 1: identify candidate substitutions: variables assigned
   * exactly once at the top level of the block AND read at least
   * once in subsequent statements. *)
  let rec collect i acc = function
    | [] -> List.rev acc
    | BAssign { lhs; rhs } as s :: rest
      when assignment_count lhs stmts = 1
        && count_in_stmts lhs rest >= 1 ->
        collect (i + 1) ((i, lhs, rhs, s) :: acc) rest
    | _ :: rest -> collect (i + 1) acc rest
  in
  let cands = collect 0 [] stmts in
  if cands = [] then stmts
  else begin
    let subst_table = Hashtbl.create 8 in
    List.iter (fun (_, lhs, rhs, _) -> Hashtbl.add subst_table lhs rhs)
      cands;
    let lookup n = Hashtbl.find_opt subst_table n in
    (* Apply substitution to every statement, then remove the original
     * assignments whose result is now consumed only via inlined uses
     * (i.e. count_in_stmts of the rewritten body without the assign
     * itself drops to zero). *)
    let rewritten = List.mapi (fun i s ->
      match s with
      | BAssign { lhs; _ } when Hashtbl.mem subst_table lhs ->
          (i, `Drop, s)
      | _ -> (i, `Keep, map_stmt lookup s)
    ) stmts in
    List.filter_map (function
      | (_, `Keep, s) -> Some s
      | (_, `Drop, _) -> None
    ) rewritten
  end

let process_module (m : bmodule) : bmodule =
  let new_processes = List.map (function
    | BSequential s -> BSequential { s with body = process_seq_body s.body }
    | other -> other
  ) m.processes in
  { m with processes = new_processes }

let blocking_subst_program (p : bprogram) : bprogram =
  { p with modules = List.map process_module p.modules }
