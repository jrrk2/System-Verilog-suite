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
      (* Variables written inside the loop body are not constant for
       * the duration of the loop — fold-ff_consts' linear scan
       * already added them to ctx (from a preceding init BAssign that
       * sits next to this BWhile in a BBlock), but treating them as
       * constants here folds the loop's exit condition to 1'b1 and
       * defeats Behavioral_unroll downstream.  Snapshot the ctx,
       * remove every body-writer from it, then restore on exit. *)
      let rec collect_lhs acc = function
        | BAssign { lhs; _ } -> lhs :: acc
        | BBlock ss -> List.fold_left collect_lhs acc ss
        | BIf { then_stmts; else_stmts; _ } ->
            let acc = List.fold_left collect_lhs acc then_stmts in
            List.fold_left collect_lhs acc else_stmts
        | BCase { cases; default; _ } ->
            let acc = List.fold_left (fun a (_, ss) ->
              List.fold_left collect_lhs a ss) acc cases in
            List.fold_left collect_lhs acc default
        | BWhile { body; _ } | BFor { body; _ } ->
            List.fold_left collect_lhs acc body
        | _ -> acc
      in
      let written = List.sort_uniq compare
        (List.fold_left collect_lhs [] body) in
      let saved = List.filter_map (fun n ->
        match Hashtbl.find_opt ctx.constants n with
        | Some v -> Hashtbl.remove ctx.constants n; Some (n, v)
        | None -> None) written in
      let condition' = propagate_expr ctx condition in
      let body' = List.map (propagate_stmt ctx) body in
      List.iter (fun (n, v) -> Hashtbl.replace ctx.constants n v) saved;
      BWhile { condition = condition'; body = body' }

  | BFor { init; condition; update; body } ->
      (* Same scoping discipline as BWhile: every signal written inside
       * the loop (the iterator AND any body-NBA target like `sum`)
       * must not be treated as a constant during condition/body
       * propagation, else we fold the loop's exit test to a literal
       * and Behavioral_unroll can no longer recognise the update.
       *
       * Init/update structures are preserved verbatim so the unroller
       * still sees `BAssign i := 0` / `i := i + step`. *)
      let rec collect_lhs_for acc = function
        | BAssign { lhs; _ } -> lhs :: acc
        | BBlock ss -> List.fold_left collect_lhs_for acc ss
        | BIf { then_stmts; else_stmts; _ } ->
            let acc = List.fold_left collect_lhs_for acc then_stmts in
            List.fold_left collect_lhs_for acc else_stmts
        | BCase { cases; default; _ } ->
            let acc = List.fold_left (fun a (_, ss) ->
              List.fold_left collect_lhs_for a ss) acc cases in
            List.fold_left collect_lhs_for acc default
        | BWhile { body; _ } | BFor { body; _ } ->
            List.fold_left collect_lhs_for acc body
        | _ -> acc
      in
      let iter_name = match init with
        | BAssign { lhs; _ } -> [lhs]
        | _ -> []
      in
      let body_writers = List.fold_left collect_lhs_for [] body in
      let scoped = List.sort_uniq compare (iter_name @ body_writers) in
      let saved = List.filter_map (fun n ->
        match Hashtbl.find_opt ctx.constants n with
        | Some v -> Hashtbl.remove ctx.constants n; Some (n, v)
        | None -> None) scoped in
      let condition' = propagate_expr ctx condition in
      let body' = List.map (propagate_stmt ctx) body in
      List.iter (fun (n, v) -> Hashtbl.replace ctx.constants n v) saved;
      BFor { init; condition = condition'; update; body = body' }

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

(* ========================================================================= *)
(* Constant folding through flip-flops.                                       *)
(*                                                                            *)
(* The per-process propagator above stays inside one process. That leaves a   *)
(* gap whenever a Verilog "wire a = 0; ... q <= a;" is split across a         *)
(* combinational assign (driving a) and a sequential always_ff (reading a):   *)
(* the sequential process never sees that a is a constant.                    *)
(*                                                                            *)
(* This extension closes the gap with two cooperating passes:                 *)
(*                                                                            *)
(*  1. Cross-process constant analysis: a signal whose every write across     *)
(*     every process is the same BConst RHS is a module-wide constant. Seed   *)
(*     that map into each process's propagation context before propagating.   *)
(*                                                                            *)
(*  2. Sequential→combinational fold: after seeded propagation, any           *)
(*     BSequential whose body reduces to constant-only BAssigns is no longer  *)
(*     stateful — convert it to BCombinational with the same body so the      *)
(*     downstream ffrip pass doesn't manufacture spurious Q/Q__D ports for    *)
(*     a register that is provably never going to hold anything but a         *)
(*     constant value.                                                        *)
(*                                                                            *)
(* This matches the behaviour Verilator's optimiser already applies, which    *)
(* is why a Verilator-vs-Verible miter on a constant-driven FF (e.g.          *)
(* always_ff @(posedge a) q <= b where wire a = 0; wire b = 0;) sees a port-  *)
(* count mismatch without it.                                                 *)
(* ========================================================================= *)

(* Walk a statement list collecting every BAssign target/RHS pair. *)
let rec collect_assign_pairs acc = function
  | BAssign { lhs; rhs } -> (lhs, rhs) :: acc
  | BBlock ss -> List.fold_left collect_assign_pairs acc ss
  | BIf { then_stmts; else_stmts; _ } ->
      let acc = List.fold_left collect_assign_pairs acc then_stmts in
      List.fold_left collect_assign_pairs acc else_stmts
  | BCase { cases; default; _ } ->
      let acc = List.fold_left (fun a (_, ss) ->
        List.fold_left collect_assign_pairs a ss) acc cases in
      List.fold_left collect_assign_pairs acc default
  | BWhile { body; _ } | BFor { body; _ } ->
      List.fold_left collect_assign_pairs acc body
  | BCallStmt _ | BReturn _ -> acc

let collect_module_assign_pairs bmod =
  List.fold_left (fun acc p ->
    let body = match p with
      | BCombinational { body; _ } -> body
      | BSequential   { body; _ } -> body
    in
    List.fold_left collect_assign_pairs acc body
  ) [] bmod.processes

(* Signals whose every write is the same BConst value across the module.
 * Returned as (name, const_value) suitable to seed a prop_context. *)
let module_constants bmod =
  let pairs = collect_module_assign_pairs bmod in
  let by_signal = Hashtbl.create 16 in
  List.iter (fun (lhs, rhs) ->
    let cur = try Hashtbl.find by_signal lhs with Not_found -> [] in
    Hashtbl.replace by_signal lhs (rhs :: cur)
  ) pairs;
  Hashtbl.fold (fun name rhss acc ->
    let const_of = function
      | BConst { value; width } -> Some (CInt (value, width))
      | _ -> None
    in
    let consts = List.map const_of rhss in
    if consts = [] || List.exists (fun c -> c = None) consts then acc
    else
      let firsts = List.filter_map (fun x -> x) consts in
      match firsts with
      | first :: rest when List.for_all (fun x -> x = first) rest ->
          (name, first) :: acc
      | _ -> acc
  ) by_signal []

(* Walk a body, returning true iff every BAssign in it (at any depth) has a
 * BConst RHS. Empty bodies count as constant-only. *)
let rec body_is_const_only stmts = List.for_all stmt_is_const_only stmts
and stmt_is_const_only = function
  | BAssign { rhs = BConst _; _ } -> true
  | BAssign _ -> false
  | BBlock ss -> body_is_const_only ss
  | BIf { then_stmts; else_stmts; _ } ->
      body_is_const_only then_stmts && body_is_const_only else_stmts
  | BCase { cases; default; _ } ->
      List.for_all (fun (_, ss) -> body_is_const_only ss) cases
      && body_is_const_only default
  | BWhile { body; _ } | BFor { body; _ } -> body_is_const_only body
  | BCallStmt _ | BReturn _ -> true

(* Single module-level pass: seed cross-process constants, propagate per
 * process, fold constant-only sequential processes to combinational, AND
 * drop sequential processes whose clock is a module-wide constant
 * (Verilator does the same optimisation: a never-edging clock means the
 * FF never fires, so the body collapses to whatever the signal's reset
 * state was — effectively nothing observable from the outside).
 * Returns (new_module, total_changes). *)
let propagate_module_with_globals bmod =
  let globals = module_constants bmod in
  let const_names = List.map fst globals in
  let total_changes = ref 0 in
  let processes' = List.map (fun proc ->
    let ctx = create_prop_context () in
    List.iter (fun (n, v) -> Hashtbl.add ctx.constants n v) globals;
    let proc' = match proc with
      | BCombinational { name; sensitivity; body } ->
          BCombinational { name; sensitivity;
                           body = List.map (propagate_stmt ctx) body }
      | BSequential { name; clock; clock_edge; reset; reset_edge;
                      reset_async; body } ->
          BSequential { name; clock; clock_edge; reset; reset_edge;
                        reset_async;
                        body = List.map (propagate_stmt ctx) body }
    in
    total_changes := !total_changes + ctx.changes;
    proc'
  ) bmod.processes in
  (* Count writers per signal across the whole module: needed below to
   * avoid hiding multi-driver bugs (e.g. illegal SV that mixes a
   * continuous `assign v = 12;` with an `always @(posedge clk) v <= ~v`
   * — dropping the constant-clock always block would leave a single,
   * valid-looking driver and silently mask the error). *)
  let writers_of = Hashtbl.create 16 in
  List.iter (fun p ->
    let body = match p with
      | BCombinational { body; _ } -> body
      | BSequential   { body; _ } -> body
    in
    List.iter (fun (lhs, _) ->
      let c = try Hashtbl.find writers_of lhs with Not_found -> 0 in
      Hashtbl.replace writers_of lhs (c + 1))
      (List.fold_left collect_assign_pairs [] body)
  ) processes';
  let lhses_of_body body =
    List.map fst (List.fold_left collect_assign_pairs [] body)
    |> List.sort_uniq compare
  in
  let folded_count = ref 0 in
  let dropped_count = ref 0 in
  let processes'' = List.filter_map (function
    | BSequential s when List.mem s.clock const_names
                         && List.for_all
                              (fun n ->
                                 (try Hashtbl.find writers_of n
                                  with Not_found -> 0) <= 1)
                              (lhses_of_body s.body) ->
        (* Constant clock AND every Q has this FF as its sole writer:
         * the FF never fires, so the signal stays at its reset state.
         * Drop the process; the signal becomes undriven internal,
         * matching what Verilator's optimiser already does. *)
        incr dropped_count; None
    | BSequential s when s.body <> [] && body_is_const_only s.body ->
        incr folded_count;
        Some (BCombinational {
          name = s.name ^ "_ff_const_fold";
          sensitivity = [BAny];
          body = s.body;
        })
    | p -> Some p
  ) processes' in
  if !folded_count > 0 then
    Printf.printf "  FF constant folding: %d sequential→combinational\n"
      !folded_count;
  if !dropped_count > 0 then
    Printf.printf "  FF constant-clock drop: %d sequential processes\n"
      !dropped_count;
  ({ bmod with processes = processes'' },
   !total_changes + !folded_count + !dropped_count)

(* Module-level fixpoint: iterate seeded propagation + FF folding until
 * no more changes. Each fold can expose new constants (e.g. a register
 * that becomes a constant wire may unlock further folding upstream). *)
let fold_ffs_module bmod =
  let rec iter bmod n =
    let (bmod', changes) = propagate_module_with_globals bmod in
    if changes = 0 || n > 16 then bmod'
    else iter bmod' (n + 1)
  in
  iter bmod 1

let fold_ffs_program (prog : bprogram) : bprogram =
  { prog with modules = List.map fold_ffs_module prog.modules }

(* ========================================================================= *)
(* BCall argument width normalisation.                                        *)
(*                                                                            *)
(* SystemVerilog implicitly truncates or extends a function-call actual to    *)
(* the formal parameter's declared width at the call boundary. Verilator's    *)
(* JSON pre-applies that cast as part of its CONST/EXTEND emission, so the    *)
(* verilator→BIR converter naturally receives correctly-widened args.        *)
(* Verible's parse-tree carries no such cast — the actual is whatever its    *)
(* source signal width happens to be — so when the two converters meet at    *)
(* the Z3 miter they encode the same BCall with different-width arg lists    *)
(* and the encoder mints two distinct uninterpreted-function decls, after    *)
(* which Z3 trivially finds a counterexample.                                *)
(*                                                                            *)
(* Walk every BCall (in expressions) and every BCallStmt (statement form),   *)
(* look up the formal parameter widths in bmodule.funcs, and emit a BSlice   *)
(* or zero-extending BConcat around each actual arg so both sides land on    *)
(* the same shape regardless of where the cast was supposed to apply.        *)
(* Signedness-aware widening (sign-extend instead of zero-extend, and the    *)
(* signed-compare-widening of a follow-up pass) is intentionally not done    *)
(* here — this pass only equalises bit widths.                               *)
(* ========================================================================= *)

let rec width_of_btype_full = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * width_of_btype_full element
  | BStruct _ -> 32

let signal_widths_of_module bmod =
  let h = Hashtbl.create 32 in
  List.iter (fun (s : bsignal) ->
    Hashtbl.replace h s.name (width_of_btype_full s.stype))
    bmod.signals;
  h

let func_sigs_of_module bmod =
  let h = Hashtbl.create 8 in
  List.iter (fun (f : bfunc) ->
    let widths =
      List.map (fun (_, ty, _) -> width_of_btype_full ty) f.params
    in
    Hashtbl.replace h f.fname widths)
    bmod.funcs;
  h

let rec width_of_expr_with sig_widths = function
  | BVar n -> Hashtbl.find_opt sig_widths n
  | BConst { width; _ } -> Some width
  | BBinOp { result_type; _ } -> Some (width_of_btype_full result_type)
  | BUnOp { result_type; _ } -> Some (width_of_btype_full result_type)
  | BCond { then_val; _ } -> width_of_expr_with sig_widths then_val
  | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
  | BConcat exprs ->
      let widths = List.map (width_of_expr_with sig_widths) exprs in
      if List.for_all Option.is_some widths
      then Some (List.fold_left (+) 0 (List.map Option.get widths))
      else None
  | BReplicate { count; value } ->
      (match width_of_expr_with sig_widths value with
       | Some w -> Some (count * w)
       | None -> None)
  | BSelect _ | BCall _ -> None

let normalize_one_arg sig_widths arg formal_w =
  match width_of_expr_with sig_widths arg with
  | None -> arg
  | Some actual_w when actual_w = formal_w -> arg
  | Some actual_w when actual_w > formal_w ->
      (* Implicit truncation: keep low formal_w bits. *)
      BSlice { signal = arg; msb = formal_w - 1; lsb = 0 }
  | Some actual_w ->
      (* actual_w < formal_w: zero-extend. Signedness-aware
       * extension is deferred to a follow-up pass. *)
      let pad = BConst { value = 0; width = formal_w - actual_w } in
      BConcat [pad; arg]

let normalize_call_args sig_widths formals args =
  let actuals_n = List.length args in
  let formals_n = List.length formals in
  if actuals_n = formals_n then
    List.map2 (normalize_one_arg sig_widths) args formals
  else if formals_n = 1 && actuals_n > 1 then
    (* Concat-spread: Verible's IR converter flattens
     * `func({a, b, c})` into `func(a, b, c)`, and because the Verible
     * grammar's `expression_list_proper` is left-recursive the args
     * arrive in REVERSE source order (TLIST [c; b; a]).  Rebuild the
     * implied BConcat in MSB-first source order by reversing the
     * collected list, then match it against the single formal. *)
    [normalize_one_arg sig_widths (BConcat (List.rev args))
       (List.hd formals)]
  else args

let rec normalize_expr sig_widths func_sigs = function
  | BCall { func; args } ->
      let args = List.map (normalize_expr sig_widths func_sigs) args in
      (match Hashtbl.find_opt func_sigs func with
       | Some formals ->
           BCall { func; args = normalize_call_args sig_widths formals args }
       | None -> BCall { func; args })
  | BBinOp r ->
      BBinOp { r with
        lhs = normalize_expr sig_widths func_sigs r.lhs;
        rhs = normalize_expr sig_widths func_sigs r.rhs }
  | BUnOp r ->
      BUnOp { r with operand = normalize_expr sig_widths func_sigs r.operand }
  | BSlice r ->
      BSlice { r with signal = normalize_expr sig_widths func_sigs r.signal }
  | BConcat es ->
      BConcat (List.map (normalize_expr sig_widths func_sigs) es)
  | BReplicate r ->
      BReplicate { r with value = normalize_expr sig_widths func_sigs r.value }
  | BCond r ->
      BCond {
        condition = normalize_expr sig_widths func_sigs r.condition;
        then_val  = normalize_expr sig_widths func_sigs r.then_val;
        else_val  = normalize_expr sig_widths func_sigs r.else_val }
  | BSelect r ->
      BSelect {
        array = normalize_expr sig_widths func_sigs r.array;
        index = normalize_expr sig_widths func_sigs r.index }
  | (BVar _ | BConst _) as e -> e

let rec normalize_stmt sig_widths func_sigs = function
  | BAssign { lhs; rhs } ->
      BAssign { lhs; rhs = normalize_expr sig_widths func_sigs rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf {
        condition = normalize_expr sig_widths func_sigs condition;
        then_stmts = List.map (normalize_stmt sig_widths func_sigs) then_stmts;
        else_stmts = List.map (normalize_stmt sig_widths func_sigs) else_stmts }
  | BCase { selector; cases; default } ->
      BCase {
        selector = normalize_expr sig_widths func_sigs selector;
        cases = List.map (fun (v, ss) ->
          (normalize_expr sig_widths func_sigs v,
           List.map (normalize_stmt sig_widths func_sigs) ss)) cases;
        default = List.map (normalize_stmt sig_widths func_sigs) default }
  | BWhile { condition; body } ->
      BWhile {
        condition = normalize_expr sig_widths func_sigs condition;
        body = List.map (normalize_stmt sig_widths func_sigs) body }
  | BFor { init; condition; update; body } ->
      BFor {
        init = normalize_stmt sig_widths func_sigs init;
        condition = normalize_expr sig_widths func_sigs condition;
        update = normalize_stmt sig_widths func_sigs update;
        body = List.map (normalize_stmt sig_widths func_sigs) body }
  | BBlock ss ->
      BBlock (List.map (normalize_stmt sig_widths func_sigs) ss)
  | BCallStmt { func; args } ->
      let args = List.map (normalize_expr sig_widths func_sigs) args in
      (match Hashtbl.find_opt func_sigs func with
       | Some formals ->
           BCallStmt { func; args = normalize_call_args sig_widths formals args }
       | None -> BCallStmt { func; args })
  | BReturn (Some e) ->
      BReturn (Some (normalize_expr sig_widths func_sigs e))
  | BReturn None -> BReturn None

let normalize_bcall_args_module bmod =
  let sig_widths = signal_widths_of_module bmod in
  let func_sigs = func_sigs_of_module bmod in
  let processes' = List.map (function
    | BCombinational { name; sensitivity; body } ->
        BCombinational { name; sensitivity;
          body = List.map (normalize_stmt sig_widths func_sigs) body }
    | BSequential { name; clock; clock_edge; reset; reset_edge;
                    reset_async; body } ->
        BSequential { name; clock; clock_edge; reset; reset_edge;
          reset_async;
          body = List.map (normalize_stmt sig_widths func_sigs) body }
  ) bmod.processes in
  { bmod with processes = processes' }

let normalize_bcall_args_program (prog : bprogram) : bprogram =
  { prog with modules = List.map normalize_bcall_args_module prog.modules }

(* ========================================================================= *)
(* Unsized-fill literal expansion.                                            *)
(*                                                                            *)
(* The Verible→IR converter emits SV's unsized fills `'0`, `'1`, `'x`, `'z`  *)
(* as BConst with width=0 — a sentinel meaning "fill at context width".       *)
(* Verilator's JSON pre-applies the LHS broadcast, so its BConsts already    *)
(* carry the right width on that side. For the verible side we walk the IR  *)
(* and rewrite every width=0 BConst to the enclosing BAssign's LHS width,    *)
(* so `acc_out <= '1` with acc_out being 64 bits becomes                     *)
(* `acc_out := 64'hFFFFFFFFFFFFFFFF` rather than the silent                  *)
(* `acc_out := 32'hFFFFFFFF` (zero-extended on assign to 0x00000000FFFFFFFF).*)
(* ========================================================================= *)

let rec expand_fills_expr target_w = function
  | BConst { value; width = 0 } -> BConst { value; width = target_w }
  | BBinOp r ->
      BBinOp { r with
        lhs = expand_fills_expr target_w r.lhs;
        rhs = expand_fills_expr target_w r.rhs }
  | BUnOp r ->
      BUnOp { r with operand = expand_fills_expr target_w r.operand }
  | BSlice r ->
      BSlice { r with signal = expand_fills_expr target_w r.signal }
  | BConcat es ->
      BConcat (List.map (expand_fills_expr target_w) es)
  | BCond r ->
      BCond {
        condition = expand_fills_expr target_w r.condition;
        then_val  = expand_fills_expr target_w r.then_val;
        else_val  = expand_fills_expr target_w r.else_val }
  | BSelect r ->
      BSelect {
        array = expand_fills_expr target_w r.array;
        index = expand_fills_expr target_w r.index }
  | BReplicate r ->
      BReplicate { r with value = expand_fills_expr target_w r.value }
  | BCall r ->
      BCall { r with args = List.map (expand_fills_expr target_w) r.args }
  | (BVar _ | BConst _) as e -> e

let rec expand_fills_stmt sig_widths = function
  | BAssign { lhs; rhs } ->
      let target_w =
        match Hashtbl.find_opt sig_widths lhs with
        | Some w -> w
        | None -> 32
      in
      BAssign { lhs; rhs = expand_fills_expr target_w rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf {
        condition = expand_fills_expr 32 condition;
        then_stmts = List.map (expand_fills_stmt sig_widths) then_stmts;
        else_stmts = List.map (expand_fills_stmt sig_widths) else_stmts }
  | BCase { selector; cases; default } ->
      BCase {
        selector = expand_fills_expr 32 selector;
        cases = List.map (fun (v, ss) ->
          (expand_fills_expr 32 v,
           List.map (expand_fills_stmt sig_widths) ss)) cases;
        default = List.map (expand_fills_stmt sig_widths) default }
  | BWhile { condition; body } ->
      BWhile {
        condition = expand_fills_expr 32 condition;
        body = List.map (expand_fills_stmt sig_widths) body }
  | BFor { init; condition; update; body } ->
      BFor {
        init = expand_fills_stmt sig_widths init;
        condition = expand_fills_expr 32 condition;
        update = expand_fills_stmt sig_widths update;
        body = List.map (expand_fills_stmt sig_widths) body }
  | BBlock ss -> BBlock (List.map (expand_fills_stmt sig_widths) ss)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (expand_fills_expr 32) args }
  | BReturn (Some e) -> BReturn (Some (expand_fills_expr 32 e))
  | BReturn None -> BReturn None

let expand_fills_module bmod =
  let sig_widths = signal_widths_of_module bmod in
  let processes' = List.map (function
    | BCombinational { name; sensitivity; body } ->
        BCombinational { name; sensitivity;
          body = List.map (expand_fills_stmt sig_widths) body }
    | BSequential { name; clock; clock_edge; reset; reset_edge;
                    reset_async; body } ->
        BSequential { name; clock; clock_edge; reset; reset_edge;
          reset_async;
          body = List.map (expand_fills_stmt sig_widths) body }
  ) bmod.processes in
  { bmod with processes = processes' }

let expand_fills_program (prog : bprogram) : bprogram =
  { prog with modules = List.map expand_fills_module prog.modules }
