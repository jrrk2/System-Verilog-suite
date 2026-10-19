(* Lowering shared by every front end.
 *
 * The three importers (verible, verilator JSON, RTLIL from yosys-slang) used
 * to each pick their own IR shape for the same construct, and every pass
 * downstream then had to recognise all of them.  This module is the one place
 * for those decisions; post_load runs [normalize_program] on whatever a front
 * end produced, so the passes see ONE shape:
 *
 *   loops     `i = c; while (cond) { body; i = i + k }`  ->  BFor
 *             (verilator's form; the unroller, the emitters and the miters
 *             then only need the BFor case)
 *   returns   `f = e; return;`  ->  `return e;`   (verilator's lowering of
 *             `return e`, which it emits as an assignment plus a JUMPGO)
 *   blocks    empty BBlocks dropped (declarations the importers strip)
 *
 * Expression-level lowerings the importers already share live in
 * Behavioral_const and are re-exported here so a new importer has one module
 * to look at: [signed_compare] (sign-extend, flip MSB, unsigned compare) and
 * [strip_signed_anywhere]. *)

open Behavioral_ir

let signed_compare = Behavioral_const.signed_compare
let strip_signed_anywhere = Behavioral_const.strip_signed_anywhere

let is_empty_block = function BBlock [] -> true | _ -> false

(* `init; while (cond) { body...; update }` where init and update assign the
   same variable -> BFor.  Applied at every position of every statement list. *)
let rec canonical_loops stmts =
  let rec scan prefix = function
    | [] -> List.rev prefix
    | (BAssign { lhs = i; _ } as init) :: BWhile { condition; body } :: rest
      when (match List.rev body with
            | BAssign { lhs; _ } :: _ -> lhs = i
            | _ -> false) ->
        let body_rev = List.rev body in
        let update = List.hd body_rev in
        let body = List.rev (List.tl body_rev) in
        let f = BFor { init; condition; update; body = canonical_loops body } in
        scan (f :: prefix) rest
    | s :: rest -> scan (lower_stmt s :: prefix) rest
  in
  scan [] (List.filter (fun s -> not (is_empty_block s)) stmts)

and lower_stmt = function
  | BBlock ss -> BBlock (canonical_loops ss)
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition; then_stmts = canonical_loops then_stmts;
            else_stmts = canonical_loops else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector; cases = List.map (fun (k, ss) -> (k, canonical_loops ss)) cases;
              default = canonical_loops default }
  | BWhile { condition; body } -> BWhile { condition; body = canonical_loops body }
  | BFor { init; condition; update; body } ->
      BFor { init; condition; update; body = canonical_loops body }
  | s -> s

(* `f = e; return` -> `return e` inside the body of function [fname] *)
let rec fold_returns fname = function
  | BAssign { lhs; rhs } :: BReturn None :: rest when lhs = fname ->
      BReturn (Some rhs) :: fold_returns fname rest
  | BBlock l :: rest -> BBlock (fold_returns fname l) :: fold_returns fname rest
  | BIf { condition; then_stmts; else_stmts } :: rest ->
      BIf { condition; then_stmts = fold_returns fname then_stmts;
            else_stmts = fold_returns fname else_stmts } :: fold_returns fname rest
  | BCase { selector; cases; default } :: rest ->
      BCase { selector; cases = List.map (fun (v, s) -> (v, fold_returns fname s)) cases;
              default = fold_returns fname default } :: fold_returns fname rest
  | s :: rest -> s :: fold_returns fname rest
  | [] -> []

(* `v[k] = d` on a VECTOR arrives as `@mem_write(v, k, d)` from verible (and
   from the unroller once `i` is a constant): the SSA fold behind the Z3
   miter, the inliner's slice coalescing and the gate lowering all understand
   `@slice_write(v, k, k, d)`, and not all of them the other.  [arrays] are the
   real memories (BArray signals, inferred mems), whose @mem_write is a word
   write and stays. *)
let rec bitwrites_to_slices arrays = function
  | BCallStmt { func = "@mem_write"; args = [BVar v; (BConst _ as k); d] }
    when not (List.mem v arrays) ->
      BCallStmt { func = "@slice_write"; args = [BVar v; k; k; d] }
  | BBlock ss -> BBlock (List.map (bitwrites_to_slices arrays) ss)
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition; then_stmts = List.map (bitwrites_to_slices arrays) then_stmts;
            else_stmts = List.map (bitwrites_to_slices arrays) else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector; cases = List.map (fun (k, ss) -> (k, List.map (bitwrites_to_slices arrays) ss)) cases;
              default = List.map (bitwrites_to_slices arrays) default }
  | BWhile { condition; body } -> BWhile { condition; body = List.map (bitwrites_to_slices arrays) body }
  | BFor { init; condition; update; body } ->
      BFor { init; condition; update; body = List.map (bitwrites_to_slices arrays) body }
  | s -> s

let array_names (m : bmodule) =
  List.filter_map (fun (s : bsignal) -> match s.stype with BArray _ -> Some s.name | _ -> None) m.signals
  @ List.map (fun (mm : bmem) -> mm.mname) m.mems

let bitwrites_module (m : bmodule) =
  let arrays = array_names m in
  { m with
    processes = List.map (function
      | BCombinational c -> BCombinational { c with body = List.map (bitwrites_to_slices arrays) c.body }
      | BSequential s -> BSequential { s with body = List.map (bitwrites_to_slices arrays) s.body }) m.processes;
    funcs = List.map (fun (f : bfunc) -> { f with body = List.map (bitwrites_to_slices arrays) f.body }) m.funcs }

let normalize_process = function
  | BCombinational c -> BCombinational { c with body = canonical_loops c.body }
  | BSequential s -> BSequential { s with body = canonical_loops s.body }

let normalize_func (f : bfunc) =
  { f with body = canonical_loops (fold_returns f.fname f.body) }

let normalize_module (m : bmodule) =
  bitwrites_module
    { m with processes = List.map normalize_process m.processes;
             funcs = List.map normalize_func m.funcs }

let normalize_program (p : bprogram) =
  if Sys.getenv_opt "SVS_NO_NORMALIZE" = Some "1" then p
  else { p with modules = List.map normalize_module p.modules }
