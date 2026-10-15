(* Non-blocking SLICE / single-bit writes in a clocked block -> one threaded
 * next-state expression per register.
 *
 *   always_ff: if (load) r <= {1'b0, d};        // whole-register NBA
 *              if (r[W]) r[W] <= 1'b0;          // later NBA to ONE bit
 *
 * Verilog semantics: the later NBA patches bit W of the value already
 * scheduled for r; the other bits keep the load.  The hardcaml lowering
 * expressed a standalone guarded bit write as a read-modify-write of the
 * register's CURRENT Q ({1'b0, r[W-1:0]}) assigned to the whole register, so
 * it silently overrode the pending load: slib_counter's mapped netlist
 * differed from the three-way-agreed source (Z3 counterexample LOAD=1,
 * r=5'b11001, D=3: source 00011, netlist 01001).
 *
 * Rewrite, per register r that is the target of a constant-range
 * `@slice_write(r, hi, lo, v)` or constant-index `@mem_write(r, k, v)` (the
 * frontend's form of `r[k] <= v` on a VECTOR register) in a BSequential body:
 * thread the pending value through the body in statement order --
 *
 *   cur := r                                  (Q, the value if nothing fires)
 *   r <= e                  cur := e
 *   r[hi:lo] <= v           cur := {cur[W-1:hi+1], v, cur[lo-1:0]}
 *   if (c) A else B         cur := c ? thread(A, cur) : thread(B, cur)
 *   case (s) k: A ... def   cur := s==k ? thread(A,cur) : ... : thread(def,cur)
 *
 * -- delete every write to r from the body and append a single `r <= cur`.
 * Reads of r in the body still mean Q (NBA semantics) because [cur] is a
 * closed expression over Q and the inputs, never a signal.  The result is
 * plain NBA BIR that hardcaml, the Z3 encoder and the emitters all already
 * understand (no blocking temps, which the Z3 side would mistake for
 * registers).  When the body is the usual `if (reset) ... else ...` shape the
 * rewrite is applied inside the else branch so the reset-value extraction
 * sees the untouched reset arm.  Registers written through a loop that the
 * unroller left in place, or with a non-constant index, are left alone. *)

open Behavioral_ir

let b1 = BInt { width = 1; signed = Unsigned }

(* writes to [r] this pass understands *)
let is_slice_of r = function
  | BCallStmt { func = "@slice_write"; args = [ BVar t; BConst _; BConst _; _ ] } -> t = r
  | BCallStmt { func = "@mem_write"; args = [ BVar t; BConst _; _ ] } -> t = r
  | _ -> false

let is_write_of r = function
  | BAssign { lhs; _ } -> lhs = r
  | s -> is_slice_of r s

(* Every register slice/bit-written with constant bounds anywhere in [body]. *)
let rec slice_targets acc = function
  | [] -> acc
  | s :: rest -> slice_targets (stmt_targets acc s) rest
and stmt_targets acc = function
  | BCallStmt { func = "@slice_write"; args = [ BVar t; BConst _; BConst _; _ ] }
  | BCallStmt { func = "@mem_write"; args = [ BVar t; BConst _; _ ] } ->
      if List.mem t acc then acc else t :: acc
  | BIf { then_stmts; else_stmts; _ } -> slice_targets (slice_targets acc then_stmts) else_stmts
  | BCase { cases; default; _ } ->
      slice_targets (List.fold_left (fun a (_, b) -> slice_targets a b) acc cases) default
  | BBlock b | BWhile { body = b; _ } | BFor { body = b; _ } -> slice_targets acc b
  | _ -> acc

(* Can [r] be threaded?  Every write must be a whole assign, an in-range
   constant slice, or an in-range constant bit; none may sit inside a loop. *)
let rec threadable ~w r = function
  | [] -> true
  | s :: rest -> stmt_ok ~w r s && threadable ~w r rest
and stmt_ok ~w r = function
  | BAssign _ -> true
  | BCallStmt { func = "@slice_write"; args = [ BVar t; BConst { value = hi; _ }; BConst { value = lo; _ }; _ ] }
    when t = r -> Z.lt hi (Z.of_int w) && Z.leq Z.zero lo && Z.leq lo hi
  | BCallStmt { func = "@mem_write"; args = [ BVar t; BConst { value = k; _ }; _ ] } when t = r ->
      Z.lt k (Z.of_int w) && Z.leq Z.zero k
  | BCallStmt { args = BVar t :: _; _ } when t = r -> false   (* non-constant index *)
  | BCallStmt _ | BReturn _ -> true
  | BIf { then_stmts; else_stmts; _ } -> threadable ~w r then_stmts && threadable ~w r else_stmts
  | BCase { cases; default; _ } ->
      List.for_all (fun (_, b) -> threadable ~w r b) cases && threadable ~w r default
  | BBlock b -> threadable ~w r b
  | BWhile { body; _ } | BFor { body; _ } -> not (List.exists (stmt_writes r) body)
and stmt_writes r = function
  | BIf { then_stmts; else_stmts; _ } ->
      List.exists (stmt_writes r) then_stmts || List.exists (stmt_writes r) else_stmts
  | BCase { cases; default; _ } ->
      List.exists (fun (_, b) -> List.exists (stmt_writes r) b) cases
      || List.exists (stmt_writes r) default
  | BBlock b | BWhile { body = b; _ } | BFor { body = b; _ } -> List.exists (stmt_writes r) b
  | s -> is_write_of r s

let patch ~w ~cur ~hi ~lo v =
  let parts =
    (if hi + 1 <= w - 1 then [ BSlice { signal = cur; msb = w - 1; lsb = hi + 1 } ] else [])
    @ [ v ]
    @ (if lo >= 1 then [ BSlice { signal = cur; msb = lo - 1; lsb = 0 } ] else []) in
  match parts with [ p ] -> p | ps -> BConcat ps

(* Thread the pending value of [r] through [stmts]; returns the value after. *)
let rec thread ~w r cur = function
  | [] -> cur
  | s :: rest -> thread ~w r (thread_stmt ~w r cur s) rest
and thread_stmt ~w r cur = function
  | BAssign { lhs; rhs } when lhs = r -> rhs
  | BCallStmt { func = "@slice_write"
              ; args = [ BVar t; BConst { value = hi; _ }; BConst { value = lo; _ }; v ] }
    when t = r -> patch ~w ~cur ~hi:(Z.to_int hi) ~lo:(Z.to_int lo) v
  | BCallStmt { func = "@mem_write"; args = [ BVar t; BConst { value = k; _ }; v ] } when t = r ->
      let v1 = match v with
        | BConst { value; _ } -> BConst { value = Z.logand value Z.one; width = 1 }
        | e -> BSlice { signal = e; msb = 0; lsb = 0 } in
      patch ~w ~cur ~hi:(Z.to_int k) ~lo:(Z.to_int k) v1
  | BIf { condition; then_stmts; else_stmts } ->
      if not (List.exists (stmt_writes r) then_stmts || List.exists (stmt_writes r) else_stmts)
      then cur
      else BCond { condition
                 ; then_val = thread ~w r cur then_stmts
                 ; else_val = thread ~w r cur else_stmts }
  | BCase { selector; cases; default } ->
      if not (List.exists (fun (_, b) -> List.exists (stmt_writes r) b) cases
              || List.exists (stmt_writes r) default)
      then cur
      else
        let dflt = thread ~w r cur default in
        List.fold_right (fun (key, body) acc ->
          BCond { condition = BBinOp { op = BEq; lhs = selector; rhs = key; result_type = b1 }
                ; then_val = thread ~w r cur body; else_val = acc }) cases dflt
  | BBlock b -> thread ~w r cur b
  | _ -> cur

(* Remove every write to [r]; empty conditionals that only wrote r collapse. *)
let rec strip r = function
  | [] -> []
  | s :: rest ->
      (match strip_stmt r s with Some s' -> s' :: strip r rest | None -> strip r rest)
and strip_stmt r = function
  | s when is_write_of r s -> None
  | BIf { condition; then_stmts; else_stmts } ->
      let t = strip r then_stmts and e = strip r else_stmts in
      if t = [] && e = [] then None else Some (BIf { condition; then_stmts = t; else_stmts = e })
  | BCase { selector; cases; default } ->
      let cases = List.map (fun (k, b) -> (k, strip r b)) cases in
      let default = strip r default in
      if List.for_all (fun (_, b) -> b = []) cases && default = [] then None
      else Some (BCase { selector; cases; default })
  | BBlock b -> (match strip r b with [] -> None | b' -> Some (BBlock b'))
  | s -> Some s

let rewrite_body ~w r stmts =
  let cur = thread ~w r (BVar r) stmts in
  strip r stmts @ [ BAssign { lhs = r; rhs = cur } ]

let mentions name e =
  let rec go = function
    | BVar n -> n = name
    | BConst _ -> false
    | BBinOp { lhs; rhs; _ } -> go lhs || go rhs
    | BUnOp { operand; _ } -> go operand
    | BSelect { array; index } -> go array || go index
    | BSlice { signal; _ } -> go signal
    | BConcat l -> List.exists go l
    | BReplicate { value; _ } -> go value
    | BCond { condition; then_val; else_val } -> go condition || go then_val || go else_val
    | BCall { args; _ } -> List.exists go args in
  go e

let rewrite_process ~width_of (p : bprocess) : bprocess =
  match p with
  | BCombinational _ -> p
  | BSequential r ->
      let targets =
        List.filter (fun t ->
          match width_of t with
          | Some w -> threadable ~w t r.body
          | None -> false) (List.rev (slice_targets [] r.body)) in
      if targets = [] then p
      else
        let body =
          List.fold_left (fun body t ->
            let w = Option.get (width_of t) in
            match body, r.reset with
            | [ BIf { condition; then_stmts; else_stmts } ], Some rst
              when mentions rst condition
                   && not (List.exists (fun s -> is_slice_of t s) then_stmts) ->
                [ BIf { condition; then_stmts; else_stmts = rewrite_body ~w t else_stmts } ]
            | _ -> rewrite_body ~w t body) r.body targets in
        BSequential { r with body }

let rewrite_module (m : bmodule) : bmodule =
  let width_of n =
    match List.find_opt (fun (s : bsignal) -> s.name = n) m.signals with
    | Some { stype = BInt { width; _ }; _ } -> Some width
    | Some { stype = BBool; _ } -> Some 1
    | _ -> None in
  { m with processes = List.map (rewrite_process ~width_of) m.processes }

let rewrite_program (p : bprogram) : bprogram =
  { p with modules = List.map rewrite_module p.modules }
