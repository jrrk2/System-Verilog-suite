(* Post-synthesis FF sharing — find flip-flops whose D-pin functions
 * are structurally identical and merge them into a single register
 * with output aliases. Mimics what `yosys opt -fast share` and
 * Vivado's elaborator do automatically: if two FFs always latch the
 * same value, only one register is needed and the other Q-port can
 * be a wire-alias of the survivor.
 *
 * Runs after Behavioral_ffrip.rip_module so the per-FF Q__Q (input)
 * and Q__D (output) signals exist. For each cluster of duplicates:
 *
 *   - Pick canonical Q (alphabetically first — matches yosys's
 *     `share` pass tie-break in practice).
 *   - Drop the duplicate's Q_dup__Q input and Q_dup__D output, plus
 *     the BCombinational that drove Q_dup__D.
 *   - Drop the duplicate's `Q_dup = Q_dup__Q` pass-through process.
 *   - If Q_dup was a primary output port, add a fresh pass-through
 *     `Q_dup = Q_canon__Q` so the port still gets driven.
 *   - Substitute every BVar Q_dup__Q in the rest of the module with
 *     BVar Q_canon__Q (used by other FFs' D-cones, etc.).
 *
 * Why structural equality on the RHS is sufficient: post-FF-rip the
 * expressions are pure functions of primary inputs and Q__Q free
 * variables. Two RHS that are structurally equal compute the same
 * value for every input/state combination, so the two registers
 * carry the same value at every cycle (modulo the initial state,
 * which the miter treats as an unconstrained free variable on each
 * side anyway). *)

open Behavioral_ir

let suffix_is s sfx =
  let l  = String.length s in
  let ls = String.length sfx in
  l > ls && String.sub s (l - ls) ls = sfx

let strip_suffix s sfx =
  String.sub s 0 (String.length s - String.length sfx)

let rec subst_var renames = function
  | BVar n ->
      let n' =
        if suffix_is n "__Q" then
          let base = strip_suffix n "__Q" in
          (try List.assoc base renames ^ "__Q" with Not_found -> n)
        else n
      in
      BVar n'
  | BConst _ as e -> e
  | BBinOp r ->
      BBinOp { r with lhs = subst_var renames r.lhs;
                      rhs = subst_var renames r.rhs }
  | BUnOp r ->
      BUnOp { r with operand = subst_var renames r.operand }
  | BSelect { array; index } ->
      BSelect { array = subst_var renames array;
                index = subst_var renames index }
  | BSlice r ->
      BSlice { r with signal = subst_var renames r.signal }
  | BConcat es ->
      BConcat (List.map (subst_var renames) es)
  | BReplicate r ->
      BReplicate { r with value = subst_var renames r.value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition = subst_var renames condition;
              then_val  = subst_var renames then_val;
              else_val  = subst_var renames else_val }
  | BCall r ->
      BCall { r with args = List.map (subst_var renames) r.args }

let rec subst_stmt renames = function
  | BAssign { lhs; rhs } -> BAssign { lhs; rhs = subst_var renames rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition = subst_var renames condition;
            then_stmts = List.map (subst_stmt renames) then_stmts;
            else_stmts = List.map (subst_stmt renames) else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = subst_var renames selector;
              cases = List.map
                (fun (k, ss) -> (subst_var renames k,
                                 List.map (subst_stmt renames) ss)) cases;
              default = List.map (subst_stmt renames) default }
  | BBlock ss -> BBlock (List.map (subst_stmt renames) ss)
  | BWhile { condition; body } ->
      BWhile { condition = subst_var renames condition;
               body = List.map (subst_stmt renames) body }
  | BFor { init; condition; update; body } ->
      BFor { init = subst_stmt renames init;
             condition = subst_var renames condition;
             update = subst_stmt renames update;
             body = List.map (subst_stmt renames) body }
  | BCallStmt r ->
      BCallStmt { r with args = List.map (subst_var renames) r.args }
  | BReturn (Some e) -> BReturn (Some (subst_var renames e))
  | BReturn None -> BReturn None

let subst_proc renames = function
  | BCombinational c ->
      BCombinational { c with body = List.map (subst_stmt renames) c.body }
  | BSequential s ->
      BSequential { s with body = List.map (subst_stmt renames) s.body }

let share_module (m : bmodule) : bmodule =
  (* Step 1: collect (Q, D-cone) for every BCombinational that drives
   * a `<Q>__D` signal. Only single-statement BAssign processes count
   * — anything with extra side effects we leave alone. *)
  let d_drivers =
    List.filter_map (function
      | BCombinational { body = [BAssign { lhs; rhs }]; _ }
        when suffix_is lhs "__D" -> Some (strip_suffix lhs "__D", rhs)
      | _ -> None
    ) m.processes
  in
  (* Step 2: cluster Qs whose RHS expressions are structurally equal.
   * OCaml's `=` walks the tree; BIR exprs are pure data so this is
   * the canonical syntactic match yosys uses too. *)
  let groups = ref [] in
  List.iter (fun (q, e) ->
    let rec add = function
      | [] -> [(e, [q])]
      | (e', qs) :: tl when e = e' -> (e', q :: qs) :: tl
      | hd :: tl -> hd :: add tl
    in
    groups := add !groups
  ) d_drivers;
  (* Step 3: build the rename map (Q_dup → Q_canon). Canonical = the
   * alphabetically-first name in each cluster (matches yosys's
   * tie-break on `share`). *)
  let renames = ref [] in
  List.iter (fun (_, qs) ->
    if List.length qs > 1 then begin
      let sorted = List.sort String.compare qs in
      let canon = List.hd sorted in
      List.iter (fun q ->
        if q <> canon then renames := (q, canon) :: !renames
      ) (List.tl sorted)
    end
  ) !groups;
  if !renames = [] then m
  else begin
    let dup_set = List.map fst !renames in
    let is_output_port name =
      List.exists (fun (s : bsignal) ->
        s.name = name && s.direction = `Output) m.signals
    in
    (* Step 4: rewrite all expressions to use canon's Q__Q. *)
    let processes_subst = List.map (subst_proc !renames) m.processes in
    (* Step 5: drop duplicates' D-cone drivers and pass-throughs, keep
     * everything else. *)
    let processes = List.filter (fun p ->
      match p with
      | BCombinational { body = [BAssign { lhs; _ }]; _ }
        when List.exists (fun d ->
               lhs = d ^ "__D" || lhs = d) dup_set -> false
      | _ -> true
    ) processes_subst in
    (* Step 6: remove the Q_dup__Q (input) and Q_dup__D (output)
     * signals from the table. Keep the bare Q_dup signal — if it was
     * an output port it still has to exist. *)
    let signals = List.filter (fun (s : bsignal) ->
      not (List.exists (fun d ->
        s.name = d ^ "__Q" || s.name = d ^ "__D") dup_set)
    ) m.signals in
    (* Step 7: for each dup that's a primary output, add an alias so
     * the port stays driven from the surviving FF. *)
    let aliases =
      List.filter_map (fun (dup, canon) ->
        if is_output_port dup then
          Some (BCombinational {
            name = "share_alias_" ^ dup;
            sensitivity = [BAny];
            body = [BAssign { lhs = dup; rhs = BVar (canon ^ "__Q") }];
          })
        else None
      ) !renames
    in
    { m with signals; processes = processes @ aliases }
  end

let share_program (p : bprogram) : bprogram =
  { p with modules = List.map share_module p.modules }
