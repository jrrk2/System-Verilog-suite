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
  | BCallStmt { func = ("@slice_write" | "@mem_write") as func;
               args = (BVar _ as tgt) :: rest } ->
      (* The FIRST arg is the write TARGET, not a read — substitution only ever
         rewrites RHS/read positions, never a write target (else a partial write
         is redirected to a different signal). *)
      BCallStmt { func; args = tgt :: List.map (map_expr f) rest }
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

(* True if `name` is the TARGET of a @slice_write / @mem_write ANYWHERE in the
 * stmt list, including nested BIf / BCase / BBlock.  Such a variable is a
 * register that is PARTIALLY written, so its top-level default assignment
 * (`sbaddr_d := sbaddress_i`) is NOT the whole story — substituting it forward
 * rewrites the partial-write TARGET itself (map_stmt maps the @slice_write's
 * first arg), turning `@slice_write(sbaddr_d,..)` into
 * `@slice_write(sbaddress_i,..)` and clobbering an unrelated signal (a real
 * input!).  Exclude such variables from substitution entirely. *)
let rec partial_write_target name stmts =
  let is_bracket_write lhs =
    (* a bracket-keyed part/bit-select write "name[..]" (an earlier pass may have
       lowered @slice_write into this BAssign form). *)
    let ln = String.length name in
    String.length lhs > ln && String.sub lhs 0 ln = name && lhs.[ln] = '[' in
  List.exists (function
    | BCallStmt { func = ("@slice_write" | "@mem_write"); args = BVar n :: _ } ->
        n = name
    | BAssign { lhs; _ } -> is_bracket_write lhs
    | BIf { then_stmts; else_stmts; _ } ->
        partial_write_target name then_stmts
        || partial_write_target name else_stmts
    | BCase { cases; default; _ } ->
        List.exists (fun (_, ss) -> partial_write_target name ss) cases
        || partial_write_target name default
    | BBlock ss -> partial_write_target name ss
    | _ -> false) stmts

let process_seq_body stmts =
  (* Pass 1: identify candidate substitutions: variables assigned
   * exactly once at the top level of the block AND read at least
   * once in subsequent statements. *)
  let rec collect i acc = function
    | [] -> List.rev acc
    | BAssign { lhs; rhs } as s :: rest
      when assignment_count lhs stmts = 1
        && count_in_stmts lhs rest >= 1
        && not (partial_write_target lhs stmts) ->
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

(* Names assigned anywhere in a statement (any branch depth). *)
let rec assigned_names acc = function
  | BAssign { lhs; _ } -> if List.mem lhs acc then acc else lhs :: acc
  | BIf { then_stmts; else_stmts; _ } ->
      List.fold_left assigned_names
        (List.fold_left assigned_names acc then_stmts) else_stmts
  | BCase { cases; default; _ } ->
      let acc = List.fold_left
        (fun a (_, ss) -> List.fold_left assigned_names a ss) acc cases in
      List.fold_left assigned_names acc default
  | BBlock ss -> List.fold_left assigned_names acc ss
  | BCallStmt { args = BVar n :: _; _ } ->
      if List.mem n acc then acc else n :: acc
  | _ -> acc

(* Does a statement contain a SELF-REFERENCING blocking assignment at ANY
   depth — `x = f(x)`, the iflift encoding of a no-else `if` that keeps the
   prior value?  Those are exactly the assignments that, left un-threaded,
   become a combinational loop. *)
let rec has_self_ref_stmt = function
  | BAssign { lhs; rhs } -> count_in_expr lhs rhs > 0
  | BIf { then_stmts; else_stmts; _ } ->
      List.exists has_self_ref_stmt then_stmts
      || List.exists has_self_ref_stmt else_stmts
  | BCase { cases; default; _ } ->
      List.exists (fun (_, ss) -> List.exists has_self_ref_stmt ss) cases
      || List.exists has_self_ref_stmt default
  | BBlock ss -> List.exists has_self_ref_stmt ss
  | _ -> false

(* Blocking→non-blocking conversion for a COMBINATIONAL block.  In always_comb,
 * `=` is blocking, so each read sees the most recent in-block write.  iflift
 * turns a no-else `if (c) x = v;` into the self-reference `x = c ? v : x`
 * (the else "keeps prior"); a `default; conditional-override` idiom — e.g.
 * dmi_jtag's shift registers
 *     dr_d = dr_q;                              (* default *)
 *     if (clear) dr_d = 0;
 *     else begin if (cap) if(sel) dr_d = …;     (* → dr_d = c ? … : dr_d *)
 *                if (shift) if(sel) dr_d = … end
 * then reads `dr_d`, and the Hardcaml lowering sees a combinational loop.
 * Thread the value-so-far (an env from names to their current expression)
 * through the block AND into every BIf/BCase branch, substituting each read;
 * the self-references resolve to the entering default and the loop dissolves.
 * Branch results are merged back (`cond ? then : else`) so post-branch reads
 * stay correct.  ONLY blocks that actually contain a self-reference are
 * rewritten — everything else is returned byte-for-byte untouched, so this
 * never perturbs blocks that already lower correctly. *)
let thread_comb_body orig_stmts =
  let rec flatten = function
    | BBlock ss :: rest -> flatten (ss @ rest)
    | s :: rest -> s :: flatten rest
    | [] -> [] in
  let stmts = flatten orig_stmts in
  if not (List.exists has_self_ref_stmt stmts) then orig_stmts
  else begin
    let env : (string, bexpr) Hashtbl.t = Hashtbl.create 16 in
    let subst e = map_expr (fun n -> Hashtbl.find_opt env n) e in
    let snapshot () = Hashtbl.fold (fun k v a -> (k, v) :: a) env [] in
    let restore snap =
      Hashtbl.reset env; List.iter (fun (k, v) -> Hashtbl.replace env k v) snap in
    let merge cond' snap_then snap_else names =
      List.iter (fun k ->
        let pv = match Hashtbl.find_opt env k with Some v -> v | None -> BVar k in
        let tv = match List.assoc_opt k snap_then with Some v -> v | None -> pv in
        let ev = match List.assoc_opt k snap_else with Some v -> v | None -> pv in
        let m = if tv = ev then tv
                else BCond { condition = cond'; then_val = tv; else_val = ev } in
        Hashtbl.replace env k m) names in
    let rec thread_stmt s =
      match s with
      | BAssign { lhs; rhs } ->
          let rhs' = subst rhs in
          Hashtbl.replace env lhs rhs';
          (* A bracket part/bit-select write `base[..] = ..` also invalidates the
             threaded whole-`base` value: later reads of `base` must see the
             partially-updated register, not the entering default. *)
          (match String.index_opt lhs '[' with
           | Some i when i > 0 -> Hashtbl.remove env (String.sub lhs 0 i)
           | _ -> ());
          BAssign { lhs; rhs = rhs' }
      | BIf { condition; then_stmts; else_stmts } ->
          let cond' = subst condition in
          let names = assigned_names (assigned_names [] (BBlock then_stmts))
                        (BBlock else_stmts) in
          let snap = snapshot () in
          let te = List.map thread_stmt then_stmts in
          let snap_then = snapshot () in
          restore snap;
          let ee = List.map thread_stmt else_stmts in
          let snap_else = snapshot () in
          restore snap;
          merge cond' snap_then snap_else names;
          (* A var that was partially written in a branch (a bracket / @mem_write
             LHS → removed from env by thread_stmt) has an uncertain post-branch
             value: it is NO LONGER equal to its entering default.  The merge
             above can't express it (its name isn't in `names` — a bracketed lhs
             `rdata[i]` collects as "rdata[i]", not "rdata"), so INVALIDATE any
             entering var that disappeared from either branch snapshot.  Without
             this, `rdata='0; if(c) rdata[i]={..}; rdata_d = rdata` folded to
             `rdata_d = 0`, dropping dm_mem's go-flag byte. *)
          List.iter (fun (k, _) ->
            if not (List.mem_assoc k snap_then)
               || not (List.mem_assoc k snap_else) then Hashtbl.remove env k) snap;
          BIf { condition = cond'; then_stmts = te; else_stmts = ee }
      | BCase { selector; cases; default } ->
          let sel' = subst selector in
          let names =
            List.fold_left (fun a (_, ss) ->
              List.fold_left assigned_names a ss)
              (List.fold_left assigned_names [] default) cases in
          let snap = snapshot () in
          let cases' = List.map (fun (k, ss) ->
            let k' = subst k in
            let ss' = List.map thread_stmt ss in
            let sn = snapshot () in restore snap; (k', ss', sn)) cases in
          let default' = List.map thread_stmt default in
          let snap_def = snapshot () in
          restore snap;
          (* Merge arms fold: sel==k0 ? env_k0 : … : default. *)
          List.iter (fun nm ->
            let pv = match Hashtbl.find_opt env nm with Some v -> v | None -> BVar nm in
            let vdef = match List.assoc_opt nm snap_def with Some v -> v | None -> pv in
            let m = List.fold_right (fun (k', _, sn) acc ->
              let va = match List.assoc_opt nm sn with Some v -> v | None -> pv in
              if va = acc then acc
              else BCond { condition = BBinOp { op = BEq; lhs = sel'; rhs = k';
                                                result_type = BBool };
                           then_val = va; else_val = acc }) cases' vdef in
            Hashtbl.replace env nm m) names;
          (* Same partial-write invalidation as BIf: any entering var that a
             case arm (or default) removed from env is now uncertain. *)
          List.iter (fun (k, _) ->
            if (not (List.mem_assoc k snap_def))
               || List.exists (fun (_, _, sn) -> not (List.mem_assoc k sn)) cases'
            then Hashtbl.remove env k) snap;
          BCase { selector = sel';
                  cases = List.map (fun (k', ss', _) -> (k', ss')) cases';
                  default = default' }
      | BBlock ss -> BBlock (List.map thread_stmt ss)
      | BCallStmt { func = ("@slice_write" | "@mem_write") as func;
                    args = (BVar tgt as tgt_e) :: rest } ->
          (* The FIRST arg is the write TARGET — NEVER substitute it, else the
             partial write is redirected to whatever value was threaded for that
             name (the `sbaddr_d := sbaddress_i` default made
             `@slice_write(sbaddr_d,..)` become `@slice_write(sbaddress_i,..)`,
             clobbering a module INPUT).  Substitute only the index/value args,
             and invalidate the target's threaded value (it is now partially
             re-written; later reads must see the real signal, not the default). *)
          Hashtbl.remove env tgt;
          BCallStmt { func; args = tgt_e :: List.map subst rest }
      | BCallStmt { func; args } ->
          BCallStmt { func; args = List.map subst args }
      | BReturn (Some e) -> BReturn (Some (subst e))
      | other -> other in
    List.map thread_stmt stmts
  end

let process_module (m : bmodule) : bmodule =
  let new_processes = List.map (function
    | BSequential s -> BSequential { s with body = process_seq_body s.body }
    | BCombinational s ->
        BCombinational { s with body = thread_comb_body s.body }
    | other -> other
  ) m.processes in
  { m with processes = new_processes }

let blocking_subst_program (p : bprogram) : bprogram =
  { p with modules = List.map process_module p.modules }
