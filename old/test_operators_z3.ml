(* Operator-level Z3 equivalence tests.
 *
 * For each tests/operators/op_*.v we
 *   1. parse it through Verible_to_behavioral to get the lowered BIR;
 *   2. *symbolically execute* the BIR into Z3 with a small encoder that
 *      tracks signal widths, ITE-merges if/else branches, and reduces
 *      logical/reduction operators correctly;
 *   3. add a per-test spec describing the *SV-correct* result and ask
 *      Z3 to find a counterexample (SAT ⇒ bug; UNSAT ⇒ proven).
 *
 * The legacy [Behavioral_to_z3] encoder hardcodes 32-bit BVars and
 * collapses BIf to "encode both branches", which is unsound for
 * exactly the lowering bugs we want to catch (e.g. `&&` lowered as
 * bitwise `&` on a 5-bit operand).  This file keeps its own encoder so
 * those bugs surface as Z3 counterexamples in seconds, before they
 * reach a whole-CPU xsim. *)

open Behavioral_ir

let ctx = Z3.mk_context [("model","true"); ("proof","false")]

(* ---------- Z3 helpers ---------- *)
let bv s w = Z3.BitVector.mk_const_s ctx s w
let bvk v w = Z3.BitVector.mk_numeral ctx (string_of_int v) w
let eq a b = Z3.Boolean.mk_eq ctx a b
let neq a b = Z3.Boolean.mk_not ctx (eq a b)
let band xs = Z3.Boolean.mk_and ctx xs
let bnot a = Z3.Boolean.mk_not ctx a
let ite c t e = Z3.Boolean.mk_ite ctx c t e
let bvw e = Z3.BitVector.get_size (Z3.Expr.get_sort e)
let to_bool e = neq e (bvk 0 (bvw e))
let to_bit e = ite (to_bool e) (bvk 1 1) (bvk 0 1)
let extend_to e w =
  let cw = bvw e in
  if cw = w then e
  else if cw < w then Z3.BitVector.mk_zero_ext ctx (w - cw) e
  else Z3.BitVector.mk_extract ctx (w-1) 0 e

(* ---------- width table ---------- *)
let width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | _ -> 32

let build_width_tbl bmod =
  let h = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    Hashtbl.replace h s.name (width_of_btype s.stype)) bmod.signals;
  h

(* ---------- expression encoder ---------- *)
(* env : current symbolic value of each signal (post any prior blocking
 *       writes in this process), keyed by name.  Reads of a name not in
 *       env fall back to a free Z3 BV constant of the declared width. *)
let rec enc env wt e : Z3.Expr.expr =
  match e with
  | BVar n ->
      (try Hashtbl.find env n
       with Not_found ->
         let w = try Hashtbl.find wt n with Not_found -> 32 in
         bv n w)
  | BConst { value; width } -> bvk value width
  | BBinOp { op; lhs; rhs; result_type } ->
      let rw = width_of_btype result_type in
      let l = enc env wt lhs and r = enc env wt rhs in
      let max_w = max (bvw l) (bvw r) in
      let l' = extend_to l max_w and r' = extend_to r max_w in
      (match op with
       | BAdd -> extend_to (Z3.BitVector.mk_add ctx l' r') rw
       | BSub -> extend_to (Z3.BitVector.mk_sub ctx l' r') rw
       | BMul -> extend_to (Z3.BitVector.mk_mul ctx l' r') rw
       | BDiv -> extend_to (Z3.BitVector.mk_udiv ctx l' r') rw
       | BMod -> extend_to (Z3.BitVector.mk_urem ctx l' r') rw
       | BAnd -> extend_to (Z3.BitVector.mk_and ctx l' r') rw
       | BOr  -> extend_to (Z3.BitVector.mk_or  ctx l' r') rw
       | BXor -> extend_to (Z3.BitVector.mk_xor ctx l' r') rw
       | BShl ->
           let l2 = extend_to l rw and r2 = extend_to r rw in
           Z3.BitVector.mk_shl ctx l2 r2
       | BShr ->
           let l2 = extend_to l rw and r2 = extend_to r rw in
           Z3.BitVector.mk_lshr ctx l2 r2
       | BAshr ->
           let l2 = extend_to l rw and r2 = extend_to r rw in
           Z3.BitVector.mk_ashr ctx l2 r2
       | BEq  -> to_bit (Z3.Expr.mk_app ctx (Z3.FuncDecl.mk_func_decl_s ctx "="
                          [Z3.Expr.get_sort l'; Z3.Expr.get_sort l']
                          (Z3.Boolean.mk_sort ctx)) [l'; r'])
                |> fun b -> extend_to b rw
       | BNe  -> to_bit (Z3.Boolean.mk_not ctx (eq l' r')) |> fun b -> extend_to b rw
       | BLt  -> to_bit (Z3.BitVector.mk_ult ctx l' r') |> fun b -> extend_to b rw
       | BLe  -> to_bit (Z3.BitVector.mk_ule ctx l' r') |> fun b -> extend_to b rw
       | BGt  -> to_bit (Z3.BitVector.mk_ugt ctx l' r') |> fun b -> extend_to b rw
       | BGe  -> to_bit (Z3.BitVector.mk_uge ctx l' r') |> fun b -> extend_to b rw)
  | BUnOp { op; operand; result_type } ->
      let rw = width_of_btype result_type in
      let o = enc env wt operand in
      (match op with
       | BNot -> extend_to (Z3.BitVector.mk_not ctx o) rw
       | BNeg -> extend_to (Z3.BitVector.mk_neg ctx o) rw
       | BRedOr  -> extend_to (to_bit o) rw
       | BRedAnd ->
           let w = bvw o in
           extend_to (to_bit (eq o (Z3.BitVector.mk_repeat ctx w (bvk 1 1)))) rw
       | BRedXor ->
           let bits = bvw o in
           let rec xor_all i acc =
             if i >= bits then acc
             else xor_all (i+1)
                    (Z3.BitVector.mk_xor ctx acc
                       (Z3.BitVector.mk_extract ctx i i o)) in
           let r1 = xor_all 1 (Z3.BitVector.mk_extract ctx 0 0 o) in
           extend_to r1 rw)
  | BCond { condition; then_val; else_val } ->
      let c = to_bool (enc env wt condition) in
      let t = enc env wt then_val and e = enc env wt else_val in
      let w = max (bvw t) (bvw e) in
      ite c (extend_to t w) (extend_to e w)
  | BSlice { signal; msb; lsb } ->
      Z3.BitVector.mk_extract ctx msb lsb (enc env wt signal)
  | BConcat es ->
      (match List.rev_map (enc env wt) es with
       | [] -> bvk 0 1
       | h::t -> List.fold_left (fun acc e -> Z3.BitVector.mk_concat ctx e acc) h t)
  | BReplicate { count; value } ->
      Z3.BitVector.mk_repeat ctx count (enc env wt value)
  | BSelect _ | BCall _ -> bvk 0 32

(* ---------- statement encoder ---------- *)
let env_lookup env wt n =
  try Hashtbl.find env n
  with Not_found ->
    let w = try Hashtbl.find wt n with Not_found -> 32 in
    bv n w

let rec exec env wt stmts =
  List.iter (exec_one env wt) stmts

and exec_one env wt s =
  match s with
  | BAssign { lhs; rhs } ->
      let rhs_e = enc env wt rhs in
      let cur_w = try bvw (env_lookup env wt lhs)
                  with _ -> try Hashtbl.find wt lhs with Not_found -> bvw rhs_e in
      Hashtbl.replace env lhs (extend_to rhs_e cur_w)
  | BBlock ss -> exec env wt ss
  | BIf { condition; then_stmts; else_stmts } ->
      let cond = to_bool (enc env wt condition) in
      let env_t = Hashtbl.copy env in
      let env_e = Hashtbl.copy env in
      exec env_t wt then_stmts;
      exec env_e wt else_stmts;
      let touched = Hashtbl.create 8 in
      Hashtbl.iter (fun k _ -> Hashtbl.replace touched k ()) env_t;
      Hashtbl.iter (fun k _ -> Hashtbl.replace touched k ()) env_e;
      Hashtbl.iter (fun k () ->
        let t_v = env_lookup env_t wt k in
        let e_v = env_lookup env_e wt k in
        let w = max (bvw t_v) (bvw e_v) in
        Hashtbl.replace env k (ite cond (extend_to t_v w) (extend_to e_v w))
      ) touched
  | BCase { selector; cases; default } ->
      let sel = enc env wt selector in
      let envs = List.map (fun (v, body) ->
        let v_e = enc env wt v in
        let w = max (bvw sel) (bvw v_e) in
        let c = eq (extend_to sel w) (extend_to v_e w) in
        let env' = Hashtbl.copy env in
        exec env' wt body;
        (c, env')) cases in
      let env_def = Hashtbl.copy env in
      exec env_def wt default;
      let touched = Hashtbl.create 8 in
      List.iter (fun (_, e) -> Hashtbl.iter (fun k _ -> Hashtbl.replace touched k ()) e) envs;
      Hashtbl.iter (fun k _ -> Hashtbl.replace touched k ()) env_def;
      Hashtbl.iter (fun k () ->
        let final = List.fold_right (fun (c, e_env) acc ->
          let v = env_lookup e_env wt k in
          let w = max (bvw v) (bvw acc) in
          ite c (extend_to v w) (extend_to acc w)
        ) envs (env_lookup env_def wt k) in
        Hashtbl.replace env k final) touched
  | BWhile { body; _ } | BFor { body; _ } -> exec env wt body
  | BCallStmt { func = "@slice_write"; args = [BVar n; m_e; l_e; rhs_e] } ->
      let int_of_bexpr = function
        | BConst { value; _ } -> value
        | _ -> 0 in
      let m_i = int_of_bexpr m_e and l_i = int_of_bexpr l_e in
      let cur = env_lookup env wt n in
      let w_full = bvw cur in
      let w_slice = max 1 (m_i - l_i + 1) in
      let rhs_v = enc env wt rhs_e in
      let rhs_slice = extend_to rhs_v w_slice in
      let pieces = List.filter_map (fun x -> x) [
        (if m_i + 1 <= w_full - 1
         then Some (Z3.BitVector.mk_extract ctx (w_full - 1) (m_i + 1) cur)
         else None);
        Some rhs_slice;
        (if l_i >= 1
         then Some (Z3.BitVector.mk_extract ctx (l_i - 1) 0 cur)
         else None);
      ] in
      let new_v = match pieces with
        | [] -> cur
        | [p] -> p
        | p :: rest ->
            List.fold_left (fun acc x -> Z3.BitVector.mk_concat ctx acc x) p rest in
      Hashtbl.replace env n new_v
  | BCallStmt { func; args = [BVar n; base_e; width_e; rhs_e] }
    when func = "@part_sel_write_up" || func = "@part_sel_write_down" ->
      let int_of_bexpr = function
        | BConst { value; _ } -> value
        | _ -> 0 in
      let base = int_of_bexpr base_e and w = int_of_bexpr width_e in
      let m_i, l_i = match func with
        | "@part_sel_write_up"   -> base + w - 1, base
        | "@part_sel_write_down" -> base, base - w + 1
        | _ -> base + w - 1, base in
      let cur = env_lookup env wt n in
      let w_full = bvw cur in
      let w_slice = max 1 (m_i - l_i + 1) in
      let rhs_v = enc env wt rhs_e in
      let rhs_slice = extend_to rhs_v w_slice in
      let pieces = List.filter_map (fun x -> x) [
        (if m_i + 1 <= w_full - 1
         then Some (Z3.BitVector.mk_extract ctx (w_full - 1) (m_i + 1) cur)
         else None);
        Some rhs_slice;
        (if l_i >= 1
         then Some (Z3.BitVector.mk_extract ctx (l_i - 1) 0 cur)
         else None);
      ] in
      let new_v = match pieces with
        | [] -> cur
        | [p] -> p
        | p :: rest ->
            List.fold_left (fun acc x -> Z3.BitVector.mk_concat ctx acc x) p rest in
      Hashtbl.replace env n new_v
  | BCallStmt _ | BReturn _ -> ()

(* Encode a module: run BSequential processes exactly once (each models
 * one clock edge), then iterate BCombinational processes to fix point
 * so cross-process references resolve.  Verible emits one
 * BCombinational per continuous `assign` so `assign hi = hi_r` and
 * `always @* hi_r = data[7:4]` are separate processes; on the first
 * combinational pass env[hi] is still the free `BVar hi_r`.  N+1
 * passes suffice for N combinational processes when the dataflow is
 * acyclic — each additional pass propagates one level of inter-
 * process dependency.  Re-running sequential processes would double-
 * apply state updates (`x := x + 1` would run N times). *)
let encode bmod =
  let wt = build_width_tbl bmod in
  let env = Hashtbl.create 64 in
  let combs = List.filter (function BCombinational _ -> true | _ -> false)
                bmod.processes in
  let seqs = List.filter (function BSequential _ -> true | _ -> false)
               bmod.processes in
  List.iter (function
    | BSequential { body; _ } -> exec env wt body
    | _ -> ()
  ) seqs;
  let n_combs = List.length combs in
  for _ = 0 to n_combs do
    List.iter (function
      | BCombinational { body; _ } -> exec env wt body
      | _ -> ()
    ) combs
  done;
  (env, wt)

(* ---------- test driver ---------- *)
let prove_property ~test_name solver prop =
  Z3.Solver.add solver [ bnot prop ];
  match Z3.Solver.check solver [] with
  | Z3.Solver.UNSATISFIABLE ->
      Printf.printf "  %-20s PASS\n%!" test_name; true
  | Z3.Solver.SATISFIABLE ->
      Printf.printf "  %-20s FAIL  (counterexample)\n%!" test_name;
      (match Z3.Solver.get_model solver with
       | Some m -> Printf.printf "    %s\n%!" (Z3.Model.to_string m)
       | None -> ());
      false
  | Z3.Solver.UNKNOWN ->
      Printf.printf "  %-20s UNKNOWN\n%!" test_name; false

(* property_builder receives the post-process env so it can look up the
 * symbolic next-value of any output / reg. *)
let tests : (string * string * ((string,Z3.Expr.expr) Hashtbl.t -> (string,int) Hashtbl.t -> Z3.Expr.expr)) list = [
  (* op_logand: y = (a && b) ? 1 : 0  with a:1, b:5.  Lowering bug if
   * `&&` is bitwise `&` on the zero-extended operands. *)
  "op_logand", "tests/operators/op_logand.v", (fun env _wt ->
    let a = bv "a" 1 and b = bv "b" 5 in
    let y = env_lookup env _wt "y" in
    let expected = ite (band [to_bool a; to_bool b]) (bvk 1 1) (bvk 0 1) in
    eq y (extend_to expected (bvw y)));

  (* op_if_logand5: clocked  if (cw && rd) x <= val.  Buggy when `&&`
   * lowered as `&` — only bit 0 of rd masks the write. *)
  "op_if_logand5", "tests/operators/op_if_logand5.v", (fun env _wt ->
    let cw = bv "cw" 1 and rd = bv "rd" 5 in
    let val_ = bv "val" 32 in
    let x_prev = bv "x" 32 in  (* free: any prior register value *)
    let x_next = env_lookup env _wt "x" in
    let cond = band [to_bool cw; to_bool rd] in
    eq x_next (ite cond val_ x_prev));

  (* op_radix: constant assignment 32'h 0010_0000 — based-literal radix. *)
  "op_radix", "tests/operators/op_radix.v", (fun env _wt ->
    let y = env_lookup env _wt "y" in
    eq y (bvk 0x00100000 32));

  (* op_shl_widen: y = data[31:12] << 12 in 32-bit LHS context.  Buggy
   * when shift is evaluated in the 20-bit operand width. *)
  "op_shl_widen", "tests/operators/op_shl_widen.v", (fun env _wt ->
    let data = bv "data" 32 in
    let y = env_lookup env _wt "y" in
    let slice = Z3.BitVector.mk_extract ctx 31 12 data in
    let widened = Z3.BitVector.mk_zero_ext ctx 12 slice in
    eq y (Z3.BitVector.mk_shl ctx widened (bvk 12 32)));

  (* op_block_inc: clocked  x = (!resetn) ? 0 : x + 1  — blocking-self-ref. *)
  "op_block_inc", "tests/operators/op_block_inc.v", (fun env _wt ->
    let resetn = bv "resetn" 1 in
    let x_prev = bv "x" 8 in
    let x_next = env_lookup env _wt "x" in
    let plus1 = Z3.BitVector.mk_add ctx x_prev (bvk 1 8) in
    eq x_next (ite (to_bool resetn) plus1 (bvk 0 8)));

  (* op_concat_lhs: 8-bit adder via two 4-bit chunks with explicit
   * carry, using `{carry, sum_slice[w-1:0]} = lhs[w-1:0] + ...`.
   * Verible emits a concat-LHS; our lowering must NOT collapse each
   * part to a whole-signal write.  This is the picorv32 pcpi_mul
   * carry-save idiom that produces the comb loop. *)
  "op_concat_lhs", "tests/operators/op_concat_lhs.v", (fun env _wt ->
    let a = bv "a" 8 and b = bv "b" 8 in
    let y = env_lookup env _wt "y" in
    let co = env_lookup env _wt "co" in
    (* Spec: y == (a + b)[7:0], co == carry-out of the high nibble add. *)
    let sum_lo = Z3.BitVector.mk_add ctx
                   (Z3.BitVector.mk_zero_ext ctx 1 (Z3.BitVector.mk_extract ctx 3 0 a))
                   (Z3.BitVector.mk_zero_ext ctx 1 (Z3.BitVector.mk_extract ctx 3 0 b)) in
    let lo = Z3.BitVector.mk_extract ctx 3 0 sum_lo in
    let c_lo = Z3.BitVector.mk_extract ctx 4 4 sum_lo in
    let sum_hi = Z3.BitVector.mk_add ctx
                   (Z3.BitVector.mk_add ctx
                      (Z3.BitVector.mk_zero_ext ctx 1 (Z3.BitVector.mk_extract ctx 7 4 a))
                      (Z3.BitVector.mk_zero_ext ctx 1 (Z3.BitVector.mk_extract ctx 7 4 b)))
                   (Z3.BitVector.mk_zero_ext ctx 4 c_lo) in
    let hi = Z3.BitVector.mk_extract ctx 3 0 sum_hi in
    let c_hi = Z3.BitVector.mk_extract ctx 4 4 sum_hi in
    band [ eq y (Z3.BitVector.mk_concat ctx hi lo)
         ; eq co c_hi ]);

  (* op_bit_select: y0 = pc[0]; y31 = pc[31].  Caught the picorv32
   * MISALIGNED-INSTRUCTION false trap: the BIR encoded the bit-0
   * select but the emit produced pc[31:31] under certain contexts. *)
  "op_bit_select", "tests/operators/op_bit_select.v", (fun env _wt ->
    let pc = bv "pc" 32 in
    let y0 = env_lookup env _wt "y0" in
    let y31 = env_lookup env _wt "y31" in
    band [ eq y0 (Z3.BitVector.mk_extract ctx 0 0 pc)
         ; eq y31 (Z3.BitVector.mk_extract ctx 31 31 pc) ]);

  (* op_case_fsm: 4-state FSM in a case statement.  Encodes the
   * picorv32 pattern where each `cpu_state_X: cpu_state <= Y` branch
   * must NOT leak its RHS into the register update for a different
   * branch.  If lowering loses the per-branch guard, we'll see the
   * default/reset/trap target chosen unconditionally. *)
  "op_case_fsm", "tests/operators/op_case_fsm.v", (fun env _wt ->
    let resetn = bv "resetn" 1 in
    let go = bv "go" 1 in
    let state_prev = bv "state" 2 in
    let state_next = env_lookup env _wt "state" in
    let s0 = bvk 0 2 and s1 = bvk 1 2 and s2 = bvk 2 2 and s3 = bvk 3 2 in
    let case_next =
      ite (eq state_prev s0) (ite (to_bool go) s1 s0)
        (ite (eq state_prev s1) s2
          (ite (eq state_prev s2) s0 s3)) in
    eq state_next (ite (to_bool resetn) case_next s0));

  (* op_pc_misalign: picorv32 MISALIGNED-INSTRUCTION pattern,
   * `cpr ? pc[0] : |pc[1:0]`.  Verible parses `pc[0]` here as a
   * 32-bit-indexed BSelect (not a BSlice) — the hardcaml lowering
   * must reduce that to a 1-bit slice, not return the whole 32-bit
   * signal.  If the lowering returns the whole signal, the outer
   * `(|...)` BRedOr fires for any non-zero pc, producing a false
   * misalign trap on the first fetch (pc=0x00100000, |pc=1).        *)
  "op_pc_misalign", "tests/operators/op_pc_misalign.v", (fun env _wt ->
    let cpr = bv "cpr" 1 in
    let pc = bv "pc" 32 in
    let y = env_lookup env _wt "y" in
    let pc0 = Z3.BitVector.mk_extract ctx 0 0 pc in
    let pc10 = Z3.BitVector.mk_extract ctx 1 0 pc in
    let or_pc10 = to_bit pc10 in
    let expected = ite (to_bool cpr) pc0 or_pc10 in
    eq y (extend_to expected (bvw y)));

  (* op_concat_lhs_balanced: `{hi, lo} = data` where total LHS width
   * (4+4) matches RHS width (8) — sidesteps the operator-context-
   * width gap (op_concat_lhs trips that one).  Tests that each part
   * of the concat-LHS receives the correct slice of the RHS, not the
   * whole-signal value. *)
  "op_concat_lhs_balanced", "tests/operators/op_concat_lhs_balanced.v",
  (fun env _wt ->
    let data = bv "data" 8 in
    let hi = env_lookup env _wt "hi" in
    let lo = env_lookup env _wt "lo" in
    band [ eq hi (Z3.BitVector.mk_extract ctx 7 4 data)
         ; eq lo (Z3.BitVector.mk_extract ctx 3 0 data) ]);

  (* op_slice_self: r = in; if (sel) r[3:0] = r[3:0] ^ 4'hF.
   * Catches (1) verible side encoding `r[3:0] = ...` as a slice-write
   * rather than a whole-signal BAssign and (2) the SSA pass versioning
   * `r` so the slice-write's RHS reads the previous version. *)
  "op_slice_self", "tests/operators/op_slice_self.v", (fun env _wt ->
    let in_ = bv "in" 8 in
    let sel = bv "sel" 1 in
    let out = env_lookup env _wt "out" in
    let hi = Z3.BitVector.mk_extract ctx 7 4 in_ in
    let lo = Z3.BitVector.mk_extract ctx 3 0 in_ in
    let lo_xor = Z3.BitVector.mk_xor ctx lo (bvk 0xF 4) in
    let toggled = Z3.BitVector.mk_concat ctx hi lo_xor in
    eq out (ite (to_bool sel) toggled in_));

  (* op_unrolled_slice_chain: distilled picorv32 pcpi_mul carry-save
   * pattern — 4 iterations of `r[i+:2] = r[i+:2] ^ 2'b11` after
   * `r = init`.  After unroll the BIR has 4 `@part_sel_write_up`
   * calls on `r` that each read their own slice.  Without SSA
   * versioning, the merged BAssign would close a self-reference loop
   * through `Always.Variable.value r` — yosys-scc reported 16 SCCs
   * on picosoc before the fix; this seed exercises the same pattern
   * at small scale. *)
  "op_unrolled_slice_chain", "tests/operators/op_unrolled_slice_chain.v",
  (fun env _wt ->
    let init = bv "init" 8 in
    let out = env_lookup env _wt "out" in
    let flip = bvk 0xFF 8 in
    eq out (Z3.BitVector.mk_xor ctx init flip));
]

let run_one (name, file, prop_builder) =
  Printf.printf "── %s ──\n%!" name;
  if not (Sys.file_exists file) then begin
    Printf.printf "  %-20s SKIP  (file not found: %s)\n%!" name file;
    false
  end else
    try
      let prog = Verible_to_behavioral.convert_files ~top:name [file] in
      (* Run the same lowering passes test_picosoc_gates uses (minus
         memlower/SSA — neither runs on these small operator seeds in
         a useful way) so for-loops are unrolled and if-trees lifted
         before the Z3 encoder sees them.  op_unrolled_slice_chain
         needs unroll to expose its 4 iterations; op_case_fsm and
         friends were tolerant of either path. *)
      let prog = prog
        |> Behavioral_unroll.unroll_program
        |> Behavioral_inline.inline_program
        |> Behavioral_iflift.lift_program
        |> Behavioral_blocking_subst.blocking_subst_program in
      let bmod =
        try List.find (fun (m : bmodule) -> m.name = name) prog.modules
        with Not_found ->
          Printf.printf "  %-20s SKIP  (module %s not in BIR)\n%!" name name;
          raise Exit in
      let (env, wt) = encode bmod in
      let solver = Z3.Solver.mk_simple_solver ctx in
      let prop = prop_builder env wt in
      prove_property ~test_name:name solver prop
    with
    | Exit -> false
    | e ->
        Printf.printf "  %-20s ERROR  %s\n%!" name (Printexc.to_string e);
        false

let () =
  Printf.printf "═══ Operator-level Z3 equivalence tests ═══\n%!";
  let results = List.map run_one tests in
  let pass = List.length (List.filter (fun b -> b) results) in
  let fail = List.length results - pass in
  Printf.printf "\n── %d passed / %d failed ──\n" pass fail;
  if fail > 0 then exit 1
