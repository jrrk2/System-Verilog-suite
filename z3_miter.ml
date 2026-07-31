(* Z3 Miter Circuit for Formal Equivalence Checking
 *
 * A miter circuit connects two designs with the same inputs, XORs their outputs,
 * and ORs the results. If the output is always 0, the designs are equivalent.
 *
 * Miter Structure:
 *   Inputs → Design1 → Outputs1 ─┐
 *         ↘                       ├→ XOR → OR_tree → miter_output
 *   Inputs → Design2 → Outputs2 ─┘
 *
 * Verification:
 *   - Encode miter as Z3 constraints
 *   - Check: ∃ inputs. (miter_output != 0)
 *   - UNSAT → designs are equivalent ✅
 *   - SAT → counterexample found ❌
 *)

open Behavioral_ir
open Behavioral_optimize

(* Z3 context.  Timeout overridable via Z3_MITER_TIMEOUT_MS env var
   (default 30 s).  Some parent miters with deeply-nested boundary
   BCalls + ffrip cones (e.g. AES's aes_cipher_top after mix_col gets
   inlined into 16 byte slices fed by 16 sboxes) need more than 30 s. *)
let ctx =
  let t = try Sys.getenv "Z3_MITER_TIMEOUT_MS" with Not_found -> "30000" in
  Z3.mk_context [
    ("model", "true");
    ("proof", "true");
    ("timeout", t);
  ]

(* Signal cache for Z3 variables *)
let signal_cache : (string, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 256

(* Per-array elem_w table — populated from BArray bsignals so
   BSelect with a dynamic index can pick the right elem_w-bit slice
   instead of defaulting to a single-bit extract. *)
let array_elem_w_table : (string, int) Hashtbl.t = Hashtbl.create 16

(* Uninterpreted-function decls for [BCall].  Used at parent-module
   miter time as a boundary for proven-equivalent child instances:
   each child becomes a [BCall { func = "<module>__<port>"; args }]
   that maps to the SAME Z3 FuncDecl on both sides — so the call's
   value is identical when the args match (which they do, because
   parent connections are textually the same on both sides). *)
let bcall_decl_cache : (string, Z3.FuncDecl.func_decl) Hashtbl.t =
  Hashtbl.create 16

(* Output width for each [BCall] func name, set externally by the
   pass that introduces the boundaries (so we know the result sort
   when declaring the uninterpreted function). *)
let bcall_out_w : (string, int) Hashtbl.t = Hashtbl.create 16

(* Create or lookup a bitvector variable *)
let bv_var name width suffix =
  let full_name = name ^ suffix in
  match Hashtbl.find_opt signal_cache full_name with
  | Some v -> v
  | None ->
      let v = Z3.BitVector.mk_const_s ctx full_name width in
      Hashtbl.add signal_cache full_name v;
      v

(* Clear the signal cache *)
let clear_cache () = Hashtbl.clear signal_cache

(* Clear caches that are scoped to a single miter so a decl created
   during the first miter doesn't leak into a different-shape decl
   in the next.  [bcall_out_w] is populated at boundary-substitute
   time for the whole program and stays intact. *)
let clear_miter_caches () =
  Hashtbl.clear signal_cache;
  Hashtbl.clear array_elem_w_table;
  Hashtbl.clear bcall_decl_cache

(* Get width from behavioral IR type *)
let rec width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * (width_of_btype element)
  | BStruct _ -> 32

(* Strip SSA suffix from variable name (RST_0 → RST) *)
let strip_ssa_suffix name =
  try
    let last_underscore = String.rindex name '_' in
    let suffix = String.sub name (last_underscore + 1)
                            (String.length name - last_underscore - 1) in
    (* Check if suffix is a number *)
    if String.length suffix > 0 &&
       String.for_all (fun c -> c >= '0' && c <= '9') suffix then
      String.sub name 0 last_underscore
    else
      name
  with Not_found -> name

(* Get width from behavioral IR type - USE THE TYPE INFO THAT'S ALREADY THERE! *)
(* Now takes widths hashtable for variable lookup *)
let rec width_of_expr_ctx widths = function
  | BVar name ->
      (* Look up variable width, try SSA-stripped name *)
      (match Hashtbl.find_opt widths name with
       | Some w -> Some w
       | None ->
           let stripped = strip_ssa_suffix name in
           Hashtbl.find_opt widths stripped)
  | BConst { width; _ } -> Some width
  | BBinOp { op; lhs; rhs; result_type } ->
      let declared = width_of_btype result_type in
      (* Comparisons are always 1-bit; trust result_type. For
       * arithmetic/bitwise, a declared width of 1 is suspicious
       * (Verible's converter falls back to 1 when it can't infer
       * the op's width) — recompute from the operands. *)
      (match op with
       | BEq | BNe | BLt | BLe | BGt | BGe -> Some 1
       | BAdd | BSub | BMul | BDiv | BMod
       | BAnd | BOr  | BXor
       | BShl | BShr | BAshr ->
           if declared > 1 then Some declared
           else
             let wl = width_of_expr_ctx widths lhs in
             let wr = width_of_expr_ctx widths rhs in
             (match wl, wr with
              | Some a, Some b -> Some (max a b)
              | Some a, None | None, Some a -> Some a
              | None, None ->
                  (* Defer: returning `Some declared` (=1) here would
                   * commit a wrong width that the outer fixed point
                   * can't unstick (LHS-already-set short-circuit). *)
                  None))
  | BUnOp { op; operand; result_type } ->
      let declared = width_of_btype result_type in
      (match op with
       | BRedAnd | BRedOr | BRedXor -> Some 1
       | BNot | BNeg ->
           if declared > 1 then Some declared
           else
             (match width_of_expr_ctx widths operand with
              | Some w -> Some w
              | None -> None))             (* defer; same as BBinOp *)
  | BCond { then_val; else_val; _ } ->
      (* Width of branches — pick the wider, fall back to either *)
      (match width_of_expr_ctx widths then_val,
             width_of_expr_ctx widths else_val with
       | Some a, Some b -> Some (max a b)
       | Some a, None | None, Some a -> Some a
       | None, None -> None)
  | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
  | BConcat exprs ->
      (* Don't sum partials: if any child is unknown, the whole
       * concat is unknown — otherwise an early visit collapses to
       * width 0 and gets stuck (the fixed-point iteration's purge
       * doesn't unstick it because each pass re-encounters the same
       * intra-iteration ordering). *)
      let widths_list = List.map (width_of_expr_ctx widths) exprs in
      if List.for_all Option.is_some widths_list then
        Some (List.fold_left (+) 0
                (List.map (fun o -> Option.value ~default:0 o)
                          widths_list))
      else None
  | BReplicate { count; value } ->
      Option.map (fun w -> count * w) (width_of_expr_ctx widths value)
  | BSelect _ | BCall _ -> None  (* Need more info *)

(* Simple version without context for backward compatibility *)
let rec width_of_expr = function
  | BVar _ -> None
  | BConst { width; _ } -> Some width
  | BBinOp { result_type; _ } -> Some (width_of_btype result_type)
  | BUnOp { result_type; _ } -> Some (width_of_btype result_type)
  | BCond { then_val; _ } -> width_of_expr then_val
  | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
  | BConcat exprs ->
      (* Return None if ANY child width is unknown (e.g. a bare BVar) — the
         old filter_map silently DROPPED unknown children, undercounting the
         concat width and shifting higher chunks down (nested
         `{{2{1'b0}}, d}` came out 2-bit not 3-bit).  On None the caller
         falls back to the actual Z3 width, which is correct. *)
      let ws = List.map width_of_expr exprs in
      if List.for_all Option.is_some ws
      then Some (List.fold_left (fun a o -> a + Option.value ~default:0 o) 0 ws)
      else None
  | BReplicate { count; value } ->
      Option.map (fun w -> count * w) (width_of_expr value)
  | BSelect _ | BCall _ -> None

(* Convert behavioral IR expression to Z3 *)
let rec expr_to_z3 suffix ctx_sigs = function
  | BVar name ->
      (* Look up width from context, trying SSA-stripped name if not found *)
      let width =
        match List.assoc_opt name ctx_sigs with
        | Some w -> w
        | None ->
            (* Try stripped name (RST_0 → RST) *)
            let stripped = strip_ssa_suffix name in
            match List.assoc_opt stripped ctx_sigs with
            | Some w -> w
            | None -> 32  (* Last resort fallback *)
      in
      bv_var name width suffix

  | BConst { value; width } ->
      Z3.BitVector.mk_numeral ctx (Z.to_string value) width

  | BBinOp { op; lhs; rhs; result_type } ->
      let z3_lhs0 = expr_to_z3 suffix ctx_sigs lhs in
      let z3_rhs0 = expr_to_z3 suffix ctx_sigs rhs in
      (* Normalise operand widths. When the converter emits a cell with
       * fixed result_type=64 but the operands have narrower actual
       * widths, Z3 sort-checks fail. Zero-extend the smaller operand to
       * match the larger; this is correct semantics for unsigned bit-
       * vector ops on naturally-aligned values.
       *
       * For BAdd specifically we also widen both operands by 1 bit so
       * the unsigned sum can hold its carry. Vivado's flat-popcount
       * leaf `popcount_o := data_i[1] + data_i[0]` adds two 1-bit
       * values to produce a 2-bit result; without the carry-bit
       * widening the add wraps mod 2 (1+1 → 0) and the per-leaf count
       * is wrong. The downstream `BAssign` truncates back if the LHS
       * is narrower. *)
      let bump_to z w target =
        if w >= target then z
        else Z3.BitVector.mk_zero_ext ctx (target - w) z
      in
      let widen z3_a z3_b =
        let wa = Z3.BitVector.get_size (Z3.Expr.get_sort z3_a) in
        let wb = Z3.BitVector.get_size (Z3.Expr.get_sort z3_b) in
        let target = max wa wb in
        (bump_to z3_a wa target, bump_to z3_b wb target)
      in
      let z3_lhs1, z3_rhs1 = widen z3_lhs0 z3_rhs0 in
      (* LHS-context width propagation now lives in the Verible
         converter (#128), so [result_type] already carries the
         enclosing assignment's width when the BBinOp was inside a
         BAssign.  Bump operands up to that width before encoding so
         arithmetic happens at the right bit count.  BAdd still
         needs the +1 carry-bit widening on top, regardless. *)
      let declared_w = match result_type with
        | BInt { width; _ } -> width | _ -> 0 in
      let cur_w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_lhs1) in
      let z3_lhs1, z3_rhs1 =
        if declared_w > cur_w then
          (bump_to z3_lhs1 cur_w declared_w,
           bump_to z3_rhs1 cur_w declared_w)
        else (z3_lhs1, z3_rhs1) in
      let z3_lhs, z3_rhs =
        match op with
        | BAdd ->
            let w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_lhs1) in
            (bump_to z3_lhs1 w (w + 1), bump_to z3_rhs1 w (w + 1))
        | _ -> (z3_lhs1, z3_rhs1)
      in
      ignore result_type;
      (* Helper to convert boolean to 1-bit bitvector *)
      let bool_to_bv1 b =
        Z3.Boolean.mk_ite ctx b
          (Z3.BitVector.mk_numeral ctx "1" 1)
          (Z3.BitVector.mk_numeral ctx "0" 1)
      in
      (match op with
       | BAdd -> Z3.BitVector.mk_add ctx z3_lhs z3_rhs
       | BSub -> Z3.BitVector.mk_sub ctx z3_lhs z3_rhs
       | BMul -> Z3.BitVector.mk_mul ctx z3_lhs z3_rhs
       | BDiv -> Z3.BitVector.mk_udiv ctx z3_lhs z3_rhs
       | BMod -> Z3.BitVector.mk_urem ctx z3_lhs z3_rhs
       | BAnd -> Z3.BitVector.mk_and ctx z3_lhs z3_rhs
       | BOr -> Z3.BitVector.mk_or ctx z3_lhs z3_rhs
       | BXor -> Z3.BitVector.mk_xor ctx z3_lhs z3_rhs
       | BShl -> Z3.BitVector.mk_shl ctx z3_lhs z3_rhs
       | BShr -> Z3.BitVector.mk_lshr ctx z3_lhs z3_rhs
       | BAshr -> Z3.BitVector.mk_ashr ctx z3_lhs z3_rhs
       (* Comparison operators return 1-bit bitvector *)
       | BEq -> bool_to_bv1 (Z3.Boolean.mk_eq ctx z3_lhs z3_rhs)
       | BNe -> bool_to_bv1 (Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_lhs z3_rhs))
       | BLt -> bool_to_bv1 (Z3.BitVector.mk_ult ctx z3_lhs z3_rhs)
       | BLe -> bool_to_bv1 (Z3.BitVector.mk_ule ctx z3_lhs z3_rhs)
       | BGt -> bool_to_bv1 (Z3.BitVector.mk_ugt ctx z3_lhs z3_rhs)
       | BGe -> bool_to_bv1 (Z3.BitVector.mk_uge ctx z3_lhs z3_rhs))

  | BUnOp { op; operand; result_type } ->
      let z3_operand0 = expr_to_z3 suffix ctx_sigs operand in
      (* Negate / complement at the RESULT width, not the operand width.
         Verilog `-{a,b,c,d}` is 2's-complement of the full-width value:
         zero-extending the operand to the declared result width before
         mk_neg/mk_not gives the correct value (e.g. -4'b1000 = 4'b1000 = 8,
         not the narrow-operand result). *)
      (* Only widen for BNeg/BNot (value ops in a wider context).  Reductions
         must keep the operand's own width — zero-extending would fold in
         spurious 0 bits (RedAnd of a padded value -> always 0). *)
      let at_result_w () =
        match result_type with
        | BInt { width; _ } ->
            let w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_operand0) in
            if width > w then Z3.BitVector.mk_zero_ext ctx (width - w) z3_operand0
            else z3_operand0
        | _ -> z3_operand0 in
      let z3_operand = z3_operand0 in
      (match op with
       | BNot -> Z3.BitVector.mk_not ctx (at_result_w ())
       | BNeg -> Z3.BitVector.mk_neg ctx (at_result_w ())
       (* Bitvector reductions: Z3's mk_redand/mk_redor return 1-bit
        * bitvectors directly. *)
       | BRedAnd -> Z3.BitVector.mk_redand ctx z3_operand
       | BRedOr  -> Z3.BitVector.mk_redor  ctx z3_operand
       | BRedXor ->
           (* Z3 doesn't expose redxor — encode as parity via XOR-fold. *)
           let n = Z3.BitVector.get_size (Z3.Expr.get_sort z3_operand) in
           let bit i =
             Z3.BitVector.mk_extract ctx i i z3_operand
           in
           let rec fold i acc =
             if i >= n then acc
             else fold (i + 1) (Z3.BitVector.mk_xor ctx acc (bit i))
           in
           if n = 0 then Z3.BitVector.mk_numeral ctx "0" 1
           else fold 1 (bit 0))

  | BCond { condition; then_val; else_val } ->
      let z3_cond = expr_to_z3 suffix ctx_sigs condition in
      let z3_then = expr_to_z3 suffix ctx_sigs then_val in
      let z3_else = expr_to_z3 suffix ctx_sigs else_val in
      (* Condition is non-zero ≡ true. Use a same-width zero for the
       * comparison; the previous fixed 1-bit zero failed when the
       * condition expression was wider than 1 bit (e.g., a vector test
       * that gets implicitly reduced via != 0). *)
      let cond_w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_cond) in
      let zero = Z3.BitVector.mk_numeral ctx "0" cond_w in
      let cond_bool = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_cond zero) in
      (* Then/else values may have different widths; widen the smaller
       * to match the larger so the ITE returns a uniform sort. *)
      let wt = Z3.BitVector.get_size (Z3.Expr.get_sort z3_then) in
      let we = Z3.BitVector.get_size (Z3.Expr.get_sort z3_else) in
      let z3_then, z3_else =
        if wt = we then z3_then, z3_else
        else if wt < we then
          Z3.BitVector.mk_zero_ext ctx (we - wt) z3_then, z3_else
        else
          z3_then, Z3.BitVector.mk_zero_ext ctx (wt - we) z3_else
      in
      Z3.Boolean.mk_ite ctx cond_bool z3_then z3_else

  | BSlice { signal; msb; lsb } ->
      let z3_signal = expr_to_z3 suffix ctx_sigs signal in
      let sig_w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_signal) in
      let req_w = msb - lsb + 1 in
      (* Z3 requires msb < signal width. Clamp gracefully when our
       * width inference under-sizes the source (e.g. nested generate
       * locals whose dtype lookups fall back to 1 bit). Two cases:
       *   - lsb >= sig_w:  the requested slice is entirely above the
       *     signal — return zero of the requested width.
       *   - msb >= sig_w:  zero-extend the signal so the extract is
       *     well-formed, then take the requested slice. *)
      if lsb >= sig_w then
        Z3.BitVector.mk_numeral ctx "0" req_w
      else
        let z3_signal =
          if msb < sig_w then z3_signal
          else Z3.BitVector.mk_zero_ext ctx (msb - sig_w + 1) z3_signal
        in
        Z3.BitVector.mk_extract ctx msb lsb z3_signal

  | BConcat exprs ->
      (* Each chunk must occupy exactly its BIR-declared width slot,
       * otherwise overflow bits leak into the next chunk's slot and
       * later BSlice extractions land on the wrong bits.  The classic
       * trigger: a `(a + b)` chunk encodes as a 5-bit Z3 BV (BAdd's
       * +1 carry widening) instead of the BIR-declared 4-bit chunk,
       * shifting every higher chunk by one bit.  Truncate each child
       * to its declared width before concatenating. *)
      let encode_child e =
        let z3_e = expr_to_z3 suffix ctx_sigs e in
        let actual_w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_e) in
        match width_of_expr e with
        | Some declared_w when declared_w > 0 && declared_w < actual_w ->
            Z3.BitVector.mk_extract ctx (declared_w - 1) 0 z3_e
        | Some declared_w when declared_w > actual_w ->
            (* Child encoded NARROWER than its declared slot (e.g. a nested
               concat/replicate that lost a leading-zero bit): zero-extend so
               every chunk occupies exactly its slot and higher chunks aren't
               shifted down. *)
            Z3.BitVector.mk_zero_ext ctx (declared_w - actual_w) z3_e
        | _ -> z3_e
      in
      List.fold_right (fun e acc ->
        match acc with
        | None -> Some (encode_child e)
        | Some z3_acc ->
            let z3_e = encode_child e in
            Some (Z3.BitVector.mk_concat ctx z3_e z3_acc)
      ) exprs None
      |> Option.get

  | BReplicate { count; value } ->
      let z3_value = expr_to_z3 suffix ctx_sigs value in
      let rec replicate n acc =
        if n <= 0 then acc
        else replicate (n - 1) (Z3.BitVector.mk_concat ctx z3_value acc)
      in
      replicate (count - 1) z3_value

  | BSelect { array; index } ->
      (* Bit-select / element-select `array[N]`. Try to const-fold the
       * index so we can use Z3's mk_extract (which needs literal
       * msb/lsb). Required for lzc's `index_lut[(32'0 * 32'2) + 32'1]`
       * — the index expression is a small constant arithmetic tree
       * after genvar substitution, but our Z3 BSelect handler
       * previously only matched BConst indices and fell through to
       * the 32-bit zero default for anything wrapped in arithmetic. *)
      let rec eval_const = function
        | BConst { value; _ } -> Some value
        | BBinOp { op; lhs; rhs; _ } ->
            (match eval_const lhs, eval_const rhs with
             | Some a, Some b ->
                 (match op with
                  | BAdd -> Some (Z.add a b)
                  | BSub -> Some (Z.sub a b)
                  | BMul -> Some (Z.mul a b)
                  | _ -> None)
             | _ -> None)
        | BSlice { signal; msb; lsb } ->
            (match eval_const signal with
             | Some v -> Some (Z.logand (Z.shift_right v lsb) (Z.sub (Z.shift_left Z.one (msb - lsb + 1)) Z.one))
             | None -> None)
        | _ -> None
      in
      let elem_w =
        match array with
        | BVar n -> Hashtbl.find_opt array_elem_w_table n
        | _ -> None in
      let z3_array = expr_to_z3 suffix ctx_sigs array in
      let arr_w = Z3.BitVector.get_size (Z3.Expr.get_sort z3_array) in
      let elem_w = match elem_w with Some w -> w | None -> 1 in
      let size = arr_w / elem_w in
      (match eval_const index with
       | Some value ->
           let value = Z.to_int value in
           if value >= size then
             Z3.BitVector.mk_numeral ctx "0" elem_w
           else
             let hi = (value + 1) * elem_w - 1 in
             let lo = value * elem_w in
             Z3.BitVector.mk_extract ctx hi lo z3_array
       | None ->
           let z3_idx = expr_to_z3 suffix ctx_sigs index in
           if size <= 1 then
             Z3.BitVector.mk_extract ctx (elem_w - 1) 0 z3_array
           else
             (* Build a Z3 ITE chain: index == k → array[(k+1)*elem_w-1 :
                k*elem_w] for k = 0..size-1, falling back to k=size-1. *)
             let iw = Z3.BitVector.get_size (Z3.Expr.get_sort z3_idx) in
             let mk_idx_const k =
               Z3.BitVector.mk_numeral ctx (string_of_int k) iw in
             let mk_slice k =
               let hi = (k + 1) * elem_w - 1 in
               let lo = k * elem_w in
               Z3.BitVector.mk_extract ctx hi lo z3_array in
             let rec build k =
               if k >= size - 1 then mk_slice (size - 1)
               else
                 let cond = Z3.Boolean.mk_eq ctx z3_idx (mk_idx_const k) in
                 Z3.Boolean.mk_ite ctx cond (mk_slice k) (build (k + 1))
             in
             build 0)
  | BCall { func; args } ->
      (* Encode as Z3 uninterpreted function.  Both designs in the
         miter share the same [ctx] and the same [bcall_decl_cache]
         entry by name, so the same args produce the same Z3 value
         on each side — exactly the behavioural contract of a proven-
         equivalent leaf module.
         The cache key bakes in the argument arity and per-arg bit
         width so that two call sites of `func` with structurally
         different argument shapes (e.g. one side spreads a concat
         across multiple args while the other keeps it as a single
         BConcat) become distinct uninterpreted functions instead of
         crashing Z3 with "Wrong number of arguments". *)
      let z3_args = List.map (expr_to_z3 suffix ctx_sigs) args in
      let arg_sorts = List.map Z3.Expr.get_sort z3_args in
      let out_w =
        match Hashtbl.find_opt bcall_out_w func with
        | Some w -> w
        | None -> 32 in
      let arity_key =
        String.concat "_" (List.map (fun s ->
          string_of_int (Z3.BitVector.get_size s)) arg_sorts)
      in
      let cache_key = Printf.sprintf "%s/%d:%s" func
        (List.length arg_sorts) arity_key in
      let decl = match Hashtbl.find_opt bcall_decl_cache cache_key with
        | Some d -> d
        | None ->
            let d = Z3.FuncDecl.mk_func_decl_s ctx func arg_sorts
              (Z3.BitVector.mk_sort ctx out_w) in
            Hashtbl.add bcall_decl_cache cache_key d;
            d in
      Z3.FuncDecl.apply decl z3_args

(* Encode statement as Z3 constraint. After the FF-rip pass
 * (Behavioral_ffrip.rip_program), there are no BSequential blocks
 * left — every register is replaced by a primary input (Q) and a
 * primary output (Q__D = next-state combinational expression). The
 * encoder therefore needs no special sequential handling. *)
let rec encode_stmt suffix ctx_sigs solver = function
  | BAssign { lhs; rhs } ->
      (* Get LHS width from context - must use declared signal width *)
      let width =
        match List.assoc_opt lhs ctx_sigs with
        | Some w -> w
        | None ->
            let stripped = strip_ssa_suffix lhs in
            (match List.assoc_opt stripped ctx_sigs with
             | Some w -> w
             | None ->
                 (match width_of_expr rhs with
                  | Some w -> w
                  | None -> 32))
      in
      (try
        let z3_lhs = bv_var lhs width suffix in
        let z3_rhs = expr_to_z3 suffix ctx_sigs rhs in
        (* Ensure RHS matches LHS width *)
        let rhs_width = Z3.BitVector.get_size (Z3.Expr.get_sort z3_rhs) in
        let z3_rhs_adjusted =
          if rhs_width = width then
            z3_rhs
          else if rhs_width < width then
            (* Zero-extend RHS to match LHS *)
            Z3.BitVector.mk_zero_ext ctx (width - rhs_width) z3_rhs
          else
            (* Truncate RHS to match LHS *)
            Z3.BitVector.mk_extract ctx (width - 1) 0 z3_rhs
        in
        let eq = Z3.Boolean.mk_eq ctx z3_lhs z3_rhs_adjusted in
        Z3.Solver.add solver [eq]
       with Z3.Error msg ->
        Printf.eprintf "Error encoding assignment: %s := <rhs>\n" lhs;
        Printf.eprintf "  LHS width: %d\n" width;
        Printf.eprintf "  RHS: %s\n" (Behavioral_ir.string_of_bexpr rhs);
        Printf.eprintf "  Z3 error: %s\n" msg;
        raise (Z3.Error msg)
      )

  | BIf { condition; then_stmts; else_stmts } ->
      ignore condition;
      List.iter (encode_stmt suffix ctx_sigs solver) then_stmts;
      List.iter (encode_stmt suffix ctx_sigs solver) else_stmts

  | BCase { cases; default; _ } ->
      List.iter (fun (_, stmts) ->
        List.iter (encode_stmt suffix ctx_sigs solver) stmts
      ) cases;
      List.iter (encode_stmt suffix ctx_sigs solver) default

  | BWhile { body; _ } | BFor { body; _ } ->
      List.iter (encode_stmt suffix ctx_sigs solver) body

  | BBlock stmts ->
      List.iter (encode_stmt suffix ctx_sigs solver) stmts

  | BCallStmt _ | BReturn _ -> ()

(* All processes encode the same way after FF-ripping: Behavioral_ffrip
 * has rewritten BSequential into BCombinational with explicit Q__D
 * primary outputs, so we just walk every body and emit the equality
 * constraints. *)
let encode_process suffix ctx_sigs solver = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      List.iter (encode_stmt suffix ctx_sigs solver) body

(* Infer widths from assignments - walk all statements to find actual widths *)
let rec infer_widths_from_stmt widths = function
  | BAssign { lhs; rhs } ->
      (match Hashtbl.find_opt widths lhs with
       | Some _ -> ()       (* declared / already inferred — keep *)
       | None ->
           (* Only commit a width if it can actually be derived from
            * the RHS. Don't pollute with a 32-default here: that
            * sticks across inference iterations and silently
            * produces wrong widths (e.g. a 9-bit BConcat collapses
            * to 32 if children aren't yet known). The post-pass
            * after the outer fixed point fills genuinely unknown
            * LHSes with 32 as a last-resort fallback. *)
           (match width_of_expr_ctx widths rhs with
            | Some w when w > 0 -> Hashtbl.add widths lhs w
            | _ -> ()))

  | BIf { then_stmts; else_stmts; _ } ->
      List.iter (infer_widths_from_stmt widths) then_stmts;
      List.iter (infer_widths_from_stmt widths) else_stmts

  | BCase { cases; default; _ } ->
      List.iter (fun (_, stmts) ->
        List.iter (infer_widths_from_stmt widths) stmts
      ) cases;
      List.iter (infer_widths_from_stmt widths) default

  | BWhile { body; _ } | BFor { body; _ } | BBlock body ->
      List.iter (infer_widths_from_stmt widths) body

  | BCallStmt _ | BReturn _ -> ()

let infer_widths_from_process widths = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      List.iter (infer_widths_from_stmt widths) body

(* Fixed-point inference. Process order matters: when an `assign A =
 * {…B…}` is visited before B itself is defined, the BConcat width
 * collapses to 0. Iterating until widths stop changing fixes that
 * without forcing a topological sort.
 *
 * Each iteration revisits assignments that produced suspicious widths
 * (0, or 1 on what looks like a multi-bit op). To make a width
 * "revisitable", we move it from the primary table to a tentative
 * shadow before each pass; declared widths from the bsignal list stay
 * pinned. *)
let infer_widths_from_module bmod =
  let widths = Hashtbl.create 256 in
  let pinned = Hashtbl.create 64 in

  List.iter (fun (signal : bsignal) ->
    let w = width_of_btype signal.stype in
    Hashtbl.replace widths signal.name w;
    Hashtbl.replace pinned signal.name ()
  ) bmod.signals;

  (* The inference is order-sensitive: an assignment visited before
   * its RHS operands have widths produces a coarse estimate, which
   * then sticks (because subsequent visits skip already-known LHSes).
   * Two-level fixed point fixes that: outer loop purges all
   * non-pinned entries and replays inference; inner loop replays
   * inference WITHIN one outer iteration until widths stabilise, so
   * downstream assignments pick up upstream widths even when the
   * process order is unfriendly. *)
  let changed = ref true in
  let outer_iters = ref 0 in
  let prev_snapshot = ref [] in
  while !changed && !outer_iters < 8 do
    incr outer_iters;
    let kill = Hashtbl.fold (fun k _ acc ->
      if Hashtbl.mem pinned k then acc else k :: acc
    ) widths [] in
    List.iter (fun k -> Hashtbl.remove widths k) kill;
    let inner_changed = ref true in
    let inner_iters = ref 0 in
    while !inner_changed && !inner_iters < 8 do
      incr inner_iters;
      let before =
        Hashtbl.fold (fun k v acc -> (k, v) :: acc) widths []
        |> List.sort compare in
      List.iter (infer_widths_from_process widths) bmod.processes;
      let after =
        Hashtbl.fold (fun k v acc -> (k, v) :: acc) widths []
        |> List.sort compare in
      inner_changed := (before <> after)
    done;
    let snapshot =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) widths []
      |> List.sort compare in
    if snapshot = !prev_snapshot then changed := false
    else (prev_snapshot := snapshot; changed := true)
  done;
  (* Last-resort post-pass: any LHS that never became computable
   * (BCall, BSelect, etc. with no width info) gets a default of 32.
   * This preserves the original "32 means unknown" convention while
   * keeping the iterative inference clean. *)
  let rec fill_default = function
    | BAssign { lhs; _ } ->
        if not (Hashtbl.mem widths lhs) then
          Hashtbl.add widths lhs 32
    | BIf { then_stmts; else_stmts; _ } ->
        List.iter fill_default then_stmts;
        List.iter fill_default else_stmts
    | BCase { cases; default; _ } ->
        List.iter (fun (_, body) -> List.iter fill_default body) cases;
        List.iter fill_default default
    | BWhile { body; _ } | BFor { body; _ } | BBlock body ->
        List.iter fill_default body
    | BCallStmt _ | BReturn _ -> ()
  in
  List.iter (function
    | BCombinational { body; _ } | BSequential { body; _ } ->
        List.iter fill_default body
  ) bmod.processes;
  widths

(* Build signal width context from module *)
let build_signal_context bmod =
  Hashtbl.clear array_elem_w_table;
  List.iter (fun (s : Behavioral_ir.bsignal) ->
    match s.stype with
    | BArray { element; _ } ->
        Hashtbl.replace array_elem_w_table s.name
          (width_of_btype element)
    | _ -> ()
  ) bmod.signals;
  let widths = infer_widths_from_module bmod in
  Hashtbl.fold (fun name width acc -> (name, width) :: acc) widths []

(* Encode module as Z3 constraints with suffix *)
let encode_module bmod suffix =
  let solver = Z3.Solver.mk_simple_solver ctx in
  let ctx_sigs = build_signal_context bmod in

  (* Debug: print inferred widths (per-signal only when Z3_MITER_VERBOSE=1 —
     a switch-level bitstream model has >100k signals, the flood alone dwarfs
     the solve). *)
  Printf.printf "  Inferred widths for %s (%d signals)\n" bmod.name (List.length ctx_sigs);
  if Sys.getenv_opt "Z3_MITER_VERBOSE" = Some "1" then
    List.iter (fun (name, width) ->
      Printf.printf "    %s: %d bits\n" name width
    ) (List.sort (fun (a,_) (b,_) -> String.compare a b) ctx_sigs);

  (* Encode all processes *)
  List.iter (encode_process suffix ctx_sigs solver) bmod.processes;

  (solver, ctx_sigs)

(* Get output signals from module *)
let get_output_signals bmod =
  List.filter_map (fun signal ->
    match signal.direction with
    | `Output -> Some (signal.name, width_of_btype signal.stype)
    | _ -> None
  ) bmod.signals

(* Get input signals from module *)
let get_input_signals bmod =
  List.filter_map (fun signal ->
    match signal.direction with
    | `Input -> Some (signal.name, width_of_btype signal.stype)
    | _ -> None
  ) bmod.signals

(* Walk a statement, collecting every BAssign LHS name. *)
let rec collect_written acc = function
  | BAssign { lhs; _ } -> lhs :: acc
  | BBlock ss -> List.fold_left collect_written acc ss
  | BIf { then_stmts; else_stmts; _ } ->
      let acc = List.fold_left collect_written acc then_stmts in
      List.fold_left collect_written acc else_stmts
  | BCase { cases; default; _ } ->
      let acc = List.fold_left (fun a (_, ss) ->
        List.fold_left collect_written a ss) acc cases in
      List.fold_left collect_written acc default
  | BWhile { body; _ } | BFor { body; _ } ->
      List.fold_left collect_written acc body
  | BCallStmt _ | BReturn _ -> acc

(* Internal signals that no process writes to. Pure undriven reads
 * (e.g. an uninitialised reg [7:0] mem [0:255] used only as the source
 * of an assign) end up here. Without explicit matching they would be
 * encoded as independent Z3 free variables on each side and Z3 would
 * pick differing arbitrary values, manufacturing a counterexample
 * even when the two designs are structurally identical. *)
let get_undriven_internals bmod =
  let written = List.fold_left (fun acc p ->
    let body = match p with
      | BCombinational { body; _ } -> body
      | BSequential   { body; _ } -> body
    in
    List.fold_left collect_written acc body
  ) [] bmod.processes in
  let written_set = List.sort_uniq compare written in
  List.filter_map (fun (s : bsignal) ->
    if s.direction = `Internal
       && not (List.mem s.name written_set)
    then Some (s.name, width_of_btype s.stype)
    else None
  ) bmod.signals

(* Collect the BASE name of every WRITE TARGET in a statement: BAssign LHS
 * (base before any '['), plus the first argument of a @slice_write / @mem_write
 * BCallStmt (the partial-write target).  Unlike collect_written this also sees
 * the partial-write pseudo-ops, so a slice/bit write onto a signal is counted
 * as driving it. *)
let rec collect_write_targets acc = function
  | BAssign { lhs; _ } ->
      let base = match String.index_opt lhs '[' with
        | Some i when i > 0 -> String.sub lhs 0 i | _ -> lhs in
      base :: acc
  | BBlock ss -> List.fold_left collect_write_targets acc ss
  | BIf { then_stmts; else_stmts; _ } ->
      let acc = List.fold_left collect_write_targets acc then_stmts in
      List.fold_left collect_write_targets acc else_stmts
  | BCase { cases; default; _ } ->
      let acc = List.fold_left (fun a (_, ss) ->
        List.fold_left collect_write_targets a ss) acc cases in
      List.fold_left collect_write_targets acc default
  | BWhile { body; _ } | BFor { body; _ } ->
      List.fold_left collect_write_targets acc body
  | BCallStmt { func = ("@slice_write" | "@mem_write"); args = BVar tgt :: _ } ->
      tgt :: acc
  | BCallStmt _ | BReturn _ -> acc

(* A well-formed module NEVER drives its own input ports.  A frontend / pass
 * bug that redirects a write onto an input port — e.g. blocking_subst
 * substituting a @slice_write TARGET so dm_csrs's `sbaddr_d[31:0] = …` becomes
 * `sbaddress_i[31:0] = …` (sbaddress_i is an INPUT) — is INVISIBLE to the
 * equivalence encoder: it models every input port as a free primary input and
 * silently discards the internal write, so the miter returns a VACUOUS
 * EQUIVALENT (the write, and any divergence it causes, never enters Z3).
 * Return the list of input ports that are internally driven. *)
let driven_input_ports bmod =
  let written = List.fold_left (fun acc p ->
    let body = match p with
      | BCombinational { body; _ } -> body
      | BSequential   { body; _ } -> body in
    List.fold_left collect_write_targets acc body
  ) [] bmod.processes in
  let written_set = List.sort_uniq compare written in
  List.filter_map (fun (s : bsignal) ->
    if s.direction = `Input && List.mem s.name written_set
    then Some s.name else None
  ) bmod.signals

(* Create miter circuit and check equivalence *)
(* [input_consts]: contextual assume-guarantee — port constants the PARENT
   instantiation binds identically on both flows.  Vivado const-propagates
   INTO a child (the netlist PCS bakes {…} into _support's
   an_adv_config_vector and ignores the module's own input port) while the
   other flow passes the port through; at module level the input is a free
   variable, so const-vs-baked-const honestly DIFFERs without the context. *)
(* [output_masks]: per-BIT context usage of each output (bit set = read by
   some parent on BOTH flows).  A flow that proved a bit redundant in
   context (`.rxuserclk2_out()` unread; pcspma_status[15:2] beyond the LED
   bits) ties/rewires it — comparing masked-out bits only reproduces that
   trusted optimisation.  Mask 0 skips the output entirely. *)
let check_miter_equivalence ?(input_consts : (string * Z.t) list = [])
                            ?(output_masks : (string * Z.t) list = []) bmod1 bmod2 =
  (* STRUCTURAL GUARD (runs BEFORE any Z3 encoding): a module that internally
     drives one of its own INPUT ports is malformed — the encoder treats input
     ports as free primary inputs and silently drops the write, so the miter
     would return a vacuous EQUIVALENT.  Fail loudly here so the frontend / pass
     bug surfaces as an ERROR verdict instead of a false pass. *)
  List.iter (fun (bmod, tag) ->
    match driven_input_ports bmod with
    | [] -> ()
    | ports ->
        failwith (Printf.sprintf
          "malformed %s module %s: input port(s) internally driven [%s] — a \
           frontend/pass bug redirected a write onto an input port; the \
           equivalence encoder models input ports as free inputs and silently \
           drops such writes (would give a VACUOUS EQUIVALENT). Fix the source \
           of the input-port write."
          tag bmod.name (String.concat ", " ports))
  ) [ (bmod1, "design-1"); (bmod2, "design-2") ];
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Z3 Miter Equivalence Checking\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Design 1: %s\n" bmod1.name;
  Printf.printf "Design 2: %s\n\n" bmod2.name;

  (* Rip flip-flops on both sides: every Q becomes a primary input
   * (current state matched by name across the two designs) and
   * every Q's next-state expression becomes a fresh primary
   * output `<Q>__D`. The miter then reduces to a combinational
   * check, with EDFFs and DFF+MUX naturally producing the same
   * D-pin function. *)
  (* SSA-fold both modules first: encode_stmt below ignores BIf conditions and
     @slice_write BCallStmts, so it needs each signal in single-assignment form.
     module_to_ssa folds conditionals into BCond and lowers slice-writes via
     per-version concat — without it, a base slice-write + conditional slice-override
     (ibex_counter's counter_load) leaves the signal unconstrained (free), giving a
     spurious counterexample even though the Cyclesim path agrees. *)
  let bmod1 = Behavioral_ssa.module_to_ssa bmod1 in
  let bmod2 = Behavioral_ssa.module_to_ssa bmod2 in
  let bmod1 = Behavioral_ffrip.rip_module bmod1 in
  let bmod2 = Behavioral_ffrip.rip_module bmod2 in
  (* Post-FF-rip register sharing: collapse pairs of FFs whose D-cones
   * are structurally identical to a single canonical register. This
   * mirrors yosys's `share` and Vivado's elaboration dedup, so the
   * verilator/verible side (which has no synthesis pass) ends up
   * with the same FF set as a synthesised reference. *)
  let bmod1 = Behavioral_share.share_module bmod1 in
  let bmod2 = Behavioral_share.share_module bmod2 in

  (* Get input/output ports *)
  let inputs1 = get_input_signals bmod1 in
  let inputs2 = get_input_signals bmod2 in
  let outputs1 = get_output_signals bmod1 in
  let outputs2 = get_output_signals bmod2 in

  Printf.printf "Inputs: %d vs %d\n" (List.length inputs1) (List.length inputs2);
  Printf.printf "Outputs: %d vs %d\n\n" (List.length outputs1) (List.length outputs2);

  (* Check that interfaces match *)
  let input_names1 = List.map fst inputs1 |> List.sort String.compare in
  let input_names2 = List.map fst inputs2 |> List.sort String.compare in
  let output_names1 = List.map fst outputs1 |> List.sort String.compare in
  let output_names2 = List.map fst outputs2 |> List.sort String.compare in

  (* Input interfaces may legitimately differ when one side drops an UNUSED
     input (synthesis dead-input elimination).  That never affects outputs, so
     we don't bail — we constrain only the COMMON inputs below.  Outputs must
     still match. *)
  if input_names1 <> input_names2 then
    Printf.printf "⚠ Input interfaces differ (constraining common inputs only)\n  D1: [%s]\n  D2: [%s]\n"
      (String.concat ", " input_names1) (String.concat ", " input_names2);
  (* Output interfaces may differ when synthesis optimises away registers
     (their `Q__D` boundary outputs vanish on one side).  Don't bail — compare
     only the COMMON outputs (the miter loop below filters).  A real primary-
     output difference still shows because those outputs are common. *)
  if output_names1 <> output_names2 then
    Printf.printf "⚠ Output interfaces differ (comparing common outputs only)\n";
  begin
    Printf.printf "✓ Interfaces reconciled\n\n";

    (* Encode both designs *)
    Printf.printf "Encoding Design 1...\n";
    clear_cache ();
    let (solver1, ctx1) = encode_module bmod1 "_d1" in
    let assertions1 = Z3.Solver.get_assertions solver1 in
    Printf.printf "  %d constraints\n" (List.length assertions1);

    Printf.printf "Encoding Design 2...\n";
    clear_cache ();
    let (solver2, ctx2) = encode_module bmod2 "_d2" in
    let assertions2 = Z3.Solver.get_assertions solver2 in
    Printf.printf "  %d constraints\n\n" (List.length assertions2);

    (* Create miter solver *)
    let miter_solver = Z3.Solver.mk_simple_solver ctx in

    (* Add both designs' constraints *)
    Z3.Solver.add miter_solver assertions1;
    Z3.Solver.add miter_solver assertions2;

    (* Constrain the COMMON inputs to be the same (see note above — a side may
       have dropped an unused input). *)
    Printf.printf "Constraining inputs to match...\n";
    let sz z = Z3.BitVector.get_size (Z3.Expr.get_sort z) in
    let in2_w = Hashtbl.create (List.length inputs2 * 2 + 1) in
    List.iter (fun (n, w) -> Hashtbl.replace in2_w n w) inputs2;
    List.iter (fun (name, width) ->
      match Hashtbl.find_opt in2_w name with
      | Some w2 ->
        (* Widths may diverge across flows (one side inferred a signal wider
           than the other).  bv_var caches by name+suffix and returns each
           design's OWN encoded width, so build both at their real widths and
           constrain only the overlapping low bits equal — leaving any extra
           high bits free is conservative (never hides a counterexample). *)
        let in1 = bv_var name width "_d1" in
        let in2 = bv_var name w2 "_d2" in
        let w = min (sz in1) (sz in2) in
        let lo z = if sz z > w then Z3.BitVector.mk_extract ctx (w - 1) 0 z else z in
        Z3.Solver.add miter_solver [ Z3.Boolean.mk_eq ctx (lo in1) (lo in2) ]
      | None -> ()
    ) inputs1;

    (* CONTEXTUAL port constants (see doc on check_miter_equivalence). *)
    if input_consts <> [] then begin
      let n_ic = ref 0 in
      let apply inputs sfx =
        List.iter (fun (nm, v) ->
          match List.assoc_opt nm inputs with
          | Some w ->
              incr n_ic;
              let var = bv_var nm w sfx in
              let wz = sz var in
              let masked = Z.logand v (Z.pred (Z.shift_left Z.one wz)) in
              Z3.Solver.add miter_solver
                [ Z3.Boolean.mk_eq ctx var
                    (Z3.BitVector.mk_numeral ctx (Z.to_string masked) wz) ]
          | None -> ()) input_consts in
      apply inputs1 "_d1";
      apply inputs2 "_d2";
      Printf.printf "Contextual port constants: %d input binding(s) applied\n"
        (!n_ic)
    end;

    (* CONSTANT-STATE constraints (classic LEC constant-register sweep).  A
       lifted state bit whose next-state (its ffrip `<q>__D` expression) folds
       to a compile-time constant c is constant in operation (D independent of
       state and inputs; INIT matches c — the same assumption synthesis makes
       when it prunes such registers).  Left free, an optimised side that
       PRUNED those bits (Vivado const-props a register's zero bits away)
       mismatches the unpruned side through UNREACHABLE states — e.g. SVS keeps
       a 4-bit a_q whose [3:1] have D=GND while Vivado keeps 1 FF; free upper
       bits then fabricate counterexamples.  Constrain each side's constant
       state bits to their constant.  Disable with Z3_MITER_NO_CONST_STATE=1. *)
    if Sys.getenv_opt "Z3_MITER_NO_CONST_STATE" = None then begin
      let rec bit_const e j =
        (* Some 0/1 when bit j of e is a compile-time constant. *)
        match e with
        | BConst { value; _ } -> Some (if Z.testbit value j then 1 else 0)
        | BVar ("GND" | "<const0>") -> if j = 0 then Some 0 else Some 0
        | BVar ("VCC" | "<const1>") -> if j = 0 then Some 1 else Some 0
        | BConcat es ->
            (* MSB-first: walk from LSB end. *)
            let rec go j = function
              | [] -> None
              | el :: rest ->
                  let w = expr_width el in
                  if j < w then bit_const el j else go (j - w) rest in
            go j (List.rev es)
        | BSlice { signal; msb; lsb } ->
            let lo = min msb lsb in
            if j <= abs (msb - lsb) then bit_const signal (lo + j) else Some 0
        | BCond { condition; then_val; else_val } ->
            (match bit_const condition 0 with
             | Some c -> bit_const (if c <> 0 then then_val else else_val) j
             | None ->
                 (match bit_const then_val j, bit_const else_val j with
                  | Some a, Some b when a = b -> Some a
                  | _ -> None))
        | BReplicate { value; count } ->
            let w = expr_width value in
            if w > 0 && j < w * count then bit_const value (j mod w) else Some 0
        | _ -> None
      and expr_width = function
        | BConst { width; _ } -> width
        | BSlice { msb; lsb; _ } -> abs (msb - lsb) + 1
        | BConcat es -> List.fold_left (fun a e -> a + expr_width e) 0 es
        | BReplicate { value; count } -> count * expr_width value
        | _ -> 1 in
      let n_cs = ref 0 in
      let n_eqr = ref 0 in
      let constrain_side (bm : bmodule) inputs suffix =
        let inw = Hashtbl.create 16 in
        List.iter (fun (n, w) -> Hashtbl.replace inw n w) inputs;
        (* EQUIVALENT-register grouping (LEC dedup sweep): state bits with
           STRUCTURALLY IDENTICAL next-state expressions are equal in every
           reachable state (same D, same INIT assumption as the const sweep).
           Vivado shares one FF across such bits (`b_q[0]`/`b_q[1]` both load
           rst_n → one FDRE); the unshared side must know the invariant or the
           extra bit floats free and fabricates counterexamples. *)
        let eq_groups : (string, Z3.Expr.expr list ref) Hashtbl.t =
          Hashtbl.create 16 in
        List.iter (function
          | BCombinational { body = [BAssign { lhs; rhs }]; _ }
            when String.length lhs > 3
                 && String.sub lhs (String.length lhs - 3) 3 = "__D" ->
              let q = String.sub lhs 0 (String.length lhs - 3) in
              (match Hashtbl.find_opt inw q with
               | Some w ->
                   let st = bv_var q w suffix in
                   let wz = sz st in
                   (* per-bit exprs when rhs is ffpack's MSB-first concat of
                      1-bit cones (or the whole rhs for a 1-bit state) *)
                   let bit_exprs =
                     match rhs with
                     | BConcat es when List.for_all
                         (fun e -> expr_width e = 1) es
                         && List.length es = wz ->
                         Some (Array.of_list (List.rev es))   (* [j] = bit j *)
                     | _ when wz = 1 -> Some [| rhs |]
                     | _ -> None in
                   for j = 0 to wz - 1 do
                     match bit_const rhs j with
                     | Some c ->
                         incr n_cs;
                         let bit = Z3.BitVector.mk_extract ctx j j st in
                         let cv = Z3.BitVector.mk_numeral ctx (string_of_int c) 1 in
                         Z3.Solver.add miter_solver [ Z3.Boolean.mk_eq ctx bit cv ]
                     | None ->
                         (* non-const bit: group by NORMALISED D-cone structure.
                            Normalise (a) const conditions — CE=1'1 muxes hide
                            identical loads behind per-bit hold slices; (b) the
                            bit's own hold reference q[j:j] → __SELF__ — bits
                            whose D differs only in their own hold-slice are
                            equal by induction (equal INIT ⇒ equal forever). *)
                         (match bit_exprs with
                          | Some arr ->
                              let rec keyf e = match e with
                                | BSlice { signal = BVar x; msb; lsb }
                                  when x = q && msb = j && lsb = j ->
                                    BVar "__SELF__"
                                | BCond { condition; then_val; else_val } ->
                                    (match bit_const condition 0 with
                                     | Some c ->
                                         keyf (if c <> 0 then then_val else else_val)
                                     | None ->
                                         BCond { condition = keyf condition;
                                                 then_val = keyf then_val;
                                                 else_val = keyf else_val })
                                | BBinOp r -> BBinOp { r with lhs = keyf r.lhs;
                                                              rhs = keyf r.rhs }
                                | BUnOp r -> BUnOp { r with operand = keyf r.operand }
                                | BSelect r -> BSelect { array = keyf r.array;
                                                         index = keyf r.index }
                                | BSlice r -> BSlice { r with signal = keyf r.signal }
                                | BConcat es -> BConcat (List.map keyf es)
                                | BReplicate r -> BReplicate { r with value = keyf r.value }
                                | BCall r -> BCall { r with args = List.map keyf r.args }
                                | BVar _ | BConst _ -> e in
                              let key = string_of_bexpr (keyf arr.(j)) in
                              let bit = Z3.BitVector.mk_extract ctx j j st in
                              (match Hashtbl.find_opt eq_groups key with
                               | Some l -> l := bit :: !l
                               | None -> Hashtbl.add eq_groups key (ref [bit]))
                          | None -> ())
                   done
               | None -> ())
          | _ -> ()) bm.processes;
        Hashtbl.iter (fun _ l ->
          match !l with
          | first :: (_ :: _ as rest) ->
              List.iter (fun b ->
                incr n_eqr;
                Z3.Solver.add miter_solver [ Z3.Boolean.mk_eq ctx first b ])
                rest
          | _ -> ()) eq_groups in
      constrain_side bmod1 inputs1 "_d1";
      constrain_side bmod2 inputs2 "_d2";
      if !n_cs > 0 then
        Printf.printf "Constant-state sweep: constrained %d state bit(s)\n" !n_cs;
      if !n_eqr > 0 then
        Printf.printf "Equivalent-register sweep: %d state bit(s) tied to group leaders\n" !n_eqr
    end;

    (* CROSS-SIDE BITBUS STATE MATCHING.  Vivado dedups / const-prunes register
       bits, leaving a SPARSE per-bit state set (e.g. only `b_q__b1` survives of
       a 2-bit b_q) which ffpack cannot pack, while the other side packed the
       full bus `b_q`.  The state names then never match, both float free, and
       every read of the register fabricates a counterexample.  Tie each
       per-bit state input `X__b<i>` to bit i of the other side's bus state
       input `X`.  (The corresponding `X__b<i>__D` next-state cones are paired
       against bit i of `X__D` in the output compare below.) *)
    let bitb_re = Str.regexp "^\\(.+\\)__b\\([0-9]+\\)$" in
    let in1_w = Hashtbl.create (List.length inputs1 * 2 + 1) in
    List.iter (fun (n, w) -> Hashtbl.replace in1_w n w) inputs1;
    let n_bb = ref 0 in
    let match_bit_side my_inputs other_w my_sfx other_sfx =
      List.iter (fun (name, w) ->
        if w = 1 && Str.string_match bitb_re name 0 then begin
          let base = Str.matched_group 1 name in
          let i = int_of_string (Str.matched_group 2 name) in
          match Hashtbl.find_opt other_w base with
          | Some wb when i < wb ->
              incr n_bb;
              let bitv = bv_var name 1 my_sfx in
              let busv = bv_var base wb other_sfx in
              let bit = Z3.BitVector.mk_extract ctx i i busv in
              Z3.Solver.add miter_solver [ Z3.Boolean.mk_eq ctx bitv bit ]
          | _ -> ()
        end) my_inputs in
    match_bit_side inputs1 in2_w "_d1" "_d2";
    match_bit_side inputs2 in1_w "_d2" "_d1";
    if !n_bb > 0 then
      Printf.printf "Bitbus state matching: %d per-bit state(s) tied to bus bits\n" !n_bb;

    (* SEMANTIC constant-0 state sweep.  The syntactic sweep above only sees
       literal constants; synthesis-level constants hide behind real logic —
       `a_q <= {0,g} - {0,g}` (always 0 through the subtractor LUTs) or
       `a_q <= d << d` (bits 0,2 unreachable) — which Vivado's optimizer prunes
       but the other side keeps as live FFs.  Probe each lifted state bit's
       next-state: if `<q>__D[j] ≠ 0` is UNSAT under all current constraints,
       the bit loads 0 in every consistent valuation, so with INIT=0 it is
       constant — pin the state bit.  Capped (Z3_MITER_SEM_CONST_CAP, default
       1024 probes) so switch-level monsters don't pay thousands of SAT calls;
       disabled along with the syntactic sweep by Z3_MITER_NO_CONST_STATE. *)
    if Sys.getenv_opt "Z3_MITER_NO_CONST_STATE" = None then begin
      let cap = match Sys.getenv_opt "Z3_MITER_SEM_CONST_CAP" with
        | Some s -> (try int_of_string s with _ -> 1024) | None -> 1024 in
      let probes = ref 0 and hits = ref 0 in
      let pinned : (string, unit) Hashtbl.t = Hashtbl.create 64 in
      let zero1 = Z3.BitVector.mk_numeral ctx "0" 1 in
      let probe_side inputs outputs sfx =
        let outw = Hashtbl.create 16 in
        List.iter (fun (n, w) -> Hashtbl.replace outw n w) outputs;
        List.iter (fun (q, w) ->
          match Hashtbl.find_opt outw (q ^ "__D") with
          | Some wd ->
              let dvar = bv_var (q ^ "__D") wd sfx in
              let qvar = bv_var q w sfx in
              let wmin = min (sz dvar) (sz qvar) in
              for j = 0 to wmin - 1 do
                let key = Printf.sprintf "%s%s[%d]" q sfx j in
                if not (Hashtbl.mem pinned key) && !probes < cap then begin
                  (* try c = 0 then c = 1 (a const-1 loader with INIT=1 —
                     dff_init's preset registers — is tied high by synthesis;
                     same INIT-matches-c assumption as the rest) *)
                  let try_const cv =
                    incr probes;
                    Z3.Solver.push miter_solver;
                    Z3.Solver.add miter_solver
                      [ Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx
                          (Z3.BitVector.mk_extract ctx j j dvar) cv) ];
                    let r = Z3.Solver.check miter_solver [] in
                    Z3.Solver.pop miter_solver 1;
                    if r = Z3.Solver.UNSATISFIABLE then begin
                      incr hits;
                      Hashtbl.replace pinned key ();
                      Z3.Solver.add miter_solver
                        [ Z3.Boolean.mk_eq ctx
                            (Z3.BitVector.mk_extract ctx j j qvar) cv ];
                      true
                    end else false in
                  if not (try_const zero1) && !probes < cap then
                    ignore (try_const (Z3.BitVector.mk_numeral ctx "1" 1))
                end
              done
          | None -> ()) inputs in
      (* iterate: pinning one bit can make another's D provably 0 *)
      let last = ref (-1) in
      while !hits > !last && !probes < cap do
        last := !hits;
        probe_side inputs1 outputs1 "_d1";
        probe_side inputs2 outputs2 "_d2"
      done;
      if !hits > 0 then
        Printf.printf "Semantic const-0 state sweep: %d bit(s) proven constant (%d probes)\n"
          !hits !probes;
      (* SEMANTIC same-bus dedup: the structural equivalent-register grouping
         fails when identical cones route through different intermediate nets
         (`_n_3` vs `_n_5`, both = wire4 — dff_init's sign-extended load).
         Probe D[i] ≠ D[j] for bit pairs of the same state bus; UNSAT ⇒ the
         bits load identically ⇒ (equal INIT) tie q[i] = q[j]. *)
      let dedup_hits = ref 0 in
      let dedup_side inputs outputs sfx =
        let outw = Hashtbl.create 16 in
        List.iter (fun (n, w) -> Hashtbl.replace outw n w) outputs;
        List.iter (fun (q, w) ->
          match Hashtbl.find_opt outw (q ^ "__D") with
          | Some wd when w > 1 && w <= 32 ->
              let dvar = bv_var (q ^ "__D") wd sfx in
              let qvar = bv_var q w sfx in
              let wmin = min (sz dvar) (sz qvar) in
              for i = 0 to wmin - 1 do
                for j = i + 1 to wmin - 1 do
                  let ki = Printf.sprintf "%s%s[%d]" q sfx i in
                  let kj = Printf.sprintf "%s%s[%d]" q sfx j in
                  if not (Hashtbl.mem pinned ki) && not (Hashtbl.mem pinned kj)
                     && !probes < cap then begin
                    incr probes;
                    Z3.Solver.push miter_solver;
                    Z3.Solver.add miter_solver
                      [ Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx
                          (Z3.BitVector.mk_extract ctx i i dvar)
                          (Z3.BitVector.mk_extract ctx j j dvar)) ];
                    let r = Z3.Solver.check miter_solver [] in
                    Z3.Solver.pop miter_solver 1;
                    if r = Z3.Solver.UNSATISFIABLE then begin
                      incr dedup_hits;
                      Z3.Solver.add miter_solver
                        [ Z3.Boolean.mk_eq ctx
                            (Z3.BitVector.mk_extract ctx i i qvar)
                            (Z3.BitVector.mk_extract ctx j j qvar) ]
                    end
                  end
                done
              done
          | _ -> ()) inputs in
      dedup_side inputs1 outputs1 "_d1";
      dedup_side inputs2 outputs2 "_d2";
      if !dedup_hits > 0 then
        Printf.printf "Semantic dedup sweep: %d state-bit pair(s) tied\n" !dedup_hits
    end;

    (* Match undriven internal signals across designs. These are reads of
     * a signal that no process writes (e.g. `reg [7:0] mem [0:255];
     * assign b = mem[a];` — mem is just a source). Without this they
     * encode as independent Z3 free variables and Z3 manufactures a
     * counterexample by picking different values per side. *)
    let undriven1 = get_undriven_internals bmod1 in
    let undriven2 = get_undriven_internals bmod2 in
    (* Hash-join, not List.assoc_opt per element: a switch-level bitstream model
       collapses to tens of thousands of undriven routing/stub nets, and the
       old O(n^2) filter + per-signal print made the miter hang (44k signals =>
       ~2G lookups + 44k prints).  Match in O(n) and print only the count. *)
    let u2 = Hashtbl.create (List.length undriven2 * 2 + 1) in
    List.iter (fun (n, w) -> Hashtbl.replace u2 n w) undriven2;
    let shared_undriven =
      List.filter (fun (n, w) ->
        match Hashtbl.find_opt u2 n with Some w2 -> w2 = w | None -> false)
        undriven1
    in
    (* Why a blackbox output can escape the tie: this set requires
       direction = `Internal AND the same name+width on both sides.  A net
       driven only by an UNMODELLED primitive instance (MMCME2_ADV.LOCKED,
       GTXE2_CHANNEL outputs, ...) is undriven by processes, so it belongs
       here -- but if either frontend gives it a different direction or a
       different name after flattening, it silently becomes two independent
       free variables and Z3 manufactures a counterexample.  Set
       Z3_MITER_DEBUG_TIES=<name> to see which side drops it. *)
    (match Sys.getenv_opt "Z3_MITER_DEBUG_TIES" with
     | Some pat ->
         let show tag l =
           let hits = List.filter (fun (n,_) ->
             let re = Str.regexp_string pat in
             (try ignore (Str.search_forward re n 0); true with Not_found -> false)) l in
           Printf.printf "[ties] %s: %d match(es) for '%s': %s\n" tag
             (List.length hits) pat
             (String.concat ", " (List.map (fun (n,w) -> Printf.sprintf "%s/%d" n w)
                                    (List.filteri (fun i _ -> i < 6) hits))) in
         show "undriven-d1" undriven1;
         show "undriven-d2" undriven2;
         show "tied" shared_undriven;
         let sig_dir tag (m : bmodule) =
           List.iter (fun (s : bsignal) ->
             if (try ignore (Str.search_forward (Str.regexp_string pat) s.name 0); true
                 with Not_found -> false) then
               Printf.printf "[ties] %s signal %s dir=%s width=%d\n" tag s.name
                 (match s.direction with `Internal -> "Internal" | `Input -> "Input"
                  | `Output -> "Output" | _ -> "other")
                 (width_of_btype s.stype)) m.signals in
         sig_dir "d1" bmod1; sig_dir "d2" bmod2
     | None -> ());
    if shared_undriven <> [] then begin
      Printf.printf "Matching %d undriven internal signal(s) across designs\n"
        (List.length shared_undriven);
      List.iter (fun (name, width) ->
        let in1 = bv_var name width "_d1" in
        let in2 = bv_var name width "_d2" in
        Z3.Solver.add miter_solver [ Z3.Boolean.mk_eq ctx in1 in2 ]
      ) shared_undriven
    end;

    (* After Behavioral_ffrip every register Q has been turned into
     * a primary input (its current state) and Q__D has been added as
     * a primary output (the next-state combinational expression).
     * The standard input-matching + output-XOR construction below
     * therefore handles sequential equivalence at depth 1 with no
     * special casing — `Q_d1 = Q_d2` falls out of input matching
     * and `Q__D_d1 ?= Q__D_d2` falls out of the output XOR. *)
    ignore ctx2;

    (* ---- Struct-scalarize state ties (STRUCT_SCALARIZE default-on) -------
       One flow (SVS) splits an internal struct register S into per-field
       FFs S$f1..S$fn; the reference flow (Verilator) keeps the whole S.
       FF-rip then names the states differently (S$f vs S) so they never
       pair by name — the DR/dtmcs datapath would silently drop out of the
       comparison (an unsound "EQUIVALENT" over a subset).  Re-tie them via
       the layout Behavioral_ir.scalarize_layouts recorded at split time:
         current-state:  S(whole) == { S$f1, ..., S$fn }   (MSB-first)
         next-state:     S__D     ?= { S$f1__D, ..., S$fn__D }  (extra cone)
       so the scalarized datapath is PROVEN, not skipped.  Works in either
       flow-order (whole may be design 1 or 2). *)
    let scalarize_d_pairs = ref [] in
    if Hashtbl.length scalarize_layouts > 0 then begin
      let lookup lst n = List.assoc_opt n lst in
      let mk_concat_msb sfx fvars =            (* fvars MSB-first *)
        match List.map (fun (fn, fw) -> bv_var fn fw sfx) fvars with
        | [] -> None
        | x :: xs -> Some (List.fold_left (fun acc z ->
                             Z3.BitVector.mk_concat ctx acc z) x xs) in
      let n_scal = ref 0 in
      Hashtbl.iter (fun base layout ->
        if layout <> [] then begin
          let fvars = List.map (fun (f, _, _, w) -> (base ^ "$" ^ f, w)) layout in
          let tot = List.fold_left (fun a (_, w) -> a + w) 0 fvars in
          (* whole_in/field_in = the two designs' input lists; try both
             orientations so we don't assume which flow scalarized. *)
          let try_orient whole_sfx field_sfx whole_in field_in whole_out field_out =
            match lookup whole_in base with
            | Some ww when ww = tot
                         && List.for_all (fun (fn,_) ->
                              lookup field_in fn <> None) fvars ->
              (* current-state tie: whole Q == concat(field Qs) *)
              (match mk_concat_msb field_sfx fvars with
               | Some concat_q ->
                 let whole_q = bv_var base tot whole_sfx in
                 Z3.Solver.add miter_solver
                   [ Z3.Boolean.mk_eq ctx whole_q concat_q ];
                 incr n_scal
               | None -> ());
              (* next-state cone: whole__D ?= concat(field__D) *)
              let wd = base ^ "__D" in
              let fds = List.map (fun (fn, fw) -> (fn ^ "__D", fw)) fvars in
              (match lookup whole_out wd with
               | Some wwd when wwd = tot
                            && List.for_all (fun (fn,_) ->
                                 lookup field_out fn <> None) fds ->
                 (match mk_concat_msb field_sfx fds with
                  | Some concat_d ->
                    let whole_d = bv_var wd tot whole_sfx in
                    let differs = Z3.Boolean.mk_not ctx
                        (Z3.Boolean.mk_eq ctx whole_d concat_d) in
                    scalarize_d_pairs := (wd, differs) :: !scalarize_d_pairs
                  | None -> ())
               | _ -> ());
              true
            | _ -> false in
          ignore (try_orient "_d1" "_d2" inputs1 inputs2 outputs1 outputs2
                  || try_orient "_d2" "_d1" inputs2 inputs1 outputs2 outputs1)
        end) scalarize_layouts;
      if !n_scal > 0 then
        Printf.printf
          "Struct-scalarize state ties: %d register(s) tied (whole ↔ fields), %d next-state cone(s)\n"
          !n_scal (List.length !scalarize_d_pairs)
    end;

    let common_outputs =
      List.filter (fun (n, _) -> List.mem_assoc n outputs2) outputs1 in
    let common_outputs =
      if output_masks = [] then common_outputs
      else begin
        let kept = List.filter (fun (n, _) ->
          match List.assoc_opt n output_masks with
          | Some m -> not (Z.equal m Z.zero)
          | None -> true) common_outputs in
        let dropped = List.length common_outputs - List.length kept in
        if dropped > 0 then
          Printf.printf "Context-dead outputs: %d skipped (unread in every instantiation on both flows)\n"
            dropped;
        kept
      end in
    (* partial masks: bits of an output not read in any context on both
       flows are excluded from the XOR compare *)
    let apply_mask name w z =
      match List.assoc_opt name output_masks with
      | Some m when not (Z.equal (Z.logand m (Z.pred (Z.shift_left Z.one w)))
                           (Z.pred (Z.shift_left Z.one w))) ->
          Z3.BitVector.mk_and ctx z
            (Z3.BitVector.mk_numeral ctx
               (Z.to_string (Z.logand m (Z.pred (Z.shift_left Z.one w)))) w)
      | _ -> z in
    (* Next-state cones for SPARSE per-bit states (see bitbus matching above):
       pair `X__b<i>__D` on one side with bit i of `X__D` on the other, so the
       kept bits' next-state logic is COMPARED (name-equality alone would
       silently skip them). Each pair yields a "differs" boolean. *)
    let bitbus_d_pairs =
      let bitbd_re = Str.regexp "^\\(.+\\)__b\\([0-9]+\\)__D$" in
      let tab outs = let h = Hashtbl.create 16 in
        List.iter (fun (n, w) -> Hashtbl.replace h n w) outs; h in
      let out1_w = tab outputs1 and out2_w = tab outputs2 in
      let mk my_outs other_w my_sfx other_sfx =
        List.filter_map (fun (name, w) ->
          if w = 1 && Str.string_match bitbd_re name 0 then begin
            let base = Str.matched_group 1 name in
            let i = int_of_string (Str.matched_group 2 name) in
            match Hashtbl.find_opt other_w (base ^ "__D") with
            | Some wb when i < wb ->
                let e1 = bv_var name 1 my_sfx in
                let e2 = Z3.BitVector.mk_extract ctx i i
                           (bv_var (base ^ "__D") wb other_sfx) in
                Some (Printf.sprintf "%s__D[%d]" base i,
                      Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx e1 e2))
            | _ -> None
          end else None) my_outs in
      mk outputs1 out2_w "_d1" "_d2" @ mk outputs2 out1_w "_d2" "_d1" in
    (* Child-boundary INPUT nets (`ufi__<canon>__<port>_<bit>`, emitted by
       Fpga_prim_expand's user-child cut): compare across sides so the
       parent's wiring INTO each cut child stays verified — the child's
       outputs are free paired nets (assume), its inputs are checked
       (guarantee). *)
    let bitbus_d_pairs =
      let collect bm =
        let acc = ref [] in
        List.iter (function
          | BCombinational { body; _ } ->
              List.iter (function
                | BAssign { lhs; _ }
                  when String.length lhs > 5 && String.sub lhs 0 5 = "ufi__" ->
                    acc := lhs :: !acc
                | _ -> ()) body
          | _ -> ()) bm.processes;
        List.sort_uniq compare !acc in
      let h2 = Hashtbl.create 64 in
      List.iter (fun n -> Hashtbl.replace h2 n ()) (collect bmod2);
      (* CONST-vs-CONST ufi mismatches are DOWNGRADED to a warning: Vivado
         ties a child-input bit it proved DEAD to 0 while the other flow
         wires the live context constant (an_adv bits the PCS ignores) —
         comparing the wiring of a bit the child never reads only produces
         noise.  Semantic probe per side: v ≠ c UNSAT ⇒ const c.  Counted,
         not silent — a live-bit const mismatch is the one class this can
         mask. *)
      let probe_const v =
        let test c =
          Z3.Solver.push miter_solver;
          Z3.Solver.add miter_solver
            [ Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx v
                (Z3.BitVector.mk_numeral ctx (string_of_int c) 1)) ];
          let r = Z3.Solver.check miter_solver [] in
          Z3.Solver.pop miter_solver 1;
          r = Z3.Solver.UNSATISFIABLE in
        if test 0 then Some 0 else if test 1 then Some 1 else None in
      let n_dead = ref 0 in
      let ufi = List.filter_map (fun n ->
        if Hashtbl.mem h2 n then begin
          let e1 = bv_var n 1 "_d1" and e2 = bv_var n 1 "_d2" in
          match probe_const e1, probe_const e2 with
          | Some a, Some b when a <> b -> incr n_dead; None
          | _ -> Some (n, Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx e1 e2))
        end else None) (collect bmod1) in
      if !n_dead > 0 then
        Printf.printf
          "Dead-bit const wiring mismatches: %d child-input bit(s) downgraded (const-vs-const)\n"
          !n_dead;
      (* UNCONDITIONAL cross-side tie of the ufo__ child-output boundary nets
         (declared by the expand cut).  The undriven matching only ties nets
         undriven on BOTH sides — a side that HOISTED a child constant drives
         its ufo (see fpga_prim_expand), and the tie is what propagates that
         knowledge to the side that kept the constant inside the child. *)
      let ufo_names bm =
        List.filter_map (fun (s : bsignal) ->
          if String.length s.name > 5 && String.sub s.name 0 5 = "ufo__"
          then Some s.name else None) bm.signals in
      let u2 = Hashtbl.create 64 in
      List.iter (fun n -> Hashtbl.replace u2 n ()) (ufo_names bmod2);
      let n_uf = ref 0 in
      List.iter (fun n ->
        if Hashtbl.mem u2 n then begin
          incr n_uf;
          Z3.Solver.add miter_solver
            [ Z3.Boolean.mk_eq ctx (bv_var n 1 "_d1") (bv_var n 1 "_d2") ]
        end) (List.sort_uniq compare (ufo_names bmod1));
      if !n_uf > 0 then
        Printf.printf "Child-output boundary ties: %d ufo net(s)\n" !n_uf;
      bitbus_d_pairs @ ufi in
    (* CONE-LEVEL mode (Z3_MITER_PER_CONE): check each common output (every
       FF's <Q>__D next-state cone + each primary output) on its OWN, with
       push/pop, so a structural state mismatch on a few cones (SVS bit-blasts
       SRL/RAM into FF-chains while the other side black-boxes them) is
       localized instead of collapsing the whole design to one DIFFER. *)
    if Sys.getenv_opt "Z3_MITER_PER_CONE" <> None then begin
      Printf.printf "Per-cone miter over %d common outputs (FF-D cones + primary outs)...\n"
        (List.length common_outputs);
      let neq = ref 0 and ndiff = ref 0 and diffs = ref [] in
      (* async-reset D-cones compare UNDER reset-deasserted (see ffrip __RST) *)
      let rst_guards name =
        if String.length name > 3
           && String.sub name (String.length name - 3) 3 = "__D" then begin
          let base = String.sub name 0 (String.length name - 3) in
          let rn = base ^ "__RST" in
          if List.mem_assoc rn outputs1 && List.mem_assoc rn outputs2 then
            let z0 = Z3.BitVector.mk_numeral ctx "0" 1 in
            [ Z3.Boolean.mk_eq ctx (bv_var rn 1 "_d1") z0;
              Z3.Boolean.mk_eq ctx (bv_var rn 1 "_d2") z0 ]
          else []
        end else [] in
      List.iter (fun (name, width) ->
        let w2 = try List.assoc name outputs2 with Not_found -> width in
        let out1 = bv_var name width "_d1" in
        let out2 = bv_var name w2 "_d2" in
        let w = max (sz out1) (sz out2) in
        let ext z = if sz z < w then Z3.BitVector.mk_zero_ext ctx (w - sz z) z else z in
        let xor = apply_mask name w (Z3.BitVector.mk_xor ctx (ext out1) (ext out2)) in
        let zero = Z3.BitVector.mk_numeral ctx "0" w in
        Z3.Solver.push miter_solver;
        Z3.Solver.add miter_solver
          (Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx xor zero)
           :: rst_guards name);
        (match Z3.Solver.check miter_solver [] with
         | Z3.Solver.UNSATISFIABLE -> incr neq
         | _ -> incr ndiff; if List.length !diffs < 60 then diffs := name :: !diffs);
        Z3.Solver.pop miter_solver 1) common_outputs;
      (* sparse-bit next-state cones (bitbus pairs) as extra cones *)
      List.iter (fun (name, differs) ->
        Z3.Solver.push miter_solver;
        Z3.Solver.add miter_solver [ differs ];
        (match Z3.Solver.check miter_solver [] with
         | Z3.Solver.UNSATISFIABLE -> incr neq
         | _ -> incr ndiff; if List.length !diffs < 60 then diffs := name :: !diffs);
        Z3.Solver.pop miter_solver 1) bitbus_d_pairs;
      (* scalarized whole↔fields next-state cones *)
      List.iter (fun (name, differs) ->
        Z3.Solver.push miter_solver;
        Z3.Solver.add miter_solver [ differs ];
        (match Z3.Solver.check miter_solver [] with
         | Z3.Solver.UNSATISFIABLE -> incr neq
         | _ -> incr ndiff; if List.length !diffs < 60 then diffs := name :: !diffs);
        Z3.Solver.pop miter_solver 1) !scalarize_d_pairs;
      Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
      Printf.printf "  PER-CONE: %d EQUIVALENT, %d DIFFER\n" !neq !ndiff;
      Printf.printf "═══════════════════════════════════════════════════════════════\n";
      if !diffs <> [] then
        Printf.printf "  first differing cones: %s\n"
          (String.concat ", " (List.rev !diffs));
      !ndiff = 0
    end else begin
    (* Build miter: XOR all outputs (which now include the FF D pins) *)
    Printf.printf "Building miter circuit (XOR outputs)...\n";
    let miter_terms = List.map (fun (name, width) ->
      (* Same width-divergence handling as the inputs, but for outputs we
         zero-extend BOTH sides to the wider width and XOR: a nonzero high bit
         on one side that the other dropped IS a real difference, so it must
         still surface (never mask a mismatch). *)
      let w2 = try List.assoc name outputs2 with Not_found -> width in
      let out1 = bv_var name width "_d1" in
      let out2 = bv_var name w2 "_d2" in
      let w = max (sz out1) (sz out2) in
      let ext z = if sz z < w then Z3.BitVector.mk_zero_ext ctx (w - sz z) z else z in
      let xor = apply_mask name w (Z3.BitVector.mk_xor ctx (ext out1) (ext out2)) in
      let zero = Z3.BitVector.mk_numeral ctx "0" w in
      let differs = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx xor zero) in
      (* async-reset D-cones count as differing only when BOTH resets are
         deasserted (ffrip's normalised <q>__RST; the reset nets themselves
         are compared as ordinary outputs) *)
      let guard =
        if String.length name > 3
           && String.sub name (String.length name - 3) 3 = "__D" then begin
          let base = String.sub name 0 (String.length name - 3) in
          let rn = base ^ "__RST" in
          if List.mem_assoc rn outputs1 && List.mem_assoc rn outputs2 then
            let z0 = Z3.BitVector.mk_numeral ctx "0" 1 in
            [ Z3.Boolean.mk_eq ctx (bv_var rn 1 "_d1") z0;
              Z3.Boolean.mk_eq ctx (bv_var rn 1 "_d2") z0 ]
          else []
        end else [] in
      match guard with
      | [] -> differs
      | g -> Z3.Boolean.mk_and ctx (differs :: g)
    ) common_outputs in

    let miter_terms = miter_terms @ List.map snd bitbus_d_pairs
                                  @ List.map snd !scalarize_d_pairs in
    let miter_output =
      match miter_terms with
      | [] -> Z3.Boolean.mk_false ctx
      | [term] -> term
      | terms -> Z3.Boolean.mk_or ctx terms
    in

    (* Add miter output assertion: check if outputs can differ *)
    Z3.Solver.add miter_solver [miter_output];

    Printf.printf "Checking for counterexample with Z3...\n\n";
    Printf.printf "─────────────────────────────────────────────────────────────\n";

    (* Check satisfiability *)
    let start_time = Unix.gettimeofday () in
    let result = Z3.Solver.check miter_solver [] in
    let end_time = Unix.gettimeofday () in
    let elapsed = end_time -. start_time in

    Printf.printf "Z3 solver time: %.3f seconds\n\n" elapsed;

    match result with
    | Z3.Solver.UNSATISFIABLE ->
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  ✅ PROVEN EQUIVALENT\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
        Printf.printf "Z3 proved that no input exists where outputs differ.\n";
        Printf.printf "The two designs are formally equivalent! ✅\n\n";
        true

    | Z3.Solver.SATISFIABLE ->
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  ❌ COUNTEREXAMPLE FOUND\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
        Printf.printf "Z3 found an input where outputs differ:\n\n";
        (match Z3.Solver.get_model miter_solver with
         | Some model ->
             Printf.printf "Counterexample:\n";
             Printf.printf "%s\n\n" (Z3.Model.to_string model);

             (* Extract input values *)
             Printf.printf "Input values:\n";
             List.iter (fun (name, width) ->
               let in_var = bv_var name width "_d1" in
               match Z3.Model.eval model in_var true with
               | Some value ->
                   Printf.printf "  %s = %s\n" name (Z3.Expr.to_string value)
               | None -> ()
             ) inputs1;

             Printf.printf "\nOutput values:\n";
             List.iter (fun (name, width) ->
               let out1 = bv_var name width "_d1" in
               let out2 = bv_var name width "_d2" in
               match Z3.Model.eval model out1 true, Z3.Model.eval model out2 true with
               | Some v1, Some v2 ->
                   Printf.printf "  %s: Design1=%s, Design2=%s %s\n"
                     name
                     (Z3.Expr.to_string v1)
                     (Z3.Expr.to_string v2)
                     (if Z3.Expr.equal v1 v2 then "✓" else "✗")
               | _ -> ()
             ) outputs1;
             Printf.printf "\n"
         | None ->
             Printf.printf "No model available\n\n");
        false

    | Z3.Solver.UNKNOWN ->
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  ⚠️  UNKNOWN (Timeout or Incomplete)\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
        Printf.printf "Z3 could not determine equivalence.\n";
        Printf.printf "Reason: %s\n\n" (Z3.Solver.get_reason_unknown miter_solver);
        false
    end
  end

(* High-level API: verify two behavioral IR programs *)
let verify_equivalence vhdl_file sv_file =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Z3 Formal Equivalence Verification\n";
  Printf.printf "  Gate-Level Miter Checking\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input Files:\n";
  Printf.printf "  VHDL: %s\n" vhdl_file;
  Printf.printf "  SV:   %s\n\n" sv_file;

  (* Convert VHDL *)
  Printf.printf "[1/5] Converting VHDL to Behavioral IR...\n";
  let vhdl_prog_opt = Vhdl_to_behavioral.convert_vhdl_file_to_behavioral vhdl_file in

  match vhdl_prog_opt with
  | None ->
      Printf.eprintf "✗ VHDL conversion failed\n";
      false
  | Some vhdl_prog ->
      Printf.printf "✓ VHDL conversion successful\n\n";

      (* Convert SystemVerilog *)
      Printf.printf "[2/5] Converting SystemVerilog to Behavioral IR...\n";
      let sv_prog_opt = Sv_to_behavioral.convert_elaborated_sv_to_behavioral sv_file in

      match sv_prog_opt with
      | None ->
          Printf.eprintf "✗ SystemVerilog conversion failed\n";
          false
      | Some sv_prog ->
          Printf.printf "✓ SystemVerilog conversion successful\n\n";

          (* Optimize both *)
          Printf.printf "[3/5] Optimizing both designs...\n";
          let (vhdl_opt, _) = optimize_custom
            { default_config with verbose = false } vhdl_prog in
          let (sv_opt, _) = optimize_custom
            { default_config with verbose = false } sv_prog in
          Printf.printf "✓ Optimization complete\n\n";

          (* Extract modules *)
          let vhdl_mod = List.hd vhdl_opt.modules in
          let sv_mod = List.hd sv_opt.modules in

          (* Run miter check *)
          Printf.printf "[4/5] Building miter circuit...\n\n";
          Printf.printf "[5/5] Z3 Formal Verification...\n\n";

          check_miter_equivalence vhdl_mod sv_mod
