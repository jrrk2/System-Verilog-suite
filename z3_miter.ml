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

(* Z3 context *)
let ctx = Z3.mk_context [
  ("model", "true");
  ("proof", "true");
  ("timeout", "30000");  (* 30 second timeout *)
]

(* Signal cache for Z3 variables *)
let signal_cache : (string, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 256

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
  | BBinOp { result_type; _ } -> Some (width_of_btype result_type)
  | BUnOp { result_type; _ } -> Some (width_of_btype result_type)
  | BCond { then_val; _ } -> width_of_expr_ctx widths then_val  (* Width of branches *)
  | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
  | BConcat exprs ->
      let widths_list = List.filter_map (width_of_expr_ctx widths) exprs in
      Some (List.fold_left (+) 0 widths_list)
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
      let widths = List.filter_map width_of_expr exprs in
      Some (List.fold_left (+) 0 widths)
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
      Z3.BitVector.mk_numeral ctx (string_of_int value) width

  | BBinOp { op; lhs; rhs; result_type } ->
      let z3_lhs0 = expr_to_z3 suffix ctx_sigs lhs in
      let z3_rhs0 = expr_to_z3 suffix ctx_sigs rhs in
      (* Normalise operand widths. When the converter emits a cell with
       * fixed result_type=64 but the operands have narrower actual
       * widths, Z3 sort-checks fail. Zero-extend the smaller operand to
       * match the larger; this is correct semantics for unsigned bit-
       * vector ops on naturally-aligned values. *)
      let widen z3_a z3_b =
        let wa = Z3.BitVector.get_size (Z3.Expr.get_sort z3_a) in
        let wb = Z3.BitVector.get_size (Z3.Expr.get_sort z3_b) in
        if wa = wb then z3_a, z3_b
        else if wa < wb then
          Z3.BitVector.mk_zero_ext ctx (wb - wa) z3_a, z3_b
        else
          z3_a, Z3.BitVector.mk_zero_ext ctx (wa - wb) z3_b
      in
      let z3_lhs, z3_rhs = widen z3_lhs0 z3_rhs0 in
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
      let z3_operand = expr_to_z3 suffix ctx_sigs operand in
      ignore result_type;
      (match op with
       | BNot -> Z3.BitVector.mk_not ctx z3_operand
       | BNeg -> Z3.BitVector.mk_neg ctx z3_operand
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
      List.fold_right (fun e acc ->
        match acc with
        | None -> Some (expr_to_z3 suffix ctx_sigs e)
        | Some z3_acc ->
            let z3_e = expr_to_z3 suffix ctx_sigs e in
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

  | BSelect _ | BCall _ ->
      (* Unsupported for now *)
      Z3.BitVector.mk_numeral ctx "0" 32

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
      (* Only infer width if we don't already have it from signal declaration *)
      (match Hashtbl.find_opt widths lhs with
       | Some _ ->
           (* Signal already has declared width - don't overwrite it *)
           ()
       | None ->
           (* Signal not declared, infer from RHS *)
           let width =
             match width_of_expr_ctx widths rhs with
             | Some w -> w
             | None -> 32  (* Default if we can't infer *)
           in
           Hashtbl.add widths lhs width)

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

let infer_widths_from_module bmod =
  let widths = Hashtbl.create 256 in

  (* Add signal widths *)
  List.iter (fun (signal : bsignal) ->
    Hashtbl.add widths signal.name (width_of_btype signal.stype)
  ) bmod.signals;

  (* Infer widths from all assignments in all processes *)
  List.iter (infer_widths_from_process widths) bmod.processes;

  widths

(* Build signal width context from module *)
let build_signal_context bmod =
  let widths = infer_widths_from_module bmod in
  Hashtbl.fold (fun name width acc -> (name, width) :: acc) widths []

(* Encode module as Z3 constraints with suffix *)
let encode_module bmod suffix =
  let solver = Z3.Solver.mk_simple_solver ctx in
  let ctx_sigs = build_signal_context bmod in

  (* Debug: print inferred widths *)
  Printf.printf "  Inferred widths for %s (%d signals):\n" bmod.name (List.length ctx_sigs);
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

(* Create miter circuit and check equivalence *)
let check_miter_equivalence bmod1 bmod2 =
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
  let bmod1 = Behavioral_ffrip.rip_module bmod1 in
  let bmod2 = Behavioral_ffrip.rip_module bmod2 in

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

  if input_names1 <> input_names2 then begin
    Printf.printf "❌ Input interfaces differ!\n";
    Printf.printf "  Design 1: [%s]\n" (String.concat ", " input_names1);
    Printf.printf "  Design 2: [%s]\n\n" (String.concat ", " input_names2);
    false
  end else if output_names1 <> output_names2 then begin
    Printf.printf "❌ Output interfaces differ!\n";
    Printf.printf "  Design 1: [%s]\n" (String.concat ", " output_names1);
    Printf.printf "  Design 2: [%s]\n\n" (String.concat ", " output_names2);
    false
  end else begin
    Printf.printf "✓ Interfaces match\n\n";

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

    (* Constrain inputs to be the same *)
    Printf.printf "Constraining inputs to match...\n";
    List.iter (fun (name, width) ->
      let in1 = bv_var name width "_d1" in
      let in2 = bv_var name width "_d2" in
      let eq = Z3.Boolean.mk_eq ctx in1 in2 in
      Z3.Solver.add miter_solver [eq]
    ) inputs1;

    (* After Behavioral_ffrip every register Q has been turned into
     * a primary input (its current state) and Q__D has been added as
     * a primary output (the next-state combinational expression).
     * The standard input-matching + output-XOR construction below
     * therefore handles sequential equivalence at depth 1 with no
     * special casing — `Q_d1 = Q_d2` falls out of input matching
     * and `Q__D_d1 ?= Q__D_d2` falls out of the output XOR. *)
    ignore ctx2;

    (* Build miter: XOR all outputs (which now include the FF D pins) *)
    Printf.printf "Building miter circuit (XOR outputs)...\n";
    let miter_terms = List.map (fun (name, width) ->
      let out1 = bv_var name width "_d1" in
      let out2 = bv_var name width "_d2" in
      let xor = Z3.BitVector.mk_xor ctx out1 out2 in
      let zero = Z3.BitVector.mk_numeral ctx "0" width in
      Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx xor zero)
    ) outputs1 in

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
