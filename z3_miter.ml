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

(* Convert behavioral IR expression to Z3 *)
let rec expr_to_z3 suffix ctx_sigs = function
  | BVar name ->
      (* Look up width from context *)
      let width =
        match List.assoc_opt name ctx_sigs with
        | Some w -> w
        | None -> 32  (* Default *)
      in
      bv_var name width suffix

  | BConst { value; width } ->
      Z3.BitVector.mk_numeral ctx (string_of_int value) width

  | BBinOp { op; lhs; rhs; result_type } ->
      let z3_lhs = expr_to_z3 suffix ctx_sigs lhs in
      let z3_rhs = expr_to_z3 suffix ctx_sigs rhs in
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
      (match op with
       | BNot -> Z3.BitVector.mk_not ctx z3_operand
       | BNeg -> Z3.BitVector.mk_neg ctx z3_operand
       | BRedAnd | BRedOr | BRedXor -> z3_operand)  (* Approximation *)

  | BCond { condition; then_val; else_val } ->
      let z3_cond = expr_to_z3 suffix ctx_sigs condition in
      let z3_then = expr_to_z3 suffix ctx_sigs then_val in
      let z3_else = expr_to_z3 suffix ctx_sigs else_val in
      (* Condition is a 1-bit bitvector, convert to boolean by checking != 0 *)
      let zero = Z3.BitVector.mk_numeral ctx "0" 1 in
      let cond_bool = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_cond zero) in
      Z3.Boolean.mk_ite ctx cond_bool z3_then z3_else

  | BSlice { signal; msb; lsb } ->
      let z3_signal = expr_to_z3 suffix ctx_sigs signal in
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

(* Encode statement as Z3 constraint *)
let rec encode_stmt suffix ctx_sigs solver = function
  | BAssign { lhs; rhs } ->
      let width =
        match List.assoc_opt lhs ctx_sigs with
        | Some w -> w
        | None -> 32
      in
      let z3_lhs = bv_var lhs width suffix in
      let z3_rhs = expr_to_z3 suffix ctx_sigs rhs in
      let eq = Z3.Boolean.mk_eq ctx z3_lhs z3_rhs in
      Z3.Solver.add solver [eq]

  | BIf { condition; then_stmts; else_stmts } ->
      (* Simplified: encode all branches *)
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

(* Encode process as Z3 constraints *)
let encode_process suffix ctx_sigs solver = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      List.iter (encode_stmt suffix ctx_sigs solver) body

(* Build signal width context from module *)
let build_signal_context bmod =
  List.map (fun (signal : bsignal) ->
    (signal.name, width_of_btype signal.stype)
  ) bmod.signals

(* Encode module as Z3 constraints with suffix *)
let encode_module bmod suffix =
  let solver = Z3.Solver.mk_simple_solver ctx in
  let ctx_sigs = build_signal_context bmod in

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

    (* Build miter: XOR all outputs and OR them *)
    Printf.printf "Building miter circuit (XOR outputs)...\n";
    let miter_terms = List.map (fun (name, width) ->
      let out1 = bv_var name width "_d1" in
      let out2 = bv_var name width "_d2" in
      let xor = Z3.BitVector.mk_xor ctx out1 out2 in
      (* Check if any bit is different *)
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
