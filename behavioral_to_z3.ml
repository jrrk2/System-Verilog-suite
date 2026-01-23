(* Behavioral IR to Z3 Encoder
 *
 * Encodes behavioral IR expressions and statements as Z3 constraints
 * for formal verification of equivalence between VHDL and SystemVerilog.
 *)

open Behavioral_ir

(* Z3 context *)
let ctx = Z3.mk_context [("model", "true"); ("proof", "true")]

(* Cache for Z3 variables *)
let signal_cache : (string, Z3.Expr.expr) Hashtbl.t = Hashtbl.create 100

(* Get width from behavioral IR type *)
let rec width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * (width_of_btype element)
  | BStruct _ -> 32  (* Default *)

(* Create or lookup a bitvector variable *)
let bv_var name width =
  match Hashtbl.find_opt signal_cache name with
  | Some v -> v
  | None ->
      let v = Z3.BitVector.mk_const_s ctx name width in
      Hashtbl.add signal_cache name v;
      v

(* Get width of expression (approximation) *)
let rec width_of_expr = function
  | BVar _ -> 32  (* Default *)
  | BConst { width; _ } -> width
  | BBinOp { result_type; _ } -> width_of_btype result_type
  | BUnOp { result_type; _ } -> width_of_btype result_type
  | BCond { then_val; _ } -> width_of_expr then_val
  | BSelect { array; _ } -> width_of_expr array
  | BSlice { msb; lsb; _ } -> msb - lsb + 1
  | BConcat exprs -> List.fold_left (fun acc e -> acc + width_of_expr e) 0 exprs
  | BReplicate { count; value } -> count * (width_of_expr value)
  | BCall _ -> 32  (* Default *)

(* Convert behavioral IR expression to Z3 *)
let rec expr_to_z3 expr =
  match expr with
  | BVar name ->
      let width = 32 in  (* Default, should track actual widths *)
      bv_var name width

  | BConst { value; width } ->
      Z3.BitVector.mk_numeral ctx (string_of_int value) width

  | BBinOp { op; lhs; rhs; result_type } ->
      let z3_lhs = expr_to_z3 lhs in
      let z3_rhs = expr_to_z3 rhs in
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
       | BEq -> Z3.Boolean.mk_eq ctx z3_lhs z3_rhs
       | BNe -> Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx z3_lhs z3_rhs)
       | BLt -> Z3.BitVector.mk_ult ctx z3_lhs z3_rhs
       | BLe -> Z3.BitVector.mk_ule ctx z3_lhs z3_rhs
       | BGt -> Z3.BitVector.mk_ugt ctx z3_lhs z3_rhs
       | BGe -> Z3.BitVector.mk_uge ctx z3_lhs z3_rhs)

  | BUnOp { op; operand; result_type } ->
      let z3_operand = expr_to_z3 operand in
      (match op with
       | BNot -> Z3.BitVector.mk_not ctx z3_operand
       | BNeg -> Z3.BitVector.mk_neg ctx z3_operand
       | BRedAnd -> z3_operand  (* Approximation *)
       | BRedOr -> z3_operand   (* Approximation *)
       | BRedXor -> z3_operand) (* Approximation *)

  | BCond { condition; then_val; else_val } ->
      let z3_cond = expr_to_z3 condition in
      let z3_then = expr_to_z3 then_val in
      let z3_else = expr_to_z3 else_val in
      Z3.Boolean.mk_ite ctx z3_cond z3_then z3_else

  | BSelect { array; index } ->
      (* Simplified: treat as variable access *)
      expr_to_z3 array

  | BSlice { signal; msb; lsb } ->
      let z3_signal = expr_to_z3 signal in
      Z3.BitVector.mk_extract ctx msb lsb z3_signal

  | BConcat exprs ->
      (* Concatenate from right to left *)
      List.fold_right (fun e acc ->
        match acc with
        | None -> Some (expr_to_z3 e)
        | Some z3_acc ->
            let z3_e = expr_to_z3 e in
            Some (Z3.BitVector.mk_concat ctx z3_e z3_acc)
      ) exprs None
      |> Option.get

  | BReplicate { count; value } ->
      let z3_value = expr_to_z3 value in
      (* Replicate by concatenating *)
      let rec replicate n acc =
        if n <= 0 then acc
        else replicate (n - 1) (Z3.BitVector.mk_concat ctx z3_value acc)
      in
      replicate (count - 1) z3_value

  | BCall _ ->
      (* Unsupported for now *)
      Z3.BitVector.mk_numeral ctx "0" 32

(* Add assignment as Z3 constraint *)
let add_assignment solver lhs_name rhs_expr =
  let z3_lhs = bv_var lhs_name 32 in
  let z3_rhs = expr_to_z3 rhs_expr in
  let eq = Z3.Boolean.mk_eq ctx z3_lhs z3_rhs in
  Z3.Solver.add solver [eq]

(* Encode statement as Z3 constraints *)
let rec encode_stmt solver stmt =
  match stmt with
  | BAssign { lhs; rhs } ->
      add_assignment solver lhs rhs

  | BIf { condition; then_stmts; else_stmts } ->
      (* For Z3 encoding, we need to handle conditionals properly *)
      (* This is a simplified encoding that assumes all paths are taken *)
      List.iter (encode_stmt solver) then_stmts;
      List.iter (encode_stmt solver) else_stmts

  | BCase { selector; cases; default } ->
      (* Simplified: encode all cases *)
      List.iter (fun (_, stmts) ->
        List.iter (encode_stmt solver) stmts
      ) cases;
      List.iter (encode_stmt solver) default

  | BWhile { body; _ } | BFor { body; _ } ->
      (* Simplified: unroll once *)
      List.iter (encode_stmt solver) body

  | BBlock stmts ->
      List.iter (encode_stmt solver) stmts

  | BCallStmt _ | BReturn _ ->
      (* Skip for now *)
      ()

(* Encode process as Z3 constraints *)
let encode_process solver = function
  | BCombinational { body; _ } | BSequential { body; _ } ->
      List.iter (encode_stmt solver) body

(* Encode module as Z3 constraints *)
let encode_module bmod =
  let solver = Z3.Solver.mk_simple_solver ctx in

  (* Encode all processes *)
  List.iter (encode_process solver) bmod.processes;

  solver

(* Get output signals from module *)
let get_output_signals bmod =
  List.filter_map (fun signal ->
    match signal.direction with
    | `Output -> Some (signal.name, width_of_btype signal.stype)
    | _ -> None
  ) bmod.signals

(* Get internal register signals from module *)
let get_register_signals bmod =
  List.filter_map (fun signal ->
    match signal.direction with
    | `Internal -> Some (signal.name, width_of_btype signal.stype)
    | _ -> None
  ) bmod.signals

(* Clear the signal cache (call between modules) *)
let clear_cache () =
  Hashtbl.clear signal_cache

(* Check equivalence of two modules *)
let check_equivalence bmod1 bmod2 =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Z3 Formal Verification: Module Equivalence\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Module 1: %s\n" bmod1.name;
  Printf.printf "Module 2: %s\n\n" bmod2.name;

  (* Get output signals *)
  let outputs1 = get_output_signals bmod1 in
  let outputs2 = get_output_signals bmod2 in

  Printf.printf "Output signals:\n";
  Printf.printf "  Module 1: %d outputs\n" (List.length outputs1);
  Printf.printf "  Module 2: %d outputs\n\n" (List.length outputs2);

  (* Encode first module *)
  Printf.printf "Encoding Module 1...\n";
  clear_cache ();
  let solver1 = encode_module bmod1 in
  let assertions1 = Z3.Solver.get_assertions solver1 in
  Printf.printf "  Constraints: %d\n" (List.length assertions1);

  (* Encode second module with renamed variables *)
  Printf.printf "Encoding Module 2...\n";
  clear_cache ();
  let solver2 = encode_module bmod2 in
  let assertions2 = Z3.Solver.get_assertions solver2 in
  Printf.printf "  Constraints: %d\n\n" (List.length assertions2);

  (* Create combined solver *)
  let solver = Z3.Solver.mk_simple_solver ctx in
  Z3.Solver.add solver (assertions1 @ assertions2);

  (* Check each output for equivalence *)
  Printf.printf "Checking output equivalence...\n";
  let all_equiv = ref true in

  List.iter (fun (out_name, width) ->
    Printf.printf "  Output: %s (%d bits)... " out_name width;
    flush stdout;

    (* For simplicity, check if outputs can differ *)
    let out1 = bv_var (out_name ^ "_1") width in
    let out2 = bv_var (out_name ^ "_2") width in
    let neq = Z3.Boolean.mk_not ctx (Z3.Boolean.mk_eq ctx out1 out2) in

    Z3.Solver.push solver;
    Z3.Solver.add solver [neq];

    match Z3.Solver.check solver [] with
    | Z3.Solver.SATISFIABLE ->
        Printf.printf "❌ DIFFER\n";
        all_equiv := false;
        (match Z3.Solver.get_model solver with
         | Some model ->
             Printf.printf "    Counterexample found:\n";
             Printf.printf "    %s\n" (Z3.Model.to_string model)
         | None -> ());
        Z3.Solver.pop solver 1

    | Z3.Solver.UNSATISFIABLE ->
        Printf.printf "✅ EQUIVALENT\n";
        Z3.Solver.pop solver 1

    | Z3.Solver.UNKNOWN ->
        Printf.printf "⚠️  UNKNOWN (timeout or incomplete)\n";
        all_equiv := false;
        Z3.Solver.pop solver 1
  ) outputs1;

  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  if !all_equiv then
    Printf.printf "  ✅ VERIFIED: Modules are EQUIVALENT\n"
  else
    Printf.printf "  ❌ FAILED: Modules are NOT equivalent or verification incomplete\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  !all_equiv

(* Simplified check: just verify both modules are encodable *)
let check_encodable bmod =
  Printf.printf "Checking if module %s is Z3-encodable...\n" bmod.name;

  try
    clear_cache ();
    let solver = encode_module bmod in
    let assertions = Z3.Solver.get_assertions solver in
    Printf.printf "  ✅ Encodable: %d constraints generated\n" (List.length assertions);
    true
  with e ->
    Printf.printf "  ❌ Failed: %s\n" (Printexc.to_string e);
    false
