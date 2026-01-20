(* Z3 verification of OCaml VM ALU operations *)

open Z3

let ctx = mk_context [("model", "true"); ("timeout", "30000")]
let solver = Solver.mk_solver ctx None

(* OCaml integer encoding functions *)
let val_int n =
  let shifted = BitVector.mk_shl ctx n (BitVector.mk_numeral ctx "1" 32) in
  let one = BitVector.mk_numeral ctx "1" 32 in
  BitVector.mk_or ctx shifted one

let int_val v =
  let one = BitVector.mk_numeral ctx "1" 32 in
  BitVector.mk_ashr ctx v one

(* ALU Operations *)
let addint accu tos =
  let a = int_val accu in
  let b = int_val tos in
  val_int (BitVector.mk_add ctx a b)

let subint accu tos =
  let a = int_val accu in
  let b = int_val tos in
  val_int (BitVector.mk_sub ctx a b)

let mulint accu tos =
  let a = int_val accu in
  let b = int_val tos in
  val_int (BitVector.mk_mul ctx a b)

let andint accu tos =
  let a = int_val accu in
  let b = int_val tos in
  val_int (BitVector.mk_and ctx a b)

let orint accu tos =
  let a = int_val accu in
  let b = int_val tos in
  val_int (BitVector.mk_or ctx a b)

let xorint accu tos =
  let a = int_val accu in
  let b = int_val tos in
  val_int (BitVector.mk_xor ctx a b)

(* Verify operation preserves tagged integer property *)
let verify_tagged_result op_name op_func =
  Printf.printf "\n%s: Verify result is tagged integer\n" op_name;
  let accu_in = BitVector.mk_const_s ctx "accu" 32 in
  let tos_in = BitVector.mk_const_s ctx "tos" 32 in
  let result = op_func accu_in tos_in in

  (* LSB must be 1 *)
  let lsb = BitVector.mk_extract ctx 0 0 result in
  let one_bit = BitVector.mk_numeral ctx "1" 1 in
  let lsb_is_one = Boolean.mk_eq ctx lsb one_bit in

  Solver.push solver;
  Solver.add solver [Boolean.mk_not ctx lsb_is_one];
  match Solver.check solver [] with
  | Solver.UNSATISFIABLE ->
      Printf.printf "  ✓ VERIFIED: Result always tagged\n";
      Solver.pop solver 1;
      true
  | Solver.SATISFIABLE ->
      let model = Solver.get_model solver |> Option.get in
      Printf.printf "  ✗ FAILED: Found counterexample\n";
      Printf.printf "    accu = %s\n" (Expr.to_string (Model.eval model accu_in true |> Option.get));
      Printf.printf "    tos = %s\n" (Expr.to_string (Model.eval model tos_in true |> Option.get));
      Solver.pop solver 1;
      false
  | Solver.UNKNOWN ->
      Printf.printf "  ? UNKNOWN\n";
      Solver.pop solver 1;
      false

(* Test specific operation *)
let test_op op_name op_func a_untagged b_untagged expected_untagged =
  let a_tagged = (a_untagged lsl 1) lor 1 in
  let b_tagged = (b_untagged lsl 1) lor 1 in
  let expected_tagged = (expected_untagged lsl 1) lor 1 in

  let a_val = BitVector.mk_numeral ctx (string_of_int a_tagged) 32 in
  let b_val = BitVector.mk_numeral ctx (string_of_int b_tagged) 32 in
  let expected_val = BitVector.mk_numeral ctx (string_of_int expected_tagged) 32 in

  let result = op_func a_val b_val in
  let eq = Boolean.mk_eq ctx result expected_val in

  Solver.push solver;
  Solver.add solver [eq];
  let status = Solver.check solver [] in
  Solver.pop solver 1;

  match status with
  | Solver.SATISFIABLE ->
      Printf.printf "  ✓ %s(%d, %d) = %d\n" op_name a_untagged b_untagged expected_untagged;
      true
  | _ ->
      Printf.printf "  ✗ %s(%d, %d) ≠ %d FAILED\n" op_name a_untagged b_untagged expected_untagged;
      false

let () =
  Printf.printf "====================================\n";
  Printf.printf "OCaml VM ALU Operations Verification\n";
  Printf.printf "====================================\n";

  let all_pass = ref true in

  (* ADDINT *)
  all_pass := !all_pass && verify_tagged_result "ADDINT" addint;
  Printf.printf "  Test cases:\n";
  all_pass := !all_pass && test_op "ADDINT" addint 0 0 0;
  all_pass := !all_pass && test_op "ADDINT" addint 1 2 3;
  all_pass := !all_pass && test_op "ADDINT" addint 100 200 300;
  all_pass := !all_pass && test_op "ADDINT" addint (-5) 3 (-2);

  (* SUBINT *)
  all_pass := !all_pass && verify_tagged_result "SUBINT" subint;
  Printf.printf "  Test cases:\n";
  all_pass := !all_pass && test_op "SUBINT" subint 5 3 2;
  all_pass := !all_pass && test_op "SUBINT" subint 10 5 5;
  all_pass := !all_pass && test_op "SUBINT" subint 0 0 0;
  all_pass := !all_pass && test_op "SUBINT" subint 3 5 (-2);

  (* MULINT *)
  all_pass := !all_pass && verify_tagged_result "MULINT" mulint;
  Printf.printf "  Test cases:\n";
  all_pass := !all_pass && test_op "MULINT" mulint 0 5 0;
  all_pass := !all_pass && test_op "MULINT" mulint 1 10 10;
  all_pass := !all_pass && test_op "MULINT" mulint 3 7 21;
  all_pass := !all_pass && test_op "MULINT" mulint (-2) 5 (-10);

  (* ANDINT *)
  all_pass := !all_pass && verify_tagged_result "ANDINT" andint;
  Printf.printf "  Test cases:\n";
  all_pass := !all_pass && test_op "ANDINT" andint 0xFF 0x0F 0x0F;
  all_pass := !all_pass && test_op "ANDINT" andint 0xAA 0x55 0x00;
  all_pass := !all_pass && test_op "ANDINT" andint 0xFF 0xFF 0xFF;

  (* ORINT *)
  all_pass := !all_pass && verify_tagged_result "ORINT" orint;
  Printf.printf "  Test cases:\n";
  all_pass := !all_pass && test_op "ORINT" orint 0xF0 0x0F 0xFF;
  all_pass := !all_pass && test_op "ORINT" orint 0xAA 0x55 0xFF;
  all_pass := !all_pass && test_op "ORINT" orint 0x00 0x00 0x00;

  (* XORINT *)
  all_pass := !all_pass && verify_tagged_result "XORINT" xorint;
  Printf.printf "  Test cases:\n";
  all_pass := !all_pass && test_op "XORINT" xorint 0xFF 0xFF 0x00;
  all_pass := !all_pass && test_op "XORINT" xorint 0xAA 0x55 0xFF;
  all_pass := !all_pass && test_op "XORINT" xorint 0x0F 0x0F 0x00;

  Printf.printf "\n====================================\n";
  if !all_pass then
    Printf.printf "✓ ALL TESTS PASSED\n"
  else
    Printf.printf "✗ SOME TESTS FAILED\n";
  Printf.printf "====================================\n"
