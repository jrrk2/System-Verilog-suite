(* Z3 verification of OCaml VM ADDINT operation *)

open Z3

let ctx = mk_context [("model", "true"); ("timeout", "30000")]
let solver = Solver.mk_solver ctx None

(* Create bitvector sort for 32-bit values *)
let bv32 = BitVector.mk_sort ctx 32
let bv1 = BitVector.mk_sort ctx 1

(* Create symbolic variables *)
let accu_in = BitVector.mk_const_s ctx "accu_in" 32
let tos_in = BitVector.mk_const_s ctx "tos_in" 32

(* OCaml integer encoding functions *)
(* Val_int(n) = (n << 1) | 1 *)
let val_int n =
  let shifted = BitVector.mk_shl ctx n (BitVector.mk_numeral ctx "1" 32) in
  let one = BitVector.mk_numeral ctx "1" 32 in
  BitVector.mk_or ctx shifted one

(* Int_val(v) = v >>> 1 (arithmetic shift right) *)
let int_val v =
  let one = BitVector.mk_numeral ctx "1" 32 in
  BitVector.mk_ashr ctx v one

(* ADDINT operation: accu_out = Val_int(Int_val(accu_in) + Int_val(tos_in)) *)
let addint_result =
  let accu_untagged = int_val accu_in in
  let tos_untagged = int_val tos_in in
  let sum = BitVector.mk_add ctx accu_untagged tos_untagged in
  val_int sum

(* Property 1: Result LSB should always be 1 (tagged integer) *)
let lsb_is_one =
  let lsb = BitVector.mk_extract ctx 0 0 addint_result in
  let one_bit = BitVector.mk_numeral ctx "1" 1 in
  Boolean.mk_eq ctx lsb one_bit

(* Property 2: Verify specific test cases *)
let test_case accu tos expected_result =
  let accu_val = BitVector.mk_numeral ctx (string_of_int accu) 32 in
  let tos_val = BitVector.mk_numeral ctx (string_of_int tos) 32 in
  let expected_val = BitVector.mk_numeral ctx (string_of_int expected_result) 32 in

  (* Calculate result *)
  let accu_untagged = int_val accu_val in
  let tos_untagged = int_val tos_val in
  let sum = BitVector.mk_add ctx accu_untagged tos_untagged in
  let result = val_int sum in

  (* Assert equality *)
  Boolean.mk_eq ctx result expected_val

(* Run verification *)
let () =
  Printf.printf "=== OCaml VM ADDINT Verification ===\n\n";

  (* Verify Property 1: LSB is always 1 *)
  Printf.printf "Property 1: Result LSB is always 1 (tagged integer)\n";
  Solver.push solver;
  Solver.add solver [Boolean.mk_not ctx lsb_is_one];
  let result1 = Solver.check solver [] in
  (match result1 with
   | Solver.UNSATISFIABLE ->
       Printf.printf "  ✓ VERIFIED: Result is always a tagged integer\n\n"
   | Solver.SATISFIABLE ->
       let model = Solver.get_model solver |> Option.get in
       Printf.printf "  ✗ FAILED: Found counterexample:\n";
       Printf.printf "    accu_in = %s\n" (Expr.to_string (Model.eval model accu_in true |> Option.get));
       Printf.printf "    tos_in = %s\n" (Expr.to_string (Model.eval model tos_in true |> Option.get));
       Printf.printf "    result = %s\n\n" (Expr.to_string (Model.eval model addint_result true |> Option.get))
   | Solver.UNKNOWN ->
       Printf.printf "  ? UNKNOWN (solver timeout)\n\n");
  Solver.pop solver 1;

  (* Test specific cases *)
  Printf.printf "Property 2: Specific test cases\n";

  (* Test 1: Val_int(0) + Val_int(0) = Val_int(0) *)
  (*   Val_int(0) = 1, Val_int(0) = 1, result should be 1 *)
  let val0 = 1 in  (* Val_int(0) = (0 << 1) | 1 = 1 *)
  Solver.push solver;
  Solver.add solver [test_case val0 val0 val0];
  (match Solver.check solver [] with
   | Solver.SATISFIABLE ->
       Printf.printf "  ✓ Test 1: Val_int(0) + Val_int(0) = Val_int(0)\n"
   | _ ->
       Printf.printf "  ✗ Test 1 FAILED\n");
  Solver.pop solver 1;

  (* Test 2: Val_int(1) + Val_int(2) = Val_int(3) *)
  (*   Val_int(1) = 3, Val_int(2) = 5, Val_int(3) = 7 *)
  let val1 = 3 in   (* Val_int(1) = (1 << 1) | 1 = 3 *)
  let val2 = 5 in   (* Val_int(2) = (2 << 1) | 1 = 5 *)
  let val3 = 7 in   (* Val_int(3) = (3 << 1) | 1 = 7 *)
  Solver.push solver;
  Solver.add solver [test_case val1 val2 val3];
  (match Solver.check solver [] with
   | Solver.SATISFIABLE ->
       Printf.printf "  ✓ Test 2: Val_int(1) + Val_int(2) = Val_int(3)\n"
   | _ ->
       Printf.printf "  ✗ Test 2 FAILED\n");
  Solver.pop solver 1;

  (* Test 3: Val_int(100) + Val_int(200) = Val_int(300) *)
  let val100 = 201 in  (* Val_int(100) = (100 << 1) | 1 = 201 *)
  let val200 = 401 in  (* Val_int(200) = (200 << 1) | 1 = 401 *)
  let val300 = 601 in  (* Val_int(300) = (300 << 1) | 1 = 601 *)
  Solver.push solver;
  Solver.add solver [test_case val100 val200 val300];
  (match Solver.check solver [] with
   | Solver.SATISFIABLE ->
       Printf.printf "  ✓ Test 3: Val_int(100) + Val_int(200) = Val_int(300)\n"
   | _ ->
       Printf.printf "  ✗ Test 3 FAILED\n");
  Solver.pop solver 1;

  (* Test 4: Negative numbers - Val_int(-5) + Val_int(3) = Val_int(-2) *)
  let val_neg5 = (-5 lsl 1) lor 1 in   (* Val_int(-5) *)
  let val_3 = 7 in                      (* Val_int(3) *)
  let val_neg2 = (-2 lsl 1) lor 1 in   (* Val_int(-2) *)
  Solver.push solver;
  Solver.add solver [test_case val_neg5 val_3 val_neg2];
  (match Solver.check solver [] with
   | Solver.SATISFIABLE ->
       Printf.printf "  ✓ Test 4: Val_int(-5) + Val_int(3) = Val_int(-2)\n"
   | _ ->
       Printf.printf "  ✗ Test 4 FAILED\n");
  Solver.pop solver 1;

  Printf.printf "\n=== Verification Complete ===\n"
