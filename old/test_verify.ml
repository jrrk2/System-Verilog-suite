(* test_verify.ml - Simple test harness for HardCaml verification
   
   This bypasses the need to re-parse Verilog by using the AST directly.
*)

let test_alu () =
  Printf.printf "Testing ALU verification...\n\n";
  
  (* Load original *)
  let original_json = "obj_dir/Valu.tree.json" in
  if not (Sys.file_exists original_json) then begin
    Printf.eprintf "Skipping: %s not found\n" original_json;
    exit 0
  end;
  
  Printf.printf "Loading original AST...\n";
  let original_ast = Sv_parse.parse (Yojson.Basic.from_file original_json) in
  
  (* For testing, we verify the original against itself *)
  (* This should always pass and tests the verification infrastructure *)
  Printf.printf "Running self-check (original vs original)...\n\n";
  
  let result = Sv_verify_hardcaml.check_equivalence original_ast original_ast in
  
  if result then begin
    Printf.printf "\n✅ Self-check PASSED\n";
    Printf.printf "Verification infrastructure is working correctly!\n\n"
  end else begin
    Printf.printf "\n❌ Self-check FAILED\n";
    Printf.printf "There may be an issue with the verification setup.\n\n";
    exit 1
  end;
  
  (* Now test with actual HardCaml output if available *)
  Printf.printf "Note: To verify HardCaml output, you need to:\n";
  Printf.printf "1. Generate HardCaml Verilog: ./sv_main_unified scan hardcaml output/\n";
  Printf.printf "2. Parse back to JSON (requires Verilator or custom parser)\n";
  Printf.printf "3. Run: ./verify_main.exe original.json hardcaml.json\n"

let () = test_alu ()
