(* test_3way_suite.ml - Comprehensive 3-way parser test suite *)
(* Tests Yosys, Verilator, and Verible parsers on multiple files *)

open Sv_ir_verify

type test_result = {
  file: string;
  module_name: string;
  yosys_verilator: bool;
  yosys_verible: bool;
  verilator_verible: bool;
  passed: bool;
  error: string option;
}

let test_single_file filename module_name =
  Printf.printf "=== Testing: %s (module: %s) ===\n" filename module_name;
  try
    (* 1. Get Yosys IR *)
    Printf.printf "  [1/3] Loading Yosys IR... ";
    flush stdout;
    let rtlil_file = "sysver_tests/obj_dir/" ^ module_name ^ ".il" in
    if not (Sys.file_exists rtlil_file) then
      raise (Failure ("RTLIL file not found: " ^ rtlil_file));
    let design = Sv_rtlil_reader.parse_rtlil_file rtlil_file in
    let yosys_ir = match Sv_rtlil_to_ir.rtlil_design_to_ir design with
      | None -> raise (Failure "Failed to convert RTLIL to IR")
      | Some ir -> Printf.printf "✓\n"; flush stdout; ir
    in

    (* 2. Get Verilator IR *)
    Printf.printf "  [2/3] Loading Verilator IR... ";
    flush stdout;
    let json_file = "sysver_tests/obj_dir/V" ^ module_name ^ ".tree.json" in
    if not (Sys.file_exists json_file) then
      raise (Failure ("JSON file not found: " ^ json_file));
    let json = match Yojson.Safe.from_file json_file with
      | `Assoc lst -> `Assoc (List.rev lst)
      | oth -> oth
    in
    let ast = Sv_parse.parse json in
    let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in
    Printf.printf "✓\n";
    flush stdout;

    (* 3. Get Verible IR *)
    Printf.printf "  [3/3] Loading Verible IR... ";
    flush stdout;
    let verible_ir = match Sv_verible_to_ir.file_to_ir filename with
      | Some ir -> Printf.printf "✓\n\n"; flush stdout; ir
      | None -> raise (Failure "Failed to create Verible IR")
    in

    (* Verify equivalence *)
    Printf.printf "  Verifying:\n";

    Printf.printf "    Yosys ↔ Verilator... ";
    flush stdout;
    let result1 = verify_ir_equivalence yosys_ir verilator_ir in
    Printf.printf "%s\n" (if result1 then "✓" else "✗");

    Printf.printf "    Yosys ↔ Verible...   ";
    flush stdout;
    let result2 = verify_ir_equivalence yosys_ir verible_ir in
    Printf.printf "%s\n" (if result2 then "✓" else "✗");

    Printf.printf "    Verilator ↔ Verible... ";
    flush stdout;
    let result3 = verify_ir_equivalence verilator_ir verible_ir in
    Printf.printf "%s\n" (if result3 then "✓" else "✗");

    let passed = result1 && result2 && result3 in
    Printf.printf "  Result: %s\n\n" (if passed then "✓ PASS" else "✗ FAIL");

    {
      file = filename;
      module_name;
      yosys_verilator = result1;
      yosys_verible = result2;
      verilator_verible = result3;
      passed;
      error = None;
    }

  with e ->
    let error_msg = Printexc.to_string e in
    Printf.printf "  ✗ ERROR: %s\n\n" error_msg;
    {
      file = filename;
      module_name;
      yosys_verilator = false;
      yosys_verible = false;
      verilator_verible = false;
      passed = false;
      error = Some error_msg;
    }

let () =
  (* Test cases: (filename, module_name) *)
  let test_cases = [
    ("sysver_tests/test_01_simple_dff.sv", "test_dff");
    ("sysver_tests/test_09_always_comb_simple.sv", "test_comb");
    ("sysver_tests/continuous_assign.sv", "cont_assign");
    ("sysver_tests/test_05_dff_enable.sv", "test_dff_en");
    ("sysver_tests/test_10_always_comb_mux.sv", "test_comb_mux");
    ("sysver_tests/test_11_always_comb_case.sv", "test_decoder");
    ("sysver_tests/test_12_always_star.sv", "test_star");
    ("sysver_tests/test_02_dff_async_reset_high.sv", "test_dff_rst");
    ("sysver_tests/test_03_dff_async_reset_low.sv", "test_dff_rstn");
    ("sysver_tests/test_04_dff_sync_reset.sv", "test_dff_sync_rst");
    ("sysver_tests/test_06_counter.sv", "test_counter");
    ("sysver_tests/test_13_negedge_clock.sv", "test_negedge");
    ("sysver_tests/test_14_multi_reg.sv", "test_multi_reg");
    ("sysver_tests/test_15_priority_encoder.sv", "test_priority");
    ("sysver_tests/signed_mult.sv", "signed_mult");
    ("sysver_tests/enable_test.sv", "enable_test");
  ] in

  Printf.printf "========================================\n";
  Printf.printf "3-Way Parser Verification Suite\n";
  Printf.printf "Yosys ↔ Verilator ↔ Verible\n";
  Printf.printf "========================================\n\n";

  let results = List.map (fun (file, modname) -> test_single_file file modname) test_cases in

  (* Print summary *)
  Printf.printf "========================================\n";
  Printf.printf "Summary\n";
  Printf.printf "========================================\n\n";

  let passed = List.filter (fun r -> r.passed) results in
  let failed = List.filter (fun r -> not r.passed && r.error = None) results in
  let errors = List.filter (fun r -> r.error <> None) results in

  Printf.printf "Total tests: %d\n" (List.length results);
  Printf.printf "Passed: %d\n" (List.length passed);
  Printf.printf "Failed: %d\n" (List.length failed);
  Printf.printf "Errors: %d\n\n" (List.length errors);

  if List.length failed > 0 then begin
    Printf.printf "Failed tests:\n";
    List.iter (fun r ->
      Printf.printf "  - %s (%s)\n" r.file r.module_name;
      if not r.yosys_verilator then Printf.printf "      Yosys ≠ Verilator\n";
      if not r.yosys_verible then Printf.printf "      Yosys ≠ Verible\n";
      if not r.verilator_verible then Printf.printf "      Verilator ≠ Verible\n";
    ) failed;
    Printf.printf "\n"
  end;

  if List.length errors > 0 then begin
    Printf.printf "Errors:\n";
    List.iter (fun r ->
      Printf.printf "  - %s: %s\n" r.file (Option.get r.error)
    ) errors;
    Printf.printf "\n"
  end;

  if List.length passed = List.length results then begin
    Printf.printf "✅ All tests passed!\n";
    Printf.printf "\nAll three parsers (Yosys, Verilator, Verible) produce\n";
    Printf.printf "mathematically equivalent results verified by Z3.\n";
    exit 0
  end else begin
    Printf.printf "❌ Some tests failed\n";
    exit 1
  end
