(* Test VHDL vs SystemVerilog equivalence using Z3 verification *)
(* This proves the SystemVerilog translation is correct against VHDL ground truth *)

open Vhdl_to_ir
open Sv_ir_verify

type comparison_result = {
  module_name: string;
  vhdl_file: string;
  sv_file: string;
  vhdl_to_ir: bool;
  sv_to_ir: bool;
  equivalent: bool;
  error: string option;
}

let test_vhdl_sv_pair vhdl_file sv_file module_name =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Testing: %s\n" module_name;
  Printf.printf "  VHDL: %s\n" (Filename.basename vhdl_file);
  Printf.printf "  SV:   %s\n" (Filename.basename sv_file);
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  try
    (* 1. Convert VHDL to IR *)
    Printf.printf "[1/3] Converting VHDL to IR... ";
    flush stdout;
    let vhdl_ir = match convert_vhdl_file_to_ir vhdl_file with
      | Some ir -> Printf.printf "✓\n"; flush stdout; ir
      | None -> raise (Failure "Failed to convert VHDL to IR")
    in

    (* 2. Convert SystemVerilog to IR using Verible *)
    Printf.printf "[2/3] Converting SystemVerilog to IR... ";
    flush stdout;
    let sv_ir = match Sv_verible_to_ir.file_to_ir sv_file with
      | Some ir -> Printf.printf "✓\n"; flush stdout; ir
      | None -> raise (Failure "Failed to convert SystemVerilog to IR")
    in

    (* 3. Verify equivalence with Z3 *)
    Printf.printf "[3/3] Verifying equivalence with Z3...\n";
    flush stdout;

    Printf.printf "  Comparing IR structures...\n";
    Printf.printf "    VHDL IR: %d inputs, %d outputs, %d nodes\n"
      (Hashtbl.length vhdl_ir.ir_inputs)
      (Hashtbl.length vhdl_ir.ir_outputs)
      (Hashtbl.length vhdl_ir.ir_nodes);
    Printf.printf "    SV IR:   %d inputs, %d outputs, %d nodes\n"
      (Hashtbl.length sv_ir.ir_inputs)
      (Hashtbl.length sv_ir.ir_outputs)
      (Hashtbl.length sv_ir.ir_nodes);

    Printf.printf "\n  Running Z3 verification... ";
    flush stdout;
    let equivalent = verify_ir_equivalence vhdl_ir sv_ir in
    Printf.printf "%s\n" (if equivalent then "✓" else "✗");

    if equivalent then
      Printf.printf "\n✅ PASS: VHDL and SystemVerilog are mathematically equivalent\n"
    else
      Printf.printf "\n❌ FAIL: VHDL and SystemVerilog are NOT equivalent\n";

    Printf.printf "\n";

    {
      module_name;
      vhdl_file;
      sv_file;
      vhdl_to_ir = true;
      sv_to_ir = true;
      equivalent;
      error = None;
    }

  with e ->
    let error_msg = Printexc.to_string e in
    Printf.printf "✗ ERROR: %s\n\n" error_msg;
    {
      module_name;
      vhdl_file;
      sv_file;
      vhdl_to_ir = false;
      sv_to_ir = false;
      equivalent = false;
      error = Some error_msg;
    }

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL vs SystemVerilog Equivalence Verification\n";
  Printf.printf "  Proving decompiler correctness against ground truth\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "\n";

  (* Test pairs: (vhdl_file, sv_file, module_name) *)
  let test_pairs = [
    ("sysver_tests/apb_uart.vhd", "sysver_tests/apb_uart.sv", "apb_uart");
    ("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv", "slib_clock_div");
    ("sysver_tests/slib_counter.vhd", "sysver_tests/slib_counter.sv", "slib_counter");
    ("sysver_tests/slib_edge_detect.vhd", "sysver_tests/slib_edge_detect.sv", "slib_edge_detect");
    ("sysver_tests/slib_fifo.vhd", "sysver_tests/slib_fifo.sv", "slib_fifo");
    ("sysver_tests/slib_input_filter.vhd", "sysver_tests/slib_input_filter.sv", "slib_input_filter");
    ("sysver_tests/slib_input_sync.vhd", "sysver_tests/slib_input_sync.sv", "slib_input_sync");
    ("sysver_tests/slib_mv_filter.vhd", "sysver_tests/slib_mv_filter.sv", "slib_mv_filter");
    ("sysver_tests/uart_baudgen.vhd", "sysver_tests/uart_baudgen.sv", "uart_baudgen");
    ("sysver_tests/uart_interrupt.vhd", "sysver_tests/uart_interrupt.sv", "uart_interrupt");
    ("sysver_tests/uart_receiver.vhd", "sysver_tests/uart_receiver.sv", "uart_receiver");
    ("sysver_tests/uart_transmitter.vhd", "sysver_tests/uart_transmitter.sv", "uart_transmitter");
  ] in

  let results = List.map (fun (vhdl, sv, name) ->
    test_vhdl_sv_pair vhdl sv name
  ) test_pairs in

  (* Print summary *)
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let passed = List.filter (fun r -> r.equivalent) results in
  let failed_conversion = List.filter (fun r ->
    r.error <> None || not r.vhdl_to_ir || not r.sv_to_ir
  ) results in
  let failed_verification = List.filter (fun r ->
    r.vhdl_to_ir && r.sv_to_ir && not r.equivalent && r.error = None
  ) results in

  Printf.printf "Total tests: %d\n" (List.length results);
  Printf.printf "Passed: %d (equivalent)\n" (List.length passed);
  Printf.printf "Failed conversion: %d\n" (List.length failed_conversion);
  Printf.printf "Failed verification: %d\n\n" (List.length failed_verification);

  if List.length failed_conversion > 0 then begin
    Printf.printf "Conversion failures:\n";
    List.iter (fun r ->
      Printf.printf "  - %s: %s\n" r.module_name
        (match r.error with Some e -> e | None -> "conversion failed")
    ) failed_conversion;
    Printf.printf "\n"
  end;

  if List.length failed_verification > 0 then begin
    Printf.printf "Verification failures:\n";
    List.iter (fun r ->
      Printf.printf "  - %s: VHDL ≠ SystemVerilog\n" r.module_name
    ) failed_verification;
    Printf.printf "\n"
  end;

  if List.length passed = List.length results then begin
    Printf.printf "✅ All tests passed!\n\n";
    Printf.printf "The SystemVerilog translations are mathematically proven\n";
    Printf.printf "equivalent to the original VHDL source code.\n";
    Printf.printf "\n";
    Printf.printf "This validates:\n";
    Printf.printf "  • VHDL parser and IR conversion\n";
    Printf.printf "  • SystemVerilog decompiler\n";
    Printf.printf "  • Z3 formal verification\n";
    exit 0
  end else begin
    Printf.printf "❌ Some tests failed\n";
    exit 1
  end
