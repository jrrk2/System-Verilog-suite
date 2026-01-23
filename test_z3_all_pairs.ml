(* Z3 Verification for all VHDL/SystemVerilog pairs *)
(* Proves that SV decompilation matches original VHDL ground truth *)

open Vhdl_to_ir
open Sv_verible_to_ir
open Sv_ir_verify

type pair_result = {
  module_name: string;
  vhdl_success: bool;
  sv_success: bool;
  equivalent: bool;
  vhdl_ir: Sv_ast.opt_ir option;
  sv_ir: Sv_ast.opt_ir option;
  error: string option;
}

let test_pair vhdl_file sv_file module_name =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Module: %s\n" module_name;
  Printf.printf "  VHDL: %s\n" (Filename.basename vhdl_file);
  Printf.printf "  SV:   %s\n" (Filename.basename sv_file);
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  try
    (* Step 1: Convert VHDL to IR *)
    Printf.printf "[1/3] Converting VHDL to IR...\n";
    flush stdout;

    let vhdl_ir = match convert_vhdl_file_to_ir vhdl_file with
      | Some ir ->
          Printf.printf "  ✓ VHDL IR: %d inputs, %d outputs, %d nodes\n"
            (Hashtbl.length ir.ir_inputs)
            (Hashtbl.length ir.ir_outputs)
            (Hashtbl.length ir.ir_nodes);
          ir
      | None ->
          raise (Failure "Failed to convert VHDL to IR")
    in

    (* Step 2: Convert SystemVerilog to IR *)
    Printf.printf "\n[2/3] Converting SystemVerilog to IR...\n";
    flush stdout;

    let sv_ir = match file_to_ir sv_file with
      | Some ir ->
          Printf.printf "  ✓ SV IR: %d inputs, %d outputs, %d nodes\n"
            (Hashtbl.length ir.ir_inputs)
            (Hashtbl.length ir.ir_outputs)
            (Hashtbl.length ir.ir_nodes);
          ir
      | None ->
          raise (Failure "Failed to convert SystemVerilog to IR")
    in

    (* Step 3: Verify equivalence with Z3 *)
    Printf.printf "\n[3/3] Verifying equivalence with Z3...\n";
    flush stdout;

    Printf.printf "  Comparing IR structures:\n";
    Printf.printf "    VHDL: %d inputs, %d outputs, %d nodes\n"
      (Hashtbl.length vhdl_ir.ir_inputs)
      (Hashtbl.length vhdl_ir.ir_outputs)
      (Hashtbl.length vhdl_ir.ir_nodes);
    Printf.printf "    SV:   %d inputs, %d outputs, %d nodes\n"
      (Hashtbl.length sv_ir.ir_inputs)
      (Hashtbl.length sv_ir.ir_outputs)
      (Hashtbl.length sv_ir.ir_nodes);

    Printf.printf "\n  Running Z3 verification... ";
    flush stdout;

    let equivalent = verify_ir_equivalence vhdl_ir sv_ir in

    if equivalent then begin
      Printf.printf "✓\n\n";
      Printf.printf "✅ PASS: VHDL and SystemVerilog are mathematically equivalent\n";
      Printf.printf "   (Verified by Z3 SMT solver)\n\n"
    end else begin
      Printf.printf "✗\n\n";
      Printf.printf "❌ FAIL: VHDL and SystemVerilog are NOT equivalent\n\n"
    end;

    {
      module_name;
      vhdl_success = true;
      sv_success = true;
      equivalent;
      vhdl_ir = Some vhdl_ir;
      sv_ir = Some sv_ir;
      error = None;
    }

  with e ->
    let error_msg = Printexc.to_string e in
    Printf.printf "✗ ERROR: %s\n\n" error_msg;
    {
      module_name;
      vhdl_success = false;
      sv_success = false;
      equivalent = false;
      vhdl_ir = None;
      sv_ir = None;
      error = Some error_msg;
    }

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Z3 Equivalence Verification Suite\n";
  Printf.printf "  VHDL Ground Truth vs SystemVerilog Translation\n";
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

  let results = List.map (fun (vhdl, sv, name) -> test_pair vhdl sv name) test_pairs in

  (* Print summary *)
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let passed = List.filter (fun r -> r.equivalent) results in
  let failed_conversion = List.filter (fun r ->
    not r.vhdl_success || not r.sv_success
  ) results in
  let failed_verification = List.filter (fun r ->
    r.vhdl_success && r.sv_success && not r.equivalent
  ) results in

  Printf.printf "Total pairs: %d\n" (List.length results);
  Printf.printf "Equivalent: %d (✅)\n" (List.length passed);
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
    Printf.printf "Verification failures (IRs differ):\n";
    List.iter (fun r ->
      Printf.printf "  - %s: VHDL ≠ SystemVerilog\n" r.module_name
    ) failed_verification;
    Printf.printf "\n"
  end;

  if List.length passed = List.length results then begin
    Printf.printf "✅ SUCCESS: All pairs mathematically equivalent!\n\n";
    Printf.printf "The SystemVerilog translations are proven equivalent to\n";
    Printf.printf "the original VHDL source code by Z3 SMT solver.\n\n";
    Printf.printf "This validates:\n";
    Printf.printf "  • VHDL parser and IR conversion\n";
    Printf.printf "  • SystemVerilog decompiler\n";
    Printf.printf "  • Translation correctness\n";
    Printf.printf "  • Ground truth alignment\n";
    exit 0
  end else if List.length passed > 0 then begin
    Printf.printf "⚠️  PARTIAL SUCCESS: %d/%d pairs equivalent\n"
      (List.length passed) (List.length results);
    exit 1
  end else begin
    Printf.printf "❌ FAILED: No pairs verified equivalent\n";
    exit 1
  end
