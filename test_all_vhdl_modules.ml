(* Test all VHDL modules - comprehensive conversion test *)

open Vhdl_to_ir

let test_vhdl_file vhdl_file =
  Printf.printf "Testing: %s\n" (Filename.basename vhdl_file);
  Printf.printf "  Parsing... ";
  flush stdout;

  match convert_vhdl_file_to_ir vhdl_file with
  | None ->
      Printf.printf "✗ FAIL\n\n";
      (vhdl_file, false, None)
  | Some ir ->
      Printf.printf "✓\n";
      Printf.printf "  IR: %d inputs, %d outputs, %d nodes\n"
        (Hashtbl.length ir.ir_inputs)
        (Hashtbl.length ir.ir_outputs)
        (Hashtbl.length ir.ir_nodes);
      Printf.printf "  ✓ SUCCESS\n\n";
      (vhdl_file, true, Some ir)

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL Module Conversion - Complete Suite\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "\n";

  let vhdl_files = [
    "sysver_tests/apb_uart.vhd";
    "sysver_tests/slib_clock_div.vhd";
    "sysver_tests/slib_counter.vhd";
    "sysver_tests/slib_edge_detect.vhd";
    "sysver_tests/slib_fifo.vhd";
    "sysver_tests/slib_input_filter.vhd";
    "sysver_tests/slib_input_sync.vhd";
    "sysver_tests/slib_mv_filter.vhd";
    "sysver_tests/uart_baudgen.vhd";
    "sysver_tests/uart_interrupt.vhd";
    "sysver_tests/uart_receiver.vhd";
    "sysver_tests/uart_transmitter.vhd";
  ] in

  let results = List.map test_vhdl_file vhdl_files in

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let passed = List.filter (fun (_, success, _) -> success) results in
  let failed = List.filter (fun (_, success, _) -> not success) results in

  Printf.printf "Total: %d modules\n" (List.length results);
  Printf.printf "Passed: %d\n" (List.length passed);
  Printf.printf "Failed: %d\n\n" (List.length failed);

  if List.length failed > 0 then begin
    Printf.printf "Failed modules:\n";
    List.iter (fun (file, _, _) ->
      Printf.printf "  - %s\n" (Filename.basename file)
    ) failed;
    Printf.printf "\n"
  end;

  if List.length passed = List.length results then begin
    Printf.printf "✅ All VHDL modules converted successfully!\n";
    exit 0
  end else begin
    Printf.printf "⚠️  Some modules failed\n";
    exit 1
  end
