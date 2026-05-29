(* Dump IR pairs to Verilog for comparison *)

open Vhdl_to_ir
open Sv_verible_to_ir

let dump_ir_to_verilog ir filename =
  try
    (* Convert IR to behavioral Verilog *)
    let sv_code = Opt_ir_to_behavioral.convert ~verbose:false ir in
    let oc = open_out filename in
    output_string oc sv_code;
    close_out oc;
    Printf.printf "  ✓ Wrote %s\n" filename;
    true
  with e ->
    Printf.printf "  ✗ Failed to write %s: %s\n" filename (Printexc.to_string e);
    false

let dump_pair vhdl_file sv_file module_name output_dir =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Module: %s\n" module_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  try
    (* Convert VHDL to IR *)
    Printf.printf "[1/4] Converting VHDL to IR... ";
    flush stdout;
    let vhdl_ir = match convert_vhdl_file_to_ir vhdl_file with
      | Some ir ->
          Printf.printf "✓ (%d nodes)\n" (Hashtbl.length ir.Sv_ast.ir_nodes);
          ir
      | None -> raise (Failure "VHDL conversion failed")
    in

    (* Convert SystemVerilog to IR *)
    Printf.printf "[2/4] Converting SystemVerilog to IR... ";
    flush stdout;
    let sv_ir = match file_to_ir sv_file with
      | Some ir ->
          Printf.printf "✓ (%d nodes)\n" (Hashtbl.length ir.Sv_ast.ir_nodes);
          ir
      | None -> raise (Failure "SV conversion failed")
    in

    (* Dump VHDL IR to Verilog *)
    Printf.printf "[3/4] Dumping VHDL IR to Verilog... ";
    let vhdl_out = Filename.concat output_dir (module_name ^ "_vhdl_ir.v") in
    let vhdl_ok = dump_ir_to_verilog vhdl_ir vhdl_out in

    (* Dump SV IR to Verilog *)
    Printf.printf "[4/4] Dumping SV IR to Verilog... ";
    let sv_out = Filename.concat output_dir (module_name ^ "_sv_ir.v") in
    let sv_ok = dump_ir_to_verilog sv_ir sv_out in

    if vhdl_ok && sv_ok then begin
      Printf.printf "\n✅ SUCCESS: Both IRs dumped\n";
      Printf.printf "   Compare with: diff %s %s\n" vhdl_out sv_out;
      (module_name, true, None)
    end else begin
      Printf.printf "\n❌ FAILED: Dump error\n";
      (module_name, false, Some "Dump failed")
    end

  with e ->
    let error_msg = Printexc.to_string e in
    Printf.printf "\n❌ ERROR: %s\n" error_msg;
    (module_name, false, Some error_msg)

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  IR Tree Dumper - VHDL vs SystemVerilog\n";
  Printf.printf "  Converts both to IR, then dumps back to BEHAVIORAL Verilog\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  (* Create output directory *)
  let output_dir = "ir_dumps" in
  (try Unix.mkdir output_dir 0o755 with Unix.Unix_error _ -> ());
  Printf.printf "\nOutput directory: %s/\n" output_dir;

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
    dump_pair vhdl sv name output_dir
  ) test_pairs in

  (* Summary *)
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let success_count = List.filter (fun (_, success, _) -> success) results |> List.length in
  let total_count = List.length results in

  Printf.printf "Total pairs: %d\n" total_count;
  Printf.printf "Successfully dumped: %d\n" success_count;
  Printf.printf "Failed: %d\n\n" (total_count - success_count);

  if success_count > 0 then begin
    Printf.printf "✅ IR dumps saved to: %s/\n\n" output_dir;
    Printf.printf "Files created:\n";
    List.iter (fun (name, success, _) ->
      if success then begin
        Printf.printf "  • %s_vhdl_ir.v  (from VHDL)\n" name;
        Printf.printf "  • %s_sv_ir.v    (from SystemVerilog)\n" name
      end
    ) results;
    Printf.printf "\nCompare with:\n";
    Printf.printf "  diff ir_dumps/<module>_vhdl_ir.v ir_dumps/<module>_sv_ir.v\n\n";
    exit 0
  end else begin
    Printf.printf "❌ All dumps failed\n";
    List.iter (fun (name, success, error) ->
      if not success then
        Printf.printf "  - %s: %s\n" name
          (match error with Some e -> e | None -> "unknown error")
    ) results;
    exit 1
  end
