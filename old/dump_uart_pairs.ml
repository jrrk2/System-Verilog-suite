(* Dump UART module IR pairs - focused on UART modules only *)

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

      (* Show file sizes *)
      let vhdl_lines =
        let ic = open_in vhdl_out in
        let rec count n =
          try ignore (input_line ic); count (n + 1)
          with End_of_file -> close_in ic; n
        in count 0
      in
      let sv_lines =
        let ic = open_in sv_out in
        let rec count n =
          try ignore (input_line ic); count (n + 1)
          with End_of_file -> close_in ic; n
        in count 0
      in

      Printf.printf "   VHDL IR: %d lines\n" vhdl_lines;
      Printf.printf "   SV IR:   %d lines\n" sv_lines;
      Printf.printf "   Compare: diff -u %s %s\n" vhdl_out sv_out;
      (module_name, true, None, vhdl_lines, sv_lines)
    end else begin
      Printf.printf "\n❌ FAILED: Dump error\n";
      (module_name, false, Some "Dump failed", 0, 0)
    end

  with e ->
    let error_msg = Printexc.to_string e in
    Printf.printf "\n❌ ERROR: %s\n" error_msg;
    (module_name, false, Some error_msg, 0, 0)

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  UART Module IR Dumper\n";
  Printf.printf "  VHDL vs SystemVerilog with Meaningful Names\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  (* Create output directory *)
  let output_dir = "uart_ir_dumps" in
  (try Unix.mkdir output_dir 0o755 with Unix.Unix_error _ -> ());
  Printf.printf "\nOutput directory: %s/\n" output_dir;

  (* UART module pairs *)
  let uart_pairs = [
    ("sysver_tests/apb_uart.vhd", "sysver_tests/apb_uart.sv", "apb_uart");
    ("sysver_tests/uart_baudgen.vhd", "sysver_tests/uart_baudgen.sv", "uart_baudgen");
    ("sysver_tests/uart_interrupt.vhd", "sysver_tests/uart_interrupt.sv", "uart_interrupt");
    ("sysver_tests/uart_receiver.vhd", "sysver_tests/uart_receiver.sv", "uart_receiver");
    ("sysver_tests/uart_transmitter.vhd", "sysver_tests/uart_transmitter.sv", "uart_transmitter");
  ] in

  let results = List.map (fun (vhdl, sv, name) ->
    dump_pair vhdl sv name output_dir
  ) uart_pairs in

  (* Summary *)
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let success_count = List.filter (fun (_, success, _, _, _) -> success) results |> List.length in
  let total_count = List.length results in

  Printf.printf "Total UART modules: %d\n" total_count;
  Printf.printf "Successfully dumped: %d\n" success_count;
  Printf.printf "Failed: %d\n\n" (total_count - success_count);

  if success_count > 0 then begin
    Printf.printf "┌─────────────────────┬────────────┬────────────┬──────────┐\n";
    Printf.printf "│ Module              │ VHDL Lines │  SV Lines  │   Ratio  │\n";
    Printf.printf "├─────────────────────┼────────────┼────────────┼──────────┤\n";
    List.iter (fun (name, success, _, vhdl_lines, sv_lines) ->
      if success then begin
        let ratio = if vhdl_lines > 0 then
          float_of_int sv_lines /. float_of_int vhdl_lines
        else 0.0 in
        Printf.printf "│ %-19s │ %10d │ %10d │ %7.2fx │\n"
          name vhdl_lines sv_lines ratio
      end
    ) results;
    Printf.printf "└─────────────────────┴────────────┴────────────┴──────────┘\n\n";

    Printf.printf "Files created in %s/:\n" output_dir;
    List.iter (fun (name, success, _, _, _) ->
      if success then begin
        Printf.printf "  • %s_vhdl_ir.v\n" name;
        Printf.printf "  • %s_sv_ir.v\n" name
      end
    ) results;

    Printf.printf "\nCompare individual modules:\n";
    List.iter (fun (name, success, _, _, _) ->
      if success then
        Printf.printf "  diff -u %s/%s_vhdl_ir.v %s/%s_sv_ir.v | less\n"
          output_dir name output_dir name
    ) results;

    exit 0
  end else begin
    Printf.printf "❌ All dumps failed\n";
    List.iter (fun (name, success, error, _, _) ->
      if not success then
        Printf.printf "  - %s: %s\n" name
          (match error with Some e -> e | None -> "unknown error")
    ) results;
    exit 1
  end
