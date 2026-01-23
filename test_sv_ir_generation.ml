(* Test SystemVerilog IR generation for all 12 UART modules *)

open Sv_verible_to_ir
open Sv_ast

let show_ir_structure module_name ir =
  Printf.printf "  ✓ %s IR generated:\n" module_name;
  Printf.printf "    Inputs: %d, Outputs: %d, Nodes: %d\n"
    (Hashtbl.length ir.ir_inputs)
    (Hashtbl.length ir.ir_outputs)
    (Hashtbl.length ir.ir_nodes);

  (* Show node type breakdown *)
  let node_types = Hashtbl.create 10 in
  Hashtbl.iter (fun _ node ->
    let op_name = match node.node_op with
      | Add _ -> "Add"
      | Sub _ -> "Sub"
      | Mul _ -> "Mul"
      | Div _ -> "Div"
      | And _ -> "And"
      | Or _ -> "Or"
      | Xor _ -> "Xor"
      | Not _ -> "Not"
      | Mux _ -> "Mux"
      | Pmux _ -> "Pmux"
      | Compare _ -> "Compare"
      | Shift _ -> "Shift"
      | Concat _ -> "Concat"
      | Extract _ -> "Extract"
      | ZeroExtend _ -> "ZeroExtend"
      | SignExtend _ -> "SignExtend"
      | Register _ -> "Register"
    in
    let count = try Hashtbl.find node_types op_name with Not_found -> 0 in
    Hashtbl.replace node_types op_name (count + 1)
  ) ir.ir_nodes;

  Printf.printf "    Nodes: ";
  let first = ref true in
  Hashtbl.iter (fun op_name count ->
    if not !first then Printf.printf ", ";
    Printf.printf "%s:%d" op_name count;
    first := false
  ) node_types;
  Printf.printf "\n"

let test_sv_file sv_file module_name =
  Printf.printf "\nTesting: %s\n" module_name;
  Printf.printf "  File: %s\n" (Filename.basename sv_file);
  Printf.printf "  Converting to IR... ";
  flush stdout;

  match file_to_ir sv_file with
  | None ->
      Printf.printf "✗ FAILED\n";
      (module_name, false, None)
  | Some ir ->
      Printf.printf "✓\n";
      show_ir_structure module_name ir;
      (module_name, true, Some ir)

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  SystemVerilog IR Generation Test\n";
  Printf.printf "  All 12 APB UART Modules\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  let modules = [
    ("sysver_tests/apb_uart.sv", "apb_uart");
    ("sysver_tests/slib_clock_div.sv", "slib_clock_div");
    ("sysver_tests/slib_counter.sv", "slib_counter");
    ("sysver_tests/slib_edge_detect.sv", "slib_edge_detect");
    ("sysver_tests/slib_fifo.sv", "slib_fifo");
    ("sysver_tests/slib_input_filter.sv", "slib_input_filter");
    ("sysver_tests/slib_input_sync.sv", "slib_input_sync");
    ("sysver_tests/slib_mv_filter.sv", "slib_mv_filter");
    ("sysver_tests/uart_baudgen.sv", "uart_baudgen");
    ("sysver_tests/uart_interrupt.sv", "uart_interrupt");
    ("sysver_tests/uart_receiver.sv", "uart_receiver");
    ("sysver_tests/uart_transmitter.sv", "uart_transmitter");
  ] in

  let results = List.map (fun (file, name) -> test_sv_file file name) modules in

  (* Summary *)
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let success_count = List.filter (fun (_, success, _) -> success) results |> List.length in
  let total_count = List.length results in

  Printf.printf "Total modules: %d\n" total_count;
  Printf.printf "Successfully converted to IR: %d\n" success_count;
  Printf.printf "Failed: %d\n" (total_count - success_count);
  Printf.printf "\n";

  if success_count = total_count then begin
    Printf.printf "✅ All SystemVerilog modules successfully converted to IR!\n";
    Printf.printf "\nNext step: Compare with VHDL IR using Z3 verification\n";
    exit 0
  end else begin
    Printf.printf "⚠️  Some modules failed to convert\n";
    Printf.printf "\nFailed modules:\n";
    List.iter (fun (name, success, _) ->
      if not success then Printf.printf "  - %s\n" name
    ) results;
    exit 1
  end
