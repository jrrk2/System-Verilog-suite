(* test_verify_yosys_verilator.ml - Verify Yosys and Verilator paths produce equivalent results *)

let create_test_verilog () =
  {|module test_adder (
  input [3:0] a,
  input [3:0] b,
  input cin,
  output [3:0] sum,
  output cout
);

  wire [4:0] full_sum;
  assign full_sum = a + b + cin;
  assign sum = full_sum[3:0];
  assign cout = full_sum[4];

endmodule
|}

let synthesize_with_yosys verilog_file rtlil_file =
  Printf.printf "=== Synthesizing with Yosys ===\n";
  let yosys_script = Printf.sprintf {|
read_verilog %s
proc
opt
write_rtlil %s
|} verilog_file rtlil_file in

  let script_file = "synth.ys" in
  let oc = open_out script_file in
  output_string oc yosys_script;
  close_out oc;

  let cmd = Printf.sprintf "yosys -q -s %s 2>&1" script_file in
  Printf.printf "Running: %s\n" cmd;

  match Sys.command cmd with
  | 0 ->
      Printf.printf "✓ Yosys synthesis complete\n";
      Sys.file_exists rtlil_file
  | code ->
      Printf.printf "✗ Yosys failed with code %d\n" code;
      false

let () =
  Printf.printf "=== Yosys ↔ Verilator Equivalence Verification ===\n\n";

  (* Step 1: Create test design *)
  Printf.printf "Step 1: Creating test design\n";
  let test_verilog = create_test_verilog () in
  let verilog_file = "test_adder.v" in
  let oc = open_out verilog_file in
  output_string oc test_verilog;
  close_out oc;
  Printf.printf "Created: %s\n\n" verilog_file;

  (* Step 2: Synthesize with Yosys to RTLIL *)
  let rtlil_file = "test_adder.il" in
  if not (synthesize_with_yosys verilog_file rtlil_file) then begin
    Printf.eprintf "Yosys synthesis failed, cannot proceed\n";
    exit 1
  end;
  Printf.printf "\n";

  (* Step 3: Parse RTLIL *)
  Printf.printf "Step 3: Parsing RTLIL\n";
  let rtlil_design = Sv_rtlil_reader.parse_rtlil_file rtlil_file in
  Printf.printf "Parsed RTLIL design\n";
  Sv_rtlil_reader.print_rtlil_summary rtlil_design;
  Printf.printf "\n";

  (* Step 4: Convert RTLIL to IR *)
  Printf.printf "Step 4: Converting RTLIL to IR\n";
  match Sv_rtlil_to_ir.rtlil_design_to_ir rtlil_design with
  | None ->
      Printf.eprintf "Failed to convert RTLIL to IR\n";
      exit 1
  | Some ir_yosys ->
      Printf.printf "RTLIL converted to IR\n";
      Sv_ir_verify.print_ir_stats ir_yosys;
      Printf.printf "\n";

      (* Step 5: Parse original Verilog and convert to IR *)
      Printf.printf "Step 5: Parsing original Verilog\n";
      (* Note: This would require full Verilog parsing and IR conversion *)
      (* For now, we'll demonstrate the RTLIL → IR path works *)
      Printf.printf "TODO: Parse behavioral Verilog to IR for comparison\n";
      Printf.printf "\n";

      (* Step 6: For demonstration, compare RTLIL IR with itself *)
      Printf.printf "Step 6: Verification (self-check demonstration)\n";
      Printf.printf "Verifying RTLIL IR against itself (should be equivalent):\n";
      let result = Sv_ir_verify.verify_ir_equivalence ir_yosys ir_yosys in
      if result then
        Printf.printf "\n✓ Verification infrastructure working correctly\n"
      else
        Printf.printf "\n✗ Self-check failed - verification infrastructure has issues\n";

      Printf.printf "\n=== Test Complete ===\n";
      Printf.printf "\nNext steps:\n";
      Printf.printf "1. Implement behavioral Verilog → IR path\n";
      Printf.printf "2. Run full Yosys vs Verilator comparison\n";
      Printf.printf "3. Test on more complex designs\n"
