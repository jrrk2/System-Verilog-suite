(* Quick test: Compare Verible IR with Yosys and Verilator *)

open Sv_ir_verify

let () =
  let filename = "simple_add.v" in
  let module_name = "simple_add" in
  Printf.printf "=== 3-Way Verification: %s ===\n\n" filename;

  (* 1. Get Yosys IR *)
  Printf.printf "Step 1: Yosys RTLIL -> IR\n";
  let rtlil_file = "obj_dir/" ^ module_name ^ ".il" in
  let design = Sv_rtlil_reader.parse_rtlil_file rtlil_file in
  let yosys_ir = match Sv_rtlil_to_ir.rtlil_design_to_ir design with
    | None -> failwith "Failed to convert RTLIL to IR"
    | Some ir -> Printf.printf "✓ Yosys IR loaded\n"; ir
  in

  (* 2. Get Verilator IR *)
  Printf.printf "\nStep 2: Verilator JSON -> IR\n";
  let json_file = "obj_dir/V" ^ module_name ^ ".tree.json" in
  let json = match Yojson.Safe.from_file json_file with
    | `Assoc lst -> `Assoc (List.rev lst)
    | oth -> oth
  in
  let ast = Sv_parse.parse json in
  let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in
  Printf.printf "✓ Verilator IR loaded\n";

  (* 3. Get Verible IR *)
  Printf.printf "\nStep 3: Verible parser -> IR\n";
  let verible_ir = match Sv_verible_to_ir.file_to_ir filename with
    | Some ir -> Printf.printf "✓ Verible IR loaded\n\n"; ir
    | None -> failwith "Failed to create Verible IR"
  in

  (* Print IRs *)
  Printf.printf "=== IR Comparison ===\n\n";

  Printf.printf "Yosys IR:\n";
  Sv_opt_ir.print_ir yosys_ir;
  Printf.printf "\n";

  Printf.printf "Verilator IR:\n";
  Sv_opt_ir.print_ir verilator_ir;
  Printf.printf "\n";

  Printf.printf "Verible IR:\n";
  Sv_opt_ir.print_ir verible_ir;
  Printf.printf "\n";

  (* Verify equivalence *)
  Printf.printf "=== Z3 Verification ===\n\n";

  Printf.printf "1. Yosys vs Verilator:\n";
  let result1 = verify_ir_equivalence yosys_ir verilator_ir in
  if result1 then Printf.printf "   ✓ EQUIVALENT\n\n" else Printf.printf "   ✗ NOT EQUIVALENT\n\n";

  Printf.printf "2. Yosys vs Verible:\n";
  let result2 = verify_ir_equivalence yosys_ir verible_ir in
  if result2 then Printf.printf "   ✓ EQUIVALENT\n\n" else Printf.printf "   ✗ NOT EQUIVALENT\n\n";

  Printf.printf "3. Verilator vs Verible:\n";
  let result3 = verify_ir_equivalence verilator_ir verible_ir in
  if result3 then Printf.printf "   ✓ EQUIVALENT\n\n" else Printf.printf "   ✗ NOT EQUIVALENT\n\n";

  Printf.printf "Summary:\n";
  Printf.printf "  Yosys vs Verilator: %s\n" (if result1 then "✓" else "✗");
  Printf.printf "  Yosys vs Verible:   %s\n" (if result2 then "✓" else "✗");
  Printf.printf "  Verilator vs Verible: %s\n" (if result3 then "✓" else "✗");
  Printf.printf "\n✓ 3-way verification complete\n"
