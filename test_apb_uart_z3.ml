(* Test Z3 equivalence verification for APB UART - Verilator vs Verible *)

let () =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  APB UART - Z3 Equivalence Verification\n";
  Printf.printf "  Comparing Verilator and Verible parsing paths\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  try
    (* 1. Load Verilator IR *)
    Printf.printf "[1/3] Loading Verilator IR...\n";
    let json_file = "sysver_tests/obj_dir/Vapb_uart.tree.json" in
    if not (Sys.file_exists json_file) then
      failwith ("Verilator JSON not found: " ^ json_file);

    let json = Yojson.Safe.from_file json_file in
    let ast = Sv_parse.parse json in
    let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in
    Printf.printf "  ✓ Verilator IR loaded\n";
    Printf.printf "    Inputs: %d, Outputs: %d, Nodes: %d\n\n"
      (Hashtbl.length verilator_ir.Sv_ast.ir_inputs)
      (Hashtbl.length verilator_ir.Sv_ast.ir_outputs)
      (Hashtbl.length verilator_ir.Sv_ast.ir_nodes);

    (* 2. Load Verible IR *)
    Printf.printf "[2/3] Loading Verible IR...\n";
    let sv_file = "sysver_tests/apb_uart.sv" in
    if not (Sys.file_exists sv_file) then
      failwith ("SystemVerilog file not found: " ^ sv_file);

    match Sv_verible_to_ir.file_to_ir sv_file with
    | None -> failwith "Verible IR conversion failed"
    | Some verible_ir ->
        Printf.printf "  ✓ Verible IR loaded\n";
        Printf.printf "    Inputs: %d, Outputs: %d, Nodes: %d\n\n"
          (Hashtbl.length verible_ir.Sv_ast.ir_inputs)
          (Hashtbl.length verible_ir.Sv_ast.ir_outputs)
          (Hashtbl.length verible_ir.Sv_ast.ir_nodes);

        (* 3. Run Z3 verification *)
        Printf.printf "[3/3] Running Z3 equivalence verification...\n";
        Printf.printf "  (This may take a while for large designs)\n\n";

        let start_time = Unix.gettimeofday () in
        let result = Sv_ir_verify.verify_ir_equivalence verilator_ir verible_ir in
        let elapsed = Unix.gettimeofday () -. start_time in

        Printf.printf "\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  VERIFICATION RESULT: %s\n" (if result then "✅ EQUIVALENT" else "❌ NOT EQUIVALENT");
        Printf.printf "  Time taken: %.2f seconds\n" elapsed;
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        if result then begin
          Printf.printf "✅ SUCCESS: Verilator and Verible produce equivalent IRs!\n";
          Printf.printf "\nThis means:\n";
          Printf.printf "  • Both parsers extract the same logic\n";
          Printf.printf "  • Async reset flip-flops are correctly handled\n";
          Printf.printf "  • All outputs are mathematically equivalent\n";
          Printf.printf "  • Z3 proved equivalence for all possible inputs\n";
          exit 0
        end else begin
          Printf.printf "❌ FAILED: IRs are not equivalent\n";
          Printf.printf "\nPossible reasons:\n";
          Printf.printf "  • Different signal mappings\n";
          Printf.printf "  • Missing logic in one path\n";
          Printf.printf "  • Width mismatches\n";
          Printf.printf "  • Constant folding differences\n";
          exit 1
        end

  with e ->
    Printf.eprintf "\n❌ ERROR: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    exit 2
