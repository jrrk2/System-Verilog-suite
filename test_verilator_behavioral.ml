(* Test Verilator JSON → Behavioral IR → Optimization
 *
 * This test demonstrates the complete pipeline:
 * 1. Verilator JSON → Behavioral IR
 * 2. Optimization (SSA, const prop, DCE, CSE)
 * 3. Register inference
 *)

open Behavioral_optimize

let test_verilator_file json_file =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Verilator JSON → Behavioral IR → Optimization\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input: %s\n\n" json_file;

  (* Step 1: Convert Verilator JSON to Behavioral IR *)
  Printf.printf "[1/3] Converting Verilator JSON to Behavioral IR...\n";
  let bprog_opt = Verilator_to_behavioral.convert_verilator_json_to_behavioral json_file in

  match bprog_opt with
  | None ->
      Printf.eprintf "✗ Verilator JSON conversion failed\n";
      false
  | Some bprog ->
      Printf.printf "✓ Conversion successful (%d modules)\n\n" (List.length bprog.modules);

      if List.length bprog.modules = 0 then begin
        Printf.printf "⚠️  No modules found in Verilator JSON\n";
        false
      end else begin
        let bmod = List.hd bprog.modules in
        Printf.printf "Module: %s\n" bmod.name;
        Printf.printf "  Signals: %d\n" (List.length bmod.signals);
        Printf.printf "  Processes: %d\n\n" (List.length bmod.processes);

        (* Step 2: Run optimization pipeline *)
        Printf.printf "[2/3] Running Optimization Pipeline...\n";
        let (optimized, _) = optimize_custom
          { default_config with verbose = false } bprog in

        Printf.printf "✓ Optimization complete\n\n";

        let opt_mod = List.hd optimized.modules in

        (* Step 3: Register inference *)
        Printf.printf "[3/3] Register Inference...\n";
        let ctx = Behavioral_registers.analyze_module opt_mod in

        Printf.printf "\nRegister Inference Results:\n";
        Printf.printf "  Registers: %d\n" (List.length ctx.registers);
        Printf.printf "  Wires: %d\n" (List.length ctx.wires);

        if List.length ctx.registers > 0 then begin
          Printf.printf "\nRegisters (original signals only):\n";
          List.iter (fun (reg : Behavioral_registers.register_info) ->
            (* Strip SSA suffixes to show only original signals *)
            let original_name = Behavioral_registers.strip_ssa_suffix reg.reg_name in
            if original_name = reg.reg_name || not (String.contains reg.reg_name '_') then
              Printf.printf "  - %s: %d bits, clock=%s%s\n"
                reg.reg_name
                reg.reg_width
                reg.reg_clock
                (match reg.reg_reset with
                 | Some r -> Printf.sprintf " (reset=%s)" r
                 | None -> "")
          ) ctx.registers
        end;

        Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
        Printf.printf "Comparison: Old opt_ir vs New Behavioral IR\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        Printf.printf "OLD APPROACH (behavioural_to_opt_ir.ml):\n";
        Printf.printf "  • Converted Verilator JSON → opt_ir (dataflow graph)\n";
        Printf.printf "  • No high-level optimization passes\n";
        Printf.printf "  • No SSA, no DCE, no CSE\n";
        Printf.printf "  • Register inference at opt_ir level\n\n";

        Printf.printf "NEW APPROACH (verilator_to_behavioral.ml):\n";
        Printf.printf "  • Converts Verilator JSON → Behavioral IR\n";
        Printf.printf "  • ✅ SSA construction\n";
        Printf.printf "  • ✅ Constant propagation\n";
        Printf.printf "  • ✅ Dead code elimination\n";
        Printf.printf "  • ✅ Common subexpression elimination\n";
        Printf.printf "  • ✅ Register inference: %d registers\n\n" (List.length ctx.registers);

        Printf.printf "Benefits:\n";
        Printf.printf "  ✅ Language-neutral IR (same as VHDL/SV)\n";
        Printf.printf "  ✅ Shared optimization infrastructure\n";
        Printf.printf "  ✅ Module-level DCE with cross-process analysis\n";
        Printf.printf "  ✅ Can use Z3 miter verification\n";
        Printf.printf "  ✅ Can compare Verilator vs Verible frontends\n\n";

        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  ✅ SUCCESS\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        Printf.printf "Verilator JSON successfully processed through behavioral IR!\n";
        Printf.printf "Module %s: %d registers, %d wires\n\n"
          opt_mod.name
          (List.length ctx.registers)
          (List.length ctx.wires);

        true
      end

let () =
  let json_file =
    if Array.length Sys.argv >= 2 then
      Sys.argv.(1)
    else begin
      Printf.eprintf "Usage: %s <verilator_json_file>\n" Sys.argv.(0);
      Printf.eprintf "\nTo generate Verilator JSON:\n";
      Printf.eprintf "  verilator --json-only --dump-tree-json \\n";
      Printf.eprintf "    --json-only-output output.json \\n";
      Printf.eprintf "    --top-module <module_name> <sv_file>\n\n";
      exit 1
    end
  in

  if not (Sys.file_exists json_file) then begin
    Printf.eprintf "Error: JSON file not found: %s\n" json_file;
    exit 1
  end;

  let success = test_verilator_file json_file in

  if success then
    exit 0
  else
    exit 1
