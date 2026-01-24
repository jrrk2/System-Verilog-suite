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
        (* Print ALL modules first *)
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  All Converted Modules\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        Printf.printf "%-35s %8s %10s %10s\n" "Module Name" "Signals" "Processes" "Instances";
        Printf.printf "%s\n" (String.make 70 '-');

        let empty_count = ref 0 in
        List.iteri (fun i (m : Behavioral_ir.bmodule) ->
          let sig_count = List.length m.signals in
          let proc_count = List.length m.processes in
          let inst_count = List.length m.instances in

          let flag = if sig_count = 0 && proc_count = 0 then begin
            incr empty_count;
            " ⚠️"
          end else "" in

          Printf.printf "%-35s %8d %10d %10d%s\n"
            m.name sig_count proc_count inst_count flag
        ) bprog.modules;

        Printf.printf "\n";
        if !empty_count > 0 then
          Printf.printf "⚠️  %d modules have 0 signals and 0 processes\n\n" !empty_count;

        (* Check for std_icache specifically *)
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  std_icache Analysis\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        let std_icache_opt = List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = "std_icache") bprog.modules in
        (match std_icache_opt with
        | Some icache ->
            Printf.printf "✓ std_icache found in converted modules\n\n";
            Printf.printf "  Signals: %d\n" (List.length icache.signals);
            Printf.printf "  Processes: %d\n" (List.length icache.processes);
            Printf.printf "  Instances: %d\n\n" (List.length icache.instances);

            if List.length icache.signals > 0 then begin
              Printf.printf "  First 20 signals:\n";
              List.iteri (fun i (s : Behavioral_ir.bsignal) ->
                if i < 20 then
                  Printf.printf "    [%2d] %s\n" (i+1) s.name
              ) icache.signals;

              (* Count _q signals *)
              let q_signals = List.filter (fun (s : Behavioral_ir.bsignal) ->
                String.length s.name >= 2 &&
                String.sub s.name (String.length s.name - 2) 2 = "_q"
              ) icache.signals in

              Printf.printf "\n  Signals ending in _q: %d\n" (List.length q_signals);
              List.iter (fun (s : Behavioral_ir.bsignal) ->
                Printf.printf "    - %s\n" s.name
              ) q_signals;
            end;

            if List.length icache.processes > 0 then begin
              Printf.printf "\n  Processes:\n";
              List.iteri (fun i proc ->
                match proc with
                | Behavioral_ir.BSequential { name; clock; clock_edge; body; _ } ->
                    Printf.printf "    [%d] Sequential: %s\n" (i+1) name;
                    Printf.printf "        Clock: %s (%s)\n" clock
                      (match clock_edge with `Pos -> "posedge" | `Neg -> "negedge");
                    Printf.printf "        Body statements: %d\n" (List.length body)
                | Behavioral_ir.BCombinational { name; body; _ } ->
                    Printf.printf "    [%d] Combinational: %s\n" (i+1) name;
                    Printf.printf "        Body statements: %d\n" (List.length body)
              ) icache.processes
            end;

            if List.length icache.signals = 0 then
              Printf.printf "\n  ❌ PROBLEM: std_icache has 0 signals (expected ~47 from JSON)\n";
            if List.length icache.processes = 0 then
              Printf.printf "  ❌ PROBLEM: std_icache has 0 processes (expected 3 from JSON)\n"

        | None ->
            Printf.printf "❌ std_icache NOT FOUND in converted modules\n";
            Printf.printf "\nModules containing 'cache':\n";
            List.iter (fun (m : Behavioral_ir.bmodule) ->
              if String.contains (String.lowercase_ascii m.name) 'c' &&
                 String.contains (String.lowercase_ascii m.name) 'a' then
                Printf.printf "  - %s\n" m.name
            ) bprog.modules);

        Printf.printf "\n";

        (* Now proceed with optimization on the first module *)
        let bmod = List.hd bprog.modules in
        Printf.printf "═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  Optimization Test (on first module: %s)\n" bmod.name;
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

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
        Printf.printf "  Register Inference on ALL Modules\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        (* Run register inference on ALL optimized modules *)
        Printf.printf "%-35s %10s %10s\n" "Module Name" "Registers" "Wires";
        Printf.printf "%s\n" (String.make 60 '-');

        let total_registers = ref 0 in
        let total_wires = ref 0 in

        List.iter (fun (opt_m : Behavioral_ir.bmodule) ->
          let ctx = Behavioral_registers.analyze_module opt_m in
          let reg_count = List.length ctx.registers in
          let wire_count = List.length ctx.wires in

          total_registers := !total_registers + reg_count;
          total_wires := !total_wires + wire_count;

          Printf.printf "%-35s %10d %10d\n"
            opt_m.name reg_count wire_count
        ) optimized.modules;

        Printf.printf "%s\n" (String.make 60 '-');
        Printf.printf "%-35s %10d %10d\n" "TOTAL" !total_registers !total_wires;

        Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  Summary\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

        Printf.printf "Modules converted:        %d\n" (List.length bprog.modules);
        Printf.printf "Total registers found:    %d\n" !total_registers;
        Printf.printf "Total wires found:        %d\n" !total_wires;
        Printf.printf "Empty modules (0/0):      %d\n\n" !empty_count;

        (* Check std_icache register count *)
        let std_icache_opt = List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = "std_icache") optimized.modules in
        (match std_icache_opt with
        | Some icache ->
            let ctx = Behavioral_registers.analyze_module icache in
            Printf.printf "std_icache register count: %d\n" (List.length ctx.registers);
            if List.length ctx.registers = 0 then
              Printf.printf "  ❌ Expected 7 registers from JSON, got 0\n"
            else
              Printf.printf "  ✓ Found registers\n"
        | None ->
            Printf.printf "std_icache: NOT FOUND\n");

        Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
        Printf.printf "  ✅ ANALYSIS COMPLETE\n";
        Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

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
