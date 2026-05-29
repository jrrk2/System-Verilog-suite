(* SystemVerilog UART Regression Test Suite
 * Tests all UART-related modules via Verilator → Behavioral IR conversion
 *)

open Behavioral_ir

let uart_modules = [
  ("slib_clock_div", "sysver_tests/slib_clock_div.sv");
  ("slib_counter", "sysver_tests/slib_counter.sv");
  ("slib_edge_detect", "sysver_tests/slib_edge_detect.sv");
  ("slib_fifo", "sysver_tests/slib_fifo.sv");
  ("slib_input_filter", "sysver_tests/slib_input_filter.sv");
  ("slib_input_sync", "sysver_tests/slib_input_sync.sv");
  ("slib_mv_filter", "sysver_tests/slib_mv_filter.sv");
  ("uart_baudgen", "sysver_tests/uart_baudgen.sv");
  ("uart_interrupt", "sysver_tests/uart_interrupt.sv");
  ("uart_receiver", "sysver_tests/uart_receiver.sv");
  ("uart_transmitter", "sysver_tests/uart_transmitter.sv");
]

type test_result = {
  module_name: string;
  success: bool;
  signal_count: int;
  process_count: int;
  error_msg: string option;
}

let rec test_module (module_name, sv_file) =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Testing: %s\n" module_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  (* Step 1: Generate Verilator JSON if needed *)
  let json_file = Printf.sprintf "sysver_tests/obj_dir/V%s.tree.json" module_name in

  if not (Sys.file_exists json_file) then begin
    Printf.printf "\n[1/2] Generating Verilator JSON...\n";
    let cmd = Printf.sprintf
      "verilator --json-only --sv -Wno-fatal --top-module %s %s -Mdir sysver_tests/obj_dir 2>&1 | head -20"
      module_name sv_file in
    Printf.printf "  Running: verilator --json-only --sv --top-module %s ...\n" module_name;
    let status = Sys.command cmd in
    if status <> 0 then begin
      Printf.printf "  ❌ Verilator failed\n";
      { module_name; success = false; signal_count = 0; process_count = 0;
        error_msg = Some "Verilator generation failed" }
    end else if not (Sys.file_exists json_file) then begin
      Printf.printf "  ❌ Verilator JSON not created\n";
      { module_name; success = false; signal_count = 0; process_count = 0;
        error_msg = Some "Verilator JSON not created" }
    end else begin
      Printf.printf "  ✓ Verilator JSON generated\n";
      test_module (module_name, sv_file)  (* Retry *)
    end
  end else begin
    (* Step 2: Convert to Behavioral IR *)
    Printf.printf "\n[2/2] Converting to Behavioral IR...\n";

    try
      let bprog_opt = Verilator_to_behavioral.convert_verilator_json_to_behavioral json_file in

      match bprog_opt with
      | None ->
          Printf.printf "  ❌ Behavioral IR conversion failed\n";
          { module_name; success = false; signal_count = 0; process_count = 0;
            error_msg = Some "Behavioral IR conversion returned None" }

      | Some bprog ->
          (* Find the main module *)
          let main_module = List.find_opt (fun m -> m.name = module_name) bprog.modules in

          match main_module with
          | None ->
              Printf.printf "  ❌ Module '%s' not found in converted IR\n" module_name;
              Printf.printf "  Available modules: %s\n"
                (String.concat ", " (List.map (fun m -> m.name) bprog.modules));
              { module_name; success = false; signal_count = 0; process_count = 0;
                error_msg = Some "Module not found in converted IR" }

          | Some bmod ->
              Printf.printf "  ✓ Behavioral IR conversion successful\n\n";

              (* Print statistics *)
              Printf.printf "Module Statistics:\n";
              Printf.printf "  Signals:   %d\n" (List.length bmod.signals);
              Printf.printf "  Processes: %d\n" (List.length bmod.processes);
              Printf.printf "  Instances: %d\n" (List.length bmod.instances);

              (* Count signals by direction *)
              let inputs = List.filter (fun s -> s.direction = `Input) bmod.signals in
              let outputs = List.filter (fun s -> s.direction = `Output) bmod.signals in
              let internals = List.filter (fun s -> s.direction = `Internal) bmod.signals in

              Printf.printf "\n  Signal breakdown:\n";
              Printf.printf "    Inputs:   %d\n" (List.length inputs);
              Printf.printf "    Outputs:  %d\n" (List.length outputs);
              Printf.printf "    Internal: %d\n" (List.length internals);

              (* Count process types *)
              let sequential = List.filter (function BSequential _ -> true | _ -> false) bmod.processes in
              let combinational = List.filter (function BCombinational _ -> true | _ -> false) bmod.processes in

              Printf.printf "\n  Process breakdown:\n";
              Printf.printf "    Sequential:    %d\n" (List.length sequential);
              Printf.printf "    Combinational: %d\n" (List.length combinational);

              Printf.printf "\n  ✓ PASS\n";

              { module_name; success = true;
                signal_count = List.length bmod.signals;
                process_count = List.length bmod.processes;
                error_msg = None }

    with e ->
      Printf.printf "  ❌ Exception: %s\n" (Printexc.to_string e);
      { module_name; success = false; signal_count = 0; process_count = 0;
        error_msg = Some (Printexc.to_string e) }
  end

let () =
  Printf.printf "╔═══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  SystemVerilog UART Regression Test Suite                    ║\n";
  Printf.printf "║  Verilator → Behavioral IR Conversion                        ║\n";
  Printf.printf "╚═══════════════════════════════════════════════════════════════╝\n";
  Printf.printf "\nTesting %d UART-related modules\n" (List.length uart_modules);

  (* Run tests *)
  let results = List.map test_module uart_modules in

  (* Print summary *)
  Printf.printf "\n\n╔═══════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  TEST SUMMARY                                                 ║\n";
  Printf.printf "╚═══════════════════════════════════════════════════════════════╝\n\n";

  let passed = List.filter (fun r -> r.success) results in
  let failed = List.filter (fun r -> not r.success) results in

  Printf.printf "Results: %d/%d passed\n\n" (List.length passed) (List.length results);

  if List.length passed > 0 then begin
    Printf.printf "✓ PASSED (%d):\n" (List.length passed);
    List.iter (fun r ->
      Printf.printf "  ✓ %-25s  %2d signals, %2d processes\n"
        r.module_name r.signal_count r.process_count
    ) passed;
    Printf.printf "\n"
  end;

  if List.length failed > 0 then begin
    Printf.printf "❌ FAILED (%d):\n" (List.length failed);
    List.iter (fun r ->
      let err = match r.error_msg with Some e -> e | None -> "Unknown error" in
      Printf.printf "  ❌ %-25s  %s\n" r.module_name err
    ) failed;
    Printf.printf "\n"
  end;

  (* Exit with status code *)
  if List.length failed > 0 then
    exit 1
  else begin
    Printf.printf "═══════════════════════════════════════════════════════════════\n";
    Printf.printf "All tests passed! 🎉\n";
    Printf.printf "═══════════════════════════════════════════════════════════════\n";
    exit 0
  end
