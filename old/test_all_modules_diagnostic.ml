(* Diagnostic test to examine ALL modules in Verilator JSON conversion
 * This will help identify why std_icache and other modules show 0 registers
 *)

open Behavioral_ir

let test_all_modules json_file =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Diagnostic: Examining All Modules in Verilator JSON\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input: %s\n\n" json_file;

  (* Convert Verilator JSON to Behavioral IR *)
  Printf.printf "Converting Verilator JSON to Behavioral IR...\n";
  let bprog_opt = Verilator_to_behavioral.convert_verilator_json_to_behavioral json_file in

  match bprog_opt with
  | None ->
      Printf.eprintf "✗ Conversion failed\n";
      false
  | Some bprog ->
      Printf.printf "✓ Conversion successful\n";
      Printf.printf "  Total modules extracted: %d\n\n" (List.length bprog.modules);

      Printf.printf "═══════════════════════════════════════════════════════════════\n";
      Printf.printf "  Module-by-Module Analysis\n";
      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

      (* Analyze each module *)
      List.iter (fun (bmod : bmodule) ->
        Printf.printf "─────────────────────────────────────────────────────────────\n";
        Printf.printf "Module: %s\n" bmod.name;
        Printf.printf "  Signals: %d\n" (List.length bmod.signals);
        Printf.printf "  Processes: %d\n" (List.length bmod.processes);
        Printf.printf "  Instances: %d\n" (List.length bmod.instances);

        (* Show first 10 signals *)
        if List.length bmod.signals > 0 then begin
          Printf.printf "\n  First few signals:\n";
          List.iteri (fun i (signal_info : bsignal) ->
            if i < 10 then
              Printf.printf "    [%d] %s (%s, %s)\n"
                i
                signal_info.name
                (match signal_info.stype with
                 | BInt { width; _ } -> Printf.sprintf "%d-bit" width
                 | _ -> "unknown")
                (match signal_info.direction with
                 | `Input -> "input"
                 | `Output -> "output"
                 | `Internal -> "internal")
          ) bmod.signals;

          (* Count _q signals *)
          let q_count = List.fold_left (fun acc (signal_info : bsignal) ->
            if String.contains signal_info.name 'q' &&
               (String.ends_with ~suffix:"_q" signal_info.name ||
                String.ends_with ~suffix:"_Q" signal_info.name) then
              acc + 1
            else acc
          ) 0 bmod.signals in

          if q_count > 0 then
            Printf.printf "  ⚠️  Contains %d signals ending in _q (potential registers)\n" q_count
        end;

        (* Show process types *)
        if List.length bmod.processes > 0 then begin
          Printf.printf "\n  Processes:\n";
          List.iteri (fun i proc ->
            match proc with
            | BSequential { name; clock; clock_edge; reset; body; _ } ->
                Printf.printf "    [%d] Sequential: %s\n" i name;
                Printf.printf "        Clock: %s (%s)\n"
                  clock
                  (match clock_edge with `Pos -> "posedge" | `Neg -> "negedge");
                (match reset with
                 | Some r -> Printf.printf "        Reset: %s\n" r
                 | None -> ());
                Printf.printf "        Body: %d statements\n" (List.length body)
            | BCombinational { name; sensitivity; body } ->
                Printf.printf "    [%d] Combinational: %s\n" i name;
                Printf.printf "        Sensitivity: %d items\n" (List.length sensitivity);
                Printf.printf "        Body: %d statements\n" (List.length body)
          ) bmod.processes
        end;

        (* Highlight modules with 0/0 (signals/processes) *)
        if List.length bmod.signals = 0 && List.length bmod.processes = 0 then
          Printf.printf "\n  ❌ EMPTY MODULE (0 signals, 0 processes)\n";

        Printf.printf "\n"
      ) bprog.modules;

      Printf.printf "═══════════════════════════════════════════════════════════════\n";
      Printf.printf "  Summary Statistics\n";
      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

      let total_signals = List.fold_left (fun acc m -> acc + List.length m.signals) 0 bprog.modules in
      let total_processes = List.fold_left (fun acc m -> acc + List.length m.processes) 0 bprog.modules in
      let empty_modules = List.fold_left (fun acc m ->
        if List.length m.signals = 0 && List.length m.processes = 0 then acc + 1 else acc
      ) 0 bprog.modules in
      let seq_processes = List.fold_left (fun acc m ->
        acc + List.length (List.filter (function
          | BSequential _ -> true
          | _ -> false
        ) m.processes)
      ) 0 bprog.modules in
      let comb_processes = List.fold_left (fun acc m ->
        acc + List.length (List.filter (function
          | BCombinational _ -> true
          | _ -> false
        ) m.processes)
      ) 0 bprog.modules in

      Printf.printf "Modules:             %d\n" (List.length bprog.modules);
      Printf.printf "Empty modules:       %d (%.1f%%)\n"
        empty_modules
        (float_of_int empty_modules /. float_of_int (List.length bprog.modules) *. 100.0);
      Printf.printf "Total signals:       %d (avg %.1f per module)\n"
        total_signals
        (float_of_int total_signals /. float_of_int (List.length bprog.modules));
      Printf.printf "Total processes:     %d (avg %.1f per module)\n"
        total_processes
        (float_of_int total_processes /. float_of_int (List.length bprog.modules));
      Printf.printf "  - Sequential:      %d\n" seq_processes;
      Printf.printf "  - Combinational:   %d\n" comb_processes;

      Printf.printf "\n";

      (* Check if std_icache exists *)
      let std_icache = List.find_opt (fun m -> m.name = "std_icache") bprog.modules in
      (match std_icache with
      | Some m ->
          Printf.printf "✓ std_icache found in extracted modules\n";
          Printf.printf "  Signals: %d\n" (List.length m.signals);
          Printf.printf "  Processes: %d\n" (List.length m.processes);
          if List.length m.signals = 0 then
            Printf.printf "  ❌ std_icache has 0 signals (expected 7+ _q registers)\n"
      | None ->
          Printf.printf "❌ std_icache NOT found in extracted modules\n";
          Printf.printf "   Available modules:\n";
          List.iter (fun m ->
            if String.contains m.name 'c' && String.contains m.name 'a' then
              Printf.printf "     - %s\n" m.name
          ) bprog.modules);

      Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
      true

let () =
  let json_file =
    if Array.length Sys.argv >= 2 then
      Sys.argv.(1)
    else begin
      Printf.eprintf "Usage: %s <verilator_json_file>\n" Sys.argv.(0);
      exit 1
    end
  in

  if not (Sys.file_exists json_file) then begin
    Printf.eprintf "Error: JSON file not found: %s\n" json_file;
    exit 1
  end;

  let success = test_all_modules json_file in
  if success then exit 0 else exit 1
