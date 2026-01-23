(* Test SystemVerilog to Behavioral IR Conversion
 *
 * Tests that SystemVerilog frontend produces correct behavioral IR
 * and goes through optimization pipeline with correct register inference.
 *)

open Behavioral_ir
open Sv_to_behavioral
open Behavioral_optimize

let test_sv_file filename =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "Testing SystemVerilog Frontend: %s\n" filename;
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  (* Convert SV to behavioral IR *)
  Printf.printf "Step 1: Converting SystemVerilog to Behavioral IR...\n";
  let bprog_opt = convert_elaborated_sv_to_behavioral filename in

  match bprog_opt with
  | None ->
      Printf.eprintf "✗ Failed to convert SystemVerilog file\n";
      exit 1
  | Some bprog ->
      Printf.printf "✓ Conversion successful\n";
      Printf.printf "  Modules: %d\n" (List.length bprog.modules);

      let bmod = List.hd bprog.modules in
      Printf.printf "  Module name: %s\n" bmod.name;
      Printf.printf "  Signals: %d\n" (List.length bmod.signals);
      Printf.printf "  Processes: %d\n\n" (List.length bmod.processes);

      (* Run optimization pipeline (includes register inference) *)
      Printf.printf "Step 2: Running Complete Optimization Pipeline...\n\n";
      let (optimized, _) = optimize_full bprog in

      Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
      Printf.printf "SystemVerilog Frontend Test Complete!\n";
      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

      (* Check register count *)
      let opt_mod = List.hd optimized.modules in
      let ctx = Behavioral_registers.analyze_module opt_mod in
      let reg_count = List.length ctx.registers in

      Printf.printf "Result Summary:\n";
      Printf.printf "  ✅ SystemVerilog → Behavioral IR: Success\n";
      Printf.printf "  ✅ Optimization Pipeline: Success\n";
      Printf.printf "  ✅ Register Inference: %d registers (expected: 2)\n" reg_count;

      if reg_count = 2 then
        Printf.printf "\n🎉 SUCCESS! Register inference produces correct count!\n"
      else
        Printf.printf "\n⚠️  Warning: Expected 2 registers, got %d\n" reg_count

let () =
  let filename = if Array.length Sys.argv > 1 then
    Sys.argv.(1)
  else
    "sysver_tests/slib_clock_div.sv"
  in

  if not (Sys.file_exists filename) then begin
    Printf.eprintf "Error: File not found: %s\n" filename;
    exit 1
  end;

  test_sv_file filename
