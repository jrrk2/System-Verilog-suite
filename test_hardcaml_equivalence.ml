(* Test Equivalence via HardCaml Circuit Generation
 *
 * This approach converts:
 *   VHDL → Behavioral IR → HardCaml Circuit → Verilog
 *   SV   → Behavioral IR → HardCaml Circuit → Verilog
 *
 * Benefits:
 * - HardCaml's type system enforces correct widths
 * - HardCaml normalizes away optimization differences
 * - Can use commercial equivalence checkers on generated Verilog
 * - Mature, battle-tested library reduces encoding bugs
 *)

open Behavioral_ir
open Behavioral_optimize

let () =
  let (vhdl_file, sv_file) =
    if Array.length Sys.argv >= 3 then
      (Sys.argv.(1), Sys.argv.(2))
    else
      ("sysver_tests/slib_input_sync.vhd", "sysver_tests/slib_input_sync.sv")
  in

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  HardCaml-Based Equivalence Verification\n";
  Printf.printf "  Behavioral IR → HardCaml Circuit → Verilog\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input Files:\n";
  Printf.printf "  VHDL: %s\n" vhdl_file;
  Printf.printf "  SV:   %s\n\n" sv_file;

  (* Convert VHDL *)
  Printf.printf "[1/6] Converting VHDL to Behavioral IR...\n";
  let vhdl_prog_opt = Vhdl_to_behavioral.convert_vhdl_file_to_behavioral vhdl_file in

  match vhdl_prog_opt with
  | None ->
      Printf.eprintf "✗ VHDL conversion failed\n";
      exit 1
  | Some vhdl_prog ->
      Printf.printf "✓ VHDL conversion successful\n\n";

      (* Convert SystemVerilog *)
      Printf.printf "[2/6] Converting SystemVerilog to Behavioral IR...\n";
      let sv_prog_opt = Sv_to_behavioral.convert_elaborated_sv_to_behavioral sv_file in

      match sv_prog_opt with
      | None ->
          Printf.eprintf "✗ SystemVerilog conversion failed\n";
          exit 1
      | Some sv_prog ->
          Printf.printf "✓ SystemVerilog conversion successful\n\n";

          (* Optimize both *)
          Printf.printf "[3/6] Optimizing both designs...\n";
          let (vhdl_opt, _) = optimize_custom
            { default_config with verbose = false } vhdl_prog in
          let (sv_opt, _) = optimize_custom
            { default_config with verbose = false } sv_prog in
          Printf.printf "✓ Optimization complete\n\n";

          (* Convert to HardCaml *)
          Printf.printf "[4/6] Converting VHDL Behavioral IR to HardCaml...\n";
          (match Behavioral_to_hardcaml.convert_to_hardcaml vhdl_opt with
           | None ->
               Printf.eprintf "✗ VHDL to HardCaml conversion failed\n";
               exit 1
           | Some (vhdl_mod, vhdl_inputs, vhdl_outputs) ->
               Printf.printf "✓ VHDL HardCaml circuit created\n";
               Printf.printf "  Module: %s\n" vhdl_mod.name;
               Printf.printf "  Inputs: %d, Outputs: %d\n\n"
                 (List.length vhdl_inputs) (List.length vhdl_outputs);

               Printf.printf "[5/6] Converting SV Behavioral IR to HardCaml...\n";
               (match Behavioral_to_hardcaml.convert_to_hardcaml sv_opt with
                | None ->
                    Printf.eprintf "✗ SV to HardCaml conversion failed\n";
                    exit 1
                | Some (sv_mod, sv_inputs, sv_outputs) ->
                    Printf.printf "✓ SV HardCaml circuit created\n";
                    Printf.printf "  Module: %s\n" sv_mod.name;
                    Printf.printf "  Inputs: %d, Outputs: %d\n\n"
                      (List.length sv_inputs) (List.length sv_outputs);

                    (* Compare interfaces *)
                    Printf.printf "[6/6] Comparing HardCaml circuits...\n\n";

                    (* Sort ports by name for comparison (order may differ) *)
                    let sort_ports ports = List.sort (fun (n1,_) (n2,_) -> String.compare n1 n2) ports in
                    let vhdl_inputs_sorted = sort_ports vhdl_inputs in
                    let sv_inputs_sorted = sort_ports sv_inputs in
                    let vhdl_outputs_sorted = sort_ports vhdl_outputs in
                    let sv_outputs_sorted = sort_ports sv_outputs in

                    (* Check input ports match (ignoring order) *)
                    let inputs_match =
                      List.length vhdl_inputs = List.length sv_inputs &&
                      List.for_all2 (fun (vn, vw) (sn, sw) ->
                        vn = sn && vw = sw
                      ) vhdl_inputs_sorted sv_inputs_sorted
                    in

                    (* Check output ports match (ignoring order) *)
                    let outputs_match =
                      List.length vhdl_outputs = List.length sv_outputs &&
                      List.for_all2 (fun (vn, vw) (sn, sw) ->
                        vn = sn && vw = sw
                      ) vhdl_outputs_sorted sv_outputs_sorted
                    in

                    if inputs_match && outputs_match then begin
                      Printf.printf "✅ INTERFACE MATCH\n\n";
                      Printf.printf "Inputs (%d):\n" (List.length vhdl_inputs);
                      List.iter (fun (name, width) ->
                        Printf.printf "  %s: %d bits\n" name width
                      ) vhdl_inputs;
                      Printf.printf "\nOutputs (%d):\n" (List.length vhdl_outputs);
                      List.iter (fun (name, width) ->
                        Printf.printf "  %s: %d bits\n" name width
                      ) vhdl_outputs;
                      Printf.printf "\n";

                      Printf.printf "═══════════════════════════════════════════════════════════════\n";
                      Printf.printf "  ✅ SUCCESS: Interfaces Match\n";
                      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
                      Printf.printf "Both designs have compatible HardCaml circuits.\n";
                      Printf.printf "HardCaml's type system verified width consistency. ✅\n\n";

                      Printf.printf "Next steps for full equivalence:\n";
                      Printf.printf "  1. Generate Verilog from both HardCaml circuits\n";
                      Printf.printf "  2. Run commercial equivalence checker (Formality/Conformal)\n";
                      Printf.printf "  3. Or: Simulate both circuits and compare waveforms\n\n";

                      exit 0
                    end else begin
                      Printf.printf "❌ INTERFACE MISMATCH\n\n";
                      if not inputs_match then begin
                        Printf.printf "Input mismatch:\n";
                        Printf.printf "  VHDL: %s\n"
                          (String.concat ", " (List.map (fun (n,w) ->
                            Printf.sprintf "%s:%d" n w) vhdl_inputs));
                        Printf.printf "  SV:   %s\n"
                          (String.concat ", " (List.map (fun (n,w) ->
                            Printf.sprintf "%s:%d" n w) sv_inputs));
                      end;
                      if not outputs_match then begin
                        Printf.printf "Output mismatch:\n";
                        Printf.printf "  VHDL: %s\n"
                          (String.concat ", " (List.map (fun (n,w) ->
                            Printf.sprintf "%s:%d" n w) vhdl_outputs));
                        Printf.printf "  SV:   %s\n"
                          (String.concat ", " (List.map (fun (n,w) ->
                            Printf.sprintf "%s:%d" n w) sv_outputs));
                      end;
                      exit 1
                    end))
