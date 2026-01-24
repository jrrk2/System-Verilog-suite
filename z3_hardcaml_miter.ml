(* HardCaml Interface Equivalence Checking
 *
 * This checks that VHDL and SystemVerilog compile to HardCaml circuits
 * with matching interfaces (port names and widths).
 *
 * Benefits:
 * - HardCaml's type system validates width consistency
 * - Both designs go through same normalization pipeline
 * - Interface match is necessary (but not sufficient) for equivalence
 *
 * For full formal verification, use: HardCaml → Verilog → Formality/Conformal
 *)

(* Interface-based equivalence check *)
let check_interface_equivalence module_name inputs1 outputs1 inputs2 outputs2 =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  HardCaml Interface Equivalence Check\n";
  Printf.printf "  Module: %s\n" module_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Design 1: %d inputs, %d outputs\n"
    (List.length inputs1) (List.length outputs1);
  Printf.printf "Design 2: %d inputs, %d outputs\n\n"
    (List.length inputs2) (List.length outputs2);

  (* Sort ports by name *)
  let sort_ports ports = List.sort (fun (n1,_) (n2,_) -> String.compare n1 n2) ports in
  let inputs1_sorted = sort_ports inputs1 in
  let inputs2_sorted = sort_ports inputs2 in
  let outputs1_sorted = sort_ports outputs1 in
  let outputs2_sorted = sort_ports outputs2 in

  (* Check interfaces match *)
  if List.length inputs1 <> List.length inputs2 then begin
    Printf.eprintf "✗ Input count mismatch: %d vs %d\n"
      (List.length inputs1) (List.length inputs2);
    false
  end else if List.length outputs1 <> List.length outputs2 then begin
    Printf.eprintf "✗ Output count mismatch: %d vs %d\n"
      (List.length outputs1) (List.length outputs2);
    false
  end else begin
    (* Check each port matches (name and width) *)
    let inputs_match = List.for_all2 (fun (n1, w1) (n2, w2) ->
      n1 = n2 && w1 = w2
    ) inputs1_sorted inputs2_sorted in

    let outputs_match = List.for_all2 (fun (n1, w1) (n2, w2) ->
      n1 = n2 && w1 = w2
    ) outputs1_sorted outputs2_sorted in

    if not inputs_match then begin
      Printf.printf "❌ INPUT MISMATCH\n\n";
      Printf.printf "VHDL inputs:\n";
      List.iter (fun (name, width) ->
        Printf.printf "  %s: %d bits\n" name width
      ) inputs1_sorted;
      Printf.printf "\nSystemVerilog inputs:\n";
      List.iter (fun (name, width) ->
        Printf.printf "  %s: %d bits\n" name width
      ) inputs2_sorted;
      Printf.printf "\n";
      false
    end else if not outputs_match then begin
      Printf.printf "❌ OUTPUT MISMATCH\n\n";
      Printf.printf "VHDL outputs:\n";
      List.iter (fun (name, width) ->
        Printf.printf "  %s: %d bits\n" name width
      ) outputs1_sorted;
      Printf.printf "\nSystemVerilog outputs:\n";
      List.iter (fun (name, width) ->
        Printf.printf "  %s: %d bits\n" name width
      ) outputs2_sorted;
      Printf.printf "\n";
      false
    end else begin
      Printf.printf "✅ INTERFACE MATCH\n\n";
      Printf.printf "Inputs (%d):\n" (List.length inputs1_sorted);
      List.iter (fun (name, width) ->
        Printf.printf "  %s: %d bits\n" name width
      ) inputs1_sorted;
      Printf.printf "\nOutputs (%d):\n" (List.length outputs1_sorted);
      List.iter (fun (name, width) ->
        Printf.printf "  %s: %d bits\n" name width
      ) outputs1_sorted;
      Printf.printf "\n";

      Printf.printf "═══════════════════════════════════════════════════════════════\n";
      Printf.printf "  ✅ INTERFACES EQUIVALENT\n";
      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
      Printf.printf "HardCaml's type system validated:\n";
      Printf.printf "  ✅ All port widths match\n";
      Printf.printf "  ✅ All port names match\n";
      Printf.printf "  ✅ Type checking passed\n\n";
      Printf.printf "Next steps for formal equivalence:\n";
      Printf.printf "  1. Generate Verilog from both HardCaml circuits\n";
      Printf.printf "  2. Use Synopsys Formality or Cadence Conformal\n";
      Printf.printf "  3. Get formal mathematical proof\n\n";
      true
    end
  end

(* Simplified verification - just check interfaces for now *)
let verify_hardcaml_equivalence vhdl_file sv_file =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  HardCaml-Based Equivalence Verification (Simplified)\n";
  Printf.printf "  Type Safety + Interface Validation\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input Files:\n";
  Printf.printf "  VHDL: %s\n" vhdl_file;
  Printf.printf "  SV:   %s\n\n" sv_file;

  (* Convert to Behavioral IR *)
  Printf.printf "[1/5] Converting VHDL to Behavioral IR...\n";
  let vhdl_prog_opt = Vhdl_to_behavioral.convert_vhdl_file_to_behavioral vhdl_file in

  match vhdl_prog_opt with
  | None ->
      Printf.eprintf "✗ VHDL conversion failed\n";
      false
  | Some vhdl_prog ->
      Printf.printf "✓ VHDL converted\n\n";

      Printf.printf "[2/5] Converting SystemVerilog to Behavioral IR...\n";
      let sv_prog_opt = Sv_to_behavioral.convert_elaborated_sv_to_behavioral sv_file in

      match sv_prog_opt with
      | None ->
          Printf.eprintf "✗ SV conversion failed\n";
          false
      | Some sv_prog ->
          Printf.printf "✓ SV converted\n\n";

          (* Optimize *)
          Printf.printf "[3/5] Optimizing both designs...\n";
          let open Behavioral_optimize in
          let (vhdl_opt, _) = optimize_custom { default_config with verbose = false } vhdl_prog in
          let (sv_opt, _) = optimize_custom { default_config with verbose = false } sv_prog in
          Printf.printf "✓ Optimized\n\n";

          (* Convert to HardCaml *)
          Printf.printf "[4/5] Converting to HardCaml circuits...\n";

          (match Behavioral_to_hardcaml.convert_to_hardcaml vhdl_opt with
           | None ->
               Printf.eprintf "✗ VHDL to HardCaml failed\n";
               false
           | Some (vhdl_mod, vhdl_inputs, vhdl_outputs) ->
               Printf.printf "✓ VHDL circuit created (%d inputs, %d outputs)\n"
                 (List.length vhdl_inputs) (List.length vhdl_outputs);

               (match Behavioral_to_hardcaml.convert_to_hardcaml sv_opt with
                | None ->
                    Printf.eprintf "✗ SV to HardCaml failed\n";
                    false
                | Some (sv_mod, sv_inputs, sv_outputs) ->
                    Printf.printf "✓ SV circuit created (%d inputs, %d outputs)\n\n"
                      (List.length sv_inputs) (List.length sv_outputs);

                    Printf.printf "[5/5] Checking equivalence...\n\n";
                    check_interface_equivalence vhdl_mod.name
                                               vhdl_inputs vhdl_outputs
                                               sv_inputs sv_outputs))
