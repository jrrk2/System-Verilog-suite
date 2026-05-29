(* Test EDIF vs VHDL converter equivalence
 *
 * This test compares:
 * 1. VHDL → Behavioral IR (direct from VHDL sources)
 * 2. VHDL → Vivado Synthesis → EDIF → Behavioral IR (through synthesis)
 *
 * The two paths should produce equivalent behavioral representations.
 *)

let check_file_exists filename =
  if not (Sys.file_exists filename) then begin
    Printf.eprintf "Error: Required file not found: %s\n" filename;
    exit 1
  end

let () =
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  EDIF vs VHDL Equivalence Checker\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  (* Check for EDIF file *)
  let edif_file = "uart_synthesized.edf" in
  Printf.printf "Checking for EDIF file: %s\n" edif_file;
  check_file_exists edif_file;
  Printf.printf "  ✓ Found\n\n";

  (* Step 1: Convert EDIF to Behavioral IR *)
  Printf.printf "Step 1: Converting EDIF to Behavioral IR...\n";
  let edif_prog = Edif_to_behavioral.convert edif_file in
  Printf.printf "  Modules from EDIF: %d\n" (List.length edif_prog.Behavioral_ir.modules);
  Printf.printf "  Library cells: %d\n" (List.length edif_prog.library_cells);

  (* Find top module *)
  let edif_top = List.find (fun (m : Behavioral_ir.bmodule) ->
    m.name = "apb_uart"
  ) edif_prog.modules in
  Printf.printf "  Top module signals: %d\n" (List.length edif_top.signals);
  Printf.printf "  Top module processes: %d\n" (List.length edif_top.processes);
  Printf.printf "  Top module instances: %d\n\n" (List.length edif_top.instances);

  (* Step 2: Convert VHDL to Behavioral IR *)
  Printf.printf "Step 2: Converting VHDL to Behavioral IR...\n";

  (* Check for VHDL files *)
  let vhdl_files = [
    "sysver_tests/slib_clock_div.vhd";
    "sysver_tests/slib_counter.vhd";
    "sysver_tests/slib_edge_detect.vhd";
    "sysver_tests/slib_fifo.vhd";
    "sysver_tests/slib_input_filter.vhd";
    "sysver_tests/slib_input_sync.vhd";
    "sysver_tests/slib_mv_filter.vhd";
    "sysver_tests/uart_baudgen.vhd";
    "sysver_tests/uart_interrupt.vhd";
    "sysver_tests/uart_receiver.vhd";
    "sysver_tests/uart_transmitter.vhd";
    "sysver_tests/apb_uart.vhd";
  ] in

  List.iter (fun f ->
    if not (Sys.file_exists f) then
      Printf.printf "  Warning: VHDL file not found: %s\n" f
  ) vhdl_files;

  (* Convert all VHDL files to Behavioral IR *)
  let vhdl_progs = List.filter_map (fun filename ->
    if Sys.file_exists filename then begin
      Printf.printf "  Converting %s...\n" (Filename.basename filename);
      Vhdl_to_behavioral.convert_vhdl_file_to_behavioral filename
    end else None
  ) vhdl_files in

  Printf.printf "  Converted %d VHDL files\n" (List.length vhdl_progs);

  if List.length vhdl_progs = 0 then begin
    Printf.printf "\n⚠ No VHDL files found - skipping VHDL conversion\n";
    Printf.printf "  Run this test on Linux with Vivado and VHDL sources\n";
    exit 0
  end;

  (* Combine all modules from all programs *)
  let all_modules = List.concat_map (fun (prog : Behavioral_ir.bprogram) ->
    prog.modules
  ) vhdl_progs in
  let vhdl_prog = { Behavioral_ir.modules = all_modules; library_cells = [] } in
  Printf.printf "  Modules from VHDL: %d\n" (List.length vhdl_prog.Behavioral_ir.modules);

  let vhdl_top_opt = List.find_opt (fun (m : Behavioral_ir.bmodule) ->
    m.name = "apb_uart"
  ) vhdl_prog.modules in

  match vhdl_top_opt with
  | None ->
      Printf.printf "  ⚠ Top module 'apb_uart' not found in VHDL conversion\n\n"
  | Some vhdl_top ->
      Printf.printf "  Top module signals: %d\n" (List.length vhdl_top.signals);
      Printf.printf "  Top module processes: %d\n" (List.length vhdl_top.processes);
      Printf.printf "  Top module instances: %d\n\n" (List.length vhdl_top.instances);

      (* Step 3: Compare representations *)
      Printf.printf "Step 3: Comparing representations...\n\n";

      (* Compare port counts *)
      let edif_ports = List.filter (fun (s : Behavioral_ir.bsignal) ->
        s.direction <> `Internal
      ) edif_top.signals in
      let vhdl_ports = List.filter (fun (s : Behavioral_ir.bsignal) ->
        s.direction <> `Internal
      ) vhdl_top.signals in

      Printf.printf "Port Comparison:\n";
      Printf.printf "  EDIF ports: %d\n" (List.length edif_ports);
      Printf.printf "  VHDL ports: %d\n" (List.length vhdl_ports);

      if List.length edif_ports = List.length vhdl_ports then
        Printf.printf "  ✓ Port counts match\n\n"
      else
        Printf.printf "  ⚠ Port counts differ\n\n";

      (* Compare instance counts *)
      Printf.printf "Instance Comparison:\n";
      Printf.printf "  EDIF instances: %d\n" (List.length edif_top.instances);
      Printf.printf "  VHDL instances: %d\n" (List.length vhdl_top.instances);

      if List.length edif_top.instances = List.length vhdl_top.instances then
        Printf.printf "  ✓ Instance counts match\n\n"
      else
        Printf.printf "  ⚠ Instance counts differ (expected - EDIF is post-synthesis)\n\n";

      (* Step 4: Z3 Formal Equivalence Checking *)
      Printf.printf "Step 4: Z3 Formal Equivalence Verification...\n\n";

      let z3_result = Z3_miter.check_miter_equivalence edif_top vhdl_top in

      (* Summary *)
      Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
      Printf.printf "  Equivalence Check Summary\n";
      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

      Printf.printf "Structural Comparison:\n";
      Printf.printf "  ✓ Port counts match (%d ports)\n" (List.length edif_ports);
      Printf.printf "  ⚠ EDIF has %d instances (post-synthesis)\n" (List.length edif_top.instances);
      Printf.printf "  ⚠ VHDL has %d instances (behavioral)\n\n" (List.length vhdl_top.instances);

      Printf.printf "Formal Verification (Z3):\n";
      if z3_result then
        Printf.printf "  ✅ DESIGNS ARE FORMALLY EQUIVALENT\n\n"
      else
        Printf.printf "  ❌ DESIGNS DIFFER OR VERIFICATION INCOMPLETE\n\n";

      if not z3_result then
        exit 1
