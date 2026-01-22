(* Test Z3 equivalence verification for UART modules - bottom-up *)

let extract_module_from_file filename module_name start_line end_line output_file =
  (* Extract a specific module from the file *)
  let ic = open_in filename in
  let oc = open_out output_file in

  (* Skip to start line *)
  for _i = 1 to start_line - 1 do
    let _ = input_line ic in ()
  done;

  (* Copy module lines *)
  for _i = start_line to end_line do
    let line = input_line ic in
    output_string oc (line ^ "\n")
  done;

  close_in ic;
  close_out oc;
  Printf.printf "  Extracted %s to %s\n" module_name output_file

let find_module_bounds filename module_name =
  let ic = open_in filename in
  let line_num = ref 0 in
  let start_line = ref 0 in
  let end_line = ref 0 in

  try
    while true do
      line_num := !line_num + 1;
      let line = input_line ic in
      if !start_line = 0 && Str.string_match (Str.regexp ("^module " ^ module_name)) line 0 then
        start_line := !line_num
      else if !start_line > 0 && !end_line = 0 && Str.string_match (Str.regexp "^endmodule") line 0 then begin
        end_line := !line_num;
        raise Exit
      end
    done;
    (0, 0)
  with
  | Exit -> close_in ic; (!start_line, !end_line)
  | End_of_file -> close_in ic; (!start_line, !end_line)

let rec test_module module_name =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Module: %s\n" module_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  try
    (* Extract module to temporary file *)
    let temp_file = Printf.sprintf "/tmp/%s.sv" module_name in
    let (start_line, end_line) = find_module_bounds "sysver_tests/apb_uart.sv" module_name in

    if start_line = 0 || end_line = 0 then begin
      Printf.printf "  ❌ Could not find module boundaries\n";
      (module_name, false, "Module not found")
    end else begin
      Printf.printf "  Found module at lines %d-%d (%d lines)\n" start_line end_line (end_line - start_line + 1);
      extract_module_from_file "sysver_tests/apb_uart.sv" module_name start_line end_line temp_file;

      (* Check if Verilator JSON exists *)
      let json_file = Printf.sprintf "sysver_tests/obj_dir/V%s.tree.json" module_name in
      if not (Sys.file_exists json_file) then begin
        Printf.printf "\n  ⚠️  Verilator JSON not found: %s\n" json_file;
        Printf.printf "  Generating with Verilator...\n";
        let abs_temp_file = if Filename.is_relative temp_file then
          Filename.concat (Sys.getcwd ()) temp_file
        else
          temp_file
        in
        let cmd = Printf.sprintf "verilator --json-only --sv -Wno-fatal --top-module %s %s -Mdir sysver_tests/obj_dir 2>&1"
          module_name abs_temp_file in
        Printf.printf "  Running: %s\n" cmd;
        let status = Sys.command cmd in
        if status <> 0 then begin
          Printf.printf "  ❌ Verilator failed\n";
          (module_name, false, "Verilator generation failed")
        end else if not (Sys.file_exists json_file) then begin
          Printf.printf "  ❌ Verilator JSON not created\n";
          (module_name, false, "Verilator JSON not created")
        end else begin
          Printf.printf "  ✓ Verilator JSON generated, retrying...\n";
          (* Recurse to try loading the JSON *)
          test_module module_name
        end
      end else begin
        (* Load Verilator IR *)
        Printf.printf "\n[1/3] Loading Verilator IR...\n";
        let json = Yojson.Safe.from_file json_file in
        let ast = Sv_parse.parse json in
        let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in
        Printf.printf "  ✓ Verilator IR loaded\n";
        Printf.printf "    Inputs: %d, Outputs: %d, Nodes: %d\n"
          (Hashtbl.length verilator_ir.Sv_ast.ir_inputs)
          (Hashtbl.length verilator_ir.Sv_ast.ir_outputs)
          (Hashtbl.length verilator_ir.Sv_ast.ir_nodes);

        (* Load Verible IR *)
        Printf.printf "\n[2/3] Loading Verible IR...\n";
        match Sv_verible_to_ir.file_to_ir temp_file with
        | None ->
            Printf.printf "  ❌ Verible parsing failed\n";
            (module_name, false, "Verible parsing failed")
        | Some verible_ir ->
            Printf.printf "  ✓ Verible IR loaded\n";
            Printf.printf "    Inputs: %d, Outputs: %d, Nodes: %d\n"
              (Hashtbl.length verible_ir.Sv_ast.ir_inputs)
              (Hashtbl.length verible_ir.Sv_ast.ir_outputs)
              (Hashtbl.length verible_ir.Sv_ast.ir_nodes);

            (* Run Z3 verification *)
            Printf.printf "\n[3/3] Running Z3 verification...\n";
            let start_time = Unix.gettimeofday () in
            let result = Sv_ir_verify.verify_ir_equivalence verilator_ir verible_ir in
            let elapsed = Unix.gettimeofday () -. start_time in

            Printf.printf "\n  Result: %s (%.2fs)\n"
              (if result then "✅ EQUIVALENT" else "❌ NOT EQUIVALENT")
              elapsed;

            if result then
              (module_name, true, "")
            else
              (module_name, false, "IRs not equivalent")
      end
    end

  with e ->
    let error_msg = Printexc.to_string e in
    Printf.printf "  ❌ ERROR: %s\n" error_msg;
    (module_name, false, error_msg)

let () =
  Printf.printf "\n╔════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  UART Modules - Z3 Equivalence Verification Suite             ║\n";
  Printf.printf "║  Bottom-Up Testing: Simplest to Most Complex                  ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════════╝\n";

  (* Test modules from simplest to most complex *)
  let modules = [
    "slib_input_sync";       (* 25 lines - simplest *)
    "slib_edge_detect";      (* 27 lines *)
    "slib_clock_div";        (* 40 lines *)
    "slib_mv_filter";        (* 43 lines *)
    "uart_baudgen";          (* 43 lines *)
    "slib_input_filter";     (* 46 lines *)
    "slib_counter";          (* 53 lines *)
    "uart_interrupt";        (* 63 lines *)
    "slib_fifo";            (* 120 lines *)
    "uart_receiver";         (* 278 lines *)
    "uart_transmitter";      (* 285 lines *)
    (* Skip apb_uart for now - too complex *)
  ] in

  let results = List.map test_module modules in

  (* Print summary *)
  Printf.printf "\n\n╔════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  FINAL SUMMARY                                                 ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════════╝\n\n";

  List.iter (fun (name, passed, error) ->
    if passed then
      Printf.printf "  ✅ %s\n" name
    else if error = "" then
      Printf.printf "  ⚠️  %s (skipped or incomplete)\n" name
    else
      Printf.printf "  ❌ %s: %s\n" name error
  ) results;

  let passed_count = List.filter (fun (_, p, _) -> p) results |> List.length in
  let total = List.length results in

  Printf.printf "\n  Passed: %d / %d\n\n" passed_count total;

  if passed_count = total then begin
    Printf.printf "✅ ALL MODULES VERIFIED EQUIVALENT!\n\n";
    exit 0
  end else begin
    Printf.printf "⚠️  Some modules need attention\n\n";
    exit 1
  end
