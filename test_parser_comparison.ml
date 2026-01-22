(* Test all parser paths and compare outputs *)

let run_verilator_path input_file output_file =
  Printf.printf "\n=== Verilator/Yosys Path ===\n";

  (* Run Verilator to generate JSON *)
  (* Verilator outputs to obj_dir/V<module>.tree.json *)
  let module_name = Filename.chop_extension (Filename.basename input_file) in
  let json_file = Printf.sprintf "obj_dir/V%s.tree.json" module_name in
  let cmd = Printf.sprintf "verilator --json-only --sv -Wno-fatal %s 2>&1" input_file in
  Printf.printf "Running: %s\n" cmd;
  let status = Sys.command cmd in

  if status <> 0 then begin
    Printf.eprintf "Verilator failed with status %d\n" status;
    None
  end else begin
    (* Parse JSON and generate output *)
    try
      if not (Sys.file_exists json_file) then begin
        Printf.eprintf "JSON file not found: %s\n" json_file;
        None
      end else begin
        let ast = Sv_parse.parse (Yojson.Safe.from_file json_file) in
        let transformed = Sv_transform.transform ~verbose:false ast in
        let output, _warnings = Sv_gen_yosys.generate_sv_with_warnings transformed 0 in

        (* Write to file *)
        let oc = open_out output_file in
        output_string oc output;
        close_out oc;

        Printf.printf "✓ Verilator path complete: %s\n" output_file;
        Printf.printf "  Output size: %d bytes\n" (String.length output);
        Some output
      end
    with e ->
      Printf.eprintf "Error in Verilator path: %s\n" (Printexc.to_string e);
      None
  end

let run_verible_path input_file output_file =
  Printf.printf "\n=== Verible Path ===\n";

  try
    (* Parse with Verible *)
    match Sv_verible_to_ir.parse_verible_file input_file with
    | None ->
        Printf.eprintf "Verible parsing failed\n";
        None
    | Some ast ->
        Printf.printf "✓ Parsed with Verible\n";

        (* Elaborate *)
        let elab_ctx = Sv_elaborate.elaborate ast in
        Printf.printf "✓ Elaboration complete\n";

        (* Get module name *)
        let module_name = match elab_ctx.Sv_elaborate.module_name with
          | Some name -> name
          | None -> Filename.chop_extension (Filename.basename input_file)
        in

        Printf.printf "  Module: %s\n" module_name;

        (* Convert to IR *)
        let ir = Sv_verible_to_ir.verible_to_ir ast module_name in
        Printf.printf "✓ IR conversion complete\n";
        Printf.printf "  Inputs: %d, Outputs: %d, Nodes: %d\n"
          (Hashtbl.length ir.Sv_ast.ir_inputs)
          (Hashtbl.length ir.Sv_ast.ir_outputs)
          (Hashtbl.length ir.Sv_ast.ir_nodes);

        (* Convert IR back to AST *)
        let ast_from_ir = Opt_ir_to_sv.convert ir in

        (* Generate Verilog *)
        let output = Sv_gen.generate_sv ast_from_ir 0 in

        (* Write to file *)
        let oc = open_out output_file in
        output_string oc output;
        close_out oc;

        Printf.printf "✓ Verible path complete: %s\n" output_file;
        Printf.printf "  Output size: %d bytes\n" (String.length output);
        Some output
  with e ->
    Printf.eprintf "Error in Verible path: %s\n" (Printexc.to_string e);
    Printexc.print_backtrace stderr;
    None

let run_yosys_rtlil_path input_file output_file =
  Printf.printf "\n=== Yosys RTLIL Path ===\n";

  (* Run Yosys to generate RTLIL *)
  let rtlil_file = Filename.temp_file "yosys_" ".il" in
  let cmd = Printf.sprintf "yosys -p 'read_verilog -sv %s; hierarchy -check; proc; write_rtlil %s' 2>&1"
    input_file rtlil_file in
  Printf.printf "Running: %s\n" cmd;
  let status = Sys.command cmd in

  if status <> 0 then begin
    Printf.eprintf "Yosys failed with status %d\n" status;
    None
  end else begin
    try
      (* Parse RTLIL and convert to IR *)
      let design = Sv_rtlil_reader.parse_rtlil_file rtlil_file in
      Printf.printf "✓ Parsed RTLIL: %d modules\n" (List.length design.Sv_rtlil_reader.design_modules);

      if design.Sv_rtlil_reader.design_modules = [] then begin
        Printf.eprintf "No modules found in RTLIL\n";
        None
      end else begin
        (* Take first module *)
        let rtlil_module = List.hd design.Sv_rtlil_reader.design_modules in
        Printf.printf "  Module: %s\n" rtlil_module.Sv_rtlil_reader.mod_name;

        (* Convert to IR *)
        let ir = Sv_rtlil_to_ir.rtlil_module_to_ir rtlil_module in
        Printf.printf "✓ IR conversion complete\n";
        Printf.printf "  Inputs: %d, Outputs: %d, Nodes: %d\n"
          (Hashtbl.length ir.Sv_ast.ir_inputs)
          (Hashtbl.length ir.Sv_ast.ir_outputs)
          (Hashtbl.length ir.Sv_ast.ir_nodes);

        (* Convert IR back to AST *)
        let ast_from_ir = Opt_ir_to_sv.convert ir in

        (* Generate Verilog *)
        let output = Sv_gen.generate_sv ast_from_ir 0 in

        (* Write to file *)
        let oc = open_out output_file in
        output_string oc output;
        close_out oc;

        Printf.printf "✓ RTLIL path complete: %s\n" output_file;
        Printf.printf "  Output size: %d bytes\n" (String.length output);
        Sys.remove rtlil_file;
        Some output
      end
    with e ->
      Printf.eprintf "Error in RTLIL path: %s\n" (Printexc.to_string e);
      Printf.eprintf "Backtrace:\n";
      Printexc.print_backtrace stderr;
      flush stderr;
      None
  end

let compare_outputs original verilator verible rtlil =
  Printf.printf "\n=== Comparison Summary ===\n\n";

  (* Show original *)
  Printf.printf "Original file:\n";
  Printf.printf "  Size: %d bytes\n" (String.length original);
  let original_lines = String.split_on_char '\n' original in
  Printf.printf "  Lines: %d\n\n" (List.length original_lines);

  (* Show Verilator output *)
  (match verilator with
   | Some v ->
       Printf.printf "Verilator/Yosys output:\n";
       Printf.printf "  Size: %d bytes\n" (String.length v);
       let lines = String.split_on_char '\n' v in
       Printf.printf "  Lines: %d\n" (List.length lines);
       let modules = List.filter (fun l -> String.length l > 6 && String.sub l 0 6 = "module") lines in
       Printf.printf "  Modules: %d\n\n" (List.length modules)
   | None ->
       Printf.printf "Verilator/Yosys output: FAILED\n\n");

  (* Show Verible output *)
  (match verible with
   | Some v ->
       Printf.printf "Verible output:\n";
       Printf.printf "  Size: %d bytes\n" (String.length v);
       let lines = String.split_on_char '\n' v in
       Printf.printf "  Lines: %d\n" (List.length lines);
       let modules = List.filter (fun l -> String.length l > 6 && String.sub l 0 6 = "module") lines in
       Printf.printf "  Modules: %d\n\n" (List.length modules)
   | None ->
       Printf.printf "Verible output: FAILED\n\n");

  (* Show RTLIL output *)
  (match rtlil with
   | Some r ->
       Printf.printf "RTLIL output:\n";
       Printf.printf "  Size: %d bytes\n" (String.length r);
       let lines = String.split_on_char '\n' r in
       Printf.printf "  Lines: %d\n" (List.length lines);
       let modules = List.filter (fun l -> String.length l > 6 && String.sub l 0 6 = "module") lines in
       Printf.printf "  Modules: %d\n\n" (List.length modules)
   | None ->
       Printf.printf "RTLIL output: FAILED\n\n");

  (* Visual diff suggestion *)
  Printf.printf "To visually compare outputs:\n";
  (match verilator with Some _ -> Printf.printf "  diff apb_uart_verilator.v apb_uart_verible.v\n" | None -> ());
  (match rtlil with Some _ -> Printf.printf "  diff apb_uart_verilator.v apb_uart_rtlil.v\n" | None -> ());
  (match verible, rtlil with Some _, Some _ -> Printf.printf "  diff apb_uart_verible.v apb_uart_rtlil.v\n" | _ -> ())

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <input.sv>\n" Sys.argv.(0);
    Printf.eprintf "Example: %s sysver_tests/apb_uart.sv\n" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in
  let base_name = Filename.chop_extension (Filename.basename input_file) in

  Printf.printf "=== Parser Comparison Test ===\n";
  Printf.printf "Input: %s\n" input_file;

  (* Read original *)
  let ic = open_in input_file in
  let original = really_input_string ic (in_channel_length ic) in
  close_in ic;

  (* Run all three paths *)
  let verilator_output = run_verilator_path input_file (base_name ^ "_verilator.v") in
  let verible_output = run_verible_path input_file (base_name ^ "_verible.v") in
  (* RTLIL path disabled temporarily - needs new parser integration *)
  let rtlil_output = None in

  (* Compare *)
  compare_outputs original verilator_output verible_output rtlil_output;

  Printf.printf "\n✓ Comparison complete\n"
