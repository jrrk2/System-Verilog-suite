(* EDIF to Behavioral Verilog Converter
 * Main program
 *)

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <input.edf> [output.v]\n" Sys.argv.(0);
    Printf.printf "\n";
    Printf.printf "Converts EDIF netlist to behavioral Verilog.\n";
    Printf.printf "\n";
    Printf.printf "Arguments:\n";
    Printf.printf "  input.edf   - EDIF file to read\n";
    Printf.printf "  output.v    - Output Verilog file (optional, defaults to <module>.v)\n";
    Printf.printf "\n";
    Printf.printf "Example:\n";
    Printf.printf "  %s uart.edf uart.v\n" Sys.argv.(0);
    Printf.printf "  %s design.edf\n" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in

  (* Check input file exists *)
  if not (Sys.file_exists input_file) then begin
    Printf.eprintf "Error: Input file not found: %s\n" input_file;
    exit 1
  end;

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  EDIF to Behavioral Verilog Converter\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input:  %s\n" input_file;

  (* Step 1: Parse EDIF *)
  Printf.printf "\nStep 1: Parsing EDIF file...\n";
  let edif = Edif_parser.parse_schematic input_file in
  Printf.printf "  Module: %s\n" edif.Edif_parser.module_name;
  Printf.printf "  Ports: %d\n" (List.length edif.ports);
  Printf.printf "  Instances: %d\n" (List.length edif.instances);
  Printf.printf "  Nets: %d\n" (List.length edif.nets);

  (* Step 2: Convert to Behavioral IR *)
  Printf.printf "\nStep 2: Converting to Behavioral IR...\n";
  let prog = Edif_to_behavioral.convert input_file in

  (* Find the top-level module by name *)
  let top_module = List.find (fun (m : Behavioral_ir.bmodule) ->
    m.name = edif.module_name
  ) prog.Behavioral_ir.modules in

  Printf.printf "  Signals: %d\n" (List.length top_module.signals);
  Printf.printf "  Processes: %d\n" (List.length top_module.processes);
  Printf.printf "  Hierarchical instances: %d\n" (List.length top_module.instances);

  (* Count assignments *)
  let num_assignments = List.fold_left (fun acc proc ->
    match proc with
    | Behavioral_ir.BCombinational { body; _ } -> acc + List.length body
    | Behavioral_ir.BSequential { body; _ } -> acc + List.length body
  ) 0 top_module.processes in
  Printf.printf "  Assignments: %d\n" num_assignments;

  (* Step 3: Generate Verilog *)
  Printf.printf "\nStep 3: Generating Verilog...\n";

  (* Determine output filename *)
  let output_file =
    if Array.length Sys.argv >= 3 then
      Sys.argv.(2)
    else
      Printf.sprintf "%s.v" edif.module_name
  in

  (* Write Verilog *)
  Behavioral_to_verilog.write_to_file output_file prog;

  (* Report *)
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Conversion Complete\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "\nOutput: %s\n" output_file;

  (* Get file size *)
  let ic = open_in output_file in
  let num_lines = ref 0 in
  (try
    while true do
      ignore (input_line ic);
      incr num_lines
    done
  with End_of_file -> close_in ic);

  Printf.printf "Lines:  %d\n" !num_lines;

  Printf.printf "\nGenerated behavioral Verilog from EDIF netlist.\n";
  Printf.printf "The output can be simulated or used for verification.\n\n"

let _ = main ()
