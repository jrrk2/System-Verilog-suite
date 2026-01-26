(* Nlview schematic to behavioral Verilog converter *)

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <input.sch> [output.v]\n" Sys.argv.(0);
    Printf.printf "\n";
    Printf.printf "Converts Nlview schematic (.sch) to behavioral Verilog.\n";
    Printf.printf "\n";
    Printf.printf "Arguments:\n";
    Printf.printf "  input.sch   - Nlview schematic file to read\n";
    Printf.printf "  output.v    - Output Verilog file (optional, defaults to <module>.v)\n";
    Printf.printf "\n";
    Printf.printf "Example:\n";
    Printf.printf "  %s uart.sch uart.v\n" Sys.argv.(0);
    Printf.printf "  %s design.sch\n" Sys.argv.(0);
    exit 1
  end;

  let input_file = Sys.argv.(1) in

  (* Check input file exists *)
  if not (Sys.file_exists input_file) then begin
    Printf.eprintf "Error: Input file not found: %s\n" input_file;
    exit 1
  end;

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Nlview Schematic to Behavioral Verilog Converter\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Input:  %s\n" input_file;

  (* Step 1: Parse schematic *)
  Printf.printf "\nStep 1: Parsing Nlview schematic...\n";
  let sch = Nlview_parser.parse_schematic input_file in
  Printf.printf "  Module: %s\n" sch.Nlview_parser.module_name;
  Printf.printf "  Ports: %d\n" sch.num_ports;
  Printf.printf "  Instances: %d\n" sch.num_instances;
  Printf.printf "  Nets: %d\n" sch.num_nets;

  (* Step 2: Convert to Behavioral IR *)
  Printf.printf "\nStep 2: Converting to Behavioral IR...\n";
  let prog = Nlview_to_behavioral.convert input_file in
  let bmod = List.hd prog.Behavioral_ir.modules in
  Printf.printf "  Signals: %d\n" (List.length bmod.signals);
  Printf.printf "  Processes: %d\n" (List.length bmod.processes);
  Printf.printf "  Hierarchical instances: %d\n" (List.length bmod.instances);

  (* Count assignments *)
  let num_assignments = List.fold_left (fun acc proc ->
    match proc with
    | Behavioral_ir.BCombinational { body; _ } -> acc + List.length body
    | Behavioral_ir.BSequential { body; _ } -> acc + List.length body
  ) 0 bmod.processes in
  Printf.printf "  Assignments: %d\n" num_assignments;

  (* Step 3: Generate Verilog *)
  Printf.printf "\nStep 3: Generating Verilog...\n";

  (* Determine output filename *)
  let output_file =
    if Array.length Sys.argv >= 3 then
      Sys.argv.(2)
    else
      Printf.sprintf "%s.v" bmod.name
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

  Printf.printf "\nGenerated behavioral Verilog from gate-level schematic.\n";
  Printf.printf "The output can be simulated or used for verification.\n\n"

let _ = main ()
