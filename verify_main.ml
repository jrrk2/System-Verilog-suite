(* verify_main.ml - Main program for Z3-based equivalence verification *)

let usage () =
  Printf.eprintf "SystemVerilog HardCaml Verification Tool\n\n";
  Printf.eprintf "Usage:\n";
  Printf.eprintf "  %s <original.json> <hardcaml.json>\n\n" Sys.argv.(0);
  Printf.eprintf "This tool verifies that HardCaml-generated Verilog is equivalent\n";
  Printf.eprintf "to the original SystemVerilog using Z3 SMT solving.\n\n";
  Printf.eprintf "Example:\n";
  Printf.eprintf "  %s obj_dir/Valu.tree.json obj_dir/Valu_hardcaml.tree.json\n\n" Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv <> 3 then usage ();
  
  let original_json = Sys.argv.(1) in
  let hardcaml_json = Sys.argv.(2) in
  
  if not (Sys.file_exists original_json) then begin
    Printf.eprintf "Error: File not found: %s\n" original_json;
    exit 1
  end;
  
  if not (Sys.file_exists hardcaml_json) then begin
    Printf.eprintf "Error: File not found: %s\n" hardcaml_json;
    exit 1
  end;
  
  try
    let result = Sv_verify_hardcaml.verify_hardcaml_output original_json hardcaml_json in
    exit (if result then 0 else 1)
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Error: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1
