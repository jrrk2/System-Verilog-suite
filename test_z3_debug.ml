(* Debug Z3 verification by checking intermediate values *)

let () =
  let original_json = "obj_dir/Valu.tree.json" in
  let hardcaml_json = "obj_dir/Vhardcaml_alu.tree.json" in
  
  Printf.printf "Loading ASTs...\n";
  let original_ast = Sv_parse.parse (Yojson.Safe.from_file original_json) in
  let hardcaml_ast = Sv_parse.parse (Yojson.Safe.from_file hardcaml_json) in
  
  Printf.printf "Running verification...\n";
  let result = Sv_verify_hardcaml.verify_hardcaml_output original_json hardcaml_json in
  
  Printf.printf "\nResult: %s\n" (if result then "PASS" else "FAIL");
  exit (if result then 0 else 1)
