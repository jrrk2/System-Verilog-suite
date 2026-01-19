(* sv_main_sat.ml - Updated with SMT *)
open Sv_ast

let jsontree = ref (`String "")

(* Main translation function *)
let translate_tree_to_ast json_file =
  print_endline ("JSON: "^json_file);
  let json = match Yojson.Basic.from_file json_file with `Assoc lst -> `Assoc (List.rev lst) | oth -> oth in
  jsontree := json;
  Sv_parse.parse json

let asthash = Hashtbl.create 255
let tranhash = Hashtbl.create 255
let opthash = Hashtbl.create 255

let scan rslt =
  let obj = "obj_dir/" in
  (try Unix.mkdir rslt 0o750 with e -> Printf.eprintf "%s: %s\n" rslt (Printexc.to_string e));
  let lst = ref [] in
  let fd = Unix.opendir obj in
  (try while true do
    let f = Unix.readdir(fd) in if f.[0]<>'.' then lst := f :: !lst;
  done with End_of_file -> Unix.closedir fd);
  
  (* Track statistics *)
  let total_files = List.length !lst in
  let successful = ref 0 in
  let failed = ref 0 in
  
  Printf.printf "Processing %d files...\n\n" total_files;
  
  List.iter (fun itm ->
    Printf.printf "Processing: %s\n" itm;
      (* 1. Parse AST *)
      let ast = translate_tree_to_ast (obj^itm) in
      Hashtbl.add asthash itm ast;
      
      (* 2. Transform non-synth to synth (SSA, loop unrolling, etc.) *)
      let transformed_ast = Sv_transform.transform ~verbose:false ast in
      Hashtbl.add tranhash itm transformed_ast;
      
      (* 3. NEW: Convert to Optimization IR *)
      Printf.printf "  Converting to optimization IR...\n";
      let opt_ir = Behavioural_to_opt_ir.convert ~verbose:false transformed_ast in
      
      (* 4. NEW: Print before optimization *)
      Printf.printf "  Before optimization: %d nodes, depth=%d, area=%d\n"
        (Hashtbl.length opt_ir.ir_nodes)
        opt_ir.ir_critical_path_length
        opt_ir.ir_area_estimate;
      
      (* 5. NEW: Optimize! *)
      Printf.printf "  Optimizing...\n";
      Sv_opt_ir.optimize opt_ir ~verbose:false ~force_balance:false;
      
      (* 6. NEW: Print after optimization *)
      Printf.printf "  After optimization: %d nodes, depth=%d, area=%d\n"
        (Hashtbl.length opt_ir.ir_nodes)
        opt_ir.ir_critical_path_length
        opt_ir.ir_area_estimate;
      
      (* 7. NEW: Convert optimized IR back to structural AST *)
      Printf.printf "  Converting back to structural AST...\n";
      let structural_ast = Opt_ir_to_sv.convert ~verbose:false opt_ir in
      Hashtbl.add opthash itm structural_ast;
      
      (* 8. Generate Equivalence test *)
      Sv_to_z3.check_equiv_all_outputs transformed_ast structural_ast
      
  ) !lst;
  
  (* Print summary *)
  Printf.printf "========================================\n";
  Printf.printf "Conversion Summary\n";
  Printf.printf "========================================\n";
  Printf.printf "Total files:  %d\n" total_files;
  Printf.printf "Successful:   %d\n" !successful;
  Printf.printf "Failed:       %d\n" !failed;
  Printf.printf "========================================\n"
