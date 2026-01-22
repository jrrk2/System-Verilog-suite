(* Test multiplier verification with debug output *)

open Sv_ast

let () =
  (* Parse both IRs *)
  let design = Sv_rtlil_reader.parse_rtlil_file "obj_dir/simple_mult.il" in
  let yosys_ir = match Sv_rtlil_to_ir.rtlil_design_to_ir design with
    | Some ir -> ir
    | None -> failwith "Failed to convert Yosys RTLIL" in

  let ast = Sv_main_unified.translate_tree_to_ast "obj_dir/Vsimple_mult.tree.json" in
  let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in

  (* Print IR structures *)
  Printf.printf "\n=== Yosys IR ===\n";
  Sv_ir_verify.print_ir_stats yosys_ir;
  Printf.printf "\nNodes:\n";
  Hashtbl.iter (fun id node ->
    Printf.printf "  Node %d: %s, inputs: [%s]\n" 
      id
      (match node.node_op with
       | Mul {width; signed} -> Printf.sprintf "Mul(w=%d, s=%b)" width signed
       | _ -> "Other")
      (String.concat ", " (List.map string_of_int node.node_inputs))
  ) yosys_ir.ir_nodes;

  Printf.printf "\n=== Verilator IR ===\n";
  Sv_ir_verify.print_ir_stats verilator_ir;
  Printf.printf "\nNodes:\n";
  Hashtbl.iter (fun id node ->
    Printf.printf "  Node %d: %s, inputs: [%s]\n" 
      id
      (match node.node_op with
       | Mul {width; signed} -> Printf.sprintf "Mul(w=%d, s=%b)" width signed
       | ZeroExtend {from_width; to_width} -> Printf.sprintf "ZeroExtend(%d→%d)" from_width to_width
       | _ -> "Other")
      (String.concat ", " (List.map string_of_int node.node_inputs))
  ) verilator_ir.ir_nodes;

  (* Try verification *)
  Printf.printf "\n=== Verification ===\n";
  let result = Sv_ir_verify.verify_ir_equivalence yosys_ir verilator_ir in
  Printf.printf "Result: %b\n" result
