(* Debug IR generation for slib_clock_div *)

let string_of_op (op : Sv_ast.operation) =
  match op with
  | Add _ -> "Add"
  | Sub _ -> "Sub"
  | Mul _ -> "Mul"
  | Div _ -> "Div"
  | And _ -> "And"
  | Or _ -> "Or"
  | Xor _ -> "Xor"
  | Not _ -> "Not"
  | Shift _ -> "Shift"
  | Compare _ -> "Compare"
  | Mux _ -> "Mux"
  | Pmux _ -> "Pmux"
  | Concat _ -> "Concat"
  | Extract _ -> "Extract"
  | ZeroExtend _ -> "ZeroExtend"
  | SignExtend _ -> "SignExtend"
  | Register _ -> "Register"

let dump_ir_structure ir_name ir =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  IR Structure: %s\n" ir_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  Printf.printf "Inputs:\n";
  Hashtbl.iter (fun name value ->
    let (id, width) = match value with
      | Sv_ast.Input { id; width; _ } -> (id, width)
      | _ -> (-1, 0)
    in
    Printf.printf "  %s -> value_id=%d (width=%d)\n" name id width
  ) ir.Sv_ast.ir_inputs;

  Printf.printf "\nWires:\n";
  Hashtbl.iter (fun name value ->
    let (id, width) = match value with
      | Sv_ast.Wire { id; width; _ } -> (id, width)
      | _ -> (-1, 0)
    in
    Printf.printf "  %s -> value_id=%d (width=%d)\n" name id width
  ) ir.Sv_ast.ir_wires;

  Printf.printf "\nOutputs:\n";
  Hashtbl.iter (fun name value ->
    let (value_id, width) = match value with
      | Sv_ast.Output { id; width; _ } -> (id, width)
      | Sv_ast.Input { id; width; _ } -> (id, width)
      | Sv_ast.Wire { id; width; _ } -> (id, width)
      | Sv_ast.Constant { id; width; _ } -> (id, width)
    in
    Printf.printf "  %s -> value_id=%d (width=%d)\n" name value_id width;
    match Hashtbl.find_opt ir.Sv_ast.ir_nodes value_id with
    | Some node ->
        Printf.printf "    Node: %s (inputs: [%s])\n"
          (string_of_op node.Sv_ast.node_op)
          (String.concat ", " (List.map string_of_int node.Sv_ast.node_inputs))
    | None ->
        Printf.printf "    Value (no node)\n"
  ) ir.Sv_ast.ir_outputs;

  Printf.printf "\nAll Nodes:\n";
  let nodes = Hashtbl.fold (fun id node acc -> (id, node) :: acc) ir.Sv_ast.ir_nodes [] in
  let sorted_nodes = List.sort (fun (id1, _) (id2, _) -> compare id1 id2) nodes in
  List.iter (fun (id, node) ->
    Printf.printf "  Node %d: %s (inputs: [%s])\n"
      id
      (string_of_op node.Sv_ast.node_op)
      (String.concat ", " (List.map string_of_int node.Sv_ast.node_inputs))
  ) sorted_nodes;
  Printf.printf "\n"

let () =
  let module_name = "slib_clock_div" in
  let sv_file = "/tmp/slib_clock_div.sv" in

  Printf.printf "Debugging IR generation for: %s\n" module_name;

  (* Generate Verilator JSON *)
  let json_file = Printf.sprintf "sysver_tests/obj_dir/V%s.tree.json" module_name in
  Printf.printf "\n[1/2] Loading Verilator IR...\n";
  let json = Yojson.Safe.from_file json_file in
  let ast = Sv_parse.parse json in
  let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in
  dump_ir_structure "Verilator" verilator_ir;

  (* Load Verible IR *)
  Printf.printf "\n[2/2] Loading Verible IR...\n";
  match Sv_verible_to_ir.file_to_ir sv_file with
  | None ->
      Printf.printf "  ❌ Verible parsing failed\n"
  | Some verible_ir ->
      dump_ir_structure "Verible" verible_ir;

      Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
      Printf.printf "  Comparison\n";
      Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

      Printf.printf "Verilator nodes: %d\n" (Hashtbl.length verilator_ir.Sv_ast.ir_nodes);
      Printf.printf "Verible nodes: %d\n" (Hashtbl.length verible_ir.Sv_ast.ir_nodes);

      Printf.printf "\nVerilator outputs: %d\n" (Hashtbl.length verilator_ir.Sv_ast.ir_outputs);
      Printf.printf "Verible outputs: %d\n" (Hashtbl.length verible_ir.Sv_ast.ir_outputs);

      Printf.printf "\nKey differences (checking output node types):\n";
      let get_value_id value = match value with
        | Sv_ast.Output { id; _ } | Sv_ast.Input { id; _ }
        | Sv_ast.Wire { id; _ } | Sv_ast.Constant { id; _ } -> id
      in
      Hashtbl.iter (fun name ver_value ->
        match Hashtbl.find_opt verible_ir.Sv_ast.ir_outputs name with
        | Some ble_value ->
            let ver_id = get_value_id ver_value in
            let ble_id = get_value_id ble_value in
            (match Hashtbl.find_opt verilator_ir.Sv_ast.ir_nodes ver_id with
             | Some ver_node ->
                 (match Hashtbl.find_opt verible_ir.Sv_ast.ir_nodes ble_id with
                  | Some ble_node ->
                      let ver_op = string_of_op ver_node.Sv_ast.node_op in
                      let ble_op = string_of_op ble_node.Sv_ast.node_op in
                      if ver_op <> ble_op then
                        Printf.printf "  Output '%s': Verilator=%s vs Verible=%s\n" name ver_op ble_op
                  | None ->
                      Printf.printf "  Output '%s': Verible has no node\n" name)
             | None ->
                 Printf.printf "  Output '%s': Verilator has no node\n" name)
        | None ->
            Printf.printf "  Output '%s': Only in Verilator\n" name
      ) verilator_ir.Sv_ast.ir_outputs
