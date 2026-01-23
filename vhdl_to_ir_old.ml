(* VHDL to IR Converter - Main converter *)
(*
 * Assumes hardwired std_logic defaults:
 * - All port types default to std_logic/std_logic_vector
 * - Integer generics used for parameterization
 * - Standard synchronous design patterns (clock + reset)
 * - No explicit IEEE library resolution needed
 *)

open Vhd_front.VhdlTypes
open Sv_ast
open Vhdl_expr_to_ir
open Vhdl_process_extract

(* Get width from operation *)
let get_op_width op =
  match op with
  | Add { width; _ } | Sub { width; _ } | Mul { width; _ } | Div { width; _ }
  | And { width } | Or { width } | Xor { width } | Not { width }
  | Mux { width } | Pmux { width; _ } | Compare { width; _ } | Shift { width; _ }
  | Extract { width; _ } | Register { width; _ } -> width
  | ZeroExtend { to_width; _ } -> to_width
  | SignExtend { to_width; _ } -> to_width
  | Concat { widths } -> List.fold_left (+) 0 widths

(* Convert VHDL architecture to IR *)
let rec convert_architecture_to_ir arch =
  (* Extract process from architecture *)
  let proc_opt = List.find_map (fun stmt ->
    match stmt with
    | ConcurrentProcessStatement proc -> Some proc
    | _ -> None
  ) arch.archstatements in

  match proc_opt with
  | None ->
      Printf.eprintf "Warning: No process found in architecture\n";
      None
  | Some proc ->
      (* Extract process information *)
      let info = extract_process_info proc in

      (* Create conversion context *)
      let ctx = create_context () in

      (* Extract entity name *)
      let (entity_name, _) = arch.archentityname in

      (* Convert all assignments to IR *)
      let converted_assignments = List.map (fun (signal_name, cond_vals) ->
        let converted_cond_vals = List.map (fun (cond_expr, val_expr) ->
          let (cond_id, cond_width) = convert_expression ctx cond_expr in
          let (val_id, val_width) = convert_expression ctx val_expr in
          (cond_id, val_id, val_width)
        ) cond_vals in
        (signal_name, converted_cond_vals)
      ) info.assignments in

      (* Build MUX trees for each signal *)
      let signal_mux_trees = List.map (fun (signal_name, cond_vals) ->
        let mux_root = build_mux_tree ctx cond_vals in
        (signal_name, mux_root)
      ) converted_assignments in

      (* Create Register nodes for each signal *)
      let registers = List.map (fun (signal_name, mux_id) ->
        let (signal_id, signal_width) = get_signal ctx signal_name 1 in

        (* Get clock and reset IDs *)
        let clock_id = match info.clock_signal with
          | Some clk -> fst (get_signal ctx clk 1)
          | None -> 0
        in
        let reset_id_opt = match info.reset_signal with
          | Some rst -> Some (fst (get_signal ctx rst 1))
          | None -> None
        in

        (* Create register node *)
        let reg_node = {
          sn_op = Register {
            width = signal_width;
            clock = clock_id;
            reset = reset_id_opt;
            reset_value = 0;  (* Default reset value *)
            enable = None;  (* No explicit enable for now *)
          };
          sn_inputs = [mux_id];
        } in

        Hashtbl.add ctx.ir_nodes signal_id reg_node;
        (signal_name, signal_id, signal_width)
      ) signal_mux_trees in

      (* Build complete IR structure *)
      let ir = build_ir_structure ctx info registers entity_name in
      Some ir

(* Build MUX tree from conditional assignments *)
and build_mux_tree ctx cond_vals =
  match cond_vals with
  | [] ->
      (* No assignments - return constant 0 *)
      add_constant ctx 0 1

  | [(cond_id, val_id, val_width)] ->
      (* Single assignment - just return the value *)
      val_id

  | (cond_id, val_id, val_width) :: rest ->
      (* Multiple assignments - build MUX tree *)
      (* MUX(condition, true_val, false_val) *)
      let false_branch = build_mux_tree ctx rest in
      let mux_id = add_node ctx (Mux { width = val_width }) [cond_id; val_id; false_branch] in
      mux_id

(* Build complete IR structure *)
and build_ir_structure ctx info registers entity_name =
  (* Create IR inputs *)
  let ir_inputs = Hashtbl.create 10 in

  (* Add clock as input *)
  (match info.clock_signal with
   | Some clk ->
       let (id, width) = get_signal ctx clk 1 in
       Hashtbl.add ir_inputs clk (Input { id; name = clk; width })
   | None -> ());

  (* Add reset as input *)
  (match info.reset_signal with
   | Some rst ->
       let (id, width) = get_signal ctx rst 1 in
       Hashtbl.add ir_inputs rst (Input { id; name = rst; width })
   | None -> ());

  (* Add other signals from signal table as inputs *)
  Hashtbl.iter (fun name (id, width) ->
    if not (Hashtbl.mem ir_inputs name) && not (Hashtbl.mem ctx.ir_wires name) then
      if not (List.exists (fun (rname, _, _) -> rname = name) registers) then
        Hashtbl.add ir_inputs name (Input { id; name; width })
  ) ctx.signals;

  (* Create IR outputs from registers *)
  let ir_outputs = Hashtbl.create 10 in
  List.iter (fun (name, id, width) ->
    Hashtbl.add ir_outputs name (Wire { id; name; width })
  ) registers;

  (* Convert simple_nodes to full IR nodes *)
  let ir_nodes = Hashtbl.create (Hashtbl.length ctx.ir_nodes) in
  Hashtbl.iter (fun id snode ->
    (* Get width from operation *)
    let width = get_op_width snode.sn_op in
    (* Create output wire for this node *)
    let output_wire = Wire { id; name = Printf.sprintf "node_%d" id; width } in
    let node = {
      node_id = id;
      node_op = snode.sn_op;
      node_inputs = snode.sn_inputs;
      node_output = output_wire;
      node_depth = 0;      (* Will be computed later *)
      node_users = [];     (* Will be computed later *)
    } in
    Hashtbl.add ir_nodes id node
  ) ctx.ir_nodes;

  (* Constants are already tracked in ctx.ir_constants *)

  (* Create value-to-node mapping (empty for now) *)
  let value_to_node = Hashtbl.create 10 in

  {
    ir_name = entity_name;
    ir_inputs = ir_inputs;
    ir_outputs = ir_outputs;
    ir_wires = ctx.ir_wires;
    ir_constants = Hashtbl.create 10;  (* Empty for now *)
    ir_nodes = ir_nodes;
    ir_value_to_node = value_to_node;
    ir_next_id = ctx.next_id;
    ir_critical_path_length = 0;
    ir_area_estimate = 0;
  }

(* Convert VHDL file to IR - using new iterative converter *)
let convert_vhdl_file_to_ir filename =
  (* Temporarily return None - need to integrate vhdl_to_ir_iterate properly *)
  Printf.eprintf "Warning: VHDL conversion not yet integrated, calling subprocess\n";

  (* Call vhdl_to_ir_iterate.exe to do the conversion *)
  (* For now, use the old method *)
  match Vhdl_parse.parse_vhdl_file filename with
  | None ->
      Printf.eprintf "Failed to parse VHDL file: %s\n" filename;
      None
  | Some design_file ->
      match Vhdl_elaborate.get_architecture_body design_file with
      | None ->
          Printf.eprintf "No architecture found in: %s\n" filename;
          None
      | Some arch ->
          convert_architecture_to_ir arch

(* Pretty print IR *)
let print_ir ir =
  Printf.printf "\n";
  Printf.printf "IR Module: %s\n" ir.ir_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  Printf.printf "\nInputs (%d):\n" (Hashtbl.length ir.ir_inputs);
  Hashtbl.iter (fun name value ->
    match value with
    | Input { id; name; width } ->
        Printf.printf "  %s: id=%d, width=%d\n" name id width
    | _ -> ()
  ) ir.ir_inputs;

  Printf.printf "\nOutputs (%d):\n" (Hashtbl.length ir.ir_outputs);
  Hashtbl.iter (fun name value ->
    match value with
    | Wire { id; name; width } ->
        Printf.printf "  %s: id=%d, width=%d\n" name id width
    | _ -> ()
  ) ir.ir_outputs;

  Printf.printf "\nWires (%d):\n" (Hashtbl.length ir.ir_wires);
  Hashtbl.iter (fun name value ->
    match value with
    | Wire { id; name; width } ->
        Printf.printf "  %s: id=%d, width=%d\n" name id width
    | _ -> ()
  ) ir.ir_wires;

  Printf.printf "\nNodes (%d):\n" (Hashtbl.length ir.ir_nodes);
  Hashtbl.iter (fun id node ->
    let op_str = match node.node_op with
      | Add _ -> "Add"
      | Sub _ -> "Sub"
      | Mul _ -> "Mul"
      | Div _ -> "Div"
      | And _ -> "And"
      | Or _ -> "Or"
      | Xor _ -> "Xor"
      | Not _ -> "Not"
      | Mux _ -> "Mux"
      | Pmux _ -> "Pmux"
      | Compare _ -> "Compare"
      | Shift _ -> "Shift"
      | Concat _ -> "Concat"
      | Extract _ -> "Extract"
      | ZeroExtend _ -> "ZeroExtend"
      | SignExtend _ -> "SignExtend"
      | Register _ -> "Register"
    in
    Printf.printf "  Node %d: %s (inputs: [%s])\n"
      id op_str
      (String.concat ", " (List.map string_of_int node.node_inputs))
  ) ir.ir_nodes;

  Printf.printf "\n"
