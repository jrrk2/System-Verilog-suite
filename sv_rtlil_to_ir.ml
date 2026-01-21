(* sv_rtlil_to_ir.ml - Convert Yosys RTLIL to hardware optimization IR *)

open Sv_ast
open Sv_rtlil_reader

(* Helper to extract parameter value from cell *)
let get_param cell param_name =
  try
    let value_str = List.assoc param_name cell.cell_params in
    (* Parameters can be integers or bit vectors like "4'b0101" *)
    if String.contains value_str '\'' then
      (* Parse bit vector format: width'format_value *)
      let parts = String.split_on_char '\'' value_str in
      match parts with
      | width_str :: _ -> int_of_string width_str
      | _ -> 1
    else
      int_of_string value_str
  with _ -> 1

let get_param_bool cell param_name =
  try
    let value_str = List.assoc param_name cell.cell_params in
    int_of_string value_str <> 0
  with _ -> false

(* Map RTLIL signal spec to IR value_id
   For now, simplified - assumes wire names map to node IDs *)
let sigspec_to_node_name sigspec =
  match sigspec with
  | SigWire name -> name
  | SigBit (name, bit) -> Printf.sprintf "%s[%d]" name bit
  | SigRange (name, hi, lo) -> Printf.sprintf "%s[%d:%d]" name hi lo
  | SigConst value -> Printf.sprintf "const_%s" value
  | SigConcat specs -> "concat"  (* TODO: handle properly *)

(* Convert RTLIL cell type to IR operation *)
let cell_to_ir_operation cell =
  match cell.cell_type with
  (* Single-bit gate cells *)
  | "$_AND_" -> Some (And { width = 1 })
  | "$_NAND_" -> Some (And { width = 1 })  (* Will need NOT wrapper *)
  | "$_OR_" -> Some (Or { width = 1 })
  | "$_NOR_" -> Some (Or { width = 1 })    (* Will need NOT wrapper *)
  | "$_XOR_" -> Some (Xor { width = 1 })
  | "$_XNOR_" -> Some (Xor { width = 1 })  (* Will need NOT wrapper *)
  | "$_NOT_" -> Some (Not { width = 1 })
  | "$_BUF_" -> Some (And { width = 1 })   (* Buffer as AND with constant 1 *)

  (* Special gates *)
  | "$_ANDNOT_" -> Some (And { width = 1 })  (* A & !B - need preprocessing *)
  | "$_ORNOT_" -> Some (Or { width = 1 })    (* A | !B - need preprocessing *)

  (* Multiplexer *)
  | "$_MUX_" -> Some (Mux { width = 1 })

  (* Multi-bit arithmetic *)
  | "$add" ->
      let width = get_param cell "Y_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Add { width; signed })

  | "$sub" ->
      let width = get_param cell "Y_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Sub { width; signed })

  | "$mul" ->
      let width = get_param cell "Y_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Mul { width; signed })

  | "$div" ->
      let width = get_param cell "Y_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Div { width; signed })

  (* Multi-bit logic *)
  | "$and" ->
      let width = get_param cell "Y_WIDTH" in
      Some (And { width })

  | "$or" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Or { width })

  | "$xor" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Xor { width })

  | "$not" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Not { width })

  (* Comparison *)
  | "$eq" ->
      let width = get_param cell "A_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Compare { width; cmp_op = `Eq; signed })

  | "$ne" ->
      let width = get_param cell "A_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Compare { width; cmp_op = `Ne; signed })

  | "$lt" ->
      let width = get_param cell "A_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Compare { width; cmp_op = `Lt; signed })

  | "$le" ->
      let width = get_param cell "A_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Compare { width; cmp_op = `Le; signed })

  | "$gt" ->
      let width = get_param cell "A_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Compare { width; cmp_op = `Gt; signed })

  | "$ge" ->
      let width = get_param cell "A_WIDTH" in
      let signed = get_param_bool cell "A_SIGNED" in
      Some (Compare { width; cmp_op = `Ge; signed })

  (* Shift operations *)
  | "$shl" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Shift { width; direction = `Left; arithmetic = false; amount = None })

  | "$shr" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Shift { width; direction = `Right; arithmetic = false; amount = None })

  | "$sshl" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Shift { width; direction = `Left; arithmetic = true; amount = None })

  | "$sshr" ->
      let width = get_param cell "Y_WIDTH" in
      Some (Shift { width; direction = `Right; arithmetic = true; amount = None })

  (* Multiplexer *)
  | "$mux" ->
      let width = get_param cell "WIDTH" in
      Some (Mux { width })

  (* Flip-flops - all DFF variants *)
  | ct when String.length ct >= 4 && String.sub ct 0 4 = "$_DF" ->
      (* TODO: Extract clock, reset, enable from connections *)
      Some (Register { width = 1; clock = 0; reset = None; enable = None; reset_value = 0 })

  | "$dff" ->
      let width = get_param cell "WIDTH" in
      (* TODO: Extract clock from connections *)
      Some (Register { width; clock = 0; reset = None; enable = None; reset_value = 0 })

  (* Unsupported or unknown cell types *)
  | _ -> None

(* Find connection by pin name *)
let find_connection cell pin_name =
  try
    let conn = List.find (fun c -> c.conn_pin = pin_name) cell.cell_conns in
    Some conn.conn_sig
  with Not_found -> None

(* Get width from wire *)
let get_wire_width wires wire_name =
  try
    let wire = List.find (fun w -> w.wire_name = wire_name) wires in
    wire.wire_width
  with Not_found -> 1

(* Convert RTLIL module to IR *)
let rtlil_module_to_ir rtlil_module =
  let ir = Sv_opt_ir.create_ir rtlil_module.mod_name in

  (* Track wire name to node ID mapping *)
  let wire_to_id = Hashtbl.create 100 in

  (* Add inputs *)
  List.iter (fun wire ->
    match wire.wire_port with
    | Some (RInput, _) ->
        let id = Sv_opt_ir.add_input ir wire.wire_name wire.wire_width in
        Hashtbl.add wire_to_id wire.wire_name id
    | _ -> ()
  ) rtlil_module.mod_wires;

  (* Add outputs *)
  List.iter (fun wire ->
    match wire.wire_port with
    | Some (ROutput, _) ->
        let id = Sv_opt_ir.add_output ir wire.wire_name wire.wire_width in
        Hashtbl.add wire_to_id wire.wire_name id
    | _ -> ()
  ) rtlil_module.mod_wires;

  (* Process cells in topological order (multi-pass until all processed) *)
  let remaining_cells = ref rtlil_module.mod_cells in
  let pass = ref 0 in
  while !remaining_cells <> [] && !pass < 10 do
    pass := !pass + 1;
    let still_remaining = ref [] in
    List.iter (fun cell ->
    match cell_to_ir_operation cell with
    | Some op ->
        (* Extract input signals based on cell type *)
        let input_ids = match cell.cell_type with
          (* Standard two-input gates *)
          | "$_AND_" | "$_NAND_" | "$_OR_" | "$_NOR_" | "$_XOR_" | "$_XNOR_" ->
              let a_sig = find_connection cell "A" in
              let b_sig = find_connection cell "B" in
              (match a_sig, b_sig with
               | Some a, Some b ->
                   let a_name = sigspec_to_node_name a in
                   let b_name = sigspec_to_node_name b in
                   (match Hashtbl.find_opt wire_to_id a_name,
                          Hashtbl.find_opt wire_to_id b_name with
                    | Some aid, Some bid -> [aid; bid]
                    | _ -> [])
               | _ -> [])

          (* Single-input gates *)
          | "$_NOT_" | "$_BUF_" ->
              let i_sig = find_connection cell "I" in
              (match i_sig with
               | Some i ->
                   let i_name = sigspec_to_node_name i in
                   (match Hashtbl.find_opt wire_to_id i_name with
                    | Some iid -> [iid]
                    | None -> [])
               | None -> [])

          (* Arithmetic/logic operations *)
          | "$add" | "$sub" | "$mul" | "$div" | "$and" | "$or" | "$xor"
          | "$eq" | "$ne" | "$lt" | "$le" | "$gt" | "$ge" ->
              let a_sig = find_connection cell "A" in
              let b_sig = find_connection cell "B" in
              (match a_sig, b_sig with
               | Some a, Some b ->
                   let a_name = sigspec_to_node_name a in
                   let b_name = sigspec_to_node_name b in
                   (match Hashtbl.find_opt wire_to_id a_name,
                          Hashtbl.find_opt wire_to_id b_name with
                    | Some aid, Some bid -> [aid; bid]
                    | _ -> [])
               | _ -> [])

          (* Multiplexer *)
          | "$_MUX_" | "$mux" ->
              let a_sig = find_connection cell "A" in
              let b_sig = find_connection cell "B" in
              let s_sig = find_connection cell "S" in
              (match a_sig, b_sig, s_sig with
               | Some a, Some b, Some s ->
                   let a_name = sigspec_to_node_name a in
                   let b_name = sigspec_to_node_name b in
                   let s_name = sigspec_to_node_name s in
                   (match Hashtbl.find_opt wire_to_id a_name,
                          Hashtbl.find_opt wire_to_id b_name,
                          Hashtbl.find_opt wire_to_id s_name with
                    | Some aid, Some bid, Some sid -> [sid; aid; bid]  (* sel, true, false *)
                    | _ -> [])
               | _ -> [])

          (* Flip-flops *)
          | ct when String.length ct >= 4 && String.sub ct 0 4 = "$_DF" ->
              let d_sig = find_connection cell "D" in
              (match d_sig with
               | Some d ->
                   let d_name = sigspec_to_node_name d in
                   (match Hashtbl.find_opt wire_to_id d_name with
                    | Some did -> [did]
                    | None -> [])
               | None -> [])

          | _ -> []
        in

        (* Add node to IR if we have valid inputs *)
        if input_ids <> [] then begin
          let node_id = Sv_opt_ir.add_node ir op input_ids in

          (* Map output signal to this node *)
          let out_sig = match cell.cell_type with
            | "$_DFF_P_" | "$_DFF_N_" -> find_connection cell "Q"
            | _ -> find_connection cell "Y"
          in
          (match out_sig with
           | Some signal_spec ->
               let out_name = sigspec_to_node_name signal_spec in
               Hashtbl.add wire_to_id out_name node_id;
               (* If this is an output wire, map output ID to node ID *)
               (match Hashtbl.find_opt ir.ir_outputs out_name with
                | Some (Output { id; _ }) ->
                    Hashtbl.add ir.ir_value_to_node id node_id
                | _ -> ())
           | None -> ())
        end else begin
          (* Inputs not ready, defer to next pass *)
          still_remaining := cell :: !still_remaining
        end
    | None ->
        Printf.eprintf "Warning: Unsupported cell type: %s\n" cell.cell_type
    ) !remaining_cells;
    remaining_cells := List.rev !still_remaining
  done;

  (* Process top-level connections (assigns, bit-select, range operations) *)
  List.iter (fun (lhs_sig, rhs_sig) ->
    let lhs_name = sigspec_to_node_name lhs_sig in
    let _rhs_name = sigspec_to_node_name rhs_sig in

    (* Find the source node ID *)
    let source_id_opt = match rhs_sig with
      | SigWire wire_name ->
          Hashtbl.find_opt wire_to_id wire_name
      | SigBit (wire_name, bit) ->
          (* Create Extract node for bit select *)
          (match Hashtbl.find_opt wire_to_id wire_name with
           | Some src_id ->
               let extract_op = Extract { width = 1; lsb = bit; msb = bit } in
               let node_id = Sv_opt_ir.add_node ir extract_op [src_id] in
               Hashtbl.add wire_to_id lhs_name node_id;
               Some node_id
           | None -> None)
      | SigRange (wire_name, hi, lo) ->
          (* Create Extract node for range *)
          (match Hashtbl.find_opt wire_to_id wire_name with
           | Some src_id ->
               let width = abs (hi - lo) + 1 in
               let lsb = min hi lo in
               let msb = max hi lo in
               let extract_op = Extract { width; lsb; msb } in
               let node_id = Sv_opt_ir.add_node ir extract_op [src_id] in
               Hashtbl.add wire_to_id lhs_name node_id;
               Some node_id
           | None -> None)
      | _ -> None
    in

    (* If LHS is an output, update ir_value_to_node *)
    (match source_id_opt with
     | Some node_id ->
         (match Hashtbl.find_opt ir.ir_outputs lhs_name with
          | Some (Output { id; _ }) ->
              Hashtbl.add ir.ir_value_to_node id node_id
          | _ -> ())
     | None -> ())
  ) rtlil_module.mod_connects;

  ir

(* Convert RTLIL design to IR (takes first module) *)
let rtlil_design_to_ir design =
  match design.design_modules with
  | module_def :: _ -> Some (rtlil_module_to_ir module_def)
  | [] -> None

(* Print IR summary *)
let print_ir_summary ir =
  Printf.printf "IR Module: %s\n" ir.ir_name;
  Printf.printf "  Inputs: %d\n" (Hashtbl.length ir.ir_inputs);
  Printf.printf "  Outputs: %d\n" (Hashtbl.length ir.ir_outputs);
  Printf.printf "  Nodes: %d\n" (Hashtbl.length ir.ir_nodes);
  Printf.printf "  Constants: %d\n" (Hashtbl.length ir.ir_constants)
