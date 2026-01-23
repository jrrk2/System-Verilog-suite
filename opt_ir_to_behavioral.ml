(* opt_ir_to_behavioral.ml - Convert opt_ir directly to behavioral Verilog *)

open Sv_ast

let debug = ref false
let use_meaningful_names = ref true

(* Convert an opt_ir operation to a behavioral expression string *)
let rec value_to_expr ir id_to_name id =
  let wire_name = match Hashtbl.find_opt id_to_name id with
    | Some name -> name
    | None -> Printf.sprintf "n%d" id
  in
  wire_name

let operation_to_expr ir id_to_name node =
  let get_input_name id = value_to_expr ir id_to_name id in

  match node.node_op with
  | Add { width; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s + %s" a b

  | Sub { width; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s - %s" a b

  | Mul { width; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s * %s" a b

  | And { width } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s & %s" a b

  | Or { width } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s | %s" a b

  | Xor { width } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s ^ %s" a b

  | Not { width } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      Printf.sprintf "~%s" a

  | Compare { width; cmp_op = `Eq; signed } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s == %s" a b

  | Compare { width; cmp_op = `Ne; signed } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s != %s" a b

  | Compare { width; cmp_op = `Lt; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s < %s" a b

  | Compare { width; cmp_op = `Le; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s <= %s" a b

  | Compare { width; cmp_op = `Gt; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s > %s" a b

  | Compare { width; cmp_op = `Ge; signed = false } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      let b = get_input_name (List.nth node.node_inputs 1) in
      Printf.sprintf "%s >= %s" a b

  | Mux { width } ->
      let sel = get_input_name (List.nth node.node_inputs 0) in
      let in0 = get_input_name (List.nth node.node_inputs 1) in
      let in1 = get_input_name (List.nth node.node_inputs 2) in
      Printf.sprintf "%s ? %s : %s" sel in1 in0

  | ZeroExtend { from_width; to_width } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      Printf.sprintf "{{%d{1'b0}}, %s}" (to_width - from_width) a

  | SignExtend { from_width; to_width } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      Printf.sprintf "{{%d{%s[%d]}}, %s}" (to_width - from_width) a (from_width - 1) a

  | Extract { width; lsb; msb } ->
      let a = get_input_name (List.nth node.node_inputs 0) in
      if lsb = msb then
        Printf.sprintf "%s[%d]" a lsb
      else
        Printf.sprintf "%s[%d:%d]" a msb lsb

  | Concat { widths } ->
      let inputs = List.map get_input_name node.node_inputs in
      Printf.sprintf "{%s}" (String.concat ", " inputs)

  | Register { width; reset_value } ->
      (* Registers need special handling - return the input for now *)
      get_input_name (List.nth node.node_inputs 3) (* d input *)

  | _ ->
      (* Fallback for unsupported operations *)
      let inputs = List.map get_input_name node.node_inputs in
      Printf.sprintf "/* unsupported op */ %s" (String.concat " " inputs)

(* Separate combinational and sequential logic *)
type behavioral_module = {
  module_name: string;
  inputs: (string * int) list;
  outputs: (string * int) list;
  wires: (string * int) list;
  combinational: (string * string) list;  (* output_name, expression *)
  registers: (string * string * string * string * int) list;  (* reg_name, clk, rst, d_expr, reset_val *)
}

(* Convert opt_ir to behavioral representation *)
let convert_to_behavioral ir =
  if !debug then Printf.eprintf "\n=== Converting Opt IR to Behavioral ===\n";

  (* Generate meaningful names for all nodes *)
  let id_to_name =
    if !use_meaningful_names then
      Opt_ir_naming.apply_naming_strategy ~verbose:!debug ir
    else
      let tbl = Hashtbl.create 200 in
      (* Map input/output IDs to their names *)
      Hashtbl.iter (fun name value ->
        match value with
        | Input { id; _ } -> Hashtbl.add tbl id name
        | _ -> ()
      ) ir.ir_inputs;
      Hashtbl.iter (fun name value ->
        match value with
        | Output { id; _ } -> Hashtbl.add tbl id name
        | _ -> ()
      ) ir.ir_outputs;
      tbl
  in

  let combinational = ref [] in
  let registers = ref [] in
  let wires = ref [] in

  (* Process all nodes *)
  Hashtbl.iter (fun id node ->
    let wire_name = match Hashtbl.find_opt id_to_name id with
      | Some name -> name
      | None -> Printf.sprintf "n%d" id
    in

    let width = match node.node_output with
      | Wire { width; _ } -> width
      | _ -> 32
    in

    (* Check if this is a register *)
    match node.node_op with
    | Register { width; clock; reset; enable; reset_value } ->
        (* Extract register inputs *)
        let clk_name = value_to_expr ir id_to_name clock in
        let rst_name = match reset with
          | Some r -> value_to_expr ir id_to_name r
          | None -> "1'b0" (* No reset *)
        in
        let d_name = value_to_expr ir id_to_name (List.nth node.node_inputs 0) in

        wires := (wire_name, width) :: !wires;
        registers := (wire_name, clk_name, rst_name, d_name, reset_value) :: !registers;

        if !debug then
          Printf.eprintf "Register: %s <= %s (clk=%s, rst=%s, reset_val=%d)\n"
            wire_name d_name clk_name rst_name reset_value

    | _ ->
        (* Combinational logic *)
        let expr = operation_to_expr ir id_to_name node in
        wires := (wire_name, width) :: !wires;
        combinational := (wire_name, expr) :: !combinational;

        if !debug then
          Printf.eprintf "Combinational: %s = %s\n" wire_name expr
  ) ir.ir_nodes;

  (* Handle constants from ir_constants hashtable *)
  Hashtbl.iter (fun const_value value_id ->
    let wire_name = match Hashtbl.find_opt id_to_name value_id with
      | Some name -> name
      | None -> Printf.sprintf "const_%d" value_id
    in
    if not (Hashtbl.mem id_to_name value_id) then
      Hashtbl.add id_to_name value_id wire_name;

    (* Look up the actual constant value *)
    let width = 32 in (* Default width - may need to infer from usage *)
    let expr = Printf.sprintf "%d'd%d" width const_value in
    combinational := (wire_name, expr) :: !combinational;

    if !debug then
      Printf.eprintf "Constant: %s = %s\n" wire_name expr
  ) ir.ir_constants;

  (* Extract inputs and outputs *)
  let inputs = Hashtbl.fold (fun name value acc ->
    match value with
    | Input { width; _ } -> (name, width) :: acc
    | _ -> acc
  ) ir.ir_inputs [] in

  let outputs = Hashtbl.fold (fun name value acc ->
    match value with
    | Output { id; width } ->
        (* Add assignment from the wire with the same id to output *)
        let driver_name = value_to_expr ir id_to_name id in
        combinational := (name, driver_name) :: !combinational;
        (name, width) :: acc
    | _ -> acc
  ) ir.ir_outputs [] in

  {
    module_name = ir.ir_name;
    inputs = List.sort (fun (a, _) (b, _) -> String.compare a b) inputs;
    outputs = List.sort (fun (a, _) (b, _) -> String.compare a b) outputs;
    wires = List.sort (fun (a, _) (b, _) -> String.compare a b) !wires;
    combinational = List.sort (fun (a, _) (b, _) -> String.compare a b) !combinational;
    registers = List.sort (fun (a, _, _, _, _) (b, _, _, _, _) -> String.compare a b) !registers;
  }

(* Generate behavioral Verilog from behavioral module *)
let generate_behavioral_verilog bmod =
  let buf = Buffer.create 4096 in

  (* Module header *)
  Buffer.add_string buf (Printf.sprintf "module %s (\n" bmod.module_name);

  (* Ports *)
  let all_ports =
    (List.map (fun (n, _) -> "input " ^ n) bmod.inputs) @
    (List.map (fun (n, _) -> "output " ^ n) bmod.outputs)
  in
  Buffer.add_string buf (Printf.sprintf "  %s\n);\n\n" (String.concat ",\n  " all_ports));

  (* Input declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  input logic %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  input logic [%d:0] %s;\n" (width-1) name)
  ) bmod.inputs;

  (* Output declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  output logic %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  output logic [%d:0] %s;\n" (width-1) name)
  ) bmod.outputs;

  Buffer.add_string buf "\n";

  (* Wire declarations *)
  List.iter (fun (name, width) ->
    (* Skip if it's an input or output *)
    let is_port = List.mem_assoc name bmod.inputs || List.mem_assoc name bmod.outputs in
    if not is_port then begin
      if width = 1 then
        Buffer.add_string buf (Printf.sprintf "  logic %s;\n" name)
      else
        Buffer.add_string buf (Printf.sprintf "  logic [%d:0] %s;\n" (width-1) name)
    end
  ) bmod.wires;

  Buffer.add_string buf "\n";

  (* Sequential logic (registers) *)
  if List.length bmod.registers > 0 then begin
    (* Group registers by clock signal *)
    let clock_groups = Hashtbl.create 10 in
    List.iter (fun (reg_name, clk, rst, d_expr, reset_val) ->
      let key = (clk, rst) in
      let existing = try Hashtbl.find clock_groups key with Not_found -> [] in
      Hashtbl.replace clock_groups key ((reg_name, d_expr, reset_val) :: existing)
    ) bmod.registers;

    (* Generate always blocks for each clock/reset combination *)
    (* Sort clock groups by (clk, rst) for deterministic output *)
    let sorted_clock_groups =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) clock_groups []
      |> List.sort (fun ((c1, r1), _) ((c2, r2), _) ->
           let cmp = String.compare c1 c2 in
           if cmp = 0 then String.compare r1 r2 else cmp)
    in
    List.iter (fun ((clk, rst), regs) ->
      (* Sort registers within the group by name *)
      let sorted_regs = List.sort (fun (a, _, _) (b, _, _) -> String.compare a b) regs in
      Buffer.add_string buf (Printf.sprintf "  always @(posedge %s or posedge %s) begin\n" clk rst);
      Buffer.add_string buf (Printf.sprintf "    if (%s) begin\n" rst);
      List.iter (fun (reg_name, _, reset_val) ->
        Buffer.add_string buf (Printf.sprintf "      %s <= %d;\n" reg_name reset_val)
      ) sorted_regs;
      Buffer.add_string buf "    end else begin\n";
      List.iter (fun (reg_name, d_expr, _) ->
        Buffer.add_string buf (Printf.sprintf "      %s <= %s;\n" reg_name d_expr)
      ) sorted_regs;
      Buffer.add_string buf "    end\n";
      Buffer.add_string buf "  end\n\n";
    ) sorted_clock_groups;
  end;

  (* Combinational logic *)
  List.iter (fun (output, expr) ->
    Buffer.add_string buf (Printf.sprintf "  assign %s = %s;\n" output expr)
  ) bmod.combinational;

  Buffer.add_string buf "\nendmodule\n";
  Buffer.contents buf

(* Main conversion function *)
let convert ?(verbose=false) ir =
  debug := verbose;
  let bmod = convert_to_behavioral ir in
  generate_behavioral_verilog bmod
