(* Gate-level mapping module *)
(* Maps HardCaml operations to standard cells from Liberty files *)

open Sv_liberty

(* Gate mapping types *)
type gate_instance = {
  inst_name: string;
  cell_name: string;
  connections: (string * string) list; (* pin_name -> signal_name *)
}

type mapped_netlist = {
  module_name: string;
  inputs: (string * int) list;  (* name, width *)
  outputs: (string * int) list;
  wires: (string * int) list;
  instances: gate_instance list;
}

(* Operation to cell mapping *)
type operation_mapping = {
  op_type: string; (* AND, OR, XOR, NOT, etc *)
  preferred_cells: string list; (* ordered by preference *)
}

(* Find suitable cell for an operation *)
let find_cell_for_op lib op_type =
  let matches = ref [] in
  Hashtbl.iter (fun name cell ->
    if cell.cell_type = "combinational" then
      (* Check if cell matches the operation *)
      let output_pins = List.filter (fun p -> p.direction = Output) cell.pins in
      List.iter (fun pin ->
        match pin.function_expr with
        | Some func ->
            (* Simple heuristic: check if function contains the operator *)
            let func_lower = String.lowercase_ascii func in
            let matches_op = match op_type with
              | "AND" -> String.contains func_lower '&' && not (String.contains func_lower '|')
              | "OR" -> String.contains func_lower '|' && not (String.contains func_lower '&')
              | "XOR" -> String.contains func_lower '^'
              | "NOT" -> String.contains func_lower '!'
              | "BUF" -> not (String.contains func_lower '&' ||
                              String.contains func_lower '|' ||
                              String.contains func_lower '!' ||
                              String.contains func_lower '^')
              | _ -> false
            in
            if matches_op then matches := (name, cell) :: !matches
        | None -> ()
      ) output_pins
  ) lib.cells;
  match !matches with
  | (name, cell) :: _ -> Some (name, cell)
  | [] -> None

(* Get input/output pins from cell *)
let get_input_pins cell =
  List.filter (fun p -> p.direction = Input) cell.pins

let get_output_pins cell =
  List.filter (fun p -> p.direction = Output) cell.pins

(* Generate unique signal name *)
let gensym =
  let counter = ref 0 in
  fun prefix ->
    incr counter;
    Printf.sprintf "%s_%d" prefix !counter

(* Map a single-bit operation to a gate *)
let map_operation lib op_type input_signals output_signal =
  match find_cell_for_op lib op_type with
  | Some (cell_name, cell) ->
      let inst_name = gensym (String.lowercase_ascii cell_name) in
      let input_pins = get_input_pins cell in
      let output_pins = get_output_pins cell in

      (* Connect inputs *)
      let connections = ref [] in
      List.iteri (fun i pin ->
        if i < List.length input_signals then
          connections := (pin.name, List.nth input_signals i) :: !connections
      ) input_pins;

      (* Connect output *)
      if output_pins <> [] then
        connections := ((List.hd output_pins).name, output_signal) :: !connections;

      Some {
        inst_name;
        cell_name;
        connections = List.rev !connections;
      }
  | None -> None

(* Map a multi-bit operation to multiple gates *)
let map_multibit_operation lib op_type input_signals_list output_signal width =
  let instances = ref [] in
  let wires = ref [] in

  for _i = 0 to width - 1 do
    let bit_inputs = List.map (fun _sigs ->
      let sig_name = gensym "wire" in
      wires := (sig_name, 1) :: !wires;
      sig_name
    ) input_signals_list in

    let bit_output = gensym "wire" in
    wires := (bit_output, 1) :: !wires;

    match map_operation lib op_type bit_inputs bit_output with
    | Some inst -> instances := inst :: !instances
    | None -> ()
  done;

  (!instances, !wires)

(* Generate Verilog for gate instance *)
let verilog_of_instance inst =
  let conn_str = String.concat ", " (List.map (fun (pin, signal) ->
    Printf.sprintf ".%s(%s)" pin signal
  ) inst.connections) in
  Printf.sprintf "  %s %s (%s);" inst.cell_name inst.inst_name conn_str

(* Generate Verilog module from mapped netlist *)
let verilog_of_mapped_netlist netlist =
  let buf = Buffer.create 1024 in

  (* Module header *)
  Buffer.add_string buf (Printf.sprintf "module %s (\n" netlist.module_name);

  (* Ports *)
  let all_ports = netlist.inputs @ netlist.outputs in
  let port_names = String.concat ",\n  " (List.map fst all_ports) in
  Buffer.add_string buf (Printf.sprintf "  %s\n);\n\n" port_names);

  (* Input declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  input %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  input [%d:0] %s;\n" (width-1) name)
  ) netlist.inputs;

  (* Output declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  output %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  output [%d:0] %s;\n" (width-1) name)
  ) netlist.outputs;

  Buffer.add_string buf "\n";

  (* Wire declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  wire %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  wire [%d:0] %s;\n" (width-1) name)
  ) netlist.wires;

  Buffer.add_string buf "\n";

  (* Instances *)
  List.iter (fun inst ->
    Buffer.add_string buf (verilog_of_instance inst);
    Buffer.add_string buf "\n"
  ) netlist.instances;

  Buffer.add_string buf "\nendmodule\n";
  Buffer.contents buf

(* Create a simple test netlist *)
let create_test_netlist lib =
  match find_cell_for_op lib "AND" with
  | Some (cell_name, cell) ->
      let inst = {
        inst_name = "u1";
        cell_name;
        connections = [("A1", "a"); ("A2", "b"); ("ZN", "y")];
      } in
      {
        module_name = "test_and";
        inputs = [("a", 1); ("b", 1)];
        outputs = [("y", 1)];
        wires = [];
        instances = [inst];
      }
  | None ->
      failwith "No AND cell found in library"
