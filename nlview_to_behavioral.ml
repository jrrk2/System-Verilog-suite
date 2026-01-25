(* Convert Nlview schematic to Behavioral IR *)

open Behavioral_ir

(* Enhanced parser to extract full netlist *)
module NetlistParser = struct
  type net_pin = {
    inst: string;
    pin: string;
  }

  type net_info = {
    name: string;
    is_bus: bool;
    connections: net_pin list;
    port: string option;
  }

  type instance_info = {
    name: string;
    cell_type: string;
    library: string;
  }

  type port_info = {
    name: string;
    direction: [`Input | `Output];
    width: int;
  }

  (* Extract net connectivity *)
  let get_nets filename =
    let ic = open_in filename in
    let nets = ref [] in
    try
      while true do
        let line = input_line ic in
        let trimmed = String.trim line in

        if String.starts_with ~prefix:"load net" trimmed then begin
          let tokens = Nlview_parser.tokenize_line trimmed in
          match tokens with
          | "load" :: net_type :: net_name :: rest ->
              let is_bus = (net_type = "netBus") in

              (* Extract pin connections *)
              let rec extract_pins tokens acc port_conn =
                match tokens with
                | "-pin" :: inst :: pin :: rest ->
                    extract_pins rest ({ inst; pin } :: acc) port_conn
                | "-port" :: port :: rest ->
                    extract_pins rest acc (Some port)
                | _ :: rest -> extract_pins rest acc port_conn
                | [] -> (List.rev acc, port_conn)
              in

              let (connections, port) = extract_pins rest [] None in
              nets := { name = net_name; is_bus; connections; port } :: !nets
          | _ -> ()
        end
      done;
      assert false
    with End_of_file ->
      close_in ic;
      List.rev !nets

  (* Get port info with widths *)
  let get_port_info filename =
    let ic = open_in filename in
    let ports = ref [] in
    try
      while true do
        let line = input_line ic in
        let trimmed = String.trim line in

        if String.starts_with ~prefix:"load port" trimmed then begin
          let tokens = Nlview_parser.tokenize_line trimmed in
          match tokens with
          | "load" :: "port" :: name :: dir :: _ ->
              let direction = if dir = "input" then `Input else `Output in
              ports := { name; direction; width = 1 } :: !ports
          | "load" :: "portBus" :: name :: dir :: range_str :: _ ->
              let direction = if dir = "input" then `Input else `Output in
              let width = match Nlview_parser.parse_range range_str with
                | Some (h, l) -> h - l + 1
                | None -> 1
              in
              ports := { name; direction; width } :: !ports
          | _ -> ()
        end
      done;
      assert false
    with End_of_file ->
      close_in ic;
      List.rev !ports

  (* Get instance info *)
  let get_instance_info filename =
    let ic = open_in filename in
    let instances = ref [] in
    try
      while true do
        let line = input_line ic in
        let trimmed = String.trim line in

        if String.starts_with ~prefix:"load inst" trimmed then begin
          let tokens = Nlview_parser.tokenize_line trimmed in
          match tokens with
          | "load" :: "inst" :: name :: cell_type :: rest ->
              let library = if List.length rest > 0 then List.hd rest else "" in
              instances := { name; cell_type; library } :: !instances
          | _ -> ()
        end
      done;
      assert false
    with End_of_file ->
      close_in ic;
      List.rev !instances
end

(* Map RTL primitives to behavioral expressions *)
let map_rtl_primitive cell_type inputs =
  match cell_type with
  | "RTL_AND" | "AND" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BAnd; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_OR" | "OR" | "RTL_OR4" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BOr; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_XOR" | "XOR" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BXor; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_NOT" | "NOT" ->
      (match inputs with
       | [a] -> Some (BUnOp { op = BNot; operand = a;
                              result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_ADD" | "RTL_ADD2" | "RTL_ADD4" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BAdd; lhs = a; rhs = b;
                                  result_type = BInt { width = 32; signed = Unsigned } })
       | _ -> None)

  | "RTL_SUB" | "RTL_SUB4" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BSub; lhs = a; rhs = b;
                                  result_type = BInt { width = 32; signed = Unsigned } })
       | _ -> None)

  | "RTL_EQ" | "RTL_EQ25" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BEq; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_NEQ" | "RTL_NEQ3" | "RTL_NEQ6" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BNe; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_MUX" | "RTL_MUX1" | "RTL_MUX16" | "RTL_MUX86" | "RTL_MUX167"
  | "RTL_MUX196" | "RTL_MUX198" | "RTL_MUX219" | "RTL_MUX256" | "RTL_MUX270" ->
      (match inputs with
       | sel :: i0 :: i1 :: _ ->
           Some (BCond { condition = sel; then_val = i1; else_val = i0 })
       | _ -> None)

  | "BUF" | "OBUF" ->
      (match inputs with
       | [a] -> Some a
       | _ -> None)

  | _ ->
      (* Unknown primitive - treat as passthrough or placeholder *)
      None

(* Convert schematic to Behavioral IR *)
let convert filename =
  let sch = Nlview_parser.parse_schematic filename in

  (* Extract detailed information *)
  let ports = NetlistParser.get_port_info filename in
  let inst_list = NetlistParser.get_instance_info filename in
  let nets = NetlistParser.get_nets filename in

  (* Create signals *)
  let signals =
    (* Ports *)
    List.map (fun p ->
      {
        name = p.NetlistParser.name;
        stype = BInt { width = p.width; signed = Unsigned };
        direction = (p.direction :> [`Input | `Output | `Internal]);
        initial_value = None;
      }
    ) ports
    @
    (* Internal nets *)
    List.filter_map (fun n ->
      if n.NetlistParser.port = None then
        Some {
          name = n.name;
          stype = BInt { width = if n.is_bus then 32 else 1; signed = Unsigned };
          direction = `Internal;
          initial_value = None;
        }
      else None
    ) nets
  in

  (* Build instance map for quick lookup *)
  let inst_map : (string, NetlistParser.instance_info) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun (i : NetlistParser.instance_info) -> Hashtbl.add inst_map i.NetlistParser.name i) inst_list;

  (* Build net map: pin -> net name *)
  let pin_to_net = Hashtbl.create 1024 in
  List.iter (fun (net : NetlistParser.net_info) ->
    List.iter (fun (pin : NetlistParser.net_pin) ->
      Hashtbl.add pin_to_net (pin.NetlistParser.inst, pin.pin) net.NetlistParser.name
    ) net.connections
  ) nets;

  (* Helper: get input expressions for an instance *)
  let get_inputs inst_name =
    let pins = Hashtbl.find_all pin_to_net (inst_name, "I0") @
               Hashtbl.find_all pin_to_net (inst_name, "I1") @
               Hashtbl.find_all pin_to_net (inst_name, "I") @
               Hashtbl.find_all pin_to_net (inst_name, "D") in
    List.map (fun net -> BVar net) pins
  in

  (* Helper: get output net for an instance *)
  let get_output inst_name =
    try
      Some (List.hd (Hashtbl.find_all pin_to_net (inst_name, "O") @
                     Hashtbl.find_all pin_to_net (inst_name, "Q")))
    with _ -> None
  in

  (* Create continuous assignments for primitives *)
  let assignments = ref [] in
  List.iter (fun (inst : NetlistParser.instance_info) ->
    match get_output inst.NetlistParser.name with
    | Some output ->
        let inputs = get_inputs inst.name in
        (match map_rtl_primitive inst.cell_type inputs with
         | Some expr ->
             assignments := BAssign { lhs = output; rhs = expr } :: !assignments
         | None ->
             (* Keep as hierarchical instance or skip *)
             ())
    | None -> ()
  ) inst_list;

  (* Create a combinational process with all assignments *)
  let main_process = BCombinational {
    name = "main_logic";
    sensitivity = [BAny];
    body = List.rev !assignments;
  } in

  (* Identify hierarchical instances (non-RTL primitives) *)
  let hier_instances = List.filter_map (fun (inst : NetlistParser.instance_info) ->
    let is_primitive =
      String.starts_with ~prefix:"RTL_" inst.NetlistParser.cell_type ||
      inst.cell_type = "AND" || inst.cell_type = "OR" ||
      inst.cell_type = "BUF" || inst.cell_type = "OBUF"
    in
    if not is_primitive then
      Some {
        inst_name = inst.name;
        module_name = inst.cell_type;
        param_values = [];
        port_connections = [];  (* Would need to extract from nets *)
      }
    else None
  ) inst_list in

  (* Create module *)
  let bmod = {
    name = sch.Nlview_parser.module_name;
    params = [];
    signals;
    processes = [main_process];
    instances = hier_instances;
  } in

  (* Create program *)
  {
    modules = [bmod];
  }

(* Convert and print statistics *)
let convert_and_report filename =
  Printf.printf "Converting Nlview schematic to Behavioral IR: %s\n\n" filename;

  let prog = convert filename in
  let bmod = List.hd prog.modules in

  Printf.printf "Module: %s\n" bmod.name;
  Printf.printf "Signals: %d\n" (List.length bmod.signals);
  Printf.printf "  Inputs: %d\n" (List.length (List.filter (fun s -> s.direction = `Input) bmod.signals));
  Printf.printf "  Outputs: %d\n" (List.length (List.filter (fun s -> s.direction = `Output) bmod.signals));
  Printf.printf "  Internal: %d\n" (List.length (List.filter (fun s -> s.direction = `Internal) bmod.signals));
  Printf.printf "Processes: %d\n" (List.length bmod.processes);
  Printf.printf "Instances: %d\n" (List.length bmod.instances);

  (* Count assignments *)
  let count_assignments = List.fold_left (fun acc proc ->
    match proc with
    | BCombinational { body; _ } -> acc + List.length body
    | BSequential { body; _ } -> acc + List.length body
  ) 0 bmod.processes in
  Printf.printf "Assignments: %d\n" count_assignments;

  prog
