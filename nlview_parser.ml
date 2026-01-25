(* Nlview Schematic Parser - Memory-efficient version *)

type pin_direction = Input | Output | Inout

type pin = {
  name: string;
  direction: pin_direction;
  is_bus: bool;
  range: (int * int) option;
}

type port = {
  port_name: string;
  port_direction: pin_direction;
  port_is_bus: bool;
  port_range: (int * int) option;
}

type net_connection = {
  inst_name: string;
  pin_name: string;
}

type net = {
  net_name: string;
  is_bus: bool;
  connections: net_connection list;
  port_connection: string option;
}

type instance = {
  inst_name: string;
  inst_type: string;
  inst_library: string;
}

type schematic = {
  filename: string;
  module_name: string;
  num_ports: int;
  num_instances: int;
  num_nets: int;
}

(* Simple tokenizer using String functions *)
let tokenize_line line =
  let len = String.length line in
  let rec scan i acc current =
    if i >= len then
      if Buffer.length current > 0 then
        Buffer.contents current :: acc
      else acc
    else
      let c = line.[i] in
      match c with
      | ' ' | '\t' ->
          if Buffer.length current > 0 then
            scan (i + 1) (Buffer.contents current :: acc) (Buffer.create 16)
          else
            scan (i + 1) acc current
      | _ ->
          Buffer.add_char current c;
          scan (i + 1) acc current
  in
  List.rev (scan 0 [] (Buffer.create 16))

(* Parse range [7:0] *)
let parse_range str =
  try
    if String.length str > 2 && str.[0] = '[' && str.[String.length str - 1] = ']' then
      let inner = String.sub str 1 (String.length str - 2) in
      Scanf.sscanf inner "%d:%d" (fun h l -> Some (h, l))
    else None
  with _ -> None

(* Main parser - just count items *)
let parse_schematic filename =
  let ic = open_in filename in
  let module_name = ref "" in
  let num_ports = ref 0 in
  let num_instances = ref 0 in
  let num_nets = ref 0 in

  try
    while true do
      let line = input_line ic in
      let trimmed = String.trim line in

      if trimmed <> "" && trimmed.[0] <> '#' then begin
        (* Just count and extract key info *)
        if String.starts_with ~prefix:"module new" trimmed then begin
          let tokens = tokenize_line trimmed in
          match tokens with
          | "module" :: "new" :: name :: _ -> module_name := name
          | _ -> ()
        end
        else if String.starts_with ~prefix:"load port" trimmed then
          incr num_ports
        else if String.starts_with ~prefix:"load portBus" trimmed then
          incr num_ports
        else if String.starts_with ~prefix:"load inst" trimmed then
          incr num_instances
        else if String.starts_with ~prefix:"load net" trimmed then
          incr num_nets
      end
    done;
    assert false
  with End_of_file ->
    close_in ic;
    {
      filename;
      module_name = !module_name;
      num_ports = !num_ports;
      num_instances = !num_instances;
      num_nets = !num_nets;
    }

(* Print statistics *)
let print_stats sch =
  Printf.printf "Schematic: %s\n" sch.filename;
  Printf.printf "Module: %s\n" sch.module_name;
  Printf.printf "Ports: %d\n" sch.num_ports;
  Printf.printf "Instances: %d\n" sch.num_instances;
  Printf.printf "Nets: %d\n" sch.num_nets;
  Printf.printf "\n"

(* Extract specific information on demand *)
let get_ports filename =
  let ic = open_in filename in
  let ports = ref [] in
  try
    while true do
      let line = input_line ic in
      let trimmed = String.trim line in

      if String.starts_with ~prefix:"load port" trimmed then begin
        let tokens = tokenize_line trimmed in
        match tokens with
        | "load" :: "port" :: name :: dir :: _ ->
            let direction = if dir = "input" then Input else Output in
            ports := (name, { port_name = name; port_direction = direction;
                             port_is_bus = false; port_range = None }) :: !ports
        | _ -> ()
      end
      else if String.starts_with ~prefix:"load portBus" trimmed then begin
        let tokens = tokenize_line trimmed in
        match tokens with
        | "load" :: "portBus" :: name :: dir :: range_str :: _ ->
            let direction = if dir = "input" then Input else Output in
            let range = parse_range range_str in
            ports := (name, { port_name = name; port_direction = direction;
                             port_is_bus = true; port_range = range }) :: !ports
        | _ -> ()
      end
    done;
    assert false
  with End_of_file ->
    close_in ic;
    List.rev !ports

let get_instances filename =
  let ic = open_in filename in
  let instances = ref [] in
  try
    while true do
      let line = input_line ic in
      let trimmed = String.trim line in

      if String.starts_with ~prefix:"load inst" trimmed then begin
        let tokens = tokenize_line trimmed in
        match tokens with
        | "load" :: "inst" :: name :: inst_type :: rest ->
            let library = if List.length rest > 0 then List.hd rest else "" in
            instances := (name, { inst_name = name; inst_type; inst_library = library }) :: !instances
        | _ -> ()
      end
    done;
    assert false
  with End_of_file ->
    close_in ic;
    List.rev !instances
