(* Yosys RTLIL format reader *)
(* Simple line-based parser for RTLIL intermediate format *)

(* RTLIL data structures *)
type port_direction = RInput | ROutput | RInout

type rtlil_wire = {
  wire_name: string;
  wire_width: int;
  wire_port: (port_direction * int) option; (* direction and port number *)
  wire_signed: bool;
  wire_offset: int;
}

type signal_spec =
  | SigConst of string  (* bit vector constant *)
  | SigWire of string   (* wire reference *)
  | SigBit of string * int  (* wire[bit] *)
  | SigRange of string * int * int  (* wire[hi:lo] *)
  | SigConcat of signal_spec list  (* { a b c } *)

type rtlil_connection = {
  conn_pin: string;
  conn_sig: signal_spec;
}

type rtlil_cell = {
  cell_type: string;
  cell_inst: string;
  cell_params: (string * string) list;
  cell_conns: rtlil_connection list;
}

type rtlil_module = {
  mod_name: string;
  mod_wires: rtlil_wire list;
  mod_cells: rtlil_cell list;
  mod_connects: (signal_spec * signal_spec) list;
}

type rtlil_design = {
  design_modules: rtlil_module list;
  design_attrs: (string * string) list;
}

(* Parsing utilities *)
let strip_backslash s =
  if String.length s > 0 && s.[0] = '\\' then
    String.sub s 1 (String.length s - 1)
  else s

let parse_sigspec s =
  let s = String.trim s in
  if String.length s > 0 && s.[0] = '{' then
    (* TODO: Parse concatenation *)
    SigWire s
  else if String.contains s '[' then
    (* Bit select or range *)
    let parts = String.split_on_char '[' s in
    if List.length parts = 2 then
      let wire = strip_backslash (List.nth parts 0) in
      let bracket = List.nth parts 1 in
      let bracket = String.sub bracket 0 (String.length bracket - 1) in
      if String.contains bracket ':' then
        let range_parts = String.split_on_char ':' bracket in
        if List.length range_parts = 2 then
          SigRange (wire, int_of_string (List.nth range_parts 0),
                         int_of_string (List.nth range_parts 1))
        else SigWire (strip_backslash s)
      else
        SigBit (wire, int_of_string bracket)
    else SigWire (strip_backslash s)
  else if String.length s > 0 && (s.[0] = '\'' || (String.length s > 1 && s.[1] = '\'')) then
    SigConst s
  else
    SigWire (strip_backslash s)

(* Line-based parser *)
let parse_rtlil_file filename =
  let ic = open_in filename in
  let lines = ref [] in
  (try
    while true do
      lines := input_line ic :: !lines
    done
  with End_of_file -> close_in ic);
  let lines = List.rev !lines in

  let modules = ref [] in
  let current_module = ref None in
  let current_cell = ref None in
  let design_attrs = ref [] in

  let add_wire wire =
    match !current_module with
    | Some (name, wires, cells, conns) ->
        current_module := Some (name, wire :: wires, cells, conns)
    | None -> ()
  in

  let add_cell cell =
    match !current_module with
    | Some (name, wires, cells, conns) ->
        current_module := Some (name, wires, cell :: cells, conns)
    | None -> ()
  in

  let add_connection conn =
    match !current_module with
    | Some (name, wires, cells, conns) ->
        current_module := Some (name, wires, cells, conn :: conns)
    | None -> ()
  in

  let finish_cell () =
    match !current_cell with
    | Some cell ->
        add_cell cell;
        current_cell := None
    | None -> ()
  in

  let finish_module () =
    finish_cell ();
    match !current_module with
    | Some (name, wires, cells, conns) ->
        let module_def = {
          mod_name = name;
          mod_wires = List.rev wires;
          mod_cells = List.rev cells;
          mod_connects = List.rev conns;
        } in
        modules := module_def :: !modules;
        current_module := None
    | None -> ()
  in

  List.iter (fun line ->
    let line = String.trim line in
    if String.length line = 0 || line.[0] = '#' then ()
    else
      let tokens = Str.split (Str.regexp "[ \t]+") line in
      match tokens with
      | "attribute" :: rest ->
          (* attribute \name value *)
          if List.length rest >= 2 then
            let name = strip_backslash (List.nth rest 0) in
            let value = String.concat " " (List.tl rest) in
            design_attrs := (name, value) :: !design_attrs

      | "module" :: name :: _ ->
          finish_module ();
          current_module := Some (strip_backslash name, [], [], [])

      | "end" :: _ ->
          if !current_cell <> None then
            finish_cell ()
          else
            finish_module ()

      | "wire" :: rest ->
          let name = ref "" in
          let width = ref 1 in
          let port = ref None in
          let signed = ref false in
          let offset = ref 0 in

          let rec parse_wire_options = function
            | [] -> ()
            | "width" :: w :: rest ->
                width := int_of_string w;
                parse_wire_options rest
            | "input" :: n :: rest ->
                port := Some (RInput, int_of_string n);
                parse_wire_options rest
            | "output" :: n :: rest ->
                port := Some (ROutput, int_of_string n);
                parse_wire_options rest
            | "inout" :: n :: rest ->
                port := Some (RInout, int_of_string n);
                parse_wire_options rest
            | "signed" :: rest ->
                signed := true;
                parse_wire_options rest
            | "offset" :: o :: rest ->
                offset := int_of_string o;
                parse_wire_options rest
            | w :: _ ->
                name := strip_backslash w
          in
          parse_wire_options rest;

          if !name <> "" then
            add_wire {
              wire_name = !name;
              wire_width = !width;
              wire_port = !port;
              wire_signed = !signed;
              wire_offset = !offset;
            }

      | "cell" :: cell_type :: inst_name :: _ ->
          finish_cell ();
          current_cell := Some {
            cell_type = strip_backslash cell_type;
            cell_inst = strip_backslash inst_name;
            cell_params = [];
            cell_conns = [];
          }

      | "parameter" :: param_name :: rest ->
          (match !current_cell with
           | Some cell ->
               let value = String.concat " " rest in
               current_cell := Some {
                 cell with
                 cell_params = (strip_backslash param_name, value) :: cell.cell_params
               }
           | None -> ())

      | "connect" :: pin :: rest ->
          let signal_spec = parse_sigspec (String.concat " " rest) in
          (match !current_cell with
           | Some cell ->
               current_cell := Some {
                 cell with
                 cell_conns = {conn_pin = strip_backslash pin; conn_sig = signal_spec} :: cell.cell_conns
               }
           | None ->
               (* Top-level connect *)
               match !current_module with
               | Some _ ->
                   let pin_sig = parse_sigspec pin in
                   add_connection (pin_sig, signal_spec)
               | None -> ())

      | _ -> ()
  ) lines;

  finish_module ();

  {
    design_modules = List.rev !modules;
    design_attrs = List.rev !design_attrs;
  }

(* Convert RTLIL cell type to operation *)
let cell_to_operation cell_type =
  match cell_type with
  | "$_AND_" -> Some "AND"
  | "$_NAND_" -> Some "NAND"
  | "$_OR_" -> Some "OR"
  | "$_NOR_" -> Some "NOR"
  | "$_XOR_" -> Some "XOR"
  | "$_XNOR_" -> Some "XNOR"
  | "$_NOT_" -> Some "NOT"
  | "$_BUF_" -> Some "BUF"
  | "$_MUX_" -> Some "MUX"
  | "$add" -> Some "ADD"
  | "$sub" -> Some "SUB"
  | "$mul" -> Some "MUL"
  | "$eq" -> Some "EQ"
  | "$ne" -> Some "NE"
  | "$lt" -> Some "LT"
  | "$le" -> Some "LE"
  | "$gt" -> Some "GT"
  | "$ge" -> Some "GE"
  | _ when String.sub cell_type 0 (min 5 (String.length cell_type)) = "$_DFF" -> Some "DFF"
  | _ -> None

(* Print RTLIL design summary *)
let print_rtlil_summary design =
  Printf.printf "RTLIL Design Summary\n";
  Printf.printf "====================\n\n";

  Printf.printf "Attributes:\n";
  List.iter (fun (name, value) ->
    Printf.printf "  %s = %s\n" name value
  ) design.design_attrs;

  Printf.printf "\nModules: %d\n\n" (List.length design.design_modules);

  List.iter (fun m ->
    Printf.printf "Module: %s\n" m.mod_name;
    Printf.printf "  Wires: %d\n" (List.length m.mod_wires);

    (* Group wires by type *)
    let inputs = List.filter (fun w ->
      match w.wire_port with
      | Some (RInput, _) -> true
      | _ -> false
    ) m.mod_wires in

    let outputs = List.filter (fun w ->
      match w.wire_port with
      | Some (ROutput, _) -> true
      | _ -> false
    ) m.mod_wires in

    Printf.printf "    Inputs: %d\n" (List.length inputs);
    List.iter (fun w ->
      Printf.printf "      %s [%d]\n" w.wire_name w.wire_width
    ) inputs;

    Printf.printf "    Outputs: %d\n" (List.length outputs);
    List.iter (fun w ->
      Printf.printf "      %s [%d]\n" w.wire_name w.wire_width
    ) outputs;

    Printf.printf "  Cells: %d\n" (List.length m.mod_cells);

    (* Group cells by type *)
    let cell_types = Hashtbl.create 16 in
    List.iter (fun c ->
      let count = try Hashtbl.find cell_types c.cell_type with Not_found -> 0 in
      Hashtbl.replace cell_types c.cell_type (count + 1)
    ) m.mod_cells;

    Hashtbl.iter (fun cell_type count ->
      match cell_to_operation cell_type with
      | Some op -> Printf.printf "    %s (%s): %d\n" cell_type op count
      | None -> Printf.printf "    %s: %d\n" cell_type count
    ) cell_types;

    Printf.printf "  Direct connections: %d\n\n" (List.length m.mod_connects)
  ) design.design_modules
