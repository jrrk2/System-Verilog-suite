(* Convert EDIF to Behavioral IR *)

open Behavioral_ir
open Edif_parser

(* Map RTL primitives to behavioral expressions *)
let map_rtl_primitive cell_type inputs =
  match cell_type with
  | "RTL_AND" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BAnd; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_OR" | "RTL_OR4" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BOr; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_XOR" | "RTL_XOR2" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BXor; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_INV" ->
      (match inputs with
       | [a] -> Some (BUnOp { op = BNot; operand = a;
                              result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_ADD" | "RTL_ADD2" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BAdd; lhs = a; rhs = b;
                                  result_type = BInt { width = 32; signed = Unsigned } })
       | _ -> None)

  | "RTL_SUB" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BSub; lhs = a; rhs = b;
                                  result_type = BInt { width = 32; signed = Unsigned } })
       | _ -> None)

  | "RTL_EQ" | "RTL_EQ2" | "RTL_EQ25" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BEq; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_NEQ" | "RTL_NEQ3" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BNe; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_LT" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BLt; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_MUX" | "RTL_MUX1" | "RTL_MUX5" | "RTL_MUX11" | "RTL_MUX16" | "RTL_MUX86"
  | "RTL_MUX167" | "RTL_MUX180" | "RTL_MUX189" | "RTL_MUX198" ->
      (match inputs with
       | s :: i0 :: i1 :: _ ->
           Some (BCond { condition = s; then_val = i1; else_val = i0 })
       | _ -> None)

  | "OBUF" | "IBUF" ->
      (match inputs with
       | [a] -> Some a  (* Passthrough *)
       | _ -> None)

  | "GND" -> Some (BConst { value = 0; width = 1 })
  | "VCC" -> Some (BConst { value = 1; width = 1 })

  | _ -> None

(* Convert a single EDIF cell to a Behavioral IR module *)
let convert_cell (edif : edif_data) =

  (* Helper: parse signal name to extract base and bit index *)
  let parse_signal_name name =
    try
      let bracket_pos = String.rindex name '[' in
      let close_pos = String.rindex name ']' in
      if close_pos = String.length name - 1 then
        let base = String.sub name 0 bracket_pos in
        let idx_str = String.sub name (bracket_pos + 1) (close_pos - bracket_pos - 1) in
        let idx = int_of_string idx_str in
        (base, Some idx)
      else
        (name, None)
    with _ -> (name, None)
  in

  (* Helper: convert net name to expression *)
  let net_to_expr net_name =
    let (base, idx_opt) = parse_signal_name net_name in
    match idx_opt with
    | Some idx -> BSelect { array = BVar base; index = BConst { value = idx; width = 32 } }
    | None -> BVar base
  in

  (* Build connectivity map: (inst, pin) -> net *)
  let pin_to_net = Hashtbl.create 1024 in
  List.iter (fun (net : net_info) ->
    List.iter (fun (pin : net_pin) ->
      match pin.inst with
      | Some inst_name ->
          let pin_name = match pin.index with
            | Some idx -> Printf.sprintf "%s[%d]" pin.pin idx
            | None -> pin.pin
          in
          Hashtbl.add pin_to_net (inst_name, pin_name) net.name
      | None -> ()  (* Top-level port *)
    ) net.connections
  ) edif.nets;

  (* Helper: get input nets for an instance *)
  let get_inputs inst_name =
    let pins = ref [] in
    (* Try common input pin names *)
    List.iter (fun pin_name ->
      try
        let net = Hashtbl.find pin_to_net (inst_name, pin_name) in
        pins := net :: !pins
      with Not_found -> ()
    ) ["I0"; "I1"; "I"; "D"; "S"];
    List.map net_to_expr (List.rev !pins)
  in

  (* Helper: get output net for an instance *)
  let get_output inst_name =
    try
      Some (Hashtbl.find pin_to_net (inst_name, "O"))
    with Not_found ->
      try Some (Hashtbl.find pin_to_net (inst_name, "Q"))
      with Not_found ->
        try Some (Hashtbl.find pin_to_net (inst_name, "G"))
        with Not_found ->
          try Some (Hashtbl.find pin_to_net (inst_name, "P"))
          with Not_found -> None
  in

  (* Group signals with indices into vectors *)
  let group_into_vectors signal_list =
    let groups : (string, (int * bsignal) list) Hashtbl.t = Hashtbl.create 256 in
    List.iter (fun (sig_ : bsignal) ->
      let (base, idx_opt) = parse_signal_name sig_.name in
      match idx_opt with
      | Some idx ->
          let existing = try Hashtbl.find groups base with Not_found -> [] in
          Hashtbl.replace groups base ((idx, sig_) :: existing)
      | None ->
          Hashtbl.replace groups sig_.name [(0, sig_)]
    ) signal_list;

    Hashtbl.fold (fun base indices acc ->
      let sorted = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) indices in
      match sorted with
      | [(0, sig_)] when not (List.exists (fun (i, _) -> i > 0) sorted) ->
          sig_ :: acc
      | indices ->
          let max_idx = List.fold_left (fun m (i, _) -> max m i) 0 indices in
          let (_, template) = List.hd sorted in
          { template with
            name = base;
            stype = BInt { width = max_idx + 1; signed = Unsigned };
          } :: acc
    ) groups []
  in

  (* Create signals from ports and nets *)
  let ungrouped_signals =
    (* Ports *)
    List.map (fun (p : port_info) ->
      let direction = match p.direction with
        | Input -> `Input
        | Output -> `Output
        | Inout -> `Internal  (* Treat inout as internal for now *)
      in
      {
        name = p.name;
        stype = BInt { width = p.width; signed = Unsigned };
        direction;
        initial_value = None; attrs = []; 
      }
    ) edif.ports
    @
    (* Internal nets *)
    List.filter_map (fun (n : net_info) ->
      (* Skip constant nets *)
      if n.name = "<const0>" || n.name = "<const1>" then None
      else
        (* Check if it's a port or matches a port's indexed name *)
        let (base, _) = parse_signal_name n.name in
        let is_port = List.exists (fun (p : port_info) ->
          p.name = n.name || p.name = base
        ) edif.ports in
        if not is_port then
          Some {
            name = n.name;
            stype = BInt { width = 1; signed = Unsigned };
            direction = `Internal;
            initial_value = None; attrs = []; 
          }
        else None
    ) edif.nets
  in

  (* Group internal signals into vectors, but keep ports as-is since they
     already have correct widths from EDIF *)
  let (port_signals, internal_signals) = List.partition (fun (s : bsignal) ->
    s.direction <> `Internal
  ) ungrouped_signals in

  let grouped_internals = group_into_vectors internal_signals in
  let signals = port_signals @ grouped_internals in

  (* Helper: check if instance is a known combinational primitive *)
  let is_known_primitive cell_type =
    match cell_type with
    | "RTL_AND" | "RTL_OR" | "RTL_OR4" | "RTL_XOR" | "RTL_XOR2"
    | "RTL_INV" | "RTL_ADD" | "RTL_ADD2" | "RTL_SUB"
    | "RTL_EQ" | "RTL_EQ2" | "RTL_EQ25" | "RTL_NEQ" | "RTL_NEQ3" | "RTL_LT"
    | "RTL_MUX" | "RTL_MUX1" | "RTL_MUX5" | "RTL_MUX11" | "RTL_MUX16" | "RTL_MUX86"
    | "RTL_MUX167" | "RTL_MUX180" | "RTL_MUX189" | "RTL_MUX198"
    | "GND" | "VCC" | "OBUF" | "IBUF" -> true
    | _ -> false
  in

  (* Create continuous assignments for RTL primitives *)
  let assignments = ref [] in
  List.iter (fun (inst : instance_info) ->
    let is_primitive = is_known_primitive inst.cell_type in

    if is_primitive then begin
      match get_output inst.name with
      | Some output ->
          (* Skip assignments to constant nets *)
          if output = "<const0>" || output = "<const1>" then ()
          else
            let inputs = get_inputs inst.name in
            (match map_rtl_primitive inst.cell_type inputs with
             | Some expr ->
                 assignments := BAssign { lhs = output; rhs = expr } :: !assignments
             | None -> ())
      | None -> ()
    end
  ) edif.instances;

  (* Create a combinational process *)
  let main_process = BCombinational {
    name = "main_logic";
    sensitivity = [BAny];
    body = List.rev !assignments;
  } in

  (* Helper: normalize port names from EDIF to Verilog array notation *)
  (* Converts "I0_0_" to "I0", "I1_5_" to "I1", etc. *)
  let normalize_port_name port_name =
    try
      (* Check if port name ends with _N_ pattern *)
      let len = String.length port_name in
      if len > 3 && port_name.[len - 1] = '_' then
        (* Find last underscore before the trailing one *)
        let rec find_last_underscore pos =
          if pos < 0 then None
          else if port_name.[pos] = '_' then
            (* Check if everything after this underscore (except last char) is digits *)
            let suffix_start = pos + 1 in
            let suffix_end = len - 1 in
            let is_numeric = ref true in
            for i = suffix_start to suffix_end - 1 do
              if not (port_name.[i] >= '0' && port_name.[i] <= '9') then
                is_numeric := false
            done;
            if !is_numeric && suffix_end > suffix_start then
              Some pos
            else
              find_last_underscore (pos - 1)
          else
            find_last_underscore (pos - 1)
        in
        match find_last_underscore (len - 2) with
        | Some pos -> String.sub port_name 0 pos
        | None -> port_name
      else
        port_name
    with _ -> port_name
  in

  (* Helper: get all port connections for an instance *)
  let get_port_connections inst_name =
    let connections = ref [] in
    List.iter (fun (net : net_info) ->
      List.iter (fun (pin : net_pin) ->
        match pin.inst with
        | Some inst when inst = inst_name ->
            let normalized_pin = normalize_port_name pin.pin in
            connections := (normalized_pin, net_to_expr net.name) :: !connections
        | _ -> ()
      ) net.connections
    ) edif.nets;
    List.rev !connections
  in

  (* Identify hierarchical instances *)
  (* No need to parse parameterized names - EDIF is post-elaboration,
     so parameterized variants are separate modules (e.g., slib_input_filter__parameterized2) *)
  let hier_instances = List.filter_map (fun (inst : instance_info) ->
    if not (is_known_primitive inst.cell_type) then
      Some {
        inst_name = inst.name;
        module_name = inst.cell_type;  (* Use full name as-is *)
        param_values = [];  (* No parameters - already elaborated *)
        port_connections = get_port_connections inst.name;
      }
    else None
  ) edif.instances in

  (* Create module *)
  {
    name = edif.module_name;
    params = [];
    signals;
    processes = [main_process];
    instances = hier_instances;
    funcs = [];
    mems = []; attrs = [];
  }

(* Convert EDIF to Behavioral IR *)
let convert filename =
  let content = Edif_parser.read_file filename in

  (* Parse all netlist cells (including top-level) *)
  let all_cells = Edif_parser.parse_all_netlist_cells content in

  (* Also get the top-level module with library cells *)
  let top_level = Edif_parser.parse_schematic filename in

  Printf.printf "Converting EDIF: %s\n" top_level.module_name;
  Printf.printf "  Found %d netlist cells to convert\n" (List.length all_cells);

  (* Convert all cells to modules *)
  let all_modules = List.map convert_cell all_cells in

  Printf.printf "  Converted %d modules\n" (List.length all_modules);

  (* Convert library cells to Behavioral IR format *)
  let library_cells_list =
    Hashtbl.fold (fun cell_name ports acc ->
      let lib_ports = List.map (fun (p : Edif_parser.port_info) ->
        let port_dir = match p.direction with
          | Edif_parser.Input -> `Input
          | Edif_parser.Output -> `Output
          | Edif_parser.Inout -> `Input  (* Treat inout as input for now *)
        in
        {
          Behavioral_ir.port_name = p.name;
          port_direction = port_dir;
          port_width = p.width;
        }
      ) ports in
      (cell_name, lib_ports) :: acc
    ) top_level.library_cells []
  in

  (* Create program with all modules *)
  {
    modules = all_modules;
    library_cells = library_cells_list;
  }
