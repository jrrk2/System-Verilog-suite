(* Netlist reader - parses gate-level Verilog with standard cells *)
(* Uses Liberty file to understand cell functions *)

open Sv_liberty

(* Netlist representation *)
type net_signal = {
  sig_name: string;
  sig_width: int;
}

type net_connection = {
  pin_name: string;
  signal: net_signal;
}

type net_instance = {
  inst_id: string;
  cell_type: string;
  conns: net_connection list;
}

type netlist = {
  top_module: string;
  net_inputs: net_signal list;
  net_outputs: net_signal list;
  net_wires: net_signal list;
  net_instances: net_instance list;
}

(* Extract netlist from Verilator JSON AST *)
(* Note: This is a placeholder - full implementation requires Sv_ast integration *)
let extract_netlist_from_ast _lib _ast_nodes =
  {
    top_module = "placeholder";
    net_inputs = [];
    net_outputs = [];
    net_wires = [];
    net_instances = [];
  }

(* Build function expression for a signal by tracing through gates *)
type expr =
  | EVar of string
  | EAnd of expr * expr
  | EOr of expr * expr
  | EXor of expr * expr
  | ENot of expr
  | EConst of int

let rec string_of_expr = function
  | EVar v -> v
  | EAnd (a, b) -> Printf.sprintf "(%s & %s)" (string_of_expr a) (string_of_expr b)
  | EOr (a, b) -> Printf.sprintf "(%s | %s)" (string_of_expr a) (string_of_expr b)
  | EXor (a, b) -> Printf.sprintf "(%s ^ %s)" (string_of_expr a) (string_of_expr b)
  | ENot e -> Printf.sprintf "!%s" (string_of_expr e)
  | EConst 0 -> "1'b0"
  | EConst 1 -> "1'b1"
  | EConst n -> string_of_int n

(* Parse Liberty function string to expr *)
let rec parse_function_expr lib_func input_map =
  let len = String.length lib_func in
  let pos = ref 0 in

  let skip_whitespace () =
    while !pos < len && (lib_func.[!pos] = ' ' || lib_func.[!pos] = '\t') do
      incr pos
    done
  in

  let rec parse_primary () =
    skip_whitespace ();
    if !pos >= len then EConst 0
    else match lib_func.[!pos] with
      | '!' ->
          incr pos;
          ENot (parse_primary ())
      | '(' ->
          incr pos;
          let e = parse_expr () in
          skip_whitespace ();
          if !pos < len && lib_func.[!pos] = ')' then incr pos;
          e
      | c when (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') || c = '_' ->
          let start = !pos in
          while !pos < len &&
                let c = lib_func.[!pos] in
                (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
                (c >= '0' && c <= '9') || c = '_' do
            incr pos
          done;
          let var_name = String.sub lib_func start (!pos - start) in
          (try
             let mapped_sig = List.assoc var_name input_map in
             EVar mapped_sig
           with Not_found -> EVar var_name)
      | _ -> incr pos; EConst 0

  and parse_and () =
    let left = ref (parse_primary ()) in
    skip_whitespace ();
    while !pos < len && lib_func.[!pos] = '&' do
      incr pos;
      skip_whitespace ();
      left := EAnd (!left, parse_primary ())
    done;
    !left

  and parse_xor () =
    let left = ref (parse_and ()) in
    skip_whitespace ();
    while !pos < len && lib_func.[!pos] = '^' do
      incr pos;
      skip_whitespace ();
      left := EXor (!left, parse_and ())
    done;
    !left

  and parse_or () =
    let left = ref (parse_xor ()) in
    skip_whitespace ();
    while !pos < len && lib_func.[!pos] = '|' do
      incr pos;
      skip_whitespace ();
      left := EOr (!left, parse_xor ())
    done;
    !left

  and parse_expr () = parse_or ()
  in
  parse_expr ()

(* Build expression for an output signal *)
let build_expr_for_signal lib netlist signal_name =
  (* Find instance that drives this signal *)
  let driving_inst = List.find_opt (fun inst ->
    List.exists (fun conn ->
      conn.signal.sig_name = signal_name &&
      (match get_cell lib inst.cell_type with
       | Some cell ->
           List.exists (fun pin ->
             pin.name = conn.pin_name && pin.direction = Output
           ) cell.pins
       | None -> false)
    ) inst.conns
  ) netlist.net_instances in

  match driving_inst with
  | Some inst ->
      (match get_cell lib inst.cell_type with
       | Some cell ->
           (* Find output pin *)
           let output_pin = List.find_opt (fun pin ->
             pin.direction = Output &&
             List.exists (fun c -> c.pin_name = pin.name && c.signal.sig_name = signal_name) inst.conns
           ) cell.pins in

           (match output_pin with
            | Some pin ->
                (match pin.function_expr with
                 | Some func ->
                     (* Build input mapping *)
                     let input_pins = List.filter (fun p -> p.direction = Input) cell.pins in
                     let input_map = List.filter_map (fun pin ->
                       let conn_opt = List.find_opt (fun c -> c.pin_name = pin.name) inst.conns in
                       match conn_opt with
                       | Some conn -> Some (pin.name, conn.signal.sig_name)
                       | None -> None
                     ) input_pins in

                     Some (parse_function_expr func input_map)
                 | None -> Some (EVar signal_name))
            | None -> Some (EVar signal_name))
       | None -> Some (EVar signal_name))
  | None -> Some (EVar signal_name)

(* Print netlist summary *)
let print_netlist_summary netlist lib =
  Printf.printf "Module: %s\n" netlist.top_module;
  Printf.printf "Inputs: %d\n" (List.length netlist.net_inputs);
  List.iter (fun signal ->
    Printf.printf "  %s[%d]\n" signal.sig_name signal.sig_width
  ) netlist.net_inputs;

  Printf.printf "Outputs: %d\n" (List.length netlist.net_outputs);
  List.iter (fun signal ->
    Printf.printf "  %s[%d] = " signal.sig_name signal.sig_width;
    (match build_expr_for_signal lib netlist signal.sig_name with
     | Some expr -> Printf.printf "%s\n" (string_of_expr expr)
     | None -> Printf.printf "?\n")
  ) netlist.net_outputs;

  Printf.printf "Instances: %d\n" (List.length netlist.net_instances);
  List.iter (fun inst ->
    Printf.printf "  %s: %s\n" inst.inst_id inst.cell_type;
    List.iter (fun conn ->
      Printf.printf "    .%s(%s)\n" conn.pin_name conn.signal.sig_name
    ) inst.conns
  ) netlist.net_instances
