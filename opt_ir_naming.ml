(* opt_ir_naming.ml - Generate meaningful names for IR nodes based on operations *)

open Sv_ast

let debug = ref false

(* Sanitize a name to be a valid Verilog identifier *)
let sanitize_name name =
  let name = String.lowercase_ascii name in
  (* Replace invalid characters with underscores *)
  let buf = Buffer.create (String.length name) in
  String.iter (fun c ->
    match c with
    | 'a'..'z' | '0'..'9' | '_' -> Buffer.add_char buf c
    | _ -> Buffer.add_char buf '_'
  ) name;
  let result = Buffer.contents buf in
  (* Ensure it doesn't start with a digit *)
  if String.length result > 0 && result.[0] >= '0' && result.[0] <= '9' then
    "n" ^ result
  else if String.length result = 0 then
    "signal"
  else
    result

(* Truncate long names *)
let truncate_name name max_len =
  if String.length name > max_len then
    String.sub name 0 (max_len - 3) ^ "___"
  else
    name

(* Get base name without array indices or bit selects *)
let get_base_name name =
  try
    let idx = String.index name '[' in
    String.sub name 0 idx
  with Not_found -> name

(* Generate a meaningful name for an operation *)
let generate_operation_name op input_names =
  let sanitized_inputs = List.map (fun n ->
    let base = get_base_name n in
    sanitize_name base
  ) input_names in

  let name = match op with
  | Add { signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_plus_%s" a b
      | _ -> "add")

  | Add { signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_sadd_%s" a b
      | _ -> "sadd")

  | Sub { signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_minus_%s" a b
      | _ -> "sub")

  | Sub { signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_ssub_%s" a b
      | _ -> "ssub")

  | Mul { signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_mul_%s" a b
      | _ -> "mul")

  | Mul { signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_smul_%s" a b
      | _ -> "smul")

  | And _ ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_and_%s" a b
      | _ -> "and")

  | Or _ ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_or_%s" a b
      | _ -> "or")

  | Xor _ ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_xor_%s" a b
      | _ -> "xor")

  | Not _ ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "not_%s" a
      | _ -> "not")

  | Compare { cmp_op = `Eq; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_eq_%s" a b
      | _ -> "eq")

  | Compare { cmp_op = `Ne; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_ne_%s" a b
      | _ -> "ne")

  | Compare { cmp_op = `Lt; signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_lt_%s" a b
      | _ -> "lt")

  | Compare { cmp_op = `Lt; signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_slt_%s" a b
      | _ -> "slt")

  | Compare { cmp_op = `Le; signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_le_%s" a b
      | _ -> "le")

  | Compare { cmp_op = `Le; signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_sle_%s" a b
      | _ -> "sle")

  | Compare { cmp_op = `Gt; signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_gt_%s" a b
      | _ -> "gt")

  | Compare { cmp_op = `Gt; signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_sgt_%s" a b
      | _ -> "sgt")

  | Compare { cmp_op = `Ge; signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_ge_%s" a b
      | _ -> "ge")

  | Compare { cmp_op = `Ge; signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_sge_%s" a b
      | _ -> "sge")

  | Mux _ ->
      (match sanitized_inputs with
      | [sel; in0; in1] -> Printf.sprintf "mux_%s_%s_%s" sel in0 in1
      | _ -> "mux")

  | Pmux { num_cases; _ } ->
      Printf.sprintf "pmux_%d" num_cases

  | Shift { direction = `Left; arithmetic = false; _ } ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "%s_shl" a
      | [a; amt] -> Printf.sprintf "%s_shl_%s" a amt
      | _ -> "shl")

  | Shift { direction = `Left; arithmetic = true; _ } ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "%s_sal" a
      | [a; amt] -> Printf.sprintf "%s_sal_%s" a amt
      | _ -> "sal")

  | Shift { direction = `Right; arithmetic = false; _ } ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "%s_shr" a
      | [a; amt] -> Printf.sprintf "%s_shr_%s" a amt
      | _ -> "shr")

  | Shift { direction = `Right; arithmetic = true; _ } ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "%s_sar" a
      | [a; amt] -> Printf.sprintf "%s_sar_%s" a amt
      | _ -> "sar")

  | ZeroExtend { from_width; to_width } ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "%s_zext_%dto%d" a from_width to_width
      | _ -> Printf.sprintf "zext_%dto%d" from_width to_width)

  | SignExtend { from_width; to_width } ->
      (match sanitized_inputs with
      | [a] -> Printf.sprintf "%s_sext_%dto%d" a from_width to_width
      | _ -> Printf.sprintf "sext_%dto%d" from_width to_width)

  | Extract { lsb; msb; _ } ->
      (match sanitized_inputs with
      | [a] ->
          if lsb = msb then
            Printf.sprintf "%s_bit%d" a lsb
          else
            Printf.sprintf "%s_%dto%d" a msb lsb
      | _ -> Printf.sprintf "extract_%dto%d" msb lsb)

  | Concat { widths } ->
      let inputs_str = String.concat "_" sanitized_inputs in
      if String.length inputs_str > 30 then
        Printf.sprintf "concat_%d" (List.length widths)
      else
        Printf.sprintf "concat_%s" inputs_str

  | Register { width; _ } ->
      (match sanitized_inputs with
      | [d] -> Printf.sprintf "%s_reg" d
      | d :: _ -> Printf.sprintf "%s_reg" d
      | _ -> Printf.sprintf "reg_%db" width)

  | Div { signed = false; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_div_%s" a b
      | _ -> "div")

  | Div { signed = true; _ } ->
      (match sanitized_inputs with
      | [a; b] -> Printf.sprintf "%s_sdiv_%s" a b
      | _ -> "sdiv")
  in

  (* Truncate to reasonable length *)
  truncate_name name 40

(* Generate names for all nodes in an IR *)
let generate_names ir =
  let id_to_name = Hashtbl.create 200 in
  let name_counts = Hashtbl.create 100 in

  (* Phase 1: Map input/output IDs to their names *)
  Hashtbl.iter (fun name value ->
    match value with
    | Input { id; _ } ->
        let sanitized = sanitize_name name in
        Hashtbl.add id_to_name id sanitized;
        if !debug then Printf.eprintf "Input: %d -> %s\n" id sanitized
    | _ -> ()
  ) ir.ir_inputs;

  Hashtbl.iter (fun name value ->
    match value with
    | Output { id; _ } ->
        let sanitized = sanitize_name name in
        Hashtbl.add id_to_name id sanitized;
        if !debug then Printf.eprintf "Output: %d -> %s\n" id sanitized
    | _ -> ()
  ) ir.ir_outputs;

  (* Phase 2: Process constants *)
  Hashtbl.iter (fun const_value value_id ->
    let name = Printf.sprintf "const_%d" const_value in
    Hashtbl.add id_to_name value_id name;
    if !debug then Printf.eprintf "Constant: %d -> %s (value=%d)\n" value_id name const_value
  ) ir.ir_constants;

  (* Helper to get name of a value ID *)
  let get_name id =
    match Hashtbl.find_opt id_to_name id with
    | Some name -> name
    | None -> Printf.sprintf "n%d" id
  in

  (* Phase 3: Generate names for all nodes based on their operations *)
  (* We need to process nodes in dependency order, so collect them first *)
  let nodes_list = Hashtbl.fold (fun id node acc -> (id, node) :: acc) ir.ir_nodes [] in

  (* Sort by node_depth to process in dependency order *)
  let sorted_nodes = List.sort (fun (_, n1) (_, n2) ->
    compare n1.node_depth n2.node_depth
  ) nodes_list in

  List.iter (fun (id, node) ->
    (* Skip if already named *)
    if not (Hashtbl.mem id_to_name id) then begin
      (* Get names of input signals *)
      let input_names = List.map get_name node.node_inputs in

      (* Generate base name from operation *)
      let base_name = generate_operation_name node.node_op input_names in

      (* Make unique if needed *)
      let count = try Hashtbl.find name_counts base_name with Not_found -> 0 in
      let final_name =
        if count = 0 then
          base_name
        else
          Printf.sprintf "%s_%d" base_name count
      in

      Hashtbl.replace name_counts base_name (count + 1);
      Hashtbl.add id_to_name id final_name;

      if !debug then
        Printf.eprintf "Node %d: %s (op=%s, inputs=[%s])\n"
          id final_name
          (match node.node_op with
           | Add _ -> "add" | Sub _ -> "sub" | Mul _ -> "mul"
           | And _ -> "and" | Or _ -> "or" | Xor _ -> "xor"
           | Not _ -> "not" | Mux _ -> "mux" | Register _ -> "reg"
           | Compare _ -> "cmp" | Shift _ -> "shift"
           | _ -> "other")
          (String.concat ", " input_names)
    end
  ) sorted_nodes;

  id_to_name

(* Apply naming strategy to IR and return updated id_to_name table *)
let apply_naming_strategy ?(verbose=false) ir =
  debug := verbose;
  generate_names ir
