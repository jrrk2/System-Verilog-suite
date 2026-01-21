(* sv_elaborate.ml - SystemVerilog Elaboration *)
(*
   Performs elaboration on Verible parse tree:
   1. Parameter evaluation and constant propagation
   2. Width resolution (evaluate range expressions)
   3. Type resolution
   4. Generate block expansion (basic support)

   Reference: /Users/jonathan/Downloads/sv-elaborator-master
*)

open Source_text_verible

(* Elaborated values *)
type elab_value =
  | EInt of int
  | EBool of bool
  | EString of string
  | EUnknown

(* Port information *)
type port_info = {
  port_name: string;
  port_direction: string;  (* "input" or "output" *)
  port_width: int;
}

(* Assign statement information *)
type assign_info = {
  assign_lhs: string;
  assign_rhs: token;  (* Expression tree *)
}

(* Elaboration context *)
type elab_context = {
  (* Parameter bindings: name -> value *)
  mutable params: (string * elab_value) list;
  (* Localparam bindings *)
  mutable localparams: (string * elab_value) list;
  (* Type definitions *)
  mutable types: (string * elab_type) list;
  (* Width cache for expressions *)
  mutable width_cache: (token, int) Hashtbl.t;
  (* Extracted ports *)
  mutable ports: port_info list;
  (* Extracted assign statements *)
  mutable assigns: assign_info list;
  (* Module name *)
  mutable module_name: string option;
}

and elab_type =
  | TyLogic of int  (* bit width *)
  | TyInt of bool   (* signed *)
  | TyStruct of (string * elab_type) list
  | TyUnknown

let create_context () = {
  params = [];
  localparams = [];
  types = [];
  width_cache = Hashtbl.create 100;
  ports = [];
  assigns = [];
  module_name = None;
}

(* Evaluate constant expression to integer *)
let rec eval_const_expr ctx expr =
  match expr with
  | TK_DecNumber s ->
      (try EInt (int_of_string s)
       with _ -> EUnknown)

  | TK_UnBasedNumber s ->
      (try EInt (int_of_string s)
       with _ -> EUnknown)

  | SymbolIdentifier name ->
      (* Look up parameter value *)
      (try
        List.assoc name ctx.params
       with Not_found ->
         try List.assoc name ctx.localparams
         with Not_found -> EUnknown)

  | TUPLE4 (STRING "add_expr2", a, PLUS, b) ->
      (match eval_const_expr ctx a, eval_const_expr ctx b with
       | EInt x, EInt y -> EInt (x + y)
       | _ -> EUnknown)

  | TUPLE4 (STRING "add_expr3", a, HYPHEN, b) ->
      (match eval_const_expr ctx a, eval_const_expr ctx b with
       | EInt x, EInt y -> EInt (x - y)
       | _ -> EUnknown)

  | TUPLE4 (STRING "mul_expr2", a, STAR, b) ->
      (match eval_const_expr ctx a, eval_const_expr ctx b with
       | EInt x, EInt y -> EInt (x * y)
       | _ -> EUnknown)

  | TUPLE4 (STRING "mul_expr3", a, SLASH, b) ->
      (match eval_const_expr ctx a, eval_const_expr ctx b with
       | EInt x, EInt y when y <> 0 -> EInt (x / y)
       | _ -> EUnknown)

  | _ -> EUnknown

(* Evaluate width from range expression [msb:lsb] *)
let eval_range_width ctx msb lsb =
  match eval_const_expr ctx msb, eval_const_expr ctx lsb with
  | EInt m, EInt l -> abs (m - l) + 1
  | _ -> 1  (* Default to 1-bit *)

(* Extract identifier name from TUPLE3 unqualified_id1 pattern *)
let extract_identifier token =
  match token with
  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> Some name
  | SymbolIdentifier name -> Some name
  | _ -> None

(* Extract range from TUPLE6 decl_variable_dimension1 pattern *)
let extract_range token =
  match token with
  | TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber high, COLON, TK_DecNumber low, RBRACK) ->
      (try
        let h = int_of_string high in
        let l = int_of_string low in
        Some (h, l)
      with _ -> None)
  | _ -> None

(* Extract port declaration: direction, name, width *)
let extract_port_decl ctx token =
  match token with
  | TUPLE5 (STRING "port_declaration_noattr1", dir, _, data_type, _) ->
      (match data_type with
       | TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
                 range_token, id_token, _) ->
           let name = extract_identifier id_token in
           let range = extract_range range_token in
           let direction = (match dir with
             | Input -> "input"
             | Output -> "output"
             | _ -> "unknown") in
           (match name, range with
            | Some n, Some (h, l) ->
                let width = h - l + 1 in
                Printf.printf "  Port: %s %s [%d:%d] (width=%d)\n" direction n h l width;
                ctx.ports <- {port_name = n; port_direction = direction; port_width = width} :: ctx.ports;
                Some (direction, n, width)
            | Some n, None ->
                Printf.printf "  Port: %s %s (width=1)\n" direction n;
                ctx.ports <- {port_name = n; port_direction = direction; port_width = 1} :: ctx.ports;
                Some (direction, n, 1)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Recursively extract all port declarations from port list *)
let rec extract_ports_from_list ctx token =
  match token with
  | CONS1 inner -> extract_ports_from_list ctx inner
  | CONS2 (left, right) ->
      ignore (extract_ports_from_list ctx left);
      (* Right side might be COMMA followed by next port, or just the port *)
      (match right with
       | COMMA -> ()
       | TUPLE5 _ -> ignore (extract_port_decl ctx right)
       | CONS2 (COMMA, port) -> ignore (extract_port_decl ctx port)
       | _ -> ignore (extract_ports_from_list ctx right))
  | TUPLE5 _ -> ignore (extract_port_decl ctx token)
  | _ -> ()

(* Extract expression from continuous assignment *)
let rec extract_expression token =
  match token with
  | TUPLE4 (STRING "add_expr2", left, PLUS, right) ->
      Printf.printf "ADD(";
      ignore (extract_expression left);
      Printf.printf ", ";
      ignore (extract_expression right);
      Printf.printf ")";
      Some token
  | TUPLE4 (STRING "mul_expr2", left, STAR, right) ->
      Printf.printf "MUL(";
      ignore (extract_expression left);
      Printf.printf ", ";
      ignore (extract_expression right);
      Printf.printf ")";
      Some token
  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) ->
      Printf.printf "%s" name;
      Some token
  | SymbolIdentifier name ->
      Printf.printf "%s" name;
      Some token
  | TK_DecNumber n ->
      Printf.printf "%s" n;
      Some token
  | _ -> None

(* Extract continuous assignment *)
let extract_continuous_assign ctx token =
  match token with
  | TUPLE6 (STRING "continuous_assign1", Assign, _, _, assigns_list, SEMICOLON) ->
      (match assigns_list with
       | CONS1 (TUPLE4 (STRING "cont_assign1", lhs, EQUALS, rhs)) ->
           Printf.printf "Assign statement:\n";
           (match extract_identifier lhs with
            | Some name ->
                Printf.printf "  LHS: %s = " name;
                ctx.assigns <- {assign_lhs = name; assign_rhs = rhs} :: ctx.assigns
            | None -> Printf.printf "  LHS: <unknown> = ");
           ignore (extract_expression rhs);
           Printf.printf "\n";
           Some token
       | _ -> None)
  | _ -> None

(* Extract continuous assignments from module items *)
let rec extract_assigns_from_items ctx token =
  match token with
  | CONS1 inner -> extract_assigns_from_items ctx inner
  | CONS2 (left, right) ->
      extract_assigns_from_items ctx left;
      extract_assigns_from_items ctx right
  | TUPLE6 (STRING "continuous_assign1", _, _, _, _, _) ->
      ignore (extract_continuous_assign ctx token)
  | _ -> ()

(* Extract module information from TUPLE12 module declaration *)
let extract_module_info ctx token =
  match token with
  | TUPLE12 (STRING "module_or_interface_declaration1", Module, _, name_token, _, params, ports, _, _, items, _, _) ->
      (match name_token with
       | SymbolIdentifier name ->
           Printf.printf "Module: %s\n" name;
           ctx.module_name <- Some name;
           (* Extract parameters if present *)
           (match params with
            | EMPTY_TOKEN -> ()
            | _ -> Printf.printf "Parameters: <present>\n");
           (* Extract ports *)
           (match ports with
            | EMPTY_TOKEN -> ()
            | TUPLE4 (STRING "module_port_list_opt1", LPAREN, port_list, RPAREN) ->
                Printf.printf "Ports:\n";
                extract_ports_from_list ctx port_list
            | _ -> ());
           (* Extract assigns *)
           (match items with
            | EMPTY_TOKEN -> ()
            | _ ->
                Printf.printf "\nStatements:\n";
                extract_assigns_from_items ctx items);
           Some name
       | _ -> None)
  | _ -> None

(* Extract parameter declarations - simplified stub *)
let rec extract_parameters ctx token =
  match token with
  | TLIST lst ->
      List.iter (extract_parameters ctx) lst
  | _ ->
      (* TODO: Implement proper extraction based on actual parse tree structure *)
      ()

(* Resolve width - stub *)
let resolve_width _ctx _token = 1

(* Main elaboration function *)
let elaborate verible_ast =
  let ctx = create_context () in

  Printf.printf "\n=== Verible Elaboration ===\n\n";

  (* Walk the parse tree to extract module info *)
  let rec walk_tree token =
    match token with
    | TUPLE3 (STRING "ml_start1", desc_list, _) ->
        walk_tree desc_list
    | CONS1 inner ->
        walk_tree inner
    | CONS2 (left, right) ->
        walk_tree left;
        walk_tree right
    | TUPLE12 (STRING "module_or_interface_declaration1", _, _, _, _, _, _, _, _, _, _, _) ->
        ignore (extract_module_info ctx token)
    | _ -> ()
  in

  (* Phase 1: Extract all parameters *)
  extract_parameters ctx verible_ast;

  (* Phase 2: Walk tree and extract module structure *)
  walk_tree verible_ast;

  (* Phase 3: Resolve all widths (with parameters substituted) *)
  (* This would recursively walk the AST and resolve all range expressions *)

  (* Phase 4: Expand generate blocks (TODO) *)

  Printf.printf "\n";
  ctx

(* Helper to get elaborated parameter value *)
let get_param_value ctx name =
  try
    match List.assoc name ctx.params with
    | EInt i -> Some i
    | _ -> None
  with Not_found ->
    try
      match List.assoc name ctx.localparams with
      | EInt i -> Some i
      | _ -> None
    with Not_found -> None

(* Helper to resolve a width expression *)
let resolve_width_expr ctx expr =
  match eval_const_expr ctx expr with
  | EInt w -> w
  | _ -> 1

(* Print elaboration context for debugging *)
let print_context ctx =
  Printf.printf "Parameters:\n";
  List.iter (fun (name, value) ->
    match value with
    | EInt i -> Printf.printf "  %s = %d\n" name i
    | EBool b -> Printf.printf "  %s = %b\n" name b
    | EString s -> Printf.printf "  %s = %s\n" name s
    | EUnknown -> Printf.printf "  %s = <unknown>\n" name
  ) ctx.params;
  Printf.printf "Localparams:\n";
  List.iter (fun (name, value) ->
    match value with
    | EInt i -> Printf.printf "  %s = %d\n" name i
    | _ -> ()
  ) ctx.localparams
