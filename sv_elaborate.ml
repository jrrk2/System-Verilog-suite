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

(* Signal information for symbol table *)
type signal_kind =
  | PortInput
  | PortOutput
  | Reg
  | Wire
  | Logic

type signal_info = {
  signal_name: string;
  signal_kind: signal_kind;
  signal_width: int;
}

(* Assign statement information *)
type assign_info = {
  assign_lhs: string;
  assign_rhs: token;  (* Expression tree *)
  assign_condition: token option;  (* Condition expression, None for unconditional *)
}

(* Always block information *)
type reset_info = {
  reset_signal: string;
  reset_edge: [`Posedge | `Negedge];
  reset_value: token;  (* The value assigned during reset *)
  is_set: bool;        (* true = async set (reset to 1), false = async reset (reset to 0) *)
}

type always_type =
  | AlwaysComb
  | AlwaysFF of {
      clock: string;
      edge: [`Posedge | `Negedge];
      async_reset: reset_info option;
    }

type always_info = {
  always_type: always_type;
  always_stmts: assign_info list;
}

(* Function parameter information *)
type func_param = {
  param_name: string;
  param_width: int;
}

(* Function information *)
type function_info = {
  func_name: string;
  func_params: func_param list;
  func_return_width: int;
  func_body: token;  (* The function body as a token tree *)
}

(* Per-module data *)
type module_data = {
  mutable mod_ports: port_info list;
  mutable mod_assigns: assign_info list;
  mutable mod_always_blocks: always_info list;
  mutable mod_functions: function_info list;
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
  (* Per-module symbol tables: module_name -> (signal_name -> signal_info) *)
  module_symbol_tables: (string, (string, signal_info) Hashtbl.t) Hashtbl.t;
  (* Per-module data: module_name -> module_data *)
  module_data: (string, module_data) Hashtbl.t;
  (* Current module being processed *)
  mutable current_module: string option;
  (* Extracted ports (DEPRECATED - use module_data) *)
  mutable ports: port_info list;
  (* Extracted assign statements (DEPRECATED - use module_data) *)
  mutable assigns: assign_info list;
  (* Extracted always blocks (DEPRECATED - use module_data) *)
  mutable always_blocks: always_info list;
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
  module_symbol_tables = Hashtbl.create 20;  (* ~20 modules per file *)
  module_data = Hashtbl.create 20;  (* Per-module data *)
  current_module = None;
  ports = [];
  assigns = [];
  always_blocks = [];
  module_name = None;
}

(* Get or create symbol table for current module *)
let get_current_symbol_table ctx =
  match ctx.current_module with
  | None ->
      Printf.eprintf "Warning: No current module set\n";
      None
  | Some mod_name ->
      match Hashtbl.find_opt ctx.module_symbol_tables mod_name with
      | Some tbl -> Some tbl
      | None ->
          (* Create new symbol table for this module *)
          let tbl = Hashtbl.create 200 in
          Hashtbl.add ctx.module_symbol_tables mod_name tbl;
          Some tbl

(* Get or create module_data for current module *)
let get_current_module_data ctx =
  match ctx.current_module with
  | None ->
      Printf.eprintf "Warning: No current module set\n";
      None
  | Some mod_name ->
      match Hashtbl.find_opt ctx.module_data mod_name with
      | Some data -> Some data
      | None ->
          (* Create new module_data *)
          let data = {
            mod_ports = [];
            mod_assigns = [];
            mod_always_blocks = [];
            mod_functions = [];
          } in
          Hashtbl.add ctx.module_data mod_name data;
          Some data

(* Get symbol table for a specific module *)
let get_module_symbol_table ctx mod_name =
  Hashtbl.find_opt ctx.module_symbol_tables mod_name

(* Get module_data for a specific module *)
let get_module_data ctx mod_name =
  Hashtbl.find_opt ctx.module_data mod_name

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

  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) ->
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

  | TUPLE4 (STRING "expr_primary_parens1", _lparen, inner_expr, _rparen) ->
      (* Unwrap parentheses *)
      eval_const_expr ctx inner_expr

  | TUPLE3 (STRING "reference_or_call_base1", func_name, args) ->
      (* System function call like $clog2(expr) *)
      let name = (match func_name with
        | SymbolIdentifier n -> n
        | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
        | _ -> "unknown") in
      if name = "$clog2" then
        (* Extract argument and evaluate *)
        let arg_val = (match args with
          | TUPLE4 (STRING "argument_list_opt1", _lparen, arg_expr, _rparen) ->
              eval_const_expr ctx arg_expr
          | TUPLE5 (STRING "argument_list_opt2", _lparen, arg_list, _, _rparen) ->
              (* First argument from list *)
              (match arg_list with
               | TUPLE3 (STRING "expression_list_proper2", first_arg, _) ->
                   eval_const_expr ctx first_arg
               | _ -> eval_const_expr ctx arg_list)
          | TUPLE4 (STRING "call_base1", _lparen, arg_expr, _rparen) ->
              eval_const_expr ctx arg_expr
          | _ -> EUnknown) in
        (match arg_val with
         | EInt n when n > 0 ->
             (* Compute ceiling log2 *)
             let rec clog2 x acc =
               if x <= 1 then acc
               else clog2 ((x + 1) / 2) (acc + 1)
             in
             EInt (clog2 n 0)
         | _ -> EUnknown)
      else
        EUnknown

  | TLIST [single] ->
      (* Unwrap single-element list *)
      eval_const_expr ctx single

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
let extract_range ctx token =
  match token with
  | TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber high, COLON, TK_DecNumber low, RBRACK) ->
      (try
        let h = int_of_string high in
        let l = int_of_string low in
        Some (h, l)
      with _ -> None)
  | TUPLE6 (STRING "decl_variable_dimension1", LBRACK, high_expr, COLON, low_expr, RBRACK) ->
      (* Dynamic range expression like [$clog2(RATIO-1)-1:0] *)
      (match eval_const_expr ctx high_expr, eval_const_expr ctx low_expr with
       | EInt h, EInt l -> Some (h, l)
       | _ -> None)
  | _ -> None

(* Extract width from data_type_primitive1 pattern *)
(* Standard widths for SystemVerilog's integer_atom_type keywords. *)
let width_of_integer_atom = function
  | Byte -> Some 8
  | Shortint -> Some 16
  | Int | Integer | Time -> Some 32
  | Longint -> Some 64
  | _ -> None

let extract_width_from_primitive ctx data_type =
  match data_type with
  | TUPLE3 (STRING "data_type_primitive1", inner, range_token) ->
      (* Integer atoms (byte/shortint/int/longint/integer/time) carry
         their bit width implicitly.  Without this branch they fall
         through to extract_range, which returns None and the caller
         defaults the signal to 1 bit — visible as the `integer i`
         iterator in tinyllm-fpga's microgpt_categorical_sampler being
         declared as uint<1> in BIR. *)
      (match inner with
       | TUPLE3 (STRING "data_type_primitive_scalar5", atom_type, _) ->
           (match width_of_integer_atom atom_type with
            | Some w -> Some (w - 1, 0)
            | None -> extract_range ctx range_token)
       | _ -> extract_range ctx range_token)
  | _ -> None

(* Extract port declaration: direction, name, width *)
let extract_port_decl ctx token =
  match token with
  | TUPLE5 (STRING "port_declaration_noattr1", dir, _, data_type, _) ->
      (match data_type with
       (* Simple wire/reg without explicit type - defaults to 1-bit *)
       | TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
                 id_token, _) ->
           let name = extract_identifier id_token in
           let direction = (match dir with
             | Input -> "input"
             | Output -> "output"
             | _ -> "unknown") in
           (match name with
            | Some n ->
                Printf.printf "  Port: %s %s (width=1)\n" direction n;
                (* Add to current module's ports *)
                (match get_current_module_data ctx with
                 | Some data -> data.mod_ports <- {port_name = n; port_direction = direction; port_width = 1} :: data.mod_ports
                 | None -> ());
                (* Also add to deprecated global list for compatibility *)
                ctx.ports <- {port_name = n; port_direction = direction; port_width = 1} :: ctx.ports;
                (* Add to current module's symbol table *)
                let kind = if direction = "input" then PortInput else PortOutput in
                (match get_current_symbol_table ctx with
                 | Some tbl -> Hashtbl.replace tbl n { signal_name = n; signal_kind = kind; signal_width = 1 }
                 | None -> ());
                Some (direction, n, 1)
            | None -> None)

       (* Explicit data type with identifier *)
       | TUPLE3 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
                 data_type_primitive, id_token)
       | TUPLE3 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
                 data_type_primitive, id_token)
       | TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
                 data_type_primitive, id_token, _)
       | TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
                 data_type_primitive, id_token, _) ->
           let name = extract_identifier id_token in
           let range = extract_width_from_primitive ctx data_type_primitive in
           let direction = (match dir with
             | Input -> "input"
             | Output -> "output"
             | _ -> "unknown") in
           (match name, range with
            | Some n, Some (h, l) ->
                let width = h - l + 1 in
                Printf.printf "  Port: %s %s [%d:%d] (width=%d)\n" direction n h l width;
                (* Add to current module's ports *)
                (match get_current_module_data ctx with
                 | Some data -> data.mod_ports <- {port_name = n; port_direction = direction; port_width = width} :: data.mod_ports
                 | None -> ());
                (* Also add to deprecated global list for compatibility *)
                ctx.ports <- {port_name = n; port_direction = direction; port_width = width} :: ctx.ports;
                (* Add to current module's symbol table *)
                let kind = if direction = "input" then PortInput else PortOutput in
                (match get_current_symbol_table ctx with
                 | Some tbl -> Hashtbl.replace tbl n { signal_name = n; signal_kind = kind; signal_width = width }
                 | None -> ());
                Some (direction, n, width)
            | Some n, None ->
                Printf.printf "  Port: %s %s (width=1)\n" direction n;
                (* Add to current module's ports *)
                (match get_current_module_data ctx with
                 | Some data -> data.mod_ports <- {port_name = n; port_direction = direction; port_width = 1} :: data.mod_ports
                 | None -> ());
                (* Also add to deprecated global list for compatibility *)
                ctx.ports <- {port_name = n; port_direction = direction; port_width = 1} :: ctx.ports;
                (* Add to current module's symbol table *)
                let kind = if direction = "input" then PortInput else PortOutput in
                (match get_current_symbol_table ctx with
                 | Some tbl -> Hashtbl.replace tbl n { signal_name = n; signal_kind = kind; signal_width = 1 }
                 | None -> ());
                Some (direction, n, 1)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Extract a simple port reference (for shared declarations like "input a, b") *)
let extract_port_ref ctx direction width token =
  match token with
  | TUPLE3 (STRING "port1",
            TUPLE3 (STRING "port_reference1",
                    TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _), _), _) ->
      Printf.printf "  Port: %s %s (width=%d)\n" direction name width;
      (* Add to current module's ports *)
      (match get_current_module_data ctx with
       | Some data -> data.mod_ports <- {port_name = name; port_direction = direction; port_width = width} :: data.mod_ports
       | None -> ());
      (* Also add to deprecated global list for compatibility *)
      ctx.ports <- {port_name = name; port_direction = direction; port_width = width} :: ctx.ports;
      (* Add to current module's symbol table *)
      let kind = if direction = "input" then PortInput else PortOutput in
      (match get_current_symbol_table ctx with
       | Some tbl -> Hashtbl.replace tbl name { signal_name = name; signal_kind = kind; signal_width = width }
       | None -> ());
      Some name
  | _ -> None

(* Extract ports from list, tracking last port declaration for shared ports *)
let rec extract_ports_from_list_items ctx last_dir last_width = function
  | [] -> ()
  | COMMA :: rest ->
      extract_ports_from_list_items ctx last_dir last_width rest
  | TUPLE5 (STRING "port_declaration_noattr1", dir, _, data_type, _) as port :: rest ->
      (match extract_port_decl ctx port with
       | Some (direction, _, width) ->
           extract_ports_from_list_items ctx (Some direction) (Some width) rest
       | None ->
           extract_ports_from_list_items ctx last_dir last_width rest)
  | TUPLE3 (STRING "port1", _, _) as port_ref :: rest ->
      (* This is a port reference sharing the previous port's direction/width *)
      (match last_dir, last_width with
       | Some dir, Some width ->
           ignore (extract_port_ref ctx dir width port_ref);
           extract_ports_from_list_items ctx last_dir last_width rest
       | _ ->
           extract_ports_from_list_items ctx last_dir last_width rest)
  | _ :: rest -> extract_ports_from_list_items ctx last_dir last_width rest

(* Extract all port declarations from port list - now expects TLIST directly from parser *)
let rec extract_ports_from_list ctx token =
  match token with
  | TLIST lst ->
      Printf.printf "  Port list has %d elements\n" (List.length lst);
      extract_ports_from_list_items ctx None None (List.rev lst)  (* Reverse because list is built backwards *)
  | single ->
      Printf.printf "  Port list is a single element\n";
      ignore (extract_port_decl ctx single)

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
  | TUPLE4 (STRING "add_expr3", left, HYPHEN, right) ->
      Printf.printf "SUB(";
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
  | TUPLE4 (STRING "xor_expr2", left, _, right) ->
      Printf.printf "XOR(";
      ignore (extract_expression left);
      Printf.printf ", ";
      ignore (extract_expression right);
      Printf.printf ")";
      Some token
  | TUPLE4 (STRING "and_expr2", left, _, right)
  | TUPLE4 (STRING "bitand_expr2", left, _, right) ->
      Printf.printf "AND(";
      ignore (extract_expression left);
      Printf.printf ", ";
      ignore (extract_expression right);
      Printf.printf ")";
      Some token
  | TUPLE4 (STRING "or_expr2", left, _, right) ->
      Printf.printf "OR(";
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
       | TLIST [TUPLE4 (STRING "cont_assign1", lhs, EQUALS, rhs)] ->
           Printf.printf "Assign statement:\n";
           (match extract_identifier lhs with
            | Some name ->
                Printf.printf "  LHS: %s = " name;
                (* Add to current module's assigns *)
                (match get_current_module_data ctx with
                 | Some data -> data.mod_assigns <- {assign_lhs = name; assign_rhs = rhs; assign_condition = None} :: data.mod_assigns
                 | None -> ());
                (* Also add to deprecated global list for compatibility *)
                ctx.assigns <- {assign_lhs = name; assign_rhs = rhs; assign_condition = None} :: ctx.assigns
            | None -> Printf.printf "  LHS: <unknown> = ");
           ignore (extract_expression rhs);
           Printf.printf "\n";
           Some token
       | _ -> None)
  | _ -> None

(* Extract assignment from statement *)
let rec extract_assignment_from_stmt stmt =
  match stmt with
  | TUPLE3 (STRING "statement_item6",
            TUPLE4 (STRING "assignment_statement_no_expr1", lhs, EQUALS, rhs),
            SEMICOLON) ->
      (match extract_identifier lhs with
       | Some name -> Some { assign_lhs = name; assign_rhs = rhs; assign_condition = None }
       | None -> None)
  | TUPLE6 (STRING "nonblocking_assignment1", lhs, _, EMPTY_TOKEN, rhs, SEMICOLON) ->
      (match extract_identifier lhs with
       | Some name -> Some { assign_lhs = name; assign_rhs = rhs; assign_condition = None }
       | None -> None)
  | _ -> None

(* Extract clock and edge from event control *)
let extract_clock_event token =
  match token with
  | TUPLE5 (STRING "event_control2", edge_token, LPAREN,
            TLIST [TUPLE3 (STRING "event_expression1", edge_inner, clock_id)], RPAREN) ->
      let edge = (match edge_token with
        | Posedge -> `Posedge
        | Negedge -> `Negedge
        | _ -> `Posedge) in
      (match extract_identifier clock_id with
       | Some clock_name -> Some (clock_name, edge)
       | None -> None)
  | _ -> None

(* Extract clock and reset from event control with multiple events *)
(* Returns: (clock_name, clock_edge, reset_name_opt, reset_edge_opt) *)
let extract_clock_and_reset_event token =
  match token with
  (* Pattern: @(posedge clk or posedge rst) - TUPLE5 with event_control2 *)
  | TUPLE5 (STRING "event_control2", _at, LPAREN, event_list, RPAREN) ->
      let rec find_edges events_list =
        match events_list with
        | TLIST events ->
            let rec process_events events =
              match events with
              | [] -> (None, None)
              | TUPLE3 (STRING "event_expression1", edge_token, signal_id) :: rest ->
                  let edge = (match edge_token with
                    | Posedge -> `Posedge
                    | Negedge -> `Negedge
                    | _ -> `Posedge) in
                  let signal_name = extract_identifier signal_id in
                  (match process_events rest with
                   | (None, None) -> (Some (signal_name, edge), None)
                   | (Some first, None) -> (Some first, Some (signal_name, edge))
                   | (first, second) -> (first, second))
              | _ :: rest -> process_events rest
            in
            process_events events
        | TUPLE3 (STRING "event_expression1", edge_token, signal_id) ->
            (* Single event *)
            let edge = (match edge_token with
              | Posedge -> `Posedge
              | Negedge -> `Negedge
              | _ -> `Posedge) in
            (Some (extract_identifier signal_id, edge), None)
        | _ -> (None, None)
      in
      (match find_edges event_list with
       | (Some (Some clk, clk_edge), Some (Some rst, rst_edge)) ->
           Some (clk, clk_edge, Some rst, Some rst_edge)
       | (Some (Some clk, clk_edge), None) ->
           Some (clk, clk_edge, None, None)
       | _ -> None)
  (* Pattern: @(posedge clk) with TUPLE4 - event_control1 *)
  | TUPLE4 (STRING "event_control1", AT, LPAREN, event_expr) ->
      (match event_expr with
       | TUPLE3 (STRING "event_expression1", edge_token, signal_id) ->
           let edge = (match edge_token with
             | Posedge -> `Posedge
             | Negedge -> `Negedge
             | _ -> `Posedge) in
           (match extract_identifier signal_id with
            | Some clk -> Some (clk, edge, None, None)
            | None -> None)
       | _ -> None)
  (* Fallback to old parser *)
  | _ ->
      (match extract_clock_event token with
       | Some (clk, edge) -> Some (clk, edge, None, None)
       | None -> None)

(* Check if a statement is a sensitivity list (combinational always) *)
let is_sensitivity_list token =
  match token with
  | TUPLE4 (STRING "event_control1", AT, LPAREN, TLIST _) ->
      (* Check if it contains only signal names without edges *)
      true  (* TODO: More precise detection *)
  | TUPLE3 (STRING "event_control4", AT, STAR) -> true
  | TUPLE5 (STRING "event_control3", AT, LPAREN, STAR, RPAREN) -> true
  | _ -> false

(* Extract assignments from case items *)
let rec extract_case_items case_items =
  match case_items with
  | TUPLE4 (STRING "case_item1", _expr_list, COLON, stmt) ->
      (* Single case item with value -> statement *)
      (match extract_assignment_from_stmt stmt with
       | Some assign -> [assign]
       | None -> [])
  | TUPLE3 (STRING "case_items1", rest, item) ->
      (* Multiple case items - recursive *)
      let rest_assigns = extract_case_items rest in
      let item_assigns = extract_case_items item in
      rest_assigns @ item_assigns
  | _ -> []

(* Extract LHS variable from case statement *)
let rec extract_case_lhs case_items =
  match case_items with
  | TUPLE4 (STRING "case_item1", _expr_list, COLON, stmt) ->
      (match extract_assignment_from_stmt stmt with
       | Some assign -> Some assign.assign_lhs
       | None -> None)
  | TUPLE3 (STRING "case_items1", _rest, item) ->
      extract_case_lhs item
  | _ -> None

(* Extract case statement as a single assignment *)
let extract_case_stmt ctx token =
  match token with
  | TUPLE8 (STRING "case_statement1", _unique, _case_kw, LPAREN, expr, RPAREN, items, Endcase) as case_token ->
      Printf.printf "  Case statement found\n";
      (* Get the LHS from one of the case items *)
      (match extract_case_lhs items with
       | Some lhs ->
           Printf.printf "    Case assigns to: %s\n" lhs;
           (* Return the entire case statement as the RHS *)
           [{ assign_lhs = lhs; assign_rhs = case_token; assign_condition = None }]
       | None -> [])
  | _ -> []

(* Extract assignments from a statement (handles case statements) *)
let rec extract_assigns_from_stmt ctx stmt =
  match stmt with
  | TUPLE3 (STRING "statement_item6", inner_stmt, _) ->
      (match extract_assignment_from_stmt stmt with
       | Some assign -> [assign]
       | None ->
           (* Check if inner_stmt is a conditional or seq_block *)
           (match inner_stmt with
            | TUPLE4 (STRING "seq_block1", _begin, stmts, _end) ->
                (match stmts with
                 | TLIST lst -> List.flatten (List.map (extract_assigns_from_stmt ctx) lst)
                 | single -> extract_assigns_from_stmt ctx single)
            | _ -> extract_assigns_from_stmt ctx inner_stmt))
  | TUPLE6 (STRING "nonblocking_assignment1", _, _, _, _, _) ->
      (* Direct nonblocking assignment *)
      (match extract_assignment_from_stmt stmt with
       | Some assign -> [assign]
       | None -> [])
  | TUPLE6 (STRING "conditional_statement2", _if_kw, _cond, then_stmt, _else_kw, else_stmt) ->
      (* if-else: extract from both branches *)
      let then_assigns = extract_assigns_from_stmt ctx then_stmt in
      let else_assigns = extract_assigns_from_stmt ctx else_stmt in
      then_assigns @ else_assigns
  | TUPLE7 (STRING "conditional_statement2", _label, _if_kw, _cond, then_stmt, _else_kw, else_stmt) ->
      (* if-else with label: extract from both branches *)
      let then_assigns = extract_assigns_from_stmt ctx then_stmt in
      let else_assigns = extract_assigns_from_stmt ctx else_stmt in
      then_assigns @ else_assigns
  | TUPLE4 (STRING "conditional_statement1", _if_kw, _cond, then_stmt) ->
      (* if without else: extract from then branch *)
      extract_assigns_from_stmt ctx then_stmt
  | TUPLE5 (STRING "conditional_statement1", _label, _if_kw, _cond, then_stmt) ->
      (* if without else, with label: extract from then branch *)
      extract_assigns_from_stmt ctx then_stmt
  | token when (match token with
                | TUPLE8 (STRING "case_statement1", _, _, _, _, _, _, _) -> true
                | _ -> false) ->
      extract_case_stmt ctx token
  | TUPLE4 (STRING "seq_block1", _begin, stmts, _end) ->
      (* Handle begin...end blocks *)
      (match stmts with
       | TLIST lst -> List.flatten (List.map (extract_assigns_from_stmt ctx) lst)
       | single -> extract_assigns_from_stmt ctx single)
  | _ -> []

(* Extract always_comb block *)
let extract_always_comb ctx stmt =
  let assigns = extract_assigns_from_stmt ctx stmt in
  if assigns <> [] then begin
    List.iter (fun assign ->
      Printf.printf "  Always_comb: %s = <expr>\n" assign.assign_lhs
    ) assigns;
    let always_blk = {
      always_type = AlwaysComb;
      always_stmts = assigns;
    } in
    (* Add to current module's always_blocks *)
    (match get_current_module_data ctx with
     | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
     | None -> ());
    (* Also add to deprecated global list for compatibility *)
    ctx.always_blocks <- always_blk :: ctx.always_blocks
  end

(* Extract always_ff block *)
let extract_always_ff ctx event_ctrl stmt =
  match extract_clock_event event_ctrl with
  | Some (clock, edge) ->
      (match extract_assignment_from_stmt stmt with
       | Some assign ->
           let edge_str = match edge with `Posedge -> "posedge" | `Negedge -> "negedge" in
           Printf.printf "  Always_ff @(%s %s): %s <= <expr>\n" edge_str clock assign.assign_lhs;
           let always_blk = {
             always_type = AlwaysFF { clock; edge; async_reset = None };
             always_stmts = [assign];
           } in
           (* Add to current module's always_blocks *)
           (match get_current_module_data ctx with
            | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
            | None -> ());
           (* Also add to deprecated global list for compatibility *)
           ctx.always_blocks <- always_blk :: ctx.always_blocks
       | None -> ())
  | None -> ()

(* Helper function to recursively extract signal identifier from expression *)
(* Handles: identifiers, comparisons like (RST == 1'b1), nested parentheses *)
let rec extract_signal_from_expr expr =
  match expr with
  (* Direct identifier *)
  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, _) -> Some id
  | SymbolIdentifier id -> Some id

  (* Comparison: extract left operand (e.g., RST == 1'b1 -> RST) *)
  | TUPLE4 (STRING "binary_eq_expr1", left_expr, _, _) ->
      extract_signal_from_expr left_expr
  | TUPLE4 (STRING "binary_ne_expr1", left_expr, _, _) ->
      extract_signal_from_expr left_expr
  | TUPLE4 (STRING "logeq_expr2", left_expr, _, _) ->
      extract_signal_from_expr left_expr
  | TUPLE4 (STRING "logneq_expr2", left_expr, _, _) ->
      extract_signal_from_expr left_expr

  (* Parentheses: unwrap and recurse *)
  | TUPLE4 (STRING "expression_in_parens1", _lparen, inner, _rparen) ->
      extract_signal_from_expr inner
  | TUPLE4 (STRING "expr_primary_parens1", _lparen, inner, _rparen) ->
      extract_signal_from_expr inner

  (* Negation: extract inner signal *)
  | TUPLE3 (STRING "unary_prefix_expr2", PLING, inner) ->
      extract_signal_from_expr inner

  (* Single-element list: unwrap *)
  | TLIST [single] ->
      extract_signal_from_expr single

  (* Multi-element list: try each element *)
  | TLIST lst ->
      List.fold_left (fun acc item ->
        match acc with
        | Some _ -> acc
        | None -> extract_signal_from_expr item
      ) None lst

  (* Verible expression wrappers: unwrap *)
  | TUPLE3 (STRING "sequence_repetition_expr1", inner, _) ->
      extract_signal_from_expr inner
  | TUPLE3 (STRING "expression_or_dist1", inner, _) ->
      extract_signal_from_expr inner

  (* Other cases: not recognized *)
  | _ -> None

(* Helper to combine two conditions with AND *)
let combine_conditions_with_and parent_cond new_cond =
  match parent_cond with
  | None -> Some new_cond
  | Some parent ->
      (* Build logical AND: parent && new_cond *)
      Some (TUPLE4 (STRING "logand_expr2", parent, AMPERSAND_AMPERSAND, new_cond))

(* Extract assignments WITH conditions for MUX tree generation *)
let rec extract_assigns_with_cond condition stmt =
  match stmt with
  | TUPLE3 (STRING "statement_item6", inner_stmt, _) ->
      (match extract_assignment_from_stmt stmt with
       | Some assign ->
           [{ assign with assign_condition = condition }]
       | None ->
           (* Check if inner_stmt is a conditional *)
           (match inner_stmt with
            | TUPLE6 (STRING "conditional_statement2", _, _, _, _, _)
            | TUPLE4 (STRING "conditional_statement1", _, _, _)
            | TUPLE5 (STRING "conditional_statement1", _, _, _, _) ->
                extract_if_else_assigns_with_cond condition inner_stmt
            | _ -> []))
  | TUPLE4 (STRING "seq_block1", _begin, stmts, _end) ->
      extract_assigns_from_seq_block_with_cond condition stmts
  | TUPLE6 (STRING "conditional_statement2", _, _, _, _, _) ->
      extract_if_else_assigns_with_cond condition stmt
  | TUPLE7 (STRING "conditional_statement2", _, _, _, _, _, _) ->
      extract_if_else_assigns_with_cond condition stmt
  | TUPLE4 (STRING "conditional_statement1", _, _, _) ->
      extract_if_else_assigns_with_cond condition stmt
  | TUPLE5 (STRING "conditional_statement1", _, _, _, _) ->
      extract_if_else_assigns_with_cond condition stmt
  | TUPLE6 (STRING "nonblocking_assignment1", _, _, _, _, _) ->
      (* Direct nonblocking assignment *)
      (match extract_assignment_from_stmt stmt with
       | Some assign ->
           [{ assign with assign_condition = condition }]
       | None -> [])
  | _ ->
      (match extract_assignment_from_stmt stmt with
       | Some assign ->
           [{ assign with assign_condition = condition }]
       | None -> [])

and extract_assigns_from_seq_block_with_cond condition stmts =
  match stmts with
  | TLIST lst ->
      let results_per_stmt = List.map (extract_assigns_with_cond condition) lst in
      List.flatten results_per_stmt
  | single -> extract_assigns_with_cond condition single

and extract_if_else_assigns_with_cond parent_cond stmt =
  match stmt with
  (* if (cond) stmt *)
  | TUPLE4 (STRING "conditional_statement1", _if_kw, cond_expr, then_stmt) ->
      (* Combine parent condition with current condition using AND *)
      let combined_cond = combine_conditions_with_and parent_cond cond_expr in
      extract_assigns_with_cond combined_cond then_stmt
  | TUPLE5 (STRING "conditional_statement1", _label, _if_kw, cond_expr, then_stmt) ->
      (* if (cond) stmt with label *)
      let combined_cond = combine_conditions_with_and parent_cond cond_expr in
      extract_assigns_with_cond combined_cond then_stmt
  (* if (cond) stmt1 else stmt2 *)
  | TUPLE6 (STRING "conditional_statement2", _if_kw, cond_expr, then_stmt, _else_kw, else_stmt) ->
      (* Then branch: parent AND current condition *)
      let then_cond = combine_conditions_with_and parent_cond cond_expr in
      let then_assigns = extract_assigns_with_cond then_cond then_stmt in
      (* Else branch: keep parent condition (else branch may have nested conditions) *)
      let else_assigns = extract_assigns_with_cond parent_cond else_stmt in
      then_assigns @ else_assigns
  | TUPLE7 (STRING "conditional_statement2", _label, _if_kw, cond_expr, then_stmt, _else_kw, else_stmt) ->
      (* if (cond) stmt1 else stmt2 with label *)
      let then_cond = combine_conditions_with_and parent_cond cond_expr in
      let then_assigns = extract_assigns_with_cond then_cond then_stmt in
      (* Else branch: keep parent condition *)
      let else_assigns = extract_assigns_with_cond parent_cond else_stmt in
      then_assigns @ else_assigns
  | _ -> []

and extract_assigns_from_if_branch stmt =
  (* Use new condition-tracking version with no parent condition *)
  extract_assigns_with_cond None stmt

and extract_assigns_from_seq_block stmts =
  match stmts with
  | TLIST lst -> List.flatten (List.map extract_assigns_from_if_branch lst)
  | single -> extract_assigns_from_if_branch single

and extract_if_else_assigns stmt =
  match stmt with
  (* if (cond) stmt *)
  | TUPLE4 (STRING "conditional_statement1", _if_kw, _cond, then_stmt) ->
      extract_assigns_from_if_branch then_stmt
  | TUPLE5 (STRING "conditional_statement1", _label, _if_kw, _cond, then_stmt) ->
      (* if (cond) stmt with label *)
      extract_assigns_from_if_branch then_stmt
  (* if (cond) stmt1 else stmt2 *)
  | TUPLE6 (STRING "conditional_statement2", _if_kw, _cond, then_stmt, _else_kw, else_stmt) ->
      let then_assigns = extract_assigns_from_if_branch then_stmt in
      let else_assigns = extract_assigns_from_if_branch else_stmt in
      then_assigns @ else_assigns
  | TUPLE7 (STRING "conditional_statement2", _label, _if_kw, _cond, then_stmt, _else_kw, else_stmt) ->
      (* if (cond) stmt1 else stmt2 with label *)
      let then_assigns = extract_assigns_from_if_branch then_stmt in
      let else_assigns = extract_assigns_from_if_branch else_stmt in
      then_assigns @ else_assigns
  | _ -> []

(* Analyze if/else structure for async reset/set pattern *)
(* Returns: (reset_signal, reset_polarity, reset_assigns, normal_assigns) *)
let rec analyze_async_reset_structure reset_signal stmt =
  match stmt with
  (* TUPLE7 conditional statement - handle this pattern *)
  | TUPLE7 (STRING "conditional_statement2", elem1, elem2, elem3, elem4, elem5, elem6) ->
      let _label = elem1 in
      let _if_kw = elem2 in
      let cond_expr = elem3 in
      let then_stmt = elem4 in
      let _else_kw = elem5 in
      let else_stmt = elem6 in

      (* Extract signal from condition using helper function *)
      let cond_str = extract_signal_from_expr cond_expr in

      (match cond_str with
       | Some cond_id when cond_id = reset_signal ->
           (* Condition is "if (reset)" - positive polarity *)
           let reset_assigns = extract_assigns_from_if_branch then_stmt in
           let normal_assigns = extract_assigns_from_if_branch else_stmt in
           Some (`Posedge, reset_assigns, normal_assigns)
       | Some _cond_id ->
           (* Condition might be negated reset or different signal *)
           let then_assigns = extract_assigns_from_if_branch then_stmt in
           let else_assigns = extract_assigns_from_if_branch else_stmt in
           Some (`Posedge, then_assigns, else_assigns)
       | None -> None)

  (* Unwrap nested TLIST *)
  | TLIST lst ->
      (match lst with
       | [single] -> analyze_async_reset_structure reset_signal single
       | _ ->
           (* Look for conditional in list *)
           (match List.find_opt (fun s ->
              match s with
              | TUPLE6 (STRING "conditional_statement2", _, _, _, _, _) -> true
              | TUPLE3 (STRING "statement_item6", TUPLE6 (STRING "conditional_statement2", _, _, _, _, _), _) -> true
              | _ -> false) lst with
            | Some if_stmt -> analyze_async_reset_structure reset_signal if_stmt
            | None -> None))

  (* Unwrap statement_item wrapper *)
  | TUPLE3 (STRING "statement_item6", inner_stmt, _) ->
      analyze_async_reset_structure reset_signal inner_stmt

  (* Unwrap seq_block (begin...end) *)
  | TUPLE4 (STRING "seq_block1", _begin, inner_stmts, _end) ->
      (match inner_stmts with
       | TLIST [single] ->
           analyze_async_reset_structure reset_signal single
       | TLIST lst ->
           (* Multiple statements in block - look for the if/else *)
           (match List.find_opt (fun s ->
              match s with
              | TUPLE6 (STRING "conditional_statement2", _, _, _, _, _) -> true
              | TUPLE7 (STRING "conditional_statement2", _, _, _, _, _, _) -> true
              | TUPLE3 (STRING "statement_item6", TUPLE6 (STRING "conditional_statement2", _, _, _, _, _), _) -> true
              | TUPLE3 (STRING "statement_item6", TUPLE7 (STRING "conditional_statement2", _, _, _, _, _, _), _) -> true
              | _ -> false) lst with
            | Some if_stmt ->
                analyze_async_reset_structure reset_signal if_stmt
            | None ->
                None)
       | single ->
           analyze_async_reset_structure reset_signal single)

  (* if (cond) then_stmt else else_stmt - typical async reset pattern *)
  | TUPLE6 (STRING "conditional_statement2", _if_kw, cond_expr, then_stmt, _else_kw, else_stmt) ->
      (* Extract signal from condition using helper function *)
      let cond_str = extract_signal_from_expr cond_expr in

      (match cond_str with
       | Some cond_id when cond_id = reset_signal ->
           (* Condition is "if (reset)" - positive polarity *)
           let reset_assigns = extract_assigns_from_if_branch then_stmt in
           let normal_assigns = extract_assigns_from_if_branch else_stmt in
           Some (`Posedge, reset_assigns, normal_assigns)
       | Some _cond_id ->
           (* Condition might be negated reset or different signal *)
           let then_assigns = extract_assigns_from_if_branch then_stmt in
           let else_assigns = extract_assigns_from_if_branch else_stmt in
           Some (`Posedge, then_assigns, else_assigns)
       | None -> None)

  (* if (cond) stmt - no else branch *)
  | TUPLE4 (STRING "conditional_statement1", _if_kw, _cond, then_stmt) ->
      (* This is not a typical async reset pattern *)
      None

  | _ -> None

(* Extract traditional always block with timing control *)
let extract_traditional_always ctx event_ctrl stmt =

  (* Try to extract clock and reset from event control *)
  match extract_clock_and_reset_event event_ctrl with
  | Some (clock, clock_edge, Some reset, Some reset_edge) ->
      (* Sequential logic with async reset/set: always @(posedge clk or posedge rst) *)
      Printf.printf "  Always @(%s %s or %s %s): Sequential with async reset/set\n"
        (match clock_edge with `Posedge -> "posedge" | `Negedge -> "negedge")
        clock
        (match reset_edge with `Posedge -> "posedge" | `Negedge -> "negedge")
        reset;

      (* Debug: print statement structure *)
      (match stmt with
       | TUPLE4 (STRING s, _, _, _) -> Printf.printf "    Statement structure: TUPLE4(%s, ...)\n" s
       | TUPLE6 (STRING s, _, _, _, _, _) -> Printf.printf "    Statement structure: TUPLE6(%s, ...)\n" s
       | _ -> Printf.printf "    Statement structure: Other\n");

      (* Analyze if/else structure to separate reset and normal assignments *)
      (match analyze_async_reset_structure reset stmt with
       | Some (_reset_polarity, reset_assigns, normal_assigns) ->
           (* We have a proper if/else structure *)
           Printf.printf "    Found %d reset assignments, %d normal assignments\n"
             (List.length reset_assigns) (List.length normal_assigns);

           (* Group all assignments by signal name to create one AlwaysFF per signal *)
           (* First, map reset assignments by signal *)
           let reset_map = Hashtbl.create 10 in
           List.iter (fun assign ->
             Hashtbl.replace reset_map assign.assign_lhs assign
           ) reset_assigns;

           (* Second, group normal assignments by signal *)
           let normal_map = Hashtbl.create 10 in
           List.iter (fun assign ->
             let existing = try Hashtbl.find normal_map assign.assign_lhs with Not_found -> [] in
             Hashtbl.replace normal_map assign.assign_lhs (assign :: existing)
           ) normal_assigns;

           (* Third, collect all unique signals *)
           let all_signals = Hashtbl.create 10 in
           List.iter (fun assign -> Hashtbl.replace all_signals assign.assign_lhs ()) reset_assigns;
           List.iter (fun assign -> Hashtbl.replace all_signals assign.assign_lhs ()) normal_assigns;

           (* Finally, create ONE AlwaysFF block per signal with ALL its assignments *)
           Hashtbl.iter (fun signal_name () ->
             let reset_assign_opt = try Some (Hashtbl.find reset_map signal_name) with Not_found -> None in
             (* Don't reverse - if Verible gives reverse chronological order, prepending already gives chronological *)
             let normal_assigns_list = try (Hashtbl.find normal_map signal_name) with Not_found -> [] in

             (match reset_assign_opt with
              | Some reset_assign ->
                  (* Create AlwaysFF with reset info and ALL normal assignments for this signal *)
                  Printf.printf "    Signal %s: has reset value\n" signal_name;
                  let reset_info = {
                    reset_signal = reset;
                    reset_edge = reset_edge;
                    reset_value = reset_assign.assign_rhs;
                    is_set = false;
                  } in
                  let always_blk = {
                    always_type = AlwaysFF { clock; edge = clock_edge; async_reset = Some reset_info };
                    always_stmts = normal_assigns_list;  (* ALL normal assignments for this signal *)
                  } in
                  (match get_current_module_data ctx with
                   | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
                   | None -> ());
                  ctx.always_blocks <- always_blk :: ctx.always_blocks
              | None ->
                  (* No reset for this signal *)
                  if normal_assigns_list <> [] then begin
                    Printf.printf "    Signal %s: no reset value\n" signal_name;
                    let always_blk = {
                      always_type = AlwaysFF { clock; edge = clock_edge; async_reset = None };
                      always_stmts = normal_assigns_list;
                    } in
                    (match get_current_module_data ctx with
                     | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
                     | None -> ());
                    ctx.always_blocks <- always_blk :: ctx.always_blocks
                  end)
           ) all_signals

       | None ->
           (* Couldn't analyze structure - fall back to extracting all assignments *)
           Printf.printf "    Could not analyze reset structure, extracting all assignments\n";
           let assigns = extract_if_else_assigns stmt in
           if assigns <> [] then begin
             List.iter (fun assign ->
               Printf.printf "    Assignment: %s <= <expr>\n" assign.assign_lhs
             ) assigns;
             let always_blk = {
               always_type = AlwaysFF { clock; edge = clock_edge; async_reset = None };
               always_stmts = assigns;
             } in
             (match get_current_module_data ctx with
              | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
              | None -> ());
             ctx.always_blocks <- always_blk :: ctx.always_blocks
           end else begin
             (* Try extracting from statement directly *)
             let direct_assigns = extract_assigns_from_stmt ctx stmt in
             if direct_assigns <> [] then begin
               List.iter (fun assign ->
                 Printf.printf "    Assignment: %s <= <expr>\n" assign.assign_lhs
               ) direct_assigns;
               let always_blk = {
                 always_type = AlwaysFF { clock; edge = clock_edge; async_reset = None };
                 always_stmts = direct_assigns;
               } in
               (match get_current_module_data ctx with
                | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
                | None -> ());
               ctx.always_blocks <- always_blk :: ctx.always_blocks
             end
           end)

  | Some (clock, clock_edge, None, None) ->
      (* Sequential logic without async reset: always @(posedge clk) *)
      Printf.printf "  Always @(%s %s): Sequential logic\n"
        (match clock_edge with `Posedge -> "posedge" | `Negedge -> "negedge")
        clock;

      (* Extract assignments *)
      let assigns = extract_assigns_from_stmt ctx stmt in
      if assigns <> [] then begin
        List.iter (fun assign ->
          Printf.printf "    Assignment: %s <= <expr>\n" assign.assign_lhs
        ) assigns;
        let always_blk = {
          always_type = AlwaysFF { clock; edge = clock_edge; async_reset = None };
          always_stmts = assigns;
        } in
        (* Add to current module's always_blocks *)
        (match get_current_module_data ctx with
         | Some data -> data.mod_always_blocks <- always_blk :: data.mod_always_blocks
         | None -> ());
        ctx.always_blocks <- always_blk :: ctx.always_blocks
      end

  | Some (_, _, Some _, None) | Some (_, _, None, Some _) ->
      (* Malformed reset event *)
      Printf.printf "  Note: Malformed reset event in always block\n"

  | None ->
      (* Might be combinational logic with explicit sensitivity list *)
      if is_sensitivity_list event_ctrl then begin
        Printf.printf "  Always @(sensitivity list): Combinational logic\n";
        extract_always_comb ctx stmt
      end else begin
        Printf.printf "  Note: Could not parse event control for traditional always block\n"
      end

(* Extract always construct *)
let extract_always_construct ctx token =
  match token with
  | TUPLE3 (STRING "always_construct1", Always_comb, stmt) ->
      Printf.printf "Always_comb block:\n";
      extract_always_comb ctx stmt
  | TUPLE3 (STRING "always_construct1", Always_ff,
            TUPLE3 (STRING "procedural_timing_control_statement2", event_ctrl, stmt)) ->
      (* Check if this is an async reset pattern *)
      (match extract_clock_and_reset_event event_ctrl with
       | Some (_, _, Some _, Some _) ->
           (* Has async reset/set - use traditional always handler *)
           Printf.printf "Always_ff block with async reset:\n";
           extract_traditional_always ctx event_ctrl stmt
       | _ ->
           (* Simple clock event - use always_ff handler *)
           Printf.printf "Always_ff block:\n";
           extract_always_ff ctx event_ctrl stmt)
  | TUPLE3 (STRING "always_construct1", Always,
            TUPLE3 (STRING "procedural_timing_control_statement2",
                    (TUPLE3 (STRING "event_control4", AT, STAR) |
                     TUPLE5 (STRING "event_control3", AT, LPAREN, STAR, RPAREN)),
                    stmt)) ->
      (* always-at-star or always-at-(star) - treat as always_comb *)
      Printf.printf "Always @* block:\n";
      extract_always_comb ctx stmt
  | TUPLE3 (STRING "always_construct1", Always,
            TUPLE3 (STRING "procedural_timing_control_statement2", event_ctrl, stmt)) ->
      (* Traditional always blocks with timing control: always @(posedge clk), always @(a or b), etc. *)
      Printf.printf "Traditional always block:\n";
      extract_traditional_always ctx event_ctrl stmt
  | _ -> ()

(* Extract data declaration (reg, wire, logic) *)
let extract_data_declaration ctx token =
  match token with
  | TUPLE3 (STRING "data_declaration1", data_type, var_decl_list) ->
      (* Extract signal type (reg, wire, logic) *)
      let signal_kind_opt = (match data_type with
        | TUPLE3 (STRING "data_type_primitive1", Reg, _) -> Some Reg
        | TUPLE3 (STRING "data_type_primitive1", Wire, _) -> Some Wire
        | TUPLE3 (STRING "data_type_primitive1", Logic, _) -> Some Logic
        | TUPLE4 (STRING "data_type_primitive1", Reg, _, _) -> Some Reg
        | TUPLE4 (STRING "data_type_primitive1", Wire, _, _) -> Some Wire
        | TUPLE4 (STRING "data_type_primitive1", Logic, _, _) -> Some Logic
        | TUPLE3 (STRING "data_type_primitive1", second, _) ->
            (match second with
             | Reg -> Some Reg
             | Wire -> Some Wire
             | Logic -> Some Logic
             | TUPLE3 (STRING "data_type_primitive_scalar1", scalar_type, _) ->
                 (match scalar_type with
                  | Reg -> Some Reg
                  | Wire -> Some Wire
                  | Logic -> Some Logic
                  | _ -> None)
             | TUPLE3 (STRING "data_type_primitive_scalar5", atom_type, _) ->
                 (* integer_atom_type: Byte/Shortint/Int/Longint/Integer/
                    Time.  All are register-like 4-state types in SV;
                    treat as Reg so the symbol-table registration in
                    extract_data_declaration fires. *)
                 (match width_of_integer_atom atom_type with
                  | Some _ -> Some Reg
                  | None -> None)
             | _ -> None)
        | _ -> None) in

      (* Extract width from data_type *)
      let width = (match extract_width_from_primitive ctx data_type with
        | Some (h, l) -> h - l + 1
        | None -> 1) in

      (* Extract variable name(s) *)
      let rec extract_var_names token =
        match token with
        | TUPLE3 (STRING "list_of_variable_decl_assignments1",
                  TUPLE3 (STRING "variable_decl_assignment1",
                          TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _), _), _) ->
            [name]
        | TUPLE3 (STRING "list_of_variable_decl_assignments2", rest, item) ->
            extract_var_names rest @ extract_var_names item
        | TUPLE3 (STRING "variable_decl_assignment1",
                  TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _), _) ->
            [name]
        | TUPLE3 (STRING s, second, third) ->
            (match s with
             | "register_variable1" ->
                 (* reg variable declaration *)
                 (match third with
                  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> [name]
                  | SymbolIdentifier name -> [name]
                  | _ -> [])
             | _ -> [])
        | TLIST lst ->
            List.concat (List.map extract_var_names lst)
        | TUPLE4 (STRING s, second, third, fourth) ->
            (match s with
             | "non_anonymous_gate_instance_or_register_variable1" ->
                 (* This could be a gate instance or register variable *)
                 (* Try second, third, and fourth elements *)
                 (match second with
                  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> [name]
                  | SymbolIdentifier name -> [name]
                  | _ ->
                      (match third with
                       | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> [name]
                       | SymbolIdentifier name -> [name]
                       | _ ->
                           (match fourth with
                            | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> [name]
                            | SymbolIdentifier name -> [name]
                            | _ -> [])))
             | _ -> [])
        | SymbolIdentifier name -> [name]
        | _ -> []
      in

      let names = extract_var_names var_decl_list in
      (match signal_kind_opt with
       | Some kind ->
           List.iter (fun name ->
             Printf.printf "  Signal: %s %s (width=%d)\n"
               (match kind with Reg -> "reg" | Wire -> "wire" | Logic -> "logic" | _ -> "?")
               name width;
             (* Add to current module's symbol table *)
             match get_current_symbol_table ctx with
             | Some tbl -> Hashtbl.replace tbl name
                 { signal_name = name; signal_kind = kind; signal_width = width }
             | None -> ()
           ) names
       | None -> ())
  | _ -> ()

(* Extract continuous assignments and always blocks from module items *)
let rec extract_assigns_from_items ctx token =
  match token with
  | TLIST lst ->
      (* Process each item in the list *)
      List.iter (extract_assigns_from_items ctx) lst
  | TUPLE3 (STRING "data_declaration1", _, _) as data_decl ->
      extract_data_declaration ctx data_decl
  | TUPLE3 (STRING "data_declaration_or_module_instantiation1", inner, _) ->
      (* This could be a data declaration or module instantiation *)
      (match inner with
       | TUPLE3 (STRING "instantiation_base1", inst_type, inst_items) ->
           (* Check if it's a data type (logic, reg, wire) or module instance *)
           (match inst_type with
            | TUPLE3 (STRING s, _, _) ->
                (match s with
                 | "data_type_or_implicit1" ->
                     (* Extract the actual data type *)
                     (match inst_type with
                      | TUPLE3 (STRING "data_type_or_implicit1", data_type, _) ->
                          extract_data_declaration ctx (TUPLE3 (STRING "data_declaration1", data_type, inst_items))
                      | _ -> ())
                 | "data_type_primitive1" ->
                     (* This is directly a primitive data type (logic, reg, wire) *)
                     extract_data_declaration ctx (TUPLE3 (STRING "data_declaration1", inst_type, inst_items))
                 | _ -> (* Likely module instantiation, skip *) ())
            | _ -> ())
       | _ ->
           extract_assigns_from_items ctx inner)
  | TUPLE4 (STRING s, _, _, _) when String.starts_with ~prefix:"data_declaration" s ->
      Printf.printf "  DEBUG: Found %s (TUPLE4)\n" s;
      extract_data_declaration ctx token
  | TUPLE6 (STRING "continuous_assign1", _, _, _, _, _) ->
      Printf.printf "Found continuous_assign\n";
      ignore (extract_continuous_assign ctx token)
  | TUPLE3 (STRING "always_construct1", _, _) as always_token ->
      Printf.printf "Found always_construct\n";
      extract_always_construct ctx always_token
  | TUPLE11 (STRING "function_declaration1", _function_kw, _lifetime, return_type_and_id, _lparen, params, _rparen, _semicolon, body, _endfunction, _label) ->
      Printf.printf "Found function_declaration1\n";
      extract_function_declaration ctx return_type_and_id params body
  | TUPLE9 (STRING "function_declaration2", _function_kw, _lifetime, return_type_and_id, _semicolon, _items, body, _endfunction, _label) ->
      Printf.printf "Found function_declaration2\n";
      extract_function_declaration ctx return_type_and_id EMPTY_TOKEN body
  | TUPLE8 (STRING "function_declaration3", _function_kw, _lifetime, return_type_and_id, _semicolon, body, _endfunction, _label) ->
      Printf.printf "Found function_declaration3\n";
      extract_function_declaration ctx return_type_and_id EMPTY_TOKEN body
  | _ -> ()

(* Extract function declaration *)
and extract_function_declaration ctx return_type_and_id params body =
  (* Extract function name from return_type_and_id *)
  let (func_name, return_width) = extract_function_name_and_width ctx return_type_and_id in
  (* Extract parameter list *)
  let param_list = extract_function_params ctx params in
  (* Store function info *)
  (match get_current_module_data ctx with
   | Some mod_data ->
       let func_info = {
         func_name;
         func_params = param_list;
         func_return_width = return_width;
         func_body = body;
       } in
       mod_data.mod_functions <- func_info :: mod_data.mod_functions;
       Printf.printf "  Function: %s (return width=%d, params=%d)\n"
         func_name return_width (List.length param_list)
   | None -> ())

and extract_function_name_and_width ctx token =
  (* function_return_type_and_id passes through data_type_or_implicit_basic_followed_by_id_and_dimensions_opt *)
  match token with
  | TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1", dtype, class_id, _dimensions) ->
      let name = (match class_id with
        | SymbolIdentifier n -> n
        | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
        | TUPLE3 (STRING "class_id1", SymbolIdentifier n, _) -> n
        | _ -> "unknown_func") in
      let width = (match extract_width_from_primitive ctx dtype with
        | Some (h, l) -> h - l + 1
        | None -> 1) in
      (name, width)
  | TUPLE3 (STRING "function_return_type_and_id1", return_type, id) ->
      let name = (match id with
        | SymbolIdentifier n -> n
        | _ -> "unknown_func") in
      let width = extract_width_from_type ctx return_type in
      (name, width)
  | _ -> ("unknown_func", 1)

and extract_function_params ctx token =
  (* tf_port_list_opt can be EMPTY_TOKEN or a list of ports *)
  match token with
  | EMPTY_TOKEN -> []
  | TUPLE3 (STRING "tf_port_list1", _items, port_list) ->
      extract_param_list ctx port_list
  | TLIST items ->
      List.concat_map (extract_param_list ctx) items
  | _ ->
      extract_param_list ctx token

and extract_param_list ctx token =
  match token with
  | TLIST items ->
      List.filter_map (extract_single_param ctx) items
  | single ->
      (match extract_single_param ctx single with
       | Some p -> [p]
       | None -> [])

and extract_single_param ctx token =
  (* Extract parameter from tf_port_item *)
  match token with
  | TUPLE3 (STRING "tf_port_item1", dtype, id_token) ->
      (* data_type identifier *)
      let param_name = extract_param_name id_token in
      let param_width = (match extract_width_from_primitive ctx dtype with
        | Some (h, l) -> h - l + 1
        | None -> 1) in
      Some { param_name; param_width }
  | TUPLE4 (STRING "tf_port_item2", _var, dtype, id_token) ->
      (* var data_type identifier *)
      let param_name = extract_param_name id_token in
      let param_width = (match extract_width_from_primitive ctx dtype with
        | Some (h, l) -> h - l + 1
        | None -> 1) in
      Some { param_name; param_width }
  | _ -> None

and extract_param_name token =
  match token with
  | SymbolIdentifier name -> name
  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> name
  | TUPLE3 (STRING "tf_port_id1", SymbolIdentifier name, _) -> name
  | TUPLE4 (STRING "tf_port_id2", SymbolIdentifier name, _, _) -> name
  | _ -> "unknown_param"

and extract_width_from_type ctx token =
  (* Extract width from data type - simplified for now *)
  match token with
  | TUPLE3 (STRING "data_type_or_implicit1", dtype, _) ->
      (match extract_width_from_primitive ctx dtype with
       | Some (h, l) -> h - l + 1
       | None -> 1)
  | _ -> 1

(* Extract module information from TUPLE12 module declaration *)
let rec extract_parameters ctx token =
  match token with
  | TLIST lst ->
      List.iter (extract_parameters ctx) lst
  | TUPLE5 (STRING "module_parameter_port_list_opt1", _hash, _lparen, param_decls, _rparen) ->
      (* Module parameter list: #(parameter ..., parameter ...) *)
      extract_parameters ctx param_decls
  | TUPLE5 (STRING "parameter_port_list2", _hash, _lparen, param_decls, _rparen) ->
      (* Module parameter list: #(parameter ..., parameter ...) *)
      extract_parameters ctx param_decls
  | TUPLE4 (STRING "module_parameter_port1", _param_kw, param_decl, after_decl) ->
      (* Single parameter declaration *)
      (* First extract the parameter name from param_decl *)
      extract_parameters ctx param_decl;
      (* Then extract the assignment value from after_decl *)
      (match after_decl with
       | TUPLE4 (STRING "trailing_assign1", _eq, value_expr, _semicolon) ->
           (* Extract the parameter name that was just added *)
           (match ctx.params with
            | (name, _) :: rest ->
                let value = eval_param_value ctx value_expr in
                Printf.printf "  Parameter %s = %d\n" name value;
                (* Update the most recent parameter with the actual value *)
                ctx.params <- (name, EInt value) :: rest
            | [] -> ())
       | _ -> ())
  | TUPLE3 (STRING "param_declaration2", _param_kw, type_assign_list) ->
      (* Parameter declaration *)
      extract_parameters ctx type_assign_list
  | TUPLE3 (STRING "type_assignment_list1", item, _) ->
      (* Single parameter *)
      extract_single_param_decl ctx item
  | TUPLE3 (STRING "type_assignment_list2", rest, item) ->
      (* Multiple parameters *)
      extract_parameters ctx rest;
      extract_single_param_decl ctx item
  | TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", type_token, name_token, dimensions, trailing_assign) ->
      (* parameter declaration: TYPE NAME [dimensions] = VALUE *)
      (* Extract name from dimensions token (name_token is typically EMPTY_TOKEN) *)
      let name = match name_token with
        | SymbolIdentifier n -> n
        | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
        | EMPTY_TOKEN ->
            (* Name is in dimensions token *)
            (match dimensions with
             | SymbolIdentifier n -> n
             | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
             | _ -> "unknown_param")
        | _ -> "unknown_param"
      in
      (* trailing_assign is typically EMPTY_TOKEN; value comes from after_decl in parent pattern *)
      Printf.printf "  Parameter: %s = %d\n" name 0;
      ctx.params <- (name, EInt 0) :: ctx.params
  | TUPLE5 (STRING "type_assignment1", _param_type, name_token, _eq, value_expr) ->
      (* parameter TYPE NAME = VALUE *)
      let name = match name_token with
        | SymbolIdentifier n -> n
        | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
        | _ -> "unknown_param"
      in
      let value = eval_param_value ctx value_expr in
      Printf.printf "  Parameter: %s = %d\n" name value;
      ctx.params <- (name, EInt value) :: ctx.params
  | _ ->
      ()

and extract_single_param_decl ctx token =
  match token with
  | TUPLE5 (STRING "type_assignment1", _param_type, name_token, _eq, value_expr) ->
      let name = match name_token with
        | SymbolIdentifier n -> n
        | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
        | _ -> "unknown_param"
      in
      let value = eval_param_value ctx value_expr in
      Printf.printf "  Parameter: %s = %d\n" name value;
      ctx.params <- (name, EInt value) :: ctx.params
  | _ -> ()

and eval_param_value ctx expr =
  (* Evaluate constant parameter expression *)
  match eval_const_expr ctx expr with
  | EInt i -> i
  | _ ->
      (* Fallback for simple cases *)
      (match expr with
       | TK_DecNumber n -> (try int_of_string n with _ -> 0)
       | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
       | _ -> 0)
let extract_module_info ctx token =
  match token with
  | TUPLE12 (STRING "module_or_interface_declaration1", Module, _, name_token, _, params, ports, _, _, items, _, _) ->
      (match name_token with
       | SymbolIdentifier name ->
           Printf.printf "Module: %s\n" name;
           ctx.module_name <- Some name;
           (* Set current module for symbol table *)
           ctx.current_module <- Some name;
           (* Extract parameters if present *)
           (match params with
            | EMPTY_TOKEN -> ()
            | _ ->
                Printf.printf "Parameters:\n";
                extract_parameters ctx params);
           (* Extract ports *)
           (match ports with
            | EMPTY_TOKEN ->
                Printf.printf "Ports: <none - EMPTY_TOKEN>\n"
            | TUPLE4 (STRING "module_port_list_opt1", LPAREN, port_list, RPAREN) ->
                Printf.printf "Ports:\n";
                extract_ports_from_list ctx port_list
            | _ ->
                Printf.printf "Ports: <unknown pattern>\n");
           (* Extract assigns *)
           (match items with
            | EMPTY_TOKEN -> ()
            | _ ->
                Printf.printf "\nStatements:\n";
                extract_assigns_from_items ctx items);
           (* Clear current module when done *)
           ctx.current_module <- None;
           Some name
       | _ -> None)
  | _ -> None

(* Extract parameter declarations - simplified stub *)

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
    | TLIST lst ->
        List.iter walk_tree lst
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
