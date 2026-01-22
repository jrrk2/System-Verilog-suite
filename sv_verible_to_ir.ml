(* sv_verible_to_ir.ml - Convert Verible AST to IR *)

[@@@warning "-33"]

open Sv_ast
open Source_text_verible
open Source_text_verible_tokens

(* ============================================================================
   TOKEN DUMPING - Convert tokens to readable JSON-like format for debugging
   ============================================================================ *)

let rec token_to_json_string ?(max_depth=3) depth token =
  if depth > max_depth then "..." else
  let indent = String.make (depth * 2) ' ' in
  let next_indent = String.make ((depth + 1) * 2) ' ' in
  match token with
  | EMPTY_TOKEN -> "EMPTY"
  | STRING s -> Printf.sprintf "\"%s\"" s
  | SymbolIdentifier id -> Printf.sprintf "SymbolIdentifier(%s)" id
  | TK_UnBasedNumber n -> Printf.sprintf "UnBasedNumber(%s)" n
  | TK_DecNumber n -> Printf.sprintf "DecNumber(%s)" n
  | TUPLE2 (a, b) ->
      Printf.sprintf "TUPLE2(\n%s%s,\n%s%s\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        indent
  | TUPLE3 (a, b, c) ->
      Printf.sprintf "TUPLE3(\n%s%s,\n%s%s,\n%s%s\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent (token_to_json_string ~max_depth (depth+1) c)
        indent
  | TUPLE4 (a, b, c, d) ->
      Printf.sprintf "TUPLE4(\n%s%s,\n%s%s,\n%s%s,\n%s%s\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent (token_to_json_string ~max_depth (depth+1) c)
        next_indent (token_to_json_string ~max_depth (depth+1) d)
        indent
  | TUPLE5 (a, b, c, d, e) ->
      Printf.sprintf "TUPLE5(\n%s%s,\n%s%s,\n%s%s,\n%s%s,\n%s%s\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent (token_to_json_string ~max_depth (depth+1) c)
        next_indent (token_to_json_string ~max_depth (depth+1) d)
        next_indent (token_to_json_string ~max_depth (depth+1) e)
        indent
  | TUPLE6 (a, b, c, d, e, f) ->
      Printf.sprintf "TUPLE6(\n%s%s,\n%s%s,\n%s%s,\n%s%s,\n%s%s,\n%s%s\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent (token_to_json_string ~max_depth (depth+1) c)
        next_indent (token_to_json_string ~max_depth (depth+1) d)
        next_indent (token_to_json_string ~max_depth (depth+1) e)
        next_indent (token_to_json_string ~max_depth (depth+1) f)
        indent
  | TUPLE7 (a, b, c, d, e, f, g) ->
      Printf.sprintf "TUPLE7(\n%s%s,\n%s%s,\n%s...\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent indent
  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      Printf.sprintf "TUPLE8(\n%s%s,\n%s%s,\n%s...\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent indent
  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      Printf.sprintf "TUPLE9(\n%s%s,\n%s%s,\n%s...\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent indent
  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      Printf.sprintf "TUPLE12(\n%s%s,\n%s%s,\n%s...\n%s)"
        next_indent (token_to_json_string ~max_depth (depth+1) a)
        next_indent (token_to_json_string ~max_depth (depth+1) b)
        next_indent indent
  | TLIST items ->
      if List.length items = 0 then "[]"
      else if List.length items <= 3 then
        Printf.sprintf "[\n%s%s\n%s]"
          next_indent
          (String.concat (",\n" ^ next_indent) (List.map (token_to_json_string ~max_depth (depth+1)) items))
          indent
      else
        Printf.sprintf "[%d items: %s, ...]"
          (List.length items)
          (token_to_json_string ~max_depth (depth+1) (List.hd items))
  (* Use getstr for any other token *)
  | other -> getstr other

(* Parse Verilog file using Verible parser *)
let parse_verible_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  try
    (* Create deflated lexer that converts token list to stream *)
    let deflated_lexer = Source_text_verible_lex.deflate Source_text_verible_lex.token in

    (* Parse with the deflated lexer to get TUPLE tree *)
    let parse_tree = Source_text_verible.ml_start deflated_lexer lexbuf in
    close_in ic;
    Some parse_tree
  with
  | Parsing.Parse_error ->
      close_in ic;
      Printf.eprintf "Parse error in %s\n" filename;
      None
  | e ->
      close_in ic;
      Printf.eprintf "Error parsing %s: %s\n" filename (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      None

(* ============================================================================
   FUNCTION INLINING - Convert function bodies to ternary expressions
   ============================================================================ *)

(* Convert function body (token tree) to expression token
   This handles case statements and return statements, converting them to nested ternary expressions *)
let rec convert_function_body_to_expr func_name body_token =

  (* Search for return statements in the function body *)
  match body_token with
  | TUPLE8 (STRING "case_statement1", _case_or_casex, _unique_or_priority, _lparen, sel_expr, _rparen, items, _endcase) ->
      (* case (...) case_item* endcase *)
      Some (convert_case_body_to_ternary sel_expr items)

  | TUPLE9 (STRING "case_statement3", _pos1, _pos2, _pos3, pos4, _pos5, _pos6, pos7, _pos8) ->
      (* case (...) inside case_item* endcase *)
      (* pos4 is the selector expression, pos7 is case_inside_items1 *)
      Some (convert_case_body_to_ternary pos4 pos7)

  | TUPLE3 (STRING "case_statement1", Case, case_items) ->
      (* case (...) ... endcase *)
      (match case_items with
       | TUPLE6 (STRING s, _lparen, sel_expr, _rparen, items, _endcase) when s = "case_statement_body1" || s = "case_inside_statement_body1" ->
           Some (convert_case_body_to_ternary sel_expr items)
       | _ -> None)

  | TUPLE4 (STRING "jump_statement1", Return, return_expr, _semicolon) ->
      (* return <expr>; *)
      Some return_expr

  | TUPLE3 (STRING "seq_block1", Begin, items) ->
      (* begin ... end *)
      convert_items_to_expr func_name items

  | TUPLE4 (STRING "seq_block2", Begin, _label, items) ->
      (* begin:label ... end *)
      convert_items_to_expr func_name items

  | TLIST items ->
      (* List of statements - search through them *)
      List.find_map (convert_function_body_to_expr func_name) items

  | _ -> None

and convert_items_to_expr func_name items_token =
  match items_token with
  | TLIST items -> List.find_map (convert_function_body_to_expr func_name) items
  | _ -> convert_function_body_to_expr func_name items_token

and convert_case_body_to_ternary sel_expr case_items =
  (* Convert case items to nested ternary expressions *)
  match case_items with
  | TLIST items -> convert_case_items_list sel_expr items
  | TUPLE3 (STRING "case_items1", _left, items) ->
      (* case_items1 wrapper *)
      convert_case_body_to_ternary sel_expr items
  | TUPLE3 (STRING "case_inside_items1", _left, items) ->
      (* case_inside_items1 wrapper *)
      convert_case_body_to_ternary sel_expr items
  | single_item ->
      (* Single case item, not wrapped in TLIST *)
      convert_case_items_list sel_expr [single_item]

and convert_case_items_list sel_expr items =
  (* Process case items from last to first, building nested ternary expressions *)
  let rec process_items items =
    match items with
    | [] ->
        (* No items, return 0 as default *)
        TK_UnBasedNumber "0"
    | [single_item] ->
        (* Last item (or only item) *)
        (match extract_case_item_value single_item with
         | Some (conditions, value_expr) ->
             if conditions = [] then
               (* Default case *)
               value_expr
             else
               (* Last conditional case - use value or default 0 *)
               let condition = build_case_condition sel_expr conditions in
               TUPLE5 (STRING "cond_expr1", condition, STRING "?", value_expr, TK_UnBasedNumber "0")
         | None -> TK_UnBasedNumber "0")
    | first_item :: rest ->
        (* Build nested ternary: condition ? value : rest *)
        let else_expr = process_items rest in
        (match extract_case_item_value first_item with
         | Some (conditions, value_expr) ->
             if conditions = [] then
               (* Default case at non-last position - use it as final else *)
               value_expr
             else
               let condition = build_case_condition sel_expr conditions in
               TUPLE5 (STRING "cond_expr1", condition, STRING "?", value_expr, else_expr)
         | None -> else_expr)
  in
  process_items items

and extract_case_item_value item_token =
  (* Extract condition and value from a case item *)
  match item_token with
  | TUPLE4 (STRING "case_item1", condition_list, _colon, stmt) ->
      (* condition: statement *)
      let conditions = extract_case_conditions condition_list in
      let value = find_return_value stmt in
      Some (conditions, value)
  | TUPLE5 (STRING "case_item2", condition_list, _colon, stmt, _) ->
      (* condition: statement *)
      let conditions = extract_case_conditions condition_list in
      let value = find_return_value stmt in
      Some (conditions, value)
  | TUPLE4 (STRING "case_item3", Default, _colon, stmt) ->
      (* default: statement *)
      let value = find_return_value stmt in
      Some ([], value) (* Empty conditions = default *)
  | TUPLE4 (STRING "case_inside_item2", condition_list, _colon, stmt) ->
      (* case inside: condition: statement *)
      let conditions = extract_case_conditions condition_list in
      let value = find_return_value stmt in
      Some (conditions, value)
  | _ -> None

and extract_case_conditions condition_token =
  (* Extract list of conditions from case item *)
  match condition_token with
  | TLIST items -> items
  | single -> [single]

and find_return_value stmt_token =
  (* Find the return value in a statement *)
  match stmt_token with
  | TUPLE4 (STRING "jump_statement1", Return, return_expr, _semicolon) ->
      return_expr
  | TUPLE3 (STRING "seq_block1", Begin, items) ->
      (match convert_items_to_expr "return" items with
       | Some expr -> expr
       | None -> TK_UnBasedNumber "0")
  | TUPLE4 (STRING "seq_block2", Begin, _label, items) ->
      (match convert_items_to_expr "return" items with
       | Some expr -> expr
       | None -> TK_UnBasedNumber "0")
  | TLIST items ->
      (match List.find_map (fun item ->
         match find_return_value item with
         | TK_UnBasedNumber "0" -> None
         | v -> Some v
       ) items with
       | Some v -> v
       | None -> TK_UnBasedNumber "0")
  | _ -> TK_UnBasedNumber "0"

and build_case_condition sel_expr conditions =
  (* Build comparison expression(s) for case conditions *)
  match conditions with
  | [] -> TK_UnBasedNumber "1" (* Always true for default *)
  | [single_cond] ->
      (* Single condition: sel_expr == condition *)
      TUPLE4 (STRING "binary_eq_expr1", sel_expr, STRING "==", single_cond)
  | first :: rest ->
      (* Multiple conditions: (sel_expr == c1) || (sel_expr == c2) || ... *)
      let first_cmp = TUPLE4 (STRING "binary_eq_expr1", sel_expr, STRING "==", first) in
      List.fold_left (fun acc cond ->
        let cmp = TUPLE4 (STRING "binary_eq_expr1", sel_expr, STRING "==", cond) in
        TUPLE4 (STRING "binary_logor_expr1", acc, STRING "||", cmp)
      ) first_cmp rest

(* Substitute parameter in expression token tree *)
let rec substitute_param_in_token param_name arg_token token =
  match token with
  | SymbolIdentifier name when name = param_name ->
      arg_token
  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, rest) when name = param_name ->
      arg_token
  | TUPLE3 (s, a, b) ->
      TUPLE3 (s, substitute_param_in_token param_name arg_token a, substitute_param_in_token param_name arg_token b)
  | TUPLE4 (s, a, b, c) ->
      TUPLE4 (s, substitute_param_in_token param_name arg_token a, substitute_param_in_token param_name arg_token b, substitute_param_in_token param_name arg_token c)
  | TUPLE5 (s, a, b, c, d) ->
      TUPLE5 (s, substitute_param_in_token param_name arg_token a, substitute_param_in_token param_name arg_token b, substitute_param_in_token param_name arg_token c, substitute_param_in_token param_name arg_token d)
  | TUPLE6 (s, a, b, c, d, e) ->
      TUPLE6 (s, substitute_param_in_token param_name arg_token a, substitute_param_in_token param_name arg_token b, substitute_param_in_token param_name arg_token c, substitute_param_in_token param_name arg_token d, substitute_param_in_token param_name arg_token e)
  | TLIST items ->
      TLIST (List.map (substitute_param_in_token param_name arg_token) items)
  | _ -> token

(* Extract arguments from function call *)
let rec extract_function_args args_token =
  match args_token with
  | TUPLE3 (STRING "list_of_arguments1", _lparen, arg_list) ->
      extract_arg_list arg_list
  | TUPLE4 (STRING "list_of_arguments2", _lparen, arg_list, _rparen) ->
      extract_arg_list arg_list
  | _ -> []

and extract_arg_list arg_list =
  match arg_list with
  | TLIST items ->
      List.filter_map (fun item ->
        match item with
        | TUPLE3 (STRING "argument1", _name, expr) -> Some expr
        | expr -> Some expr
      ) items
  | single -> [single]

(* Get width of a value by looking it up in IR *)
let get_value_width ir value_id =
  (* Check if it's an input *)
  let input_width = Hashtbl.fold (fun _name value acc ->
    match acc with
    | Some w -> Some w
    | None ->
        match value with
        | Sv_ast.Input { id; width; _ } when id = value_id -> Some width
        | _ -> None
  ) ir.ir_inputs None in
  match input_width with
  | Some w -> w
  | None ->
      (* Check if it's a node *)
      match Hashtbl.find_opt ir.ir_nodes value_id with
      | Some node -> (match node.node_op with
          | Sv_ast.Add { width; _ } | Sv_ast.Sub { width; _ } | Sv_ast.Mul { width; _ }
          | Sv_ast.And { width } | Sv_ast.Or { width } | Sv_ast.Xor { width } | Sv_ast.Not { width } -> width
          | _ -> 32)
      | None -> 32  (* Default width *)

(* Convert expression tree to IR value_id *)
let rec expr_to_ir ir expr_cache symbol_table functions expr =
  (* Check if we've already converted this expression *)
  try
    Hashtbl.find expr_cache expr
  with Not_found ->
    let result =
      match expr with
      | TUPLE4 (STRING "add_expr2", left, PLUS, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Add { width; signed = false }) [left_id; right_id]

      | TUPLE4 (STRING "add_expr3", left, HYPHEN, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Sub { width; signed = false }) [left_id; right_id]

      | TUPLE4 (STRING "mul_expr2", left, STAR, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width_l = get_value_width ir left_id in
          let width_r = get_value_width ir right_id in
          let width = width_l + width_r in  (* Multiply result width *)
          Sv_opt_ir.add_node ir (Mul { width; signed = false }) [left_id; right_id]

      | TUPLE4 (STRING "xor_expr2", left, _, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Xor { width }) [left_id; right_id]

      | TUPLE4 (STRING "and_expr2", left, _, right)
      | TUPLE4 (STRING "bitand_expr2", left, _, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (And { width }) [left_id; right_id]

      | TUPLE4 (STRING "or_expr2", left, _, right)
      | TUPLE4 (STRING "bitor_expr2", left, _, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Or { width }) [left_id; right_id]

      | TUPLE4 (STRING "binary_eq_expr1", left, _, right) ->
          (* Equality comparison: left == right *)
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Compare { width; cmp_op = `Eq; signed = false }) [left_id; right_id]

      | TUPLE4 (STRING "binary_logor_expr1", left, _, right) ->
          (* Logical OR: left || right *)
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = 1 in (* Logical operations return 1 bit *)
          Sv_opt_ir.add_node ir (Or { width }) [left_id; right_id]

      | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) ->
          (* Look up in symbol table *)
          (match Hashtbl.find_opt symbol_table name with
           | Some signal_info ->
               (* Check if it's an input in the IR *)
               (try
                 let v = Hashtbl.find ir.ir_inputs name in
                 (match v with
                  | Sv_ast.Input { id; _ } -> id
                  | _ -> Sv_opt_ir.get_new_id ir)
               with Not_found ->
                 (* Check if it's a wire *)
                 (try
                   let v = Hashtbl.find ir.ir_wires name in
                   (match v with
                    | Sv_ast.Wire { id; _ } -> id
                    | _ -> Sv_opt_ir.get_new_id ir)
                 with Not_found ->
                   (* Not found in IR - might not be assigned yet *)
                   Sv_opt_ir.get_new_id ir))
           | None ->
               Printf.eprintf "Warning: Unknown identifier '%s' in expression\n" name;
               Sv_opt_ir.get_new_id ir)

      | SymbolIdentifier name ->
          (* Look up in symbol table *)
          (match Hashtbl.find_opt symbol_table name with
           | Some signal_info ->
               (* Check if it's an input in the IR *)
               (try
                 let v = Hashtbl.find ir.ir_inputs name in
                 (match v with
                  | Sv_ast.Input { id; _ } -> id
                  | _ -> Sv_opt_ir.get_new_id ir)
               with Not_found ->
                 (* Check if it's a wire *)
                 (try
                   let v = Hashtbl.find ir.ir_wires name in
                   (match v with
                    | Sv_ast.Wire { id; _ } -> id
                    | _ -> Sv_opt_ir.get_new_id ir)
                 with Not_found ->
                   (* Not found in IR - might not be assigned yet *)
                   Sv_opt_ir.get_new_id ir))
           | None ->
               Printf.eprintf "Warning: Unknown identifier '%s'\n" name;
               Sv_opt_ir.get_new_id ir)

      | TUPLE3 (STRING "reference3", base_expr, index_expr) ->
          (* Array indexing or bit selection: array[index] or signal[bit] *)
          (* Check if this is actually bit-select (index is select_variable_dimension) *)
          (match index_expr with
           | TUPLE4 (STRING "select_variable_dimension2", _lbracket, idx, _rbracket) ->
               (* This is bit selection: signal[index] *)
               let base_id = expr_to_ir ir expr_cache symbol_table functions base_expr in
               let index_val = (match idx with
                 | TK_DecNumber n -> (try int_of_string n with _ -> 0)
                 | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
                 | _ ->
                     Printf.printf "  Warning: Dynamic bit index, using index 0\n";
                     0
               ) in
               Printf.printf "  Creating bit-select [%d] from signal (id=%d)\n" index_val base_id;
               let extract_op = Sv_ast.Extract { width = 1; lsb = index_val; msb = index_val } in
               Sv_opt_ir.add_node ir extract_op [base_id]
           | TUPLE6 (STRING "select_variable_dimension1", _lbracket, high_expr, _colon, low_expr, _rbracket) ->
               (* This is bit slice: signal[high:low] *)
               let base_id = expr_to_ir ir expr_cache symbol_table functions base_expr in
               let high_val = (match high_expr with
                 | TK_DecNumber n -> (try int_of_string n with _ -> 0)
                 | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
                 | _ -> 0) in
               let low_val = (match low_expr with
                 | TK_DecNumber n -> (try int_of_string n with _ -> 0)
                 | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
                 | _ -> 0) in
               let width = high_val - low_val + 1 in
               Printf.printf "  Creating bit-slice [%d:%d] (width=%d) from signal (id=%d)\n"
                 high_val low_val width base_id;
               let extract_op = Sv_ast.Extract { width; lsb = low_val; msb = high_val } in
               Sv_opt_ir.add_node ir extract_op [base_id]
           | _ ->
               (* True array indexing *)
               Printf.printf "  Note: Array indexing not yet fully supported, creating placeholder\n";
               let _base_id = expr_to_ir ir expr_cache symbol_table functions base_expr in
               let _index_id = expr_to_ir ir expr_cache symbol_table functions index_expr in
               Sv_opt_ir.get_new_id ir)

      | TUPLE3 (STRING "reference_or_call_base1", func_name, args) ->
          (* Function call: func(args) *)
          (* Extract function name *)
          let name = (match func_name with
            | SymbolIdentifier n -> n
            | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier n, _) -> n
            | _ -> "unknown_func") in

          (* Look up function in the functions list *)
          let func_opt = List.find_opt (fun (f : Sv_elaborate.function_info) ->
            f.func_name = name
          ) functions in

          (match func_opt with
           | Some func ->
               Printf.printf "  Inlining function '%s' (width=%d)...\n" name func.func_return_width;

               (* Extract actual arguments from the call *)
               let actual_args = extract_function_args args in

               (* Convert function body to expression *)
               (match convert_function_body_to_expr name func.func_body with
                | Some body_expr ->
                    (* Substitute parameters with actual arguments *)
                    let substituted_expr =
                      if List.length func.func_params = List.length actual_args then
                        List.fold_left2 (fun acc_expr param arg ->
                          substitute_param_in_token param.Sv_elaborate.param_name arg acc_expr
                        ) body_expr func.func_params actual_args
                      else begin
                        Printf.eprintf "Warning: Parameter count mismatch for function '%s' (expected %d, got %d)\n"
                          name (List.length func.func_params) (List.length actual_args);
                        body_expr
                      end
                    in

                    (* Now convert the substituted expression to IR *)
                    Printf.printf "  Successfully inlined function '%s'\n" name;
                    expr_to_ir ir expr_cache symbol_table functions substituted_expr

                | None ->
                    Printf.eprintf "Warning: Could not convert function '%s' body to expression\n" name;
                    Sv_opt_ir.get_or_create_constant ir 0 func.func_return_width)
           | None ->
               (* Not a function, might be a signal reference *)
               Printf.eprintf "Warning: Unknown identifier '%s' in expression\n" name;
               Sv_opt_ir.get_new_id ir)

      | TUPLE4 (STRING "expr_primary_parens1", _lparen, inner_expr, _rparen) ->
          (* Parenthesized expression: (expr) *)
          expr_to_ir ir expr_cache symbol_table functions inner_expr

      | TUPLE4 (STRING "select_variable_dimension2", _lbracket, index_expr, _rbracket) ->
          (* Array index selector: [expr] *)
          expr_to_ir ir expr_cache symbol_table functions index_expr

      | TUPLE6 (STRING "select_variable_dimension1", _lbracket, high_expr, _colon, low_expr, _rbracket) ->
          (* Bit slice range selector: [high:low] *)
          (* For now, just return the high expression (simplified) *)
          Printf.printf "  Note: Bit slice range [high:low] not fully supported\n";
          expr_to_ir ir expr_cache symbol_table functions high_expr

      | TLIST [single_item] ->
          (* Singleton list - unwrap and process *)
          expr_to_ir ir expr_cache symbol_table functions single_item

      | TUPLE5 (STRING "cond_expr1", cond, _qmark, true_expr, false_expr) ->
          (* Ternary operator: cond ? true_val : false_val *)
          let cond_id = expr_to_ir ir expr_cache symbol_table functions cond in
          let true_id = expr_to_ir ir expr_cache symbol_table functions true_expr in
          let false_id = expr_to_ir ir expr_cache symbol_table functions false_expr in
          (* Create a mux: output = cond ? true : false *)
          let width = max (get_value_width ir true_id) (get_value_width ir false_id) in
          Sv_opt_ir.add_node ir (Sv_ast.Mux { width }) [cond_id; true_id; false_id]

      | TUPLE6 (STRING "cond_expr2", cond, _query, true_expr, _colon, false_expr) ->
          (* Ternary operator: cond ? true_val : false_val *)
          (* Grammar: logor_expr QUERY expression COLON cond_expr *)
          let cond_id = expr_to_ir ir expr_cache symbol_table functions cond in
          let true_id = expr_to_ir ir expr_cache symbol_table functions true_expr in
          let false_id = expr_to_ir ir expr_cache symbol_table functions false_expr in
          (* Create a mux: output = cond ? true : false *)
          let width = max (get_value_width ir true_id) (get_value_width ir false_id) in
          Sv_opt_ir.add_node ir (Sv_ast.Mux { width }) [cond_id; true_id; false_id]

      | TUPLE3 (STRING "concatenation_expression1", _lbrace, expr_list) ->
          (* Concatenation: {a, b, c} *)
          (* For now, treat as the first element (simplified) *)
          Printf.printf "  Note: Concatenation not fully supported, using first element\n";
          (match expr_list with
           | TLIST (first :: _) -> expr_to_ir ir expr_cache symbol_table functions first
           | TLIST [] -> Sv_opt_ir.get_or_create_constant ir 0 1
           | single -> expr_to_ir ir expr_cache symbol_table functions single)

      | TUPLE4 (STRING "range_list_in_braces1", _lbrace, range_expr, _rbrace) ->
          (* Range in braces like {8{1'b0}} - replication *)
          Printf.printf "  Note: Replication not fully supported, creating constant\n";
          (* Extract the replicated value *)
          (match range_expr with
           | TUPLE3 (STRING "replication1", _count, value_expr) ->
               (* For now, just use the value expression *)
               expr_to_ir ir expr_cache symbol_table functions value_expr
           | _ -> Sv_opt_ir.get_or_create_constant ir 0 8)

      | TUPLE4 (STRING "select1", base, _lbracket, index) ->
          (* Bit selection: signal[index] *)
          let base_id = expr_to_ir ir expr_cache symbol_table functions base in
          (* Try to extract constant index value *)
          let index_val = (match index with
            | TK_DecNumber n -> (try int_of_string n with _ -> 0)
            | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
            | _ ->
                (* Dynamic index - for now use 0, but this needs proper support *)
                Printf.printf "  Warning: Dynamic bit index not fully supported, using index 0\n";
                0
          ) in
          Printf.printf "  Creating bit-select [%d] from signal (id=%d)\n" index_val base_id;
          let extract_op = Sv_ast.Extract { width = 1; lsb = index_val; msb = index_val } in
          Sv_opt_ir.add_node ir extract_op [base_id]

      | TUPLE4 (STRING "select2", base, _lbracket, range) ->
          (* Bit slice: signal[high:low] *)
          let base_id = expr_to_ir ir expr_cache symbol_table functions base in
          (* Extract high and low from range *)
          let (high_val, low_val) = (match range with
            | TUPLE6 (STRING "select_variable_dimension1", _lb, high_expr, _colon, low_expr, _rb) ->
                let high = (match high_expr with
                  | TK_DecNumber n -> (try int_of_string n with _ -> 0)
                  | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
                  | _ -> 0) in
                let low = (match low_expr with
                  | TK_DecNumber n -> (try int_of_string n with _ -> 0)
                  | TK_UnBasedNumber n -> (try int_of_string n with _ -> 0)
                  | _ -> 0) in
                (high, low)
            | _ ->
                Printf.printf "  Warning: Unexpected range format in bit slice\n";
                (0, 0)
          ) in
          let width = high_val - low_val + 1 in
          Printf.printf "  Creating bit-slice [%d:%d] (width=%d) from signal (id=%d)\n"
            high_val low_val width base_id;
          let extract_op = Sv_ast.Extract { width; lsb = low_val; msb = high_val } in
          Sv_opt_ir.add_node ir extract_op [base_id]

      | TK_DecNumber n ->
          (try
            let value = int_of_string n in
            (* For now, assume constants are 32-bit - TODO: infer proper width *)
            Sv_opt_ir.get_or_create_constant ir value 32
          with _ ->
            Printf.eprintf "Warning: Invalid number '%s'\n" n;
            Sv_opt_ir.get_new_id ir)

      | TK_UnBasedNumber n ->
          (try
            let value = int_of_string n in
            Sv_opt_ir.get_or_create_constant ir value 32
          with _ ->
            Printf.eprintf "Warning: Invalid unbased number '%s'\n" n;
            Sv_opt_ir.get_new_id ir)

      (* Binary numbers: 2'b00, 4'b1010, etc. *)
      | TUPLE3 (STRING "bin_based_number1", TK_BinBase width_base, TK_BinDigits digits) ->
          (try
            (* Parse width from "N'b" format *)
            let width_str = String.sub width_base 0 (String.index width_base '\'') in
            let width = int_of_string width_str in
            (* Parse binary digits to integer *)
            let value = int_of_string ("0b" ^ digits) in
            Sv_opt_ir.get_or_create_constant ir value width
          with _ ->
            Printf.eprintf "Warning: Invalid binary number %s%s\n" width_base digits;
            Sv_opt_ir.get_or_create_constant ir 0 32)

      (* Hex numbers: 4'hF, 8'hAB, etc. *)
      | TUPLE3 (STRING "hex_based_number1", TK_HexBase width_base, TK_HexDigits digits) ->
          (try
            let width_str = String.sub width_base 0 (String.index width_base '\'') in
            let width = int_of_string width_str in
            let value = int_of_string ("0x" ^ digits) in
            Sv_opt_ir.get_or_create_constant ir value width
          with _ ->
            Printf.eprintf "Warning: Invalid hex number %s%s\n" width_base digits;
            Sv_opt_ir.get_or_create_constant ir 0 32)

      (* Octal numbers: 3'o7, 6'o77, etc. *)
      | TUPLE3 (STRING "oct_based_number1", TK_OctBase width_base, TK_OctDigits digits) ->
          (try
            let width_str = String.sub width_base 0 (String.index width_base '\'') in
            let width = int_of_string width_str in
            let value = int_of_string ("0o" ^ digits) in
            Sv_opt_ir.get_or_create_constant ir value width
          with _ ->
            Printf.eprintf "Warning: Invalid octal number %s%s\n" width_base digits;
            Sv_opt_ir.get_or_create_constant ir 0 32)

      (* Decimal based numbers: 8'd255, etc. *)
      | TUPLE3 (STRING "dec_based_number1", TK_DecBase width_base, TK_DecDigits digits) ->
          (try
            let width_str = String.sub width_base 0 (String.index width_base '\'') in
            let width = int_of_string width_str in
            let value = int_of_string digits in
            Sv_opt_ir.get_or_create_constant ir value width
          with _ ->
            Printf.eprintf "Warning: Invalid decimal based number %s%s\n" width_base digits;
            Sv_opt_ir.get_or_create_constant ir 0 32)

      | TUPLE8 (STRING "case_statement1", _unique, _case_kw, LPAREN, sel_expr, RPAREN, case_items, Endcase) ->
          (* Convert case statement to Pmux *)
          Printf.printf "  Converting case statement to Pmux\n";
          let sel_id = expr_to_ir ir expr_cache symbol_table functions sel_expr in

          (* Extract case items: each has condition expression(s) and result value *)
          let rec extract_cases items =
            match items with
            | TUPLE4 (STRING "case_item1", expr_list, COLON, stmt) ->
                (* Single case item *)
                let case_values = (match expr_list with
                  | TLIST lst -> lst
                  | single -> [single]) in
                let result_val = (match stmt with
                  (* Pattern 1: statement_item6 with blocking_assignment1 *)
                  | TUPLE3 (STRING "statement_item6",
                            TUPLE4 (STRING "blocking_assignment1", _lhs, _eq, rhs), _) -> rhs
                  (* Pattern 2: statement_item6 with assignment_statement_no_expr1 *)
                  | TUPLE3 (STRING "statement_item6",
                            TUPLE4 (STRING "assignment_statement_no_expr1", _lhs, _eq, rhs), _) -> rhs
                  (* Pattern 3: Direct blocking_assignment1 *)
                  | TUPLE4 (STRING "blocking_assignment1", _lhs, _eq, rhs) -> rhs
                  | _ ->
                      Printf.eprintf "\n=== Warning: Unexpected case item statement structure ===\n";
                      Printf.eprintf "%s\n" (token_to_json_string ~max_depth:3 0 stmt);
                      Printf.eprintf "========================================================\n\n";
                      TK_DecNumber "0") in
                [(case_values, result_val)]
            | TUPLE3 (STRING "case_items1", rest, item) ->
                (* Multiple case items - recursive *)
                let rest_cases = extract_cases rest in
                let item_cases = extract_cases item in
                rest_cases @ item_cases
            | _ -> []
          in

          let cases = extract_cases case_items in

          if cases = [] then begin
            Printf.eprintf "Warning: No case items found\n";
            Sv_opt_ir.get_or_create_constant ir 0 4
          end else begin
            (* Create comparison nodes for each case *)
            let selector_ids = List.map (fun (case_vals, _result_val) ->
              (* For now, take first case value if multiple *)
              let case_val_expr = List.hd case_vals in
              let case_val_id = expr_to_ir ir expr_cache symbol_table functions case_val_expr in
              (* Create equality comparison: sel == case_value *)
              let sel_width = get_value_width ir sel_id in
              let cmp_op = Sv_ast.Compare { width = sel_width; cmp_op = `Eq; signed = false } in
              Sv_opt_ir.add_node ir cmp_op [sel_id; case_val_id]
            ) cases in

            (* Convert result values to IR *)
            let data_ids = List.map (fun (_case_vals, result_val) ->
              expr_to_ir ir expr_cache symbol_table functions result_val
            ) cases in

            (* Get output width from first data value *)
            let output_width = get_value_width ir (List.hd data_ids) in

            (* Create default value (0) *)
            let default_id = Sv_opt_ir.get_or_create_constant ir 0 output_width in

            (* Create Pmux node: inputs are [default; selectors...; data...] *)
            let num_cases = List.length cases in
            let pmux_inputs = default_id :: selector_ids @ data_ids in
            let pmux_op = Sv_ast.Pmux { width = output_width; num_cases } in
            Printf.printf "  Created Pmux with %d cases, width=%d\n" num_cases output_width;
            Sv_opt_ir.add_node ir pmux_op pmux_inputs
          end

      (* Logical equality: == operator *)
      | TUPLE4 (STRING "logeq_expr2", left, EQ_EQ, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Compare { width; cmp_op = `Eq; signed = false }) [left_id; right_id]

      (* Logical inequality: != operator *)
      | TUPLE4 (STRING "logeq_expr3", left, PLING_EQ, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          let width = max (get_value_width ir left_id) (get_value_width ir right_id) in
          Sv_opt_ir.add_node ir (Compare { width; cmp_op = `Ne; signed = false }) [left_id; right_id]

      (* Logical AND: && operator *)
      | TUPLE4 (STRING "logand_expr2", left, AMPERSAND_AMPERSAND, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          (* Logical AND returns 1-bit result *)
          Sv_opt_ir.add_node ir (And { width = 1 }) [left_id; right_id]

      (* Logical OR: || operator *)
      | TUPLE4 (STRING "logor_expr2", left, VBAR_VBAR, right) ->
          let left_id = expr_to_ir ir expr_cache symbol_table functions left in
          let right_id = expr_to_ir ir expr_cache symbol_table functions right in
          (* Logical OR returns 1-bit result *)
          Sv_opt_ir.add_node ir (Or { width = 1 }) [left_id; right_id]

      (* Sequence repetition - unwrap the expression inside *)
      | TUPLE3 (STRING "sequence_repetition_expr1", expr, EMPTY_TOKEN) ->
          (* This wraps expression_or_dist1, just unwrap it *)
          expr_to_ir ir expr_cache symbol_table functions expr

      (* Expression or dist - unwrap to get the actual expression *)
      | TUPLE3 (STRING "expression_or_dist1", expr, EMPTY_TOKEN) ->
          expr_to_ir ir expr_cache symbol_table functions expr

      | _ ->
          Printf.eprintf "\n=== Warning: Unhandled expression type ===\n";
          (* Write full token to file for inspection *)
          let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 "unhandled_tokens.txt" in
          Printf.fprintf oc "\n=== Unhandled Expression ===\n";
          Printf.fprintf oc "%s\n" (token_to_json_string ~max_depth:5 0 expr);
          Printf.fprintf oc "============================\n\n";
          close_out oc;
          Printf.eprintf "Token structure dumped to unhandled_tokens.txt\n";
          Printf.eprintf "==========================================\n\n";
          Sv_opt_ir.get_new_id ir
    in
    Hashtbl.add expr_cache expr result;
    result

(* Convert Verible AST to IR with elaboration *)
let verible_to_ir verible_ast module_name =
  (* Step 1: Elaborate - resolve parameters and widths *)
  let elab_ctx = Sv_elaborate.elaborate verible_ast in

  (* Debug: print elaboration context *)
  if !Sys.interactive then begin
    Printf.printf "=== Elaboration Context ===\n";
    Sv_elaborate.print_context elab_ctx
  end;

  (* Step 2: Create IR with elaborated information *)
  let ir = Sv_opt_ir.create_ir module_name in

  (* Step 3: Get data for this specific module *)
  let symbol_table = match Sv_elaborate.get_module_symbol_table elab_ctx module_name with
    | Some tbl -> tbl
    | None ->
        Printf.eprintf "Warning: No symbol table found for module '%s'\n" module_name;
        Hashtbl.create 1  (* Empty fallback table *)
  in

  let module_data = match Sv_elaborate.get_module_data elab_ctx module_name with
    | Some data -> data
    | None ->
        Printf.eprintf "Warning: No module_data found for module '%s'\n" module_name;
        { mod_ports = []; mod_assigns = []; mod_always_blocks = []; mod_functions = [] }
  in

  Printf.printf "\n=== Converting to IR ===\n\n";

  (* Add inputs from this module only *)
  List.iter (fun (port : Sv_elaborate.port_info) ->
    match port.port_direction with
    | "input" ->
        ignore (Sv_opt_ir.add_input ir port.port_name port.port_width);
        Printf.printf "Added input: %s[%d]\n" port.port_name port.port_width
    | _ -> ()
  ) (List.rev module_data.mod_ports);  (* Reverse to get original order *)

  (* Add outputs from this module only *)
  List.iter (fun (port : Sv_elaborate.port_info) ->
    match port.port_direction with
    | "output" ->
        ignore (Sv_opt_ir.add_output ir port.port_name port.port_width);
        Printf.printf "Added output: %s[%d]\n" port.port_name port.port_width
    | _ -> ()
  ) (List.rev module_data.mod_ports);

  (* Add internal signals (wires/regs/logic) from symbol table *)
  Hashtbl.iter (fun name signal_info ->
    match signal_info.Sv_elaborate.signal_kind with
    | Sv_elaborate.Reg | Sv_elaborate.Wire | Sv_elaborate.Logic ->
        ignore (Sv_opt_ir.add_wire ir name signal_info.Sv_elaborate.signal_width);
        Printf.printf "Added wire: %s[%d]\n" name signal_info.Sv_elaborate.signal_width
    | _ -> ()
  ) symbol_table;

  (* Step 4: Convert assign statements from this module only *)
  let expr_cache = Hashtbl.create 50 in
  List.iter (fun (assign : Sv_elaborate.assign_info) ->
    Printf.printf "Converting assign: %s = <expr>\n" assign.assign_lhs;
    (* Debug: Show RHS expression structure if unhandled *)
    let value_id =
      try
        expr_to_ir ir expr_cache symbol_table module_data.mod_functions assign.assign_rhs
      with
      | e ->
          Printf.eprintf "\nException while converting RHS of %s:\n" assign.assign_lhs;
          Printf.eprintf "%s\n" (token_to_json_string ~max_depth:3 0 assign.assign_rhs);
          raise e
    in

    (* Try to connect to wire first, then output *)
    (try
      let wire_val = Hashtbl.find ir.ir_wires assign.assign_lhs in
      (match wire_val with
       | Sv_ast.Wire { width; name; _ } ->
           (* Replace the wire's ID with the computed value ID *)
           Hashtbl.replace ir.ir_wires assign.assign_lhs
             (Sv_ast.Wire { id = value_id; name; width });
           Printf.printf "  Connected to wire %s (value_id=%d)\n" assign.assign_lhs value_id
       | _ -> ())
    with Not_found ->
      (* Not a wire, try output *)
      (try
        let output_val = Hashtbl.find ir.ir_outputs assign.assign_lhs in
        (match output_val with
         | Sv_ast.Output { width; name; _ } ->
             (* Replace the output's ID with the computed value ID *)
             Hashtbl.replace ir.ir_outputs assign.assign_lhs
               (Sv_ast.Output { id = value_id; name; width });
             Printf.printf "  Connected to output %s (value_id=%d)\n" assign.assign_lhs value_id
         | _ ->
             Printf.eprintf "Warning: '%s' is not an output\n" assign.assign_lhs)
      with Not_found ->
        Printf.eprintf "Warning: Signal '%s' not found (not a wire or output)\n" assign.assign_lhs))
  ) (List.rev module_data.mod_assigns);

  (* Step 5: Convert always blocks to IR operations *)
  List.iter (fun (always_blk : Sv_elaborate.always_info) ->
    match always_blk.Sv_elaborate.always_type with
    | Sv_elaborate.AlwaysComb ->
        (* Treat always_comb like continuous assignments *)
        List.iter (fun (assign : Sv_elaborate.assign_info) ->
          Printf.printf "Converting always_comb: %s = <expr>\n" assign.Sv_elaborate.assign_lhs;
          let value_id = expr_to_ir ir expr_cache symbol_table module_data.mod_functions assign.Sv_elaborate.assign_rhs in
          (* Try to connect to wire first, then output *)
          (try
            let wire_val = Hashtbl.find ir.ir_wires assign.Sv_elaborate.assign_lhs in
            (match wire_val with
             | Sv_ast.Wire { width; name; _ } ->
                 Hashtbl.replace ir.ir_wires assign.Sv_elaborate.assign_lhs
                   (Sv_ast.Wire { id = value_id; name; width });
                 Printf.printf "  Connected to wire %s (value_id=%d)\n" assign.Sv_elaborate.assign_lhs value_id
             | _ -> ())
          with Not_found ->
            (* Not a wire, try output *)
            (try
              let output_val = Hashtbl.find ir.ir_outputs assign.Sv_elaborate.assign_lhs in
              (match output_val with
               | Sv_ast.Output { width; name; _ } ->
                   Hashtbl.replace ir.ir_outputs assign.Sv_elaborate.assign_lhs
                     (Sv_ast.Output { id = value_id; name; width });
                   Printf.printf "  Connected to output %s (value_id=%d)\n" assign.Sv_elaborate.assign_lhs value_id
               | _ -> ())
            with Not_found ->
              Printf.eprintf "Warning: Signal '%s' not found (not a wire or output)\n" assign.Sv_elaborate.assign_lhs))
        ) always_blk.Sv_elaborate.always_stmts
    | Sv_elaborate.AlwaysFF { clock; edge; async_reset } ->
        (* Create register operations *)
        List.iter (fun (assign : Sv_elaborate.assign_info) ->
          (match async_reset with
           | Some reset_info ->
               Printf.printf "Converting always_ff @(%s %s or %s %s): %s <= <expr> (reset_value=<expr>)\n"
                 (match edge with `Posedge -> "posedge" | `Negedge -> "negedge")
                 clock
                 (match reset_info.reset_edge with `Posedge -> "posedge" | `Negedge -> "negedge")
                 reset_info.reset_signal
                 assign.Sv_elaborate.assign_lhs
           | None ->
               Printf.printf "Converting always_ff @(%s %s): %s <= <expr>\n"
                 (match edge with `Posedge -> "posedge" | `Negedge -> "negedge")
                 clock assign.Sv_elaborate.assign_lhs);

          let d_value_id = expr_to_ir ir expr_cache symbol_table module_data.mod_functions assign.Sv_elaborate.assign_rhs in

          (* Get clock input *)
          let clock_id = (try
            let clock_val = Hashtbl.find ir.ir_inputs clock in
            (match clock_val with
             | Sv_ast.Input { id; _ } -> id
             | _ -> 0)
          with Not_found ->
            (* Clock might be a wire *)
            (try
              let wire_val = Hashtbl.find ir.ir_wires clock in
              (match wire_val with
               | Sv_ast.Wire { id; _ } -> id
               | _ -> 0)
            with Not_found -> 0)) in

          (* Get signal width (check wire first, then output) *)
          let signal_width = (try
            let wire_val = Hashtbl.find ir.ir_wires assign.Sv_elaborate.assign_lhs in
            (match wire_val with
             | Sv_ast.Wire { width; _ } -> width
             | _ -> 1)
          with Not_found ->
            (try
              let output_val = Hashtbl.find ir.ir_outputs assign.Sv_elaborate.assign_lhs in
              (match output_val with
               | Sv_ast.Output { width; _ } -> width
               | _ -> 1)
            with Not_found -> 1)) in

          (* Process async reset if present *)
          let (reset_id_opt, reset_value) = (match async_reset with
            | Some reset_info ->
                (* Get reset signal ID *)
                let reset_id = (try
                  let reset_val = Hashtbl.find ir.ir_inputs reset_info.reset_signal in
                  (match reset_val with
                   | Sv_ast.Input { id; _ } -> id
                   | _ -> 0)
                with Not_found ->
                  (try
                    let wire_val = Hashtbl.find ir.ir_wires reset_info.reset_signal in
                    (match wire_val with
                     | Sv_ast.Wire { id; _ } -> id
                     | _ -> 0)
                  with Not_found -> 0)) in

                (* Convert reset value expression to IR *)
                let reset_val_id = expr_to_ir ir expr_cache symbol_table module_data.mod_functions reset_info.reset_value in

                (* For now, try to extract constant value - TODO: handle expressions *)
                (* Search for the constant value in the constants table *)
                let reset_const = (
                  let found = ref None in
                  Hashtbl.iter (fun value value_id ->
                    if value_id = reset_val_id then found := Some value
                  ) ir.ir_constants;
                  match !found with
                  | Some v -> v
                  | None -> 0  (* Default to 0 if we can't extract the value *)
                ) in

                (Some reset_id, reset_const)
            | None -> (None, 0)) in

          (* Create register node *)
          let reg_node_id = Sv_opt_ir.add_node ir
            (Sv_ast.Register { width = signal_width; clock = clock_id; reset = reset_id_opt; enable = None; reset_value })
            [d_value_id] in

          (* Connect register output to wire or output *)
          (try
            let wire_val = Hashtbl.find ir.ir_wires assign.Sv_elaborate.assign_lhs in
            (match wire_val with
             | Sv_ast.Wire { width; name; _ } ->
                 Hashtbl.replace ir.ir_wires assign.Sv_elaborate.assign_lhs
                   (Sv_ast.Wire { id = reg_node_id; name; width });
                 Printf.printf "  Connected register to wire %s (reg_id=%d)\n" assign.Sv_elaborate.assign_lhs reg_node_id
             | _ -> ())
          with Not_found ->
            (* Not a wire, try output *)
            (try
              let output_val = Hashtbl.find ir.ir_outputs assign.Sv_elaborate.assign_lhs in
              (match output_val with
               | Sv_ast.Output { width; name; _ } ->
                   Hashtbl.replace ir.ir_outputs assign.Sv_elaborate.assign_lhs
                     (Sv_ast.Output { id = reg_node_id; name; width });
                   Printf.printf "  Connected register to output %s (reg_id=%d)\n" assign.Sv_elaborate.assign_lhs reg_node_id
               | _ -> ())
            with Not_found ->
              Printf.eprintf "Warning: Signal '%s' not found (not a wire or output)\n" assign.Sv_elaborate.assign_lhs))
        ) always_blk.Sv_elaborate.always_stmts
  ) (List.rev module_data.Sv_elaborate.mod_always_blocks);

  Printf.printf "\n✓ IR conversion complete\n";
  Printf.printf "  Inputs: %d, Outputs: %d, Nodes: %d\n"
    (Hashtbl.length ir.ir_inputs)
    (Hashtbl.length ir.ir_outputs)
    (Hashtbl.length ir.ir_nodes);

  ir

(* Main entry point: parse Verilog file and convert to IR *)
let file_to_ir filename =
  match parse_verible_file filename with
  | None -> None
  | Some ast ->
      (* Elaborate to get the actual module name *)
      let elab_ctx = Sv_elaborate.elaborate ast in
      let module_name = match elab_ctx.Sv_elaborate.module_name with
        | Some name -> name
        | None ->
            (* Fallback to filename if no module found *)
            Filename.chop_extension (Filename.basename filename)
      in
      Some (verible_to_ir ast module_name)
