(* Verilator JSON to Behavioral IR Converter
 *
 * Converts Verilator's JSON AST (sv_ast) to language-neutral behavioral IR.
 * This provides a path from Verilator → Behavioral IR → Optimization → Z3.
 *)

open Behavioral_ir
open Sv_ast

let debug = ref false

(* Get width from dtype_ref *)
let rec get_width_from_dtype = function
  | Some (BasicType { range = Some r; _ }) ->
      (try
        let parts = String.split_on_char ':' r in
        match parts with
        | [msb; lsb] ->
            abs (int_of_string (String.trim msb) - int_of_string (String.trim lsb)) + 1
        | _ -> 32
      with _ -> 32)
  | Some (ArrayType { range; _ }) | Some (PackArrayType { range; _ }) ->
      (try
        let parts = String.split_on_char ':' range in
        match parts with
        | [msb; lsb] ->
            abs (int_of_string (String.trim msb) - int_of_string (String.trim lsb)) + 1
        | _ -> 32
      with _ -> 32)
  | Some (RefType { refdtype_ref; _ }) -> get_width_from_dtype refdtype_ref
  | _ -> 32

(* Check if dtype is signed *)
let is_signed_dtype = function
  | Some (BasicType { keyword = "int" | "integer" | "shortint" | "longint"; _ }) -> true
  | _ -> false

(* Parse constant value from Verilator format *)
let parse_const_value name =
  try
    if String.contains name '\'' then
      let parts = String.split_on_char '\'' name in
      match parts with
      | width_str :: rest ->
          let value_str = String.concat "'" rest in
          if String.length value_str >= 2 then
            match value_str.[0], value_str.[1] with
            | 's', 'h' ->
                int_of_string ("0x" ^ String.sub value_str 2 (String.length value_str - 2))
            | 's', 'd' ->
                int_of_string (String.sub value_str 2 (String.length value_str - 2))
            | 'h', _ ->
                int_of_string ("0x" ^ String.sub value_str 1 (String.length value_str - 1))
            | 'd', _ ->
                int_of_string (String.sub value_str 1 (String.length value_str - 1))
            | 'b', _ ->
                int_of_string ("0b" ^ String.sub value_str 1 (String.length value_str - 1))
            | _ -> int_of_string value_str
          else int_of_string value_str
      | _ -> int_of_string name
    else
      int_of_string name
  with _ -> 0

(* Convert Verilator expression to behavioral IR expression *)
let rec expr_to_bexpr = function
  | VarRef { name; _ } | VarRef' { name } ->
      BVar name

  | Const { name; dtype_ref } ->
      let value = parse_const_value name in
      let width = get_width_from_dtype dtype_ref in
      BConst { value; width }

  | Const' { name } ->
      let value = parse_const_value name in
      BConst { value; width = 32 }

  | BinaryOp { op; lhs; rhs; dtype_ref } ->
      let lhs_expr = expr_to_bexpr lhs in
      let rhs_expr = expr_to_bexpr rhs in
      let width = get_width_from_dtype dtype_ref in
      let signed = is_signed_dtype dtype_ref in
      let signedness = if signed then Signed else Unsigned in

      (match op with
       | "ADD" -> BBinOp { op = BAdd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SUB" -> BBinOp { op = BSub; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MUL" -> BBinOp { op = BMul; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "DIV" -> BBinOp { op = BDiv; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MODDIV" -> BBinOp { op = BMod; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "AND" -> BBinOp { op = BAnd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "OR" -> BBinOp { op = BOr; lhs = lhs_expr; rhs = rhs_expr;
                          result_type = BInt { width; signed = signedness } }
       | "XOR" -> BBinOp { op = BXor; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SHIFTL" -> BBinOp { op = BShl; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTR" -> BBinOp { op = BShr; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTRS" -> BBinOp { op = BAshr; lhs = lhs_expr; rhs = rhs_expr;
                               result_type = BInt { width; signed = Signed } }
       | "EQ" -> BBinOp { op = BEq; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "NEQ" -> BBinOp { op = BNe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LT" -> BBinOp { op = BLt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LTE" -> BBinOp { op = BLe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GT" -> BBinOp { op = BGt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GTE" -> BBinOp { op = BGe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown binary op %s\n" op;
           BConst { value = 0; width = 1 })

  | BinaryOp' { op; lhs; rhs } ->
      let lhs_expr = expr_to_bexpr lhs in
      let rhs_expr = expr_to_bexpr rhs in
      let width = 32 in
      let signedness = Unsigned in

      (match op with
       | "ADD" -> BBinOp { op = BAdd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SUB" -> BBinOp { op = BSub; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MUL" -> BBinOp { op = BMul; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "DIV" -> BBinOp { op = BDiv; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "MODDIV" -> BBinOp { op = BMod; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "AND" -> BBinOp { op = BAnd; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "OR" -> BBinOp { op = BOr; lhs = lhs_expr; rhs = rhs_expr;
                          result_type = BInt { width; signed = signedness } }
       | "XOR" -> BBinOp { op = BXor; lhs = lhs_expr; rhs = rhs_expr;
                           result_type = BInt { width; signed = signedness } }
       | "SHIFTL" -> BBinOp { op = BShl; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTR" -> BBinOp { op = BShr; lhs = lhs_expr; rhs = rhs_expr;
                              result_type = BInt { width; signed = signedness } }
       | "SHIFTRS" -> BBinOp { op = BAshr; lhs = lhs_expr; rhs = rhs_expr;
                               result_type = BInt { width; signed = Signed } }
       | "EQ" -> BBinOp { op = BEq; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "NEQ" -> BBinOp { op = BNe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LT" -> BBinOp { op = BLt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "LTE" -> BBinOp { op = BLe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GT" -> BBinOp { op = BGt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | "GTE" -> BBinOp { op = BGe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown binary op %s\n" op;
           BConst { value = 0; width = 1 })

  | UnaryOp { op; operand; dtype_ref } ->
      let operand_expr = expr_to_bexpr operand in
      let width = get_width_from_dtype dtype_ref in
      let signed = is_signed_dtype dtype_ref in
      let signedness = if signed then Signed else Unsigned in

      (match op with
       | "NOT" -> BUnOp { op = BNot; operand = operand_expr;
                          result_type = BInt { width; signed = signedness } }
       | "NEGATE" -> BUnOp { op = BNeg; operand = operand_expr;
                             result_type = BInt { width; signed = signedness } }
       | "REDAND" -> BUnOp { op = BRedAnd; operand = operand_expr; result_type = BBool }
       | "REDOR" -> BUnOp { op = BRedOr; operand = operand_expr; result_type = BBool }
       | "REDXOR" -> BUnOp { op = BRedXor; operand = operand_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown unary op %s\n" op;
           operand_expr)

  | UnaryOp' { op; operand; _ } ->
      let operand_expr = expr_to_bexpr operand in
      let width = 32 in
      let signedness = Unsigned in

      (match op with
       | "NOT" -> BUnOp { op = BNot; operand = operand_expr;
                          result_type = BInt { width; signed = signedness } }
       | "NEGATE" -> BUnOp { op = BNeg; operand = operand_expr;
                             result_type = BInt { width; signed = signedness } }
       | "REDAND" -> BUnOp { op = BRedAnd; operand = operand_expr; result_type = BBool }
       | "REDOR" -> BUnOp { op = BRedOr; operand = operand_expr; result_type = BBool }
       | "REDXOR" -> BUnOp { op = BRedXor; operand = operand_expr; result_type = BBool }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unknown unary op %s\n" op;
           operand_expr)

  | Cond { condition; then_val; else_val } ->
      let cond_expr = expr_to_bexpr condition in
      let then_expr = expr_to_bexpr then_val in
      let else_expr = expr_to_bexpr else_val in
      BCond { condition = cond_expr; then_val = then_expr; else_val = else_expr }

  | Sel { expr; lsb; width; _ } ->
      let signal_expr = expr_to_bexpr expr in
      let lsb_val = match lsb with
        | Some l -> (match expr_to_bexpr l with BConst { value; _ } -> value | _ -> 0)
        | None -> 0
      in
      let width_val = match width with
        | Some w -> (match expr_to_bexpr w with BConst { value; _ } -> value | _ -> 1)
        | None -> 1
      in
      BSlice { signal = signal_expr; msb = lsb_val + width_val - 1; lsb = lsb_val }

  | Concat { parts } ->
      let exprs = List.map expr_to_bexpr parts in
      BConcat exprs

  | Replicate { count; src; _ } | Replicate' { count; src; _ } ->
      let count_val = match expr_to_bexpr count with
        | BConst { value; _ } -> value
        | _ -> 1
      in
      let value_expr = expr_to_bexpr src in
      BReplicate { count = count_val; value = value_expr }

  | _ ->
      if !debug then Printf.eprintf "Warning: Unsupported expression type\n";
      BConst { value = 0; width = 1 }

(* Convert Verilator statement to behavioral IR statement *)
let rec stmt_to_bstmt = function
  | Assign { lhs; rhs; _ } ->
      (match lhs with
       | VarRef { name; _ } | VarRef' { name; _ } ->
           let rhs_expr = expr_to_bexpr rhs in
           BAssign { lhs = name; rhs = rhs_expr }
       | _ ->
           if !debug then Printf.eprintf "Warning: Unsupported LHS in assignment\n";
           BBlock [])

  | If { condition; then_stmt; else_stmt } ->
      let cond_expr = expr_to_bexpr condition in
      let then_stmts = [stmt_to_bstmt then_stmt] in
      let else_stmts = match else_stmt with
        | Some s -> [stmt_to_bstmt s]
        | None -> []
      in
      BIf { condition = cond_expr; then_stmts; else_stmts }

  | Case { expr; items } ->
      let sel_expr = expr_to_bexpr expr in
      let cases = List.map (fun item ->
        (* Extract first condition as value, handle case items *)
        match item.conditions with
        | cond :: _ ->
            let value = expr_to_bexpr cond in
            let stmts = List.map stmt_to_bstmt item.statements in
            (value, stmts)
        | [] ->
            (BConst { value = 0; width = 1 }, [])
      ) items in
      BCase { selector = sel_expr; cases; default = [] }

  | Begin { stmts; _ } ->
      let bstmts = List.map stmt_to_bstmt stmts in
      BBlock bstmts

  | _ ->
      if !debug then Printf.eprintf "Warning: Unsupported statement type\n";
      BBlock []

(* Check if sensitivity list contains edge-triggered signals *)
let rec is_edge_triggered = function
  | [] -> false
  | SenItem { edge_str; _ } :: _ when edge_str <> "" -> true
  | SenTree items :: rest -> is_edge_triggered items || is_edge_triggered rest
  | _ :: rest -> is_edge_triggered rest

(* Extract clock signal name from sensitivity list *)
let rec get_clock_signal = function
  | [] -> None
  | SenItem { edge_str; signal } :: rest ->
      if edge_str <> "" then
        (match signal with
         | VarRef { name; _ } | VarRef' { name; _ } -> Some name
         | _ -> get_clock_signal rest)
      else get_clock_signal rest
  | SenTree items :: rest ->
      (match get_clock_signal items with
       | Some c -> Some c
       | None -> get_clock_signal rest)
  | _ :: rest -> get_clock_signal rest

(* Check if edge is posedge *)
let rec is_posedge = function
  | [] -> false
  | SenItem { edge_str; _ } :: _ ->
      let e = String.uppercase_ascii edge_str in
      e = "POS" || e = "POSEDGE"
  | SenTree items :: rest -> is_posedge items || is_posedge rest
  | _ :: rest -> is_posedge rest

(* Convert Verilator always block to behavioral process *)
let always_to_bprocess = function
  | Always { senses; stmts; _ } ->
      let is_edge = is_edge_triggered senses in
      if is_edge then
        (* Sequential logic *)
        let clock = match get_clock_signal senses with
          | Some c -> c
          | None -> "clk"
        in
        let edge = if is_posedge senses then `Pos else `Neg in
        let body = List.map stmt_to_bstmt stmts in
        BSequential {
          name = "always_ff";
          clock;
          clock_edge = edge;
          reset = None;
          reset_edge = None;
          reset_async = false;
          body;
        }
      else
        (* Combinational logic *)
        let sensitivity = [BAny] in  (* Simplified *)
        let body = List.map stmt_to_bstmt stmts in
        BCombinational { name = "always_comb"; sensitivity; body }
  | _ -> BCombinational { name = "always"; sensitivity = [BAny]; body = [] }

(* Extract signals from module *)
let extract_signals stmts =
  List.filter_map (function
    | Var { name; dtype_ref; direction; _ } ->
        let width = get_width_from_dtype dtype_ref in
        let signed = is_signed_dtype dtype_ref in
        let dir = match String.uppercase_ascii direction with
          | "INPUT" -> `Input
          | "OUTPUT" -> `Output
          | _ -> `Internal
        in
        Some {
          name;
          stype = BInt { width; signed = if signed then Signed else Unsigned };
          direction = dir;
          initial_value = None;
        }
    | Var' { name; direction; _ } ->
        (* No dtype_ref, use default width *)
        let dir = match String.uppercase_ascii direction with
          | "INPUT" -> `Input
          | "OUTPUT" -> `Output
          | _ -> `Internal
        in
        Some {
          name;
          stype = BInt { width = 32; signed = Unsigned };
          direction = dir;
          initial_value = None;
        }
    | _ -> None
  ) stmts

(* Convert Verilator module to behavioral IR module *)
let module_to_bmodule = function
  | Module { name; stmts } ->
      let signals = extract_signals stmts in

      (* Extract always blocks and convert to processes *)
      let processes = List.filter_map (function
        | Always _ as a -> Some (always_to_bprocess a)
        | _ -> None
      ) stmts in

      {
        name;
        params = [];
        signals;
        processes;
        instances = [];
      }
  | _ ->
      { name = "unknown"; params = []; signals = []; processes = []; instances = [] }

(* Convert Verilator AST to behavioral program *)
let convert_ast ast =
  match ast with
  | Netlist modules ->
      let bmodules = List.filter_map (function
        | Module _ as m -> Some (module_to_bmodule m)
        | _ -> None
      ) modules in
      { modules = bmodules }
  | Module _ as m ->
      { modules = [module_to_bmodule m] }
  | _ ->
      { modules = [] }

(* Main entry point: Convert Verilator JSON file to behavioral IR *)
let convert_verilator_json_to_behavioral json_file =
  try
    if !debug then Printf.printf "Reading Verilator JSON: %s\n" json_file;

    let json = Yojson.Safe.from_file json_file in
    let ast = Sv_parse.parse json in

    if !debug then Printf.printf "Successfully parsed Verilator JSON\n";
    let bprog = convert_ast ast in
    Some bprog
  with
  | Sys_error msg ->
      Printf.eprintf "Error reading file: %s\n" msg;
      None
  | Yojson.Json_error msg ->
      Printf.eprintf "JSON parse error: %s\n" msg;
      None
  | e ->
      Printf.eprintf "Unexpected error: %s\n%s\n"
        (Printexc.to_string e)
        (Printexc.get_backtrace ());
      None
