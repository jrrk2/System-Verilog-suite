(* SystemVerilog to Behavioral IR Converter
 *
 * Converts elaborated SystemVerilog AST to language-neutral behavioral IR.
 * This eliminates SV-isms and provides a clean intermediate representation.
 *)

open Behavioral_ir
open Source_text_verible
open Sv_elaborate

(* Convert elaborated token tree to behavioral expression
   Uses the Verible token tree format (TUPLE-based) *)
let rec token_to_bexpr = function
  | SymbolIdentifier name -> BVar name
  | TK_DecNumber n ->
      (try
        let value = int_of_string n in
        BConst { value; width = 32 }
      with _ -> BConst { value = 0; width = 1 })
  | TK_UnBasedNumber n ->
      (try
        let value = int_of_string n in
        BConst { value; width = 32 }
      with _ -> BConst { value = 0; width = 1 })

  (* Binary operations *)
  | TUPLE4 (STRING "add_expr2", left, PLUS, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BAdd; lhs; rhs; result_type = BInt { width = 32; signed = Unsigned } }

  | TUPLE4 (STRING "add_expr3", left, HYPHEN, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BSub; lhs; rhs; result_type = BInt { width = 32; signed = Unsigned } }

  | TUPLE4 (STRING "mul_expr2", left, STAR, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BMul; lhs; rhs; result_type = BInt { width = 32; signed = Unsigned } }

  | TUPLE4 (STRING "binary_eq_expr1", left, _, right)
  | TUPLE4 (STRING "logeq_expr2", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BEq; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "logeq_expr3", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BNe; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "comp_expr2", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BLt; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "comp_expr3", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BGt; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "comp_expr4", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BLe; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "comp_expr5", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BGe; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "and_expr2", left, _, right)
  | TUPLE4 (STRING "bitand_expr2", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BAnd; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "or_expr2", left, _, right)
  | TUPLE4 (STRING "bitor_expr2", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BOr; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "xor_expr2", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BXor; lhs; rhs; result_type = BBool }

  | TUPLE4 (STRING "shift_expr2", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BShl; lhs; rhs; result_type = BInt { width = 32; signed = Unsigned } }

  | TUPLE4 (STRING "shift_expr3", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BShr; lhs; rhs; result_type = BInt { width = 32; signed = Unsigned } }

  | TUPLE4 (STRING "shift_expr4", left, _, right) ->
      let lhs = token_to_bexpr left in
      let rhs = token_to_bexpr right in
      BBinOp { op = BAshr; lhs; rhs; result_type = BInt { width = 32; signed = Signed } }

  (* Unary operations *)
  | TUPLE3 (STRING "unary_prefix_expr2", _, operand) ->
      let op_expr = token_to_bexpr operand in
      BUnOp { op = BNot; operand = op_expr; result_type = BBool }

  | TUPLE3 (STRING "unary_prefix_expr3", _, operand) ->
      let op_expr = token_to_bexpr operand in
      BUnOp { op = BNot; operand = op_expr; result_type = BBool }

  | TUPLE3 (STRING "unary_prefix_expr5", _, operand) ->
      let op_expr = token_to_bexpr operand in
      BUnOp { op = BNeg; operand = op_expr; result_type = BInt { width = 32; signed = Signed } }

  (* Ternary conditional *)
  | TUPLE5 (STRING "cond_expr1", cond, _, true_val, false_val)
  | TUPLE6 (STRING "cond_expr2", cond, _, true_val, _, false_val) ->
      let cond_expr = token_to_bexpr cond in
      let then_expr = token_to_bexpr true_val in
      let else_expr = token_to_bexpr false_val in
      BCond { condition = cond_expr; then_val = then_expr; else_val = else_expr }

  (* Parenthesized expressions *)
  | TUPLE4 (STRING "expr_primary_parens1", _, inner, _)
  | TUPLE4 (STRING "expression_in_parens1", _, inner, _) ->
      token_to_bexpr inner

  (* Identifiers *)
  | TUPLE3 (STRING "unqualified_id1", SymbolIdentifier name, _) -> BVar name

  (* Binary based numbers *)
  | TUPLE3 (STRING "bin_based_number1", TK_BinBase width_base, TK_BinDigits digits) ->
      (try
        let width_str = String.sub width_base 0 (String.index width_base '\'') in
        let width = int_of_string width_str in
        let value = int_of_string ("0b" ^ digits) in
        BConst { value; width }
      with _ -> BConst { value = 0; width = 1 })

  | TUPLE3 (STRING "hex_based_number1", TK_HexBase width_base, TK_HexDigits digits) ->
      (try
        let width_str = String.sub width_base 0 (String.index width_base '\'') in
        let width = int_of_string width_str in
        let value = int_of_string ("0x" ^ digits) in
        BConst { value; width }
      with _ -> BConst { value = 0; width = 1 })

  | TUPLE3 (STRING "dec_based_number1", TK_DecBase width_base, TK_DecDigits digits) ->
      (try
        let width_str = String.sub width_base 0 (String.index width_base '\'') in
        let width = int_of_string width_str in
        let value = int_of_string digits in
        BConst { value; width }
      with _ -> BConst { value = 0; width = 1 })

  (* Unwrap wrappers *)
  | TUPLE3 (STRING "expression_or_dist1", expr, _) -> token_to_bexpr expr
  | TUPLE3 (STRING "sequence_repetition_expr1", expr, _) -> token_to_bexpr expr
  | TLIST [single] -> token_to_bexpr single

  | _ -> BConst { value = 0; width = 1 }

(* Convert assign_info to behavioral IR statement *)
let convert_assign (assign : assign_info) =
  let rhs_expr = token_to_bexpr assign.assign_rhs in
  let stmt = BAssign { lhs = assign.assign_lhs; rhs = rhs_expr } in

  (* If there's a condition, wrap in if statement *)
  match assign.assign_condition with
  | None -> stmt
  | Some cond_token ->
      let cond_expr = token_to_bexpr cond_token in
      BIf {
        condition = cond_expr;
        then_stmts = [stmt];
        else_stmts = [];
      }

(* Convert always block to behavioral IR process *)
let always_to_bprocess (always_blk : always_info) =
  let body_stmts = List.map convert_assign always_blk.always_stmts in

  match always_blk.always_type with
  | AlwaysComb ->
      BCombinational {
        name = "always_comb";
        sensitivity = [BAny];
        body = body_stmts;
      }

  | AlwaysFF { clock; edge; async_reset } ->
      let clock_edge = match edge with
        | `Posedge -> `Pos
        | `Negedge -> `Neg
      in

      let (reset_name, reset_edge) = match async_reset with
        | Some rst_info ->
            let edge = match rst_info.reset_edge with
              | `Posedge -> `Pos
              | `Negedge -> `Neg
            in
            (Some rst_info.reset_signal, Some edge)
        | None -> (None, None)
      in

      BSequential {
        name = "always_ff";
        clock;
        clock_edge;
        reset = reset_name;
        reset_edge;
        reset_async = (async_reset <> None);
        body = body_stmts;
      }

(* Extract internal signals from symbol table *)
let extract_internal_signals symbol_table =
  let internals = ref [] in
  Hashtbl.iter (fun name (sig_info : signal_info) ->
    match sig_info.signal_kind with
    | Reg | Wire | Logic ->
        let signal = {
          name;
          stype = if sig_info.signal_width = 1 then BBool
                  else BInt { width = sig_info.signal_width; signed = Unsigned };
          direction = `Internal;
          initial_value = None;
        } in
        internals := signal :: !internals
    | _ -> ()
  ) symbol_table;
  List.rev !internals

(* Convert module data to behavioral IR module *)
let module_data_to_bmodule module_name (mod_data : module_data) symbol_table =
  (* Convert ports to signals *)
  let port_signals = List.map (fun (port : port_info) ->
    let direction = match port.port_direction with
      | "input" -> `Input
      | "output" -> `Output
      | _ -> `Internal
    in
    {
      name = port.port_name;
      stype = if port.port_width = 1 then BBool
              else BInt { width = port.port_width; signed = Unsigned };
      direction;
      initial_value = None;
    }
  ) mod_data.mod_ports in

  (* Extract internal signals from symbol table *)
  let internal_signals = extract_internal_signals symbol_table in

  (* Convert always blocks to processes *)
  let processes = List.map always_to_bprocess mod_data.mod_always_blocks in

  (* If there are continuous assignments, create a combinational process *)
  let assign_process = if List.length mod_data.mod_assigns > 0 then
    let assign_stmts = List.map convert_assign mod_data.mod_assigns in
    [BCombinational {
      name = "continuous_assigns";
      sensitivity = [BAny];
      body = assign_stmts;
    }]
  else
    []
  in

  {
    name = module_name;
    params = [];
    signals = port_signals @ internal_signals;
    processes = processes @ assign_process;
    instances = [];
  }

(* Main conversion function *)
let convert_sv_to_behavioral module_name (mod_data : module_data) symbol_table =
  let bmodule = module_data_to_bmodule module_name mod_data symbol_table in
  { modules = [bmodule]; library_cells = [] }

(* Helper: Convert elaborated SV file to behavioral IR *)
let convert_elaborated_sv_to_behavioral filename =
  (* Parse and elaborate the file *)
  let verible_ast = Sv_verible_to_ir.parse_verible_file filename in
  match verible_ast with
  | None ->
      Printf.eprintf "Failed to parse file: %s\n" filename;
      None
  | Some ast ->
      (* Elaborate *)
      let elab_ctx = Sv_elaborate.elaborate ast in

      (* Get module name *)
      let module_name = match elab_ctx.module_name with
        | Some name -> name
        | None -> Filename.chop_extension (Filename.basename filename)
      in

      (* Get module data and symbol table *)
      let mod_data_opt = Sv_elaborate.get_module_data elab_ctx module_name in
      let symbol_table_opt = Sv_elaborate.get_module_symbol_table elab_ctx module_name in

      match mod_data_opt, symbol_table_opt with
      | Some mod_data, Some symbol_table ->
          let bprog = convert_sv_to_behavioral module_name mod_data symbol_table in
          Some bprog
      | _ ->
          Printf.eprintf "Failed to get module data for: %s\n" module_name;
          None
