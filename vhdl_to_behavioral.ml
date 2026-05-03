(* VHDL to Behavioral IR Converter
 *
 * Converts VHDL AST (VhdlTree) to language-neutral behavioral IR.
 * This eliminates VHDL-isms and provides a clean intermediate representation.
 *)

open Vhd_front.VhdlTree
open Behavioral_ir

(* Conversion context *)
type context = {
  mutable signal_types: (string * btype) list;
  mutable next_temp_id: int;
}

let create_context () = {
  signal_types = [];
  next_temp_id = 0;
}

let fresh_temp ctx =
  let id = ctx.next_temp_id in
  ctx.next_temp_id <- id + 1;
  Printf.sprintf "_temp%d" id

let add_signal_type ctx name ty =
  ctx.signal_types <- (name, ty) :: ctx.signal_types

let get_signal_type ctx name =
  try List.assoc name ctx.signal_types
  with Not_found -> BInt { width = 32; signed = Unsigned }  (* Default *)

(* Convert VHDL expressions to behavioral IR expressions *)
let rec expr_to_bexpr ctx = function
  (* Simple name *)
  | Str name -> BVar name

  (* Integer literal *)
  | Double (VhdIntPrimary, Num num_str) ->
      (try
         let value = int_of_string num_str in
         BConst { value; width = 32 }
       with _ -> BConst { value = 0; width = 32 })

  (* Bit string literal: "0001", "1100", etc. *)
  | Double (VhdOperatorString, Str bit_str) ->
      (try
         let value = int_of_string ("0b" ^ bit_str) in
         BConst { value; width = String.length bit_str }
       with _ -> BConst { value = 0; width = 1 })

  (* Character literal: '0', '1' *)
  | Double (VhdCharPrimary, Char c) ->
      let value = if c = '1' then 1 else 0 in
      BConst { value; width = 1 }

  (* Relational operators *)
  | Triple (VhdEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BEq; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdNotEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BNe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdLessRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BLt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdGreaterRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BGt; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdLessOrEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BLe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdGreaterOrEqualRelation, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BGe; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  (* Arithmetic operators *)
  | Triple (VhdAddSimpleExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BAdd; lhs = lhs_expr; rhs = rhs_expr;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdSubSimpleExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BSub; lhs = lhs_expr; rhs = rhs_expr;
               result_type = BInt { width = 32; signed = Unsigned } }

  | Triple (VhdMultTerm, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BMul; lhs = lhs_expr; rhs = rhs_expr;
               result_type = BInt { width = 32; signed = Unsigned } }

  (* Logical operators *)
  | Triple (VhdAndLogicalExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BAnd; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  | Triple (VhdOrLogicalExpression, lhs, rhs) ->
      let lhs_expr = expr_to_bexpr ctx lhs in
      let rhs_expr = expr_to_bexpr ctx rhs in
      BBinOp { op = BOr; lhs = lhs_expr; rhs = rhs_expr; result_type = BBool }

  (* Unary operators *)
  | Double (VhdNotFactor, expr) ->
      let operand = expr_to_bexpr ctx expr in
      BUnOp { op = BNot; operand; result_type = BBool }

  (* Parenthesized expression *)
  | Double (VhdParenthesedPrimary, expr) ->
      expr_to_bexpr ctx expr

  (* Condition wrapper *)
  | Double (VhdCondition, expr) ->
      expr_to_bexpr ctx expr

  (* Attribute name (like clk'event) - ignore for now *)
  | Double (VhdAttributeName, _) ->
      BConst { value = 1; width = 1 }

  (* Array/indexed access: signal(index) *)
  | Triple (VhdNameParametersPrimary, Str name, _index) ->
      BVar name

  (* Target dotted (for assignments) *)
  | Double (VhdTargetDotted, inner) ->
      expr_to_bexpr ctx inner

  | VhdNone -> BConst { value = 0; width = 1 }

  | other ->
      Printf.eprintf "Warning: Unhandled expression pattern in vhdl_to_behavioral\n";
      BConst { value = 0; width = 1 }

(* Convert VHDL statements to behavioral IR statements *)
let rec stmt_to_bstmt ctx = function
  (* Signal assignment: signal <= value *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", Str name, VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
      let rhs_expr = expr_to_bexpr ctx rhs in
      [BAssign { lhs = name; rhs = rhs_expr }]

  (* Variable assignment: variable := value *)
  | Double (VhdSequentialVariableAssignment,
           Double (VhdSimpleVariableAssignment,
                  Quadruple (Vhdsimple_variable_assignment, Str "", Str name, rhs))) ->
      let rhs_expr = expr_to_bexpr ctx rhs in
      [BAssign { lhs = name; rhs = rhs_expr }]

  (* Signal assignment with indexed target *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", target, VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
      (* Extract target name *)
      let rec get_target_name = function
        | Str n -> n
        | Double (VhdTargetDotted, Triple (VhdNameParametersPrimary, Str n, _)) -> n
        | Double (VhdTargetDotted, inner) -> get_target_name inner
        | _ -> "_unknown"
      in
      let name = get_target_name target in
      let rhs_expr = expr_to_bexpr ctx rhs in
      [BAssign { lhs = name; rhs = rhs_expr }]

  (* If statement *)
  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     cond, then_clause, elsif_part)) ->
      let cond_expr = expr_to_bexpr ctx cond in
      let then_stmts = List.concat (List.map (stmt_to_bstmt ctx)
                                    (match then_clause with List l -> l | x -> [x])) in
      let else_stmts = convert_elsif_to_else ctx elsif_part in
      [BIf { condition = cond_expr; then_stmts; else_stmts }]

  (* List of statements *)
  | List stmts ->
      List.concat (List.map (stmt_to_bstmt ctx) stmts)

  (* Null statement *)
  | Double (VhdSequentialNull, _) -> []

  | VhdNone -> []

  | other ->
      Printf.eprintf "Warning: Unhandled statement pattern in vhdl_to_behavioral\n";
      []

(* Convert elsif chain to nested if-else *)
and convert_elsif_to_else ctx = function
  | Double (VhdElsif,
           Quintuple (Vhdif_statement, Str "",
                     cond, then_clause, else_part)) ->
      let cond_expr = expr_to_bexpr ctx cond in
      let then_stmts = List.concat (List.map (stmt_to_bstmt ctx)
                                    (match then_clause with List l -> l | x -> [x])) in
      let else_stmts = convert_elsif_to_else ctx else_part in
      [BIf { condition = cond_expr; then_stmts; else_stmts }]

  | Double (VhdElse, stmts) ->
      List.concat (List.map (stmt_to_bstmt ctx)
                   (match stmts with List l -> l | x -> [x]))

  | VhdElseNone -> []

  | other -> []

(* Extract clock signal from process body *)
let rec find_clock_in_expr = function
  (* Pattern: signal'event and signal = '1' *)
  | Triple (VhdAndLogicalExpression,
           Double (VhdAttributeName,
                  Triple (Vhdattribute_name,
                         Double (VhdSuffixSimpleName, Str sig_name),
                         Str "event")),
           _comparison) ->
      Some (sig_name, `Pos)

  (* Pattern: rising_edge(signal) *)
  | Triple (VhdNameParametersPrimary, Str "rising_edge",
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, Str sig_name))) ->
      Some (sig_name, `Pos)

  (* Pattern: falling_edge(signal) *)
  | Triple (VhdNameParametersPrimary, Str "falling_edge",
           Triple (Vhdassociation_element, _,
                  Double (VhdActualExpression, Str sig_name))) ->
      Some (sig_name, `Neg)

  | Double (VhdParenthesedPrimary, expr) -> find_clock_in_expr expr
  | Double (VhdCondition, expr) -> find_clock_in_expr expr

  | _ -> None

(* Find reset signal in condition *)
let rec find_reset_in_expr = function
  | Str s -> Some s
  | Triple (VhdEqualRelation, Str s, _) -> Some s
  | Triple (VhdEqualRelation, _, Str s) -> Some s
  | Double (VhdCondition, expr) -> find_reset_in_expr expr
  | Double (VhdParenthesedPrimary, expr) -> find_reset_in_expr expr
  | _ -> None

(* Analyze process to determine if it's sequential or combinational *)
let analyze_process_structure body =
  let clock_info = ref None in
  let reset_info = ref None in

  let rec scan = function
    (* Pattern: if reset then ... elsif rising_edge(clk) then ... *)
    | Double (VhdSequentialIf,
             Quintuple (Vhdif_statement, _, reset_cond, _reset_clause, elsif_part)) ->
        (match find_reset_in_expr reset_cond with
         | Some rst -> reset_info := Some (rst, `Pos)
         | None -> ());

        (* Look for clock in elsif *)
        (match elsif_part with
         | Double (VhdElsif,
                  Quintuple (Vhdif_statement, _, clk_cond, _, _)) ->
             (match find_clock_in_expr clk_cond with
              | Some (clk, edge) -> clock_info := Some (clk, edge)
              | None -> ())
         | _ -> ())

    (* Pattern: if rising_edge(clk) then ... *)
    | Double (VhdSequentialIf,
             Quintuple (Vhdif_statement, _, cond, _then_clause, _else_part)) ->
        (match find_clock_in_expr cond with
         | Some (clk, edge) -> clock_info := Some (clk, edge)
         | None -> ())

    | List items -> List.iter scan items
    | _ -> ()
  in
  scan body;
  (!clock_info, !reset_info)

(* Convert VHDL process to behavioral IR process *)
let process_to_bprocess ctx name sens_list body =
  let (clock_info, reset_info) = analyze_process_structure body in

  (* Convert body statements *)
  let body_stmts = match body with
    | List stmts -> List.concat (List.map (stmt_to_bstmt ctx) stmts)
    | stmt -> stmt_to_bstmt ctx stmt
  in

  match clock_info with
  | Some (clock, edge) ->
      (* Sequential process *)
      let (reset_name, reset_edge) = match reset_info with
        | Some (rst, edge) -> (Some rst, Some edge)
        | None -> (None, None)
      in
      BSequential {
        name;
        clock;
        clock_edge = edge;
        reset = reset_name;
        reset_edge;
        reset_async = true;  (* VHDL async reset if in sensitivity list *)
        body = body_stmts;
      }

  | None ->
      (* Combinational process *)
      BCombinational {
        name;
        sensitivity = [BAny];  (* Simplify for now *)
        body = body_stmts;
      }

(* Extract entity ports *)
let extract_entity_ports ctx = function
  | Triple (Vhddesign_unit, _,
           Double (VhdPrimaryUnit,
                  Double (VhdEntityDeclaration,
                         Quintuple (Vhdentity_declaration, Str entity_name,
                                   Triple (Vhdentity_header, _generics, port_list),
                                   _decls, _stmts)))) ->

      let rec extract_ports signals = function
        | List ports ->
            List.concat (List.map (extract_ports signals) ports)

        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeIn, _subtype, _kind, _default))) ->
            let signal = {
              name;
              stype = BBool;  (* Default to bool for now *)
              direction = `Input;
              initial_value = None;
            } in
            add_signal_type ctx name BBool;
            [signal]

        | Double (VhdInterfaceObjectDeclaration,
                 Double (VhdInterfaceDefaultDeclaration,
                        Sextuple (Vhdinterface_default_declaration, Str name,
                                 VhdInterfaceModeOut, _subtype, _kind, _default))) ->
            let signal = {
              name;
              stype = BBool;
              direction = `Output;
              initial_value = None;
            } in
            add_signal_type ctx name BBool;
            [signal]

        | _ -> []
      in
      (entity_name, extract_ports [] port_list)

  | _ -> ("", [])

(* Extract architecture body and convert processes *)
let extract_architecture ctx entity_name = function
  | Triple (Vhddesign_unit, _,
           Double (VhdSecondaryUnit,
                  Double (VhdArchitectureBody,
                         Quintuple (Vhdarchitecture_body, Str arch_name,
                                   Str _entity_ref, decls, stmts)))) ->

      (* Extract internal signals from declarations *)
      let internal_signals = ref [] in

      let rec extract_decls = function
        | List decl_list -> List.iter extract_decls decl_list
        | Double (VhdSignalDeclaration,
                 Quintuple (Vhdsignal_declaration, List names, _subtype, _kind, _init)) ->
            List.iter (function
              | Str name ->
                  let signal = {
                    name;
                    stype = BInt { width = 2; signed = Unsigned };  (* Default *)
                    direction = `Internal;
                    initial_value = Some (BConst { value = 0; width = 2 });
                  } in
                  add_signal_type ctx name (BInt { width = 2; signed = Unsigned });
                  internal_signals := signal :: !internal_signals
              | _ -> ()
            ) names
        | _ -> ()
      in
      extract_decls decls;

      (* Extract processes from statements *)
      let processes = ref [] in

      let rec extract_stmts = function
        | List stmt_list -> List.iter extract_stmts stmt_list

        | Double (VhdConcurrentProcessStatement,
                 Sextuple (Vhdprocess_statement, Str proc_name, _postponed,
                          Double (VhdSensitivityExpressionList, List _sens_list),
                          _proc_decls, proc_body)) ->
            let proc = process_to_bprocess ctx proc_name _sens_list proc_body in
            processes := proc :: !processes

        | _ -> ()
      in
      extract_stmts stmts;

      (!internal_signals, !processes)

  | _ -> ([], [])

(* Main conversion function *)
let convert_vhdl_to_behavioral vhdl_ast =
  let ctx = create_context () in

  (* Extract entity and ports *)
  let (entity_name, entity_ports) =
    List.fold_left (fun (name, ports) design_unit ->
      let (n, p) = extract_entity_ports ctx design_unit in
      if n <> "" then (n, p) else (name, ports)
    ) ("", []) vhdl_ast
  in

  (* Extract architecture *)
  let (internal_signals, processes) =
    List.fold_left (fun (sigs, procs) design_unit ->
      let (s, p) = extract_architecture ctx entity_name design_unit in
      (s @ sigs, p @ procs)
    ) ([], []) vhdl_ast
  in

  (* Combine all signals *)
  let all_signals = entity_ports @ internal_signals in

  (* Build module *)
  let bmodule = {
    name = entity_name;
    params = [];
    signals = all_signals;
    processes;
    instances = [];
    funcs = [];
    mems = [];
  } in

  { modules = [bmodule]; library_cells = [] }

(* Convert multiple VHDL ASTs to behavioral IR *)
let convert_multiple vhdl_asts =
  let all_modules = List.concat_map (fun ast ->
    let prog = convert_vhdl_to_behavioral ast in
    prog.Behavioral_ir.modules
  ) vhdl_asts in
  { Behavioral_ir.modules = all_modules; library_cells = [] }

(* Helper: Convert VHDL file to behavioral IR *)
let convert_vhdl_file_to_behavioral filename =
  (* Create a fresh hashtable for this parse to ensure isolation *)
  let fresh_hash = Hashtbl.create 256 in
  let old_hash = !Vhd_front.Vabstraction.vhdlhash in
  Vhd_front.Vabstraction.vhdlhash := fresh_hash;

  (* Also clear the settings filelists to prevent accumulation *)
  let old_settings = !Vhd_front.VhdlSettings.settings in
  Vhd_front.VhdlSettings.settings := {!Vhd_front.VhdlSettings.settings with
    fileparsedlist = [];
    filefailedlist = [];
  };

  try
    (* Parse the file *)
    let succ = ref true in
    Vhd_front.VhdlMain.main succ [filename];

    if not !succ then begin
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;
      None
    end else begin
      (* Extract vhdintf trees from our fresh hashtable *)
      let trees = ref [] in
      Hashtbl.iter (fun (k, _) _ ->
        let simplified = Vhd_front.Rewrite.abstraction (Vhd_front.Rewrite.abstraction k) in
        trees := simplified :: !trees
      ) fresh_hash;

      (* Convert to behavioral IR *)
      let result = convert_vhdl_to_behavioral !trees in

      (* Restore old hashtable and settings before returning *)
      Vhd_front.Vabstraction.vhdlhash := old_hash;
      Vhd_front.VhdlSettings.settings := old_settings;

      Some result
    end
  with e ->
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Vhd_front.VhdlSettings.settings := old_settings;
    None
