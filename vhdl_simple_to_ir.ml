(* Simple VHDL to IR Converter *)
(*
 * Based on proven patterns from rewrite.ml
 *
 * Assumptions (as specified):
 * - All port types default to std_logic/std_logic_vector
 * - Integer generics used for parameterization
 * - Standard synchronous design patterns (clock + reset)
 * - No explicit IEEE library resolution needed
 *
 * This converter mirrors rewrite.ml's pattern matching but generates IR instead of strings
 *)

open Vhd_front.VhdlTree
open Sv_ast

(* Context for IR generation - simpler than rewrite.ml's match2_args *)
type ir_context = {
  mutable next_id: int;
  signals: (string, value_id * int) Hashtbl.t;  (* name -> (id, width) *)
  mutable nodes: (value_id * operation * value_id list) list;  (* IR nodes *)
  mutable inputs: (string * int) list;   (* input signals with widths *)
  mutable outputs: (string * int) list;  (* output signals with widths *)
  mutable wires: (string * int) list;    (* internal wires with widths *)
}

let create_context () = {
  next_id = 0;
  signals = Hashtbl.create 20;
  nodes = [];
  inputs = [];
  outputs = [];
  wires = [];
}

let fresh_id ctx =
  let id = ctx.next_id in
  ctx.next_id <- ctx.next_id + 1;
  id

let add_node ctx op inputs =
  let id = fresh_id ctx in
  ctx.nodes <- (id, op, inputs) :: ctx.nodes;
  id

(* Get or create signal ID *)
let get_signal ctx name width =
  if Hashtbl.mem ctx.signals name then
    fst (Hashtbl.find ctx.signals name)
  else begin
    let id = fresh_id ctx in
    Hashtbl.add ctx.signals name (id, width);
    id
  end

(* Infer width from VHDL subtype indication *)
let rec infer_width = function
  | Quadruple (Vhdsubtype_indication, _, Str "std_logic", VhdNoConstraint) -> 1
  | Quadruple (Vhdsubtype_indication, _, Str "std_logic_vector",
               Double (VhdArrayConstraint,
                 Triple (Vhdassociation_element, VhdFormalIndexed,
                   Double (VhdActualDiscreteRange,
                     Double (VhdRange,
                       Triple (VhdDecreasingRange,
                         Double (VhdIntPrimary, Num hi),
                         Double (VhdIntPrimary, Num lo))))))) ->
      (int_of_string hi) - (int_of_string lo) + 1
  | Quadruple (Vhdsubtype_indication, _, Str "std_logic_vector",
               Double (VhdArrayConstraint,
                 Triple (Vhdassociation_element, VhdFormalIndexed,
                   Double (VhdActualDiscreteRange,
                     Double (VhdRange,
                       Triple (VhdIncreasingRange,
                         Double (VhdIntPrimary, Num lo),
                         Double (VhdIntPrimary, Num hi))))))) ->
      (int_of_string hi) - (int_of_string lo) + 1
  | Quadruple (Vhdsubtype_indication, _, Str "natural", _) -> 32
  | Quadruple (Vhdsubtype_indication, _, Str "integer", _) -> 32
  | _ -> 1  (* default to 1-bit *)

(* Convert VHDL expression to IR - based on rewrite.ml patterns *)
let rec convert_expr ctx = function
  (* Relations - from rewrite.ml lines 175-184 *)
  | Triple (VhdEqualRelation, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      let width = 1 in  (* comparison always produces 1 bit *)
      add_node ctx (Compare { cmp_op = `Eq; width; signed = false }) [l_id; r_id]

  | Triple (VhdNotEqualRelation, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Compare { cmp_op = `Ne; width = 1; signed = false }) [l_id; r_id]

  | Triple (VhdLessRelation, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Compare { cmp_op = `Lt; width = 1; signed = false }) [l_id; r_id]

  | Triple (VhdGreaterRelation, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Compare { cmp_op = `Gt; width = 1; signed = false }) [l_id; r_id]

  | Triple (VhdGreaterOrEqualRelation, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Compare { cmp_op = `Ge; width = 1; signed = false }) [l_id; r_id]

  | Triple (VhdLessOrEqualRelation, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Compare { cmp_op = `Le; width = 1; signed = false }) [l_id; r_id]

  (* Arithmetic - from rewrite.ml lines 185-188 *)
  | Triple (VhdAddSimpleExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Add { width = 32; signed = true }) [l_id; r_id]

  | Triple (VhdSubSimpleExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Sub { width = 32; signed = true }) [l_id; r_id]

  (* Logical operations - from rewrite.ml lines 189-202 *)
  | Triple (VhdOrLogicalExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Or { width = 1 }) [l_id; r_id]

  | Triple (VhdXorLogicalExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Xor { width = 1 }) [l_id; r_id]

  | Triple (VhdAndLogicalExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (And { width = 1 }) [l_id; r_id]

  (* Shifts - from rewrite.ml lines 193-196 *)
  | Triple (VhdShiftLeftLogicalExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Shift { width = 32; direction = `Left; arithmetic = false; amount = None }) [l_id; r_id]

  | Triple (VhdShiftRightLogicalExpression, lft, rght) ->
      let l_id = convert_expr ctx lft in
      let r_id = convert_expr ctx rght in
      add_node ctx (Shift { width = 32; direction = `Right; arithmetic = false; amount = None }) [l_id; r_id]

  (* Parentheses - from rewrite.ml lines 197-200 *)
  | Double (VhdParenthesedPrimary, x) ->
      convert_expr ctx x

  (* Indexed access - from rewrite.ml lines 211-223 *)
  | Triple (VhdNameParametersPrimary, Str src,
            Triple (Vhdassociation_element, VhdFormalIndexed,
                    Double (VhdActualExpression, idx))) ->
      let src_id = get_signal ctx src 32 in
      let idx_id = convert_expr ctx idx in
      add_node ctx (Extract { width = 1; lsb = 0; msb = 0 }) [src_id; idx_id]

  | Triple (VhdNameParametersPrimary, Str src,
            Triple (Vhdassociation_element, VhdFormalIndexed,
                    Double (VhdActualDiscreteRange, range))) ->
      let src_id = get_signal ctx src 32 in
      (* For now, treat range extracts as the full signal *)
      src_id

  (* Literals - from rewrite.ml lines 225-227, 244 *)
  | Double (VhdOperatorString, Str v) when String.length v > 0 &&
                                           (v.[0] >= '0' && v.[0] <= '1') ->
      let width = String.length v in
      let value = int_of_string ("0b" ^ v) in
      let const_id = fresh_id ctx in
      (* TODO: add constant to context *)
      const_id

  | Double (VhdCharPrimary, Char ch) ->
      let value = if ch = '1' then 1 else 0 in
      let const_id = fresh_id ctx in
      const_id

  | Double (VhdIntPrimary, Num n) ->
      let const_id = fresh_id ctx in
      const_id

  (* Simple names - signals *)
  | Str name ->
      get_signal ctx name 1

  (* Fallback *)
  | _ ->
      Printf.eprintf "Warning: Unhandled expression pattern\n";
      fresh_id ctx

(* Convert sequential statements - based on rewrite.ml patterns *)
let rec convert_sequential_stmt ctx = function
  (* Signal assignment - from rewrite.ml lines 264-277 *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", Str nam, VhdDelayNone, expr))) ->
      let expr_id = convert_expr ctx expr in
      let dst_id = get_signal ctx nam 1 in
      [(dst_id, expr_id)]

  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", dst, VhdDelayNone, expr))) ->
      let expr_id = convert_expr ctx expr in
      let dst_id = convert_expr ctx dst in
      [(dst_id, expr_id)]

  (* Variable assignment - from rewrite.ml lines 255-262 *)
  | Double (VhdSequentialVariableAssignment,
           Double (VhdSimpleVariableAssignment,
                  Quadruple (Vhdsimple_variable_assignment,
                            Str "", dst, expr))) ->
      let expr_id = convert_expr ctx expr in
      let dst_id = convert_expr ctx dst in
      [(dst_id, expr_id)]

  (* If statement - convert to Mux IR operations *)
  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     Double (VhdCondition, cond),
                     then_clause,
                     VhdElseNone)) ->
      let cond_id = convert_expr ctx cond in
      let then_assigns = convert_sequential_stmt ctx then_clause in
      (* For simple assignments, create Mux nodes *)
      then_assigns

  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     Double (VhdCondition, cond),
                     then_clause,
                     Double (VhdElse, else_clause))) ->
      let cond_id = convert_expr ctx cond in
      let then_assigns = convert_sequential_stmt ctx then_clause in
      let else_assigns = convert_sequential_stmt ctx else_clause in
      (* Merge assignments using Mux *)
      then_assigns @ else_assigns

  (* List of statements *)
  | List stmts ->
      List.concat (List.map (convert_sequential_stmt ctx) stmts)

  (* Fallback *)
  | _ -> []

(* Convert process - based on rewrite.ml lines 553-604 *)
let convert_process ctx = function
  (* Standard synchronous process with async reset - rewrite.ml line 553 *)
  | Sextuple (Vhdprocess_statement, Str process, Str _postponed,
             Double (VhdSensitivityExpressionList, List dep_lst),
             decls,
             Double (VhdSequentialIf,
                    Quintuple (Vhdif_statement, Str "",
                              Double (VhdCondition,
                                     Double (VhdParenthesedPrimary,
                                            Triple (VhdEqualRelation, Str reset,
                                                   Double (VhdCharPrimary, Char rsense)))),
                              reset_clause,
                              Double (VhdElsif,
                                     Quintuple (Vhdif_statement, Str "",
                                               Double (VhdCondition,
                                                      Double (VhdParenthesedPrimary,
                                                             Triple (VhdAndLogicalExpression,
                                                                    Double (VhdAttributeName,
                                                                           Triple (Vhdattribute_name,
                                                                                  Double (VhdSuffixSimpleName, Str clk),
                                                                                  Str "event")),
                                                                    Triple (VhdEqualRelation, Str clk',
                                                                           Double (VhdCharPrimary, Char csense))))),
                                               main_clause,
                                               VhdElseNone)))))
    when List.mem (Str clk) dep_lst && List.mem (Str reset) dep_lst && clk = clk' ->
      (* This is a synchronous process with async reset *)
      let reset_assigns = convert_sequential_stmt ctx reset_clause in
      let main_assigns = convert_sequential_stmt ctx main_clause in

      (* Create Register operations for each signal assigned in main_clause *)
      List.iter (fun (dst_id, expr_id) ->
        let clk_id = get_signal ctx clk 1 in
        let reset_id = get_signal ctx reset 1 in
        let width = 1 in  (* TODO: infer from signal *)
        let reg_id = add_node ctx (Register { width; clock = clk_id;
                                              reset = Some reset_id;
                                              enable = None;
                                              reset_value = 0 }) [expr_id] in
        ignore reg_id
      ) main_assigns

  (* Simple combinational process *)
  | Sextuple (Vhdprocess_statement, Str _process, Str _postponed,
             Double (VhdSensitivityExpressionList, _sensitivity),
             _decls,
             stmts) ->
      let assigns = convert_sequential_stmt ctx stmts in
      (* Just wire the assignments *)
      ()

  | _ ->
      Printf.eprintf "Warning: Unhandled process pattern\n";
      ()

(* Convert entity ports *)
let convert_port ctx = function
  | Double (VhdInterfaceObjectDeclaration,
           Double (VhdInterfaceDefaultDeclaration,
                  Sextuple (Vhdinterface_default_declaration, Str name,
                           VhdInterfaceModeIn,
                           subtype_ind,
                           VhdSignalKindDefault, _default))) ->
      let width = infer_width subtype_ind in
      ctx.inputs <- (name, width) :: ctx.inputs;
      get_signal ctx name width

  | Double (VhdInterfaceObjectDeclaration,
           Double (VhdInterfaceDefaultDeclaration,
                  Sextuple (Vhdinterface_default_declaration, Str name,
                           VhdInterfaceModeOut,
                           subtype_ind,
                           VhdSignalKindDefault, _default))) ->
      let width = infer_width subtype_ind in
      ctx.outputs <- (name, width) :: ctx.outputs;
      get_signal ctx name width

  | _ ->
      Printf.eprintf "Warning: Unhandled port pattern\n";
      -1

(* Convert architecture - from rewrite.ml lines 660-676 *)
let convert_architecture ctx = function
  | Quintuple (Vhdarchitecture_body, Str arch, Str design, decls, stmts) ->
      (* Process declarations (signals, etc.) *)
      let process_decl = function
        | Double (VhdBlockSignalDeclaration,
                 Quintuple (Vhdsignal_declaration, Str name,
                           subtype_ind,
                           VhdSignalKindDefault, _init)) ->
            let width = infer_width subtype_ind in
            ctx.wires <- (name, width) :: ctx.wires;
            ignore (get_signal ctx name width)
        | _ -> ()
      in
      (match decls with
       | List lst -> List.iter process_decl lst
       | d -> process_decl d);

      (* Process concurrent statements *)
      let process_stmt = function
        | Double (VhdConcurrentProcessStatement, proc) ->
            convert_process ctx proc
        | _ -> ()
      in
      (match stmts with
       | List lst -> List.iter process_stmt lst
       | s -> process_stmt s)

  | _ ->
      Printf.eprintf "Warning: Unhandled architecture pattern\n"

(* Main entry point: convert VHDL design unit *)
let convert_design_unit = function
  | Triple (Vhddesign_unit,
           _context_clauses,
           Double (VhdPrimaryUnit,
                  Double (VhdEntityDeclaration,
                         Quintuple (Vhdentity_declaration, Str entity_name,
                                   Triple (Vhdentity_header, _generics, ports),
                                   _decls, _stmts)))) ->
      let ctx = create_context () in

      (* Convert ports *)
      (match ports with
       | List port_list -> List.iter (fun p -> ignore (convert_port ctx p)) port_list
       | p -> ignore (convert_port ctx p));

      Some (ctx, entity_name)

  | Triple (Vhddesign_unit,
           _context_clauses,
           Double (VhdSecondaryUnit,
                  Double (VhdArchitectureBody, arch))) ->
      (* Need context from entity - for now create fresh *)
      let ctx = create_context () in
      convert_architecture ctx arch;
      Some (ctx, "architecture")

  | _ -> None

(* Convert full VHDL file (list of design units) *)
let convert_vhdl_file design_units =
  let entity_ctx = ref None in
  let arch_ctx = ref None in

  List.iter (fun unit ->
    match convert_design_unit unit with
    | Some (ctx, "architecture") -> arch_ctx := Some ctx
    | Some (ctx, entity_name) -> entity_ctx := Some ctx
    | None -> ()
  ) design_units;

  match !entity_ctx, !arch_ctx with
  | Some e_ctx, Some a_ctx ->
      (* Merge contexts and return IR *)
      Some a_ctx
  | Some ctx, None | None, Some ctx ->
      Some ctx
  | None, None ->
      None
