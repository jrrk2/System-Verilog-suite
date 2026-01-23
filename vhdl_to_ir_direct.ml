(* VHDL to IR Direct Converter *)
(*
 * Based on proven match2' patterns from rewrite.ml
 * Instead of Buffer.add_string for SystemVerilog, we generate IR nodes
 *
 * Key insight: Copy the pattern matching from rewrite.ml (lines 169-676)
 * but replace string generation with IR construction
 *)

open Vhd_front.VhdlTree
open Sv_ast

(* IR generation context - similar to rewrite.ml's match2_args *)
type ir_context = {
  mutable next_id: int;
  signals: (string, value_id * int) Hashtbl.t;  (* name -> (id, width) *)
  mutable nodes: (value_id * operation * value_id list) list;
  mutable inputs: (string * int) list;
  mutable outputs: (string * int) list;
  mutable wires: (string * int) list;
  mutable constants: (value_id * int * int) list;  (* id, value, width *)
}

let create_context () = {
  next_id = 0;
  signals = Hashtbl.create 20;
  nodes = [];
  inputs = [];
  outputs = [];
  wires = [];
  constants = [];
}

let fresh_id ctx =
  let id = ctx.next_id in
  ctx.next_id <- ctx.next_id + 1;
  id

let add_node ctx op inputs =
  let id = fresh_id ctx in
  ctx.nodes <- (id, op, inputs) :: ctx.nodes;
  id

let get_signal ctx name width =
  if Hashtbl.mem ctx.signals name then
    fst (Hashtbl.find ctx.signals name)
  else begin
    let id = fresh_id ctx in
    Hashtbl.add ctx.signals name (id, width);
    id
  end

let add_constant ctx value width =
  let id = fresh_id ctx in
  ctx.constants <- (id, value, width) :: ctx.constants;
  id

(* Convert VHDL expression to IR - based on rewrite.ml match2' patterns *)
let rec expr_to_ir ctx = function
  (* Relations - from rewrite.ml lines 175-184 *)
  | Triple (VhdEqualRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Eq; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdNotEqualRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Ne; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdLessRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Lt; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdGreaterRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Gt; width = 32; signed = false }) [l_id; r_id]

  | Triple (VhdGreaterOrEqualRelation, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Compare { cmp_op = `Ge; width = 32; signed = false }) [l_id; r_id]

  (* Arithmetic - from rewrite.ml lines 185-188 *)
  | Triple (VhdAddSimpleExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Add { width = 32; signed = true }) [l_id; r_id]

  | Triple (VhdSubSimpleExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Sub { width = 32; signed = true }) [l_id; r_id]

  (* Logical - from rewrite.ml lines 189-202 *)
  | Triple (VhdOrLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Or { width = 1 }) [l_id; r_id]

  | Triple (VhdXorLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Xor { width = 1 }) [l_id; r_id]

  | Triple (VhdAndLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (And { width = 1 }) [l_id; r_id]

  (* Shifts - from rewrite.ml lines 193-196 *)
  | Triple (VhdShiftLeftLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Shift { width = 32; direction = `Left; arithmetic = false; amount = None }) [l_id; r_id]

  | Triple (VhdShiftRightLogicalExpression, lft, rght) ->
      let l_id = expr_to_ir ctx lft in
      let r_id = expr_to_ir ctx rght in
      add_node ctx (Shift { width = 32; direction = `Right; arithmetic = false; amount = None }) [l_id; r_id]

  (* Parentheses - from rewrite.ml lines 197-200 *)
  | Double (VhdParenthesedPrimary, x) ->
      expr_to_ir ctx x

  (* Power of 2 optimization - from rewrite.ml line 203-204 *)
  | Triple (VhdExpFactor, Double (VhdIntPrimary, Num "2"), expr) ->
      let one_id = add_constant ctx 1 32 in
      let exp_id = expr_to_ir ctx expr in
      add_node ctx (Shift { width = 32; direction = `Left; arithmetic = false; amount = None }) [one_id; exp_id]

  (* Indexed access - from rewrite.ml lines 211-223 *)
  | Triple (VhdNameParametersPrimary, Str src,
            Triple (Vhdassociation_element, VhdFormalIndexed,
                    Double (VhdActualExpression, idx))) ->
      let src_id = get_signal ctx src 32 in
      let idx_id = expr_to_ir ctx idx in
      (* Extract single bit *)
      add_node ctx (Extract { width = 1; lsb = 0; msb = 0 }) [src_id; idx_id]

  | Triple (VhdNameParametersPrimary, Str src,
            Triple (Vhdassociation_element, VhdFormalIndexed,
                    Double (VhdActualDiscreteRange, _range))) ->
      (* For now, return the full signal - could parse range for Extract *)
      get_signal ctx src 32

  (* Character literals - from rewrite.ml line 227 *)
  | Double (VhdCharPrimary, Char ch) ->
      let value = if ch = '1' then 1 else 0 in
      add_constant ctx value 1

  (* Bit string literals - from rewrite.ml lines 225-226 *)
  | Double (VhdOperatorString, Str v) when String.length v > 0 && (v.[0] = '0' || v.[0] = '1') ->
      let width = String.length v in
      let value = int_of_string ("0b" ^ v) in
      add_constant ctx value width

  (* Integer literals - from rewrite.ml line 244 *)
  | Double (VhdIntPrimary, Num n) ->
      add_constant ctx (int_of_string n) 32

  (* Condition wrapper - from rewrite.ml line 224 *)
  | Double (VhdCondition, x) ->
      expr_to_ir ctx x

  (* Simple names (signals) - from rewrite.ml line 174 *)
  | Str name ->
      get_signal ctx name 32

  (* Fallback *)
  | _ ->
      Printf.eprintf "Warning: Unhandled expression in expr_to_ir\n";
      fresh_id ctx

(* Convert sequential statements - based on rewrite.ml patterns *)
let rec stmt_to_ir ctx = function
  (* Signal assignment - from rewrite.ml lines 264-277 *)
  | Double (VhdSequentialSignalAssignment,
           Double (VhdSimpleSignalAssignment,
                  Quintuple (Vhdsimple_signal_assignment_statement,
                            Str "", Str nam, VhdDelayNone,
                            Double (Vhdwaveform_element, rhs)))) ->
      let rhs_id = expr_to_ir ctx rhs in
      let lhs_id = get_signal ctx nam 32 in
      [(lhs_id, rhs_id)]

  (* Variable assignment - from rewrite.ml lines 255-262 *)
  | Double (VhdSequentialVariableAssignment,
           Double (VhdSimpleVariableAssignment,
                  Quadruple (Vhdsimple_variable_assignment, Str "", Str dst, rhs))) ->
      let rhs_id = expr_to_ir ctx rhs in
      let lhs_id = get_signal ctx dst 32 in
      [(lhs_id, rhs_id)]

  (* If statement - convert to Mux *)
  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     Double (VhdCondition, cond),
                     then_clause,
                     VhdElseNone)) ->
      let _cond_id = expr_to_ir ctx cond in
      stmt_to_ir ctx then_clause

  | Double (VhdSequentialIf,
           Quintuple (Vhdif_statement, Str "",
                     Double (VhdCondition, cond),
                     then_clause,
                     Double (VhdElse, else_clause))) ->
      let cond_id = expr_to_ir ctx cond in
      let then_assigns = stmt_to_ir ctx then_clause in
      let else_assigns = stmt_to_ir ctx else_clause in
      (* Create Mux for each assignment *)
      List.map2 (fun (dst_then, val_then) (dst_else, val_else) ->
        assert (dst_then = dst_else);
        let mux_id = add_node ctx (Mux { width = 32 }) [cond_id; val_then; val_else] in
        (dst_then, mux_id)
      ) then_assigns else_assigns

  (* List of statements *)
  | List stmts ->
      List.concat (List.map (stmt_to_ir ctx) stmts)

  (* Fallback *)
  | _ ->
      Printf.eprintf "Warning: Unhandled statement in stmt_to_ir\n";
      []

(* Convert process - based on rewrite.ml lines 553-604 *)
let process_to_ir ctx = function
  (* Synchronous process with async reset - rewrite.ml line 553 *)
  | Sextuple (Vhdprocess_statement, Str _process, Str _postponed,
             Double (VhdSensitivityExpressionList, List dep_lst),
             _decls,
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
                                                                           Double (VhdCharPrimary, Char _csense))))),
                                               main_clause,
                                               VhdElseNone)))))
    when List.mem (Str clk) dep_lst && List.mem (Str reset) dep_lst && clk = clk' ->
      (* Create registers for all assignments in main_clause *)
      let assigns = stmt_to_ir ctx main_clause in
      let clk_id = get_signal ctx clk 1 in
      let reset_id = get_signal ctx reset 1 in
      List.iter (fun (dst_id, data_id) ->
        let _reg_id = add_node ctx
          (Register { width = 32; clock = clk_id; reset = Some reset_id;
                     enable = None; reset_value = 0 })
          [data_id] in
        ()
      ) assigns

  (* Simple combinational process *)
  | Sextuple (Vhdprocess_statement, Str _process, Str _postponed,
             Double (VhdSensitivityExpressionList, _sensitivity),
             _decls,
             stmts) ->
      let _assigns = stmt_to_ir ctx stmts in
      ()

  | _ ->
      Printf.eprintf "Warning: Unhandled process pattern\n"

(* Main conversion function *)
let convert_vhdl_to_ir vhdl_files =
  Printf.printf "VHDL → IR Direct Conversion\n";
  Printf.printf "%s\n" (String.make 70 '=');

  (* Step 1: Parse VHDL using proven infrastructure *)
  Printf.printf "Step 1: Parsing VHDL files...\n";
  let succ = ref true in
  Vhd_front.VhdlMain.main succ vhdl_files;

  if not !succ then begin
    Printf.eprintf "❌ Parsing failed\n";
    exit 1
  end;
  Printf.printf "   ✅ Parsed successfully\n\n";

  (* Step 2: Extract and simplify vhdintf trees *)
  Printf.printf "Step 2: Extracting vhdintf trees...\n";
  let vhdintf_list = ref [] in
  Hashtbl.iter (fun (k, _) _ ->
    let simplified = Vhd_front.Rewrite.abstraction (Vhd_front.Rewrite.abstraction k) in
    vhdintf_list := simplified :: !vhdintf_list
  ) !(Vhd_front.Vabstraction.vhdlhash);
  Printf.printf "   Processed %d design units\n\n" (List.length !vhdintf_list);

  (* Step 3: Convert to IR using our match2'-inspired converter *)
  Printf.printf "Step 3: Converting to IR (using match2' patterns)...\n";
  let ctx = create_context () in

  List.iter (fun tree ->
    match tree with
    | Double (VhdConcurrentProcessStatement, proc) ->
        process_to_ir ctx proc
    | _ -> ()
  ) !vhdintf_list;

  Printf.printf "   Generated %d IR nodes\n" (List.length ctx.nodes);
  Printf.printf "   Signals: %d\n" (Hashtbl.length ctx.signals);
  Printf.printf "   ✅ IR generation complete\n\n";

  ctx

let () =
  Printf.printf "VHDL to IR Direct Converter\n";
  Printf.printf "Based on proven match2' patterns from rewrite.ml\n\n";
  Printf.printf "Usage: Build this into a complete converter\n";
  Printf.printf "   - Copy match2' pattern matching\n";
  Printf.printf "   - Replace Buffer.add_string with IR node creation\n";
  Printf.printf "   - Skip intermediate SystemVerilog generation\n"
