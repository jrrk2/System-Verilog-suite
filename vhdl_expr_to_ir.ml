(* VHDL Expression to IR Converter *)
(*
 * Assumes hardwired std_logic defaults:
 * - std_logic types are 1-bit by default
 * - Character literals '0', '1', 'Z', 'X', etc. are treated as 1-bit values
 * - No explicit library path resolution needed (IEEE.std_logic_1164 assumed)
 * - std_logic_vector widths are inferred from signal declarations
 *)

open Vhd_front.VhdlTypes
open Sv_ast

(* Simplified node representation for building IR *)
type simple_node = {
  sn_op: operation;
  sn_inputs: value_id list;
}

(* Context for tracking signals and generating IDs *)
type convert_context = {
  mutable next_id: int;
  signals: (string, value_id * int) Hashtbl.t;  (* name -> (id, width) *)
  mutable ir_nodes: (value_id, simple_node) Hashtbl.t;
  mutable ir_inputs: (string, value) Hashtbl.t;
  mutable ir_wires: (string, value) Hashtbl.t;
  mutable ir_constants: (value_id, value) Hashtbl.t;  (* id -> Constant value *)
}

(* Create new context *)
let create_context () = {
  next_id = 0;
  signals = Hashtbl.create 20;
  ir_nodes = Hashtbl.create 100;
  ir_inputs = Hashtbl.create 10;
  ir_wires = Hashtbl.create 20;
  ir_constants = Hashtbl.create 50;
}

(* Get next ID *)
let get_next_id ctx =
  let id = ctx.next_id in
  ctx.next_id <- ctx.next_id + 1;
  id

(* Add node to IR *)
let add_node ctx op inputs =
  let id = get_next_id ctx in
  let node = { sn_op = op; sn_inputs = inputs } in
  Hashtbl.add ctx.ir_nodes id node;
  id

(* Add constant to IR *)
let add_constant ctx value width =
  let id = get_next_id ctx in
  let const_value = Constant { id; value; width } in
  Hashtbl.add ctx.ir_constants id const_value;
  id

(* Get or create signal *)
let get_signal ctx name width =
  try
    Hashtbl.find ctx.signals name
  with Not_found ->
    let id = get_next_id ctx in
    Hashtbl.add ctx.signals name (id, width);
    Hashtbl.add ctx.ir_wires name (Wire { id; width; name });
    (id, width)

(* Extract string from identifier *)
let string_of_identifier (name, _pos) = name

(* Extract name string *)
let rec name_to_string = function
  | SimpleName id -> string_of_identifier id
  | OperatorString (s, _) -> s
  | SelectedName suffixes ->
      (* Handle library.type references - extract the last suffix (actual name) *)
      (* e.g., IEEE.std_logic_1164.std_logic -> std_logic *)
      (* suffixes is a list, take the last one which is the actual signal/type name *)
      (match List.rev suffixes with
       | SuffixSimpleName name :: _ -> name_to_string name
       | SuffixCharLiteral (c, _) :: _ -> String.make 1 c
       | SuffixOpSymbol (s, _) :: _ -> s
       | SuffixAll :: _ -> "all"
       | [] -> "<selected>")
  | AttributeName attr ->
      (* Handle signal'attribute - extract the prefix (signal name) *)
      (* e.g., CLK'event -> CLK *)
      let prefix_str = match attr.attributeprefix with
        | SuffixSimpleName name -> name_to_string name
        | SuffixCharLiteral (c, _) -> String.make 1 c
        | SuffixOpSymbol (s, _) -> s
        | SuffixAll -> "all"
      in
      (* For now, ignore the attribute part and just return the signal name *)
      prefix_str
  | SubscriptName (id, _) -> string_of_identifier id

(* Convert VHDL integer to OCaml int *)
let int_of_vhdl_int (big_int, _pos) =
  try
    Big_int.int_of_big_int big_int
  with _ -> 0  (* Fallback for very large numbers *)

(* Convert primary to IR value *)
let rec convert_primary ctx = function
  | IntPrimary vhdl_int ->
      let value = int_of_vhdl_int vhdl_int in
      (* For now, assume 32-bit integers *)
      let width = 32 in
      let id = add_constant ctx value width in
      (id, width)

  | CharPrimary (c, _pos) ->
      (* std_logic character literals: '0', '1', 'Z', 'X', 'U', 'W', 'L', 'H', '-' *)
      let value = match c with
        | '0' | 'L' -> 0  (* Logic 0 / Weak 0 *)
        | '1' | 'H' -> 1  (* Logic 1 / Weak 1 *)
        | 'Z' -> 0        (* High-impedance - map to 0 for behavioral analysis *)
        | 'X' | 'U' | 'W' -> 0  (* Unknown/Uninitialized/Weak unknown - map to 0 *)
        | '-' -> 0        (* Don't care - map to 0 *)
        | _ -> int_of_char c  (* Other characters - use ASCII value *)
      in
      let width = 1 in
      let id = add_constant ctx value width in
      (id, width)

  | NamePrimary name ->
      let name_str = name_to_string name in
      (* Assume all signals are 1-bit unless specified *)
      get_signal ctx name_str 1

  | ParenthesedPrimary expr ->
      convert_expression ctx expr

  | _ ->
      (* Fallback for unhandled primary types *)
      Printf.eprintf "Warning: Unhandled primary type\n";
      let id = add_constant ctx 0 1 in
      (id, 1)

(* Convert dotted to IR *)
and convert_dotted ctx = function
  | AtomDotted prim -> convert_primary ctx prim
  | Ldotted (p1, p2) ->
      (* For now, treat as accessing p1 (ignoring dot access) *)
      convert_primary ctx p1

(* Convert factor to IR *)
and convert_factor ctx = function
  | AtomFactor dotted ->
      convert_dotted ctx dotted

  | NotFactor dotted ->
      let (operand_id, width) = convert_dotted ctx dotted in
      let result_id = add_node ctx (Not { width }) [operand_id] in
      (result_id, width)

  | AndFactor dotted ->
      (* Reduction AND *)
      convert_dotted ctx dotted

  | OrFactor dotted ->
      (* Reduction OR *)
      convert_dotted ctx dotted

  | XorFactor dotted ->
      (* Reduction XOR *)
      convert_dotted ctx dotted

  | _ ->
      Printf.eprintf "Warning: Unhandled factor type\n";
      let id = add_constant ctx 0 1 in
      (id, 1)

(* Convert term to IR *)
and convert_term ctx = function
  | AtomTerm factor ->
      convert_factor ctx factor

  | MultTerm (f1, f2) ->
      let (id1, w1) = convert_factor ctx f1 in
      let (id2, w2) = convert_factor ctx f2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Mul { width; signed = false }) [id1; id2] in
      (result_id, width)

  | DivTerm (f1, f2) ->
      let (id1, w1) = convert_factor ctx f1 in
      let (id2, w2) = convert_factor ctx f2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Div { width; signed = false }) [id1; id2] in
      (result_id, width)

  | _ ->
      Printf.eprintf "Warning: Unhandled term type\n";
      let id = add_constant ctx 0 1 in
      (id, 1)

(* Convert simple_expression to IR *)
and convert_simple_expression ctx = function
  | AtomSimpleExpression term ->
      convert_term ctx term

  | AddSimpleExpression (t1, t2) ->
      let (id1, w1) = convert_term ctx t1 in
      let (id2, w2) = convert_term ctx t2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Add { width; signed = false }) [id1; id2] in
      (result_id, width)

  | SubSimpleExpression (t1, t2) ->
      let (id1, w1) = convert_term ctx t1 in
      let (id2, w2) = convert_term ctx t2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Sub { width; signed = false }) [id1; id2] in
      (result_id, width)

  | NegSimpleExpression term ->
      let (operand_id, width) = convert_term ctx term in
      (* Negate by subtracting from 0 *)
      let zero_id = add_constant ctx 0 width in
      let result_id = add_node ctx (Sub { width; signed = false }) [zero_id; operand_id] in
      (result_id, width)

  | ConcatSimpleExpression terms ->
      let ids_widths = List.map (convert_term ctx) terms in
      let ids = List.map fst ids_widths in
      let widths = List.map snd ids_widths in
      let total_width = List.fold_left (+) 0 widths in
      let result_id = add_node ctx (Concat { widths }) ids in
      (result_id, total_width)

(* Convert shift_expression to IR *)
and convert_shift_expression ctx = function
  | AtomShiftExpression simple_expr ->
      convert_simple_expression ctx simple_expr

  | ShiftLeftLogicalExpression (se1, se2) ->
      let (id1, width) = convert_simple_expression ctx se1 in
      let (id2, _) = convert_simple_expression ctx se2 in
      (* For now, use fixed shift without extracting amount *)
      let result_id = add_node ctx (Shift { width; direction = `Left; arithmetic = false; amount = None }) [id1; id2] in
      (result_id, width)

  | ShiftRightLogicalExpression (se1, se2) ->
      let (id1, width) = convert_simple_expression ctx se1 in
      let (id2, _) = convert_simple_expression ctx se2 in
      let result_id = add_node ctx (Shift { width; direction = `Right; arithmetic = false; amount = None }) [id1; id2] in
      (result_id, width)

  | _ ->
      Printf.eprintf "Warning: Unhandled shift expression\n";
      let id = add_constant ctx 0 1 in
      (id, 1)

(* Convert relation to IR *)
and convert_relation ctx = function
  | AtomRelation shift_expr ->
      convert_shift_expression ctx shift_expr

  | EqualRelation (se1, se2) ->
      let (id1, w1) = convert_shift_expression ctx se1 in
      let (id2, w2) = convert_shift_expression ctx se2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Compare { width; cmp_op = `Eq; signed = false }) [id1; id2] in
      (result_id, 1)  (* Comparison result is 1 bit *)

  | NotEqualRelation (se1, se2) ->
      let (id1, w1) = convert_shift_expression ctx se1 in
      let (id2, w2) = convert_shift_expression ctx se2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Compare { width; cmp_op = `Ne; signed = false }) [id1; id2] in
      (result_id, 1)

  | LessRelation (se1, se2) ->
      let (id1, w1) = convert_shift_expression ctx se1 in
      let (id2, w2) = convert_shift_expression ctx se2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Compare { width; cmp_op = `Lt; signed = false }) [id1; id2] in
      (result_id, 1)

  | LessOrEqualRelation (se1, se2) ->
      let (id1, w1) = convert_shift_expression ctx se1 in
      let (id2, w2) = convert_shift_expression ctx se2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Compare { width; cmp_op = `Le; signed = false }) [id1; id2] in
      (result_id, 1)

  | GreaterRelation (se1, se2) ->
      let (id1, w1) = convert_shift_expression ctx se1 in
      let (id2, w2) = convert_shift_expression ctx se2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Compare { width; cmp_op = `Gt; signed = false }) [id1; id2] in
      (result_id, 1)

  | GreaterOrEqualRelation (se1, se2) ->
      let (id1, w1) = convert_shift_expression ctx se1 in
      let (id2, w2) = convert_shift_expression ctx se2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Compare { width; cmp_op = `Ge; signed = false }) [id1; id2] in
      (result_id, 1)

  | _ ->
      Printf.eprintf "Warning: Unhandled relation type\n";
      let id = add_constant ctx 0 1 in
      (id, 1)

(* Convert logical_expression to IR *)
and convert_logical_expression ctx = function
  | AtomLogicalExpression relation ->
      convert_relation ctx relation

  | AndLogicalExpression (r1, r2) ->
      let (id1, w1) = convert_relation ctx r1 in
      let (id2, w2) = convert_relation ctx r2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (And { width }) [id1; id2] in
      (result_id, width)

  | OrLogicalExpression (r1, r2) ->
      let (id1, w1) = convert_relation ctx r1 in
      let (id2, w2) = convert_relation ctx r2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Or { width }) [id1; id2] in
      (result_id, width)

  | XorLogicalExpression (r1, r2) ->
      let (id1, w1) = convert_relation ctx r1 in
      let (id2, w2) = convert_relation ctx r2 in
      let width = max w1 w2 in
      let result_id = add_node ctx (Xor { width }) [id1; id2] in
      (result_id, width)

  | NandLogicalExpression (r1, r2) ->
      let (id1, w1) = convert_relation ctx r1 in
      let (id2, w2) = convert_relation ctx r2 in
      let width = max w1 w2 in
      let and_id = add_node ctx (And { width }) [id1; id2] in
      let result_id = add_node ctx (Not { width }) [and_id] in
      (result_id, width)

  | NorLogicalExpression (r1, r2) ->
      let (id1, w1) = convert_relation ctx r1 in
      let (id2, w2) = convert_relation ctx r2 in
      let width = max w1 w2 in
      let or_id = add_node ctx (Or { width }) [id1; id2] in
      let result_id = add_node ctx (Not { width }) [or_id] in
      (result_id, width)

  | XnorLogicalExpression (r1, r2) ->
      let (id1, w1) = convert_relation ctx r1 in
      let (id2, w2) = convert_relation ctx r2 in
      let width = max w1 w2 in
      let xor_id = add_node ctx (Xor { width }) [id1; id2] in
      let result_id = add_node ctx (Not { width }) [xor_id] in
      (result_id, width)

(* Convert expression to IR *)
and convert_expression ctx = function
  | AtomExpression logical_expr ->
      convert_logical_expression ctx logical_expr

  | ConditionExpression prim ->
      convert_primary ctx prim

(* Convert condition (used in if statements) *)
let convert_condition ctx (cond : vhdl_condition) =
  match cond with
  | Condition expr -> convert_expression ctx expr
