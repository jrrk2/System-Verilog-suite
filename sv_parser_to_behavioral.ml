(* sv-parser CST → Behavioral_ir.bprogram.
 *
 * MVP scope (this commit): interface shape only — module name and
 * ports (name, direction, packed width).  Enough to participate as
 * a port-surface oracle in the miter: if our verilator-vs-verible
 * equivalence run says the two designs have different port lists,
 * sv-parser's CST-derived view is a third independent source of
 * truth that breaks the tie.
 *
 * The CST node names follow IEEE 1800-2017 Annex A — anyone needing
 * to extend this file can browse sv-parser's syntaxtree crate
 * directly.
 *
 * Roadmap (tasks #37/#38/#39 will extend this same module):
 *   - expressions (BBinOp, BUnOp, BSlice, BConst, BCond, BConcat, …)
 *   - statement bodies (always_ff / always_comb, BAssign, BIf, …)
 *   - declarations (localparam, typedef, function/task → bfunc)
 *)

open Behavioral_ir
open Sv_parser_dump

(* --------------------------------------------------------------- *)
(* CST navigation helpers                                          *)
(* --------------------------------------------------------------- *)

(* Pull the simple-identifier name out of a node that wraps one,
 * e.g. ModuleIdentifier → Identifier → SimpleIdentifier → '<name>'.
 * Falls back to None when the shape doesn't match. *)
let identifier_name node =
  let id_node = find_first node
    ~name_is:(fun n -> n = "SimpleIdentifier") in
  match id_node with
  | Some t -> first_leaf t
  | None -> None

(* True if a node has a child Keyword carrying the given token text.
 * Used to read PortDirection / Signing / IntegerVectorType flavours
 * without descending through a fixed chain of wrapper-only nodes. *)
let has_keyword_token node target =
  let found = ref false in
  walk node ~f:(function
    | Leaf { value; _ } when value = target -> found := true
    | _ -> ());
  !found

(* Parse an integer literal token (UnsignedNumber / DecimalNumber).
 * Strips underscores; ignores base prefixes (the lexer-level
 * token-tree carries 'b'/'h' bases via different node names that
 * we'd need separately — out of scope for the interface-shape
 * MVP since port widths use plain decimals). *)
let int_of_token s =
  let cleaned = String.concat "" (String.split_on_char '_' s) in
  try Some (int_of_string cleaned) with _ -> None

(* Constant-expression evaluator.  Handles enough shapes to compute
 * port widths from parametrised dimensions like `[W-1:0]` where W
 * comes from a ParameterPortList.  Anything more elaborate
 * (functions, conditional expressions, struct/array literals) is
 * deferred to the expression-conversion task (#37); we return None
 * and the caller falls back to a default width.
 *
 * Recognised shapes:
 *   ConstantExpression → single child → recurse
 *   ConstantExpressionBinary { lhs; op; rhs } → fold
 *   ConstantExpressionUnary  { op; operand }   → negate
 *   ConstantPrimary → child → recurse
 *   ConstantFunctionCall → bare identifier → parameter lookup
 *   PrimaryLiteral → Number → IntegralNumber → DecimalNumber
 *                  → UnsignedNumber → leaf integer
 *   Identifier-ish chains → first identifier leaf → parameter lookup
 *)
let rec eval_const_expr ~params node =
  match node with
  | Leaf { value; _ } -> int_of_token value
  | Node { name; children } ->
      match name with
      | "ConstantExpressionBinary" ->
          let exprs = List.filter_map (function
            | Node { name = "ConstantExpression"; _ } as t -> Some t
            | _ -> None) children in
          let op_str = List.find_map (function
            | Node { name = "BinaryOperator"; _ } as t -> first_leaf t
            | _ -> None) children in
          (match exprs, op_str with
           | [a; b], Some op ->
               (match eval_const_expr ~params a,
                      eval_const_expr ~params b with
                | Some x, Some y ->
                    (match op with
                     | "+" -> Some (x + y)
                     | "-" -> Some (x - y)
                     | "*" -> Some (x * y)
                     | "/" when y <> 0 -> Some (x / y)
                     | "%" when y <> 0 -> Some (x mod y)
                     | "<<" -> Some (x lsl y)
                     | ">>" -> Some (x lsr y)
                     | "&" -> Some (x land y)
                     | "|" -> Some (x lor y)
                     | "^" -> Some (x lxor y)
                     | _ -> None)
                | _ -> None)
           | _ -> None)
      | "ConstantExpressionUnary" ->
          let operand = List.find_map (function
            | Node { name = "ConstantPrimary"; _ } as t -> Some t
            | Node { name = "ConstantExpression"; _ } as t -> Some t
            | _ -> None) children in
          let op_str = List.find_map (function
            | Node { name = "UnaryOperator"; _ } as t -> first_leaf t
            | _ -> None) children in
          (match operand, op_str with
           | Some o, Some "-" ->
               Option.map (fun v -> -v) (eval_const_expr ~params o)
           | Some o, Some "+" ->
               eval_const_expr ~params o
           | Some o, Some "~" ->
               Option.map lnot (eval_const_expr ~params o)
           | _ -> None)
      | "ConstantFunctionCall" ->
          (* `W` in `[W-1:0]` lands here as a 1-token "function call"
             with no arguments — really just a parameter reference. *)
          let id_node = find_first node
            ~name_is:(fun n -> n = "SimpleIdentifier") in
          (match Option.bind id_node first_leaf with
           | Some name -> List.assoc_opt name params
           | None -> None)
      | "SimpleIdentifier" ->
          (match first_leaf node with
           | Some name -> List.assoc_opt name params
           | None -> None)
      | "UnsignedNumber" | "DecimalNumber"
      | "IntegralNumber" | "Number"
      | "PrimaryLiteral" ->
          (match first_leaf node with
           | Some s -> int_of_token s
           | None -> None)
      | _ ->
          (* Unwrap single-child wrapper nodes (ConstantExpression,
             ConstantPrimary, etc.) by recursing. *)
          (match children with
           | [single] -> eval_const_expr ~params single
           | _ ->
               (* Multi-child unknown — give up. *)
               None)

(* Extract (msb, lsb) from a PackedDimension subtree.  Shape:
 *   PackedDimension
 *     PackedDimensionRange
 *       Symbol '['
 *       ConstantRange
 *         ConstantExpression (msb)
 *         Symbol ':'
 *         ConstantExpression (lsb)
 *       Symbol ']'
 *
 * The ConstantRange's child ConstantExpressions are the msb and
 * lsb, in source order; the Symbol leaves between them are noise
 * for us. *)
let extract_packed_range ~params pdim =
  let cr = find_first pdim ~name_is:(fun n -> n = "ConstantRange") in
  match cr with
  | None -> None
  | Some (Node { children; _ }) ->
      let exprs = List.filter_map (fun c ->
        match c with
        | Node { name = "ConstantExpression"; _ } as t -> Some t
        | _ -> None) children
      in
      (match exprs with
       | msb_e :: lsb_e :: _ ->
           (match eval_const_expr ~params msb_e,
                  eval_const_expr ~params lsb_e with
            | Some m, Some l -> Some (m, l)
            | _ -> None)
       | _ -> None)
  | _ -> None

(* Extract direction (`Input | `Output | `Internal) from a NetPortHeader
 * subtree by looking for a PortDirection child whose keyword token
 * is input/output/inout.  Returns None when direction is inherited
 * (subsequent comma-listed port in `input a, b, c;`).  Defaults to
 * the caller's "inherited" value in extract_port_decls below. *)
let extract_direction header =
  let pd = find_first header
    ~name_is:(fun n -> n = "PortDirection") in
  match pd with
  | None -> None
  | Some node ->
      if has_keyword_token node "input"  then Some `Input
      else if has_keyword_token node "output" then Some `Output
      else if has_keyword_token node "inout"  then Some `Internal
      else None

(* Extract bit width from a NetPortType subtree.  Walks for a
 * PackedDimension; if absent the port is implicitly 1-bit. *)
let extract_port_width ~params header =
  match find_first header ~name_is:(fun n -> n = "PackedDimension") with
  | None -> 1
  | Some pdim ->
      (match extract_packed_range ~params pdim with
       | Some (m, l) -> abs (m - l) + 1
       | None -> 1)

let extract_port_signed header =
  has_keyword_token header "signed"

(* Pull the ports out of a ListOfPortDeclarations subtree.  Each
 * AnsiPortDeclaration child contributes one port; comma Symbols
 * between them are ignored.  Direction inheritance handles the
 * shared-decl case (`input a, b, c;` — only `a` has an explicit
 * direction; `b` and `c` reuse the most-recent one).
 *
 * The signedness state is fresh per port: SV doesn't inherit
 * signedness across separate AnsiPortDeclaration entries, only
 * within a single comma-separated identifier list under one decl.
 *)
let extract_port_decls ~params plist : bsignal list =
  let acc = ref [] in
  let cur_dir = ref `Internal in
  let cur_width = ref 1 in
  let has_own_type port_node =
    (* `AnsiPortDeclarationNet` and `AnsiPortDeclarationVariable`
       distinguish "this port has its own direction/type" from
       "this port inherits from the previous one in a comma list".
       sv-parser marks bare-identifier ports as Variable. *)
    match find_first port_node
            ~name_is:(fun n ->
              n = "AnsiPortDeclarationNet"
              || n = "AnsiPortDeclarationVariable") with
    | Some (Node { name = "AnsiPortDeclarationNet"; _ }) -> true
    | _ -> false
  in
  let handle_port port_node =
    let dir = match extract_direction port_node with
      | Some d -> cur_dir := d; d
      | None   -> !cur_dir in
    if has_own_type port_node then begin
      (* Fresh declaration: width is whatever this port specifies,
         defaulting to 1 when no PackedDimension is present.  The
         previous port's width must NOT leak across. *)
      cur_width := extract_port_width ~params port_node
    end;
    let port_signed = extract_port_signed port_node in
    let port_id = find_first port_node
      ~name_is:(fun n -> n = "PortIdentifier") in
    (match Option.bind port_id identifier_name with
     | Some name ->
         let signed = if port_signed then Signed else Unsigned in
         acc := {
           name;
           stype = BInt { width = !cur_width; signed };
           direction = dir;
           initial_value = None;
           attrs = [];
         } :: !acc
     | None -> ())
  in
  walk plist ~f:(fun n ->
    match n with
    | Node { name = "AnsiPortDeclaration"; _ } -> handle_port n
    | _ -> ());
  List.rev !acc

(* Pull the parameter declarations out of a ParameterPortList
 * subtree (the `#(parameter int W = 8, ...)` block).  Each
 * ParamAssignment has a ParameterIdentifier and a
 * ConstantParamExpression we evaluate to an int.  Returned as a
 * (name, value) assoc-list so eval_const_expr can substitute uses
 * inside subsequent port packed dimensions. *)
let extract_param_decls plist : (string * int) list =
  let acc = ref [] in
  walk plist ~f:(fun n ->
    match n with
    | Node { name = "ParamAssignment"; _ } ->
        let id_node = find_first n
          ~name_is:(fun s -> s = "ParameterIdentifier") in
        let nm = Option.bind id_node identifier_name in
        let val_node = find_first n
          ~name_is:(fun s -> s = "ConstantParamExpression") in
        let v = match val_node with
          | Some t -> eval_const_expr ~params:!acc t
          | None -> None
        in
        (match nm, v with
         | Some n, Some v -> acc := (n, v) :: !acc
         | _ -> ())
    | _ -> ());
  List.rev !acc

(* --------------------------------------------------------------- *)
(* Expression converter — CST `Expression` → bexpr                  *)
(* --------------------------------------------------------------- *)

(* Map a BinaryOperator's string-literal to the BIR op constructor.
 * Returns None for operators we don't model yet (e.g. case-equality,
 * wildcard compare) — the caller silently drops to a placeholder so
 * a complex expression with one unsupported subop still emits a
 * structurally-sensible IR for the rest. *)
let binop_of_string = function
  | "+"  -> Some BAdd  | "-"  -> Some BSub
  | "*"  -> Some BMul  | "/"  -> Some BDiv  | "%" -> Some BMod
  | "&"  -> Some BAnd  | "|"  -> Some BOr   | "^" -> Some BXor
  | "<<" -> Some BShl  | ">>" -> Some BShr  | ">>>" -> Some BAshr
  | "==" -> Some BEq   | "!=" -> Some BNe
  | "<"  -> Some BLt   | "<=" -> Some BLe
  | ">"  -> Some BGt   | ">=" -> Some BGe
  | "&&" -> Some BAnd  | "||" -> Some BOr
  | _ -> None

let unop_of_string = function
  | "-"  -> Some BNeg
  | "~"  -> Some BNot
  | "!"  -> Some BNot
  | "&"  -> Some BRedAnd
  | "|"  -> Some BRedOr
  | "^"  -> Some BRedXor
  | _ -> None

let dummy_t = BInt { width = 1; signed = Unsigned }

(* Parse a sized literal like "8'd5" / "16'hAB" / "1'b0" into
 * (value, width). The token text comes from the IntegralNumber
 * leaf when it's a based form; the lexer-level breakup happens in
 * sv-parser before us. *)
let parse_sized_literal s =
  match String.index_opt s '\'' with
  | None ->
      (* Unsized decimal. *)
      (try Some (int_of_string s, 32) with _ -> None)
  | Some q ->
      let width_str = String.sub s 0 q in
      let rest = String.sub s (q + 1) (String.length s - q - 1) in
      let width = try Some (int_of_string width_str)
                  with _ -> None in
      let rest = String.trim rest in
      if rest = "" then None
      else
        let signed_skip =
          if rest.[0] = 's' || rest.[0] = 'S' then 1 else 0 in
        let base_idx = signed_skip in
        if String.length rest <= base_idx then None
        else
          let base = Char.lowercase_ascii rest.[base_idx] in
          let digits = String.sub rest (base_idx + 1)
                         (String.length rest - base_idx - 1) in
          let digits = String.concat "" (String.split_on_char '_' digits) in
          let digits = String.map (function
            | 'x' | 'X' | 'z' | 'Z' | '?' -> '0'
            | c -> c) digits in
          let prefix = match base with
            | 'b' -> "0b" | 'o' -> "0o" | 'd' -> ""
            | 'h' -> "0x" | _ -> ""
          in
          let value = try Some (int_of_string (prefix ^ digits))
                      with _ -> None in
          match value, width with
          | Some v, Some w -> Some (v, w)
          | _ -> None

(* Walk a Number-subtree to extract the literal value + width.
 * Recognises:
 *   Number → IntegralNumber → DecimalNumber → UnsignedNumber → leaf
 *   Number → IntegralNumber → BinaryNumber  → "<w>'b<digits>"
 *   Number → IntegralNumber → HexNumber     → "<w>'h<digits>"
 *   Number → IntegralNumber → OctalNumber   → "<w>'o<digits>"
 * The "<w>'b…" forms arrive with the size+base+digits as a single
 * leaf token from sv-parser. *)
let rec parse_number_node node =
  match node with
  | Leaf { value; _ } ->
      (* Bare leaf — try as sized or decimal. *)
      parse_sized_literal value
  | Node { name = "UnsignedNumber"; _ } ->
      (match first_leaf node with
       | Some s -> (try Some (int_of_string s, 32) with _ -> None)
       | None -> None)
  | Node { name = ("BinaryNumber" | "OctalNumber" | "HexNumber"
                  | "DecimalNumber" | "IntegralNumber" | "Number");
           children } ->
      (* The literal as a whole may be glued into one leaf (e.g.
         "8'd5" as a single token) or split across several leaves
         (size + base + digits). Concatenate leaves and parse the
         result. *)
      let joined =
        let buf = Buffer.create 16 in
        walk node ~f:(function
          | Leaf { value; _ } -> Buffer.add_string buf value
          | _ -> ());
        Buffer.contents buf
      in
      (match parse_sized_literal joined with
       | Some r -> Some r
       | None ->
           (* Fallback: take the first child. *)
           (match children with
            | c :: _ -> parse_number_node c
            | [] -> None))
  | Node { children; _ } ->
      (* Unwrap wrapper nodes. *)
      (match children with
       | [c] -> parse_number_node c
       | _ -> None)

(* Convert one CST Expression node to a BIR bexpr.  Drops to a
 * 1-bit zero placeholder for shapes we don't model yet so the
 * surrounding expression still has a tree. *)
let rec expr_to_bexpr ~params node =
  let zero = BConst { value = 0; width = 1 } in
  let recurse = expr_to_bexpr ~params in
  match node with
  | Leaf _ -> zero
  | Node { name; children } ->
      match name with
      | "Expression" | "MintypmaxExpression"
      | "ExpressionOrCondPattern"
      | "ConditionalExpression" ->
          (match children with
           | [c] -> recurse c
           | _ ->
               (* Multi-child: typically Expression ? Expression : Expression
                  (conditional). Look for "Expression" children, "?", ":". *)
               let exprs = List.filter (function
                 | Node { name = "Expression"; _ } -> true
                 | _ -> false) children in
               (match exprs with
                | [cond; t; e] ->
                    BCond {
                      condition = recurse cond;
                      then_val  = recurse t;
                      else_val  = recurse e }
                | [single] -> recurse single
                | _ -> zero))
      | "ExpressionBinary" ->
          let exprs = List.filter (function
            | Node { name = "Expression"; _ } -> true
            | _ -> false) children in
          let op_str = List.find_map (function
            | Node { name = "BinaryOperator"; _ } as t -> first_leaf t
            | _ -> None) children in
          (match exprs, Option.bind op_str binop_of_string with
           | [a; b], Some op ->
               BBinOp {
                 op;
                 lhs = recurse a;
                 rhs = recurse b;
                 result_type = dummy_t }
           | _ -> zero)
      | "ExpressionUnary"
      | "UnaryExpression" ->
          let operand = List.find_opt (function
            | Node { name = "Primary"; _ }
            | Node { name = "Expression"; _ } -> true
            | _ -> false) children in
          let op_str = List.find_map (function
            | Node { name = "UnaryOperator"; _ } as t -> first_leaf t
            | _ -> None) children in
          (match operand, Option.bind op_str unop_of_string with
           | Some o, Some op ->
               BUnOp { op; operand = recurse o; result_type = dummy_t }
           | Some o, None -> recurse o
           | None, _ -> zero)
      | "Primary" ->
          (match children with
           | [c] -> recurse c
           | _ -> zero)
      | "PrimaryLiteral" ->
          (* Number / StringLiteral / TimeLiteral. *)
          (match children with
           | [Node { name = "Number"; _ } as n] ->
               (match parse_number_node n with
                | Some (v, w) -> BConst { value = v; width = w }
                | None -> zero)
           | [Node { name = "StringLiteral"; _ }] ->
               (* String literals don't have a meaningful bit value
                  for synthesis — emit a placeholder. *)
               zero
           | _ -> zero)
      | "Number" ->
          (match parse_number_node node with
           | Some (v, w) -> BConst { value = v; width = w }
           | None -> zero)
      | "PrimaryMintypmaxExpression" ->
          (* `( <expr> )`.  The inner Expression is in the
             MintypmaxExpression child; recurse straight through. *)
          let inner = find_first node
            ~name_is:(fun n -> n = "Expression") in
          (match inner with
           | Some e -> recurse e
           | None -> zero)
      | "PrimaryHierarchical" ->
          (* `name[idx]` / `name[hi:lo]` / `name`. *)
          let id_name = identifier_name node in
          let select_node = find_first node
            ~name_is:(fun n -> n = "Select") in
          (match id_name with
           | None -> zero
           | Some n ->
               let base = BVar n in
               (match select_node with
                | None -> base
                | Some s -> apply_select ~params base s))
      | "PrimaryConcatenation" ->
          let concat = find_first node
            ~name_is:(fun n -> n = "Concatenation") in
          (match concat with
           | Some (Node { children; _ }) ->
               let parts = List.filter_map (function
                 | Node { name = "Expression"; _ } as t -> Some (recurse t)
                 | _ -> None) children in
               (match parts with
                | [] -> zero
                | _ -> BConcat parts)
           | _ -> zero)
      | "PrimaryMultipleConcatenation" ->
          (* `{N{value}}` — outer concat with one inner expr_list. *)
          let nodes_named name =
            List.filter_map (function
              | Node { name = n; _ } as t when n = name -> Some t
              | _ -> None) children in
          let exprs = nodes_named "Expression" in
          (match exprs with
           | [count_e; v] ->
               let count = match eval_const_expr ~params count_e with
                 | Some n -> n | None -> 1 in
               BReplicate { count; value = recurse v }
           | _ -> zero)
      | _ ->
          (* Unwrap single-child wrapper. *)
          (match children with
           | [c] -> recurse c
           | _ -> zero)

(* Apply a Select subtree to a base bexpr (BVar of a signal). Supports:
 *   ConstantBitSelect → [<const>]  — single bit
 *   BitSelect         → [<expr>]   — dynamic bit-select; lower to
 *                       (base >> expr) & 1 to match Verilator's IR
 *   ConstantPartSelectRange → [hi:lo] — packed range slice
 *   PartSelectRange         → [base +: w] / [base -: w]
 *
 * Anything else falls through and returns the unsliced base. *)
and apply_select ~params base sel =
  let recurse_e = expr_to_bexpr ~params in
  (* Walk the select subtree.  The Select node may carry multiple
     bit/part selects in sequence; we apply them in order. *)
  let rec apply current = function
    | Leaf _ -> current
    | Node { name; children } ->
        match name with
        | "ConstantBitSelect" | "BitSelect" ->
            (* [idx] — try const, else dynamic shift+mask. *)
            let idx = find_first (Node { name; children })
              ~name_is:(fun n -> n = "Expression"
                              || n = "ConstantExpression") in
            (match idx with
             | None -> current
             | Some i ->
                 (match eval_const_expr ~params i with
                  | Some k -> BSlice { signal = current; msb = k; lsb = k }
                  | None ->
                      let one = BConst { value = 1; width = 1 } in
                      let res_t = BInt { width = 1; signed = Unsigned } in
                      let shifted = BBinOp {
                        op = BShr;
                        lhs = current;
                        rhs = recurse_e i;
                        result_type = res_t } in
                      BBinOp { op = BAnd; lhs = shifted; rhs = one;
                               result_type = res_t }))
        | "ConstantPartSelectRange" | "PartSelectRange" ->
            let ranges = List.filter_map (function
              | Node { name = ("ConstantRange" | "ConstantRangeExpression"
                              | "PartSelectRange"); _ } as t -> Some t
              | _ -> None) children in
            (match ranges with
             | [] ->
                 (* Maybe the child is directly a ConstantRange or
                    ConstantIndexedPartSelectRange. *)
                 List.fold_left apply current children
             | r :: _ -> apply current r)
        | "ConstantRange" ->
            (* hi : lo *)
            let exprs = List.filter_map (function
              | Node { name = "ConstantExpression"; _ } as t -> Some t
              | _ -> None) children in
            (match exprs with
             | [hi_e; lo_e] ->
                 (match eval_const_expr ~params hi_e,
                        eval_const_expr ~params lo_e with
                  | Some hi, Some lo ->
                      BSlice { signal = current; msb = hi; lsb = lo }
                  | _ -> current)
             | _ -> current)
        | _ ->
            List.fold_left apply current children
  in
  apply base sel

(* ---------------------------------------------------------------- *)
(* Operator-precedence rebalancing.                                   *)
(*                                                                    *)
(* sv-parser's CST builds binary expressions right-associatively with *)
(* no precedence awareness — `a * b + c` arrives as a BBinOp tree     *)
(* whose root is the leftmost operator, which means `a * (b + c)`     *)
(* rather than the SV-correct `(a * b) + c`. Walk the tree and        *)
(* left-rotate whenever the outer op binds at least as tightly as     *)
(* the inner op on the RHS:                                           *)
(*    outer(L, inner(M, R))   becomes   inner(outer(L, M), R)         *)
(* This fixes both precedence and left-associativity at same prec.    *)
(* ---------------------------------------------------------------- *)

let bin_prec = function
  | BMul | BDiv | BMod -> 1
  | BAdd | BSub -> 2
  | BShl | BShr | BAshr -> 3
  | BLt | BLe | BGt | BGe -> 4
  | BEq | BNe -> 5
  | BAnd -> 6
  | BXor -> 7
  | BOr  -> 8

let rec rebalance = function
  | BBinOp { op = outer; lhs; rhs =
        BBinOp { op = inner; lhs = mid; rhs; result_type = rt_in };
                  result_type = rt_out }
    when bin_prec outer <= bin_prec inner ->
      let new_lhs = rebalance (BBinOp {
        op = outer;
        lhs = rebalance lhs;
        rhs = rebalance mid;
        result_type = rt_out;
      }) in
      rebalance (BBinOp {
        op = inner;
        lhs = new_lhs;
        rhs = rebalance rhs;
        result_type = rt_in;
      })
  | BBinOp r ->
      BBinOp { r with lhs = rebalance r.lhs; rhs = rebalance r.rhs }
  | BUnOp r ->
      BUnOp { r with operand = rebalance r.operand }
  | BSlice r ->
      BSlice { r with signal = rebalance r.signal }
  | BConcat es -> BConcat (List.map rebalance es)
  | BReplicate r ->
      BReplicate { r with value = rebalance r.value }
  | BCond r ->
      BCond {
        condition = rebalance r.condition;
        then_val  = rebalance r.then_val;
        else_val  = rebalance r.else_val }
  | BSelect r ->
      BSelect {
        array = rebalance r.array;
        index = rebalance r.index }
  | BCall r ->
      BCall { r with args = List.map rebalance r.args }
  | (BVar _ | BConst _) as e -> e

(* Pull a continuous-assign target name out of a NetLvalue subtree.
 * Handles the simple `name = ...` and `name[range] = ...` forms;
 * for the latter we currently target the whole signal (slice
 * writes will need part-select-write IR support, deferred). *)
let lvalue_name node =
  let id = find_first node
    ~name_is:(fun n -> n = "NetIdentifier"
                    || n = "VariableIdentifier"
                    || n = "HierarchicalIdentifier") in
  Option.bind id identifier_name

(* Extract `assign lhs = rhs;` items into BCombinational processes —
 * one process per assign so downstream passes (ffrip, share, …)
 * can treat them uniformly with always_comb assignments. *)
let extract_continuous_assigns ~params module_node =
  let assigns = find_all module_node
    ~name_is:(fun n -> n = "NetAssignment") in
  List.filter_map (fun assign ->
    let lhs_node = find_first assign
      ~name_is:(fun n -> n = "NetLvalue") in
    let rhs_node = find_first assign
      ~name_is:(fun n -> n = "Expression") in
    match lhs_node, rhs_node with
    | Some l, Some r ->
        (match lvalue_name l with
         | Some name ->
             let rhs = rebalance (expr_to_bexpr ~params r) in
             Some (BCombinational {
               name = "assign_" ^ name;
               sensitivity = [BAny];
               body = [BAssign { lhs = name; rhs }];
             })
         | None -> None)
    | _ -> None) assigns

(* Find every ModuleDeclaration in the tree.  sv-parser nests them
 * under SourceText → Description → ModuleDeclaration.  We don't
 * try to handle nested modules; the LRM forbids them anyway. *)
let module_nodes tree =
  find_all tree ~name_is:(fun n -> n = "ModuleDeclaration")

(* Build a bmodule from one ModuleDeclaration node.  Returns None if
 * the node doesn't carry a recognisable module name (defensive — we
 * shouldn't get here for malformed input that sv-parser would have
 * rejected upstream). *)
let module_of_node node : bmodule option =
  let name =
    let mid = find_first node
      ~name_is:(fun n -> n = "ModuleIdentifier") in
    Option.bind mid identifier_name
  in
  match name with
  | None -> None
  | Some name ->
      (* Parameters first, then ports (port packed dims may use
         parameter values). *)
      let ppl = find_first node
        ~name_is:(fun n -> n = "ParameterPortList") in
      let params = match ppl with
        | Some p -> extract_param_decls p
        | None -> []
      in
      let plist = find_first node
        ~name_is:(fun n -> n = "ListOfPortDeclarations") in
      let signals = match plist with
        | Some p -> extract_port_decls ~params p
        | None -> []
      in
      let processes = extract_continuous_assigns ~params node in
      Some {
        name;
        params;
        signals;
        processes;
        instances = [];
        funcs = [];
        mems = [];
        attrs = [];
      }

let convert_tree (t : Sv_parser_dump.t) : bprogram =
  let mods = List.filter_map module_of_node (module_nodes t) in
  { modules = mods; library_cells = [] }

(* Convenience: parse a file via sv_parser_dump and convert. *)
let convert_file ?bin ?(incdirs = []) ?(defines = []) file =
  match Sv_parser_dump.parse_file ?bin ~incdirs ~defines file with
  | Error e -> Error e
  | Ok tree -> Ok (convert_tree tree)
