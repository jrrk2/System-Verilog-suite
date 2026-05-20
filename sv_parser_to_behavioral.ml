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
      | "PrimaryHierarchical" ->
          (* Bare identifier reference appearing inside an Expression
             (not a ConstantExpression).  Only constant-foldable when
             the Select subtree carries no actual bit/part selectors
             — otherwise the value is a runtime indexed lookup, which
             we can't fold. *)
          let id_node = find_first node
            ~name_is:(fun n -> n = "SimpleIdentifier") in
          let has_real_select =
            match find_first node ~name_is:(fun n -> n = "Select") with
            | Some s ->
                let inner = find_first s
                  ~name_is:(fun n -> n = "BitSelect"
                                  || n = "PartSelectRange"
                                  || n = "ConstantBitSelect"
                                  || n = "ConstantPartSelectRange") in
                (match inner with
                 | Some (Node { children; _ }) -> children <> []
                 | _ -> false)
            | None -> false
          in
          if has_real_select then None
          else
            (match Option.bind id_node first_leaf with
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
               (* ConditionalExpression in sv-parser's CST is laid out
                  as: CondPredicate, '?', Expression, ':', Expression.
                  The condition lives in CondPredicate (which itself
                  wraps an ExpressionOrCondPattern→Expression), NOT in
                  a top-level "Expression" child.  Pick it explicitly. *)
               let cond_node = List.find_opt (function
                 | Node { name = ("CondPredicate"
                               | "ExpressionOrCondPattern"); _ } -> true
                 | _ -> false) children in
               let exprs = List.filter (function
                 | Node { name = "Expression"; _ } -> true
                 | _ -> false) children in
               (match cond_node, exprs with
                | Some c, [t; e] ->
                    BCond {
                      condition = recurse c;
                      then_val  = recurse t;
                      else_val  = recurse e }
                | None, [cond; t; e] ->
                    BCond {
                      condition = recurse cond;
                      then_val  = recurse t;
                      else_val  = recurse e }
                | _, [single] -> recurse single
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
      | "SystemTfCallArgExpression" | "SystemTfCall"
      | "FunctionSubroutineCall" | "SubroutineCall" ->
          (* `$name(arg, ...)` system task call.  SystemTfCall has a
             single SystemTfCallArgExpression child; the identifier
             and arguments live there.  Walk the subtree to find the
             identifier and the first inner Expression — that's the
             argument for cast-style ($signed/$unsigned/$clog2) calls.
             For sign casts we wrap in a @signed marker BCall; for
             $unsigned we just recurse (our IR is width-typed without
             explicit signedness on expressions, so $unsigned is a
             no-op at this level). *)
          let self = Node { name; children } in
          let id_name =
            Option.bind
              (find_first self ~name_is:(fun n -> n = "SystemTfIdentifier"))
              first_leaf
          in
          let inner = find_first self ~name_is:(fun n -> n = "Expression") in
          (match id_name, inner with
           | Some "$signed", Some e ->
               BCall { func = "@signed"; args = [recurse e] }
           | Some "$unsigned", Some e ->
               recurse e
           | _, Some e ->
               (* Other tasks: recurse so something flows downstream,
                  but downstream may not model the semantics. *)
               recurse e
           | _, None -> zero)
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

(* ---------------------------------------------------------------- *)
(* Statement converter — CST Statement → bstmt                       *)
(* ---------------------------------------------------------------- *)

(* Pull a variable/net lvalue target name. Same shape across
 * NetLvalue and VariableLvalue subtrees — both descend through a
 * HierarchicalIdentifier or NetIdentifier wrapper to a
 * SimpleIdentifier leaf. Doesn't yet support concat-LHS or
 * struct-field LHS; those are a follow-up. *)
let extract_lvalue_name node =
  let id = find_first node
    ~name_is:(fun n -> n = "NetIdentifier"
                    || n = "VariableIdentifier"
                    || n = "HierarchicalIdentifier") in
  Option.bind id identifier_name

(* Pull a (msb, lsb) pair out of an Lvalue's Select subtree.  Only
 * succeeds for *constant* bit/part-selects (variable indices fall
 * back to whole-signal write — semantically wrong, but the same
 * limitation as the Verible frontend's dynamic LHS path).  Returns
 * None when the Lvalue is a plain identifier (no bit/part select). *)
let extract_lvalue_const_slice ~params node =
  let sel = find_first node ~name_is:(fun n -> n = "Select") in
  match sel with
  | None -> None
  | Some s ->
      (* Two shapes: ConstantBitSelect [k] or ConstantPartSelectRange [hi:lo]
         — also the unconstrained-name "BitSelect" (empty child list) shows
         up when there's no actual select. *)
      let bit = find_first s ~name_is:(fun n -> n = "ConstantBitSelect"
                                            || n = "BitSelect") in
      let range = find_first s ~name_is:(fun n -> n = "ConstantPartSelectRange"
                                             || n = "PartSelectRange") in
      (match bit, range with
       | Some (Node { children = []; _ }), None -> None
       | Some b, _ ->
           (* Single bit: [k] *)
           let e = find_first b ~name_is:(fun n -> n = "Expression"
                                              || n = "ConstantExpression") in
           (match Option.bind e (eval_const_expr ~params) with
            | Some k -> Some (k, k)
            | None -> None)
       | None, Some r ->
           let exprs = match r with
             | Node { children; _ } ->
                 List.filter_map (function
                   | Node { name = ("Expression" | "ConstantExpression"); _ } as t ->
                       Some t
                   | _ -> None) children
             | _ -> []
           in
           (match exprs with
            | [hi_e; lo_e] ->
                (match eval_const_expr ~params hi_e,
                       eval_const_expr ~params lo_e with
                 | Some hi, Some lo -> Some (hi, lo)
                 | _ -> None)
            | _ -> None)
       | _ -> None)

(* Convert a CST Statement subtree to a bstmt. Unsupported shapes
 * collapse to BBlock [] so the surrounding always-body keeps its
 * structure but the unknown branch drops to a no-op.  Recognised
 * shapes (the ones that show up in synthesisable RTL):
 *   - SeqBlock          → BBlock
 *   - ConditionalStatement (if/else) → BIf
 *   - CaseStatement     → BCase
 *   - NonblockingAssignment → BAssign  (treated as = for the BIR;
 *     ffrip + share recover the NBA semantics later)
 *   - BlockingAssignment    → BAssign
 *   - LoopStatement (for/while) → BFor / BWhile *)
let rec stmt_to_bstmt ~params node =
  match node with
  | Leaf _ -> BBlock []
  | Node { name; children } ->
      match name with
      | "Statement" | "StatementOrNull" | "StatementItem"
      | "FunctionStatementOrNull" | "FunctionStatement"
      | "SubroutineCallStatement" ->
          (* Single-child wrappers — recurse.  When there's more
             than one child (StatementItem usually has the actual
             statement + a trailing `;` Symbol), pick the first
             child that names a statement form. *)
          (match children with
           | [c] -> stmt_to_bstmt ~params c
           | _ ->
               let is_stmt_kind = function
                 | Node { name =
                       ("SeqBlock" | "ConditionalStatement"
                      | "CaseStatement"
                      | "NonblockingAssignment" | "BlockingAssignment"
                      | "ProceduralTimingControlStatement"
                      | "LoopStatement"
                      | "SubroutineCallStatement"
                      | "StatementItem" | "Statement" | "StatementOrNull"
                      | "FunctionStatement" | "FunctionStatementOrNull"
                      ); _ } -> true
                 | _ -> false
               in
               (match List.find_opt is_stmt_kind children with
                | Some s -> stmt_to_bstmt ~params s
                | None -> BBlock []))
      | "ProceduralTimingControlStatement" ->
          (* `@(...) <stmt>` or `#<delay> <stmt>` — drop the timing
             control wrapper, recurse into the StatementOrNull child
             which carries the real body.  The always-block extractor
             reads the event control separately for clocking info. *)
          let body = List.find_opt (function
            | Node { name = "StatementOrNull"; _ } -> true
            | _ -> false) children in
          (match body with
           | Some s -> stmt_to_bstmt ~params s
           | None -> BBlock [])
      | "SeqBlock" ->
          let stmts = List.filter_map (function
            | Node { name = "StatementOrNull"; _ } as t ->
                Some (stmt_to_bstmt ~params t)
            | _ -> None) children in
          BBlock stmts
      | "ConditionalStatement" ->
          (* sv-parser flattens `if ... else if ... else ...` chains
             into a single ConditionalStatement whose children are
             alternating Keyword('if')/Symbol('(')/CondPredicate/
             Symbol(')')/StatementOrNull/Keyword('else') ... pairs.
             Walk the child list and collect (predicate, then-body)
             pairs plus an optional trailing else-body, then build a
             right-nested BIf cascade. *)
          let cond_expr_of pred =
            let inner = find_first pred
              ~name_is:(fun n -> n = "Expression") in
            match inner with
            | Some e -> rebalance (expr_to_bexpr ~params e)
            | None -> BConst { value = 1; width = 1 }
          in
          let is_pred = function
            | Node { name = ("CondPredicate" | "ExpressionOrCondPattern"); _ } ->
                true
            | _ -> false
          in
          let rec collect acc cur_pred = function
            | [] -> (List.rev acc, None)
            | n :: rest when is_pred n ->
                collect acc (Some n) rest
            | (Node { name = "StatementOrNull"; _ } as s) :: rest ->
                (match cur_pred with
                 | Some p ->
                     collect ((p, s) :: acc) None rest
                 | None ->
                     (* trailing else body *)
                     (List.rev acc, Some s))
            | _ :: rest -> collect acc cur_pred rest
          in
          let pairs, final_else = collect [] None children in
          let final_else_stmts = match final_else with
            | Some s -> [stmt_to_bstmt ~params s]
            | None -> []
          in
          (* Build right-nested BIf: pair_1 ? ... : (pair_2 ? ... : final_else) *)
          let rec build = function
            | [] -> final_else_stmts
            | (p, t) :: rest ->
                [BIf {
                   condition = cond_expr_of p;
                   then_stmts = [stmt_to_bstmt ~params t];
                   else_stmts = build rest;
                 }]
          in
          (match build pairs with
           | [s] -> s
           | ss -> BBlock ss)
      | "CaseStatement" ->
          (match children with
           | [Node { name = "CaseStatementNormal"; children = c2; _ }] ->
               extract_case ~params c2
           | _ -> BBlock [])
      | "CaseStatementNormal" ->
          extract_case ~params children
      | "NonblockingAssignment" | "BlockingAssignment" ->
          (* CST shape differs by kind:
               BlockingAssignment    → [OperatorAssignment]
               OperatorAssignment    → VariableLvalue '=' Expression
               NonblockingAssignment → VariableLvalue '<=' Expression
             In both cases the LHS+RHS live as *direct* children of one
             specific level — picking them via a recursive `find_first`
             on the outer node would mis-land on an Expression nested
             inside the LHS bit-select (e.g. `a[i] <= e` puts `i` in
             pre-order before `e`).  So drill to the right level first
             and then select direct children there. *)
          let pick_level () =
            (* For BlockingAssignment, unwrap a single OperatorAssignment
               child; for NonblockingAssignment, use the node itself. *)
            match children with
            | [Node { name = "OperatorAssignment"; children = oc }] -> oc
            | _ -> children
          in
          let level = pick_level () in
          let direct kind = List.find_opt (function
            | Node { name; _ } -> name = kind
            | _ -> false) level in
          let lhs_node = match direct "VariableLvalue", direct "NetLvalue" with
            | Some t, _ -> Some t
            | _, Some t -> Some t
            | _ -> None
          in
          let rhs_node = direct "Expression" in
          (match Option.bind lhs_node extract_lvalue_name, rhs_node with
           | Some name, Some r ->
               let rhs_e = rebalance (expr_to_bexpr ~params r) in
               (* If the LHS has a constant bit/part-select, emit a
                  @slice_write so behavioral_mem_merge's RMW lowering
                  reads/writes only the addressed slice instead of
                  clobbering the whole signal. *)
               (match Option.bind lhs_node
                        (extract_lvalue_const_slice ~params) with
                | Some (hi, lo) ->
                    BCallStmt {
                      func = "@slice_write";
                      args = [BVar name;
                              BConst { value = hi; width = 32 };
                              BConst { value = lo; width = 32 };
                              rhs_e] }
                | None -> BAssign { lhs = name; rhs = rhs_e })
           | _ -> BBlock [])
      | "LoopStatement" ->
          extract_loop ~params children
      | _ ->
          (* Try descending into a single statement-bearing child. *)
          let inner = List.find_opt (function
            | Node { name = ("SeqBlock" | "ConditionalStatement"
                            | "CaseStatement" | "NonblockingAssignment"
                            | "BlockingAssignment" | "LoopStatement"); _ } ->
                true
            | _ -> false) children in
          match inner with
          | Some s -> stmt_to_bstmt ~params s
          | None -> BBlock []

and extract_case ~params children =
  (* CaseStatementNormal children:
       Keyword 'case' Symbol '(' CaseExpression Symbol ')'
       CaseItem* Keyword 'endcase'
     where CaseExpression wraps the Expression we want.  Try direct
     Expression first (some grammar variants put it directly); fall
     back to the CaseExpression's nested Expression. *)
  let selector =
    let ce_or_expr = List.find_opt (function
      | Node { name = ("Expression" | "CaseExpression"); _ } -> true
      | _ -> false) children
    in
    let expr_node = match ce_or_expr with
      | Some (Node { name = "Expression"; _ } as t) -> Some t
      | Some (Node { name = "CaseExpression"; _ } as t) ->
          find_first t ~name_is:(fun n -> n = "Expression")
      | _ -> None
    in
    match expr_node with
    | Some t -> rebalance (expr_to_bexpr ~params t)
    | None -> BConst { value = 0; width = 1 }
  in
  let items = List.filter (function
    | Node { name = "CaseItem"; _ } -> true
    | _ -> false) children in
  let cases, default = List.fold_left (fun (cs, def) it ->
    match it with
    | Node { name = "CaseItem"; children = c } ->
        let kind = List.find_opt (function
          | Node { name = ("CaseItemNondefault" | "CaseItemDefault"); _ } -> true
          | _ -> false) c
        in
        (match kind with
         | Some (Node { name = "CaseItemNondefault"; children = cc; _ }) ->
             let case_exprs = List.filter_map (function
               | Node { name = "CaseItemExpression"; _ } as t ->
                   let inner = find_first t
                     ~name_is:(fun n -> n = "Expression") in
                   Option.map (fun e ->
                     rebalance (expr_to_bexpr ~params e)) inner
               | _ -> None) cc in
             let body = List.find_opt (function
               | Node { name = "StatementOrNull"; _ } -> true
               | _ -> false) cc in
             let body_stmts = match body with
               | Some s -> [stmt_to_bstmt ~params s]
               | None -> []
             in
             let entries = List.map (fun ce -> (ce, body_stmts)) case_exprs in
             (cs @ entries, def)
         | Some (Node { name = "CaseItemDefault"; children = cc; _ }) ->
             let body = List.find_opt (function
               | Node { name = "StatementOrNull"; _ } -> true
               | _ -> false) cc in
             let body_stmts = match body with
               | Some s -> [stmt_to_bstmt ~params s]
               | None -> []
             in
             (cs, def @ body_stmts)
         | _ -> (cs, def))
    | _ -> (cs, def)) ([], []) items
  in
  BCase { selector; cases; default }

and extract_loop ~params children =
  (* LoopStatement children include the loop keyword and body.
     For ForStatement: 'for' '(' for_init ';' expr ';' for_step ')' stmt.
     For WhileStatement: 'while' '(' expr ')' stmt.
     The CST tags differ by sub-node; we dispatch on the first
     statement-shaped wrapper present. *)
  let kw = List.find_map (function
    | Node { name = "Keyword"; _ } as t -> first_leaf t
    | _ -> None) children in
  match kw with
  | Some "for" ->
      (* Initialise: BAssign from ForInitialization → ListOfVariableAssignments.
         Condition: Expression.  Update: ForStep → ForStepAssignment.
         Body: the trailing StatementOrNull. *)
      let init_node = List.find_opt (function
        | Node { name = "ForInitialization"; _ } -> true
        | _ -> false) children in
      let cond_node = List.find_opt (function
        | Node { name = "Expression"; _ } -> true
        | _ -> false) children in
      let step_node = List.find_opt (function
        | Node { name = "ForStep"; _ } -> true
        | _ -> false) children in
      let body_node = List.find_opt (function
        | Node { name = "StatementOrNull"; _ } -> true
        | _ -> false) children in
      let init_b = match init_node with
        | Some n ->
            (* Look for the first assignment inside the init. *)
            let v = find_first n
              ~name_is:(fun s -> s = "VariableAssignment"
                              || s = "ListOfVariableAssignments") in
            (match v with
             | Some va ->
                 let id = find_first va
                   ~name_is:(fun s -> s = "VariableIdentifier") in
                 let rhs = find_first va
                   ~name_is:(fun s -> s = "Expression") in
                 (match Option.bind id identifier_name, rhs with
                  | Some name, Some e ->
                      BAssign { lhs = name;
                                rhs = rebalance (expr_to_bexpr ~params e) }
                  | _ -> BBlock [])
             | None -> BBlock [])
        | None -> BBlock []
      in
      let cond_b = match cond_node with
        | Some e -> rebalance (expr_to_bexpr ~params e)
        | None -> BConst { value = 1; width = 1 }
      in
      let update_b = match step_node with
        | Some n ->
            (* ForStep wraps a ForStepAssignment (or
               OperatorAssignment / IncOrDecExpression). *)
            let id = find_first n
              ~name_is:(fun s -> s = "VariableIdentifier") in
            let rhs = find_first n
              ~name_is:(fun s -> s = "Expression") in
            (match Option.bind id identifier_name, rhs with
             | Some name, Some e ->
                 BAssign { lhs = name;
                           rhs = rebalance (expr_to_bexpr ~params e) }
             | _ -> BBlock [])
        | None -> BBlock []
      in
      let body_stmts = match body_node with
        | Some s -> [stmt_to_bstmt ~params s]
        | None -> []
      in
      BFor { init = init_b; condition = cond_b;
             update = update_b; body = body_stmts }
  | Some "while" ->
      let cond_node = List.find_opt (function
        | Node { name = "Expression"; _ } -> true
        | _ -> false) children in
      let body_node = List.find_opt (function
        | Node { name = "StatementOrNull"; _ } -> true
        | _ -> false) children in
      let cond_b = match cond_node with
        | Some e -> rebalance (expr_to_bexpr ~params e)
        | None -> BConst { value = 1; width = 1 }
      in
      let body_stmts = match body_node with
        | Some s -> [stmt_to_bstmt ~params s]
        | None -> []
      in
      BWhile { condition = cond_b; body = body_stmts }
  | _ -> BBlock []

(* ---------------------------------------------------------------- *)
(* Always-block extractor — CST AlwaysConstruct → bprocess           *)
(* ---------------------------------------------------------------- *)

(* From an EventExpression subtree, collect (edge, signal_name)
 * pairs.  Order is source order; the caller picks a clock vs reset
 * by heuristic (clk-named signals or the first posedge are typical
 * conventions). *)
let extract_event_items node =
  let items = find_all node
    ~name_is:(fun n -> n = "EventExpressionExpression") in
  List.filter_map (fun it ->
    let edge = List.find_map (function
      | Node { name = "EdgeIdentifier"; _ } as t -> first_leaf t
      | _ -> None) (match it with Node { children; _ } -> children | _ -> []) in
    let sig_node = find_first it
      ~name_is:(fun n -> n = "Expression") in
    let sig_name = Option.bind sig_node (fun e ->
      let id = find_first e
        ~name_is:(fun n -> n = "SimpleIdentifier") in
      Option.bind id first_leaf) in
    match edge, sig_name with
    | Some e, Some n -> Some (e, n)
    | _ -> None) items

let always_kind kw_node =
  match kw_node with
  | Some "always_ff" -> `Seq
  | Some "always_comb" -> `Comb
  | Some "always_latch" -> `Comb  (* treat latch as comb for now *)
  | Some "always" -> `Plain
  | _ -> `Plain

(* Convert one AlwaysConstruct subtree to a bprocess. *)
let convert_always ~params node =
  let kw = List.find_map (function
    | Node { name = "AlwaysKeyword"; _ } as t -> first_leaf t
    | _ -> None) (match node with Node { children; _ } -> children | _ -> []) in
  let event_ctrl = find_first node
    ~name_is:(fun n -> n = "EventControl"
                    || n = "EventControlEventExpression") in
  let body_stmt = find_first node
    ~name_is:(fun n -> n = "Statement") in
  let body_stmt = match body_stmt with
    | Some s -> s
    | None -> Leaf { value = ""; line = 0 }
  in
  let body = stmt_to_bstmt ~params body_stmt in
  let body_stmts = match body with
    | BBlock ss -> ss
    | other -> [other]
  in
  let kind = always_kind kw in
  (* For plain `always`, the event control decides:
   *   - any posedge/negedge → sequential (FF)
   *   - level-sensitive only @ star or @(a or b) → combinational *)
  let plain_event_kind ec =
    let items = extract_event_items ec in
    if List.exists (fun (e, _) -> e = "posedge" || e = "negedge") items
    then `Seq else `Comb
  in
  let effective_kind = match kind, event_ctrl with
    | `Plain, Some ec -> plain_event_kind ec
    | _ -> kind
  in
  match effective_kind, event_ctrl with
  | `Seq, Some ec ->
      let items = extract_event_items ec in
      (* Pick clock = first posedge or negedge entry; if there's
         more than one edge, the rest are reset(s). *)
      (match items with
       | (clock_edge, clock_sig) :: rest ->
           let edge_of = function "posedge" -> `Pos | _ -> `Neg in
           let reset, reset_edge, reset_async =
             match rest with
             | (re, rn) :: _ -> Some rn, Some (edge_of re), true
             | [] -> None, None, false
           in
           BSequential {
             name = "always_ff";
             clock = clock_sig;
             clock_edge = edge_of clock_edge;
             reset;
             reset_edge;
             reset_async;
             body = body_stmts;
           }
       | [] ->
           BCombinational {
             name = "always_ff_no_edge";
             sensitivity = [BAny];
             body = body_stmts;
           })
  | (`Comb | `Plain), _ ->
      BCombinational {
        name = (match kw with
                | Some "always_comb" -> "always_comb"
                | Some "always_latch" -> "always_latch"
                | _ -> "always");
        sensitivity = [BAny];
        body = body_stmts;
      }
  | `Seq, None ->
      (* always_ff without an event control — pathological; treat
         as combinational so downstream doesn't trip. *)
      BCombinational {
        name = "always_ff_unclocked";
        sensitivity = [BAny];
        body = body_stmts;
      }

(* Pull all AlwaysConstruct subtrees out of a module body. *)
let extract_always_blocks ~params module_node =
  let nodes = find_all module_node
    ~name_is:(fun n -> n = "AlwaysConstruct") in
  List.map (convert_always ~params) nodes

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

(* ---------------------------------------------------------------- *)
(* Module-body declarations: localparam/parameter, signals, funcs    *)
(* ---------------------------------------------------------------- *)

(* Pick out body-level localparam / parameter declarations as
 * (name, value) pairs, ordered left-to-right so each entry can
 * reference earlier ones. *)
let extract_body_params ~init_params module_node =
  let nodes = find_all module_node
    ~name_is:(fun n ->
      n = "LocalParameterDeclaration"
      || n = "ParameterDeclaration") in
  let acc = ref init_params in
  List.iter (fun n ->
    walk n ~f:(fun child ->
      match child with
      | Node { name = "ParamAssignment"; _ } ->
          let id_node = find_first child
            ~name_is:(fun s -> s = "ParameterIdentifier") in
          let nm = Option.bind id_node identifier_name in
          let val_node = find_first child
            ~name_is:(fun s -> s = "ConstantParamExpression"
                            || s = "ConstantExpression") in
          let v = match val_node with
            | Some t -> eval_const_expr ~params:!acc t
            | None -> None
          in
          (match nm, v with
           | Some n, Some v when not (List.mem_assoc n !acc) ->
               acc := (n, v) :: !acc
           | _ -> ())
      | _ -> ())) nodes;
  List.rev !acc

(* Compute the width of a packed-dim list from a DataType subtree.
 * Walks for a (singular) PackedDimension; multi-dim packed arrays
 * aren't modelled here yet. *)
let signal_width_of ~params type_node =
  match find_first type_node
          ~name_is:(fun n -> n = "PackedDimension") with
  | None ->
      (* Maybe an integer atom (`int`, `integer`, …) with implicit
         width.  Read the atom type to pick standard widths. *)
      let atom = find_first type_node
        ~name_is:(fun n -> n = "IntegerAtomType") in
      (match atom with
       | Some t ->
           (match first_leaf t with
            | Some "byte" -> 8
            | Some "shortint" -> 16
            | Some "int" | Some "integer" | Some "time" -> 32
            | Some "longint" -> 64
            | _ -> 1)
       | None -> 1)
  | Some pdim ->
      (match extract_packed_range ~params pdim with
       | Some (m, l) -> abs (m - l) + 1
       | None -> 1)

let signal_signed_of type_node =
  let signing = find_first type_node
    ~name_is:(fun n -> n = "Signing") in
  match signing with
  | Some t ->
      (match first_leaf t with
       | Some "signed" -> Signed
       | _ -> Unsigned)
  | None -> Unsigned

(* Extract internal signals from DataDeclaration / NetDeclaration
 * nodes in the module body.  Each declaration may carry multiple
 * variable / net identifiers; emit one bsignal per name with the
 * shared type+width.  Skip names that are already in the port list
 * (the caller passes them in `port_names` so we don't duplicate
 * ports as internal signals).
 *
 * `cur_signal_widths` lookups are populated downstream by the
 * miter; we just produce bsignal records here. *)
let extract_internal_signals ~params ~port_names module_node =
  let data_decls = find_all module_node
    ~name_is:(fun n -> n = "DataDeclarationVariable") in
  let net_decls = find_all module_node
    ~name_is:(fun n -> n = "NetDeclarationNetType") in
  let from_data acc node =
    let type_node = find_first node
      ~name_is:(fun n -> n = "DataTypeOrImplicit"
                      || n = "DataType") in
    let width, signed = match type_node with
      | Some t -> signal_width_of ~params t, signal_signed_of t
      | None -> 1, Unsigned
    in
    let names = find_all node
      ~name_is:(fun n -> n = "VariableIdentifier"
                      || n = "VariableDeclAssignmentVariable") in
    List.fold_left (fun acc nid ->
      match identifier_name nid with
      | Some nm when not (List.mem nm port_names)
                   && not (List.mem nm (List.map fst acc)) ->
          (nm, {
            name = nm;
            stype = BInt { width; signed };
            direction = `Internal;
            initial_value = None;
            attrs = [];
          }) :: acc
      | _ -> acc) acc names
  in
  let from_net acc node =
    let type_node = find_first node
      ~name_is:(fun n -> n = "DataTypeOrImplicit") in
    let width, signed = match type_node with
      | Some t -> signal_width_of ~params t, signal_signed_of t
      | None -> 1, Unsigned
    in
    let names = find_all node
      ~name_is:(fun n -> n = "NetIdentifier"
                      || n = "NetDeclAssignment") in
    List.fold_left (fun acc nid ->
      match identifier_name nid with
      | Some nm when not (List.mem nm port_names)
                   && not (List.mem nm (List.map fst acc)) ->
          (nm, {
            name = nm;
            stype = BInt { width; signed };
            direction = `Internal;
            initial_value = None;
            attrs = [];
          }) :: acc
      | _ -> acc) acc names
  in
  let acc = List.fold_left from_data [] data_decls in
  let acc = List.fold_left from_net acc net_decls in
  List.rev_map snd acc

(* ---------------------------------------------------------------- *)
(* Function declarations → bfunc                                      *)
(* ---------------------------------------------------------------- *)

(* Extract input ports from a FunctionBodyDeclarationWithoutPort.
 * Each TfPortDeclaration carries TfPortDirection + DataTypeOrImplicit
 * + a list of PortIdentifier(s).  Returns (name, btype, dir) tuples
 * in source order. *)
let extract_function_ports ~params body =
  let port_decls = find_all body
    ~name_is:(fun n -> n = "TfPortDeclaration") in
  List.concat_map (fun pd ->
    let dir = match find_first pd
                ~name_is:(fun n -> n = "TfPortDirection") with
      | Some t ->
          (match first_leaf t with
           | Some "output" -> `Output
           | Some "inout" -> `Inout
           | _ -> `Input)
      | None -> `Input
    in
    let type_node = find_first pd
      ~name_is:(fun n -> n = "DataTypeOrImplicit") in
    let width = match type_node with
      | Some t -> signal_width_of ~params t
      | None -> 1
    in
    let signed = match type_node with
      | Some t -> signal_signed_of t
      | None -> Unsigned
    in
    let ports = find_all pd
      ~name_is:(fun n -> n = "PortIdentifier"
                      || n = "TfPortItem") in
    List.filter_map (fun p ->
      match identifier_name p with
      | Some name -> Some (name, BInt { width; signed }, dir)
      | None -> None) ports
  ) port_decls

let extract_functions ~params module_node =
  let funcs = find_all module_node
    ~name_is:(fun n -> n = "FunctionDeclaration") in
  List.filter_map (fun fn ->
    let body = find_first fn
      ~name_is:(fun n -> n = "FunctionBodyDeclaration"
                      || n = "FunctionBodyDeclarationWithoutPort") in
    match body with
    | None -> None
    | Some b ->
        let fname_node = find_first b
          ~name_is:(fun n -> n = "FunctionIdentifier") in
        let fname = Option.bind fname_node identifier_name in
        (match fname with
         | None -> None
         | Some fname ->
             let return_type =
               let dt = find_first b
                 ~name_is:(fun n -> n = "FunctionDataTypeOrImplicit"
                                 || n = "DataTypeOrVoid") in
               match dt with
               | Some t ->
                   BInt {
                     width = signal_width_of ~params t;
                     signed = signal_signed_of t;
                   }
               | None -> BInt { width = 1; signed = Unsigned }
             in
             let ftype = return_type in
             let f_params = extract_function_ports ~params b in
             (* Function body: in sv-parser the body is a list of
                FunctionStatementOrNull directly under the body decl
                (or under a nested SeqBlock if begin/end was used).
                Pick the outermost wrappers only so we don't process
                the same statement twice via the inner FunctionStatement. *)
             let body_children = match b with
               | Node { children; _ } -> children
               | _ -> []
             in
             let rec collect_top_stmts nodes =
               List.concat_map (function
                 | Node { name =
                     ("FunctionStatementOrNull" | "FunctionStatement"); _ } as t ->
                     [t]
                 | Node { name =
                     ("FunctionBodyDeclarationWithoutPort"
                    | "FunctionBodyDeclarationWithPort"
                    | "FunctionBodyDeclaration"); children } ->
                     collect_top_stmts children
                 | _ -> []) nodes
             in
             let stmts = collect_top_stmts body_children in
             let body_stmts = List.concat_map (fun s ->
               match stmt_to_bstmt ~params s with
               | BBlock ss -> ss
               | other -> [other]) stmts in
             Some {
               fname;
               is_task = false;
               ftype;
               params = f_params;
               locals = [];
               body = body_stmts;
             }))
    funcs

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
      (* Parameter port list first (#(...)) so port packed dims
         can reference them.  Then body-level localparam/parameter
         (extracted with the port-list scope already in `params`
         so a body localparam can build on a port param). *)
      let ppl = find_first node
        ~name_is:(fun n -> n = "ParameterPortList") in
      let header_params = match ppl with
        | Some p -> extract_param_decls p
        | None -> []
      in
      let params = extract_body_params ~init_params:header_params node in
      let plist = find_first node
        ~name_is:(fun n -> n = "ListOfPortDeclarations") in
      let port_signals = match plist with
        | Some p -> extract_port_decls ~params p
        | None -> []
      in
      let port_names = List.map (fun (s : bsignal) -> s.name) port_signals in
      let internal_signals =
        extract_internal_signals ~params ~port_names node
      in
      let signals = port_signals @ internal_signals in
      let assign_procs = extract_continuous_assigns ~params node in
      let always_procs = extract_always_blocks ~params node in
      let funcs = extract_functions ~params node in
      Some {
        name;
        params;
        signals;
        processes = assign_procs @ always_procs;
        instances = [];
        funcs;
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
