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
      Some {
        name;
        params;
        signals;
        processes = [];
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
