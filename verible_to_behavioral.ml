(* Verible → Behavioral IR — a Verilator replacement frontend.
 *
 * Takes a list of Verible-elaborated specialised modules (from
 * Verible_elaborate.specialise_design) and produces a Behavioral_ir
 * `bprogram`, equivalent to what
 * Verilator_to_behavioral.convert_verilator_json_to_behavioral
 * builds from Verilator's JSON output. The downstream miter pipeline
 * (test_xilinx_rtl_miter, test_cva6_bottom_up) accepts either
 * source.
 *
 * Coverage is incremental — what's handled today:
 *   - module port declarations (input/output, scalar + [N-1:0])
 *   - net/reg declarations as internal signals
 *   - continuous `assign lhs = rhs;`
 *   - `always_comb` and `always_ff @(posedge clk)` blocks (best-effort)
 *   - expressions: identifier, decimal/sized number, binary op,
 *     unary op, ternary, paren, bit-select, slice
 *
 * Anything we don't recognise is replaced with a 1-bit zero so the
 * pipeline keeps running. The miter then surfaces the gap as a
 * counter-example, not a parse failure. *)

open Behavioral_ir
open Source_text_verible
open Verible_elaborate

(* ─── Width / range evaluation ───────────────────────────────────── *)

(* Walk an expression token tree, fold to an int when every leaf is
 * a literal or a known package constant. Returns None when the
 * expression has a free identifier. *)
let rec eval_int ~pkgs ~params tok =
  let lookup name =
    match List.assoc_opt name params with
    | Some v -> int_of_pvalue (PStr v)
    | None ->
        (* Search all packages — first match wins. *)
        List.find_map (fun (p : package_decl) ->
          List.assoc_opt name p.pkg_params |> Option.map (fun v ->
            match int_of_pvalue v with
            | Some n -> Some n | None -> None)
          |> Option.value ~default:None
        ) pkgs
  in
  match tok with
  | TK_DecNumber n | TK_UnBasedNumber n ->
      (try Some (int_of_string n) with _ -> None)
  | SymbolIdentifier id -> lookup id
  | TUPLE4 (STRING "add_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a + b)
       | _ -> None)
  | TUPLE4 (STRING "add_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a - b)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs) when prefix_is "mul_expr2" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a * b)
       | _ -> None)
  (* Function-like call wrapper: `reference_or_call_base1(reference,
   * call_base)`. The lexer treats `$clog2` as a SymbolIdentifier
   * (not SystemTFIdentifier — only `$test$plusargs` is whitelisted),
   * so $clog2(x) parses through the same shape as a regular function
   * call. Detect the function name and apply $clog2 specially. *)
  | TUPLE3 (STRING "reference_or_call_base1", ref_node, call_base) ->
      let fname = ref None in
      walk (function
        | SymbolIdentifier id when !fname = None -> fname := Some id
        | _ -> ()) ref_node;
      let inner_arg () =
        let cands = collect_by (function
          | TUPLE4 (STRING t, _, _, _)
            when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
          | TK_DecNumber _ -> true
          | TUPLE3 (STRING t, _, _)
            when prefix_is "reference_or_call_base" t
              || prefix_is "unqualified_id" t -> true
          | _ -> false) call_base in
        match cands with first :: _ -> Some first | [] -> None
      in
      (match !fname with
       | Some "$clog2" ->
           (match inner_arg () with
            | Some e ->
                (match eval_int ~pkgs ~params e with
                 | Some n when n > 1 ->
                     let rec lg n acc =
                       if n <= 1 then acc else lg ((n + 1) / 2) (acc + 1)
                     in
                     Some (lg n 0)
                 | Some _ -> Some 0
                 | None -> None)
            | None -> None)
       (* `$unsigned(x)` / `$signed(x)` — sign-cast, no value change. *)
       | Some ("$unsigned" | "$signed") ->
           (match inner_arg () with
            | Some e -> eval_int ~pkgs ~params e
            | None -> None)
       | _ -> eval_int ~pkgs ~params ref_node)
  | TUPLE4 (STRING tag, _, _, _) when prefix_is "expr_primary_parens" tag ->
      (* Inside (...) — recurse to find the inner expression. *)
      let inner = collect_by (function
        | TUPLE4 (STRING t, _, _, _) when prefix_is "add_expr" t -> true
        | TK_DecNumber _ | SymbolIdentifier _ -> true
        | _ -> false) tok in
      (match inner with
       | x :: _ -> eval_int ~pkgs ~params x
       | [] -> None)
  | TUPLE2 (a, _) | TUPLE3 (_, a, _) -> eval_int ~pkgs ~params a
  | _ -> None

(* `decl_variable_dimension1`: TUPLE6(tag, LBRACK, msb_expr, COLON,
 * lsb_expr, RBRACK). Pull the msb/lsb subtrees directly — collect_by
 * would also descend into them and pick up nested expressions, which
 * shifts the lsb. *)
let extract_range ~pkgs ~params tok =
  let pairs = collect_by (has_tag (prefix_is "decl_variable_dimension")) tok in
  match pairs with
  | (TUPLE6 (STRING _, _lb, msb, _colon, lsb, _rb)) :: _ ->
      let m = eval_int ~pkgs ~params msb in
      let l = eval_int ~pkgs ~params lsb in
      (match m, l with
       | Some mi, Some li -> Some (mi, li)
       | _ -> None)
  | _ -> None

(* Extract every `typedef <data_type> <name>;` from the module body and
 * return [(name, width)]. Verible parses these as `type_declaration1`
 * = TUPLE6(tag, Typedef, data_type, GenericIdentifier, decl_dimensions_opt, ;).
 * Width comes from the inner data_type subtree (we just reuse
 * extract_range, which finds the packed dimension wherever it is). *)
let extract_typedefs ~pkgs ~params tok =
  let nodes = collect_by (has_tag (prefix_is "type_declaration")) tok in
  List.filter_map (fun n -> match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _) ->
        (match extract_range ~pkgs ~params data_type with
         | Some (m, l) -> Some (nm, abs (m - l) + 1)
         | None -> None)
    | _ -> None
  ) nodes

let width_of ?(typedefs = []) ~pkgs ~params tok =
  match extract_range ~pkgs ~params tok with
  | Some (m, l) -> abs (m - l) + 1
  | None ->
      (* No explicit packed dimension — the type might be a typedef
       * reference like `state_type CState`. Walk for the first
       * SymbolIdentifier and look it up in the typedef map. *)
      let nm = ref None in
      walk (function
        | SymbolIdentifier id when !nm = None -> nm := Some id
        | _ -> ()) tok;
      (match !nm with
       | Some id ->
           (match List.assoc_opt id typedefs with
            | Some w -> w
            | None -> 1)
       | None -> 1)

(* `typedef enum logic [N-1:0] { A, B = 5, C, … } t;` — every enum
 * item folds to its integer value. Default sequential numbering
 * starts at 0 and increments; an explicit `= expr` resets the
 * counter. Returns [(item_name, decimal_string)] which can be merged
 * into params so expr_to_bexpr substitutes uses inline. *)
let extract_enum_items ~pkgs ~params tok =
  let nodes = collect_by (has_tag (prefix_is "enum_data_type")) tok in
  List.concat_map (fun n ->
    let list_node = match n with
      | TUPLE5 (_, _, _, ln, _) -> ln  (* enum_data_type1: no base type *)
      | TUPLE6 (_, _, _, _, ln, _) -> ln  (* enum_data_type2: with base *)
      | _ -> EMPTY_TOKEN
    in
    (* enum_name_list is built left-recursively through
     * `enum_name_list_item_last` nodes that nest each item inside the
     * previous list. A simple TLIST iteration only sees the outermost
     * item. Walk the whole list_node and collect every name plus its
     * (optional) explicit value, then process in source order. *)
    let raw = ref [] in
    let consume_item item =
      let nm = ref None in
      walk (function
        | SymbolIdentifier id when !nm = None -> nm := Some id
        | _ -> ()) item;
      let value_node = match item with
        | TUPLE4 (STRING t, _, _, expr) when prefix_is "enum_name" t ->
            Some expr
        | _ -> None
      in
      (match !nm with
       | Some id -> raw := (id, value_node) :: !raw
       | None -> ())
    in
    (* Walk the list_node — every enum_name-tagged node is a wrapped
     * variant; a passthrough bare GenericIdentifier appears as a
     * SymbolIdentifier outside any enum_name wrapper. The flat-list
     * trick: collect items by matching either the enum_name shapes or
     * bare SymbolIdentifiers that aren't already inside one. *)
    (* Strict tag match — `prefix_is "enum_name"` would also catch
     * `enum_name_list_item_last1` and friends. *)
    let is_enum_name_tag t =
      t = "enum_name1" || t = "enum_name2" || t = "enum_name3"
      || t = "enum_name4" || t = "enum_name5" || t = "enum_name6"
    in
    let rec scan t =
      match t with
      | TUPLE4 (STRING tag, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | TUPLE5 (STRING tag, _, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | TUPLE7 (STRING tag, _, _, _, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | TUPLE9 (STRING tag, _, _, _, _, _, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | SymbolIdentifier id ->
          raw := (id, None) :: !raw
      | TUPLE2 (a, b) -> scan a; scan b
      | TUPLE3 (_, a, b) -> scan a; scan b
      | TUPLE4 (_, a, b, c) -> scan a; scan b; scan c
      | TUPLE5 (_, a, b, c, d) -> scan a; scan b; scan c; scan d
      | TUPLE6 (_, a, b, c, d, e) -> List.iter scan [a; b; c; d; e]
      | TUPLE7 (_, a, b, c, d, e, f) -> List.iter scan [a; b; c; d; e; f]
      | TLIST xs -> List.iter scan xs
      | _ -> ()
    in
    scan list_node;
    let items_in_source_order = List.rev !raw in
    let counter = ref 0 in
    List.filter_map (fun (id, value_node) ->
      let v = match value_node with
        | Some expr ->
            (match eval_int ~pkgs ~params expr with
             | Some n -> counter := n + 1; n
             | None -> let n = !counter in counter := !counter + 1; n)
        | None ->
            let n = !counter in counter := !counter + 1; n
      in
      Some (id, string_of_int v)
    ) items_in_source_order
  ) nodes

(* ─── Expression conversion ──────────────────────────────────────── *)

let dummy_bool = BInt { width = 1; signed = Unsigned }

(* Best-effort expr → bexpr translator. Walks one level at a time,
 * recursing where the shape is recognised. Anything else becomes a
 * 1-bit zero with a stderr note (when MITER_VERIBLE_DEBUG is set). *)
let debug_expr = lazy (Sys.getenv_opt "MITER_VERIBLE_DEBUG" <> None)

let rec expr_to_bexpr ~pkgs ~params tok =
  let recurse = expr_to_bexpr ~pkgs ~params in
  let bin op a b =
    BBinOp { op; lhs = recurse a; rhs = recurse b;
             result_type = dummy_bool }
  in
  let un op a =
    BUnOp { op; operand = recurse a; result_type = dummy_bool }
  in
  (* Sized literal: TUPLE3(STRING "bin_based_number1",
   *   TK_BinBase "<W>'b", TK_BinDigits "<bits>")
   *   etc. Decode <W> and the digits. *)
  let parse_sized prefix base_token digits_token =
    let width =
      try
        let bs = match base_token with
          | TK_BinBase s | TK_HexBase s | TK_DecBase s | TK_OctBase s -> s
          | _ -> ""
        in
        let i = String.index bs '\'' in
        int_of_string (String.sub bs 0 i)
      with _ -> 32
    in
    let digits = match digits_token with
      | TK_BinDigits s | TK_HexDigits s
      | TK_DecDigits s | TK_OctDigits s -> s
      | _ -> "0"
    in
    let value =
      try int_of_string ("0" ^ prefix ^ digits)
      with _ -> 0
    in
    BConst { value; width }
  in
  match tok with
  | SymbolIdentifier id ->
      (match List.assoc_opt id params with
       | Some v ->
           (try BConst { value = int_of_string v; width = 32 }
            with _ -> BVar id)
       | None -> BVar id)
  | TK_DecNumber n | TK_UnBasedNumber n ->
      (try BConst { value = int_of_string n; width = 32 }
       with _ -> BConst { value = 0; width = 32 })
  | TUPLE3 (STRING tag, base, digits) when prefix_is "bin_based_number" tag ->
      parse_sized "b" base digits
  | TUPLE3 (STRING tag, base, digits) when prefix_is "hex_based_number" tag ->
      parse_sized "x" base digits
  | TUPLE3 (STRING tag, base, digits) when prefix_is "dec_based_number" tag ->
      parse_sized "" base digits
  | TUPLE3 (STRING tag, base, digits) when prefix_is "oct_based_number" tag ->
      parse_sized "o" base digits
  | TUPLE3 (STRING t, SymbolIdentifier id, _) when prefix_is "unqualified_id" t ->
      (* If the identifier is a known parameter, fold to its constant
       * value here so downstream Z3 sees a concrete int rather than a
       * free variable. *)
      (match List.assoc_opt id params with
       | Some v ->
           (try BConst { value = int_of_string v; width = 32 }
            with _ -> BVar id)
       | None -> BVar id)
  (* Concatenation `{a, b, c}` — Verible parses as
   * `range_list_in_braces1`: TUPLE4(tag, LBRACE, open_range_list, RBRACE).
   * The open_range_list is a TLIST of expressions (with COMMA separators
   * folded in); collect every expression-shaped child and concat them.
   * Note: SV concat orders MSB → LSB, which matches BConcat semantics. *)
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let exprs = match body with
        | TLIST xs ->
            (* TLIST is reverse source order (parser uses left recursion).
             * Reverse to preserve MSB → LSB ordering. *)
            List.filter_map (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN -> None
              | _ -> Some (recurse e)) (List.rev xs)
        | other -> [recurse other]
      in
      (match exprs with
       | [single] -> single
       | _ -> BConcat exprs)
  (* Function-like call: `reference_or_call_base1`. The lexer treats
   * `$unsigned`/`$signed` as ordinary SymbolIdentifier (not
   * SystemTFIdentifier), so they parse through this shape too — both
   * are sign-cast no-ops at the Z3 BV level. Other recognised system
   * tasks: $clog2 (folded to a constant by eval_int — see above). *)
  | TUPLE3 (STRING "reference_or_call_base1", ref_node, call_base) ->
      let fname = ref None in
      walk (function
        | SymbolIdentifier id when !fname = None -> fname := Some id
        | _ -> ()) ref_node;
      (match !fname with
       | Some ("$unsigned" | "$signed") ->
           (* Find the inner expression argument. *)
           let cands = collect_by (function
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "range_list_in_braces" t -> true
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
             | TUPLE3 (STRING t, _, _) when prefix_is "unqualified_id" t -> true
             | TK_DecNumber _ -> true
             | _ -> false) call_base in
           (match cands with
            | first :: _ -> recurse first
            | [] -> recurse call_base)
       | Some name when (try name.[0] = '$' with _ -> false) ->
           (* Other $foo() — fall back to evaluating the integer
            * literal (works for $clog2 et al). *)
           (match eval_int ~pkgs ~params tok with
            | Some n -> BConst { value = n; width = 32 }
            | None -> BConst { value = 0; width = 1 })
       | Some fname ->
           (* Plain user function call. Emit BCall so the inline pass
            * (Behavioral_inline) can substitute the body in. Pull the
            * argument expressions out of the call_base. *)
           let arg_exprs =
             let cands = collect_by (function
               | TUPLE4 (STRING t, _, _, _)
                 when prefix_is "add_expr" t || prefix_is "mul_expr" t
                   || prefix_is "comp_expr" t || prefix_is "logeq_expr" t
                   || prefix_is "and_expr" t || prefix_is "or_expr" t
                   || prefix_is "xor_expr" t
                   || prefix_is "logand_expr" t || prefix_is "logor_expr" t
                   || prefix_is "shift_expr" t -> true
               | TUPLE3 (STRING t, _, _)
                 when prefix_is "unqualified_id" t
                   || prefix_is "reference" t -> true
               | TUPLE3 (STRING t, _, _)
                 when prefix_is "bin_based_number" t
                   || prefix_is "hex_based_number" t
                   || prefix_is "dec_based_number" t
                   || prefix_is "oct_based_number" t -> true
               | TK_DecNumber _ | TK_UnBasedNumber _ -> true
               | _ -> false) call_base in
             List.map recurse cands
           in
           BCall { func = fname; args = arg_exprs }
       | None -> BConst { value = 0; width = 1 })
  (* Bit-select / part-select on a reference: `reference3` is
   *   TUPLE3(tag, reference, select_variable_dimension).
   * The inner dimension is either select_variable_dimension2 (single
   * index `[N]`, TUPLE4) or select_variable_dimension1 (range `[M:N]`,
   * TUPLE6). Emit BSlice — z3_miter handles BSlice but not BSelect. *)
  | TUPLE3 (STRING t, ref_node, dim_node) when prefix_is "reference" t
                                              && t <> "reference1" ->
      let signal = recurse ref_node in
      (match dim_node with
       | TUPLE6 (STRING dt, _, msb, _, lsb, _)
         when prefix_is "select_variable_dimension" dt ->
           (match eval_int ~pkgs ~params msb,
                  eval_int ~pkgs ~params lsb with
            | Some m, Some l -> BSlice { signal; msb = m; lsb = l }
            | _ -> signal)
       | TUPLE4 (STRING dt, _, idx, _)
         when prefix_is "select_variable_dimension" dt ->
           (match eval_int ~pkgs ~params idx with
            | Some i -> BSlice { signal; msb = i; lsb = i }
            | None -> signal)
       | _ -> signal)
  (* `unary_prefix_expr2`: TUPLE3(tag, unary_op_token, operand).
   * Must come before the generic TUPLE3 wrapper below — otherwise the
   * fallback recurses into the operator token and discards the operand.
   * unary_op variants we recognise: TILDE → bitwise NOT,
   * HYPHEN → arithmetic negation, AMPERSAND/VBAR/CARET → reductions
   * (AND/OR/XOR), TILDE_AMPERSAND/_VBAR/_CARET → reduce-then-NOT,
   * PLING → logical NOT (= reduce-OR + NOT), PLUS → no-op. *)
  | TUPLE3 (STRING tag, op_tok, operand) when prefix_is "unary_prefix_expr" tag ->
      let inner = recurse operand in
      let result_t = dummy_bool in
      let red op = BUnOp { op; operand = inner; result_type = result_t } in
      (match op_tok with
       | TILDE                -> BUnOp { op = BNot; operand = inner; result_type = result_t }
       | HYPHEN               -> BUnOp { op = BNeg; operand = inner; result_type = result_t }
       | PLUS                 -> inner
       | AMPERSAND            -> red BRedAnd
       | VBAR                 -> red BRedOr
       | CARET                -> red BRedXor
       | TILDE_AMPERSAND      ->
           BUnOp { op = BNot; operand = red BRedAnd; result_type = result_t }
       | TILDE_VBAR           ->
           BUnOp { op = BNot; operand = red BRedOr; result_type = result_t }
       | TILDE_CARET          ->
           BUnOp { op = BNot; operand = red BRedXor; result_type = result_t }
       | PLING                ->
           BUnOp { op = BNot; operand = red BRedOr; result_type = result_t }
       | _                    ->
           BUnOp { op = BNot; operand = inner; result_type = result_t })
  | TUPLE3 (STRING _, a, b) ->
      (* Generic single-content wrapper. Verible puts the meaningful
       * subtree in slot 1 most of the time (slot 2 is usually
       * EMPTY_TOKEN or punctuation). Prefer slot 1; fall back to
       * slot 2 if slot 1 isn't a tree. *)
      (match a with
       | EMPTY_TOKEN -> recurse b
       | _ -> recurse a)
  | TUPLE4 (STRING tag, lhs, _op, rhs) ->
      (* Binary expressions: add_expr, mul_expr, comp_expr, and_expr,
       * or_expr, xor_expr, shift_expr, logeq_expr, etc. *)
      if prefix_is "add_expr2" tag then bin BAdd lhs rhs
      else if prefix_is "add_expr3" tag then bin BSub lhs rhs
      else if prefix_is "mul_expr2" tag then bin BMul lhs rhs
      else if prefix_is "and_expr" tag || prefix_is "bitand_expr" tag
        then bin BAnd lhs rhs
      else if prefix_is "or_expr" tag || prefix_is "bitor_expr" tag
        then bin BOr lhs rhs
      else if prefix_is "xor_expr" tag || prefix_is "bitxor_expr" tag
        then bin BXor lhs rhs
      (* `&&` and `||` — logical AND/OR. For 1-bit operands these
       * coincide with bitwise AND/OR; for wider operands the SV
       * semantics is "any non-zero bit ⇒ true" but the existing
       * BAnd/BOr Z3 encoding already widens to the operand width
       * and the result is non-zero iff both/either are non-zero. *)
      else if prefix_is "logand_expr" tag then bin BAnd lhs rhs
      else if prefix_is "logor_expr" tag then bin BOr lhs rhs
      else if prefix_is "shift_expr2" tag then bin BShl lhs rhs
      else if prefix_is "shift_expr3" tag then bin BShr lhs rhs
      else if prefix_is "shift_expr4" tag then bin BAshr lhs rhs
      else if prefix_is "comp_expr2" tag then bin BLt lhs rhs
      else if prefix_is "comp_expr3" tag then bin BGt lhs rhs
      else if prefix_is "comp_expr4" tag then bin BLe lhs rhs
      else if prefix_is "comp_expr5" tag then bin BGe lhs rhs
      else if prefix_is "logeq_expr2" tag || prefix_is "binary_eq_expr1" tag
        then bin BEq lhs rhs
      else if prefix_is "logeq_expr3" tag then bin BNe lhs rhs
      else if prefix_is "expr_primary_parens" tag then recurse _op
      else begin
        if Lazy.force debug_expr then
          Printf.eprintf "[verible_to_bir] unhandled TUPLE4 expr %s\n" tag;
        BConst { value = 0; width = 1 }
      end
  (* unary_prefix_expr handled above the generic TUPLE3 fallback. *)
  (* `cond_expr2`: TUPLE6(tag, cond, QUERY, then_expr, COLON, else_expr).
   * The ternary `?:` operator. *)
  | TUPLE6 (STRING tag, cond, _, t, _, e) when prefix_is "cond_expr" tag ->
      BCond { condition = recurse cond;
              then_val = recurse t;
              else_val = recurse e }
  | TLIST [single] -> recurse single
  | _ ->
      if Lazy.force debug_expr then
        Printf.eprintf "[verible_to_bir] unhandled expr shape\n";
      BConst { value = 0; width = 1 }

(* ─── Module port + signal extraction ────────────────────────────── *)

(* Pull the SymbolIdentifiers out of a `module_port_declaration*`
 * subtree so each name becomes one signal. *)
let extract_port_decl ~pkgs ~params tok =
  let dir = ref `Internal in
  walk (function
    | Input -> dir := `Input
    | Output -> dir := `Output
    | Inout -> dir := `Internal
    | _ -> ()
  ) tok;
  (* Collect port names — but skip identifiers that live inside a
   * packed/select dimension subtree (e.g. the `WIDTH` inside
   * `output [WIDTH-1:0] Q`). Without this filter `WIDTH` would be
   * extracted as a port of the same direction as `Q`. *)
  let names = ref [] in
  let rec walk_skip t =
    match t with
    | TUPLE6 (STRING t', _, _, _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    | TUPLE4 (STRING t', _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    | TUPLE3 (STRING t', SymbolIdentifier id, _)
      when prefix_is "unqualified_id" t' ->
        names := id :: !names
    | TUPLE2 (a, b) -> walk_skip a; walk_skip b
    | TUPLE3 (a, b, c) -> walk_skip a; walk_skip b; walk_skip c
    | TUPLE4 (a, b, c, d) -> walk_skip a; walk_skip b; walk_skip c; walk_skip d
    | TUPLE5 (a, b, c, d, e) ->
        walk_skip a; walk_skip b; walk_skip c; walk_skip d; walk_skip e
    | TUPLE6 (a, b, c, d, e, f) ->
        List.iter walk_skip [a; b; c; d; e; f]
    | TUPLE7 (a, b, c, d, e, f, g) ->
        List.iter walk_skip [a; b; c; d; e; f; g]
    | TUPLE8 (a, b, c, d, e, f, g, h) ->
        List.iter walk_skip [a; b; c; d; e; f; g; h]
    | TLIST xs -> List.iter walk_skip xs
    | _ -> ()
  in
  walk_skip tok;
  let w = width_of ~pkgs ~params tok in
  List.rev !names |> List.sort_uniq compare |> List.map (fun n ->
    {
      name = n;
      stype = BInt { width = w; signed = Unsigned };
      direction = !dir;
      initial_value = None;
    })

(* ─── Continuous assigns ─────────────────────────────────────────── *)

(* Walk inside a token, return the first SymbolIdentifier found
 * inside an `unqualified_id` tag — the canonical way the lhs of
 * `cont_assign1` / `assignment_statement_no_expr1` /
 * `nonblocking_assignment1` carries the target name. *)
let lhs_name_of tok =
  let n = ref None in
  walk (function
    | TUPLE3 (STRING t, SymbolIdentifier id, _)
      when prefix_is "unqualified_id" t && !n = None -> n := Some id
    | _ -> ()
  ) tok;
  !n

let extract_assign ~pkgs ~params tok =
  let assigns = collect_by (has_tag (prefix_is "cont_assign")) tok in
  List.filter_map (fun a ->
    match a with
    | TUPLE4 (STRING tag, lhs, _eq, rhs) when prefix_is "cont_assign" tag ->
        (match lhs_name_of lhs with
         | Some name ->
             let rhs_e = expr_to_bexpr ~pkgs ~params rhs in
             Some (BCombinational {
               name = "assign_" ^ name;
               sensitivity = [BAny];
               body = [BAssign { lhs = name; rhs = rhs_e }];
             })
         | None -> None)
    | _ -> None
  ) assigns

(* ─── Procedural statement → BIR statement ───────────────────────── *)

(* Recognised statement shapes inside an `always_*` body. Anything
 * else becomes BBlock []. *)
let rec stmt_to_bstmt ~pkgs ~params tok =
  let recurse_e = expr_to_bexpr ~pkgs ~params in
  let recurse_s = stmt_to_bstmt ~pkgs ~params in
  match tok with
  (* Blocking assignment: lhs = rhs *)
  | TUPLE4 (STRING tag, lhs, _eq, rhs)
    when prefix_is "assignment_statement_no_expr" tag ->
      (match lhs_name_of lhs with
       | Some name -> BAssign { lhs = name; rhs = recurse_e rhs }
       | None -> BBlock [])
  (* Non-blocking: lhs <= rhs (Verible: nonblocking_assignment1
   *   TUPLE6(tag, lhs, _, _, rhs, _)) *)
  | TUPLE6 (STRING tag, lhs, _, _, rhs, _)
    when prefix_is "nonblocking_assignment" tag ->
      (match lhs_name_of lhs with
       | Some name -> BAssign { lhs = name; rhs = recurse_e rhs }
       | None -> BBlock [])
  (* Conditional statement. Match by direct tuple structure to pick
   * the immediate then/else slots (don't recurse — collect_by would
   * pull in statements from nested conditionals too). Arities seen:
   *   TUPLE7 — full `if (cond) then else else`:
   *     (tag, _, if_kw, expr_in_parens, then, else_kw, else)
   *   TUPLE5 — `if (cond) then` (no else):
   *     (tag, _, if_kw, expr_in_parens, then) *)
  | TUPLE7 (STRING tag, _, _, exp_par, then_node, _, else_node)
    when prefix_is "conditional_statement" tag ->
      let cond = match exp_par with
        | TUPLE4 (_, _, e, _) -> recurse_e e
        | other -> recurse_e other
      in
      BIf { condition = cond;
            then_stmts = [recurse_s then_node];
            else_stmts = [recurse_s else_node] }
  | TUPLE5 (STRING tag, _, _, exp_par, then_node)
    when prefix_is "conditional_statement" tag ->
      let cond = match exp_par with
        | TUPLE4 (_, _, e, _) -> recurse_e e
        | other -> recurse_e other
      in
      BIf { condition = cond;
            then_stmts = [recurse_s then_node];
            else_stmts = [] }
  (* `case (selector) ... endcase` — case_statement1 (TUPLE8):
   *   tag, _, _, LPAREN, selector, RPAREN, case_items1, ENDCASE *)
  | TUPLE8 (STRING tag, _, _, _, selector, _, items, _)
    when prefix_is "case_statement" tag ->
      let sel = recurse_e selector in
      let case_items = collect_by (has_tag (prefix_is "case_item")) items in
      let cases, default =
        List.fold_left (fun (cs, def) ci ->
          match ci with
          | TUPLE4 (STRING t, key, _colon, body) when prefix_is "case_item" t ->
              if prefix_is "case_item2" t then
                (* `default:` arm. *)
                (cs, [recurse_s body])
              else
                let k = recurse_e key in
                let b = [recurse_s body] in
                (cs @ [(k, b)], def)
          | _ -> (cs, def)
        ) ([], []) case_items in
      BCase { selector = sel; cases; default }
  (* `seq_block1(begin1, TLIST [stmts], end)`. Verible's parser builds
   * the statement list with `TLIST ($2 :: lst)`, prepending each new
   * statement — so the TLIST is in *reverse* source order. We must
   * reverse it back before generating BIR, or unconditional defaults
   * end up after their conditional refinements and overwrite them. *)
  | TUPLE4 (STRING tag, _begin, body, _end)
    when prefix_is "seq_block" tag ->
      let stmts = match body with
        | TLIST xs ->
            List.filter_map (fun s ->
              let mapped = recurse_s s in
              match mapped with
              | BBlock [] -> None
              | other -> Some other
            ) (List.rev xs)
        | _ -> []
      in
      BBlock stmts
  (* statement_item wrappers — descend. *)
  | TUPLE3 (STRING tag, inner, _) when prefix_is "statement_item" tag ->
      recurse_s inner
  | TUPLE3 (STRING tag, _, inner) when prefix_is "statement_item" tag ->
      recurse_s inner
  | TLIST [single] -> recurse_s single
  | _ -> BBlock []

(* ─── always blocks ──────────────────────────────────────────────── *)

(* `always_construct1(<keyword>, body)`. Sequential iff the sensitivity
 * list contains a posedge/negedge edge operator. The strictly-correct
 * rule (state-holding iff some lhs isn't assigned on every code path)
 * would also catch level-sensitive latches like
 * `always @(en) if (en) a <= b;` — but a simple "any if without else"
 * proxy fires too often (e.g. an unconditional default at the top of
 * the block followed by a case-branch with conditional refinement is
 * still combinational). Keeping the simple edge-based rule until we
 * have per-lhs path-coverage analysis. *)
let extract_always ~pkgs ~params tok =
  let always_nodes = collect_by
    (has_tag (prefix_is "always_construct")) tok in
  List.map (fun an ->
    let has_edge = ref false in
    walk (function
      | Posedge | Negedge -> has_edge := true
      | _ -> ()) an;
    match !has_edge with
    | true ->
        (* Sequential. Find the clock identifier from the first
         * event_expression1. *)
        let clk =
          let evs = collect_by (has_tag (prefix_is "event_expression")) an in
          match evs with
          | e :: _ ->
              let ids = ref [] in
              walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) e;
              (match !ids with
               | id :: _ -> id
               | [] -> "clk")
          | [] -> "clk"
        in
        (* Body is the statement that follows the timing-control
         * statement; find a seq_block or any procedural shape. *)
        let body_nodes = collect_by (has_tag (fun t ->
          prefix_is "seq_block" t ||
          prefix_is "nonblocking_assignment" t ||
          prefix_is "assignment_statement_no_expr" t ||
          prefix_is "conditional_statement" t ||
          prefix_is "case_statement" t)) an in
        let body = match body_nodes with
          | b :: _ -> [stmt_to_bstmt ~pkgs ~params b]
          | [] -> []
        in
        BSequential {
          name = "always_ff";
          clock = clk;
          clock_edge = `Pos;
          reset = None;
          reset_edge = None;
          reset_async = false;
          body;
        }
    | false ->
        (* Level-sensitive always block. Treated as combinational here
         * to match the Verilator-side classifier. The strictly-correct
         * rule (must-assign analysis to detect true latches) was tried
         * but caused 12 cva6 leaves to regress — many `always_comb`
         * blocks in cva6 leave some lhs conditionally driven and rely
         * on the value being initialised earlier. Revisit once we can
         * thread a "no aggressive latch detect on cva6" toggle through
         * both sides consistently. *)
        let body_nodes = collect_by (has_tag (fun t ->
          prefix_is "seq_block" t ||
          prefix_is "assignment_statement_no_expr" t ||
          prefix_is "conditional_statement" t ||
          prefix_is "case_statement" t)) an in
        let body = match body_nodes with
          | b :: _ -> [stmt_to_bstmt ~pkgs ~params b]
          | [] -> []
        in
        BCombinational {
          name = "always_comb";
          sensitivity = [BAny];
          body;
        }
  ) always_nodes

(* ─── Module instances ───────────────────────────────────────────── *)

(* `non_anonymous_gate_instance_or_register_variable2`:
 *   <inst_name>, _, LPAREN, TLIST [port_named1 ...], RPAREN
 * We treat each `port_named1(_, _, port_name, LPAREN, expr, RPAREN)`
 * as a port connection. Returns Behavioral_ir.binstance, with the
 * containing module name picked up from the surrounding
 * instantiation_base1. *)
let extract_instances ~pkgs:_ ~params:_ tok =
  let bases = collect_by (has_tag (prefix_is "instantiation_base")) tok in
  List.concat_map (fun base ->
    let mod_name = ref None in
    walk (function
      | TUPLE3 (STRING t, SymbolIdentifier id, _)
        when prefix_is "unqualified_id" t && !mod_name = None ->
          mod_name := Some id
      | _ -> ()
    ) base;
    let inst_nodes = collect_by (has_tag (fun t ->
      prefix_is "non_anonymous_gate_instance" t ||
      prefix_is "module_instance" t)) base in
    List.filter_map (fun in_ ->
      let inst_name = ref None in
      let port_conns = ref [] in
      let pn_tags = collect_by (has_tag (prefix_is "port_named")) in_ in
      List.iter (fun pn ->
        match pn with
        | TUPLE6 (_, _, SymbolIdentifier port, _, expr, _) ->
            port_conns := (port, BVar (
              match expr with
              | TUPLE3 (_, SymbolIdentifier id, _) -> id
              | SymbolIdentifier id -> id
              | _ -> "?"
            )) :: !port_conns
        | _ -> ()
      ) pn_tags;
      (match in_ with
       | TUPLE6 (_, SymbolIdentifier id, _, _, _, _)
       | TUPLE5 (_, SymbolIdentifier id, _, _, _) ->
           inst_name := Some id
       | _ ->
           walk (function
             | SymbolIdentifier id when !inst_name = None -> inst_name := Some id
             | _ -> ()) in_);
      match !mod_name, !inst_name with
      | Some m, Some i ->
          Some {
            inst_name = i;
            module_name = m;
            param_values = [];
            port_connections = List.rev !port_conns;
          }
      | _ -> None
    ) inst_nodes
  ) bases

(* ─── Module conversion ──────────────────────────────────────────── *)

(* `parameter X = 4;` declared either in the module header
 *   (`module foo #(parameter X = 4) (...)`) or the body
 *   (`parameter X = 4;`). The instance-override map (`params`) wins;
 * defaults declared here fill in for unbound names. Both shapes have a
 * `trailing_assign1` slot whose subtree is the value expression. *)
let extract_body_params ~pkgs ~params tok =
  let nodes =
    collect_by (has_tag (prefix_is "any_param_declaration")) tok
    @ collect_by (has_tag (prefix_is "module_parameter_port")) tok
  in
  List.filter_map (fun n ->
    let nm = ref None in
    walk (function
      | SymbolIdentifier id when !nm = None -> nm := Some id
      | _ -> ()) n;
    let value_node = ref None in
    walk (function
      | TUPLE4 (STRING t, _, v, _) when prefix_is "trailing_assign" t
                                         && !value_node = None ->
          value_node := Some v
      | _ -> ()) n;
    match !nm, !value_node with
    | Some id, Some v ->
        (match eval_int ~pkgs ~params v with
         | Some i -> Some (id, string_of_int i)
         | None -> None)
    | _ -> None
  ) nodes

let convert_module ~pkgs (mdecl : module_decl)
                          (params : (string * string) list) : bmodule =
  (* Merge instance-override params with body-declared defaults; the
   * override wins on conflict (List.assoc_opt finds it first). *)
  let body_params = extract_body_params ~pkgs ~params mdecl.m_body in
  let params =
    let known = List.map fst params in
    params @ List.filter (fun (n, _) -> not (List.mem n known)) body_params
  in
  (* Enum item names fold to their integer values via the same
   * params lookup that handles `parameter`. *)
  let enum_items = extract_enum_items ~pkgs ~params mdecl.m_body in
  let params =
    let known = List.map fst params in
    params @ List.filter (fun (n, _) -> not (List.mem n known)) enum_items
  in
  let typedefs = extract_typedefs ~pkgs ~params mdecl.m_body in
  (* Three port-list flavours we need to handle together:
   *   - module_port_declaration5  → K&R: `input clk;`
   *   - port_declaration_noattr1  → ANSI explicit: `input logic [W-1:0] x`
   *   - port1 / port_reference1   → ANSI inherited: in `input a, b, c`
   *     the names `b` and `c` only appear as port_reference1 entries
   *     with the direction inherited from the preceding decl.
   *)
  let port_nodes = collect_by (has_tag (fun t ->
    prefix_is "module_port_declaration" t ||
    prefix_is "port_declaration_noattr" t)) mdecl.m_body in
  let explicit_signals =
    List.concat_map (extract_port_decl ~pkgs ~params) port_nodes
  in
  let explicit_names = List.map (fun (s : bsignal) -> s.name)
                         explicit_signals in
  (* Pull every port_reference1 name from the module_port_list_opt.
   * For names not already declared explicitly, default to Input
   * (modules rarely have implicit-direction outputs in cva6). *)
  let port_list_node = collect_by
    (has_tag (prefix_is "module_port_list_opt")) mdecl.m_body in
  let inherited_names =
    List.concat_map (fun pl ->
      let refs = collect_by (has_tag (prefix_is "port_reference")) pl in
      List.filter_map (fun r ->
        let n = ref None in
        walk (function
          | TUPLE3 (STRING t, SymbolIdentifier id, _)
            when prefix_is "unqualified_id" t && !n = None -> n := Some id
          | _ -> ()
        ) r;
        !n
      ) refs
    ) port_list_node
    |> List.sort_uniq compare
  in
  let inherited_signals =
    List.filter_map (fun n ->
      if List.mem n explicit_names then None
      else Some {
        name = n;
        stype = BInt { width = 1; signed = Unsigned };
        direction = `Input;
        initial_value = None;
      }
    ) inherited_names
  in
  let port_signals = explicit_signals @ inherited_signals in
  (* Internal nets — `net_declaration1` (wires) plus
   * `non_anonymous_gate_instance_or_register_variable1` (the
   * register-variable shape Verible uses for `reg X;` / `logic X;`
   * declared in the module body). The ...register_variable*2* tag,
   * by contrast, is reserved for actual module instantiations (its
   * parent's instantiation_base1 has an unqualified_id rather than
   * a data_type_primitive). *)
  let net_nodes = collect_by
    (has_tag (prefix_is "net_declaration")) mdecl.m_body in
  (* `reg [W:0] foo, bar;` parses as `instantiation_base1` whose first
   * child is the `instantiation_type` (carrying the packed dimensions)
   * and whose second child is the register-variable list. Collect at
   * the instantiation_base level so the width and the variable names
   * stay paired; bases without a `register_variable1` child are module
   * instantiations and handled elsewhere. *)
  let instbase_nodes = collect_by
    (has_tag (prefix_is "instantiation_base")) mdecl.m_body in
  if Sys.getenv_opt "MITER_VERIBLE_DEBUG" <> None then
    Printf.eprintf "[verible_to_bir] %s: %d instantiation_base nodes\n%!"
      mdecl.m_name (List.length instbase_nodes);
  let reg_var_signals = List.concat_map (fun base ->
    (* The first variable in a comma-separated reg-var list is tagged
     * `non_anonymous_gate_instance_or_register_variable1`; subsequent
     * ones are tagged `gate_instance_or_register_variable1` (no
     * "non_anonymous_" prefix). Collect both so `state_type CState,
     * NState;` gives two signals, not one. *)
    let var_nodes =
      collect_by (has_tag (fun t ->
        prefix_is "non_anonymous_gate_instance_or_register_variable1" t
        || prefix_is "gate_instance_or_register_variable1" t)) base
    in
    if var_nodes = [] then []
    else
      let inst_type = match base with
        | TUPLE3 (_, t, _) -> t
        | _ -> base
      in
      let w = width_of ~typedefs ~pkgs ~params inst_type in
      List.filter_map (fun n ->
        let nm = ref None in
        walk (function
          | SymbolIdentifier id when !nm = None -> nm := Some id
          | _ -> ()) n;
        match !nm with
        | Some id -> Some {
            name = id;
            stype = BInt { width = w; signed = Unsigned };
            direction = `Internal;
            initial_value = None;
          }
        | None -> None
      ) var_nodes
  ) instbase_nodes in
  let internal_signals =
    (List.concat_map (extract_port_decl ~pkgs ~params) net_nodes
     |> List.map (fun s -> { s with direction = `Internal }))
    @ reg_var_signals
  in
  let signals : bsignal list =
    let seen = Hashtbl.create 16 in
    List.filter (fun (s : bsignal) ->
      if Hashtbl.mem seen s.name then false
      else (Hashtbl.replace seen s.name (); true)
    ) (port_signals @ internal_signals)
  in
  let assign_procs = List.concat_map (fun ca ->
    extract_assign ~pkgs ~params ca
  ) (collect_by (has_tag (prefix_is "continuous_assign")) mdecl.m_body) in
  let always_procs = extract_always ~pkgs ~params mdecl.m_body in
  let instances = extract_instances ~pkgs ~params mdecl.m_body in
  {
    name = mdecl.m_name;
    params = [];
    signals;
    processes = assign_procs @ always_procs;
    instances;
    funcs = [];
    mems = [];
  }

(* ─── Top-level entry ────────────────────────────────────────────── *)

(* Parse a list of SV files via Verible, find the top module, and
 * convert it (and its specialised children) to BIR. *)
let convert_files ~top files : bprogram =
  let mods, pkgs = parse_files_full files in
  let by_name = Hashtbl.create 32 in
  List.iter (fun m -> Hashtbl.replace by_name m.m_name m) mods;
  let specs = specialise_design ~pkgs mods ~top_name:top in
  let bmods = List.filter_map (fun (s : specialised) ->
    match Hashtbl.find_opt by_name s.s_base with
    | None ->
        Printf.eprintf "[verible_to_bir] no source module for base %s\n"
          s.s_base;
        None
    | Some mdecl ->
        let m = convert_module ~pkgs mdecl s.s_params in
        Some { m with name = s.s_name }
  ) specs in
  { modules = bmods; library_cells = [] }
