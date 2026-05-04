(* Verible-driven SV elaboration — a Verilator-equivalent pass
 * focused on parameter resolution and module specialisation.
 *
 * Verilator gives us a flat list of monomorphic specialised modules
 * named with opaque hashes (`lzc__W4`, `cva6_fifo_v3__pi19`). Vivado
 * gives us a list of `__parameterizedN` entities whose actual
 * parameter values are recoverable from VHDL `attribute` declarations.
 * Pairing the two needs the parameter dictionary on each side.
 *
 * This module pulls the parameter dictionary out of the Verible parse
 * tree directly: every module's declared parameters and every
 * instantiation's parameter overrides. Walking from a top-level
 * module, we then specialise instantiated modules per unique
 * parameter set and yield a flat list of monomorphic modules with
 * deterministic readable names. *)

open Source_text_verible
open Source_text_verible_tokens

(* ─── Tree-walk helpers ──────────────────────────────────────────── *)

let prefix_is p s =
  let ls = String.length s and lp = String.length p in
  ls >= lp && String.sub s 0 lp = p

(* Run a function over each node of a Verible token tree. The walk
 * covers every TUPLE arity used by Source_text_verible.mly
 * (TUPLE2 .. TUPLE15) and descends into TLIST children. *)
let rec walk f tok =
  f tok;
  match tok with
  | TUPLE2 (a, b) -> walk f a; walk f b
  | TUPLE3 (a, b, c) -> walk f a; walk f b; walk f c
  | TUPLE4 (a, b, c, d) -> walk f a; walk f b; walk f c; walk f d
  | TUPLE5 (a, b, c, d, e) ->
      walk f a; walk f b; walk f c; walk f d; walk f e
  | TUPLE6 (a, b, c, d, e, f') ->
      List.iter (walk f) [a; b; c; d; e; f']
  | TUPLE7 (a, b, c, d, e, f', g) ->
      List.iter (walk f) [a; b; c; d; e; f'; g]
  | TUPLE8 (a, b, c, d, e, f', g, h) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h]
  | TUPLE9 (a, b, c, d, e, f', g, h, i) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i]
  | TUPLE10 (a, b, c, d, e, f', g, h, i, j) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i; j]
  | TUPLE11 (a, b, c, d, e, f', g, h, i, j, k) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i; j; k]
  | TUPLE12 (a, b, c, d, e, f', g, h, i, j, k, l) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i; j; k; l]
  | TUPLE13 (a, b, c, d, e, f', g, h, i, j, k, l, m) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i; j; k; l; m]
  | TUPLE14 (a, b, c, d, e, f', g, h, i, j, k, l, m, n) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i; j; k; l; m; n]
  | TUPLE15 (a, b, c, d, e, f', g, h, i, j, k, l, m, n, o) ->
      List.iter (walk f) [a; b; c; d; e; f'; g; h; i; j; k; l; m; n; o]
  | TLIST xs -> List.iter (walk f) xs
  | _ -> ()

let collect_by pred root =
  let acc = ref [] in
  walk (fun t -> if pred t then acc := t :: !acc) root;
  List.rev !acc

(* Tag accessor for any TUPLE-with-STRING-head node. *)
let tag_of = function
  | TUPLE2 (STRING t, _)
  | TUPLE3 (STRING t, _, _)
  | TUPLE4 (STRING t, _, _, _)
  | TUPLE5 (STRING t, _, _, _, _)
  | TUPLE6 (STRING t, _, _, _, _, _)
  | TUPLE7 (STRING t, _, _, _, _, _, _)
  | TUPLE8 (STRING t, _, _, _, _, _, _, _)
  | TUPLE9 (STRING t, _, _, _, _, _, _, _, _)
  | TUPLE10 (STRING t, _, _, _, _, _, _, _, _, _)
  | TUPLE11 (STRING t, _, _, _, _, _, _, _, _, _, _)
  | TUPLE12 (STRING t, _, _, _, _, _, _, _, _, _, _, _)
  | TUPLE13 (STRING t, _, _, _, _, _, _, _, _, _, _, _, _)
  | TUPLE14 (STRING t, _, _, _, _, _, _, _, _, _, _, _, _, _)
  | TUPLE15 (STRING t, _, _, _, _, _, _, _, _, _, _, _, _, _, _) -> Some t
  | _ -> None

let has_tag p t = match tag_of t with
  | Some s -> p s
  | None -> false

(* Render a token's leaf value as a string. Numbers, identifiers,
 * keywords. For composite trees we recurse into the first child. *)
let rec value_of = function
  | TK_DecNumber n | TK_UnBasedNumber n
  | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n -> n
  | SymbolIdentifier id -> id
  | STRING s -> s
  | TUPLE2 (a, _) | TUPLE3 (_, a, _) -> value_of a
  | TUPLE4 (_, _, _, d) -> value_of d
  | TUPLE6 (_, _, _, _, v, _) -> value_of v   (* parameter_value_byname1 *)
  | other -> getstr other

(* Render a token tree as one whitespace-joined string of all its
 * meaningful leaves, in source order. Unlike `value_of`, which only
 * walks the first child of each tuple node, this preserves the full
 * expression — required for the constant evaluator to see e.g. the
 * `/2` part of `PaddedWidth/2`. Operator/punctuation tokens are
 * mapped back to their original text so the output re-tokenises
 * cleanly. *)
(* Strict allowlist — anything not listed (TUPLEn, TLIST, EMPTY_TOKEN,
 * STRING tag-labels) returns None. Without this, walk emits "TUPLE3"
 * etc. as literal text and the resulting expression is garbage. *)
let leaf_text = function
  | TK_DecNumber n | TK_UnBasedNumber n
  | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n -> Some n
  | SymbolIdentifier id -> Some id
  | LT_LT -> Some "<<"  | GT_GT -> Some ">>"
  | SLASH -> Some "/"   | STAR  -> Some "*"
  | STAR_STAR -> Some "**"
  | PERCENT -> Some "%" | PLUS  -> Some "+" | HYPHEN -> Some "-"
  | LPAREN -> Some "("  | RPAREN -> Some ")"
  | COMMA  -> Some ","  | DOT    -> Some "."
  | COLON_COLON -> Some "::"
  | EQ_EQ  -> Some "==" | PLING_EQ -> Some "!="
  | LESS   -> Some "<"  | GREATER  -> Some ">"
  | LT_EQ  -> Some "<=" | GT_EQ -> Some ">="
  | AMPERSAND_AMPERSAND -> Some "&&"
  | VBAR_VBAR -> Some "||"
  | PLING -> Some "!"
  | QUERY -> Some "?" | COLON -> Some ":"
  | _ -> None

let deep_string_of_token tok =
  let buf = Buffer.create 32 in
  walk (fun t ->
    match leaf_text t with
    | Some s ->
        if Buffer.length buf > 0 then Buffer.add_char buf ' ';
        Buffer.add_string buf s
    | None -> ()
  ) tok;
  Buffer.contents buf

(* ─── Module parameter declarations ──────────────────────────────── *)

(* `module_parameter_port1` shape:
 *   TUPLE4(STRING "module_parameter_port1", Parameter, <type tree>,
 *          <name+default>)
 * The name+default sub-tuple typically wraps a SymbolIdentifier with
 * an optional `=` default expression — pull the name and the default
 * if present.
 *)
let extract_module_param tok =
  let names = ref [] in
  walk (function
    | SymbolIdentifier id -> names := id :: !names
    | _ -> ()
  ) tok;
  match List.rev !names with
  | n :: _ -> Some n
  | [] -> None

(* ─── Module declarations ────────────────────────────────────────── *)

type module_decl = {
  m_name: string;
  m_params: string list;
  m_body: token;     (* the entire module token tree, used later *)
}

(* Locate every `module_or_interface_declaration1` node and pull the
 * module name + parameter port list. *)
let extract_modules root : module_decl list =
  let mod_nodes = collect_by
    (has_tag (prefix_is "module_or_interface_declaration")) root
  in
  List.filter_map (fun node ->
    let names = ref [] in
    walk (function SymbolIdentifier id -> names := id :: !names | _ -> ()) node;
    match List.rev !names with
    | mname :: _ ->
        let p_subs = collect_by
          (has_tag (prefix_is "module_parameter_port")) node in
        let params =
          List.filter_map extract_module_param p_subs
          |> List.sort_uniq compare
        in
        Some { m_name = mname; m_params = params; m_body = node }
    | [] -> None
  ) mod_nodes

(* ─── Instantiations + parameter overrides ───────────────────────── *)

type instantiation = {
  i_module: string;
  i_inst:   string;
  i_overrides: (string * string) list;
  (* Raw token of each override, kept for downstream resolution
   * (e.g. `pkg::name` refs that the basic value_of can't fold). *)
  i_overrides_tok: (string * token) list;
}

(* `parameter_value_byname1` shape:
 *   TUPLE6(STRING "parameter_value_byname1", <dot>, SymbolIdentifier name,
 *          LPAREN, <value_token>, RPAREN)
 *)
let extract_overrides_tok node =
  let ov_nodes = collect_by
    (has_tag (prefix_is "parameter_value_byname")) node
  in
  List.filter_map (fun n -> match n with
    | TUPLE6 (_, _, SymbolIdentifier param, _, value, _) ->
        Some (param, value)
    | _ -> None
  ) ov_nodes

let extract_overrides node =
  List.map (fun (k, v) -> (k, value_of v)) (extract_overrides_tok node)

(* `instantiation_base1` shape:
 *   TUPLE3(STRING "instantiation_base1",
 *          TUPLE3(STRING "unqualified_id1", SymbolIdentifier modname,
 *                 TUPLE5(STRING "parameters2", _, LPAREN, TLIST [overrides], RPAREN)),
 *          TLIST [instance_with_port_connections])
 *)
(* Find the instance name, which Verible wraps in a
 * `gate_instance_or_register_variable_list1` (or similar) tag.
 * Fall back to "?" if we can't find it. *)
let extract_inst_name node =
  let candidates = collect_by (has_tag (fun s ->
    prefix_is "gate_instance" s ||
    prefix_is "register_variable" s ||
    prefix_is "module_instance" s ||
    prefix_is "non_anonymous_gate_instance" s
  )) node in
  let rec first_id = function
    | SymbolIdentifier id -> Some id
    | TUPLE2 (a, b) -> (match first_id a with Some _ as r -> r
                        | None -> first_id b)
    | TUPLE3 (_, a, b) ->
        (match first_id a with Some _ as r -> r | None -> first_id b)
    | _ -> None
  in
  match candidates with
  | c :: _ ->
      let ids = ref [] in
      walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) c;
      (match List.rev !ids with
       | id :: _ -> Some id
       | [] -> first_id c)
  | [] -> None

(* Walk a token tree like `walk`, but when a conditional_generate_construct
 * node is reached, evaluate the if-condition (or case selector) in the
 * supplied scope and descend only into the live branch. Falls back to
 * walking everything when the condition can't be reduced.
 *
 * Knows the three shapes from Source_text_verible.mly:
 *   conditional_generate_construct1: TUPLE5(STRING tag, generate_if, item,
 *                                            Else, item)
 *   conditional_generate_construct2: TUPLE3(STRING tag, generate_if, item)
 *   conditional_generate_construct3: TUPLE7(STRING tag, Case, LPAREN,
 *                                            expression, RPAREN,
 *                                            generate_case_items, Endcase)
 *
 * `generate_if1` is TUPLE3(STRING tag, If, expression_in_parens). *)
let extract_if_cond = function
  | TUPLE3 (STRING tag, _, expr) when prefix_is "generate_if" tag -> Some expr
  | _ -> None

(* The forward-decl: walk_live needs Eval.eval_string and resolve_value
 * but those are defined further down. Refs broken at the chained call
 * sites: assigned in specialise_design before any walk_live runs. *)
let resolver_for_walk : (token -> string) ref = ref (fun _ -> "")
let evaluator_for_walk
    : ((string * int) list -> string -> int option) ref
    = ref (fun _ _ -> None)

let walk_live_debug = ref false

let rec walk_live scope f tok =
  let take_branch cond_expr_opt branches =
    let resolved =
      match cond_expr_opt with
      | None -> None
      | Some e ->
          let s = !resolver_for_walk e in
          if !walk_live_debug then
            Printf.eprintf "[walk_live] cond=%S → %s\n%!" s
              (match !evaluator_for_walk scope s with
               | Some n -> string_of_int n | None -> "?");
          !evaluator_for_walk scope s
    in
    match resolved, branches with
    | Some n, [_, body] when n <> 0 -> walk_live scope f body
    | Some 0, [_, _] -> ()           (* if only, condition false → skip *)
    | Some n, [_, then_b; _, else_b] ->
        walk_live scope f (if n <> 0 then then_b else else_b)
    | _ ->
        (* Can't decide — fall back to walking every branch. *)
        List.iter (fun (_, b) -> walk_live scope f b) branches
  in
  match tok with
  | TUPLE5 (STRING tag, gif, t_item, _, e_item)
    when prefix_is "conditional_generate_construct" tag ->
      f tok;
      take_branch (extract_if_cond gif) [(`T, t_item); (`E, e_item)]
  | TUPLE3 (STRING tag, gif, t_item)
    when prefix_is "conditional_generate_construct" tag ->
      f tok;
      take_branch (extract_if_cond gif) [(`T, t_item)]
  | _ ->
      (* Fall through to the same recursion shape walk uses. *)
      f tok;
      (match tok with
       | TUPLE2 (a, b) -> walk_live scope f a; walk_live scope f b
       | TUPLE3 (a, b, c) ->
           walk_live scope f a; walk_live scope f b; walk_live scope f c
       | TUPLE4 (a, b, c, d) ->
           List.iter (walk_live scope f) [a; b; c; d]
       | TUPLE5 (a, b, c, d, e) ->
           List.iter (walk_live scope f) [a; b; c; d; e]
       | TUPLE6 (a, b, c, d, e, f') ->
           List.iter (walk_live scope f) [a; b; c; d; e; f']
       | TUPLE7 (a, b, c, d, e, f', g) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g]
       | TUPLE8 (a, b, c, d, e, f', g, h) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h]
       | TUPLE9 (a, b, c, d, e, f', g, h, i) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i]
       | TUPLE10 (a, b, c, d, e, f', g, h, i, j) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i; j]
       | TUPLE11 (a, b, c, d, e, f', g, h, i, j, k) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i; j; k]
       | TUPLE12 (a, b, c, d, e, f', g, h, i, j, k, l) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i; j; k; l]
       | TUPLE13 (a, b, c, d, e, f', g, h, i, j, k, l, m) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i; j; k; l; m]
       | TUPLE14 (a, b, c, d, e, f', g, h, i, j, k, l, m, n) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i; j; k; l; m; n]
       | TUPLE15 (a, b, c, d, e, f', g, h, i, j, k, l, m, n, o) ->
           List.iter (walk_live scope f) [a; b; c; d; e; f'; g; h; i; j; k; l; m; n; o]
       | TLIST xs -> List.iter (walk_live scope f) xs
       | _ -> ())

let collect_live_by scope pred root =
  let acc = ref [] in
  walk_live scope (fun t -> if pred t then acc := t :: !acc) root;
  List.rev !acc

(* Tree transformer: replace every `function_declaration` and
 * `task_declaration` subtree with EMPTY_TOKEN. Used by
 * verible_to_behavioral.convert_module to keep function-local
 * variable declarations (e.g. `logic [63:0] out;` inside an SV
 * function body) from leaking into the enclosing module's signal
 * list. The function body is consumed separately by extract_functions. *)
let rec strip_function_decls tok =
  match tok with
  | TUPLE2 (STRING tag, _)
  | TUPLE3 (STRING tag, _, _)
  | TUPLE4 (STRING tag, _, _, _)
  | TUPLE5 (STRING tag, _, _, _, _)
  | TUPLE6 (STRING tag, _, _, _, _, _)
  | TUPLE7 (STRING tag, _, _, _, _, _, _)
  | TUPLE8 (STRING tag, _, _, _, _, _, _, _)
  | TUPLE9 (STRING tag, _, _, _, _, _, _, _, _)
  | TUPLE10 (STRING tag, _, _, _, _, _, _, _, _, _)
  | TUPLE11 (STRING tag, _, _, _, _, _, _, _, _, _, _)
  | TUPLE12 (STRING tag, _, _, _, _, _, _, _, _, _, _, _)
  | TUPLE13 (STRING tag, _, _, _, _, _, _, _, _, _, _, _, _)
  | TUPLE14 (STRING tag, _, _, _, _, _, _, _, _, _, _, _, _, _)
  | TUPLE15 (STRING tag, _, _, _, _, _, _, _, _, _, _, _, _, _, _)
    when prefix_is "function_declaration" tag
      || prefix_is "task_declaration" tag ->
      EMPTY_TOKEN
  | TUPLE2 (a, b) -> TUPLE2 (strip_function_decls a, strip_function_decls b)
  | TUPLE3 (a, b, c) ->
      TUPLE3 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c)
  | TUPLE4 (a, b, c, d) ->
      TUPLE4 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c, strip_function_decls d)
  | TUPLE5 (a, b, c, d, e) ->
      TUPLE5 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c, strip_function_decls d,
              strip_function_decls e)
  | TUPLE6 (a, b, c, d, e, f) ->
      TUPLE6 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c, strip_function_decls d,
              strip_function_decls e, strip_function_decls f)
  | TUPLE7 (a, b, c, d, e, f, g) ->
      TUPLE7 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c, strip_function_decls d,
              strip_function_decls e, strip_function_decls f,
              strip_function_decls g)
  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      TUPLE8 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c, strip_function_decls d,
              strip_function_decls e, strip_function_decls f,
              strip_function_decls g, strip_function_decls h)
  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      TUPLE9 (strip_function_decls a, strip_function_decls b,
              strip_function_decls c, strip_function_decls d,
              strip_function_decls e, strip_function_decls f,
              strip_function_decls g, strip_function_decls h,
              strip_function_decls i)
  | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
      TUPLE10 (strip_function_decls a, strip_function_decls b,
               strip_function_decls c, strip_function_decls d,
               strip_function_decls e, strip_function_decls f,
               strip_function_decls g, strip_function_decls h,
               strip_function_decls i, strip_function_decls j)
  | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
      TUPLE11 (strip_function_decls a, strip_function_decls b,
               strip_function_decls c, strip_function_decls d,
               strip_function_decls e, strip_function_decls f,
               strip_function_decls g, strip_function_decls h,
               strip_function_decls i, strip_function_decls j,
               strip_function_decls k)
  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      TUPLE12 (strip_function_decls a, strip_function_decls b,
               strip_function_decls c, strip_function_decls d,
               strip_function_decls e, strip_function_decls f,
               strip_function_decls g, strip_function_decls h,
               strip_function_decls i, strip_function_decls j,
               strip_function_decls k, strip_function_decls l)
  | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
      TUPLE13 (strip_function_decls a, strip_function_decls b,
               strip_function_decls c, strip_function_decls d,
               strip_function_decls e, strip_function_decls f,
               strip_function_decls g, strip_function_decls h,
               strip_function_decls i, strip_function_decls j,
               strip_function_decls k, strip_function_decls l,
               strip_function_decls m)
  | TUPLE14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) ->
      TUPLE14 (strip_function_decls a, strip_function_decls b,
               strip_function_decls c, strip_function_decls d,
               strip_function_decls e, strip_function_decls f,
               strip_function_decls g, strip_function_decls h,
               strip_function_decls i, strip_function_decls j,
               strip_function_decls k, strip_function_decls l,
               strip_function_decls m, strip_function_decls n)
  | TUPLE15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) ->
      TUPLE15 (strip_function_decls a, strip_function_decls b,
               strip_function_decls c, strip_function_decls d,
               strip_function_decls e, strip_function_decls f,
               strip_function_decls g, strip_function_decls h,
               strip_function_decls i, strip_function_decls j,
               strip_function_decls k, strip_function_decls l,
               strip_function_decls m, strip_function_decls n,
               strip_function_decls o)
  | TLIST xs -> TLIST (List.map strip_function_decls xs)
  | leaf -> leaf

(* Tree transformer: replace every dead conditional_generate branch
 * with EMPTY_TOKEN, leaving the live branch in place. Used by
 * verible_to_behavioral.convert_module to prune dead generate
 * branches from the body BEFORE the BIR-level extractors walk it,
 * so a popcount__W2 specialisation doesn't carry assigns from the
 * W==1 / W>=3 branches that fight the W==2 branch. *)
let rec prune_dead_generates scope tok =
  let take_branch cond_expr_opt branches =
    match cond_expr_opt with
    | None -> None
    | Some e ->
        let s = !resolver_for_walk e in
        !evaluator_for_walk scope s
  in
  let _ = take_branch in
  let take_branch cond_expr_opt _ =
    match cond_expr_opt with
    | None -> None
    | Some e ->
        let s = !resolver_for_walk e in
        !evaluator_for_walk scope s
  in
  match tok with
  (* Loop-generate construct (`for (genvar k = 0; cond; step) body`).
   * Unrolls the loop into N copies of the body, with `k` bound to
   * its iteration value in the recursive prune scope so any
   * conditional generate inside the body can resolve. Falls back
   * to a no-op walk if init/cond/step can't be evaluated. The
   * grammar shape is TUPLE13 (For, LPAREN, genvar_opt, ID, EQUALS,
   * init_expr, SEMICOLON, cond_expr, SEMICOLON, for_step, RPAREN,
   * body) so we destructure that here. Without this lzc's
   * `for (genvar level = 0; level < NumLevels; level++)` body —
   * which contains nested generate-ifs based on `level` and the
   * inner k — never had the dead branches pruned, leaving multiple
   * processes writing to the same sel_nodes/index_nodes. *)
  | TUPLE13 (STRING tag, _, _, _, gv_id, _, init, _, cond, _, step, _, body)
    when prefix_is "loop_generate_construct" tag ->
      let extract_id t =
        let id = ref None in
        walk (function
          | SymbolIdentifier s when !id = None -> id := Some s
          | _ -> ()) t;
        !id
      in
      let gv_name = extract_id gv_id in
      let resolve_int e =
        let s = !resolver_for_walk e in
        !evaluator_for_walk scope s
      in
      let step_delta name =
        match step with
        | TUPLE3 (STRING t, _, _) when prefix_is "inc_or_dec_expression" t ->
            (* `++name` / `name++` / `--name` / `name--`. Polarity
             * comes from the position of PLUS_PLUS / HYPHEN_HYPHEN. *)
            let s_pre = match step with
              | TUPLE3 (_, PLUS_PLUS, _) -> Some 1
              | TUPLE3 (_, HYPHEN_HYPHEN, _) -> Some (-1)
              | TUPLE3 (_, _, PLUS_PLUS) -> Some 1
              | TUPLE3 (_, _, HYPHEN_HYPHEN) -> Some (-1)
              | _ -> None
            in
            (match s_pre with
             | Some d -> Some d
             | None -> ignore name; None)
        | TUPLE4 (STRING t, _, _, rhs)
          when prefix_is "assignment_statement_no_expr" t ->
            (* `name = expr` — evaluate expr in current scope and
             * subtract the current name-value to get the delta. *)
            (match resolve_int rhs with
             | Some new_v ->
                 (match List.assoc_opt name scope with
                  | Some old_v -> Some (new_v - old_v)
                  | None -> None)
             | None -> None)
        | _ -> None
      in
      (* Substitute every reference to `name` (as a SymbolIdentifier)
       * with a decimal numeric leaf carrying the current iteration
       * value. Without this, the unrolled body still has the genvar
       * as an opaque identifier and downstream eval_int / extract_*
       * sees `k * 2 + 1` as a free variable expression. *)
      let rec subst_genvar name v t =
        match t with
        | SymbolIdentifier id when id = name ->
            TK_DecNumber (string_of_int v)
        | TUPLE2 (a, b) ->
            TUPLE2 (subst_genvar name v a, subst_genvar name v b)
        | TUPLE3 (a, b, c) ->
            TUPLE3 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c)
        | TUPLE4 (a, b, c, d) ->
            TUPLE4 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c, subst_genvar name v d)
        | TUPLE5 (a, b, c, d, e) ->
            TUPLE5 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c, subst_genvar name v d,
                    subst_genvar name v e)
        | TUPLE6 (a, b, c, d, e, f) ->
            TUPLE6 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c, subst_genvar name v d,
                    subst_genvar name v e, subst_genvar name v f)
        | TUPLE7 (a, b, c, d, e, f, g) ->
            TUPLE7 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c, subst_genvar name v d,
                    subst_genvar name v e, subst_genvar name v f,
                    subst_genvar name v g)
        | TUPLE8 (a, b, c, d, e, f, g, h) ->
            TUPLE8 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c, subst_genvar name v d,
                    subst_genvar name v e, subst_genvar name v f,
                    subst_genvar name v g, subst_genvar name v h)
        | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
            TUPLE9 (subst_genvar name v a, subst_genvar name v b,
                    subst_genvar name v c, subst_genvar name v d,
                    subst_genvar name v e, subst_genvar name v f,
                    subst_genvar name v g, subst_genvar name v h,
                    subst_genvar name v i)
        | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
            TUPLE10 (subst_genvar name v a, subst_genvar name v b,
                     subst_genvar name v c, subst_genvar name v d,
                     subst_genvar name v e, subst_genvar name v f,
                     subst_genvar name v g, subst_genvar name v h,
                     subst_genvar name v i, subst_genvar name v j)
        | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
            TUPLE11 (subst_genvar name v a, subst_genvar name v b,
                     subst_genvar name v c, subst_genvar name v d,
                     subst_genvar name v e, subst_genvar name v f,
                     subst_genvar name v g, subst_genvar name v h,
                     subst_genvar name v i, subst_genvar name v j,
                     subst_genvar name v k)
        | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
            TUPLE12 (subst_genvar name v a, subst_genvar name v b,
                     subst_genvar name v c, subst_genvar name v d,
                     subst_genvar name v e, subst_genvar name v f,
                     subst_genvar name v g, subst_genvar name v h,
                     subst_genvar name v i, subst_genvar name v j,
                     subst_genvar name v k, subst_genvar name v l)
        | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
            TUPLE13 (subst_genvar name v a, subst_genvar name v b,
                     subst_genvar name v c, subst_genvar name v d,
                     subst_genvar name v e, subst_genvar name v f,
                     subst_genvar name v g, subst_genvar name v h,
                     subst_genvar name v i, subst_genvar name v j,
                     subst_genvar name v k, subst_genvar name v l,
                     subst_genvar name v m)
        | TLIST xs -> TLIST (List.map (subst_genvar name v) xs)
        | leaf -> leaf
      in
      (match gv_name, resolve_int init with
       | Some name, Some init_v ->
           let max_iter = 256 in
           let unrolled = ref [] in
           let v = ref init_v in
           let live = ref true in
           let iter = ref 0 in
           while !live && !iter < max_iter do
             let scope' = (name, !v) :: scope in
             (* Evaluate condition in current scope *)
             let still_live =
               let s = !resolver_for_walk cond in
               match !evaluator_for_walk scope' s with
               | Some n -> n <> 0
               | None -> false  (* bail — can't decide *)
             in
             if still_live then begin
               let inst = subst_genvar name !v body in
               unrolled := prune_dead_generates scope' inst :: !unrolled;
               (* Advance v per step. *)
               match step_delta name with
               | Some d -> v := !v + d
               | None -> live := false
             end else
               live := false;
             incr iter
           done;
           (match List.rev !unrolled with
            | [] -> EMPTY_TOKEN
            | [x] -> x
            | xs -> TLIST xs)
       | _ ->
           (* Couldn't evaluate; fall through to default subtree
            * recursion so we at least walk into the body. *)
           TUPLE13 (STRING tag, EMPTY_TOKEN, EMPTY_TOKEN, EMPTY_TOKEN,
                    gv_id, EMPTY_TOKEN, init, EMPTY_TOKEN, cond,
                    EMPTY_TOKEN, step, EMPTY_TOKEN,
                    prune_dead_generates scope body))
  | TUPLE5 (STRING tag, gif, t_item, e_kw, e_item)
    when prefix_is "conditional_generate_construct" tag ->
      (match take_branch (extract_if_cond gif)
               [(`T, t_item); (`E, e_item)] with
       | Some n when n <> 0 ->
           prune_dead_generates scope t_item
       | Some _ ->
           prune_dead_generates scope e_item
       | None ->
           TUPLE5 (STRING tag, gif,
                   prune_dead_generates scope t_item, e_kw,
                   prune_dead_generates scope e_item))
  | TUPLE3 (STRING tag, gif, t_item)
    when prefix_is "conditional_generate_construct" tag ->
      (match take_branch (extract_if_cond gif) [(`T, t_item)] with
       | Some n when n <> 0 -> prune_dead_generates scope t_item
       | Some _ -> EMPTY_TOKEN
       | None -> TUPLE3 (STRING tag, gif,
                         prune_dead_generates scope t_item))
  | TUPLE2 (a, b) ->
      TUPLE2 (prune_dead_generates scope a,
              prune_dead_generates scope b)
  | TUPLE3 (a, b, c) ->
      TUPLE3 (prune_dead_generates scope a,
              prune_dead_generates scope b,
              prune_dead_generates scope c)
  | TUPLE4 (a, b, c, d) ->
      TUPLE4 (prune_dead_generates scope a,
              prune_dead_generates scope b,
              prune_dead_generates scope c,
              prune_dead_generates scope d)
  | TUPLE5 (a, b, c, d, e) ->
      TUPLE5 (prune_dead_generates scope a,
              prune_dead_generates scope b,
              prune_dead_generates scope c,
              prune_dead_generates scope d,
              prune_dead_generates scope e)
  | TUPLE6 (a, b, c, d, e, f) ->
      TUPLE6 (prune_dead_generates scope a, prune_dead_generates scope b,
              prune_dead_generates scope c, prune_dead_generates scope d,
              prune_dead_generates scope e, prune_dead_generates scope f)
  | TUPLE7 (a, b, c, d, e, f, g) ->
      TUPLE7 (prune_dead_generates scope a, prune_dead_generates scope b,
              prune_dead_generates scope c, prune_dead_generates scope d,
              prune_dead_generates scope e, prune_dead_generates scope f,
              prune_dead_generates scope g)
  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      TUPLE8 (prune_dead_generates scope a, prune_dead_generates scope b,
              prune_dead_generates scope c, prune_dead_generates scope d,
              prune_dead_generates scope e, prune_dead_generates scope f,
              prune_dead_generates scope g, prune_dead_generates scope h)
  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      TUPLE9 (prune_dead_generates scope a, prune_dead_generates scope b,
              prune_dead_generates scope c, prune_dead_generates scope d,
              prune_dead_generates scope e, prune_dead_generates scope f,
              prune_dead_generates scope g, prune_dead_generates scope h,
              prune_dead_generates scope i)
  | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
      TUPLE10 (prune_dead_generates scope a, prune_dead_generates scope b,
               prune_dead_generates scope c, prune_dead_generates scope d,
               prune_dead_generates scope e, prune_dead_generates scope f,
               prune_dead_generates scope g, prune_dead_generates scope h,
               prune_dead_generates scope i, prune_dead_generates scope j)
  | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
      TUPLE11 (prune_dead_generates scope a, prune_dead_generates scope b,
               prune_dead_generates scope c, prune_dead_generates scope d,
               prune_dead_generates scope e, prune_dead_generates scope f,
               prune_dead_generates scope g, prune_dead_generates scope h,
               prune_dead_generates scope i, prune_dead_generates scope j,
               prune_dead_generates scope k)
  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      TUPLE12 (prune_dead_generates scope a, prune_dead_generates scope b,
               prune_dead_generates scope c, prune_dead_generates scope d,
               prune_dead_generates scope e, prune_dead_generates scope f,
               prune_dead_generates scope g, prune_dead_generates scope h,
               prune_dead_generates scope i, prune_dead_generates scope j,
               prune_dead_generates scope k, prune_dead_generates scope l)
  | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
      TUPLE13 (prune_dead_generates scope a, prune_dead_generates scope b,
               prune_dead_generates scope c, prune_dead_generates scope d,
               prune_dead_generates scope e, prune_dead_generates scope f,
               prune_dead_generates scope g, prune_dead_generates scope h,
               prune_dead_generates scope i, prune_dead_generates scope j,
               prune_dead_generates scope k, prune_dead_generates scope l,
               prune_dead_generates scope m)
  | TUPLE14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) ->
      TUPLE14 (prune_dead_generates scope a, prune_dead_generates scope b,
               prune_dead_generates scope c, prune_dead_generates scope d,
               prune_dead_generates scope e, prune_dead_generates scope f,
               prune_dead_generates scope g, prune_dead_generates scope h,
               prune_dead_generates scope i, prune_dead_generates scope j,
               prune_dead_generates scope k, prune_dead_generates scope l,
               prune_dead_generates scope m, prune_dead_generates scope n)
  | TUPLE15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) ->
      TUPLE15 (prune_dead_generates scope a, prune_dead_generates scope b,
               prune_dead_generates scope c, prune_dead_generates scope d,
               prune_dead_generates scope e, prune_dead_generates scope f,
               prune_dead_generates scope g, prune_dead_generates scope h,
               prune_dead_generates scope i, prune_dead_generates scope j,
               prune_dead_generates scope k, prune_dead_generates scope l,
               prune_dead_generates scope m, prune_dead_generates scope n,
               prune_dead_generates scope o)
  | TLIST xs -> TLIST (List.map (prune_dead_generates scope) xs)
  | leaf -> leaf

(* Pull the begin-label (if any) from a `begin_rule` node:
 *   begin1:    TUPLE3("begin1", Begin, label_opt)
 *   label_opt: TUPLE3("label_opt1", COLON, symbol_or_label) | EMPTY *)
let extract_begin_label = function
  | TUPLE3 (STRING t, _, lbl) when prefix_is "begin1" t ->
      let ids = ref [] in
      walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) lbl;
      (match List.rev !ids with x :: _ -> Some x | _ -> None)
  | _ -> None

(* Pull the label from a `generate_block` shape:
 *   generate_block1: TUPLE4(tag, begin_rule, item_list, end_rule)
 *   generate_block2: TUPLE6(tag, label_id, COLON, Begin, item_list, end_rule)
 * Either returns the begin-label name or None when the block is
 * unlabeled. SV-2017 requires synthesis to use this label as a
 * hierarchical-name segment, e.g. `if (...) begin : non_leaf_node`
 * makes the contained `left_child` instance hierarchically known as
 * `non_leaf_node.left_child`. *)
let extract_block_label = function
  | TUPLE4 (STRING t, b, _, _) when prefix_is "generate_block1" t ->
      extract_begin_label b
  | TUPLE6 (STRING t, id_node, _, _, _, _) when prefix_is "generate_block2" t ->
      let ids = ref [] in
      walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) id_node;
      (match List.rev !ids with x :: _ -> Some x | _ -> None)
  | _ -> None

(* Walk like walk_live but additionally maintain a stack of enclosing
 * generate-block labels. Calls `f label_stack node` on every visit.
 * The stack is in outer-to-inner order; `String.concat "."` over it
 * produces the SV hierarchical-name prefix that yosys-slang and
 * synthesis tools use for instances inside labeled generate blocks. *)
let rec walk_live_labeled scope label_stack f tok =
  let take_branch cond_expr_opt branches =
    let resolved =
      match cond_expr_opt with
      | None -> None
      | Some e ->
          let s = !resolver_for_walk e in
          !evaluator_for_walk scope s
    in
    match resolved, branches with
    | Some n, [_, body] when n <> 0 ->
        walk_live_labeled scope label_stack f body
    | Some 0, [_, _] -> ()
    | Some n, [_, then_b; _, else_b] ->
        walk_live_labeled scope label_stack f
          (if n <> 0 then then_b else else_b)
    | _ ->
        List.iter (fun (_, b) -> walk_live_labeled scope label_stack f b)
          branches
  in
  let push_block_label_then_recurse t =
    let new_stack = match extract_block_label t with
      | Some lbl -> label_stack @ [lbl]
      | None -> label_stack
    in
    f new_stack t;
    (match t with
     | TUPLE2 (a, b) ->
         walk_live_labeled scope new_stack f a;
         walk_live_labeled scope new_stack f b
     | TUPLE3 (a, b, c) ->
         List.iter (walk_live_labeled scope new_stack f) [a; b; c]
     | TUPLE4 (a, b, c, d) ->
         List.iter (walk_live_labeled scope new_stack f) [a; b; c; d]
     | TUPLE5 (a, b, c, d, e) ->
         List.iter (walk_live_labeled scope new_stack f) [a; b; c; d; e]
     | TUPLE6 (a, b, c, d, e, f') ->
         List.iter (walk_live_labeled scope new_stack f) [a; b; c; d; e; f']
     | TUPLE7 (a, b, c, d, e, f', g) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g]
     | TUPLE8 (a, b, c, d, e, f', g, h) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h]
     | TUPLE9 (a, b, c, d, e, f', g, h, i) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i]
     | TUPLE10 (a, b, c, d, e, f', g, h, i, j) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i; j]
     | TUPLE11 (a, b, c, d, e, f', g, h, i, j, k) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i; j; k]
     | TUPLE12 (a, b, c, d, e, f', g, h, i, j, k, l) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i; j; k; l]
     | TUPLE13 (a, b, c, d, e, f', g, h, i, j, k, l, m) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i; j; k; l; m]
     | TUPLE14 (a, b, c, d, e, f', g, h, i, j, k, l, m, n) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i; j; k; l; m; n]
     | TUPLE15 (a, b, c, d, e, f', g, h, i, j, k, l, m, n, o) ->
         List.iter (walk_live_labeled scope new_stack f)
           [a; b; c; d; e; f'; g; h; i; j; k; l; m; n; o]
     | TLIST xs -> List.iter (walk_live_labeled scope new_stack f) xs
     | _ -> ())
  in
  match tok with
  | TUPLE5 (STRING tag, gif, t_item, _, e_item)
    when prefix_is "conditional_generate_construct" tag ->
      f label_stack tok;
      take_branch (extract_if_cond gif) [(`T, t_item); (`E, e_item)]
  | TUPLE3 (STRING tag, gif, t_item)
    when prefix_is "conditional_generate_construct" tag ->
      f label_stack tok;
      take_branch (extract_if_cond gif) [(`T, t_item)]
  | _ -> push_block_label_then_recurse tok

let extract_instantiations ?(scope = []) root : instantiation list =
  let acc = ref [] in
  walk_live_labeled scope [] (fun label_stack node ->
    if has_tag (prefix_is "instantiation_base") node then begin
      let mod_name = ref None in
      walk (function
        | SymbolIdentifier id when !mod_name = None -> mod_name := Some id
        | _ -> ()
      ) node;
      match !mod_name with
      | None -> ()
      | Some mn ->
          let leaf = match extract_inst_name node with
            | Some s -> s
            | None -> "?"
          in
          (* Prepend any enclosing labeled-generate-block segments so
           * the inst_name matches what synthesis and yosys-slang use,
           * e.g. `non_leaf_node.left_child`. *)
          let in_ = String.concat "."
                      (label_stack @ [leaf]) in
          acc := {
            i_module = mn;
            i_inst = in_;
            i_overrides = extract_overrides node;
            i_overrides_tok = extract_overrides_tok node;
          } :: !acc
    end
  ) root;
  List.rev !acc

(* ─── Package declarations (must precede specialise_design) ──────── *)

(* Symbolic value for a package member. *)
type pvalue =
  | PInt of int
  | PStr of string
  | PExpr of token

let rec int_of_pvalue = function
  | PInt n -> Some n
  | PStr s ->
      (try
         let s = String.trim s in
         let s =
           try
             let i = String.index s '\'' in
             let rest = String.sub s (i + 1) (String.length s - i - 1) in
             match rest.[0] with
             | 'd' | 'D' -> String.sub rest 1 (String.length rest - 1)
             | 'h' | 'H' -> "0x" ^ String.sub rest 1 (String.length rest - 1)
             | 'b' | 'B' -> "0b" ^ String.sub rest 1 (String.length rest - 1)
             | 'o' | 'O' -> "0o" ^ String.sub rest 1 (String.length rest - 1)
             | _ -> rest
           with Not_found -> s
         in
         Some (int_of_string s)
       with _ -> None)
  | PExpr _ -> None

and string_of_pvalue = function
  | PInt n -> string_of_int n
  | PStr s -> s
  | PExpr t -> value_of t

type package_decl = {
  pkg_name: string;
  pkg_params: (string * pvalue) list;
  pkg_body: token;
}

let extract_pvalue_from_param_decl node =
  let assigns = collect_by (has_tag (prefix_is "trailing_assign")) node in
  match assigns with
  | a :: _ ->
      let leaves = ref [] in
      walk (function
        | TK_DecNumber n | TK_UnBasedNumber n -> leaves := n :: !leaves
        | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n ->
            leaves := n :: !leaves
        | SymbolIdentifier id -> leaves := id :: !leaves
        | _ -> ()
      ) a;
      (match List.rev !leaves with
       | v :: _ ->
           (match int_of_pvalue (PStr v) with
            | Some n -> PInt n
            | None -> PStr v)
       | _ -> PExpr a)
  | [] -> PExpr node

let extract_packages root : package_decl list =
  let pkg_nodes = collect_by
    (has_tag (prefix_is "package_declaration")) root
  in
  List.filter_map (fun node ->
    let pname = ref None in
    walk (function
      | SymbolIdentifier id when !pname = None -> pname := Some id
      | _ -> ()) node;
    match !pname with
    | None -> None
    | Some n ->
        let param_nodes = collect_by
          (has_tag (prefix_is "any_param_declaration")) node in
        let params = List.filter_map (fun pn ->
          let id_subs = collect_by
            (has_tag (prefix_is "param_type_followed_by_id")) pn in
          let pname = match id_subs with
            | s :: _ ->
                let ids = ref [] in
                walk (function SymbolIdentifier id ->
                              ids := id :: !ids | _ -> ()) s;
                (match List.rev !ids with
                 | last :: _ -> Some last
                 | [] -> None)
            | [] -> None
          in
          match pname with
          | None -> None
          | Some name -> Some (name, extract_pvalue_from_param_decl pn)
        ) param_nodes in
        Some { pkg_name = n; pkg_params = params; pkg_body = node }
  ) pkg_nodes

(* `pkg::name` lookup. Returns the pvalue if known. *)
let resolve_pkg_ref (pkgs : package_decl list) ~pkg ~name =
  match List.find_opt (fun p -> p.pkg_name = pkg) pkgs with
  | None -> None
  | Some p -> List.assoc_opt name p.pkg_params

(* ─── Symbolic-execution domain (Verilator V3Const-style) ────────── *)

(* Lattice domain for constant function values. Mirrors Verilator's
 * V3Number for ints + a structured tag for typed-config-parameter
 * struct/array returns. SVUnknown = lattice top, no info. *)
type sv_value =
  | SVInt    of int
  | SVStruct of (string * sv_value) list
  | SVArray  of sv_value list
  | SVUnknown

let rec int_of_sv = function
  | SVInt n -> Some n
  | _ -> None
let _ = int_of_sv

(* SV functions discovered in the parse tree. Body kept as a token —
 * we walk it when called. *)
type sv_function = {
  fn_name: string;
  fn_args: string list;
  fn_body: token;
}

(* Module-level struct table, shared between Eval (which reads it
 * for `X.Y` field-access lookup) and the const-fn machinery (which
 * populates it from top-level parameter defaults and pushes/pops
 * function-arg bindings during eval_function). Defined here so
 * eval_function can reference it before Eval is declared. *)
let struct_table : (string, sv_value) Hashtbl.t = Hashtbl.create 16

(* Walk the parse tree for `function_declaration` nodes. Pulls the
 * function name (first SymbolIdentifier inside the return-type-and-id
 * subtree), the formal-parameter names from tf_port_list, and the
 * body token (last child before `Endfunction`). Conservative: drops
 * any function we can't extract cleanly. *)
let extract_functions root : sv_function list =
  let nodes = collect_by
    (has_tag (prefix_is "function_declaration")) root in
  (* Pull function name from the return-type-and-id subtree at
   * TUPLE11 position 4 (function_declaration1's shape). The
   * data_type form of function_return_type_and_id is a grammar
   * passthrough with no tag, so we can't use prefix_is here —
   * destructure directly and take the LAST identifier in the
   * subtree (the name comes after the type). *)
  let return_subtree n =
    match n with
    | TUPLE11 (_, _, _, ret, _, _, _, _, _, _, _) -> Some ret
    | TUPLE9  (_, _, _, ret, _, _, _, _, _) -> Some ret
    | TUPLE8  (_, _, _, ret, _, _, _, _) -> Some ret
    | _ -> None
  in
  List.filter_map (fun n ->
    let fname = match return_subtree n with
      | None -> None
      | Some r ->
          let ids = ref [] in
          walk (function
            | SymbolIdentifier id -> ids := id :: !ids
            | _ -> ()) r;
          (* !ids is reverse-source-order, so the head is the LAST. *)
          (match !ids with last :: _ -> Some last | [] -> None)
    in
    let arg_nodes = collect_by
      (has_tag (prefix_is "tf_port_item")) n in
    let args = List.filter_map (fun a ->
      let ids = ref [] in
      walk (function
        | SymbolIdentifier id -> ids := id :: !ids
        | _ -> ()) a;
      match !ids with last :: _ -> Some last | _ -> None
    ) arg_nodes in
    match fname with
    | None -> None
    | Some name -> Some { fn_name = name; fn_args = args; fn_body = n }
  ) nodes

(* Walk an assignment_pattern_expression / assignment_pattern token
 * and yield an SVStruct of (field-name, value) pairs. Recognises
 *   '{ FIELD: <expr>, FIELD: <expr>, ... }
 * (assignment_pattern2 → structure_or_array_pattern_expression1 list).
 * Field values that don't fold to a literal int stay SVUnknown
 * (still useful — at least the field is named). *)
let rec extract_struct_literal pkgs tok : sv_value option =
  let pairs = collect_by
    (has_tag (prefix_is "structure_or_array_pattern_expression")) tok in
  if pairs = [] then None
  else
    let fields = List.filter_map (fun p ->
      match p with
      | TUPLE4 (_, key, _, value) ->
          (* Key: typically a SymbolIdentifier wrapped in unqualified_id. *)
          let kname = ref None in
          walk (function
            | SymbolIdentifier id -> kname := Some id
            | _ -> ()) key;
          (match !kname with
           | None -> None
           | Some name ->
               let v = resolve_arg_to_sv pkgs value in
               Some (name, v))
      | _ -> None
    ) pairs in
    if fields = [] then None
    else Some (SVStruct fields)

(* Look up `name` as a localparam across every package, recurse into
 * its RHS. Caller is responsible for ensuring the search isn't
 * ambiguous (CVA6's config identifiers are uniquely named). *)
and lookup_pkg_localparam pkgs name : sv_value =
  let rec scan = function
    | [] -> SVUnknown
    | p :: rest ->
        let lps = collect_by
          (has_tag (prefix_is "any_param_declaration")) p.pkg_body in
        let matched = List.find_opt (fun lp ->
          let ids = ref [] in
          let id_subs = collect_by
            (has_tag (prefix_is "param_type_followed_by_id")) lp in
          List.iter (fun s ->
            walk (function
              | SymbolIdentifier id -> ids := id :: !ids
              | _ -> ()) s) id_subs;
          match !ids with last :: _ -> last = name | _ -> false
        ) lps in
        (match matched with
         | Some lp ->
             let rhs = collect_by
               (has_tag (prefix_is "trailing_assign")) lp in
             (match rhs with
              | r :: _ -> resolve_arg_to_sv pkgs r
              | [] -> scan rest)
         | None -> scan rest)
  in scan pkgs

(* Resolve a function-call argument or struct-literal field value
 * to an sv_value. Handles:
 *  - struct literal `'{...}`
 *  - integer literal
 *  - bare identifier (looked up across all packages' localparams)
 *  - `pkg::name` qualified reference
 *  - cast wrappers like `unsigned'(...)` are transparent — strip
 *    them and try the inner expression
 * Returns SVUnknown when nothing matches. *)
and resolve_arg_to_sv pkgs tok : sv_value =
  match extract_struct_literal pkgs tok with
  | Some v -> v
  | None ->
      let s = String.trim (deep_string_of_token tok) in
      (* Plain integer literal? *)
      (match int_of_string_opt s with
       | Some n -> SVInt n
       | None ->
           (* `pkg :: name` reference to a struct localparam? *)
           let qrefs = collect_by (function
             | TUPLE4 (STRING tag, _, _, _)
               when prefix_is "qualified_id" tag -> true
             | _ -> false) tok in
           match qrefs with
           | TUPLE4 (_,
                     TUPLE3 (_, SymbolIdentifier pkg, _), _,
                     TUPLE3 (_, SymbolIdentifier name, _)) :: _ ->
               (match List.find_opt (fun p -> p.pkg_name = pkg) pkgs with
                | Some _ ->
                    (* Restrict to that one package by name. *)
                    let one_pkg =
                      List.filter (fun p -> p.pkg_name = pkg) pkgs in
                    lookup_pkg_localparam one_pkg name
                | None -> SVUnknown)
           | _ ->
               (* No qualified ref — last resort: walk the token for
                * a single bare SymbolIdentifier and try cross-package
                * localparam lookup. Catches `unsigned'(NAME)` cast
                * wrappers and bare `NAME` constants alike. *)
               let ids = ref [] in
               walk (function
                 | SymbolIdentifier id -> ids := id :: !ids
                 | _ -> ()) tok;
               (match !ids with
                | [name] -> lookup_pkg_localparam pkgs name
                | _ -> SVUnknown))

(* Evaluate a constant function call. Walks the body collecting
 * `<local>.<field> = <expr>` assignments into an SVStruct, and
 * `<local> = <expr>` assignments into a plain SVInt. Returns the
 * value of the local matching the function name (the SV convention
 * for return) or the value of the last `return` statement.
 *
 * Now handles arg substitution: each formal binds to its actual
 * sv_value, with struct-typed args feeding a tiny resolver inside
 * eval_int_expr that lets `<arg>.<field>` references in the body
 * fold to integer field values. *)
let eval_function ~functions ~lookup_int ~pkgs
                  (fn : sv_function) (args : sv_value list) : sv_value =
  let _ = functions in
  let _ = pkgs in
  (* Two parallel scopes: struct-typed args go into a per-call
   * field-resolver hashtable, int-typed args go into the integer
   * scope. Keeps the existing int eval cheap; struct field access
   * inside the body redirects via the call-local hashtable. *)
  let call_struct_table : (string, sv_value) Hashtbl.t =
    Hashtbl.create 4 in
  let bindings = try List.combine fn.fn_args args with _ -> [] in
  let scope : (string * int) list ref =
    ref (List.filter_map (fun (n, v) ->
           match v with SVInt i -> Some (n, i) | _ -> None
         ) bindings) in
  List.iter (fun (n, v) ->
    match v with
    | SVStruct _ -> Hashtbl.replace call_struct_table n v
    | _ -> ()
  ) bindings;
  (* During body evaluation we want struct-field accesses on the
   * formals (e.g. `CVA6Cfg.XLEN` where CVA6Cfg is the function arg)
   * to resolve through call_struct_table. The simplest hookup is
   * to push call_struct_table into the shared struct_table for the
   * duration of the call, then restore. Saves threading another
   * scope through Eval. *)
  let saved =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) struct_table [] in
  Hashtbl.iter (fun k v -> Hashtbl.replace struct_table k v)
    call_struct_table;
  let restore () =
    Hashtbl.clear struct_table;
    List.iter (fun (k, v) -> Hashtbl.replace struct_table k v) saved
  in
  let eval_int_expr tok : sv_value =
    let s = deep_string_of_token tok in
    match lookup_int !scope s with
    | Some n -> SVInt n
    | None -> SVUnknown
  in
  (* Track the canonical struct-build local. *)
  let struct_acc : (string * sv_value) list ref = ref [] in
  let returned : sv_value option ref = ref None in
  (* Recognise `<id>.<field> = <expr>` from an assignment_statement
   * subtree. Returns Some (id, field, rhs_token) when matched. *)
  let try_field_assign n =
    match n with
    | TUPLE4 (STRING tag, lp, _, rhs)
      when prefix_is "assignment_statement_no_expr" tag ->
        let ids = ref [] in
        walk (function
          | SymbolIdentifier id -> ids := id :: !ids
          | _ -> ()) lp;
        (match List.rev !ids with
         | base :: field :: _ -> Some (base, field, rhs)
         | _ -> None)
    | _ -> None
  in
  let try_return n =
    match n with
    | TUPLE4 (STRING tag, _, e, _)
      when prefix_is "jump_statement" tag -> Some e
    | _ -> None
  in
  walk (fun stmt ->
    if !returned <> None then () else
    match try_field_assign stmt with
    | Some (_base, field, rhs) ->
        let v = eval_int_expr rhs in
        struct_acc := (field, v) :: !struct_acc
    | None ->
        match try_return stmt with
        | Some e ->
            (* If the return references the struct local, hand it back. *)
            let s = deep_string_of_token e in
            let id_only = String.trim s in
            if List.exists (fun (f, _) -> id_only = f) !struct_acc then ()
            else (match lookup_int !scope s with
                  | Some n -> returned := Some (SVInt n)
                  | None -> ());
            (* Fall through to also publish struct *)
        | None -> ()
  ) fn.fn_body;
  let _ = scope in  (* suppress warn — used inside eval_int_expr *)
  let result =
    match !returned with
    | Some v -> v
    | None when !struct_acc <> [] ->
        let dedup =
          List.fold_left (fun acc (k, v) ->
            if List.mem_assoc k acc then acc else (k, v) :: acc
          ) [] (List.rev !struct_acc) in
        SVStruct (List.rev dedup)
    | None -> SVUnknown
  in
  restore ();
  result

(* Resolve a parameter default to an sv_value, evaluating function
 * calls if recognised. Looks for the call's first arg in the parse
 * tree (function_call_or_method node) and resolves it via
 * resolve_arg_to_sv before passing to eval_function. *)
let resolve_param_default ~functions ~lookup_int ~pkgs ~tok : sv_value =
  let s = deep_string_of_token tok in
  let s = String.trim s in
  match String.index_opt s '(' with
  | None ->
      (match lookup_int [] s with
       | Some n -> SVInt n | None -> SVUnknown)
  | Some lp ->
      let prefix = String.trim (String.sub s 0 lp) in
      let bare =
        match String.rindex_opt prefix ':' with
        | Some i -> String.trim (String.sub prefix (i + 1)
                                  (String.length prefix - i - 1))
        | None -> prefix
      in
      (match List.find_opt (fun f -> f.fn_name = bare) functions with
       | Some fn ->
           (* Walk tok for argument expressions. The grammar wraps a
            * call's args in `call_base1`: TUPLE4(STRING, LPAREN,
            * argument_list_opt, RPAREN). For our single-arg case we
            * take the argument_list_opt position (index 2) and walk
            * it to find the first expression. *)
           let call_subs = collect_by (function
             | TUPLE4 (STRING tag, _, _, _)
               when prefix_is "call_base" tag -> true
             | _ -> false) tok in
           let args =
             match call_subs with
             | TUPLE4 (_, _, arg_node, _) :: _ ->
                 [resolve_arg_to_sv pkgs arg_node]
             | _ -> []
           in
           eval_function ~functions ~lookup_int ~pkgs fn args
       | None ->
           (match lookup_int [] s with
            | Some n -> SVInt n | None -> SVUnknown))

(* Resolve an override expression to a printable string, folding
 * `pkg::name` references through the package table when possible.
 * Returns the deep stringification by default so the constant
 * evaluator sees the full expression text — falling back to the
 * single-leaf value_of only for trivially-wrapped scalars. *)
let rec resolve_value (pkgs : package_decl list) tok =
  let try_qualified t =
    match t with
    | TUPLE4 (STRING tag,
              TUPLE3 (STRING tg1, SymbolIdentifier pkg, _),
              _,
              TUPLE3 (STRING tg2, SymbolIdentifier name, _))
      when prefix_is "qualified_id" tag
        && prefix_is "unqualified_id" tg1
        && prefix_is "unqualified_id" tg2 ->
        Option.map string_of_pvalue (resolve_pkg_ref pkgs ~pkg ~name)
    | _ -> None
  in
  match try_qualified tok with
  | Some v -> v
  | None ->
      (* Substitute every `pkg::name` reference inside the expression
       * with its resolved string by string-replacement on the deep
       * rendering. Non-resolvable refs survive as-is and the
       * evaluator just returns None for them. *)
      let raw = deep_string_of_token tok in
      let qrefs = collect_by (function
        | TUPLE4 (STRING tag, _, _, _) when prefix_is "qualified_id" tag -> true
        | _ -> false) tok in
      List.fold_left (fun acc qr ->
        match qr with
        | TUPLE4 (_,
                  TUPLE3 (_, SymbolIdentifier pkg, _),
                  _,
                  TUPLE3 (_, SymbolIdentifier name, _)) ->
            (match resolve_pkg_ref pkgs ~pkg ~name with
             | None -> acc
             | Some pv ->
                 let pat = pkg ^ " :: " ^ name in
                 let r = string_of_pvalue pv in
                 (* Naive substring replace; OK for these short tags. *)
                 let rec rep s =
                   match String.index_opt s pat.[0] with
                   | None -> s
                   | Some _ ->
                       let lp = String.length pat and ls = String.length s in
                       let rec scan i =
                         if i + lp > ls then s
                         else if String.sub s i lp = pat then
                           String.sub s 0 i ^ r ^
                           rep (String.sub s (i + lp) (ls - i - lp))
                         else scan (i + 1)
                       in scan 0
                 in
                 rep acc)
        | _ -> acc
      ) raw qrefs

let _ = resolve_value
let _ = int_of_pvalue

(* ─── Constant expression evaluator ──────────────────────────────── *)

(* Recursive-descent evaluator over the `value_of` / `resolve_value`
 * string output. Substitutes identifiers from `scope`, folds the
 * standard SV integer ops (`+ - * / % << >>`), and a few system
 * functions (`$clog2`, `$bits`, `$signed`, `$unsigned`). Returns
 * None on unknown identifiers / unsupported operators — callers fall
 * back to the original string then. Required so that recursive
 * instantiations like `popcount #(.INPUT_WIDTH(PaddedWidth/2))` (where
 * PaddedWidth is a localparam computed from INPUT_WIDTH) get
 * specialised through the recursion tower instead of bottoming out at
 * the first level. *)
module Eval = struct
  type tok =
    | TNum of int | TId of string
    | TLP | TRP | TComma | TDot
    | TPlus | TMinus | TStar | TPow | TSlash | TPercent
    | TShl | TShr
    | TEq | TNeq | TLt | TLe | TGt | TGe
    | TLAnd | TLOr | TBang
    | TQuestion | TColon
    | TDollar of string

  (* Aliased forward-declared module-level table; see comment at
   * the file's top. parse_atom consults this for "X.Y" field
   * lookups that don't resolve via plain scope. *)
  let struct_table = struct_table

  let tokenize s =
    let n = String.length s in
    let i = ref 0 in
    let out = ref [] in
    let push t = out := t :: !out in
    let is_id_start c =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
    let is_id c = is_id_start c || (c >= '0' && c <= '9') in
    let is_dig c = c >= '0' && c <= '9' in
    while !i < n do
      let c = s.[!i] in
      match c with
      | ' ' | '\t' | '\n' | '\r' -> incr i
      | '(' -> push TLP; incr i
      | ')' -> push TRP; incr i
      | ',' -> push TComma; incr i
      | '.' -> push TDot; incr i
      | '?' -> push TQuestion; incr i
      | ':' when !i + 1 < n && s.[!i + 1] = ':' -> incr i; incr i  (* `::` already handled at deep_string level *)
      | ':' -> push TColon; incr i
      | '+' -> push TPlus; incr i
      | '-' -> push TMinus; incr i
      | '*' when !i + 1 < n && s.[!i + 1] = '*' -> push TPow; i := !i + 2
      | '*' -> push TStar; incr i
      | '/' -> push TSlash; incr i
      | '%' -> push TPercent; incr i
      | '<' when !i + 1 < n && s.[!i + 1] = '<' -> push TShl; i := !i + 2
      | '>' when !i + 1 < n && s.[!i + 1] = '>' -> push TShr; i := !i + 2
      | '=' when !i + 1 < n && s.[!i + 1] = '=' -> push TEq; i := !i + 2
      | '!' when !i + 1 < n && s.[!i + 1] = '=' -> push TNeq; i := !i + 2
      | '<' when !i + 1 < n && s.[!i + 1] = '=' -> push TLe; i := !i + 2
      | '>' when !i + 1 < n && s.[!i + 1] = '=' -> push TGe; i := !i + 2
      | '<' -> push TLt; incr i
      | '>' -> push TGt; incr i
      | '&' when !i + 1 < n && s.[!i + 1] = '&' -> push TLAnd; i := !i + 2
      | '|' when !i + 1 < n && s.[!i + 1] = '|' -> push TLOr; i := !i + 2
      | '!' -> push TBang; incr i
      | '$' ->
          let j = ref (!i + 1) in
          while !j < n && is_id s.[!j] do incr j done;
          push (TDollar (String.sub s (!i + 1) (!j - !i - 1)));
          i := !j
      | '\'' ->
          (* `'0`, `'1`, `'x`, `'z` — bare unsized literals. *)
          if !i + 1 < n then begin
            let nc = s.[!i + 1] in
            (match nc with
             | '0' -> push (TNum 0)
             | '1' -> push (TNum 1)
             | _   -> push (TNum 0));
            i := !i + 2
          end else incr i
      | _ when is_dig c ->
          let j = ref !i in
          while !j < n && is_dig s.[!j] do incr j done;
          let lead = String.sub s !i (!j - !i) in
          if !j < n && s.[!j] = '\'' && !j + 1 < n then begin
            (* sized literal: <width>'<base><digits> *)
            let base = Char.lowercase_ascii s.[!j + 1] in
            let k = ref (!j + 2) in
            while !k < n &&
                  (let cc = s.[!k] in
                   is_dig cc || (cc >= 'a' && cc <= 'f')
                             || (cc >= 'A' && cc <= 'F') || cc = '_')
            do incr k done;
            let digits =
              String.concat "" (String.split_on_char '_'
                (String.sub s (!j + 2) (!k - !j - 2))) in
            let v =
              try
                match base with
                | 'h' -> int_of_string ("0x" ^ digits)
                | 'b' -> int_of_string ("0b" ^ digits)
                | 'o' -> int_of_string ("0o" ^ digits)
                | _   -> int_of_string digits
              with _ -> 0
            in
            push (TNum v); i := !k
          end else begin
            push (TNum (try int_of_string lead with _ -> 0));
            i := !j
          end
      | _ when is_id_start c ->
          let j = ref !i in
          while !j < n && is_id s.[!j] do incr j done;
          push (TId (String.sub s !i (!j - !i)));
          i := !j
      | _ -> incr i  (* skip unknown punctuation *)
    done;
    List.rev !out

  let bool_int b = if b then 1 else 0

  let rec parse_expr scope toks = parse_ternary scope toks
  and parse_ternary scope toks =
    let c, t = parse_lor scope toks in
    match t with
    | TQuestion :: rest ->
        let then_v, t' = parse_ternary scope rest in
        (match t' with
         | TColon :: rest' ->
             let else_v, t'' = parse_ternary scope rest' in
             (match c with
              | Some n ->
                  if n <> 0 then (then_v, t'') else (else_v, t'')
              | None -> (None, t''))
         | _ -> (None, t'))
    | _ -> (c, t)
  and parse_lor scope toks =
    let l, t = parse_land scope toks in
    let rec loop l = function
      | TLOr :: rest ->
          let r, t' = parse_land scope rest in
          (match l, r with
           | Some a, Some b -> loop (Some (bool_int (a <> 0 || b <> 0))) t'
           | _ -> loop None t')
      | rest -> (l, rest)
    in loop l t
  and parse_land scope toks =
    let l, t = parse_eq scope toks in
    let rec loop l = function
      | TLAnd :: rest ->
          let r, t' = parse_eq scope rest in
          (match l, r with
           | Some a, Some b -> loop (Some (bool_int (a <> 0 && b <> 0))) t'
           | _ -> loop None t')
      | rest -> (l, rest)
    in loop l t
  and parse_eq scope toks =
    let l, t = parse_rel scope toks in
    let rec loop l = function
      | TEq  :: rest ->
          let r, t' = parse_rel scope rest in
          (match l, r with Some a, Some b -> loop (Some (bool_int (a = b))) t'
                         | _ -> loop None t')
      | TNeq :: rest ->
          let r, t' = parse_rel scope rest in
          (match l, r with Some a, Some b -> loop (Some (bool_int (a <> b))) t'
                         | _ -> loop None t')
      | rest -> (l, rest)
    in loop l t
  and parse_rel scope toks =
    let l, t = parse_add scope toks in
    let rec loop l = function
      | TLt :: rest -> rel scope (<)  l rest loop
      | TLe :: rest -> rel scope (<=) l rest loop
      | TGt :: rest -> rel scope (>)  l rest loop
      | TGe :: rest -> rel scope (>=) l rest loop
      | rest -> (l, rest)
    in loop l t
  and rel scope op l rest cont =
    let r, t = parse_add scope rest in
    (match l, r with
     | Some a, Some b -> cont (Some (bool_int (op a b))) t
     | _ -> cont None t)
  and parse_add scope toks =
    let l, t = parse_mul scope toks in
    let rec loop l = function
      | TPlus  :: rest -> binop scope (+)   l rest loop
      | TMinus :: rest -> binop scope (-)   l rest loop
      | TShl   :: rest -> binop scope (lsl) l rest loop
      | TShr   :: rest -> binop scope (lsr) l rest loop
      | rest -> (l, rest)
    in loop l t
  and parse_mul scope toks =
    let l, t = parse_pow scope toks in
    let rec loop l = function
      | TStar    :: rest -> binop_pow scope ( * ) l rest loop
      | TSlash   :: rest -> safe_binop_pow scope (/) l rest loop
      | TPercent :: rest -> safe_binop_pow scope (mod) l rest loop
      | rest -> (l, rest)
    in loop l t
  and binop_pow scope op l rest cont =
    let r, t = parse_pow scope rest in
    (match l, r with
     | Some a, Some b -> cont (Some (op a b)) t
     | _ -> cont None t)
  and safe_binop_pow scope op l rest cont =
    let r, t = parse_pow scope rest in
    (match l, r with
     | Some a, Some b when b <> 0 -> cont (Some (op a b)) t
     | _ -> cont None t)
  (* Power: right-associative, tighter than `*`/`/`. SV uses `**`.
   * For non-negative integer exponents, fold by repeated multiply. *)
  and parse_pow scope toks =
    let l, t = parse_unary scope toks in
    match t with
    | TPow :: rest ->
        let r, t' = parse_pow scope rest in
        let v = match l, r with
          | Some a, Some e when e >= 0 ->
              let rec pow b e =
                if e = 0 then 1
                else b * pow b (e - 1)
              in
              Some (pow a e)
          | _ -> None
        in
        (v, t')
    | _ -> (l, t)
  and parse_unary scope = function
    | TMinus :: rest ->
        let v, t = parse_unary scope rest in
        (Option.map (~-) v, t)
    | TPlus :: rest -> parse_unary scope rest
    | TBang :: rest ->
        let v, t = parse_unary scope rest in
        (Option.map (fun n -> bool_int (n = 0)) v, t)
    | toks -> parse_atom scope toks
  and parse_atom scope = function
    | TNum n :: rest -> (Some n, rest)
    | TId base :: TDot :: TId field :: rest ->
        (* Struct field access: try the global struct table first
         * (populated by resolve_param_default for top-level
         * struct-typed parameters like CVA6Cfg), then fall back to
         * the int scope. *)
        (match Hashtbl.find_opt struct_table base with
         | Some (SVStruct fs) ->
             (match List.assoc_opt field fs with
              | Some (SVInt n) -> (Some n, rest)
              | _ -> (None, rest))
         | _ -> (List.assoc_opt (base ^ "." ^ field) scope, rest))
    | TId id :: rest -> (List.assoc_opt id scope, rest)
    | TDollar fname :: TLP :: rest ->
        let arg, t = parse_expr scope rest in
        let rec drop_to_rp = function
          | TRP :: r -> r | _ :: r -> drop_to_rp r | [] -> []
        in
        let r = drop_to_rp t in
        let v = match fname, arg with
          | "clog2", Some n when n > 1 ->
              let rec lg n acc = if n <= 1 then acc else lg ((n + 1) / 2) (acc + 1) in
              Some (lg n 0)
          | "clog2", Some _ -> Some 0
          | ("bits" | "size" | "unsigned" | "signed"), _ -> arg
          | _ -> None
        in
        (v, r)
    | TLP :: rest ->
        let v, t = parse_expr scope rest in
        (match t with TRP :: r -> (v, r) | _ -> (v, t))
    | rest -> (None, rest)
  and binop scope op l rest cont =
    let r, t = parse_mul scope rest in
    (match l, r with
     | Some a, Some b -> cont (Some (op a b)) t
     | _ -> cont None t)
  and safe_binop scope op l rest cont =
    let r, t = parse_unary scope rest in
    (match l, r with
     | Some a, Some b when b <> 0 -> cont (Some (op a b)) t
     | _ -> cont None t)

  let eval_string scope s =
    (* Strip SystemVerilog type-cast prefixes `id'(` → `(`. The cast
     * preserves value (it just narrows/extends the type which we
     * don't track), so dropping it is sound for integer evaluation.
     * Without this, `unsigned'(k) * 2 < WIDTH - 1` (used in lzc.sv's
     * generate-for body conditions) tokenises as garbage and the
     * outer prune_dead_generates can't decide which inner if-branch
     * is live. *)
    let s =
      let buf = Buffer.create (String.length s) in
      let n = String.length s in
      let i = ref 0 in
      while !i < n do
        let c = s.[!i] in
        let is_id c =
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
          || c = '_' || (c >= '0' && c <= '9') in
        if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' then begin
          let j = ref !i in
          while !j < n && is_id s.[!j] do incr j done;
          let id_end = !j in
          (* Skip optional whitespace before `'(`. *)
          while !j < n && (s.[!j] = ' ' || s.[!j] = '\t') do incr j done;
          if !j + 1 < n && s.[!j] = '\'' && s.[!j + 1] = '(' then begin
            (* Cast — drop the id and the apostrophe. *)
            i := !j + 1
          end else begin
            Buffer.add_substring buf s !i (id_end - !i);
            i := id_end
          end
        end else begin
          Buffer.add_char buf c;
          incr i
        end
      done;
      Buffer.contents buf
    in
    try fst (parse_expr scope (tokenize s)) with _ -> None
end

(* Pull module-port parameter DEFAULTS (everything in the
 * `#(parameter ...)` header), as (name, rhs_token) pairs. The grammar
 * tags each one with `module_parameter_port`. Used by int_scope_of to
 * fold defaults into the scope when no override was supplied at the
 * call site — without this, a top-level invocation like
 *   popcount #() top();   // INPUT_WIDTH default = 256
 * leaves INPUT_WIDTH unset and Eval can't resolve PaddedWidth=...
 * making the recursive child come out as `popcount__IWPaddedWidth/2`
 * (literal text) instead of `popcount__IW128`. *)
let extract_module_port_param_defaults (body : token) : (string * token) list =
  let nodes = collect_by
    (has_tag (prefix_is "module_parameter_port")) body in
  List.filter_map (fun n ->
    let id_subs = collect_by
      (has_tag (prefix_is "param_type_followed_by_id")) n in
    let pname = match id_subs with
      | s :: _ ->
          let ids = ref [] in
          walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) s;
          (match List.rev !ids with last :: _ -> Some last | [] -> None)
      | [] -> None
    in
    let rhs = match collect_by
                (has_tag (prefix_is "trailing_assign")) n with
      | a :: _ -> Some a | [] -> None
    in
    match pname, rhs with
    | Some name, Some r -> Some (name, r)
    | _ -> None
  ) nodes

(* Pull `localparam`/`parameter` declarations from a module body, as
 * (name, rhs_token) pairs. The grammar tags both as
 * `any_param_declaration<N>`; we trust that only ones inside the
 * module body show up here. Walks the trailing_assign for the RHS. *)
let extract_module_internal_params (body : token) : (string * token) list =
  let nodes = collect_by
    (has_tag (prefix_is "any_param_declaration")) body in
  List.filter_map (fun n ->
    let id_subs = collect_by
      (has_tag (prefix_is "param_type_followed_by_id")) n in
    let pname = match id_subs with
      | s :: _ ->
          let ids = ref [] in
          walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) s;
          (match List.rev !ids with last :: _ -> Some last | [] -> None)
      | [] -> None
    in
    let rhs = match collect_by
                (has_tag (prefix_is "trailing_assign")) n with
      | a :: _ -> Some a | [] -> None
    in
    match pname, rhs with
    | Some name, Some r -> Some (name, r)
    | _ -> None
  ) nodes

(* ─── Specialisation ─────────────────────────────────────────────── *)

(* Encode a parameter-value dictionary into a compact suffix that
 * matches the spirit of Verilator's mangling: `__W4_M1_CW3`. *)
let suffix_of_params params =
  if params = [] then ""
  else
    let abbrev s =
      (* Take initials of camelCase / snake-case identifier. *)
      let buf = Buffer.create 4 in
      let next_upper = ref true in
      String.iter (fun c ->
        if c = '_' then next_upper := true
        else if !next_upper then begin
          Buffer.add_char buf (Char.uppercase_ascii c);
          next_upper := false
        end
      ) s;
      Buffer.contents buf
    in
    "__" ^ String.concat "_"
      (List.map (fun (k, v) ->
        let k' = abbrev k in
        let v' =
          (* Strip leading apostrophe / format prefix from numbers. *)
          try
            let i = String.index v '\'' in
            String.sub v (i + 2) (String.length v - i - 2)
          with Not_found -> v
        in
        k' ^ v') params)

type specialised = {
  s_base:   string;
  s_name:   string;          (* base + suffix *)
  s_params: (string * string) list;
  s_inst:   string option;   (* original instance label, if known *)
}

(* Walk the design from a chosen top, specialise each instantiated
 * module per unique override set, recurse. When a `pkgs` table is
 * provided, every override expression is resolved through it so
 * `config_pkg::XLEN` collapses to its integer value before we hash
 * it into the specialised name. *)
(* Module-level: published map (parent_specname, inst_label) →
 * child_specname, populated in specialise_design's visit. Used by
 * verible_to_behavioral.convert_files to rewrite each binstance's
 * module_name from base to its specialised counterpart, so that
 * Behavioral_flatten can pick the right child to inline. *)
let inst_specialised : (string * string, string) Hashtbl.t =
  Hashtbl.create 64

let specialise_design ?(pkgs = []) (mods : module_decl list) ~top_name =
  let by_name = Hashtbl.create 32 in
  List.iter (fun m -> Hashtbl.replace by_name m.m_name m) mods;
  Hashtbl.clear inst_specialised;
  (* Build the function table once from every module + package body.
   * Used by resolve_param_default to evaluate constant function calls
   * appearing as parameter defaults (e.g. CVA6Cfg = build_config(...)).
   * Mirrors Verilator's V3Inline + V3Const interaction: identify
   * pure functions, evaluate them once, cache the result. *)
  let functions =
    List.concat_map (fun m -> extract_functions m.m_body) mods
    @ List.concat_map (fun p -> extract_functions p.pkg_body) pkgs
  in
  Hashtbl.clear struct_table;
  let debug = Sys.getenv_opt "ELAB_DEBUG" <> None in
  walk_live_debug := debug;
  if debug then
    Printf.eprintf "[elab] %d modules, %d packages, %d const-functions\n%!"
      (List.length mods) (List.length pkgs) (List.length functions);
  (* Evaluate each top-level module's struct-typed parameter defaults
   * once and stash in struct_table so override expressions like
   * `CVA6Cfg.XLEN` in child instantiations can resolve via field
   * lookup. Only the top module's defaults are needed — non-top
   * modules pick up their CVA6Cfg through override propagation. *)
  (match Hashtbl.find_opt by_name top_name with
   | None -> ()
   | Some top ->
       let lookup_int sc s = Eval.eval_string sc s in
       let param_decls = collect_by
         (has_tag (prefix_is "module_parameter_port")) top.m_body in
       List.iter (fun pn ->
         (* The param name is the identifier in the
          * `param_type_followed_by_id*` subtree (excludes both the
          * type's pkg::type qualifier AND the default expression's
          * identifiers). Without this restriction the walk drifts
          * into either the type or default and picks up wrong ids. *)
         let ids =
           let acc = ref [] in
           let id_subs = collect_by
             (has_tag (prefix_is "param_type_followed_by_id")) pn in
           List.iter (fun s ->
             walk (function
               | SymbolIdentifier id -> acc := id :: !acc
               | _ -> ()) s) id_subs;
           !acc
         in
         let pname = match ids with last :: _ -> Some last | _ -> None in
         let default = match collect_by
                          (has_tag (prefix_is "trailing_assign")) pn with
           | a :: _ -> Some a | _ -> None in
         match pname, default with
         | Some name, Some d ->
             let v = resolve_param_default ~functions ~lookup_int ~pkgs ~tok:d in
             (match v with
              | SVStruct fs ->
                  if debug then
                    Printf.eprintf
                      "[elab] %s ← struct (%d known of %d fields)\n%!"
                      name
                      (List.length (List.filter
                        (function (_, SVInt _) -> true | _ -> false) fs))
                      (List.length fs);
                  Hashtbl.replace struct_table name v
              | _ -> ())
         | _ -> ()
       ) param_decls);
  (* Build the integer-valued scope for the current module: start with
   * each override that resolves to an int (via package + Eval), then
   * fold in localparams whose RHS Eval can reduce against that scope.
   * Localparams are evaluated in source order; each new value extends
   * the scope used for the next, so chains like
   *   localparam A = 8; localparam B = A * 2;
   * resolve correctly. *)
  let int_scope_of mdecl overrides =
    let scope = List.filter_map (fun (k, v) ->
      Option.map (fun n -> (k, n)) (Eval.eval_string [] v)
    ) overrides in
    (* For every module port-parameter that does NOT have an override,
     * fold its DEFAULT into the scope. Lets a top-level instantiation
     * with no `#(...)` use the declared defaults — without it Eval
     * can't resolve later localparams that reference the parameter. *)
    let defaults = extract_module_port_param_defaults mdecl.m_body in
    let scope = List.fold_left (fun sc (name, rhs_tok) ->
      if List.mem_assoc name sc then sc
      else
        let s = resolve_value pkgs rhs_tok in
        match Eval.eval_string sc s with
        | Some n -> (name, n) :: sc
        | None -> sc
    ) scope defaults in
    let lps = extract_module_internal_params mdecl.m_body in
    List.fold_left (fun sc (name, rhs_tok) ->
      let s = resolve_value pkgs rhs_tok in
      match Eval.eval_string sc s with
      | Some n -> (name, n) :: sc
      | None -> sc
    ) scope lps
  in
  let resolve_overrides_with scope ovs =
    List.map (fun (name, tok) ->
      let s = resolve_value pkgs tok in
      let v =
        match Eval.eval_string scope s with
        | Some n -> string_of_int n
        | None -> s
      in
      (name, v)
    ) ovs
  in
  let seen : (string, specialised) Hashtbl.t = Hashtbl.create 64 in
  let rec visit ?(inst_label = None) base overrides =
    match Hashtbl.find_opt by_name base with
    | None -> ()
    | Some mdecl ->
        let suffix = suffix_of_params overrides in
        let sname = base ^ suffix in
        if not (Hashtbl.mem seen sname) then begin
          Hashtbl.add seen sname {
            s_base = base; s_name = sname;
            s_params = overrides; s_inst = inst_label;
          };
          let scope = int_scope_of mdecl overrides in
          if debug then
            Printf.eprintf "[elab] visit %s, scope=[%s]\n%!" sname
              (String.concat ", "
                (List.map (fun (k, v) ->
                  Printf.sprintf "%s=%d" k v) scope));
          (* Plug the resolver+eval through the globals so walk_live
           * can fold scope refs in the if-condition expression text. *)
          resolver_for_walk := (fun t -> resolve_value pkgs t);
          evaluator_for_walk := Eval.eval_string;
          List.iter (fun inst ->
            (* Skip ghost "instances" — signal declarations whose
             * type-tag the parser miscategorised as an
             * instantiation_base (e.g. `logic [PaddedWidth-1:0]
             * padded_input;` showing up as an instance of
             * "PaddedWidth"). The cheap check is whether the
             * target module name actually exists. *)
            if not (Hashtbl.mem by_name inst.i_module) then ()
            else begin
              let ovs = resolve_overrides_with scope inst.i_overrides_tok in
              if debug then
                Printf.eprintf "[elab]   inst %s of %s: %s\n%!"
                  inst.i_inst inst.i_module
                  (String.concat ", "
                    (List.map (fun (k, v) -> k^"="^v) ovs));
              let child_suffix = suffix_of_params ovs in
              let child_sname = inst.i_module ^ child_suffix in
              Hashtbl.replace inst_specialised
                (sname, inst.i_inst) child_sname;
              visit ~inst_label:(Some inst.i_inst) inst.i_module ovs
            end
          ) (extract_instantiations ~scope mdecl.m_body)
        end
  in
  visit ~inst_label:None top_name [];
  Hashtbl.fold (fun _ v acc -> v :: acc) seen []
  |> List.sort (fun a b -> compare a.s_name b.s_name)

(* ─── Multi-file convenience ─────────────────────────────────────── *)

(* Parse multiple SV files and yield (modules, packages). *)
let parse_files_full files =
  let mods = ref [] in
  let pkgs = ref [] in
  List.iter (fun f ->
    match Sv_verible_to_ir.parse_verible_file f with
    | None -> Printf.eprintf "[verible] parse failed for %s\n" f
    | Some root ->
        mods := extract_modules root @ !mods;
        pkgs := extract_packages root @ !pkgs
  ) files;
  (List.rev !mods, List.rev !pkgs)

let parse_files files = fst (parse_files_full files)
