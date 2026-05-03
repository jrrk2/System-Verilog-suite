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
  | PERCENT -> Some "%" | PLUS  -> Some "+" | HYPHEN -> Some "-"
  | LPAREN -> Some "("  | RPAREN -> Some ")"
  | COMMA  -> Some ","  | DOT    -> Some "."
  | COLON_COLON -> Some "::"
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

let extract_instantiations root : instantiation list =
  let inst_nodes = collect_by
    (has_tag (prefix_is "instantiation_base")) root
  in
  List.filter_map (fun node ->
    (* The first SymbolIdentifier in the instantiation_base subtree
     * is the instantiated module name (modulo wrapping). *)
    let mod_name = ref None in
    walk (function
      | SymbolIdentifier id when !mod_name = None -> mod_name := Some id
      | _ -> ()
    ) node;
    match !mod_name with
    | None -> None
    | Some mn ->
        let in_ = match extract_inst_name node with
          | Some s -> s
          | None -> "?"
        in
        Some {
          i_module = mn;
          i_inst = in_;
          i_overrides = extract_overrides node;
          i_overrides_tok = extract_overrides_tok node;
        }
  ) inst_nodes

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
    | TLP | TRP | TComma
    | TPlus | TMinus | TStar | TSlash | TPercent
    | TShl | TShr
    | TDollar of string

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
      | '+' -> push TPlus; incr i
      | '-' -> push TMinus; incr i
      | '*' -> push TStar; incr i
      | '/' -> push TSlash; incr i
      | '%' -> push TPercent; incr i
      | '<' when !i + 1 < n && s.[!i + 1] = '<' -> push TShl; i := !i + 2
      | '>' when !i + 1 < n && s.[!i + 1] = '>' -> push TShr; i := !i + 2
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

  let rec parse_expr scope toks = parse_add scope toks
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
    let l, t = parse_unary scope toks in
    let rec loop l = function
      | TStar    :: rest -> binop scope ( * ) l rest loop
      | TSlash   :: rest -> safe_binop scope (/) l rest loop
      | TPercent :: rest -> safe_binop scope (mod) l rest loop
      | rest -> (l, rest)
    in loop l t
  and parse_unary scope = function
    | TMinus :: rest ->
        let v, t = parse_unary scope rest in
        (Option.map (~-) v, t)
    | TPlus :: rest -> parse_unary scope rest
    | toks -> parse_atom scope toks
  and parse_atom scope = function
    | TNum n :: rest -> (Some n, rest)
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
    try fst (parse_expr scope (tokenize s)) with _ -> None
end

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
let specialise_design ?(pkgs = []) (mods : module_decl list) ~top_name =
  let by_name = Hashtbl.create 32 in
  List.iter (fun m -> Hashtbl.replace by_name m.m_name m) mods;
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
          List.iter (fun inst ->
            visit ~inst_label:(Some inst.i_inst)
              inst.i_module
              (resolve_overrides_with scope inst.i_overrides_tok)
          ) (extract_instantiations mdecl.m_body)
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
