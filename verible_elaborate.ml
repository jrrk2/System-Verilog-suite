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

(* name -> (value, bit-width) of already-evaluated params/localparams, populated
   in SOURCE order as packages/params are extracted, so a concatenation that
   references an earlier param (e.g. RV_DM_JTAG_IDCODE = {JTAG_VERSION, …}) can
   place it at the right bit offset. *)
let param_vw : (string, int * int) Hashtbl.t = Hashtbl.create 256

(* name -> packed bit-width of a declared type; populated during
   specialise_design (collect_type_widths).  Declared up here so eval_cw can fold
   `$bits(<type>)` directly. *)
let type_widths : (string, int) Hashtbl.t = Hashtbl.create 128

(* struct type name -> ordered [(field_name, packed_width)] in DECLARED order
   (first field = MSB), so eval_cw can pack a `'{field: value, …}` struct literal
   to an integer (e.g. status_t's MSTATUS_RST_VAL). Populated alongside
   type_widths in collect_type_widths. *)
let struct_fields : (string, (string * int) list) Hashtbl.t = Hashtbl.create 64

(* Width-aware constant fold of `{a, b, c}` concatenations and their leaves.
   Returns (value, bit-width).  Handles sized literals (width from the base
   prefix), nested concats (widths sum, elements placed MSB→LSB), identifiers
   (resolved to (value,width) via param_vw), and single-child wrappers.  Returns
   None if any leaf can't be resolved — the caller then keeps its old behaviour.
   This is the fix for value_of / extract_pvalue flattening a concat to just one
   element (dropping JTAG_VERSION/MFG and leaving RV_DM_JTAG_IDCODE = 1). *)
let cw_children = function
  | TUPLE2 (a,b) -> [a;b]
  | TUPLE3 (a,b,c) -> [a;b;c]
  | TUPLE4 (a,b,c,d) -> [a;b;c;d]
  | TUPLE5 (a,b,c,d,e) -> [a;b;c;d;e]
  | TUPLE6 (a,b,c,d,e,f) -> [a;b;c;d;e;f]
  | TUPLE7 (a,b,c,d,e,f,g) -> [a;b;c;d;e;f;g]
  | TUPLE8 (a,b,c,d,e,f,g,h) -> [a;b;c;d;e;f;g;h]
  | TLIST xs -> xs
  | _ -> []

let rec eval_cw tok : (int * int) option =
  match tok with
  | TK_DecNumber n | TK_UnBasedNumber n ->
      (* SV unsized fills `'0`/`'1` carry value 0 / all-ones at width 0 (a
         "fill at context width" sentinel — the emitter renders `'0`). *)
      if n = "'0" then Some (0, 0)
      else if n = "'1" then Some (-1, 0)
      else (try Some (int_of_string n, 32) with _ -> None)
  | TUPLE3 (STRING tag, base_tok, digits)
    when prefix_is "bin_based_number" tag || prefix_is "hex_based_number" tag
      || prefix_is "oct_based_number" tag || prefix_is "dec_based_number" tag ->
      let width = match base_tok with
        | TK_BinBase s | TK_HexBase s | TK_OctBase s | TK_DecBase s ->
            (try Some (int_of_string (String.sub s 0 (String.index s '\''))) with _ -> None)
        | _ -> None in
      let ds = Buffer.create 16 in
      walk (function
        | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n
        | TK_DecDigits n | TK_DecNumber n -> Buffer.add_string ds n
        | _ -> ()) digits;
      let s = Buffer.contents ds in
      let value =
        if s = "" then None
        else (try Some (int_of_string
               ((if prefix_is "bin_based_number" tag then "0b"
                 else if prefix_is "hex_based_number" tag then "0x"
                 else if prefix_is "oct_based_number" tag then "0o"
                 else "") ^ s)) with _ -> None) in
      (match width, value with Some w, Some v -> Some (v, w) | _ -> None)
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let elems = match body with
        | TLIST xs -> List.rev xs          (* TLIST is reverse source order *)
        | other -> [other] in
      let r = List.fold_left (fun acc e ->
        match acc with
        | None -> None
        | Some (av, aw) ->
            (match eval_cw e with
             | Some (ev, ew) when ew > 0 ->
                 let mask = if ew >= 62 then -1 else (1 lsl ew) - 1 in
                 Some (((av lsl ew) lor (ev land mask)), aw + ew)
             | Some _ -> Some (av, aw)      (* zero-width: skip *)
             | None -> None)) (Some (0, 0)) elems in
      (match r with Some (_, 0) -> None | _ -> r)
  (* `PARAM[i]` as a constant override value.  Xilinx's reset_sync/sync_block
     write `FDP #(.INIT(INITIALISE[0]))`; folding the base but ignoring the
     bit-select silently substitutes the WHOLE parameter (INITIALISE = 2'b11
     -> INIT=3 on a 1-bit FF).  Yield the selected bit, width 1. *)
  | TUPLE3 (STRING t, ref_node, TUPLE4 (STRING dt, _, idx, _))
    when prefix_is "reference" t && t <> "reference1"
      && prefix_is "select_variable_dimension" dt ->
      let base =
        match eval_cw ref_node with
        | Some vw -> Some vw
        | None ->
            let nm = ref None in
            walk (function
              | SymbolIdentifier id when !nm = None -> nm := Some id
              | _ -> ()) ref_node;
            (match !nm with
             | Some id -> Hashtbl.find_opt param_vw id
             | None -> None) in
      (match base, eval_cw idx with
       | Some (v, _), Some (i, _) when i >= 0 && i < Sys.int_size ->
           Some ((v lsr i) land 1, 1)
       | _ -> None)
  | SymbolIdentifier id -> Hashtbl.find_opt param_vw id
  | TUPLE4 (STRING tag, _, _, TUPLE3 (STRING tg2, SymbolIdentifier name, _))
    when prefix_is "qualified_id" tag && prefix_is "unqualified_id" tg2 ->
      Hashtbl.find_opt param_vw name
  (* `$bits(<type>)` → the declared packed width of the type (from type_widths).
     Folds `.Width($bits(dmi_resp_o))` on the typed tree instead of via
     resolve_value's string output. *)
  | TUPLE3 (STRING t, SystemTFIdentifier ("$bits" | "bits"), call_base)
    when prefix_is "system_tf_call" t ->
      let last = ref None in
      walk (function
        | SymbolIdentifier id when Hashtbl.mem type_widths id -> last := Some id
        | _ -> ()) call_base;
      (match !last with
       | Some id -> Some (Hashtbl.find type_widths id, 32)
       | None -> None)
  (* Binary arithmetic on the TYPED tree — `add_expr`/`mul_expr`/`shift_expr`/
     `pow_expr` are `TUPLE4(tag, lhs, op, rhs)`.  Fold directly (dispatching on
     the operator leaf) so overrides like `.Depth(MEM_SIZE / 4)` resolve without
     the fragile stringify→re-parse Eval path. *)
  | TUPLE4 (STRING tag, lhs, op, rhs)
    when prefix_is "add_expr" tag || prefix_is "mul_expr" tag
      || prefix_is "shift_expr" tag || prefix_is "pow_expr" tag ->
      (match eval_cw lhs, eval_cw rhs with
       | Some (a, wa), Some (b, wb) ->
           let w = max wa wb in
           let r = match op with
             | PLUS -> Some (a + b)
             | HYPHEN -> Some (a - b)
             | STAR -> Some (a * b)
             | SLASH -> Some (if b = 0 then 0 else a / b)
             | PERCENT -> Some (if b = 0 then 0 else a mod b)
             | LT_LT -> Some (a lsl b)
             | GT_GT -> Some (a lsr b)
             | STAR_STAR when b >= 0 ->
                 let rec p x e = if e = 0 then 1 else x * p x (e - 1) in Some (p a b)
             | _ -> None
           in
           (match r with Some n -> Some (n, w) | None -> None)
       | _ -> None)
  (* Size / sign cast `T'(expr)` — TUPLE6(tag, casting_type, quote, lparen,
     expr, rparen).  When the casting type folds to a positive width, TRUNCATE
     the value to that many bits (`RegFileDataWidth'(0x2A00000000)` → the
     register-file zero word); a non-width cast (`unsigned'(x)`) is
     value-preserving.  Avoids the Eval string cast-strip hack. *)
  | TUPLE6 (STRING tag, ct, _, _, expr, _) when prefix_is "cast" tag ->
      (match eval_cw expr with
       | None -> None
       | Some (v, vw) ->
           (match eval_cw ct with
            | Some (w, _) when w > 0 && w < 62 -> Some (v land ((1 lsl w) - 1), w)
            | Some (w, _) when w > 0 -> Some (v, w)
            | _ -> Some (v, vw)))
  (* Struct literal `'{field: value, …}` (assignment_pattern2) → pack to an int
     using the matching typedef's declared field order/widths (struct_fields).
     Lets `.ResetValue({MSTATUS_RST_VAL})` fold instead of surviving as a symbol. *)
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "assignment_pattern2" tag ->
      let pair_nodes =
        collect_by (has_tag (prefix_is "structure_or_array_pattern_expression")) body in
      (* A `default: <val>` clause supplies every field not explicitly named
         (dcsr's DCSR_RESET_VAL names 3 of 15 fields, `default: '0` the rest).
         The key of such a pair is the `default` KEYWORD (Default token), not a
         SymbolIdentifier — detect it structurally and record under "default". *)
      let kv = List.filter_map (fun p -> match p with
        | TUPLE4 (_, key, _, value) ->
            let kn = ref None and is_default = ref false in
            walk (function
              | Default -> is_default := true
              | SymbolIdentifier id when !kn = None -> kn := Some id
              | _ -> ()) key;
            let k = if !is_default then Some "default" else !kn in
            (match k, eval_cw value with
             | Some k, Some (v, _) -> Some (k, v)
             | _ -> None)
        | _ -> None) pair_nodes in
      if kv = [] then None
      else begin
        let default_val = List.assoc_opt "default" kv in
        let named = List.filter (fun (k, _) -> k <> "default") kv in
        let named_keys = List.sort_uniq compare (List.map fst named) in
        (* Choose the struct layout.  With no default, the named keys must match
           a struct's field set EXACTLY.  With a default, the named keys need only
           be a SUBSET of the fields (default fills the rest); require a UNIQUE
           such struct so the choice is unambiguous. *)
        let layouts = Hashtbl.fold (fun _ fields acc ->
          let fkeys = List.sort_uniq compare (List.map fst fields) in
          let named_are_fields = List.for_all (fun k -> List.mem k fkeys) named_keys in
          let ok = match default_val with
            | Some _ -> named_are_fields
            | None    -> fkeys = named_keys in
          if ok then fields :: acc else acc) struct_fields [] in
        match layouts with
        | [fields] ->
            let total = List.fold_left (fun a (_, w) -> a + w) 0 fields in
            let v = List.fold_left (fun acc (fn, fw) ->
                let fv = match List.assoc_opt fn named with
                  | Some x -> x
                  | None -> (match default_val with Some d -> d | None -> 0) in
                let mask = if fw >= 62 then -1 else (1 lsl fw) - 1 in
                (acc lsl fw) lor (fv land mask)) 0 fields in
            Some (v, total)
        | _ -> None      (* no match, or ambiguous superset *)
      end
  (* structural wrapper (trailing_assign, expression, primary, …): descend to
     the UNIQUE child that folds.  If two children fold (an operator expr a+b)
     it's ambiguous → None, so the legacy reader handles it instead. *)
  | other ->
      (match List.filter_map eval_cw (cw_children other) with
       | [single] -> Some single
       | _ -> None)

(* Render a token's leaf value as a string. Numbers, identifiers,
 * keywords. For composite trees we recurse into the first child. *)
let rec value_of = function
  | TK_DecNumber n | TK_UnBasedNumber n
  | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n -> n
  (* real / string / based-literal parameter values (e.g. an MMCM's
     CLKIN1_PERIOD(16.000000), BANDWIDTH("OPTIMIZED")).  The lexer carries the
     text in the token payload; without these cases value_of returns the token
     TAG ("TK_RealTime") and the primitive parameter is emitted as garbage. *)
  | TK_RealTime s -> s
  | TK_StringLiteral s -> s
  | TK_BinBase s | TK_HexBase s | TK_OctBase s | TK_DecBase s -> s
  | SymbolIdentifier id -> id
  | STRING s -> s
  (* Based literal `<width>'<base><digits>` (e.g. FDSE #(.INIT(1'b1))).
     Verible surfaces it as TUPLE3(STRING "bin_based_number", <base tok
     "1'b">, <digits tok "1">).  The generic `TUPLE3 (_, a, _)` case below
     recurses only into the base and DROPS the digits, truncating "1'b1"
     to "1'b" — which then reaches the EDIF as a valueless INIT and Vivado
     reads it as 0.  Concatenate base and digits so the value survives. *)
  | TUPLE3 (STRING tag, base, digits)
      when prefix_is "bin_based_number" tag
        || prefix_is "hex_based_number" tag
        || prefix_is "oct_based_number" tag
        || prefix_is "dec_based_number" tag ->
      value_of base ^ value_of digits
  (* Concatenation `{a, b, c}`: fold width-aware to a single integer instead of
     flattening to one element (the generic TUPLE4->d case below did the latter,
     dropping all but the last element). *)
  | TUPLE4 (STRING tag, _, _, d) as concat when prefix_is "range_list_in_braces" tag ->
      (match eval_cw concat with
       | Some (v, _) -> string_of_int v
       | None -> value_of d)
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
  | TK_DecNumber n | TK_UnBasedNumber n -> Some n
  (* Preserve the radix: verible surfaces a based literal's digits in a
   * typed token (TK_HexDigits etc.) with the base stripped, so emitting
   * the bare digits would make the downstream evaluator read e.g. a hex
   * value as decimal (PROGADDR_RESET 32'h00100000 -> 100000).  Re-attach
   * the Verilog base marker so [Eval] parses the radix correctly. *)
  | TK_HexDigits n -> Some ("'h" ^ n)
  | TK_BinDigits n -> Some ("'b" ^ n)
  | TK_OctDigits n -> Some ("'o" ^ n)
  (* STRING parameter values (.LFSR_CONFIG("GALOIS")): without this the
     deep rendering dropped the literal entirely, the override resolved
     to "" and the specialised clone LOST the parameter — downstream
     string compares (`LFSR_CONFIG == "GALOIS"`) then read an unbound
     var as 0 and rgmii_lfsr's CRC mask generation never ran.  Quote-
     normalised so param_value_to_bexpr packs it as an ASCII constant. *)
  | TK_StringLiteral s ->
      let n = String.length s in
      let s = if n >= 2 && s.[0] = '"' && s.[n-1] = '"'
              then String.sub s 1 (n - 2) else s in
      Some ("\"" ^ s ^ "\"")
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
  (* Cast tick: `RegFileDataWidth'(expr)` carries the `'` as a QUOTE leaf.
     Dropping it rendered the cast as a function-call-looking
     `RegFileDataWidth ( expr )`, so the evaluator returned the width id
     (32) instead of the cast value — x0 read 32 in the register file.
     Emit it so the cast-strip in Eval.eval_string recognises the cast. *)
  | QUOTE -> Some "'"
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
  (* The parameter name is the last identifier BEFORE the `=` default.  Taking
     the first id named the TYPE for a user-typed param (`rv32b_e RV32B`) or the
     RANGE param for a ranged one (`bit [Width-1:0] ResetValue` -> "Width"), so
     the override never matched and the body reference leaked.  Skip the default
     expression (else `rv32b_e RV32B = RV32BNone` would pick "RV32BNone") and
     take the last remaining id. *)
  let names = ref [] in
  let rec w t = match t with
    | SymbolIdentifier id -> names := id :: !names
    | TUPLE3 (STRING tag, _, _) when prefix_is "trailing_assign" tag -> ()
    | TUPLE4 (STRING tag, _, _, _) when prefix_is "trailing_assign" tag -> ()
    | TUPLE2 (a,b) -> w a; w b
    | TUPLE3 (a,b,c) -> w a; w b; w c
    | TUPLE4 (a,b,c,d) -> List.iter w [a;b;c;d]
    | TUPLE5 (a,b,c,d,e) -> List.iter w [a;b;c;d;e]
    | TUPLE6 (a,b,c,d,e,f) -> List.iter w [a;b;c;d;e;f]
    | TUPLE7 (a,b,c,d,e,f,g) -> List.iter w [a;b;c;d;e;f;g]
    | TUPLE8 (a,b,c,d,e,f,g,h) -> List.iter w [a;b;c;d;e;f;g;h]
    | TUPLE9 (a,b,c,d,e,f,g,h,i) -> List.iter w [a;b;c;d;e;f;g;h;i]
    | TUPLE10 (a,b,c,d,e,f,g,h,i,j) -> List.iter w [a;b;c;d;e;f;g;h;i;j]
    | TUPLE11 (a,b,c,d,e,f,g,h,i,j,k) -> List.iter w [a;b;c;d;e;f;g;h;i;j;k]
    | TUPLE12 (a,b,c,d,e,f,g,h,i,j,k,l) -> List.iter w [a;b;c;d;e;f;g;h;i;j;k;l]
    | TUPLE13 (a,b,c,d,e,f,g,h,i,j,k,l,m) -> List.iter w [a;b;c;d;e;f;g;h;i;j;k;l;m]
    | TLIST xs -> List.iter w xs
    | _ -> ()
  in w tok;
  match !names with
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
let extract_begin_label = function
  | TUPLE3 (STRING t, _, lbl) when prefix_is "begin1" t ->
      let ids = ref [] in
      walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) lbl;
      (match List.rev !ids with x :: _ -> Some x | _ -> None)
  | _ -> None

(* Pull the label from a `generate_block` shape (generate_block1 = begin/end,
   generate_block2 = label : begin).  None when unlabeled. *)
let extract_block_label = function
  | TUPLE4 (STRING t, b, _, _) when prefix_is "generate_block1" t ->
      extract_begin_label b
  | TUPLE6 (STRING t, id_node, _, _, _, _) when prefix_is "generate_block2" t ->
      let ids = ref [] in
      walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) id_node;
      (match List.rev !ids with x :: _ -> Some x | _ -> None)
  | _ -> None

(* ── Generate-block-local declaration namespacing ─────────────────────
   When a labeled generate-FOR block declares LOCAL nets (`begin:L wire C; …`)
   the unroller replicates the body per iteration, but every replica keeps the
   bare name `C` — so all iterations collapse onto ONE net, and a cross-iteration
   hierarchical reference `L[i-1].C` parses as a bit-select of an undriven net
   `L` (→ 0).  This rewrites, for iteration `v` of block `L`:
     • block-local declarations/refs   `C`        → `L__<v>__C`   (this iteration)
     • hierarchical refs               `L[k].C`   → `L__<k>__C`   (iteration k)
   so each iteration gets its own net and cross-references thread correctly.
   Only fires for a LABELED block with ≥1 local declaration (local-free blocks —
   the overwhelmingly common case — are untouched, so no regression). *)
let namespace_genblk_locals ~label ~v ~eval_idx tok =
  let locals : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let is_decl t = prefix_is "net_variable" t || prefix_is "net_decl_assign" t
                  || prefix_is "register_variable" t in
  (* A bare `logic [W:0] name;` with no port list parses as the ambiguous
     `(non_anonymous_)gate_instance_or_register_variable1` — the REGISTER-
     VARIABLE form (the `…2` variant is the module/gate INSTANCE form with a
     port list, which we must NOT rename as a net).  ibex_register_file_ff's
     generate-local `rf_reg_q` is exactly this, so without covering the `…1`
     tag every iteration shared one flop and the whole RF aliased. *)
  let is_gate_var_decl t =
    prefix_is "gate_instance_or_register_variable1" t
    || prefix_is "non_anonymous_gate_instance_or_register_variable1" t in
  (* First SymbolIdentifier in a subtree = the declared name (the tag is a
     STRING leaf, and the name precedes any dimension/init expression). *)
  let first_sym t =
    let r = ref None in
    let rec go t = if !r = None then match t with
      | SymbolIdentifier id -> r := Some id
      | TUPLE2 (a,b) -> go a; go b
      | TUPLE3 (a,b,c) -> go a; go b; go c
      | TUPLE4 (a,b,c,d) -> go a; go b; go c; go d
      | TUPLE5 (a,b,c,d,e) -> go a; go b; go c; go d; go e
      | TUPLE6 (a,b,c,d,e,f) -> List.iter go [a;b;c;d;e;f]
      | TLIST xs -> List.iter go xs
      | _ -> () in
    go t; !r in
  let rec collect t =
    (match t with
     | TUPLE3 (STRING tg, SymbolIdentifier nm, _) when is_decl tg ->
         Hashtbl.replace locals nm ()
     | TUPLE4 (STRING tg, SymbolIdentifier nm, _, _) when is_decl tg ->
         Hashtbl.replace locals nm ()
     | (TUPLE2 (STRING tg, _) | TUPLE3 (STRING tg, _, _)
       | TUPLE4 (STRING tg, _, _, _) | TUPLE5 (STRING tg, _, _, _, _))
       when is_gate_var_decl tg ->
         (match first_sym t with Some nm -> Hashtbl.replace locals nm () | None -> ())
     | _ -> ());
    iter_children collect t
  and iter_children f = function
    | TUPLE2 (a,b) -> f a; f b
    | TUPLE3 (a,b,c) -> f a; f b; f c
    | TUPLE4 (a,b,c,d) -> f a; f b; f c; f d
    | TUPLE5 (a,b,c,d,e) -> f a; f b; f c; f d; f e
    | TUPLE6 (a,b,c,d,e,g) -> f a; f b; f c; f d; f e; f g
    | TUPLE7 (a,b,c,d,e,g,h) -> List.iter f [a;b;c;d;e;g;h]
    | TUPLE8 (a,b,c,d,e,g,h,i) -> List.iter f [a;b;c;d;e;g;h;i]
    | TUPLE9 (a,b,c,d,e,g,h,i,j) -> List.iter f [a;b;c;d;e;g;h;i;j]
    | TUPLE10 (a,b,c,d,e,g,h,i,j,k) -> List.iter f [a;b;c;d;e;g;h;i;j;k]
    | TUPLE11 (a,b,c,d,e,g,h,i,j,k,l) -> List.iter f [a;b;c;d;e;g;h;i;j;k;l]
    | TUPLE12 (a,b,c,d,e,g,h,i,j,k,l,m) -> List.iter f [a;b;c;d;e;g;h;i;j;k;l;m]
    | TUPLE13 (a,b,c,d,e,g,h,i,j,k,l,m,n) -> List.iter f [a;b;c;d;e;g;h;i;j;k;l;m;n]
    | TLIST xs -> List.iter f xs
    | _ -> () in
  collect tok;
  if Hashtbl.length locals = 0 then tok
  else begin
    let mangle k x = Printf.sprintf "%s__%d__%s" label k x in
    (* match a hierarchical reference `L[idx].field`; return (idx_tok, field) *)
    let hier_ref ref3 ext =
      let base_idx = match ref3 with
        | TUPLE3 (STRING r3,
                  TUPLE3 (STRING u, SymbolIdentifier base, _),
                  TUPLE4 (STRING sd, _, idx, _))
          when prefix_is "reference3" r3 && prefix_is "unqualified_id" u
               && prefix_is "select_variable_dimension" sd && base = label ->
            Some idx
        | _ -> None in
      let field = match ext with
        | TUPLE3 (STRING he, _, TUPLE3 (STRING u, SymbolIdentifier f, _))
          when prefix_is "hierarchy_extension" he && prefix_is "unqualified_id" u ->
            Some f
        | _ -> None in
      match base_idx, field with
      | Some idx, Some f when Hashtbl.mem locals f -> Some (idx, f)
      | _ -> None in
    (* map over immediate children, rebuilding the node *)
    let rec map_tok g = function
      | TUPLE2 (a,b) -> TUPLE2 (g a, g b)
      | TUPLE3 (a,b,c) -> TUPLE3 (g a, g b, g c)
      | TUPLE4 (a,b,c,d) -> TUPLE4 (g a, g b, g c, g d)
      | TUPLE5 (a,b,c,d,e) -> TUPLE5 (g a, g b, g c, g d, g e)
      | TUPLE6 (a,b,c,d,e,f) -> TUPLE6 (g a, g b, g c, g d, g e, g f)
      | TUPLE7 (a,b,c,d,e,f,h) -> TUPLE7 (g a, g b, g c, g d, g e, g f, g h)
      | TUPLE8 (a,b,c,d,e,f,h,i) -> TUPLE8 (g a, g b, g c, g d, g e, g f, g h, g i)
      | TUPLE9 (a,b,c,d,e,f,h,i,j) -> TUPLE9 (g a, g b, g c, g d, g e, g f, g h, g i, g j)
      | TUPLE10 (a,b,c,d,e,f,h,i,j,k) -> TUPLE10 (g a, g b, g c, g d, g e, g f, g h, g i, g j, g k)
      | TUPLE11 (a,b,c,d,e,f,h,i,j,k,l) -> TUPLE11 (g a, g b, g c, g d, g e, g f, g h, g i, g j, g k, g l)
      | TUPLE12 (a,b,c,d,e,f,h,i,j,k,l,m) -> TUPLE12 (g a, g b, g c, g d, g e, g f, g h, g i, g j, g k, g l, g m)
      | TUPLE13 (a,b,c,d,e,f,h,i,j,k,l,m,n) -> TUPLE13 (g a, g b, g c, g d, g e, g f, g h, g i, g j, g k, g l, g m, g n)
      | TLIST xs -> TLIST (List.map g xs)
      | leaf -> leaf in
    (* PASS 1: rewrite hierarchical refs L[k].field -> unqualified_id(mangle k field) *)
    let rec rw_hier t = match t with
      | TUPLE3 (STRING r2, ref3, ext) when prefix_is "reference2" r2 ->
          (match hier_ref ref3 ext with
           | Some (idx, f) ->
               (match eval_idx idx with
                | Some k ->
                    TUPLE3 (STRING "unqualified_id1",
                            SymbolIdentifier (mangle k f), EMPTY_TOKEN)
                | None -> map_tok rw_hier t)
           | None -> map_tok rw_hier t)
      | _ -> map_tok rw_hier t in
    (* PASS 2: rename bare block-local decls/refs -> mangle v name *)
    let rec rw_bare t = match t with
      | SymbolIdentifier nm when Hashtbl.mem locals nm ->
          SymbolIdentifier (mangle v nm)
      | _ -> map_tok rw_bare t in
    rw_bare (rw_hier tok)
  end

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
      (* Both helpers take an explicit scope so they can be called
         with `scope'` (which has the current iteration's genvar
         value bound) instead of the outer `scope` (which doesn't).
         Without this, `for (j = 0; j < 8; j = j + 1)` failed to
         compute step delta on iteration 0 because `j` wasn't in
         the outer scope and `eval_string "j + 1"` returned None,
         so the loop exited after one iteration and only j=0 was
         unrolled.                                                  *)
      let resolve_int_in sc e =
        let s = !resolver_for_walk e in
        !evaluator_for_walk sc s
      in
      let step_delta_in sc name =
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
            (* `name = expr` — evaluate expr in `sc` (which must
               carry the current iteration's genvar binding) and
               subtract the current name-value to get the delta. *)
            (match resolve_int_in sc rhs with
             | Some new_v ->
                 (match List.assoc_opt name sc with
                  | Some old_v -> Some (new_v - old_v)
                  | None -> None)
             | None -> None)
        | _ -> None
      in
      let _ = step_delta_in in
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
      (match gv_name, resolve_int_in scope init with
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
               (* Evaluate GENERATE-SCOPED localparams (now constant after the
                  genvar substitution) and substitute their references — e.g.
                  lowRISC gpio's `for(i) begin localparam int e = (i+1)*8<=W ?
                  (i+1)*8 : W; assign x[e-1:i*8] = … end`.  Without this `e`
                  is unbound and the slice bounds collapse (→ a comb loop).
                  Substituting the name everywhere also mangles the decl's own
                  name to a number, which the downstream param extractor then
                  harmlessly skips (no SymbolIdentifier name). *)
               let inst =
                 let pdecls =
                   collect_by (has_tag (prefix_is "any_param_declaration")) inst in
                 List.fold_left (fun inst pn ->
                   let pname =
                     match collect_by
                             (has_tag (prefix_is "param_type_followed_by_id")) pn with
                     | s :: _ ->
                         let ids = ref [] in
                         walk (function SymbolIdentifier id -> ids := id :: !ids
                                      | _ -> ()) s;
                         (match List.rev !ids with last :: _ -> Some last | [] -> None)
                     | [] -> None in
                   let rhs = match collect_by
                               (has_tag (prefix_is "trailing_assign")) pn with
                     | a :: _ -> Some a | [] -> None in
                   match pname, rhs with
                   | Some pnm, Some r ->
                       (match !evaluator_for_walk scope' (!resolver_for_walk r) with
                        | Some pv -> subst_genvar pnm pv inst
                        | None -> inst)
                   | _ -> inst) inst pdecls in
               let inst =
                 match extract_block_label body with
                 | Some label ->
                     namespace_genblk_locals ~label ~v:!v
                       ~eval_idx:(fun e -> resolve_int_in scope' e) inst
                 | None -> inst in
               unrolled := prune_dead_generates scope' inst :: !unrolled;
               (* Advance v per step.  Pass scope' (which has the
                  current iteration's genvar value bound) so that
                  `j = j + 1`-style steps can resolve `j + 1`.   *)
               match step_delta_in scope' name with
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

let extract_pvalue_from_param_decl ?name node =
  let assigns = collect_by (has_tag (prefix_is "trailing_assign")) node in
  match assigns with
  | a :: _ ->
    (* Width-aware fold first: handles `{a,b,c}` concatenations (including refs
       to earlier params) correctly, and records this param's (value,width) so
       LATER concats can place it.  Falls through to the legacy single-leaf
       reader when the expression isn't a foldable constant. *)
    (match eval_cw a with
     | Some (v, w) ->
         (match name with Some nm -> Hashtbl.replace param_vw nm (v, w) | None -> ());
         PInt v
     | None ->
      (* A sized/based literal (`4'b0101`, `64'h800`) must be read in ITS OWN
         base: the flat leaf walk below grabs the digit string and int_of_pvalue
         parses it base-10 (4'b0101 -> 101 not 5, 64'h800 -> 800 not 0x800).
         Parse the first based_number node explicitly first. *)
      let based =
        let parse_based = function
          | TUPLE3 (STRING tag, _b, digits) ->
              let pfx =
                if prefix_is "bin_based_number" tag then Some "0b"
                else if prefix_is "hex_based_number" tag then Some "0x"
                else if prefix_is "oct_based_number" tag then Some "0o"
                else if prefix_is "dec_based_number" tag then Some ""
                else None in
              (match pfx with
               | None -> None
               | Some pfx ->
                   let s = ref "" in
                   walk (function
                     | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n
                     | TK_DecDigits n | TK_DecNumber n -> s := !s ^ n
                     | _ -> ()) digits;
                   if !s = "" then None
                   else (try Some (int_of_string (pfx ^ !s)) with _ -> None))
          | _ -> None in
        match collect_by (has_tag (fun t ->
          prefix_is "bin_based_number" t || prefix_is "hex_based_number" t
          || prefix_is "oct_based_number" t || prefix_is "dec_based_number" t)) a with
        | bn :: _ -> parse_based bn
        | [] -> None in
      (match based with
       | Some n -> PInt n
       | None ->
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
       | _ -> PExpr a)))
  | [] -> PExpr node

(* Walk the root token but stop descending when we hit a node that
   has its OWN parameter scope: a module / interface / package
   declaration.  The nodes we yield are everything else (description
   items, data_declarations, parameter_declarations) at $unit scope.
   Used by [extract_file_scope_params] to find `localparam`s declared
   in `.svh` headers that are textually included into a `.sv` outside
   any module body — slang and verilator elaborate these via
   $unit-scope; verible's elaborator was previously missing them. *)
let collect_outside_module_or_package pred root =
  let acc = ref [] in
  let rec go t =
    let stop = match tag_of t with
      | Some s ->
          prefix_is "module_or_interface_declaration" s
          || prefix_is "package_declaration" s
      | None -> false in
    if stop then ()
    else begin
      if pred t then acc := t :: !acc;
      match t with
      | TUPLE2 (a, b) -> go a; go b
      | TUPLE3 (a, b, c) -> go a; go b; go c
      | TUPLE4 (a, b, c, d) -> List.iter go [a; b; c; d]
      | TUPLE5 (a, b, c, d, e) -> List.iter go [a; b; c; d; e]
      | TUPLE6 (a, b, c, d, e, f) -> List.iter go [a; b; c; d; e; f]
      | TUPLE7 (a, b, c, d, e, f, g) -> List.iter go [a; b; c; d; e; f; g]
      | TUPLE8 (a, b, c, d, e, f, g, h) -> List.iter go [a; b; c; d; e; f; g; h]
      | TUPLE9 (a, b, c, d, e, f, g, h, i) -> List.iter go [a; b; c; d; e; f; g; h; i]
      | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
          List.iter go [a; b; c; d; e; f; g; h; i; j]
      | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
          List.iter go [a; b; c; d; e; f; g; h; i; j; k]
      | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
          List.iter go [a; b; c; d; e; f; g; h; i; j; k; l]
      | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
          List.iter go [a; b; c; d; e; f; g; h; i; j; k; l; m]
      | TUPLE14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) ->
          List.iter go [a; b; c; d; e; f; g; h; i; j; k; l; m; n]
      | TUPLE15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) ->
          List.iter go [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o]
      | TLIST xs -> List.iter go xs
      | _ -> ()
    end
  in
  go root;
  List.rev !acc

(* Pull every `any_param_declaration` that sits at $unit scope — i.e.
   outside any `module ... endmodule` or `package ... endpackage`
   block.  This catches `localparam int X = ...;` declarations sitting
   at the top of a `.svh` header file that was `\`include`d into the
   `.sv` source.  Returns `(name, pvalue)` pairs in the same shape as
   `package_decl.pkg_params` so `eval_int`'s package-fallback path can
   find them without any code change there. *)
let extract_file_scope_params root : (string * pvalue) list =
  let param_nodes = collect_outside_module_or_package
    (has_tag (prefix_is "any_param_declaration")) root in
  List.filter_map (fun node ->
    let id_subs = collect_by
      (has_tag (prefix_is "param_type_followed_by_id")) node in
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
    | Some name -> Some (name, extract_pvalue_from_param_decl ~name node)
  ) param_nodes

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
        (* SOURCE order (collect_by yields reverse), so an earlier localparam is
           recorded in param_vw before a later one references it in a concat
           (e.g. RV_DM_JTAG_IDCODE = {JTAG_VERSION, …, JEDEC_MANUFACTURER_ID, …}). *)
        let param_nodes = List.rev (collect_by
          (has_tag (prefix_is "any_param_declaration")) node) in
        let params = List.filter_map (fun pn ->
          let id_subs = collect_by
            (has_tag (prefix_is "param_type_followed_by_id")) pn in
          let pname = match id_subs with
            | s :: _ ->
                let ids = ref [] in
                walk (function SymbolIdentifier id ->
                              ids := id :: !ids | _ -> ()) s;
                (* The param NAME is the LAST identifier in `<type> NAME` — `!ids`
                   is reverse-visit order so its head IS the last id.  Taking the
                   FIRST id named a USER-TYPED param after its type
                   (`ibex_mubi_t IbexMuBiOn` -> "ibex_mubi_t"), so the real name
                   never entered pkg_params and bare `IbexMuBiOn` never resolved. *)
                (match !ids with
                 | last :: _ -> Some last
                 | [] -> None)
            | [] -> None
          in
          match pname with
          | None -> None
          | Some name -> Some (name, extract_pvalue_from_param_decl ~name pn)
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
      (* Plain integer literal? (int_of_pvalue understands the Verilog
       * based form `'h..`/`'b..`/`'o..` that leaf_text now emits, as well
       * as plain decimals.) *)
      (match int_of_pvalue (PStr s) with
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

(* type_widths (type name -> packed bit width, populated by collect_type_widths
   in specialise_design) is declared near the top of this file so eval_cw can
   fold `$bits(<type>)`.  Consulted by resolve_value / Eval / eval_cw. *)

(* Resolve an override expression to a printable string, folding
 * `pkg::name` references through the package table when possible.
 * Returns the deep stringification by default so the constant
 * evaluator sees the full expression text — falling back to the
 * single-leaf value_of only for trivially-wrapped scalars. *)
let rec resolve_value (pkgs : package_decl list) tok =
  (* `$bits(<type>)` overrides (e.g. `.Width($bits(dcsr_t))`) fold to the
   * registered type width; deep-stringification would otherwise mangle the
   * type name into the specialization suffix and leave the width unresolved. *)
  let bits_width () =
    match collect_by (function
      | TUPLE3 (STRING t, SystemTFIdentifier ("$bits" | "bits"), _)
        when prefix_is "system_tf_call" t -> true
      | _ -> false) tok with
    | (TUPLE3 (_, _, call_base)) :: _ ->
        let last = ref None in
        walk (function
          | SymbolIdentifier id when Hashtbl.mem type_widths id -> last := Some id
          | _ -> ()) call_base;
        Option.map (Hashtbl.find type_widths) !last
    | _ -> None
  in
  match bits_width () with
  | Some w -> string_of_int w
  | None ->
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

  (* When the tokenizer can't make sense of a character or sub-string,
     it raises this rather than silently substituting TNum 0 — the
     top-level eval_string catches it via its existing `with _ -> None`
     and returns None.  Caller sees "I can't evaluate" instead of a
     fabricated zero (task #139 root cause: `'{...}` array literals
     were tokenising to TNum 0 from the `'<not-0/1>` fallback).      *)
  let tokenize s =
    let n = String.length s in
    let i = ref 0 in
    let out = ref [] in
    let push t = out := t :: !out in
    let bail what =
      raise (Failure
        (Printf.sprintf "eval_tokenize: %s in %S at offset %d" what s !i)) in
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
      | '"' ->
          (* String literal → a STABLE large hash, so `==`/`!=` on strings
             evaluate correctly without threading a full string value type
             through the integer evaluator.  lowRISC's `localparam int UseDsp
             = CounterWidth < 49 ? "yes" : "no";` then `if (UseDsp == "yes")`
             folds to hash("yes") on both sides → 1.  The high bit is forced
             set so a string hash never collides with a small integer literal
             (equal strings → equal hash; different strings collide only with
             negligible probability, and string-vs-int compares are nonsense
             SV that never appears in a real generate condition). *)
          let j = ref (!i + 1) in
          while !j < n && s.[!j] <> '"' do incr j done;
          let str = String.sub s (!i + 1) (!j - !i - 1) in
          push (TNum (0x40000000 lor (Hashtbl.hash str land 0x3fffffff)));
          i := if !j < n then !j + 1 else !j
      | '\'' ->
          (* `'0`, `'1`, `'x`, `'z` — bare unsized fill literals.  Also
             unsized based literals `'h..`, `'b..`, `'o..`, `'d..` (these
             are what [leaf_text] emits for a based number whose width
             token verible kept separate).  `'{` (assignment pattern) and
             anything else bail rather than fold to a bogus 0 (task #139). *)
          if !i + 1 < n then begin
            let nc = Char.lowercase_ascii s.[!i + 1] in
            (match nc with
             | '0' | 'x' | 'z' -> push (TNum 0); i := !i + 2
             | '1' -> push (TNum 1); i := !i + 2
             | 'h' | 'b' | 'o' | 'd' ->
                 let k = ref (!i + 2) in
                 while !k < n &&
                       (let cc = s.[!k] in
                        is_dig cc || (cc >= 'a' && cc <= 'f')
                                  || (cc >= 'A' && cc <= 'F') || cc = '_')
                 do incr k done;
                 let digits =
                   String.concat "" (String.split_on_char '_'
                     (String.sub s (!i + 2) (!k - !i - 2))) in
                 let v =
                   try
                     match nc with
                     | 'h' -> int_of_string ("0x" ^ digits)
                     | 'b' -> int_of_string ("0b" ^ digits)
                     | 'o' -> int_of_string ("0o" ^ digits)
                     | _   -> int_of_string digits
                   with _ -> bail (Printf.sprintf "based literal %S failed" digits)
                 in
                 push (TNum v); i := !k
             | _ -> bail (Printf.sprintf "unhandled apostrophe-prefix '%c" s.[!i + 1]))
          end else bail "trailing apostrophe"
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
              with _ ->
                bail (Printf.sprintf "sized literal int_of_string %S failed"
                        digits)
            in
            push (TNum v); i := !k
          end else begin
            let v = try int_of_string lead
                    with _ -> bail (Printf.sprintf
                      "decimal literal int_of_string %S failed" lead) in
            push (TNum v); i := !j
          end
      | _ when is_id_start c ->
          let j = ref !i in
          while !j < n && is_id s.[!j] do incr j done;
          push (TId (String.sub s !i (!j - !i)));
          i := !j
      | c ->
          (* Unknown punctuation (`{`, `}`, `;`, `[`, `]`, …) means
             this isn't a plain integer expression — bail rather than
             silently skip and let parse_expr proceed against a
             mis-tokenised stream (task #139).                        *)
          bail (Printf.sprintf "unhandled char %C" c)
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
    | TDollar "bits" :: TLP :: rest ->
        (* $bits(<type>) / $bits(pkg::<type>): the LAST identifier before `)`
           is the type name — look up its width.  Falls through to the numeric
           parse (arg's value) only when no identifier resolves as a type. *)
        let rec upto acc = function
          | TRP :: r -> (List.rev acc, r) | x :: r -> upto (x :: acc) r | [] -> (List.rev acc, []) in
        let inside, r = upto [] rest in
        let last_ty = List.fold_left (fun acc t -> match t with
          | TId s when Hashtbl.mem type_widths s -> Some (Hashtbl.find type_widths s)
          | _ -> acc) None inside in
        (if Sys.getenv_opt "TYPEW_DEBUG" <> None then
           Printf.eprintf "[bits] $bits(...) inside_ids=[%s] -> %s\n%!"
             (String.concat "," (List.filter_map (function TId s -> Some s | _ -> None) inside))
             (match last_ty with Some w -> string_of_int w | None -> "None"));
        (match last_ty with
         | Some _ -> (last_ty, r)
         | None ->
             (* not a known type: keep the old behaviour ($bits(expr) -> arg) *)
             let arg, _ = parse_expr scope rest in (arg, r))
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
          (* Skip optional whitespace before the `'`. *)
          while !j < n && (s.[!j] = ' ' || s.[!j] = '\t') do incr j done;
          (* A cast is `id '(...)`.  deep_string_of_token renders the tick and
             paren with spaces (`RegFileDataWidth ' ( … )`), so also skip
             whitespace BETWEEN the `'` and the `(` — otherwise the cast isn't
             recognised and `parse_expr` evaluates just the width identifier
             (RegFileDataWidth=32), which is how WordZeroVal='0-cast became 32
             and x0 read non-zero in the register file. *)
          let paren_after_tick =
            !j < n && s.[!j] = '\'' &&
            (let k = ref (!j + 1) in
             while !k < n && (s.[!k] = ' ' || s.[!k] = '\t') do incr k done;
             !k < n && s.[!k] = '(') in
          if paren_after_tick then begin
            (* `(`, skipping ws after the tick. *)
            let k = ref (!j + 1) in
            while !k < n && (s.[!k] = ' ' || s.[!k] = '\t') do incr k done;
            let idname = String.sub s !i (id_end - !i) in
            (* If the cast type is a WIDTH that resolves in scope (e.g.
               `RegFileDataWidth'(SecdedInv3932ZeroWord)`, width 32), the cast
               TRUNCATES to that many bits — dropping it kept the full 39-bit
               0x2A00000000 and misaligned the register-file concat.  Rewrite
               `W'(E)` -> `((E) % 2^W)` so the value is masked to W bits (→0).
               A non-width cast (`unsigned'(x)`) isn't in scope: drop as before
               (value-preserving). *)
            match List.assoc_opt idname scope with
            | Some w when w >= 1 && w < 62 ->
                (* find the ')' matching the cast's '(' *)
                let depth = ref 0 and p = ref !k and closed = ref (-1) in
                while !p < n && !closed < 0 do
                  (if s.[!p] = '(' then incr depth
                   else if s.[!p] = ')' then begin
                     decr depth; if !depth = 0 then closed := !p end);
                  incr p
                done;
                if !closed >= 0 then begin
                  let inner = String.sub s (!k + 1) (!closed - !k - 1) in
                  let modulus = Int64.shift_left 1L w in
                  Buffer.add_string buf
                    (Printf.sprintf "((%s) %% %Ld)" inner modulus);
                  i := !closed + 1
                end else i := !k        (* unbalanced — just drop the cast *)
            | _ -> i := !k              (* non-width cast — drop it *)
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

(* Width of the first packed dimension [msb:lsb] under [tok], via string eval. *)
let elab_range_width ?(scope = []) tok =
  match collect_by (has_tag (prefix_is "decl_variable_dimension")) tok with
  | (TUPLE6 (_, _, msb, _, lsb, _)) :: _ ->
      (* [scope] carries package/param constants so a parameterised typedef range
         resolves, e.g. `typedef logic [IbexMuBiWidth-1:0] ibex_mubi_t` needs
         IbexMuBiWidth=4 or $bits(ibex_mubi_t) folds to 1 (a 1-bit fetch_enable
         buffer -> undriven mubi bits -> the ibex core never fetches). *)
      (match Eval.eval_string scope (deep_string_of_token msb),
             Eval.eval_string scope (deep_string_of_token lsb) with
       | Some m, Some l -> Some (abs (m - l) + 1)
       | _ -> None)
  | _ -> None

(* Populate [type_widths] from every `typedef` in [body]: a packed struct sums
 * its members' packed-dim widths (field with no dim = 1 bit); any other typedef
 * takes its own packed-dim width.  First definition wins. *)
let collect_type_widths ?(scope = []) body =
  let nodes = collect_by (has_tag (prefix_is "type_declaration")) body in
  (* returns (name, width, is_struct) *)
  let width_of_node n = match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _) ->
        let members = collect_by (has_tag (prefix_is "struct_union_member")) data_type in
        let w =
          if members <> [] then
            List.fold_left (fun acc m ->
              match elab_range_width ~scope m with
              | Some w -> acc + w
              | None ->
                  (* member with no packed dim: an enum/typedef-typed field
                     (`dtm_op_e op`) whose width is that of its TYPE, not 1. *)
                  let tw = ref None in
                  walk (function
                    | SymbolIdentifier id when !tw = None ->
                        (match Hashtbl.find_opt type_widths id with
                         | Some w -> tw := Some w | None -> ())
                    | _ -> ()) m;
                  acc + (match !tw with Some w -> w | None -> 1)) 0 members
          else (match elab_range_width ~scope data_type with Some w -> w | None -> 1) in
        Some (nm, w, members <> [])
    | _ -> None in
  (* Pass 1: register enums/scalars + provisional struct widths (first-wins). *)
  List.iter (fun n -> match width_of_node n with
    | Some (nm, w, _) when w > 0 && not (Hashtbl.mem type_widths nm) ->
        Hashtbl.replace type_widths nm w
    | _ -> ()) nodes;
  (* Pass 2: recompute STRUCT widths now every enum/typedef is registered.  A
     struct summed before its enum field's type was known UNDERCOUNTS (dmi_req_t
     = 40 vs 41, `dtm_op_e op` = 1 not 2) — which sized the DMI CDC FIFO one bit
     short and dropped an op bit crossing the tck→clk domain.  Update only on an
     improvement so a correct width is never clobbered. *)
  List.iter (fun n -> match width_of_node n with
    | Some (nm, w, true) when w > 0 ->
        (match Hashtbl.find_opt type_widths nm with
         | Some old when old >= w -> ()
         | _ -> Hashtbl.replace type_widths nm w)
    | _ -> ()) nodes;
  (* Pass 3: record per-field layout of each struct (declared order, MSB-first)
     so eval_cw can pack `'{field: value}` literals. Runs after all widths known. *)
  let member_field m =
    let fw = match elab_range_width ~scope m with
      | Some w -> w
      | None ->
          let tw = ref None in
          walk (function
            | SymbolIdentifier id when !tw = None ->
                (match Hashtbl.find_opt type_widths id with Some w -> tw := Some w | None -> ())
            | _ -> ()) m;
          (match !tw with Some w -> w | None -> 1) in
    (* field name = LAST identifier of the member (`<type> <name>`) *)
    let last = ref None in
    walk (function SymbolIdentifier id -> last := Some id | _ -> ()) m;
    match !last with Some nm -> Some (nm, fw) | None -> None
  in
  List.iter (fun n -> match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _) ->
        let members = collect_by (has_tag (prefix_is "struct_union_member")) data_type in
        if members <> [] && not (Hashtbl.mem struct_fields nm) then begin
          (* collect_by yields REVERSE source order; reverse to declared order
             (first field = MSB) so packing matches SV's packed-struct layout. *)
          let fields = List.rev (List.filter_map member_field members) in
          if fields <> [] then Hashtbl.replace struct_fields nm fields
        end
    | _ -> ()) nodes;
  if Sys.getenv_opt "TYPEW_DEBUG" <> None then
    List.iter (fun n -> match width_of_node n with
      | Some (nm, w, m) -> Printf.eprintf "[typew] %s = %d (struct=%b)\n%!" nm w m
      | None -> ()) nodes

(* Map struct/enum-typed SIGNAL / PORT names to their type width, so
 * `$bits(<signal>)` folds (e.g. dm_csrs's `.Width($bits(dmi_resp_o))` where
 * `dm::dmi_resp_t dmi_resp_o` is 34-bit).  Must run AFTER all typedefs are
 * registered (package structs included).  The declared name is the LAST
 * identifier of a data/port declaration whose type is a known struct. *)
let collect_signal_widths body =
  List.iter (fun n ->
    let tw = ref None and last = ref None in
    walk (function
      | SymbolIdentifier id ->
          last := Some id;
          if !tw = None && Hashtbl.mem type_widths id then tw := Hashtbl.find_opt type_widths id
      | _ -> ()) n;
    match !tw, !last with
    | Some w, Some nm when w > 0 && not (Hashtbl.mem type_widths nm) ->
        Hashtbl.replace type_widths nm w
    | _ -> ())
    (collect_by (has_tag (fun s ->
       prefix_is "data_declaration" s || prefix_is "port_declaration_noattr" s)) body)

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
          (* walk prepends, so !ids is reverse-source-order and its HEAD is
             the LAST source id — the param NAME.  A ranged/user-typed param
             (`logic [DataWidth-1:0] WordZeroVal = '0`) also has the type's
             identifier (DataWidth) EARLIER in source; List.rev-then-head
             wrongly took that, so WordZeroVal's `='0` default registered
             under "DataWidth" and the body reference fell back to the width
             (32) — x0 read 32 in the register file. Take !ids head. *)
          (match !ids with last :: _ -> Some last | [] -> None)
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
          (* walk prepends, so !ids is reverse-source-order and its HEAD is
             the LAST source id — the param NAME.  A ranged/user-typed param
             (`logic [DataWidth-1:0] WordZeroVal = '0`) also has the type's
             identifier (DataWidth) EARLIER in source; List.rev-then-head
             wrongly took that, so WordZeroVal's `='0` default registered
             under "DataWidth" and the body reference fell back to the width
             (32) — x0 read 32 in the register file. Take !ids head. *)
          (match !ids with last :: _ -> Some last | [] -> None)
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

(* A resolved parameter override value: kept TYPED end-to-end through the
   specialisation pipeline so an integer never has to be stringified and
   re-parsed.  OStr is a genuine string param (VMEM path, a quoted literal, or an
   as-yet-unresolved reference propagated one level up). *)
type oval = OInt of int | OStr of string

let string_of_oval = function OInt n -> string_of_int n | OStr s -> s

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
        let vs = string_of_oval v in
        let v' =
          (* Strip leading apostrophe / format prefix from numbers. *)
          try
            let i = String.index vs '\'' in
            String.sub vs (i + 2) (String.length vs - i - 2)
          with Not_found -> vs
        in
        (* keep the mangled name a plain identifier: string param values
           arrive quoted ("GALOIS") — drop any non-alphanumeric chars *)
        let v' = String.to_seq v'
                 |> Seq.filter (fun c ->
                      (c >= '0' && c <= '9') || (c >= 'a' && c <= 'z')
                      || (c >= 'A' && c <= 'Z'))
                 |> String.of_seq in
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

let specialise_design ?(pkgs = []) ?(top_params : (string * string) list = [])
    (mods : module_decl list) ~top_name =
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
  (* Register type widths so `$bits(<type>)` folds during specialization.
     Feed package constants as the eval scope so a parameterised typedef range
     (`logic [IbexMuBiWidth-1:0] ibex_mubi_t`) resolves to its real width. *)
  Hashtbl.clear type_widths;
  let pkg_scope =
    List.concat_map (fun (p : package_decl) ->
      List.filter_map (fun (n, v) ->
        match int_of_pvalue v with Some i -> Some (n, i) | None -> None)
        p.pkg_params) pkgs in
  (* PACKAGES FIRST: a module's packed struct can name a package enum/typedef
     (`priv_lvl_e mpp` in ibex's status_t, from ibex_pkg), but a package can never
     reference a module-local type.  Registering module bodies first summed such a
     struct while the package enum was still unknown — the field took the width-1
     None fallback (status_t = 5 not 6 → mstatus Width/ResetValue off by a bit).
     Collect packages, then modules, so every shared type is known downstream. *)
  List.iter (fun p -> collect_type_widths ~scope:pkg_scope p.pkg_body) pkgs;
  List.iter (fun m -> collect_type_widths ~scope:pkg_scope m.m_body) mods;
  (* second pass: struct-typed signal/port names (needs all typedefs first) *)
  List.iter (fun m -> collect_signal_widths m.m_body) mods;
  List.iter (fun p -> collect_signal_widths p.pkg_body) pkgs;
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
    (* Overrides arrive already TYPED (oval): an OInt is an integer scope entry,
       an OStr is a string parameter (VMEM path, "TDP", …) that must stay OUT of
       the int scope — it travels via the str_env path instead.  No re-parsing. *)
    let scope = List.filter_map (function
      | (k, OInt n) -> Some (k, n)
      | (_, OStr _) -> None
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
        (* A string-typed param DEFAULT (empty "" or a quoted literal) must not be
           Eval-coerced into the int scope: an empty default folds to a junk int
           (2^30) that then shadows the real string override during propagation. *)
        let st = String.trim s in
        if st = "" || (String.length st >= 1 && st.[0] = '"') then sc
        else match Eval.eval_string sc s with
        | Some n -> (name, n) :: sc
        | None -> sc
    ) scope defaults in
    (* Localparams from collect_by come in REVERSE parse-tree order
       (and Verible's parse-tree is itself reverse-of-source-order
       for child cons-lists), which means the source-order chain
       `localparam H = ...; localparam H2 = ...; localparam ICW = $clog2(H+1);`
       processes ICW BEFORE H, leaving ICW unresolved.  Iterate to
       fixed-point: each pass folds a fresh batch of newly-resolvable
       params; stop when no new ones bind.  Two passes covers the
       chain depth in real designs (rope: H→ICW). *)
    let lps = extract_module_internal_params mdecl.m_body in
    (* Package-scoped constants (enum members + localparams) as a low-priority
     * base, so a generate condition that references a bare imported enum —
     * ibex_alu's `if (RV32B != RV32BNone)` with `import ibex_pkg::*` — resolves
     * and the dead RV32B branch is PRUNED.  Module params/localparams keep
     * priority.  Needed in param_vw too so eval_cw resolves bare enum refs
     * (e.g. PRIV_LVL_U inside a localparam's struct literal). *)
    let pkg_consts =
      List.concat_map (fun (p : package_decl) ->
        List.filter_map (fun (n, v) ->
          Option.map (fun i -> (n, i)) (int_of_pvalue v)) p.pkg_params) pkgs in
    (* eval_cw folds via param_vw; expose pkg consts + scope + each localparam as
       it binds (so a struct-literal localparam like MSTATUS_RST_VAL packs), then
       restore so this module's scope doesn't leak. *)
    let touched = List.sort_uniq compare
        (List.map fst scope @ List.map fst lps @ List.map fst pkg_consts) in
    let saved = List.map (fun k -> (k, Hashtbl.find_opt param_vw k)) touched in
    List.iter (fun (k, v) ->
      if not (Hashtbl.mem param_vw k) then Hashtbl.replace param_vw k (v, 32)) pkg_consts;
    List.iter (fun (k, v) -> Hashtbl.replace param_vw k (v, 32)) scope;
    let rec fixed_point sc remaining =
      let sc', remaining' = List.fold_left (fun (sc, rem) (name, rhs_tok) ->
        if List.mem_assoc name sc then (sc, rem)
        else
          let bind n = Hashtbl.replace param_vw name (n, 32); ((name, n) :: sc, rem) in
          match eval_cw rhs_tok with
          | Some (n, _) -> bind n
          | None ->
              (match Eval.eval_string sc (resolve_value pkgs rhs_tok) with
               | Some n -> bind n
               | None -> (sc, (name, rhs_tok) :: rem))
      ) (sc, []) remaining in
      if List.length sc' > List.length sc && remaining' <> [] then
        fixed_point sc' remaining'
      else sc'
    in
    let scope = fixed_point scope lps in
    List.iter (fun (k, ov) -> match ov with
      | Some v -> Hashtbl.replace param_vw k v
      | None -> Hashtbl.remove param_vw k) saved;
    scope @ List.filter (fun (n, _) -> not (List.mem_assoc n scope)) pkg_consts
  in
  (* [str_env] = the enclosing module's own overrides (its s_params), so a child
     instantiation `#(.MemInitFile(SRAMInitFile))` whose RHS is a bare STRING
     parameter of the parent resolves to that parent value — propagating string
     params (VMEM paths, RAM_MODE, …) down the hierarchy, which the int-only
     Eval path cannot do. *)
  let resolve_overrides_with scope str_env ovs =
    (* Let the TYPED token evaluator see the instance's own param scope, so an
       override referencing a sibling param resolves. Save/restore the touched
       keys so this instance's scope doesn't leak into the next. *)
    let saved = List.map (fun (k, _) -> (k, Hashtbl.find_opt param_vw k)) scope in
    List.iter (fun (k, v) -> Hashtbl.replace param_vw k (v, 32)) scope;
    let result = List.map (fun (name, tok) ->
      let v : oval =
        (* Evaluate the typed override token DIRECTLY (eval_cw handles sized
           literals `32'd1`/`32'h40101104`, concats and param refs) — NOT the
           fragile stringify→re-parse `Eval.eval_string` round-trip that dropped
           e.g. `.RV(32'd1)` to empty (mtvec/mstatus/dcsr reset-value loss). The
           result stays TYPED (OInt), so int_scope_of never re-parses it. *)
        match eval_cw tok with
        | Some (n, _) -> OInt n
        | None ->
            if debug then
              Printf.eprintf "[eval_cw-None] override %s = %S\n%!"
                name (String.trim (resolve_value pkgs tok));
            (* Decide int-vs-string from the TYPED token, not by inspecting the
               resolved string's characters.  A token carrying any numeric leaf,
               operator or system-function call is an INTEGER expression eval_cw
               couldn't yet fold (e.g. `$clog2(...)` — resolve_value reduces it
               via the type/width tables, so parse that decimal); a token that is
               a pure reference (identifiers only) is a STRING parameter —
               propagate via str_env (carrying the parent's TYPED value), or raw
               for one-level-up resolution. *)
            let numeric = ref false in
            walk (function
              | TK_DecNumber _ | TK_UnBasedNumber _
              | TK_BinDigits _ | TK_HexDigits _ | TK_OctDigits _ | TK_DecDigits _
              | PLUS | HYPHEN | STAR | STAR_STAR | SLASH | PERCENT | LT_LT | GT_GT
              | SystemTFIdentifier _ -> numeric := true
              | _ -> ()) tok;
            let s = String.trim (resolve_value pkgs tok) in
            if !numeric then
              (match int_of_string_opt s with
               | Some n -> OInt n
               | None ->
                   failwith (Printf.sprintf
                     "resolve_overrides: could not fold numeric override %s=%s \
                      (extend eval_cw)." name s))
            else
              (match List.assoc_opt s str_env with Some sv -> sv | None -> OStr s)
      in
      (name, v)
    ) ovs in
    List.iter (fun (k, ov) -> match ov with
      | Some v -> Hashtbl.replace param_vw k v
      | None -> Hashtbl.remove param_vw k) saved;
    result
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
            (* s_params is the string-valued public interface consumed by
               convert_module; render the typed overrides at this boundary. *)
            s_params = List.map (fun (k, v) -> (k, string_of_oval v)) overrides;
            s_inst = inst_label;
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
              let ovs = resolve_overrides_with scope overrides inst.i_overrides_tok in
              if debug then
                Printf.eprintf "[elab]   inst %s of %s: %s\n%!"
                  inst.i_inst inst.i_module
                  (String.concat ", "
                    (List.map (fun (k, v) -> k^"="^string_of_oval v) ovs));
              let child_suffix = suffix_of_params ovs in
              let child_sname = inst.i_module ^ child_suffix in
              Hashtbl.replace inst_specialised
                (sname, inst.i_inst) child_sname;
              visit ~inst_label:(Some inst.i_inst) inst.i_module ovs
            end
          ) (extract_instantiations ~scope mdecl.m_body)
        end
  in
  (* top_params is the string-valued public interface (pinned from the driver);
     lift it into the typed domain ONCE here.  Eval.eval_string is used only as
     the boundary parser for these externally-supplied strings — the internal
     pipeline never re-parses. *)
  let top_params_ov = List.map (fun (k, s) ->
    (* A quoted string parameter value (SRAMInitFile="…/johnson.vmem") is NOT an
       integer: Eval.eval_string would coerce the path to a junk number, which
       then mangles the specialised suffix (S1694613367 vs the cleaned path) and
       corrupts the value propagated to ram_2p.MemInitFile.  Keep quoted strings
       as OStr; only int-parse a bare numeric literal. *)
    let st = String.trim s in
    if String.length st >= 1 && st.[0] = '"' then (k, OStr s)
    else match Eval.eval_string [] s with
      | Some n -> (k, OInt n)
      | None -> (k, OStr s)) top_params in
  visit ~inst_label:None top_name top_params_ov;
  Hashtbl.fold (fun _ v acc -> v :: acc) seen []
  |> List.sort (fun a b -> compare a.s_name b.s_name)

(* ─── Multi-file convenience ─────────────────────────────────────── *)

(* Parse multiple SV files and yield (modules, packages). *)
(* Resolve DERIVED package localparams.  extract_pvalue_from_param_decl is a
   crude first-leaf extractor with no scope, so a chained localparam like
     localparam logic [63:0] HaltAddress   = 64'h800;
     localparam logic [63:0] ResumeAddress = HaltAddress + 8;
   left ResumeAddress as PStr "HaltAddress" (the first identifier leaf), and any
   body reference `dm::ResumeAddress` stayed an unresolved bare id.  Re-evaluate
   each package's params in fixpoint against an accumulating int scope seeded
   with the literal params, using the same resolve_value+Eval path as the
   module-level int_scope_of.  Only params that newly fold to an int are
   updated; everything else keeps its existing pvalue (non-regressive). *)
let resolve_pkg_param_chains (pkgs : package_decl list) : package_decl list =
  List.map (fun (p : package_decl) ->
    if p.pkg_body = EMPTY_TOKEN then p else
    let pn_list =
      let nodes = collect_by
        (has_tag (prefix_is "any_param_declaration")) p.pkg_body in
      List.filter_map (fun pn ->
        let id_subs = collect_by
          (has_tag (prefix_is "param_type_followed_by_id")) pn in
        let name = match id_subs with
          | s :: _ ->
              let ids = ref [] in
              walk (function SymbolIdentifier id -> ids := id :: !ids
                           | _ -> ()) s;
              (* LAST id is the name (see extract_packages); !ids head = last. *)
              (match !ids with last :: _ -> Some last | [] -> None)
          | [] -> None in
        let rhs = match collect_by
                    (has_tag (prefix_is "trailing_assign")) pn with
          | a :: _ -> Some a | [] -> None in
        match name, rhs with Some n, Some r -> Some (n, r) | _ -> None) nodes in
    let seed = List.filter_map (fun (n, v) ->
      match int_of_pvalue v with Some i -> Some (n, i) | None -> None)
      p.pkg_params in
    let rec fixpoint scope =
      let scope', changed = List.fold_left (fun (sc, ch) (n, rhs_tok) ->
        if List.mem_assoc n sc then (sc, ch)
        else match Eval.eval_string sc (resolve_value pkgs rhs_tok) with
          | Some i -> ((n, i) :: sc, true)
          | None -> (sc, ch)) (scope, false) pn_list in
      if changed then fixpoint scope' else scope' in
    let resolved = fixpoint seed in
    let pkg_params' = List.map (fun (n, v) ->
      match int_of_pvalue v with
      | Some _ -> (n, v)                        (* already a literal *)
      | None -> (match List.assoc_opt n resolved with
                 | Some i -> (n, PInt i) | None -> (n, v))) p.pkg_params in
    { p with pkg_params = pkg_params' }
  ) pkgs

let parse_files_full files =
  let mods = ref [] in
  let pkgs = ref [] in
  let file_scope = ref [] in
  let file_types = ref [] in
  List.iter (fun f ->
    match Sv_verible_to_ir.parse_verible_file f with
    | None ->
        (* BOMB by default: a parse failure silently DROPS every module in the
           file.  A dropped module that is instantiated elsewhere leaves the
           instance with no port list — its `.*` / by-name connections vanish and
           Vivado later reports "module not found" with no hint at the real cause
           (e.g. prim_generic_clock_mux2's SVA `ASSERT failed to parse until
           SYNTHESIS was defined).  Fail loudly instead.  Escape hatch
           SVS_ALLOW_PARSE_FAIL=1 for a file that is genuinely unused (an
           unreferenced vendor .sv left in the file list). *)
        if Sys.getenv_opt "SVS_ALLOW_PARSE_FAIL" <> None then
          Printf.eprintf "[verible] WARNING: parse failed for %s (skipped; \
                          its modules are dropped)\n%!" f
        else
          failwith (Printf.sprintf
            "[verible] parse failed for %s — every module it defines is dropped, \
             so any instance of them gets empty ports / 'module not found' \
             downstream.  Fix the parse error (a missing define like SYNTHESIS \
             for SVA `ASSERT is a common cause), or set SVS_ALLOW_PARSE_FAIL=1 \
             if the file is genuinely unused." f)
    | Some root ->
        mods := extract_modules root @ !mods;
        pkgs := extract_packages root @ !pkgs;
        file_scope := extract_file_scope_params root @ !file_scope;
        (* $unit-scope `typedef`s (e.g. `typedef int unsigned count_t;` above a
           `module`, as in debounce.sv).  These are visible to every module in
           the compilation unit but were captured nowhere, so a `count_t'(x)`
           cast fell back to eval_int(count_t) -> the parameter value, blowing a
           compare out to a 500-bit CARRY4 chain.  Fold them into the $unit
           package body so convert_files_inner's per-package extract_typedefs
           registers their widths in type_widths. *)
        file_types := collect_outside_module_or_package
          (has_tag (prefix_is "type_declaration")) root @ !file_types
  ) files;
  (* Collapse all file-scope localparams (from `.svh` headers and the
     `.sv` files themselves outside their `module ... endmodule`
     blocks) into a single synthetic package called "$unit".  eval_int
     already searches every package by name when a parameter isn't in
     the module-local scope, so wrapping these in a pkg is enough —
     no eval_int change needed.                                     *)
  let unit_pkg =
    if !file_scope = [] && !file_types = [] then []
    else [{ pkg_name  = "$unit";
            pkg_params = List.rev !file_scope;
            pkg_body   = TLIST (List.rev !file_types) }] in
  (List.rev !mods, resolve_pkg_param_chains (List.rev !pkgs) @ unit_pkg)

let parse_files files = fst (parse_files_full files)
