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
(* Width prefix from a sized literal subtree (e.g. `4'b0001` → 4).
   Used by eval_int's concat handler to know how many bits each part
   contributes when folding {a, b, c} into an integer.              *)
let width_of_sized_literal tok =
  match tok with
  | TUPLE3 (STRING tag, base_token, _digits)
    when prefix_is "bin_based_number" tag
      || prefix_is "hex_based_number" tag
      || prefix_is "dec_based_number" tag
      || prefix_is "oct_based_number" tag ->
      let bs = match base_token with
        | TK_BinBase s | TK_HexBase s | TK_DecBase s | TK_OctBase s -> s
        | _ -> ""
      in
      (try
         let i = String.index bs '\'' in
         Some (int_of_string (String.sub bs 0 i))
       with _ -> None)
  | _ -> None

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
  (* Sized literals: `1'b0`, `4'hA`, `8'd255`. Verible parses each as
   * `<base>_based_number` wrapping the digits — for our purposes the
   * width prefix doesn't change the integer value. *)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "bin_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_BinDigits n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string ("0b" ^ !s)) with _ -> None)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "hex_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_HexDigits n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string ("0x" ^ !s)) with _ -> None)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "dec_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_DecNumber n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string !s) with _ -> None)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "oct_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_OctDigits n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string ("0o" ^ !s)) with _ -> None)
  | SymbolIdentifier id -> lookup id
  | TUPLE4 (STRING "add_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a + b)
       | _ -> None)
  | TUPLE4 (STRING "add_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a - b)
       | _ -> None)
  | TUPLE4 (STRING "mul_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a * b)
       | _ -> None)
  | TUPLE4 (STRING "mul_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b when b <> 0 -> Some (a / b)
       | _ -> None)
  | TUPLE4 (STRING "mul_expr4", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b when b <> 0 -> Some (a mod b)
       | _ -> None)
  | TUPLE4 (STRING "shift_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lsl b)
       | _ -> None)
  | TUPLE4 (STRING "shift_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lsr b)
       | _ -> None)
  (* `**` SystemVerilog power. Right-associative, parsed as
   * `pow_expr STAR_STAR unary_expr`. Required for packed-array
   * dimensions like `[2**NumLevels-1:0]` in lzc/cf_math_pkg. *)
  | TUPLE4 (STRING "pow_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some e when e >= 0 ->
           let rec p b e = if e = 0 then 1 else b * p b (e - 1) in
           Some (p a e)
       | _ -> None)
  (* Bitwise / logical / comparison operators on constants — extends
     eval_int's coverage so picorv32-style body localparams like
       localparam WITH_PCPI = ENABLE_PCPI || ENABLE_MUL || …;
       localparam irqregs_offset = ENABLE_REGS_16_31 ? 32 : 16;
     resolve at elaboration time instead of leaking through as bare
     identifiers downstream.                                          *)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "and_expr"    tag
      || prefix_is "bitand_expr" tag
      || prefix_is "logand_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a land b)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "or_expr"    tag
      || prefix_is "bitor_expr" tag
      || prefix_is "logor_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lor b)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "xor_expr"    tag
      || prefix_is "bitxor_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lxor b)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a <  b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a >  b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr4", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a <= b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr5", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a >= b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "logeq_expr2"     tag
      || prefix_is "binary_eq_expr1" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a = b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "logneq_expr"     tag
      || prefix_is "binary_neq_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a <> b then 1 else 0)
       | _ -> None)
  (* Ternary `cond ? t : e` — used by irqregs_offset and many similar
     control-knob localparams.                                         *)
  | TUPLE6 (STRING tag, cond, _, t, _, e) when prefix_is "cond_expr" tag ->
      (match eval_int ~pkgs ~params cond with
       | Some 0 -> eval_int ~pkgs ~params e
       | Some _ -> eval_int ~pkgs ~params t
       | None -> None)
  (* Concat `{a, b, c}` — fold to integer when every part is a sized
     literal (so we know its width).  picorv32-style trace-flag
     localparams like
       localparam [35:0] TRACE_BRANCH = {4'b0001, 32'b0};
     are this shape exactly.  Body is a TLIST in reverse source
     order; reverse for MSB → LSB.                                   *)
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let parts = match body with
        | TLIST xs ->
            List.filter (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN -> false | _ -> true) (List.rev xs)
        | other -> [other]
      in
      let folded =
        List.fold_left (fun acc part ->
          match acc with
          | None -> None
          | Some acc_val ->
              (match eval_int ~pkgs ~params part,
                     width_of_sized_literal part with
               | Some v, Some w ->
                   let mask = (1 lsl w) - 1 in
                   Some ((acc_val lsl w) lor (v land mask))
               | _ -> None)
        ) (Some 0) parts
      in
      folded
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

(* Return every packed dimension as `(msb, lsb)`, in declaration
 * order (outermost first). For `logic [WIDTH-1:0][NumLevels-1:0]
 * index_lut`, returns [(WIDTH-1, 0); (NumLevels-1, 0)] — the outer
 * dim is the array index, the inner is the per-element width. The
 * grammar shape for chained dimensions is left-recursive
 * `decl_dimensions2`, so collect_by (prefix_is "decl_variable_dimension")
 * already finds them all; this helper just evaluates each. *)
let extract_packed_dims ~pkgs ~params tok =
  let pairs = collect_by
    (has_tag (prefix_is "decl_variable_dimension")) tok in
  List.filter_map (fun n ->
    match n with
    | TUPLE6 (STRING _, _, msb, _, lsb, _) ->
        let m = eval_int ~pkgs ~params msb in
        let l = eval_int ~pkgs ~params lsb in
        (match m, l with
         | Some mi, Some li -> Some (mi, li)
         | _ -> None)
    | _ -> None
  ) pairs

(* Extract every `typedef <data_type> <name>;` from the module body and
 * return [(name, width)]. Verible parses these as `type_declaration1`
 * = TUPLE6(tag, Typedef, data_type, GenericIdentifier, decl_dimensions_opt, ;).
 * Width comes from the inner data_type subtree (we just reuse
 * extract_range, which finds the packed dimension wherever it is). *)
let extract_typedefs ~pkgs ~params tok =
  let nodes = collect_by (has_tag (prefix_is "type_declaration")) tok in
  List.filter_map (fun n -> match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _) ->
        (* Check struct first — extract_range would otherwise dive
         * into the first field's packed dim and short-circuit. *)
        let members = collect_by
          (has_tag (prefix_is "struct_union_member")) data_type in
        if members <> [] then
          let total = List.fold_left (fun acc m ->
            match extract_range ~pkgs ~params m with
            | Some (mb, lb) -> acc + abs (mb - lb) + 1
            | None -> acc + 1
          ) 0 members in
          if total > 0 then Some (nm, total) else None
        else
          (match extract_range ~pkgs ~params data_type with
           | Some (m, l) -> Some (nm, abs (m - l) + 1)
           | None -> None)
    | _ -> None
  ) nodes

(* For each `typedef struct packed { ... } t;`, extract an ordered
 * list of (field_name, width). Field declaration order is MSB →
 * LSB per SV semantics: `'{f1: x, f2: y}` packs f1 in the high
 * bits, f2 in the low bits. The struct_union_member nodes appear in
 * source (= MSB-first) order in Verible's AST. *)
let extract_struct_defs ~pkgs ~params tok
    : (string * (string * int) list) list =
  let nodes = collect_by (has_tag (prefix_is "type_declaration")) tok in
  List.filter_map (fun n -> match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _)
      when (let ms = collect_by
              (has_tag (prefix_is "struct_union_member")) data_type in
            ms <> []) ->
        (* The struct_union_member_list TLIST is left-recursive, so
         * collect_by ends up walking it in reverse source order; the
         * final List.rev in collect_by un-reverses *that* but leaves
         * the original reversal in place. Reverse here to recover
         * source (= MSB-first) order. *)
        let members = List.rev (collect_by
          (has_tag (prefix_is "struct_union_member")) data_type) in
        let fields = List.filter_map (fun m ->
          let w = match extract_range ~pkgs ~params m with
            | Some (mb, lb) -> abs (mb - lb) + 1
            | None -> 1
          in
          (* Field name is the SymbolIdentifier inside the member. *)
          let fname = ref None in
          walk (function
            | SymbolIdentifier id when !fname = None -> fname := Some id
            | _ -> ()) m;
          match !fname with
          | Some id -> Some (id, w)
          | None -> None
        ) members in
        if fields <> [] then Some (nm, fields) else None
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

(* ─── Module-scoped state for struct typedef lookup ──────────────── *)
(* Set by convert_module before walking each module body.  Read by
 * expr_to_bexpr's `'{f1: x, ...}` and `p.field` handlers and by the
 * signal-table builder when it sees a struct-typed declaration. *)
let cur_struct_defs : (string * (string * int) list) list ref = ref []
(* Map of in-module signal names → their struct typedef name (for
 * member-select on bare references like `p.field`). *)
let cur_signal_struct : (string * string) list ref = ref []

(* Per-module signal name → declared width. Populated by
 * `convert_module` before any `expr_to_bexpr` call so the operator
 * builders can compute a real `result_type` width instead of falling
 * back to dummy_bool. *)
let cur_signal_widths : (string * int) list ref = ref []

(* Per-module table of array-typed localparams from .svh includes /
   inline `'{e1, e2, …}` initialisers.  Maps the localparam name to
   the list of element bexprs in source order (index 0 = first elem).
   Consulted by the reference3 walker: `LUT[i]` with i a known
   integer literal returns the i-th element directly; with i a
   runtime signal returns a balanced BCond mux tree over the
   elements.  Task #139's ROM-promotion path.                       *)
let cur_array_params : (string, bexpr list) Hashtbl.t = Hashtbl.create 4

(* ─── Expression conversion ──────────────────────────────────────── *)

let dummy_bool = BInt { width = 1; signed = Unsigned }

(* Parse a parameter value string that came in via specialise_design /
 * extract_body_params. Handles plain integers (`255`, `-1`), SV-style
 * sized literals (`8'd255`, `64'hFF`, `1'b1`), and the all-ones short-
 * hand `'1`. Falls back to `BVar id` if the value isn't recognisable
 * as a numeric constant — that way `parameter type T = …` keeps its
 * symbolic name. *)
let param_value_to_bexpr id v =
  let v = String.trim v in
  if v = "" then BVar id
  else
    let parse_radix base s =
      try Some (int_of_string ("0" ^ base ^ s))
      with _ -> None in
    let parse_dec s = try Some (int_of_string s) with _ -> None in
    (* Sized literal: <w>'<base><digits>. Width determines bit-width. *)
    (match String.index_opt v '\'' with
     | Some i when i > 0 && i + 1 < String.length v ->
         let w_str = String.sub v 0 i in
         let base = v.[i + 1] in
         let digits = String.sub v (i + 2) (String.length v - i - 2) in
         let w = try int_of_string w_str with _ -> 32 in
         let n = match base with
           | 'd' | 'D' -> parse_dec digits
           | 'h' | 'H' -> parse_radix "x" digits
           | 'b' | 'B' -> parse_radix "b" digits
           | 'o' | 'O' -> parse_radix "o" digits
           | _ -> None in
         (match n with
          | Some n -> BConst { value = n; width = w }
          | None -> BVar id)
     | Some 0 ->
         (* `'1` / `'0` — width inferred elsewhere; use 32 as default *)
         (try
           let bit = String.sub v 1 (String.length v - 1) in
           if bit = "1" then BConst { value = -1; width = 32 }
           else if bit = "0" then BConst { value = 0; width = 32 }
           else BVar id
          with _ -> BVar id)
     | _ ->
         (match parse_dec v with
          | Some n -> BConst { value = n; width = 32 }
          | None -> BVar id))

(* Best-effort expr → bexpr translator. Walks one level at a time,
 * recursing where the shape is recognised. Anything else becomes a
 * 1-bit zero with a stderr note (when MITER_VERIBLE_DEBUG is set). *)
let debug_expr = lazy (Sys.getenv_opt "MITER_VERIBLE_DEBUG" <> None)

(* Unhandled-syntax safety net.  Each "fall back to BConst 0" path
 * in the expression walker is routed through [silent_zero], which by
 * default raises [Silent_zero_substitution] so we see immediately
 * which shapes we don't handle.  Setting SV_DECOMP_LENIENT=1 reverts
 * to the historical "log + return BConst 0" behaviour for situations
 * where surfacing the bug isn't an option (e.g. a downstream consumer
 * still depends on the wrong-but-survivable BIR).  The flip from
 * default-lenient to default-strict was task #139: the rope ORFS
 * layout produced an all-zero data path because Verible silently
 * substituted 0 for an .svh-defined array localparam; making the
 * default loud means we catch the next instance at parse time
 * instead of post-route.                                              *)
exception Silent_zero_substitution of string

let lenient_mode = lazy (Sys.getenv_opt "SV_DECOMP_LENIENT" = Some "1")

let silent_zero ~reason ~width =
  if Lazy.force lenient_mode then begin
    if Lazy.force debug_expr then
      Printf.eprintf "[verible_to_bir] silent zero: %s\n" reason;
    BConst { value = 0; width }
  end else
    raise (Silent_zero_substitution reason)

(* Recursive width computation over a bexpr against `cur_signal_widths`.
 * Returns `Some w` when computable; `None` when we can't tell (caller
 * falls back to `dummy_bool`, and z3_miter's fixed-point inference
 * picks it up from operands later). *)
let rec width_of_bexpr_ctx widths = function
  | BVar n -> List.assoc_opt n widths
  | BConst { width; _ } -> Some width
  | BBinOp { op = (BEq|BNe|BLt|BLe|BGt|BGe); _ } -> Some 1
  | BBinOp { op = _; lhs; rhs; result_type } ->
      let dw = match result_type with BInt { width; _ } -> width | _ -> 0 in
      if dw > 1 then Some dw
      else
        (match width_of_bexpr_ctx widths lhs,
               width_of_bexpr_ctx widths rhs with
         | Some a, Some b -> Some (max a b)
         | Some a, None | None, Some a -> Some a
         | None, None -> None)
  | BUnOp { op = (BRedAnd|BRedOr|BRedXor); _ } -> Some 1
  | BUnOp { operand; _ } -> width_of_bexpr_ctx widths operand
  | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
  | BConcat es ->
      let ws = List.map (width_of_bexpr_ctx widths) es in
      if List.for_all Option.is_some ws then
        Some (List.fold_left (+) 0
                (List.map (Option.value ~default:0) ws))
      else None
  | BReplicate { count; value } ->
      Option.map (fun w -> count * w) (width_of_bexpr_ctx widths value)
  | BCond { then_val; else_val; _ } ->
      (match width_of_bexpr_ctx widths then_val,
             width_of_bexpr_ctx widths else_val with
       | Some a, Some b -> Some (max a b)
       | Some a, None | None, Some a -> Some a
       | _ -> None)
  | BSelect _ | BCall _ -> None

let result_type_for op lhs rhs =
  let comparison = match op with
    | BEq | BNe | BLt | BLe | BGt | BGe -> true
    | _ -> false in
  if comparison then BInt { width = 1; signed = Unsigned }
  else
    let widths = !cur_signal_widths in
    match width_of_bexpr_ctx widths lhs,
          width_of_bexpr_ctx widths rhs with
    | Some a, Some b ->
        BInt { width = max a b; signed = Unsigned }
    | Some a, None | None, Some a ->
        BInt { width = a; signed = Unsigned }
    | None, None ->
        BInt { width = 1; signed = Unsigned }   (* legacy dummy_bool fallback *)

let result_type_for_un op operand =
  let reduction = match op with
    | BRedAnd | BRedOr | BRedXor -> true
    | _ -> false in
  if reduction then BInt { width = 1; signed = Unsigned }
  else
    match width_of_bexpr_ctx !cur_signal_widths operand with
    | Some w -> BInt { width = w; signed = Unsigned }
    | None -> BInt { width = 1; signed = Unsigned }

let rec expr_to_bexpr ~pkgs ~params ~arrays tok =
  let recurse = expr_to_bexpr ~pkgs ~params ~arrays in
  let bin op a b =
    let lhs = recurse a in
    let rhs = recurse b in
    BBinOp { op; lhs; rhs; result_type = result_type_for op lhs rhs }
  in
  let un op a =
    let operand = recurse a in
    BUnOp { op; operand; result_type = result_type_for_un op operand }
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
      with _ ->
        if Lazy.force lenient_mode then 0
        else raise (Silent_zero_substitution
          (Printf.sprintf "sized literal int_of_string %S%S failed" prefix digits))
    in
    BConst { value; width }
  in
  match tok with
  | SymbolIdentifier id ->
      (match List.assoc_opt id params with
       | Some v -> param_value_to_bexpr id v
       | None -> BVar id)
  | TK_DecNumber n | TK_UnBasedNumber n ->
      (* SV unbased-unsized literals: `'0`, `'1`, `'x`, `'z`. The
       * value should be the bit pattern broadcast to the LHS width.
       * We don't know the LHS width here so emit BConst with -1
       * (all-ones in two's complement) for `'1` and 0 for the
       * others; downstream encoders mask to the destination width. *)
      let n2 = String.trim n in
      if n2 = "'1" then BConst { value = -1; width = 32 }
      else if n2 = "'0" then BConst { value = 0; width = 32 }
      else if n2 = "'x" || n2 = "'z" || n2 = "'X" || n2 = "'Z" then
        BConst { value = 0; width = 32 }
      else
        (try BConst { value = int_of_string n; width = 32 }
         with _ ->
           silent_zero ~width:32
             ~reason:(Printf.sprintf "TK_DecNumber int_of_string %S failed" n))
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
       | Some v -> param_value_to_bexpr id v
       | None -> BVar id)
  (* Array assignment pattern `'{e1, e2, …}` — Verible's
     `assignment_pattern1` wraps a TLIST of bare expressions (no key
     prefix, unlike the struct variant).  Distilled use case:
     array-typed localparam initialiser from an included .svh, as in
     test/regressions/svh_array_localparam.svh.  Without this case
     the walker fell into the silent-zero fallback and the whole
     array localparam value became 0 (task #139).  Emit BConcat of
     elements in source order; index lookups (`LUT[sel]`) are
     handled separately by the reference3 → BSelect path.            *)
  | TUPLE4 (STRING tag, _, body, _)
    when prefix_is "assignment_pattern1" tag ->
      (* The body is a left-recursive cons of `expression_list_proper`
         nodes:  TUPLE4(expression_list_proper, prev_list, comma, new_elem).
         The grammar grows the list to the LEFT (Verible parses the
         comma-separated list bottom-up), so the outermost node carries
         the LAST element on its right, with everything before it
         nested in the left child.  Walk the left spine accumulating
         right-side elements, then add the final non-list node
         (the leftmost element) at the front.                          *)
      let rec flatten_explist acc t =
        match t with
        | TUPLE4 (STRING tag', lhs, _comma, rhs)
          when prefix_is "expression_list_proper" tag' ->
            flatten_explist (rhs :: acc) lhs
        | TLIST xs ->
            (* Older / different shape — bare TLIST.  Append as-is. *)
            xs @ acc
        | other -> other :: acc
      in
      let elems = flatten_explist [] body in
      let exprs = List.map recurse elems in
      (match exprs with
       | [] -> BConst { value = 0; width = 1 }  (* `'{}` — empty *)
       | [single] -> single
       | _ -> BConcat exprs)
  (* Struct assignment pattern `'{f1: x, f2: y}`. The Verible parse
   * is `assignment_pattern2` wrapping a TLIST of
   * `structure_or_array_pattern_expression1` nodes — each carries a
   * key (the field name) and a value expression. We emit a BConcat
   * in the field's DECLARED order (MSB-first). The struct typedef
   * is looked up via `cur_struct_defs`; if no typedef matches we
   * fall back to source-order BConcat. *)
  | TUPLE4 (STRING tag, _, body, _)
    when prefix_is "assignment_pattern2" tag ->
      let pairs = match body with
        | TLIST xs ->
            List.filter_map (fun e -> match e with
              | TUPLE4 (STRING t, key, _, value)
                when prefix_is "structure_or_array_pattern_expression" t ->
                  let kname = ref None in
                  walk (function
                    | SymbolIdentifier id when !kname = None ->
                        kname := Some id
                    | _ -> ()) key;
                  (match !kname with
                   | Some k -> Some (k, recurse value)
                   | None -> None)
              | _ -> None) (List.rev xs)
        | _ -> []
      in
      (* Pick a typedef whose field set EXACTLY matches the keys —
       * this is how we link `'{hi: x, lo: y}` back to `pair_t`. *)
      let key_set = List.map fst pairs |> List.sort compare in
      let matching_def =
        List.find_opt (fun (_, fields) ->
          List.map fst fields |> List.sort compare = key_set
        ) !cur_struct_defs
      in
      (match matching_def with
       | Some (_, fields) ->
           let in_order =
             List.map (fun (fname, _w) ->
               try List.assoc fname pairs
               with Not_found -> BConst { value = 0; width = 1 }
             ) fields
           in
           (match in_order with
            | [single] -> single
            | _ -> BConcat in_order)
       | None ->
           let exprs = List.map snd pairs in
           (match exprs with
            | [single] -> single
            | _ -> BConcat exprs))
  (* Replication `{N{value}}` — Verible parses as `expr_primary_braces2`:
   *   TUPLE7(tag, LBRACE, value_range, LBRACE, expression_list_proper,
   *          RBRACE, RBRACE)
   * value_range is the count N (constant), and expression_list_proper
   * is the inner value to be replicated. Both ride on the standard
   * eval_int / recurse paths. *)
  | TUPLE7 (STRING tag, _, count_node, _, value_node, _, _)
    when prefix_is "expr_primary_braces2" tag ->
      let n = match eval_int ~pkgs ~params count_node with
        | Some n -> n
        | None -> 1
      in
      let value = recurse value_node in
      BReplicate { count = n; value }
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
           (* Find the inner expression argument.  `reference` matches
              indexed/struct/bit-select forms (reference2/3/...), and
              `reference_or_call_base` matches nested calls — without
              these the argument's index/slice is silently dropped
              (e.g. `$signed(x_mem[i])` would collapse to
              `BVar x_mem` when only `unqualified_id` was matched). *)
           let cands = collect_by (function
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "range_list_in_braces" t -> true
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "reference_or_call_base" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "reference" t -> true
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
            | None ->
                silent_zero ~width:1
                  ~reason:(Printf.sprintf
                    "unrecognised system call %s, eval_int returned None" name))
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
       | None ->
           silent_zero ~width:1
             ~reason:"function call with no resolvable callee name")
  (* Struct member-select `p.field` — Verible parses as `reference2`:
   *   TUPLE3(tag, reference, hierarchy_extension)
   * with `hierarchy_extension1: TUPLE3(tag, DOT, unqualified_id)`.
   * The signal name (typically a BVar) must be a struct-typed signal
   * in `cur_signal_struct`, and we look up the field's bit range in
   * `cur_struct_defs` to emit a BSlice. *)
  | TUPLE3 (STRING t, ref_node,
            TUPLE3 (STRING ht, _, ext_id))
    when prefix_is "reference2" t
      && prefix_is "hierarchy_extension1" ht ->
      let signal = recurse ref_node in
      let field_name = ref None in
      walk (function
        | SymbolIdentifier id when !field_name = None ->
            field_name := Some id
        | _ -> ()) ext_id;
      (match signal, !field_name with
       | BVar sig_name, Some fname ->
           (match List.assoc_opt sig_name !cur_signal_struct with
            | Some struct_name ->
                (match List.assoc_opt struct_name !cur_struct_defs with
                 | Some fields ->
                     (* Field declared first is in the MSBs.  Bit
                      * range = [W - prefix_w - 1 : W - prefix_w - field_w]. *)
                     let total_w = List.fold_left (fun a (_, w) -> a + w)
                                     0 fields in
                     let rec find_bit_range pos = function
                       | [] -> None
                       | (n, w) :: rest ->
                           if n = fname then
                             Some (total_w - pos - 1,
                                   total_w - pos - w)
                           else find_bit_range (pos + w) rest
                     in
                     (match find_bit_range 0 fields with
                      | Some (msb, lsb) ->
                          BSlice { signal; msb; lsb }
                      | None -> signal)
                 | None -> signal)
            | None -> signal)
       | _ -> signal)
  (* Bit-select / part-select on a reference: `reference3` is
   *   TUPLE3(tag, reference, select_variable_dimension).
   * The inner dimension is either select_variable_dimension2 (single
   * index `[N]`, TUPLE4) or select_variable_dimension1 (range `[M:N]`,
   * TUPLE6). For packed regs we emit BSlice; for memory arrays
   * (signal name in `arrays`) the single-index case must emit
   * BSelect so meminfer recognises the read. *)
  | TUPLE3 (STRING t, ref_node, dim_node) when prefix_is "reference" t
                                              && t <> "reference1" ->
      let signal = recurse ref_node in
      (* Array-typed localparam ROM lookup (task #139).  If `signal`
         names a localparam whose RHS was an `'{e1, …, eN}` initialiser
         that we registered into [cur_array_params], and `dim_node` is
         a single-index `select_variable_dimension2`, return either the
         constant-folded element (if the index evaluates to an integer)
         or a balanced BCond mux tree over the elements (runtime index).
         Short-circuits before the rest of reference3 so the lookup
         doesn't fall through to the BVar fallback that drops [sel].   *)
      let array_param_elems = match signal with
        | BVar n -> Hashtbl.find_opt cur_array_params n
        | _ -> None in
      if Sys.getenv_opt "TRACE_ARRAY_PARAMS" = Some "1" then
        Printf.eprintf "[ref3] signal=%s array_param_elems=%s table_size=%d\n%!"
          (match signal with BVar n -> n | _ -> "<other>")
          (match array_param_elems with
           | Some es -> Printf.sprintf "Some[%d]" (List.length es)
           | None -> "None")
          (Hashtbl.length cur_array_params);
      (match array_param_elems, dim_node with
       | Some elems, TUPLE4 (STRING dt, _, idx, _)
         when prefix_is "select_variable_dimension" dt ->
           let idx_e = recurse idx in
           (match eval_int ~pkgs ~params idx with
            | Some i ->
                (try List.nth elems i
                 with _ -> BConst { value = 0; width = 1 })
            | None ->
                let n = List.length elems in
                (* Balanced BCond mux: (idx == 0) ? e0 : (idx == 1) ? e1 : … *)
                let rec build i =
                  if i >= n - 1 then List.nth elems i
                  else BCond {
                    condition = BBinOp {
                      op = BEq;
                      lhs = idx_e;
                      rhs = BConst { value = i; width = 32 };
                      result_type = BInt { width = 1; signed = Unsigned } };
                    then_val = List.nth elems i;
                    else_val = build (i + 1) }
                in build 0)
       | _ ->
      let is_array_sig = match signal with
        | BVar n -> List.mem n arrays
        | _ -> false
      in
      (match dim_node with
       | TUPLE6 (STRING dt, _, e1, _, e2, _)
         when prefix_is "select_variable_dimension" dt ->
           let signal_width () = match signal with
             | BVar n -> List.assoc_opt n !cur_signal_widths
             | _ -> None in
           (* Distinguish dimension subtype:
                "1" → range  [m:l]   (e1=msb,  e2=lsb)
                "3" → up     [b +: w] (e1=base, e2=width)
                "4" → down   [b -: w] (e1=base, e2=width) *)
           let tag = String.sub dt
               (String.length "select_variable_dimension")
               (String.length dt
                - String.length "select_variable_dimension") in
           (match tag with
            | "3" ->
                (* `signal[base +: width]`: bits [base+width-1 : base].
                   When base is dynamic (e.g. a for-loop variable
                   that unroll will later substitute), preserve the
                   shape as a `@part_select_up` BCall so a downstream
                   pass can fold it once base becomes constant. *)
                (match eval_int ~pkgs ~params e1,
                       eval_int ~pkgs ~params e2 with
                 | Some b, Some w when w > 0 ->
                     BSlice { signal; msb = b + w - 1; lsb = b }
                 | _, Some w when w > 0 ->
                     BCall { func = "@part_select_up";
                             args = [signal;
                                     recurse e1;
                                     BConst { value = w; width = 32 }] }
                 | _ -> signal)
            | "4" ->
                (* `signal[base -: width]`: bits [base : base-width+1]. *)
                (match eval_int ~pkgs ~params e1,
                       eval_int ~pkgs ~params e2 with
                 | Some b, Some w when w > 0 ->
                     BSlice { signal; msb = b; lsb = b - w + 1 }
                 | _, Some w when w > 0 ->
                     BCall { func = "@part_select_down";
                             args = [signal;
                                     recurse e1;
                                     BConst { value = w; width = 32 }] }
                 | _ -> signal)
            | _ ->
                (* dimension 1 (range) or unknown → existing range
                   logic with $high-style fallback. *)
                (match eval_int ~pkgs ~params e1,
                       eval_int ~pkgs ~params e2 with
                 | Some m, Some l -> BSlice { signal; msb = m; lsb = l }
                 | None, Some l ->
                     (match signal_width () with
                      | Some w when w > 0 ->
                          BSlice { signal; msb = w - 1; lsb = l }
                      | _ -> signal)
                 | Some m, None ->
                     BSlice { signal; msb = m; lsb = 0 }
                 | _ -> signal))
       | TUPLE4 (STRING dt, _, idx, _)
         when prefix_is "select_variable_dimension" dt ->
           if is_array_sig then
             BSelect { array = signal; index = recurse idx }
           else
             (match eval_int ~pkgs ~params idx with
              | Some i -> BSlice { signal; msb = i; lsb = i }
              | None -> signal)
       | _ -> signal))
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
  (* `**` constant power. BIR has no native power op, so we evaluate
   * eagerly. Common case after genvar-substitution is `2 ** 0` etc.
   * which folds to a small constant. *)
  | TUPLE4 (STRING tag, lhs, _op, rhs) when prefix_is "pow_expr2" tag ->
      (match eval_int ~pkgs ~params tok with
       | Some n ->
           (* Width: at minimum cover the value. 32-bit by default
            * matches what SV unsized-int literals get. *)
           BConst { value = n; width = 32 }
       | None ->
           (* No fold — best-effort: encode as repeated mul if rhs
            * is small. For the unsupported general case, fall back
            * to lhs (drops the power). *)
           ignore rhs; recurse lhs)
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
      else
        silent_zero ~width:1
          ~reason:(Printf.sprintf "unhandled TUPLE4 expression tag %s" tag)
  (* unary_prefix_expr handled above the generic TUPLE3 fallback. *)
  (* `cond_expr2`: TUPLE6(tag, cond, QUERY, then_expr, COLON, else_expr).
   * The ternary `?:` operator. *)
  | TUPLE6 (STRING tag, cond, _, t, _, e) when prefix_is "cond_expr" tag ->
      BCond { condition = recurse cond;
              then_val = recurse t;
              else_val = recurse e }
  (* `<type>'(<expr>)` cast — value-preserving for our integer
   * arithmetic. cast1: TUPLE6(tag, casting_type, QUOTE, LPAREN,
   * expression, RPAREN). Drop the cast and recurse on the inner
   * expression. lzc.sv uses `(NumLevels)'(unsigned'(j))` to
   * size-extend the genvar index — without unwrapping these the
   * expression falls to the BConst{0,1} default. *)
  | TUPLE6 (STRING tag, _ct, _quote, _lp, expr, _rp) when prefix_is "cast" tag ->
      recurse expr
  | TLIST [single] -> recurse single
  | _ ->
      silent_zero ~width:1
        ~reason:"unhandled expression shape (no matching pattern)"

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
    (* `wire [W:0] foo = expr;` parses as net_declaration2 holding a
       net_decl_assign1 whose first SymbolIdentifier is the LHS name
       and whose third slot is the RHS expression.  Take the LHS,
       skip the RHS — without this every identifier appearing inside
       the initialiser would be wrongly promoted to a declaration with
       the enclosing wire's width. *)
    | TUPLE4 (STRING t', SymbolIdentifier id, _, _)
      when prefix_is "net_decl_assign" t' ->
        names := id :: !names
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
  (* Always run a permissive sweep to pick up bare SymbolIdentifier
     port names that walk_skip missed.  K&R `input clk, rst;` parses
     such that only the first name is wrapped in `unqualified_id`;
     subsequent comma-separated names are bare SymbolIdentifier
     leaves.  Without the loose sweep, the second name is silently
     lost and ends up classified as Internal/Output by the
     downstream port-list merge.  Dedup later via sort_uniq. *)
  let rec walk_loose t =
    match t with
    | TUPLE6 (STRING t', _, _, _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    | TUPLE4 (STRING t', _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    (* As walk_skip: take the LHS of net_decl_assign, skip the RHS.
       Without this, identifiers inside the initialiser
       (`wire [W:0] foo = mem[i];` → `mem`, `i`) are wrongly added
       as declaration names with the wrapping wire's width. *)
    | TUPLE4 (STRING t', SymbolIdentifier id, _, _)
      when prefix_is "net_decl_assign" t' ->
        names := id :: !names
    | SymbolIdentifier id -> names := id :: !names
    | TUPLE2 (a, b) -> walk_loose a; walk_loose b
    | TUPLE3 (a, b, c) -> walk_loose a; walk_loose b; walk_loose c
    | TUPLE4 (a, b, c, d) -> walk_loose a; walk_loose b; walk_loose c; walk_loose d
    | TUPLE5 (a, b, c, d, e) ->
        List.iter walk_loose [a; b; c; d; e]
    | TUPLE6 (a, b, c, d, e, f) ->
        List.iter walk_loose [a; b; c; d; e; f]
    | TUPLE7 (a, b, c, d, e, f, g) ->
        List.iter walk_loose [a; b; c; d; e; f; g]
    | TUPLE8 (a, b, c, d, e, f, g, h) ->
        List.iter walk_loose [a; b; c; d; e; f; g; h]
    | TLIST xs -> List.iter walk_loose xs
    | _ -> ()
  in
  walk_loose tok;
  let w = width_of ~pkgs ~params tok in
  (* Detect unpacked-array net decls (`wire [W:0] arr[D:0]`):
     two decl_variable_dimensions, first packed, second unpacked.
     Emit BArray so [merge_array_writes] uses (size, elem_w) from
     the type rather than its (writes_count, 1) fallback (which is
     right for cell-Verilog `assign out[i] = X` but wrong for
     source `assign in_[i] = in_$00i` where each element is many
     bits).  Single-dim case stays BInt. *)
  let dims = extract_packed_dims ~pkgs ~params tok in
  let stype =
    match dims with
    | [(im, il); (om, ol)] ->
        BArray {
          element = BInt { width = abs (im - il) + 1; signed = Unsigned };
          size = abs (om - ol) + 1;
        }
    | _ ->
        BInt { width = w; signed = Unsigned }
  in
  List.rev !names |> List.sort_uniq compare |> List.map (fun n ->
    {
      name = n;
      stype;
      direction = !dir;
      initial_value = None; attrs = [];
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

(* Same as lhs_name_of but ALSO returns the optional index token if
 * the lhs is `name[idx]` (e.g. `regs[~waddr[4:0]] <= wdata`). The
 * index lives inside a `select_variable_dimension2` (TUPLE4 with
 * `LBRACK expr RBRACK`) sibling of the unqualified_id. *)
let lhs_indexed_of tok =
  let n = ref None and idx = ref None in
  walk (function
    | TUPLE3 (STRING t, SymbolIdentifier id, _)
      when prefix_is "unqualified_id" t && !n = None -> n := Some id
    | TUPLE4 (STRING dt, _, e, _)
      when prefix_is "select_variable_dimension" dt && !idx = None ->
        (* Single index `[N]`: select_variable_dimension2 = TUPLE4. *)
        idx := Some e
    | TUPLE6 (STRING dt, _, msb, _, _, _)
      when prefix_is "select_variable_dimension" dt && !idx = None ->
        (* Range `[M:N]`: select_variable_dimension1 = TUPLE6.  Use
           the msb subtree as the "index" — we don't actually need
           the value here, just to mark this LHS as indexed for the
           array_names auto-promotion. *)
        idx := Some msb
    | _ -> ()
  ) tok;
  match !n with
  | Some name -> Some (name, !idx)
  | None -> None

(* Detect concat-LHS `{a, b, c, d}` and split into per-name (name,
   width) parts.  Returns None if lhs isn't a concat or any part
   can't be resolved to a plain name.  Per-part widths come from
   the signal-width cache. *)
let concat_lhs_parts lhs =
  match lhs with
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let parts =
        match body with
        | TLIST xs ->
            List.rev (List.filter (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN -> false
              | _ -> true) xs)
        | other -> [other] in
      let infos = List.map (fun p ->
        match lhs_indexed_of p with
        | Some (n, _) ->
            let w =
              match List.assoc_opt n !cur_signal_widths with
              | Some w -> w | None -> 1 in
            Some (n, w)
        | None -> None
      ) parts in
      if List.for_all (fun x -> x <> None) infos then
        Some (List.map (function Some x -> x | None -> assert false) infos)
      else None
  | _ -> None

let extract_assign ~pkgs ~params ~arrays tok =
  let assigns = collect_by (has_tag (prefix_is "cont_assign")) tok in
  List.concat_map (fun a ->
    match a with
    | TUPLE4 (STRING tag, lhs, _eq, rhs) when prefix_is "cont_assign" tag ->
        (match concat_lhs_parts lhs with
         | Some parts ->
             (* Split: each name gets a slice of the RHS, MSB-first. *)
             let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
             let total_w = List.fold_left (fun acc (_, w) -> acc + w) 0 parts in
             let cursor = ref total_w in
             List.map (fun (name, w) ->
               let hi = !cursor - 1 and lo = !cursor - w in
               cursor := !cursor - w;
               let part_rhs = BSlice { signal = rhs_e; msb = hi; lsb = lo } in
               BCombinational {
                 name = "assign_concat_" ^ name;
                 sensitivity = [BAny];
                 body = [BAssign { lhs = name; rhs = part_rhs }];
               }
             ) parts
         | None ->
             (match lhs_indexed_of lhs with
              | Some (name, Some idx_node) when List.mem name arrays ->
                  let idx = expr_to_bexpr ~pkgs ~params ~arrays idx_node in
                  let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
                  [BCombinational {
                    name = "assign_" ^ name;
                    sensitivity = [BAny];
                    body = [BCallStmt {
                      func = "@mem_write";
                      args = [BVar name; idx; rhs_e];
                    }];
                  }]
              | Some (name, _) ->
                  let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
                  [BCombinational {
                    name = "assign_" ^ name;
                    sensitivity = [BAny];
                    body = [BAssign { lhs = name; rhs = rhs_e }];
                  }]
              | None -> []))
    | _ -> []
  ) assigns

(* ─── Procedural statement → BIR statement ───────────────────────── *)

(* Recognised statement shapes inside an `always_*` body. Anything
 * else becomes BBlock []. *)
let rec stmt_to_bstmt ~pkgs ~params ~arrays tok =
  let recurse_e = expr_to_bexpr ~pkgs ~params ~arrays in
  let recurse_s = stmt_to_bstmt ~pkgs ~params ~arrays in
  let assign_to lhs rhs =
    (* Concat-LHS detection: `{a, b, c, d} = expr` splits into
       per-name assigns where each name takes a slice of the RHS,
       MSB-first.  Names in the concat may themselves be sliced
       (rare but handled).  Per-name width comes from the signal
       width cache. *)
    let concat_parts =
      match lhs with
      | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
          let parts =
            match body with
            | TLIST xs ->
                List.rev (List.filter (fun e -> match e with
                  | TLIST [] | EMPTY_TOKEN -> false
                  | _ -> true) xs)
            | other -> [other] in
          (* For each part, get name + optional range. *)
          let get_part p =
            let name_opt = match lhs_indexed_of p with
              | Some (n, _) -> Some n | None -> None in
            let range = match
              (let m = ref None and l = ref None in
               walk (function
                 | TUPLE6 (STRING dt, _, m_n, _, l_n, _)
                   when prefix_is "select_variable_dimension" dt
                     && !m = None ->
                     m := Some m_n; l := Some l_n
                 | _ -> ()) p;
               !m, !l)
            with
            | Some mn, Some ln -> Some (mn, ln)
            | _ -> None
            in
            (name_opt, range, p) in
          let infos = List.map get_part parts in
          if List.for_all (fun (n, _, _) -> n <> None) infos then Some infos
          else None
      | _ -> None
    in
    (match concat_parts with
     | Some infos ->
         (* Compute per-part widths: from explicit range, or from
            signal width cache, or default 1.  Then slice RHS
            MSB-first at decreasing positions. *)
         let rhs_e = recurse_e rhs in
         let part_widths = List.map (fun (n, range, _) ->
           let nm = match n with Some s -> s | None -> "?" in
           match range with
           | Some (m_node, l_node) ->
               (match recurse_e m_node, recurse_e l_node with
                | BConst { value = m; _ }, BConst { value = l; _ } ->
                    abs (m - l) + 1
                | _ -> 1)
           | None ->
               (match List.assoc_opt nm !cur_signal_widths with
                | Some w -> w | None -> 1)
         ) infos in
         let total_w = List.fold_left (+) 0 part_widths in
         let cursor = ref total_w in
         let stmts = List.map2 (fun (n_opt, _, _) w ->
           let nm = match n_opt with Some s -> s | None -> "?" in
           let hi = !cursor - 1 and lo = !cursor - w in
           cursor := !cursor - w;
           let part_rhs = BSlice { signal = rhs_e; msb = hi; lsb = lo } in
           BAssign { lhs = nm; rhs = part_rhs }
         ) infos part_widths in
         BBlock stmts
     | None ->
         (* Single LHS — fall through to the existing range / array /
            plain dispatch. *)
         (* Distinguish three select-variable-dimension shapes on the
            outermost LHS bracket.  All three are TUPLE6 with a
            "select_variable_dimensionN" tag — N tells us which:
              N=1: `[m:l]` (range, both literal indices)
              N=3: `[base +: w]` (indexed-up part-select)
              N=4: `[base -: w]` (indexed-down part-select)
            TUPLE4 is the single-index `[i]` shape (mem-write).
            We walk the LHS deliberately and stop at the first match
            so a nested inner range like `arr[i[hi:lo]]` doesn't
            get mistaken for the outer dimension. *)
         let range_kind =
           let kind = ref `None in
           let stop = ref false in
           let rec walk_outer t =
             if !stop then ()
             else match t with
               | TUPLE6 (STRING dt, _, m, _, l, _)
                 when prefix_is "select_variable_dimension" dt ->
                   let tag = String.sub dt
                       (String.length "select_variable_dimension")
                       (String.length dt
                        - String.length "select_variable_dimension") in
                   (match tag with
                    | "1" -> kind := `Range (m, l); stop := true
                    | "3" -> kind := `IndexedUp (m, l); stop := true
                    | "4" -> kind := `IndexedDown (m, l); stop := true
                    | _   -> stop := true)  (* unknown shape → leave alone *)
               | TUPLE4 (STRING dt, _, _, _)
                 when prefix_is "select_variable_dimension" dt ->
                   stop := true
               | TUPLE2 (a, b) -> walk_outer a; walk_outer b
               | TUPLE3 (a, b, c) -> List.iter walk_outer [a; b; c]
               | TUPLE4 (a, b, c, d) ->
                   List.iter walk_outer [a; b; c; d]
               | TUPLE5 (a, b, c, d, e) ->
                   List.iter walk_outer [a; b; c; d; e]
               | TUPLE6 (a, b, c, d, e, f) ->
                   List.iter walk_outer [a; b; c; d; e; f]
               | TUPLE7 (a, b, c, d, e, f, g) ->
                   List.iter walk_outer [a; b; c; d; e; f; g]
               | TUPLE8 (a, b, c, d, e, f, g, h) ->
                   List.iter walk_outer [a; b; c; d; e; f; g; h]
               | TLIST xs -> List.iter walk_outer xs
               | _ -> ()
           in
           walk_outer lhs; !kind
         in
         (match lhs_indexed_of lhs, range_kind with
          | Some (name, _), `Range (m_node, l_node) ->
              BCallStmt {
                func = "@slice_write";
                args = [BVar name;
                        recurse_e m_node;
                        recurse_e l_node;
                        recurse_e rhs];
              }
          | Some (name, _), `IndexedUp (base, width) ->
              (* `name[base +: width] <= rhs` — indexed part-select up.
                 base may be dynamic; width is constant.  Lowered by
                 [lower_part_sel_writes_in_seq] to a full-bus BAssign
                 that picks rhs into the appropriate slot when
                 base == k * width, else self-reads. *)
              BCallStmt {
                func = "@part_sel_write_up";
                args = [BVar name;
                        recurse_e base;
                        recurse_e width;
                        recurse_e rhs];
              }
          | Some (name, _), `IndexedDown (base, width) ->
              (* `name[base -: width] <= rhs` — indexed part-select down.
                 Equivalent to `[base-width+1 +: width]`. *)
              BCallStmt {
                func = "@part_sel_write_down";
                args = [BVar name;
                        recurse_e base;
                        recurse_e width;
                        recurse_e rhs];
              }
          | Some (name, Some idx_node), `None when List.mem name arrays ->
              BCallStmt {
                func = "@mem_write";
                args = [BVar name; recurse_e idx_node; recurse_e rhs];
              }
          | Some (name, _), `None -> BAssign { lhs = name; rhs = recurse_e rhs }
          | None, _ -> BBlock []))
  in
  match tok with
  (* Blocking assignment: lhs = rhs *)
  | TUPLE4 (STRING tag, lhs, _eq, rhs)
    when prefix_is "assignment_statement_no_expr" tag -> assign_to lhs rhs
  (* Non-blocking: lhs <= rhs (Verible: nonblocking_assignment1
   *   TUPLE6(tag, lhs, _, _, rhs, _)) *)
  | TUPLE6 (STRING tag, lhs, _, _, rhs, _)
    when prefix_is "nonblocking_assignment" tag -> assign_to lhs rhs
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
  (* `for (init; cond; step) body` — procedural for-loop.
   *   loop_statement1: TUPLE10(tag, For, LPAREN, init_opt, SEMICOLON,
   *                            cond_opt, SEMICOLON, step_opt, RPAREN, body)
   * Emit BFor; Behavioral_unroll handles the actual unrolling
   * downstream when init/cond/step are constants. Required for
   * lzc.sv's `flip_vector: for (int unsigned i = 0; i < WIDTH;
   * i++) in_tmp[i] = …` — without this the always_comb body
   * extracts as empty, in_tmp is undriven, and the trailing-zero
   * counter computes wrong values (cnt_o = 0 instead of 1 for
   * in_i = 2'b10). *)
  | TUPLE10 (STRING tag, _For, _, init_opt, _, cond_opt, _, step_opt, _, body)
    when prefix_is "loop_statement" tag ->
      let var_name = ref None in
      let init_expr = ref None in
      (match init_opt with
       | TUPLE5 (STRING t, _, SymbolIdentifier id, _, expr)
         when prefix_is "for_init_decl_or_assign" t ->
           var_name := Some id; init_expr := Some expr
       | TUPLE6 (STRING t, _, _, SymbolIdentifier id, _, expr)
         when prefix_is "for_init_decl_or_assign" t ->
           var_name := Some id; init_expr := Some expr
       | TUPLE4 (STRING t, lhs, _, expr)
         when prefix_is "for_init_decl_or_assign" t ->
           walk (function
             | SymbolIdentifier id when !var_name = None -> var_name := Some id
             | _ -> ()) lhs;
           init_expr := Some expr
       | _ -> ());
      let step_node = match step_opt with
        | TUPLE3 (STRING t, _, _) when prefix_is "inc_or_dec_expression" t ->
            Some step_opt
        | _ -> None
      in
      (match !var_name, !init_expr, step_node with
       | Some name, Some init_e, Some step_e ->
           let delta = match step_e with
             | TUPLE3 (_, PLUS_PLUS, _) -> 1
             | TUPLE3 (_, _, PLUS_PLUS) -> 1
             | TUPLE3 (_, HYPHEN_HYPHEN, _) -> -1
             | TUPLE3 (_, _, HYPHEN_HYPHEN) -> -1
             | _ -> 1
           in
           let init_b = BAssign { lhs = name; rhs = recurse_e init_e } in
           let cond_b = recurse_e cond_opt in
           let upd_b = BAssign {
             lhs = name;
             rhs = BBinOp {
               op = if delta < 0 then BSub else BAdd;
               lhs = BVar name;
               rhs = BConst { value = abs delta; width = 32 };
               result_type = BInt { width = 32; signed = Unsigned };
             }
           } in
           BFor { init = init_b; condition = cond_b;
                  update = upd_b; body = [recurse_s body] }
       | _ -> BBlock [])
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
let extract_always ~pkgs ~params ~arrays tok =
  let always_nodes = collect_by
    (has_tag (prefix_is "always_construct")) tok in
  List.map (fun an ->
    let has_edge = ref false in
    walk (function
      | Posedge | Negedge -> has_edge := true
      | _ -> ()) an;
    match !has_edge with
    | true ->
        (* Sequential.  Walk the event_expression collecting (edge, id)
         * pairs in source order so we can pair edges to their idents
         * properly.  For `@(posedge clk or posedge rst)` we get
         * [(`Pos, "clk"); (`Pos, "rst")].  Order in the source must
         * NOT matter — clock vs reset is decided from body shape. *)
        let edge_idents =
          let evs = collect_by (has_tag (prefix_is "event_expression")) an in
          let acc = ref [] in
          let pending = ref None in
          let collect = function
            | Posedge -> pending := Some `Pos
            | Negedge -> pending := Some `Neg
            | SymbolIdentifier id ->
                (match !pending with
                 | Some e ->
                     acc := (e, id) :: !acc;
                     pending := None
                 | None -> ())
            | _ -> ()
          in
          List.iter (walk collect) evs;
          List.rev !acc
        in
        let body_nodes = collect_by (has_tag (fun t ->
          prefix_is "seq_block" t ||
          prefix_is "nonblocking_assignment" t ||
          prefix_is "assignment_statement_no_expr" t ||
          prefix_is "conditional_statement" t ||
          prefix_is "case_statement" t ||
          prefix_is "loop_statement" t)) an in
        let body = match body_nodes with
          | b :: _ -> [stmt_to_bstmt ~pkgs ~params ~arrays b]
          | [] -> []
        in
        (* Find the outer if in the body — skipping BBlock wrappers —
         * and decide whether its condition is an async-reset test.
         * Per the project's no-name-heuristics rule, classification
         * comes purely from structure: a sensitivity ID is the reset
         * iff the outer-if condition references it (directly or
         * negated).  Returns:
         *   `Reset (rst_name, polarity)   — async reset detected
         *   `NoReset                      — body has no outer reset-if *)
        let classify_reset () =
          let sens_names = List.map snd edge_idents in
          let mem n = List.mem n sens_names in
          let rec outer = function
            | [] -> None
            | BBlock xs :: _ -> outer xs
            | BIf { condition; _ } :: _ -> Some condition
            | _ :: tl -> outer tl
          in
          (* Strip wrappers Verible emits around 1-bit conditions:
             logical-not lowers via `or_reduce` for 1-bit operands so
             `!rst_n` arrives as `BUnOp(BNot, BCall(or_reduce, [rst_n]))`.
             Reduction-or of a 1-bit value is the value itself. *)
          let rec simplify = function
            | BCall { func = "or_reduce"; args = [x] } -> simplify x
            | BCall { func = "and_reduce"; args = [x] } -> simplify x
            (* For 1-bit operands, reduction-or/and is the identity;
               Verible inserts these around `!x` to widen to a bool
               truth-test, so strip them here. *)
            | BUnOp { op = BRedOr; operand; _ } -> simplify operand
            | BUnOp { op = BRedAnd; operand; _ } -> simplify operand
            | BUnOp { op = BNot; operand; result_type } ->
                BUnOp { op = BNot; operand = simplify operand; result_type }
            | other -> other
          in
          match outer body with
          | None -> `NoReset
          | Some cond ->
              (match simplify cond with
               | BVar n when mem n -> `Reset (n, `Pos)
               | BUnOp { op = BNot; operand = BVar n; _ } when mem n ->
                   `Reset (n, `Neg)
               | BBinOp { op = BEq; lhs = BVar n;
                          rhs = BConst { value = 0; _ }; _ } when mem n ->
                   `Reset (n, `Neg)
               | BBinOp { op = BEq; lhs = BVar n;
                          rhs = BConst { value = 1; _ }; _ } when mem n ->
                   `Reset (n, `Pos)
               | BBinOp { op = BNe; lhs = BVar n;
                          rhs = BConst { value = 0; _ }; _ } when mem n ->
                   `Reset (n, `Pos)
               | BBinOp { op = BNe; lhs = BVar n;
                          rhs = BConst { value = 1; _ }; _ } when mem n ->
                   `Reset (n, `Neg)
               | _ -> `NoReset)
        in
        let pick_clock () =
          (* Default = first edge ident (used when no reset detected). *)
          match edge_idents with
          | (e, n) :: _ -> (n, e)
          | [] -> ("clk", `Pos)
        in
        let clk_name, clk_edge, reset, reset_edge, reset_async =
          let nedges = List.length edge_idents in
          match nedges, classify_reset () with
          | 0, _ ->
              (* No edges parsed (shouldn't happen given has_edge) —
               * preserve old behaviour. *)
              ("clk", `Pos, None, None, false)
          | 1, _ ->
              (* Single edge: that's the clock; sync reset (if any)
               * stays in the body — caller derives sync-reset
               * semantics from `if (rst) ...` shape, no FF-spec
               * change needed. *)
              let (e, n) = List.hd edge_idents in
              (n, e, None, None, false)
          | _, `Reset (rst, rst_pol) ->
              (* Multi-edge: pick the non-reset edge as clock.  Reset
               * polarity comes from the body test, not from the
               * sensitivity edge — `@(... or posedge rst) if (!rst)`
               * is a real (if rare) idiom; the body wins. *)
              let clock = List.find (fun (_, n) -> n <> rst) edge_idents in
              let (cke, cn) = clock in
              let r_edge = try Some (fst (List.find (fun (_, n) -> n = rst) edge_idents))
                           with Not_found -> Some rst_pol in
              let _ = r_edge in
              (cn, cke, Some rst, Some rst_pol, true)
          | _, `NoReset ->
              (* Multi-edge but body has no reset-if — fall back to
               * first edge as clock, no reset.  Conservative. *)
              let (n, e) = pick_clock () in
              (n, e, None, None, false)
        in
        BSequential {
          name = "always_ff";
          clock = clk_name;
          clock_edge = clk_edge;
          reset;
          reset_edge;
          reset_async;
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
          | b :: _ -> [stmt_to_bstmt ~pkgs ~params ~arrays b]
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
let extract_instances ~pkgs ~params tok =
  let arrays = [] in
  (* Use the labeled walker so each instance picks up the
   * `<begin_label>.` prefix of every enclosing labeled generate
   * block. This matches the inst_name format that
   * Verible_elaborate.specialise_design records into
   * inst_specialised, and the format yosys-slang and synthesis
   * tools use as the SV-2017 hierarchical-name segment
   * (e.g. `non_leaf_node.left_child`). Without the prefix,
   * convert_files's lookup `(s.s_name, i.inst_name)` against
   * inst_specialised misses (it has the labeled key but i.inst_name
   * was unlabeled) and the instance gets dropped. *)
  let bases_with_labels = ref [] in
  Verible_elaborate.walk_live_labeled
    (List.filter_map (fun (k, v) ->
       try Some (k, int_of_string v) with _ -> None) params)
    [] (fun label_stack node ->
    if has_tag (prefix_is "instantiation_base") node then
      bases_with_labels := (label_stack, node) :: !bases_with_labels
  ) tok;
  let bases_with_labels = List.rev !bases_with_labels in
  List.concat_map (fun (label_stack, base) ->
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
            (* Use the full expression converter so port connections
             * carrying slices, concats, replications, and constant
             * expressions survive intact through to BIR — required
             * for Behavioral_flatten to substitute correctly. *)
            let be =
              try expr_to_bexpr ~pkgs ~params ~arrays expr
              (* Re-raise Silent_zero so default-strict actually fires;
                 only fall back to a BVar reference for genuine other
                 expr-shape failures. *)
              with Silent_zero_substitution _ as e -> raise e
                 | _ -> BVar (match expr with
                              | TUPLE3 (_, SymbolIdentifier id, _) -> id
                              | SymbolIdentifier id -> id
                              | _ -> "?")
            in
            port_conns := (port, be) :: !port_conns
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
          (* Verible's `instantiation_base` non-terminal is shared
             between real module instantiations and certain unpacked-
             array data declarations (e.g.
                  reg [31:0] mem [0:WORDS-1];
             gets surfaced as if WORDS were the module type and `mem`
             the instance name).  Real instantiations always have a
             paren-bound port list (possibly empty); mis-tagged array
             decls have no port_named children at all and their
             "module name" is a parameter in scope giving the bound.
             Drop the candidate when both signs apply.              *)
          let param_names = List.map fst params in
          if !port_conns = [] && List.mem m param_names then None
          else
            let prefixed_inst = String.concat "." (label_stack @ [i]) in
            Some {
              inst_name = prefixed_inst;
              module_name = m;
              param_values = [];
              port_connections = List.rev !port_conns;
            }
      | _ -> None
    ) inst_nodes
  ) bases_with_labels

(* ─── Module conversion ──────────────────────────────────────────── *)

(* `parameter X = 4;` declared either in the module header
 *   (`module foo #(parameter X = 4) (...)`) or the body
 *   (`parameter X = 4;`). The instance-override map (`params`) wins;
 * defaults declared here fill in for unbound names. Both shapes have a
 * `trailing_assign1` slot whose subtree is the value expression. *)
let extract_body_params ~pkgs ~params tok =
  (* Order matters: SystemVerilog localparams in the body resolve
   * left-to-right against earlier ones, so PaddedWidth = 1 <<
   * $clog2(INPUT_WIDTH) needs INPUT_WIDTH (a port-param default) to
   * already be in scope. We accumulate `params` as we process each
   * node, so the next eval_int call sees prior defaults/localparams. *)
  let port_nodes =
    collect_by (has_tag (prefix_is "module_parameter_port")) tok in
  let local_nodes =
    collect_by (has_tag (prefix_is "any_param_declaration")) tok in
  let one acc n =
    (* Find the parameter NAME — but skip identifiers that live inside
     * the parameter's type/range subtree. For
     * `parameter logic [LfsrWidth-1:0] RstVal = '1`, a naive first-id
     * walk would grab `LfsrWidth` (inside the range) instead of
     * `RstVal`. The actual parameter name lives just before
     * `trailing_assign` — usually in a `param_assignment` or sibling. *)
    let nm = ref None in
    let rec walk_skip_type t =
      match t with
      | TUPLE6 (STRING t', _, _, _, _, _)
        when prefix_is "decl_variable_dimension" t'
          || prefix_is "select_variable_dimension" t' -> ()
      | TUPLE4 (STRING t', _, _, _)
        when prefix_is "decl_variable_dimension" t'
          || prefix_is "select_variable_dimension" t' -> ()
      | TUPLE3 (STRING t', _, _) when prefix_is "instantiation_type" t'
                                    || prefix_is "data_type" t' -> ()
      | TUPLE3 (STRING t', SymbolIdentifier id, _)
        when prefix_is "param_assignment" t' || prefix_is "param_decl" t' ->
          if !nm = None then nm := Some id
      | SymbolIdentifier id when !nm = None -> nm := Some id
      | TUPLE2 (a, b) -> walk_skip_type a; walk_skip_type b
      | TUPLE3 (a, b, c) -> walk_skip_type a; walk_skip_type b; walk_skip_type c
      | TUPLE4 (a, b, c, d) -> List.iter walk_skip_type [a; b; c; d]
      | TUPLE5 (a, b, c, d, e) -> List.iter walk_skip_type [a; b; c; d; e]
      | TUPLE6 (a, b, c, d, e, f) -> List.iter walk_skip_type [a; b; c; d; e; f]
      | TLIST xs -> List.iter walk_skip_type xs
      | _ -> ()
    in
    walk_skip_type n;
    let value_node = ref None in
    walk (function
      | TUPLE4 (STRING t, _, v, _) when prefix_is "trailing_assign" t
                                         && !value_node = None ->
          value_node := Some v
      | _ -> ()) n;
    match !nm, !value_node with
    | Some id, Some v ->
        let cur = List.assoc_opt id acc in
        let new_val =
          match eval_int ~pkgs ~params:acc v with
          | Some i -> Some (string_of_int i)
          | None ->
              (try
                 match expr_to_bexpr ~pkgs ~params:acc ~arrays:[] v with
                 | BConst { value; _ } -> Some (string_of_int value)
                 | BConcat elems ->
                     (* Array-typed localparam (`'{e1, …, eN}` initialiser).
                        Register the elements so LUT[i] lookups in the
                        body can fold to the i-th element or build a
                        BCond mux tree.  Returns None so this param
                        doesn't enter the integer scope (it isn't an
                        int).  Task #139's ROM-promotion path.       *)
                     Hashtbl.replace cur_array_params id elems;
                     None
                 | _ -> None
               with _ -> None)
        in
        (match cur, new_val with
         | None, Some v -> (id, v) :: acc
         | Some old, Some v when v <> old ->
             (* Re-bind: a later fixed-point pass found a better value
                (e.g. ICW=0 from expr_to_bexpr fallback got overridden
                to ICW=7 once H landed in acc and eval_int's $clog2 path
                could fire).  Without this, the partial first-pass
                result stuck and `[ICW-1:0]` widths stayed at 1 bit. *)
             (id, v) :: List.remove_assoc id acc
         | _ -> acc)
    | _ -> acc
  in
  (* Verible's parse tree presents parameter declarations in
     reverse source order, so a derived param like
       parameter mwidth = dwidth + cwidth;
     gets visited before its operands resolve.  Iterate to fixed
     point: after each pass, retry the unresolved ones; stop when
     no new bindings appear (cycle or all done).  Bound at
     [List.length nodes] passes per group to guarantee
     termination on any acyclic dependency graph. *)
  let resolve_until_fixed nodes acc =
    (* Stop only when nothing changes — both length AND values must
       be stable.  Length-only stop misses cases where a binding
       improves from a fallback-0 to a real value (e.g. ICW=0 → ICW=7
       once $clog2's inner H resolves on a later pass). *)
    let rec loop acc passes =
      let acc' = List.fold_left one acc nodes in
      if (List.length acc' = List.length acc
          && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && v1 = v2)
               (List.sort compare acc) (List.sort compare acc'))
         || passes <= 0
      then acc'
      else loop acc' (passes - 1)
    in
    loop acc (List.length nodes) in
  let acc = resolve_until_fixed port_nodes  params in
  let acc = resolve_until_fixed local_nodes acc in
  (* Drop the original `params` we seeded with — caller already has
   * those, we only want the additions. *)
  List.filter (fun (k, _) -> not (List.mem_assoc k params)) acc

(* Auto-discovered list of directories searched by `$readmemh`'s
 * resolver in addition to MEM_INIT_DIR / cwd / cwd/generated.  We
 * walk up from each input .sv file looking for sibling `generated/`
 * directories — the convention used by smollm and other RTL trees
 * that vendor their hex initialiser files alongside the source.
 * Populated by [convert_files_inner] before per-module conversion. *)
let mem_init_search_paths : string list ref = ref []

let convert_module ~pkgs (mdecl : module_decl)
                          (params : (string * string) list) : bmodule =
  (* Reset the per-module array-typed localparam ROM table BEFORE
     extract_body_params populates it.  Otherwise the Hashtbl.clear
     down in the signal-extraction block wipes the registrations,
     and the reference3 walker sees an empty table when processing
     `LUT[sel]` in the always_comb body.                              *)
  Hashtbl.clear cur_array_params;
  (* Merge instance-override params with body-declared defaults; the
   * override wins on conflict (List.assoc_opt finds it first). *)
  let body_params = extract_body_params ~pkgs ~params mdecl.m_body in
  let params =
    let known = List.map fst params in
    params @ List.filter (fun (n, _) -> not (List.mem n known)) body_params
  in
  (* Build int scope from the merged params and prune dead generate
   * branches from the body. Without this, popcount__W2's body would
   * carry assigns from the W==1 / W>=3 branches alongside the W==2
   * branch, with multiple writes to the same signal corrupting
   * downstream Behavioral_flatten. *)
  let int_scope =
    List.filter_map (fun (n, v) ->
      Option.map (fun i -> (n, i)) (Verible_elaborate.Eval.eval_string [] v)
    ) params
  in
  Verible_elaborate.resolver_for_walk :=
    (fun t -> Verible_elaborate.resolve_value pkgs t);
  Verible_elaborate.evaluator_for_walk :=
    Verible_elaborate.Eval.eval_string;
  (* Default-on. The fuzzer (test/random/fuzz_const_fn.sh) shows
   * the prune turns cfg_recursive from 0/25 to 25/25, and the
   * 3 ct_vfdsu_* Z3 PASSes the prune-off path showed were false
   * positives (multi-driver pollution where the un-pruned bmodule
   * happened to be Z3-equivalent to Vivado's elaborated form by
   * coincidence, not by elaboration correctness). Opt-out via
   * DISABLE_GEN_PRUNE for triaging the residual width-mismatch
   * cases. *)
  let mdecl =
    if Sys.getenv_opt "DISABLE_GEN_PRUNE" <> None then mdecl
    else { mdecl with m_body =
      Verible_elaborate.prune_dead_generates int_scope mdecl.m_body }
  in
  (* Capture the pre-strip body so the function extractor below can
     walk function_declaration subtrees.  Strip only affects
     downstream signal/process/instance extraction. *)
  let mdecl_body = mdecl.m_body in
  let mdecl =
    { mdecl with m_body =
      Verible_elaborate.strip_function_decls mdecl.m_body }
  in
  (* Enum item names fold to their integer values via the same
   * params lookup that handles `parameter`. *)
  let enum_items = extract_enum_items ~pkgs ~params mdecl.m_body in
  let params =
    let known = List.map fst params in
    params @ List.filter (fun (n, _) -> not (List.mem n known)) enum_items
  in
  let typedefs = extract_typedefs ~pkgs ~params mdecl.m_body in
  if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
    List.iter (fun (n, w) ->
      Printf.eprintf "[%s] typedef %s width=%d\n" mdecl.m_name n w
    ) typedefs;
  (* Populate the module-scoped struct typedef table for the
   * expression converter. Reset between modules so a typedef in one
   * doesn't leak into another. *)
  cur_struct_defs :=
    extract_struct_defs ~pkgs ~params mdecl.m_body;
  if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then begin
    Printf.eprintf "[%s] struct_defs:\n" mdecl.m_name;
    List.iter (fun (n, fs) ->
      Printf.eprintf "  %s: %s\n" n
        (String.concat ", "
           (List.map (fun (f, w) -> Printf.sprintf "%s(%d)" f w) fs))
    ) !cur_struct_defs
  end;
  (* Build signal_name → struct_typedef_name for every reg/net
   * declared with a struct-typed instantiation_type. *)
  let signal_struct_decls =
    let acc = ref [] in
    let bases = collect_by
      (has_tag (prefix_is "instantiation_base")) mdecl.m_body in
    List.iter (fun base ->
      (* The type name is the first SymbolIdentifier under the
       * `instantiation_type`; collect the var names that follow. *)
      let type_name = ref None in
      walk (function
        | TUPLE3 (STRING t, SymbolIdentifier id, _)
          when prefix_is "unqualified_id" t && !type_name = None ->
            type_name := Some id
        | _ -> ()) base;
      if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
        Printf.eprintf "  base type_name = %s\n"
          (Option.value !type_name ~default:"<none>");
      match !type_name with
      | Some tn when List.mem_assoc tn !cur_struct_defs ->
          (* Pick out variable names — register_variable nodes. *)
          let vars = collect_by (has_tag (fun t ->
            (let l = String.length "register_variable" in
             String.length t >= l && String.sub t 0 l = "register_variable")
            || prefix_is "non_anonymous_gate_instance" t
            || prefix_is "gate_instance_or_register_variable" t)) base in
          if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
            Printf.eprintf "    %d var nodes\n" (List.length vars);
          List.iter (fun v ->
            let nm = ref None in
            walk (function
              | TUPLE3 (STRING t, SymbolIdentifier id, _)
                when prefix_is "unqualified_id" t && !nm = None ->
                  nm := Some id
              | SymbolIdentifier id when !nm = None ->
                  nm := Some id
              | _ -> ()) v;
            if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
              Printf.eprintf "    var=%s\n"
                (Option.value !nm ~default:"<none>");
            match !nm with
            | Some n when n <> tn -> acc := (n, tn) :: !acc
            | _ -> ()
          ) vars
      | _ -> ()
    ) bases;
    !acc
  in
  cur_signal_struct := signal_struct_decls;
  (* Reset per-module width cache; populated below as soon as the
   * port + internal signals are extracted, before any expr_to_bexpr
   * call. Keeps result_type_for from falling back to dummy_bool. *)
  cur_signal_widths := [];
  if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
    List.iter (fun (n, t) ->
      Printf.eprintf "  signal %s : %s\n" n t
    ) !cur_signal_struct;
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
  if Sys.getenv_opt "PORT_DEBUG" <> None then
    Printf.eprintf "[port] %s: port_nodes=%d explicit=%d\n%!"
      mdecl.m_name (List.length port_nodes) (List.length explicit_signals);
  let explicit_names = List.map (fun (s : bsignal) -> s.name)
                         explicit_signals in
  (* Inherited ports (`output [3:0] sum1, sum2;` — sum2 is a bare
   * port_reference1 sibling that inherits BOTH the direction AND the
   * width from sum1's preceding port_declaration_noattr1).
   *
   * Strategy: walk the module_port_list_opt's children in source
   * order, maintaining a "current direction + width" state. When we
   * encounter an explicit port decl, update the state from it. When
   * we encounter a port_reference1 not covered by an explicit decl,
   * emit a signal using the current state. *)
  let port_list_node = collect_by
    (has_tag (prefix_is "module_port_list_opt")) mdecl.m_body in
  let inherited_signals =
    (* Verible visits port-related nodes in REVERSE of source order
     * (the explicit decl comes after its bare-reference siblings in
     * DFS). So: collect every port-related event in DFS order, then
     * iterate the list in REVERSE to apply direction+width
     * inheritance the way the source intended. *)
    let cur_dir = ref `Input in
    let cur_width = ref 1 in
    let acc = ref [] in
    let events = ref [] in (* (dir_opt, width_opt, name_opt) per port1 *)
    (* Iterate each `port1` in the module_port_list_opt in order. Each
     * `port1` is one comma-separated port — it either has its own
     * direction+width (the first port in `output [3:0] a, b`) OR is a
     * bare name that inherits from the previous port1 (b, in the same
     * example). *)
    (* Per-event collector — extract direction/width/name from a
     * port-related node. The name walker must skip identifier-bearing
     * dimension subtrees so `[$clog2(N)-1:0]` doesn't surface `$clog2`
     * or `N` as a port name. *)
    let process_port1 p1 =
      let dir = ref None in
      walk (function
        | Input -> dir := Some `Input
        | Output -> dir := Some `Output
        | Inout -> dir := Some `Internal
        | _ -> ()) p1;
      let explicit_w = extract_range ~pkgs ~params p1 in
      let w_opt =
        match explicit_w with
        | Some _ -> Some (width_of ~pkgs ~params p1)
        | None ->
            (* No packed dimension — but if this port has an explicit
             * direction token, it's the start of a fresh comma group
             * (e.g. `output logic a, b` after `input [3:0] d`) and
             * the width defaults to 1, NOT inherited from the
             * previous group. *)
            (match !dir with
             | Some _ -> Some 1
             | None -> None)
      in
      let n = ref None in
      let rec walk_skip t =
        match t with
        | TUPLE6 (STRING t', _, _, _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE4 (STRING t', _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE3 (STRING t', SymbolIdentifier id, _)
          when prefix_is "unqualified_id" t' && !n = None ->
            n := Some id
        | TUPLE2 (a, b) -> walk_skip a; walk_skip b
        | TUPLE3 (a, b, c) -> walk_skip a; walk_skip b; walk_skip c
        | TUPLE4 (a, b, c, d) ->
            walk_skip a; walk_skip b; walk_skip c; walk_skip d
        | TUPLE5 (a, b, c, d, e) ->
            List.iter walk_skip [a; b; c; d; e]
        | TUPLE6 (a, b, c, d, e, f) ->
            List.iter walk_skip [a; b; c; d; e; f]
        | TUPLE7 (a, b, c, d, e, f, g) ->
            List.iter walk_skip [a; b; c; d; e; f; g]
        | TUPLE8 (a, b, c, d, e, f, g, h) ->
            List.iter walk_skip [a; b; c; d; e; f; g; h]
        | TLIST xs -> List.iter walk_skip xs
        | _ -> ()
      in
      walk_skip p1;
      events := (!dir, w_opt, !n) :: !events
    in
    (* Use the global walk (which handles all TUPLE arities up to
     * TUPLE15) to visit every node in depth-first pre-order. The
     * port1 / explicit-decl handler updates inheritance state and
     * emits names. *)
    let visit t =
      let is_port_tag tag =
        prefix_is "port1" tag
        || prefix_is "module_port_declaration" tag
        || prefix_is "port_declaration_noattr" tag
      in
      match t with
      | TUPLE2 (STRING tag, _) when is_port_tag tag -> process_port1 t
      | TUPLE3 (STRING tag, _, _) when is_port_tag tag -> process_port1 t
      | TUPLE4 (STRING tag, _, _, _) when is_port_tag tag -> process_port1 t
      | TUPLE5 (STRING tag, _, _, _, _) when is_port_tag tag -> process_port1 t
      | _ -> ()
    in
    (* Walk the whole module body — explicit port decls and the
     * module_port_list_opt's port1 entries are siblings in Verible's
     * AST, so we need a global in-order walk to keep direction+width
     * inheritance correct. *)
    walk visit mdecl.m_body;
    let _ = port_list_node in
    (* Verible's DFS order is the REVERSE of source order for sibling
     * port nodes (left-recursive grammar builds TLISTs back-to-front).
     * Walk the events list as-is (it was prepended) which gives
     * source order, applying inheritance as we go. *)
    let source_order = !events in
    List.iter (fun (dir_opt, w_opt, n_opt) ->
      (match dir_opt with Some d when d <> `Internal -> cur_dir := d | _ -> ());
      (match w_opt with Some w -> cur_width := w | None -> ());
      match n_opt with
      | Some name when not (List.mem name explicit_names) ->
          acc := {
            name;
            stype = BInt { width = !cur_width; signed = Unsigned };
            direction = !cur_dir;
            initial_value = None; attrs = []; 
          } :: !acc
      | _ -> ()
    ) source_order;
    (* Dedup by name, preserving first occurrence. *)
    let rev = List.rev !acc in
    List.fold_left (fun (seen, out) (s : bsignal) ->
      if List.mem s.name seen then (seen, out)
      else (s.name :: seen, s :: out)
    ) ([], []) rev
    |> snd |> List.rev
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
      let packed_dims = extract_packed_dims ~pkgs ~params inst_type in
      List.filter_map (fun n ->
        let nm = ref None in
        walk (function
          | SymbolIdentifier id when !nm = None -> nm := Some id
          | _ -> ()) n;
        (* Unpacked array: `reg [W-1:0] mem [0:N]` carries a
         * `decl_variable_dimension1` *inside the var node* (slot 2 of
         * `…_register_variable1`). When present, treat as BArray
         * with depth = |msb − lsb| + 1.
         *
         * 2-D packed array (`logic [A-1:0][B-1:0] x`): outer dim is
         * the array index, inner dim is the per-element width. lzc's
         * `index_lut[WIDTH-1:0][NumLevels-1:0]` and `index_nodes` are
         * exactly this shape — without recognising it as BArray, the
         * per-element writes `index_lut[k] = …` collapse into multi-
         * driver writes on a flat 1-D wire and the output is wrong. *)
        let unpacked = extract_range ~pkgs ~params n in
        let stype = match unpacked, packed_dims with
          | Some (m, l), _ ->
              let elem_w = match packed_dims with
                | (mi, li) :: _ -> abs (mi - li) + 1
                | [] -> 1
              in
              BArray {
                element = BInt { width = elem_w; signed = Unsigned };
                size = abs (m - l) + 1;
              }
          | None, [(om, ol); (im, il)] ->
              BArray {
                element = BInt { width = abs (im - il) + 1; signed = Unsigned };
                size = abs (om - ol) + 1;
              }
          | None, _ ->
              let elem_w = width_of ~typedefs ~pkgs ~params inst_type in
              BInt { width = elem_w; signed = Unsigned }
        in
        match !nm with
        | Some id -> Some {
            name = id;
            stype;
            direction = `Internal;
            initial_value = None; attrs = [];
          }
        | None -> None
      ) var_nodes
  ) instbase_nodes in
  (* Names of memory-array signals — used by the assign/always
   * handlers below to switch indexed lhs to `@mem_write` and indexed
   * rhs to BSelect. *)
  let array_names =
    List.filter_map (fun (s : bsignal) ->
      match s.stype with
      | BArray _ -> Some s.name
      | _ -> None
    ) reg_var_signals
  in
  let internal_signals =
    (List.concat_map (extract_port_decl ~pkgs ~params) net_nodes
     |> List.map (fun s -> { s with direction = `Internal }))
    @ reg_var_signals
  in
  (* Merge by name. When the same signal appears twice (bare name from
   * the port-list AND an explicit K&R `output [W:0] X;` decl), the
   * second entry usually carries better information — explicit
   * direction (Output rather than the default Internal) and explicit
   * width. Old code did first-wins dedup which kept the bare-name
   * Internal/width=1 placeholder, so cva6's K&R-style ct_vfdsu_*
   * modules ended up with all ports classed as Input/1-bit. New rule:
   * later wins for direction (Output > Input > Internal) and for
   * width (>1 > 1).  *)
  let signals : bsignal list =
    let dir_rank = function
      | `Output -> 2 | `Input -> 1 | `Internal -> 0 in
    let bsignal_width (s : bsignal) =
      match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 0 in
    let merge a b : bsignal =
      let pick = if dir_rank b.direction > dir_rank a.direction then b
                 else if dir_rank b.direction < dir_rank a.direction then a
                 else if bsignal_width b > bsignal_width a then b
                 else a in
      pick
    in
    let by_name : (string, bsignal) Hashtbl.t = Hashtbl.create 16 in
    let order = ref [] in
    List.iter (fun (s : bsignal) ->
      match Hashtbl.find_opt by_name s.name with
      | None -> Hashtbl.replace by_name s.name s; order := s.name :: !order
      | Some prev -> Hashtbl.replace by_name s.name (merge prev s)
    ) (port_signals @ internal_signals);
    List.rev_map (Hashtbl.find by_name) !order
  in
  (* Populate per-module width cache before any expr_to_bexpr call —
   * lets `result_type_for` set real widths on BBinOp/BUnOp instead
   * of falling back to dummy_bool. *)
  cur_signal_widths := List.filter_map (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 0 in
    if w > 0 then Some (s.name, w) else None
  ) signals;
  (* Pre-scan: any continuous assign with an indexed LHS gets its
     target promoted into array_names — even if the underlying
     signal is a plain BInt rather than a BArray.  This lets the
     extract_assign path emit @mem_write for cases like
     `assign out[i] = X` (cell-mapped Verilog from our own synth),
     so the downstream merge_array_writes pass can consolidate the
     N per-bit drives into a single full-bus concat assign — which
     is what both the Z3 miter (slice indices preserved) and
     OpenROAD's [read_verilog] (bit-blasted assigns visible in
     source) need to see. *)
  let cont_assigns =
    collect_by (has_tag (prefix_is "continuous_assign")) mdecl.m_body in
  let scan_indexed_lhs nodes tag_prefix lhs_pos =
    List.concat_map (fun n ->
      let found = collect_by (has_tag (prefix_is tag_prefix)) n in
      let _ = tag_prefix in
      List.filter_map (fun a ->
        let lhs = match a, lhs_pos with
          | TUPLE4 (_, l, _, _), 1 -> Some l
          | TUPLE6 (_, l, _, _, _, _), 1 -> Some l
          | _ -> None in
        match lhs with
        | Some lhs ->
            (match lhs_indexed_of lhs with
             | Some (name, Some _) -> Some name
             | _ -> None)
        | None -> None) found
    ) nodes in
  (* Pre-scan: collect every name that appears with an indexed LHS,
     across continuous assigns (`assign x[i] = ...`) AND procedural
     assigns (`x[i] <= ...` inside always_ff / always_comb).  Promote
     each into array_names so the assign/always handlers route them
     through the @mem_write path → merge_array_writes consolidates
     the per-slice drives into a single full-bus assign that both
     Verible's downstream and OpenROAD's read_verilog handle. *)
  let indexed_targets =
    let cont = scan_indexed_lhs cont_assigns "cont_assign" 1 in
    let blk = scan_indexed_lhs [mdecl.m_body] "assignment_statement_no_expr" 1 in
    let nb  = scan_indexed_lhs [mdecl.m_body] "nonblocking_assignment" 1 in
    List.sort_uniq compare (cont @ blk @ nb) in
  let array_names =
    List.sort_uniq compare (array_names @ indexed_targets) in
  let assign_procs = List.concat_map (fun ca ->
    extract_assign ~pkgs ~params ~arrays:array_names ca
  ) cont_assigns in
  (* Wire-with-initialiser shape: `wire [W:0] foo = expr;` parses
     as a [net_declaration*] containing a [net_decl_assign1] subnode
     that carries the RHS expression.  Treat each one as an extra
     continuous assign — without this the wire is declared but never
     driven, and Hardcaml rejects the parent module with "circuit
     input signal must have a port name (unassigned wire?)".  Common
     in the SmolLM rtl (swiglu, matvec_int8_engine) and elsewhere. *)
  let net_decl_assigns =
    let nodes = collect_by (has_tag
      (fun t -> prefix_is "net_decl_assign1" t)) mdecl.m_body in
    List.filter_map (function
      | TUPLE4 (STRING tag, SymbolIdentifier name, _eq, rhs)
        when prefix_is "net_decl_assign" tag ->
          let rhs_e =
            try Some (expr_to_bexpr ~pkgs ~params
                        ~arrays:array_names rhs)
            (* Let Silent_zero_substitution propagate — default-strict
               should hit the top, not be silently dropped here. *)
            with Silent_zero_substitution _ as e -> raise e
               | _ -> None in
          (match rhs_e with
           | None -> None
           | Some rhs_e ->
               Some (BCombinational {
                 name = "net_assign_" ^ name;
                 sensitivity = [BAny];
                 body = [BAssign { lhs = name; rhs = rhs_e }];
               }))
      | _ -> None) nodes in
  let assign_procs = assign_procs @ net_decl_assigns in
  (* `initial $readmemh("file.hex", mem);` ROM initialiser (#132).
     Hardcaml has no concept of an initial block, so a memory whose
     content comes from $readmemh has no driver at the IR level and
     synthesis aborts ("circuit input signal must have a port name
     (unassigned wire?)").  Translate the call into a constant
     driver: read the hex file at convert time, build a BConcat of
     element-wide BConsts (MSB-first), and emit a BCombinational
     that drives the target memory with that BConcat.

     Path resolution: try the path verbatim, then relative to the
     directory of every input .sv (the converter doesn't see the
     file path here, so we use $MEM_INIT_DIR if set, plus PWD).
     Errors are non-fatal — we log to stderr and skip; the synth
     will then surface the unassigned-wire error and the user can
     point MEM_INIT_DIR at the correct directory. *)
  let mem_init_procs =
    let mem_shape name =
      List.find_map (fun (s : bsignal) ->
        if s.name = name then
          match s.stype with
          | BArray { size; element = BInt { width; _ } } -> Some (size, width)
          | _ -> None
        else None) reg_var_signals in
    let resolve path =
      let basename = Filename.basename path in
      let candidates =
        let env_dir = match Sys.getenv_opt "MEM_INIT_DIR" with
          | Some d -> [Filename.concat d basename]
          | None -> [] in
        let auto_dirs =
          List.map (fun d -> Filename.concat d basename)
            !mem_init_search_paths in
        path :: env_dir @ auto_dirs
        @ (List.map (fun d -> Filename.concat d basename)
             [Sys.getcwd ();
              Filename.concat (Sys.getcwd ()) "generated";
              Filename.concat (Sys.getcwd ()) "../generated"]) in
      List.find_opt Sys.file_exists candidates in
    let read_hex elem_w n_words path =
      try
        let ic = open_in path in
        let words = ref [] in
        let count = ref 0 in
        (try
          while !count < n_words do
            let line = input_line ic in
            (* Trim and strip Verilog `//` comments. *)
            let trim_at s c =
              try String.sub s 0 (String.index s c)
              with Not_found -> s in
            let line = trim_at line '/' in
            let line = String.trim line in
            if line <> "" then begin
              let v = int_of_string ("0x" ^ line) in
              let mask =
                if elem_w >= 63 then -1
                else (1 lsl elem_w) - 1 in
              words := (v land mask) :: !words;
              incr count
            end
          done
        with End_of_file -> ());
        close_in ic;
        (* Pad with zero if file shorter than memory. *)
        while !count < n_words do
          words := 0 :: !words; incr count
        done;
        Some (List.rev !words)
      with _ -> None in
    let initial_nodes =
      collect_by (has_tag (prefix_is "initial_construct")) mdecl.m_body in
    List.filter_map (fun init ->
      let saw_readmemh = ref false in
      let str = ref None in
      let target = ref None in
      walk (function
        | SymbolIdentifier id when id = "$readmemh" -> saw_readmemh := true
        | TK_StringLiteral s when !str = None -> str := Some s
        | SymbolIdentifier id
          when !saw_readmemh && !str <> None && id <> "$readmemh"
               && !target = None -> target := Some id
        | _ -> ()) init;
      match !saw_readmemh, !str, !target with
      | true, Some path, Some tgt ->
          let path =
            (* Strip surrounding quotes if Verible kept them. *)
            let n = String.length path in
            if n >= 2 && path.[0] = '"' && path.[n - 1] = '"'
            then String.sub path 1 (n - 2) else path in
          (match mem_shape tgt with
           | None ->
               Printf.eprintf
                 "[verible_to_bir] WARNING: $readmemh target %s is not a \
                  recognised BArray; skipping ROM init from %s\n%!" tgt path;
               None
           | Some (size, elem_w) ->
               (match resolve path with
                | None ->
                    Printf.eprintf
                      "[verible_to_bir] WARNING: $readmemh hex file not \
                       found: %s (set MEM_INIT_DIR or run from the \
                       directory containing %s)\n%!"
                      path (Filename.basename path);
                    None
                | Some p ->
                    (match read_hex elem_w size p with
                     | None ->
                         Printf.eprintf
                           "[verible_to_bir] WARNING: failed to read %s\n%!" p;
                         None
                     | Some words ->
                         (* MSB-first BConcat: words[size-1] is the high
                            slot, words[0] is the low.  Match the layout
                            used by the @mem_write lowering in
                            behavioral_to_hardcaml so reads via BSelect
                            land on the same slot the file declared. *)
                         let parts = List.rev_map (fun v ->
                           BConst { value = v; width = elem_w }) words in
                         Some (BCombinational {
                           name = "mem_init_" ^ tgt;
                           sensitivity = [BAny];
                           body = [BAssign {
                             lhs = tgt;
                             rhs = BConcat parts;
                           }];
                         })))
           )
      | _ -> None) initial_nodes
  in
  let assign_procs = assign_procs @ mem_init_procs in
  let always_procs = extract_always ~pkgs ~params ~arrays:array_names mdecl.m_body in
  let instances = extract_instances ~pkgs ~params mdecl.m_body in
  (* Post-pass: merge combinational @mem_write groups targeting the
   * same array into a single full-array concat assignment. lzc's
   * `for (genvar j…) assign index_lut[j] = …` unrolls to N
   * BCombinational @mem_write nodes; downstream Z3 / meminfer
   * doesn't track combinational @mem_write side-effects (meminfer's
   * find_ram_writes only walks BSequential), so each @mem_write
   * is a no-op and the array stays free. Fold them here while we
   * still have ergonomic per-call info: collect all combinational
   * @mem_write to a given array, sort by literal index ascending
   * (LSB-first), build a BConcat in MSB-first order, and emit a
   * single BAssign. *)
  let merge_array_writes processes =
    let mem_writes : (string, (int * bexpr) list) Hashtbl.t =
      Hashtbl.create 8 in
    (* Constant-fold a small set of arithmetic shapes so addresses
     * like `((32'1 - 32'1) + 32'0)` (the lzc generate-for index
     * expression after genvar-substitution) reduce to BConst, which
     * lets the merge match on a literal index. *)
    let rec fold = function
      | BBinOp { op = BAdd;
                 lhs = BConst { value = a; width = w };
                 rhs = BConst { value = b; _ }; _ } ->
          BConst { value = a + b; width = w }
      | BBinOp { op = BSub;
                 lhs = BConst { value = a; width = w };
                 rhs = BConst { value = b; _ }; _ } ->
          BConst { value = a - b; width = w }
      | BBinOp { op = BMul;
                 lhs = BConst { value = a; width = w };
                 rhs = BConst { value = b; _ }; _ } ->
          BConst { value = a * b; width = w }
      | BBinOp r ->
          let lhs' = fold r.lhs and rhs' = fold r.rhs in
          (match lhs', rhs' with
           | BConst { value = a; width = w }, BConst { value = b; _ } ->
               (match r.op with
                | BAdd -> BConst { value = a + b; width = w }
                | BSub -> BConst { value = a - b; width = w }
                | BMul -> BConst { value = a * b; width = w }
                | _ -> BBinOp { r with lhs = lhs'; rhs = rhs' })
           | _ -> BBinOp { r with lhs = lhs'; rhs = rhs' })
      | e -> e
    in
    let other_procs = List.filter_map (fun p ->
      match p with
      | BCombinational { body = [BCallStmt {
          func = "@mem_write";
          args = [BVar arr; addr; data] }]; _ }
        when List.mem arr array_names ->
          (match fold addr with
           | BConst { value = idx; _ } ->
               let bucket =
                 try Hashtbl.find mem_writes arr with Not_found -> [] in
               Hashtbl.replace mem_writes arr ((idx, data) :: bucket);
               None
           | _ -> Some p)
      | _ -> Some p
    ) processes in
    let merged = Hashtbl.fold (fun arr writes acc ->
      let arr_size, elem_w, is_scalar =
        match List.find_opt (fun (s : bsignal) -> s.name = arr) signals with
        | Some { stype = BArray { size; element = BInt { width; _ } }; _ } ->
            (size, width, false)
        | Some { stype = BInt { width; _ }; _ } ->
            (* Scalar bit-vector — `assign foo[k] = X` writes a single
               bit, not an array element.  Each "index" corresponds to
               one bit of width 1, so the concat reassembles bit-wise. *)
            (width, 1, true)
        | _ -> (List.length writes, 1, false)
      in
      let truncate_to_elem e =
        if elem_w >= 32 then e
        else BSlice { signal = e; msb = elem_w - 1; lsb = 0 }
      in
      (* Sort msb-first so the concat reads top-to-bottom. *)
      let sorted = List.sort (fun (a, _) (b, _) -> compare b a) writes in
      (* Filler for uncovered slots.  For true unpacked arrays we keep
         the self-read so existing behaviour around inferred RAMs
         doesn't change.  For scalar bit-vectors a self-read closes a
         combinational loop (cfgreg_do[k] driven by cfgreg_do[k]) the
         moment any bit isn't covered by an explicit `assign cfgreg_do[i]
         = …` site — which happens whenever multi-bit slice assigns
         like `assign cfgreg_do[30:23] = 0` aren't decomposed to per-bit
         writes by the upstream collector.  Fall back to constant zero
         instead, which is the synthesizable default for an undriven
         output wire and unblocks Hardcaml's Always.compile.            *)
      let filler_at _k =
        if is_scalar then BConst { value = 0; width = elem_w }
        else BSelect {
          array = BVar arr;
          index = BConst { value = _k; width = 32 };
        }
      in
      let parts = ref [] in
      let cursor = ref (arr_size - 1) in
      List.iter (fun (idx, data) ->
        if idx < !cursor then
          for k = !cursor downto idx + 1 do
            parts := filler_at k :: !parts
          done;
        parts := truncate_to_elem data :: !parts;
        cursor := idx - 1
      ) sorted;
      if !cursor >= 0 then
        for k = !cursor downto 0 do
          parts := filler_at k :: !parts
        done;
      let rhs = match List.rev !parts with
        | [single] -> single
        | many -> BConcat many
      in
      BCombinational {
        name = "merged_array_" ^ arr;
        sensitivity = [BAny];
        body = [BAssign { lhs = arr; rhs }];
      } :: acc
    ) mem_writes [] in
    other_procs @ merged
  in
  (* Convert @mem_write inside BSequential bodies to a full-bus
     BAssign with read-modify-write semantics — same shape as the
     downstream behavioral_to_hardcaml lowering, lifted to BIR
     level so Behavioral_ffrip recognises BArray FFs as ordinary
     register writes.  Without this, source's `reg [W:0] arr[D:0]`
     with `always_ff @(posedge clk) arr[k] <= data` produces a
     BSequential with [BCallStmt @mem_write(arr, k, data)] — ffrip
     ignores BCallStmts, so arr never appears in the rip set, and
     the parent miter's interface check fails because the cell-
     mapped side (which uses BAssign throughout) does have arr. *)
  let lower_mem_writes_in_seq processes =
    List.map (fun p ->
      match p with
      | BSequential ({ body; _ } as s) ->
          (* Collect every @mem_write per target array, building an
             ordered list of (idx, data) pairs.  After merge_seq_processes,
             all writes from a clock domain live in one body, so we can
             build ONE full-bus BAssign per array — folding multiple
             writes via BCond chains so that non-blocking semantics
             survive ffrip's last-write-wins behaviour on BAssign. *)
          let writes : (string, (bexpr * bexpr) list) Hashtbl.t =
            Hashtbl.create 4 in
          List.iter (fun stmt ->
            match stmt with
            | BCallStmt { func = "@mem_write";
                          args = [BVar arr; idx; data] } ->
                let cur = try Hashtbl.find writes arr with Not_found -> [] in
                Hashtbl.replace writes arr (cur @ [(idx, data)])
            | _ -> ()) body;
          let emitted = Hashtbl.create 4 in
          let body' = List.filter_map (fun stmt ->
            match stmt with
            | BCallStmt { func = "@mem_write"; args = [BVar arr; _; _] } ->
                if Hashtbl.mem emitted arr then None
                else begin
                  Hashtbl.add emitted arr ();
                  match List.find_opt (fun (sg : bsignal) -> sg.name = arr)
                          signals with
                  | Some { stype = BArray { size; element = BInt { width=_; _ } }; _ } ->
                      let pairs = Hashtbl.find writes arr in
                      let parts = List.init size (fun k ->
                        let k_const = BConst { value = k; width = 32 } in
                        let init = BSelect { array = BVar arr;
                                             index = k_const } in
                        List.fold_left (fun acc (idx, data) ->
                          BCond {
                            condition = BBinOp {
                              op = BEq;
                              lhs = idx;
                              rhs = k_const;
                              result_type = BInt { width = 1; signed = Unsigned };
                            };
                            then_val = data;
                            else_val = acc;
                          }
                        ) init pairs) in
                      let rhs = BConcat (List.rev parts) in
                      Some (BAssign { lhs = arr; rhs })
                  | _ -> Some stmt
                end
            | other -> Some other
          ) body in
          BSequential { s with body = body' }
      | other -> other
    ) processes
  in
  (* Merge BSequentials with the same clock-domain key, so that 16
     separate `always @(posedge clk) text_out[hi:lo] <= byte` blocks
     coalesce into one BSequential whose body has 16 @slice_writes —
     then [lower_slice_writes_in_seq] can fuse them into a single
     [BAssign { lhs=text_out; rhs=BConcat […] }].  Without this, ffrip
     processes each slice's BSequential separately and the last one
     wins, leaving 15/16 slices undriven. *)
  let merge_seq_processes processes =
    let domain_key clock clock_edge reset reset_edge reset_async =
      Printf.sprintf "%s|%s|%s|%s|%b"
        clock
        (match clock_edge with `Pos -> "p" | `Neg -> "n")
        (Option.value reset ~default:"")
        (match reset_edge with
         | None -> "" | Some `Pos -> "rp" | Some `Neg -> "rn")
        reset_async
    in
    let groups = Hashtbl.create 4 in
    let order = ref [] in
    let other_procs = List.filter (fun p -> match p with
      | BSequential { name; clock; clock_edge; reset; reset_edge;
                      reset_async; body } ->
          let key = domain_key clock clock_edge reset reset_edge reset_async in
          (match Hashtbl.find_opt groups key with
           | None ->
               Hashtbl.add groups key
                 (name, clock, clock_edge, reset, reset_edge,
                  reset_async, ref body);
               order := key :: !order
           | Some (_, _, _, _, _, _, body_ref) ->
               body_ref := !body_ref @ body);
          false
      | _ -> true
    ) processes in
    let merged_seqs =
      List.rev_map (fun key ->
        let (name, clock, clock_edge, reset, reset_edge, reset_async,
             body_ref) = Hashtbl.find groups key in
        BSequential { name; clock; clock_edge; reset; reset_edge;
                      reset_async; body = !body_ref }
      ) !order
    in
    other_procs @ merged_seqs
  in
  (* Lower @slice_write calls inside BSequential bodies to a single
     full-bus BAssign per target lvalue.  After [merge_seq_processes]
     a target's slice writes all live in the same body, so we can
     build a [BConcat] high-to-low covering the whole bus. *)
  let lower_slice_writes_in_seq processes =
    List.map (fun p ->
      match p with
      | BSequential ({ body; _ } as s) ->
          (* Collect @slice_writes per target. *)
          let slices : (string, (int * int * bexpr) list) Hashtbl.t =
            Hashtbl.create 4 in
          let const_of = function
            | BConst { value; _ } -> Some value
            | _ -> None in
          List.iter (fun stmt ->
            match stmt with
            | BCallStmt { func = "@slice_write";
                          args = [BVar lhs; m; l; data] } ->
                (match const_of m, const_of l with
                 | Some msb, Some lsb ->
                     let cur = try Hashtbl.find slices lhs with Not_found -> [] in
                     Hashtbl.replace slices lhs ((msb, lsb, data) :: cur)
                 | _ -> ())
            | _ -> ()
          ) body;
          let body' = List.filter_map (fun stmt ->
            match stmt with
            | BCallStmt { func = "@slice_write";
                          args = [BVar lhs; _; _; _] }
              when Hashtbl.mem slices lhs ->
                if Hashtbl.find slices lhs = [] then None
                else begin
                  let writes = Hashtbl.find slices lhs in
                  Hashtbl.replace slices lhs [];
                  let total_w =
                    match List.find_opt
                            (fun (sg : bsignal) -> sg.name = lhs) signals with
                    | Some s -> width_of_type s.stype
                    | None -> 1 in
                  let sorted =
                    List.sort (fun (a, _, _) (b, _, _) -> compare b a) writes in
                  let parts = ref [] in
                  let cursor = ref (total_w - 1) in
                  List.iter (fun (msb, lsb, data) ->
                    let hi = max msb lsb and lo = min msb lsb in
                    if hi < !cursor then begin
                      let gap_msb = !cursor in
                      let gap_lsb = hi + 1 in
                      parts := BSlice { signal = BVar lhs;
                                        msb = gap_msb; lsb = gap_lsb }
                               :: !parts
                    end;
                    parts := data :: !parts;
                    cursor := lo - 1
                  ) sorted;
                  if !cursor >= 0 then
                    parts := BSlice { signal = BVar lhs;
                                      msb = !cursor; lsb = 0 } :: !parts;
                  let rhs = match List.rev !parts with
                    | [single] -> single
                    | many -> BConcat many in
                  Some (BAssign { lhs; rhs })
                end
            | other -> Some other
          ) body in
          BSequential { s with body = body' }
      | other -> other
    ) processes
  in
  (* Lower @part_sel_write_up calls inside BSequential bodies to a
     full-bus BAssign per target.  `name[base +: width] <= rhs` (with
     constant width) becomes
        name := { … slot[N-1], …, slot[1], slot[0] }
     where slot[k] = (base == k * width) ? rhs : name[k*w +: w].
     N = total_width / width.  This mirrors the @mem_write lowering
     so [behavioral_to_hardcaml]'s downstream BCase / Always handling
     sees a single full-bus driver (not a partial slice) for the FF
     pre-pass and downstream encoders. *)
  let lower_part_sel_writes_in_seq processes =
    List.map (fun p ->
      match p with
      | BSequential ({ body; _ } as s) ->
          let body' = List.map (fun stmt ->
            match stmt with
            | BCallStmt { func = "@part_sel_write_up";
                          args = [BVar lhs; base; width; data] } ->
                let const_of = function
                  | BConst { value; _ } -> Some value
                  | _ -> None in
                (match const_of width with
                 | Some w when w > 0 ->
                     let total_w =
                       match List.find_opt
                               (fun (sg : bsignal) -> sg.name = lhs) signals with
                       | Some s -> width_of_type s.stype
                       | None -> 0 in
                     if total_w = 0 || total_w mod w <> 0 then stmt
                     else
                       let n = total_w / w in
                       let parts = List.init n (fun k ->
                         let k_const = BConst { value = k * w; width = 32 } in
                         let cur_slot = BSlice {
                           signal = BVar lhs;
                           msb = (k + 1) * w - 1;
                           lsb = k * w;
                         } in
                         BCond {
                           condition = BBinOp {
                             op = BEq;
                             lhs = base;
                             rhs = k_const;
                             result_type = BInt { width = 1; signed = Unsigned };
                           };
                           then_val = data;
                           else_val = cur_slot;
                         }) in
                       BAssign { lhs;
                                 rhs = BConcat (List.rev parts) }
                 | _ -> stmt)
            | other -> other
          ) body in
          BSequential { s with body = body' }
      | other -> other
    ) processes in
  let processes =
    let merged = merge_array_writes (assign_procs @ always_procs) in
    if Sys.getenv_opt "DISABLE_MEM_WRITE_LOWER" <> None then merged
    else
      merged
      |> merge_seq_processes
      |> lower_mem_writes_in_seq
      |> lower_slice_writes_in_seq
      |> lower_part_sel_writes_in_seq
  in
  (* LHS-context width propagation (#128).
     SystemVerilog evaluates `r = a OP b` (and other arithmetic)
     at width max(width(r), width(a), width(b)).  result_type_for
     above only sees max(operand widths) and has no LHS context,
     so an arithmetic op assigned to a wider target ends up
     truncated mid-expression and the consumer (z3_miter,
     hardcaml lowering, lib_map) compensates per-op or doesn't.
     Walk each BAssign here, look up its LHS width, and propagate
     that down through arithmetic operators in the rhs.  Per the
     LRM, comparisons / reductions / concat / slice / select are
     "self-determined" — context width stops there. *)
  let processes =
    let lhs_width name =
      match List.find_opt (fun (s : bsignal) -> s.name = name) signals with
      | Some s -> width_of_type s.stype
      | None -> 0 in
    let rec propagate ctx_w e =
      match e with
      | BVar _ | BConst _ | BSelect _ | BSlice _
      | BConcat _ | BReplicate _ | BCall _ -> e
      | BBinOp { op; lhs; rhs; result_type } ->
          let comparison = match op with
            | BEq | BNe | BLt | BLe | BGt | BGe -> true
            | _ -> false in
          if comparison then
            (* Self-determined: don't propagate. *)
            BBinOp { op; lhs = propagate 0 lhs;
                            rhs = propagate 0 rhs;
                            result_type }
          else
            let cur = match result_type with
              | BInt { width; _ } -> width | _ -> 0 in
            let target = max ctx_w cur in
            let result_type' =
              if target > cur
              then BInt { width = target; signed = Unsigned }
              else result_type in
            BBinOp { op;
                     lhs = propagate target lhs;
                     rhs = propagate target rhs;
                     result_type = result_type' }
      | BUnOp { op; operand; result_type } ->
          let reduction = match op with
            | BRedAnd | BRedOr | BRedXor -> true
            | _ -> false in
          if reduction then
            BUnOp { op; operand = propagate 0 operand; result_type }
          else
            let cur = match result_type with
              | BInt { width; _ } -> width | _ -> 0 in
            let target = max ctx_w cur in
            let result_type' =
              if target > cur
              then BInt { width = target; signed = Unsigned }
              else result_type in
            BUnOp { op; operand = propagate target operand;
                    result_type = result_type' }
      | BCond { condition; then_val; else_val } ->
          BCond {
            condition = propagate 0 condition;
            then_val  = propagate ctx_w then_val;
            else_val  = propagate ctx_w else_val;
          }
    in
    let rec walk_stmt = function
      | BAssign { lhs; rhs } ->
          BAssign { lhs; rhs = propagate (lhs_width lhs) rhs }
      | BIf { condition; then_stmts; else_stmts } ->
          BIf { condition = propagate 0 condition;
                then_stmts = List.map walk_stmt then_stmts;
                else_stmts = List.map walk_stmt else_stmts }
      | BCase { selector; cases; default } ->
          BCase { selector = propagate 0 selector;
                  cases = List.map (fun (k, ss) ->
                    (k, List.map walk_stmt ss)) cases;
                  default = List.map walk_stmt default }
      | BBlock ss -> BBlock (List.map walk_stmt ss)
      | BWhile { condition; body } ->
          BWhile { condition = propagate 0 condition;
                   body = List.map walk_stmt body }
      | BFor { init; condition; update; body } ->
          BFor { init = walk_stmt init;
                 condition = propagate 0 condition;
                 update = walk_stmt update;
                 body = List.map walk_stmt body }
      | other -> other
    in
    List.map (fun p ->
      match p with
      | BCombinational c ->
          BCombinational { c with body = List.map walk_stmt c.body }
      | BSequential s ->
          BSequential { s with body = List.map walk_stmt s.body }
    ) processes
  in
  let funcs =
    (* Extract every function_declaration before the strip pass
       erased its body — using the ORIGINAL mdecl_body we captured.
       Each function becomes a [bfunc] consumed by behavioral_inline.

       Heuristics over the function_declaration subtree:
         - return-type packed dim → ftype width
         - first unqualified_id with `function` keyword nearby → fname
         - input declarations under the function → params
         - statements not under input/output decls → body *)
    let function_decls = collect_by (has_tag (fun t ->
      prefix_is "function_declaration" t)) mdecl_body in
    List.filter_map (fun fdecl ->
      (* Function name: header has `function ... NAME ;`.  The first
         unqualified_id under the function_declaration whose parent
         isn't a packed dim, port decl, or statement is the name. *)
      let names = ref [] in
      walk (function
        | TUPLE3 (STRING t, SymbolIdentifier id, _)
          when prefix_is "unqualified_id" t -> names := id :: !names
        | _ -> ()) fdecl;
      let fname =
        match List.rev !names with
        | [] -> ""
        | first :: _ -> first
      in
      if fname = "" then None
      else
        let return_w = width_of ~pkgs ~params fdecl in
        let return_w = if return_w <= 0 then 1 else return_w in
        (* Input declarations: walk for `port_declaration_noattr`,
           `tf_port_declaration`, `tf_port_item`, or `module_port_declaration`
           tags.  SystemVerilog `function f (input X, Y, Z);` ports
           parse as `tf_port_item1` — the extractor used to miss
           these and produced 0-param functions, blocking inline. *)
        let port_nodes = collect_by (has_tag (fun t ->
          prefix_is "tf_port_declaration" t
          || prefix_is "tf_port_item" t
          || prefix_is "port_declaration_noattr" t
          || prefix_is "module_port_declaration" t)) fdecl in
        (* In SV `input logic [15:0] a, p, b, q;` the `[15:0]` precedes
           a list of names — and Verible attaches the type info only to
           the FIRST tf_port_item.  Subsequent items (like p, b, q) get
           a `type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4`
           wrapper without the packed dim, so extract_port_decl reports
           them as 1-bit.  Sweep through and propagate the most-recently
           seen explicit width forward. *)
        let last_w = ref 1 in
        let func_params =
          List.concat_map (fun pn ->
            let signals = extract_port_decl ~pkgs ~params pn in
            List.filter_map (fun (s : bsignal) ->
              if s.name = fname then None  (* skip the function name itself *)
              else
                let w =
                  match s.stype with
                  | BInt { width; _ } when width > 1 ->
                      last_w := width; width
                  | BInt { width; _ } -> width
                  | _ -> 1
                in
                let w = if w = 1 then !last_w else w in
                Some (s.name, BInt { width = w; signed = Unsigned }, `Input)
            ) signals
          ) port_nodes in
        (* Body: walk the function looking for statement nodes, but
           STOP at the outermost matching node — don't descend into
           it.  Otherwise a function with `if(c) x = a; else x = b;`
           collects both the conditional_statement AND its inner
           assignment_statements as sibling top-level body stmts.
           Same hazard for a function with multiple top-level
           assigns followed by an if: collect_by returns depth-first,
           so the inner assigns of the if come BEFORE the standalone
           assigns, and `top :: _` previously took the wrong one. *)
        let is_stmt_tag t =
          prefix_is "case_statement" t
          || prefix_is "conditional_statement" t
          || prefix_is "assignment_statement_no_expr" t
          || prefix_is "nonblocking_assignment" t
          || prefix_is "seq_block" t in
        let stmt_nodes =
          let acc = ref [] in
          let rec walk_outer t =
            if has_tag is_stmt_tag t then acc := t :: !acc
            else match t with
              | TUPLE2 (a, b) -> List.iter walk_outer [a; b]
              | TUPLE3 (a, b, c) -> List.iter walk_outer [a; b; c]
              | TUPLE4 (a, b, c, d) -> List.iter walk_outer [a; b; c; d]
              | TUPLE5 (a, b, c, d, e) -> List.iter walk_outer [a; b; c; d; e]
              | TUPLE6 (a, b, c, d, e, f) ->
                  List.iter walk_outer [a; b; c; d; e; f]
              | TUPLE7 (a, b, c, d, e, f, g) ->
                  List.iter walk_outer [a; b; c; d; e; f; g]
              | TUPLE8 (a, b, c, d, e, f, g, h) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h]
              | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i]
              | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i; j]
              | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i; j; k]
              | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i; j; k; l]
              | TLIST xs -> List.iter walk_outer xs
              | _ -> ()
          in
          walk_outer fdecl;
          List.rev !acc
        in
        (* Verible's parse tree returns top-level function body
           statements in REVERSE source order (cons-list build-up).
           Source order matters because behavioral_inline's body_to_expr
           treats the LAST statement as the fname-yielding one and
           substitutes earlier locals into it. *)
        let stmt_nodes = List.rev stmt_nodes in
        let body =
          match stmt_nodes with
          | [] -> []
          | [top] ->
              [stmt_to_bstmt ~pkgs ~params ~arrays:array_names top]
          | many ->
              [BBlock (List.map
                (fun n -> stmt_to_bstmt ~pkgs ~params ~arrays:array_names n)
                many)]
        in
        Some {
          fname;
          is_task = false;
          ftype = BInt { width = return_w; signed = Unsigned };
          params = func_params;
          locals = [];
          body;
        }
    ) function_decls
  in
  {
    name = mdecl.m_name;
    params = [];
    signals;
    processes;
    instances;
    funcs;
    mems = []; attrs = [];
  }

(* ─── Top-level entry ────────────────────────────────────────────── *)

let discover_init_dirs files =
  let acc = ref [] in
  List.iter (fun f ->
    let dir = ref (Filename.dirname f) in
    let last = ref "" in
    while !dir <> !last && !dir <> "/" do
      let candidate = Filename.concat !dir "generated" in
      if Sys.file_exists candidate
         && (try Sys.is_directory candidate with _ -> false)
         && not (List.mem candidate !acc) then
        acc := candidate :: !acc;
      last := !dir;
      dir := Filename.dirname !dir
    done
  ) files;
  List.rev !acc

(* Parse a list of SV files via Verible, find the top module, and
 * convert it (and its specialised children) to BIR. *)
let convert_files_inner ~keep_external ~top files : bprogram =
  mem_init_search_paths := discover_init_dirs files;
  if Sys.getenv_opt "MEM_INIT_DEBUG" <> None && !mem_init_search_paths <> [] then
    Printf.eprintf "[mem_init] auto-discovered: %s\n"
      (String.concat ", " !mem_init_search_paths);
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
        (* Rewrite each instance's module_name from the BASE name (which
         * extract_instances records) to the SPECIALISED sibling that
         * specialise_design picked for this exact instance site. Without
         * this, Behavioral_flatten would pick an arbitrary popcount__W*
         * for an internal `popcount` instance — usually the wrong one. *)
        let rewritten = List.filter_map (fun (i : Behavioral_ir.binstance) ->
          let key = (s.s_name, i.inst_name) in
          match Hashtbl.find_opt
                  Verible_elaborate.inst_specialised key with
          | Some specname -> Some { i with module_name = specname }
          | None ->
              (* No specialise_design entry → either a dead
               * generate branch, a non-module identifier the
               * extractor mis-classified ($clog2-typed locals),
               * or — when keep_external is true — an external
               * library cell (Liberty cell, vendor primitive)
               * that has no SV body in `files`. Default behaviour
               * drops these so Behavioral_flatten doesn't get
               * confused by ghost instances; gate-level miters
               * pass keep_external=true to retain them. *)
              if keep_external then Some i else None
        ) m.instances in
        Some { m with name = s.s_name; instances = rewritten }
  ) specs in
  let prog = { modules = bmods; library_cells = [] } in
  (* Stamp `(* sv_decomp_* *)` attributes from each source file's
   * pre-scan. Sv_attr_extract is a regex-based side pass that runs
   * on the raw SV text — Verible's parse-tree carries attributes in
   * tag layouts that vary per declaration kind, and threading them
   * through the converter cleanly would touch every extract_*
   * helper. The side pass is sufficient for module / signal /
   * port-level attributes, which is where sv_decomp_adder /
   * sv_decomp_mul attach in practice. *)
  let attr_tables = List.map Sv_attr_extract.extract_file files in
  List.fold_left (fun p tbl -> Sv_attr_extract.stamp_program tbl p)
    prog attr_tables

let convert_files ~top files : bprogram =
  convert_files_inner ~keep_external:false ~top files

(* Variant that retains unmatched instances. Use for gate-level
 * netlists where every leaf is a Liberty cell with no in-file
 * module declaration. *)
let convert_files_with_externals ~top files : bprogram =
  convert_files_inner ~keep_external:true ~top files

(* No-top "read everything" convenience: parse the input files, emit one
 * bmodule per declared module at its body-declared default parameters,
 * and skip the top-down specialise_design walk entirely.  Useful when
 * the caller (e.g. the GUI) wants to inspect a file before deciding
 * which module to elaborate as top.  Param-dependent specialisations
 * (popcount__W8 vs popcount__W16) are NOT generated here — they require
 * a known instantiation site and so live downstream of elaboration.   *)
let convert_files_all files : bprogram =
  mem_init_search_paths := discover_init_dirs files;
  let mods, pkgs = parse_files_full files in
  let bmods = List.map (fun (mdecl : module_decl) ->
    let m = convert_module ~pkgs mdecl [] in
    { m with name = mdecl.m_name }
  ) mods in
  let prog = { modules = bmods; library_cells = [] } in
  let attr_tables = List.map Sv_attr_extract.extract_file files in
  List.fold_left (fun p tbl -> Sv_attr_extract.stamp_program tbl p)
    prog attr_tables
