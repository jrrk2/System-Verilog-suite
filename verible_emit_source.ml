(* verible_emit_source — initial scaffolding for a Verible-CST →
 * SystemVerilog-source emitter.
 *
 * Context: the miter pipeline needs to consume Xilinx unisim
 * primitives (RAMB18E1, etc.) but slang's strict static elaboration
 * rejects parameter-dead width-OOB code paths, and verilator/verible
 * each have their own quirks with the upstream Vivado models.  An
 * awk-based preprocessor was tried and rejected (see project memory
 * `verible-elab-source-dumper`); the agreed path is to elaborate
 * with verible (parameter-specialise, prune non-synth scaffolding)
 * and emit a clean simplified source the other frontends can swallow.
 *
 * Pipeline (mirrors VHDL→SV via VhdlMain.main → Rewrite.abstraction →
 * Rewrite.dump):
 *
 *   1. Verible parse           — existing, Source_text_verible.mly
 *   2. verible_elaborate       — existing, parameter resolution
 *   3. synth_filter (here)     — drop initial / specify / `always @(GSR)`
 *                                / $display / $finish / deassign /
 *                                procedural continuous assign nodes
 *   4. emit_program (here)     — token-by-token source emit
 *
 * Status: SCAFFOLD ONLY.
 *   - token_to_source handles ~60 of 529 token variants; the rest
 *     fall through to Source_text_verible_tokens.getstr which yields
 *     a debug-style identifier, not Verilog source.
 *   - synth_filter is a stub that drops nothing.
 *   - emit_program concatenates leaves with single spaces — no
 *     indentation, no newlines between declarations, no formatting.
 *
 * The intent is to provide a compileable foundation that a follow-up
 * pass (or several) extends until counter.sv round-trips identically
 * through verilator.
 *)

open Source_text_verible

(* ──────────────────────────────────────────────────────────────────
 * Token → source-text mapping.
 *
 * For most tokens the SV source spelling is mechanically derivable
 * from the variant constructor:
 *   - lowercase keywords:  `Always` → "always", `Module` → "module"
 *   - symbolic forms:      `AMPERSAND` → "&", `LBRACK` → "["
 *   - directives:          `BACKQUOTE_define` → "`define"
 *
 * The handful below is the manually-curated common set.  Tokens not
 * listed here fall through to `getstr`, which is wrong but loud —
 * downstream the round-trip diff will surface which tokens need
 * adding.
 *)
let token_to_source : token -> string = function
  (* Whitespace / structural keywords *)
  | Module           -> "module"
  | Endmodule        -> "endmodule"
  | Function         -> "function"
  | Endfunction      -> "endfunction"
  | Task             -> "task"
  | Endtask          -> "endtask"
  | Automatic        -> "automatic"
  | Static           -> "static"
  | Return           -> "return"
  | Void             -> "void"
  | Genvar           -> "genvar"
  | Repeat           -> "repeat"
  | Forever          -> "forever"
  | Foreach          -> "foreach"
  | Break            -> "break"
  | Continue         -> "continue"
  | Begin            -> "begin"
  | End              -> "end"
  | If               -> "if"
  | Else             -> "else"
  | For              -> "for"
  | While            -> "while"
  | Case             -> "case"
  | Casez            -> "casez"
  | Casex            -> "casex"
  | Endcase          -> "endcase"
  | Default          -> "default"
  | Always           -> "always"
  | Always_comb      -> "always_comb"
  | Always_ff        -> "always_ff"
  | Always_latch     -> "always_latch"
  | Assign           -> "assign"
  | Parameter        -> "parameter"
  | Localparam       -> "localparam"
  | Generate         -> "generate"
  | Endgenerate      -> "endgenerate"
  | Input            -> "input"
  | Output           -> "output"
  | Inout            -> "inout"
  | Wire             -> "wire"
  | Reg              -> "reg"
  | Logic            -> "logic"
  | Tri             -> "tri"
  | Tri0            -> "tri0"
  | Tri1            -> "tri1"
  | Wand            -> "wand"
  | Wor             -> "wor"
  | Supply0         -> "supply0"
  | Supply1         -> "supply1"
  | Integer          -> "integer"
  | Int              -> "int"
  | Signed           -> "signed"
  | Unsigned         -> "unsigned"
  | Real             -> "real"
  | Realtime         -> "realtime"
  | Time             -> "time"
  | Shortint         -> "shortint"
  | Longint          -> "longint"
  | Shortreal        -> "shortreal"
  | Bit              -> "bit"
  | Byte             -> "byte"
  | Posedge          -> "posedge"
  | Negedge          -> "negedge"
  | Or               -> "or"
  | And              -> "and"
  | Xor              -> "xor"
  | Not              -> "not"
  (* Operators *)
  | AMPERSAND        -> "&"
  | AMPERSAND_AMPERSAND -> "&&"
  | VBAR             -> "|"
  | VBAR_VBAR        -> "||"
  | EQUALS           -> "="
  | EQ_EQ            -> "=="
  | EQ_EQ_EQ         -> "==="
  | EQ_GT            -> "=>"
  | STAR_GT          -> "*>"
  | PLING            -> "!"
  | PLING_EQ         -> "!="
  | PLING_EQ_EQ      -> "!=="
  | PLUS_COLON       -> "+:"
  | HYPHEN_COLON     -> "-:"
  | LESS             -> "<"
  | GREATER          -> ">"
  | LT_EQ            -> "<="
  | GT_EQ            -> ">="
  | PLUS             -> "+"
  | HYPHEN           -> "-"
  | STAR             -> "*"
  | SLASH            -> "/"
  | PERCENT          -> "%"
  | TILDE            -> "~"
  | CARET            -> "^"
  | QUERY            -> "?"
  | TILDE_AMPERSAND  -> "~&"
  | TILDE_VBAR       -> "~|"
  | TILDE_CARET      -> "~^"
  | LT_LT            -> "<<"
  | GT_GT            -> ">>"
  | GT_GT_GT         -> ">>>"
  (* Compound assignments — `<lhs> <op>= <rhs>` *)
  | PLUS_EQ          -> "+="
  | HYPHEN_EQ        -> "-="
  | STAR_EQ          -> "*="
  | SLASH_EQ         -> "/="
  | PERCENT_EQ       -> "%="
  | AMPERSAND_EQ     -> "&="
  | VBAR_EQ          -> "|="
  | CARET_EQ         -> "^="
  | TK_LS_EQ         -> "<<="
  | TK_RS_EQ         -> ">>="
  | TK_RSS_EQ        -> ">>>="
  (* Brackets / punctuation *)
  | LPAREN           -> "("
  | RPAREN           -> ")"
  | LBRACK           -> "["
  | RBRACK           -> "]"
  | LBRACE           -> "{"
  | RBRACE           -> "}"
  | QUOTE_LBRACE     -> "'{"   (* assignment pattern `'{...}` *)
  | QUOTE            -> "'"
  (* Newline after every statement/declaration terminator: verilator
   * caps preprocessor tokens at 40000 per line, and the whole emit
   * is otherwise one giant line.  The trailing \n is cosmetic for the
   * other frontends but required for verilator. *)
  | SEMICOLON        -> ";\n"
  | COMMA            -> ","
  | COLON            -> ":"
  | DOT              -> "."
  | AT               -> "@"
  | HASH             -> "#"
  | other -> Source_text_verible_tokens.getstr other

(* ──────────────────────────────────────────────────────────────────
 * Synth-subset filter.  Recursively replaces non-synthesizable CST
 * subtrees with EMPTY_TOKEN (which emits as ""), so the round-tripped
 * source is acceptable to the strict frontends (slang especially).
 *
 * Dropped by grammar-production tag prefix:
 *   - initial_construct*        — `initial begin … end` (mem init,
 *                                 X-prop scaffolding)
 *   - specify_block*            — `specify … endspecify` path delays
 *   - system_tf_call*           — `$display`/`$finish`/`$readmemh`/…
 *   - procedural_continuous_assignment*
 *                               — procedural `assign`/`deassign`/
 *                                 `force`/`release` (the `always @(GSR)`
 *                                 X-prop block reduces to empty if/else
 *                                 once these are gone)
 *
 * Leftover separators (a stray `;` where a $display statement was, an
 * empty `begin end`) are legal SystemVerilog, so we don't need to
 * prune the enclosing wrappers.
 *)
let prefix_is p s =
  let lp = String.length p and ls = String.length s in
  ls >= lp && String.sub s 0 lp = p

let droppable_tag tag =
  prefix_is "initial_construct" tag
  || prefix_is "specify_block" tag
  || prefix_is "system_tf_call" tag
  || prefix_is "procedural_continuous_assignment" tag

(* True when a tuple's leading STRING tag marks a droppable shape. *)
let tagged_droppable = function
  | STRING tag -> droppable_tag tag
  | _ -> false

(* Leftmost SymbolIdentifier in a subtree, skipping STRING grammar
 * tags.  Used to spot `$`-prefixed call statements ($display,
 * $finish, $write, $fatal, …) that lex as ordinary SymbolIdentifier
 * calls — they aren't on the lexer's elaboration-builtin whitelist
 * so they don't become system_tf_call nodes. *)
let rec leftmost_ident = function
  | SymbolIdentifier s -> Some s
  | STRING _ -> None
  | TLIST xs -> List.find_map leftmost_ident xs
  | TUPLE2 (a, b) -> List.find_map leftmost_ident [a; b]
  | TUPLE3 (a, b, c) -> List.find_map leftmost_ident [a; b; c]
  | TUPLE4 (a, b, c, d) -> List.find_map leftmost_ident [a; b; c; d]
  | TUPLE5 (a, b, c, d, e) -> List.find_map leftmost_ident [a; b; c; d; e]
  | _ -> None

let is_dollar_call t =
  match leftmost_ident t with
  | Some s -> String.length s > 0 && s.[0] = '$'
  | None -> false

(* Replacement for a dropped node.  Statements that were the sole
 * body of an `if`/`else`/`for`/`while` can't vanish entirely or the
 * enclosing clause is left dangling (`if (c)` with no statement), so
 * we substitute an empty statement `;` rather than EMPTY_TOKEN.  A
 * stray `;` is a legal null statement / module item in either
 * context. *)
let dropped = STRING ";"

let rec synth_filter (t : token) : token =
  match t with
  | TLIST xs -> TLIST (List.map synth_filter xs)
  (* Call statements whose target is a `$`-prefixed runtime task
   * ($display/$finish/$write/$fatal/…).  These lex as ordinary
   * SymbolIdentifier calls, so they surface in several wrapper
   * shapes depending on whether they have an argument list:
   *   - statement3                      `$display(args);`
   *   - statement_item19                subroutine_call SEMICOLON
   *   - block_item_or_statement_or_null6  bare `$finish;`
   *   - data_declaration_or_module_instantiation1
   *       a `$display(args);` inside a begin-block parses here —
   *       verible can't tell a bare call from a module instance in
   *       that context, so it lands in the instantiation shape.
   * Drop the whole statement only when the call target is a `$`-task
   * (a real module instantiation keeps its non-`$` identifier). *)
  | TUPLE3 (STRING tag, call, _)
    when (tag = "statement3"
          || tag = "statement_item19"
          || tag = "block_item_or_statement_or_null6"
          || tag = "data_declaration_or_module_instantiation1")
         && is_dollar_call call ->
      dropped
  (* `net_decl_assign1`: <id> = <expr>.  Xilinx unisims source the
   * Global Set/Reset net from the top-level `glbl` module via a
   * hierarchical reference (`tri0 GSR = glbl.GSR;`).  That reference
   * can't resolve when a primitive is elaborated standalone, and the
   * synth-correct behaviour is GSR tied low (tri0 default).  Drop the
   * `= glbl.<x>` initializer, leaving the bare net name. *)
  | TUPLE4 (STRING "net_decl_assign1", id, _eq, rhs)
    when leftmost_ident rhs = Some "glbl" ->
      synth_filter id
  | TUPLE2 (a, _) when tagged_droppable a -> dropped
  | TUPLE3 (a, _, _) when tagged_droppable a -> dropped
  | TUPLE4 (a, _, _, _) when tagged_droppable a -> dropped
  | TUPLE5 (a, _, _, _, _) when tagged_droppable a -> dropped
  | TUPLE6 (a, _, _, _, _, _) when tagged_droppable a -> dropped
  | TUPLE7 (a, _, _, _, _, _, _) when tagged_droppable a -> dropped
  | TUPLE8 (a, _, _, _, _, _, _, _) when tagged_droppable a -> dropped
  | TUPLE2 (a, b) -> TUPLE2 (synth_filter a, synth_filter b)
  | TUPLE3 (a, b, c) -> TUPLE3 (synth_filter a, synth_filter b, synth_filter c)
  | TUPLE4 (a, b, c, d) ->
      TUPLE4 (synth_filter a, synth_filter b, synth_filter c, synth_filter d)
  | TUPLE5 (a, b, c, d, e) ->
      TUPLE5 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
              synth_filter e)
  | TUPLE6 (a, b, c, d, e, f) ->
      TUPLE6 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
              synth_filter e, synth_filter f)
  | TUPLE7 (a, b, c, d, e, f, g) ->
      TUPLE7 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
              synth_filter e, synth_filter f, synth_filter g)
  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      TUPLE8 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
              synth_filter e, synth_filter f, synth_filter g, synth_filter h)
  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      TUPLE9 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
              synth_filter e, synth_filter f, synth_filter g, synth_filter h,
              synth_filter i)
  | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
      TUPLE10 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
               synth_filter e, synth_filter f, synth_filter g, synth_filter h,
               synth_filter i, synth_filter j)
  | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
      TUPLE11 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
               synth_filter e, synth_filter f, synth_filter g, synth_filter h,
               synth_filter i, synth_filter j, synth_filter k)
  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      TUPLE12 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
               synth_filter e, synth_filter f, synth_filter g, synth_filter h,
               synth_filter i, synth_filter j, synth_filter k, synth_filter l)
  | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
      TUPLE13 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
               synth_filter e, synth_filter f, synth_filter g, synth_filter h,
               synth_filter i, synth_filter j, synth_filter k, synth_filter l,
               synth_filter m)
  | TUPLE14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) ->
      TUPLE14 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
               synth_filter e, synth_filter f, synth_filter g, synth_filter h,
               synth_filter i, synth_filter j, synth_filter k, synth_filter l,
               synth_filter m, synth_filter n)
  | TUPLE15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) ->
      TUPLE15 (synth_filter a, synth_filter b, synth_filter c, synth_filter d,
               synth_filter e, synth_filter f, synth_filter g, synth_filter h,
               synth_filter i, synth_filter j, synth_filter k, synth_filter l,
               synth_filter m, synth_filter n, synth_filter o)
  | leaf -> leaf

(* ──────────────────────────────────────────────────────────────────
 * CST walker.  Emits children in tree order, joined by a single
 * space.  Adequate for proving the round-trip plumbing works; the
 * next iteration adds shape-aware spacing/newlines/indentation by
 * dispatching on the tagged-tuple prefix (`module_decl`, `if_else`,
 * `seq_block`, etc.).
 *)
(* Grammar-production tags appear as the first STRING child of every
 * tagged tuple — they identify the rule (e.g. "ml_start1",
 * "module_declaration2") and must NOT be emitted as source text.
 * Plain STRING leaves outside that position are source-bearing
 * (identifiers, literals) and are emitted as-is.
 *)
let emit_children xs =
  let rec drop_first_tag = function
    | STRING _ :: rest -> rest             (* the production tag *)
    | xs -> xs
  in
  drop_first_tag xs

let rec emit (t : token) : string =
  match t with
  (* Every multi-element TLIST in Source_text_verible.mly is built by
   * prepending (`$3 :: lst`), so storage order is reversed from source
   * order.  Reverse on emit to recover source order — this fixes
   * statement/item lists (where order is semantically load-bearing)
   * globally.  Comma/or-separated lists additionally need their
   * separator re-injected, handled by the per-parent cases below via
   * emit_comma_list / emit_or_list. *)
  | TLIST xs -> String.concat " " (List.map emit (List.rev xs))
  (* `range_list_in_braces1`: LBRACE open_range_list RBRACE.  The
   * `open_range_list` action in Source_text_verible.mly prepends each
   * new element onto the TLIST (`$3 :: lst`) and discards the COMMA
   * token, so the parsed list is reversed and comma-less.  Reverse
   * the TLIST and re-inject commas on emit so `{e1, e2, e3}` round-
   * trips faithfully.  Other comma-separated reversed-TLIST
   * productions follow the same pattern and will be added here as
   * round-trip diffs surface them. *)
  | TUPLE4 (STRING tag, lb, body, rb) when prefix_is "range_list_in_braces" tag ->
      String.concat " " [emit lb; emit_comma_list body; emit rb]
  (* `event_control2`: AT LPAREN event_expression_list RPAREN.  The
   * list production prepends elements and drops the Or/COMMA token,
   * so `@(a or b or c)` parses to a reversed comma-less TLIST.
   * Reverse and rejoin with `or` (the canonical separator). *)
  | TUPLE5 (STRING "event_control2", at, lp, body, rp) ->
      String.concat " " [emit at; emit lp; emit_or_list body; emit rp]
  (* `instantiation_base1`: instantiation_type <var/instance list>.
   * Covers both `reg a, b, c;` (register-variable list) and
   * `foo u1(...), u2(...);` (multi-instance).  The list production
   * (non_anonymous_gate_instance_or_register_variable_list) reverses
   * and drops COMMA, so reverse and recomma.  Single-element lists
   * (the common one-instance case) emit unchanged. *)
  | TUPLE3 (STRING tag, ty, lst)
    when tag = "instantiation_base1"
      || tag = "non_anonymous_instantiation_base1" ->
      emit ty ^ " " ^ emit_comma_list lst
  (* `module_port_declaration5`: port_direction signed_unsigned_opt
   * list_of_module_item_identifiers SEMICOLON — `input a, b, c;`.
   * The identifier list reverses + drops COMMA. *)
  | TUPLE5 (STRING "module_port_declaration5", dir, su, lst, semi) ->
      String.concat " " [emit dir; emit su; emit_comma_list lst; emit semi]
  (* Sized literals: `dec_/bin_/oct_/hex_based_number` wrap a base
   * token (which already carries `<size>'<base>`, e.g. "1'b") and a
   * digits token ("0").  They must be concatenated with NO separator
   * so `1'b0` emits as `1'b0`, not `1'b 0`.  `number3` is the split
   * form `<size> <based_number>` (size lexed as a separate decimal
   * token) and likewise glues with no space. *)
  | TUPLE3 (STRING tag, a, b)
    when prefix_is "dec_based_number" tag
      || prefix_is "bin_based_number" tag
      || prefix_is "oct_based_number" tag
      || prefix_is "hex_based_number" tag
      || tag = "number3" ->
      emit a ^ emit b
  | TUPLE2 (a, b) ->
      String.concat " " (List.map emit (emit_children [a; b]))
  | TUPLE3 (a, b, c) ->
      String.concat " " (List.map emit (emit_children [a; b; c]))
  | TUPLE4 (a, b, c, d) ->
      String.concat " " (List.map emit (emit_children [a; b; c; d]))
  | TUPLE5 (a, b, c, d, e) ->
      String.concat " " (List.map emit (emit_children [a; b; c; d; e]))
  | TUPLE6 (a, b, c, d, e, f) ->
      String.concat " " (List.map emit (emit_children [a; b; c; d; e; f]))
  | TUPLE7 (a, b, c, d, e, f, g) ->
      String.concat " " (List.map emit (emit_children [a; b; c; d; e; f; g]))
  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h]))
  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h; i]))
  | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h; i; j]))
  | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h; i; j; k]))
  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h; i; j; k; l]))
  | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h; i; j; k; l; m]))
  | TUPLE14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) ->
      String.concat " "
        (List.map emit (emit_children [a; b; c; d; e; f; g; h; i; j; k; l; m; n]))
  | TUPLE15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) ->
      String.concat " "
        (List.map emit (emit_children
                          [a; b; c; d; e; f; g; h; i; j; k; l; m; n; o]))
  | STRING s -> s
  | EMPTY_TOKEN -> ""
  | End_of_file -> ""
  | SymbolIdentifier s -> s
  (* Numeric literal pieces — TK_BinBase carries "'b" or "'sb" and
   * TK_BinDigits carries the actual digit string.  The grammar
   * concatenates them as adjacent tokens so the emitter just
   * pastes the payload through. *)
  | TK_DecNumber s -> s
  | TK_UnBasedNumber s -> s
  | TK_BinBase s -> s
  | TK_BinDigits s -> s
  | TK_OctBase s -> s
  | TK_OctDigits s -> s
  | TK_HexBase s -> s
  | TK_HexDigits s -> s
  | TK_DecBase s -> s
  | TK_DecDigits s -> s
  | TK_RealTime s -> s
  | TK_StringLiteral s -> "\"" ^ s ^ "\""
  | leaf -> token_to_source leaf

(* Reverse, emit each element, drop any that render empty (the list
 * productions sometimes carry an EMPTY_TOKEN sentinel as the base of
 * the prepend chain — emitting it would leave a dangling separator),
 * then join with [sep]. *)
and emit_sep_list sep = function
  | TLIST xs ->
      List.rev xs
      |> List.map emit
      |> List.filter (fun s -> String.trim s <> "")
      |> String.concat sep
  | single -> emit single

and emit_comma_list t = emit_sep_list " , " t
and emit_or_list t = emit_sep_list " or " t

let emit_program (t : token) : string = emit (synth_filter t)

(* ──────────────────────────────────────────────────────────────────
 * Elaborating entry-point: parse → verible_elaborate → synth_filter
 * → emit.  Mirrors the int-scope construction that
 * verible_to_behavioral.convert_files uses (a copy of
 * verible_elaborate's module-local `int_scope_of`, which isn't
 * exported), then prunes dead generate branches per module before
 * the synth filter and source emit run.
 *
 * `overrides` are top-level parameter values (e.g. RAMB18E1 as
 * instantiated by RAMB16_S9_S9 with READ_WIDTH_A=9): they seed the
 * scope for every module whose name is in `apply_to` (or all modules
 * when `apply_to` is empty).  Modules without an override fall back
 * to their declared defaults.
 *)
let int_scope_of ~pkgs ~overrides (body : token) : (string * int) list =
  let eval sc s = Verible_elaborate.Eval.eval_string sc s in
  let resolve rhs = Verible_elaborate.resolve_value pkgs rhs in
  let scope =
    List.filter_map (fun (k, v) -> Option.map (fun n -> (k, n)) (eval [] v))
      overrides in
  (* Port-parameter defaults for any param not overridden. *)
  let defaults = Verible_elaborate.extract_module_port_param_defaults body in
  let scope =
    List.fold_left (fun sc (name, rhs) ->
      if List.mem_assoc name sc then sc
      else match eval sc (resolve rhs) with
        | Some n -> (name, n) :: sc
        | None -> sc) scope defaults in
  (* Internal localparams resolved to a fixed point (parse-tree order
   * is reversed, so a single pass can miss forward references). *)
  let lps = Verible_elaborate.extract_module_internal_params body in
  let rec fixed_point sc remaining =
    let sc', remaining' =
      List.fold_left (fun (sc, rem) (name, rhs) ->
        if List.mem_assoc name sc then (sc, rem)
        else match eval sc (resolve rhs) with
          | Some n -> ((name, n) :: sc, rem)
          | None -> (sc, (name, rhs) :: rem)) (sc, []) remaining in
    if List.length sc' > List.length sc && remaining' <> [] then
      fixed_point sc' remaining'
    else sc'
  in
  fixed_point scope lps

(* ──────────────────────────────────────────────────────────────────
 * Procedural dead-code elimination.  prune_dead_generates only folds
 * `generate` constructs; this folds *procedural* `if`/`case` whose
 * controlling expression resolves to a constant in [scope] (after
 * parameter elaboration).  Xilinx unisim primitives gate width-
 * specific code on localparams derived from READ_WIDTH/WRITE_WIDTH
 * (`case (rb_width) … 16: mem[..][..*16 +: 16] … endcase`), so the
 * out-of-bounds slices that slang rejects live in `case`/`if` arms
 * that are dead once rb_width is known — but they aren't generate
 * blocks, so only this pass removes them.
 *
 * Uses the same resolver/evaluator refs that emit_elaborated sets up
 * for prune_dead_generates.
 *)
let eval_const scope tok =
  !Verible_elaborate.evaluator_for_walk scope
    (!Verible_elaborate.resolver_for_walk tok)

(* Collect the value expressions of a case_item's
 * expression_list_proper (`1, 2, 4 :` → [1;2;4]).  The production is
 * left-recursive (expression_list_proper1: list COMMA expr), single
 * value bottoms out as the bare expression. *)
let rec case_values = function
  | TUPLE4 (STRING tag, lst, _comma, e) when prefix_is "expression_list_proper" tag ->
      case_values lst @ [e]
  | other -> [other]

let dropped_stmt = STRING ";"

let rec prune_dead_procedural scope tok =
  let pp = prune_dead_procedural scope in
  match tok with
  (* if (cond) then  — no else *)
  | TUPLE5 (STRING "conditional_statement1", _up, _if, cond, then_s) ->
      (match eval_const scope cond with
       | Some 0 -> dropped_stmt
       | Some _ -> pp then_s
       | None -> TUPLE5 (STRING "conditional_statement1", _up, _if,
                         pp cond, pp then_s))
  (* if (cond) then else else_s *)
  | TUPLE7 (STRING "conditional_statement2", _up, _if, cond, then_s, _else, else_s) ->
      (match eval_const scope cond with
       | Some 0 -> pp else_s
       | Some _ -> pp then_s
       | None -> TUPLE7 (STRING "conditional_statement2", _up, _if,
                         pp cond, pp then_s, _else, pp else_s))
  (* case (sel) … endcase *)
  | TUPLE8 (STRING "case_statement1", up, ca, lp, sel, rp, items, ec) ->
      (match eval_const scope sel with
       | None ->
           TUPLE8 (STRING "case_statement1", up, ca, lp, pp sel, rp, pp items, ec)
       | Some v ->
           (* Flatten case_items (left-recursive case_items1) into a
            * list, find the arm matching v, else the Default arm. *)
           let rec flat = function
             | TUPLE3 (STRING t, rest, item) when prefix_is "case_items" t ->
                 flat rest @ [item]
             | other -> [other] in
           let items_l = flat items in
           let matches item =
             match item with
             | TUPLE4 (STRING t, vals, _colon, _stmt) when prefix_is "case_item1" t ->
                 List.exists (fun ve -> eval_const scope ve = Some v)
                   (case_values vals)
             | _ -> false in
           let stmt_of = function
             | TUPLE4 (STRING t, _vals, _colon, s)
               when prefix_is "case_item1" t || prefix_is "case_item2" t -> Some s
             | TUPLE3 (STRING t, _default, s) when prefix_is "case_item3" t -> Some s
             | _ -> None in
           let is_default = function
             | TUPLE4 (STRING t, _, _, _) when prefix_is "case_item2" t -> true
             | TUPLE3 (STRING t, _, _) when prefix_is "case_item3" t -> true
             | _ -> false in
           let chosen =
             match List.find_opt matches items_l with
             | Some it -> stmt_of it
             | None ->
                 (match List.find_opt is_default items_l with
                  | Some it -> stmt_of it
                  | None -> None) in
           (match chosen with
            | Some s -> pp s
            | None -> dropped_stmt))
  | TLIST xs -> TLIST (List.map pp xs)
  | TUPLE2 (a, b) -> TUPLE2 (pp a, pp b)
  | TUPLE3 (a, b, c) -> TUPLE3 (pp a, pp b, pp c)
  | TUPLE4 (a, b, c, d) -> TUPLE4 (pp a, pp b, pp c, pp d)
  | TUPLE5 (a, b, c, d, e) -> TUPLE5 (pp a, pp b, pp c, pp d, pp e)
  | TUPLE6 (a, b, c, d, e, f) -> TUPLE6 (pp a, pp b, pp c, pp d, pp e, pp f)
  | TUPLE7 (a, b, c, d, e, f, g) -> TUPLE7 (pp a, pp b, pp c, pp d, pp e, pp f, pp g)
  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      TUPLE8 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h)
  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      TUPLE9 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i)
  | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
      TUPLE10 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i, pp j)
  | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
      TUPLE11 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i, pp j, pp k)
  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      TUPLE12 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i, pp j, pp k, pp l)
  | TUPLE13 (a, b, c, d, e, f, g, h, i, j, k, l, m) ->
      TUPLE13 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i, pp j, pp k, pp l, pp m)
  | TUPLE14 (a, b, c, d, e, f, g, h, i, j, k, l, m, n) ->
      TUPLE14 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i, pp j, pp k, pp l, pp m, pp n)
  | TUPLE15 (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) ->
      TUPLE15 (pp a, pp b, pp c, pp d, pp e, pp f, pp g, pp h, pp i, pp j, pp k, pp l, pp m, pp n, pp o)
  | leaf -> leaf

let emit_elaborated ?(overrides = []) ?(apply_to = []) files : string =
  let mods, pkgs = Verible_elaborate.parse_files_full files in
  Verible_elaborate.resolver_for_walk :=
    (fun t -> Verible_elaborate.resolve_value pkgs t);
  Verible_elaborate.evaluator_for_walk := Verible_elaborate.Eval.eval_string;
  List.map (fun (m : Verible_elaborate.module_decl) ->
    let ov = if apply_to = [] || List.mem m.m_name apply_to
             then overrides else [] in
    let scope = int_scope_of ~pkgs ~overrides:ov m.m_body in
    let pruned =
      m.m_body
      |> Verible_elaborate.prune_dead_generates scope
      |> prune_dead_procedural scope
    in
    emit_program pruned
  ) mods
  |> String.concat "\n\n"
