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
  | Integer          -> "integer"
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
  | LESS             -> "<"
  | GREATER          -> ">"
  | LT_EQ            -> "<="
  | GT_EQ            -> ">="
  | PLUS             -> "+"
  | HYPHEN           -> "-"
  | STAR             -> "*"
  | SLASH            -> "/"
  | TILDE            -> "~"
  (* Brackets / punctuation *)
  | LPAREN           -> "("
  | RPAREN           -> ")"
  | LBRACK           -> "["
  | RBRACK           -> "]"
  | LBRACE           -> "{"
  | RBRACE           -> "}"
  | SEMICOLON        -> ";"
  | COMMA            -> ","
  | COLON            -> ":"
  | DOT              -> "."
  | AT               -> "@"
  | other -> Source_text_verible_tokens.getstr other

(* ──────────────────────────────────────────────────────────────────
 * Synth-subset filter.  Currently a no-op; downstream passes will
 * pattern-match the relevant CST shapes (initial_construct,
 * specify_block, always_construct with @(GSR), $display calls,
 * deassign_statement, procedural-continuous-assign) and elide them.
 *)
let synth_filter (t : token) : token = t

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
  | TLIST xs -> String.concat " " (List.map emit xs)
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
  | TK_DecNumber s -> s
  | TK_UnBasedNumber s -> s
  | TK_StringLiteral s -> "\"" ^ s ^ "\""
  | leaf -> token_to_source leaf

(* Convenience entry-point.  Eventually:
 *   parse → elaborate → synth_filter → emit
 * For now the test driver supplies the parsed tree directly so we
 * can wire the emit step before the elaborator gets a corresponding
 * CST-preserving variant.
 *)
let emit_program (t : token) : string = emit (synth_filter t)
