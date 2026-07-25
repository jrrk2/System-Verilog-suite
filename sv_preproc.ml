(* SystemVerilog preprocessor — minimal in-memory implementation that
 * runs over a source file and produces a single string with:
 *   - `define ... ` macros substituted (parameterless and function-like)
 *   - `ifdef / `ifndef / `else / `elsif / `endif conditional sections
 *     respected (inactive lines are blanked out, preserving line count)
 *   - other Verilog directives (`timescale, `default_nettype, etc.)
 *     stripped silently
 *   - `include "file" recursively pulled in (file searched in cwd and
 *     the directory of the parent file)
 *
 * The output is fed straight to `Lexing.from_string` so the Verible
 * lexer/parser sees the post-preprocessor token stream — same as a
 * synthesis tool would. This replaces the earlier "skip macros at
 * the lexer level" hack: macros that produce real logic (e.g.
 * `assert(cond)` in FORMAL mode) now expand instead of being elided.
 *
 * Constraints / known gaps:
 *   - String comparison in `ifdef NAME is text-only — no `nettype-
 *     style numeric arguments. *)

type macro =
  | Plain of string                          (* `define X text *)
  | Func  of string list * string            (* `define X(a,b) text *)

let defines : (string, macro) Hashtbl.t = Hashtbl.create 64

(* Search path for `include "file"`.  Set by preprocess_file /
 * preprocess_string callers; defaults to [].  When an include is
 * encountered, the file is searched first in the directory of the
 * including file (pushed onto the front of the path on entry, popped
 * on exit), then in each entry of this list in order. *)
let include_dirs : string list ref = ref []

(* Forward-declared mutable ref so process_directive can recurse into
 * preprocess_string (which is defined below).  Filled in at the
 * bottom of this file. *)
let preprocess_string_ref :
  (?keep_defines:bool -> string -> string) ref =
  ref (fun ?keep_defines:_ s -> s)

(* Already-included files — avoid include-cycle infinite loops without
 * insisting on `\`ifndef GUARD` everywhere.  Cleared by reset(). *)
let included_files : (string, unit) Hashtbl.t = Hashtbl.create 16

(* Seed project-wide defines from the SVS_DEFINE env var so a multi-file
 * synthesis can pass `+define+` equivalents (FPGA_XILINX, VC707, …) that
 * the per-file reset() would otherwise wipe.  Format: entries separated by
 * ';', each "NAME=BODY" (substituted) or bare "NAME" (membership only, empty
 * expansion — enough for `\`ifdef).  Applied on every reset() so the tokens
 * are visible in EVERY file, matching a tool-level +define+.  In-file
 * `\`define / `\`undef still work normally on top of these. *)
let seed_defines_from_env () =
  match Sys.getenv_opt "SVS_DEFINE" with
  | None | Some "" -> ()
  | Some s ->
      List.iter (fun entry ->
        let entry = String.trim entry in
        if entry <> "" then
          match String.index_opt entry '=' with
          | Some i ->
              let name = String.trim (String.sub entry 0 i) in
              let body = String.sub entry (i+1) (String.length entry - i - 1) in
              if name <> "" then Hashtbl.replace defines name (Plain body)
          | None -> Hashtbl.replace defines entry (Plain ""))
        (String.split_on_char ';' s)

let reset () =
  Hashtbl.clear defines;
  Hashtbl.clear included_files;
  seed_defines_from_env ()

let is_id_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_id_char c =
  is_id_start c || (c >= '0' && c <= '9') || c = '$'

let read_ident s pos =
  let n = String.length s in
  let p = ref pos in
  while !p < n && is_id_char s.[!p] do incr p done;
  (String.sub s pos (!p - pos), !p)

let skip_ws s pos =
  let n = String.length s in
  let p = ref pos in
  while !p < n && (s.[!p] = ' ' || s.[!p] = '\t') do incr p done;
  !p

(* Parse a comma-separated arg list starting at `(`. Honours nested
 * parens, string literals, and SV `\`name` macro references inside
 * args (they pass through verbatim — the caller may re-expand). *)
let parse_args s pos =
  assert (pos < String.length s && s.[pos] = '(');
  let n = String.length s in
  let args = ref [] in
  let cur = Buffer.create 32 in
  let depth = ref 1 in
  let in_string = ref false in
  let p = ref (pos + 1) in
  while !p < n && !depth > 0 do
    let c = s.[!p] in
    if !in_string then begin
      Buffer.add_char cur c;
      if c = '"' && (!p = 0 || s.[!p - 1] <> '\\') then in_string := false;
      incr p
    end else begin
      match c with
      | '"' -> Buffer.add_char cur c; in_string := true; incr p
      | '(' -> Buffer.add_char cur c; incr depth; incr p
      | ')' ->
          decr depth;
          if !depth = 0 then begin
            args := Buffer.contents cur :: !args;
            incr p  (* consume the closing `)` *)
          end else begin
            Buffer.add_char cur c;
            incr p
          end
      | ',' when !depth = 1 ->
          args := Buffer.contents cur :: !args;
          Buffer.clear cur;
          incr p
      | _ -> Buffer.add_char cur c; incr p
    end
  done;
  (List.rev_map String.trim !args, !p)

(* Substitute formal → actual within a macro body. We do whole-token
 * matching (so `b` inside `body` doesn't get rewritten to `actual`). *)
let substitute formals actuals body =
  if formals = [] then body
  else begin
    let pairs = List.combine formals actuals in
    let n = String.length body in
    let out = Buffer.create n in
    let p = ref 0 in
    while !p < n do
      let c = body.[!p] in
      if is_id_start c then begin
        let (id, p') = read_ident body !p in
        (match List.assoc_opt id pairs with
         | Some v -> Buffer.add_string out v
         | None   -> Buffer.add_string out id);
        p := p'
      end else if c = '`' && !p + 1 < n && body.[!p + 1] = '`' then begin
        (* `` is the SV token-paste operator: it concatenates the adjacent
         * tokens (e.g. `assert_static_in_package_``__name` → one identifier).
         * Emit nothing so the surrounding text joins, after the formals on
         * either side have been substituted. *)
        p := !p + 2
      end else begin
        Buffer.add_char out c;
        incr p
      end
    done;
    Buffer.contents out
  end

(* Expand every `<name>[(args)] reference in a string. We track string
 * literals and skip over them. Recursion: if expansion produced more
 * macro references, run again (capped to avoid infinite loops). *)
let rec expand_string ?(fuel = 16) s =
  if fuel <= 0 then s
  else
    let n = String.length s in
    let out = Buffer.create n in
    let p = ref 0 in
    let in_string = ref false in
    let progress = ref false in
    while !p < n do
      let c = s.[!p] in
      if !in_string then begin
        Buffer.add_char out c;
        if c = '"' && (!p = 0 || s.[!p - 1] <> '\\') then in_string := false;
        incr p
      end else if c = '"' then begin
        Buffer.add_char out c;
        in_string := true;
        incr p
      end else if c = '`' && !p + 1 < n && is_id_start s.[!p + 1] then begin
        let (name, p') = read_ident s (!p + 1) in
        match Hashtbl.find_opt defines name with
        | None ->
            (* Unknown macro — leave verbatim. The lexer will still
             * skip it via the bare-`<ident> rule we kept. *)
            Buffer.add_substring out s !p (p' - !p);
            p := p'
        | Some (Plain text) ->
            Buffer.add_string out text;
            p := p';
            progress := true
        | Some (Func (formals, body)) ->
            let q = skip_ws s p' in
            if q < n && s.[q] = '(' then begin
              let (actuals, p'') = parse_args s q in
              if List.length actuals = List.length formals then begin
                Buffer.add_string out (substitute formals actuals body);
                p := p'';
                progress := true
              end else begin
                (* arity mismatch — emit verbatim and move on *)
                Buffer.add_substring out s !p (p'' - !p);
                p := p''
              end
            end else begin
              (* `name without args, but defined as Func — emit body
               * with formals untouched (best-effort for picorv32's
               * `FORMAL_KEEP-style toggle macros that some sources
               * declare with empty arg lists). *)
              Buffer.add_string out body;
              p := p';
              progress := true
            end
      end else begin
        Buffer.add_char out c;
        incr p
      end
    done;
    let result = Buffer.contents out in
    if !progress && String.contains result '`'
    then expand_string ~fuel:(fuel - 1) result
    else result

(* Process one directive line (already trimmed, starts with `).
 * Returns text to emit on this line (typically empty). Mutates the
 * ifdef stack and the macro table. *)
(* Each ifdef-stack level carries (branch_active, any_matched):
 *   branch_active — is the CURRENT branch of this if-chain emitting;
 *   any_matched   — has ANY branch of this chain been taken yet.
 * Tracking any_matched is essential for multi-way `ifdef/`elsif/`elsif/`else
 * chains: without it, a trailing `else re-activates once the current (last
 * `elsif) branch is false even though an EARLIER `elsif already matched —
 * e.g. prim_assert.sv (`ifdef VERILATOR / `elsif SYNTHESIS / `elsif YOSYS /
 * `else) would emit BOTH the SYNTHESIS branch and the `else branch. *)
let process_directive ifdef_stk line =
  let branch_active (a, _) = a in
  let active () = List.for_all branch_active !ifdef_stk in
  let outer_of tl = List.for_all branch_active tl in
  let n = String.length line in
  let (kw, p) = read_ident line 1 in
  let rest = if p < n then String.sub line p (n - p) else "" in
  let rest = String.trim rest in
  match kw with
  | "ifdef" ->
      let cond = active () && Hashtbl.mem defines rest in
      ifdef_stk := (cond, cond) :: !ifdef_stk;
      ""
  | "ifndef" ->
      let cond = active () && not (Hashtbl.mem defines rest) in
      ifdef_stk := (cond, cond) :: !ifdef_stk;
      ""
  | "else" ->
      (match !ifdef_stk with
       | (_, matched) :: tl ->
           let outer = outer_of tl in
           ifdef_stk := ((outer && not matched), true) :: tl
       | [] -> ());
      ""
  | "elsif" ->
      (match !ifdef_stk with
       | (_, matched) :: tl ->
           let outer = outer_of tl in
           let cond = outer && (not matched) && Hashtbl.mem defines rest in
           ifdef_stk := (cond, matched || cond) :: tl
       | [] -> ());
      ""
  | "endif" ->
      (match !ifdef_stk with _ :: t -> ifdef_stk := t | [] -> ());
      ""
  | "define" when active () ->
      let n2 = String.length rest in
      let (name, p1) = read_ident rest 0 in
      if p1 < n2 && rest.[p1] = '(' then begin
        let (formals, p2) = parse_args rest p1 in
        let body =
          if p2 < n2
          then String.trim (String.sub rest p2 (n2 - p2))
          else ""
        in
        Hashtbl.replace defines name (Func (formals, body))
      end else begin
        let body =
          if p1 < n2
          then String.trim (String.sub rest p1 (n2 - p1))
          else ""
        in
        Hashtbl.replace defines name (Plain body)
      end;
      ""
  | "undef" when active () ->
      Hashtbl.remove defines rest; ""
  | "include" when active () ->
      (* `include "filename"` (or `include <filename>`).  Search:
         1. directory of the including file (front of include_dirs)
         2. each entry in include_dirs in order.
         Recursively preprocess the contents and inline it.  Cycles
         are short-circuited via included_files.  If the include
         can't be resolved, emit a marker comment and continue —
         most missing-include cases are vendor `\`include
         "definitions.svh"` files we don't have, and the source is
         designed to compile without them. *)
      let strip_quotes s =
        let n = String.length s in
        if n >= 2 &&
           ((s.[0] = '"' && s.[n-1] = '"') ||
            (s.[0] = '<' && s.[n-1] = '>'))
        then String.sub s 1 (n-2) else s
      in
      let target = strip_quotes rest in
      let resolved =
        if Filename.is_relative target then
          List.find_map (fun d ->
            let p = Filename.concat d target in
            if Sys.file_exists p then Some p else None) !include_dirs
        else if Sys.file_exists target then Some target
        else None
      in
      (match resolved with
       | None ->
           (* BOMB by default: a missing `include silently dropped whole
              constructs (e.g. prim_util_memload.svh carries the RAM
              `initial $readmemh(MemInitFile, mem)` — without it the program
              image never gets baked and the SoC boots an empty RAM, with NO
              diagnostic).  Fail loudly and name the search path so the caller
              fixes SVS_INCDIR.  Set SVS_ALLOW_MISSING_INCLUDE=1 for the rare
              genuinely-optional vendor `definitions.svh` the source is designed
              to compile without. *)
           if Sys.getenv_opt "SVS_ALLOW_MISSING_INCLUDE" <> None then begin
             Printf.eprintf
               "[sv_preproc] WARNING: `include \"%s\" not found (searched: %s)\n%!"
               target (String.concat ":" !include_dirs);
             Printf.sprintf "// [sv_preproc] include not resolved: %s\n" target
           end else
             failwith (Printf.sprintf
               "[sv_preproc] `include \"%s\" not found — searched [%s]. \
                Set SVS_INCDIR to the directory containing it (colon-separated), \
                or set SVS_ALLOW_MISSING_INCLUDE=1 if it is genuinely optional."
               target
               (match !include_dirs with [] -> "<empty>" | ds -> String.concat ":" ds))
       | Some path when Hashtbl.mem included_files path ->
           Printf.sprintf "// [sv_preproc] include (already-seen): %s\n"
             path
       | Some path ->
           Hashtbl.add included_files path ();
           let ic = open_in path in
           let n = in_channel_length ic in
           let buf = Bytes.create n in
           really_input ic buf 0 n;
           close_in ic;
           let body = Bytes.unsafe_to_string buf in
           (* Push the included file's dir onto the search path so
              its own relative includes resolve. *)
           let parent_dir = Filename.dirname path in
           let saved = !include_dirs in
           include_dirs := parent_dir :: saved;
           let expanded = !preprocess_string_ref ~keep_defines:true body in
           include_dirs := saved;
           expanded)
  | "timescale" | "default_nettype" | "resetall"
  | "celldefine" | "endcelldefine" | "begin_keywords" | "end_keywords"
  | "pragma" | "line" | "nounconnected_drive" | "unconnected_drive" ->
      ""
  | _ ->
      (* Not a recognised directive. The token starting with `…` is
       * almost certainly a macro reference (e.g. `assert(…) used as
       * a statement on its own line). Run the expander so it folds
       * out via the macro table built from earlier `define lines. *)
      if active () then expand_string line else ""

(* Strip a `// line comment from the END of a line (so directive
 * detection isn't confused by trailing comments). Block comments
 * are left for the Verilog lexer. *)
let strip_line_comment s =
  let n = String.length s in
  let in_string = ref false in
  let cut = ref n in
  let i = ref 0 in
  while !i < n - 1 && !cut = n do
    let c = s.[!i] in
    if !in_string then begin
      if c = '"' && (!i = 0 || s.[!i - 1] <> '\\') then in_string := false;
      incr i
    end else if c = '"' then begin
      in_string := true; incr i
    end else if c = '/' && s.[!i + 1] = '/' then begin
      cut := !i
    end else
      incr i
  done;
  if !cut = n then s else String.sub s 0 !cut

(* Fold SystemVerilog line continuations: `\` at end of line joins
 * the next line into the same logical line.  Per IEEE 1800-2017,
 * this is specifically for multi-line `define bodies, e.g.
 *   `define MACRO(args) \
 *     body_part_1 \
 *     body_part_2
 * The rule is text-level: any line whose last non-whitespace char is
 * `\` continues.  We replace the trailing `\` with a single space and
 * concatenate, then EMIT BLANK lines at the joined positions so
 * downstream error reporting still maps to the right source line.
 * Robust against trailing whitespace after the backslash; doesn't
 * split escaped names like `\identifier` (those would have a space
 * before the next token, never end-of-line). *)
let fold_continuations lines =
  let out = ref [] in
  let pending = Buffer.create 64 in
  let blanks_after = ref 0 in
  List.iter (fun line ->
    let trimmed_r =
      let n = String.length line in
      let p = ref n in
      while !p > 0 && (let c = line.[!p - 1] in c = ' ' || c = '\t') do
        decr p
      done;
      !p in
    if trimmed_r > 0 && line.[trimmed_r - 1] = '\\' then begin
      (* Continuation: append everything up to (but not including)
         the backslash, then a space.  Track that this line shouldn't
         emit its own blank — the joined line will emit later. *)
      Buffer.add_string pending (String.sub line 0 (trimmed_r - 1));
      Buffer.add_char pending ' ';
      incr blanks_after
    end else begin
      Buffer.add_string pending line;
      out := Buffer.contents pending :: !out;
      Buffer.clear pending;
      (* Emit blank-line placeholders for the lines we consumed,
         so the parser's line numbers still match the source. *)
      for _ = 1 to !blanks_after do out := "" :: !out done;
      blanks_after := 0
    end
  ) lines;
  (* If the file ends mid-continuation, flush. *)
  if Buffer.length pending > 0 then begin
    out := Buffer.contents pending :: !out;
    for _ = 1 to !blanks_after do out := "" :: !out done
  end;
  List.rev !out

(* Split a string on newlines into a list of lines (no trailing
 * newline retained on each line). *)
let lines_of_string s =
  let n = String.length s in
  let out = ref [] in
  let start = ref 0 in
  for i = 0 to n - 1 do
    if s.[i] = '\n' then begin
      out := String.sub s !start (i - !start) :: !out;
      start := i + 1
    end
  done;
  if !start < n then out := String.sub s !start (n - !start) :: !out;
  List.rev !out

(* Main entry — string in, preprocessed string out.  Suitable as a
 * `parse-from-string` style hook for callers that already have the
 * source text in memory (tests, embedded library use, the Lua glue,
 * any pipeline that doesn't want a temp-file round-trip).  Resets
 * the macro table on entry; pass `~keep_defines:true` to preserve
 * the table built from a previous call (handy when feeding several
 * fragments through with a shared preamble). *)
(* Strip SystemVerilog attribute instances (open-paren-star ... star-close-paren,
 * e.g. xprop_off, keep="true").  They are synthesis-irrelevant metadata but can
 * appear in almost any position — before an always_comb statement, on a module
 * item, on a port — where the grammar has no production for them, so dropping
 * them in the preprocessor is far simpler than teaching every rule to accept an
 * optional attribute.  A '(' immediately followed by '*' is never a valid SV
 * expression, and attributes do not nest, so a flat scan to the next star-paren
 * is safe.  Runs after line comments are already stripped. *)
let strip_attributes s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if !i + 1 < n && s.[!i] = '(' && s.[!i + 1] = '*' then begin
      let j = ref (!i + 2) in
      let closed = ref false in
      while not !closed && !j + 1 < n do
        if s.[!j] = '*' && s.[!j + 1] = ')' then closed := true else incr j
      done;
      if !closed then (Buffer.add_char buf ' '; i := !j + 2)
      else (Buffer.add_char buf s.[!i]; incr i)  (* unterminated: leave as-is *)
    end else (Buffer.add_char buf s.[!i]; incr i)
  done;
  Buffer.contents buf

let preprocess_string ?(keep_defines = false) text =
  if not keep_defines then reset ();
  let raw = fold_continuations (lines_of_string text) in
  let ifdef_stk = ref [] in
  let out = Buffer.create 8192 in
  List.iter (fun line ->
    let stripped = String.trim (strip_line_comment line) in
    if String.length stripped > 0 && stripped.[0] = '`' then begin
      let txt = process_directive ifdef_stk stripped in
      if txt <> "" then Buffer.add_string out txt;
      Buffer.add_char out '\n'
    end else begin
      let active = List.for_all (fun (a, _) -> a) !ifdef_stk in
      if active then Buffer.add_string out (expand_string line);
      Buffer.add_char out '\n'
    end
  ) raw;
  strip_attributes (Buffer.contents out)

let () = preprocess_string_ref := preprocess_string

(* File-level convenience: read into a string, then funnel through
 * preprocess_string.  The optional ~incdirs seeds the include search
 * path for THIS file's parse only; if the caller has already set
 * `include_dirs := [...]` externally those entries are kept and the
 * argument is appended.  The file's own directory is pushed onto the
 * front automatically so relative `\`include` resolves like a
 * synthesis tool's. *)
(* Scan preprocessed SV text for top-level `module|interface|program <id>`
   declarations and return the names in source order.  Intended to run on
   the OUTPUT of `preprocess_string`/`preprocess_file` so macro-expanded
   and `\`include`-pulled module decls are visible.  Used by tools (e.g.
   the lablgtk3 GUI) that need to pick a sensible top when the user has
   not specified one. *)
let module_decl_re =
  Str.regexp
    "\\(^\\|[^A-Za-z0-9_]\\)\\(module\\|interface\\|program\\)[ \t\n\r]+\\([A-Za-z_][A-Za-z0-9_]*\\)"

let find_module_names text =
  let names = ref [] in
  let pos = ref 0 in
  (try while true do
    let p = Str.search_forward module_decl_re text !pos in
    names := Str.matched_group 3 text :: !names;
    pos := p + 1
  done with Not_found -> ());
  List.rev !names

let preprocess_file ?(incdirs=[]) filename =
  let ic = open_in filename in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let parent_dir = Filename.dirname filename in
  (* SVS_INCDIR: colon-separated project include search path, appended after
   * any caller-supplied incdirs (tool-level +incdir+ equivalent). *)
  let env_incdirs =
    match Sys.getenv_opt "SVS_INCDIR" with
    | None | Some "" -> []
    | Some s -> List.filter (fun d -> d <> "") (String.split_on_char ':' s) in
  let saved = !include_dirs in
  include_dirs := parent_dir :: (saved @ incdirs @ env_incdirs);
  let result = preprocess_string (Bytes.unsafe_to_string buf) in
  include_dirs := saved;
  result
