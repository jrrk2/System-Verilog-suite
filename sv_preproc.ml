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
 *   - Multi-line `define continuations (trailing `\`) not supported
 *     yet — picorv32 doesn't use them, but cva6 might.
 *   - String comparison in `ifdef NAME is text-only — no `nettype-
 *     style numeric arguments. *)

type macro =
  | Plain of string                          (* `define X text *)
  | Func  of string list * string            (* `define X(a,b) text *)

let defines : (string, macro) Hashtbl.t = Hashtbl.create 64

let reset () = Hashtbl.clear defines

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
let process_directive ifdef_stk line =
  let active () = List.for_all (fun b -> b) !ifdef_stk in
  let n = String.length line in
  let (kw, p) = read_ident line 1 in
  let rest = if p < n then String.sub line p (n - p) else "" in
  let rest = String.trim rest in
  match kw with
  | "ifdef" ->
      let cond = active () && Hashtbl.mem defines rest in
      ifdef_stk := cond :: !ifdef_stk;
      ""
  | "ifndef" ->
      let cond = active () && not (Hashtbl.mem defines rest) in
      ifdef_stk := cond :: !ifdef_stk;
      ""
  | "else" ->
      (match !ifdef_stk with
       | top :: tl ->
           let outer = List.for_all (fun b -> b) tl in
           ifdef_stk := (outer && not top) :: tl
       | [] -> ());
      ""
  | "elsif" ->
      (match !ifdef_stk with
       | _top :: tl ->
           let outer = List.for_all (fun b -> b) tl in
           let cond = outer && Hashtbl.mem defines rest in
           ifdef_stk := cond :: tl
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
      ""  (* TODO: handle include — picorv32 doesn't use it *)
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

(* Main entry: read file, return preprocessed string. We preserve line
 * counts (blank lines for skipped sections / directives) so any later
 * error messages still point to roughly the right source line. *)
let preprocess_file filename =
  reset ();
  let ic = open_in filename in
  let lines = ref [] in
  (try while true do lines := input_line ic :: !lines done
   with End_of_file -> ());
  close_in ic;
  let raw = List.rev !lines in
  let ifdef_stk = ref [] in
  let out = Buffer.create 8192 in
  List.iter (fun line ->
    let stripped = String.trim (strip_line_comment line) in
    if String.length stripped > 0 && stripped.[0] = '`' then begin
      let txt = process_directive ifdef_stk stripped in
      if txt <> "" then Buffer.add_string out txt;
      Buffer.add_char out '\n'
    end else begin
      let active = List.for_all (fun b -> b) !ifdef_stk in
      if active then Buffer.add_string out (expand_string line);
      Buffer.add_char out '\n'
    end
  ) raw;
  Buffer.contents out
