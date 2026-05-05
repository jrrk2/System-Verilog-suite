(* SystemVerilog `(* key = "val" *)` attribute side-extractor.
 *
 * Verible's parse tree carries attributes inside specific tag layouts
 * that we'd need to learn case-by-case to extract cleanly. This
 * module sidesteps that: it scans the raw SV text with a small
 * tokenizer, recognises attribute groups attached to module / port /
 * declaration sites, and returns side tables keyed by name.
 *
 * After `Verible_to_behavioral.convert_files` produces a bprogram,
 * the convert path stamps each bsignal/bmodule's `attrs` field from
 * these tables. Crude relative to a real parse-tree traversal, but
 * sufficient for the keys we actually care about
 * (sv_decomp_adder / sv_decomp_mul) which only appear on signal and
 * module declarations.
 *
 * Recognised forms:
 *
 *   (* k1 = "v1", k2 = "v2" *) module M (...) ...   ⇒ module M attrs
 *   (* k = "v" *) wire/reg/logic/input/output [...] name ...  ⇒ signal attrs
 *   (* k = "v" *) input/output [...] name           ⇒ port attrs *)

type attrs = (string * string) list

type extracted = {
  module_attrs : (string, attrs) Hashtbl.t;
  signal_attrs : (string * string, attrs) Hashtbl.t;
}

(* Strip line and block comments — they don't contain attributes and
 * confuse the regexes below. *)
let strip_comments s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  while !i < len do
    if !i + 1 < len && s.[!i] = '/' && s.[!i + 1] = '/' then begin
      while !i < len && s.[!i] <> '\n' do incr i done
    end else if !i + 1 < len && s.[!i] = '/' && s.[!i + 1] = '*' then begin
      (* Don't strip SV attribute paren-stars — those start with the
       * other character. Match exactly the C-style block comment. *)
      i := !i + 2;
      while !i + 1 < len && not (s.[!i] = '*' && s.[!i + 1] = '/') do
        incr i
      done;
      if !i + 1 < len then i := !i + 2
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(* Parse a single `(* … *)` block's body into key=value pairs.
 * Body looks like `key1 = "val1", key2 = "val2"`. Multiple attribute
 * blocks in sequence get merged by the caller. *)
let parse_attr_body s : attrs =
  let pairs = String.split_on_char ',' s |> List.map String.trim
              |> List.filter (fun s -> s <> "") in
  List.filter_map (fun p ->
    match String.index_opt p '=' with
    | None ->
        (* Bare key like `(* keep *)` — record with empty value. *)
        Some (String.trim p, "")
    | Some i ->
        let k = String.trim (String.sub p 0 i) in
        let v_raw = String.trim (String.sub p (i + 1) (String.length p - i - 1)) in
        let v =
          let n = String.length v_raw in
          if n >= 2 && v_raw.[0] = '"' && v_raw.[n - 1] = '"' then
            String.sub v_raw 1 (n - 2)
          else v_raw in
        Some (k, v)
  ) pairs

(* Find every `(* … *)` block and the position immediately following.
 * Returns (attrs, position_after_close) for each block, in source
 * order. Adjacent blocks `(* a *)(* b *)` get returned as separate
 * entries; the caller merges if their `position_after_close` is
 * contiguous. *)
let find_attr_blocks src : (attrs * int) list =
  let len = String.length src in
  let acc = ref [] in
  let i = ref 0 in
  while !i + 1 < len do
    if src.[!i] = '(' && src.[!i + 1] = '*'
       && (!i + 2 >= len || src.[!i + 2] <> ')')
    then begin
      let body_start = !i + 2 in
      let j = ref body_start in
      while !j + 1 < len
            && not (src.[!j] = '*' && src.[!j + 1] = ')') do
        incr j
      done;
      if !j + 1 < len then begin
        let body = String.sub src body_start (!j - body_start) in
        let attrs = parse_attr_body body in
        let close = !j + 2 in
        acc := (attrs, close) :: !acc;
        i := close
      end else
        incr i
    end else
      incr i
  done;
  List.rev !acc

(* Skip whitespace from position p, return new position. *)
let rec skip_ws src p =
  let len = String.length src in
  if p >= len then p
  else match src.[p] with
    | ' ' | '\t' | '\n' | '\r' -> skip_ws src (p + 1)
    | _ -> p

(* Read an identifier starting at position p, return (name, end_pos)
 * or None if no identifier there. *)
let read_ident src p =
  let len = String.length src in
  if p >= len then None
  else
    let c = src.[p] in
    if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
       || c = '_' || c = '\\'
    then begin
      let q = ref (p + 1) in
      while !q < len && (let c = src.[!q] in
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9') || c = '_')
      do incr q done;
      Some (String.sub src p (!q - p), !q)
    end else None

(* Skip a SystemVerilog packed/unpacked range `[A:B]` starting at p.
 * If no range, return p unchanged. *)
let skip_range src p =
  let len = String.length src in
  let p = skip_ws src p in
  if p < len && src.[p] = '[' then begin
    let depth = ref 1 in
    let q = ref (p + 1) in
    while !q < len && !depth > 0 do
      (match src.[!q] with
       | '[' -> incr depth
       | ']' -> decr depth
       | _ -> ());
      incr q
    done;
    !q
  end else p

(* Decide what (if any) declared name a position points to. We only
 * recognise a few decl-leading keywords. Returns:
 *   `Module name        — `module <name>`
 *   `Signal name        — wire/reg/logic/input/output/inout … <name>
 *   `None               — anything else
 * Multiple ranges/dimensions are skipped. *)
let classify_decl src p =
  let p = skip_ws src p in
  match read_ident src p with
  | None -> `None
  | Some (kw, p1) ->
      if kw = "module" then begin
        let p2 = skip_ws src p1 in
        match read_ident src p2 with
        | Some (n, _) -> `Module n
        | None -> `None
      end else if List.mem kw
                   ["wire"; "reg"; "logic"; "input"; "output"; "inout"]
      then begin
        (* Possibly: kw [range] [signed] name [range...] *)
        let p2 = skip_ws src p1 in
        (* Skip optional `signed` keyword *)
        let p2 = match read_ident src p2 with
          | Some (k, q) when k = "signed" -> skip_ws src q
          | _ -> p2 in
        let p2 = skip_range src p2 in
        let p2 = skip_ws src p2 in
        match read_ident src p2 with
        | Some (n, _) -> `Signal n
        | None -> `None
      end else `None

(* Top-level: scan `src` for attribute blocks, classify the following
 * declaration, and stuff the side tables. The current module name is
 * tracked by depth-1 scanning of `module <X>` openers (we attribute
 * signals to whichever module they appear inside). *)
let extract (src_with_comments : string) : extracted =
  let src = strip_comments src_with_comments in
  let blocks = find_attr_blocks src in
  let module_attrs : (string, attrs) Hashtbl.t = Hashtbl.create 16 in
  let signal_attrs :
        (string * string, attrs) Hashtbl.t = Hashtbl.create 64 in
  let current_module = ref "" in
  (* Pre-scan once for module openers so signal classifications inside
   * each module get the right key. *)
  let len = String.length src in
  let module_starts = ref [] in
  let i = ref 0 in
  while !i < len do
    match read_ident src !i with
    | Some ("module", q) ->
        let p = skip_ws src q in
        (match read_ident src p with
         | Some (n, _) -> module_starts := (!i, n) :: !module_starts
         | None -> ());
        incr i
    | _ -> incr i
  done;
  let module_starts =
    List.sort (fun (a, _) (b, _) -> compare a b) (List.rev !module_starts) in
  let mod_at p =
    let rec loop best = function
      | [] -> best
      | (start, name) :: rest when start <= p -> loop name rest
      | _ -> best
    in loop "" module_starts
  in
  ignore current_module;
  List.iter (fun (attrs, after_close) ->
    let scope = mod_at after_close in
    match classify_decl src after_close with
    | `Module n ->
        let prev = try Hashtbl.find module_attrs n with Not_found -> [] in
        Hashtbl.replace module_attrs n (attrs @ prev)
    | `Signal n when scope <> "" ->
        let key = (scope, n) in
        let prev = try Hashtbl.find signal_attrs key with Not_found -> [] in
        Hashtbl.replace signal_attrs key (attrs @ prev)
    | _ -> ()
  ) blocks;
  { module_attrs; signal_attrs }

(* Convenience: read the file and extract attributes. *)
let extract_file path : extracted =
  let ic = open_in path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  extract s

(* Stamp a bprogram with attrs from a side table — modifies signals
 * and modules in place by returning a new program with updated lists. *)
let stamp_program (e : extracted) (p : Behavioral_ir.bprogram)
  : Behavioral_ir.bprogram =
  let stamp_signal mod_name (s : Behavioral_ir.bsignal) =
    let key = (mod_name, s.name) in
    match Hashtbl.find_opt e.signal_attrs key with
    | None -> s
    | Some extra ->
        { s with attrs = extra @ s.attrs }
  in
  let stamp_module (m : Behavioral_ir.bmodule) =
    let module_a =
      try Hashtbl.find e.module_attrs m.name with Not_found -> [] in
    {
      m with
      attrs = module_a @ m.attrs;
      signals = List.map (stamp_signal m.name) m.signals;
    }
  in
  { p with modules = List.map stamp_module p.modules }
