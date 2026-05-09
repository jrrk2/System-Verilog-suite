(* convert_hdl: cross-translate VHDL ↔ (System)Verilog for synthesizable
   structures, preserving the leading license / copyright header.
   Pipeline:
     source.{vhd,vhdl}  ──vhdl_to_behavioral──┐
                                              ├─→ BIR ─→ behavioral_to_{verilog,vhdl}
     source.{v,sv}      ──verible_to_behav────┘                     ↓
                                                              target file

   Header preservation: we read the source file's contiguous leading
   comment-or-blank block (up to first code line), strip the
   source's comment markers, and re-emit it with target-language
   markers at the top of the output.  This keeps copyright/SPDX/
   license text intact across translation, which is the dominant
   reason humans care about preservation.

   "Synthesizable only" comes from the BIR itself: vhdl_to_behavioral
   already drops `wait`, `assert`, `report`, file declarations, and
   `after T ns` delays; verible_to_behavioral skips initial blocks
   when not used as ROM init.  So this dispatcher's job is just
   plumbing — the BIR is the synthesis-only contract.

   High-level processes (BSequential / BCombinational with their
   if/case/loop bodies) round-trip as `process` blocks on both sides
   rather than being collapsed to nets — see
   behavioral_to_{verilog,vhdl}'s process emitters. *)

let usage () =
  prerr_endline "convert_hdl <input.{vhd,vhdl,v,sv}> [-o <output>] [--top <name>]";
  prerr_endline "  output extension determines target language; if omitted,";
  prerr_endline "  defaults to the opposite language of the input.";
  exit 2

(* Read leading comment + blank lines, return (header_lines, body) where
   header_lines is the raw lines (with trailing \n stripped) and body is
   the remainder of the file as a single string.  Recognises VHDL `--`
   line comments and Verilog `//` line comments.  A `/* ... */` block at
   the top is consumed as a unit. *)
let extract_header_lines kind src =
  let lines = String.split_on_char '\n' src in
  let is_blank s =
    let s = String.trim s in s = ""
  in
  let is_vhdl_comment s =
    let s = String.trim s in
    String.length s >= 2 && String.sub s 0 2 = "--"
  in
  let is_v_comment s =
    let s = String.trim s in
    String.length s >= 2 && String.sub s 0 2 = "//"
  in
  let starts_block_comment s =
    let s = String.trim s in
    String.length s >= 2 && String.sub s 0 2 = "/*"
  in
  let ends_block_comment s =
    let len = String.length s in
    let rec find i =
      if i >= len - 1 then false
      else if s.[i] = '*' && s.[i+1] = '/' then true
      else find (i+1)
    in find 0
  in
  let rec take acc rest in_block =
    match rest with
    | [] -> (List.rev acc, [])
    | line :: tl ->
        if in_block then
          if ends_block_comment line then take (line :: acc) tl false
          else take (line :: acc) tl true
        else
          let keep = match kind with
            | `Vhdl -> is_blank line || is_vhdl_comment line
            | `Verilog -> is_blank line || is_v_comment line || starts_block_comment line
          in
          if keep then
            let new_in_block =
              kind = `Verilog && starts_block_comment line
              && not (ends_block_comment line)
            in
            take (line :: acc) tl new_in_block
          else (List.rev acc, rest)
  in
  let header, body_lines = take [] lines false in
  (header, String.concat "\n" body_lines)

(* Strip source-language comment markers from each line, returning the
   "raw" header content.  We then re-wrap with target-language markers. *)
let strip_markers kind lines =
  let lstrip_prefix s p =
    let lp = String.length p in
    if String.length s >= lp && String.sub s 0 lp = p
    then String.sub s lp (String.length s - lp) else s
  in
  List.map (fun line ->
    let trimmed_left =
      let n = String.length line in
      let i = ref 0 in
      while !i < n && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
      String.sub line !i (n - !i)
    in
    match kind with
    | `Vhdl -> lstrip_prefix trimmed_left "--" |> String.trim
    | `Verilog ->
        let s = trimmed_left in
        let s = lstrip_prefix s "//" in
        let s = lstrip_prefix s "/*" in
        let n = String.length s in
        let s = if n >= 2 && String.sub s (n-2) 2 = "*/"
                then String.sub s 0 (n-2) else s in
        String.trim s
  ) lines

(* `rewrap ~src ~dst lines` strips src markers from each header line
   and re-emits with dst markers.  E.g. src=`Vhdl, dst=`Verilog
   strips leading `-- ` and re-prefixes with `// `. *)
let rewrap ~src ~dst lines =
  let prefix = match dst with `Vhdl -> "-- " | `Verilog -> "// " in
  let bare = strip_markers src lines in
  String.concat "\n" (List.map (fun s ->
    if s = "" then "" else prefix ^ s) bare)
  ^ "\n"

let infer_kind path =
  let ext = String.lowercase_ascii (Filename.extension path) in
  match ext with
  | ".vhd" | ".vhdl" -> `Vhdl
  | ".v" | ".sv" -> `Verilog
  | _ ->
      Printf.eprintf "convert_hdl: cannot infer language from extension '%s'\n" ext;
      exit 2

let read_file p =
  let ic = open_in p in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  Bytes.unsafe_to_string buf

let () =
  let args = Array.to_list Sys.argv |> List.tl in
  let rec parse acc = function
    | [] -> acc
    | "-o" :: v :: rest -> parse (("output", v) :: acc) rest
    | "--top" :: v :: rest -> parse (("top", v) :: acc) rest
    | x :: rest when not (String.length x >= 1 && x.[0] = '-') ->
        parse (("input", x) :: acc) rest
    | x :: _ ->
        Printf.eprintf "convert_hdl: unknown arg %s\n" x;
        usage ()
  in
  let opts = parse [] args in
  let input = match List.assoc_opt "input" opts with
    | Some s -> s
    | None -> usage (); ""
  in
  let src_kind = infer_kind input in
  let dst_kind = match List.assoc_opt "output" opts with
    | Some o -> infer_kind o
    | None -> (match src_kind with `Vhdl -> `Verilog | `Verilog -> `Vhdl)
  in
  let output = match List.assoc_opt "output" opts with
    | Some o -> o
    | None ->
        let stem = Filename.remove_extension input in
        let ext = match dst_kind with `Vhdl -> ".vhd" | `Verilog -> ".v" in
        stem ^ "_converted" ^ ext
  in
  let top = List.assoc_opt "top" opts in

  let src = read_file input in
  let header_lines, _body = extract_header_lines src_kind src in
  let header = if header_lines = [] then "" else rewrap ~src:src_kind ~dst:dst_kind header_lines in

  (* Frontend → BIR *)
  let prog : Behavioral_ir.bprogram option = match src_kind with
    | `Vhdl -> Vhdl_to_behavioral.convert_vhdl_file_to_behavioral input
    | `Verilog ->
        (* When no --top is supplied, scan the source for the first
           `module <name>` declaration so the SV frontend's
           specialisation step doesn't drop the only module on the
           floor.  Falls back to file basename if no match. *)
        let auto_top () =
          let re = Str.regexp "^[ \t]*module[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)" in
          try
            let _ = Str.search_forward re src 0 in
            Str.matched_group 1 src
          with Not_found ->
            Filename.remove_extension (Filename.basename input)
        in
        let top = match top with Some t -> t | None -> auto_top () in
        (try Some (Verible_to_behavioral.convert_files ~top [input])
         with e ->
           Printf.eprintf "convert_hdl: verible frontend failed: %s\n"
             (Printexc.to_string e);
           None)
  in
  match prog with
  | None ->
      Printf.eprintf "convert_hdl: front-end conversion of %s failed\n" input;
      exit 1
  | Some p ->
      (match dst_kind with
       | `Vhdl -> Behavioral_to_vhdl.write_to_file ~header output p
       | `Verilog ->
           (* prepend header by writing it manually first, then append
              the verilog body — Behavioral_to_verilog.write_to_file
              has no header parameter. *)
           let body = Behavioral_to_verilog.verilog_of_program p in
           let oc = open_out output in
           if header <> "" then output_string oc header;
           output_string oc body;
           close_out oc);
      Printf.printf "wrote %s (%s → %s)\n" output
        (match src_kind with `Vhdl -> "VHDL" | `Verilog -> "Verilog")
        (match dst_kind with `Vhdl -> "VHDL" | `Verilog -> "Verilog")
