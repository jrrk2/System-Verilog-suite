(* Smoke test: tokenise a tiny DEF and dump K_*/non-K tokens. *)
open Lef_def

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "sample.def" in
  let ch = open_in path in
  let lb = Lexing.from_channel ch in
  let n = ref 0 in
  let rec loop () =
    let t = Def_file_lex.token lb in
    incr n;
    let extra = match t with
      | Def_file.NUMBER i  -> Printf.sprintf " = %d" i
      | Def_file.STRING s  -> Printf.sprintf " = %S" s
      | Def_file.QSTRING s -> Printf.sprintf " = %S" s
      | _ -> ""
    in
    Printf.printf "%3d  %s%s\n" !n (Def_file_tokens.getstr t) extra;
    if t <> Def_file.EOF_TOKEN && !n < 200 then loop ()
  in
  loop ();
  close_in ch
