(* Smoke test: tokenise a LEF file end-to-end, count tokens, report
   how far the lexer got.  Token kind histogram is by token tag, using
   the lexer's verbose-mode printer (set `verbose := true` to dump
   every token). *)

open Lef_def

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "nangate.lef" in
  let ch = open_in path in
  let lb = Lexing.from_channel ch in
  let total = ref 0 in
  let last_kind = ref "?" in
  let kind_of t =
    let module L = Lef_file in
    match t with
    | L.STAR -> "STAR"  | L.PLUS -> "PLUS"  | L.HYPHEN -> "HYPHEN"
    | L.SLASH -> "SLASH" | L.SEMICOLON -> "SEMI" | L.LESS -> "LESS"
    | L.EQUALS -> "EQ"  | L.GREATER -> "GT"  | L.LPAREN -> "LP"
    | L.RPAREN -> "RP"  | L.LBRACK -> "LBK" | L.RBRACK -> "RBK"
    | L.LBRACE -> "LBC" | L.RBRACE -> "RBC" | L.COMMA -> "COMMA"
    | L.QSTRING _ -> "QSTRING" | L.NUMBER _ -> "NUMBER"
    | L.EOF_TOKEN -> "EOF"
    | _ -> "K_/etc"
  in
  (try
     while true do
       let t = Lef_file_lex.token lb in
       incr total;
       last_kind := kind_of t;
       if t = Lef_file.EOF_TOKEN then raise End_of_file
     done
   with End_of_file -> ()
      | e ->
        Printf.eprintf "Lex stopped at token #%d (last=%s), line ~%d: %s\n"
          !total !last_kind !Lef_file_lex.lincnt (Printexc.to_string e));
  close_in ch;
  Printf.printf "Tokens: %d   Lines walked: %d\n" !total !Lef_file_lex.lincnt
