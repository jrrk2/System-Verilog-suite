(* VHDL Parser Integration *)

(* Parse a VHDL file and return the AST *)
let parse_vhdl_file filename =
  let chan = open_in filename in
  let lexbuf = Lexing.from_channel chan in
  try
    let ast = VhdlParser.top_level_file VhdlLexer.lexer lexbuf in
    close_in chan;
    Some ast
  with
  | Parsing.Parse_error ->
      close_in chan;
      Printf.eprintf "Parse error in file %s\n" filename;
      None
  | e ->
      close_in chan;
      Printf.eprintf "Error parsing %s: %s\n" filename (Printexc.to_string e);
      None

(* Simple test function *)
let test_parse filename =
  Printf.printf "Parsing VHDL file: %s\n" filename;
  match parse_vhdl_file filename with
  | Some ast ->
      Printf.printf "✅ Successfully parsed %s\n" filename;
      Printf.printf "   AST has %d design units\n" (List.length ast);
      true
  | None ->
      Printf.printf "❌ Failed to parse %s\n" filename;
      false
