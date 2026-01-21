(* Test what the Verible OCaml parser actually produces *)

open Source_text_verible

(* Helper to print token structure *)
let rec print_token indent token =
  let spaces = String.make (indent * 2) ' ' in
  match token with
  | STRING s -> Printf.printf "%sSTRING \"%s\"\n" spaces s
  | SymbolIdentifier s -> Printf.printf "%sSymbolIdentifier \"%s\"\n" spaces s
  | TK_DecNumber s -> Printf.printf "%sTK_DecNumber \"%s\"\n" spaces s
  | TK_UnBasedNumber s -> Printf.printf "%sTK_UnBasedNumber \"%s\"\n" spaces s
  | PLUS -> Printf.printf "%sPLUS\n" spaces
  | HYPHEN -> Printf.printf "%sHYPHEN\n" spaces
  | STAR -> Printf.printf "%sSTAR\n" spaces
  | SLASH -> Printf.printf "%sSLASH\n" spaces
  | LBRACK -> Printf.printf "%sLBRACK\n" spaces
  | RBRACK -> Printf.printf "%sRBRACK\n" spaces
  | COLON -> Printf.printf "%sCOLON\n" spaces
  | COMMA -> Printf.printf "%sCOMMA\n" spaces
  | SEMICOLON -> Printf.printf "%sSEMICOLON\n" spaces
  | EQUALS -> Printf.printf "%sEQUALS\n" spaces
  | LPAREN -> Printf.printf "%sLPAREN\n" spaces
  | RPAREN -> Printf.printf "%sRPAREN\n" spaces
  | Input -> Printf.printf "%sInput\n" spaces
  | Output -> Printf.printf "%sOutput\n" spaces
  | Module -> Printf.printf "%sModule\n" spaces
  | Endmodule -> Printf.printf "%sEndmodule\n" spaces
  | Assign -> Printf.printf "%sAssign\n" spaces
  | Parameter -> Printf.printf "%sParameter\n" spaces

  | TLIST lst ->
      Printf.printf "%sTLIST [\n" spaces;
      List.iter (print_token (indent + 1)) lst;
      Printf.printf "%s]\n" spaces

  | TUPLE2 (a, b) ->
      Printf.printf "%sTUPLE2 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      Printf.printf "%s)\n" spaces

  | TUPLE3 (a, b, c) ->
      Printf.printf "%sTUPLE3 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      Printf.printf "%s)\n" spaces

  | TUPLE4 (a, b, c, d) ->
      Printf.printf "%sTUPLE4 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      Printf.printf "%s)\n" spaces

  | TUPLE5 (a, b, c, d, e) ->
      Printf.printf "%sTUPLE5 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      Printf.printf "%s)\n" spaces

  | TUPLE6 (a, b, c, d, e, f) ->
      Printf.printf "%sTUPLE6 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      Printf.printf "%s)\n" spaces

  | TUPLE7 (a, b, c, d, e, f, g) ->
      Printf.printf "%sTUPLE7 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      Printf.printf "%s)\n" spaces

  | TUPLE8 (a, b, c, d, e, f, g, h) ->
      Printf.printf "%sTUPLE8 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      Printf.printf "%s)\n" spaces

  | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
      Printf.printf "%sTUPLE9 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      print_token (indent + 1) i;
      Printf.printf "%s)\n" spaces

  | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
      Printf.printf "%sTUPLE10 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      print_token (indent + 1) i;
      print_token (indent + 1) j;
      Printf.printf "%s)\n" spaces

  | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
      Printf.printf "%sTUPLE11 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      print_token (indent + 1) i;
      print_token (indent + 1) j;
      print_token (indent + 1) k;
      Printf.printf "%s)\n" spaces

  | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
      Printf.printf "%sTUPLE12 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      print_token (indent + 1) i;
      print_token (indent + 1) j;
      print_token (indent + 1) k;
      print_token (indent + 1) l;
      Printf.printf "%s)\n" spaces

  | CONS1 a ->
      Printf.printf "%sCONS1 (\n" spaces;
      print_token (indent + 1) a;
      Printf.printf "%s)\n" spaces

  | CONS2 (a, b) ->
      Printf.printf "%sCONS2 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      Printf.printf "%s)\n" spaces

  | CONS3 (a, b, c) ->
      Printf.printf "%sCONS3 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      Printf.printf "%s)\n" spaces

  | CONS4 (a, b, c, d) ->
      Printf.printf "%sCONS4 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      Printf.printf "%s)\n" spaces

  | CONS5 (a, b, c, d, e) ->
      Printf.printf "%sCONS5 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      Printf.printf "%s)\n" spaces

  | CONS6 (a, b, c, d, e, f) ->
      Printf.printf "%sCONS6 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      Printf.printf "%s)\n" spaces

  | CONS7 (a, b, c, d, e, f, g) ->
      Printf.printf "%sCONS7 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      Printf.printf "%s)\n" spaces

  | CONS8 (a, b, c, d, e, f, g, h) ->
      Printf.printf "%sCONS8 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      Printf.printf "%s)\n" spaces

  | CONS9 (a, b, c, d, e, f, g, h, i) ->
      Printf.printf "%sCONS9 (\n" spaces;
      print_token (indent + 1) a;
      print_token (indent + 1) b;
      print_token (indent + 1) c;
      print_token (indent + 1) d;
      print_token (indent + 1) e;
      print_token (indent + 1) f;
      print_token (indent + 1) g;
      print_token (indent + 1) h;
      print_token (indent + 1) i;
      Printf.printf "%s)\n" spaces

  | End_of_file -> Printf.printf "%sEnd_of_file\n" spaces
  | EMPTY_TOKEN -> Printf.printf "%sEMPTY_TOKEN\n" spaces
  | _ -> Printf.printf "%s<other token>\n" spaces

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "Usage: %s <verilog_file>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  Printf.printf "=== Parsing %s with Verible OCaml Parser ===\n\n" filename;

  (* Use the proper parser with deflated lexer *)
  match Sv_verible_to_ir.parse_verible_file filename with
  | None ->
      Printf.eprintf "✗ Failed to parse\n";
      exit 1
  | Some parse_tree ->
      Printf.printf "✓ Parsed successfully!\n\n";
      Printf.printf "Parse tree structure:\n";
      print_token 0 parse_tree;
      Printf.printf "\nDone!\n"
