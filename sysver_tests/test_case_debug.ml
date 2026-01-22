(* Debug case statement parsing *)
let () =
  let filename = "test_11_always_comb_case.sv" in
  match Sv_verible_to_ir.parse_verible_file filename with
  | None -> Printf.eprintf "Failed to parse\n"
  | Some ast ->
      let rec find_case token =
        match token with
        | Source_text_verible.TUPLE8 (Source_text_verible.STRING "case_statement1", _, _, _, _, _, items, _) ->
            Printf.printf "Found case statement!\n";
            Printf.printf "Case items structure:\n";
            let rec print_token indent t =
              let spaces = String.make (indent * 2) ' ' in
              match t with
              | Source_text_verible.TUPLE4 (Source_text_verible.STRING s, a, b, c) ->
                  Printf.printf "%sTUPLE4(%s, ...)\n" spaces s;
                  print_token (indent+1) a;
                  print_token (indent+1) b;
                  print_token (indent+1) c
              | Source_text_verible.TUPLE3 (Source_text_verible.STRING s, a, b) ->
                  Printf.printf "%sTUPLE3(%s, ...)\n" spaces s;
                  print_token (indent+1) a;
                  print_token (indent+1) b
              | Source_text_verible.TK_DecNumber n -> Printf.printf "%sTK_DecNumber(%s)\n" spaces n
              | Source_text_verible.SymbolIdentifier n -> Printf.printf "%sSymbolIdentifier(%s)\n" spaces n
              | Source_text_verible.TLIST lst ->
                  Printf.printf "%sTLIST[%d items]\n" spaces (List.length lst);
                  List.iter (print_token (indent+1)) lst
              | _ -> Printf.printf "%s<other>\n" spaces
            in
            print_token 0 items
        | Source_text_verible.TUPLE3 (_, a, b) ->
            find_case a; find_case b
        | Source_text_verible.TUPLE4 (_, a, b, c, d) ->
            find_case a; find_case b; find_case c; find_case d
        | Source_text_verible.TUPLE8 (_, a, b, c, d, e, f, g, h) ->
            find_case a; find_case b; find_case c; find_case d;
            find_case e; find_case f; find_case g; find_case h
        | Source_text_verible.TUPLE12 (_, a, b, c, d, e, f, g, h, i, j, k, l) ->
            find_case a; find_case b; find_case c; find_case d;
            find_case e; find_case f; find_case g; find_case h;
            find_case i; find_case j; find_case k; find_case l
        | Source_text_verible.TLIST lst -> List.iter find_case lst
        | _ -> ()
      in
      find_case ast
