open Source_text_verible
open Source_text_verible_lex
open Source_text_verible_tokens

let () =
  let filename = "sysver_tests/slib_clock_div.sv" in
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  
  try
    let deflated_lexer = deflate token in
    let ast = Source_text_verible.ml_start deflated_lexer lexbuf in
    close_in ic;
    
    (* Find module and port list *)
    let rec find_ports token =
      match token with
      | TUPLE9 (STRING "module_declaration1", _, _, TUPLE3 (STRING "module_header1", _, port_list), _, _, _, _, _) ->
          Printf.printf "Found module with port list:\n";
          (match port_list with
           | TLIST items ->
               Printf.printf "Port list has %d items\n" (List.length items);
               List.iteri (fun i item ->
                 Printf.printf "\nPort %d: %s\n" i (getstr item |> String.sub 0 (min 80 (String.length (getstr item))))
               ) items
           | _ -> Printf.printf "Port list is not TLIST\n")
      | TLIST items -> List.iter find_ports items
      | TUPLE2 (a, b) -> find_ports a; find_ports b
      | TUPLE3 (a, b, c) -> find_ports a; find_ports b; find_ports c
      | TUPLE4 (a, b, c, d) -> find_ports a; find_ports b; find_ports c; find_ports d
      | TUPLE5 (a, b, c, d, e) -> find_ports a; find_ports b; find_ports c; find_ports d; find_ports e
      | TUPLE6 (a, b, c, d, e, f) -> find_ports a; find_ports b; find_ports c; find_ports d; find_ports e; find_ports f
      | TUPLE7 (a, b, c, d, e, f, g) -> find_ports a; find_ports b; find_ports c; find_ports d; find_ports e; find_ports f; find_ports g
      | TUPLE8 (a, b, c, d, e, f, g, h) -> find_ports a; find_ports b; find_ports c; find_ports d; find_ports e; find_ports f; find_ports g; find_ports h
      | TUPLE9 (a, b, c, d, e, f, g, h, i) -> find_ports a; find_ports b; find_ports c; find_ports d; find_ports e; find_ports f; find_ports g; find_ports h; find_ports i
      | _ -> ()
    in
    find_ports ast
  with e ->
    close_in ic;
    Printf.eprintf "Error: %s\n" (Printexc.to_string e)
