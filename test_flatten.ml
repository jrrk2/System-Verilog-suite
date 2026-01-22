open Source_text_verible

let () =
  let test = CONS2 (CONS1 (SymbolIdentifier "a"), SymbolIdentifier "b") in
  let result = Sv_elaborate.flatten_cons test in
  match result with
  | TLIST lst ->
      Printf.printf "Flattened to TLIST with %d elements\n" (List.length lst)
  | _ -> Printf.printf "Did not flatten to TLIST\n"
