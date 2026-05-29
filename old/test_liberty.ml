(* Test program for liberty file parser *)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <liberty_file>\n" Sys.argv.(0);
    exit 1
  end;

  let lib_file = Sys.argv.(1) in
  Printf.printf "Parsing liberty file: %s\n" lib_file;

  try
    let lib = Sv_liberty.parse_liberty_file lib_file in
    Printf.printf "\nLibrary Summary:\n";
    Printf.printf "================\n";
    Printf.printf "Library name: %s\n" lib.lib_name;
    Printf.printf "Total cells: %d\n\n" (Hashtbl.length lib.cells);

    (* Print first 10 cells *)
    let count = ref 0 in
    Hashtbl.iter (fun name cell ->
      if !count < 10 then begin
        Sv_liberty.print_cell_info cell;
        Printf.printf "\n";
        incr count
      end
    ) lib.cells;

    (* Test specific cell lookup *)
    Printf.printf "Testing cell lookup:\n";
    Printf.printf "====================\n";
    (match Sv_liberty.get_cell lib "AND2_X1" with
     | Some cell ->
         Printf.printf "Found AND2_X1:\n";
         Sv_liberty.print_cell_info cell
     | None ->
         Printf.printf "AND2_X1 not found\n");
    Printf.printf "\n";

    (match Sv_liberty.get_cell lib "OR2_X1" with
     | Some cell ->
         Printf.printf "Found OR2_X1:\n";
         Sv_liberty.print_cell_info cell
     | None ->
         Printf.printf "OR2_X1 not found\n");

  with
  | Failure msg -> Printf.eprintf "Error: %s\n" msg; exit 1
  | e -> Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e); exit 1
