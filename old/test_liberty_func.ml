(* Smoke-test for the Liberty function-expression parser.
 *
 * Reads simcells.lib (or any other Liberty), iterates every
 * combinational cell, parses its `function:` string into a BIR
 * bexpr, and prints the round-trip rendering. A second pass
 * exercises a hand-curated set of expressions against expected
 * shapes so a regression in the parser surfaces immediately. *)

open Behavioral_ir

let () =
  let lib_file =
    if Array.length Sys.argv >= 2 then Sys.argv.(1)
    else "/home/jonathan/hardcaml-lua.0.0.1/liberty/simcells.lib" in
  if not (Sys.file_exists lib_file) then begin
    Printf.eprintf "Liberty file not found: %s\n" lib_file;
    exit 2
  end;

  Printf.printf "Loading %s\n" lib_file;
  let lib = Sv_liberty.parse_liberty_file lib_file in
  Printf.printf "  %d cells\n\n" (Hashtbl.length lib.cells);

  let comb = ref 0 and ff = ref 0 and parsed = ref 0 and failed = ref 0 in
  Hashtbl.iter (fun _name (cell : Sv_liberty.cell_info) ->
    (match cell.cell_type with
     | "ff" -> incr ff
     | _ -> incr comb);
    List.iter (fun (pin : Sv_liberty.pin_info) ->
      match pin.function_expr with
      | None -> ()
      | Some fexpr ->
          (try
            let bexpr = Sv_liberty.parse_function_to_bexpr [] fexpr in
            ignore (string_of_bexpr bexpr);
            incr parsed
          with e ->
            incr failed;
            Printf.printf "  ✗ %s.%s = %S → %s\n"
              cell.cell_name pin.name fexpr (Printexc.to_string e))
    ) cell.pins
  ) lib.cells;

  Printf.printf "  combinational cells: %d\n" !comb;
  Printf.printf "  ff cells:           %d\n" !ff;
  Printf.printf "  function exprs ok:  %d\n" !parsed;
  Printf.printf "  function exprs err: %d\n\n" !failed;

  Printf.printf "Curated round-trip checks:\n";
  let curated = [
    "(A*B)";              (* AND   *)
    "(A+B)";              (* OR    *)
    "(A^B)";              (* XOR   *)
    "(A)'";               (* NOT postfix *)
    "(((A*B)+C))'";       (* AOI3 *)
    "(((A*B)+(C*D)))'";   (* AOI4 *)
    "(A*(B)')";           (* ANDNOT *)
    "A";                  (* BUF *)
    "IQ";                 (* FF state *)
  ] in
  List.iter (fun s ->
    try
      let e = Sv_liberty.parse_function_to_bexpr [] s in
      Printf.printf "  %-22s ⇒ %s\n" s (string_of_bexpr e)
    with e ->
      Printf.printf "  %-22s ⇒ ERROR %s\n" s (Printexc.to_string e)
  ) curated;

  (* Substitution test: feed an input_map so identifiers are
   * resolved into specific bexprs. *)
  Printf.printf "\nSubstitution check (A→a_net, B→b_net):\n";
  let m = [ ("A", BVar "a_net"); ("B", BVar "b_net") ] in
  let s = "(A*B)+(A^B)" in
  let e = Sv_liberty.parse_function_to_bexpr m s in
  Printf.printf "  %s\n  ⇒ %s\n" s (string_of_bexpr e);

  if !failed > 0 then exit 1
