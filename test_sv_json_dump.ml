(* test_sv_json_dump.ml - Test JSON dumping of SystemVerilog patterns *)

open Source_text_verible_json
open Sv_dump_json

let () =
  Printf.printf "=== Testing SystemVerilog JSON Dumping ===\n\n";

  (* Create some test tokens *)
  let test_tokens = [
    ("simple_identifier", SymbolIdentifier "test_signal");
    ("string_literal", STRING "module_name");
    ("tuple2", TUPLE2 (STRING "test", SymbolIdentifier "foo"));
    ("tuple3", TUPLE3 (STRING "binary_add", SymbolIdentifier "a", SymbolIdentifier "b"));
    ("list", TLIST [SymbolIdentifier "x"; SymbolIdentifier "y"; SymbolIdentifier "z"]);
  ] in

  (* Dump each test pattern *)
  List.iter (fun (name, token) ->
    Printf.printf "Dumping %s pattern...\n" name;
    dump_unhandled "test" name token;
    Printf.printf "\n";
  ) test_tokens;

  (* Create summary *)
  Printf.printf "\n";
  create_summary ()
