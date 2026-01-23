(* Test JSON dumping *)

open Vhd_front.VhdlTree

let () =
  Printf.printf "Testing JSON dump...\n";

  (* Create a simple vhdintf structure *)
  let test_vhd = Triple (Str "VhdTest", Str "field1", Num "42") in

  (* Dump it *)
  Vhdl_dump_json.dump_unhandled "test_context" "test_pattern" test_vhd;

  Printf.printf "Test complete - check for unhandled_test_pattern.json\n"
