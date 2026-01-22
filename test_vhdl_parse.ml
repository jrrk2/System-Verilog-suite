(* Test VHDL parser integration *)

let test_files = [
  "/Users/jonathan/gnusynthesis/vhd_front/slib_clock_div.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/slib_input_filter.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/slib_mv_filter.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/uart_baudgen.vhd";
]

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL Parser Integration Test\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let total = List.length test_files in
  let passed = List.fold_left (fun count file ->
    let result = Vhdl_parse.test_parse file in
    Printf.printf "\n";
    if result then count + 1 else count
  ) 0 test_files in

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Results: %d/%d files parsed successfully\n" passed total;
  Printf.printf "═══════════════════════════════════════════════════════════════\n"
