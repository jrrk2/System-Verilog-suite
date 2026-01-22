(* Test VHDL Elaboration *)

let test_files = [
  "/Users/jonathan/gnusynthesis/vhd_front/slib_clock_div.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/slib_input_filter.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/slib_mv_filter.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/uart_baudgen.vhd";
]

let () =
  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  VHDL Elaboration Test\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";

  let passed = List.fold_left (fun count file ->
    let result = Vhdl_elaborate.analyze_file file in
    if result then count + 1 else count
  ) 0 test_files in

  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Results: %d/%d files analyzed successfully\n" passed (List.length test_files);
  Printf.printf "═══════════════════════════════════════════════════════════════\n"
