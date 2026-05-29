(* Test VHDL AST Dumper *)

let test_files = [
  "/Users/jonathan/gnusynthesis/vhd_front/slib_clock_div.vhd";
  "/Users/jonathan/gnusynthesis/vhd_front/slib_input_filter.vhd";
]

let () =
  List.iter (fun file ->
    let _ = Vhdl_dump.dump_design_file file in
    Printf.printf "\n"
  ) test_files
