(* Test program for RTLIL reader *)

let create_test_rtlil () =
  {|# Test RTLIL file
autoidx 1
attribute \top 1

module \test_and
  wire width 1 input 1 \a
  wire width 1 input 2 \b
  wire width 1 output 3 \y

  cell $_AND_ $and_inst
    connect \A \a
    connect \B \b
    connect \Y \y
  end
end

module \test_add
  wire width 4 input 1 \a
  wire width 4 input 2 \b
  wire width 4 output 3 \sum

  cell $add $add_inst
    parameter \A_WIDTH 4
    parameter \B_WIDTH 4
    parameter \Y_WIDTH 4
    connect \A \a
    connect \B \b
    connect \Y \sum
  end
end

module \test_dff
  wire input 1 \clk
  wire input 2 \d
  wire output 3 \q

  cell $_DFF_P_ $ff_inst
    connect \C \clk
    connect \D \d
    connect \Q \q
  end
end
|}

let () =
  Printf.printf "=== RTLIL Reader Test ===\n\n";

  (* Create test RTLIL file *)
  let test_file = "test_design.il" in
  let oc = open_out test_file in
  output_string oc (create_test_rtlil ());
  close_out oc;
  Printf.printf "Created test file: %s\n\n" test_file;

  (* Parse RTLIL *)
  Printf.printf "Parsing RTLIL file...\n";
  let design = Sv_rtlil_reader.parse_rtlil_file test_file in
  Printf.printf "Parsing complete!\n\n";

  (* Print summary *)
  Sv_rtlil_reader.print_rtlil_summary design;

  (* Test parsing real RTLIL file if available *)
  let gold_file = "/Users/jonathan/hardcaml-lua/blocking_add/gold.il" in
  if Sys.file_exists gold_file then begin
    Printf.printf "\n=== Testing with Real Yosys Output ===\n\n";
    Printf.printf "Parsing: %s\n" gold_file;
    let real_design = Sv_rtlil_reader.parse_rtlil_file gold_file in
    Sv_rtlil_reader.print_rtlil_summary real_design
  end else begin
    Printf.printf "\nNote: Real Yosys file not found at %s\n" gold_file
  end;

  Printf.printf "\n=== Test Complete ===\n"
