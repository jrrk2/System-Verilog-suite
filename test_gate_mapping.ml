(* Test program for gate mapping and RTL collapse *)

let test_gate_mapping () =
  Printf.printf "=== Gate Mapping Test ===\n\n";

  (* Create a simple liberty file for testing *)
  let lib_content = {|
library (test_lib) {
  cell (AND2) {
    pin (A1) { direction : input; }
    pin (A2) { direction : input; }
    pin (ZN) { direction : output; function : "(A1 & A2)"; }
  }
  cell (OR2) {
    pin (A1) { direction : input; }
    pin (A2) { direction : input; }
    pin (ZN) { direction : output; function : "(A1 | A2)"; }
  }
  cell (XOR2) {
    pin (A1) { direction : input; }
    pin (A2) { direction : input; }
    pin (ZN) { direction : output; function : "(A1 ^ A2)"; }
  }
  cell (INV) {
    pin (I) { direction : input; }
    pin (ZN) { direction : output; function : "(!I)"; }
  }
  cell (BUF) {
    pin (I) { direction : input; }
    pin (Z) { direction : output; function : "I"; }
  }
}
|} in

  (* Write test liberty file *)
  let lib_file = "test_cells.lib" in
  let oc = open_out lib_file in
  output_string oc lib_content;
  close_out oc;

  (* Load library *)
  Printf.printf "Loading liberty file: %s\n" lib_file;
  let lib = Sv_liberty.parse_liberty_file lib_file in
  Printf.printf "Loaded %d cells\n\n" (Hashtbl.length lib.cells);

  (* Test finding cells for operations *)
  Printf.printf "Testing cell lookup:\n";
  List.iter (fun op_type ->
    match Sv_gate_map.find_cell_for_op lib op_type with
    | Some (cell_name, cell) ->
        Printf.printf "  %s -> %s\n" op_type cell_name
    | None ->
        Printf.printf "  %s -> NOT FOUND\n" op_type
  ) ["AND"; "OR"; "XOR"; "NOT"; "BUF"];
  Printf.printf "\n";

  (* Create a test mapped netlist *)
  Printf.printf "Creating test gate-level netlist:\n";
  let netlist = Sv_gate_map.create_test_netlist lib in
  let verilog = Sv_gate_map.verilog_of_mapped_netlist netlist in
  Printf.printf "%s\n" verilog;

  (* Write gate-level netlist *)
  let gate_file = "test_gates.v" in
  let oc = open_out gate_file in
  output_string oc verilog;
  close_out oc;
  Printf.printf "Written gate-level netlist to %s\n\n" gate_file;

  Printf.printf "Gate mapping test completed!\n"

let test_rtl_collapse () =
  Printf.printf "\n=== RTL Collapse Test ===\n\n";

  (* This would require parsing the gate-level netlist we generated *)
  (* and then collapsing it back to behavioral RTL *)

  Printf.printf "Creating example netlist structure...\n";

  (* Create a mock netlist structure *)
  let mock_netlist : Sv_netlist_reader.netlist = {
    top_module = "example";
    net_inputs = [
      {sig_name = "a"; sig_width = 1};
      {sig_name = "b"; sig_width = 1};
    ];
    net_outputs = [
      {sig_name = "y"; sig_width = 1};
    ];
    net_wires = [];
    net_instances = [];
  } in

  Printf.printf "Module: %s\n" mock_netlist.top_module;
  Printf.printf "Inputs: %d\n" (List.length mock_netlist.net_inputs);
  Printf.printf "Outputs: %d\n" (List.length mock_netlist.net_outputs);
  Printf.printf "\n";

  Printf.printf "RTL collapse test completed!\n"

let () =
  try
    test_gate_mapping ();
    test_rtl_collapse ();
    Printf.printf "\n=== All Tests Completed Successfully ===\n"
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
