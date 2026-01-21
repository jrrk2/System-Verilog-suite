(* Complete round-trip transformation example *)
(* Behavioral RTL → Gate-level → Behavioral RTL *)

let create_behavioral_verilog () =
  {|module example (
  input a,
  input b,
  input c,
  input sel,
  output y,
  output z
);

  // Basic logic operations
  assign y = (a & b) | c;

  // Multiplexer
  assign z = sel ? a : b;

endmodule
|}

let create_gate_mapped_verilog () =
  {|module example (
  a,
  b,
  c,
  sel,
  y,
  z
);

  input a;
  input b;
  input c;
  input sel;
  output y;
  output z;

  wire w1, w2, w3, w4;

  // y = (a & b) | c
  AND2 u1 (.A1(a), .A2(b), .ZN(w1));
  OR2 u2 (.A1(w1), .A2(c), .ZN(y));

  // z = sel ? a : b
  AND2 u3 (.A1(sel), .A2(a), .ZN(w2));
  INV u4 (.I(sel), .ZN(w3));
  AND2 u5 (.A1(w3), .A2(b), .ZN(w4));
  OR2 u6 (.A1(w2), .A2(w4), .ZN(z));

endmodule
|}

let demonstrate_round_trip () =
  Printf.printf "=== Round-Trip Transformation Demo ===\n\n";

  (* Step 1: Create behavioral RTL *)
  Printf.printf "STEP 1: Original Behavioral RTL\n";
  Printf.printf "================================\n";
  let behavioral = create_behavioral_verilog () in
  Printf.printf "%s\n" behavioral;

  (* Write to file *)
  let oc = open_out "example_behavioral.v" in
  output_string oc behavioral;
  close_out oc;

  (* Step 2: Load Liberty file *)
  Printf.printf "\nSTEP 2: Load Liberty File\n";
  Printf.printf "==========================\n";
  let lib = Sv_liberty.parse_liberty_file "test_cells.lib" in
  Printf.printf "Loaded %d cells from test_cells.lib\n" (Hashtbl.length lib.cells);

  (* Show available cells *)
  Printf.printf "\nAvailable cells:\n";
  Hashtbl.iter (fun name cell ->
    Printf.printf "  %s: " name;
    let pins = List.map (fun p -> p.Sv_liberty.name) cell.Sv_liberty.pins in
    Printf.printf "pins(%s)\n" (String.concat ", " pins)
  ) lib.Sv_liberty.cells;

  (* Step 3: Map to gates (simulated) *)
  Printf.printf "\nSTEP 3: Map to Gate-Level Netlist\n";
  Printf.printf "==================================\n";
  let gate_mapped = create_gate_mapped_verilog () in
  Printf.printf "%s\n" gate_mapped;

  (* Write to file *)
  let oc = open_out "example_gates.v" in
  output_string oc gate_mapped;
  close_out oc;

  (* Step 4: Parse gate-level netlist *)
  Printf.printf "\nSTEP 4: Parse Gate-Level Netlist\n";
  Printf.printf "=================================\n";
  Printf.printf "Creating netlist structure...\n";

  (* Create mock netlist for demonstration *)
  let netlist : Sv_netlist_reader.netlist = {
    top_module = "example";
    net_inputs = [
      {sig_name = "a"; sig_width = 1};
      {sig_name = "b"; sig_width = 1};
      {sig_name = "c"; sig_width = 1};
      {sig_name = "sel"; sig_width = 1};
    ];
    net_outputs = [
      {sig_name = "y"; sig_width = 1};
      {sig_name = "z"; sig_width = 1};
    ];
    net_wires = [
      {sig_name = "w1"; sig_width = 1};
      {sig_name = "w2"; sig_width = 1};
      {sig_name = "w3"; sig_width = 1};
      {sig_name = "w4"; sig_width = 1};
    ];
    net_instances = [
      (* u1: AND2 for w1 = a & b *)
      {
        inst_id = "u1";
        cell_type = "AND2";
        conns = [
          {pin_name = "A1"; signal = {sig_name = "a"; sig_width = 1}};
          {pin_name = "A2"; signal = {sig_name = "b"; sig_width = 1}};
          {pin_name = "ZN"; signal = {sig_name = "w1"; sig_width = 1}};
        ];
      };
      (* u2: OR2 for y = w1 | c *)
      {
        inst_id = "u2";
        cell_type = "OR2";
        conns = [
          {pin_name = "A1"; signal = {sig_name = "w1"; sig_width = 1}};
          {pin_name = "A2"; signal = {sig_name = "c"; sig_width = 1}};
          {pin_name = "ZN"; signal = {sig_name = "y"; sig_width = 1}};
        ];
      };
      (* u3: AND2 for w2 = sel & a *)
      {
        inst_id = "u3";
        cell_type = "AND2";
        conns = [
          {pin_name = "A1"; signal = {sig_name = "sel"; sig_width = 1}};
          {pin_name = "A2"; signal = {sig_name = "a"; sig_width = 1}};
          {pin_name = "ZN"; signal = {sig_name = "w2"; sig_width = 1}};
        ];
      };
      (* u4: INV for w3 = !sel *)
      {
        inst_id = "u4";
        cell_type = "INV";
        conns = [
          {pin_name = "I"; signal = {sig_name = "sel"; sig_width = 1}};
          {pin_name = "ZN"; signal = {sig_name = "w3"; sig_width = 1}};
        ];
      };
      (* u5: AND2 for w4 = w3 & b *)
      {
        inst_id = "u5";
        cell_type = "AND2";
        conns = [
          {pin_name = "A1"; signal = {sig_name = "w3"; sig_width = 1}};
          {pin_name = "A2"; signal = {sig_name = "b"; sig_width = 1}};
          {pin_name = "ZN"; signal = {sig_name = "w4"; sig_width = 1}};
        ];
      };
      (* u6: OR2 for z = w2 | w4 *)
      {
        inst_id = "u6";
        cell_type = "OR2";
        conns = [
          {pin_name = "A1"; signal = {sig_name = "w2"; sig_width = 1}};
          {pin_name = "A2"; signal = {sig_name = "w4"; sig_width = 1}};
          {pin_name = "ZN"; signal = {sig_name = "z"; sig_width = 1}};
        ];
      };
    ];
  } in

  Printf.printf "Module: %s\n" netlist.top_module;
  Printf.printf "Inputs: %d, Outputs: %d, Wires: %d, Instances: %d\n"
    (List.length netlist.net_inputs)
    (List.length netlist.net_outputs)
    (List.length netlist.net_wires)
    (List.length netlist.net_instances);

  (* Step 5: Build expressions for outputs *)
  Printf.printf "\nSTEP 5: Build Expressions from Gates\n";
  Printf.printf "=====================================\n";

  List.iter (fun output ->
    Printf.printf "\n%s = " output.Sv_netlist_reader.sig_name;
    match Sv_netlist_reader.build_expr_for_signal lib netlist output.sig_name with
    | Some expr ->
        Printf.printf "%s\n" (Sv_netlist_reader.string_of_expr expr)
    | None ->
        Printf.printf "?\n"
  ) netlist.net_outputs;

  (* Step 6: Collapse to behavioral RTL *)
  Printf.printf "\nSTEP 6: Collapse to Behavioral RTL\n";
  Printf.printf "===================================\n";

  let rtl = Sv_rtl_collapse.collapse_netlist lib netlist in
  Printf.printf "\nRecognized operations:\n";
  List.iter (fun op ->
    Printf.printf "  %s\n" (Sv_rtl_collapse.verilog_of_rtl_operation op)
  ) rtl.rtl_ops;

  (* Generate behavioral Verilog *)
  let collapsed_verilog = Sv_rtl_collapse.verilog_of_rtl_module rtl in
  Printf.printf "\nGenerated Behavioral RTL:\n";
  Printf.printf "%s\n" collapsed_verilog;

  (* Write to file *)
  let oc = open_out "example_collapsed.v" in
  output_string oc collapsed_verilog;
  close_out oc;

  Printf.printf "\n=== Round-Trip Complete ===\n";
  Printf.printf "Files generated:\n";
  Printf.printf "  example_behavioral.v  - Original behavioral RTL\n";
  Printf.printf "  example_gates.v       - Gate-level netlist\n";
  Printf.printf "  example_collapsed.v   - Collapsed behavioral RTL\n"

let () =
  try
    demonstrate_round_trip ()
  with
  | Failure msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      exit 1
