(* Test assignment ordering in MUX tree generation *)

let extract_module_to_temp module_name =
  (* Extract the specific module from the multi-module file *)
  let input_file = "test_assignment_order.sv" in
  let temp_file = Printf.sprintf "/tmp/%s.sv" module_name in

  let ic = open_in input_file in
  let oc = open_out temp_file in

  let in_module = ref false in
  let module_pattern = Str.regexp (Printf.sprintf "^module %s" module_name) in
  let endmodule_pattern = Str.regexp "^endmodule" in

  try
    while true do
      let line = input_line ic in
      if Str.string_match module_pattern line 0 then begin
        in_module := true;
        output_string oc (line ^ "\n")
      end else if !in_module then begin
        output_string oc (line ^ "\n");
        if Str.string_match endmodule_pattern line 0 then begin
          in_module := false;
          raise Exit
        end
      end
    done;
    close_in ic;
    close_out oc;
    temp_file
  with
  | Exit -> close_in ic; close_out oc; temp_file
  | End_of_file -> close_in ic; close_out oc; temp_file

let test_module module_name test_cases =
  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Testing: %s\n" module_name;
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  try
    (* Extract module to temp file *)
    let sv_file = extract_module_to_temp module_name in
    let json_file = Printf.sprintf "sysver_tests/obj_dir/V%s.tree.json" module_name in

    Printf.printf "[1/3] Generating Verilator IR...\n";
    let cmd = Printf.sprintf "verilator --json-only --sv -Wno-fatal --top-module %s %s -Mdir sysver_tests/obj_dir 2>&1 > /dev/null"
      module_name sv_file in
    let status = Sys.command cmd in
    if status <> 0 then begin
      Printf.printf "  ❌ Verilator failed\n";
      false
    end else begin
      (* Load Verilator IR *)
      let json = Yojson.Safe.from_file json_file in
      let ast = Sv_parse.parse json in
      let verilator_ir = Behavioural_to_opt_ir.convert ~verbose:false ast in
      Printf.printf "  ✓ Verilator IR loaded (nodes: %d)\n"
        (Hashtbl.length verilator_ir.Sv_ast.ir_nodes);

      (* Load Verible IR *)
      Printf.printf "\n[2/3] Generating Verible IR...\n";
      match Sv_verible_to_ir.file_to_ir sv_file with
      | None ->
          Printf.printf "  ❌ Verible parsing failed\n";
          false
      | Some verible_ir ->
          Printf.printf "  ✓ Verible IR loaded (nodes: %d)\n"
            (Hashtbl.length verible_ir.Sv_ast.ir_nodes);

          (* Run Z3 verification *)
          Printf.printf "\n[3/3] Running Z3 verification...\n";
          let result = Sv_ir_verify.verify_ir_equivalence verilator_ir verible_ir in

          if result then begin
            Printf.printf "  ✅ PASS: IRs are equivalent\n";
            true
          end else begin
            Printf.printf "  ❌ FAIL: IRs are NOT equivalent\n";
            false
          end
    end
  with e ->
    Printf.printf "  ❌ Exception: %s\n" (Printexc.to_string e);
    false

let () =
  Printf.printf "\n";
  Printf.printf "╔════════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║  Assignment Ordering Test Suite                               ║\n";
  Printf.printf "║  Verifies correct chronological ordering in MUX trees         ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════════╝\n";

  let tests = [
    ("test_unconditional_then_conditional", [
      "Tests: unconditional default followed by conditional override";
      "Pattern: iQ <= default; if (cond) iQ <= override";
      "Bug: Without fix, conditional might have lower priority than default";
    ]);
    ("test_multiple_conditionals", [
      "Tests: multiple sequential conditional assignments";
      "Pattern: if (c1) q<=v1; if (c2) q<=v2; if (c3) q<=v3";
      "Bug: Without fix, priority order could be reversed";
    ]);
    ("test_sequential_ifs", [
      "Tests: non-nested sequential if statements";
      "Pattern: q<=default; if (A) q<=vA; if (B) q<=vB";
      "Bug: B should win when both A and B are true (later assignment)";
    ]);
    ("test_nested_with_outer_unconditional", [
      "Tests: nested if-else with outer unconditional";
      "Pattern: q<=default; if (en) { if (sel) q<=v1 else q<=v2 }";
      "Bug: Extra List.rev would swap v1 and v2 priority";
    ]);
    ("test_clock_div_pattern", [
      "Tests: actual slib_clock_div pattern (simplified)";
      "Pattern: q<=0; if (CE && AT_MAX) q<=1";
      "Bug: This was failing before the ordering fix";
    ]);
    ("test_overlapping_conditions", [
      "Tests: overlapping conditions where order matters";
      "Pattern: if (A) q<=v1; if (A&&B) q<=v2";
      "Bug: When A=1,B=1, v2 should win (more specific, later)";
    ]);
  ] in

  let results = List.map (fun (name, description) ->
    List.iter (fun line -> Printf.printf "  %s\n" line) description;
    Printf.printf "\n";
    let pass = test_module name description in
    (name, pass)
  ) tests in

  Printf.printf "\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Summary\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";

  let passed = List.filter snd results |> List.length in
  let total = List.length results in

  List.iter (fun (name, pass) ->
    Printf.printf "  %s %s\n" (if pass then "✅" else "❌") name
  ) results;

  Printf.printf "\n  Result: %d/%d tests passed (%.0f%%)\n\n"
    passed total (float_of_int passed /. float_of_int total *. 100.0);

  exit (if passed = total then 0 else 1)
