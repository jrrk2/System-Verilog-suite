(* Test Z3 Miter-Based Formal Equivalence Checking
 *
 * This test verifies VHDL ≡ SystemVerilog equivalence using a miter circuit
 * and Z3 SAT solving.
 *)

let () =
  let (vhdl_file, sv_file) =
    if Array.length Sys.argv >= 3 then
      (Sys.argv.(1), Sys.argv.(2))
    else
      (* Default test case *)
      ("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")
  in

  (* Check files exist *)
  if not (Sys.file_exists vhdl_file) then begin
    Printf.eprintf "Error: VHDL file not found: %s\n" vhdl_file;
    exit 1
  end;

  if not (Sys.file_exists sv_file) then begin
    Printf.eprintf "Error: SystemVerilog file not found: %s\n" sv_file;
    exit 1
  end;

  (* Run miter equivalence check *)
  let result = Z3_miter.verify_equivalence vhdl_file sv_file in

  (* Print final result *)
  if result then begin
    Printf.printf "═══════════════════════════════════════════════════════════════\n";
    Printf.printf "  🎉 VERIFICATION SUCCESS\n";
    Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
    Printf.printf "The VHDL and SystemVerilog designs are formally equivalent!\n";
    Printf.printf "Z3 proved no counterexample exists. ✅\n\n";
    exit 0
  end else begin
    Printf.printf "═══════════════════════════════════════════════════════════════\n";
    Printf.printf "  ❌ VERIFICATION FAILED\n";
    Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
    Printf.printf "The designs are NOT equivalent or verification incomplete.\n";
    Printf.printf "Check the counterexample or timeout logs above.\n\n";
    exit 1
  end
