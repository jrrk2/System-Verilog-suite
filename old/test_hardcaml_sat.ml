(* Test HardCaml → Z3 SAT Equivalence Verification
 *
 * This combines the best of both worlds:
 * - HardCaml's type safety, normalization, and width resolution
 * - Z3's formal SAT proving for mathematical equivalence
 *
 * Expected improvements over direct Behavioral IR → Z3:
 * - No width inference bugs (HardCaml resolves all widths)
 * - No type mismatches (HardCaml type system enforces correctness)
 * - Cleaner encoding (HardCaml DAG is well-structured)
 * - Fewer false positives (normalization eliminates spurious differences)
 *)

let () =
  let (vhdl_file, sv_file) =
    if Array.length Sys.argv >= 3 then
      (Sys.argv.(1), Sys.argv.(2))
    else
      (* Default: test on module that had false positive before *)
      ("sysver_tests/slib_clock_div.vhd", "sysver_tests/slib_clock_div.sv")
  in

  (* Check files exist *)
  if not (Sys.file_exists vhdl_file) then begin
    Printf.eprintf "Error: VHDL file not found: %s\n" vhdl_file;
    exit 1
  end;

  if not (Sys.file_exists sv_file) then begin
    Printf.eprintf "Error: SV file not found: %s\n" sv_file;
    exit 1
  end;

  (* Run HardCaml → Z3 verification *)
  let result = Z3_hardcaml_miter.verify_hardcaml_equivalence vhdl_file sv_file in

  if result then begin
    Printf.printf "═══════════════════════════════════════════════════════════════\n";
    Printf.printf "  🎉 FORMAL EQUIVALENCE PROVEN\n";
    Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
    Printf.printf "HardCaml circuits are mathematically equivalent.\n";
    Printf.printf "Benefits of this approach:\n";
    Printf.printf "  ✅ HardCaml type system validated widths\n";
    Printf.printf "  ✅ Circuit normalization eliminated false differences\n";
    Printf.printf "  ✅ Z3 SAT solver provided formal proof\n\n";
    exit 0
  end else begin
    Printf.printf "═══════════════════════════════════════════════════════════════\n";
    Printf.printf "  Verification Result\n";
    Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
    Printf.printf "Check the output above for details.\n\n";
    exit 1
  end
