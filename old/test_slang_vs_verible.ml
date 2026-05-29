(* Slang AST-JSON ↔ Verible parse-tree miter.
 *
 * Both paths take SV source as input and produce a Behavioral_ir
 * bmodule. Slang elaborates via its own front-end (independent of
 * Yosys / Yosys-slang); the comparison is "did Verible understand
 * the source the same way Slang did?". Pre-synth, no vendor
 * primitives, no Xilinx-specific tricks.
 *
 * Usage:
 *   test_slang_vs_verible <top> <file.sv> [more.sv ...]
 *
 * Optional env vars:
 *   BIR_DUMP=1 — print both BIRs side-by-side before the miter. *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Slang ↔ Verible miter: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] Slang → BIR ...\n%!";
  let slang_prog =
    match Slang_to_behavioral.convert_files ~top files with
    | Some p -> p
    | None -> Printf.eprintf "Slang side load failed\n"; exit 1
  in
  Printf.printf "  %d modules\n" (List.length slang_prog.modules);

  Printf.printf "[2/3] Verible → BIR ...\n%!";
  let ver_prog = Verible_to_behavioral.convert_files ~top files in
  Printf.printf "  %d modules\n" (List.length ver_prog.modules);

  let pick label src =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top) src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s side: no module '%s'. Available: %s\n"
          label top
          (String.concat ", "
             (List.map (fun (m : Behavioral_ir.bmodule) -> m.name) src));
        exit 1
  in
  let slang_top = pick "slang"   slang_prog.modules in
  let ver_top   = pick "verible" ver_prog.modules in

  if Sys.getenv_opt "BIR_DUMP" <> None then begin
    Printf.printf "\n=== SLANG BIR ===\n%s\n=== VERIBLE BIR ===\n%s\n"
      (Behavioral_ir.string_of_bmodule slang_top)
      (Behavioral_ir.string_of_bmodule ver_top)
  end;

  Printf.printf "[3/3] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence slang_top ver_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (Slang ≡ Verible)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
