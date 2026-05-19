(* Z3 miter for a behavioral source ↔ gate-level netlist pair.
 *
 * Legacy entry point.  All logic lives in shared library modules; the
 * preferred CLI is `sv_suite gate-miter <top> <beh.sv> <gate.sv>
 * [<lib>]`. This wrapper stays for older scripts and the
 * test/gate_miter regression runner.
 *
 *   behavioral.sv ─ Verible_to_behavioral ─► bmodule_beh
 *   gate.sv       ─ Gate_netlist_to_behavioral.preprocess_gate_file
 *                 ─ Verible_to_behavioral.convert_files_with_externals
 *                 ─ Gate_netlist_to_behavioral.expand_program(lib)
 *                                            ─► bmodule_gate
 *   bmodule_beh ↔ bmodule_gate → Z3_miter.check_miter_equivalence
 *
 * Default Liberty: $HOME/hardcaml-lua.0.0.1/liberty/simcells.lib *)

open Behavioral_ir

let usage () =
  Printf.eprintf
    "usage: %s <top> <behavioral.sv> <gate.sv> [<liberty.lib>]\n"
    Sys.argv.(0);
  exit 2

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top     = Sys.argv.(1) in
  let beh_sv  = Sys.argv.(2) in
  let gate_sv = Sys.argv.(3) in
  let lib_file =
    if Array.length Sys.argv >= 5 then Sys.argv.(4)
    else
      let home = try Sys.getenv "HOME" with Not_found -> "" in
      home ^ "/hardcaml-lua.0.0.1/liberty/simcells.lib"
  in
  if not (Sys.file_exists lib_file) then begin
    Printf.eprintf "Liberty file not found: %s\n" lib_file;
    exit 2
  end;

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Gate-level ↔ behavioral Z3 miter\n";
  Printf.printf "  top:        %s\n" top;
  Printf.printf "  beh:        %s\n" beh_sv;
  Printf.printf "  gate:       %s\n" gate_sv;
  Printf.printf "  liberty:    %s\n" lib_file;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/4] Liberty load ...\n%!";
  let lib = Sv_liberty.parse_liberty_file lib_file in
  Printf.printf "  %d cells\n" (Hashtbl.length lib.cells);

  Printf.printf "[2/4] Verible → BIR (behavioral) ...\n%!";
  let beh_prog = Verible_to_behavioral.convert_files ~top [beh_sv] in
  Printf.printf "  %d modules\n" (List.length beh_prog.modules);

  Printf.printf "[3/4] Verible → BIR (gate-level) + cell expansion ...\n%!";
  let gate_sv_clean = Gate_netlist_to_behavioral.preprocess_gate_file gate_sv in
  if gate_sv_clean <> gate_sv then
    Printf.printf "  preprocessed yosys IDs → %s\n" gate_sv_clean;
  let gate_prog =
    Verible_to_behavioral.convert_files_with_externals
      ~top [gate_sv_clean] in
  let (known, unknown, unknown_names) =
    Gate_netlist_to_behavioral.instance_coverage lib gate_prog in
  Printf.printf "  cell coverage: %d known / %d unknown\n" known unknown;
  if unknown > 0 then
    Printf.printf "    unknown cell types: %s\n"
      (String.concat ", " unknown_names);
  let gate_prog = Gate_netlist_to_behavioral.expand_program lib gate_prog in

  let pick label src =
    match List.find_opt (fun (m : bmodule) -> m.name = top) src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s: no module '%s'. Available: %s\n"
          label top
          (String.concat ", "
             (List.map (fun (m : bmodule) -> m.name) src));
        exit 1
  in
  let beh_top  = pick "behavioral" beh_prog.modules in
  let gate_top = pick "gate"       gate_prog.modules in

  if Sys.getenv_opt "BIR_DUMP" <> None then begin
    Printf.printf "\n=== BEH BIR ===\n%s\n=== GATE-EXPANDED BIR ===\n%s\n"
      (string_of_bmodule beh_top)
      (string_of_bmodule gate_top)
  end;

  Printf.printf "[4/4] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence beh_top gate_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (behavioral ≡ gate-level)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
