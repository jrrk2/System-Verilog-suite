(* Post-`synth_design` equivalence check: Vivado's full synthesis
   output (EDIF with LUTs, FFs, CARRY4, etc.) vs the original SV.

   Pipeline:
     SV  ──Verible──▶  BIR_sv
     EDIF ─Edif_to_behavioral──▶  BIR_edif  (LUTs lowered via INIT
                                             property → mux tree;
                                             FDR/FDS/FDC/FDP/FDRE/…
                                             lowered to BSequential)
     BIR_sv  ⊕  BIR_edif  ──Z3_miter──▶  proof

   Usage:
     test_full_synth_equiv <top> <design.edf> <src.sv> [<more.sv>...]

   The EDIF must be Vivado's `write_edif` output after a plain
   `synth_design` (not `synth_design -rtl`).  LUTs and FFs are
   modelled per their INIT property and pin shape; CARRY4 / RAM /
   DSP48 instances remain as hierarchical children (no BIR body) and
   will fail the miter — Stage-2 work tracks adding their semantic
   models.                                                              *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <design.edf> <src.sv> [<more.sv>...]\n" Sys.argv.(0);
  exit 2

let check_file f =
  if not (Sys.file_exists f) then begin
    Printf.eprintf "error: file not found: %s\n" f;
    exit 1
  end

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top = Sys.argv.(1) in
  let edif_path = Sys.argv.(2) in
  let sv_files =
    Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in
  check_file edif_path;
  List.iter check_file sv_files;

  Printf.printf
    "═════════════════════════════════════════════════════════════\n\
     Full-synth equivalence: %s\n\
     ─────────────────────────────────────────────────────────────\n\
     EDIF: %s\n\
     SV  : %s\n\n%!"
    top edif_path (String.concat ", " sv_files);

  Printf.printf "Step 1: SV → BIR via Verible\n%!";
  let sv_prog = Verible_to_behavioral.convert_files ~top sv_files in
  Printf.printf "  modules: %d\n" (List.length sv_prog.modules);
  let sv_top =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) ->
            m.name = top) sv_prog.modules with
    | Some m -> m
    | None ->
        Printf.eprintf "error: top module %s not found in SV BIR\n" top;
        exit 1
  in
  Printf.printf "  top %s: signals=%d processes=%d instances=%d\n\n%!"
    sv_top.name (List.length sv_top.signals)
    (List.length sv_top.processes) (List.length sv_top.instances);

  Printf.printf "Step 2: EDIF → BIR (LUT / FF / MUXF lowering)\n%!";
  let edif_prog = Edif_to_behavioral.convert edif_path in
  Printf.printf "  modules: %d\n" (List.length edif_prog.modules);
  let edif_top =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) ->
            m.name = top) edif_prog.modules with
    | Some m -> m
    | None ->
        Printf.eprintf "error: top module %s not found in EDIF\n" top;
        exit 1
  in
  Printf.printf "  top %s: signals=%d processes=%d instances=%d\n\n%!"
    edif_top.name (List.length edif_top.signals)
    (List.length edif_top.processes) (List.length edif_top.instances);

  let count_proc_kind procs =
    let comb = ref 0 and seq = ref 0 in
    List.iter (function
      | Behavioral_ir.BCombinational _ -> incr comb
      | Behavioral_ir.BSequential   _ -> incr seq) procs;
    (!comb, !seq) in
  let (sc, ss) = count_proc_kind sv_top.processes in
  let (ec, es) = count_proc_kind edif_top.processes in
  Printf.printf "  SV   : comb=%d seq=%d\n" sc ss;
  Printf.printf "  EDIF : comb=%d seq=%d\n\n%!" ec es;

  Printf.printf "Step 3: Z3 miter\n%!";
  let ok = Z3_miter.check_miter_equivalence sv_top edif_top in
  Printf.printf "\n═════════════════════════════════════════════════════════════\n";
  if ok then begin
    Printf.printf "  ✅ %s : SV ≡ post-`synth_design` EDIF\n" top;
    exit 0
  end else begin
    Printf.printf "  ❌ %s : NOT EQUIVALENT (or proof incomplete)\n" top;
    exit 1
  end
