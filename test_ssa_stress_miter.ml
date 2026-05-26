(* ssa_stress miter: Verilator JSON ↔ Verible parse-tree, with the
 * verible side run through Behavioral_mem_merge.merge_slice_writes
 * so @slice_write / @part_sel_write_* calls become BAssigns with
 * proper read-modify-write semantics before the Z3 miter sees them.
 *
 * Verilator is the oracle because it's the most mature open-source SV
 * frontend and independent from Verible — disagreement means a
 * Verible/SSA bug, not a yosys-RTLIL→BIR conversion gap (the yosys
 * oracle path mis-encoded simple if-tree always_ff bodies; see
 * `Rtlil_to_behavioral`).  Without the slice-write lowering, Z3_miter
 * encodes @slice_write as a no-op and slice-chain testcases come out
 * with `r = init` unchanged on the verible side.
 *
 * Usage: test_ssa_stress_miter <top> <file.sv> [more.sv ...] *)

let usage () =
  Printf.eprintf "usage: %s <top> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

(* Pull the Verilator-side BIR program from a JSON dump, mirroring
   what test_verilator_vs_verible does internally. *)
let load_verilator ~top files =
  let mdir = Filename.concat (Filename.get_temp_dir_name ())
               (Printf.sprintf "miter_vlt_vrb_%s_%d" top (Unix.getpid ())) in
  let _ = Sys.command (Printf.sprintf "rm -rf %s" mdir) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote mdir)) in
  let files_str = String.concat " " (List.map Filename.quote files) in
  let log = Filename.concat mdir "verilator.log" in
  let cmd =
    Printf.sprintf
      "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
       -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING \
       -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
       --top-module %s %s --Mdir %s > %s 2>&1"
      (Filename.quote top) files_str (Filename.quote mdir) (Filename.quote log)
  in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    Printf.eprintf "verilator failed (rc=%d), see %s\n" rc log;
    exit 1
  end;
  let json = Filename.concat mdir (Printf.sprintf "V%s.tree.json" top) in
  match Verilator_to_behavioral.convert_verilator_json_to_behavioral json with
  | Some p -> p
  | None -> Printf.eprintf "verilator-side BIR conversion failed\n"; exit 1

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  ssa_stress miter: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] Verilator → BIR ...\n%!";
  let vlt_prog = load_verilator ~top files in
  Printf.printf "  %d modules\n" (List.length vlt_prog.modules);

  Printf.printf "[2/3] Verible → BIR (with slice-write lowering) ...\n%!";
  let ver_prog = Verible_to_behavioral.convert_files ~top files in
  let ver_prog = Behavioral_mem_merge.merge_slice_writes_program ver_prog in
  Printf.printf "  %d modules\n" (List.length ver_prog.modules);

  let pick label src =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
            src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s side: no module '%s'. Available: %s\n"
          label top
          (String.concat ", "
             (List.map (fun (m : Behavioral_ir.bmodule) -> m.name) src));
        exit 1 in
  let vlt_top = pick "verilator" vlt_prog.modules in
  let ver_top = pick "verible"   ver_prog.modules in

  Printf.printf "[3/3] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence vlt_top ver_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (Verilator ≡ Verible+slice-merge)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
