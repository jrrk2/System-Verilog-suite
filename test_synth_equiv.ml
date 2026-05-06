(* #113 — formal equivalence between source SV and our cell-mapped
   output.  Demonstrates the verification-quality story end-to-end:

      source SV ──► Verible ──► BIR_src
      cell.v    ──► Verible ──► BIR_cells (with cell binstances)
                                     │
                                     ▼
                            gate_netlist_to_behavioral
                            + Liberty function strings
                                     │
                                     ▼
                              BIR_cells_behav
      Z3 miter (BIR_src ≡ BIR_cells_behav) ?

   Usage:
       test_synth_equiv <top> <source.sv> <cellmapped.v> <liberty.lib>

   Iterates every module pair found by name in BOTH bprograms and
   prints a per-module verdict.  Exits 0 iff all modules prove
   equivalent, 1 if any module is not equivalent or can't be
   compared (missing on one side, etc.).  *)

open Behavioral_ir

let usage () =
  Printf.eprintf
    "usage: %s <top> <source.sv> <cellmapped.v> <liberty.lib>\n"
    Sys.argv.(0);
  exit 2

let () =
  if Array.length Sys.argv < 5 then usage ();
  let top      = Sys.argv.(1) in
  let src_file = Sys.argv.(2) in
  let cell_file= Sys.argv.(3) in
  let lib_file = Sys.argv.(4) in

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Cell-mapped equivalence check (#113)\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  top      : %s\n" top;
  Printf.printf "  source   : %s\n" src_file;
  Printf.printf "  cell-map : %s\n" cell_file;
  Printf.printf "  liberty  : %s\n" lib_file;
  Printf.printf "\n";

  let src_prog = Verible_to_behavioral.convert_files ~top [src_file] in
  if src_prog.modules = [] then begin
    Printf.eprintf "FATAL: no modules in source\n"; exit 1
  end;

  Printf.printf "Source: %d module(s)\n" (List.length src_prog.modules);

  (* Cell-mapped Verilog: pre-process via the gate-netlist text
     normaliser, then run through Verible to get a bprogram with
     binstances, then expand cells using the Liberty function
     expressions. *)
  let normalised_path = Gate_netlist_to_behavioral.preprocess_gate_file cell_file in
  let cell_raw = Verible_to_behavioral.convert_files ~top [normalised_path] in
  if cell_raw.modules = [] then begin
    Printf.eprintf "FATAL: Verible couldn't parse the cell-mapped file %s\n"
      cell_file;
    exit 1
  end;
  Printf.printf "Cells : %d module(s) before expand\n"
    (List.length cell_raw.modules);

  let cell_behav =
    Gate_netlist_to_behavioral.expand_program_with_liberty lib_file cell_raw in
  Printf.printf "Cells : %d module(s) after  expand\n"
    (List.length cell_behav.modules);
  Printf.printf "\n";

  let by_name (p : bprogram) =
    let h = Hashtbl.create 16 in
    List.iter (fun (m : bmodule) -> Hashtbl.replace h m.name m) p.modules;
    h in
  let src_h  = by_name src_prog in
  let cell_h = by_name cell_behav in

  let names =
    Hashtbl.fold (fun k _ acc ->
      if Hashtbl.mem cell_h k then k :: acc else acc) src_h []
    |> List.sort compare in

  if names = [] then begin
    Printf.eprintf "FATAL: no modules with matching names between source \
                    and cell-mapped output\n";
    Printf.eprintf "  source modules: %s\n"
      (String.concat ", "
         (Hashtbl.fold (fun k _ acc -> k :: acc) src_h []));
    Printf.eprintf "  cell modules  : %s\n"
      (String.concat ", "
         (Hashtbl.fold (fun k _ acc -> k :: acc) cell_h []));
    exit 1
  end;

  let n_pass = ref 0 and n_fail = ref 0 in
  List.iter (fun name ->
    let src_m  = Hashtbl.find src_h  name in
    let cell_m = Hashtbl.find cell_h name in
    Printf.printf "──── %s ────\n" name;
    let ok =
      try Z3_miter.check_miter_equivalence src_m cell_m
      with e ->
        Printf.printf "  ERROR: %s\n" (Printexc.to_string e);
        false
    in
    if ok then begin Printf.printf "  ✅ equivalent\n\n"; incr n_pass end
    else begin       Printf.printf "  ❌ NOT equivalent\n\n"; incr n_fail end
  ) names;

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Summary: %d pass, %d fail (%d modules total)\n"
    !n_pass !n_fail (List.length names);
  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  if !n_fail = 0 then exit 0 else exit 1
