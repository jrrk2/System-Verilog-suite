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

(* Verible's BIR converter only records instances of modules it has
   seen in its compilation unit (the call to specialise_design
   filters unknown ones).  For our cell-mapped output the cells are
   library primitives whose Verilog bodies aren't part of the file —
   so without stub modules, every cell instance is silently dropped.

   We synthesise empty-body stubs from the Liberty file so Verible
   counts each cell instance as a real instantiation, then
   gate_netlist_to_behavioral expands them using the Liberty
   function expressions. *)
let generate_cell_stubs lib_file cell_file =
  let lib = Sv_liberty.parse_liberty_file lib_file in
  (* Find every cell type used in the cell-mapped file by simple
     line scan: `CELLNAME instance_name ( ... )`. *)
  let used = Hashtbl.create 16 in
  let ic = open_in cell_file in
  (try
    while true do
      let line = input_line ic in
      let trimmed = String.trim line in
      let re = Str.regexp "^\\([A-Z][A-Z0-9_]*\\) +[_A-Za-z]" in
      if Str.string_match re trimmed 0 then begin
        let cell_name = Str.matched_group 1 trimmed in
        if Hashtbl.mem lib.cells cell_name then
          Hashtbl.replace used cell_name ()
      end
    done
  with End_of_file -> ());
  close_in ic;

  let stubs_path = Filename.temp_file "cell_stubs_" ".v" in
  let oc = open_out stubs_path in
  Hashtbl.iter (fun name () ->
    let cell = Hashtbl.find lib.cells name in
    let port_list =
      List.map (fun (p : Sv_liberty.pin_info) ->
        let dir = match p.direction with
          | Input -> "input" | Output -> "output"
          | Inout -> "inout" | Internal -> "input" in
        Printf.sprintf "%s %s" dir p.name
      ) cell.pins in
    Printf.fprintf oc "module %s (%s);\nendmodule\n\n"
      name (String.concat ", " port_list)
  ) used;
  close_out oc;
  stubs_path

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
     normaliser, generate stub `module CELL (...)` decls for every
     cell instantiated in the file (Verible refuses to record
     instantiations of modules it can't see in the same compilation
     unit), concatenate stubs + cell-mapped Verilog into one file,
     then run Verible. *)
  let normalised_path = Gate_netlist_to_behavioral.preprocess_gate_file cell_file in
  let stubs_path = generate_cell_stubs lib_file normalised_path in
  let cell_raw =
    Verible_to_behavioral.convert_files ~top [stubs_path; normalised_path] in
  if Sys.getenv_opt "DUMP_PRE_EXPAND" <> None then
    List.iter (fun (m : bmodule) ->
      Printf.printf "  raw     %s: %d signals %d processes %d instances\n"
        m.name (List.length m.signals) (List.length m.processes) (List.length m.instances)
    ) cell_raw.modules;
  if cell_raw.modules = [] then begin
    Printf.eprintf "FATAL: Verible couldn't parse the cell-mapped file %s\n"
      cell_file;
    exit 1
  end;
  Printf.printf "Cells : %d module(s) before expand\n"
    (List.length cell_raw.modules);
  if Sys.getenv_opt "DUMP_PRE_EXPAND" <> None then
    List.iter (fun (m : bmodule) ->
      Printf.printf "  pre-expand %s: %d signals %d processes %d instances\n"
        m.name (List.length m.signals) (List.length m.processes) (List.length m.instances);
      List.iter (fun (i : binstance) ->
        Printf.printf "    inst %s : %s\n" i.inst_name i.module_name
      ) m.instances
    ) cell_raw.modules;

  let cell_behav =
    Gate_netlist_to_behavioral.expand_program_with_liberty lib_file cell_raw in
  Printf.printf "Cells : %d module(s) after  expand\n"
    (List.length cell_behav.modules);

  (* Hierarchical substitution (#115).  Parent modules with child
     instances would otherwise miter against unconstrained child
     outputs — Z3 picks any non-matching values and the parent
     "fails" even though every leaf has been independently proven
     equivalent.  Behavioral_flatten inlines combinational children
     into parents.  Apply to BOTH sides equally so the miter sees
     parent logic with all child operations explicit and matched
     module-by-module.

     We keep this OFF in the synth shim (gates emitted into ORFS
     must preserve module boundaries for the dual hier/flat
     representation) but turn it ON here in the miter, where
     flattening is a verification convenience. *)
  let src_prog =
    src_prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
    |> Behavioral_boundary.substitute_program
  in
  let cell_behav =
    cell_behav
    |> Behavioral_boundary.substitute_program
  in
  Printf.printf "After flatten: source=%d, cells=%d module(s)\n"
    (List.length src_prog.modules)
    (List.length cell_behav.modules);
  if Sys.getenv_opt "DUMP_EXPAND" <> None then
    List.iter (fun m ->
      Printf.printf "\n%s\n" (Behavioral_ir.string_of_bmodule m)
    ) cell_behav.modules;
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

  if Sys.getenv_opt "DUMP_BMOD" <> None then begin
    let dump (label : string) (p : bprogram) =
      List.iter (fun (m : bmodule) ->
        if Sys.getenv_opt "DUMP_BMOD" = Some m.name
        || Sys.getenv_opt "DUMP_BMOD" = Some "all" then begin
          Printf.printf "\n##### %s : %s #####\n" label m.name;
          Printf.printf "%s\n" (Behavioral_ir.string_of_bmodule m)
        end
      ) p.modules in
    dump "SRC" src_prog;
    dump "CELL" cell_behav;
  end;
  let n_pass = ref 0 and n_fail = ref 0 in
  List.iter (fun name ->
    let src_m  = Hashtbl.find src_h  name in
    let cell_m = Hashtbl.find cell_h name in
    Printf.printf "──── %s ────\n" name;
    Z3_miter.clear_miter_caches ();
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
