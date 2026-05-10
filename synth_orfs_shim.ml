(* End-to-end synth driver for the ORFS Makefile hijack (#102).

   Reads SystemVerilog files, runs them through:
       Verible → BIR → elaborate → hardcaml.Circuit → lib_map → cell-Verilog
   and writes the resulting structural netlist to the path ORFS expects.

   Exit codes:
       0 — emitted a usable [1_2_yosys.v]
       1 — failure of any kind (parse, unsupported construct, lowering,
           emission).  Stderr says why.

   IMPORTANT: there is *no* silent fallback to yosys in this driver.
   When this exits non-zero, the ORFS make target also fails — that
   visibility is the point.  yosys remains available for explicit
   oracle/comparison runs (#101), but never as a trapdoor that hides
   bugs in our pipeline.  *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <output.v> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 1

let fail reason =
  Printf.eprintf "[synth_orfs_shim] FAIL: %s\n" reason;
  exit 1

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top      = Sys.argv.(1) in
  let out_path = Sys.argv.(2) in
  let files    = Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in

  Printf.eprintf "[synth_orfs_shim] top=%s, %d input files\n"
    top (List.length files);

  let prog = Verible_to_behavioral.convert_files ~top files in
  if prog.modules = [] then fail "Verible produced no modules";

  (* Intra-module elaboration only — DO NOT run Behavioral_flatten
     here.  Flatten is a hierarchical inlining pass that pulls
     combinational children's signals + processes into their parents;
     for our hier-synth flow that defeats the whole boundary-preserving
     architecture and produces parent modules with `child.signal` style
     names that ORFS would reject anyway.  Hierarchy is handled module-
     by-module in [Hier_synth.synth_program]. *)
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_mem_merge.merge_program
    |> Behavioral_mem_merge.merge_slice_writes_program
    |> Behavioral_mem_merge.merge_bytewise_writes_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  (* Memory-macro lowering: replace bit-blastable BIR memories with
     instances of OpenRAM-generated SRAM macros.  In-scope (v2): RAM
     with sync read, 1 write port and 1–2 read ports; mutex if/else
     write/read sites fold via priority mux.  Out-of-scope memories
     keep the bit-blast.  Set MEMLOWER=0 to disable.  Generated
     artifacts are appended to a manifest the caller can use to
     register .lef/.lib with OpenROAD. *)
  let prog, mem_arts =
    if Sys.getenv_opt "MEMLOWER" = Some "0" then prog, []
    else Behavioral_memlower.lower_program prog
  in
  if mem_arts <> [] then begin
    Printf.eprintf "[synth_orfs_shim] %d memory macro(s) instantiated:\n"
      (List.length mem_arts);
    List.iter (fun a ->
      Printf.eprintf "  %s\n    .v   %s\n    .lib %s\n"
        a.Mem_macro_resolve.module_name a.verilog_path a.liberty_path
    ) mem_arts
  end;

  if not (List.exists
            (fun (m : Behavioral_ir.bmodule) -> m.name = top) prog.modules)
  then fail (Printf.sprintf "top %s not found in Verible output" top);

  Printf.eprintf "[synth_orfs_shim] %d module(s) after elaboration\n"
    (List.length prog.modules);

  (* Synth each module to gate level, leaves first.  Fails loudly on
     any leaf the hardcaml lowerer can't handle — surfaces real bugs
     instead of silently omitting cells. *)
  let netlists =
    try Hier_synth.synth_program prog
    with Failure msg -> fail msg
       | e -> fail (Printf.sprintf "hier_synth crashed: %s" (Printexc.to_string e))
  in

  (* Load-aware drive-strength selection.  Pre-placement: walk each
     net's fanout, sum sink pin caps + per-fanout wire estimate, swap
     each driver to the smallest Liberty variant whose max_capacitance
     covers the load.  Sends a pre-sized netlist to OpenROAD so the
     placer has slack to optimise rather than spending its budget on
     repair.  Set SV_DECOMP_NO_SIZE=1 to skip. *)
  let netlists =
    if Sys.getenv_opt "SV_DECOMP_NO_SIZE" = Some "1" then netlists
    else
      match Lib_size.liberty_path_or_default () with
      | None ->
          Printf.eprintf
            "[lib_size] no Liberty found; skipping drive-strength sizing \
             (set SV_DECOMP_LIBERTY=<path> to enable)\n";
          netlists
      | Some lib_path ->
          let cat = Lib_size.catalogue_for_path lib_path in
          let total = ref 0 in
          let netlists' = List.map (fun (mn : Hier_synth.module_netlist) ->
            let nl', n = Lib_size.resize_module ~cat mn.mn_netlist in
            total := !total + n;
            { mn with mn_netlist = nl' }
          ) netlists in
          Printf.eprintf
            "[lib_size] resized %d cell(s) using %s (wire_cap_ff=%.3f)\n"
            !total
            (Filename.basename lib_path)
            (Lib_size.wire_cap_ff ());
          netlists'
  in

  (* Emit one Verilog file with all module blocks in dependency order
     (children first, top last).  ORFS doesn't care about order, but
     readability does. *)
  let oc = open_out out_path in
  let total_cells = ref 0 in
  let total_children = ref 0 in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    let child_insts =
      List.map (fun (c : Hier_synth.child_inst_emit) ->
        { Cell_verilog_emit.ci_module = c.ci_module;
          ci_inst = c.ci_inst;
          ci_conns = c.ci_conns }) mn.mn_child_insts in
    Cell_verilog_emit.emit_module_hier
      ~oc ~module_name:mn.mn_name
      ~real_inputs:mn.mn_real_inputs
      ~real_outputs:mn.mn_real_outputs
      ~child_insts
      mn.mn_netlist;
    total_cells := !total_cells + List.length mn.mn_netlist.insts;
    total_children := !total_children + List.length mn.mn_child_insts
  ) netlists;
  close_out oc;

  Printf.eprintf "[synth_orfs_shim] OK — %d module block(s), %d cells, %d child instances\n"
    (List.length netlists) !total_cells !total_children;
  Printf.eprintf "[synth_orfs_shim] wrote %s\n" out_path
