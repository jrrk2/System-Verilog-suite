(* Shared SystemVerilog → cell-mapped Verilog pipeline.

   Used by:
     - [Synth_orfs_shim]  — invoked from inside ORFS make as the synth step.
     - [Orfs_prep]        — preflight that stages a per-stamp ORFS variant.

   The body is the same in both: Verible → BIR → elaborate →
   memlower → hier_synth → load-aware sizing → cell Verilog.  Splitting
   it out so [Orfs_prep] can call it without invoking [synth_orfs_shim]
   as a subprocess (and so we don't drift two near-identical
   pipelines).  *)

let fail reason =
  Printf.eprintf "[synth_pipeline] FAIL: %s\n" reason;
  exit 1

let run ?(emit_verilog=true) ~top ~out_path ~files () =
  Printf.eprintf "[synth_pipeline] top=%s, %d input files\n"
    top (List.length files);

  let prog = Verible_to_behavioral.convert_files ~top files in
  if prog.modules = [] then fail "Verible produced no modules";

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
  let prog, mem_arts =
    if Sys.getenv_opt "MEMLOWER" = Some "0" then prog, []
    else Behavioral_memlower.lower_program prog
  in
  if mem_arts <> [] then begin
    Printf.eprintf "[synth_pipeline] %d memory macro(s) instantiated:\n"
      (List.length mem_arts);
    List.iter (fun a ->
      Printf.eprintf "  %s\n    .v   %s\n    .lib %s\n"
        a.Mem_macro_resolve.module_name a.verilog_path a.liberty_path
    ) mem_arts
  end;

  if not (List.exists
            (fun (m : Behavioral_ir.bmodule) -> m.name = top) prog.modules)
  then fail (Printf.sprintf "top %s not found in Verible output" top);

  Printf.eprintf "[synth_pipeline] %d module(s) after elaboration\n"
    (List.length prog.modules);

  let netlists =
    try Hier_synth.synth_program prog
    with Failure msg -> fail msg
       | e -> fail (Printf.sprintf "hier_synth crashed: %s" (Printexc.to_string e))
  in

  (* Mux-chain flattening — collapse priority-chain MUX2 cascades
     into balanced one-hot AND-OR.  Off by default for now; flip to
     on once the picosoc result confirms it.                         *)
  let netlists =
    if Sys.getenv_opt "SV_DECOMP_MUX_FLATTEN" <> Some "1" then netlists
    else
      List.map (fun (mn : Hier_synth.module_netlist) ->
        let nl', _, _ = Mux_chain_flatten.flatten_module mn.mn_netlist in
        { mn with mn_netlist = nl' }
      ) netlists
  in
  (* k-ary AND/OR merge — collapse chains/trees of AND2/OR2 into
     AND3/AND4/OR3/OR4 where intermediate nets are single-fanout.
     A 7-deep OR2 chain (8 inputs) becomes a 2-deep OR4 tree:
     2.3× fewer cells, 3.5× shorter depth.  On by default;
     SV_DECOMP_NO_KARY_MERGE=1 skips.                                *)
  let netlists =
    if Sys.getenv_opt "SV_DECOMP_NO_KARY_MERGE" = Some "1" then netlists
    else
      let total = ref 0 in
      let netlists' = List.map (fun (mn : Hier_synth.module_netlist) ->
        let nl', n = Kary_merge.merge_module mn.mn_netlist in
        total := !total + n;
        { mn with mn_netlist = nl' }
      ) netlists in
      netlists'
  in
  (* Tie-fanout limiter — splits over-loaded LOGIC0/LOGIC1 cells so
     OpenROAD's repair_tie_fanout doesn't have to.  Default fanout
     cap 16; SV_DECOMP_TIE_FANOUT_MAX overrides; SV_DECOMP_NO_TIE_FAN=1
     skips entirely.                                                  *)
  let netlists =
    if Sys.getenv_opt "SV_DECOMP_NO_TIE_FAN" = Some "1" then netlists
    else
      let total = ref 0 in
      let netlists' = List.map (fun (mn : Hier_synth.module_netlist) ->
        let nl', n = Tie_fanout.split_module mn.mn_netlist in
        total := !total + n;
        { mn with mn_netlist = nl' }
      ) netlists in
      netlists'
  in
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
  (* Scan-chain insertion — swaps DFF → SDFF and stitches every FF in
     emission order into a single chain.  Adds scan_en, scan_in inputs
     and a scan_out output to each module.  Off by default;
     SV_DECOMP_SCAN=1 enables.  Sits after [Lib_size] so the SDFF
     variants inherit any drive-strength bumps the original DFFs got. *)
  let netlists =
    if not (Scan_insert.enabled ()) then netlists
    else
      let total_ffs = ref 0 in
      let netlists' = List.map (fun (mn : Hier_synth.module_netlist) ->
        let nl', n = Scan_insert.scan_module mn.mn_netlist in
        if n > 0 then begin
          total_ffs := !total_ffs + n;
          let new_inputs  = mn.mn_real_inputs  @ [("scan_en", 1); ("scan_in", 1)] in
          let new_outputs = mn.mn_real_outputs @ [("scan_out", 1)] in
          { mn with
            mn_netlist     = nl';
            mn_real_inputs = new_inputs;
            mn_real_outputs = new_outputs }
        end else mn
      ) netlists in
      Printf.eprintf
        "[scan_insert] stitched %d FF(s) into scan chain(s) across %d module(s)\n"
        !total_ffs (List.length netlists');
      netlists'
  in

  if emit_verilog then begin
    let oc = open_out out_path in
    let total_cells = ref 0 in
    let total_children = ref 0 in
    (* Macro stubs need their wrapper body inlined.  Without it the
       module declares its ports and ends — OpenROAD then sees the
       wrapper as a module with no implementation, and the inner
       fakeram45 instance never reaches the layout (the LEF is
       loaded but unused).  Look up by mn_name in mem_arts and
       splice in the cache .v file content instead of calling the
       generic empty-body emitter.                                  *)
    let macro_v_by_name : (string, string) Hashtbl.t = Hashtbl.create 4 in
    List.iter (fun a ->
      Hashtbl.replace macro_v_by_name
        a.Mem_macro_resolve.module_name a.verilog_path
    ) mem_arts;
    let inline_file path =
      try
        let ic = open_in path in
        let buf = Buffer.create 1024 in
        (try
          while true do Buffer.add_channel buf ic 4096 done
        with End_of_file -> ());
        close_in ic;
        output_string oc (Buffer.contents buf);
        output_char oc '\n'
      with Sys_error msg ->
        Printf.eprintf "[synth_pipeline] WARN: failed to inline %s: %s\n"
          path msg
    in
    List.iter (fun (mn : Hier_synth.module_netlist) ->
      match Hashtbl.find_opt macro_v_by_name mn.mn_name with
      | Some wrapper_v ->
          (* Macro stub — emit the cache wrapper instead of the
             empty-body stub.  No netlist cells to count; no child
             instances on the macro itself. *)
          inline_file wrapper_v
      | None ->
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

    Printf.eprintf
      "[synth_pipeline] OK — %d module block(s), %d cells, %d child instances\n"
      (List.length netlists) !total_cells !total_children;
    Printf.eprintf "[synth_pipeline] wrote %s\n" out_path;
    (* Sidecar JSON: full RTL signal names + module-hash mappings +
       per-block kind/width/arch.  Reverse mapping for downstream
       pretty-printers / fanout-cone extractors that need the
       untruncated info.                                          *)
    let blocks_json_path = out_path ^ ".blocks.json" in
    Block_tag.write_blocks_json blocks_json_path;
    Printf.eprintf "[synth_pipeline] wrote %s\n" blocks_json_path;
    (* Pre-empt OpenROAD's [replace_arith_modules]: predict which
       arith blocks would be candidates for an arch swap.  This is
       analysis-only for now; SV_DECOMP_ARCH_SWAP=1 will gate the
       in-place swap once it's certified end-to-end.              *)
    Block_arch_swap.report ()
  end;
  netlists, mem_arts
