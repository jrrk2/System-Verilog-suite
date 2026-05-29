(* Full picosoc through the FPGA flow:
 *   Verible -> flatten -> unroll/inline/iflift/blocking_subst/meminfer
 *   -> memlower(FPGA BRAM) -> behavioral_to_hardcaml ~emit_instances
 *   -> bir_to_aig -> fpga_map ~io -> yosys-JSON.
 * Usage: test_picosoc_fpga <top> <file.sv> [more ...] *)
let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  Printf.eprintf "[picosoc] parsing %s (%d files)\n%!" top (List.length files);
  let prog = Verible_to_behavioral.convert_files ~top files in
  Printf.eprintf "[picosoc] parsed %d modules\n%!" (List.length prog.modules);
  (* flatten_for_z3 fully inlines the submodule hierarchy into one bmodule
     (param-specialised) — what the miter uses; flatten_program only
     flattens generate blocks and leaves instances. *)
  let flat = Behavioral_hier.flatten_for_z3 prog ~top in
  let prog = { Behavioral_ir.modules = [ flat ]; library_cells = prog.library_cells } in
  Printf.eprintf "[picosoc] flattened -> 1 module, %d signals %d processes %d insts\n%!"
    (List.length flat.signals) (List.length flat.processes) (List.length flat.instances);
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  Printf.eprintf "[picosoc] pipeline done\n%!";
  let lowered, _ = Behavioral_memlower.lower_program prog in
  Printf.eprintf "[picosoc] memlower done\n%!";
  (* SSA after memlower (matches test_picosoc_gates).  Versions multi-
     write targets like cpu__reg_pc[slice] so each write gets a unique
     intermediate name; behavioral_to_hardcaml then emits those
     versions as combinational wires (not extra FFs).  Without this,
     picorv32's pcpi_mul carry-save chain has unassigned wires at
     Circuit.create_exn time. *)
  let lowered = { lowered with
    modules = List.map Behavioral_ssa.module_to_ssa lowered.modules } in
  let m =
    List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) lowered.modules
  in
  Printf.eprintf "[picosoc] top %s: signals=%d processes=%d instances=[%s] mems=%d\n%!"
    m.name (List.length m.signals) (List.length m.processes)
    (String.concat "," (List.map (fun (i : Behavioral_ir.binstance) -> i.module_name) m.instances))
    (List.length m.mems);
  (* Persistent build dir — survives reboot, unlike /tmp.  Default
     ~/picosoc_build/, overridable via $PICOSOC_BUILD. *)
  let build =
    match Sys.getenv_opt "PICOSOC_BUILD" with
    | Some d -> d
    | None ->
        Filename.concat (Sys.getenv "HOME") "picosoc_build" in
  (try Unix.mkdir build 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let flat_txt = Filename.concat build "picosoc_flat.txt" in
  let json_path = Filename.concat build "picosoc_fpga.json" in
  (let oc = open_out flat_txt in
   output_string oc (Behavioral_ir.string_of_bmodule m);
   close_out oc);
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true m in
  Printf.eprintf "[picosoc] hardcaml circuit built\n%!";
  let l = Fpga_synth.Bir_to_aig.lower_circuit circ in
  Printf.eprintf "[picosoc] AIG: nodes=%d insts=%d regs=%d outputs=%d\n%!"
    (Array.length l.Fpga_synth.Bir_to_aig.graph.Fpga_synth.Lut_cover.nodes)
    (List.length l.Fpga_synth.Bir_to_aig.insts)
    (List.length l.Fpga_synth.Bir_to_aig.regs)
    (List.length l.Fpga_synth.Bir_to_aig.graph.Fpga_synth.Lut_cover.outputs);
  (* k=8 enables MUXF7/MUXF8 wide-mux emission for 7- and 8-input cuts. *)
  let k =
    match Sys.getenv_opt "FPGA_LUT_K" with
    | Some s -> int_of_string s | None -> 8 in
  (* LUT-cover cost mode:
       FPGA_LUT_COST=area              area-flow first, depth tiebreak (default)
       FPGA_LUT_COST=delay             depth first, area-flow tiebreak
       FPGA_LUT_COST=mixed[:slack_tol] area pass + critical-cone re-cover *)
  let mode : Fpga_synth.Lut_cover.cost_mode =
    match Sys.getenv_opt "FPGA_LUT_COST" with
    | None | Some "area" -> `Area
    | Some "delay" -> `Delay
    | Some s when String.length s >= 5 && String.sub s 0 5 = "mixed" ->
      let tol =
        if String.length s > 6 then int_of_string (String.sub s 6 (String.length s - 6))
        else 0
      in
      `Mixed tol
    | Some other ->
      Printf.eprintf "[picosoc] unknown FPGA_LUT_COST=%s, falling back to area\n" other;
      `Area
  in
  let lutpack = Sys.getenv_opt "FPGA_LUT_PACK" = Some "1" in
  let mfs2_var_elim = Sys.getenv_opt "FPGA_MFS_VAR_ELIM" = Some "1" in
  let mfs2_odc = Sys.getenv_opt "FPGA_MFS_ODC" = Some "1" in
  (* Optional differential-clock pad insertion.
     DIFF_CLOCKS="clk:clk_p,clk_n;sysclk:sysclk_p,sysclk_n" — each
     entry says "the BIR clock signal <name> arrives as a P/N pad
     pair; wrap it in IBUFDS+BUFG instead of IBUF+BUFG."  Used by
     VC707-style boards whose 200 MHz sysclk is LVDS. *)
  let diff_clocks =
    match Sys.getenv_opt "DIFF_CLOCKS" with
    | None -> []
    | Some s ->
      String.split_on_char ';' s |> List.filter_map (fun entry ->
        match String.split_on_char ':' entry with
        | [name; pads] ->
          (match String.split_on_char ',' pads with
           | [p; n] -> Some (name, (p, n))
           | _ -> None)
        | _ -> None)
  in
  let mapped = Fpga_synth.Fpga_map.map_lowered
    ~io:true ~mode ~lutpack ~mfs2_var_elim ~mfs2_odc ~diff_clocks
    ~k ~name:top l in
  Fpga_synth.Fpga_emit.write_yosys_json ~path:json_path mapped;
  Printf.eprintf "[picosoc] wrote %s\n%!" json_path;
  (* Also emit EDIF so the same mapped netlist can be P&R'd by Vivado for
     a head-to-head against nextpnr-xilinx.  Vivado expects the EDIF file
     name to match the top-cell name.                                    *)
  let edif_path = Filename.concat build (top ^ ".edif") in
  Fpga_synth.Fpga_emit.write_edif ~path:edif_path mapped;
  Printf.eprintf "[picosoc] wrote %s\n%!" edif_path
