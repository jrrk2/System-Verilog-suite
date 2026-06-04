(* Lua scripting layer for sv_suite.
 *
 * Modeled directly on hardcaml-lua's myluaclient.ml: a small sum type
 * of artifacts (Prog/Mod/Lib/Bool/Str), a hashtable keyed by string
 * handles, and one Lua-callable per existing subcommand. The Lua side
 * only ever sees opaque handle strings; OCaml values stay in the
 * hashtable.
 *
 * Lua API (all under the `svd` module):
 *
 *   h = svd.parse(frontend, top, {file1, file2, …})  -> prog handle
 *   h = svd.pick(prog_handle, top)                   -> module handle
 *   r = svd.miter(mod_a, mod_b)                      -> "EQUIVALENT" / "DIFFER"
 *   r = svd.gate_miter(top, beh, gate)               -> same, with default Liberty
 *   r = svd.gate_miter(top, beh, gate, lib)          -> same, explicit Liberty
 *   h = svd.liberty(file)                            -> Liberty handle
 *   h = svd.expand(prog_handle, lib_handle)          -> new prog handle
 *   s = svd.bir(handle)                              -> textual BIR / dump
 *   s = svd.name(handle)                             -> stored name
 *   s = svd.items()                                  -> tab-separated index *)

open Behavioral_ir

(* ──────────────────────────────────────────────────────────────────
 * Handle storage *)

type luaitm =
  | Prog of string * bprogram                 (* label, program *)
  | Mod  of string * bmodule * bprogram       (* mod name, bmodule, owning program (for hier flatten) *)
  | Lib  of string * Sv_liberty.library_info
  | Mapped of string * Hardcaml.Circuit.t       (* gate-mapped Circuit.t (label, circ) *)
  (* Flat structural netlist: post flatten_struct, NO processes, just
   * binstances + nets.  Operations on this handle are restricted to
   * EDIF / yosys-JSON writers — BIR passes (Z3, flatten, prep_for_z3,
   * splice, …) don't make sense and are not bound to accept it. *)
  | Netlist of string * bmodule * (string * library_port list) list

let lhash : (string, luaitm) Hashtbl.t = Hashtbl.create 64

let nxtitm =
  let c = ref 0 in
  fun () -> incr c; "itm" ^ string_of_int !c

(* Insert and return a unique handle. If an identical artifact is
 * already stored, reuse its handle (cheap dedup mirrors hardcaml-lua). *)
let hadd x =
  let found = ref None in
  Hashtbl.iter (fun k v -> match !found, x, v with
    | Some _, _, _ -> ()
    | None, Prog (n, p), Prog (n', p') when n = n' && p == p' -> found := Some k
    | None, Mod  (n, m, _), Mod  (n', m', _) when n = n' && m == m' -> found := Some k
    | None, Lib  (n, l), Lib  (n', l') when n = n' && l == l' -> found := Some k
    | None, Mapped (n, c), Mapped (n', c') when n = n' && c == c' -> found := Some k
    | None, Netlist (n, m, _), Netlist (n', m', _) when n = n' && m == m' -> found := Some k
    | _ -> ()
  ) lhash;
  match !found with
  | Some k -> k
  | None ->
      let h = nxtitm () in
      Hashtbl.add lhash h x;
      h

let find_prog h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (n, p)) -> (n, p)
  | _ -> failwith ("handle " ^ h ^ " is not a program")

let find_mod h =
  match Hashtbl.find_opt lhash h with
  | Some (Mod (n, m, p)) -> (n, m, p)
  | _ -> failwith ("handle " ^ h ^ " is not a module")

let find_mapped h =
  match Hashtbl.find_opt lhash h with
  | Some (Mapped (n, c)) -> (n, c)
  | _ -> failwith ("handle " ^ h ^ " is not a mapped circuit")

let find_lib h =
  match Hashtbl.find_opt lhash h with
  | Some (Lib (n, l)) -> (n, l)
  | _ -> failwith ("handle " ^ h ^ " is not a library")

let find_netlist h =
  match Hashtbl.find_opt lhash h with
  | Some (Netlist (n, m, lc)) -> (n, m, lc)
  | _ -> failwith ("handle " ^ h ^ " is not a netlist")

(* ──────────────────────────────────────────────────────────────────
 * Frontend / pipeline shims — duplicated from sv_suite.ml's
 * load_frontend so the Lua layer doesn't pull in the executable. The
 * shared library functions (Verible_to_behavioral, Slang_to_behavioral,
 * Rtlil_to_behavioral, etc.) do the real work. *)

let find_yosys () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0
  ) [
    home ^ "/oss-cad-suite/bin/yosys";
    "/usr/local/bin/yosys";
    "/usr/bin/yosys";
    "yosys";
  ]

let run_yosys_to_rtlil ~top ~files ~out =
  let yosys = match find_yosys () with
    | Some y -> y
    | None -> failwith "yosys not found" in
  let script = Filename.temp_file "yosys_" ".ys" in
  let oc = open_out script in
  if Sys.getenv_opt "YOSYS_SLANG" <> None then begin
    Printf.fprintf oc "plugin -i slang\n";
    Printf.fprintf oc "read_slang --top %s %s\n" top
      (String.concat " " files);
    Printf.fprintf oc "hierarchy -top %s\nproc\nflatten\n" top
  end else begin
    Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
    Printf.fprintf oc
      "hierarchy -top %s\nproc\nopt -fast\nflatten\nopt -fast\n" top
  end;
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  let rc = Sys.command
             (Printf.sprintf "%s -q -s %s 2>&1"
                (Filename.quote yosys) (Filename.quote script)) in
  (try Sys.remove script with _ -> ());
  if rc <> 0 then failwith (Printf.sprintf "yosys exit %d" rc)

(* synlig is a yosys fork that reads SystemVerilog through Surelog,
 * emitting yosys-compatible RTLIL.  Same downstream consumer
 * (Rtlil_to_behavioral); the only difference is the read script. *)
let find_synlig () =
  let env_override = Sys.getenv_opt "SYNLIG_BIN" in
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  let candidates =
    (match env_override with Some s -> [s] | None -> [])
    @ [
      home ^ "/synlig/build/release/synlig/synlig";
      "/usr/local/bin/synlig";
      "/usr/bin/synlig";
      "synlig";
    ]
  in
  List.find_opt (fun p ->
    Sys.file_exists p
    || (try
          let ic = Unix.open_process_in
                     (Printf.sprintf "command -v %s 2>/dev/null" (Filename.quote p)) in
          let r = (try input_line ic with End_of_file -> "") in
          ignore (Unix.close_process_in ic); r <> ""
        with _ -> false)) candidates

let run_synlig_to_rtlil ~top ~files ~out =
  let synlig = match find_synlig () with
    | Some s -> s
    | None -> failwith "synlig not found (set SYNLIG_BIN)" in
  let script = Filename.temp_file "synlig_" ".ys" in
  let oc = open_out script in
  Printf.fprintf oc "read_systemverilog %s\n" (String.concat " " files);
  Printf.fprintf oc
    "hierarchy -top %s\nproc\nopt -fast\nflatten\nopt -fast\n" top;
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  let rc = Sys.command
             (Printf.sprintf "%s -q -s %s 2>&1"
                (Filename.quote synlig) (Filename.quote script)) in
  (try Sys.remove script with _ -> ());
  if rc <> 0 then failwith (Printf.sprintf "synlig exit %d" rc)

(* Pipeline applied to every frontend's raw BIR: expand width=0 fill
 * sentinels against their LHS context, normalise BCall formal-arg
 * widths, strip @signed markers.  Without expand_fills, an SV `'0`
 * fill literal lands as a `BConst { value=0; width=0 }` and Z3
 * rejects it with "bit-vector size must be greater than zero". *)
let post_load (p : bprogram) =
  p
  |> Behavioral_const.normalize_bcall_args_program
  |> Behavioral_const.expand_fills_program
  |> Behavioral_const.strip_signed_program

let load_frontend ~frontend ~top ~files : bprogram =
  match frontend with
  | "verible" ->
      Verible_to_behavioral.convert_files ~top files |> post_load
  | "verible-ext" ->
      Verible_to_behavioral.convert_files_with_externals ~top files
      |> post_load
  | "slang" ->
      (match Slang_to_behavioral.convert_files ~top files with
       | Some p -> post_load p
       | None -> failwith "slang frontend failed")
  | "yosys" ->
      let tmp = Filename.temp_file "yosys_" ".il" in
      run_yosys_to_rtlil ~top ~files ~out:tmp;
      let p = Rtlil_to_behavioral.convert_file tmp in
      (try Sys.remove tmp with _ -> ());
      post_load p
  | "verilator" ->
      (match files with
       | [j] ->
           (match Verilator_to_behavioral.convert_verilator_json_to_behavioral j with
            | Some p -> post_load p
            | None -> failwith "verilator JSON parse failed")
       | _ -> failwith "verilator frontend takes a single .json")
  | "synlig" ->
      (* synlig (yosys fork using Surelog SV frontend).  Same RTLIL
         path as the yosys frontend; different read script. *)
      let tmp = Filename.temp_file "synlig_" ".il" in
      run_synlig_to_rtlil ~top ~files ~out:tmp;
      let p = Rtlil_to_behavioral.convert_file tmp in
      (try Sys.remove tmp with _ -> ());
      post_load p
  | "sv-parser" ->
      (* dalance/sv-parser oracle: parse via the `parse_sv -t` dump
         and walk the CST into BIR.  Multi-file builds concatenate
         each file's modules into one bprogram so the miter can
         find the requested top.  Falls back to a no-op program on
         per-file parse failure so a partial set still resolves.
         Same normalisation pipeline that test_verilator_vs_verible
         runs on the verilator/verible IRs — sv-parser's MVP emits
         unsized literals at their nominal 32-bit width, so the
         expand_fills + normalize_bcall_args + strip_signed_markers
         passes fix LHS-width / formal-width / sign-marker shape
         before the miter sees them. *)
      let combined = List.fold_left (fun acc f ->
        match Sv_parser_to_behavioral.convert_file f with
        | Ok p -> { modules = acc.modules @ p.modules;
                    library_cells = acc.library_cells }
        | Error e ->
            Printf.eprintf "sv-parser: %s: %s\n" f e;
            acc) { modules = []; library_cells = [] } files in
      if combined.modules = [] then
        failwith "sv-parser frontend produced no modules";
      (* fold_ffs_program here is too aggressive: it folds sequential
         processes whose every leaf BAssign has a constant RHS, even
         when the BIf guards read mutable inputs (e.g. uart_interrupt
         picks one of 6 constants per cycle based on iRLSInterrupt
         &c. — the FF genuinely holds state across cycles).  Leave it
         to the per-design optimiser; the shared `post_load` pipeline
         is enough for sv-parser. *)
      combined |> post_load
  | "vhdl" ->
      (match files with
       | [] -> failwith "vhdl frontend needs at least one .vhd"
       | fs ->
           (* Parse all given .vhd together so a top architecture's
              component instantiations resolve against sibling entities
              and prep_for_z3 can flatten the hierarchy. *)
           (match Vhdl_to_behavioral.convert_vhdl_files_to_behavioral fs with
            | Some p -> p
            | None -> failwith "vhdl frontend failed"))
  | "surelog" ->
      (* Surelog UHDM dump path.  The frontend currently extracts
         module-level port surface only; processes/cont_assigns are
         the open task #50.  Useful as an interface-shape oracle and
         as a pipecleaner for leaf cells, but not yet a full Z3
         miter peer.  Argument is a path to a pre-captured
         uhdm-dump text file. *)
      (match files with
       | [f] when Filename.check_suffix f ".dump" ->
           Surelog_to_behavioral.convert_dump_file f
       | _ -> failwith
                "surelog frontend takes a single .dump (run \
                 `surelog -parse -sverilog FILE.sv && uhdm-dump …` first)")
  | other -> failwith ("unknown frontend: " ^ other)

(* ──────────────────────────────────────────────────────────────────
 * Lua-callable shims. Each takes/returns string handles. *)

let lparse frontend top files =
  let p = load_frontend ~frontend ~top ~files in
  hadd (Prog (top, p))

(* No-top "read everything" entry — see Verible_to_behavioral.convert_files_all.
 * Currently only the verible frontend has a no-top path; the others fall
 * back to load_frontend with "" as top, which slang/yosys will reject —
 * fine, the GUI guards on frontend before calling this. *)
let lparse_all frontend files =
  let p = match frontend with
    | "verible" -> Verible_to_behavioral.convert_files_all files
    | other -> load_frontend ~frontend:other ~top:"" ~files
  in
  hadd (Prog ("(all)", p))

let lpick prog_h top =
  let _, p = find_prog prog_h in
  match List.find_opt (fun (m : bmodule) -> m.name = top) p.modules with
  | Some m -> hadd (Mod (top, m, p))
  | None ->
      failwith (Printf.sprintf "no module '%s' in %s" top prog_h)

(* Verification pipeline (mirrors cmd_miter in sv_suite.ml):
 *   1. Behavioral_arch_subst — abstract attributed adder/mul leaves
 *      to BBinOp ops gated on a `verify-arch` certificate.
 *   2. Behavioral_hier — transiently flatten what remains for Z3.
 * The source bprograms are not modified. *)
let prep_for_z3 (m : bmodule) (p : bprogram) : bmodule =
  let p, _n = Behavioral_arch_subst.substitute_program p in
  match List.find_opt (fun (mm : bmodule) -> mm.name = m.name) p.modules with
  | None -> m
  | Some m' ->
      if m'.instances = [] then m'
      else Behavioral_hier.flatten_for_z3 p ~top:m'.name

let lmiter a_h b_h =
  let (_, ma, pa) = find_mod a_h in
  let (_, mb, pb) = find_mod b_h in
  (* prep_for_z3 internally calls Behavioral_hier.flatten_for_z3 which
   * registers any unresolved binstance into Behavioral_hier.unresolved.
   * Drain it on both sides before deciding the verdict — per
   * feedback-no-silent-lossage, an unresolved instance is its own
   * verification-failure category, distinct from EQUIVALENT/DIFFER. *)
  let _ = Behavioral_hier.take_unresolved () in   (* clear any stale *)
  let ma' = prep_for_z3 ma pa in
  let unres_a = Behavioral_hier.take_unresolved () in
  let mb' = prep_for_z3 mb pb in
  let unres_b = Behavioral_hier.take_unresolved () in
  let unres = unres_a @ unres_b in
  if unres <> [] then begin
    let summary = String.concat ", "
      (List.map (fun (_, inst, ty) -> Printf.sprintf "%s:%s" inst ty)
         unres) in
    Printf.sprintf "INCONCLUSIVE — %d unresolved primitive bodies: %s"
      (List.length unres) summary
  end else if Z3_miter.check_miter_equivalence ma' mb' then "EQUIVALENT"
  else "DIFFER"

let default_lib () =
  let home = try Sys.getenv "HOME" with Not_found -> "" in
  home ^ "/hardcaml-lua.0.0.1/liberty/simcells.lib"

let lliberty file =
  let lib = Sv_liberty.parse_liberty_file file in
  hadd (Lib (lib.lib_name, lib))

let lexpand prog_h lib_h =
  let label, p = find_prog prog_h in
  let _, lib  = find_lib lib_h in
  let p' = Gate_netlist_to_behavioral.expand_program lib p in
  hadd (Prog (label, p'))

(* ──────────────────────────────────────────────────────────────────
 * Pipeline building-blocks: each transformation as a stand-alone
 * Lua-callable function so a recipe script can compose them per design.
 * These replace the per-design test_*.exe pattern.
 *
 * GENERIC behavioural passes (used by both ASIC and FPGA flows):
 *   unroll / inline / iflift / blocking_subst / meminfer / memlower /
 *   ssa / flatten_z3 / flatten_struct.
 * Behavioral_meminfer / Behavioral_memlower honour the MEMLOWER_FPGA
 * env var; recipes can set it before calling, or leave it unset for
 * the ASIC path.  Neither lmeminfer nor lmemlower touches the env
 * here.
 *
 * FPGA-SPECIFIC (use Fpga_synth and consume LUT/FDRE/CARRY4 cells):
 *   gate_map (BIR → AIG → LUT-cover → Hardcaml Circuit.t),
 *   write_cellmapped_v, write_mapped_json, write_nextpnr_json.
 * ASIC flows substitute their own cell-mapper at the gate_map stage.
 *
 * Conventions:
 *   - Every prog→prog pass returns a fresh prog handle (label preserved).
 *   - Flatteners take prog + top name, return a module handle.
 *   - The gate-mapper takes a module handle (behavioural body) and
 *     returns a Mapped (Hardcaml Circuit.t) handle. *)

let lunroll prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_unroll.unroll_program p))

let linline prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_inline.inline_program p))

let liflift prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_iflift.lift_program p))

let lblocking_subst prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_blocking_subst.blocking_subst_program p))

let lmeminfer prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_meminfer.infer_program p))

let lmemlower prog_h =
  let label, p = find_prog prog_h in
  let p', _ = Behavioral_memlower.lower_program p in
  hadd (Prog (label, p'))

let lssa prog_h =
  let label, p = find_prog prog_h in
  let p' = { p with modules = List.map Behavioral_ssa.module_to_ssa p.modules } in
  hadd (Prog (label, p'))

(* The two flatteners are intentionally separate so a recipe makes
 * its choice explicit: flatten_z3 drops primitive binstances and
 * keeps behavioural processes (Z3-equivalence shape); flatten_struct
 * keeps every binstance and expects an already-structural program
 * (nextpnr JSON shape). *)
let lflatten_z3 prog_h top =
  let _, p = find_prog prog_h in
  let m = Behavioral_hier.flatten_for_z3 p ~top in
  let p' = { Behavioral_ir.modules = [m];
             library_cells = p.library_cells } in
  hadd (Mod (top, m, p'))

let lflatten_struct prog_h top =
  let _, p = find_prog prog_h in
  let m = Behavioral_hier_struct.flatten_structural p ~top in
  hadd (Netlist (top, m, p.library_cells))

(* Gate-map one module: behavioural BIR → AIG → LUT-cover → Hardcaml
 * Circuit.t of LUT/FDRE/CARRY4/IBUF/OBUF/BUFG cells. *)
let lgate_map mod_h k_lut io_flag =
  let _, m, _ = find_mod mod_h in
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true m in
  let l = Fpga_synth.Bir_to_aig.lower_circuit circ in
  let mapped = Fpga_synth.Fpga_map.map_lowered
    ~io:(io_flag <> 0) ~k:k_lut ~name:m.name l in
  hadd (Mapped (m.name, mapped))

(* Dump a Mapped circuit as cell-mapped Verilog via Hardcaml.Rtl,
 * suitable for ver_front to re-parse into structural BIR. *)
let lwrite_cellmapped_v mapped_h path =
  let _, circ = find_mapped mapped_h in
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect ~finally:(fun () -> Stdlib.close_out oc)
    (fun () ->
       Hardcaml.Rtl.output
         ~output_mode:(Hardcaml.Rtl.Output_mode.To_channel oc)
         Hardcaml.Rtl.Language.Verilog circ);
  path

(* Dump a Mapped circuit directly as yosys JSON (Fpga_emit path).
 * Use this when no further BIR-level manipulation is wanted. *)
let lwrite_mapped_json mapped_h path =
  let _, circ = find_mapped mapped_h in
  Fpga_synth.Fpga_emit.write_yosys_json ~path circ;
  path

(* Dump a Mapped circuit as EDIF 2.0.0 (Fpga_emit.write_edif), suitable
 * for Vivado read_edif / link_design as a DRC oracle on the open flow. *)
let lwrite_edif mapped_h path =
  let _, circ = find_mapped mapped_h in
  Fpga_synth.Fpga_emit.write_edif ~path circ;
  path

(* Same EDIF emit but from a Mod handle (post-flatten_struct bmodule).
 * Lifts the structural BIR to a hardcaml Circuit.t via the existing
 * behavioral_to_hardcaml entry, then routes through write_edif.
 *
 * WARNING: behavioral_to_hardcaml.create_circuit's emit_instances
 * path re-elaborates pure-structural BIR as RTL and silently drops
 * binstances that don't have BCombinational/BSequential drivers — so
 * a fully-flattened structural wrapper comes out with only GND/VCC
 * cells.  Prefer svd.write_bir_edif (bir_to_edif) for any flattened
 * wrapper netlist; this entry is kept for the gate-mapped-via-
 * hardcaml use case only. *)
let lwrite_mod_edif mod_h path =
  let _, m, _ = find_mod mod_h in
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true m in
  Fpga_synth.Fpga_emit.write_edif ~path circ;
  path

(* Direct Netlist → EDIF, bypassing both hardcaml and yosys.  Used to
 * feed a flattened structural wrapper into Vivado as a DRC / bitstream
 * oracle with LUT INIT parameters preserved correctly. *)
let lwrite_netlist_edif net_h path =
  let _, m, lc = find_netlist net_h in
  Bir_to_edif.write_edif ~library_cells:lc ~path m;
  path

(* Direct Netlist → gate-level structural Verilog.  Same internal graph
 * as write_netlist_edif / write_nextpnr_json — a third independent view
 * so xsim can functionally simulate the flattened netlist (against
 * unisims_ver) as a sanity check that the SVS internal representation
 * is correct.                                                          *)
let lwrite_netlist_verilog net_h path =
  let _, m, lc = find_netlist net_h in
  Bir_to_verilog_netlist.write_verilog ~library_cells:lc ~path m;
  path

(* Expand each Netlist binstance against its VHDL primitive implementation
 * body (LUT4.vhd, FDRE.vhd, CARRY4.vhd, …), producing a bprogram whose
 * top is the netlist's flat bmodule and whose remaining modules are the
 * primitive impl bodies.  Feeds the existing prep_for_z3 + miter flow so
 * the open-flow gate-mapped netlist can be Z3-equivalence-checked
 * against the source RTL (task #44). *)
let lexpand_primitives_for_z3 net_h =
  let label, m, lc = find_netlist net_h in
  let types =
    List.fold_left (fun acc (i : binstance) ->
      if List.mem i.module_name acc then acc else i.module_name :: acc)
      [] m.instances in
  let impls = Vhdl_to_behavioral.lookup_xil_primitive_impl types in
  let impl_mods = List.map snd impls in
  let p = { Behavioral_ir.modules = m :: impl_mods; library_cells = lc } in
  hadd (Prog (label ^ "+prims", p))

(* Same as expand_primitives_for_z3 but for a Prog handle.  The source-
 * side miter input (top.v parsed by Verible) is a Prog with no primitive
 * bodies: BUFG/IBUFDS/LUT/FDRE/CARRY4 instances reference primitives
 * whose bodies live in Vivado's VHDL stubs, not in the user RTL.  When
 * prep_for_z3 runs flatten_for_z3 on such a Prog, every primitive
 * binstance hits lookup_resolving's None branch and the miter declares
 * INCONCLUSIVE (task #45).  This augments the Prog by walking ALL
 * binstances in ALL bmodules, gathering the distinct cell-type names,
 * and appending the VHDL impl bodies so both miter sides see the same
 * primitive definitions. *)
let laugment_prog_with_primitives prog_h =
  let label, p = find_prog prog_h in
  let known = List.fold_left (fun acc (m : bmodule) -> m.name :: acc)
                [] p.modules in
  let types =
    List.fold_left (fun acc (m : bmodule) ->
      List.fold_left (fun acc (i : binstance) ->
        if List.mem i.module_name known || List.mem i.module_name acc
        then acc
        else i.module_name :: acc) acc m.instances) [] p.modules in
  let impls = Vhdl_to_behavioral.lookup_xil_primitive_impl types in
  let impl_mods = List.map snd impls in
  let p' = { p with modules = p.modules @ impl_mods } in
  hadd (Prog (label ^ "+prims", p'))

(* No-Vivado replacement for augment_prog_with_primitives: instead of parsing
 * Vivado's unisim VHDL, synthesise each Xilinx primitive body directly as BIR
 * from the binstance's INIT/params (Xil_prim_models).  Each instance is
 * specialised to a uniquely-named body baking in its INIT, so flatten_for_z3
 * — which inlines without parameter substitution and caches by module_name —
 * gets the right function per instance.  Use this on BOTH miter sides. *)
let laugment_xil_models prog_h =
  let label, p = find_prog prog_h in
  let p' = Xil_prim_models.augment_program p in
  hadd (Prog (label ^ "+xilmodels", p'))

(* Read an actual nextpnr post-P&R netlist (yosys JSON from `nextpnr --write`)
 * into structural BIR, reconstructing logical primitives from the X_ORIG_TYPE
 * / X_ORIG_PORT attributes (Nextpnr_json_to_behavioral).  Pair with
 * augment_xil_models + miter to check the pack/place mapping against source. *)
let lread_nextpnr_json path =
  let p = Nextpnr_json_to_behavioral.read_program path in
  let label = match p.modules with m :: _ -> m.name | [] -> "nextpnr" in
  hadd (Prog (label, p))

(* Physical routing-completeness report: which FF D-pins are not actually
 * reached by their net's ROUTING (the bypass-FFMUX defect signature). *)
let lroute_check path = Nextpnr_json_to_behavioral.route_report path

let lxil_models_coverage prog_h =
  let _, p = find_prog prog_h in
  let cov = Xil_prim_models.coverage p in
  if cov = [] then "(no modelled primitives)"
  else String.concat " "
         (List.map (fun (k, n) -> Printf.sprintf "%s=%d" k n) cov)

(* Lift a Mapped Circuit.t directly into BIR as a structural bprogram
 * with a single bmodule, preserving bus widths on top-level ports.
 * Bypasses the lossy write_cellmapped_v + ver_front re-parse loop that
 * tasks #38/#39 traced as the source of CARRY4.DI/S width collapse and
 * vector-port flattening. *)
let lmapped_to_prog mapped_h =
  let _, circ = find_mapped mapped_h in
  let m = Hardcaml_to_behavioral.of_circuit circ in
  let p = { Behavioral_ir.modules = [m]; library_cells = [] } in
  hadd (Prog (m.name, p))

(* Parse cell-mapped Verilog back into BIR via Ver_front_to_behavioral,
 * returning a prog handle.  Used to splice a gate-mapped sub-module
 * back under a wrapper that has user-instantiated primitive cells. *)
let lparse_v_cells path =
  match Ver_front_to_behavioral.convert_v_file path with
  | None -> failwith ("ver_front failed to parse " ^ path)
  | Some p -> hadd (Prog (Filename.basename path, p))

(* Replace bmodule named [child] in [prog_h] with the same-named
 * module taken from [src_h] (the splice). *)
let lsplice prog_h child src_h =
  let label, p = find_prog prog_h in
  let _, src  = find_prog src_h  in
  let src_m =
    match List.find_opt (fun (m : bmodule) -> m.name = child) src.modules with
    | Some m -> m
    | None -> failwith ("splice: no module " ^ child ^ " in source program")
  in
  let modules' = List.map (fun (m : bmodule) ->
    if m.name = child then src_m else m) p.modules in
  let modules' =
    if List.exists (fun (m : bmodule) -> m.name = child) p.modules
    then modules'
    else modules' @ [src_m]
  in
  let p' = { p with modules = modules' } in
  hadd (Prog (label ^ ":+" ^ child, p'))

(* Emit a flat structural netlist as nextpnr-xilinx-compatible yosys
 * JSON.  Takes a Netlist handle (the result of flatten_struct) — the
 * Mod path is gone because consumers of write_nextpnr_json always
 * want a structural shape, not RTL BIR. *)
let lwrite_nextpnr_json net_h path =
  let _, m, lc = find_netlist net_h in
  Bir_to_nextpnr_json.write_yosys_json
    ~library_cells:lc
    ~path m;
  path

(* Comma-separated list of module names in a prog. *)
let lmodule_names prog_h =
  let _, p = find_prog prog_h in
  String.concat "," (List.map (fun (m : bmodule) -> m.name) p.modules)

(* ──────────────────────────────────────────────────────────────────
 * Second-tier generic operations: extracted from the standalones in
 * old/, used both by ASIC miters and FPGA recipes.  Where a pass has
 * a `?force_ff` or similar option we expose only the most common
 * defaulted form — the underlying library function is one open away
 * if a recipe needs the variant. *)

let lflatten prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_flatten.flatten_program p))

let loptimize prog_h =
  let label, p = find_prog prog_h in
  let p', _ = Behavioral_optimize.optimize_full p in
  hadd (Prog (label, p'))

let lffrip mod_h =
  let label, m, p = find_mod mod_h in
  let m' = Behavioral_ffrip.rip_module m in
  let p' = { p with modules = List.map (fun (mm : bmodule) ->
              if mm.name = m.name then m' else mm) p.modules } in
  hadd (Mod (label, m', p'))

let lregister_analyse mod_h =
  let _, m, _ = find_mod mod_h in
  let ctx = Behavioral_registers.analyze_module m in
  Printf.sprintf "module %s: %d registers" m.name
    (List.length ctx.Behavioral_registers.registers)

let lcdc_analyse mod_h =
  let _, m, _ = find_mod mod_h in
  let report = Cdc_analysis.analyse m in
  Cdc_analysis.format_report report

let lprep_for_z3 mod_h =
  let _, m, p = find_mod mod_h in
  let m' = prep_for_z3 m p in
  hadd (Mod (m.name, m', p))

(* Recover the owning bprogram of a module handle as a Prog handle.
 * Lets a recipe go Prog -> flatten_z3 -> Mod -> owner -> Prog and
 * continue the prog→prog pipeline (unroll, inline, …). *)
let lowner mod_h =
  let label, _m, p = find_mod mod_h in
  hadd (Prog (label, p))

(* Read an EDIF netlist file and return a Prog handle of structural
 * BIR (binstances of Xilinx/library primitives). Companion of
 * svd.parse for the EDIF frontend; used by recipes/edif_vs_vhdl.lua. *)
let lread_edif path =
  if not (Sys.file_exists path) then
    failwith ("read_edif: file not found: " ^ path);
  let p = Edif_to_behavioral.convert path in
  hadd (Prog (Filename.basename path, p))

(* Structural variant: keep every EDIF instance as a binstance with its
 * primitive cell_type intact (LUT6/FDRE/CARRY4/BUFG/…), parameters in
 * param_strs.  For the Vivado-synth → SVS → nextpnr-xilinx flow where
 * write_nextpnr_json needs gate-level cells, not decoded behaviour. *)
let lread_edif_structural path =
  if not (Sys.file_exists path) then
    failwith ("read_edif_structural: file not found: " ^ path);
  let p = Edif2_to_structural.convert path in
  hadd (Prog (Filename.basename path, p))

let lgate_miter top beh gate lib_opt =
  let lib_path = match lib_opt with "" -> default_lib () | s -> s in
  if not (Sys.file_exists lib_path) then
    failwith ("Liberty file not found: " ^ lib_path);
  let lib = Sv_liberty.parse_liberty_file lib_path in
  let beh_p =
    Verible_to_behavioral.convert_files ~top [beh] in
  let gate_clean =
    Gate_netlist_to_behavioral.preprocess_gate_file gate in
  let gate_p =
    Verible_to_behavioral.convert_files_with_externals
      ~top [gate_clean] in
  let gate_p = Gate_netlist_to_behavioral.expand_program lib gate_p in
  let pick label src =
    match List.find_opt (fun (m : bmodule) -> m.name = top) src with
    | Some m -> m
    | None -> failwith (label ^ ": no module " ^ top) in
  let mb = pick "behavioral" beh_p.modules in
  let mg = pick "gate"       gate_p.modules in
  if Z3_miter.check_miter_equivalence mb mg then "EQUIVALENT" else "DIFFER"

let lbir h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (_, p))    -> string_of_bprogram p
  | Some (Mod  (_, m, _)) -> string_of_bmodule m
  | Some (Lib  (n, l)) ->
      Printf.sprintf "library %s: %d cells" n (Hashtbl.length l.cells)
  | Some (Mapped (n, _)) ->
      Printf.sprintf "mapped %s: (gate-mapped Hardcaml Circuit.t — \
                      use svd.write_cellmapped_v / svd.write_mapped_json)" n
  | Some (Netlist (n, m, _)) ->
      Printf.sprintf "netlist %s: %d cells (flat structural — use \
                      svd.write_nextpnr_json / svd.write_netlist_edif)"
        n (List.length m.instances)
  | None -> failwith ("unknown handle " ^ h)

(* Critical-path timing report for a module. Returns the printable
 * report (same string the CLI prints). Optional target-depth: if
 * provided, also returns suggested cert-gated upgrades. *)
let ltiming h target_depth =
  let _, m, _ = find_mod h in
  let arrivals = Behavioral_timing.compute_arrivals m in
  let paths = Behavioral_timing.endpoint_paths arrivals m in
  let target = if target_depth <= 0 then max_int else target_depth in
  let report = Behavioral_timing.report ~target_depth:target paths in
  let upgrades =
    if target = max_int then []
    else
      let failing =
        List.filter (fun (p : Behavioral_timing.path_report) ->
          p.arrival > target) paths in
      Behavioral_timing.suggest_upgrades failing in
  report ^ Behavioral_timing.format_upgrades upgrades

let linsts h =
  match Hashtbl.find_opt lhash h with
  | Some (Mod (_, m, _)) ->
      String.concat "\n"
        (List.map (fun (i : binstance) ->
          let conns = String.concat ", "
            (List.map (fun (p, e) ->
              Printf.sprintf ".%s(%s)" p (string_of_bexpr e)
            ) i.port_connections) in
          Printf.sprintf "  %s : %s (%s)"
            i.inst_name i.module_name conns
        ) m.instances)
  | _ -> "<not a module>"

let lname h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (n, _)) | Some (Mod (n, _, _)) | Some (Lib (n, _))
  | Some (Mapped (n, _)) | Some (Netlist (n, _, _)) -> n
  | None -> failwith ("unknown handle " ^ h)

(* ──────────────────────────────────────────────────────────────────
 * HDL emit + cross-translate.  These expose the BIR→Verilog and
 * BIR→VHDL emitters plus the convert_hdl pipeline (license-header
 * preserving, language-detected by extension). *)

let lemit_verilog h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (_, p)) -> Behavioral_to_verilog.verilog_of_program p
  | Some (Mod  (_, m, _)) ->
      Behavioral_to_verilog.verilog_of_program
        { modules=[m]; library_cells=[] }
  | _ -> failwith ("emit_verilog: not a program/module handle: " ^ h)

let lemit_vhdl h =
  match Hashtbl.find_opt lhash h with
  | Some (Prog (_, p)) -> Behavioral_to_vhdl.vhdl_of_program p
  | Some (Mod  (_, m, _)) ->
      Behavioral_to_vhdl.vhdl_of_program
        { modules=[m]; library_cells=[] }
  | _ -> failwith ("emit_vhdl: not a program/module handle: " ^ h)

let lwrite_verilog h path =
  let body = lemit_verilog h in
  let oc = open_out path in
  output_string oc body;
  close_out oc;
  "ok"

let lwrite_vhdl h path =
  let body = lemit_vhdl h in
  let oc = open_out path in
  output_string oc body;
  close_out oc;
  "ok"

(* Run the full convert_hdl pipeline (header preservation +
 * frontend dispatch + emitter dispatch) without spawning a subprocess
 * — gives Lua scripts the same end-to-end translation that the
 * convert_hdl exe offers, and returns the output path. *)
let lconvert_hdl input output =
  let kind p =
    match String.lowercase_ascii (Filename.extension p) with
    | ".vhd" | ".vhdl" -> `Vhdl
    | ".v" | ".sv" -> `Verilog
    | _ -> failwith ("convert_hdl: unknown extension: " ^ p)
  in
  let src = kind input and dst = kind output in
  (* read source *)
  let ic = open_in input in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let src_text = Bytes.unsafe_to_string buf in
  (* extract leading comment block (matching convert_hdl's logic) *)
  let lines = String.split_on_char '\n' src_text in
  let is_blank s = String.trim s = "" in
  let comment_match k s =
    let s = String.trim s in
    String.length s >= 2 &&
    (match k with
     | `Vhdl -> String.sub s 0 2 = "--"
     | `Verilog -> String.sub s 0 2 = "//" || String.sub s 0 2 = "/*")
  in
  let rec take acc = function
    | [] -> List.rev acc
    | l :: tl when is_blank l || comment_match src l -> take (l :: acc) tl
    | _ -> List.rev acc
  in
  let header_lines = take [] lines in
  let strip_marker s =
    let s = String.trim s in
    let lp p = let lp = String.length p in
      if String.length s >= lp && String.sub s 0 lp = p
      then String.sub s lp (String.length s - lp) |> String.trim else s in
    match src with
    | `Vhdl -> lp "--"
    | `Verilog ->
        let s = lp "//" in let s = if String.length s >= 2 && String.sub s 0 2 = "/*"
                                   then String.sub s 2 (String.length s - 2) else s in
        let n = String.length s in
        let s = if n >= 2 && String.sub s (n-2) 2 = "*/" then String.sub s 0 (n-2) else s in
        String.trim s
  in
  let prefix = match dst with `Vhdl -> "-- " | `Verilog -> "// " in
  let header =
    if header_lines = [] then ""
    else String.concat "\n"
           (List.map (fun l ->
              let stripped = strip_marker l in
              if stripped = "" then "" else prefix ^ stripped)
              header_lines) ^ "\n"
  in
  let prog =
    match src with
    | `Vhdl ->
        (match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral input with
         | Some p -> p | None -> failwith "vhdl frontend failed")
    | `Verilog ->
        let auto_top () =
          let re = Str.regexp "^[ \t]*module[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)" in
          try let _ = Str.search_forward re src_text 0 in
              Str.matched_group 1 src_text
          with Not_found -> Filename.remove_extension (Filename.basename input)
        in
        Verible_to_behavioral.convert_files ~top:(auto_top ()) [input]
  in
  let body = match dst with
    | `Vhdl -> Behavioral_to_vhdl.vhdl_of_program prog
    | `Verilog -> Behavioral_to_verilog.verilog_of_program prog
  in
  let oc = open_out output in
  if header <> "" then output_string oc header;
  output_string oc body;
  close_out oc;
  output

let litems () =
  let lst = Hashtbl.fold (fun k v acc ->
    let kind = match v with
      | Prog (n, p) ->
          Printf.sprintf "program %s (%d modules)" n
            (List.length p.modules)
      | Mod (n, _, _) -> Printf.sprintf "module %s" n
      | Lib (n, l) ->
          Printf.sprintf "library %s (%d cells)" n
            (Hashtbl.length l.cells)
      | Mapped (n, _) -> Printf.sprintf "mapped %s (gate-mapped Circuit)" n
      | Netlist (n, m, _) ->
          Printf.sprintf "netlist %s (%d cells, flat structural)" n
            (List.length m.instances)
    in
    (k ^ "\t" ^ kind) :: acc
  ) lhash [] in
  String.concat "\n" (List.sort compare lst)

(* ──────────────────────────────────────────────────────────────────
 * GUI hooks — populated by sv_gui.ml at startup so the embedded Lua
 * interpreter can drive lablgtk3 widgets. CLI users (sv_suite,
 * sv_main_unified, …) never set these, so the gui.* Lua functions
 * collapse to no-ops there. Hooks stay primitive (string/unit) so this
 * file does NOT depend on lablgtk3. *)

let gui_add_menu_hook       : (string -> unit) ref = ref (fun _ -> ())
let gui_add_item_hook       : (string -> string -> string -> unit) ref =
  ref (fun _ _ _ -> ())
let gui_set_text_hook       : (string -> unit) ref = ref (fun _ -> ())
let gui_get_text_hook       : (unit -> string) ref = ref (fun () -> "")
let gui_append_text_hook    : (string -> unit) ref = ref (fun _ -> ())
let gui_message_hook        : (string -> unit) ref = ref (fun s -> print_endline s)
let gui_error_hook          : (string -> unit) ref =
  ref (fun s -> prerr_endline s)
let gui_open_file_hook      : (unit -> string) ref = ref (fun () -> "")
let gui_save_file_hook      : (unit -> string) ref = ref (fun () -> "")
let gui_set_status_hook     : (string -> unit) ref = ref (fun _ -> ())
let gui_quit_hook           : (unit -> unit) ref = ref (fun () -> ())

let lgui_add_menu  name      = !gui_add_menu_hook name; ""
let lgui_add_item  m l h     = !gui_add_item_hook m l h; ""
let lgui_set_text  s         = !gui_set_text_hook s; ""
let lgui_get_text  ()        = !gui_get_text_hook ()
let lgui_append_text s       = !gui_append_text_hook s; ""
let lgui_message   s         = !gui_message_hook s; ""
let lgui_error     s         = !gui_error_hook s; ""
let lgui_open_file ()        = !gui_open_file_hook ()
let lgui_save_file ()        = !gui_save_file_hook ()
let lgui_set_status s        = !gui_set_status_hook s; ""
let lgui_quit      ()        = !gui_quit_hook (); ""

(* ──────────────────────────────────────────────────────────────────
 * lua-ml interpreter setup. Boilerplate copied from
 * hardcaml-lua/myluaclient.ml; the Char/Pair user-types are kept so the
 * standard library combine works, but we don't expose them in scripts. *)

module LuaChar = struct
  type 'a t       = char
  let tname       = "char"
  let eq _        = fun x y -> x = y
  let to_string   = fun _ c -> String.make 1 c
end

module Pair = struct
  type 'a t       = 'a * 'a
  let tname       = "pair"
  let eq _        = fun x y -> x = y
  let to_string   = fun f (x, y) -> Printf.sprintf "(%s,%s)" (f x) (f y)
end

module T =
  Lua.Lib.Combine.T3
    (LuaChar)
    (Pair)
    (Luaiolib.T)

module LuaCharT = T.TV1
module PairT    = T.TV2
module LuaioT   = T.TV3

module MakeLib
    (CharV : Lua.Lib.TYPEVIEW with type 'a t = 'a LuaChar.t)
    (PairV : Lua.Lib.TYPEVIEW with type 'a t = 'a Pair.t
                              and  type 'a combined = 'a CharV.combined)
  : Lua.Lib.USERCODE with type 'a userdata' = 'a CharV.combined = struct

  type 'a userdata' = 'a PairV.combined
  module M (C : Lua.Lib.CORE with type 'a V.userdata' = 'a userdata') = struct
    module V = C.V
    let ( **-> )  = V.( **-> )
    let ( **->> ) x y = x **-> V.result y

    let wrap1 f a   = try f a   with e -> Printexc.print_backtrace stdout; raise e
    let wrap2 f a b = try f a b with e -> Printexc.print_backtrace stdout; raise e
    let wrap3 f a b c   = try f a b c   with e -> Printexc.print_backtrace stdout; raise e
    let wrap4 f a b c d = try f a b c d with e -> Printexc.print_backtrace stdout; raise e

    let init g =
      C.register_module "svd" [
        "parse",      V.efunc (V.string **-> V.string **-> V.list V.string
                               **->> V.string)
                       (wrap3 lparse);
        "parse_all",  V.efunc (V.string **-> V.list V.string
                               **->> V.string)
                       (wrap2 lparse_all);
        "pick",       V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lpick);
        "miter",      V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lmiter);
        "liberty",    V.efunc (V.string **->> V.string)
                       (wrap1 lliberty);
        "expand",     V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lexpand);
        "gate_miter", V.efunc (V.string **-> V.string **-> V.string
                               **-> V.string **->> V.string)
                       (wrap4 lgate_miter);
        "bir",        V.efunc (V.string **->> V.string) (wrap1 lbir);
        "insts",      V.efunc (V.string **->> V.string) (wrap1 linsts);
        "timing",     V.efunc (V.string **-> V.int **->> V.string)
                       (wrap2 ltiming);
        "name",       V.efunc (V.string **->> V.string) (wrap1 lname);
        "items",      V.efunc (V.unit **->> V.string)   (wrap1 litems);
        "emit_verilog",  V.efunc (V.string **->> V.string) (wrap1 lemit_verilog);
        "emit_vhdl",     V.efunc (V.string **->> V.string) (wrap1 lemit_vhdl);
        "write_verilog", V.efunc (V.string **-> V.string **->> V.string)
                          (wrap2 lwrite_verilog);
        "write_vhdl",    V.efunc (V.string **-> V.string **->> V.string)
                          (wrap2 lwrite_vhdl);
        "convert_hdl",   V.efunc (V.string **-> V.string **->> V.string)
                          (wrap2 lconvert_hdl);

        (* ──── Generic behavioural passes (ASIC and FPGA flows) ──── *)
        "unroll",         V.efunc (V.string **->> V.string) (wrap1 lunroll);
        "inline",         V.efunc (V.string **->> V.string) (wrap1 linline);
        "iflift",         V.efunc (V.string **->> V.string) (wrap1 liflift);
        "blocking_subst", V.efunc (V.string **->> V.string)
                           (wrap1 lblocking_subst);
        "meminfer",       V.efunc (V.string **->> V.string) (wrap1 lmeminfer);
        "memlower",       V.efunc (V.string **->> V.string) (wrap1 lmemlower);
        "ssa",            V.efunc (V.string **->> V.string) (wrap1 lssa);
        "flatten_z3",     V.efunc (V.string **-> V.string **->> V.string)
                           (wrap2 lflatten_z3);
        "flatten_struct", V.efunc (V.string **-> V.string **->> V.string)
                           (wrap2 lflatten_struct);
        "module_names",   V.efunc (V.string **->> V.string)
                           (wrap1 lmodule_names);
        "splice",         V.efunc (V.string **-> V.string **-> V.string
                                   **->> V.string)
                           (wrap3 lsplice);
        "parse_v_cells",  V.efunc (V.string **->> V.string)
                           (wrap1 lparse_v_cells);
        "flatten",        V.efunc (V.string **->> V.string) (wrap1 lflatten);
        "optimize",       V.efunc (V.string **->> V.string) (wrap1 loptimize);
        "ffrip",          V.efunc (V.string **->> V.string) (wrap1 lffrip);
        "register_analyse", V.efunc (V.string **->> V.string)
                             (wrap1 lregister_analyse);
        "cdc_analyse",    V.efunc (V.string **->> V.string)
                           (wrap1 lcdc_analyse);
        "prep_for_z3",    V.efunc (V.string **->> V.string)
                           (wrap1 lprep_for_z3);
        "owner",          V.efunc (V.string **->> V.string) (wrap1 lowner);
        "read_edif",      V.efunc (V.string **->> V.string) (wrap1 lread_edif);
        "read_edif_structural",
                          V.efunc (V.string **->> V.string)
                            (wrap1 lread_edif_structural);

        (* ──────── FPGA-specific (Fpga_synth + nextpnr-xilinx) ──── *)
        "gate_map",       V.efunc (V.string **-> V.int **-> V.int
                                   **->> V.string)
                           (wrap3 lgate_map);
        "write_cellmapped_v", V.efunc (V.string **-> V.string **->> V.string)
                               (wrap2 lwrite_cellmapped_v);
        "write_mapped_json",  V.efunc (V.string **-> V.string **->> V.string)
                               (wrap2 lwrite_mapped_json);
        "write_nextpnr_json", V.efunc (V.string **-> V.string **->> V.string)
                               (wrap2 lwrite_nextpnr_json);
        "write_edif",         V.efunc (V.string **-> V.string **->> V.string)
                               (wrap2 lwrite_edif);
        "write_mod_edif",     V.efunc (V.string **-> V.string **->> V.string)
                               (wrap2 lwrite_mod_edif);
        "write_netlist_edif", V.efunc (V.string **-> V.string **->> V.string)
                               (wrap2 lwrite_netlist_edif);
        "write_netlist_verilog", V.efunc (V.string **-> V.string **->> V.string)
                                  (wrap2 lwrite_netlist_verilog);
        "expand_primitives_for_z3", V.efunc (V.string **->> V.string)
                               (wrap1 lexpand_primitives_for_z3);
        "augment_prog_with_primitives", V.efunc (V.string **->> V.string)
                               (wrap1 laugment_prog_with_primitives);
        "augment_xil_models", V.efunc (V.string **->> V.string)
                               (wrap1 laugment_xil_models);
        "read_nextpnr_json", V.efunc (V.string **->> V.string)
                               (wrap1 lread_nextpnr_json);
        "route_check", V.efunc (V.string **->> V.string)
                               (wrap1 lroute_check);
        "xil_models_coverage", V.efunc (V.string **->> V.string)
                               (wrap1 lxil_models_coverage);
        "mapped_to_prog",     V.efunc (V.string **->> V.string)
                               (wrap1 lmapped_to_prog);
      ] g;
      C.register_module "gui" [
        "add_menu",    V.efunc (V.string **->> V.string) (wrap1 lgui_add_menu);
        "add_item",    V.efunc (V.string **-> V.string **-> V.string
                                **->> V.string)
                       (wrap3 lgui_add_item);
        "set_text",    V.efunc (V.string **->> V.string) (wrap1 lgui_set_text);
        "get_text",    V.efunc (V.unit **->> V.string)   (wrap1 lgui_get_text);
        "append_text", V.efunc (V.string **->> V.string)
                       (wrap1 lgui_append_text);
        "message",     V.efunc (V.string **->> V.string) (wrap1 lgui_message);
        "error",       V.efunc (V.string **->> V.string) (wrap1 lgui_error);
        "open_file",   V.efunc (V.unit **->> V.string)   (wrap1 lgui_open_file);
        "save_file",   V.efunc (V.unit **->> V.string)   (wrap1 lgui_save_file);
        "set_status",  V.efunc (V.string **->> V.string)
                       (wrap1 lgui_set_status);
        "quit",        V.efunc (V.unit **->> V.string)   (wrap1 lgui_quit);
      ] g
  end
end

module W = Lua.Lib.WithType (T)
module C =
  Lua.Lib.Combine.C5
    (Luaiolib.Make (LuaioT))
    (Luacamllib.Make (LuaioT))
    (W (Luastrlib.M))
    (W (Luamathlib.M))
    (MakeLib (LuaCharT) (PairT))

module I =
  Lua.MakeInterp
    (Lua.Parser.MakeStandard)
    (Lua.MakeEval (T) (C))

(* Public entry: run a Lua script file and return its exit code (0 on
 * success, 1 on Lua exception). *)
let run_script ?(args : string list = []) path =
  if not (Sys.file_exists path) then begin
    Printf.eprintf "lua script not found: %s\n" path;
    1
  end else begin
    let ic = open_in path in
    let buf = Buffer.create 1024 in
    (try while true do
       Buffer.add_channel buf ic 4096
     done with End_of_file -> ());
    close_in ic;
    let state = I.mk () in
    (* Pre-populate ARGV global from [args] so recipes can read the
     * shell-script-style positional arguments via ARGV[1], ARGV[2], ….
     * Lua 2.5-style table literal — we build it as a string of source
     * code and dostring before running the recipe.
     *
     * ARGN is a convenience holding the count, since lua-ml's getn
     * helpers aren't in the table namespace. *)
    let argv_src =
      let parts = List.mapi (fun i a ->
        let esc = String.concat ""
          (List.map (fun c ->
             if c = '"' then "\\\""
             else if c = '\\' then "\\\\"
             else String.make 1 c)
            (List.init (String.length a) (String.get a))) in
        Printf.sprintf "ARGV[%d] = \"%s\"" (i + 1) esc) args in
      "ARGV = {}\n" ^
      String.concat "\n" parts ^
      Printf.sprintf "\nARGN = %d\n" (List.length args)
    in
    try
      ignore (I.dostring state argv_src);
      ignore (I.dostring state (Buffer.contents buf));
      0
    with e ->
      Printf.eprintf "lua: %s\n" (Printexc.to_string e);
      1
  end
