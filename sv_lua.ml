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
  (* For a FRONTEND miter (SYNLIG_MIN) skip yosys `opt` — it merges/removes FFs so
     the register SET no longer matches the RTL, defeating register correspondence.
     `proc` + `flatten` (+ opt_clean = unused-cell removal, no FF merge) keeps one
     $dff per source register, isolating Surelog's elaboration from synthesis opt. *)
  if Sys.getenv_opt "SYNLIG_MIN" <> None then
    Printf.fprintf oc "hierarchy -top %s\nproc\nflatten\nopt_clean\n" top
  else
    Printf.fprintf oc "hierarchy -top %s\nproc\nopt -fast\nflatten\nopt -fast\n" top;
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

(* Guard against a stale Verilator.  Versions < 5.048 mis-emit packed-struct
 * ports in --json-only (the self-reference dtype collapse that flattened
 * dmi_req_i/dmi_resp_o to a single bit), producing a silently-wrong IR.  A
 * too-old `verilator` on PATH (e.g. a lingering /usr/local 5.024 alongside a
 * newer $HOME build) is a real point of confusion — fail loudly instead of
 * miter-ing against garbage.  Set VERILATOR_MIN_OK=0 to bypass. *)
let verilator_min = (5, 48)
let check_verilator_version () =
  if Sys.getenv_opt "VERILATOR_MIN_OK" = Some "0" then ()
  else begin
    let line =
      try
        let ic = Unix.open_process_in "verilator --version 2>/dev/null" in
        let l = (try input_line ic with End_of_file -> "") in
        ignore (Unix.close_process_in ic); l
      with _ -> "" in
    (* "Verilator 5.050 2026-07-01 rev v5.050" -> (5, 50) *)
    let ver =
      try Scanf.sscanf line "Verilator %d.%d" (fun a b -> Some (a, b))
      with _ -> None in
    match ver with
    | Some (maj, min) ->
        let (rmaj, rmin) = verilator_min in
        if (maj, min) < (rmaj, rmin) then
          failwith (Printf.sprintf
            "verilator on PATH is %d.%03d but the struct-port JSON fix needs \
             >= %d.%03d; upgrade or fix PATH (or set VERILATOR_MIN_OK=0 to override). \
             Got: %S" maj min rmaj rmin line)
    | None ->
        failwith (Printf.sprintf
          "could not determine verilator version (need >= %d.%03d); is verilator \
           on PATH?  Got: %S (set VERILATOR_MIN_OK=0 to override)"
          (fst verilator_min) (snd verilator_min) line)
  end

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
      check_verilator_version ();
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
(* Cut every STATEFUL / ANALOGUE / UNKNOWN black-box instance in the picked
 * top module into external I/O with TIED variables, so the miter can compare
 * the logic around it instead of bailing INCONCLUSIVE:
 *   - each black-box OUTPUT net  -> a primary INPUT  (the miter constrains
 *     common inputs equal by name, so the read is the SAME free variable in
 *     both designs — a "tied variable" shared across the two flows);
 *   - each black-box INPUT net   -> a primary OUTPUT (compared by name, so
 *     both flows must drive the box identically: address/data/we, GT config…).
 * A box is cut when it is (a) not a user module, (b) has no combinational
 * model in Xil_prim_models (so it is stateful RAM / SRL / GT / MMCM / …), and
 * (c) has a known port-direction surface.  This mirrors how Behavioral_ffrip
 * turns an FF's Q into a tied input and its D into a compared output.
 * Disable with CUT_BLACKBOX=0. *)
let cut_blackboxes ?(trusted : string list = []) (m : bmodule) (p : bprogram) : bmodule =
  let open Behavioral_ir in
  if (match Sys.getenv_opt "CUT_BLACKBOX" with Some "0" -> true | _ -> false)
  then m
  else begin
    let is_user name = List.exists (fun (mm : bmodule) -> mm.name = name) p.modules in
    let is_trusted name = List.mem name trusted in
    (* Port directions for a TRUSTED user submodule come from its own
     * declared signal directions (module ports keep RTL names across flows,
     * so these tied/compared boundary nets match name-to-name between the two
     * designs — the whole point of comparing bottom-up at module ports). *)
    let user_port_dirs name =
      match List.find_opt (fun (mm : bmodule) -> mm.name = name) p.modules with
      | None -> None
      | Some mm -> Some (List.filter_map (fun (s : bsignal) ->
          match s.direction with
          | `Input  -> Some (s.name, `Input,  0)
          | `Output -> Some (s.name, `Output, 0)
          | `Internal -> None) mm.signals) in
    let port_dirs name =
      if is_trusted name then user_port_dirs name
      else Hashtbl.find_opt (Lazy.force Bir_to_edif.xil_json_ports) name in
    (* Distributed-RAM (RAM32X1D/RAM32M/RAM64M/RAM*X1S/…) are absent from the
     * Xilinx port-JSON oracle, but their pin directions are unambiguous by
     * name: SPO/DPO/O/DO* are the async read outputs, everything else feeds
     * the write/address side. *)
    let is_dram name =
      let u = String.uppercase_ascii name in
      String.length u >= 5 && String.sub u 0 3 = "RAM" && u.[3] <> 'B' in
    let dram_dir pin : [ `Input | `Output ] =
      let u = String.uppercase_ascii pin in
      let pre s = String.length u >= String.length s && String.sub u 0 (String.length s) = s in
      if u = "SPO" || u = "DPO" || u = "O" || pre "DO" then `Output else `Input in
    (* Bidirectional / pad primitives absent from the oracle: the fabric-side
     * output pin (O) is a read from the pad → tied input; I/T are driven by
     * fabric → compared outputs; the IO pad itself is external → ignored. *)
    let is_iopad name = name = "IOBUF" || name = "IOBUFDS" in
    let iopad_dir pin : [ `Input | `Output ] = if pin = "O" then `Output else `Input in
    (* pin-direction oracle: JSON, then RAM, then IO-pad fallback *)
    let pin_dir name pin : [ `Input | `Output ] option =
      match port_dirs name with
      | Some ps -> List.find_map (fun (pn, d, _) -> if pn = pin then Some d else None) ps
      | None ->
          if is_dram name then Some (dram_dir pin)
          else if is_iopad name && pin <> "IO" && pin <> "IOB" then Some (iopad_dir pin)
          else None in
    let cutable (i : binstance) =
      if is_trusted i.module_name then true (* proven-equivalent submodule → black-box *)
      else
        (not (is_user i.module_name))
        && (match Xil_prim_models.synth i with None -> true | Some _ -> false)
        && (port_dirs i.module_name <> None || is_dram i.module_name
            || is_iopad i.module_name) in
    let rec bases = function
      | BVar n -> [ n ]
      | BSlice { signal; _ } -> bases signal
      | BSelect { array; _ } -> bases array
      | BConcat es -> List.concat (List.map bases es)
      | BReplicate { value; _ } -> bases value
      | _ -> [] in
    let cut_insts, keep_insts = List.partition cutable m.instances in
    if cut_insts = [] then m
    else begin
      (* width of a connected expression, from the base net's declaration *)
      let sigw = Hashtbl.create 128 in
      List.iter (fun (s : bsignal) ->
        let w = match s.stype with BInt { width; _ } -> width | BBool -> 1 | _ -> 0 in
        if w > 0 then Hashtbl.replace sigw s.name w) m.signals;
      let expr_w e = match bases e with
        | n :: _ -> (match Hashtbl.find_opt sigw n with Some w -> w | None -> 1)
        | [] -> 1 in
      (* Each cut cell's pin becomes a boundary signal named <inst>/<pin> — a
       * CANONICAL name (RTL instance + primitive port), identical across the
       * two flows, so the miter's by-name input-equality / output-comparison
       * actually ties the two designs (a connected-net name would differ). *)
      let new_sigs = ref [] and new_assigns = ref [] in
      let n_in = ref 0 and n_out = ref 0 in
      let mk_sig name w dir =
        new_sigs := { name; stype = BInt { width = w; signed = Unsigned };
                      direction = dir; initial_value = None; attrs = [] } :: !new_sigs in
      List.iter (fun (i : binstance) ->
        List.iter (fun (pin, expr) ->
          match pin_dir i.module_name pin with
          | Some dir ->
              (* Strip gate_map's numeric uniquing suffix (`cpu_mmcm_451`) so
                 the boundary name pairs with the other flow's RTL instance
                 name (`cpu_mmcm`) — an unpaired box boundary leaves both
                 sides' tied inputs as INDEPENDENT frees and every cone fed
                 by the box (the MMCM's cpu_clk!) spuriously differs. *)
              let inst_canon =
                let n = i.inst_name in
                match String.rindex_opt n '_' with
                | Some k when k > 0 && k < String.length n - 1
                    && (let ok = ref true in
                        String.iteri (fun j c ->
                          if j > k && not (c >= '0' && c <= '9') then ok := false) n;
                        !ok) ->
                    String.sub n 0 k
                | _ -> n in
              let bname = inst_canon ^ "/" ^ pin in
              let w = expr_w expr in
              (match dir with
               | `Output ->
                   (* cell drives the net → tie it to a shared free INPUT and
                      buffer that input onto the original net so downstream
                      logic reads the tied variable. *)
                   mk_sig bname w `Input; incr n_in;
                   (match expr with
                    | BVar n -> new_assigns := BAssign { lhs = n; rhs = BVar bname } :: !new_assigns
                    | _ -> ())
               | `Input ->
                   (* cell reads the net → expose what drives it as a compared
                      OUTPUT, so both flows must compute it identically. *)
                   mk_sig bname w `Output; incr n_out;
                   new_assigns := BAssign { lhs = bname; rhs = expr } :: !new_assigns)
          | None -> ()) i.port_connections) cut_insts;
      let cut_proc = BCombinational
        { name = "__blackbox_cut"; sensitivity = []; body = List.rev !new_assigns } in
      Printf.eprintf
        "[cut_blackbox] %s: cut %d black boxes -> %d tied inputs + %d compared outputs (cell/pin named)\n"
        m.name (List.length cut_insts) !n_in !n_out;
      { m with instances = keep_insts;
               signals = m.signals @ List.rev !new_sigs;
               processes = m.processes @ [ cut_proc ] }
    end
  end

(* Miter-only bitbus resolution.  of_circuit (mapped_to_prog) emits a multi-bit
   INPUT port `base : uint<W>` but wires its consumers to per-bit nets
   `base__0 .. base__{W-1}` that are reconnected ONLY by the bitbus convention
   bir_to_nextpnr_json / bir_to_edif apply — the flatten/ffrip/z3 path never
   drives them, so they float free and the miter spuriously DIFFERs on ANY
   vector-input design.  Rewrite every read of `base__i` to `base[i:i]` so the
   port bits reach the consumers.  This touches only the miter's flattened view;
   of_circuit itself is unchanged (its output also feeds the silicon nextpnr
   path, which resolves the bitbus its own way). *)
let resolve_input_bitbus (m : bmodule) : bmodule =
  (* inw = multi-bit INPUT ports (port bitbus `base__<i>`);
     allw = every multi-bit signal (register bitbus `base__b<i>`, produced by
     fpga_map's per-bit register split + ffpack's re-pack). *)
  let inw : (string, int) Hashtbl.t = Hashtbl.create 16 in
  let allw : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    let w = Behavioral_boundary.width_of_btype s.stype in
    (* allw includes WIDTH-1 signals: a single-FF register still packs to a
       width-1 bus (Vivado pruned a_q to one FF), and its `a_q__b0` readers
       must resolve to `a_q[0:0]` — excluded from allw they'd tie to 0. *)
    Hashtbl.replace allw s.name w;
    if w > 1 && s.direction = `Input then Hashtbl.replace inw s.name w) m.signals;
  if Hashtbl.length allw = 0 then m
  else begin
    (* Canonicalise bracket-string WRITES `base[i] := e` (the expand path's
       bus-bit outputs: `p_0_in__0[0] := lut`, frontend per-bit `acc_ins[9]
       := …`) onto the obuf convention `obuf_<base>_<i>__O` when `base` is a
       DECLARED bus — out_drivers below then reconstructs the bus, and
       split_bracket redirects bracket READS to it, so string-writers and
       slice-readers meet on ONE namespace instead of a phantom-scalar /
       floating-bus split. *)
    let bw_re = Str.regexp "^\\(.+\\)\\[\\([0-9]+\\)\\]$" in
    let bw_sigs = ref [] in
    let bw_lhs lhs =
      if Str.string_match bw_re lhs 0 then begin
        let base = Str.matched_group 1 lhs in
        let bit = Str.matched_group 2 lhs in
        match Hashtbl.find_opt allw base, int_of_string_opt bit with
        | Some w, Some b when b >= 0 && b < w ->
            let ob = Printf.sprintf "obuf_%s_%s__O" base bit in
            bw_sigs := { name = ob;
                         stype = BInt { width = 1; signed = Unsigned };
                         direction = `Internal; initial_value = None;
                         attrs = [] } :: !bw_sigs;
            ob
        | _ -> lhs
      end else lhs in
    let rec bw_stmt s = match s with
      | BAssign r -> BAssign { r with lhs = bw_lhs r.lhs }
      | BIf r -> BIf { r with then_stmts = List.map bw_stmt r.then_stmts;
                              else_stmts = List.map bw_stmt r.else_stmts }
      | BCase r -> BCase { r with cases = List.map (fun (g, b) ->
                                    (g, List.map bw_stmt b)) r.cases;
                                  default = List.map bw_stmt r.default }
      | BWhile r -> BWhile { r with body = List.map bw_stmt r.body }
      | BFor r -> BFor { r with body = List.map bw_stmt r.body }
      | BBlock b -> BBlock (List.map bw_stmt b)
      | _ -> s in
    let m = { m with
              processes = List.map (function
                | BCombinational r ->
                    BCombinational { r with body = List.map bw_stmt r.body }
                | BSequential r ->
                    BSequential { r with body = List.map bw_stmt r.body })
                m.processes;
              signals = !bw_sigs @ m.signals } in
    let all_digits s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s in
    (* register bitbus: rightmost "__b<digits>" whose base is any multi-bit sig *)
    let split_reg name =
      let n = String.length name in
      let rec find i =
        if i < 0 then None
        else if i + 2 < n && name.[i] = '_' && name.[i+1] = '_' && name.[i+2] = 'b'
                && all_digits (String.sub name (i+3) (n-i-3))
        then
          let base = String.sub name 0 i in
          let idx = int_of_string (String.sub name (i+3) (n-i-3)) in
          (match Hashtbl.find_opt allw base with
           | Some w when idx >= 0 && idx < w -> Some (base, idx)
           | _ -> find (i - 1))
        else find (i - 1) in
      find (n - 4) in
    (* port bitbus: rightmost "__<digits>" whose base is a multi-bit input port *)
    let split_port name =
      let n = String.length name in
      let rec last_dd i =
        if i < 0 then None
        else if name.[i] = '_' && i + 1 < n && name.[i+1] = '_' then Some i
        else last_dd (i - 1) in
      match last_dd (n - 2) with
      | None -> None
      | Some k ->
          let base = String.sub name 0 k in
          let suf = String.sub name (k + 2) (n - k - 2) in
          (match int_of_string_opt suf, Hashtbl.find_opt inw base with
           | Some i, Some w when i >= 0 && i < w -> Some (base, i)
           | _ -> None) in
    (* bracket bit read `base[i]` of a DECLARED bus redirects to the bus —
       sound only because bracket-string WRITES are canonicalised onto the
       obuf convention below, so the bus is reconstructed and reader/writer
       meet on the bus (redirecting reads alone severed them from string
       writers — rx_axis_packer's cnt increment read a free bus). *)
    let split_bracket name =
      let n = String.length name in
      if n >= 3 && name.[n-1] = ']' then
        match String.rindex_opt name '[' with
        | Some lb when lb > 0 ->
            let base = String.sub name 0 lb in
            let inner = String.sub name (lb+1) (n-lb-2) in
            (match int_of_string_opt inner, Hashtbl.find_opt allw base with
             | Some i, Some w when i >= 0 && i < w -> Some (base, i)
             | _ -> None)
        | _ -> None
      else None in
    let split_bitbus name =
      match split_reg name with
      | Some r -> Some r
      | None -> (match split_port name with
                 | Some r -> Some r | None -> split_bracket name) in
    (* Set of nets that HAVE a driver: ports, every process assign target, and
       (conservatively) every net touched by a surviving black-box instance.  A
       read of anything else is an unconnected pin, which Xilinx defaults to 0 —
       e.g. a CARRY4 whose CI is omitted (only CYINIT wired), or a floating GND/
       VCC.  Undriven, they float free and the miter reports spurious diffs. *)
    let driven : (string, unit) Hashtbl.t = Hashtbl.create 256 in
    List.iter (fun (s : bsignal) ->
      match s.direction with
      | `Input | `Output -> Hashtbl.replace driven s.name ()
      | _ -> ()) m.signals;
    let rec add_lhs = function
      | BAssign { lhs; _ } -> Hashtbl.replace driven lhs ()
      | BIf { then_stmts; else_stmts; _ } ->
          List.iter add_lhs then_stmts; List.iter add_lhs else_stmts
      | BCase { cases; default; _ } ->
          List.iter (fun (_, b) -> List.iter add_lhs b) cases;
          List.iter add_lhs default
      | BWhile { body; _ } -> List.iter add_lhs body
      | BFor { init; update; body; _ } ->
          add_lhs init; add_lhs update; List.iter add_lhs body
      | BBlock b -> List.iter add_lhs b
      (* array / part-select writes drive their first argument — without this the
         undriven-tie below wrongly grounds a @mem_write-updated register. *)
      | BCallStmt { func; args }
        when func = "@mem_write" || func = "@slice_write"
             || func = "@part_sel_write_up" || func = "@part_sel_write_down" ->
          (match args with BVar arr :: _ -> Hashtbl.replace driven arr () | _ -> ())
      | BCallStmt _ | BReturn _ -> () in
    List.iter (function
      | BCombinational { body; _ } -> List.iter add_lhs body
      | BSequential { body; _ } -> List.iter add_lhs body) m.processes;
    let rec add_vars = function
      | BVar n -> Hashtbl.replace driven n ()
      | BConst _ -> ()
      | BBinOp { lhs; rhs; _ } -> add_vars lhs; add_vars rhs
      | BUnOp { operand; _ } -> add_vars operand
      | BSelect { array; index } -> add_vars array; add_vars index
      | BSlice { signal; _ } -> add_vars signal
      | BConcat es -> List.iter add_vars es
      | BReplicate { value; _ } -> add_vars value
      | BCond { condition; then_val; else_val } ->
          add_vars condition; add_vars then_val; add_vars else_val
      | BCall { args; _ } -> List.iter add_vars args in
    List.iter (fun (i : binstance) ->
      List.iter (fun (_, e) -> add_vars e) i.port_connections) m.instances;
    let c0 = BConst { value = Z.zero; width = 1 } in
    let c1 = BConst { value = Z.one; width = 1 } in
    let rec rw e = match e with
      | BVar "GND" -> c0
      | BVar "VCC" -> c1
      | BVar n ->
          (match split_bitbus n with
           | Some (base, i) -> BSlice { signal = BVar base; msb = i; lsb = i }
           | None ->
               (* ufo__* child-boundary outputs must stay FREE variables —
                  the miter pairs them by name across sides (assume half of
                  the child cut).  Zero-tying them made every child-fed cone
                  vacuously 0=0 "equivalent". *)
               if Hashtbl.mem driven n then e
               else if String.length n > 5 && String.sub n 0 5 = "ufo__" then e
               else c0)
      | BConst _ -> e
      | BBinOp r -> BBinOp { r with lhs = rw r.lhs; rhs = rw r.rhs }
      | BUnOp r -> BUnOp { r with operand = rw r.operand }
      | BSelect r -> BSelect { array = rw r.array; index = rw r.index }
      | BSlice r -> BSlice { r with signal = rw r.signal }
      | BConcat es -> BConcat (List.map rw es)
      | BReplicate r -> BReplicate { r with value = rw r.value }
      | BCond r -> BCond { condition = rw r.condition;
                           then_val = rw r.then_val; else_val = rw r.else_val }
      | BCall r -> BCall { r with args = List.map rw r.args } in
    let rec rws s = match s with
      | BAssign r -> BAssign { r with rhs = rw r.rhs }
      | BIf r -> BIf { condition = rw r.condition;
                       then_stmts = List.map rws r.then_stmts;
                       else_stmts = List.map rws r.else_stmts }
      | BCase r -> BCase { selector = rw r.selector;
                           cases = List.map (fun (c, b) -> (rw c, List.map rws b)) r.cases;
                           default = List.map rws r.default }
      | BWhile r -> BWhile { condition = rw r.condition; body = List.map rws r.body }
      | BFor r -> BFor { init = rws r.init; condition = rw r.condition;
                         update = rws r.update; body = List.map rws r.body }
      | BBlock b -> BBlock (List.map rws b)
      | BCallStmt r -> BCallStmt { r with args = List.map rw r.args }
      | BReturn eo -> BReturn (Option.map rw eo) in
    let rwp = function
      | BCombinational r -> BCombinational { r with body = List.map rws r.body }
      | BSequential r -> BSequential { r with body = List.map rws r.body } in
    (* OUTPUT side: of_circuit drives a multi-bit output port `base` through
       per-bit buffer instances `obuf_<base>_<i>` whose O pin connects to the bus
       SLICE `base[i:i]`.  flatten_for_z3 cannot bind an instance output to a
       slice (a BAssign lhs is a whole signal), so each buffer's value lands in
       the net `obuf_<base>_<i>__O` and `base` itself floats.  Rebuild the whole
       port as a concat of its per-bit buffer nets (MSB first), when they all
       exist.  Scalar outputs use a bare-BVar O connection that flatten already
       resolves, so they need nothing here. *)
    let sig_names = Hashtbl.create 256 in
    List.iter (fun (s : bsignal) -> Hashtbl.replace sig_names s.name ())
      m.signals;
    (* A registered output (packed FF whose bus IS the output port, e.g. a
       registered `count`) is already driven by its BSequential — do NOT also
       rebuild it from the per-bit obuf nets, or the port gets two drivers and
       the miter reads an inconsistent value.  This matters once cross-flow
       FF-name canonicalisation makes a Vivado FDRE-driven output bus pack. *)
    let seq_driven : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    List.iter (function
      | BSequential { body; _ } ->
          List.iter (function
            | BAssign { lhs; _ } -> Hashtbl.replace seq_driven lhs ()
            | _ -> ()) body
      | _ -> ()) m.processes;
    let out_drivers = List.filter_map (fun (s : bsignal) ->
      (* Reconstruct any multi-bit signal — OUTPUT port OR INTERNAL bus (e.g.
         Vivado's `p_0_in` next-state D-bus) — whose bits were each driven
         through a per-bit `obuf_<sig>_<i>__O` net by the bus-bit inline fanout.
         Skip registered outputs (already driven by their BSequential).  Gated on
         all obuf nets existing, so it only fires where the fanout created them. *)
      match s.direction with
      | (`Output | `Internal) when not (Hashtbl.mem seq_driven s.name) ->
          let w = Behavioral_boundary.width_of_btype s.stype in
          if w <= 1 then None
          else begin
            let nets = List.init w (fun i ->
              Printf.sprintf "obuf_%s_%d__O" s.name i) in
            (* PARTIAL buses reconstruct too: Vivado leaves a bus bit undriven
               when nothing reads it (`wire [2:0] ^a` with only [0],[2] driven)
               — requiring ALL bits made the whole bus tie to 0.  Missing bits
               get 0 (undriven-and-unread, value irrelevant). *)
            if List.exists (Hashtbl.mem sig_names) nets
            then Some (BAssign { lhs = s.name;
                                 rhs = BConcat (List.rev_map (fun n ->
                                   if Hashtbl.mem sig_names n then BVar n
                                   else BConst { value = Z.zero; width = 1 }) nets) })
            else None
          end
      | _ -> None) m.signals in
    (* Register the reconstructed buses as DRIVEN before rw rewrites the
       existing processes — `driven` was built from m.processes alone, so a read
       of a reconstructed bus (an FDRE's `.D(p_0_in[i])`) would otherwise still
       be tied to 0 even though `__out_bitbus` is about to drive it.  `rw` reads
       the mutable `driven` table at apply time, so adding entries here is
       sufficient. *)
    List.iter (function
      | BAssign { lhs; _ } -> Hashtbl.replace driven lhs ()
      | _ -> ()) out_drivers;
    let processes' = List.map rwp m.processes in
    let processes' =
      if out_drivers = [] then processes'
      else processes' @ [ BCombinational { name = "__out_bitbus";
                                           sensitivity = [BAny];
                                           body = out_drivers } ] in
    { m with
      processes = processes';
      instances = List.map (fun (i : binstance) ->
        { i with port_connections =
                   List.map (fun (p, e) -> (p, rw e)) i.port_connections }) m.instances }
  end

(* Cross-flow FF-name alignment.  Vivado's write_verilog names a bit-blasted
   register FF `<base>_reg[<i>]` (and a scalar reg `<base>_reg`), while SVS's
   FPGA_LEC_NAMES uses `<base>__b<i>`.  Rewriting the Vivado form to the SVS form
   lets Behavioral_ffpack re-pack BOTH sides' per-bit FFs into the same bus
   register, so ffrip's per-state Q/D cones line up by name in the cross-flow
   miter (else Vivado stays 4×1-bit `count_reg[i]__Q` vs SVS 1×4-bit `count__Q`).
   Harmless on the SVS side (no `_reg` names) and on same-flow self-miters. *)
let canon_ff_name =
  (* Vivado's FDRE state pin surfaces as `<base>_reg[<i>]__Q` (bit-blasted reg)
     or `<base>_reg__Q` (scalar reg); ffpack packs a BSequential whose LHS ends
     in `<bus>__b<idx>`, so map the FF STATE net to that form (dropping the __Q).
     Non-Q FDRE pins keep their `_reg[i]`→`__b<i>` rewrite for consistency. *)
  let re_q   = Str.regexp "^\\(.+\\)_reg\\[\\([0-9]+\\)\\]__Q$" in
  let re_sq  = Str.regexp "^\\(.+\\)_reg__Q$" in
  let re_bit = Str.regexp "^\\(.+\\)_reg\\[\\([0-9]+\\)\\]\\(__[A-Za-z].*\\)?$" in
  let re_sc  = Str.regexp "^\\(.+\\)_reg\\(__[A-Za-z].*\\)?$" in
  fun n ->
    if Str.string_match re_q n 0 then
      Str.matched_group 1 n ^ "__b" ^ Str.matched_group 2 n
    else if Str.string_match re_sq n 0 then
      Str.matched_group 1 n ^ "__b0"
    else if Str.string_match re_bit n 0 then
      Str.matched_group 1 n ^ "__b" ^ Str.matched_group 2 n
        ^ (try Str.matched_group 3 n with Not_found -> "")
    else if Str.string_match re_sc n 0 then
      Str.matched_group 1 n ^ "__b0"
        ^ (try Str.matched_group 2 n with Not_found -> "")
    else n

let canonicalize_ff_names (m : bmodule) : bmodule =
  let open Behavioral_ir in
  (* WIDTH-AWARE canon: the scalar `<base>_reg` -> `<base>__b0` rule must not
     fire on a MULTI-BIT net that merely ends in `_reg` (Vivado's 8-bit wire
     `rx_word_addr_reg`) — renaming a bus to a BIT name makes every slice
     `[k:k]` of it collapse onto bit 0 downstream (split_reg rewrites the
     __b0 name to bus[0:0]).  Bracketed `<base>_reg[<i>]` forms stay. *)
  let widths : (string, int) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    match s.stype with
    | BInt { width; _ } -> Hashtbl.replace widths s.name width
    | BBool -> Hashtbl.replace widths s.name 1
    | _ -> ()) m.signals;
  let canon_ff_name nm =
    let c = canon_ff_name nm in
    if String.equal c nm then nm
    else
      (* did the SCALAR rule fire? (result ends in __b0 while the source has
         no bracket index) *)
      let scalar_rule =
        not (String.contains nm '[')
        && String.length c > 4
        && String.sub c (String.length c - 4) 4 = "__b0" in
      if scalar_rule
         && (match Hashtbl.find_opt widths nm with
             | Some w -> w > 1 | None -> false)
      then nm else c in
  let rec re_e e = match e with
    | BVar n -> BVar (canon_ff_name n)
    | BConst _ -> e
    | BBinOp r -> BBinOp { r with lhs = re_e r.lhs; rhs = re_e r.rhs }
    | BUnOp r -> BUnOp { r with operand = re_e r.operand }
    | BSelect r -> BSelect { array = re_e r.array; index = re_e r.index }
    | BSlice r -> BSlice { r with signal = re_e r.signal }
    | BConcat es -> BConcat (List.map re_e es)
    | BReplicate r -> BReplicate { r with value = re_e r.value }
    | BCond r -> BCond { condition = re_e r.condition;
                         then_val = re_e r.then_val; else_val = re_e r.else_val }
    | BCall r -> BCall { r with args = List.map re_e r.args } in
  let rec re_s s = match s with
    | BAssign r -> BAssign { lhs = canon_ff_name r.lhs; rhs = re_e r.rhs }
    | BIf r -> BIf { condition = re_e r.condition;
                     then_stmts = List.map re_s r.then_stmts;
                     else_stmts = List.map re_s r.else_stmts }
    | BCase r -> BCase { selector = re_e r.selector;
                         cases = List.map (fun (g,b) -> (re_e g, List.map re_s b)) r.cases;
                         default = List.map re_s r.default }
    | BWhile r -> BWhile { condition = re_e r.condition; body = List.map re_s r.body }
    | BFor r -> BFor { init = re_s r.init; condition = re_e r.condition;
                       update = re_s r.update; body = List.map re_s r.body }
    | BBlock b -> BBlock (List.map re_s b)
    | BCallStmt r -> BCallStmt { r with args = List.map re_e r.args }
    | BReturn eo -> BReturn (Option.map re_e eo) in
  let re_proc = function
    | BCombinational r -> BCombinational { r with body = List.map re_s r.body }
    | BSequential r -> BSequential { r with clock = canon_ff_name r.clock;
                                            reset = Option.map canon_ff_name r.reset;
                                            body = List.map re_s r.body } in
  (* State-name-from-INSTANCE pre-pass.  Vivado can rename an FF's Q NET after
     the output it aliases (`assign a = a_q` → FDRE \a_q_reg[0] with .Q(\^a)),
     so the inlined state lands on the net name `^a` — unrecognisable to the
     name canon.  But the inlined FF process name preserves the INSTANCE
     (`a_q_reg[0]__seq`), which carries the RTL register identity.  Rewrite the
     state write onto `<base>__b<i>` (the ffpack-able form), substitute the
     state read in its own hold branch, and alias the old Q net to the new
     state so downstream readers stay connected. *)
  let re_seq = Str.regexp "^\\(.+\\)_reg\\(\\[\\([0-9]+\\)\\]\\)?__seq$" in
  let canon_bracket_re = Str.regexp "^\\(.+\\)\\[\\([0-9]+\\)\\]$" in
  (* EVICT collisions first: Vivado can reuse a register's BASE name for an
     unrelated live net (rand_26: `wire [0:0] a_q` is the shared reset-LUT
     output feeding each \a_q_reg[i]/.R) — packing the canonicalised states
     into a bus named `a_q` would then collide with it (truncated state, twin
     writers).  Rename such an INTERNAL signal and every reference to
     `<base>__vivnet`; a PORT named base is left alone (that is the ordinary
     registered-output pattern the packer expects). *)
  let bus_bases : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  List.iter (function
    | BSequential { name = pn; _ } when Str.string_match re_seq pn 0 ->
        Hashtbl.replace bus_bases (Str.matched_group 1 pn) ()
    | _ -> ()) m.processes;
  let evict : (string, string) Hashtbl.t = Hashtbl.create 4 in
  List.iter (fun (s : bsignal) ->
    if Hashtbl.mem bus_bases s.name && s.direction = `Internal then
      Hashtbl.replace evict s.name (s.name ^ "__vivnet")) m.signals;
  let m =
    if Hashtbl.length evict = 0 then m
    else begin
      let ev n = match Hashtbl.find_opt evict n with Some n' -> n' | None -> n in
      let rec ee e = match e with
        | BVar n -> BVar (ev n)
        | BConst _ -> e
        | BBinOp r -> BBinOp { r with lhs = ee r.lhs; rhs = ee r.rhs }
        | BUnOp r -> BUnOp { r with operand = ee r.operand }
        | BSelect r -> BSelect { array = ee r.array; index = ee r.index }
        | BSlice r -> BSlice { r with signal = ee r.signal }
        | BConcat es -> BConcat (List.map ee es)
        | BReplicate r -> BReplicate { r with value = ee r.value }
        | BCond r -> BCond { condition = ee r.condition;
                             then_val = ee r.then_val; else_val = ee r.else_val }
        | BCall r -> BCall { r with args = List.map ee r.args } in
      let rec es_ s = match s with
        | BAssign r -> BAssign { lhs = ev r.lhs; rhs = ee r.rhs }
        | BIf r -> BIf { condition = ee r.condition;
                         then_stmts = List.map es_ r.then_stmts;
                         else_stmts = List.map es_ r.else_stmts }
        | BCase r -> BCase { selector = ee r.selector;
                             cases = List.map (fun (g,b) -> (ee g, List.map es_ b)) r.cases;
                             default = List.map es_ r.default }
        | BWhile r -> BWhile { condition = ee r.condition; body = List.map es_ r.body }
        | BFor r -> BFor { init = es_ r.init; condition = ee r.condition;
                           update = es_ r.update; body = List.map es_ r.body }
        | BBlock b -> BBlock (List.map es_ b)
        | BCallStmt r -> BCallStmt { r with args = List.map ee r.args }
        | BReturn eo -> BReturn (Option.map ee eo) in
      { m with
        signals = List.map (fun (s:bsignal) -> { s with name = ev s.name }) m.signals;
        processes = List.map (function
          | BCombinational r -> BCombinational { r with body = List.map es_ r.body }
          | BSequential r -> BSequential { r with clock = ev r.clock;
                                                  reset = Option.map ev r.reset;
                                                  body = List.map es_ r.body })
          m.processes;
        instances = List.map (fun (i:binstance) ->
          { i with port_connections =
                     List.map (fun (p,e) -> (p, ee e)) i.port_connections })
          m.instances }
    end in
  let extra_procs = ref [] and extra_sigs = ref [] in
  let pre_procs = List.map (fun proc ->
    match proc with
    | BSequential ({ name = pn; body = [BAssign { lhs; rhs }]; _ } as r)
      when Str.string_match re_seq pn 0 ->
        let base = Str.matched_group 1 pn in
        let idx = (try Str.matched_group 3 pn with Not_found -> "0") in
        let q_new = base ^ "__b" ^ idx in
        (* A bracket Q onto a DECLARED BUS (`.Q(rx_word_addr_reg[7])` with an
           8-bit wire rx_word_addr_reg) must NOT be skipped even though the
           name canon maps it to q_new: the bus's READERS (LUT cones slicing
           rx_word_addr_reg[k:k]) need the obuf alias + reconstruction to
           reach the renamed state, else the bus floats. *)
        let bus_bracket =
          Str.string_match canon_bracket_re lhs 0
          && (let base = Str.matched_group 1 lhs in
              match Hashtbl.find_opt widths base with
              | Some w -> w > 1 | None -> false) in
        if canon_ff_name lhs = q_new && not bus_bracket then proc
        else begin
          let rec subst e = match e with
            | BVar n when n = lhs -> BVar q_new
            | BVar _ | BConst _ -> e
            | BBinOp rr -> BBinOp { rr with lhs = subst rr.lhs; rhs = subst rr.rhs }
            | BUnOp rr -> BUnOp { rr with operand = subst rr.operand }
            | BSelect rr -> BSelect { array = subst rr.array; index = subst rr.index }
            | BSlice rr -> BSlice { rr with signal = subst rr.signal }
            | BConcat es -> BConcat (List.map subst es)
            | BReplicate rr -> BReplicate { rr with value = subst rr.value }
            | BCond rr -> BCond { condition = subst rr.condition;
                                  then_val = subst rr.then_val;
                                  else_val = subst rr.else_val }
            | BCall rr -> BCall { rr with args = List.map subst rr.args } in
          (* Alias target: when the Q net is a BIT OF A DECLARED BUS
             (Vivado names FF Qs into buses — `\cnt_reg[0]/.Q(p_0_in[3])`),
             a bracket-STRING assign `p_0_in[0] := …` drives a phantom
             scalar while every cone reads the BUS slice p_0_in[0:0] —
             writer/reader split, the bus floats.  Route the alias through
             `obuf_<bus>_<i>__O`, which resolve_input_bitbus assembles into
             the bus.  A bracket name whose base is NOT a declared signal
             (`acc_reg_n_0_[9]` scalar wires) keeps the plain alias. *)
          let alias_lhs =
            if Str.string_match canon_bracket_re lhs 0 then begin
              let base = Str.matched_group 1 lhs in
              let bit = Str.matched_group 2 lhs in
              if List.exists (fun (s : bsignal) -> s.name = base) m.signals
              then begin
                let ob = Printf.sprintf "obuf_%s_%s__O" base bit in
                extra_sigs := { name = ob;
                                stype = BInt { width = 1; signed = Unsigned };
                                direction = `Internal; initial_value = None;
                                attrs = [] } :: !extra_sigs;
                ob
              end else lhs
            end else lhs in
          extra_procs := BCombinational {
            name = q_new ^ "__qalias"; sensitivity = [BAny];
            body = [BAssign { lhs = alias_lhs; rhs = BVar q_new }] } :: !extra_procs;
          extra_sigs := { name = q_new;
                          stype = BInt { width = 1; signed = Unsigned };
                          direction = `Internal; initial_value = None;
                          attrs = [] } :: !extra_sigs;
          BSequential { r with body = [BAssign { lhs = q_new; rhs = subst rhs }] }
        end
    | _ -> proc) m.processes in
  let m = { m with signals = !extra_sigs @ m.signals;
                   processes = pre_procs @ !extra_procs } in
  { m with
    signals = List.map (fun (s:bsignal) -> { s with name = canon_ff_name s.name }) m.signals;
    processes = List.map re_proc m.processes;
    instances = List.map (fun (i:binstance) ->
      { i with inst_name = canon_ff_name i.inst_name;
               port_connections =
                 List.map (fun (p,e) -> (p, re_e e)) i.port_connections }) m.instances }

let prep_for_z3 ?(trusted : string list = []) (m : bmodule) (p : bprogram) : bmodule =
  let p, _n = Behavioral_arch_subst.substitute_program p in
  let result =
    match List.find_opt (fun (mm : bmodule) -> mm.name = m.name) p.modules with
    | None -> m
    | Some m' ->
        let m' = cut_blackboxes ~trusted m' p in
        if m'.instances = [] then m'
        else
          let p' = { p with modules =
            List.map (fun (mm : bmodule) -> if mm.name = m'.name then m' else mm)
              p.modules } in
          let flat = Behavioral_hier.flatten_for_z3 p' ~top:m'.name in
          (* The pre-flatten cut only saw TOP-LEVEL black boxes.  Deep stateful /
             analogue primitives (GTXE2/MMCME2/RAM64M/SRL…, buried inside user
             submodules) surface as top-level instances only after flattening.
             Discard flatten's unresolved report, cut those now-top-level boxes,
             and re-record only whatever genuinely can't be cut. *)
          let _ = Behavioral_hier.take_unresolved () in
          let flat' = cut_blackboxes ~trusted flat p in
          List.iter (fun (i : Behavioral_ir.binstance) ->
              Behavioral_hier.unresolved_register ~parent_name:flat'.name i)
            flat'.instances;
          flat'
  in
  (* fpga_map bit-blasts a W-bit register into W 1-bit FDREs driving
     `<bus>__b<i>`, so ffrip yields W independent Q/D cones while the behavioural
     reference has a single bus-level cone — the names never match.  Collapse the
     FDRE-model conditional bodies to a single BAssign (iflift) and re-pack the
     per-bit FFs into one bus FF (ffpack) so state lines up by name.  Then
     resolve the multi-bit-port bitbus. *)
  let result = Behavioral_ffpack.pack_module
                 (canonicalize_ff_names (Behavioral_iflift.lift_module result)) in
  resolve_input_bitbus result

(* One module's cross-design verdict, with [trusted] submodules black-boxed
 * (not flattened) on both sides. *)
let miter_core ?(trusted : string list = [])
               ?(input_consts : (string * Z.t) list = [])
               ?(output_masks : (string * Z.t) list = [])
               (ma, pa) (mb, pb) : string =
  let _ = Behavioral_hier.take_unresolved () in   (* clear any stale *)
  let ma' = prep_for_z3 ~trusted ma pa in
  let unres_a = Behavioral_hier.take_unresolved () in
  let mb' = prep_for_z3 ~trusted mb pb in
  let unres_b = Behavioral_hier.take_unresolved () in
  let unres = unres_a @ unres_b in
  if unres <> [] then begin
    let summary = String.concat ", "
      (List.map (fun (_, inst, ty) -> Printf.sprintf "%s:%s" inst ty) unres) in
    Printf.sprintf "INCONCLUSIVE — %d unresolved primitive bodies: %s"
      (List.length unres) summary
  end else
    (try if Z3_miter.check_miter_equivalence ~input_consts ~output_masks ma' mb'
         then "EQUIVALENT" else "DIFFER"
     with e -> Printf.sprintf "ERROR — %s" (Printexc.to_string e))

let lmiter a_h b_h =
  let (_, ma, pa) = find_mod a_h in
  let (_, mb, pb) = find_mod b_h in
  miter_core (ma, pa) (mb, pb)

(* ── Compositional bottom-up hierarchical miter ─────────────────────────
 * Compare two whole programs (two synthesis flows of the same RTL) module
 * by module in LEAVES-FIRST topo order.  Each module is mitered with its
 * already-proven-equivalent submodules BLACK-BOXED to tied I/O (their read
 * outputs become shared free inputs, their driven inputs become compared
 * outputs) rather than flattened — so an EQUIVALENT verdict on a parent is
 * an assume-guarantee proof resting on its children's separately-proven
 * equivalence.  A submodule is only flattened (inlined) into its parent when
 * it did NOT prove equivalent, to localize the divergence.  Returns a
 * multi-line report; the first divergent module in topo order is the tightest
 * localization of a real mismatch. *)
let lmiter_hier a_prog_h b_prog_h top =
  (* RAW programs carry the full user hierarchy (topo order); the EXPANDED
   * programs abstract every user-submodule instance as an uninterpreted
   * function (Fpga_prim_expand.set_user_ports) — identical on both sides, so
   * children cancel and each module is verified against its OWN logic + child
   * wiring, children NOT flattened.  cut_blackboxes additionally ties stateful
   * / analogue primitives (RAM/GT).  Leaves-first order makes the first DIFFER
   * the tightest localization; a parent's EQUIVALENT is assume-guarantee over
   * its (separately-verified) children. *)
  let _, pa_raw = find_prog a_prog_h in
  let _, pb_raw = find_prog b_prog_h in
  let names p = List.map (fun (m : bmodule) -> m.name) p.modules in
  let common =
    let nb = names pb_raw in
    List.filter (fun n -> List.mem n nb) (names pa_raw) in
  let by_name p n = List.find_opt (fun (m : bmodule) -> m.name = n) p.modules in
  (* leaves-first topo order over the RAW common-module instance DAG *)
  let order = ref [] and visited = Hashtbl.create 32 in
  let rec dfs n =
    if List.mem n common && not (Hashtbl.mem visited n) then begin
      Hashtbl.add visited n ();
      (match by_name pa_raw n with
       | Some m ->
           let kids = List.sort_uniq compare
             (List.filter_map (fun (i : binstance) ->
                if List.mem i.module_name common then Some i.module_name else None)
                m.instances) in
           List.iter dfs kids
       | None -> ());
      order := n :: !order
    end in
  (if List.mem top common then dfs top else List.iter dfs common);
  let ordered = List.rev !order in
  (* expand each side, abstracting its own user submodules as UFs *)
  Fpga_prim_expand.set_user_ports pa_raw.Behavioral_ir.modules;
  let pa = Fpga_prim_expand.expand_program pa_raw in
  Fpga_prim_expand.set_user_ports pb_raw.Behavioral_ir.modules;
  let pb = Fpga_prim_expand.expand_program pb_raw in
  (* CONTEXTUAL constant port bindings for module [n] on one side: a pin is
     kept only when EVERY instantiation of n binds it to the same constant
     (BConst / GND / VCC / const-sentinels / concat thereof).  The caller
     intersects the two sides and equal bindings become input constraints in
     the child compare — Vivado const-propagates INTO children, so without
     the context const-vs-port-passthrough honestly differs. *)
  let const_binds (p_raw : bprogram) n : (string * Z.t) list =
    let rec cst = function
      | Behavioral_ir.BConst { value; width } -> Some (value, width)
      | BVar ("GND" | "<const0>") -> Some (Z.zero, 1)
      | BVar ("VCC" | "<const1>") -> Some (Z.one, 1)
      | BConcat es ->                       (* MSB-first *)
          List.fold_left (fun acc e ->
            match acc, cst e with
            | Some (v, w), Some (ve, we) ->
                Some (Z.logor (Z.shift_left v we) ve, w + we)
            | _ -> None) (Some (Z.zero, 0)) es
      | BReplicate { count; value } ->
          (match cst value with
           | Some (v, w) ->
               let rec rep acc k =
                 if k = 0 then acc
                 else rep (Z.logor (Z.shift_left acc w) v) (k - 1) in
               Some (rep Z.zero count, count * w)
           | None -> None)
      | _ -> None in
    let acc : (string, Z.t option) Hashtbl.t = Hashtbl.create 8 in
    List.iter (fun (m : bmodule) ->
      List.iter (fun (i : binstance) ->
        if i.module_name = n then
          List.iter (fun (pin, e) ->
            match cst e with
            | Some (v, _) ->
                (match Hashtbl.find_opt acc pin with
                 | None -> Hashtbl.add acc pin (Some v)
                 | Some (Some v') when Z.equal v v' -> ()
                 | Some _ -> Hashtbl.replace acc pin None)
            | None -> Hashtbl.replace acc pin None)
            i.port_connections)
        m.instances) p_raw.modules;
    (* restrict to the child's INPUT ports *)
    let inputs = match by_name p_raw n with
      | Some mm -> List.filter_map (fun (s : bsignal) ->
          if s.direction = `Input then Some s.name else None) mm.signals
      | None -> [] in
    Hashtbl.fold (fun pin v l ->
      match v with
      | Some v when List.mem pin inputs -> (pin, v) :: l
      | _ -> l) acc [] in
  (* Per-BIT context usage of module [n]'s OUTPUT pins on one side.  A pin
     bit is USED when some instantiation connects the pin to a net whose
     corresponding bit is READ elsewhere in the parent (slice-aware: the top
     reads only pcspma_status[1:0] for the LEDs, so Vivado keeps 2 bits of
     the child's 16-bit port and ties the rest — comparing the dead bits
     only reproduces that context optimisation).  Whole-net reads and
     dynamic selects mark all bits. *)
  let used_outputs (p_raw : bprogram) n : (string, Z.t) Hashtbl.t =
    let used : (string, Z.t) Hashtbl.t = Hashtbl.create 8 in
    let ones w = Z.pred (Z.shift_left Z.one (max 1 w)) in
    let add_mask pin m =
      let prev = match Hashtbl.find_opt used pin with
        | Some v -> v | None -> Z.zero in
      Hashtbl.replace used pin (Z.logor prev m) in
    List.iter (fun (pm : bmodule) ->
      let insts_of_n = List.filter (fun (i : binstance) ->
        i.module_name = n) pm.instances in
      if insts_of_n <> [] then begin
        (* read-mask per net across the parent, EXCLUDING the pin connections
           of the instances of n themselves *)
        let rmask : (string, Z.t) Hashtbl.t = Hashtbl.create 256 in
        let radd v m =
          let prev = match Hashtbl.find_opt rmask v with
            | Some x -> x | None -> Z.zero in
          Hashtbl.replace rmask v (Z.logor prev m) in
        let all = Z.pred (Z.shift_left Z.one 256) in
        let rec re e = match e with
          | Behavioral_ir.BVar v -> radd v all
          | BSlice { signal = BVar v; msb; lsb } ->
              let lo = max 0 (min msb lsb) and hi = max msb lsb in
              radd v (Z.shift_left (ones (hi - lo + 1)) lo)
          | BSelect { array = BVar v; index = BConst { value; _ } } ->
              (try
                 let b = Z.to_int value in
                 if b >= 0 then radd v (Z.shift_left Z.one b) else radd v all
               with _ -> radd v all)
          | BSlice { signal; _ } -> re signal
          | BSelect { array; index } -> re array; re index
          | BConcat es -> List.iter re es
          | BReplicate { value; _ } -> re value
          | BBinOp { lhs; rhs; _ } -> re lhs; re rhs
          | BUnOp { operand; _ } -> re operand
          | BCond { condition; then_val; else_val } ->
              re condition; re then_val; re else_val
          | BCall { args; _ } -> List.iter re args
          | BConst _ -> () in
        let rec stmt s = match s with
          | Behavioral_ir.BAssign { rhs; _ } -> re rhs
          | BIf { condition; then_stmts; else_stmts } ->
              re condition; List.iter stmt then_stmts; List.iter stmt else_stmts
          | BCase { selector; cases; default } ->
              re selector;
              List.iter (fun (g, b) -> re g; List.iter stmt b) cases;
              List.iter stmt default
          | BWhile { condition; body } -> re condition; List.iter stmt body
          | BFor { init; condition; update; body } ->
              stmt init; re condition; stmt update; List.iter stmt body
          | BBlock b -> List.iter stmt b
          | BCallStmt { args; _ } -> List.iter re args
          | BReturn (Some e) -> re e
          | BReturn None -> () in
        List.iter (function
          | Behavioral_ir.BCombinational { body; _ } -> List.iter stmt body
          | BSequential { body; _ } -> List.iter stmt body) pm.processes;
        List.iter (fun (i : binstance) ->
          if not (i.module_name = n) then
            List.iter (fun (_, e) -> re e) i.port_connections) pm.instances;
        (* the parent's own output ports are read by ITS parent *)
        List.iter (fun (s : bsignal) ->
          if s.direction = `Output then radd s.name all) pm.signals;
        (* map connection → pin mask: pin bit (bitpos+k) reads iff the
           connected net's corresponding bit is read *)
        let netw = Hashtbl.create 64 in
        List.iter (fun (s : bsignal) ->
          match s.stype with
          | BInt { width; _ } -> Hashtbl.replace netw s.name width
          | BBool -> Hashtbl.replace netw s.name 1
          | _ -> ()) pm.signals;
        let width_of v =
          match Hashtbl.find_opt netw v with Some w -> w | None -> 1 in
        let net_mask v = match Hashtbl.find_opt rmask v with
          | Some m -> m | None -> Z.zero in
        List.iter (fun (i : binstance) ->
          List.iter (fun (pin, e) ->
            let rec conn_mask e bitpos = let bitpos = max 0 bitpos in match e with
              | Behavioral_ir.BVar v ->
                  let w = width_of v in
                  (Z.shift_left (Z.logand (net_mask v) (ones w)) bitpos, w)
              | BSlice { signal = BVar v; msb; lsb } ->
                  let lo = max 0 (min msb lsb) and hi = max msb lsb in
                  let m = Z.logand (Z.shift_right (net_mask v) lo)
                            (ones (hi - lo + 1)) in
                  (Z.shift_left m bitpos, hi - lo + 1)
              | BConcat es ->
                  let rec go acc pos = function
                    | [] -> (acc, pos - bitpos)
                    | el :: rest ->
                        let m, w = conn_mask el pos in
                        go (Z.logor acc m) (pos + w) rest in
                  go Z.zero bitpos (List.rev es)   (* MSB-first → LSB-first *)
              | BConst { width; _ } -> (Z.zero, width)
              | _ -> (Z.shift_left Z.one bitpos, 1)  (* conservative: used *)
            in
            add_mask pin (fst (conn_mask e 0)))
            i.port_connections)
          insts_of_n
      end) p_raw.modules;
    used in
  let n_eq = ref 0 in
  let buf = Buffer.create 256 in
  List.iter (fun n ->
    match by_name pa n, by_name pb n with
    | Some ma, Some mb ->
        let kids = List.sort_uniq compare
          (List.filter_map (fun (i : binstance) ->
             if List.mem i.module_name common then Some i.module_name else None)
             (match by_name pa_raw n with Some m -> m.instances | None -> [])) in
        let ca = const_binds pa_raw n and cb = const_binds pb_raw n in
        let input_consts =
          List.filter_map (fun (pin, v) ->
            match List.assoc_opt pin cb with
            | Some v' when Z.equal v v' -> Some (pin, v)
            | _ -> None) ca in
        let ua = used_outputs pa_raw n and ub = used_outputs pb_raw n in
        (* per-BIT context masks, intersected: a bit dead on EITHER side is
           masked — a flow that proved it redundant in context (rx_clk ≡
           eth_clk; pcspma_status[15:2] unread by the LED logic) rewires or
           ties it, and comparing only reproduces that trusted optimisation. *)
        let output_masks =
          match by_name pa_raw n with
          | Some mm ->
              List.filter_map (fun (s : bsignal) ->
                if s.direction = `Output then begin
                  let m v = match Hashtbl.find_opt v s.name with
                    | Some x -> x | None -> Z.zero in
                  Some (s.name, Z.logand (m ua) (m ub))
                end else None) mm.signals
          | None -> [] in
        let verdict = miter_core ~input_consts ~output_masks (ma, pa) (mb, pb) in
        if verdict = "EQUIVALENT" then incr n_eq;
        Buffer.add_string buf
          (Printf.sprintf "HIER %-34s %s%s\n" n verdict
             (if kids = [] then ""
              else "  [children abstracted: " ^ String.concat "," kids ^ "]"))
    | _ -> ()) ordered;
  Buffer.add_string buf
    (Printf.sprintf "HIER-SUMMARY %d/%d modules EQUIVALENT%s"
       !n_eq (List.length ordered)
       (if !n_eq = List.length ordered && ordered <> []
        then " → whole design EQUIVALENT (assume-guarantee)" else ""));
  Buffer.contents buf

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

(* Expand FPGA primitives (LUT/FF/CARRY4/buffers -> behavioural BIR;
 * GT/MMCM/RAMB -> uninterpreted BCall) so a gate-mapped prog can be Z3-mitered
 * against its behavioural source.  Pair with a netlist generated under
 * FPGA_LEC_NAMES=1 so register nets stay name-correspondent. *)
let lexpand_fpga prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label ^ "+fpga_exp", Fpga_prim_expand.expand_program p))

(* Hierarchical variant: user-submodule instances named in [ref_prog] become
 * uninterpreted functions (identical both miter sides) instead of being
 * flattened, so each module is Z3-verified with its children abstracted —
 * bounded capacity, and a DIFFER localises the bug to that module's own logic
 * + child wiring.  Apply to BOTH miter sides (behavioural and gate-mapped)
 * with the same ref_prog. *)
let lexpand_fpga_h prog_h ref_h =
  let label, p = find_prog prog_h in
  let _, refp = find_prog ref_h in
  Fpga_prim_expand.set_user_ports refp.Behavioral_ir.modules;
  hadd (Prog (label ^ "+fpga_exp_h", Fpga_prim_expand.expand_program p))

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
  (* initial-block compile-time evaluation right after generate expansion:
     array reads then carry literal indices (behavioral_initeval.ml —
     rgmii_lfsr CRC mask matrices; dropping initial blocks zeroed every
     CRC output → FCS = ~0 → all TX frames discarded by the peer NIC) *)
  hadd (Prog (label,
    Behavioral_initeval.eval_program (Behavioral_unroll.unroll_program p)))

let linline prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_inline.inline_program p))

(* Canonicalise interface / hierarchy separators ('.', '$', '\') in every name to
 * a single '_', so a verible-scalarised interface member `m$awid` and a Vivado-
 * flattened `m.awid` / `m\.awid` align by name in a cross-flow miter.  Intended
 * for LEAF (or already-flattened) modules — a module with instances would also
 * need its children canonicalised for the flatten to reconnect. *)
let canon_sep_name s =
  (* Collapse any run of separator chars ('.', '$', '\', '_') to a SINGLE '_'.
   * Different flows spell a scalarized record/interface member differently — the
   * VHDL frontend uses `ctrl_i__ir_funct3` (double underscore), GHDL-synth
   * Verilog uses `ctrl_i_ir_funct3` (single), Vivado uses `m.awid` — this maps
   * them all to one canonical `ctrl_i_ir_funct3` so they align by name. *)
  let b = Buffer.create (String.length s) in
  let prev = ref false in
  String.iter (fun c ->
    if c = '.' || c = '$' || c = '\\' || c = '_' then
      (if not !prev then Buffer.add_char b '_'; prev := true)
    else (Buffer.add_char b c; prev := false)) s;
  Buffer.contents b

(* Generic name-rewrite over a module (f : name -> name). *)
let rec rn_expr f e =
  match e with
  | BVar s -> BVar (f s)
  | BConst _ -> e
  | BBinOp r -> BBinOp { r with lhs = rn_expr f r.lhs; rhs = rn_expr f r.rhs }
  | BUnOp r -> BUnOp { r with operand = rn_expr f r.operand }
  | BSelect r -> BSelect { array = rn_expr f r.array; index = rn_expr f r.index }
  | BSlice r -> BSlice { r with signal = rn_expr f r.signal }
  | BConcat es -> BConcat (List.map (rn_expr f) es)
  | BReplicate r -> BReplicate { r with value = rn_expr f r.value }
  | BCond r -> BCond { condition = rn_expr f r.condition;
                       then_val = rn_expr f r.then_val; else_val = rn_expr f r.else_val }
  | BCall r -> BCall { r with args = List.map (rn_expr f) r.args }

let rec rn_stmt f s =
  match s with
  | BAssign r -> BAssign { lhs = f r.lhs; rhs = rn_expr f r.rhs }
  | BIf r -> BIf { condition = rn_expr f r.condition;
                   then_stmts = List.map (rn_stmt f) r.then_stmts;
                   else_stmts = List.map (rn_stmt f) r.else_stmts }
  | BCase r -> BCase { selector = rn_expr f r.selector;
                       cases = List.map (fun (e, ss) -> (rn_expr f e, List.map (rn_stmt f) ss)) r.cases;
                       default = List.map (rn_stmt f) r.default }
  | BWhile r -> BWhile { condition = rn_expr f r.condition; body = List.map (rn_stmt f) r.body }
  | BFor r -> BFor { init = rn_stmt f r.init; condition = rn_expr f r.condition;
                     update = rn_stmt f r.update; body = List.map (rn_stmt f) r.body }
  | BBlock ss -> BBlock (List.map (rn_stmt f) ss)
  | BCallStmt r -> BCallStmt { r with args = List.map (rn_expr f) r.args }
  | BReturn eo -> BReturn (Option.map (rn_expr f) eo)

let rn_sens f = function
  | BPosEdge s -> BPosEdge (f s) | BNegEdge s -> BNegEdge (f s)
  | BLevel s -> BLevel (f s) | BAny -> BAny

let rn_proc f = function
  | BCombinational r -> BCombinational { r with sensitivity = List.map (rn_sens f) r.sensitivity;
                                                body = List.map (rn_stmt f) r.body }
  | BSequential r -> BSequential { r with clock = f r.clock;
                                          reset = Option.map f r.reset;
                                          body = List.map (rn_stmt f) r.body }

let rename_module f (m : bmodule) =
  { m with
    signals = List.map (fun (s : bsignal) -> { s with name = f s.name }) m.signals;
    processes = List.map (rn_proc f) m.processes;
    instances = List.map (fun (i : binstance) ->
      { i with port_connections = List.map (fun (k, e) -> (f k, rn_expr f e)) i.port_connections })
      m.instances;
    mems = List.map (fun (mm : bmem) -> { mm with mname = f mm.mname }) m.mems }

let canon_module m = rename_module canon_sep_name m

(* Rename a register aliased directly to a named signal (`assign cnt = n61_q` /
 * `assign res_o = n101_q`, n61_q/n101_q registers) so it carries that signal's
 * name.  GHDL/yosys synth name registers `nNN_q` but keep the source signal as a
 * wire aliasing them — the behavioural reference names the register after that
 * same source signal, so this makes the FF-rip state (`__Q`) names correspond
 * across flows for EVERY register, not just output-driving ones.  A register with
 * a synthetic name aliased to a meaningful one is renamed to the meaningful name;
 * ties (two aliases of one register) resolve to the first, preferring outputs. *)
let alias_output_regs (m : bmodule) =
  let is_synth n =
    (* nNN_q / nNN_o style synthesizer temporaries *)
    String.length n >= 2 && n.[0] = 'n'
    && (let ok = ref true in String.iteri (fun i c ->
          if i > 0 && i < String.length n - 2 && not (c >= '0' && c <= '9') then ok := false) n;
        !ok)
    && (let l = String.length n in
        (l >= 2 && String.sub n (l-2) 2 = "_q") || (l >= 2 && String.sub n (l-2) 2 = "_o")) in
  let outs = List.filter_map (fun (s : bsignal) ->
    if s.direction = `Output then Some s.name else None) m.signals in
  let seq_lhs = Hashtbl.create 16 in
  let rec collect = function
    | BAssign { lhs; _ } -> Hashtbl.replace seq_lhs lhs ()
    | BIf r -> List.iter collect r.then_stmts; List.iter collect r.else_stmts
    | BCase r -> List.iter (fun (_, ss) -> List.iter collect ss) r.cases;
                 List.iter collect r.default
    | BBlock ss -> List.iter collect ss
    | BWhile r -> List.iter collect r.body
    | BFor r -> List.iter collect r.body
    | _ -> () in
  List.iter (function BSequential r -> List.iter collect r.body | _ -> ()) m.processes;
  let renames = Hashtbl.create 8 in
  (* pass 1: outputs win the name; pass 2: any other named alias *)
  let consider ~outputs_only =
    List.iter (function
      | BCombinational { body = [BAssign { lhs; rhs = BVar r }]; _ }
        when r <> lhs && Hashtbl.mem seq_lhs r && not (Hashtbl.mem seq_lhs lhs)
             && not (is_synth lhs) && not (Hashtbl.mem renames r)
             && (if outputs_only then List.mem lhs outs else true) ->
          Hashtbl.replace renames r lhs
      | _ -> ()) m.processes in
  consider ~outputs_only:true;
  consider ~outputs_only:false;
  if Hashtbl.length renames = 0 then m
  else begin
    let f n = match Hashtbl.find_opt renames n with Some o -> o | None -> n in
    let m = rename_module f m in
    (* drop the now self-referential alias `O := O` and the merged reg signals *)
    let processes = List.filter (function
      | BCombinational { body = [BAssign { lhs; rhs = BVar r }]; _ } -> lhs <> r
      | _ -> true) m.processes in
    let seen = Hashtbl.create 16 in
    let signals = List.filter (fun (s : bsignal) ->
      if Hashtbl.mem seen s.name then false
      else (Hashtbl.replace seen s.name (); true)) m.signals in
    { m with processes; signals }
  end

let lalias_output_regs mod_h =
  let n, m, p = find_mod mod_h in
  let m' = alias_output_regs m in
  (* Update the module in the PROGRAM too — prep_for_z3 re-fetches by name. *)
  let cp = { p with modules =
    List.map (fun (mm : bmodule) -> if mm.name = m.name then m' else mm) p.modules } in
  hadd (Mod (n, m', cp))

(* ── Simulation-based register correspondence ─────────────────────────────
 * Match a target module's registers to a reference module's by their next-state
 * VALUES under shared random stimulus, refining register equivalence classes to
 * a fixpoint, then rename each unique correspondent.  Robust to synthesis
 * restructuring / register RENAMING (synlig, yosys) where name-based FF matching
 * and alias_output_regs fail: equivalent registers produce identical __D values
 * under identical (input, class-assigned state) vectors, so they land in the same
 * class; non-equivalent ones diverge and split.  Anchored by primary inputs
 * (shared by name) — the classic random-simulation signal-correspondence method. *)
let reg_correspond (ref_m : bmodule) (tgt_m : bmodule) : (string * string) list =
  let module BI = Behavioral_initeval in
  let rr = Behavioral_ffrip.rip_module ref_m in
  let rt = Behavioral_ffrip.rip_module tgt_m in
  let suf s n = let l = String.length s and k = String.length n in
    k >= l && String.sub n (k - l) l = s in
  let sigw (s : bsignal) = match s.stype with
    | BInt { width; _ } -> width | BBool -> 1
    | BArray { size; element = BInt { width; _ }; _ } -> size * width | _ -> 1 in
  (* registers = base X for each X__D output; state input = X__Q if present (output
     FF) else X (internal reg promoted to input). *)
  let regs_of (rm : bmodule) =
    let inset = Hashtbl.create 128 in
    List.iter (fun (s : bsignal) -> if s.direction = `Input then Hashtbl.replace inset s.name ()) rm.signals;
    List.filter_map (fun (s : bsignal) ->
      if s.direction = `Output && suf "__D" s.name then
        let base = String.sub s.name 0 (String.length s.name - 3) in
        let qin = if Hashtbl.mem inset (base ^ "__Q") then base ^ "__Q" else base in
        Some (base, sigw s, qin)
      else None) rm.signals in
  let rregs = regs_of rr and tregs = regs_of rt in
  if Sys.getenv_opt "REGCORR_DEBUG" <> None then begin
    let outs m = List.filter_map (fun (s:bsignal) -> if s.direction=`Output then Some s.name else None) m.signals in
    let seqs m = List.length (List.filter (function BSequential _ -> true | _ -> false) m.processes) in
    Printf.eprintf "[regcorr] pre-check rregs=%d tregs=%d | ref: sigs=%d outs=%d seq=%d sample_outs=[%s]\n%!"
      (List.length rregs) (List.length tregs) (List.length rr.signals)
      (List.length (outs rr)) (seqs rr)
      (String.concat "," (List.filteri (fun i _ -> i < 8) (outs rr)))
  end;
  if rregs = [] || tregs = [] then [] else begin
    let make_sim (rm : bmodule) (regs : (string * int * string) list) =
      let widths = Hashtbl.create 512 in
      List.iter (fun (s : bsignal) -> Hashtbl.replace widths s.name (sigw s)) rm.signals;
      let comb = List.filter_map (function BCombinational r -> Some r.body | _ -> None) rm.processes in
      let cap = 4 + List.length rm.signals in
      let inames = List.filter_map (fun (s : bsignal) ->
        if s.direction = `Input then Some s.name else None) rm.signals in
      let dnames = List.map (fun (b, _, _) -> b ^ "__D") regs in
      (inames, fun (assign : (string * Z.t) list) ->
         let env = { BI.widths; arrays = Hashtbl.create 4; elemw = Hashtbl.create 4;
                     scalars = Hashtbl.create 512; awrites = Hashtbl.create 4 } in
         List.iter (fun (n, v) -> BI.set_scalar env n v) assign;
         (* iterate combinational net to a fixpoint (early-exit on __D stability) *)
         let chksum () = List.fold_left (fun a d ->
           Z.logxor (Z.of_int (Hashtbl.hash d)) (Z.add a
             (try Hashtbl.find env.BI.scalars d with Not_found -> Z.zero))) Z.zero dnames in
         let prev = ref (chksum ()) and i = ref 0 and stop = ref false in
         while not !stop && !i < cap do
           List.iter (List.iter (fun st -> try BI.exec env st with _ -> ())) comb;
           incr i;
           let c = chksum () in if Z.equal c !prev && !i > 1 then stop := true else prev := c
         done;
         env.BI.scalars)
    in
    let (r_in, r_sim) = make_sim rr rregs and (t_in, t_sim) = make_sim rt tregs in
    let rng = Random.State.make [| 0x5eed; 0x1234; 0x9a1c |] in
    let randz w =
      if w <= 0 then Z.zero else begin
        let rec go acc bits = if bits <= 0 then acc
          else go (Z.logor (Z.shift_left acc 30) (Z.of_int (Random.State.bits rng))) (bits - 30) in
        Z.logand (go Z.zero w) (Z.sub (Z.shift_left Z.one w) Z.one)
      end in
    let key tag b = tag ^ "\000" ^ b in
    let cls = Hashtbl.create 256 and rw = Hashtbl.create 256 in
    List.iter (fun (b, w, _) -> Hashtbl.replace cls (key "R" b) w; Hashtbl.replace rw (key "R" b) w) rregs;
    List.iter (fun (b, w, _) -> Hashtbl.replace cls (key "T" b) w; Hashtbl.replace rw (key "T" b) w) tregs;
    let all_in = List.sort_uniq compare (r_in @ t_in) in
    let nclasses () = let s = Hashtbl.create 64 in
      Hashtbl.iter (fun _ c -> Hashtbl.replace s c ()) cls; Hashtbl.length s in
    let refine () =
      let inval = List.map (fun n -> (n, randz 64)) all_in in
      let class_val = Hashtbl.create 64 in
      Hashtbl.iter (fun k c -> if not (Hashtbl.mem class_val c) then
                      Hashtbl.replace class_val c (randz (Hashtbl.find rw k))) cls;
      let qvals regs tag = List.filter_map (fun (b, _, qin) ->
        match Hashtbl.find_opt cls (key tag b) with
        | Some c -> Some (qin, Hashtbl.find class_val c) | None -> None) regs in
      let r_out = r_sim (inval @ qvals rregs "R") in
      let t_out = t_sim (inval @ qvals tregs "T") in
      let dval out b = try Hashtbl.find out (b ^ "__D") with Not_found -> Z.zero in
      let sig_of tag regs out = List.map (fun (b, _, _) ->
        (key tag b, (Hashtbl.find cls (key tag b), dval out b))) regs in
      let sigs = sig_of "R" rregs r_out @ sig_of "T" tregs t_out in
      let g = Hashtbl.create 256 and next = ref 0 in
      List.iter (fun (k, sg) ->
        let c = match Hashtbl.find_opt g sg with Some c -> c
          | None -> let c = !next in incr next; Hashtbl.replace g sg c; c in
        Hashtbl.replace cls k c) sigs in
    let stable = ref 0 and it = ref 0 in
    while !stable < 3 && !it < 40 do
      let b = nclasses () in refine (); incr it;
      if nclasses () = b then incr stable else stable := 0
    done;
    if Sys.getenv_opt "REGCORR_DEBUG" <> None then begin
      (* probe: how many distinct __D values does one random vector give each side *)
      let inval = List.map (fun n -> (n, randz 64)) all_in in
      let cv = Hashtbl.create 64 in
      Hashtbl.iter (fun k c -> if not (Hashtbl.mem cv c) then Hashtbl.replace cv c (randz (Hashtbl.find rw k))) cls;
      let qv regs tag = List.filter_map (fun (b,_,q) -> match Hashtbl.find_opt cls (key tag b) with Some c -> Some (q, Hashtbl.find cv c) | None -> None) regs in
      let ro = r_sim (inval @ qv rregs "R") in
      let nz = List.length (List.filter (fun (b,_,_) -> not (Z.equal (try Hashtbl.find ro (b^"__D") with Not_found -> Z.zero) Z.zero)) rregs) in
      Printf.eprintf "[regcorr] rregs=%d tregs=%d classes=%d iters=%d ref_nonzero_D=%d/%d\n%!"
        (List.length rregs) (List.length tregs) (nclasses ()) !it nz (List.length rregs);
      if List.length rregs <= 12 then begin
        List.iter (fun (b,_,_) -> Printf.eprintf "  R %s -> c%d\n%!" b (Hashtbl.find cls (key "R" b))) rregs;
        List.iter (fun (b,_,_) -> Printf.eprintf "  T %s -> c%d\n%!" b (Hashtbl.find cls (key "T" b))) tregs
      end
    end;
    let byc = Hashtbl.create 256 in
    Hashtbl.iter (fun k c ->
      let tag = String.sub k 0 1 and b = String.sub k 2 (String.length k - 2) in
      let (rl, tl) = try Hashtbl.find byc c with Not_found -> ([], []) in
      Hashtbl.replace byc c (if tag = "R" then (b :: rl, tl) else (rl, b :: tl))) cls;
    let map = ref [] in
    (* Registers in one class are simulation-EQUIVALENT (interchangeable), so when a
       class has equal ref/target counts pair them by sorted canonical name — this
       matches same-named regs and pairs symmetric/constant ones consistently.  A
       class with unequal ref/target counts is a genuine structural mismatch: skip. *)
    Hashtbl.iter (fun _ (rl, tl) ->
      if List.length rl = List.length tl then begin
        let sortc l = List.map snd (List.sort compare (List.map (fun x -> (canon_sep_name x, x)) l)) in
        List.iter2 (fun r t -> if r <> t then map := (t, r) :: !map) (sortc rl) (sortc tl)
      end) byc;
    !map
  end

(* Miter with simulation-based register correspondence: prep BOTH modules ONCE
 * (prep_for_z3 lowers always-blocks so ffrip can see registers), match b's
 * registers to a's by simulation, rename, then check equivalence on the prepped
 * modules directly (no second prep that would re-rename). *)
let lmiter_regcorr a_h b_h =
  let _, ma, pa = find_mod a_h in
  let _, mb, pb = find_mod b_h in
  let _ = Behavioral_hier.take_unresolved () in
  let ma' = prep_for_z3 ma pa in
  let mb' = prep_for_z3 mb pb in
  let _ = Behavioral_hier.take_unresolved () in
  let map = reg_correspond ma' mb' in
  let f x = match List.assoc_opt x map with Some r -> r | None -> x in
  let mb'' = rename_module f mb' in
  (try if Z3_miter.check_miter_equivalence ma' mb'' then "EQUIVALENT" else "DIFFER"
   with e -> Printf.sprintf "ERROR — %s" (Printexc.to_string e))

let lreg_correspond tgt_h ref_h =
  let n, tm, tp = find_mod tgt_h in
  let _, rm, _ = find_mod ref_h in
  let mp = reg_correspond rm tm in
  if Sys.getenv_opt "REGCORR_DEBUG" <> None then
    Printf.eprintf "[regcorr] %s<-%s: %d register(s) matched\n%!" tm.name rm.name (List.length mp);
  let f x = match List.assoc_opt x mp with Some r -> r | None -> x in
  let tm' = rename_module f tm in
  let cp = { tp with modules =
    List.map (fun (mm : bmodule) -> if mm.name = tm.name then tm' else mm) tp.modules } in
  hadd (Mod (n, tm', cp))

let lcanon_sep mod_h =
  let n, m, p = find_mod mod_h in
  (* prep_for_z3 re-fetches the module by name from the PROGRAM and flattens
   * using it, so the whole program must be canonicalised, not just [m]. *)
  let cp = { p with modules = List.map canon_module p.modules } in
  hadd (Mod (n, canon_module m, cp))

let liflift prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_iflift.lift_program p))

let lblocking_subst prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_blocking_subst.blocking_subst_program p))

let lmeminfer prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_meminfer.infer_program p))

let lsrl_infer prog_h =
  let label, p = find_prog prog_h in
  hadd (Prog (label, Behavioral_srl_infer.infer_program p))

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
  (* library_cells is captured at parse time; later passes that SYNTHESISE new
     primitive instances — memlower turns an async-read memory into RAM32X1D,
     srl_infer emits SRL16E/SRLC32E — introduce cell TYPES absent from it.  Top
     it up from the same authoritative VHD-interface lookup (primitive/ +
     secureip/) so every primitive in the flat netlist has known port
     directions/widths; otherwise the structural emitters must guess an input
     for every pin (orphaning outputs -> false nextpnr combinatorial loops) —
     which they now refuse to do, bombing instead.  Memoised: already-resolved
     types (LUT/FF/CARRY4/GT/…) cost nothing. *)
  let covered = List.map fst p.library_cells in
  let missing =
    List.fold_left (fun acc (i : Behavioral_ir.binstance) ->
      let mn = i.module_name in
      if mn = "GND" || mn = "VCC" || List.mem mn covered || List.mem mn acc
      then acc else mn :: acc) [] m.instances |> List.rev in
  let lc = p.library_cells @ Vhdl_to_behavioral.lookup_xil_primitive_ports missing in
  hadd (Netlist (top, m, lc))

(* Read a nextpnr-xilinx routed JSON (post-pack/place/route) and reconstruct
   a UNISIM-primitive Netlist handle (un-packing SLICE_LUTX/SLICE_FFX/... via
   the X_ORIG_TYPE / X_ORIG_PORT_* attributes).  Pairs with write_netlist_verilog
   to emit Verilog for functional xsim of the post-layout open-flow netlist. *)
let lread_nextpnr_json path =
  if not (Sys.file_exists path) then
    failwith ("read_nextpnr_json: file not found: " ^ path);
  let top, m, lc = Nextpnr_json_to_bir.read_netlist path in
  hadd (Netlist (top, m, lc))

(* Gate-map one module: behavioural BIR → AIG → LUT-cover → Hardcaml
 * Circuit.t of LUT/FDRE/CARRY4/IBUF/OBUF/BUFG cells. *)
(* (b) Persistent snapshot of every module's ORIGINAL declared port directions,
   keyed by module name, first-seen-wins.  ibex_svs.lua gate_maps + splices each
   module in turn; by the time a parent is gate-mapped, a child already spliced
   may have had a dead input port pruned (of_circuit dead-input elim), so
   building port_dir from the CURRENT (pruned) program misses it.  We record
   each module's interface the first time it appears (before its own splice
   prunes anything), then fall back to this snapshot when the live program lacks
   a port — belt-and-braces with fix (a) in hardcaml_to_behavioral. *)
let orig_port_dirs :
  (string, (string * [ `Input | `Output ]) list) Hashtbl.t = Hashtbl.create 128

let lgate_map mod_h k_lut io_flag =
  let _, m, prog = find_mod mod_h in
  (* snapshot originals (first-seen) before any pruning of these modules *)
  List.iter (fun (mm : Behavioral_ir.bmodule) ->
    if not (Hashtbl.mem orig_port_dirs mm.Behavioral_ir.name) then
      Hashtbl.replace orig_port_dirs mm.Behavioral_ir.name
        (List.filter_map (fun (s : Behavioral_ir.bsignal) ->
           match s.Behavioral_ir.direction with
           | `Input -> Some (s.Behavioral_ir.name, `Input)
           | `Output -> Some (s.Behavioral_ir.name, `Output)
           | _ -> None) mm.Behavioral_ir.signals))
    prog.Behavioral_ir.modules;
  (* Record the module's declared INPUT-port widths so of_circuit can pad
     regrouped input ports back to full width (a wide input whose high bits'
     fanout was pruned would otherwise narrow, dropping data). *)
  Base.Hashtbl.set Hardcaml_to_behavioral.declared_input_widths
    ~key:m.Behavioral_ir.name
    ~data:(List.filter_map (fun (s : Behavioral_ir.bsignal) ->
        match s.direction with
        | `Input -> Some (s.name, Behavioral_boundary.width_of_btype s.stype)
        | _ -> None) m.Behavioral_ir.signals);
  (* Port-direction lookup so create_circuit classifies instance ports by the
     primitive's DECLARED direction (library_cells, else Vivado unisim VHDL),
     not the net heuristic — which misclassifies an instance input reading a
     net driven by another instance (e.g. MMCM.CLKIN1 <- BUFG) as an output. *)
  let pd_tbl : (string * string, [ `Input | `Output ]) Hashtbl.t =
    Hashtbl.create 128 in
  let add_ports (cn, ports) =
    List.iter (fun (p : Behavioral_ir.library_port) ->
      Hashtbl.replace pd_tbl (cn, p.port_name) p.port_direction) ports in
  List.iter add_ports prog.Behavioral_ir.library_cells;
  (* USER modules too: a structural parent (eth_macro) instantiates user
     modules (sgmii_soc, rx_axis_packer); without their declared port
     directions the net heuristic misclassifies an instance INPUT reading an
     inter-instance net as an output -> the net orphans (driverless). *)
  List.iter (fun (mm : Behavioral_ir.bmodule) ->
    List.iter (fun (s : Behavioral_ir.bsignal) ->
      match s.direction with
      | `Input  -> Hashtbl.replace pd_tbl (mm.Behavioral_ir.name, s.name) `Input
      | `Output -> Hashtbl.replace pd_tbl (mm.Behavioral_ir.name, s.name) `Output
      | _ -> ()) mm.Behavioral_ir.signals) prog.Behavioral_ir.modules;
  (* (b) fill any (module, port) the CURRENT program lacks (a spliced child had
     the port pruned) from the first-seen original-interface snapshot. *)
  Hashtbl.iter (fun name ports ->
    List.iter (fun (p, d) ->
      if not (Hashtbl.mem pd_tbl (name, p)) then Hashtbl.replace pd_tbl (name, p) d)
      ports) orig_port_dirs;
  let covered = Hashtbl.create 64 in
  List.iter (fun (cn, _) -> Hashtbl.replace covered cn ()) prog.Behavioral_ir.library_cells;
  let inst_types =
    List.sort_uniq compare
      (List.map (fun (i : Behavioral_ir.binstance) -> i.module_name) m.instances) in
  let missing = List.filter (fun t -> not (Hashtbl.mem covered t)) inst_types in
  (* Authoritative port directions come from the primitive's VHDL entity
     interface in the Vivado unisim library.  Do NOT swallow a failure here —
     a silent fallback to the net-usage heuristic misclassifies feedback/clock
     inputs (MMCM.CLKFBIN, BUFG-fed clocks) as outputs and drops the cell. *)
  List.iter add_ports (Vhdl_to_behavioral.lookup_xil_primitive_ports missing);
  (* Xilinx HARD primitives (GTXE2_COMMON/GTXE2_CHANNEL, …) have no unisim
     primitive .vhd, so the VHDL lookup above supplies nothing for them and the
     net heuristic then misclassifies an input pin reading an instance-driven
     net (GTXE2.GTREFCLK0 <- IBUFDS_GTE2.O) as an OUTPUT.  That fragments the
     shared refclk net — the GT's GTREFCLK0 orphans driverless (Vivado REQP-51).
     Fall back to the authoritative xil_primitive_ports.json (the same source
     bir_to_edif uses for EDIF interfaces) for any still-uncovered type. *)
  List.iter (fun t ->
    if not (Hashtbl.mem covered t) then
      match Hashtbl.find_opt (Lazy.force Bir_to_edif.xil_json_ports) t with
      | Some ports ->
          List.iter (fun (pn, dir, _w) ->
            if not (Hashtbl.mem pd_tbl (t, pn)) then
              Hashtbl.replace pd_tbl (t, pn) dir) ports
      | None -> ()) inst_types;
  (* No silent lossage: a PRIMITIVE instance (not a user submodule) whose
     connected ports have no resolved direction would be handed to the
     net-usage heuristic in create_circuit, which misclassifies and silently
     DROPS the cell (a BUFG/MMCM/GT vanishes and its clock net orphans).  Fail
     loudly, naming the primitive.port pairs whose direction we could not read
     from library_cells / the unisim VHDL interface / xil_primitive_ports.json. *)
  let user_mods = Hashtbl.create 64 in
  List.iter (fun (mm : Behavioral_ir.bmodule) -> Hashtbl.replace user_mods mm.name ())
    prog.Behavioral_ir.modules;
  let unresolved =
    List.concat_map (fun (i : Behavioral_ir.binstance) ->
      if Hashtbl.mem user_mods i.module_name then []
      else List.filter_map (fun (port, _) ->
        if Hashtbl.mem pd_tbl (i.module_name, port) then None
        else Some (i.module_name ^ "." ^ port)) i.port_connections) m.instances
    |> List.sort_uniq compare in
  if unresolved <> [] then
    failwith (Printf.sprintf
      "gate_map %s: unresolved primitive port directions — no library_cell, \
       unisim VHDL interface, or xil_primitive_ports.json entry for: %s. The \
       net heuristic would misclassify these and drop the cell; add the \
       primitive's port interface."
      m.Behavioral_ir.name (String.concat ", " unresolved));
  let port_dir mn port = Hashtbl.find_opt pd_tbl (mn, port) in
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true ~port_dir m in
  let l = Fpga_synth.Bir_to_aig.lower_circuit circ in
  (* Cost mode for the LUT cover, selectable via env so the timing-driven
     mapping can be exercised without recompiling.  Default `Area keeps the
     historical (compact, deep) behaviour.
       GATE_MAP_MODE = area | delay | mixed[:N]   (N = slack tolerance, default 1)
       GATE_MAP_LUTPACK=1   enable LUT packing (area recovery)
       GATE_MAP_MFS2=1      enable mfs2 don't-care optimisation *)
  let mode : Fpga_synth.Lut_cover.cost_mode =
    match Sys.getenv_opt "GATE_MAP_MODE" with
    | Some "delay" -> `Delay
    | Some s when String.length s >= 5 && String.sub s 0 5 = "mixed" ->
        let tol =
          if String.length s > 6 && s.[5] = ':'
          then (try int_of_string (String.sub s 6 (String.length s - 6)) with _ -> 1)
          else 1 in
        `Mixed tol
    | _ -> `Area in
  let envflag v = Sys.getenv_opt v = Some "1" in
  let lutpack = envflag "GATE_MAP_LUTPACK" in
  let mfs2 = envflag "GATE_MAP_MFS2" in
  let mfs2_var = envflag "GATE_MAP_MFS2_VAR" || mfs2 in
  let mfs2_odc = envflag "GATE_MAP_MFS2_ODC" || mfs2 in
  let aig_balance =
    match Sys.getenv_opt "GATE_MAP_AIG_BALANCE" with
    | Some s -> (try int_of_string s with _ -> 0)
    | None -> 0 in
  let mapped = Fpga_synth.Fpga_map.map_lowered
    ~io:(io_flag <> 0) ~k:k_lut ~name:m.name
    ~mode ~lutpack ~mfs2_var_elim:mfs2_var ~mfs2_odc ~aig_balance l in
  hadd (Mapped (m.name, mapped))

(* Dump a Mapped circuit as cell-mapped Verilog via Hardcaml.Rtl,
 * suitable for ver_front to re-parse into structural BIR. *)
let lwrite_cellmapped_v mapped_h path =
  let name, circ = find_mapped mapped_h in
  (* Refuse to write a netlist that isn't fully techmapped — otherwise
     Hardcaml.Rtl.output re-elaborates the surviving RTL operators as
     behavioural assigns and produces a 0-cell-instance stub that looks like a
     valid cellmapped netlist but carries none of the gate structure. *)
  (match Hardcaml_to_behavioral.unmapped_node_kinds circ with
   | [] -> ()
   | viol ->
       let s = String.concat ", "
         (List.map (fun (k, n) -> Printf.sprintf "%s=%d" k n) viol) in
       failwith (Printf.sprintf
         "write_cellmapped_v: circuit '%s' is NOT fully cell-mapped (%s still \
          present) — run gate_map/fpga_map first; refusing to write a \
          half-mapped stub to %s" name s path));
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
  (* Apply the same structural collapses the nextpnr emitter does, so the EDIF
     is legal for Vivado: drop the GT-serial identity-LUT chain (GTXTXP/GTXTXN
     must drive a dedicated GT pad, not a fabric buffer/OBUF -> Vivado Place
     30-713) and bypass the clock identity buffers (a fabric LUT driving 100s of
     register clock pins -> Place 30-568 / global-signal congestion). *)
  let m = Bir_to_nextpnr_json.collapse_gt_serial m in
  let m = Bir_to_nextpnr_json.bypass_clock_ident_buffers m in
  Bir_to_edif.write_edif ~library_cells:lc ~path m;
  path

(* Hierarchical EDIF straight from a gate-mapped PROGRAM — one work cell per
 * module, submodule instances as work cellrefs — WITHOUT flatten_struct.  Keeps
 * module boundaries (avoids the flatten bit-bus artifacts) for Vivado read_edif. *)
let lwrite_hier_edif prog_h top path =
  let _, p = find_prog prog_h in
  Bir_to_edif.write_edif_hier ~library_cells:p.Behavioral_ir.library_cells ~path p ~top;
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

(* Strip the SVS parameter-specialization suffix (`base__P1_D32_…`) from every
 * module whose BASE (text before the first `__`) is unique in the program, and
 * rewrite all instance module_name references to match.  This reconciles SVS's
 * per-parameterization module names with a Vivado post-synth netlist's clean
 * base names (`ibex_cs_registers`, `ibex_alu`, …) so miter_hier pairs them by
 * name.  Modules whose base collides (multiple specializations, e.g. several
 * ibex_counter/ibex_csr) are left untouched — merging them would be unsound. *)
let lcanon_module_names prog_h =
  let _, p = find_prog prog_h in
  let base n =
    let len = String.length n in
    let rec find i =
      if i + 1 >= len then n
      else if n.[i] = '_' && n.[i + 1] = '_' then String.sub n 0 i
      else find (i + 1)
    in find 0 in
  (* Interface signature = sorted "port:width" over I/O ports.  Two modules
     that are the SAME primitive at the SAME parameterisation have identical
     signatures across BOTH flows (Vivado `prim_flop_2sync__parameterized0`
     Width=1 and SVS `prim_flop_2sync__W1_R0` Width=1 both -> clk_i:1,d_i:1,
     q_o:1,rst_ni:1) even though their suffix schemes differ.  Used to
     disambiguate a base shared by several variants — the reason the DMI CDC
     FIFO / 2-sync flops never paired, so the miter FF-ripped them on one side
     and abstracted them on the other (the 152-vs-14 dmi_cdc mismatch). *)
  let iface_sig (m : bmodule) =
    m.signals
    |> List.filter_map (fun (s : bsignal) ->
        match s.direction with
        | `Input | `Output ->
            let w = match s.stype with
              | BInt { width; _ } -> width | BBool -> 1 | _ -> 0 in
            Some (Printf.sprintf "%s:%d" s.name w)
        | _ -> None)
    |> List.sort compare |> String.concat "," in
  (* Per-base set of distinct interface signatures.  A base whose variants all
     share ONE interface is the same primitive at a param that does not change
     ports (BSCANE2__J3 / BSCANE2__J4 differ only in JTAG_CHAIN) -> collapse to
     the plain base so it pairs with the other flow's plainly-named instance.
     A base with SEVERAL interfaces is genuinely different sizes (the CDC FIFO
     __W34_E1 vs __W41_E1) -> disambiguate by interface. *)
  let base_ifaces : (string, (string, unit) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 128 in
  List.iter (fun (m : bmodule) ->
    let b = base m.name in
    let s = match Hashtbl.find_opt base_ifaces b with
      | Some s -> s | None -> let s = Hashtbl.create 4 in Hashtbl.replace base_ifaces b s; s in
    Hashtbl.replace s (iface_sig m) ())
    p.modules;
  let sig_tag m =
    let s = iface_sig m in
    let h = ref 5381 in
    String.iter (fun c -> h := (!h * 33 + Char.code c) land 0xffffff) s;
    Printf.sprintf "%06x" !h in
  let ren : (string, string) Hashtbl.t = Hashtbl.create 128 in
  List.iter (fun (m : bmodule) ->
    let b = base m.name in
    let multi = (try Hashtbl.length (Hashtbl.find base_ifaces b) with Not_found -> 1) > 1 in
    (* single-interface base -> plain base (unchanged for uniquely-named tops
       like dm_top/top_vc707); multi-interface base -> base + iface tag, applied
       even to the plainly-named variant so BOTH flows agree. *)
    let canon = if multi then b ^ "__i" ^ sig_tag m else b in
    if canon <> m.name then Hashtbl.replace ren m.name canon)
    p.modules;
  let map n = try Hashtbl.find ren n with Not_found -> n in
  let modules' = List.map (fun (m : bmodule) ->
    { m with name = map m.name
    ; instances = List.map (fun (i : binstance) ->
        { i with module_name = map i.module_name }) m.instances })
    p.modules in
  Printf.eprintf "[canon] renamed %d module(s) to their base names\n"
    (Hashtbl.length ren);
  hadd (Prog (fst (find_prog prog_h) ^ ":canon", { p with modules = modules' }))

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

(* Lift attributed adder/mul subcells (sv_decomp_adder/mul) to abstract
   BBinOp BAdd/BMul.  On the FPGA synthesis path run with ARCH_SUBST_FPGA=1
   so the lift is unconditional (CARRY4/DSP is the one FPGA choice, no cert
   needed) -- bir_to_aig then lowers the abstract op onto CARRY4/DSP48
   instead of a LUT-mapped prefix tree. *)
let larch_subst prog_h =
  let label, p = find_prog prog_h in
  let p', _n = Behavioral_arch_subst.substitute_program p in
  hadd (Prog (label, p'))

(* Gate-map-safe logical optimisation: constant-prop + DCE + CSE, but NO SSA
   (SSA mints multi-driver writebacks that crash Behavioral_to_hardcaml) and no
   register inference.  Shrinks the BIR before gate_map without breaking it. *)
let loptimize_logic prog_h =
  let label, p = find_prog prog_h in
  let cfg = { Behavioral_optimize.default_config with
              enable_ssa = false;
              enable_register_inference = false;
              verbose = false } in
  let p', _ = Behavioral_optimize.optimize_custom cfg p in
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
        "miter_hier", V.efunc (V.string **-> V.string **-> V.string
                               **->> V.string)
                       (wrap3 lmiter_hier);
        "liberty",    V.efunc (V.string **->> V.string)
                       (wrap1 lliberty);
        "expand",     V.efunc (V.string **-> V.string **->> V.string)
                       (wrap2 lexpand);
        "expand_fpga", V.efunc (V.string **->> V.string) (wrap1 lexpand_fpga);
        "expand_fpga_h", V.efunc (V.string **-> V.string **->> V.string) (wrap2 lexpand_fpga_h);
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
        "srl_infer",      V.efunc (V.string **->> V.string) (wrap1 lsrl_infer);
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
        "canon_module_names", V.efunc (V.string **->> V.string)
                           (wrap1 lcanon_module_names);
        "canon_sep",  V.efunc (V.string **->> V.string) (wrap1 lcanon_sep);
        "alias_output_regs", V.efunc (V.string **->> V.string) (wrap1 lalias_output_regs);
        "reg_correspond", V.efunc (V.string **-> V.string **->> V.string) (wrap2 lreg_correspond);
        "miter_regcorr", V.efunc (V.string **-> V.string **->> V.string) (wrap2 lmiter_regcorr);
        "splice",         V.efunc (V.string **-> V.string **-> V.string
                                   **->> V.string)
                           (wrap3 lsplice);
        "parse_v_cells",  V.efunc (V.string **->> V.string)
                           (wrap1 lparse_v_cells);
        "flatten",        V.efunc (V.string **->> V.string) (wrap1 lflatten);
        "optimize",       V.efunc (V.string **->> V.string) (wrap1 loptimize);
        "arch_subst",     V.efunc (V.string **->> V.string) (wrap1 larch_subst);
        "optimize_logic", V.efunc (V.string **->> V.string) (wrap1 loptimize_logic);
        "ffrip",          V.efunc (V.string **->> V.string) (wrap1 lffrip);
        "register_analyse", V.efunc (V.string **->> V.string)
                             (wrap1 lregister_analyse);
        "cdc_analyse",    V.efunc (V.string **->> V.string)
                           (wrap1 lcdc_analyse);
        "prep_for_z3",    V.efunc (V.string **->> V.string)
                           (wrap1 lprep_for_z3);
        "owner",          V.efunc (V.string **->> V.string) (wrap1 lowner);
        "read_nextpnr_json", V.efunc (V.string **->> V.string)
                              (wrap1 lread_nextpnr_json);
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
        "write_hier_edif",    V.efunc (V.string **-> V.string **-> V.string **->> V.string)
                               (wrap3 lwrite_hier_edif);
        "write_netlist_verilog", V.efunc (V.string **-> V.string **->> V.string)
                                  (wrap2 lwrite_netlist_verilog);
        "expand_primitives_for_z3", V.efunc (V.string **->> V.string)
                               (wrap1 lexpand_primitives_for_z3);
        "augment_prog_with_primitives", V.efunc (V.string **->> V.string)
                               (wrap1 laugment_prog_with_primitives);
        "augment_xil_models", V.efunc (V.string **->> V.string)
                               (wrap1 laugment_xil_models);
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
      ] g;
      (* OS module — this embedded lua-ml has no standard `os` table,
         so expose the handful of bits recipes need.  getenv returns
         Lua nil for unset variables (via V.option). *)
      C.register_module "os" [
        "getenv", V.efunc (V.string **->> V.option V.string)
                   (fun k -> Sys.getenv_opt k);
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
