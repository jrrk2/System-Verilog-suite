(* sv_suite — coherent CLI for the SV-decompiler/miter toolchain.
 *
 * Verbs are commands; designs (cva6, swerv, picorv32, …) are just
 * file inputs. One executable, multiple subcommands, all calling the
 * shared library functions directly.
 *
 *   sv_suite parse       <frontend> <top> <files…>
 *   sv_suite miter       <a> <b> <top> <files…>
 *   sv_suite gate-miter  <top> <beh.sv> <gate.sv> [<lib>]
 *   sv_suite sweep       <a> <b> <flat.sv> [--top T] [--filter S]
 *   sv_suite liberty     <file>
 *   sv_suite random      [-n N] [-seed S] [-out DIR] [-features M]
 *   sv_suite list-mods   <frontend> <top> <files…>
 *   sv_suite ff-stats    <frontend> <top> <files…>
 *   sv_suite --help
 *
 * <frontend> ∈ {verible, slang, yosys, verilator, vhdl}.  *)

open Behavioral_ir

(* ──────────────────────────────────────────────────────────────────
 * Frontend dispatcher: name → bprogram. Each branch encapsulates the
 * tool-specific pre-processing (yosys script, verilator -E, etc.) so
 * downstream commands don't care which frontend was used. *)

let find_yosys () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  let candidates = [
    home ^ "/oss-cad-suite/bin/yosys";
    "/usr/local/bin/yosys";
    "/usr/bin/yosys";
    "yosys";
  ] in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0
  ) candidates

let run_yosys_to_rtlil ~top ~files ~out =
  let yosys = match find_yosys () with
    | Some y -> y
    | None -> Printf.eprintf "yosys not found\n"; exit 1 in
  let script = Filename.temp_file "yosys_" ".ys" in
  let oc = open_out script in
  let use_slang = Sys.getenv_opt "YOSYS_SLANG" <> None in
  if use_slang then begin
    Printf.fprintf oc "plugin -i slang\n";
    Printf.fprintf oc "read_slang --top %s %s\n" top
      (String.concat " " files);
    Printf.fprintf oc "hierarchy -top %s\n" top;
    Printf.fprintf oc "proc\nflatten\n"
  end else begin
    Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
    Printf.fprintf oc "hierarchy -top %s\n" top;
    Printf.fprintf oc "proc\nopt -fast\nflatten\nopt -fast\n"
  end;
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  let cmd = Printf.sprintf "%s -q -s %s 2>&1"
              (Filename.quote yosys) (Filename.quote script) in
  let rc = Sys.command cmd in
  (try Sys.remove script with _ -> ());
  if rc <> 0 then begin
    Printf.eprintf "yosys exit %d\n" rc; exit 1
  end

let load_frontend name ~top files : bprogram =
  match name with
  | "verible" ->
      Verible_to_behavioral.convert_files ~top files
  | "verible-ext" ->
      Verible_to_behavioral.convert_files_with_externals ~top files
  | "slang" ->
      (match Slang_to_behavioral.convert_files ~top files with
       | Some p -> p
       | None ->
           Printf.eprintf "slang frontend failed\n"; exit 1)
  | "yosys" ->
      let tmp = Filename.temp_file "yosys_" ".il" in
      run_yosys_to_rtlil ~top ~files ~out:tmp;
      let p = Rtlil_to_behavioral.convert_file tmp in
      (try Sys.remove tmp with _ -> ());
      p
  | "synlig" ->
      (* synlig (yosys+Surelog SV frontend): same RTLIL consumer as yosys,
         different read script.  A cross-tool miter peer for the SVS frontend. *)
      let tmp = Filename.temp_file "synlig_" ".il" in
      Sv_lua.run_synlig_to_rtlil ~top ~files ~out:tmp;
      let p = Rtlil_to_behavioral.convert_file tmp in
      (try Sys.remove tmp with _ -> ());
      p
  | "verilator" ->
      (* Verilator path expects exactly one JSON file produced by
       * `verilator --json-only`; running verilator end-to-end is the
       * job of dedicated wrappers. For the master CLI we accept a
       * pre-dumped JSON so this command stays deterministic. *)
      (match files with
       | [json] ->
           (match Verilator_to_behavioral.convert_verilator_json_to_behavioral
                    json with
            | Some p -> p
            | None -> Printf.eprintf "verilator JSON parse failed\n"; exit 1)
       | _ ->
           Printf.eprintf
             "verilator frontend takes one .json file (verilator --json-only output)\n";
           exit 2)
  | "vhdl" ->
      (match files with
       | [f] ->
           (match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral f with
            | Some p -> p
            | None -> Printf.eprintf "vhdl frontend failed\n"; exit 1)
       | _ ->
           Printf.eprintf "vhdl frontend takes one .vhd file\n"; exit 2)
  | other ->
      Printf.eprintf "unknown frontend: %s (expected verible/slang/yosys/verilator/vhdl)\n"
        other;
      exit 2

let pick_top top (p : bprogram) =
  match List.find_opt (fun (m : bmodule) -> m.name = top) p.modules with
  | Some m -> m
  | None ->
      Printf.eprintf "no module '%s'. Available: %s\n" top
        (String.concat ", "
           (List.map (fun (m : bmodule) -> m.name) p.modules));
      exit 1

(* ──────────────────────────────────────────────────────────────────
 * Subcommands *)

let cmd_parse args =
  match args with
  | frontend :: top :: files when files <> [] ->
      let p = load_frontend frontend ~top files in
      Printf.printf "module count: %d\n" (List.length p.modules);
      let m = pick_top top p in
      print_endline (string_of_bmodule m)
  | _ ->
      prerr_endline
        "usage: sv_suite parse <frontend> <top> <files…>";
      exit 2

let cmd_miter args =
  match args with
  | a :: b :: top :: files when files <> [] ->
      Printf.printf "═══════════════════════════════════════════════════════\n";
      Printf.printf "  Miter %s ↔ %s on %s\n" a b top;
      Printf.printf "═══════════════════════════════════════════════════════\n\n";
      Printf.printf "[1/3] %s → BIR …\n%!" a;
      let pa = load_frontend a ~top files in
      Printf.printf "  %d modules\n" (List.length pa.modules);
      Printf.printf "[2/3] %s → BIR …\n%!" b;
      let pb = load_frontend b ~top files in
      Printf.printf "  %d modules\n" (List.length pb.modules);
      (* Verification pipeline (boundary preservation + arch
       * substitution):
       *   1. Behavioral_arch_subst replaces instances of attribute-
       *      tagged adder/mul modules with abstract BBinOp ops, gated
       *      on a leaf certificate from `verify-arch`. Set SUBST_OFF=1
       *      to disable for comparison runs.
       *   2. Behavioral_hier transiently flattens what's left for Z3
       *      encoding, leaving the source bprogram intact. *)
      let prep_for_z3 prog =
        let prog, n = Behavioral_arch_subst.substitute_program prog in
        if n > 0 then
          Printf.printf "  [arch-subst] %d instances abstracted\n" n;
        let m = pick_top top prog in
        if m.instances = [] then m
        else Behavioral_hier.flatten_for_z3 prog ~top in
      let ma = prep_for_z3 pa in
      let mb = prep_for_z3 pb in
      if Sys.getenv_opt "BIR_DUMP" <> None then
        Printf.printf "\n=== %s ===\n%s\n=== %s ===\n%s\n" a
          (string_of_bmodule ma) b (string_of_bmodule mb);
      Printf.printf "[3/3] Z3 miter …\n";
      let ok = Z3_miter.check_miter_equivalence ma mb in
      if ok then begin
        Printf.printf "\n  ✅ FORMALLY EQUIVALENT (%s ≡ %s)\n" a b; exit 0
      end else begin
        Printf.printf "\n  ❌ NOT EQUIVALENT\n"; exit 1
      end
  | _ ->
      prerr_endline
        "usage: sv_suite miter <a> <b> <top> <files…>";
      exit 2

(* equiv: the GUI equivalence workbench, headless.
 *
 * The point of this verb is that a verdict shown in the workbench window can
 * be reproduced — by someone else, in CI, from a Makefile — with the project
 * file the window saved.  A checker whose answers only exist inside a GUI is
 * not evidence of anything.
 *
 * Exit: 0 EQUIVALENT, 1 DIFFER, 2 anything else (inconclusive, uncomparable,
 * error) — an INCONCLUSIVE must never look like a pass to a Makefile. *)
let cmd_equiv args =
  let usage () =
    prerr_endline
      ("usage: sv_suite equiv <project.json> [options]\n\
      \       sv_suite equiv --a <fe>,<top>,<file>[,<file>…] \\\n\
      \                      --b <fe>,<top>,<file>[,<file>…] [options]\n\
       options:\n\
      \  --mode flat|per-cone|hier   miter mode (default: the project's, else flat)\n\
      \  --timeout <ms>              Z3 timeout per check (default 30000)\n\
      \  --no-sim                    skip simulation-signature register matching\n\
      \  --scan                      list every differing cone\n\
      \  --list-regs                 print the register-matching table\n\
      \  --explain <cone>            counterexample + first-divergence report\n\
      \  --save <file>               write the report\n\
      \  --save-project <file>       write a project file the GUI can open\n\
      \  --tools                     scan for the external tools and print what\n\
      \                              each frontend needs, found, and used\n\
      \  --tool <fe>=<path>          select the binary for one frontend and\n\
      \                              remember it (same picker as the GUI's Tools…)\n\
       <fe> is one of the USABLE frontends found by the scan:\n\
      \  " ^ String.concat ", " (Tool_scan.available_frontends ()));
    exit 2 in
  let parse_side tag spec =
    match String.split_on_char ',' spec with
    | fe :: top :: (_ :: _ as files) ->
        { Equiv_core.s_tag = tag; s_frontend = fe; s_top = top;
          s_files = files; s_post = "none" }
    | _ -> usage () in
  (* --tools / --tool are handled before anything else: they are how you get
     OUT of a "that frontend is not installed" failure, so they must work even
     when no design has been named. *)
  let rec pre = function
    | [] -> ()
    | "--tool" :: v :: t ->
        (match String.index_opt v '=' with
         | None ->
             prerr_endline "usage: --tool <frontend>=<path-to-binary>"; exit 2
         | Some i ->
             let fe = String.sub v 0 i
             and path = String.sub v (i + 1) (String.length v - i - 1) in
             (match Tool_scan.select fe path with
              | Ok () -> Printf.printf "selected %s → %s\n" fe path
              | Error e -> prerr_endline ("--tool: " ^ e); exit 2));
        pre t
    | _ :: t -> pre t in
  pre args;
  if List.mem "--tools" args then begin
    print_string (Tool_scan.report (Tool_scan.get ~rescan:true ()));
    exit 0
  end;
  let proj = ref None and a = ref None and b = ref None in
  let mode = ref None and timeout = ref None and use_sim = ref true in
  let scan = ref false and explain = ref None and save = ref None in
  let list_regs = ref false in
  let save_proj = ref None in
  let rec go = function
    | [] -> ()
    | "--a" :: v :: t -> a := Some (parse_side "A" v); go t
    | "--b" :: v :: t -> b := Some (parse_side "B" v); go t
    | "--mode" :: v :: t ->
        mode := Some (match v with
          | "per-cone" -> Equiv_core.Per_cone
          | "hier" | "hierarchical" -> Equiv_core.Hierarchical
          | "flat" -> Equiv_core.Flat
          | _ -> usage ()); go t
    | "--timeout" :: v :: t -> timeout := Some (int_of_string v); go t
    | "--no-sim" :: t -> use_sim := false; go t
    | "--scan" :: t -> scan := true; go t
    | "--list-regs" :: t -> list_regs := true; go t
    | "--explain" :: v :: t -> explain := Some v; go t
    | "--save" :: v :: t -> save := Some v; go t
    | "--save-project" :: v :: t -> save_proj := Some v; go t
    | "--tool" :: _ :: t -> go t          (* handled in [pre] *)
    | "--tools" :: t -> go t
    | f :: t when !proj = None && !a = None && String.length f > 0 && f.[0] <> '-' ->
        proj := Some f; go t
    | _ -> usage () in
  go args;
  let (sa, sb, overrides, pmode, ptmo) =
    match !proj with
    | Some p -> Equiv_core.load_project p
    | None ->
        (match !a, !b with
         | Some x, Some y -> (x, y, [], Equiv_core.Flat, 30000)
         | _ -> usage ()) in
  let mode = match !mode with Some m -> m | None -> pmode in
  let timeout = match !timeout with Some t -> t | None -> ptmo in
  (* Writing the project from the CLI is what makes the GUI's persistence
     testable, and lets a run be handed to someone else as one file. *)
  (match !save_proj with
   | Some f ->
       Equiv_core.save_project f sa sb overrides mode timeout;
       Printf.printf "wrote project %s\n%!" f
   | None -> ());
  (* A missing tool is a user-fixable condition, not a crash: print the
     message (which names the fix) and exit, rather than an OCaml backtrace
     with the "…" escaped into octal. *)
  let load sp =
    try Equiv_core.load_side sp with
    | Failure m -> prerr_endline ("error: " ^ m); exit 2
    | e -> prerr_endline ("error: " ^ Printexc.to_string e); exit 2 in
  Printf.printf "[1/4] loading A (%s, top %s) …\n%!" sa.Equiv_core.s_frontend
    sa.Equiv_core.s_top;
  let a = load sa in
  Printf.printf "[2/4] loading B (%s, top %s) …\n%!" sb.Equiv_core.s_frontend
    sb.Equiv_core.s_top;
  let b = load sb in
  Printf.printf "[3/4] matching register spaces …\n%!";
  let (pairs, b_left, stale) =
    Equiv_core.match_registers ~use_sim:!use_sim ~overrides a b in
  if stale <> [] then
    List.iter (fun (an, bn) ->
      Printf.eprintf "  ⚠ override %s → %s could not be applied (no such register)\n%!"
        an bn) stale;
  if !list_regs then print_string (Equiv_core.report_registers ~stale pairs b_left);
  Printf.printf "[4/4] miter (%s, timeout %d ms) …\n%!"
    (Equiv_core.mode_str mode) timeout;
  let r = Equiv_core.run_miter ~mode ~timeout_ms:timeout a b pairs in
  print_string r.Equiv_core.rr_log;
  let report = Equiv_core.report_run a b pairs b_left r in
  print_string report;
  let extra = Buffer.create 1024 in
  if !scan then begin
    let (diff, unknown) = Equiv_core.differing_cones a b pairs in
    Printf.bprintf extra "Differing cones (%d): %s\n" (List.length diff)
      (String.concat ", " diff);
    if unknown <> [] then
      Printf.bprintf extra "Inconclusive cones (%d): %s\n" (List.length unknown)
        (String.concat ", " unknown)
  end;
  (match !explain with
   | None -> ()
   | Some cone ->
       (match Equiv_core.explain_cone a b pairs cone with
        | Ok ce -> Buffer.add_string extra ("\n" ^ Equiv_core.report_ce ce)
        | Error why -> Printf.bprintf extra "\ncounterexample: %s\n" why));
  print_string (Buffer.contents extra);
  (match !save with
   | Some f ->
       let oc = open_out f in
       output_string oc report;
       output_string oc (Buffer.contents extra);
       close_out oc;
       Printf.printf "wrote %s\n" f
   | None -> ());
  exit (Equiv_core.exit_code_of_verdict r.Equiv_core.rr_verdict)

(* gate-miter: behavioral SV  ↔  gate-level SV (Liberty cells) *)
let cmd_gate_miter args =
  let (top, beh, gate, lib_opt) = match args with
    | [t; b; g] -> (t, b, g, None)
    | [t; b; g; l] -> (t, b, g, Some l)
    | _ ->
        prerr_endline
          "usage: sv_suite gate-miter <top> <beh.sv> <gate.sv> [<lib>]";
        exit 2 in
  let lib_file = match lib_opt with
    | Some l -> l
    | None ->
        let home = try Sys.getenv "HOME" with Not_found -> "" in
        home ^ "/hardcaml-lua.0.0.1/liberty/simcells.lib" in
  if not (Sys.file_exists lib_file) then begin
    Printf.eprintf "Liberty file not found: %s\n" lib_file; exit 2
  end;
  Printf.printf "  liberty: %s\n" lib_file;
  let lib = Sv_liberty.parse_liberty_file lib_file in
  Printf.printf "  %d cells\n" (Hashtbl.length lib.cells);
  let beh_p = load_frontend "verible" ~top [beh] in
  let gate_clean = Gate_netlist_to_behavioral.preprocess_gate_file gate in
  if gate_clean <> gate then
    Printf.printf "  preprocessed gate file → %s\n" gate_clean;
  let gate_p = load_frontend "verible-ext" ~top [gate_clean] in
  let (known, unknown, unk) =
    Gate_netlist_to_behavioral.instance_coverage lib gate_p in
  Printf.printf "  cells known/unknown: %d / %d\n" known unknown;
  if unknown > 0 then
    Printf.printf "    unknown: %s\n" (String.concat ", " unk);
  let gate_p = Gate_netlist_to_behavioral.expand_program lib gate_p in
  let mb = pick_top top beh_p in
  let mg = pick_top top gate_p in
  if Sys.getenv_opt "BIR_DUMP" <> None then
    Printf.printf "\n=== BEH ===\n%s\n=== GATE ===\n%s\n"
      (string_of_bmodule mb) (string_of_bmodule mg);
  let ok = Z3_miter.check_miter_equivalence mb mg in
  if ok then exit 0 else exit 1

(* sweep: per-entity miter across a flat design (cva6, swerv, …) *)
let cmd_sweep args =
  let top = ref "cva6" in
  let filter = ref [] in
  let do_flatten = ref false in
  let positional = ref [] in
  let rec scan = function
    | "--top" :: v :: rest -> top := v; scan rest
    | "--filter" :: v :: rest -> filter := v :: !filter; scan rest
    | "--flatten" :: rest -> do_flatten := true; scan rest
    | x :: rest -> positional := x :: !positional; scan rest
    | [] -> ()
  in
  scan args;
  let pos = List.rev !positional in
  let (a, b, flat) = match pos with
    | [a; b; f] -> (a, b, f)
    | _ ->
        prerr_endline
          "usage: sv_suite sweep <a-frontend> <b-frontend> <flat.sv> \
           [--top T] [--filter S] [--flatten]";
        exit 2 in
  let contains s sub =
    let ns = String.length s and nb = String.length sub in
    if nb > ns then false
    else
      let rec loop i =
        i + nb <= ns &&
        (String.sub s i nb = sub || loop (i + 1)) in
      loop 0 in
  let matches name =
    !filter = [] || List.exists (contains name) !filter in
  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Per-entity sweep: %s ↔ %s on %s (top=%s)\n" a b flat !top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";
  let t0 = Unix.gettimeofday () in
  let pa = load_frontend a ~top:!top [flat] in
  Printf.printf "  [1/2] %s → %d modules in %.1fs\n" a
    (List.length pa.modules) (Unix.gettimeofday () -. t0);
  let t1 = Unix.gettimeofday () in
  let pb = load_frontend b ~top:!top [flat] in
  (* Boundary preservation (#79): flatten is opt-in via --flatten.
   * Default keeps the per-module hierarchy intact; the hierarchical
   * miter handles cross-instance encoding transiently. The legacy
   * specialisation-aware Behavioral_flatten remains available for
   * cva6's per-entity pair matching where it was the original
   * intent. *)
  let pb = pb
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program in
  let pb = if !do_flatten then Behavioral_flatten.flatten_program pb
           else pb in
  Printf.printf "  [2/2] %s → %d modules in %.1fs%s\n\n" b
    (List.length pb.modules) (Unix.gettimeofday () -. t1)
    (if !do_flatten then " (flattened)" else "");
  let base_of n =
    try
      let i = Str.search_forward (Str.regexp "__") n 0 in
      String.sub n 0 i
    with Not_found -> n in
  let port_shape (m : bmodule) =
    List.filter_map (fun (s : bsignal) ->
      match s.direction with
      | `Input | `Output ->
          let w = match s.stype with
            | BInt { width; _ } -> width
            | BArray { size; element = BInt { width; _ }; _ } -> size * width
            | _ -> 0 in
          Some (s.name, w)
      | _ -> None
    ) m.signals
    |> List.sort compare in
  let by_base = List.fold_left (fun acc (m : bmodule) ->
    let b = base_of m.name in
    let bucket = try List.assoc b acc with Not_found -> [] in
    (b, m :: bucket) :: List.remove_assoc b acc
  ) [] pb.modules in
  let pair_for ma =
    match List.find_opt (fun (m : bmodule) -> m.name = ma.name) pb.modules with
    | Some m -> Some m
    | None ->
        let bn = base_of ma.name in
        let s = port_shape ma in
        let cand = try List.assoc bn by_base with Not_found -> [] in
        List.find_opt (fun m -> port_shape m = s) cand in
  let pass = ref 0 and fail = ref 0 and err = ref 0 and skip = ref 0 in
  let fails = ref [] in
  let prep m owner =
    if m.instances = [] then m
    else Behavioral_hier.flatten_for_z3 owner ~top:m.name in
  List.iter (fun ma ->
    if not (matches ma.name) then ()
    else
      match pair_for ma with
      | None -> incr skip
      | Some mb ->
          Printf.printf "  %-60s … %!" ma.name;
          (try
             let ma' = prep ma pa in
             let mb' = prep mb pb in
             if Z3_miter.check_miter_equivalence ma' mb' then
               (incr pass; Printf.printf "✅\n")
             else
               (incr fail; fails := ma.name :: !fails;
                Printf.printf "❌\n")
           with e ->
             incr err; Printf.printf "⚠ %s\n" (Printexc.to_string e))
  ) pa.modules;
  Printf.printf "\nSummary: %d ✅  %d ❌  %d ⚠  (%d unpaired)\n"
    !pass !fail !err !skip;
  if !fail > 0 then begin
    Printf.printf "Failing entities (first 20):\n";
    List.iteri (fun i n ->
      if i < 20 then Printf.printf "  %s\n" n) (List.rev !fails);
    exit 1
  end

let cmd_liberty args =
  match args with
  | [file] ->
      if not (Sys.file_exists file) then begin
        Printf.eprintf "no such file: %s\n" file; exit 2
      end;
      let lib = Sv_liberty.parse_liberty_file file in
      Printf.printf "library: %s\n" lib.lib_name;
      Printf.printf "cells:   %d\n\n" (Hashtbl.length lib.cells);
      let parsed = ref 0 and failed = ref 0 in
      Hashtbl.iter (fun _ (cell : Sv_liberty.cell_info) ->
        Printf.printf "  cell %s (%s)\n" cell.cell_name cell.cell_type;
        List.iter (fun (pin : Sv_liberty.pin_info) ->
          (match pin.function_expr with
           | None ->
               Printf.printf "    pin %s : %s\n" pin.name
                 (Sv_liberty.string_of_direction pin.direction)
           | Some f ->
               (try
                  let e = Sv_liberty.parse_function_to_bexpr [] f in
                  incr parsed;
                  Printf.printf "    pin %s : %s = %s\n" pin.name
                    (Sv_liberty.string_of_direction pin.direction)
                    (string_of_bexpr e)
                with exn ->
                  incr failed;
                  Printf.printf "    pin %s : %s = %S → %s\n" pin.name
                    (Sv_liberty.string_of_direction pin.direction)
                    f (Printexc.to_string exn)))
        ) cell.pins;
        match cell.ff with
        | None -> ()
        | Some ff ->
            Printf.printf "    ff(%s, %s) clk=%S next=%S\n"
              ff.iq_name ff.iqn_name ff.clocked_on ff.next_state;
            (match ff.clear  with Some s -> Printf.printf "      clear=%S\n" s | None -> ());
            (match ff.preset with Some s -> Printf.printf "      preset=%S\n" s | None -> ())
      ) lib.cells;
      Printf.printf "\nfunction expressions parsed: %d ok / %d failed\n"
        !parsed !failed
  | _ ->
      prerr_endline "usage: sv_suite liberty <file>"; exit 2

let cmd_list_mods args =
  match args with
  | frontend :: top :: files when files <> [] ->
      let p = load_frontend frontend ~top files in
      Printf.printf "%s: %d modules\n" frontend (List.length p.modules);
      List.iter (fun (m : bmodule) ->
        Printf.printf "  %-40s  %d sig / %d proc / %d inst\n" m.name
          (List.length m.signals)
          (List.length m.processes)
          (List.length m.instances)
      ) p.modules
  | _ ->
      prerr_endline
        "usage: sv_suite list-mods <frontend> <top> <files…>";
      exit 2

let cmd_ff_stats args =
  match args with
  | frontend :: top :: files when files <> [] ->
      let p = load_frontend frontend ~top files in
      let m = pick_top top p in
      let rec collect_lhs acc = function
        | BAssign { lhs; _ } -> lhs :: acc
        | BIf { then_stmts; else_stmts; _ } ->
            let acc = List.fold_left collect_lhs acc then_stmts in
            List.fold_left collect_lhs acc else_stmts
        | BCase { cases; default; _ } ->
            let acc = List.fold_left (fun a (_, body) ->
              List.fold_left collect_lhs a body) acc cases in
            List.fold_left collect_lhs acc default
        | BBlock body -> List.fold_left collect_lhs acc body
        | _ -> acc in
      let ffs = List.filter_map (function
        | BSequential { name; clock; clock_edge; reset; body; _ } ->
            let lhs =
              List.fold_left collect_lhs [] body
              |> List.sort_uniq String.compare in
            Some (name, clock, clock_edge, reset, lhs)
        | BCombinational _ -> None
      ) m.processes in
      Printf.printf "module %s: %d FF processes\n" m.name (List.length ffs);
      List.iter (fun (n, clk, edge, rst, lhs) ->
        let edge_s = match edge with `Pos -> "posedge" | `Neg -> "negedge" in
        let rst_s = match rst with
          | None -> ""
          | Some r -> Printf.sprintf " rst=%s" r in
        Printf.printf "  %s @(%s %s)%s drives [%s]\n" n edge_s clk rst_s
          (String.concat ", " lhs)
      ) ffs
  | _ ->
      prerr_endline
        "usage: sv_suite ff-stats <frontend> <top> <files…>";
      exit 2

(* `random` is a thin shim that re-runs `random_sv_gen` via spawn for
 * now — its argument parsing is mature and not worth duplicating
 * inline. Future work can move its logic into a library function. *)
let cmd_random args =
  let exe = Filename.concat (Filename.dirname Sys.executable_name)
              "random_sv_gen.exe" in
  if not (Sys.file_exists exe) then begin
    Printf.eprintf
      "random_sv_gen.exe not found next to sv_suite — run dune build\n";
    exit 2
  end;
  let argv = Array.of_list (exe :: args) in
  let pid = Unix.create_process exe argv
              Unix.stdin Unix.stdout Unix.stderr in
  let (_, status) = Unix.waitpid [] pid in
  match status with
  | Unix.WEXITED n -> exit n
  | _ -> exit 1

let cmd_script args =
  match args with
  | path :: extra -> exit (Sv_lua.run_script ~args:extra path)
  | _ ->
      prerr_endline "usage: sv_suite script <file.lua> [args…]"; exit 2

let parse_arch_args args =
  let width = ref 8 in
  let positional = ref [] in
  let rec scan = function
    | "--width" :: v :: rest -> width := int_of_string v; scan rest
    | x :: rest -> positional := x :: !positional; scan rest
    | [] -> ()
  in
  scan args;
  (List.rev !positional, !width)

let parse_kind k = match k with
  | "adder" -> Arch_verify.Adder
  | "mul"   -> Arch_verify.Mul
  | _ ->
      Printf.eprintf
        "kind must be 'adder' or 'mul', got %s\n" k;
      exit 2

let cmd_verify_arch args =
  let pos, width = parse_arch_args args in
  match pos with
  | [k; arch] ->
      exit (Arch_verify.verify
              ~kind:(parse_kind k) ~arch_name:arch ~width)
  | _ ->
      prerr_endline
        "usage: sv_suite verify-arch <adder|mul> <arch> [--width N]\n\
        \  adder archs: ripple sklansky brent_kung kogge_stone\n\
        \  mul   archs: ripple wallace dadda";
      exit 2

let cmd_timing args =
  let target = ref max_int in
  let arch_adder = ref Behavioral_timing.default_adder_arch in
  let arch_mul   = ref Behavioral_timing.default_mul_arch in
  let positional = ref [] in
  let rec scan = function
    | "--target-depth" :: v :: rest -> target := int_of_string v; scan rest
    | "--default-adder" :: v :: rest -> arch_adder := v; scan rest
    | "--default-mul"   :: v :: rest -> arch_mul   := v; scan rest
    | x :: rest -> positional := x :: !positional; scan rest
    | [] -> ()
  in
  scan args;
  let pos = List.rev !positional in
  let (top, files) = match pos with
    | t :: fs when fs <> [] -> (t, fs)
    | _ ->
        prerr_endline
          "usage: sv_suite timing <top> <files…> \
           [--target-depth N] [--default-adder X] [--default-mul Y]";
        exit 2 in
  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Timing analysis: %s\n" top;
  Printf.printf "  default arch:  adder=%s  mul=%s\n" !arch_adder !arch_mul;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";
  let prog = load_frontend "verible" ~top files in
  (* No arch-subst here: we want to SEE what each op will become, not
   * abstract it away. The timing model reads `sv_decomp_*` attrs
   * directly per-signal to pick depth. *)
  let m_top =
    let m = pick_top top prog in
    if m.instances = [] then m
    else Behavioral_hier.flatten_for_z3 prog ~top in
  Printf.printf "[1/3] Walking dataflow graph …\n%!";
  let arrivals = Behavioral_timing.compute_arrivals
    ~arch_adder:!arch_adder ~arch_mul:!arch_mul m_top in
  Printf.printf "[2/3] Endpoint paths …\n%!";
  let paths = Behavioral_timing.endpoint_paths arrivals m_top in
  print_string (Behavioral_timing.report ~target_depth:!target paths);
  Printf.printf "[3/3] Optimisation suggestions …\n%!";
  let upgrades =
    if !target = max_int then []
    else
      let failing =
        List.filter (fun (p : Behavioral_timing.path_report) ->
          p.arrival > !target) paths in
      Behavioral_timing.suggest_upgrades
        ~arch_adder:!arch_adder ~arch_mul:!arch_mul failing
  in
  print_string (Behavioral_timing.format_upgrades upgrades);
  if !target <> max_int then
    let n_fail = List.length (List.filter
      (fun (p : Behavioral_timing.path_report) ->
        p.arrival > !target) paths) in
    if n_fail = 0 then
      Printf.printf "\n  ✅ all endpoints meet target depth %d\n" !target
    else if upgrades <> [] then
      Printf.printf "\n  ⚠ %d endpoints over target; %d cert-gated upgrades available\n"
        n_fail (List.length upgrades)
    else
      Printf.printf "\n  ❌ %d endpoints over target; no certified upgrades available\n"
        n_fail

let cmd_emit_arch args =
  let pos, width = parse_arch_args args in
  match pos with
  | [k; arch; out_path] ->
      Arch_verify.emit
        ~kind:(parse_kind k) ~arch_name:arch ~width ~out_path;
      Printf.printf "wrote %s/%s/%d → %s\n" k arch width out_path
  | _ ->
      prerr_endline
        "usage: sv_suite emit-arch <adder|mul> <arch> <out.sv> [--width N]\n\
        \  Writes a SystemVerilog block built via Hardcaml_circuits with\n\
        \  the `(* sv_decomp_<kind> *)` attribute on the module header so\n\
        \  the substitution pass picks it up via the verify-arch cert.";
      exit 2

(* Topographical placer (svs_place_core) as a standalone verb — same core the
   place_lef.exe CLI and the Lua svd.place_lef binding call.  All tuning via
   TOPO_* env (TOPO_PLACE, TOPO_SEED, BELS_OUT, TOPO_STAMPED_JSON, …). *)
let cmd_place = function
  | floorplan :: netlist :: _ -> Place_lef_core.run floorplan netlist
  | _ ->
      prerr_endline
        "usage: sv_suite place <floorplan.json> <netlist.json>  (config via TOPO_* env)";
      exit 2

let usage () =
  print_endline {|sv_suite — SV decompiler / miter toolchain

Verbs:
  parse       <frontend> <top> <files…>             dump BIR
  miter       <a> <b> <top> <files…>                Z3-equivalence between two frontends
  gate-miter  <top> <beh.sv> <gate.sv> [<lib>]      behavioral ↔ gate-level (Liberty cells)
  equiv       <project.json> | --a <fe>,<top>,<files> --b …   equivalence workbench, headless
                                                    (register matching + census + counterexample)
  equiv --tools                                     scan for external tools; --tool fe=path selects one
  sweep       <a> <b> <flat.sv> [--top T] [--filter S]   per-entity miter across a flat design
  liberty     <file>                                dump Liberty + parse function exprs
  random      [-n N] [-seed S] [-out DIR] [-features M]  constrained-random generator
  list-mods   <frontend> <top> <files…>             module index per frontend
  ff-stats    <frontend> <top> <files…>             FF set summary
  script      <file.lua>                            run a Lua script (svd.* API).  Bindings:
                                                       svd.parse / pick / miter / gate_miter
                                                       svd.bir / insts / timing / name / items
                                                       svd.liberty / expand
                                                       svd.emit_verilog / emit_vhdl       (BIR → text)
                                                       svd.write_verilog / write_vhdl     (BIR → file)
                                                       svd.convert_hdl(in, out)           (cross-translate, header-preserving)
  place       <floorplan.json> <netlist.json>       topographical place (TOPO_* env) → BELs/stamped json
  verify-arch <adder|mul> <arch> [--width N]        prove arch ≡ `+` / `*`, cache cert
  emit-arch   <adder|mul> <arch> <out.sv> [--width N]  write certified arch block as SV
  timing      <top> <files…> [--target-depth N]     critical-path report + cert-gated upgrade hints

<frontend> ∈ {verible, slang, yosys, verilator, vhdl}
  verible    — Verible parse-tree → BIR (default; pure OCaml, no subprocess)
  slang      — Slang AST-JSON     → BIR (calls slang for elaboration)
  yosys      — yosys RTLIL        → BIR (proc; opt; flatten; opt)
  verilator  — verilator JSON     → BIR (expects pre-dumped .json)
  vhdl       — VHDL frontend      → BIR

Examples:
  sv_suite parse  verible top   foo.sv
  sv_suite miter  slang verible top  foo.sv bar.sv
  sv_suite gate-miter and8 and8.sv and8_gate.v
  sv_suite sweep  slang verible test/cva6_ram/cva6_flat.sv --top cva6
  sv_suite liberty ~/hardcaml-lua.0.0.1/liberty/simcells.lib

Env:
  BIR_DUMP=1     — print both sides' BIR before the miter (parse / miter / gate-miter)
  YOSYS_SLANG=1  — yosys frontend uses read_slang plugin instead of read_verilog -sv|}

let () =
  let args = Array.to_list Sys.argv in
  match List.tl args with
  | [] | "--help" :: _ | "-h" :: _ | "help" :: _ ->
      usage ()
  | "parse"      :: rest -> cmd_parse rest
  | "miter"      :: rest -> cmd_miter rest
  | "gate-miter" :: rest -> cmd_gate_miter rest
  | "equiv"      :: rest -> cmd_equiv rest
  | "sweep"      :: rest -> cmd_sweep rest
  | "liberty"    :: rest -> cmd_liberty rest
  | "random"     :: rest -> cmd_random rest
  | "list-mods"  :: rest -> cmd_list_mods rest
  | "ff-stats"   :: rest -> cmd_ff_stats rest
  | "preprocess" :: f :: _ ->
      (* Debug: dump the SVS-preprocessed text of one file (honours
       * SVS_DEFINE / SVS_INCDIR like the frontend). *)
      print_string (Sv_preproc.preprocess_file f)
  | "parseraw" :: f :: _ ->
      (* Debug: lex+parse an ALREADY-preprocessed file (no preprocessing),
       * so a .pp dump can be bisected to localise a grammar failure. *)
      let ic = open_in f in
      let n = in_channel_length ic in
      let s = really_input_string ic n in
      close_in ic;
      let lexbuf = Lexing.from_string s in
      Lexing.set_filename lexbuf f;
      (try
        let dl = Source_text_verible_lex.deflate Source_text_verible_lex.token in
        let _ = Source_text_verible.ml_start dl lexbuf in
        print_string "PARSE_OK\n"
      with e ->
        let p = Lexing.lexeme_start_p lexbuf in
        Printf.printf "PARSE_FAIL line %d col %d token %S: %s\n"
          p.pos_lnum (p.pos_cnum - p.pos_bol) (Lexing.lexeme lexbuf)
          (Printexc.to_string e))
  | "script"     :: rest -> cmd_script rest
  | "place"      :: rest -> cmd_place rest
  | "verify-arch":: rest -> cmd_verify_arch rest
  | "emit-arch"  :: rest -> cmd_emit_arch rest
  | "timing"     :: rest -> cmd_timing rest
  | sub :: _ ->
      Printf.eprintf "unknown command: %s\n\n" sub;
      usage ();
      exit 2
