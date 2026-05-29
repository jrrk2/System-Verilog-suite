(* Parallel-correctness harness: any-frontend-as-oracle.
 *
 * Walks a corpus of single-file SystemVerilog designs, derives the top
 * module name from `module <NAME>` (first match), runs each through
 * BOTH a chosen oracle frontend (default yosys) and a comparison
 * frontend (default verible), and Z3-miters the resulting BIRs.  The
 * point is *parallel correctness*: the oracle is an independent
 * elaboration path; each design is a check that our peer frontend
 * agrees with the oracle on what the design means.
 *
 * Per design we report EQUIV / NOTEQUIV / load-fail / Z3-error.  At
 * the end we tally and exit non-zero iff any design produced
 * NOTEQUIV (load failures and Z3 errors are reported but don't gate
 * the exit code — we want to surface real semantic disagreements,
 * not transient tooling issues).
 *
 * Usage:
 *   test_yosys_oracle_sweep [--peer FRONTEND] [--top NAME] FILE.sv [FILE.sv...]
 *   test_yosys_oracle_sweep [--peer FRONTEND] --dir DIR
 *
 *   --peer  comparison frontend; default = verible.
 *   --top   override the auto-detected top module name
 *           (only sensible for single-file invocations).
 *   --dir   sweep every .sv in the directory. *)

open Behavioral_ir

let usage () =
  prerr_endline
    "usage: test_yosys_oracle_sweep [--peer FRONTEND] [--top NAME] \
     [--dir DIR] [FILE.sv ...]";
  exit 2

(* ─── Frontend dispatch (mirrors test_z3_oracle) ─────────────────── *)

let find_yosys () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0)
    [ home ^ "/oss-cad-suite/bin/yosys";
      "/usr/local/bin/yosys";
      "/usr/bin/yosys";
      "yosys" ]

let run_yosys_to_rtlil ~top ~files ~out =
  let yosys = match find_yosys () with
    | Some y -> y
    | None -> failwith "yosys not found" in
  let script = Filename.temp_file "yosys_" ".ys" in
  let oc = open_out script in
  Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
  Printf.fprintf oc
    "hierarchy -top %s\nproc\nopt -fast\nflatten\nopt -fast\n" top;
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  let rc = Sys.command (Printf.sprintf "%s -q -s %s 2>&1 1>/dev/null"
                          (Filename.quote yosys) (Filename.quote script)) in
  (try Sys.remove script with _ -> ());
  if rc <> 0 then failwith (Printf.sprintf "yosys exit %d" rc)

(* Search-path entries that apply to BOTH frontends:
   - Sv_preproc.include_dirs is filled with these for verible's
     in-OCaml preprocessing of `\`include "…"` directives
   - verilator gets them as `-I<dir>` (glued — verilator's CLI quirk)
     plus `-y <dir>` (space-separated) so module instantiation
     resolves across files. *)
let incdirs : string list ref = ref []

let run_verilator_to_json ~top ~files : string =
  let mdir = Filename.temp_file "vsweep_" "" in
  Sys.remove mdir;
  Unix.mkdir mdir 0o755;
  (* `-I<dir>` only — `-y <dir>` would have verilator scan the whole
     directory looking for unknown modules, which sweeps in any
     selftest/testbench .sv that the caller deliberately filtered
     out (those include data .svh files we don't have).  Module
     instantiations resolve fine from the explicit file list. *)
  let inc_flags = String.concat " "
    (List.map (fun d -> Printf.sprintf "-I%s" (Filename.quote d))
       !incdirs) in
  let cmd =
    Printf.sprintf
      "verilator --json-only --bbox-sys --bbox-unsup -Wno-fatal \
       -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE \
       -Wno-PINMISSING -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
       %s --top-module %s %s --Mdir %s > %s/verilator.log 2>&1"
      inc_flags
      (Filename.quote top)
      (String.concat " " (List.map Filename.quote files))
      (Filename.quote mdir) (Filename.quote mdir)
  in
  let rc = Sys.command cmd in
  if rc <> 0 then begin
    failwith (Printf.sprintf "verilator exit %d (see %s/verilator.log)"
                rc mdir)
  end;
  Filename.concat mdir (Printf.sprintf "V%s.tree.json" top)

let load ~frontend ~top ~files : bprogram =
  match frontend with
  | "verible" -> Verible_to_behavioral.convert_files ~top files
  | "slang" ->
      (match Slang_to_behavioral.convert_files ~top files with
       | Some p -> p | None -> failwith "slang frontend failed")
  | "yosys" ->
      let tmp = Filename.temp_file "yosys_" ".il" in
      run_yosys_to_rtlil ~top ~files ~out:tmp;
      let p = Rtlil_to_behavioral.convert_file tmp in
      (try Sys.remove tmp with _ -> ());
      p
  | "verilator" ->
      let json = run_verilator_to_json ~top ~files in
      (match Verilator_to_behavioral.convert_verilator_json_to_behavioral json with
       | Some p -> p
       | None -> failwith "verilator JSON parse failed")
  | other -> failwith ("unsupported frontend: " ^ other)

(* Auto-discover the top module from a single .sv file. *)
let auto_top file =
  let ic = open_in file in
  let n = in_channel_length ic in
  let buf = Bytes.create n in
  really_input ic buf 0 n;
  close_in ic;
  let s = Bytes.unsafe_to_string buf in
  let re = Str.regexp "^[ \t]*module[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)" in
  try let _ = Str.search_forward re s 0 in Str.matched_group 1 s
  with Not_found -> Filename.remove_extension (Filename.basename file)

let pick_top top src =
  List.find_opt (fun (m : bmodule) -> m.name = top) src

(* Run the miter for a single file; returns a categorical result + a
   single-line summary string.  The Z3 miter sometimes raises on
   interface mismatches; catch it and tag as Z3Error so the harness
   keeps going. *)
type verdict = Equiv | NotEquiv | LoadFail | Z3Error | NoTop

let verdict_label = function
  | Equiv -> "EQUIV" | NotEquiv -> "NOTEQUIV"
  | LoadFail -> "LOADFAIL" | Z3Error -> "Z3ERR"
  | NoTop -> "NOTOP"

let run_one ?explicit_top ~oracle ~peer file =
  let top = match explicit_top with
    | Some t -> t
    | None -> auto_top file
  in
  let try_load f =
    try Ok (load ~frontend:f ~top ~files:[file])
    with e -> Error (Printexc.to_string e)
  in
  match try_load oracle, try_load peer with
  | Error e, _ -> (top, LoadFail, oracle ^ ": " ^ e)
  | _, Error e -> (top, LoadFail, peer ^ ": " ^ e)
  | Ok po, Ok pp ->
      (match pick_top top po.modules, pick_top top pp.modules with
       | None, _ -> (top, NoTop, oracle ^ " side missing top")
       | _, None -> (top, NoTop, peer ^ " side missing top")
       | Some mo, Some mp ->
           (try
              if Z3_miter.check_miter_equivalence mo mp
              then (top, Equiv, "")
              else (top, NotEquiv, "miter SAT counterexample")
            with e -> (top, Z3Error, Printexc.to_string e)))

(* ─── Main ───────────────────────────────────────────────────────── *)

let () =
  let oracle = ref "yosys" in
  let peer = ref "verible" in
  let dir = ref None in
  let explicit_top = ref None in
  let files = ref [] in
  let rec parse = function
    | [] -> ()
    | "--oracle" :: v :: rest -> oracle := v; parse rest
    | "--peer" :: v :: rest -> peer := v; parse rest
    | "--top" :: v :: rest -> explicit_top := Some v; parse rest
    | "--dir" :: v :: rest -> dir := Some v; parse rest
    | ("--incdir" | "-I") :: v :: rest ->
        incdirs := !incdirs @ [v]; parse rest
    | "--help" :: _ -> usage ()
    | x :: _ when String.length x >= 1 && x.[0] = '-' ->
        Printf.eprintf "unknown flag: %s\n" x; usage ()
    | x :: rest -> files := x :: !files; parse rest
  in
  parse (List.tl (Array.to_list Sys.argv));
  (* Seed the verible preprocessor's search path so `\`include "…"`
     directives resolve in-OCaml without leaning on verilator -E. *)
  Sv_preproc.include_dirs := !incdirs;
  let files = match !dir with
    | Some d ->
        Sys.readdir d
        |> Array.to_list
        |> List.filter (fun f -> Filename.check_suffix f ".sv")
        |> List.map (Filename.concat d)
        |> List.sort compare
    | None -> List.rev !files
  in
  if files = [] then usage ();

  Printf.printf "═══════════════════════════════════════════════════════════\n";
  Printf.printf "  oracle parallel-correctness sweep\n";
  Printf.printf "  oracle=%s  peer=%s  files=%d\n"
    !oracle !peer (List.length files);
  Printf.printf "═══════════════════════════════════════════════════════════\n%!";

  let counts = Hashtbl.create 8 in
  let bump k =
    let n = try Hashtbl.find counts k with Not_found -> 0 in
    Hashtbl.replace counts k (n + 1)
  in
  (* Two modes:
     1. `--top NAME` is given: treat the file list as one elaboration
        set, produce ONE verdict for the named top.  All files go to
        verilator and verible together so cross-file module
        instantiations resolve.  This is the right shape when the
        peer files include the dependencies of a project hierarchy.
     2. No `--top`: per-file iteration via auto_top — useful when
        sweeping many independent designs each in their own .sv. *)
  let run_per_file f =
    let _, v, why =
      run_one ?explicit_top:!explicit_top ~oracle:!oracle ~peer:!peer f in
    let tag = verdict_label v in
    let detail = if why = "" then "" else "  -- " ^ why in
    Printf.printf "  [%-8s] %s%s\n%!" tag (Filename.basename f) detail;
    bump tag;
    (f, v, why)
  in
  let run_project_set top =
    let try_load f =
      try Ok (load ~frontend:f ~top ~files)
      with e -> Error (Printexc.to_string e)
    in
    match try_load !oracle, try_load !peer with
    | Error e, _ ->
        let v = LoadFail in
        let tag = verdict_label v in
        Printf.printf "  [%-8s] %s  -- %s: %s\n%!" tag top !oracle e;
        bump tag;
        [(top, v, e)]
    | _, Error e ->
        let v = LoadFail in
        let tag = verdict_label v in
        Printf.printf "  [%-8s] %s  -- %s: %s\n%!" tag top !peer e;
        bump tag;
        [(top, v, e)]
    | Ok po, Ok pp ->
        (match pick_top top po.modules, pick_top top pp.modules with
         | None, _ ->
             bump (verdict_label NoTop);
             Printf.printf "  [%-8s] %s  -- %s side missing top\n%!"
               (verdict_label NoTop) top !oracle;
             [(top, NoTop, !oracle ^ " missing top")]
         | _, None ->
             bump (verdict_label NoTop);
             Printf.printf "  [%-8s] %s  -- %s side missing top\n%!"
               (verdict_label NoTop) top !peer;
             [(top, NoTop, !peer ^ " missing top")]
         | Some mo, Some mp ->
             let v, why =
               try
                 if Z3_miter.check_miter_equivalence mo mp
                 then Equiv, ""
                 else NotEquiv, "miter SAT counterexample"
               with e -> Z3Error, Printexc.to_string e
             in
             let tag = verdict_label v in
             let detail = if why = "" then "" else "  -- " ^ why in
             Printf.printf "  [%-8s] %s%s\n%!" tag top detail;
             bump tag;
             [(top, v, why)])
  in
  let results = match !explicit_top with
    | Some top when List.length files > 1 -> run_project_set top
    | _ -> List.map run_per_file files
  in

  Printf.printf "\n──────────── tally ────────────\n";
  ["EQUIV"; "NOTEQUIV"; "LOADFAIL"; "Z3ERR"; "NOTOP"]
  |> List.iter (fun k ->
       let n = try Hashtbl.find counts k with Not_found -> 0 in
       if n > 0 then Printf.printf "  %-10s %d\n" k n);
  Printf.printf "  %-10s %d\n" "TOTAL" (List.length files);

  let any_diff =
    List.exists (fun (_, v, _) -> v = NotEquiv) results in
  exit (if any_diff then 1 else 0)
