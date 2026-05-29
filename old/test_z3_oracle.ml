(* Unified Z3 oracle harness: pair any two frontends, run a Z3 miter.
 *
 * Frontends supported (mirrors sv_lua's load_frontend):
 *   verible  — Verible parse-tree → BIR (default; pure OCaml)
 *   slang    — slang AST-JSON     → BIR (calls slang for elaboration)
 *   yosys    — yosys RTLIL        → BIR (proc; opt; flatten; opt)
 *   verilator— verilator JSON     → BIR (expects pre-dumped .json)
 *   vhdl     — VHDL frontend      → BIR (single .vhd input)
 *   surelog  — surelog UHDM dump  → BIR (single .dump or .sv)
 *
 * Usage:
 *   test_z3_oracle <frontend_a> <frontend_b> <top> <files…>
 *   test_z3_oracle <frontend_a> <frontend_b> <top> -a <a-files...> -b <b-files...>
 *
 * The second form is required when the two frontends consume different
 * inputs — e.g. surelog wants a uhdm-dump text file while verible
 * wants the original .sv.
 *
 * Exit code: 0 if EQUIVALENT, 1 if NOT EQUIVALENT, 2 on harness error.
 *
 * Replaces the per-pair test drivers (test_slang_vs_verible,
 * test_yosys_vs_verible, test_verilator_vs_verible) with one knob. *)

open Behavioral_ir

let usage () =
  prerr_endline
    "usage: test_z3_oracle <frontend_a> <frontend_b> <top> <files...>";
  prerr_endline
    "  frontend ∈ {verible, slang, yosys, verilator, vhdl, surelog}";
  exit 2

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
  let rc = Sys.command (Printf.sprintf "%s -q -s %s 2>&1"
                          (Filename.quote yosys) (Filename.quote script)) in
  (try Sys.remove script with _ -> ());
  if rc <> 0 then failwith (Printf.sprintf "yosys exit %d" rc)

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
      (match files with
       | [j] ->
           (match Verilator_to_behavioral.convert_verilator_json_to_behavioral j with
            | Some p -> p | None -> failwith "verilator JSON parse failed")
       | _ -> failwith "verilator frontend takes a single .json")
  | "vhdl" ->
      (match files with
       | [f] ->
           (match Vhdl_to_behavioral.convert_vhdl_file_to_behavioral f with
            | Some p -> p | None -> failwith "vhdl frontend failed")
       | _ -> failwith "vhdl frontend takes a single .vhd")
  | "surelog" ->
      (match files with
       | [f] ->
           if Filename.check_suffix f ".dump"
           then Surelog_to_behavioral.convert_dump_file f
           else failwith
             "surelog frontend currently takes a pre-captured .dump file; \
              run `surelog -parse -sverilog FILE.sv && uhdm-dump …` first"
       | _ -> failwith "surelog frontend takes a single .dump")
  | other -> failwith ("unknown frontend: " ^ other)

let pick label top src =
  match List.find_opt (fun (m : bmodule) -> m.name = top) src with
  | Some m -> m
  | None ->
      let names = String.concat ", "
                    (List.map (fun (m : bmodule) -> m.name) src) in
      Printf.eprintf "%s side has no module '%s'. Available: %s\n"
        label top names;
      exit 2

let () =
  if Array.length Sys.argv < 5 then usage ();
  let fa = Sys.argv.(1) in
  let fb = Sys.argv.(2) in
  let top = Sys.argv.(3) in
  let rest = Array.to_list (Array.sub Sys.argv 4 (Array.length Sys.argv - 4)) in
  (* Parse remaining args.  If `-a` / `-b` flags are present, split
     into per-frontend file lists; otherwise both frontends share the
     same file list. *)
  let files_a, files_b =
    let a = ref [] and b = ref [] and shared = ref [] in
    let bucket = ref `Both in
    List.iter (fun arg -> match arg with
      | "-a" | "--a" -> bucket := `A
      | "-b" | "--b" -> bucket := `B
      | other ->
          (match !bucket with
           | `A -> a := other :: !a
           | `B -> b := other :: !b
           | `Both -> shared := other :: !shared)) rest;
    let s = List.rev !shared in
    let av = if !a = [] then s else List.rev !a in
    let bv = if !b = [] then s else List.rev !b in
    (av, bv)
  in
  if files_a = [] || files_b = [] then begin
    prerr_endline "test_z3_oracle: missing input files for one side";
    usage ()
  end;
  Printf.printf "═══════════════════════════════════════════════════\n";
  Printf.printf "  Z3 oracle: %s ↔ %s : top=%s\n" fa fb top;
  Printf.printf "═══════════════════════════════════════════════════\n%!";
  Printf.printf "[1/3] loading %s : %s …\n%!"
    fa (String.concat " " files_a);
  let pa = load ~frontend:fa ~top ~files:files_a in
  Printf.printf "  %d modules\n" (List.length pa.modules);
  Printf.printf "[2/3] loading %s : %s …\n%!"
    fb (String.concat " " files_b);
  let pb = load ~frontend:fb ~top ~files:files_b in
  Printf.printf "  %d modules\n" (List.length pb.modules);
  let ma = pick fa top pa.modules in
  let mb = pick fb top pb.modules in
  Printf.printf "[3/3] Z3 miter …\n%!";
  let ok = Z3_miter.check_miter_equivalence ma mb in
  if ok then begin
    Printf.printf "  ✅ FORMALLY EQUIVALENT (%s ≡ %s)\n" fa fb;
    exit 0
  end else begin
    Printf.printf "  ❌ NOT EQUIVALENT\n";
    exit 1
  end
