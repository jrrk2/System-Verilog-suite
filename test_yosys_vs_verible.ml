(* Yosys RTLIL ↔ Verible parse-tree miter.
 *
 * Both paths take SV source as input and produce a Behavioral_ir
 * bmodule, with no Vivado anywhere. Useful as a software-only
 * equivalence check that exercises two independent SV frontends
 * (Yosys's open-source synthesiser parser and the Verible parser)
 * against each other — disagreement indicates a bug in *either*
 * frontend's BIR conversion.
 *
 * Usage:
 *   test_yosys_vs_verible <top> <file.sv> [more.sv ...] *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

(* Two yosys candidates: the OSS CAD Suite (which ships yosys 0.64+
 * AND the slang.so plugin in share/yosys/plugins/) is preferred when
 * present, since `read_slang` has much wider SV-2017 coverage than
 * yosys's built-in `read_verilog -sv`. Falls back to the f4pga yosys
 * for the legacy code path. *)
let find_yosys () =
  let home = try Sys.getenv "HOME" with Not_found -> "/root" in
  let candidates = [
    home ^ "/oss-cad-suite/bin/yosys";
    "/home/local/f4pga/xc7/conda/envs/xc7/bin/yosys";
    "/usr/local/bin/yosys";
    "/usr/bin/yosys";
    "yosys";
  ] in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else (Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0)
  ) candidates

(* Whether the slang.so plugin is reachable from this yosys. Triggered
 * by the YOSYS_SLANG=1 env var; without it we keep the legacy
 * `read_verilog -sv` path so existing regressions stay deterministic
 * across machines that don't have OSS CAD Suite. *)
let use_slang () =
  Sys.getenv_opt "YOSYS_SLANG" <> None

let run_yosys ~top ~files ~out =
  let yosys = match find_yosys () with
    | Some y -> y
    | None -> Printf.eprintf "yosys not found\n"; exit 1
  in
  let script_file = Filename.temp_file "yosys_script_" ".ys" in
  let oc = open_out script_file in
  if use_slang () then begin
    Printf.fprintf oc "plugin -i slang\n";
    Printf.fprintf oc "read_slang --top %s %s\n" top
      (String.concat " " files);
    Printf.fprintf oc "hierarchy -top %s\n" top;
    (* Skip `opt -fast` for the slang path — opt rewrites whole-wire
     * cell outputs into bit-sliced part-writes (`connect \Y \g [3:0]`),
     * which our rtlil_to_behavioral can't represent (BIR's BAssign LHS
     * is a bare string, not a slice). Plain `proc; flatten` keeps the
     * cell shape canonical: cell outputs go to whole-wire intermediates
     * and a $buf collects them via SigConcat into the final port. *)
    Printf.fprintf oc "proc\nflatten\n"
  end else begin
    Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
    Printf.fprintf oc "hierarchy -top %s\n" top;
    Printf.fprintf oc "proc\nopt -fast\nflatten\nopt -fast\n"
  end;
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  let cmd = Printf.sprintf "%s -q -s %s 2>&1"
              (Filename.quote yosys) (Filename.quote script_file) in
  let rc = Sys.command cmd in
  (try Sys.remove script_file with _ -> ());
  if rc <> 0 then (Printf.eprintf "yosys failed: %d\n" rc; exit 1)

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Yosys RTLIL ↔ Verible parse-tree miter: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] Yosys → BIR ...\n%!";
  let tmp = Filename.temp_file "yosys_" ".il" in
  run_yosys ~top ~files ~out:tmp;
  let yos_prog = Rtlil_to_behavioral.convert_file tmp in
  (try Sys.remove tmp with _ -> ());
  Printf.printf "  %d modules\n" (List.length yos_prog.modules);

  Printf.printf "[2/3] Verible → BIR ...\n%!";
  let ver_prog = Verible_to_behavioral.convert_files ~top files in
  Printf.printf "  %d modules\n" (List.length ver_prog.modules);

  let pick label src =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
            src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s side: no module '%s'. Available: %s\n"
          label top
          (String.concat ", "
             (List.map (fun (m : Behavioral_ir.bmodule) -> m.name) src));
        exit 1
  in
  let yos_top = pick "yosys"   yos_prog.modules in
  let ver_top = pick "verible" ver_prog.modules in

  Printf.printf "[3/3] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence yos_top ver_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (Yosys RTLIL ≡ Verible parse-tree)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
