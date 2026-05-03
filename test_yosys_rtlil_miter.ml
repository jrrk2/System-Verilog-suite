(* Yosys → RTLIL → BIR → Z3 miter against Vivado VHDL.
 *
 * This adds a fourth reference stream alongside Verilator JSON,
 * Vivado VHDL, and the Verible-frontend BIR. The pipeline:
 *
 *   yosys -p 'read_verilog -sv f1.sv f2.sv ...; hierarchy -top T;
 *             proc; opt -fast; flatten; opt -fast;
 *             write_rtlil <tmp.il>'
 *
 * The resulting RTLIL is parsed via Sv_rtlil_reader and converted
 * by Rtlil_to_behavioral. We then formally compare the named top
 * module's BIR against the matching entity in a Vivado-elaborated
 * VHDL.
 *
 * Usage:
 *   test_yosys_rtlil_miter <top> <vivado.vhd> <file.sv> [more.sv ...]
 *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <vivado.vhd> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

(* Locate yosys in the conda env we know about plus the usual PATH
 * fallbacks. *)
let find_yosys () =
  let candidates = [
    "/home/local/f4pga/xc7/conda/envs/xc7/bin/yosys";
    "/usr/local/bin/yosys";
    "/usr/bin/yosys";
    "yosys";
  ] in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else (Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0)
  ) candidates

let run_yosys ~top ~files ~out =
  let yosys = match find_yosys () with
    | Some y -> y
    | None ->
        Printf.eprintf "yosys not found in any known location\n";
        exit 1
  in
  (* Yosys script via a temp .ys file — avoids the double-quoting
   * mess of `-p 'read_verilog file...; ...'`. *)
  let script_file = Filename.temp_file "yosys_script_" ".ys" in
  let oc = open_out script_file in
  Printf.fprintf oc "read_verilog -sv %s\n"
    (String.concat " " files);
  Printf.fprintf oc "hierarchy -top %s\n" top;
  Printf.fprintf oc "proc\n";
  Printf.fprintf oc "opt -fast\n";
  Printf.fprintf oc "flatten\n";
  Printf.fprintf oc "opt -fast\n";
  Printf.fprintf oc "write_rtlil %s\n" out;
  close_out oc;
  Printf.printf "[yosys] script %s\n%!" script_file;
  let cmd = Printf.sprintf "%s -q -s %s 2>&1"
              (Filename.quote yosys) (Filename.quote script_file)
  in
  let rc = Sys.command cmd in
  (try Sys.remove script_file with _ -> ());
  if rc <> 0 then begin
    Printf.eprintf "yosys exited with status %d\n" rc;
    exit 1
  end

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top = Sys.argv.(1) in
  let vhd = Sys.argv.(2) in
  let files = Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Yosys RTLIL → BIR vs Vivado VHDL: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  let tmp = Filename.temp_file "yosys_" ".il" in
  run_yosys ~top ~files ~out:tmp;

  Printf.printf "[1/3] Parsing RTLIL...\n%!";
  let prog = Rtlil_to_behavioral.convert_file tmp in
  Printf.printf "  %d modules\n" (List.length prog.modules);

  Printf.printf "[2/3] Loading Vivado VHDL...\n%!";
  let viv =
    match Vhdl_to_ver_front.convert_vhd_file vhd with
    | Some p -> p
    | None -> Printf.eprintf "Vivado parse failed\n"; exit 1
  in
  Printf.printf "  %d modules\n" (List.length viv.modules);

  let find_top src_modules =
    List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top) src_modules
  in
  let yosys_top = match find_top prog.modules with
    | Some m -> m
    | None ->
        Printf.eprintf "yosys side: no module '%s'. Available: %s\n" top
          (String.concat ", " (List.map (fun (m : Behavioral_ir.bmodule) ->
             m.name) prog.modules));
        exit 1
  in
  let viv_top = match find_top viv.modules with
    | Some m -> m
    | None ->
        Printf.eprintf "vivado side: no entity '%s'\n" top;
        exit 1
  in

  Printf.printf "[3/3] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence viv_top yosys_top in
  (try Sys.remove tmp with _ -> ());
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (Yosys RTLIL ≡ Vivado VHDL)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
