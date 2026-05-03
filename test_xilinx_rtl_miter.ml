(* Z3 miter equivalence check: Vivado RTL elaboration vs original SV.
 *
 * Both sides go through Verilator → Behavioral IR (no edif_to_behavioral
 * needed). The Vivado side is `write_verilog` after `synth_design -rtl`,
 * which emits a Verilog netlist of generic RTL_* primitives;
 * `rtl_primitives.sv` provides behavioural models for them.
 *
 * Usage:
 *   test_xilinx_rtl_miter <top> <vivado_v_file> <orig_sv_file>...
 *     [-- <orig_sv_file>...]
 *
 * The two file lists are separated by `--`. Files before `--` make up the
 * Vivado-side compilation (the elaborated .v plus rtl_primitives.sv plus any
 * stubs). Files after `--` make up the original-source compilation.
 *
 * If `--` is not present, the second positional list is treated as both
 * "everything after <vivado_v_file>" → Vivado side, and the original SV is
 * required to be the same as the source list passed by compare.sh — see
 * the wrapper for details.
 *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <vivado_files...> -- <original_files...>\n" Sys.argv.(0);
  exit 2

let check_file f =
  if not (Sys.file_exists f) then begin
    Printf.eprintf "error: file not found: %s\n" f;
    exit 1
  end

(* Run verilator --json-only on a list of SV/Verilog files; return path to
 * the V<top>.tree.json file. *)
let run_verilator label top files =
  let mdir = Filename.concat (Filename.get_temp_dir_name ())
               (Printf.sprintf "miter_%s_%s_%d" label top (Unix.getpid ())) in
  let _ = Sys.command (Printf.sprintf "rm -rf %s" mdir) in
  let files_str = String.concat " " (List.map Filename.quote files) in
  let cmd =
    Printf.sprintf
      "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
       -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING \
       -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
       --top-module %s %s --Mdir %s 2>&1"
      (Filename.quote top) files_str (Filename.quote mdir)
  in
  let log = Filename.concat mdir "verilator.log" in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote mdir)) in
  let rc = Sys.command (Printf.sprintf "%s > %s 2>&1" cmd (Filename.quote log)) in
  if rc <> 0 then begin
    Printf.eprintf "error: verilator (%s side) failed (rc=%d)\n" label rc;
    Printf.eprintf "  cmd: %s\n" cmd;
    let ic = open_in log in
    (try while true do prerr_endline (input_line ic) done
     with End_of_file -> close_in ic);
    exit 1
  end;
  Filename.concat mdir (Printf.sprintf "V%s.tree.json" top)

let find_top_module name (modules : Behavioral_ir.bmodule list) =
  match List.find_opt
          (fun (m : Behavioral_ir.bmodule) -> m.name = name) modules with
  | Some m -> m
  | None ->
      Printf.eprintf
        "error: top module '%s' not found. Available: %s\n"
        name
        (String.concat ", "
           (List.map (fun (m : Behavioral_ir.bmodule) -> m.name) modules));
      exit 1

(* Split argv after `top` into (vivado_files, orig_files) at the `--`
 * separator. *)
let split_at_sep args =
  let rec loop acc = function
    | [] -> (List.rev acc, [])
    | "--" :: rest -> (List.rev acc, rest)
    | x :: rest -> loop (x :: acc) rest
  in
  loop [] args

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let rest = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let (vivado_files, orig_files) = split_at_sep rest in
  if vivado_files = [] || orig_files = [] then usage ();
  List.iter check_file vivado_files;
  List.iter check_file orig_files;

  Printf.printf "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  Z3 miter: Vivado-elab Verilog vs original SV\n";
  Printf.printf "═══════════════════════════════════════════════════════════════\n\n";
  Printf.printf "Top:    %s\n" top;
  Printf.printf "Vivado: %s\n" (String.concat ", " vivado_files);
  Printf.printf "Orig:   %s\n\n" (String.concat ", " orig_files);

  Printf.printf "[1/3] Vivado-elab → Behavioral IR...\n";
  (* If the Vivado-side input is a .vhd file, route through
   * Vhdl_to_ver_front (which translates the structural VHDL tree into
   * ver_front's tree shape, then reuses Ver_front_to_behavioral). For a
   * .v file we use ver_front directly. The VHDL path preserves vector
   * ports cleanly (no bit-blasted PRDATA, no .NAME shorthand), while
   * the .v path is still useful when VHDL isn't available. *)
  let viv_v_file = match vivado_files with
    | f :: _ -> f
    | [] -> Printf.eprintf "error: no Vivado file\n"; exit 1
  in
  let is_vhdl = Filename.check_suffix viv_v_file ".vhd"
             || Filename.check_suffix viv_v_file ".vhdl" in
  let result =
    if is_vhdl then begin
      Printf.printf "  using Vhdl_to_ver_front (VHDL → ver_front tree)\n";
      Vhdl_to_ver_front.convert_vhd_file viv_v_file
    end else begin
      Printf.printf "  using ver_front directly on .v\n";
      Ver_front_to_behavioral.convert_v_file viv_v_file
    end
  in
  let viv_prog =
    match result with
    | Some p -> p
    | None ->
        Printf.eprintf "error: Vivado-side parse of %s failed\n" viv_v_file;
        exit 1
  in
  let viv_top = find_top_module top viv_prog.modules in
  Printf.printf "  signals=%d processes=%d instances=%d\n\n"
    (List.length viv_top.signals)
    (List.length viv_top.processes)
    (List.length viv_top.instances);

  Printf.printf "[2/3] Original SV → Behavioral IR...\n";
  let orig_json = run_verilator "orig" top orig_files in
  let orig_prog =
    match Verilator_to_behavioral.convert_verilator_json_to_behavioral orig_json with
    | Some p -> p
    | None ->
        Printf.eprintf "error: original-side BIR conversion failed\n";
        exit 1
  in
  let orig_top = find_top_module top orig_prog.modules in
  Printf.printf "  signals=%d processes=%d instances=%d\n\n"
    (List.length orig_top.signals)
    (List.length orig_top.processes)
    (List.length orig_top.instances);

  Printf.printf "[3/3] Z3 miter equivalence check...\n\n";
  let ok = Z3_miter.check_miter_equivalence viv_top orig_top in

  Printf.printf "\n═══════════════════════════════════════════════════════════════\n";
  if ok then begin
    Printf.printf "  ✅ FORMALLY EQUIVALENT (UNSAT)\n";
    exit 0
  end else begin
    Printf.printf "  ❌ NOT EQUIVALENT (SAT or interface mismatch)\n";
    exit 1
  end
