(* Reader-fidelity miter: yosys RTLIL ↔ yosys Verilog.
 *
 * Both paths start from the *same* yosys state — after
 * read_verilog -sv → hierarchy → proc → opt -fast → flatten →
 * opt -fast — and yosys writes the design twice from there:
 *
 *   PATH A: write_rtlil  → Rtlil_to_behavioral   → BIR_a
 *   PATH B: write_verilog (with -noattr -renameprefix n
 *                          -noparallelcase) →
 *                          Verible_to_behavioral → BIR_b
 *
 * Then Z3_miter formally compares the two.  Disagreement isolates
 * to one of two readers (Rtlil_to_behavioral or
 * Verible_to_behavioral), because the underlying RTLIL semantics
 * is identical between the two writers — both are dumps of yosys's
 * post-opt design.
 *
 * `Behavioral_mem_merge.merge_slice_writes` is applied to the
 * Verible path because `write_verilog` emits `assign out[hi:lo] = …`
 * for slice-LHS, which Verible turns into @slice_write calls that
 * Z3 needs lowered.  The RTLIL path doesn't have @slice_write —
 * Rtlil_to_behavioral builds direct full-bus assignments — so no
 * merge is needed there.
 *
 * Usage: test_yosys_rtlil_vs_verilog <top> <file.sv> [more.sv ...] *)

let usage () =
  Printf.eprintf "usage: %s <top> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

let find_yosys () =
  let candidates = [
    Filename.concat (Sys.getenv "HOME")
      "OpenROAD-flow-scripts/dependencies/bin/yosys";
    "/usr/local/bin/yosys";
    "/usr/bin/yosys";
  ] in
  try List.find Sys.file_exists candidates
  with Not_found ->
    Printf.eprintf "no yosys found\n"; exit 1

(* Run yosys once, dumping both write_rtlil and write_verilog from the
   same post-opt design state.  This guarantees both outputs reflect
   the *same* RTLIL — if write_rtlil and write_verilog ever drift,
   we'd be comparing apples to oranges. *)
let yosys_dump_both ~top ~files ~rtlil_out ~v_out =
  let yosys = find_yosys () in
  let script_file = Filename.temp_file "yos_both_" ".ys" in
  let oc = open_out script_file in
  Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
  Printf.fprintf oc "hierarchy -top %s\n" top;
  Printf.fprintf oc "proc\nopt -fast\nflatten\nopt -fast\n";
  Printf.fprintf oc "write_rtlil %s\n" rtlil_out;
  Printf.fprintf oc "write_verilog -noattr -renameprefix n -noparallelcase %s\n" v_out;
  close_out oc;
  let log = Filename.temp_file "yos_both_" ".log" in
  let cmd = Printf.sprintf "%s -q -s %s > %s 2>&1"
              (Filename.quote yosys) (Filename.quote script_file)
              (Filename.quote log) in
  let rc = Sys.command cmd in
  (try Sys.remove script_file with _ -> ());
  if rc <> 0 then begin
    Printf.eprintf "yosys failed (rc=%d):\n" rc;
    (try
       let ic = open_in log in
       (try while true do print_endline (input_line ic) done
        with End_of_file -> close_in ic)
     with _ -> ());
    exit 1
  end;
  (try Sys.remove log with _ -> ())

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Yosys RTLIL ↔ yosys Verilog reader-fidelity miter: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  let rtlil_out = Filename.temp_file "yos_il_" ".il" in
  let v_out     = Filename.temp_file "yos_v_"  ".v"  in
  Printf.printf "[1/4] yosys dumping RTLIL + Verilog ...\n%!";
  yosys_dump_both ~top ~files ~rtlil_out ~v_out;

  Printf.printf "[2/4] RTLIL → BIR (Rtlil_to_behavioral) ...\n%!";
  let rtlil_prog = Rtlil_to_behavioral.convert_file rtlil_out in
  (try Sys.remove rtlil_out with _ -> ());
  Printf.printf "  %d modules\n" (List.length rtlil_prog.modules);

  Printf.printf "[3/4] yosys-Verilog → BIR (Verible_to_behavioral + slice merge) ...\n%!";
  let v_prog = Verible_to_behavioral.convert_files ~top [v_out] in
  (try Sys.remove v_out with _ -> ());
  let v_prog = Behavioral_mem_merge.merge_slice_writes_program v_prog in
  Printf.printf "  %d modules\n" (List.length v_prog.modules);

  let pick label src =
    match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
            src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s side: no module '%s'. Available: %s\n"
          label top
          (String.concat ", "
             (List.map (fun (m : Behavioral_ir.bmodule) -> m.name) src));
        exit 1 in
  let rtlil_top = pick "rtlil"   rtlil_prog.modules in
  let v_top     = pick "verible" v_prog.modules in

  Printf.printf "[4/4] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence rtlil_top v_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (RTLIL reader ≡ Verilog reader)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT — reader divergence\n";
    exit 1
  end
