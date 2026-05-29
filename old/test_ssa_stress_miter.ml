(* ssa_stress miter: yosys-write_verilog oracle ↔ Verible direct.
 *
 * Both paths converge on Verible (ver_front) for the BIR conversion;
 * only the *source SV* differs:
 *
 *   ORACLE: original SV → yosys (proc; opt -fast; flatten; opt
 *           -fast; write_verilog) → ver_front → BIR
 *   SUT:    original SV → ver_front → BIR → merge_slice_writes
 *
 * Yosys's `write_verilog` emits a flattened, opt-cleaned Verilog
 * that has the same SV semantics but a different surface form (gate-
 * level assigns, expanded slice writes, etc.).  When we re-parse it
 * via Verible, the resulting BIR is yosys's *interpretation* of the
 * design.  This bypasses Rtlil_to_behavioral entirely — which earlier
 * mis-encoded `always @(posedge clk) ... else if-tree` bodies and
 * produced spurious counterexamples.
 *
 * Behavioral_mem_merge.merge_slice_writes is applied only to the
 * SUT side because yosys's write_verilog has already lowered
 * `r[hi:lo] = ...` into flat `assign r = {...}` form, so the BIR
 * has no @slice_write calls to lower.
 *
 * Usage: test_ssa_stress_miter <top> <file.sv> [more.sv ...] *)

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
    Printf.eprintf "no yosys found in standard locations\n"; exit 1

(* Run yosys: read sv files, canonicalise via proc/opt/flatten, dump
   write_verilog to [out_v].  Returns the path. *)
let yosys_to_verilog ~top ~files ~out_v =
  let yosys = find_yosys () in
  let script_file = Filename.temp_file "yos_ssa_" ".ys" in
  let oc = open_out script_file in
  Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
  Printf.fprintf oc "hierarchy -top %s\n" top;
  Printf.fprintf oc "proc\nopt -fast\nflatten\nopt -fast\n";
  (* `write_verilog -noattr -noexpr` produces compact synthesisable
     Verilog: no `(* src *)` annotations, expressions kept folded
     instead of broken into per-bit cells.  ver_front handles both
     forms; `-noattr` just keeps the re-parsed BIR smaller. *)
  (* `-renameprefix n` replaces yosys's `_NN_` internal-wire names
     with `nN` so Verible's grammar accepts them — the default
     leading-underscore form trips Source_text_verible.  (TODO:
     allow `_`-prefixed identifiers in the verible lexer.)
     `-noparallelcase` keeps the case statement as plain `case` rather
     than a `casez` with `?` wildcards — Verible mis-evaluates the
     wildcard semantics on yosys's emitted function-of-casez. *)
  Printf.fprintf oc "write_verilog -noattr -renameprefix n -noparallelcase %s\n" out_v;
  close_out oc;
  let log = Filename.temp_file "yos_ssa_" ".log" in
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
  Printf.printf "  ssa_stress miter: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  Printf.printf "[1/3] yosys write_verilog → ver_front → BIR (oracle) ...\n%!";
  let out_v = Filename.temp_file "yos_ssa_" ".v" in
  yosys_to_verilog ~top ~files ~out_v;
  let yos_prog = Verible_to_behavioral.convert_files ~top [out_v] in
  (try Sys.remove out_v with _ -> ());
  (* yosys's write_verilog still emits `assign out[i:j] = ...` for
     slice-LHS continuous assigns, which Verible converts to
     @slice_write — apply the same lowering as the SUT side so Z3
     sees per-bit assignments instead of no-ops on both.  Also
     inline functions: yosys lowers parallel-case to a Verilog
     function (e.g. `n_07_(...)`), and without inlining the BCall
     becomes a Z3 uninterpreted function. *)
  let yos_prog = Behavioral_mem_merge.merge_slice_writes_program yos_prog in
  let yos_prog = Behavioral_inline.inline_program yos_prog in
  Printf.printf "  %d modules\n" (List.length yos_prog.modules);

  Printf.printf "[2/3] ver_front → BIR (SUT, with slice-write lowering + inline) ...\n%!";
  let ver_prog = Verible_to_behavioral.convert_files ~top files in
  let ver_prog = Behavioral_mem_merge.merge_slice_writes_program ver_prog in
  let ver_prog = Behavioral_inline.inline_program ver_prog in
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
        exit 1 in
  let yos_top = pick "yosys-canon" yos_prog.modules in
  let ver_top = pick "verible"     ver_prog.modules in

  Printf.printf "[3/3] Z3 miter ...\n";
  let ok = Z3_miter.check_miter_equivalence yos_top ver_top in
  if ok then begin
    Printf.printf "\n  ✅ FORMALLY EQUIVALENT (yosys-canon ≡ Verible+slice-merge)\n";
    exit 0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT\n";
    exit 1
  end
