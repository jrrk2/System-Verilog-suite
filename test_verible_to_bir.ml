(* Driver: convert SV files via Verible → BIR, dump the result.
 *
 * Usage:
 *   test_verible_to_bir <top> <file.sv> [more.sv ...]
 *
 * Optional `--miter <vivado.vhd>` runs the Z3 miter on the
 * Verible-derived top module against the matching Vivado entity. *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <file.sv> [more ...] [--miter <vhd>]\n" Sys.argv.(0);
  exit 2

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let rest = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let rec split sv mit = function
    | [] -> List.rev sv, mit
    | "--miter" :: v :: r -> split sv (Some v) r
    | x :: r -> split (x :: sv) mit r
  in
  let files, miter_vhd = split [] None rest in
  if files = [] then usage ();

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  Verible → BIR conversion: %s\n" top;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";
  let prog = Verible_to_behavioral.convert_files ~top files in
  Printf.printf "Got %d bmodule(s):\n" (List.length prog.modules);
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "  %-40s %d signals, %d processes\n"
      m.name (List.length m.signals) (List.length m.processes);
    List.iter (fun (s : Behavioral_ir.bsignal) ->
      let dir = match s.direction with
        | `Input -> "input" | `Output -> "output" | `Internal -> "wire" in
      let w = match s.stype with
        | BInt { width; _ } -> width
        | _ -> 1
      in
      Printf.printf "    %-10s %-6s [w=%d]\n" dir s.name w
    ) m.signals
  ) prog.modules;

  (* Exit non-zero when nothing came out — convert_files traps the
   * Verible parse exception and returns an empty bprogram, so the
   * only signal that parsing failed is the empty modules list.
   * sv-tests' Decompiler_Verible_Parse runner relies on this rc to
   * tell PASS from FAIL. *)
  if prog.modules = [] then exit 1;

  (* Brain-dead semantic checks that the parser doesn't catch:
   * multi-driver, mixed proc/continuous, duplicate decls. Anything
   * here means the SV is syntactically valid but semantically
   * illegal — exit 1 so sv-tests' should_fail tests get the right
   * verdict. Skip when MITER_NO_SANITY is set (for cases where the
   * miter wants to compare a deliberately-broken design). *)
  if Sys.getenv_opt "MITER_NO_SANITY" = None then begin
    let n = Behavioral_sanity.report prog in
    if n > 0 then exit 1
  end;

  match miter_vhd with
  | None -> ()
  | Some vhd ->
      Printf.printf "\nZ3 miter: Verible-BIR ≡ Vivado %s ?\n" vhd;
      let viv =
        match Vhdl_to_ver_front.convert_vhd_file vhd with
        | Some p -> p
        | None -> Printf.eprintf "Vivado parse failed\n"; exit 1
      in
      let viv_top =
        match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
                viv.modules with
        | Some m -> m
        | None -> Printf.eprintf "no Vivado entity %s\n" top; exit 1
      in
      let sv_top =
        match List.find_opt (fun (m : Behavioral_ir.bmodule) -> m.name = top)
                prog.modules with
        | Some m -> m
        | None -> Printf.eprintf "no Verible module %s\n" top; exit 1
      in
      let ok = Z3_miter.check_miter_equivalence viv_top sv_top in
      if ok then begin
        Printf.printf "  ✅ FORMALLY EQUIVALENT\n"; exit 0
      end else begin
        Printf.printf "  ❌ NOT EQUIVALENT\n"; exit 1
      end
