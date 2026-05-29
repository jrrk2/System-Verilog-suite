(* Sweep cva6_elab.vhd through Vhdl_to_ver_front.convert_vhd_file and
 * report which top-level VHDL entities make it cleanly into the BIR.
 *
 * Usage:
 *   test_cva6_sweep <cva6_elab.vhd>
 *
 * For each entity, prints a one-line scorecard:
 *   <name> | bir_signals=N | bir_processes=N | bir_instances=N
 *
 * If MITER_STRICT=1 is set, the converter bombs on the first
 * unrecognised pattern (per STRICT_MODE.md), which surfaces the next
 * gap to teach the converter. Without strict mode the run prints
 * counts for every entity that came through. *)

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s <file.vhd>\n" Sys.argv.(0); exit 2
  end;
  let f = Sys.argv.(1) in
  Printf.printf "Sweeping %s through Vhdl_to_ver_front.convert_vhd_file...\n%!" f;
  match Vhdl_to_ver_front.convert_vhd_file f with
  | None ->
      Printf.eprintf "convert_vhd_file returned None (parser/converter failure)\n";
      exit 1
  | Some prog ->
      let mods = prog.Behavioral_ir.modules in
      Printf.printf "Got %d modules\n\n" (List.length mods);
      let mods_sorted =
        List.sort (fun (a : Behavioral_ir.bmodule) b ->
          compare a.name b.name) mods
      in
      List.iter (fun (m : Behavioral_ir.bmodule) ->
        Printf.printf "%-50s | sigs=%4d | procs=%3d | insts=%4d\n"
          m.name
          (List.length m.signals)
          (List.length m.processes)
          (List.length m.instances)
      ) mods_sorted;
      Printf.printf "\nTotal: %d modules came through.\n" (List.length mods)
