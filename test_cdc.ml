(* test_cdc — run CDC analysis (#137) over a Verible-converted BIR.
 * Usage: test_cdc <top> <file.sv> [<file.sv>...]
 *
 * Exit code: 0 if no UNSYNC edges, 1 if any UNSYNC edges, 2 on usage
 * error. The "UNSYNC count" line at the end is grep-friendly for
 * downstream tooling. *)

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s <top> <file.sv>+\n" Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let files =
    Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let p = Verible_to_behavioral.convert_files ~top files in
  let p = Behavioral_meminfer.infer_program p in
  let total_edges = ref 0 and total_unsync = ref 0 in
  let any_multi = ref false in
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    let r = Cdc_analysis.analyse m in
    if List.length r.domains > 1 || r.edges <> [] then begin
      any_multi := true;
      print_string (Cdc_analysis.format_report r);
      print_newline ();
      total_edges := !total_edges + List.length r.edges;
      total_unsync :=
        !total_unsync +
        List.length (List.filter
                       (fun (e : Cdc_analysis.cdc_edge) ->
                          e.sync = Cdc_analysis.Unsynchronised) r.edges)
    end
  ) p.modules;
  if not !any_multi then
    Printf.printf "No multi-domain modules in %s\n" top;
  Printf.printf "CDC summary: %d edge(s), %d UNSYNC\n"
    !total_edges !total_unsync;
  exit (if !total_unsync > 0 then 1 else 0)
