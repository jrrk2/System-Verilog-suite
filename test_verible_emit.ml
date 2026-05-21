(* test_verible_emit — round-trip a .sv file through the Verible
 * parser and the emit_source pipeline.
 *
 * Usage:
 *   test_verible_emit <sv_file> [PARAM=val ...]
 *
 * With no extra args: parse → synth_filter → emit (single-file, no
 * parameter elaboration).
 *
 * With `PARAM=val` overrides OR the env var ELABORATE=1: parse →
 * verible_elaborate (parameter resolution + dead-generate pruning) →
 * synth_filter → emit, using emit_elaborated.  The overrides seed the
 * top-level parameter scope, e.g.
 *   test_verible_emit RAMB18E1.v READ_WIDTH_A=9 READ_WIDTH_B=9
 *
 * Output: the round-tripped source on stdout.
 *)
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s <sv_file> [PARAM=val ...]\n" Sys.argv.(0);
    exit 2
  end;
  let sv = Sys.argv.(1) in
  let overrides =
    Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
    |> List.filter_map (fun s ->
         match String.index_opt s '=' with
         | Some i ->
             Some (String.sub s 0 i,
                   String.sub s (i + 1) (String.length s - i - 1))
         | None -> None)
  in
  let elaborate = overrides <> [] || Sys.getenv_opt "ELABORATE" <> None in
  if elaborate then
    print_endline (Verible_emit_source.emit_elaborated ~overrides [sv])
  else
    match Sv_verible_to_ir.parse_verible_file sv with
    | None -> Printf.eprintf "parse failed for %s\n" sv; exit 1
    | Some tok ->
        print_string (Verible_emit_source.emit_program tok);
        print_newline ()
