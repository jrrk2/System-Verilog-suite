(* test_verible_emit — round-trip a .sv file through the Verible
 * parser and the (in-progress) emit_source pipeline.  Useful for
 * tracking which tokens still fall through to the debug-style
 * getstr fallback while the emitter is being filled in.
 *
 * Usage:
 *   test_verible_emit <sv_file>
 *
 * Output: the round-tripped source on stdout.  Compare against the
 * input with `diff` (modulo whitespace) to see what's still missing.
 *)
let () =
  if Array.length Sys.argv < 2 then begin
    Printf.eprintf "usage: %s <sv_file>\n" Sys.argv.(0);
    exit 2
  end;
  let sv = Sys.argv.(1) in
  match Sv_verible_to_ir.parse_verible_file sv with
  | None ->
      Printf.eprintf "parse failed for %s\n" sv;
      exit 1
  | Some tok ->
      print_string (Verible_emit_source.emit_program tok);
      print_newline ()
