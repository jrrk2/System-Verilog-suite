(* Smoke-test: parse the sv-parser text-tree dump for a file and
 * pretty-print the round-tripped OCaml tree.  Used to confirm the
 * dump parser is structurally sound before we build the CST→BIR
 * converter on top of it.
 *
 * Usage:
 *   test_sv_parser_dump <file.sv> [-i incdir]... [-d define]... *)

let usage () =
  prerr_endline "usage: test_sv_parser_dump <file.sv> [-i incdir]... [-d define]...";
  exit 2

let () =
  if Array.length Sys.argv < 2 then usage ();
  let file = ref None in
  let incdirs = ref [] in
  let defines = ref [] in
  let i = ref 1 in
  while !i < Array.length Sys.argv do
    let a = Sys.argv.(!i) in
    (match a with
     | "-i" when !i + 1 < Array.length Sys.argv ->
         incdirs := Sys.argv.(!i + 1) :: !incdirs; incr i
     | "-d" when !i + 1 < Array.length Sys.argv ->
         defines := Sys.argv.(!i + 1) :: !defines; incr i
     | _ when a.[0] <> '-' && !file = None -> file := Some a
     | _ -> Printf.eprintf "ignoring arg %S\n" a);
    incr i
  done;
  match !file with
  | None -> usage ()
  | Some f ->
      match Sv_parser_dump.parse_file
              ~incdirs:(List.rev !incdirs)
              ~defines:(List.rev !defines)
              f with
      | Error e ->
          Printf.eprintf "sv-parser failed: %s\n" e;
          exit 1
      | Ok tree ->
          print_endline (Sv_parser_dump.to_string tree)
