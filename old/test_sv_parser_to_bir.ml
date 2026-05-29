(* Smoke-test: run sv-parser on a file, convert the CST to BIR,
 * pretty-print the resulting bprogram.  At this stage the
 * conversion is interface-only (module name + ports), so the
 * output shows ports + directions + widths only. *)

let usage () =
  prerr_endline "usage: test_sv_parser_to_bir <file.sv> [-i incdir]... [-d define]...";
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
      match Sv_parser_to_behavioral.convert_file
              ~incdirs:(List.rev !incdirs)
              ~defines:(List.rev !defines)
              f with
      | Error e ->
          Printf.eprintf "sv-parser failed: %s\n" e;
          exit 1
      | Ok prog ->
          print_endline (Behavioral_ir.string_of_bprogram prog)
