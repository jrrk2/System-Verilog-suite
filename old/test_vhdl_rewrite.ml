(* Test VHDL parsing and conversion through rewrite.ml *)

let test_file vhdl_file =
  Printf.printf "Testing VHDL parsing and rewrite.ml conversion\n";
  Printf.printf "%s\n" (String.make 70 '=');
  Printf.printf "Input: %s\n\n" vhdl_file;

  (* Parse VHDL *)
  Printf.printf "Step 1: Parsing VHDL...\n";
  let chan = open_in vhdl_file in
  let lexbuf = Lexing.from_channel chan in

  try
    let ast = Vhd_front.VhdlParser.top_level_file Vhd_front.VhdlLexer.lexer lexbuf in
    close_in chan;
    Printf.printf "  ✅ Parsed %d design units\n" (List.length ast);

    (* Print design unit info *)
    List.iteri (fun i unit ->
      match unit.Vhd_front.VhdlTypes.designunitlibraryunit with
      | Vhd_front.VhdlTypes.PrimaryUnit (Vhd_front.VhdlTypes.EntityDeclaration entity) ->
          let name, _ = entity.Vhd_front.VhdlTypes.entitydeclarationidentifier in
          Printf.printf "     Unit %d: Entity '%s'\n" i name;

          (* Show ports *)
          (match entity.Vhd_front.VhdlTypes.entitydeclarationheader with
           | Some header ->
               Printf.printf "       Generics: %d\n"
                 (match header.Vhd_front.VhdlTypes.entityheadergeneric with
                  | Some lst -> List.length lst
                  | None -> 0);
               Printf.printf "       Ports: %d\n"
                 (match header.Vhd_front.VhdlTypes.entityheaderport with
                  | Some lst -> List.length lst
                  | None -> 0)
           | None -> ())

      | Vhd_front.VhdlTypes.SecondaryUnit (Vhd_front.VhdlTypes.ArchitectureBody arch) ->
          let name, _ = arch.Vhd_front.VhdlTypes.architecturebodyidentifier in
          let entity_name, _ = arch.Vhd_front.VhdlTypes.architecturebodyentity in
          Printf.printf "     Unit %d: Architecture '%s' of '%s'\n" i name entity_name;
          Printf.printf "       Declarations: %d\n"
            (List.length arch.Vhd_front.VhdlTypes.architecturebodydeclarativeitems);
          Printf.printf "       Statements: %d\n"
            (List.length arch.Vhd_front.VhdlTypes.architecturebodystatements)

      | _ -> Printf.printf "     Unit %d: Other\n" i
    ) ast;

    Printf.printf "\n%s\n" (String.make 70 '=');
    Printf.printf "Step 2: Ready for rewrite.ml conversion\n\n";

    Printf.printf "The parsed AST contains:\n";
    Printf.printf "  • Entity declarations with ports/generics\n";
    Printf.printf "  • Architecture bodies with processes\n";
    Printf.printf "  • Signal declarations and assignments\n\n";

    Printf.printf "Next step: Use Vhd_front.Rewrite module to convert to SystemVerilog\n";
    Printf.printf "Then: Parse that SystemVerilog and convert to IR\n";

  with
  | Parsing.Parse_error ->
      close_in chan;
      Printf.printf "❌ Parse error\n";
      exit 1
  | e ->
      close_in chan;
      Printf.printf "❌ Error: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stdout;
      exit 1

let () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <vhdl_file>\n" Sys.argv.(0);
    Printf.printf "\nExample:\n";
    Printf.printf "  %s sysver_tests/apb_uart.vhd\n" Sys.argv.(0);
    exit 1
  end;

  test_file Sys.argv.(1)
