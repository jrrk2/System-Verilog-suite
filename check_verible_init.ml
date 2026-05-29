let () =
  let file = "/home/jonathan/picosoc_build/telegraph.v.declinit" in
  let prog = Verible_to_behavioral.convert_files ~top:"sonata_top" [ file ] in
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.eprintf "module %s: %d signals\n" m.name (List.length m.signals);
    List.iter (fun (s : Behavioral_ir.bsignal) ->
      let iv = match s.initial_value with
        | None -> "none"
        | Some (BConst { value; width }) -> Printf.sprintf "BConst(%d,%d)" value width
        | Some _ -> "OTHER"
      in
      Printf.eprintf "  %s : %s\n" s.name iv
    ) m.signals
  ) prog.modules
