(* Test Nlview to Behavioral IR conversion *)

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <schematic.sch>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in

  (* Convert *)
  let prog = Nlview_to_behavioral.convert_and_report filename in

  (* Show sample of generated IR *)
  let bmod = List.hd prog.Behavioral_ir.modules in

  Printf.printf "\nSample signals (first 10):\n";
  List.iteri (fun i sig_ ->
    if i < 10 then
      let dir_str = match sig_.Behavioral_ir.direction with
        | `Input -> "input "
        | `Output -> "output"
        | `Internal -> "wire  "
      in
      Printf.printf "  %s %s : %s\n"
        dir_str
        sig_.name
        (Behavioral_ir.string_of_btype sig_.stype)
  ) bmod.signals;

  Printf.printf "\nSample assignments (first 10):\n";
  List.iter (fun proc ->
    match proc with
    | Behavioral_ir.BCombinational { body; _ } ->
        List.iteri (fun i stmt ->
          if i < 10 then
            match stmt with
            | Behavioral_ir.BAssign { lhs; rhs } ->
                Printf.printf "  assign %s = %s\n"
                  lhs
                  (Behavioral_ir.string_of_bexpr rhs)
            | _ -> ()
        ) body
    | _ -> ()
  ) bmod.processes;

  Printf.printf "\nHierarchical instances:\n";
  List.iteri (fun i inst ->
    if i < 10 then
      Printf.printf "  %s : %s\n"
        inst.Behavioral_ir.inst_name
        inst.module_name
  ) bmod.instances;

  Printf.printf "\nConversion complete!\n"

let _ = main ()
