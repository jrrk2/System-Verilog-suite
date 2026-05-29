(* test_verible_vhdl_miter — run the Z3 miter on a SV design (parsed
   by one of our frontends) vs a Vivado-VHDL-elaborated version of the
   same design (parsed by vhd_front).

   usage:
     test_verible_vhdl_miter <top> <sv_file> <vhdl_file> [frontend]

   The optional 4th argument picks the SV-side frontend: "verible"
   (default), "slang", "yosys", "verilator", "surelog".  For
   verilator, pass a verilator-emitted .json file in place of the
   .sv (the underlying sv_lua loader requires JSON).

   Exits 0 on EQUIVALENT, 1 on DIFFER, 2 on parse/other error. *)

let () =
  if Array.length Sys.argv < 4 then begin
    Printf.eprintf
      "usage: %s <top> <sv_file> <vhdl_file> [frontend=verible]\n"
      Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let sv  = Sys.argv.(2) in
  let vhd = Sys.argv.(3) in
  let frontend =
    if Array.length Sys.argv >= 5 then Sys.argv.(4) else "verible" in
  let ok =
    try
      let p_sv  = Sv_lua.load_frontend ~frontend ~top ~files:[sv] in
      let p_vhd = Sv_lua.load_frontend ~frontend:"vhdl" ~top ~files:[vhd] in
      let m_sv = match List.find_opt
                       (fun (m : Behavioral_ir.bmodule) -> m.name = top) p_sv.modules with
        | Some m -> m
        | None ->
            (match p_sv.modules with
             | m :: _ -> m
             | [] -> failwith "verible: no modules") in
      let m_vhd = match List.find_opt
                        (fun (m : Behavioral_ir.bmodule) -> m.name = top) p_vhd.modules with
        | Some m -> m
        | None ->
            (match p_vhd.modules with
             | m :: _ -> m
             | [] -> failwith "vhdl: no modules") in
      let m_sv'  = Sv_lua.prep_for_z3 m_sv  p_sv  in
      let m_vhd' = Sv_lua.prep_for_z3 m_vhd p_vhd in
      Z3_miter.check_miter_equivalence m_sv' m_vhd'
    with e ->
      Printf.eprintf "[miter] error: %s\n" (Printexc.to_string e);
      exit 2
  in
  if ok then begin print_endline "EQUIVALENT"; exit 0 end
  else        begin print_endline "DIFFER";     exit 1 end
