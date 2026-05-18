(* test_two_frontend_miter — run the Z3 miter on the same SystemVerilog
   design parsed by two independent frontends.  Useful as a
   "uncorrelated parser" sanity check: if frontend A and frontend B
   were built independently and both produce BIRs the Z3 miter calls
   equivalent, the parser-side semantics agree.

   usage:
     test_two_frontend_miter <top> <sv_file> <frontendA> <frontendB>

   frontends: verible, verible-ext, slang, yosys, surelog, verilator
   (verilator wants a .json file, not the .sv source — pass a separately
   produced verilator --json-only output as <sv_file> in that case).

   Exits 0 on EQUIVALENT, 1 on DIFFER, 2 on parse/other error. *)

let () =
  if Array.length Sys.argv < 5 then begin
    Printf.eprintf
      "usage: %s <top> <sv_file> <frontendA> <frontendB>\n" Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let sv  = Sys.argv.(2) in
  let fa  = Sys.argv.(3) in
  let fb  = Sys.argv.(4) in
  let ok =
    try
      let p_a = Sv_lua.load_frontend ~frontend:fa ~top ~files:[sv] in
      let p_b = Sv_lua.load_frontend ~frontend:fb ~top ~files:[sv] in
      let find_mod p =
        match List.find_opt
                (fun (m : Behavioral_ir.bmodule) -> m.name = top) p.Behavioral_ir.modules with
        | Some m -> m
        | None -> (match p.modules with m :: _ -> m | [] -> failwith "no modules") in
      let m_a = find_mod p_a in
      let m_b = find_mod p_b in
      let m_a' = Sv_lua.prep_for_z3 m_a p_a in
      let m_b' = Sv_lua.prep_for_z3 m_b p_b in
      Z3_miter.check_miter_equivalence m_a' m_b'
    with e ->
      Printf.eprintf "[miter] error: %s\n" (Printexc.to_string e);
      exit 2
  in
  if ok then begin print_endline "EQUIVALENT"; exit 0 end
  else        begin print_endline "DIFFER";     exit 1 end
