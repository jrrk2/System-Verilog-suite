(* test_uart_vhdl_5way_miter — pairwise Z3 miters across four
   SystemVerilog frontends (verible, slang, verilator, sv-parser)
   plus a parallel VHDL implementation as a fifth independent oracle.

   usage:
     test_uart_vhdl_5way_miter <top> <sv_file> <verilator_json> <vhd_file>

   Runs ten pairwise miters (5 choose 2).  When the VHDL version and
   one of the SystemVerilog frontends agree against the other three,
   that triangulates a likely SV-frontend BIR bug (the VHDL spec
   serves as a cross-language reference).  When the VHDL version
   disagrees with the SV consensus, the .vhd is the suspect. *)

let load_bir ~frontend ~top ~files : Behavioral_ir.bprogram option =
  try Some (Sv_lua.load_frontend ~frontend ~top ~files)
  with e ->
    Printf.eprintf "[%s] %s\n" frontend (Printexc.to_string e);
    None

let prep ~top (p : Behavioral_ir.bprogram) : Behavioral_ir.bmodule option =
  match List.find_opt
          (fun (m : Behavioral_ir.bmodule) -> m.name = top) p.modules with
  | Some m -> Some (Sv_lua.prep_for_z3 m p)
  | None ->
      (match p.modules with
       | m :: _ -> Some (Sv_lua.prep_for_z3 m p)
       | [] -> None)

let miter_pair label_a m_a label_b m_b : [`Eq | `Diff | `Err of string] =
  try
    if Z3_miter.check_miter_equivalence m_a m_b then `Eq else `Diff
  with e ->
    `Err (Printf.sprintf "[%s vs %s] %s"
            label_a label_b (Printexc.to_string e))

let () =
  if Array.length Sys.argv < 5 then begin
    Printf.eprintf
      "usage: %s <top> <sv_file> <verilator_json> <vhd_file>\n"
      Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let sv  = Sys.argv.(2) in
  let vj  = Sys.argv.(3) in
  let vhd = Sys.argv.(4) in
  (* Load every .vhd in the top's directory, not just the named file,
     so the top architecture's component instantiations resolve against
     their sibling entities and the VHDL design flattens like the SV
     frontends (otherwise the sub-entities are missing and vhdl comes
     out a hollow top). *)
  let vhd_files =
    let dir = Filename.dirname vhd in
    match Sys.readdir dir with
    | entries ->
        Array.to_list entries
        |> List.filter (fun f -> Filename.check_suffix f ".vhd")
        |> List.map (fun f -> Filename.concat dir f)
        |> (fun fs -> if fs = [] then [vhd] else fs)
    | exception _ -> [vhd]
  in

  let p_v = load_bir ~frontend:"verible"   ~top ~files:[sv] in
  let p_s = load_bir ~frontend:"slang"     ~top ~files:[sv] in
  let p_l = load_bir ~frontend:"verilator" ~top ~files:[vj] in
  let p_p = load_bir ~frontend:"sv-parser" ~top ~files:[sv] in
  let p_h = load_bir ~frontend:"vhdl"      ~top ~files:vhd_files in

  let m_v = Option.bind p_v (prep ~top) in
  let m_s = Option.bind p_s (prep ~top) in
  let m_l = Option.bind p_l (prep ~top) in
  let m_p = Option.bind p_p (prep ~top) in
  let m_h = Option.bind p_h (prep ~top) in

  let run lbl_a m_a lbl_b m_b =
    match m_a, m_b with
    | Some ma, Some mb -> miter_pair lbl_a ma lbl_b mb
    | None,    _       -> `Err (Printf.sprintf "[%s] load failed" lbl_a)
    | _,    None       -> `Err (Printf.sprintf "[%s] load failed" lbl_b)
  in

  let v_s = run "verible"   m_v "slang"     m_s in
  let v_l = run "verible"   m_v "verilator" m_l in
  let v_p = run "verible"   m_v "sv-parser" m_p in
  let v_h = run "verible"   m_v "vhdl"      m_h in
  let s_l = run "slang"     m_s "verilator" m_l in
  let s_p = run "slang"     m_s "sv-parser" m_p in
  let s_h = run "slang"     m_s "vhdl"      m_h in
  let l_p = run "verilator" m_l "sv-parser" m_p in
  let l_h = run "verilator" m_l "vhdl"      m_h in
  let p_h2 = run "sv-parser" m_p "vhdl"     m_h in

  let print_pair (a, b, r) =
    let v = match r with
      | `Eq -> "EQUIVALENT"
      | `Diff -> "DIFFER"
      | `Err msg -> "ERROR: " ^ msg in
    Printf.printf "  %-30s %s\n" (a ^ " vs " ^ b) v in
  Printf.printf "\n5-way miter (with VHDL) for %s\n" top;
  print_pair ("verible",   "slang",     v_s);
  print_pair ("verible",   "verilator", v_l);
  print_pair ("verible",   "sv-parser", v_p);
  print_pair ("verible",   "vhdl",      v_h);
  print_pair ("slang",     "verilator", s_l);
  print_pair ("slang",     "sv-parser", s_p);
  print_pair ("slang",     "vhdl",      s_h);
  print_pair ("verilator", "sv-parser", l_p);
  print_pair ("verilator", "vhdl",      l_h);
  print_pair ("sv-parser", "vhdl",      p_h2);

  let pairs = [v_s; v_l; v_p; v_h; s_l; s_p; s_h; l_p; l_h; p_h2] in
  let any_diff =
    List.exists (function `Diff -> true | _ -> false) pairs in
  let any_err =
    List.exists (function `Err _ -> true | _ -> false) pairs in
  if any_diff then exit 1
  else if any_err then exit 2
  else exit 0
