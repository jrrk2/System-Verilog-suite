(* test_four_frontend_miter — pairwise Z3 miters across four
   independent SystemVerilog frontends: verible, slang, verilator,
   and sv-parser (dalance).

   usage:
     test_four_frontend_miter <top> <sv_file> <verilator_json>

   The sv-parser frontend reads the same .sv file as verible/slang;
   verilator alone needs the pre-rendered .json AST.  Runs six
   pairwise miters (4 choose 2) and reports each verdict on its own
   line.  Exit 0 only when all six agree; exit 1 if any pair DIFFERs;
   exit 2 if any pair errors out.

   Adding a fourth independent CST parser tightens the cross-check:
   when three of the four agree but one disagrees, the dissenting
   frontend is the one to investigate.  When the disagreement
   straddles parser families (e.g. verible+sv-parser vs slang+verilator)
   that points at a semantics question rather than a parser bug. *)

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
  if Array.length Sys.argv < 4 then begin
    Printf.eprintf
      "usage: %s <top> <sv_file> <verilator_json>\n" Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let sv  = Sys.argv.(2) in
  let vj  = Sys.argv.(3) in

  let p_v = load_bir ~frontend:"verible"   ~top ~files:[sv] in
  let p_s = load_bir ~frontend:"slang"     ~top ~files:[sv] in
  let p_l = load_bir ~frontend:"verilator" ~top ~files:[vj] in
  let p_p = load_bir ~frontend:"sv-parser" ~top ~files:[sv] in

  let m_v = Option.bind p_v (prep ~top) in
  let m_s = Option.bind p_s (prep ~top) in
  let m_l = Option.bind p_l (prep ~top) in
  let m_p = Option.bind p_p (prep ~top) in

  let run lbl_a m_a lbl_b m_b =
    match m_a, m_b with
    | Some ma, Some mb -> miter_pair lbl_a ma lbl_b mb
    | None,    _       -> `Err (Printf.sprintf "[%s] load failed" lbl_a)
    | _,    None       -> `Err (Printf.sprintf "[%s] load failed" lbl_b)
  in

  let v_s = run "verible"   m_v "slang"     m_s in
  let v_l = run "verible"   m_v "verilator" m_l in
  let v_p = run "verible"   m_v "sv-parser" m_p in
  let s_l = run "slang"     m_s "verilator" m_l in
  let s_p = run "slang"     m_s "sv-parser" m_p in
  let l_p = run "verilator" m_l "sv-parser" m_p in

  let print_pair (a, b, r) =
    let v = match r with
      | `Eq -> "EQUIVALENT"
      | `Diff -> "DIFFER"
      | `Err msg -> "ERROR: " ^ msg in
    Printf.printf "  %-30s %s\n" (a ^ " vs " ^ b) v in
  Printf.printf "\n4-way miter for %s\n" top;
  print_pair ("verible",   "slang",     v_s);
  print_pair ("verible",   "verilator", v_l);
  print_pair ("verible",   "sv-parser", v_p);
  print_pair ("slang",     "verilator", s_l);
  print_pair ("slang",     "sv-parser", s_p);
  print_pair ("verilator", "sv-parser", l_p);

  let pairs = [v_s; v_l; v_p; s_l; s_p; l_p] in
  let any_diff =
    List.exists (function `Diff -> true | _ -> false) pairs in
  let any_err =
    List.exists (function `Err _ -> true | _ -> false) pairs in
  if any_diff then exit 1
  else if any_err then exit 2
  else exit 0
