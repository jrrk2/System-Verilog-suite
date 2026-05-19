(* test_three_frontend_miter — pairwise Z3 miters across three
   independent SystemVerilog frontends: verible, slang, and verilator.

   usage:
     test_three_frontend_miter <top> <sv_file> <verilator_json>

   Runs three pairwise Z3 miters:
       verible   ≡ slang
       verible   ≡ verilator
       slang     ≡ verilator
   Reports each pair's verdict on a separate line and exits 0 only
   when all three agree.  Exit 1 if any pair DIFFERs; exit 2 if any
   pair errors out (parser/Z3 exception).

   Pairwise verification gives a stronger signal than a single
   verible-vs-slang miter: when all three agree, two independent
   parsers cross-check the third.  When one frontend disagrees with
   the other two, that's the one to investigate.                  *)

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

  let m_v = Option.bind p_v (prep ~top) in
  let m_s = Option.bind p_s (prep ~top) in
  let m_l = Option.bind p_l (prep ~top) in

  let run lbl_a m_a lbl_b m_b =
    match m_a, m_b with
    | Some ma, Some mb -> miter_pair lbl_a ma lbl_b mb
    | None,    _       -> `Err (Printf.sprintf "[%s] load failed" lbl_a)
    | _,    None       -> `Err (Printf.sprintf "[%s] load failed" lbl_b)
  in

  let v_s = run "verible"   m_v "slang"     m_s in
  let v_l = run "verible"   m_v "verilator" m_l in
  let s_l = run "slang"     m_s "verilator" m_l in

  let print_pair (a, b, r) =
    let v = match r with
      | `Eq -> "EQUIVALENT"
      | `Diff -> "DIFFER"
      | `Err msg -> "ERROR: " ^ msg in
    Printf.printf "  %-25s %s\n" (a ^ " vs " ^ b) v in
  Printf.printf "\n3-way miter for %s\n" top;
  print_pair ("verible",   "slang",     v_s);
  print_pair ("verible",   "verilator", v_l);
  print_pair ("slang",     "verilator", s_l);

  let any_diff =
    List.exists (function `Diff -> true | _ -> false) [v_s; v_l; s_l] in
  let any_err =
    List.exists (function `Err _ -> true | _ -> false) [v_s; v_l; s_l] in
  if any_diff then exit 1
  else if any_err then exit 2
  else exit 0
