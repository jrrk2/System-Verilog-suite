(* test_vss_miter — 3-way pairwise miter across verible, slang, and
   sv-parser.  Skips both verilator (for codebases that trip its
   --json-only path) and synlig (which segfaults on the 50K-line
   verilator-preprocessed cva6 input).

   Usage:
     test_vss_miter <top> <sv_file>

   Three pairwise miters; exit 0 only when all agree, 1 if any
   DIFFER, 2 if any error. *)

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
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "usage: %s <top> <sv_file>\n" Sys.argv.(0);
    exit 2
  end;
  let top = Sys.argv.(1) in
  let sv  = Sys.argv.(2) in

  let p_v = load_bir ~frontend:"verible"   ~top ~files:[sv] in
  let p_s = load_bir ~frontend:"slang"     ~top ~files:[sv] in
  let p_p = load_bir ~frontend:"sv-parser" ~top ~files:[sv] in

  let m_v = Option.bind p_v (prep ~top) in
  let m_s = Option.bind p_s (prep ~top) in
  let m_p = Option.bind p_p (prep ~top) in

  let run lbl_a m_a lbl_b m_b =
    match m_a, m_b with
    | Some ma, Some mb -> miter_pair lbl_a ma lbl_b mb
    | None,    _       -> `Err (Printf.sprintf "[%s] load failed" lbl_a)
    | _,    None       -> `Err (Printf.sprintf "[%s] load failed" lbl_b)
  in

  let pairs = [
    "verible",   "slang",     run "verible"   m_v "slang"     m_s;
    "verible",   "sv-parser", run "verible"   m_v "sv-parser" m_p;
    "slang",     "sv-parser", run "slang"     m_s "sv-parser" m_p;
  ] in

  let label_of = function
    | `Eq -> "EQUIVALENT"
    | `Diff -> "DIFFER"
    | `Err msg -> "ERROR: " ^ msg in
  Printf.printf "\n3-way (verible/slang/sv-parser) miter for %s\n" top;
  List.iter (fun (a, b, r) ->
    Printf.printf "  %-30s %s\n" (a ^ " vs " ^ b) (label_of r)) pairs;

  let verdicts = List.map (fun (_, _, r) -> r) pairs in
  let any_diff = List.exists (function `Diff -> true | _ -> false) verdicts in
  let any_err  = List.exists (function `Err _ -> true | _ -> false) verdicts in
  if any_diff then exit 1
  else if any_err then exit 2
  else exit 0
