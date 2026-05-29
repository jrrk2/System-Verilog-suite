(* Dump BIR after each pass to localize a combinational-loop introduction.
 * Usage: test_loop_dump <top> <file.v> [more ...] *)
let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let prog = Verible_to_behavioral.convert_files ~top files in
  let flat = Behavioral_hier.flatten_for_z3 prog ~top in
  let prog = { Behavioral_ir.modules = [ flat ]; library_cells = prog.library_cells } in
  let dump tag (p : Behavioral_ir.bprogram) =
    let m = List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) p.modules in
    Printf.printf "\n========== after %s ==========\n%s\n%!" tag
      (Behavioral_ir.string_of_bmodule m)
  in
  dump "flatten" prog;
  let p = Behavioral_unroll.unroll_program prog in dump "unroll" p;
  let p = Behavioral_inline.inline_program p in dump "inline" p;
  let p = Behavioral_iflift.lift_program p in dump "iflift" p;
  let p = Behavioral_blocking_subst.blocking_subst_program p in dump "blocking_subst" p;
  let p = Behavioral_meminfer.infer_program p in dump "meminfer" p;
  let lowered, _ = Behavioral_memlower.lower_program p in dump "memlower" lowered
