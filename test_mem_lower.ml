(* Drive memlower on a parsed SV file:
 *   1. parse via Verible → BIR
 *   2. run unroll/inline/iflift/blocking_subst/meminfer
 *   3. apply Behavioral_memlower.lower_program
 *   4. dump the resulting BIR
 *
 * Usage: test_mem_lower <top> <file.sv> [more.sv ...] *)

let usage () =
  prerr_endline "usage: test_mem_lower <top> <file.sv> [more ...]";
  exit 2

let () =
  if Array.length Sys.argv < 3 then usage ();
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  Printf.printf "[memlower] parsing %s …\n" top;
  let prog = Verible_to_behavioral.convert_files ~top files in
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  Printf.printf "Pre-lower:\n%s\n\n" (Behavioral_ir.string_of_bprogram prog);
  let lowered, arts = Behavioral_memlower.lower_program prog in
  Printf.printf "Post-lower:\n%s\n\n" (Behavioral_ir.string_of_bprogram lowered);
  Printf.printf "Generated macros (%d):\n" (List.length arts);
  List.iter (fun a ->
    Printf.printf "  %s\n    .v   %s\n    .lib %s\n"
      a.Mem_macro_resolve.module_name
      a.verilog_path a.liberty_path
  ) arts
