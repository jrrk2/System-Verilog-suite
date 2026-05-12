(* Stage-1 ATPG entry point.

   Reads a SV file, runs the full synth_pipeline (with scan insertion
   forced on so FFs are externally controllable), and runs random-
   pattern fault simulation per module.  Prints a coverage report.

   Usage:
     test_atpg <file.sv> [top] [pattern-words]

   With pattern-words = 16 (default) the simulator runs 16 × 64 = 1024
   random patterns per module.  Increase to push coverage higher on
   designs that random alone can't reach the deep AND-of-many-inputs
   patterns of.                                                         *)

let derive_top path =
  let base = Filename.basename path in
  try Filename.chop_extension base with Invalid_argument _ -> base

(* Args:
     test_atpg <file1.sv> [<fileN.sv> ...] [--top NAME] [--words N]
   Or with positional shorthand (legacy):
     test_atpg <file.sv> <top> <pattern-words>
   Anything ending in .sv / .v is a file; non-flag remaining positionals
   fill --top and --words in order.                                     *)
let parse_args argv =
  let files = ref [] in
  let top = ref None in
  let n_words = ref 16 in
  let positional = ref [] in
  let i = ref 1 in
  while !i < Array.length argv do
    let a = argv.(!i) in
    (match a with
     | "--top" -> incr i; top := Some argv.(!i)
     | "--words" -> incr i; n_words := int_of_string argv.(!i)
     | s when Filename.check_suffix s ".sv"
            || Filename.check_suffix s ".v" -> files := s :: !files
     | s -> positional := s :: !positional);
    incr i
  done;
  (match !top, List.rev !positional with
   | None, t :: _ -> top := Some t
   | _ -> ());
  (match List.rev !positional with
   | _ :: w :: _ -> (try n_words := int_of_string w with _ -> ())
   | _ -> ());
  (List.rev !files,
   (match !top with Some t -> t
                  | None -> derive_top (List.hd (List.rev !files))),
   !n_words)

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test_atpg <file1.sv> [<fileN.sv> ...] [--top NAME] [--words N]";
    exit 1
  end;
  let files, top, n_words = parse_args Sys.argv in
  let path = List.hd files in
  ignore path;
  Unix.putenv "SV_DECOMP_SCAN" "1";
  (* DCE / kary_merge can prune the small smoke-test designs to empty;
     turn them off here unless the user explicitly overrides. *)
  if Sys.getenv_opt "SV_DECOMP_NO_DCE" = None then
    Unix.putenv "SV_DECOMP_NO_DCE" "1";
  if Sys.getenv_opt "SV_DECOMP_NO_KARY_MERGE" = None then
    Unix.putenv "SV_DECOMP_NO_KARY_MERGE" "1";
  Printf.printf "ATPG smoke test: top=%s, %d × 64 = %d patterns\n%!"
    top n_words (n_words * 64);
  let netlists, _ =
    Synth_pipeline.run ~emit_verilog:false
      ~top ~out_path:"/tmp/_atpg_ignored.v" ~files () in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    Printf.printf "\n=== %s: %d cells, %d wires, %d inputs, %d outputs, %d assigns ===\n%!"
      mn.mn_name
      (List.length mn.mn_netlist.insts)
      (List.length mn.mn_netlist.wires)
      (List.length mn.mn_netlist.inputs)
      (List.length mn.mn_netlist.outputs)
      (List.length mn.mn_netlist.assigns);
    if mn.mn_netlist.insts = [] then
      Printf.printf "  (empty netlist — skipping fault sim)\n"
    else
      let r =
        Fault_sim.run_atpg ~n_pattern_words:n_words
          ~module_name:mn.mn_name mn.mn_netlist in
      print_endline (Fault_sim.render_report r)
  ) netlists
