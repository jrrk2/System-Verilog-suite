(* test_pass_trace.ml — runs the same prefix of synth_pipeline that
   precedes hardcaml lowering, dumping signal counts and the iCounter
   width after each transform.  Lets us pinpoint which pass drops a
   width-N counter into 0-bit. *)

let dump label (p : Behavioral_ir.bprogram) =
  Printf.printf "\n──── %s ────\n" label;
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "  module %s: %d signals, %d processes, %d instances\n"
      m.name (List.length m.signals) (List.length m.processes)
      (List.length m.instances);
    List.iter (fun (s : Behavioral_ir.bsignal) ->
      let w = match s.stype with
        | Behavioral_ir.BInt { width; _ } -> width
        | BBool -> 1
        | _ -> -1 in
      let dir = match s.direction with
        | `Input -> "in" | `Output -> "out" | `Internal -> "wire" in
      Printf.printf "    %-6s %-12s w=%d\n" dir s.name w
    ) m.signals
  ) p.modules

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "usage: %s <top> <file.sv>\n" Sys.argv.(0); exit 1
  end;
  let top = Sys.argv.(1) and file = Sys.argv.(2) in
  let p = Verible_to_behavioral.convert_files ~top [file] in
  dump "after Verible parse" p;
  let p = Behavioral_unroll.unroll_program p in
  dump "after unroll" p;
  let p = Behavioral_mem_merge.merge_program p in
  dump "after merge" p;
  let p = Behavioral_mem_merge.merge_slice_writes_program p in
  dump "after merge_slice_writes" p;
  let p = Behavioral_mem_merge.merge_bytewise_writes_program p in
  dump "after merge_bytewise_writes" p;
  let p = Behavioral_inline.inline_program p in
  dump "after inline" p;
  let p = Behavioral_iflift.lift_program p in
  dump "after iflift" p;
  let p = Behavioral_blocking_subst.blocking_subst_program p in
  dump "after blocking_subst" p;
  let p = Behavioral_meminfer.infer_program p in
  dump "after meminfer" p;
  (* Dump processes for slib_clock_div. *)
  Printf.printf "\n──── final processes ────\n";
  List.iter (fun (m : Behavioral_ir.bmodule) ->
    Printf.printf "\nmodule %s:\n" m.name;
    List.iteri (fun i (proc : Behavioral_ir.bprocess) ->
      Printf.printf "  process %d: %s\n" i
        (match proc with
         | BSequential _ -> "Sequential"
         | BCombinational _ -> "Combinational");
      let body = match proc with
        | BSequential { body; _ } -> body
        | BCombinational { body; _ } -> body in
      Printf.printf "%s\n"
        (String.concat "\n"
           (List.map (Behavioral_ir.string_of_bstmt 4) body))
    ) m.processes
  ) p.modules
