(* hardcaml-level cycle-accurate sim of the lowered picosoc_noflash,
 * driving the same {clk, resetn} sequence as the xsim miter and
 * tracing the principal CPU registers (cpu__cpu_state, cpu__reg_pc,
 * cpu__trap, cpu__mem_do_rinst, cpu__decoder_trigger,
 * cpu__latched_store, cpu__latched_branch, cpu__latched_rd).  Lets us
 * confirm whether the fetch->trap divergence already appears at the
 * hardcaml Circuit level (i.e. BIR->hardcaml is the bug) or only after
 * the RTL emit step.
 *
 * Usage: test_picosoc_hcsim <top> <file.sv> [more.sv ...] *)
open Hardcaml

let () =
  Unix.putenv "MEMLOWER_FPGA" "1";
  let top = Sys.argv.(1) in
  let files = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2)) in
  let prog = Verible_to_behavioral.convert_files ~top files in
  let flat = Behavioral_hier.flatten_for_z3 prog ~top in
  let prog = { Behavioral_ir.modules = [ flat ]; library_cells = prog.library_cells } in
  let prog =
    prog
    |> Behavioral_unroll.unroll_program
    |> Behavioral_inline.inline_program
    |> Behavioral_iflift.lift_program
    |> Behavioral_blocking_subst.blocking_subst_program
    |> Behavioral_meminfer.infer_program
  in
  let lowered, _ = Behavioral_memlower.lower_program prog in
  (* SSA after memlower: matches test_picosoc_gates.  Versions multi-
     write targets (e.g. picorv32 pcpi_mul's carry-save next_rd slice-
     write chain) so each write gets its own intermediate signal —
     without this, hardcaml sees a wire driven multiple times and
     ends up with an unassigned wire input at Circuit.create_exn. *)
  let lowered = { lowered with
    modules = List.map Behavioral_ssa.module_to_ssa lowered.modules } in
  let m = List.find (fun (m : Behavioral_ir.bmodule) -> m.name = top) lowered.modules in
  let circ =
    Behavioral_to_hardcaml.create_circuit ~emit_instances:true ~detect_loops:false m
  in
  Printf.eprintf "[hcsim] circuit built\n%!";

  (* Names of signals we want to observe. *)
  let watch_names = [
    "cpu__cpu_state"; "cpu__reg_pc"; "cpu__reg_next_pc"; "cpu__trap";
    "cpu__mem_do_rinst"; "cpu__decoder_trigger"; "cpu__latched_store";
    "cpu__latched_branch"; "cpu__latched_rd"; "cpu__decoded_rd";
    "cpu__do_waitirq"; "mem_valid"; "progmem__o_ready";
    "_231"; "_232"; "_7099"; "_7191"; "_7258"; "_7425"; "_7419"; "_7423"
  ] in
  let want_traced =
    let h = Hashtbl.create 32 in
    List.iter (fun n -> Hashtbl.add h n ()) watch_names; h in

  let config = {
    Cyclesim.Config.default with
    is_internal_port = Some (fun s ->
      List.exists (fun n -> Hashtbl.mem want_traced n) (Signal.names s));
  } in
  let sim = Cyclesim.create ~config circ in
  Printf.eprintf "[hcsim] sim ready: %d in_ports, %d out_ports\n%!"
    (List.length (Cyclesim.in_ports sim)) (List.length (Cyclesim.out_ports sim));

  let in_ports = Cyclesim.in_ports sim in
  List.iter (fun (n, _) -> Printf.eprintf "  in:  %s\n" n) in_ports;
  List.iter (fun (n, _) -> Printf.eprintf "  out: %s\n" n) (Cyclesim.out_ports sim);

  let find_in n =
    try List.assoc n in_ports
    with Not_found -> Printf.eprintf "missing input %s\n%!" n; exit 2 in
  let resetn_p = find_in "resetn" in
  let iomem_ready_p = try Some (find_in "iomem_ready") with _ -> None in
  let iomem_rdata_p = try Some (find_in "iomem_rdata") with _ -> None in
  let ser_rx_p = try Some (find_in "ser_rx") with _ -> None in

  (* Lookup helpers for traced internals. *)
  let lookup_name n =
    match Cyclesim.lookup_node_or_reg_by_name sim n with
    | Some node -> Some node
    | None -> None in
  let watch = List.map (fun n -> (n, lookup_name n)) watch_names in
  let read = function
    | Some node ->
        let bits = Cyclesim.Node.to_bits node in
        let w = Bits.width bits in
        Printf.sprintf "%d'h%s" w (Bits.to_constant bits |> Constant.to_hex_string ~signedness:Unsigned)
    | None -> "?" in

  (* Drive ser_rx=1, iomem_rdata=0, iomem_ready follows valid externally
   * if there's an iomem_valid output to look at.  Otherwise tie low. *)
  Option.iter (fun p -> p := Bits.of_int ~width:(Bits.width !p) 1) ser_rx_p;
  Option.iter (fun p -> p := Bits.of_int ~width:(Bits.width !p) 0) iomem_rdata_p;
  Option.iter (fun p -> p := Bits.of_int ~width:(Bits.width !p) 0) iomem_ready_p;

  (* 20 cycles of reset, then deasserted. *)
  resetn_p := Bits.of_int ~width:(Bits.width !resetn_p) 0;
  for _ = 1 to 20 do Cyclesim.cycle sim done;
  resetn_p := Bits.of_int ~width:(Bits.width !resetn_p) 1;

  Printf.printf "── hardcaml-level cycle trace (post-reset) ──\n";
  for c = 0 to 30 do
    Printf.printf "c=%d" c;
    List.iter (fun (n, node) ->
      Printf.printf " %s=%s" n (read node)
    ) watch;
    Printf.printf "\n";
    Cyclesim.cycle sim
  done
