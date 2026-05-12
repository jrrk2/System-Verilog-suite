(* Stuck-at fault simulator + ATPG random-pattern coverage report.

   For every cell output net in the compiled netlist, enumerate two
   faults: stuck-at-0 and stuck-at-1.  For each fault, re-run the
   bit-parallel combinational simulator with that net forced to the
   stuck value, compare every PO and every FF.D net to the golden
   (fault-free) result, and call the fault "detected" if any output
   differs in any pattern.

   Stage 1 is the simple all-cells-all-faults loop.  Optimisations
   that will land in Stage 2:
     * cone-of-influence pruning (re-eval only cells downstream of
       the fault)
     * parallel-fault simulation (pack 64 faults into one re-eval)
     * fault dropping (skip already-detected faults on later patterns)

   Random patterns come from a seeded Random.State so runs are
   reproducible.                                                      *)

open Lib_map
open Gate_sim

(* ── Fault enumeration ──────────────────────────────────────────── *)

type fault = {
  f_net   : net_id;
  f_stuck : int;       (* 0 or 1 *)
}

let enumerate (c : compiled) : fault list =
  (* Stuck-at at every cell output net.  Could extend to per-input-pin
     faults (more thorough but ~4× the list); per-output is the
     standard collapsed fault list. *)
  let acc = ref [] in
  Array.iter (fun (_cell, _ins, out) ->
    if out >= 0 then begin
      acc := { f_net = out; f_stuck = 0 } :: !acc;
      acc := { f_net = out; f_stuck = 1 } :: !acc
    end
  ) c.c_evals;
  List.rev !acc

(* ── Pattern bench-vector ───────────────────────────────────────── *)

(* Build a closure that maps net-name → 64-bit pattern word.  PIs +
   FF.Q nets get one fresh random word each; everything else returns 0
   (never queried, but defensive). *)
let make_random_patterns ~seed (c : compiled) =
  let rng = Random.State.make [| seed |] in
  let by_name : (string, int) Hashtbl.t = Hashtbl.create 1024 in
  let alloc_word name =
    let hi = Random.State.bits rng in
    let lo = Random.State.bits rng in
    let mid = Random.State.bits rng in
    (* 3 × 30-bit chunks → 90 bits; mask to 64. *)
    let w = (hi lsl 30) lor (mid lsl 60) lor lo in
    Hashtbl.replace by_name name w in
  List.iter (fun nid -> alloc_word c.c_name_of_net.(nid)) c.c_pi_nets;
  List.iter (fun nid -> alloc_word c.c_name_of_net.(nid)) c.c_ff_q_nets;
  fun name ->
    try Hashtbl.find by_name name with Not_found -> 0

(* ── Coverage report ────────────────────────────────────────────── *)

type report = {
  r_module       : string;
  r_total_faults : int;
  r_detected     : int;
  r_patterns     : int;
  r_directed_attempts : int;
  r_directed_hits     : int;
}

let render_report (r : report) =
  let pct =
    if r.r_total_faults = 0 then 0.
    else 100. *. float_of_int r.r_detected /. float_of_int r.r_total_faults in
  let directed_line =
    if r.r_directed_attempts = 0 then ""
    else
      Printf.sprintf
        "Directed   : %d / %d justifications hit\n"
        r.r_directed_hits r.r_directed_attempts
  in
  Printf.sprintf
    "ATPG coverage report\n\
     ────────────────────\n\
     Module     : %s\n\
     Patterns   : %d  (one int = 64 patterns; bit-parallel)\n\
     Faults     : %d  (stuck-at-{0,1} at every cell output)\n\
     %sDetected   : %d  (%.2f %%)\n\
     Undetected : %d  (%.2f %%)\n"
    r.r_module r.r_patterns r.r_total_faults directed_line
    r.r_detected pct
    (r.r_total_faults - r.r_detected) (100. -. pct)

(* Simulate one fault: re-run the netlist with the fault net forced
   to the stuck-at value, compare against [golden] at every observable
   net (POs + FF.D nets).  Returns [true] if any pattern differs.    *)
let detected_with ~c ~golden ~fault ~input_pat : bool =
  let fault_fn out_net raw =
    if out_net = fault.f_net then
      (if fault.f_stuck = 0 then 0 else -1)
    else raw in
  let faulty = run ~fault:fault_fn c ~input_pat in
  (* Compare every PO and every FF.D net pattern-word: detected if
     any bit differs. *)
  let mismatch = ref false in
  List.iter (fun nid ->
    if not !mismatch && golden.(nid) lxor faulty.(nid) <> 0 then
      mismatch := true) c.c_po_nets;
  if not !mismatch then
    List.iter (fun nid ->
      if not !mismatch && golden.(nid) lxor faulty.(nid) <> 0 then
        mismatch := true) c.c_ff_d_nets;
  !mismatch

(* ── Top-level entry ────────────────────────────────────────────── *)

let run_atpg
    ?(seed = 0xC0DE)
    ?(n_pattern_words = 16)   (* 16 × 64 = 1024 patterns *)
    ~module_name
    (nl : netlist) : report =
  let c = compile nl in
  let faults = enumerate c in
  let n_faults = List.length faults in
  let detected_table = Array.make n_faults false in

  let debug = Sys.getenv_opt "ATPG_DEBUG" = Some "1" in
  if debug then
    Printf.eprintf "[atpg] %s: %d cells, %d faults, %d obs nets (%d POs + %d FF.D)\n%!"
      module_name (Array.length c.c_evals) n_faults
      (List.length c.c_po_nets + List.length c.c_ff_d_nets)
      (List.length c.c_po_nets) (List.length c.c_ff_d_nets);
  for word = 0 to n_pattern_words - 1 do
    let input_pat = make_random_patterns ~seed:(seed + word) c in
    let golden = run c ~input_pat in
    List.iteri (fun idx fault ->
      if not detected_table.(idx) then begin
        if detected_with ~c ~golden ~fault ~input_pat then
          detected_table.(idx) <- true
      end
    ) faults
  done;

  let n_detected_random =
    Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 detected_table in
  ignore n_detected_random;

  (* ── Directed pass — backwards-justify activation for each fault
     still undetected.  Random fills the rest of the input space, so
     each successful justification gets the same propagation lottery
     as a random pattern — but with the fault site GUARANTEED at the
     right polarity.  Most of the gain over pure random comes from
     activating deep AND/OR chains whose only-one-controlling-value
     activation pattern has a 1/2ⁿ random hit rate.  *)
  let directed_attempts = ref 0 in
  let directed_hits = ref 0 in
  let directed_seed = seed + n_pattern_words + 1 in
  let directed_rng = Random.State.make [| directed_seed |] in
  let random_word () =
    let a = Random.State.bits directed_rng in
    let b = Random.State.bits directed_rng in
    let c = Random.State.bits directed_rng in
    (a lsl 30) lor (c lsl 60) lor b in
  let consumers = Atpg_directed.consumers_of c in
  let dist_to_obs = Atpg_directed.propagation_distances c consumers in
  List.iteri (fun idx fault ->
    if not detected_table.(idx) then begin
      let needed_val = if fault.f_stuck = 0 then 1 else 0 in
      match Atpg_directed.justify_fault c ~consumers ~dist_to_obs
              ~target_net:fault.f_net ~target_val:needed_val with
      | None -> ()
      | Some pi_pat ->
          incr directed_attempts;
          (* Build a pattern word per PI / FF.Q: bit 0 = the
             justification value, bits 1..63 = randomised so the
             remaining inputs explore the propagation space.  64
             distinct patterns per fault — same as a random word
             but with bit 0 pinned. *)
          let input_pat name =
            let nid =
              try Hashtbl.find c.c_net_of_name name
              with Not_found -> -1 in
            let rand = random_word () in
            if nid < 0 then rand
            else
              match Hashtbl.find_opt pi_pat nid with
              | None -> rand
              | Some v ->
                  (* clear bit 0, set to required value *)
                  let cleared = rand land (lnot 1) in
                  if v = 1 then cleared lor 1 else cleared
          in
          let golden = run c ~input_pat in
          if detected_with ~c ~golden ~fault ~input_pat then begin
            detected_table.(idx) <- true;
            incr directed_hits
          end
    end
  ) faults;

  let n_detected =
    Array.fold_left (fun acc b -> if b then acc + 1 else acc) 0 detected_table in
  if Sys.getenv_opt "ATPG_DEBUG" = Some "1" then
    Printf.eprintf "[atpg] %s: random=%d, directed=%d/%d\n%!"
      module_name n_detected_random !directed_hits !directed_attempts;
  { r_module       = module_name;
    r_total_faults = n_faults;
    r_detected     = n_detected;
    r_patterns     = n_pattern_words * 64;
    r_directed_attempts = !directed_attempts;
    r_directed_hits = !directed_hits }
