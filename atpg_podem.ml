(* PODEM (Path-Oriented DEcision-Making) — second cut, with correct
   state semantics.

   The Stage-3 implementation had two bugs that together explained why
   the search either hung (`picorv32 CPU`) or quietly returned 0 hits
   (`spimemio_xfer`):

     1. After a backtrack flipped a PI, the values array still held
        the old forward-implied intermediate values.  Imply would
        converge to a fixed point that *looked* consistent with the
        new PI but actually wasn't, because cells whose inputs hadn't
        changed in this iteration weren't reconsidered.

     2. The decision-stack pop logic conflated "tried the other
        polarity" with "exhausted both" — a decision flagged
        tried_other and then popped would lose the fact that *both*
        polarities had been tried, so when the same PI surfaced in a
        future backtrace it could be re-set.

   This rewrite fixes both:

     * Every PI / FF.Q assignment change triggers a full re-imply from
       a fresh array (PIs set, fault site set, everything else VX).
       Cheaper than it sounds — imply is O(cells × iters_to_converge),
       and for combinational netlists 3-4 iterations almost always
       suffice.

     * Decisions are tracked as a stack of (pi, v).  A separate
       [tried_alts : Hashtbl] records PIs whose *other* polarity has
       already been attempted.  Backtrack flips iff tried_alts doesn't
       already contain the PI; otherwise it pops deeper.

   Five-valued logic, cell coverage, and budget knobs are unchanged.   *)

open Gate_sim

type d_val = V0 | V1 | VX | VD | VDb

let inv_d = function
  | V0 -> V1 | V1 -> V0 | VX -> VX | VD -> VDb | VDb -> VD

let and2_d a b =
  match a, b with
  | V0, _ | _, V0 -> V0
  | V1, x | x, V1 -> x
  | VX, _ | _, VX -> VX
  | VD, VD -> VD
  | VDb, VDb -> VDb
  | VD, VDb | VDb, VD -> V0

let or2_d a b =
  match a, b with
  | V1, _ | _, V1 -> V1
  | V0, x | x, V0 -> x
  | VX, _ | _, VX -> VX
  | VD, VD -> VD
  | VDb, VDb -> VDb
  | VD, VDb | VDb, VD -> V1

let xor2_d a b =
  match a, b with
  | VX, _ | _, VX -> VX
  | V0, x | x, V0 -> x
  | V1, x | x, V1 -> inv_d x
  | VD, VD -> V0
  | VDb, VDb -> V0
  | VD, VDb | VDb, VD -> V1

let and_n = List.fold_left and2_d V1
let or_n  = List.fold_left or2_d  V0

let cell_family s =
  let s = String.uppercase_ascii s in
  try let i = String.rindex s '_' in
    if i + 1 < String.length s && s.[i+1] = 'X' then String.sub s 0 i else s
  with Not_found -> s

let eval_cell_d cell ins =
  match cell_family cell, ins with
  | "AND2", _ | "AND3", _ | "AND4", _ -> and_n ins
  | "OR2",  _ | "OR3",  _ | "OR4",  _ -> or_n  ins
  | "NAND2", _ | "NAND3", _ | "NAND4", _ -> inv_d (and_n ins)
  | "NOR2", _  | "NOR3", _  | "NOR4", _  -> inv_d (or_n  ins)
  | "XOR2", [a; b]  -> xor2_d a b
  | "XNOR2", [a; b] -> inv_d (xor2_d a b)
  | "INV",  [a]    -> inv_d a
  | "BUF",  [a]    -> a
  | "LOGIC0", []   -> V0
  | "LOGIC1", []   -> V1
  | "MUX2", [a; b; s] -> or2_d (and2_d a (inv_d s)) (and2_d b s)
  | "AOI21", [a; b; ci] -> inv_d (or2_d (and2_d a b) ci)
  | "OAI21", [a; b; ci] -> inv_d (and2_d (or2_d a b) ci)
  | _ -> VX

let is_d = function VD | VDb -> true | _ -> false

let controlling = function
  | "AND2" | "AND3" | "AND4" | "NAND2" | "NAND3" | "NAND4" -> Some V0
  | "OR2"  | "OR3"  | "OR4"  | "NOR2"  | "NOR3"  | "NOR4"  -> Some V1
  | _ -> None
let non_controlling fam =
  match controlling fam with Some V0 -> Some V1 | Some V1 -> Some V0 | _ -> None
let output_inverts = function
  | "NAND2" | "NAND3" | "NAND4" | "NOR2" | "NOR3" | "NOR4" | "INV" -> true
  | _ -> false

(* ── Incremental forward implication via worklist ────────────────
   Faster than the naïve "iterate-until-fixed-point" sweep because we
   only re-evaluate cells whose inputs actually changed.  Bounded by
   the netlist's combinational depth.                                *)

let consumers_of (c : compiled) : (net_id, int list) Hashtbl.t =
  let h = Hashtbl.create 1024 in
  Array.iteri (fun idx (_, ins, _) ->
    List.iter (fun in_net ->
      let cur = try Hashtbl.find h in_net with Not_found -> [] in
      Hashtbl.replace h in_net (idx :: cur)
    ) ins
  ) c.c_evals;
  h

(* Schedule consumers of every PI/seed onto the worklist, then evaluate
   each scheduled cell; if its output changes, schedule its consumers.
   Skips the cell driving [fault_net] so the fault stays pinned.      *)
let imply_from
    (c : compiled) (cons : (net_id, int list) Hashtbl.t)
    ~fault_net values seed_nets =
  let q = Queue.create () in
  let scheduled = Array.make (Array.length c.c_evals) false in
  let schedule_for_net n =
    match Hashtbl.find_opt cons n with
    | None -> ()
    | Some ids ->
        List.iter (fun i ->
          if not scheduled.(i) then begin
            scheduled.(i) <- true;
            Queue.push i q
          end
        ) ids in
  List.iter schedule_for_net seed_nets;
  (* Also seed cells with no inputs (LOGIC0/LOGIC1) so they get a
     starting value. *)
  Array.iteri (fun i (cell, ins, _) ->
    if ins = [] && (cell_family cell = "LOGIC0" || cell_family cell = "LOGIC1")
       && not scheduled.(i)
    then begin scheduled.(i) <- true; Queue.push i q end
  ) c.c_evals;
  let steps = ref 0 in
  let max_steps = Array.length c.c_evals * 8 in
  while not (Queue.is_empty q) && !steps < max_steps do
    incr steps;
    let i = Queue.pop q in
    scheduled.(i) <- false;
    let cell, ins, out = c.c_evals.(i) in
    if out >= 0 && out <> fault_net then begin
      let ins_v = List.map (fun n -> values.(n)) ins in
      let new_v = eval_cell_d cell ins_v in
      if new_v <> values.(out) then begin
        values.(out) <- new_v;
        match Hashtbl.find_opt cons out with
        | None -> ()
        | Some ids ->
            List.iter (fun j ->
              if not scheduled.(j) then begin
                scheduled.(j) <- true;
                Queue.push j q
              end
            ) ids
      end
    end
  done

(* ── D-frontier + observable check ───────────────────────────────── *)

let d_frontier (c : compiled) values =
  let acc = ref [] in
  Array.iteri (fun i (_, ins, out) ->
    if out >= 0 && values.(out) = VX
       && List.exists (fun n -> is_d values.(n)) ins
    then acc := i :: !acc
  ) c.c_evals;
  !acc

let d_at_observable (c : compiled) values =
  List.exists (fun n -> is_d values.(n)) c.c_po_nets
  || List.exists (fun n -> is_d values.(n)) c.c_ff_d_nets

(* Objective: an X-valued side input of the D-front cell closest to a
   PO/FF.D, target = non-controlling value of the cell's family. *)
let pick_objective (c : compiled) ~dist values =
  let cands = d_frontier c values in
  if cands = [] then None
  else begin
    (* Score every candidate.  Cells without a distance entry (output
       not reachable to an observable) sort last with a sentinel ∞
       rather than being filtered — sometimes the only viable cell on
       the D-front has no recorded distance but the next step's imply
       will give it one as side inputs get settled.                  *)
    let inf = max_int in
    let scored = List.map (fun i ->
      let _, _, out = c.c_evals.(i) in
      let d = try Hashtbl.find dist out with Not_found -> inf in
      (d, i)
    ) cands in
    let sorted = List.sort compare scored in
    (* Walk candidates in distance order; pick the first one we can
       actually act on (AND/OR family with an X side input).         *)
    let rec try_cands = function
      | [] -> None
      | (_, i) :: rest ->
          let cell, ins, _ = c.c_evals.(i) in
          let fam = cell_family cell in
          match non_controlling fam with
          | None -> try_cands rest
          | Some nc ->
              let d_inputs =
                List.filter (fun n -> is_d values.(n)) ins in
              let x_inputs =
                List.filter (fun n -> values.(n) = VX) ins in
              (* If all side inputs are already at non-controlling
                 value, the D should already be propagating — skip
                 (the cell will exit d-front on the next imply). *)
              if x_inputs = [] && d_inputs <> [] then try_cands rest
              else match x_inputs with
                | n :: _ -> Some (n, nc)
                | [] -> try_cands rest
    in
    try_cands sorted
  end

(* BFS distance from every net to the nearest PO/FF.D, walking
   *backwards* through driver cells.  Reused from Atpg_directed-style
   trick — used to bias the objective picker.                       *)
let distances_to_obs (c : compiled) : (net_id, int) Hashtbl.t =
  let dist = Hashtbl.create 1024 in
  let q = Queue.create () in
  List.iter (fun n -> Hashtbl.replace dist n 0; Queue.push n q) c.c_po_nets;
  List.iter (fun n ->
    if not (Hashtbl.mem dist n) then begin
      Hashtbl.replace dist n 0; Queue.push n q
    end) c.c_ff_d_nets;
  while not (Queue.is_empty q) do
    let n = Queue.pop q in
    let d = Hashtbl.find dist n in
    match Hashtbl.find_opt c.c_driver_of n with
    | None -> ()
    | Some idx ->
        let _, ins, _ = c.c_evals.(idx) in
        List.iter (fun in_net ->
          if not (Hashtbl.mem dist in_net) then begin
            Hashtbl.replace dist in_net (d + 1);
            Queue.push in_net q
          end
        ) ins
  done;
  dist

(* ── Backtrace from (net, want_val) to a PI/FF.Q ──────────────────

   Walks back through driver cells, picking a fanin whose value would
   satisfy the cell's output goal.  Bounded recursion depth (256) and
   visited-set guard against combinational loops the upstream pipeline
   might leave behind. *)
(* A net is a "primary" (externally controllable) net if either:
   - It's in c.c_pi_nets or c.c_ff_q_nets directly, OR
   - It has no cell driver (bit-level slice of a primary input bus —
     gate_sim emits `a[0]` etc as separate nets but cells from
     `Lib_map` only drive named outputs, so PI-bit nets land as
     undriven and externally controllable).
   The simpler check works in practice — Stage 2 it tightens.        *)
let is_primary (c : compiled) net =
  List.mem net c.c_pi_nets
  || List.mem net c.c_ff_q_nets
  || not (Hashtbl.mem c.c_driver_of net)

let backtrace (c : compiled) values net target =
  let visited : (net_id, unit) Hashtbl.t = Hashtbl.create 16 in
  let rec go net target depth =
    if depth >= 256 then None
    else if Hashtbl.mem visited net then None
    else begin
      Hashtbl.add visited net ();
      if is_primary c net then Some (net, target)
      else
        match Hashtbl.find_opt c.c_driver_of net with
        | None -> Some (net, target)
        | Some idx ->
            let cell, ins, _ = c.c_evals.(idx) in
            let fam = cell_family cell in
            let want_for_cell_out =
              if output_inverts fam then inv_d target else target in
            match fam with
            | "INV" -> (match ins with [a] -> go a (inv_d target) (depth+1) | _ -> None)
            | "BUF" -> (match ins with [a] -> go a target (depth+1) | _ -> None)
            | "AND2" | "AND3" | "AND4" | "OR2" | "OR3" | "OR4"
            | "NAND2" | "NAND3" | "NAND4" | "NOR2" | "NOR3" | "NOR4" ->
                let c_v = match controlling fam with Some v -> v | None -> V0 in
                let nc_v = match non_controlling fam with Some v -> v | None -> V1 in
                let want_input =
                  if want_for_cell_out = nc_v then nc_v else c_v in
                (match List.find_opt (fun n -> values.(n) = VX) ins with
                 | Some n -> go n want_input (depth+1)
                 | None -> None)
            | "XOR2" | "XNOR2" ->
                (match ins with
                 | [a; b] ->
                     (match values.(a), values.(b) with
                      | VX, _ -> go a V0 (depth+1)
                      | _, VX -> go b
                          (if want_for_cell_out = V0 then values.(a)
                           else inv_d values.(a))
                          (depth+1)
                      | _ -> None)
                 | _ -> None)
            | "MUX2" ->
                (match ins with
                 | [a; b; s] ->
                     (match values.(s) with
                      | VX -> go s V0 (depth+1)
                      | V0 -> go a target (depth+1)
                      | V1 -> go b target (depth+1)
                      | _ -> None)
                 | _ -> None)
            | _ -> None
    end
  in
  go net target 0

(* ── Main PODEM loop ────────────────────────────────────────────── *)

let extract_pattern (pi_state : (net_id, d_val) Hashtbl.t) =
  let pat = Hashtbl.create 32 in
  Hashtbl.iter (fun pi v ->
    match v with
    | V0 -> Hashtbl.replace pat pi 0
    | V1 -> Hashtbl.replace pat pi 1
    | _ -> ()
  ) pi_state;
  pat
let _ = is_d  (* may be used by future cone-pruning heuristic *)

let podem (c : compiled) ~cons ~dist ~target_net ~target_val ~budget =
  let n_nets = c.c_n_nets in
  let values = Array.make n_nets VX in
  let pi_state : (net_id, d_val) Hashtbl.t = Hashtbl.create 32 in
  let decisions : (net_id * d_val) Stack.t = Stack.create () in
  let tried_alt : (net_id, unit) Hashtbl.t = Hashtbl.create 16 in
  let fault_val_d = if target_val = 1 then VD else VDb in

  (* Reset values to "PIs at their current assignment, fault site at
     fault_val_d, everything else X" and forward-imply. *)
  let resim () =
    Array.fill values 0 n_nets VX;
    Hashtbl.iter (fun pi v -> values.(pi) <- v) pi_state;
    values.(target_net) <- fault_val_d;
    let seeds = ref [target_net] in
    Hashtbl.iter (fun pi _ -> seeds := pi :: !seeds) pi_state;
    imply_from c cons ~fault_net:target_net values !seeds
  in

  let push_decision pi v =
    Hashtbl.replace pi_state pi v;
    Stack.push (pi, v) decisions;
    resim ()
  in

  let rec backtrack () =
    if Stack.is_empty decisions then false
    else begin
      let (pi, v) = Stack.pop decisions in
      Hashtbl.remove pi_state pi;
      if Hashtbl.mem tried_alt pi then begin
        Hashtbl.remove tried_alt pi;
        backtrack ()
      end else begin
        let v' = inv_d v in
        Hashtbl.add tried_alt pi ();
        Hashtbl.replace pi_state pi v';
        Stack.push (pi, v') decisions;
        resim ();
        true
      end
    end
  in

  let t0 = Unix.gettimeofday () in
  let timeout_s =
    match Sys.getenv_opt "SV_DECOMP_PODEM_TIMEOUT_MS" with
    | Some s -> (try float_of_string s /. 1000. with _ -> 0.020)
    | None -> 0.020 in

  resim ();
  let result = ref None in
  let steps = ref 0 in
  let stop = ref false in
  let debug = Sys.getenv_opt "ATPG_PODEM_DEBUG" = Some "1" in
  if debug then begin
    let df = d_frontier c values in
    let with_dist = List.filter (fun i ->
      let _, _, out = c.c_evals.(i) in
      Hashtbl.mem dist out) df in
    Printf.eprintf "[podem] start fault@%d val=%d D-front=%d (with-dist=%d, total nets w/ dist=%d)\n%!"
      target_net target_val (List.length df)
      (List.length with_dist) (Hashtbl.length dist)
  end;
  while not !stop do
    incr steps;
    if !steps > budget
       || Unix.gettimeofday () -. t0 > timeout_s
    then stop := true
    else if d_at_observable c values then begin
      result := Some (extract_pattern pi_state);
      stop := true
    end else begin
      match pick_objective c ~dist values with
      | Some (objective_net, objective_val) ->
          if debug && !steps < 8 then
            Printf.eprintf "[podem] step %d obj=(net %d, val %s) d-front=%d\n%!"
              !steps objective_net
              (match objective_val with V0 -> "0" | V1 -> "1"
                                      | VD -> "D" | VDb -> "D'" | VX -> "X")
              (List.length (d_frontier c values));
          (match backtrace c values objective_net objective_val with
           | Some (pi, pv) ->
               if debug && !steps < 8 then
                 Printf.eprintf "[podem]   backtrace → pi=%d val=%s state=%s\n%!"
                   pi (match pv with V0 -> "0" | V1 -> "1" | _ -> "?")
                   (if Hashtbl.mem pi_state pi then "already-set" else "fresh");
               if Hashtbl.mem pi_state pi then begin
                 if not (backtrack ()) then stop := true
               end else
                 push_decision pi pv
           | None ->
               if debug && !steps < 8 then
                 Printf.eprintf "[podem]   backtrace returned None\n%!";
               if not (backtrack ()) then stop := true)
      | None ->
          if not (backtrack ()) then stop := true
    end
  done;
  if debug then begin
    let r = match !result with Some _ -> "FOUND" | None -> "FAIL" in
    Printf.eprintf "[podem] fault@%d %s after %d steps (decisions=%d)\n%!"
      target_net r !steps (Stack.length decisions)
  end;
  !result
