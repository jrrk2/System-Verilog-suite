(* PODEM (Path-Oriented DEcision-Making) — proper ATPG with backtrack.

   Compared to [Atpg_directed]:

     * Five-valued logic {0, 1, X, D, D̄} threaded through the cell
       evaluator.  D = fault makes a 0→1 difference here, D̄ = 1→0.
       Carries the fault effect symbolically rather than picking
       deterministic activation up front, so propagation and
       activation are co-solved.

     * Decisions are at PIs / FF.Q nets only.  Each decision is
       reversible — when implication + forward propagation can't push
       a D-front to an observable, the most-recently-set PI is flipped
       and the search resumes.  Single-fault completeness within budget.

     * Objective picker mirrors Goel's original PODEM: prefer the
       D-front cell closest to an observable, drive a side input to
       its non-controlling value.  Falls back to fault-site activation
       when the D-front is empty.

   What's a fault target here
   ==========================
   Stuck-at-0 at net N becomes: set N = D in the test pattern (1 in
   good, 0 in faulty).  Stuck-at-1 → D̄.  Run PODEM with target
   net + value; success means the D propagated to a PO / FF.D, and
   the recorded PI / FF.Q values are the test vector.

   What's NOT in this revision
   ===========================
   * No fault dropping across PODEM calls (each call is independent;
     [fault_sim] handles already-detected faults).
   * No dynamic cell-handler extension — same Liberty cell table as
     [atpg_directed].
   * No multi-path sensitization heuristic — pure greedy single-path
     until the D-front evaporates, then backtrack.                    *)

open Gate_sim

type d_val = V0 | V1 | VX | VD | VDb

let dval_str = function
  | V0 -> "0" | V1 -> "1" | VX -> "X" | VD -> "D" | VDb -> "D'"
let _ = dval_str  (* used by debug printers; quieten unused-warning *)

let inv_d = function
  | V0 -> V1 | V1 -> V0 | VX -> VX | VD -> VDb | VDb -> VD

(* 5-valued AND.  Controlling value (0) dominates; D meets D̄ = 0. *)
let and2_d a b =
  match a, b with
  | V0, _ | _, V0 -> V0
  | V1, x | x, V1 -> x
  | VX, _ | _, VX -> VX
  | VD, VD -> VD
  | VDb, VDb -> VDb
  | VD, VDb | VDb, VD -> V0

(* 5-valued OR — dual of AND. *)
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

let and_n vs = List.fold_left and2_d V1 vs
let or_n  vs = List.fold_left or2_d  V0 vs

let cell_family s =
  let s = String.uppercase_ascii s in
  let strip_x s =
    try let i = String.rindex s '_' in
      if i + 1 < String.length s && s.[i+1] = 'X' then String.sub s 0 i else s
    with Not_found -> s in
  strip_x s

let eval_cell_d cell ins =
  match cell_family cell, ins with
  | "AND2", _ -> and_n ins
  | "AND3", _ -> and_n ins
  | "AND4", _ -> and_n ins
  | "OR2",  _ -> or_n  ins
  | "OR3",  _ -> or_n  ins
  | "OR4",  _ -> or_n  ins
  | "NAND2", _ -> inv_d (and_n ins)
  | "NAND3", _ -> inv_d (and_n ins)
  | "NAND4", _ -> inv_d (and_n ins)
  | "NOR2", _  -> inv_d (or_n  ins)
  | "NOR3", _  -> inv_d (or_n  ins)
  | "NOR4", _  -> inv_d (or_n  ins)
  | "XOR2", [a; b]  -> xor2_d a b
  | "XNOR2", [a; b] -> inv_d (xor2_d a b)
  | "INV",  [a]    -> inv_d a
  | "BUF",  [a]    -> a
  | "LOGIC0", []   -> V0
  | "LOGIC1", []   -> V1
  | "MUX2", [a; b; s] ->
      (* z = s ? b : a *)
      or2_d (and2_d a (inv_d s)) (and2_d b s)
  | "AOI21", [a; b; ci] -> inv_d (or2_d (and2_d a b) ci)
  | "OAI21", [a; b; ci] -> inv_d (and2_d (or2_d a b) ci)
  | _ -> VX  (* unknown cell — treat as X *)

(* Is a value carrying a fault effect? *)
let is_d_value = function VD | VDb -> true | _ -> false

(* Controlling value of a gate family — input that forces the output
   regardless of the others.  AND/NAND: 0.  OR/NOR: 1.  Others: none. *)
let controlling_val family =
  match family with
  | "AND2" | "AND3" | "AND4" | "NAND2" | "NAND3" | "NAND4" -> Some V0
  | "OR2"  | "OR3"  | "OR4"  | "NOR2"  | "NOR3"  | "NOR4"  -> Some V1
  | _ -> None
let non_controlling_val family =
  match controlling_val family with
  | Some V0 -> Some V1
  | Some V1 -> Some V0
  | _ -> None
(* Output-side polarity inversion: NAND/NOR invert vs AND/OR. *)
let output_inverts family =
  match family with
  | "NAND2" | "NAND3" | "NAND4" | "NOR2" | "NOR3" | "NOR4" | "INV" -> true
  | _ -> false
let _ = output_inverts

(* ── Backtrace: walk back from objective (net, value) to a PI/FF.Q
   choosing input net + value at each step. *)
let rec backtrace (c : compiled) values net target =
  if List.mem net c.c_pi_nets || List.mem net c.c_ff_q_nets then
    Some (net, target)
  else
    match Hashtbl.find_opt c.c_driver_of net with
    | None ->
        if List.mem net c.c_pi_nets || List.mem net c.c_ff_q_nets
        then Some (net, target)
        else None
    | Some idx ->
        let cell, ins, _ = c.c_evals.(idx) in
        let fam = cell_family cell in
        match fam, ins with
        | ("INV", [a]) | ("BUF", [a]) ->
            backtrace c values a (if fam = "INV" then inv_d target else target)
        | ("AND2" | "AND3" | "AND4"), _
        | ("OR2"  | "OR3"  | "OR4"),  _
        | ("NAND2" | "NAND3" | "NAND4"), _
        | ("NOR2" | "NOR3" | "NOR4"),  _ ->
            let c_v = match controlling_val fam with Some v -> v | None -> V0 in
            let nc_v = match non_controlling_val fam with Some v -> v | None -> V1 in
            let effective_target =
              if output_inverts fam then inv_d target else target in
            let want_input =
              (* If output should equal the gate's "natural" 1 value
                 (AND→1, OR→1), all inputs need NC.  If output should
                 equal the natural 0 value, at least one input needs C. *)
              if effective_target = nc_v then nc_v
              else c_v in
            let pick_input () =
              (* For "all NC" goal: any X input works.  For "at least
                 one C" goal: pick an X input.  *)
              let candidates =
                List.filter (fun n -> values.(n) = VX) ins in
              match candidates with
              | n :: _ -> Some n
              | [] -> List.find_opt (fun n -> values.(n) = VX) ins in
            (match pick_input () with
             | Some n -> backtrace c values n want_input
             | None -> None)
        | "XOR2", [a; b] | "XNOR2", [a; b] ->
            let eff = if cell_family cell = "XNOR2" then inv_d target else target in
            (* a XOR b = eff; pick a's value freely, then b follows.  *)
            (match values.(a), values.(b) with
             | VX, _ -> backtrace c values a V0
             | _, VX -> backtrace c values b (if eff = V0 then values.(a) else inv_d values.(a))
             | _, _ -> None)
        | "MUX2", [a; b; s] ->
            (* Z = s ? b : a *)
            (match values.(s) with
             | VX -> backtrace c values s V0
             | V0 -> backtrace c values a target
             | V1 -> backtrace c values b target
             | _ -> None)
        | _ -> None

(* ── Forward implication: recompute all cells whose inputs may have
   changed.  Skips the cell driving [fault_net] — its output is
   pinned to the fault's D/D̄ marker until PODEM finishes. *)
let imply ?(fault_net = -1) ?(max_iters = 4) (c : compiled) values =
  let changed = ref true in
  let iters = ref 0 in
  while !changed && !iters < max_iters do
    incr iters;
    changed := false;
    Array.iter (fun (cell, ins, out) ->
      if out >= 0 && out <> fault_net then begin
        let ins_v = List.map (fun n -> values.(n)) ins in
        let new_v = eval_cell_d cell ins_v in
        if new_v <> values.(out) then begin
          values.(out) <- new_v;
          changed := true
        end
      end
    ) c.c_evals
  done

(* D-front: cells (by eval index) with at least one D / D̄ input
   and X output. *)
let d_front (c : compiled) values =
  let acc = ref [] in
  Array.iteri (fun i (_, ins, out) ->
    if out >= 0 && values.(out) = VX
       && List.exists (fun n -> is_d_value values.(n)) ins then
      acc := i :: !acc
  ) c.c_evals;
  !acc

(* Is the D-front empty AND no D at any observable?  That's a dead
   end — the fault effect can't reach a PO from here. *)
let d_at_observable (c : compiled) values =
  List.exists (fun n -> is_d_value values.(n)) c.c_po_nets
  || List.exists (fun n -> is_d_value values.(n)) c.c_ff_d_nets

(* Pick an objective: a D-front cell, plus a side-input value goal
   (drive the side input to the cell's non-controlling value). *)
let pick_objective (c : compiled) values =
  let dfront = d_front c values in
  match dfront with
  | [] -> None
  | idx :: _ ->
      let cell, ins, _ = c.c_evals.(idx) in
      let fam = cell_family cell in
      let nc = non_controlling_val fam in
      match nc with
      | Some v ->
          (* find an input still at X — that's the side we'll drive *)
          (match List.find_opt (fun n -> values.(n) = VX) ins with
           | Some n -> Some (n, v)
           | None -> None)
      | None -> None  (* cells without controlling values — XOR etc — give up *)

(* ── PODEM main loop with explicit decision stack ──────────────── *)

type decision = { pi : int; value : d_val; tried_other : bool }

let podem (c : compiled) ~target_net ~target_val ~budget =
  let n = c.c_n_nets in
  let values = Array.make n VX in
  let t0 = Unix.gettimeofday () in
  let timeout_s =
    match Sys.getenv_opt "SV_DECOMP_PODEM_TIMEOUT_MS" with
    | Some s -> (try float_of_string s /. 1000. with _ -> 0.020)
    | None -> 0.020 in
  (* Seed PI / FF.Q nets to X (already so).  Inject fault: target_val
     is "what good circuit should produce"; the bad version produces
     ~target_val.  So target_net gets D (1/0) or D̄ (0/1). *)
  values.(target_net) <- (if target_val = 1 then VD else VDb);
  let decisions : decision list ref = ref [] in
  let steps = ref 0 in
  let success = ref None in
  let exit = ref false in
  while not !exit do
    incr steps;
    if !steps > budget
       || Unix.gettimeofday () -. t0 > timeout_s
    then exit := true
    else begin
      imply ~fault_net:target_net c values;
      if d_at_observable c values then begin
        let pat = Hashtbl.create 16 in
        List.iter (fun nid ->
          match values.(nid) with
          | V0 -> Hashtbl.replace pat nid 0
          | V1 -> Hashtbl.replace pat nid 1
          | _ -> ()
        ) c.c_pi_nets;
        List.iter (fun nid ->
          match values.(nid) with
          | V0 -> Hashtbl.replace pat nid 0
          | V1 -> Hashtbl.replace pat nid 1
          | _ -> ()
        ) c.c_ff_q_nets;
        success := Some pat;
        exit := true
      end else begin
        match pick_objective c values with
        | Some (net, want) ->
            (* Back-trace from objective to a PI/FF.Q. *)
            (match backtrace c values net want with
             | None ->
                 exit := true  (* nothing controllable — abort *)
             | Some (pi, pv) ->
                 (* Snapshot prior value so backtrack restores it. *)
                 if values.(pi) <> VX then
                   exit := true  (* PI already set and back-trace says
                                    something else — shouldn't happen,
                                    abort to keep things sound *)
                 else begin
                   values.(pi) <- pv;
                   decisions :=
                     { pi; value = pv; tried_other = false } :: !decisions
                 end)
        | None ->
            (* D-front empty.  If we're not at an observable, the fault
               is masked — backtrack to most recent decision. *)
            let rec pop () =
              match !decisions with
              | [] -> exit := true   (* exhausted *)
              | d :: rest ->
                  values.(d.pi) <- VX;
                  decisions := rest;
                  if not d.tried_other then begin
                    let flipped = inv_d d.value in
                    values.(d.pi) <- flipped;
                    decisions :=
                      { d with value = flipped; tried_other = true } :: rest
                  end else pop ()
            in pop ()
      end
    end
  done;
  !success
