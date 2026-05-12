(* Directed (systematic) ATPG — backwards justification.

   For each fault that random-pattern simulation didn't reach, walk
   back from the fault site through the cell graph and pick a PI / FF.Q
   assignment that *activates* the fault (drives the fault net to the
   opposite of its stuck-at value).  Propagation to a PO is then left
   to whatever pattern the unconstrained inputs happen to take —
   random for inputs not on the justification path.

   This is a PODEM-light: no D-frontier maintenance, no propagation
   objective, no backtrack-when-stuck.  Instead we lean on parallel
   random patterns for everything outside the activation cone.  In
   practice this lifts coverage from "random's structural floor" to
   "every fault whose activation pattern fits the cell-handler table",
   with cost ~one extra simulation per undetected fault.

   What's handled
   ==============
   * AND / OR / NAND / NOR / XOR / XNOR (2-, 3-, 4-input)
   * INV / BUF / LOGIC0 / LOGIC1
   * MUX2 (with both S-controlled paths)
   * AOI21 / OAI21 (analytical truth table)

   For target=1 on an AND, every input must be 1.  For target=0 on an
   AND, at least one input must be 0 — we pick the first.  Conflicts
   (the same PI required at both polarities by different sub-targets)
   abort the justification for that fault.                              *)

open Gate_sim

(* Per-fault recursion budget — picorv32's deepest activation cone is
   ~30 levels.  64 is generous; bigger doesn't usually help because
   conflicts surface long before. *)
let default_budget = 64

(* The justification context is a per-net "must be" table.  None = no
   constraint yet, Some v = constrained to v in {0, 1}.  Conflicts
   manifest when a recursion needs Some 1 on a net currently Some 0. *)
type ctx = {
  must     : (net_id, int) Hashtbl.t;
  visiting : (net_id, unit) Hashtbl.t;  (* cycle guard *)
}

let make_ctx () = {
  must = Hashtbl.create 64;
  visiting = Hashtbl.create 16;
}

let constrain ctx net v =
  match Hashtbl.find_opt ctx.must net with
  | None     -> Hashtbl.add ctx.must net v; true
  | Some v'  -> v = v'

let is_input_net (c : compiled) net =
  List.mem net c.c_pi_nets || List.mem net c.c_ff_q_nets

let cell_family cell =
  let s = String.uppercase_ascii cell in
  let strip_x s =
    try
      let i = String.rindex s '_' in
      if i + 1 < String.length s && s.[i+1] = 'X' then String.sub s 0 i
      else s
    with Not_found -> s
  in
  strip_x s

(* Single cell handler: given target output value, recurse into inputs.
   Returns [true] when a satisfying assignment exists. *)
let rec justify_cell (c : compiled) ctx ~budget cell ins target =
  if budget <= 0 then false
  else
    match cell_family cell, ins with
    | "AND2", [a; b] | "AND2", [b; a] when target = 1 ->
        justify c ctx ~budget:(budget - 1) a 1
        && justify c ctx ~budget:(budget - 1) b 1
    | "AND2", _ when target = 0 ->
        List.exists (fun n -> justify c ctx ~budget:(budget - 1) n 0) ins
    | "AND3", _ when target = 1 ->
        List.for_all (fun n -> justify c ctx ~budget:(budget - 1) n 1) ins
    | "AND3", _ when target = 0 ->
        List.exists (fun n -> justify c ctx ~budget:(budget - 1) n 0) ins
    | "AND4", _ when target = 1 ->
        List.for_all (fun n -> justify c ctx ~budget:(budget - 1) n 1) ins
    | "AND4", _ when target = 0 ->
        List.exists (fun n -> justify c ctx ~budget:(budget - 1) n 0) ins
    | "OR2", _ when target = 0 ->
        List.for_all (fun n -> justify c ctx ~budget:(budget - 1) n 0) ins
    | "OR2", _ when target = 1 ->
        List.exists (fun n -> justify c ctx ~budget:(budget - 1) n 1) ins
    | "OR3", _ when target = 0 ->
        List.for_all (fun n -> justify c ctx ~budget:(budget - 1) n 0) ins
    | "OR3", _ when target = 1 ->
        List.exists (fun n -> justify c ctx ~budget:(budget - 1) n 1) ins
    | "OR4", _ when target = 0 ->
        List.for_all (fun n -> justify c ctx ~budget:(budget - 1) n 0) ins
    | "OR4", _ when target = 1 ->
        List.exists (fun n -> justify c ctx ~budget:(budget - 1) n 1) ins
    | "NAND2", _ -> justify_cell c ctx ~budget "AND2" ins (1 - target)
    | "NAND3", _ -> justify_cell c ctx ~budget "AND3" ins (1 - target)
    | "NAND4", _ -> justify_cell c ctx ~budget "AND4" ins (1 - target)
    | "NOR2", _  -> justify_cell c ctx ~budget "OR2"  ins (1 - target)
    | "NOR3", _  -> justify_cell c ctx ~budget "OR3"  ins (1 - target)
    | "NOR4", _  -> justify_cell c ctx ~budget "OR4"  ins (1 - target)
    | "XOR2", [a; b] ->
        (* a XOR b = target — try (a=0, b=target) first, then (a=1, b=1-target) *)
        (justify c ctx ~budget:(budget - 1) a 0
         && justify c ctx ~budget:(budget - 1) b target)
        || (justify c ctx ~budget:(budget - 1) a 1
            && justify c ctx ~budget:(budget - 1) b (1 - target))
    | "XNOR2", [a; b] ->
        justify_cell c ctx ~budget "XOR2" [a;b] (1 - target)
    | "INV", [a] -> justify c ctx ~budget:(budget - 1) a (1 - target)
    | "BUF", [a] -> justify c ctx ~budget:(budget - 1) a target
    | "LOGIC0", []  -> target = 0
    | "LOGIC1", []  -> target = 1
    | "MUX2",  [a; b; s] ->
        (* Z = s ? b : a — try both arms *)
        (justify c ctx ~budget:(budget - 1) s 0
         && justify c ctx ~budget:(budget - 1) a target)
        || (justify c ctx ~budget:(budget - 1) s 1
            && justify c ctx ~budget:(budget - 1) b target)
    | "AOI21", [a; b; cc] ->
        (* Z = !((a AND b) OR c) — target=1 ⇒ a AND b = 0 AND c = 0 *)
        if target = 1 then
          justify c ctx ~budget:(budget - 1) cc 0
          && (justify c ctx ~budget:(budget - 1) a 0
              || justify c ctx ~budget:(budget - 1) b 0)
        else
          (* target = 0 ⇒ (a AND b) OR c = 1 *)
          justify c ctx ~budget:(budget - 1) cc 1
          || (justify c ctx ~budget:(budget - 1) a 1
              && justify c ctx ~budget:(budget - 1) b 1)
    | "OAI21", [a; b; cc] ->
        if target = 1 then
          justify c ctx ~budget:(budget - 1) cc 0
          || (justify c ctx ~budget:(budget - 1) a 0
              && justify c ctx ~budget:(budget - 1) b 0)
        else
          justify c ctx ~budget:(budget - 1) cc 1
          && (justify c ctx ~budget:(budget - 1) a 1
              || justify c ctx ~budget:(budget - 1) b 1)
    | _ -> false  (* unhandled cell — give up on this fault *)

and justify (c : compiled) ctx ~budget net target =
  if Hashtbl.mem ctx.visiting net then
    (* Cycle in the supposedly combinational graph — usually means an
       upstream tool emitted a wire that loops; refuse to recurse. *)
    false
  else if is_input_net c net then
    constrain ctx net target
  else
    match Hashtbl.find_opt c.c_driver_of net with
    | None ->
        (* No cell drives this net — it's likely a dead/unassigned
           bus reconstruction.  Treat as input-equivalent. *)
        constrain ctx net target
    | Some idx ->
        let cell, ins, _ = c.c_evals.(idx) in
        (match Hashtbl.find_opt ctx.must net with
         | Some v when v <> target -> false
         | Some _ -> true            (* already constrained — done *)
         | None ->
             Hashtbl.add ctx.must net target;
             Hashtbl.add ctx.visiting net ();
             let ok = justify_cell c ctx ~budget cell ins target in
             Hashtbl.remove ctx.visiting net;
             ok)

(* ── Path sensitization ──────────────────────────────────────────
   Activating a fault is necessary but not sufficient: the fault effect
   must propagate to an observable.  For each cell on the propagation
   path, the *side inputs* (those not carrying the fault) must be at
   the cell's non-controlling value (AND/NAND: 1, OR/NOR: 0, XOR/XNOR:
   any fixed value).  We walk forward from the fault site one cell at
   a time, picking the cell that has the fewest side inputs first,
   until we reach a PO or FF.D net.                                   *)

(* Build a reverse-index: for each net id, every cell that reads it. *)
let consumers_of (c : compiled) : (net_id, int list) Hashtbl.t =
  let h = Hashtbl.create 1024 in
  Array.iteri (fun idx (_, ins, _) ->
    List.iter (fun in_net ->
      let cur = try Hashtbl.find h in_net with Not_found -> [] in
      Hashtbl.replace h in_net (idx :: cur)
    ) ins
  ) c.c_evals;
  h

let is_observable (c : compiled) net =
  List.mem net c.c_po_nets || List.mem net c.c_ff_d_nets

(* Constrain side inputs of [cell_idx] to non-controlling values for
   propagation.  Returns true if all constraints were satisfiable. *)
let sensitize_through (c : compiled) consumers ctx ~budget cell_idx through_net =
  let cell, ins, _ = c.c_evals.(cell_idx) in
  let side = List.filter (fun n -> n <> through_net) ins in
  let nc_val = match cell_family cell with
    | "AND2" | "AND3" | "AND4" | "NAND2" | "NAND3" | "NAND4" -> Some 1
    | "OR2"  | "OR3"  | "OR4"  | "NOR2"  | "NOR3"  | "NOR4"  -> Some 0
    | "INV" | "BUF" -> None  (* no side inputs *)
    | "XOR2" | "XNOR2" -> Some 0  (* fix at 0 — propagates either way *)
    | _ -> None in
  match nc_val with
  | None -> side = []
  | Some v ->
      List.for_all (fun n -> justify c ctx ~budget n v) side
  [@warning "-26"]
  |> fun ok -> ignore consumers; ok

(* Pick the consumer cell to propagate through.  Greedy: pick the one
   whose output is closest to a PO via the consumer graph.  Stage 2
   uses a simple BFS-precomputed distance table. *)
let propagation_distances (c : compiled) consumers : (net_id, int) Hashtbl.t =
  let dist = Hashtbl.create 1024 in
  let q = Queue.create () in
  List.iter (fun n -> Hashtbl.replace dist n 0; Queue.push n q) c.c_po_nets;
  List.iter (fun n ->
    if not (Hashtbl.mem dist n) then begin
      Hashtbl.replace dist n 0; Queue.push n q
    end) c.c_ff_d_nets;
  while not (Queue.is_empty q) do
    let net = Queue.pop q in
    let d = Hashtbl.find dist net in
    (* Walk back to driver cell, then to its input nets. *)
    match Hashtbl.find_opt c.c_driver_of net with
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
  ignore consumers; dist

(* Forward-walk from [fault_net] toward the closest PO/FF.D, picking
   the consumer cell at each step that has the smallest propagation
   distance.  Sensitize side inputs as we go.  Returns true iff the
   walk reached an observable and all sensitization succeeded.       *)
let sensitize_to_observable (c : compiled) consumers dist_to_obs ctx ~budget
    ~start_net =
  let rec walk net hops =
    if hops <= 0 then false
    else if is_observable c net then true
    else
      match Hashtbl.find_opt consumers net with
      | None | Some [] -> false
      | Some cands ->
          (* Pick the consumer cell whose OUTPUT has the smallest
             distance to an observable. *)
          let scored = List.filter_map (fun idx ->
            let _, _, out = c.c_evals.(idx) in
            match Hashtbl.find_opt dist_to_obs out with
            | Some d -> Some (d, idx, out)
            | None -> None
          ) cands in
          let scored = List.sort (fun (a, _, _) (b, _, _) -> compare a b) scored in
          (* Try each in order until one's sensitization succeeds. *)
          let try_one (_, idx, out) =
            (* Snapshot ctx; on failure restore. *)
            let saved_must =
              Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx.must [] in
            if sensitize_through c consumers ctx ~budget idx net
               && walk out (hops - 1)
            then true
            else begin
              Hashtbl.reset ctx.must;
              List.iter (fun (k, v) -> Hashtbl.add ctx.must k v) saved_must;
              false
            end
          in
          List.exists try_one scored
  in
  walk start_net 256

(* Public entry — return the PI/FF.Q assignment map for activating
   [fault], best-effort attempting to sensitize a path too.  If
   sensitization conflicts with activation, restore to activation-only
   constraints and let random fill propagation.                       *)
let justify_fault ?(budget = default_budget) (c : compiled)
    ~consumers ~dist_to_obs
    ~(target_net : net_id) ~(target_val : int) =
  let ctx = make_ctx () in
  if justify c ctx ~budget target_net target_val then begin
    (* Snapshot the activation-only ctx so we can roll back if
       sensitization adds conflicting constraints. *)
    let snap =
      Hashtbl.fold (fun k v acc -> (k, v) :: acc) ctx.must [] in
    let _ =
      sensitize_to_observable c consumers dist_to_obs ctx ~budget
        ~start_net:target_net in
    (* Even on partial-path success the ctx now has the maximum-
       compatible side-input constraints — but if sensitize threw
       a conflict mid-walk the visited part is already set.  Don't
       roll back — keep whatever stuck.  We only need the
       activation to be GUARANTEED. *)
    let _ = snap in
    let pi_pat = Hashtbl.create 32 in
    Hashtbl.iter (fun net v ->
      if is_input_net c net then Hashtbl.replace pi_pat net v
    ) ctx.must;
    Some pi_pat
  end else None
