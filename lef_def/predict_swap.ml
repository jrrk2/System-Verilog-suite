(* Predict-only resynthesis: "what would the worst arrival become
   if we swapped binding B from arch A_current to arch A_new?"

   The pipeline:
     1.  Place + Liberty + LEF -> per-instance arrival.
     2.  Worst endpoint + critical-path fanout cone (budget 0).
     3.  Bin the cone's cells by BIR-source binding.
     4.  For each (binding × candidate arch) in the user's
         swap-candidate table, project the new arrival as
            new_arrival = old_arrival
                        - cone_delay_through_binding * (1 - depth_factor)
         where [depth_factor] = D(arch_new) / D(arch_current),
         taken from textbook analytical depth.

   This is a first-order projection — it ignores re-routing
   effects, slew shifts, and the area cost of the new arch — but
   it's good enough to ORDER candidates: any swap with positive
   savings is a real candidate for ECO; the absolute number tells
   you how much budget that swap recovers.

   Once the user picks a candidate, the cert-gated swap pass (#95)
   verifies the chosen arch's certificate exists at the relevant
   width and the ECO emitter (#94) writes the actual change-list. *)

type candidate = {
  binding_name : string;
  current_arch : string;
  to_arch      : string;
  depth_factor : float;
    (* D(to_arch) / D(current_arch); e.g. ripple W=8 -> kogge_stone
       W=8 has depth_factor = 3 / 8 = 0.375 *)
}

type prediction = {
  cand               : candidate;
  cells_in_cone      : int;
  cone_delay_ps      : float;
  predicted_savings  : float;
  predicted_new_arr  : float;
}

(* For each cell in [cone], compute its "own" delay along the
   critical chain — i.e. its arrival minus the maximum
   (fanin_arrival + wire_delay).  Sum that contribution over
   the cells of each binding to get the binding's footprint on
   the critical path. *)
let cone_delay_per_binding ~bindings ~arrival_tbl ~fanin ~cone =
  let in_cone = Hashtbl.create (List.length cone) in
  List.iter (fun n -> Hashtbl.replace in_cone n ()) cone;
  let inst_delay inst =
    if not (Hashtbl.mem in_cone inst) then 0.0
    else
      let arr =
        try Hashtbl.find arrival_tbl inst with Not_found -> 0.0 in
      let fanins =
        try Hashtbl.find fanin inst with Not_found -> [] in
      let max_in = List.fold_left
        (fun acc (src, w) ->
           let a = try Hashtbl.find arrival_tbl src with Not_found -> 0.0 in
           max acc (a +. w))
        neg_infinity fanins in
      if max_in = neg_infinity then arr  (* primary input *)
      else max 0.0 (arr -. max_in)
  in
  List.map (fun (b : Bir_def_bind.binding) ->
    let n_in_cone, total_delay =
      List.fold_left (fun (n, d) (p : Placement.placement) ->
        if Hashtbl.mem in_cone p.Placement.inst
        then (n + 1, d +. inst_delay p.Placement.inst)
        else (n, d)) (0, 0.0) b.members in
    (b.bir_path, n_in_cone, total_delay)) bindings

(* Top-level: given arrivals + fanin + bindings + candidates,
   produce a sorted list of predictions. *)
let predict
    ~arrival_tbl ~fanin ~bindings ~candidates () =
  let ep, ep_arr =
    match Fanout_cone.worst_endpoint arrival_tbl with
    | Some r -> r
    | None -> ("(none)", 0.0) in
  let cone = Fanout_cone.cone_of_endpoint
               ~slack_budget:0.0
               ~arrival_tbl ~fanin ~endpoint:ep () in
  let per_binding =
    cone_delay_per_binding ~bindings ~arrival_tbl ~fanin ~cone in

  let preds = List.filter_map (fun c ->
    match List.find_opt (fun (n, _, _) -> n = c.binding_name) per_binding with
    | None -> None
    | Some (_, n_cone, dly) ->
        if n_cone = 0 then None
        else
          let savings = dly *. (1.0 -. c.depth_factor) in
          Some {
            cand = c;
            cells_in_cone     = n_cone;
            cone_delay_ps     = dly;
            predicted_savings = savings;
            predicted_new_arr = ep_arr -. savings;
          }) candidates in

  let sorted = List.sort
    (fun a b -> compare b.predicted_savings a.predicted_savings) preds in
  ((ep, ep_arr), cone, sorted)
