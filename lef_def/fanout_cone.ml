(* Critical-path fanout-cone extractor.

   Given a placed netlist and its per-instance arrival table,
   return the cone of cells feeding the worst endpoint within a
   slack budget.  The cone is the set of cells that any
   arch-swap pass may touch when trying to speed up the design;
   cells outside the cone are off the critical path and a swap
   to them gains nothing.

   The slack-budget knob trades cone size against opportunity:
   tight (e.g. 5% of A_end) returns just the longest path,
   loose (e.g. 20%) returns every path within 20% of worst —
   the more aggressive resynth target. *)

(* Compute fanin from fanout edges.  [fanout_edges] is the
   [driver -> [(load, wire_delay); ...]] table built by
   Placement_timing.fanout_edges. *)
let invert_edges fanout_edges =
  let fanin = Hashtbl.create (Hashtbl.length fanout_edges) in
  Hashtbl.iter (fun src outs ->
    List.iter (fun (dst, w) ->
      let cur = try Hashtbl.find fanin dst with Not_found -> [] in
      Hashtbl.replace fanin dst ((src, w) :: cur)) outs) fanout_edges;
  fanin

(* Locate the worst-arrival instance in the table.  Returns
   [None] when the table is empty. *)
let worst_endpoint arrival_tbl =
  let best = ref None in
  Hashtbl.iter (fun inst v ->
    match !best with
    | None -> best := Some (inst, v)
    | Some (_, b) when v > b -> best := Some (inst, v)
    | _ -> ()) arrival_tbl;
  !best

(* Top-[k] endpoints by arrival, descending. *)
let top_k_endpoints ~k arrival_tbl =
  let pairs = Hashtbl.fold (fun i v acc -> (i, v) :: acc) arrival_tbl [] in
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) pairs in
  let rec take n = function
    | [] -> []
    | _ when n = 0 -> []
    | x :: tl -> x :: take (n-1) tl
  in
  take k sorted

(* Backward BFS from [endpoint] collecting cells on paths whose
   delay is within [slack_budget] picoseconds of the longest
   path through each step.

   At each cell X already in the cone, the longest-path fanin
   is the one whose [F_arr + wire(F->X)] is maximum.  A fanin
   is included iff its [F_arr + wire] is within [slack_budget]
   of that maximum.  Setting [slack_budget = 0] gives exactly
   the longest path; loosening pulls in side-paths.

   This is correct regardless of where the endpoint's slack is
   — we walk relative to each step, not absolute to the
   endpoint. *)
let cone_of_endpoint
    ?(slack_budget=0.0)
    ~arrival_tbl ~fanin ~endpoint () =
  let cone = Hashtbl.create 256 in
  let rec walk inst =
    if Hashtbl.mem cone inst then ()
    else begin
      Hashtbl.add cone inst ();
      let fanins = try Hashtbl.find fanin inst with Not_found -> [] in
      let arr_with_wire (src, w) =
        let a = try Hashtbl.find arrival_tbl src with Not_found -> 0.0 in
        a +. w in
      let max_in =
        List.fold_left
          (fun acc fe -> max acc (arr_with_wire fe))
          neg_infinity fanins in
      List.iter (fun fe ->
        let v = arr_with_wire fe in
        if v >= max_in -. slack_budget -. 1e-9
        then walk (fst fe)) fanins
    end
  in
  let _ = arrival_tbl in
  walk endpoint;
  Hashtbl.fold (fun k () acc -> k :: acc) cone []

(* Bounding box of the cone's placements — useful for sizing a
   resynth ECO region. *)
let cone_bbox placements cone =
  let in_cone = Hashtbl.create (List.length cone) in
  List.iter (fun n -> Hashtbl.replace in_cone n ()) cone;
  let xmn = ref max_int and xmx = ref min_int in
  let ymn = ref max_int and ymx = ref min_int in
  let n = ref 0 in
  List.iter (fun (p : Placement.placement) ->
    if Hashtbl.mem in_cone p.Placement.inst then begin
      incr n;
      if p.x < !xmn then xmn := p.x;
      if p.x > !xmx then xmx := p.x;
      if p.y < !ymn then ymn := p.y;
      if p.y > !ymx then ymx := p.y;
    end) placements;
  if !n = 0 then None
  else Some ((!xmn, !ymn), (!xmx, !ymx))

(* Cell-type histogram of the cone — what kinds of gates
   dominate the critical path?  This is the "what to optimise"
   answer at first granularity. *)
let cone_cell_histogram placements cone =
  let in_cone = Hashtbl.create (List.length cone) in
  List.iter (fun n -> Hashtbl.replace in_cone n ()) cone;
  let h = Hashtbl.create 32 in
  List.iter (fun (p : Placement.placement) ->
    if Hashtbl.mem in_cone p.Placement.inst then
      let cur = try Hashtbl.find h p.cell with Not_found -> 0 in
      Hashtbl.replace h p.cell (cur + 1)) placements;
  Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []
  |> List.sort (fun (_, a) (_, b) -> compare b a)
