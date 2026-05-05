(* BIR ↔ DEF instance-name binding.

   Synthesis tools (Yosys, Vivado, DC) emit gate-level instance
   names that retain a hierarchical prefix corresponding to the
   pre-synth source.  An RTL instance "u_add" of an adder ends up
   placed as DEF instances named "u_add.x", "u_add._123_",
   "\u_add._GEN_4_", etc.  This module recovers the BIR ↔ DEF
   mapping by prefix matching, then exposes per-BIR-instance
   physical timing so the cert-gated arch swap in
   behavioral_arch_subst can decide based on real placed delay
   instead of analytical depth alone.

   The matching is heuristic — there is no universal naming
   convention — but the rules below cover what Yosys emits with
   `flatten` plus what OpenROAD writes through detail-place. *)

type binding = {
  bir_path : string;
  members  : Placement.placement list;
}

(* Strip a leading backslash from Verilog-escaped names. *)
let unescape s =
  if String.length s > 0 && s.[0] = '\\'
  then String.sub s 1 (String.length s - 1)
  else s

(* Predicate: does [def_inst_name] descend from BIR [bir_path]?
   The DEF name must start with the BIR path, and the next
   character (if any) must be a hierarchy/index separator we
   recognise:
       . _ / [ ] 0..9
   The digit case covers user-emitted bus/loop names like
   "output1", "rebuffer3"; the punctuation cases cover Yosys
   hierarchy ("u_add._123_") and Verilog escaped names
   ("\u_add"). Letters following the prefix reject collisions
   like "rebuffer" → "rebuffered". *)
let belongs_to bir_path def_name =
  let bp = unescape bir_path in
  let ds = unescape def_name in
  let n = String.length bp and m = String.length ds in
  m >= n && String.sub ds 0 n = bp
  && (m = n
      || (let c = ds.[n] in
          c = '.' || c = '_' || c = '/' || c = '[' || c = ']'
          || (c >= '0' && c <= '9')))

let bind_by_prefix paths placements =
  List.map (fun bir_path ->
    let members = List.filter
      (fun (p : Placement.placement) ->
         belongs_to bir_path p.Placement.inst)
      placements in
    { bir_path; members }) paths

(* DEF instances not claimed by any binding.  Useful for
   diagnostics — usually most "_NNN_" anonymous gates fall
   through, indicating they were created by Yosys after
   flattening lost the source-level association. *)
let unbound bindings placements =
  let claimed = Hashtbl.create (List.length placements) in
  List.iter (fun b ->
    List.iter (fun (p : Placement.placement) ->
      Hashtbl.replace claimed p.Placement.inst ()) b.members) bindings;
  List.filter (fun (p : Placement.placement) ->
    not (Hashtbl.mem claimed p.Placement.inst)) placements

(* Worst gate-level arrival within a binding's members,
   evaluated against the [arrival_tbl] returned by
   [Placement_timing.arrival_table]. *)
let subgraph_worst arrival_tbl b =
  List.fold_left (fun acc (p : Placement.placement) ->
    match Hashtbl.find_opt arrival_tbl p.Placement.inst with
    | Some v -> max acc v
    | None -> acc) 0. b.members

(* Bounding box of a binding's placements — useful as an input
   to floor-planning decisions, and as a sanity check that the
   binding really did cluster spatially. *)
let bbox b =
  match b.members with
  | [] -> None
  | first :: rest ->
      let xmn = List.fold_left
        (fun a (p:Placement.placement) -> min a p.Placement.x)
        first.Placement.x rest in
      let xmx = List.fold_left
        (fun a (p:Placement.placement) -> max a p.Placement.x)
        first.Placement.x rest in
      let ymn = List.fold_left
        (fun a (p:Placement.placement) -> min a p.Placement.y)
        first.Placement.y rest in
      let ymx = List.fold_left
        (fun a (p:Placement.placement) -> max a p.Placement.y)
        first.Placement.y rest in
      Some ((xmn, ymn), (xmx, ymx))

(* ── Bbox + connectivity refinement ───────────────────────────── *)

(* Iteratively claim unbound cells via a flood-fill over net
   connectivity — an unbound cell joins binding B if a plurality
   of its connected neighbours already belong to B.  Optional
   [bbox_pad] confines the spread to within [pad] dbu of B's
   current bbox; set [bbox_pad=None] to disable spatial gating
   (pure connectivity flood).

   Connectivity comes from [edges] (the [Placement_timing.fanout_edges]
   table) — we walk both directions: a cell's neighbours are the
   union of its fanin and fanout sets.

   Iterates until no further claims happen, capped at [max_iter]
   sweeps to bound runtime on pathological designs. *)

let bbox_of_pls pls =
  match pls with
  | [] -> None
  | first :: rest ->
      let xmn = List.fold_left
        (fun a (p:Placement.placement) -> min a p.x) first.Placement.x rest in
      let xmx = List.fold_left
        (fun a (p:Placement.placement) -> max a p.x) first.Placement.x rest in
      let ymn = List.fold_left
        (fun a (p:Placement.placement) -> min a p.y) first.Placement.y rest in
      let ymx = List.fold_left
        (fun a (p:Placement.placement) -> max a p.y) first.Placement.y rest in
      Some ((xmn, ymn), (xmx, ymx))

let inside_padded_bbox bbox pad p =
  match bbox with
  | None -> false
  | Some ((xa,ya),(xb,yb)) ->
      p.Placement.x >= xa - pad && p.x <= xb + pad
      && p.y >= ya - pad && p.y <= yb + pad

let bind_with_connectivity
    ?(bbox_pad=Some 5000)
    ?(max_iter=8)
    (bindings : binding list) ~edges ~placements =
  (* inst -> currently assigned binding name *)
  let assigned = Hashtbl.create (List.length placements) in
  List.iter (fun b ->
    List.iter (fun (p : Placement.placement) ->
      Hashtbl.replace assigned p.Placement.inst b.bir_path) b.members)
    bindings;

  (* placement-by-name lookup, for spatial gating *)
  let placement_of = Hashtbl.create (List.length placements) in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace placement_of p.Placement.inst p) placements;

  (* fanin from fanout *)
  let fanin = Fanout_cone.invert_edges edges in

  let neighbours inst =
    let outs = try Hashtbl.find edges inst with Not_found -> [] in
    let ins  = try Hashtbl.find fanin inst with Not_found -> [] in
    List.map fst outs @ List.map fst ins
  in

  (* per-iteration cached bbox map (recomputed when claims grow) *)
  let bbox_of_name name =
    let pls = Hashtbl.fold
      (fun inst nm acc ->
         if nm = name then
           match Hashtbl.find_opt placement_of inst with
           | Some p -> p :: acc
           | None -> acc
         else acc)
      assigned [] in
    bbox_of_pls pls in

  let changed = ref true in
  let iters = ref 0 in
  while !changed && !iters < max_iter do
    changed := false;
    incr iters;

    (* refresh bbox cache *)
    let bbox_cache = Hashtbl.create 16 in
    Hashtbl.iter (fun _ name ->
      if not (Hashtbl.mem bbox_cache name) then
        Hashtbl.replace bbox_cache name (bbox_of_name name)) assigned;

    List.iter (fun (p : Placement.placement) ->
      if not (Hashtbl.mem assigned p.inst) then begin
        let counts = Hashtbl.create 8 in
        List.iter (fun nb ->
          match Hashtbl.find_opt assigned nb with
          | Some name -> begin
              let pad_ok =
                match bbox_pad with
                | None -> true
                | Some pad ->
                    let bb = try Hashtbl.find bbox_cache name with _ -> None in
                    inside_padded_bbox bb pad p in
              if pad_ok then
                let c = try Hashtbl.find counts name with Not_found -> 0 in
                Hashtbl.replace counts name (c + 1)
            end
          | None -> ()) (neighbours p.inst);
        if Hashtbl.length counts > 0 then begin
          let best = ref None in
          Hashtbl.iter (fun name c ->
            match !best with
            | None -> best := Some (name, c)
            | Some (_, b) when c > b -> best := Some (name, c)
            | _ -> ()) counts;
          (match !best with
           | Some (name, _) ->
               Hashtbl.replace assigned p.inst name;
               changed := true
           | None -> ())
        end
      end) placements
  done;

  (* Reconstruct the bindings list from the final assignment. *)
  let by_name = Hashtbl.create (List.length bindings) in
  List.iter (fun b ->
    Hashtbl.replace by_name b.bir_path []) bindings;
  Hashtbl.iter (fun inst name ->
    match Hashtbl.find_opt placement_of inst with
    | Some p ->
        let cur = try Hashtbl.find by_name name with Not_found -> [] in
        Hashtbl.replace by_name name (p :: cur)
    | None -> ()) assigned;
  List.map (fun b ->
    { bir_path = b.bir_path;
      members  = try Hashtbl.find by_name b.bir_path with Not_found -> [] })
    bindings,
  !iters
