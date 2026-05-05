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
