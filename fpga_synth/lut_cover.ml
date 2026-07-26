(* LUT covering: the new kernel the old read_library mapper didn't need.
 *
 * A std-cell mapper recognises a cone against a *fixed* set of gate
 * functions.  A LUT computes an ARBITRARY k-input function, so instead
 * we (1) enumerate k-feasible cuts per node of the subject graph,
 * (2) evaluate each chosen cut's cone over all 2^k input assignments to
 * get its truth table, which is exactly the LUTk INIT, and (3) select a
 * cut per root (area- or depth-oriented) to cover the graph.
 *
 * This is the FlowMap / ABC-`if` family of mappers specialised to k<=6
 * (so a truth table fits in one Int64). *)

open! Base
open Hardcaml

(* Subject graph: an And-Inverter-ish DAG.  Inputs and constants are
 * leaves; internal nodes are 2-input gates with optional input
 * inversions (AIG form keeps cut enumeration + truth-table eval
 * uniform).  Real lowering from Behavioral_ir/hardcaml populates this.
 *
 * Invariant: [nodes] is in topological order, nodes.(i).id = i, and an
 * And2's children a,b satisfy a < i and b < i. *)
type gate =
  | Input of string
  | Const of bool
  | And2 of { a : int; b : int; a_inv : bool; b_inv : bool }

type node = { id : int; gate : gate }

type graph = { nodes : node array; outputs : (string * int * bool) list }
(* outputs: (port_name, node id, inverted) *)

(* A k-feasible cut: a cone rooted at [root] whose [leaves] (<= k node
 * ids, kept sorted ascending) are the LUT inputs. *)
type cut = { root : int; leaves : int list }

(* ---- cut enumeration --------------------------------------------- *)

(* Merge two ascending, duplicate-free int lists. *)
let rec union_sorted xs ys =
  match xs, ys with
  | [], l | l, [] -> l
  | x :: xs', y :: ys' ->
    if x = y then x :: union_sorted xs' ys'
    else if x < y then x :: union_sorted xs' ys
    else y :: union_sorted xs ys'

let is_subset small big =
  List.for_all small ~f:(fun x -> List.mem big x ~equal:Int.equal)

(* Drop functionally-redundant (duplicate leaf-set) and dominated cuts,
 * then keep only the [cap] smallest by leaf count (priority cuts). *)
let prune ~cap cuts =
  let deduped =
    List.fold cuts ~init:[] ~f:(fun acc c ->
      if List.exists acc ~f:(fun d -> List.equal Int.equal d.leaves c.leaves)
      then acc
      else c :: acc)
    |> List.rev
  in
  let kept =
    List.filter deduped ~f:(fun c ->
      (* c is dominated if some OTHER cut's leaves are a proper subset. *)
      not
        (List.exists deduped ~f:(fun d ->
           (not (List.equal Int.equal c.leaves d.leaves))
           && is_subset d.leaves c.leaves)))
  in
  List.sort kept ~compare:(fun a b ->
    Int.compare (List.length a.leaves) (List.length b.leaves))
  |> fun sorted -> List.take sorted cap

(* All k-feasible cuts per node (priority-cut style). *)
let enumerate_cuts ~(k : int) (g : graph) : cut list array =
  let n = Array.length g.nodes in
  let cuts = Array.create ~len:n [] in
  for i = 0 to n - 1 do
    let trivial = { root = i; leaves = [ i ] } in
    (match g.nodes.(i).gate with
     | Input _ | Const _ -> cuts.(i) <- [ trivial ]
     | And2 { a; b; _ } ->
       let merged =
         List.concat_map cuts.(a) ~f:(fun ca ->
           List.filter_map cuts.(b) ~f:(fun cb ->
             let leaves = union_sorted ca.leaves cb.leaves in
             if List.length leaves <= k then Some { root = i; leaves } else None))
       in
       (* trivial cut kept so parents can use [i] as a leaf. *)
       cuts.(i) <- prune ~cap:8 (trivial :: merged))
  done;
  cuts

(* ---- truth tables (= LUT INIT) ----------------------------------- *)

(* Truth tables for cones with up to 8 variables (256 bits).  Stored as
 * a `bytes` so AND/OR/NOT are byte-parallel and lookups are O(1).
 * k≤6 keeps the historical 64-bit shape (8 bytes); k=7 → 16 bytes,
 * k=8 → 32 bytes.  Bit i of the truth table is bit (i mod 8) of byte
 * (i / 8) — and represents the cone output when leaf j carries bit j
 * of i.                                                                *)
module Tt = struct
  type t = bytes
  let nbytes ~k = max 1 ((1 lsl k) / 8)
  let zero ~k = Bytes.make (nbytes ~k) '\x00'
  let ones ~k =
    let n = 1 lsl k in
    let b = Bytes.make (nbytes ~k) '\xff' in
    if n < 8 then Bytes.set b 0 (Char.of_int_exn ((1 lsl n) - 1));
    b
  let bit t i =
    (Char.to_int (Bytes.get t (i / 8)) lsr (i land 7)) land 1 = 1
  let map2 f a b =
    let n = Bytes.length a in
    let r = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set r i (Char.of_int_exn (f (Char.to_int (Bytes.get a i))
                                        (Char.to_int (Bytes.get b i))))
    done;
    r
  let bit_or  = map2 ( lor )
  let bit_and = map2 ( land )
  let bit_not a =
    let n = Bytes.length a in
    let r = Bytes.create n in
    for i = 0 to n - 1 do
      Bytes.set r i (Char.of_int_exn ((Char.to_int (Bytes.get a i)) lxor 0xff))
    done; r
  let set_bit t i v =
    let byte_i = i / 8 in
    let mask = 1 lsl (i land 7) in
    let cur = Char.to_int (Bytes.get t byte_i) in
    let new_b = if v then cur lor mask else cur land (lnot mask) in
    Bytes.set t byte_i (Char.of_int_exn (new_b land 0xff))
  (* Elementary truth table for variable p out of k vars. *)
  let elem_mask ~k ~p =
    let n_bits = 1 lsl k in
    let t = zero ~k in
    for i = 0 to n_bits - 1 do
      if (i lsr p) land 1 = 1 then
        let b = i / 8 in
        Bytes.set t b
          (Char.of_int_exn (Char.to_int (Bytes.get t b) lor (1 lsl (i land 7))))
    done;
    t
  let to_bool_list ~k t = List.init (1 lsl k) ~f:(bit t)
  (* Lower / upper half (treating MSB of address as a fresh var). *)
  let halves t : t * t =
    let n = Bytes.length t in
    Bytes.sub ~pos:0 ~len:(n / 2) t,
    Bytes.sub ~pos:(n / 2) ~len:(n / 2) t
end

(* Truth table of a cut's cone: 2^|leaves| bits, index i giving the
 * cone output when leaf j carries bit j of i.                          *)
let truth_table_of_cut (g : graph) (cut : cut) : Tt.t =
  let k = List.length cut.leaves in
  let memo = Hashtbl.create (module Int) in
  List.iteri cut.leaves ~f:(fun pos id ->
    Hashtbl.set memo ~key:id ~data:(Tt.elem_mask ~k ~p:pos));
  let rec tt id =
    match Hashtbl.find memo id with
    | Some t -> t
    | None ->
      let t =
        match g.nodes.(id).gate with
        | Const b -> if b then Tt.ones ~k else Tt.zero ~k
        | Input _ ->
          failwith "truth_table_of_cut: input not in cut leaves"
        | And2 { a; b; a_inv; b_inv } ->
          let ta = tt a and tb = tt b in
          let ta = if a_inv then Tt.bit_not ta else ta in
          let tb = if b_inv then Tt.bit_not tb else tb in
          Tt.bit_and ta tb
      in
      Hashtbl.set memo ~key:id ~data:t;
      t
  in
  tt cut.root

(* ---- lutpack: post-mapping LUT merging ---------------------------
 *
 * Inspired by ABC's `lutpack`.  For each pair (parent, child) of chosen
 * LUTs where:
 *   - child has parent.root as one of its leaves;
 *   - parent has exactly one consumer (= child);
 *   - (parent.leaves ∪ (child.leaves \ {parent.root})) fits in ≤ k inputs;
 * compose the two truth tables into a single LUT and drop the parent.
 * Pure LUT-layer rewrite: no AIG round-trip, no placement info needed.
 * Iterate to fixpoint.                                                *)
type packed_lut = {
  pl_root   : int;        (* AIG node id this LUT drives (same as cut.root) *)
  pl_leaves : int list;   (* sorted ascending; carry TT bit positions *)
  pl_tt     : Tt.t;       (* width = 2 ^ |pl_leaves| *)
}

let cut_to_packed (g : graph) (c : cut) : packed_lut =
  { pl_root = c.root
  ; pl_leaves = c.leaves
  ; pl_tt = truth_table_of_cut g c }

(* Compose two truth tables.  parent's TT is over `parent_leaves` in
   order; child's TT is over `child_leaves` in order, and one of
   child's leaves equals parent.pl_root (at position `child_in_idx`).
   The result's variables are the (sorted, deduplicated) union of
   parent's leaves and (child's leaves minus parent.pl_root).         *)
let compose_tt
    ~(combined_leaves : int list)
    ~(parent : packed_lut)
    ~(child  : packed_lut)
    ~(child_in_idx : int)
  : Tt.t
  =
  let n_combined = List.length combined_leaves in
  let combined_arr = Array.of_list combined_leaves in
  let pos_of id =
    (* combined_leaves is small (≤8); linear scan is fine *)
    let r = ref (-1) in
    Array.iteri combined_arr ~f:(fun i v -> if v = id && !r = -1 then r := i);
    if !r < 0 then failwith "compose_tt: leaf not in combined" else !r
  in
  let parent_pos =
    Array.of_list (List.map parent.pl_leaves ~f:pos_of) in
  let child_arr = Array.of_list child.pl_leaves in
  let nc = Array.length child_arr in
  let np = Array.length parent_pos in
  let n_pts = 1 lsl n_combined in
  let new_tt = Tt.zero ~k:n_combined in
  for i = 0 to n_pts - 1 do
    (* Parent's input bits for this assignment. *)
    let p_in = ref 0 in
    for j = 0 to np - 1 do
      if ((i lsr parent_pos.(j)) land 1) = 1 then
        p_in := !p_in lor (1 lsl j)
    done;
    let p_out = Tt.bit parent.pl_tt !p_in in
    (* Child's input bits: take parent's output for child_in_idx, else
       read from the combined assignment. *)
    let c_in = ref 0 in
    for j = 0 to nc - 1 do
      let bit_v =
        if j = child_in_idx then p_out
        else ((i lsr pos_of child_arr.(j)) land 1) = 1
      in
      if bit_v then c_in := !c_in lor (1 lsl j)
    done;
    if Tt.bit child.pl_tt !c_in then Tt.set_bit new_tt i true
  done;
  new_tt

(* Try to fuse parent into child via child's `child_in_idx` leaf.
   Returns the fused packed_lut if it fits in k inputs, else None.    *)
let try_fuse ~(k : int) ~(parent : packed_lut) ~(child : packed_lut)
    ~(child_in_idx : int) : packed_lut option =
  let other_child_leaves =
    List.filteri child.pl_leaves ~f:(fun i _ -> i <> child_in_idx) in
  let combined =
    let rec union_sorted xs ys =
      match xs, ys with
      | [], l | l, [] -> l
      | x :: xs', y :: ys' ->
        if x = y then x :: union_sorted xs' ys'
        else if x < y then x :: union_sorted xs' ys
        else y :: union_sorted xs ys'
    in
    union_sorted parent.pl_leaves other_child_leaves
  in
  if List.length combined > k then None
  else
    let tt = compose_tt ~combined_leaves:combined ~parent ~child ~child_in_idx in
    Some { pl_root = child.pl_root; pl_leaves = combined; pl_tt = tt }

(* Compute fanout count for each root across the LUT set + register/inst
   boundaries.  When a LUT's output is consumed only by ONE other LUT in
   the set, we may absorb it.  Boundary consumers (registers, top
   outputs, instance pins) are counted externally and prevent merging. *)
let lutpack ~(k : int) ~(graph_outputs : (string * int * bool) list)
    ~(extra_consumers : int -> int)
    (luts : packed_lut list)
  : packed_lut list
  =
  let by_root : (int, packed_lut) Hashtbl.t = Hashtbl.create (module Int) in
  List.iter luts ~f:(fun l -> Hashtbl.set by_root ~key:l.pl_root ~data:l);
  let lut_set = Hash_set.of_list (module Int) (List.map luts ~f:(fun l -> l.pl_root)) in
  ignore lut_set;
  (* Fanout: count how many LUTs reference each root as a leaf. *)
  let fanout : (int, int) Hashtbl.t = Hashtbl.create (module Int) in
  let bump root =
    let cur = match Hashtbl.find fanout root with Some n -> n | None -> 0 in
    Hashtbl.set fanout ~key:root ~data:(cur + 1)
  in
  List.iter luts ~f:(fun child ->
    List.iter child.pl_leaves ~f:bump);
  List.iter graph_outputs ~f:(fun (_, id, _) -> bump id);
  let fanout_of r =
    (match Hashtbl.find fanout r with Some n -> n | None -> 0)
    + extra_consumers r
  in
  let fused = ref 0 in
  let rec sweep () =
    let candidate =
      List.find_map luts ~f:(fun child ->
        (* still a current LUT? (unchanged by earlier fusions in this sweep) *)
        match Hashtbl.find by_root child.pl_root with
        | None -> None
        | Some child_cur when not (List.equal Int.equal child_cur.pl_leaves child.pl_leaves) ->
          None  (* skip: child was rewritten this round *)
        | Some child_cur ->
          List.find_mapi child_cur.pl_leaves ~f:(fun idx leaf ->
            if fanout_of leaf > 1 then None
            else
              match Hashtbl.find by_root leaf with
              | None -> None
              | Some parent ->
                (match try_fuse ~k ~parent ~child:child_cur ~child_in_idx:idx with
                 | None -> None
                 | Some merged -> Some (parent, child_cur, merged))))
    in
    match candidate with
    | None -> ()
    | Some (parent, child, merged) ->
      Hashtbl.remove by_root parent.pl_root;
      Hashtbl.set by_root ~key:merged.pl_root ~data:merged;
      (* update fanout: removed parent.pl_root entry (was 1).  Child's
         input set changed (parent's leaves added, parent.pl_root removed).
         Conservatively recompute fanout from scratch below.            *)
      Hashtbl.clear fanout;
      Hashtbl.iter by_root ~f:(fun l -> List.iter l.pl_leaves ~f:bump);
      List.iter graph_outputs ~f:(fun (_, id, _) -> bump id);
      Int.incr fused;
      sweep ()
  in
  sweep ();
  let result =
    Hashtbl.data by_root
    |> List.sort ~compare:(fun a b -> Int.compare a.pl_root b.pl_root) in
  Stdlib.Printf.eprintf "[lutpack] fused %d LUT pairs (%d -> %d)\n%!"
    !fused (List.length luts) (List.length result);
  result

(* ---- covering ---------------------------------------------------- *)

(* Cost preference for cut selection.
 *   `Area  : minimise area-flow first, depth as tie-breaker  (the
 *            historical default — gives compact mappings).
 *   `Delay : minimise depth first, area-flow as tie-breaker  (timing-
 *            driven — emulates the primary key of ABC `if -D` /
 *            `if -F`).
 *   `Mixed slack_tol_steps: per-node hybrid.  Two passes:
 *            (1) area-first cover to compute baseline arrival per node;
 *            (2) backward required-time propagation; (3) re-cover with
 *            depth-first cost on nodes whose slack ≤ slack_tol_steps
 *            (in LUT levels), area-first elsewhere.  Equivalent to ABC
 *            `if -D <target>` where target is the area-pass max-depth. *)
type cost_mode =
  [ `Area | `Delay | `Mixed of int ]

(* Pick one cut per used node (area-flow forward, required-driven
 * backward).  Returns the chosen cuts in topological order of root. *)
let cover ?(mode : cost_mode = `Area) ~(k : int) (g : graph) : cut list =
  let cuts = enumerate_cuts ~k g in
  let n = Array.length g.nodes in
  let fanout = Array.create ~len:n 0 in
  Array.iter g.nodes ~f:(fun nd ->
    match nd.gate with
    | And2 { a; b; _ } ->
      fanout.(a) <- fanout.(a) + 1;
      fanout.(b) <- fanout.(b) + 1
    | _ -> ());
  List.iter g.outputs ~f:(fun (_, id, _) -> fanout.(id) <- fanout.(id) + 1);
  let depth = Array.create ~len:n 0 in
  let area_flow = Array.create ~len:n 0.0 in
  let best = Array.create ~len:n None in
  (* Cost comparator for a single forward pass given a `critical : int
     option array` that says, per node, "use depth-priority here".  None
     means the node isn't critical so use area-priority. *)
  let do_pass ~critical =
    for i = 0 to n - 1 do
      match g.nodes.(i).gate with
      | Input _ | Const _ ->
        depth.(i) <- 0;
        area_flow.(i) <- 0.0
      | And2 _ ->
        let candidates =
          List.filter cuts.(i) ~f:(fun c -> not (List.equal Int.equal c.leaves [ i ]))
        in
        let cost c =
          let d =
            1 + List.fold c.leaves ~init:0 ~f:(fun acc l -> Int.max acc depth.(l))
          in
          let af =
            1.0
            +. List.fold c.leaves ~init:0.0 ~f:(fun acc l ->
                 acc +. (area_flow.(l) /. Float.of_int (Int.max 1 fanout.(l))))
          in
          d, af
        in
        let scored = List.map candidates ~f:(fun c -> c, cost c) in
        let prefer_delay =
          match mode with
          | `Delay -> true
          | `Area -> false
          | `Mixed _ ->
            (match critical with
             | Some arr -> arr.(i) > 0
             | None -> false)
        in
        let cmp (_, (da, aa)) (_, (db, ab)) =
          if prefer_delay
          then (match Int.compare da db with 0 -> Float.compare aa ab | c -> c)
          else (match Float.compare aa ab with 0 -> Int.compare da db | c -> c)
        in
        (match List.min_elt scored ~compare:cmp with
         | None -> failwith "cover: And2 with no non-trivial cut"
         | Some (c, (d, af)) ->
           depth.(i) <- d;
           area_flow.(i) <- af;
           best.(i) <- Some c)
    done
  in
  do_pass ~critical:None;
  (* Mixed mode: run a second pass with depth-priority on critical-path
     nodes.  Critical = "slack ≤ slack_tol".  Slack is computed against
     the max depth observed at any required output. *)
  (match mode with
   | `Mixed slack_tol ->
     let target =
       List.fold g.outputs ~init:0 ~f:(fun acc (_, id, _) -> Int.max acc depth.(id))
     in
     let required = Array.create ~len:n Int.max_value in
     List.iter g.outputs ~f:(fun (_, id, _) -> required.(id) <- target);
     for i = n - 1 downto 0 do
       match g.nodes.(i).gate with
       | And2 _ ->
         (match best.(i) with
          | None -> ()
          | Some c ->
            List.iter c.leaves ~f:(fun l ->
              required.(l) <- Int.min required.(l) (required.(i) - 1)))
       | _ -> ()
     done;
     let critical = Array.init n ~f:(fun i ->
       let slack = required.(i) - depth.(i) in
       if slack <= slack_tol then 1 else 0) in
     do_pass ~critical:(Some critical)
   | _ -> ());
  let required = Array.create ~len:n false in
  List.iter g.outputs ~f:(fun (_, id, _) -> required.(id) <- true);
  let chosen = ref [] in
  for i = n - 1 downto 0 do
    if required.(i) then
      match g.nodes.(i).gate with
      | And2 _ ->
        (match best.(i) with
         | Some c ->
           chosen := c :: !chosen;
           List.iter c.leaves ~f:(fun l ->
             match g.nodes.(l).gate with
             | And2 _ -> required.(l) <- true
             | _ -> ())
         | None -> ())
      | _ -> ()
  done;
  !chosen

(* ---- cut emission helper -----------------------------------------
 * Shared between map_to_luts and fpga_map.  Turns a cut's truth table
 * and input Signal.t list into the Xilinx primitive cone:
 *   k≤6:  one LUTk
 *   k=7:  2 LUT6 + MUXF7 (sel = ins[6])
 *   k=8:  4 LUT6 + 2 MUXF7 + 1 MUXF8 (sels = ins[6], ins[7])
 * Setting [complement] flips every output bit of the truth table
 * before emission (fpga_map uses this for inverted register D cones). *)
let emit_cut_signal
    ~(complement : bool)
    ~(ins : Hardcaml.Signal.t list)
    (tt : Tt.t)
  : Hardcaml.Signal.t =
  let k = List.length ins in
  let tt = if complement then Tt.bit_not tt else tt in
  if k = 0 then
    (* A 0-input cut is a constant (lutpack/mfs2 can eliminate every leaf).
       There is no LUT0 primitive — tie to VCC/GND per the single truth value. *)
    (match Tt.to_bool_list ~k:0 tt with
     | true :: _ -> Hardcaml.Signal.vdd
     | _ -> Hardcaml.Signal.gnd)
  else if k <= 6 then
    Xil_prim.lutk ~truth:(Tt.to_bool_list ~k tt) ins
  else if k = 7 then begin
    let lo, hi = Tt.halves tt in
    let lo6 = List.take ins 6 in
    let s   = List.nth_exn ins 6 in
    let o0 = Xil_prim.lutk ~truth:(Tt.to_bool_list ~k:6 lo) lo6 in
    let o1 = Xil_prim.lutk ~truth:(Tt.to_bool_list ~k:6 hi) lo6 in
    Xil_prim.muxf7 o0 o1 s
  end
  else if k = 8 then begin
    let q01, q23 = Tt.halves tt in
    let q0, q1   = Tt.halves q01 in
    let q2, q3   = Tt.halves q23 in
    let lo6 = List.take ins 6 in
    let s6  = List.nth_exn ins 6 in
    let s7  = List.nth_exn ins 7 in
    let lut q = Xil_prim.lutk ~truth:(Tt.to_bool_list ~k:6 q) lo6 in
    let m7a = Xil_prim.muxf7 (lut q0) (lut q1) s6 in
    let m7b = Xil_prim.muxf7 (lut q2) (lut q3) s6 in
    Xil_prim.muxf8 m7a m7b s7
  end
  else
    failwith (Printf.sprintf "emit_cut_signal: k=%d > 8 not supported" k)

(* ---- netlist construction --------------------------------------- *)

(* Map a subject graph to a Xilinx-LUT netlist: each chosen cut becomes a
 * LUTk via Xil_prim, wired by node id.  Primary inputs are created
 * lazily; chosen cuts are processed in topological order so a cut's
 * And2 leaves already have signals. *)
let map_to_luts ?(mode : cost_mode = `Area) ~(k : int) ~(name : string) (g : graph) : Circuit.t =
  let chosen = cover ~mode ~k g in
  let n = Array.length g.nodes in
  let sig_of = Array.create ~len:n None in
  let signal_of_node id =
    match sig_of.(id) with
    | Some s -> s
    | None ->
      let s =
        match g.nodes.(id).gate with
        | Input nm -> Signal.input nm 1
        | Const b -> if b then Signal.vdd else Signal.gnd
        | And2 _ -> failwith "map_to_luts: And2 node used before its LUT was built"
      in
      sig_of.(id) <- Some s;
      s
  in
  List.iter chosen ~f:(fun c ->
    let ins = List.map c.leaves ~f:signal_of_node in
    let tt = truth_table_of_cut g c in
    sig_of.(c.root) <- Some (emit_cut_signal ~complement:false ~ins tt));
  let outs =
    List.map g.outputs ~f:(fun (nm, id, inv) ->
      let s = signal_of_node id in
      let s = if inv then Signal.( ~: ) s else s in
      Signal.output nm s)
  in
  Circuit.create_exn ~name outs
