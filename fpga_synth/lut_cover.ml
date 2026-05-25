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

(* Elementary truth tables over 6 variables, packed in an Int64: bit i of
 * elem_masks.(p) is set iff bit p of i is 1.  A cut's truth table is then
 * AND/NOT of these per the cone structure. *)
let elem_masks =
  Array.init 6 ~f:(fun p ->
    let m = ref Int64.zero in
    for i = 0 to 63 do
      if (i lsr p) land 1 = 1 then
        m := Int64.bit_or !m (Int64.shift_left Int64.one i)
    done;
    !m)

let bit_set t i =
  Int64.equal (Int64.bit_and (Int64.shift_right_logical t i) Int64.one) Int64.one

(* Truth table of a cut's cone: 2^|leaves| values, index i giving the
 * cone output when leaf j carries bit j of i.  Fed directly to
 * Xil_prim.lutk ~truth. *)
let truth_table_of_cut (g : graph) (cut : cut) : bool list =
  let memo = Hashtbl.create (module Int) in
  List.iteri cut.leaves ~f:(fun pos id ->
    Hashtbl.set memo ~key:id ~data:elem_masks.(pos));
  let rec tt id =
    match Hashtbl.find memo id with
    | Some t -> t
    | None ->
      let t =
        match g.nodes.(id).gate with
        | Const b -> if b then Int64.bit_not Int64.zero else Int64.zero
        | Input _ ->
          (* a primary input inside the cone must have been a leaf. *)
          failwith "truth_table_of_cut: input not in cut leaves"
        | And2 { a; b; a_inv; b_inv } ->
          let ta = tt a and tb = tt b in
          let ta = if a_inv then Int64.bit_not ta else ta in
          let tb = if b_inv then Int64.bit_not tb else tb in
          Int64.bit_and ta tb
      in
      Hashtbl.set memo ~key:id ~data:t;
      t
  in
  let t = tt cut.root in
  let m = 1 lsl List.length cut.leaves in
  List.init m ~f:(fun i -> bit_set t i)

(* ---- covering ---------------------------------------------------- *)

(* Pick one cut per used node (area-flow forward, required-driven
 * backward).  Returns the chosen cuts in topological order of root. *)
let cover ~(k : int) (g : graph) : cut list =
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
  for i = 0 to n - 1 do
    match g.nodes.(i).gate with
    | Input _ | Const _ ->
      depth.(i) <- 0;
      area_flow.(i) <- 0.0
    | And2 _ ->
      (* skip the self cut: a node never implements itself with a LUT
         whose only input is itself. *)
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
      (match
         List.min_elt scored ~compare:(fun (_, (da, aa)) (_, (db, ab)) ->
           match Float.compare aa ab with 0 -> Int.compare da db | c -> c)
       with
       | None -> failwith "cover: And2 with no non-trivial cut"
       | Some (c, (d, af)) ->
         depth.(i) <- d;
         area_flow.(i) <- af;
         best.(i) <- Some c)
  done;
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

(* ---- netlist construction --------------------------------------- *)

(* Map a subject graph to a Xilinx-LUT netlist: each chosen cut becomes a
 * LUTk via Xil_prim, wired by node id.  Primary inputs are created
 * lazily; chosen cuts are processed in topological order so a cut's
 * And2 leaves already have signals. *)
let map_to_luts ~(k : int) ~(name : string) (g : graph) : Circuit.t =
  let chosen = cover ~k g in
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
    let truth = truth_table_of_cut g c in
    sig_of.(c.root) <- Some (Xil_prim.lutk ~truth ins));
  let outs =
    List.map g.outputs ~f:(fun (nm, id, inv) ->
      let s = signal_of_node id in
      let s = if inv then Signal.( ~: ) s else s in
      Signal.output nm s)
  in
  Circuit.create_exn ~name outs
