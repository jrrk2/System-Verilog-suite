(* mfs2 — Maximum Fanin Substitution v2, inspired by ABC's `mfs2`.
 *
 * A post-mapping pass that takes a set of packed LUTs and tries to
 * minimize each one using don't-cares contributed by the surrounding
 * window.  Two transform tiers:
 *
 *   1. variable_elim  — pure-TT.  If a LUT's function is independent of
 *      one of its inputs, drop that input.  k inputs → k−1.  No BDDs,
 *      no window construction.  Catches cover/lutpack leftovers.
 *
 *   2. odc_elim (TODO) — BDD-based.  For each LUT L, build the BDD of
 *      its TFO window and project to compute the observability
 *      don't-care set (input vectors where flipping L's output value
 *      doesn't change ANY window output).  Then re-test each L input
 *      for redundancy on the care set.
 *
 * The two transforms compose: variable_elim cheap → odc_elim deeper.
 * Both reduce LUT-input count; the LUTs whose inputs drop to 0 become
 * constants and propagate out via downstream consumers.  *)

open! Base

(* ---- Phase 1: pure-TT variable elimination ----------------------- *)

(* Is variable [pos] (the [pos]th input, 0-based) functionally redundant
   in [tt] over [k] vars?  True iff for every input vector v with bit
   [pos]=0, tt(v) == tt(v|(1<<pos)).                                  *)
let var_redundant_tt ~(k : int) (tt : Lut_cover.Tt.t) ~(pos : int) : bool =
  let mask_pos = 1 lsl pos in
  let n = 1 lsl k in
  let rec scan v =
    if v >= n then true
    else if (v land mask_pos) <> 0 then scan (v + 1)
    else
      Bool.equal
        (Lut_cover.Tt.bit tt v)
        (Lut_cover.Tt.bit tt (v lor mask_pos))
      && scan (v + 1)
  in
  scan 0

(* Project tt over (k-1) vars, dropping variable [pos].  When we know
   the function ignores [pos], take the half where bit [pos] = 0 and
   renumber the remaining bits.                                       *)
let project_tt ~(k : int) (tt : Lut_cover.Tt.t) ~(pos : int) : Lut_cover.Tt.t =
  let k' = k - 1 in
  let n' = 1 lsl k' in
  let new_tt = Lut_cover.Tt.zero ~k:k' in
  (* For new index v' (0..2^(k-1)-1), the old index is constructed by
     splicing the bits below [pos] back into their positions and shifting
     the rest up — but since the function is independent of bit [pos] we
     just pick the half with bit [pos] = 0. *)
  for v' = 0 to n' - 1 do
    let low  = v' land ((1 lsl pos) - 1) in
    let high = (v' lsr pos) lsl (pos + 1) in
    let old_v = low lor high in
    if Lut_cover.Tt.bit tt old_v then Lut_cover.Tt.set_bit new_tt v' true
  done;
  new_tt

let variable_elim (l : Lut_cover.packed_lut) : Lut_cover.packed_lut =
  let rec loop (lut : Lut_cover.packed_lut) =
    let k = List.length lut.pl_leaves in
    if k = 0 then lut
    else
      let leaves_arr = Array.of_list lut.pl_leaves in
      let redundant = ref (-1) in
      for pos = 0 to k - 1 do
        if !redundant < 0
        && var_redundant_tt ~k lut.pl_tt ~pos
        then redundant := pos
      done;
      if !redundant < 0 then lut
      else begin
        let pos = !redundant in
        let new_tt = project_tt ~k lut.pl_tt ~pos in
        let new_leaves =
          List.filteri lut.pl_leaves ~f:(fun i _ -> i <> pos) in
        ignore leaves_arr;
        loop { Lut_cover.pl_root = lut.pl_root
             ; pl_leaves = new_leaves
             ; pl_tt = new_tt }
      end
  in
  loop l

(* ---- Phase 2: BDD-based ODC elimination ---------
 *
 * For each LUT L, look at the LUTs that directly consume L's output
 * (its "depth-1 fanout window").  Compute the BDD of every consumer
 * with L's output replaced by a single shared variable.  Then:
 *
 *   F0_i = restrict consumer_i.bdd  L_OUT  false
 *   F1_i = restrict consumer_i.bdd  L_OUT  true
 *   CARE_i = F0_i XOR F1_i              (* consumer i distinguishes L_out *)
 *   CARE   = OR_i CARE_i                (* any consumer distinguishes *)
 *
 * For each L input i:
 *   L0_i = restrict L.bdd  L_input_i  false
 *   L1_i = restrict L.bdd  L_input_i  true
 *   DIFF_i = L0_i XOR L1_i              (* where input i flips L's output *)
 *   if (DIFF_i ∧ CARE) = ZERO then input i is redundant under ODC.
 *
 * Re-project L's TT (drop input i and pick either half — they agree on
 * CARE).  Iterate per-LUT to fixpoint. *)

(* Collect window consumers: for each AIG root r, the list of
   (consumer LUT, position of r in consumer's leaves). *)
let build_consumer_index (luts : Lut_cover.packed_lut list)
  : (int, (Lut_cover.packed_lut * int) list) Hashtbl.t
  =
  let h = Hashtbl.create (module Int) in
  List.iter luts ~f:(fun consumer ->
    List.iteri consumer.pl_leaves ~f:(fun pos leaf ->
      let cur = match Hashtbl.find h leaf with Some l -> l | None -> [] in
      Hashtbl.set h ~key:leaf ~data:((consumer, pos) :: cur)));
  h

(* Build a BDD for one LUT, mapping its k leaves to consecutive BDD
   variables [start_var..start_var+k-1].  If [subst_pos] is Some p, the
   leaf at position p uses BDD var [subst_var] (i.e. it's the "L output"
   variable in the window, not the leaf's own variable mapping).        *)
let bdd_of_tt
    (type bdd) (module B : Bdd.BDD with type t = bdd)
    ~(var_of_leaf : int -> int)
    ?(subst : (int * int) option = None)
    (lut : Lut_cover.packed_lut) : bdd
  =
  let leaves_arr = Array.of_list lut.pl_leaves in
  let k = Array.length leaves_arr in
  let n_pts = 1 lsl k in
  let acc = ref B.zero in
  for v = 0 to n_pts - 1 do
    if Lut_cover.Tt.bit lut.pl_tt v then begin
      let term = ref B.one in
      for i = 0 to k - 1 do
        let var_id =
          match subst with
          | Some (p, sv) when p = i -> sv
          | _ -> var_of_leaf leaves_arr.(i)
        in
        let lit =
          if (v lsr i) land 1 = 1 then B.mk_var var_id
          else B.mk_not (B.mk_var var_id)
        in
        term := B.mk_and !term lit
      done;
      acc := B.mk_or !acc !term
    end
  done;
  !acc

(* Re-derive a packed_lut's TT after fixing one of its leaves to a
   constant value (0 or 1).  Equivalent to BDD restrict, but stays in
   the Tt domain so we don't need a global BDD module.                  *)
let restrict_tt_drop_var
    ~(k : int) (tt : Lut_cover.Tt.t) ~(pos : int) ~(value : bool)
  : Lut_cover.Tt.t
  =
  let k' = k - 1 in
  let n' = 1 lsl k' in
  let new_tt = Lut_cover.Tt.zero ~k:k' in
  for v' = 0 to n' - 1 do
    let low  = v' land ((1 lsl pos) - 1) in
    let high = (v' lsr pos) lsl (pos + 1) in
    let bit_v = if value then 1 lsl pos else 0 in
    let old_v = low lor high lor bit_v in
    if Lut_cover.Tt.bit tt old_v then Lut_cover.Tt.set_bit new_tt v' true
  done;
  new_tt

(* Per-LUT ODC test using a *shared* BDD module.  The shared module's
   max_var is chosen large enough for the worst-case window (k inputs
   per LUT + k inputs per consumer + 1 for L_OUT ≤ 8 + 8*8 + 1 ≈ 73).
   Variable indices 1..max_var are *reused* across LUTs by renumbering
   each window's AIG ids to 1..n_window+1.  Re-using indices means the
   hash-cons table grows but only with distinct boolean functions, not
   distinct LUT × consumer combinations.                              *)
let odc_max_var = 160

(* BFS the fanout from L to depth `depth`, returning the LUTs at each
   level (depth 0 = [L]).  Stops descending past LUTs not in
   consumers_idx (e.g., boundary consumers like register Ds).        *)
let bfs_window
    ~(consumers_idx : (int, (Lut_cover.packed_lut * int) list) Hashtbl.t)
    ~(depth : int)
    (l : Lut_cover.packed_lut)
  : Lut_cover.packed_lut list array
  =
  let levels = Array.create ~len:(depth + 1) [] in
  let in_window = Hash_set.create (module Int) in
  levels.(0) <- [l];
  Hash_set.add in_window l.pl_root;
  for d = 1 to depth do
    levels.(d) <-
      List.concat_map levels.(d - 1) ~f:(fun lut ->
        match Hashtbl.find consumers_idx lut.pl_root with
        | None -> []
        | Some cs ->
          List.filter_map cs ~f:(fun (c, _) ->
            if Hash_set.mem in_window c.pl_root then None
            else begin
              Hash_set.add in_window c.pl_root;
              Some c
            end))
  done;
  levels

(* Depth-N ODC redundancy check for one LUT.                          *)
let odc_elim_one
    (type bdd) (module B : Bdd.BDD with type t = bdd)
    ~(consumers_idx : (int, (Lut_cover.packed_lut * int) list) Hashtbl.t)
    ~(depth : int)
    ~(window_max : int)
    (l : Lut_cover.packed_lut)
  : Lut_cover.packed_lut option
  =
  let k = List.length l.pl_leaves in
  if k = 0 then None
  else begin
    let levels = bfs_window ~consumers_idx ~depth l in
    let window_luts =
      Array.fold levels ~init:[] ~f:(fun acc lst -> acc @ lst)
    in
    let window_roots = Hash_set.of_list (module Int)
      (List.map window_luts ~f:(fun w -> w.Lut_cover.pl_root)) in
    if List.length window_luts <= 1 then None
    else begin
      (* "Boundary" vars = leaves of window LUTs that are not themselves
         in-window LUT roots.  Plus a dedicated L_OUT var so consumers
         see L's output as a free variable.                            *)
      let boundary =
        let s = Hash_set.create (module Int) in
        List.iter window_luts ~f:(fun w ->
          List.iter w.Lut_cover.pl_leaves ~f:(fun leaf ->
            if not (Hash_set.mem window_roots leaf) then Hash_set.add s leaf));
        List.sort (Hash_set.to_list s) ~compare:Int.compare
      in
      let n_boundary = List.length boundary in
      let l_out_var = n_boundary + 1 in
      if l_out_var > odc_max_var || l_out_var > window_max then None
      else begin
        let var_of_boundary =
          let tbl = Hashtbl.create (module Int) in
          List.iteri boundary ~f:(fun i v ->
            Hashtbl.set tbl ~key:v ~data:(i + 1));
          fun aig -> Hashtbl.find_exn tbl aig
        in
        (* Build the BDD for each in-window LUT in topo order (level 0
           first).  L's BDD: as if L's output is a free var (we'll
           substitute it via L_OUT in downstream LUTs).  For LUTs C in
           level ≥ 1: build using the BDDs of any in-window leaves,
           plus the boundary var for outside leaves; L's input slot
           uses L_OUT directly.                                       *)
        let bdd_of : (int, B.t) Hashtbl.t = Hashtbl.create (module Int) in
        let l_bdd = bdd_of_tt (module B) ~var_of_leaf:var_of_boundary l in
        Hashtbl.set bdd_of ~key:l.pl_root ~data:l_bdd;
        (* Topological build order: window LUTs other than L, sorted by
           AIG root id ascending.  AIG ids are topological by
           construction (an And2's children have lower ids), so siblings
           at the same BFS level that reference each other still come in
           the right order.                                              *)
        let other_window =
          List.filter window_luts ~f:(fun w ->
            w.Lut_cover.pl_root <> l.pl_root)
          |> List.sort ~compare:(fun a b ->
              Int.compare a.Lut_cover.pl_root b.Lut_cover.pl_root)
        in
        List.iter other_window ~f:(fun lut ->
          let tt = lut.Lut_cover.pl_tt in
          let k_c = List.length lut.Lut_cover.pl_leaves in
          let leaves_arr = Array.of_list lut.Lut_cover.pl_leaves in
          let acc = ref B.zero in
          let n_pts = 1 lsl k_c in
          for v = 0 to n_pts - 1 do
            if Lut_cover.Tt.bit tt v then begin
              let term = ref B.one in
              for i = 0 to k_c - 1 do
                let leaf_id = leaves_arr.(i) in
                let lit_pos =
                  if leaf_id = l.pl_root then
                    B.mk_var l_out_var
                  else
                    match Hashtbl.find bdd_of leaf_id with
                    | Some b -> b
                    | None -> B.mk_var (var_of_boundary leaf_id)
                in
                let lit =
                  if (v lsr i) land 1 = 1 then lit_pos else B.mk_not lit_pos
                in
                term := B.mk_and !term lit
              done;
              acc := B.mk_or !acc !term
            end
          done;
          Hashtbl.set bdd_of ~key:lut.Lut_cover.pl_root ~data:!acc);
        ignore levels;  (* topo build above replaces the level walk *)
        (* CARE = OR over window LUTs whose output drives outside the
           window of (F0_i XOR F1_i).  A window LUT is a "window output"
           if any of its consumers is NOT in the window. *)
        let care =
          List.fold window_luts ~init:B.zero ~f:(fun acc w ->
            (* Skip L itself when computing CARE — L's output IS the
               variable we're analyzing.  *)
            if w.Lut_cover.pl_root = l.pl_root then acc
            else
              let consumers =
                match Hashtbl.find consumers_idx w.Lut_cover.pl_root with
                | Some lst -> lst | None -> [] in
              let any_outside =
                List.is_empty consumers
                || List.exists consumers ~f:(fun (c, _) ->
                    not (Hash_set.mem window_roots c.Lut_cover.pl_root))
              in
              if not any_outside then acc
              else
                let f = Hashtbl.find_exn bdd_of w.Lut_cover.pl_root in
                let f0 = B.restrict f l_out_var false in
                let f1 = B.restrict f l_out_var true in
                let ci = B.apply (fun a b -> Bool.( <> ) a b) f0 f1 in
                B.mk_or acc ci)
        in
        let leaves_arr = Array.of_list l.pl_leaves in
        let rec try_drop i =
          if i >= k then None
          else
            let vi = var_of_boundary leaves_arr.(i) in
            let l0 = B.restrict l_bdd vi false in
            let l1 = B.restrict l_bdd vi true in
            let diff = B.apply (fun a b -> Bool.( <> ) a b) l0 l1 in
            let conflict = B.mk_and diff care in
            if B.equivalent conflict B.zero then Some i
            else try_drop (i + 1)
        in
        match try_drop 0 with
        | None -> None
        | Some pos ->
          let new_tt = restrict_tt_drop_var ~k l.pl_tt ~pos ~value:false in
          let new_leaves =
            List.filteri l.pl_leaves ~f:(fun i _ -> i <> pos) in
          Some { Lut_cover.pl_root = l.pl_root
               ; pl_leaves = new_leaves
               ; pl_tt = new_tt }
      end
    end
  end

(* odc_elim: tunable.  ODC analysis is O(2^window_size) per LUT and
   the BDD hash-cons table grows monotonically across LUTs.  Guards:
     - per-LUT window-size cap (window_max)
     - per-LUT consumer-count cap (max_consumers)
     - per-LUT BDD-node budget (effectively via window_max)
     - whole-pass time/LUT cap (max_luts; 0 = unlimited)
     - single pass, no fixpoint iteration (the variable_elim pre-pass
       already runs its own fixpoint)                                   *)
let odc_elim
    ?(depth = 2)
    ?(window_max = 24)
    ?(max_consumers = 8)
    ?(max_luts = 0)
    (luts : Lut_cover.packed_lut list)
  : Lut_cover.packed_lut list
  =
  let consumers_idx = build_consumer_index luts in
  let module B = (val Bdd.make ~size:4096 odc_max_var : Bdd.BDD) in
  let dropped = ref 0 in
  let skipped_too_big = ref 0 in
  let attempted = ref 0 in
  let result = List.map luts ~f:(fun l ->
    if max_luts > 0 && !attempted >= max_luts then l
    else
      let direct_consumers =
        match Hashtbl.find consumers_idx l.Lut_cover.pl_root with
        | Some lst -> lst
        | None -> []
      in
      if List.length direct_consumers > max_consumers
      || List.is_empty direct_consumers
      then begin
        if not (List.is_empty direct_consumers) then Int.incr skipped_too_big;
        l
      end else begin
        Int.incr attempted;
        match odc_elim_one (module B) ~consumers_idx ~depth ~window_max l with
        | None -> l
        | Some l' -> Int.incr dropped; l'
      end)
  in
  Stdlib.Printf.eprintf
    "[mfs2] odc_elim (depth=%d, window<=%d, max_consumers<=%d): attempted %d, dropped %d, skipped %d\n%!"
    depth window_max max_consumers !attempted !dropped !skipped_too_big;
  result

(* ---- top-level driver ------------------------------------------- *)

let run ~(enable_var_elim : bool) ~(enable_odc : bool)
    (luts : Lut_cover.packed_lut list)
  : Lut_cover.packed_lut list
  =
  let before_var_elim_avg =
    if List.is_empty luts then 0.0
    else
      Float.of_int
        (List.fold luts ~init:0 ~f:(fun a l -> a + List.length l.pl_leaves))
      /. Float.of_int (List.length luts)
  in
  let after_var_elim =
    if enable_var_elim then List.map luts ~f:variable_elim else luts
  in
  let dropped_inputs =
    List.fold2_exn luts after_var_elim ~init:0 ~f:(fun acc a b ->
      acc + List.length a.pl_leaves - List.length b.pl_leaves)
  in
  if enable_var_elim then
    Stdlib.Printf.eprintf "[mfs2] variable_elim: dropped %d LUT inputs (avg %.2f -> %.2f)\n%!"
      dropped_inputs before_var_elim_avg
      (if List.is_empty after_var_elim then 0.0
       else
         Float.of_int
           (List.fold after_var_elim ~init:0 ~f:(fun a l -> a + List.length l.pl_leaves))
         /. Float.of_int (List.length after_var_elim));
  if enable_odc then odc_elim after_var_elim
  else after_var_elim
