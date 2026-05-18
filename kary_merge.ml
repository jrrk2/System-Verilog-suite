(* k-ary AND/OR merge — collapse chains/trees of same-family
   AND2/OR2 cells into AND3/AND4/OR3/OR4 cells when intermediate
   nets are single-fanout.

   Why this is a clear win
   =======================
   A chain of N OR2_X1 cells combines (N+1) signals using N gates and
   has depth N.  Replacing with OR4_X1 cells:

     OR2 chain of length 7 (combines 8 signals)
       cells = 7    depth = 7
     OR4 tree balanced (combines 8 signals)
       cells = 3    depth = 2

   2.3× fewer cells; 3.5× shorter depth.  AND4_X1's intrinsic delay
   is ~1.4× AND2_X1, but the eliminated wire+input-cap delay between
   the absorbed hops is typically larger — and even when neutral on
   delay, the area win is unconditional.

   Strategy
   ========
   For each instance H of a 2-input AND/OR family cell, look at H's
   inputs.  For each input net n:

     - if n's *only* reader is H (fanout = 1), and
     - n is the output of another AND/OR cell J of the *same* family,
       and J's output is not consumed by anything else,

   then J is "absorbable" into H: drop J, append J's two inputs to
   H's input list, and bump H to the next-wider variant of the
   family.  Cap at arity 4 (Nangate45's widest AND/OR).

   This works on chains AND on balanced trees — the predicate is
   per-edge, not per-shape.

   What we do NOT touch
   ====================
   - NAND/NOR families: NAND2(NAND2(a,b),c) ≠ NAND3(a,b,c) because
     of the inversion, so the merge isn't a structural identity.
     (Could pattern-match NAND2-chain → NAND2 of AND-of-rest, but
     that's a different rewrite.)
   - Mixed AND-OR (covered by AOI21/OAI21).  Future work.
   - INV-following AND/OR: AND2→INV is just NAND2.  Future work
     (also a single-cell saving).

   Verification
   ============
   k-input AND / OR is a trivial boolean identity, so cert-gating
   is unnecessary — but the per-merge proof obligation
       AND4(a,b,c,d) ≡ AND2(AND2(a,b), AND2(c,d))
   takes <1ms to discharge in Z3 if we ever want belt-and-braces.

   Off-switch
   ==========
   SV_DECOMP_NO_KARY_MERGE=1 disables.                                *)

open Lib_map

(* ── Family classification ───────────────────────────────────── *)

type family = AND_F | OR_F

let starts_with pfx s =
  let pl = String.length pfx and sl = String.length s in
  sl >= pl && String.sub s 0 pl = pfx

(* Returns (family, arity) for AND/OR cells of arity 2..4.
   Anything else returns None. *)
let family_arity_of cell_name =
  let try_one fam name_pfx =
    if starts_with (name_pfx ^ "2_") cell_name then Some (fam, 2)
    else if starts_with (name_pfx ^ "3_") cell_name then Some (fam, 3)
    else if starts_with (name_pfx ^ "4_") cell_name then Some (fam, 4)
    else None
  in
  match try_one AND_F "AND" with
  | Some _ as r -> r
  | None -> try_one OR_F "OR"

let cell_for_family_arity fam k =
  let pfx = match fam with AND_F -> "AND" | OR_F -> "OR" in
  let in_pins = match k with
    | 2 -> ["A1"; "A2"]
    | 3 -> ["A1"; "A2"; "A3"]
    | 4 -> ["A1"; "A2"; "A3"; "A4"]
    | _ -> assert false in
  { cell_name = Printf.sprintf "%s%d_X1" pfx k;
    in_pins;
    out_pin = "ZN" }

(* ── Pass ────────────────────────────────────────────────────── *)

(* Result counters: (number of cells removed, number of merges performed).
   #removed cells equals #merges (each merge eats exactly one driver).

   Note: this is one *pass*.  [merge_module] iterates it to fixed point;
   each pass absorbs ~2 cells per cell at the head of every remaining
   chain segment, so a chain of N AND2 settles after ⌈log₃ N⌉ passes. *)
let merge_module_once ?(timing_ref = None) (nl : netlist) : netlist * int =
  begin
    (* Index instances by output net.  No load count needed: we now
       allow *duplicating* absorption — a sink can pull a same-family
       2-input driver's inputs into itself regardless of fanout.  The
       driver stays in the netlist for its other consumers; if all of
       them eventually absorb it, [dce_module] sweeps it away.

       Exception: when [timing_ref] flags the sink as on the prior
       run's worst-slack path, we revert to single-fanout absorption
       — duplicating a high-fanout driver into a critical-path sink
       widens *that* cell, and OR4_X4's 1.4× per-cell delay vs OR2_X4
       can lose more than it saves.  Off-critical-path sinks keep the
       aggressive dup behaviour (area win + neutral-or-better depth). *)
    let driver : (string, instance) Hashtbl.t = Hashtbl.create 1024 in
    List.iter (fun i ->
      List.iter (fun c ->
        if c.pin = i.cell.out_pin then Hashtbl.replace driver c.net i
      ) i.conns
    ) nl.insts;
    (* Compute load (count of inst pin connections + assign references
       reading the net) only when timing_ref is active — otherwise we
       allow unconditional duplicating absorb and don't need it. *)
    let load : (string, int) Hashtbl.t option =
      match timing_ref with
      | None -> None
      | Some _ ->
          let h = Hashtbl.create 1024 in
          let bump n =
            let c = try Hashtbl.find h n with Not_found -> 0 in
            Hashtbl.replace h n (c + 1) in
          List.iter (fun i ->
            List.iter (fun c ->
              if c.pin <> i.cell.out_pin then bump c.net
            ) i.conns
          ) nl.insts;
          List.iter (fun (n, _) -> bump n) nl.outputs;
          Some h in
    let merges = ref 0 in

    let in_pin_nets inst =
      List.filter_map (fun c ->
        if c.pin = inst.cell.out_pin then None
        else Some (c.pin, c.net)
      ) inst.conns in

    (* Per-sink slack gate: when timing_ref flags this inst as on the
       prior run's critical path, require single-fanout absorption
       (don't duplicate a high-fanout driver into a hot path). *)
    let single_fanout_required (h : instance) : bool =
      Timing_ref.mem timing_ref h.inst_name in

    let load_of net =
      match load with
      | None -> 1  (* unconditional absorb allowed when no slack gate *)
      | Some h -> (try Hashtbl.find h net with Not_found -> 0) in

    (* Rewrite each H: walk inputs, replace any same-family 2-input
       driver's output-net with the driver's two input nets.  Repeat
       up to arity 4. *)
    let rewrite_inst (h : instance) : instance =
      match family_arity_of h.cell.cell_name with
      | None -> h
      | Some (fam, ar0) ->
          let arity = ref ar0 in
          let nets = ref (List.map snd (in_pin_nets h)) in
          let made_progress = ref true in
          let need_single_fanout = single_fanout_required h in
          while !made_progress && !arity < 4 do
            made_progress := false;
            let cap = 4 - !arity in
            let new_nets = ref [] in
            let did = ref false in
            List.iter (fun net ->
              if !did || cap < 1 then new_nets := net :: !new_nets
              else
                match Hashtbl.find_opt driver net with
                | None -> new_nets := net :: !new_nets
                | Some j ->
                    (match family_arity_of j.cell.cell_name with
                     | Some (jfam, jar) when jfam = fam && jar = 2
                                          && (not need_single_fanout
                                              || load_of net = 1) ->
                         let j_ins = List.map snd (in_pin_nets j) in
                         List.iter (fun ni -> new_nets := ni :: !new_nets) j_ins;
                         arity := !arity + (jar - 1);
                         incr merges;
                         did := true;
                         made_progress := true
                     | _ -> new_nets := net :: !new_nets)
            ) !nets;
            nets := List.rev !new_nets
          done;
          if !arity = ar0 then h
          else
            let new_cell = cell_for_family_arity fam !arity in
            let out_pc =
              List.find (fun c -> c.pin = h.cell.out_pin) h.conns in
            let ins =
              List.mapi (fun i n ->
                { pin = List.nth new_cell.in_pins i; net = n }
              ) !nets in
            let new_conns =
              { pin = new_cell.out_pin; net = out_pc.net } :: ins in
            { cell = new_cell; inst_name = h.inst_name; conns = new_conns }
    in

    (* Sink-first walk: drivers visited later still find their
       *original* inputs in [driver], so duplicating absorption is
       order-independent.  Reversing matches synth's typical
       driver-before-sink topological emit. *)
    let new_insts =
      List.map rewrite_inst (List.rev nl.insts) |> List.rev in

    { nl with insts = new_insts }, !merges
  end

(* Drop instances whose output net has zero readers (and isn't a
   top-level output port).  Runs after each merge pass to clean up
   drivers that were inlined into every consumer.                  *)
let dce_module (nl : netlist) : netlist * int =
  let load : (string, int) Hashtbl.t = Hashtbl.create 1024 in
  let bump n =
    let c = try Hashtbl.find load n with Not_found -> 0 in
    Hashtbl.replace load n (c + 1) in
  List.iter (fun i ->
    List.iter (fun c ->
      if c.pin <> i.cell.out_pin then bump c.net
    ) i.conns
  ) nl.insts;
  List.iter (fun (n, _) -> bump n) nl.outputs;
  (* Continuous assigns: any occurrence of a driven net in the RHS
     counts as a load.  Substring match is conservative but cheap. *)
  let driven_nets =
    let h = Hashtbl.create 1024 in
    List.iter (fun i ->
      List.iter (fun c ->
        if c.pin = i.cell.out_pin then Hashtbl.replace h c.net ()
      ) i.conns
    ) nl.insts;
    h in
  let base_of n =
    try
      let i = String.index n '[' in
      String.sub n 0 i
    with Not_found -> n in
  List.iter (fun (_lhs, rhs) ->
    Hashtbl.iter (fun net _ ->
      if String.length net > 0 then begin
        let scan_for str =
          let nlen = String.length str in
          let rlen = String.length rhs in
          if rlen >= nlen then
            let rec loop i =
              if i + nlen > rlen then false
              else if String.sub rhs i nlen = str then true
              else loop (i + 1)
            in loop 0
          else false in
        let bare = base_of net in
        if scan_for net then bump net
        else if bare <> net && scan_for bare then bump net
      end
    ) driven_nets
  ) nl.assigns;
  let dropped = ref 0 in
  let kept =
    List.filter (fun i ->
      match List.find_opt (fun c -> c.pin = i.cell.out_pin) i.conns with
      | Some c when not (Hashtbl.mem load c.net) ->
          incr dropped; false
      | _ -> true
    ) nl.insts in
  let kept_out_nets = Hashtbl.create 1024 in
  List.iter (fun i ->
    List.iter (fun c ->
      if c.pin = i.cell.out_pin then Hashtbl.replace kept_out_nets c.net ()
    ) i.conns
  ) kept;
  (* Wires that no surviving cell drives become dead. *)
  let kept_wires =
    List.filter (fun (n, _) -> Hashtbl.mem kept_out_nets n) nl.wires in
  { nl with insts = kept; wires = kept_wires }, !dropped

(* Iterate [merge_module_once] → [dce_module] until no more merges.
   Each merge pass absorbs same-family 2-input drivers into every
   downstream AND/OR sink whose arity has slack — *regardless of
   fanout*.  Duplicating absorption leaves the original driver in
   place; the DCE pass that follows sweeps any driver whose entire
   consumer set has now absorbed its inputs (load = 0).

   Multiple passes are essential because absorption shortens the
   chain at the head, and the *new* head — formerly a chain-internal
   cell — then becomes eligible to absorb further back.  Cap at 16. *)
let merge_module_iter_cap =
  match Sys.getenv_opt "SV_DECOMP_KARY_MAX_PASSES" with
  | Some s -> (try int_of_string s with _ -> 64)
  | None -> 64

let merge_module (nl : netlist) : netlist * int =
  if Sys.getenv_opt "SV_DECOMP_NO_KARY_MERGE" = Some "1" then nl, 0
  else begin
    let timing_ref = Timing_ref.from_env () in
    let total_merges = ref 0 in
    let total_dropped = ref 0 in
    let nl_ref = ref nl in
    let continue = ref true in
    let pass = ref 0 in
    while !continue && !pass < merge_module_iter_cap do
      incr pass;
      let nl', m = merge_module_once ~timing_ref !nl_ref in
      let nl'', d = dce_module nl' in
      if Sys.getenv_opt "SV_DECOMP_KARY_DEBUG" = Some "1" then
        Printf.eprintf "[kary_merge] pass %d: %d merge(s), %d drop(s)\n%!"
          !pass m d;
      total_merges := !total_merges + m;
      total_dropped := !total_dropped + d;
      nl_ref := nl'';
      if m = 0 && d = 0 then continue := false
    done;
    if !total_merges > 0 || !total_dropped > 0 then
      Printf.eprintf
        "[kary_merge] %d merge(s), %d dead cell(s) swept (%d pass%s)\n"
        !total_merges !total_dropped !pass
        (if !pass = 1 then "" else "es");
    !nl_ref, !total_merges
  end
