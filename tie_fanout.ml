(* Pre-empt OpenROAD's [repair_tie_fanout] — the floorplan-stage
   transform that splits a single LOGIC0_X1 / LOGIC1_X1 cell when
   too many gates load it.

   Why move it here, before OpenROAD reads our Verilog
   ===================================================
   Lib_map currently emits one tie cell per type per module
   (one LOGIC0_X1, one LOGIC1_X1).  Big designs end up with
   hundreds of sinks pulling from a single tie output:

     LOGIC0_X1 _tie_lo_inst_ ( .Z(_tie_lo_) );
     ...
     INV_X1 _foo_       ( .A(_tie_lo_), .ZN(...) );  ← sink 1
     AND2_X1 _bar_      ( .A1(_tie_lo_), ... );      ← sink 2
     ... × 200 more

   200-fanout on an X1 driver violates max_capacitance + max_transition
   immediately.  OpenROAD eventually catches this in repair_design but
   spends optimisation budget there that should go to the actual
   critical path.  Splitting at synth time costs ~1 cell per N sinks
   and removes the violation up front.

   Strategy
   ========
   For every LOGIC0/LOGIC1 cell whose output drives more than
   [SV_DECOMP_TIE_FANOUT_MAX] sinks (default 16):

   1. Compute  N_extra = ceil(fanout / fanout_max) - 1.
   2. Mint N_extra additional tie cells, each driving its own fresh
      net (with Block_tag-encoded names so the split is traceable).
   3. Partition the sink connections evenly across the original +
      new nets and rewrite each sink's pin_conn.net.

   The split is purely electrical — no logic change.  Verifies
   trivially against itself (the constants are the same value);
   cert-gating not needed.                                          *)

open Lib_map

(* ── Config ──────────────────────────────────────────────────── *)

let max_fanout_default = 16

let max_fanout () =
  match Sys.getenv_opt "SV_DECOMP_TIE_FANOUT_MAX" with
  | Some s -> (try int_of_string s with _ -> max_fanout_default)
  | None -> max_fanout_default

let is_tie_cell name =
  let prefix p s =
    let pl = String.length p and sl = String.length s in
    sl >= pl && String.sub s 0 pl = p in
  prefix "LOGIC0" name || prefix "LOGIC1" name

(* ── Pass ────────────────────────────────────────────────────── *)

(* For each tie cell whose fanout exceeds [max], spawn extras and
   redistribute sinks.  Returns (rewritten netlist, split_count).  *)
let split_module (nl : netlist) : netlist * int =
  let max = max_fanout () in
  (* 1.  Locate all tie cells; record the net each drives. *)
  let tie_drivers : (string * instance) list =
    List.filter_map (fun inst ->
      if not (is_tie_cell inst.cell.cell_name) then None
      else
        match List.find_opt (fun c -> c.pin = inst.cell.out_pin)
                inst.conns with
        | Some c -> Some (c.net, inst)
        | None -> None
    ) nl.insts in
  if tie_drivers = [] then nl, 0
  else begin
    (* 2.  Count each tie net's load.  Two contributors:
         (a) gate input pins whose net IS the tie net directly; and
         (b) continuous assigns of the shape `wire = _tie_lo_` —
             after read_verilog merges those, every instance that
             references the LHS becomes a gate input on the tie
             net.  We count assigns conservatively as 1 sink each;
             accurate enough for the > N decision (we don't need
             the precise count, just "is it over threshold").  *)
    let load_table : (string, int) Hashtbl.t = Hashtbl.create 8 in
    List.iter (fun (net, _) -> Hashtbl.replace load_table net 0) tie_drivers;
    let bump net =
      if Hashtbl.mem load_table net then
        Hashtbl.replace load_table net (1 + Hashtbl.find load_table net)
    in
    List.iter (fun inst ->
      List.iter (fun c ->
        if c.pin <> inst.cell.out_pin then bump c.net
      ) inst.conns
    ) nl.insts;
    (* Count assigns whose RHS is exactly a tie net (the common case;
       multi-bit concats `{_tie_hi_, _tie_lo_, …}` we count once per
       occurrence — also conservative). *)
    List.iter (fun (_lhs, rhs) ->
      List.iter (fun (net, _) ->
        if rhs = net then bump net
        else if String.length net > 0 then begin
          (* substring search for {…, _tie_lo_, …} concat shape *)
          let nlen = String.length net in
          let rlen = String.length rhs in
          if rlen >= nlen then
            try
              let rec scan i =
                if i + nlen > rlen then ()
                else if String.sub rhs i nlen = net then bump net
                else scan (i + 1)
              in scan 0
            with _ -> ()
        end
      ) tie_drivers
    ) nl.assigns;
    if Sys.getenv_opt "SV_DECOMP_TIE_DEBUG" = Some "1" then
      Hashtbl.iter (fun net load ->
        Printf.eprintf "[tie_fanout DEBUG] net=%s load=%d max=%d\n"
          net load max) load_table;
    (* 3.  For each over-loaded tie, allocate extras and partition. *)
    let new_insts = ref [] in
    let new_wires = ref [] in
    let split_count = ref 0 in
    let net_remap : (string * int, string) Hashtbl.t =
      Hashtbl.create 16 in
    (* We'll walk the existing instances assigning each sink-pin a
       slot id (modulo the number of partitions for that net) and
       map (net, slot) → new_net.                                *)
    List.iter (fun (orig_net, orig_inst) ->
      let load = try Hashtbl.find load_table orig_net with Not_found -> 0 in
      if load > max then begin
        let n_partitions = (load + max - 1) / max in
        let n_extra = n_partitions - 1 in
        let bit_ctx = Printf.sprintf "tiefan_%s" orig_net in
        for k = 1 to n_extra do
          let new_net =
            if !Block_tag.current_modhash <> ""
            then Block_tag.mint_in_scope ~kind:Block_tag.AUX
                   ~signal:orig_net ~role:(Printf.sprintf "tie%d" k) ()
            else Lib_map.mint (Printf.sprintf "tie%d" k) in
          let new_inst_name =
            if !Block_tag.current_modhash <> ""
            then Block_tag.mint_in_scope ~kind:Block_tag.AUX
                   ~signal:orig_net
                   ~role:(Printf.sprintf "tie%d_%s" k orig_inst.cell.cell_name)
                   ()
            else Lib_map.mint orig_inst.cell.cell_name in
          ignore bit_ctx;
          new_wires := (new_net, 1) :: !new_wires;
          new_insts := { cell = orig_inst.cell;
                         inst_name = new_inst_name;
                         conns = [{ pin = orig_inst.cell.out_pin;
                                    net = new_net }] } :: !new_insts;
          Hashtbl.add net_remap (orig_net, k) new_net
        done;
        Hashtbl.add net_remap (orig_net, 0) orig_net;
        Hashtbl.replace load_table orig_net n_partitions;
        incr split_count
      end
    ) tie_drivers;
    (* 4.  Walk references — both inst sink pins AND continuous assign
       RHSs containing the tie net — and round-robin assign each one
       to a partition slot.  After this rewrite, the original
       _tie_lo_ has at most fanout_max sinks; the new _tie_lo_tieN
       cells share the rest.  *)
    let sink_counter : (string, int) Hashtbl.t = Hashtbl.create 16 in
    List.iter (fun (net, _) ->
      Hashtbl.replace sink_counter net 0
    ) tie_drivers;
    let pick_slot net =
      let n_part = try Hashtbl.find load_table net with Not_found -> 1 in
      let cur = Hashtbl.find sink_counter net in
      let slot = if n_part > 0 then cur mod n_part else 0 in
      Hashtbl.replace sink_counter net (cur + 1);
      if slot = 0 then net
      else
        try Hashtbl.find net_remap (net, slot)
        with Not_found -> net
    in
    let rewritten_insts =
      List.map (fun inst ->
        let conns' = List.map (fun c ->
          if c.pin = inst.cell.out_pin then c
          else if Hashtbl.mem sink_counter c.net then
            { c with net = pick_slot c.net }
          else c
        ) inst.conns in
        { inst with conns = conns' }
      ) nl.insts in
    let rewrite_one_occurrence rhs (orig_net, _) =
      (* Replace exactly one occurrence of orig_net in rhs with the
         partition-chosen net.  Done as a substring replace so concat
         expressions like `{_tie_hi_, _tie_lo_, _tie_lo_}` get each
         occurrence remapped independently across multiple calls.   *)
      if not (Hashtbl.mem sink_counter orig_net) then rhs
      else
        let nlen = String.length orig_net in
        let rlen = String.length rhs in
        let rec find i =
          if i + nlen > rlen then None
          else if String.sub rhs i nlen = orig_net then Some i
          else find (i + 1)
        in
        match find 0 with
        | None -> rhs
        | Some i ->
            let replacement = pick_slot orig_net in
            String.sub rhs 0 i
            ^ replacement
            ^ String.sub rhs (i + nlen) (rlen - i - nlen)
    in
    let rewritten_assigns =
      List.map (fun (lhs, rhs) ->
        let rec apply_once_per_net rhs = function
          | [] -> rhs
          | drv :: rest ->
              (* Apply repeatedly to handle multiple occurrences in
                 a concat — but only as many times as orig appears. *)
              let rec loop rhs =
                let rhs' = rewrite_one_occurrence rhs drv in
                if rhs' = rhs then rhs else loop rhs'
              in
              apply_once_per_net (loop rhs) rest in
        (lhs, apply_once_per_net rhs tie_drivers)
      ) nl.assigns in
    if !split_count > 0 then
      Printf.eprintf
        "[tie_fanout] split %d over-loaded tie net(s) (max fanout=%d) — \
         %d extra cell(s) emitted\n"
        !split_count max (List.length !new_insts);
    { nl with insts = rewritten_insts @ !new_insts;
              wires = nl.wires @ !new_wires;
              assigns = rewritten_assigns }, !split_count
  end
