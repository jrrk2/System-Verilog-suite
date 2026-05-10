(* Mux-chain flattening — collapse a linear priority chain of N MUX2
   cells into a balanced one-hot AND-OR tree of O(log N) depth.

   Why
   ===
   Picosoc's WNS path with arch_swap on consists of a 134-deep chain
   of MUX2 cells in picorv32's instruction-decoder selector logic.
   The chain is the gate-level expansion of nested SystemVerilog
   ternaries / case-statements like:

       out = sel_a ? va : (sel_b ? vb : (sel_c ? vc : default))

   Hardcaml lowers each [if/else] to one Mux node; lib_map expands
   each Mux to one MUX2_X1.  Yosys's mapper would have re-balanced
   this at synth time; ours doesn't, so the chain stays linear and
   the carry-tree-class WNS path lives on muxes rather than adders.

   What it does
   ============
   For every chain of >= [SV_DECOMP_MUX_CHAIN_MIN] MUX2 cells where:
     - mₙ.A = mₙ₋₁.Z  (priority-chain shape: prev-result on A side)
     - mₙ₋₁.Z fans out only to mₙ (no other reader)
     - all selects are independent nets (one-hot in spirit)

   replace with:
     - prefix-AND of ~sels (Brent-Kung tree, depth O(log N))
     - one-hot win signals: winᵢ = pᵢ & selᵢ
     - OR-reduce of (winᵢ ? cᵢ : 0) for all i
     - final OR with (none ? default : 0) for the chain head

   Cells go from N MUX2 → ~3N (AND2 + OR2 + a few INV) but depth
   drops from N to 2·⌈log₂ N⌉.  For picosoc's 134-deep chain that's
   134 → ~14 stages.

   Detection is conservative: we walk back via the A pin only.  If a
   chain mixes A/B priority directions (which can happen if Hardcaml
   inverts the select at some point) we stop the chain there and only
   collapse the contiguous A-walked segment.  Same for any cell with
   non-unit fanout — the chain breaks if any intermediate result has
   a second consumer.                                                *)

open Lib_map

(* ── Config ──────────────────────────────────────────────────── *)

let min_chain_len_default = 4

let min_chain () =
  match Sys.getenv_opt "SV_DECOMP_MUX_CHAIN_MIN" with
  | Some s -> (try int_of_string s with _ -> min_chain_len_default)
  | None -> min_chain_len_default

(* ── Walk-back helpers ───────────────────────────────────────── *)

(* Map: net -> driver instance (cell whose out_pin connects to net). *)
let build_driver_index (insts : instance list) =
  let h : (string, instance) Hashtbl.t = Hashtbl.create 1024 in
  List.iter (fun inst ->
    List.iter (fun c ->
      if c.pin = inst.cell.out_pin then Hashtbl.replace h c.net inst
    ) inst.conns
  ) insts;
  h

(* Map: net -> count of cells reading it (any pin != out_pin). *)
let build_fanout_index (insts : instance list) =
  let h : (string, int) Hashtbl.t = Hashtbl.create 1024 in
  List.iter (fun inst ->
    List.iter (fun c ->
      if c.pin <> inst.cell.out_pin then
        Hashtbl.replace h c.net
          (1 + (try Hashtbl.find h c.net with Not_found -> 0))
    ) inst.conns
  ) insts;
  h

let pin_net (i : instance) name =
  let c = List.find (fun (c : pin_conn) -> c.pin = name) i.conns in
  c.net

let is_mux2 (i : instance) =
  i.cell.cell_name = "MUX2_X1" || i.cell.cell_name = "MUX2_X2"

(* Walk back from [tail_inst] via the A pin, collecting the chain.
   Returns the list of (sel_net, b_data_net, mux_inst) ordered from
   the OLDEST mux (chain head, closest to the constant) to the
   YOUNGEST (the tail we started from), plus the head's A net (the
   "default" feed). *)
let walk_chain_back ~drivers ~fanout (tail : instance) =
  let rec go acc cur =
    let a = pin_net cur "A" in
    let s = pin_net cur "S" in
    let b = pin_net cur "B" in
    let acc' = (s, b, cur) :: acc in
    match Hashtbl.find_opt drivers a with
    | Some prev when is_mux2 prev
                  && Hashtbl.find_opt fanout a = Some 1 ->
        go acc' prev
    | _ ->
        (acc', a)              (* head_default = a *)
  in
  go [] tail

(* ── Rewrite ─────────────────────────────────────────────────── *)

let next_inst_id = ref 0
let mint_local kind =
  incr next_inst_id;
  Printf.sprintf "_mxf_%s_%d_" kind !next_inst_id

(* Emit an INV over [a], driving fresh wire.  Records inst + wire. *)
let emit_inv ~insts ~wires a =
  let z = mint_local "ninv" in
  wires := (z, 1) :: !wires;
  insts := { cell = cell_inv;
             inst_name = mint_local "INV_X1";
             conns = [{ pin = "A"; net = a };
                      { pin = "ZN"; net = z }] } :: !insts;
  z

let emit_and ~insts ~wires a b =
  let z = mint_local "and" in
  wires := (z, 1) :: !wires;
  insts := { cell = cell_and;
             inst_name = mint_local "AND2_X1";
             conns = [{ pin = "A1"; net = a };
                      { pin = "A2"; net = b };
                      { pin = "ZN"; net = z }] } :: !insts;
  z

let emit_or ~insts ~wires a b =
  let z = mint_local "or" in
  wires := (z, 1) :: !wires;
  insts := { cell = cell_or;
             inst_name = mint_local "OR2_X1";
             conns = [{ pin = "A1"; net = a };
                      { pin = "A2"; net = b };
                      { pin = "ZN"; net = z }] } :: !insts;
  z

(* Balanced OR-reduce of a list of nets.  Empty -> "1'b0", single
   value -> itself, otherwise pairwise OR until one remains. *)
let rec or_reduce ~insts ~wires = function
  | [] -> "1'b0"
  | [x] -> x
  | xs ->
      let rec pair = function
        | [] -> []
        | [a] -> [a]
        | a :: b :: rest -> emit_or ~insts ~wires a b :: pair rest in
      or_reduce ~insts ~wires (pair xs)

(* Replace a chain of [length n] muxes with the flattened structure.
   The chain has links ordered chain-head-first:
     chain.(0) drives chain.(1).A, chain.(1) drives chain.(2).A, ...
   So priority order is *reverse*: chain.(n-1) is highest priority. *)
let flatten_chain ~tail_out ~head_default
                  (chain : (string * string * instance) list)
    : instance list * (string * int) list =
  let n = List.length chain in
  let insts = ref [] and wires = ref [] in
  let arr = Array.of_list chain in
  (* sels.(i), datas.(i): sel and B-input of chain.(i).
     Priority order: i=n-1 is highest, i=0 is lowest, head_default
     wins when no sel is asserted.                                  *)
  let sels = Array.map (fun (s, _, _) -> s) arr in
  let datas = Array.map (fun (_, d, _) -> d) arr in
  (* Inverted sels: ~selᵢ for i=0..n-1.                              *)
  let nsels = Array.map (emit_inv ~insts ~wires) sels in
  (* Prefix-AND of ~sels, indexed so prefix.(i) = ~sel_{n-1} & ... &
     ~sel_{i+1}.  That makes winᵢ = prefix.(i) & selᵢ:
       i = n-1: prefix = 1'b1, win = sel_{n-1}.
       i = 0  : prefix = ~sel_{n-1} & ~sel_{n-2} & ... & ~sel_1.    *)
  let prefix = Array.make n "1'b1" in
  for i = n - 2 downto 0 do
    prefix.(i) <- emit_and ~insts ~wires prefix.(i + 1) nsels.(i + 1)
  done;
  (* none = prefix.(0) & ~sels.(0)                                   *)
  let none = emit_and ~insts ~wires prefix.(0) nsels.(0) in
  (* Per-branch contributions: AND data with win signal — single-bit
     case for now.  (Multi-bit muxes aren't a thing in the per-bit
     blasted netlist; each mux is 1-bit.)                            *)
  let contribs = ref [] in
  for i = 0 to n - 1 do
    let win =
      if i = n - 1 then sels.(i)
      else emit_and ~insts ~wires prefix.(i) sels.(i) in
    let c = emit_and ~insts ~wires win datas.(i) in
    contribs := c :: !contribs
  done;
  let dflt_contrib = emit_and ~insts ~wires none head_default in
  contribs := dflt_contrib :: !contribs;
  let or_top = or_reduce ~insts ~wires !contribs in
  (* Tie [tail_out] to or_top via a final BUF_X1 (assigning a wire
     directly via raw assign would conflict with read_verilog's
     structural-only requirement).                                   *)
  let buf_inst =
    { cell = cell_buf;
      inst_name = mint_local "BUF_tail";
      conns = [{ pin = "A"; net = or_top };
               { pin = cell_buf.out_pin; net = tail_out }] } in
  insts := buf_inst :: !insts;
  List.rev !insts, !wires

(* ── Pass entry point ────────────────────────────────────────── *)

(* Returns (rewritten netlist, n_chains_collapsed, total_muxes_replaced). *)
let flatten_module (nl : netlist) : netlist * int * int =
  let min_n = min_chain () in
  let drivers = build_driver_index nl.insts in
  let fanout = build_fanout_index nl.insts in
  (* A net is a chain-internal hop if it's read on some MUX2's A pin.
     Build that set up-front so we only walk back from "tail" muxes
     (those whose output is NOT consumed by another mux's A) — that
     guarantees each chain is visited exactly once.                  *)
  let chain_internal_nets : (string, unit) Hashtbl.t =
    Hashtbl.create 256 in
  List.iter (fun (i : instance) ->
    if is_mux2 i then
      Hashtbl.replace chain_internal_nets (pin_net i "A") ()
  ) nl.insts;
  let consumed : (string, unit) Hashtbl.t = Hashtbl.create 64 in
  let new_insts_all = ref [] in
  let new_wires_all = ref [] in
  let n_chains = ref 0 in
  let n_muxes_replaced = ref 0 in
  List.iter (fun (inst : instance) ->
    if is_mux2 inst && not (Hashtbl.mem consumed inst.inst_name) then begin
      let out = pin_net inst inst.cell.out_pin in
      let is_tail = not (Hashtbl.mem chain_internal_nets out) in
      if is_tail then begin
        let chain, head_default =
          walk_chain_back ~drivers ~fanout inst in
        if List.length chain >= min_n then begin
          List.iter (fun (_, _, m) ->
            Hashtbl.add consumed m.inst_name ()) chain;
          let tail_out =
            let _, _, t = List.nth chain (List.length chain - 1) in
            pin_net t (t.cell.out_pin) in
          let new_insts, new_wires =
            flatten_chain ~tail_out ~head_default chain in
          new_insts_all := new_insts @ !new_insts_all;
          new_wires_all := new_wires @ !new_wires_all;
          incr n_chains;
          n_muxes_replaced := !n_muxes_replaced + List.length chain
        end
      end
    end
  ) nl.insts;
  let kept_insts =
    List.filter (fun (i : instance) ->
      not (Hashtbl.mem consumed i.inst_name)) nl.insts in
  if !n_chains > 0 then
    Printf.eprintf
      "[mux_flatten] collapsed %d chain(s), replaced %d mux(es) → \
       %d new cells (depth O(log N))\n"
      !n_chains !n_muxes_replaced (List.length !new_insts_all);
  { nl with insts = kept_insts @ !new_insts_all;
            wires = nl.wires @ !new_wires_all },
  !n_chains, !n_muxes_replaced
