(* Scan-chain insertion (Phase A of the DFT track).

   For every flip-flop instance whose cell has a same-footprint scan
   variant in the platform Liberty (`DFF_X1` → `SDFF_X1`, `DFFR_X1` →
   `SDFFR_X1`, …), swap the cell type and add two extra pin connections:

     SE — scan enable, tied to a new top-level input
     SI — scan input, tied to the previous FF's Q (or to a new top-level
          scan_in pin for the first FF in the chain)

   The last FF's Q is exposed as a new top-level output `scan_out`.

   Functionally invisible at the design's top-level interface when
   scan_en=0: the scan FF behaves exactly like the original DFF.  With
   scan_en=1, the chain shifts one bit per posedge — N cycles to load
   N bits, one capture cycle with scan_en=0, then N cycles to shift out.

   Stage 1 keeps things simple: single chain in emission order across
   every FF in the module.  N-parallel chains are a later config knob
   (`SV_DECOMP_SCAN_CHAINS=N`).

   Hooked into [synth_pipeline] between lib_size and tie_fanout — Lib_size
   sizes the SDFF cells using the same Liberty pin-cap LUTs, and
   Tie_fanout treats them like any other DFF.                         *)

open Lib_map

(* Map functional DFF → scan equivalent in Nangate45.  Other libraries
   (sky130hd, asap7) have analogous variants; the table extends easily.
   Returning None means "no scan variant available — leave alone". *)
let scan_variant = function
  | "DFF_X1"    -> Some "SDFF_X1"
  | "DFF_X2"    -> Some "SDFF_X2"
  | "DFFR_X1"   -> Some "SDFFR_X1"
  | "DFFR_X2"   -> Some "SDFFR_X2"
  | "DFFS_X1"   -> Some "SDFFS_X1"
  | "DFFS_X2"   -> Some "SDFFS_X2"
  | "DFFRS_X1"  -> Some "SDFFRS_X1"
  | "DFFRS_X2"  -> Some "SDFFRS_X2"
  | _ -> None

let is_scannable_dff cell_name = Option.is_some (scan_variant cell_name)

(* Q-net of an instance.  Falls back to "" — only reached if Hier_synth
   somehow emitted a DFF with no Q connection, which would mean an
   upstream bug; we degrade gracefully rather than fail.              *)
let q_net (i : instance) : string =
  match List.find_opt (fun c -> c.pin = "Q") i.conns with
  | Some c -> c.net
  | None -> ""

(* Public entry — converts a single module's netlist.  Returns
   [(rewritten netlist, FF count stitched)]. *)
let scan_module
    ?(scan_en_name="scan_en")
    ?(scan_in_name="scan_in")
    ?(scan_out_name="scan_out")
    (nl : netlist) : netlist * int =
  let ffs =
    List.filter (fun (i : instance) ->
      is_scannable_dff i.cell.cell_name) nl.insts in
  let n = List.length ffs in
  if n = 0 then nl, 0
  else begin
    (* Index FFs by inst_name so we can recover position in O(1). *)
    let idx_of : (string, int) Hashtbl.t = Hashtbl.create n in
    List.iteri (fun i ff -> Hashtbl.add idx_of ff.inst_name i) ffs;
    let ff_array = Array.of_list ffs in

    (* SI of FF[k] = (k=0 → scan_in pin) else Q-net of FF[k-1].  Holds
       even if intermediate FFs use the same Q-net as something else —
       fanout is fine. *)
    let si_of k =
      if k = 0 then scan_in_name
      else q_net ff_array.(k - 1) in

    let new_insts =
      List.map (fun (i : instance) ->
        match Hashtbl.find_opt idx_of i.inst_name with
        | None -> i
        | Some k ->
            let scan_name = Option.get (scan_variant i.cell.cell_name) in
            (* in_pins on the cell record drives [Cell_verilog_emit]'s
               port-order rendering.  Append SE+SI so they emit after
               the original pins — purely cosmetic, matches what most
               scan-aware libraries' docs show. *)
            let new_in_pins = i.cell.in_pins @ ["SE"; "SI"] in
            let new_cell =
              { i.cell with cell_name = scan_name; in_pins = new_in_pins } in
            let new_conns = i.conns @ [
              { pin = "SE"; net = scan_en_name };
              { pin = "SI"; net = si_of k };
            ] in
            { cell = new_cell; inst_name = i.inst_name; conns = new_conns }
      ) nl.insts in

    (* scan_out = Q of the last FF, exposed as a top-level output via
       a continuous assign so the original Q net can keep all its
       other consumers. *)
    let scan_out_src = q_net ff_array.(n - 1) in
    let new_inputs  = nl.inputs  @ [(scan_en_name, 1); (scan_in_name, 1)] in
    let new_outputs = nl.outputs @ [(scan_out_name, 1)] in
    let new_assigns = nl.assigns @ [(scan_out_name, scan_out_src)] in
    { nl with
      inputs  = new_inputs;
      outputs = new_outputs;
      insts   = new_insts;
      assigns = new_assigns },
    n
  end

(* Env-gated entry for synth_pipeline.  Skips when SV_DECOMP_SCAN is
   anything other than "1".                                          *)
let enabled () = Sys.getenv_opt "SV_DECOMP_SCAN" = Some "1"
