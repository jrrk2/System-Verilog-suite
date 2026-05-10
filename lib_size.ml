(* Load-aware drive-strength selection (pre-placement).

   Runs after [Lib_map] / [Hier_synth] and before [Cell_verilog_emit].
   For every output net in every module, sum the input pin
   capacitances at all sinks plus an estimated wire cap, then pick the
   smallest Liberty cell variant of the same logical kind whose
   max_capacitance covers that load.

   Why pre-placement?  ORFS today receives a netlist in which every
   gate is the X1 variant; OpenROAD then has to re-size in repair_design
   / repair_timing under tight optimisation budgets.  Picosoc lands at
   WNS −6.9 ns / 10 ns clock — most of that is undriven high-fanout
   nets that should have started life on bigger gates.  Sending a
   pre-sized netlist gives the placer a head start and frees its
   optimisation budget for actual layout-driven repair.

   Pin layouts are identical across X-variants (AND2_X1 / AND2_X2 /
   AND2_X4 all expose A1, A2, ZN), so only the cell_name changes —
   the [Lib_map.instance.conns] list stays untouched.

   Wire-cap is the calibration knob; it's per-fanout and tunable via
   [SV_DECOMP_WIRE_CAP_FF].  The default rule of thumb (0.5 fF/pin
   for nangate45 short wires) intentionally errs slightly conservative
   — better to upsize unnecessarily than to ship under-driven nets.
   Calibration against measured ORFS QoR is task #109. *)

open Lib_map

(* ── Variant catalogue ─────────────────────────────────────────── *)

type variant = {
  drive    : int;                       (* X1 → 1, X2 → 2, … *)
  name     : string;                    (* full Liberty name *)
  max_cap  : float;                     (* cell-level max_capacitance, ff *)
  pin_caps : (string * float) list;     (* per-input-pin capacitance *)
  is_ff    : bool;                      (* skip during sizing for now *)
}

(* base name (X-suffix stripped) → variants ascending by drive *)
type catalogue = (string, variant list) Hashtbl.t

let drive_re = Str.regexp "_X\\([0-9]+\\)$"

let base_of name =
  try
    let i = Str.search_forward drive_re name 0 in
    String.sub name 0 i
  with Not_found -> name

let drive_of name =
  try
    let _ = Str.search_forward drive_re name 0 in
    int_of_string (Str.matched_group 1 name)
  with Not_found | Failure _ -> 1

(* ── Liberty extraction ────────────────────────────────────────── *)

let extract_one_cell name body : variant option =
  let max_cap = ref None in
  let pin_caps = ref [] in
  let is_ff = ref false in
  List.iter (function
    | Liberty_rewrite.CellPin (pin_name, attrs) ->
        let cap = ref None in
        let dir = ref None in
        let pin_max_cap = ref None in
        List.iter (function
          | Liberty_rewrite.Parameter ("capacitance", v) -> cap := Some v
          | Liberty_rewrite.Parameter ("max_capacitance", v) ->
              pin_max_cap := Some v
          | Liberty_rewrite.Direction d -> dir := Some d
          | _ -> ()) attrs;
        (* max_capacitance lives on the output pin in Nangate's lib —
           that's the largest C_load a single drive can sustain.  Some
           cells have multiple output pins (e.g. NAND/AOI variants);
           take the smallest max_cap across them since any output has
           to be able to drive its load. *)
        (match !pin_max_cap, !dir with
         | Some v, Some "output" ->
             max_cap := Some (match !max_cap with
                              | None -> v
                              | Some prev -> min prev v)
         | _ -> ());
        (match !cap, !dir with
         | Some c, Some "input" -> pin_caps := (pin_name, c) :: !pin_caps
         | _ -> ())
    | Liberty_rewrite.FlipFlop (_, _) | Liberty_rewrite.Latch (_, _) ->
        is_ff := true
    | _ -> ()) body;
  (* Skip cells without a max_cap or input pins — physical-only or
     macros (LOGIC0/1, FILLCELL, ANTENNA, sram/fakeram) shouldn't
     enter the variant table. *)
  match !max_cap, !pin_caps with
  | Some m, (_::_) ->
      Some { drive = drive_of name;
             name;
             max_cap = m;
             pin_caps = List.rev !pin_caps;
             is_ff = !is_ff }
  | _ -> None

let load_catalogue path : catalogue =
  let lib_root, _hash = Liberty_rewrite.rewrite path in
  let cat : catalogue = Hashtbl.create 256 in
  let cells_iter = function
    | Liberty_rewrite.Library (_, items) ->
        List.iter (function
          | Liberty_rewrite.LibCell (n, body) ->
              (match extract_one_cell n body with
               | Some v ->
                   let bn = base_of n in
                   let prev =
                     try Hashtbl.find cat bn with Not_found -> [] in
                   Hashtbl.replace cat bn (v :: prev)
               | None -> ())
          | _ -> ()) items
    | _ -> () in
  cells_iter lib_root;
  (* Sort each variant family ascending by drive strength. *)
  Hashtbl.iter (fun bn vs ->
    Hashtbl.replace cat bn
      (List.sort (fun a b -> compare a.drive b.drive) vs)
  ) cat;
  cat

(* ── Load lookup helpers ───────────────────────────────────────── *)

let pin_cap_in_cat cat full_cell_name pin_name : float option =
  let bn = base_of full_cell_name in
  match Hashtbl.find_opt cat bn with
  | None -> None
  | Some vs ->
      (match List.find_opt (fun v -> v.name = full_cell_name) vs with
       | None -> None
       | Some v -> List.assoc_opt pin_name v.pin_caps)

let pick_for_load cat base_name load : variant option =
  match Hashtbl.find_opt cat base_name with
  | None | Some [] -> None
  | Some variants ->
      (match List.find_opt (fun v -> v.max_cap >= load) variants with
       | Some v -> Some v
       | None ->
           (* Past the largest variant — return it and let OpenROAD
              decide what to do (will probably need a buffer tree).  *)
           Some (List.nth variants (List.length variants - 1)))

(* ── Sizing pass ───────────────────────────────────────────────── *)

(* Wire-cap per fanout pin, in fF.  Nangate45 uses capacitive_load_unit
   = (1, ff), and AND2_X1 max_cap is 60.58 fF; a typical short wire to
   each fanout pin adds ~0.5 fF on average for 45-nm chip-scale.
   Calibrate against measured ORFS QoR (#109).  *)
let wire_cap_default = 0.5  (* fF per fanout pin *)

let wire_cap_ff () =
  match Sys.getenv_opt "SV_DECOMP_WIRE_CAP_FF" with
  | Some s -> (try float_of_string s with _ -> wire_cap_default)
  | None -> wire_cap_default

(* Returns (resized_netlist, change_count). *)
let resize_module ~cat (nl : netlist) : netlist * int =
  let wire_cap = wire_cap_ff () in
  (* fanout sinks: net → (driver_cell_name, sink_pin_name) list *)
  let build_sinks insts =
    let h : (string, (string * string) list) Hashtbl.t =
      Hashtbl.create 1024 in
    List.iter (fun (inst : instance) ->
      List.iter (fun (c : pin_conn) ->
        if c.pin <> inst.cell.out_pin then begin
          let prev = try Hashtbl.find h c.net with Not_found -> [] in
          Hashtbl.replace h c.net ((inst.cell.cell_name, c.pin) :: prev)
        end
      ) inst.conns
    ) insts;
    h in
  let net_load sinks net =
    let pieces = try Hashtbl.find sinks net with Not_found -> [] in
    let pin_load = List.fold_left (fun acc (cell, pin) ->
      acc +. (match pin_cap_in_cat cat cell pin with
              | Some v -> v | None -> 0.0)
    ) 0.0 pieces in
    pin_load +. wire_cap *. float_of_int (List.length pieces)
  in
  let one_pass insts =
    let sinks = build_sinks insts in
    let changed = ref 0 in
    let next = List.map (fun (inst : instance) ->
      (* Skip cells without a known base (tie cells, macros). *)
      let bn = base_of inst.cell.cell_name in
      match Hashtbl.find_opt cat bn with
      | None | Some [] -> inst
      | Some _ ->
          (* FFs are not sized in this pass — Q drives many things
             and we'd rather see the net upsized through buffers. *)
          let is_ff =
            match Hashtbl.find_opt cat bn with
            | Some (v :: _) -> v.is_ff
            | _ -> false in
          if is_ff then inst
          else
            let out_conn =
              List.find_opt (fun c -> c.pin = inst.cell.out_pin)
                inst.conns in
            (match out_conn with
             | None -> inst
             | Some c ->
                 let load = net_load sinks c.net in
                 (match pick_for_load cat bn load with
                  | Some v when v.name <> inst.cell.cell_name ->
                      incr changed;
                      { inst with
                        cell = { inst.cell with cell_name = v.name } }
                  | _ -> inst))
    ) insts in
    next, !changed
  in
  (* Iterate to fixpoint: upsizing a sink raises its pin cap, which
     can require upsizing the upstream driver too.  Three passes
     converges in practice (verified on picosoc). *)
  let rec loop iter prev_total prev =
    if iter >= 5 then prev, prev_total
    else
      let next, ch = one_pass prev in
      if ch = 0 then next, prev_total
      else loop (iter + 1) (prev_total + ch) next
  in
  let final, total = loop 0 0 nl.insts in
  { nl with insts = final }, total

(* ── Convenience: catalogue-once + resize-many ─────────────────── *)

let cached_catalogue : catalogue option ref = ref None

let catalogue_for_path path =
  match !cached_catalogue with
  | Some c -> c
  | None ->
      let c = load_catalogue path in
      cached_catalogue := Some c;
      c

(* Default Liberty for nangate45 if none specified.  Match what ORFS
   uses for the picosoc demo. *)
let default_liberty_paths = [
  "/home/jonathan/OpenROAD-flow-scripts/flow/platforms/nangate45/lib/NangateOpenCellLibrary_typical.lib";
]

let liberty_path_or_default () =
  match Sys.getenv_opt "SV_DECOMP_LIBERTY" with
  | Some p when Sys.file_exists p -> Some p
  | _ ->
      List.find_opt Sys.file_exists default_liberty_paths
