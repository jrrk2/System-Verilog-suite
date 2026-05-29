(* EDIF -> Behavioral_ir, *structurally*.
 *
 * Sibling of Edif_to_behavioral.convert.  The latter decodes Xilinx
 * primitives (LUTk INIT -> mux tree, FDRE -> sequential process) into
 * behavioral code so that VHDL/SV-vs-EDIF equivalence checking can
 * compare logic.  This lowerer does the opposite: it keeps EVERY EDIF
 * instance as a binstance, with INIT (and other parameters) preserved
 * in `param_strs`.  Downstream tools (bir_to_nextpnr_json, behavioral
 * Z3 with cell semantics, …) consume the primitives directly.
 *
 * Use case: Vivado synth_design produces an EDIF of LUT6/FDRE/CARRY4/
 * BUFG primitives.  We want to feed that to nextpnr-xilinx's P&R via
 * the suite's existing yosys-JSON writer convention. *)

open Behavioral_ir
open Edif_parser

(* ── helpers ─────────────────────────────────────────────────────────── *)

(* Parse a possibly-bit-indexed signal name into (base, bit_index option). *)
let parse_signal_name (name : string) : string * int option =
  try
    let bracket_pos = String.rindex name '[' in
    let close_pos = String.rindex name ']' in
    if close_pos = String.length name - 1 then
      let base = String.sub name 0 bracket_pos in
      let idx = int_of_string (String.sub name (bracket_pos + 1)
                                 (close_pos - bracket_pos - 1)) in
      (base, Some idx)
    else (name, None)
  with _ -> (name, None)

(* Turn a flat net name into a bexpr referencing the signal of that name.  *)
let net_to_expr (net_name : string) : bexpr =
  let (base, idx_opt) = parse_signal_name net_name in
  match idx_opt with
  | Some idx -> BSelect { array = BVar base; index = BConst { value = idx; width = 32 } }
  | None     -> BVar base

(* btype for a signal: BInt with unsigned signedness; nextpnr-bound nets
   are all unsigned bit vectors regardless of width. *)
let btype_for_width w = BInt { width = w; signed = Unsigned }

(* Direction conversion *)
let dir_of_edif = function
  | Input  -> `Input
  | Output -> `Output
  | Inout  -> `Input  (* nextpnr-xilinx doesn't expect inouts in our flows *)

(* ── core lowerer ────────────────────────────────────────────────────── *)

let convert_cell (edif : edif_data) : bmodule =
  (* Per-instance pin lookup.  EDIF can name a pin either as a scalar
     (`(portref CI (instanceref X))` -> idx=None) or as a member of an
     array bus (`(portref (member S 0) ...)` -> idx=Some 0).  We keep
     ALL of them — multi-bit pins like CARRY4's S[3:0] need every bit.    *)
  let pin_entries : (string * string, (int option * string) list) Hashtbl.t =
    Hashtbl.create 1024
  in
  List.iter (fun (net : net_info) ->
    List.iter (fun (pin : net_pin) ->
      match pin.inst with
      | Some inst ->
        let k = (inst, pin.pin) in
        let cur = try Hashtbl.find pin_entries k with Not_found -> [] in
        Hashtbl.replace pin_entries k ((pin.index, net.name) :: cur)
      | None -> ()
    ) net.connections
  ) edif.nets;

  (* Top-level port signals.  Direction comes from the EDIF port table.   *)
  let port_signals = List.map (fun (p : port_info) ->
    { name = p.name;
      stype = btype_for_width p.width;
      direction = dir_of_edif p.direction;
      initial_value = None;
      attrs = [];
    }
  ) edif.ports in

  let port_names =
    List.fold_left (fun s (p : port_info) -> p.name :: s) [] edif.ports
  in
  let is_port nm = List.mem nm port_names in

  (* Internal nets: every net name not already a port becomes a 1-bit
     internal signal.  EDIF nets are scalar, so collect their bases:
     a bus net "led[2]" maps to a 1-bit signal "led[2]" (preserving the
     bracket so the JSON writer can keep them distinct).  We use a
     normalized scalar-net name as the signal name; downstream emission
     allocates one yosys-JSON net id per such signal. *)
  let net_names = ref [] in
  let seen = Hashtbl.create 4096 in
  List.iter (fun (n : net_info) ->
    if not (is_port n.name) && not (Hashtbl.mem seen n.name) then begin
      Hashtbl.add seen n.name ();
      net_names := n.name :: !net_names
    end
  ) edif.nets;
  let net_signals = List.rev_map (fun nm ->
    { name = nm;
      stype = BInt { width = 1; signed = Unsigned };
      direction = `Internal;
      initial_value = None;
      attrs = [];
    }) !net_names in

  let signals = port_signals @ List.rev net_signals in

  (* Build per-instance pin → bexpr.  For a vector pin EDIF gives us a
     list of `(member P k, net_k)`.  Sort by k ascending (LSB-first) and
     emit a BConcat so downstream consumers see a single multi-bit pin.   *)
  let instances = List.map (fun (i : instance_info) ->
    let pcs = ref [] in
    Hashtbl.iter (fun (inst, pin) entries ->
      if inst = i.name then begin
        let has_idx = List.exists (fun (idx, _) -> idx <> None) entries in
        if not has_idx then begin
          match entries with
          | [(_, nm)] -> pcs := (pin, net_to_expr nm) :: !pcs
          | _ ->
            (* multiple scalar refs to the same pin — shouldn't happen
               for a well-formed netlist, but be tolerant *)
            let nm = match List.hd entries with (_, n) -> n in
            pcs := (pin, net_to_expr nm) :: !pcs
        end else begin
          let sorted = List.sort (fun (a, _) (b, _) ->
            compare (match a with Some x -> x | None -> -1)
                    (match b with Some x -> x | None -> -1)) entries in
          let bits = List.map (fun (_, nm) -> net_to_expr nm) sorted in
          (* yosys-JSON connection lists are LSB-first; BConcat in the
             suite's convention puts MSB first.  Reverse so the JSON
             writer (which walks BConcat left-to-right) emits LSB-first. *)
          pcs := (pin, BConcat (List.rev bits)) :: !pcs
        end
      end
    ) pin_entries;
    let pcs = List.sort (fun (a,_) (b,_) -> String.compare a b) !pcs in
    let param_strs =
      match i.init with
      | Some s -> [("INIT", s)]
      | None   -> []
    in
    { inst_name    = i.name;
      module_name  = i.cell_type;
      param_values = [];
      param_strs;
      port_connections = pcs;
    }
  ) edif.instances in

  { name = edif.module_name;
    params = [];
    signals;
    processes = [];
    instances;
    funcs = [];
    mems  = [];
    attrs = [];
  }

(* ── top-level entry ────────────────────────────────────────────────── *)

let convert (filename : string) : bprogram =
  let content = Edif_parser.read_file filename in
  let all_cells = Edif_parser.parse_all_netlist_cells content in
  let top = Edif_parser.parse_schematic filename in
  (* Each EDIF cell becomes a bmodule; library cells (hdi_primitives) are
     declared via bprogram.library_cells so consumers know each
     primitive's port directions and widths. *)
  let modules = List.map convert_cell all_cells in
  let library_cells =
    Hashtbl.fold (fun cell_name (ports : port_info list) acc ->
      let lps = List.map (fun (p : port_info) ->
        { port_name      = p.name;
          port_direction = (match p.direction with
                            | Output -> `Output
                            | _      -> `Input);
          port_width     = p.width;
        }
      ) ports in
      (cell_name, lps) :: acc
    ) top.library_cells []
  in
  { modules; library_cells }
