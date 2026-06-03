(* Direct BIR -> yosys-JSON for nextpnr-xilinx.
 *
 * Sibling of fpga_synth/fpga_emit.write_yosys_json (which walks a
 * hardcaml `Circuit.t`) — but takes a `bmodule` containing only
 * `binstance`s.  No hardcaml, no AIG, no LUT cover.  Used by the
 * EDIF-frontend flow where the input is already a structural netlist
 * of Xilinx primitives (LUT6, FDRE, CARRY4, BUFG, …).
 *
 * The JSON format matches yosys's `write_json`, which is what
 * nextpnr-xilinx consumes.  See fpga_synth/fpga_emit.ml for the
 * canonical shape.  Net ids start at 2; "0" / "1" are reserved by
 * yosys for the two constant drivers. *)

open Behavioral_ir

(* A net bit in JSON is either an integer id or a constant string "0"/"1". *)
type bit = I of int | C of string

let bit_json = function I i -> `Int i | C s -> `String s
let bits_json lst = `List (List.map bit_json lst)

(* Map a Vivado-style constant net name to its yosys-JSON constant. *)
let const_of_name = function
  | "<const0>" | "GND" -> Some "0"
  | "<const1>" | "VCC" -> Some "1"
  | _                  -> None

(* Net key — what we put in the (signal_base, bit) -> id table.        *)
type net_key = { base : string; bit : int }

(* Net id allocator.  Constants short-circuit to C "0"/"1".            *)
type ctx = {
  next_id     : int ref;
  ids         : (net_key, int) Hashtbl.t;
  net_names   : (string, int list) Hashtbl.t;     (* base -> bit ids, in bit-order *)
  widths      : (string, int) Hashtbl.t;          (* signal-name -> declared width *)
}

let mk_ctx () = {
  next_id   = ref 2;
  ids       = Hashtbl.create 4096;
  net_names = Hashtbl.create 1024;
  widths    = Hashtbl.create 1024;
}

(* Populate the widths table from the bmodule's signal declarations.
 * Lets bits_of_conn expand a bare `BVar name` reference into its full
 * vector of bits when name is a multi-bit signal — without this,
 * CARRY4.S = BVar "_671" expanded to a single bit instead of four,
 * which left nextpnr-xilinx's carry packer accessing past-the-end of
 * the bit list and segfaulting. *)
let populate_widths ctx (m : bmodule) =
  List.iter (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BBool             -> 1
      | _                 -> 1 in
    Hashtbl.replace ctx.widths s.name w
  ) m.signals

let alloc ctx (key : net_key) : int =
  match Hashtbl.find_opt ctx.ids key with
  | Some i -> i
  | None   ->
    let i = !(ctx.next_id) in
    ctx.next_id := i + 1;
    Hashtbl.add ctx.ids key i;
    let cur = try Hashtbl.find ctx.net_names key.base with Not_found -> [] in
    Hashtbl.replace ctx.net_names key.base (cur @ [i]);
    i

(* Reduce a bexpr-as-net-reference to a single (name, bit_index option). *)
let rec resolve_bit_ref (e : bexpr) : (string * int option) option =
  match e with
  | BVar nm -> Some (nm, None)
  | BSelect { array = BVar nm; index = BConst { value; _ } } ->
      Some (nm, Some value)
  | BConcat _ ->
      (* Vivado EDIF doesn't emit concatenations on instance pins. *)
      None
  | _ -> None

(* Resolve a port connection's bexpr to its list of net bits.  Scalar
   pins -> 1-element list; CARRY4 S[3:0] -> 4-element list (LSB first). *)
let rec bits_of_conn ctx (e : bexpr) : bit list =
  match e with
  | BConst { value; width } ->
      (* Verilog literal on an instance pin (e.g. `.CEB(1'b0)` on
         IBUFDS_GTE2).  Emit one `C "0"` / `C "1"` per bit, LSB first. *)
      let rec range i = if i >= width then [] else
        let b = (value lsr i) land 1 in
        C (if b = 1 then "1" else "0") :: range (i + 1) in
      range 0
  | BConcat es ->
      (* BIR's BConcat is MSB-first by convention; reverse to get the
         LSB-first order yosys-JSON `bits` lists use. *)
      List.concat_map (bits_of_conn ctx) (List.rev es)
  | BSlice { signal; msb; lsb } ->
      (* Expand signal[msb:lsb] as the LSB-first list of single-bit refs
         signal[lsb], signal[lsb+1], ..., signal[msb]. *)
      let base = match signal with
        | BVar nm -> nm
        | _ -> failwith ("bir_to_nextpnr_json: slice of non-BVar signal: "
                         ^ Behavioral_ir.string_of_bexpr signal)
      in
      let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi in
      List.map (fun i ->
        match const_of_name base with
        | Some s -> C s
        | None -> I (alloc ctx { base; bit = i })
      ) (range lsb msb)
  | _ ->
      (match resolve_bit_ref e with
        | Some (nm, _) when const_of_name nm <> None ->
            [C (match const_of_name nm with Some s -> s | None -> assert false)]
        | Some (nm, Some bit) -> [I (alloc ctx { base = nm; bit })]
        | Some (nm, None    ) ->
            (* Bare `BVar nm` reference — expand to nm's declared
             * width if known, else fall back to one bit.  Multi-bit
             * expansion is essential for CARRY4 inputs like .S(_671)
             * where _671 is a 4-bit wire. *)
            let w = try Hashtbl.find ctx.widths nm with Not_found -> 1 in
            List.init w (fun i -> I (alloc ctx { base = nm; bit = i }))
        | None ->
            failwith ("bir_to_nextpnr_json: unsupported pin expression: "
                      ^ Behavioral_ir.string_of_bexpr e))

(* Convert a Verilog literal like "64'h12AB" / "1'b0" / "2'h1" to the
   raw binary string yosys-JSON expects (MSB-first, length = stated width).
   Falls back to the input verbatim if it doesn't match.                  *)
let verilog_lit_to_binary (s : string) : string =
  let s = String.trim s in
  match String.index_opt s '\'' with
  | None -> s
  | Some apos ->
      let width = try int_of_string (String.sub s 0 apos) with _ -> -1 in
      if width <= 0 then s
      else
        let base = if apos + 1 < String.length s then s.[apos + 1] else 'h' in
        let digits =
          if apos + 2 < String.length s
          then String.sub s (apos + 2) (String.length s - apos - 2)
          else "" in
        let buf = Buffer.create width in
        (match Char.lowercase_ascii base with
         | 'b' ->
             String.iter (fun c -> if c = '0' || c = '1' then Buffer.add_char buf c)
               digits
         | 'h' | 'x' ->
             String.iter (fun c ->
               let n = match c with
                 | '0'..'9' -> Some (Char.code c - Char.code '0')
                 | 'a'..'f' -> Some (Char.code c - Char.code 'a' + 10)
                 | 'A'..'F' -> Some (Char.code c - Char.code 'A' + 10)
                 | '_'      -> None
                 | _        -> None in
               match n with
               | Some n ->
                 for i = 3 downto 0 do
                   Buffer.add_char buf (if (n lsr i) land 1 = 1 then '1' else '0')
                 done
               | None -> ()) digits
         | 'd' ->
             let n = try int_of_string digits with _ -> 0 in
             for i = width - 1 downto 0 do
               Buffer.add_char buf (if (n lsr i) land 1 = 1 then '1' else '0')
             done
         | 'o' ->
             String.iter (fun c ->
               match c with
               | '0'..'7' ->
                 let n = Char.code c - Char.code '0' in
                 for i = 2 downto 0 do
                   Buffer.add_char buf (if (n lsr i) land 1 = 1 then '1' else '0')
                 done
               | _ -> ()) digits
         | _ -> Buffer.add_string buf digits);
        let bits = Buffer.contents buf in
        let n = String.length bits in
        if n = width then bits
        else if n > width then
          (* trim from the LEFT (drop leading zeros / over-padded hex)    *)
          String.sub bits (n - width) width
        else
          (* pad on the LEFT with zeros to reach the width                *)
          String.make (width - n) '0' ^ bits

(* Hard-coded port directions for Xilinx 7-series primitives the recipes
 * commonly encounter — used as a fallback when library_cells doesn't
 * supply them (Verible doesn't always know about vendor primitives).
 * Without this, nextpnr-xilinx asserts during pack with
 *   old.type == rep.type
 * because every port defaulted to PORT_IN — the output ports of CARRY4,
 * BUFG, IBUFDS, etc. then mismatch nextpnr's silicon-truth.
 *
 * Keep this list ordered to match the prjxray/openXC7 primitive set.
 * For composite cells (CARRY4) the output ports come first below for
 * grep-ability. *)
let xil_primitive_ports : (string * (string * [`Input|`Output]) list) list = [
  "LUT1",    [ "O", `Output; "I0", `Input ];
  "LUT2",    [ "O", `Output; "I0", `Input; "I1", `Input ];
  "LUT3",    [ "O", `Output; "I0", `Input; "I1", `Input; "I2", `Input ];
  "LUT4",    [ "O", `Output; "I0", `Input; "I1", `Input; "I2", `Input; "I3", `Input ];
  "LUT5",    [ "O", `Output; "I0", `Input; "I1", `Input; "I2", `Input; "I3", `Input; "I4", `Input ];
  "LUT6",    [ "O", `Output; "I0", `Input; "I1", `Input; "I2", `Input; "I3", `Input; "I4", `Input; "I5", `Input ];
  "FDRE",    [ "Q", `Output; "C", `Input; "CE", `Input; "D", `Input; "R", `Input ];
  "FDCE",    [ "Q", `Output; "C", `Input; "CE", `Input; "D", `Input; "CLR", `Input ];
  "FDPE",    [ "Q", `Output; "C", `Input; "CE", `Input; "D", `Input; "PRE", `Input ];
  "FDSE",    [ "Q", `Output; "C", `Input; "CE", `Input; "D", `Input; "S", `Input ];
  "CARRY4",  [ "CO", `Output; "O", `Output;
               "CYINIT", `Input; "CI", `Input; "DI", `Input; "S", `Input ];
  "MUXF7",   [ "O", `Output; "I0", `Input; "I1", `Input; "S", `Input ];
  "MUXF8",   [ "O", `Output; "I0", `Input; "I1", `Input; "S", `Input ];
  "BUFG",    [ "O", `Output; "I", `Input ];
  "BUFGCTRL",[ "O", `Output; "I0", `Input; "I1", `Input; "S0", `Input;
               "S1", `Input; "CE0", `Input; "CE1", `Input;
               "IGNORE0", `Input; "IGNORE1", `Input ];
  "BUFH",    [ "O", `Output; "I", `Input ];
  "BUFHCE",  [ "O", `Output; "I", `Input; "CE", `Input ];
  "BUFR",    [ "O", `Output; "I", `Input; "CE", `Input; "CLR", `Input ];
  "BUFIO",   [ "O", `Output; "I", `Input ];
  "IBUF",    [ "O", `Output; "I", `Input ];
  "OBUF",    [ "O", `Output; "I", `Input ];
  "IBUFDS",  [ "O", `Output; "I", `Input; "IB", `Input ];
  "OBUFDS",  [ "O", `Output; "OB", `Output; "I", `Input ];
  "IBUFDS_GTE2", [ "O", `Output; "ODIV2", `Output;
                   "I", `Input; "IB", `Input; "CEB", `Input ];
  "INV",     [ "O", `Output; "I", `Input ];
  "GND",     [ "G", `Output ];
  "VCC",     [ "P", `Output ];
]

(* port_direction lookup table built from bprogram.library_cells, plus
 * the Xilinx primitive baseline so output ports stay output even when
 * Verible parses the wrapper without primitive-cell port-direction
 * information. *)
let port_dir_table (cells : (string * library_port list) list)
    : (string * string, [`Input|`Output]) Hashtbl.t =
  let t = Hashtbl.create 256 in
  (* Seed with Xilinx primitives first so library_cells (when present)
   * still wins via Hashtbl.replace. *)
  List.iter (fun (cname, ports) ->
    List.iter (fun (port_name, dir) ->
      Hashtbl.replace t (cname, port_name) dir) ports
  ) xil_primitive_ports;
  List.iter (fun (cname, ports) ->
    List.iter (fun (p : library_port) ->
      Hashtbl.replace t (cname, p.port_name) p.port_direction)
      ports
  ) cells;
  t

(* Declared port widths for Xilinx primitives.  Anything not in this
 * table is assumed 1-bit; that's correct for every LUT/FF/buffer pin.
 * The widths matter because nextpnr-xilinx's pack_carry_xc7 rewrites
 *   port_xform[id("O[i]")] = id("Oi")  for i in 0..3
 * and the router then resolves `<bel>.O0..O3` from the chipdb.  If our
 * JSON emits a bare `O` connection with a single bit (because only one
 * tap was used in the user RTL), pack_carry's xform doesn't fire and
 * routing dies with "No wire found for port O on source cell" on the
 * chain-MSB CARRY4 — that was task #35.  We pad short connections with
 * fresh unique net IDs so the bus shape always matches the BEL.        *)
let xil_primitive_widths : (string * (string * int) list) list = [
  "CARRY4", [ "CO", 4; "O", 4; "DI", 4; "S", 4 ];
]

let port_width_table (cells : (string * library_port list) list)
    : (string * string, int) Hashtbl.t =
  let t = Hashtbl.create 64 in
  List.iter (fun (cname, ports) ->
    List.iter (fun (port_name, w) ->
      Hashtbl.replace t (cname, port_name) w) ports
  ) xil_primitive_widths;
  List.iter (fun (cname, ports) ->
    List.iter (fun (p : library_port) ->
      if p.port_width > 1 then
        Hashtbl.replace t (cname, p.port_name) p.port_width)
      ports
  ) cells;
  t

let dir_str : [< `Input | `Output | `Inout | `Internal ] -> string = function
  | `Input    -> "input"
  | `Output   -> "output"
  | `Inout    -> "inout"
  | `Internal -> "input"  (* unreachable for cells/ports; keeps types unified *)

let yosys_json
    ~(library_cells : (string * library_port list) list)
    (m : bmodule) : Yojson.Safe.t =
  let ctx = mk_ctx () in
  populate_widths ctx m;
  let port_dirs = port_dir_table library_cells in
  let port_widths = port_width_table library_cells in

  (* Pre-allocate net ids for top-level ports so they are stable.
     For vector ports, we allocate one id per bit. *)
  let port_bits = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    if s.direction <> `Internal then begin
      let w = match s.stype with
        | BInt { width; _ } -> width
        | BBool             -> 1
        | _                 -> 1 in
      let bs = List.init w (fun i -> I (alloc ctx { base = s.name; bit = i })) in
      Hashtbl.add port_bits s.name (s.direction, bs)
    end
  ) m.signals;

  (* nextpnr-xilinx auto-inserts GND/VCC tie cells, so we drop ours to
     avoid multi-driver conflicts.  The constants are already represented
     as the "0"/"1" string tokens in any consumer's connections. *)
  let is_skipped_cell = function "GND" | "VCC" -> true | _ -> false in
  let cells = List.filter_map (fun (i : binstance) ->
    if is_skipped_cell i.module_name then None else Some (
    (* port direction lookup: per-cell port_name -> input/output *)
    let dir_of_pin pin =
      match Hashtbl.find_opt port_dirs (i.module_name, pin) with
      | Some d -> d
      | None   -> `Input    (* default to input if library_cells omitted *)
    in
    (* Pad short bit lists up to the declared port width with fresh net
     * IDs.  See the comment on xil_primitive_widths above — necessary so
     * pack_carry_xc7's `O[i] -> Oi` xform fires on every CARRY4 in the
     * chain, even ones whose top O bits have no consumers in the RTL. *)
    let pad_bits_to_width pin bits =
      let actual = List.length bits in
      let declared =
        try Hashtbl.find port_widths (i.module_name, pin)
        with Not_found -> actual
      in
      if actual >= declared then bits
      else
        let pad_count = declared - actual in
        let pads = List.init pad_count (fun _ ->
          I (alloc ctx { base = "__pad_" ^ i.inst_name ^ "_" ^ pin;
                         bit = !(ctx.next_id) })) in
        bits @ pads
    in
    let conns =
      List.map (fun (pin, expr) ->
        pin, bits_json (pad_bits_to_width pin (bits_of_conn ctx expr)))
        i.port_connections in
    let dirs =
      List.map (fun (pin, _) -> pin, `String (dir_str (dir_of_pin pin)))
        i.port_connections in
    let int_to_bin32 (n : int) =
      String.init 32 (fun i ->
        if (n lsr (31 - i)) land 1 = 1 then '1' else '0') in
    let params =
      List.map (fun (k, s) -> k, `String (verilog_lit_to_binary s)) i.param_strs
      @ List.map (fun (k, n) -> k, `String (int_to_bin32 n)) i.param_values
    in
    i.inst_name,
    `Assoc [
      "hide_name",       `Int 1;
      "type",            `String i.module_name;
      "parameters",      `Assoc params;
      "attributes",      `Assoc [];
      "port_directions", `Assoc dirs;
      "connections",     `Assoc conns;
    ])
  ) m.instances in

  let ports =
    Hashtbl.fold (fun nm (dir, bs) acc ->
      (nm, `Assoc [
        "direction", `String (dir_str dir);
        "bits",      `List (List.map bit_json bs);
      ]) :: acc
    ) port_bits []
  in

  let netnames =
    Hashtbl.fold (fun nm ids acc ->
      (nm, `Assoc [
        "hide_name",  `Int (if Hashtbl.mem port_bits nm then 0 else 1);
        "bits",       `List (List.map (fun i -> `Int i) ids);
        "attributes", `Assoc [];
      ]) :: acc
    ) ctx.net_names []
  in

  `Assoc [
    "creator", `String "edif_to_structural";
    "modules", `Assoc [
      m.name, `Assoc [
        "attributes", `Assoc [ "top", `String "00000000000000000000000000000001" ];
        "ports",      `Assoc ports;
        "cells",      `Assoc cells;
        "netnames",   `Assoc netnames;
      ]
    ]
  ]

let write_yosys_json ~(library_cells : (string * library_port list) list)
                    ~(path : string) (m : bmodule) : unit =
  Yojson.Safe.to_file path (yosys_json ~library_cells m)
