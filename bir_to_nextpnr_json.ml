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
}

let mk_ctx () = {
  next_id   = ref 2;
  ids       = Hashtbl.create 4096;
  net_names = Hashtbl.create 1024;
}

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
      [(match resolve_bit_ref e with
        | Some (nm, _) when const_of_name nm <> None ->
            C (match const_of_name nm with Some s -> s | None -> assert false)
        | Some (nm, Some bit) -> I (alloc ctx { base = nm; bit })
        | Some (nm, None    ) -> I (alloc ctx { base = nm; bit = 0 })
        | None ->
            failwith ("bir_to_nextpnr_json: unsupported pin expression: "
                      ^ Behavioral_ir.string_of_bexpr e))]

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

(* port_direction lookup table built from bprogram.library_cells. *)
let port_dir_table (cells : (string * library_port list) list)
    : (string * string, [`Input|`Output]) Hashtbl.t =
  let t = Hashtbl.create 256 in
  List.iter (fun (cname, ports) ->
    List.iter (fun (p : library_port) ->
      Hashtbl.replace t (cname, p.port_name) p.port_direction)
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
  let port_dirs = port_dir_table library_cells in

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
    let conns =
      List.map (fun (pin, expr) -> pin, bits_json (bits_of_conn ctx expr))
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
