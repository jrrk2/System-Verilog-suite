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
(* Substring test (no Stdlib helper for this). *)
let contains_sub (s : string) (sub : string) : bool =
  let n = String.length s and m = String.length sub in
  let rec go i = i + m <= n && (String.sub s i m = sub || go (i + 1)) in
  m = 0 || go 0

let const_of_name = function
  | "<const0>" | "GND" -> Some "0"
  | "<const1>" | "VCC" -> Some "1"
  (* edif2_to_structural pads a SPARSE EDIF bus pin (only some `(member P k)`
     present) with a fresh dangling net per absent member, so the present
     members keep their absolute positions.  An absent member on an INPUT is an
     unconnected pin bit, which Verilog reads as 0 -- but emitted as a real net
     it is a driverless net, and nextpnr then fails the whole design with
     "timing analysis failed ... incomplete specification of timing ports"
     (352 of them here, nearly all RAMB36E1 WEBWE/DIADI/DIBDI).  Tie them to 0.
     On an OUTPUT this is still right: sanitize_output_bits redirects a const
     bit on an output pin to a fresh dead net, which is exactly the isolated
     dangling net the gap wants.
     flatten_struct PREFIXES the hierarchy path onto the sentinel, so this must
     be a substring test, not a prefix test:
       u_ibex_demo_system__u_eth__...__ram_inst__$svs_unconn$ram$WEBWE$3 *)
  | s when contains_sub s "$svs_unconn$" -> Some "0"
  | _                  -> None

(* Net key — what we put in the (signal_base, bit) -> id table.        *)
type net_key = { base : string; bit : int }

(* Net id allocator.  Constants are emitted the way nextpnr-xilinx writes its
   ROUTED json (which is what json2dcp consumes): as references to two real
   nets $PACKER_GND_NET / $PACKER_VCC_NET, NOT yosys-style C "0"/"1" string
   bits.  json2dcp maps those net names to GLOBAL_LOGIC0/1; it does not parse
   string consts in cell connections.  Ids 2 and 3 are reserved for them. *)
type ctx = {
  next_id     : int ref;
  ties        : (net_key, bool) Hashtbl.t;  (* GND/VCC CELL output nets -> const *)
  ids         : (net_key, int) Hashtbl.t;
  net_names   : (string, int list) Hashtbl.t;     (* base -> bit ids, in bit-order *)
  widths      : (string, int) Hashtbl.t;          (* signal-name -> declared width *)
  gnd_id      : int;
  vcc_id      : int;
  gnd_used    : bool ref;
  vcc_used    : bool ref;
}

let mk_ctx () = {
  next_id   = ref 4;          (* 0/1 yosys-reserved; 2/3 = GND/VCC packer nets *)
  ties      = Hashtbl.create 64;
  ids       = Hashtbl.create 4096;
  net_names = Hashtbl.create 1024;
  widths    = Hashtbl.create 1024;
  gnd_id    = 2;
  vcc_id    = 3;
  gnd_used  = ref false;
  vcc_used  = ref false;
}

(* A constant bit.  Two conventions:
   - DEFAULT ($PACKER nets): emit a reference to a real $PACKER_GND_NET /
     $PACKER_VCC_NET net.  This is what json2dcp / the Vivado read_edif path
     consumes, and how nextpnr writes its own ROUTED json.
   - NEXTPNR_JSON_CONST_STRINGS=1 (DIRECT nextpnr consumption of natively-
     compiled Verilog): emit yosys-style "0"/"1" string bits.  Feeding a
     $PACKER_VCC_NET *input* net to nextpnr COLLIDES with the const network
     nextpnr builds itself during packing -- it rebinds the name to a device
     VCC wire (e.g. CMT_TOP_SW4END0_1), orphaning cells that still reference
     the old net -> post-pack check() assertion (nets.find). *)
let const_strings = Sys.getenv_opt "NEXTPNR_JSON_CONST_STRINGS" <> None
let const_bit ctx (is_one : bool) : bit =
  if const_strings then C (if is_one then "1" else "0")
  else if is_one then (ctx.vcc_used := true; I ctx.vcc_id)
  else                (ctx.gnd_used := true; I ctx.gnd_id)

(* Populate the widths table from the bmodule's signal declarations.
 * Lets bits_of_conn expand a bare `BVar name` reference into its full
 * vector of bits when name is a multi-bit signal — without this,
 * CARRY4.S = BVar "_671" expanded to a single bit instead of four,
 * which left nextpnr-xilinx's carry packer accessing past-the-end of
 * the bit list and segfaulting. *)
let populate_widths ctx (m : bmodule) =
  List.iter (fun (s : bsignal) ->
    (* Canonical width: a `_ -> 1` fallback silently collapsed an array/struct
       bus to one net (the CARRY4-packer under-expansion class). *)
    let w = width_of_btype s.stype in
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

(* Net-key reference that honours GND/VCC CELL ties: the tie cells are
   SKIPPED at emission (constants ride const_bit), so a net driven by a
   GND/VCC INSTANCE (pcs_pma_flat's VCC_2 feeding reset-sync CE pins)
   must resolve to a constant, not a driverless net id — the EDIF-side
   twin of this bug left CE pins floating on silicon. *)
let net_ref const_bit ctx (k : net_key) =
  match Hashtbl.find_opt ctx.ties k with
  | Some one -> const_bit ctx one
  | None -> I (alloc ctx k)

(* Reduce a bexpr-as-net-reference to a single (name, bit_index option). *)
let rec resolve_bit_ref (e : bexpr) : (string * int option) option =
  match e with
  | BVar nm -> Some (nm, None)
  | BSelect { array = BVar nm; index = BConst { value; _ } } ->
      Some (nm, Some (Z.to_int value))
  | BSlice { signal = BVar nm; msb; lsb } when msb = lsb ->
      (* A WIDTH-1 slice IS a bit-select.  Vivado EDIF renders a scalar
         instance-pin member as `name[0:0]` and then wraps it in a chain of
         redundant `[0]` selects, e.g.
           u_dm_top_n_8[0:0][32'0][32'0][32'0][32'0][32'0][32'0][32'0]
         Without this case the recursion bottoms out on a BSlice that
         resolve_bit_ref cannot reduce, returns None, and the whole pin
         expression is rejected.  bits_of_conn matches the DIRECT
         `BSlice { signal = BVar _; _ }` earlier, so this only ever fires for
         the nested form -- no change to the ordinary slice path. *)
      Some (nm, Some lsb)
  | BSelect { array; index = BConst { value; _ } } ->
      (* Nested select, e.g. Vivado EDIF renders a scalar top-level port
         as IO_CLK_P[0][0].  Resolve the inner ref: indexing a scalar
         (None) gives that bit; a redundant outer [0] on an already-
         resolved bit is the identity. *)
      (match resolve_bit_ref array with
       | Some (nm, None)                  -> Some (nm, Some (Z.to_int value))
       | Some (nm, Some b) when Z.equal value Z.zero -> Some (nm, Some b)
       | _ -> None)
  | BConcat _ ->
      (* Vivado EDIF doesn't emit concatenations on instance pins. *)
      None
  | _ -> None

(* Recognise of_circuit's multi-bit-input memo naming `<base>__<i>` and map it
   to bit i of the vector `<base>` (a declared multi-bit signal).  Without this
   a per-bit input reference `framing_rdata__40` allocates its own scalar net,
   disconnected from the port bit `framing_rdata[40]` -> driverless net. *)
let bitbus_ref ctx nm =
  (* A name that is ITSELF a declared signal (Vivado's next-state twin
     `wr_addr__0`, a distinct `wire [4:4]`) must NOT be memo-stripped to
     `{wr_addr,0}` — that aliased it onto bit 0 of the bus and produced a
     multiply-driven net (wr_addr_reg[0].Q vs wr_addr[4]_i_1.O).  Same
     class as the EDIF declared/inferred split. *)
  if Hashtbl.mem ctx.widths nm then None else
  let n = String.length nm in
  let rec find i =
    if i < 1 then None
    else if nm.[i] = '_' && nm.[i - 1] = '_' then Some i else find (i - 1) in
  match find (n - 1) with
  | Some j when j + 1 < n ->
      let suf = String.sub nm (j + 1) (n - j - 1) in
      let base = String.sub nm 0 (j - 1) in
      if suf <> "" && String.for_all (fun c -> c >= '0' && c <= '9') suf
         && (match Hashtbl.find_opt ctx.widths base with Some w -> w > 1 | None -> false)
      then Some (base, int_of_string suf) else None
  | _ -> None

(* Resolve a port connection's bexpr to its list of net bits.  Scalar
   pins -> 1-element list; CARRY4 S[3:0] -> 4-element list (LSB first). *)
let rec bits_of_conn ctx (e : bexpr) : bit list =
  match e with
  | BConst { value; width } ->
      (* Verilog literal on an instance pin (e.g. `.CEB(1'b0)` on
         IBUFDS_GTE2).  Emit one `C "0"` / `C "1"` per bit, LSB first. *)
      let rec range i = if i >= width then [] else
        let b = (if Z.testbit value i then 1 else 0) in
        const_bit ctx (b = 1) :: range (i + 1) in
      range 0
  | BConcat es ->
      (* BIR's BConcat is MSB-first by convention; reverse to get the
         LSB-first order yosys-JSON `bits` lists use. *)
      List.concat_map (bits_of_conn ctx) (List.rev es)
  | BSlice { signal = BVar base; msb; lsb } ->
      (* Expand base[msb:lsb] as the LSB-first list of single-bit refs
         base[lsb], base[lsb+1], ..., base[msb]. *)
      let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi in
      List.map (fun i ->
        match const_of_name base with
        | Some s -> const_bit ctx (s = "1")
        | None -> net_ref const_bit ctx { base; bit = i }
      ) (range lsb msb)
  | BSlice { signal; msb; lsb } ->
      (* Slice of a composite signal (e.g. flatten_struct emits a bus as a
         BConcat of individual nets, then slices it): expand the inner signal
         to its full LSB-first bit list and take indices [lsb..msb]. *)
      let full = Array.of_list (bits_of_conn ctx signal) in
      let n = Array.length full in
      let rec range lo hi = if lo > hi then [] else lo :: range (lo + 1) hi in
      List.map (fun i ->
        if i >= 0 && i < n then full.(i)
        else begin
          (* Out-of-range bit-select: Verilog reads x here (yosys ties it to
             0).  Match that instead of failing so a design with a benign
             over-select still emits. *)
          Printf.eprintf
            "[bir_to_nextpnr_json] WARN: slice bit %d of a width-%d composite \
             out of range -> tied 0\n" i n;
          const_bit ctx false
        end
      ) (range lsb msb)
  | _ ->
      (match resolve_bit_ref e with
        | Some (nm, _) when const_of_name nm <> None ->
            [const_bit ctx (const_of_name nm = Some "1")]
        | Some (nm, Some bit) -> [net_ref const_bit ctx { base = nm; bit }]
        | Some (nm, None    ) ->
            (match bitbus_ref ctx nm with
             | Some (base, bit) ->
                 (* of_circuit names a multi-bit INPUT port's bit-i reference
                  * `<base>__<i>` (hardcaml_to_behavioral line ~252).  Resolve
                  * it to bit i of the vector `<base>` so the reader connects
                  * to the actual port bit instead of an orphan scalar net —
                  * the driverless-net bug that made Vivado opt trim the
                  * design. *)
                 [net_ref const_bit ctx { base; bit }]
             | None ->
                 (* Bare `BVar nm` reference — expand to nm's declared
                  * width if known, else fall back to one bit.  Multi-bit
                  * expansion is essential for CARRY4 inputs like .S(_671)
                  * where _671 is a 4-bit wire. *)
                 let w = try Hashtbl.find ctx.widths nm with Not_found -> 1 in
                 List.init w (fun i -> net_ref const_bit ctx { base = nm; bit = i }))
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

(* Collapse the identity-LUT6 buffer chain SVS's of_circuit inserts between a GT
   SERIAL output pin (GTXE2_CHANNEL.GTXTXP/GTXTXN — an OPAD driver) and its
   top-level output port.  That chain (INIT=64'hFFFFFFFF00000000, all six inputs
   tied) is a nextpnr OUTMUX workaround: correct for FABRIC output ports, but on
   a GT serial port nextpnr then auto-inserts an OBUF and binds it to the GT
   serial OPAD -> "No Bel OPAD/.../OUTBUF".  Rewrite the GT pin to drive the port
   net directly and drop the chain buffers, so constrain_gt binds GTXTXP->OPAD
   with no fabric buffer.  Only chains that TERMINATE at a GT serial pin are
   collapsed — the ~4000 legitimate fabric OUTMUX buffers are untouched — and the
   test is STRUCTURAL (no sgmii_ port-name heuristic).  This is the upstream
   equivalent of the removed build-script strip_gt_pins identity-chain pass and
   mirrors bir_to_edif's ident-obuf bypass. *)
let collapse_gt_serial (m : bmodule) : bmodule =
  let key_of : bexpr -> (string * int) option = function
    | BVar nm -> Some (nm, 0)
    | BSlice { signal = BVar b; msb; lsb } when msb = lsb -> Some (b, lsb)
    | BSelect { array = BVar b; index = BConst { value; _ } } -> Some (b, Z.to_int value)
    | _ -> None in
  let is_ident (i : binstance) =
    String.equal i.module_name "LUT6"
    && List.mem ("INIT", "64'hFFFFFFFF00000000") i.param_strs
    && (match List.assoc_opt "I0" i.port_connections with
        | Some i0 ->
            List.for_all (fun p ->
              match List.assoc_opt p i.port_connections with
              | Some e -> e = i0 | None -> false) ["I1"; "I2"; "I3"; "I4"; "I5"]
        | None -> false) in
  let out_ports : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    if s.direction = `Output then Hashtbl.replace out_ports s.name ()) m.signals;
  (* ident buffer indexed by its INPUT net key -> (inst_name, its output conn) *)
  let by_in : (string * int, string * bexpr) Hashtbl.t = Hashtbl.create 64 in
  List.iter (fun (i : binstance) ->
    if is_ident i then
      match List.assoc_opt "I0" i.port_connections,
            List.assoc_opt "O" i.port_connections with
      | Some i0, Some o ->
          (match key_of i0 with Some k -> Hashtbl.replace by_in k (i.inst_name, o)
                              | None -> ())
      | _ -> ()) m.instances;
  let drop : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let rewrite : (string * string, bexpr) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (i : binstance) ->
    if String.equal i.module_name "GTXE2_CHANNEL" then
      List.iter (fun pin ->
        match List.assoc_opt pin i.port_connections with
        | Some conn ->
            (* walk the ident chain forward from the GT pin to a terminal port *)
            let rec walk k chain seen =
              match Hashtbl.find_opt by_in k with
              | Some (inst, out) when not (List.mem inst seen) ->
                  (match key_of out with
                   | Some ok when Hashtbl.mem out_ports (fst ok) ->
                       Some (out, inst :: chain)
                   | Some ok -> walk ok (inst :: chain) (inst :: seen)
                   | None -> None)
              | _ -> None in
            (match key_of conn with
             | Some k ->
                 (match walk k [] [] with
                  | Some (port_conn, chain) ->
                      Hashtbl.replace rewrite (i.inst_name, pin) port_conn;
                      List.iter (fun n -> Hashtbl.replace drop n ()) chain
                  | None -> ())
             | None -> ())
        | None -> ()) ["GTXTXP"; "GTXTXN"]) m.instances;
  if Hashtbl.length rewrite = 0 then m
  else
    let instances =
      List.filter_map (fun (i : binstance) ->
        if Hashtbl.mem drop i.inst_name then None
        else if String.equal i.module_name "GTXE2_CHANNEL" then
          Some { i with port_connections =
            List.map (fun (pin, conn) ->
              match Hashtbl.find_opt rewrite (i.inst_name, pin) with
              | Some pc -> (pin, pc) | None -> (pin, conn)) i.port_connections }
        else Some i) m.instances in
    { m with instances }

(* Bypass an identity-LUT6 buffer (INIT=64'hFFFFFFFF00000000, all six inputs
   tied) sitting DIRECTLY on a clock-buffer output.  SVS's of_circuit inserts
   these as an OUTMUX workaround, but on a CLOCK net it is disastrous: the
   BUFG/BUFH/BUFR output (a real clock-network source) is re-driven onto a
   FABRIC LUT, so its fanout (here rx_clk: 245 CK/WCLK pins) lands on general
   routing and router2 cannot reach the far CLKINV inputs ("failed to find a
   route using dedicated resources" -> SKIP_FAILED_ARCS).  Drop the buffer and
   alias its readers back onto the clock-buffer net so the clock stays on the
   dedicated network.  Reader-side (net alias), vs collapse_gt_serial's
   driver-side rewrite; mirrors bir_to_edif's ident-obuf bypass. *)
let bypass_clock_ident_buffers (m : bmodule) : bmodule =
  let is_clock_buf t = List.mem t
    ["BUFG"; "BUFGCTRL"; "BUFGCE"; "BUFH"; "BUFHCE"; "BUFR"; "BUFIO"; "BUFMR"] in
  let key_of : bexpr -> (string * int) option = function
    | BVar nm -> Some (nm, 0)
    | BSlice { signal = BVar b; msb; lsb } when msb = lsb -> Some (b, lsb)
    | BSelect { array = BVar b; index = BConst { value; _ } } -> Some (b, Z.to_int value)
    | _ -> None in
  let is_ident (i : binstance) =
    String.equal i.module_name "LUT6"
    && List.mem ("INIT", "64'hFFFFFFFF00000000") i.param_strs
    && (match List.assoc_opt "I0" i.port_connections with
        | Some i0 ->
            List.for_all (fun p ->
              match List.assoc_opt p i.port_connections with
              | Some e -> e = i0 | None -> false) ["I1"; "I2"; "I3"; "I4"; "I5"]
        | None -> false) in
  (* top-level output port names: an identity buffer that drives a clock-forward
     OUTPUT PORT is the legitimate OUTMUX case -- keep it (dropping it would leave
     the port undriven / on fabric anyway). *)
  let out_ports : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    if s.direction = `Output then Hashtbl.replace out_ports s.name ()) m.signals;
  (* clock_src: net key -> the ROOT clock-buffer output expr that (transitively,
     through identity buffers) drives it.  Seed with every clock-buffer O pin. *)
  let clock_src : (string * int, bexpr) Hashtbl.t = Hashtbl.create 32 in
  List.iter (fun (i : binstance) ->
    if is_clock_buf i.module_name then
      match List.assoc_opt "O" i.port_connections with
      | Some o -> (match key_of o with Some k -> Hashtbl.replace clock_src k o | None -> ())
      | None -> ()) m.instances;
  let drop : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let alias : (string * int, bexpr) Hashtbl.t = Hashtbl.create 16 in
  (* Iterate to a fixpoint: the recovered clock is forwarded through a CHAIN of
     these buffers (rx_clk -> eth_clk -> ...), so bypassing one exposes the next.
     Each dropped buffer aliases its output to the chain's ROOT clock net. *)
  let changed = ref true in
  while !changed do
    changed := false;
    List.iter (fun (i : binstance) ->
      if is_ident i && not (Hashtbl.mem drop i.inst_name) then
        match List.assoc_opt "I0" i.port_connections,
              List.assoc_opt "O" i.port_connections with
        | Some i0, Some o ->
            (match key_of i0, key_of o with
             | Some ik, Some ok
               when Hashtbl.mem clock_src ik && not (Hashtbl.mem out_ports (fst ok)) ->
                 let root = Hashtbl.find clock_src ik in
                 Hashtbl.replace drop i.inst_name ();
                 Hashtbl.replace alias ok root;
                 Hashtbl.replace clock_src ok root;
                 changed := true
             | _ -> ())
        | _ -> ()) m.instances
  done;
  if Hashtbl.length alias = 0 then m
  else begin
    let repl e = match key_of e with
      | Some k -> (match Hashtbl.find_opt alias k with Some r -> Some r | None -> None)
      | None -> None in
    let rec rw e =
      match repl e with
      | Some r -> r
      | None ->
        (match e with
         | BVar _ | BConst _ -> e
         | BSlice { signal; msb; lsb } -> BSlice { signal = rw signal; msb; lsb }
         | BSelect { array; index } -> BSelect { array = rw array; index = rw index }
         | BConcat es -> BConcat (List.map rw es)
         | BReplicate { count; value } -> BReplicate { count; value = rw value }
         | BBinOp { op; lhs; rhs; result_type } ->
             BBinOp { op; lhs = rw lhs; rhs = rw rhs; result_type }
         | BUnOp { op; operand; result_type } ->
             BUnOp { op; operand = rw operand; result_type }
         | BCond { condition; then_val; else_val } ->
             BCond { condition = rw condition; then_val = rw then_val; else_val = rw else_val }
         | BCall { func; args } -> BCall { func; args = List.map rw args }) in
    let instances =
      List.filter_map (fun (i : binstance) ->
        if Hashtbl.mem drop i.inst_name then None
        else Some { i with port_connections =
          List.map (fun (p, e) -> (p, rw e)) i.port_connections }) m.instances in
    { m with instances }
  end

let yosys_json
    ~(library_cells : (string * library_port list) list)
    (m : bmodule) : Yojson.Safe.t =
  let m = collapse_gt_serial m in
  let m = bypass_clock_ident_buffers m in
  let ctx = mk_ctx () in
  populate_widths ctx m;
  (* GND/VCC CELL instances are skipped below — bind their output nets to
     constants or every reader dangles (nextpnr GT packer rejected the GT
     config pins; reset-sync CE pins would float). *)
  List.iter (fun (i : binstance) ->
    match i.module_name with
    | ("GND" | "VCC") as mn ->
        let one = (mn = "VCC") in
        let pin = if one then "P" else "G" in
        (match List.assoc_opt pin i.port_connections with
         | Some e ->
             let rec keys = function
               | BVar nm when const_of_name nm = None ->
                   [{ base = nm; bit = 0 }]
               | BSlice { signal = BVar b; msb; lsb } when const_of_name b = None ->
                   List.init (abs (msb - lsb) + 1)
                     (fun k -> { base = b; bit = min msb lsb + k })
               | BConcat es -> List.concat_map keys es
               | _ -> [] in
             List.iter (fun k -> Hashtbl.replace ctx.ties k one) (keys e)
         | None -> ())
    | _ -> ()) m.instances;
  let port_dirs = port_dir_table library_cells in
  let port_widths = port_width_table library_cells in

  (* nextpnr-xilinx auto-inserts GND/VCC tie cells, so we drop ours below. *)
  let is_skipped_cell = function "GND" | "VCC" -> true | _ -> false in
  (* No silent input-default.  The netlist is flattened here (post
     flatten_struct), so every instance is a primitive.  A pin with no resolved
     direction — absent from the Xilinx baseline, library_cells, AND the unisim
     VHD interface — must NOT be guessed as input: that is exactly what emitted
     every GTXE2 OUTPUT (CPLLLOCK, RXOUTCLK, …) as an input, orphaning its net
     and producing false nextpnr combinatorial loops.  Fail loudly, naming the
     primitive.port pairs, so the fix (add its VHD interface — primitive/ or
     secureip/) is unambiguous. *)
  let unresolved =
    List.concat_map (fun (i : binstance) ->
      if is_skipped_cell i.module_name then []
      else List.filter_map (fun (pin, _) ->
        if Hashtbl.mem port_dirs (i.module_name, pin) then None
        else Some (i.module_name ^ "." ^ pin)) i.port_connections) m.instances
    |> List.sort_uniq compare in
  if unresolved <> [] then
    failwith (Printf.sprintf
      "bir_to_nextpnr_json %s: unresolved primitive port directions — no Xilinx \
       baseline, library_cell, or unisim VHD interface entry for: %s. Guessing \
       these as inputs orphans real outputs and yields false nextpnr \
       combinatorial loops; supply the primitive's VHD interface."
      m.name (String.concat ", " unresolved));

  (* Pre-allocate net ids for top-level ports so they are stable.
     For vector ports, we allocate one id per bit. *)
  (* `__keep_<net>` outputs are synthetic retention handles added by
     behavioral_to_hardcaml to stop Hardcaml pruning clock-only boxes
     (IBUFDS/BUFG); they must NOT surface as top-level IO ports/pads. *)
  let is_keep_port nm =
    String.length nm >= 7 && String.sub nm 0 7 = "__keep_" in
  let port_bits = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    if s.direction <> `Internal && not (is_keep_port s.name) then begin
      let w = match s.stype with
        | BInt { width; _ } -> width
        | BBool             -> 1
        | _                 -> 1 in
      let bs = List.init w (fun i -> I (alloc ctx { base = s.name; bit = i })) in
      Hashtbl.add port_bits s.name (s.direction, bs)
    end
  ) m.signals;

  (* nextpnr-xilinx auto-inserts GND/VCC tie cells, so we drop ours (above,
     is_skipped_cell) to avoid multi-driver conflicts.  The constants are
     already represented as the "0"/"1" string tokens in any consumer's
     connections. *)
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
      let found, declared =
        try true, Hashtbl.find port_widths (i.module_name, pin)
        with Not_found -> false, actual
      in
      if found && actual > declared && dir_of_pin pin <> `Output then
        (* Clamp an over-wide INPUT connection down to the pin's declared
           width: a 32-bit const param bound to a narrow config pin (GTXE2
           LOOPBACK is [2:0]; C_GT_LOOPBACK arrives 32-bit const-0) leaves
           LOOPBACK[3..] with no wire on the bel -> nextpnr router error
           "No wire found for port LOOPBACK3".  Match bir_to_edif's clamp. *)
        List.filteri (fun k _ -> k < declared) bits
      else if actual >= declared then bits
      else
        let pad_count = declared - actual in
        let pads = List.init pad_count (fun _ ->
          I (alloc ctx { base = "__pad_" ^ i.inst_name ^ "_" ^ pin;
                         bit = !(ctx.next_id) })) in
        bits @ pads
    in
    (* A cell OUTPUT pin must never drive the constant network: an
     * out-of-range bit-select (bits_of_conn ties it to const-0, matching
     * Verilog x-read semantics) is fine on an INPUT, but on an OUTPUT it makes
     * the cell a second driver of the shared GND/VCC net → nextpnr rejects the
     * design with "multiply driven".  Redirect any const bit on an output pin
     * to a fresh dead net so the degenerate driver stands alone. *)
    let sanitize_output_bits pin bits =
      List.mapi (fun idx b ->
        let is_const = match b with
          | C _ -> true
          | I id -> id = ctx.gnd_id || id = ctx.vcc_id in
        if is_const then
          I (alloc ctx { base = "__oor_" ^ i.inst_name ^ "_" ^ pin; bit = idx })
        else b) bits
    in
    let conns =
      List.map (fun (pin, expr) ->
        let bits = bits_of_conn ctx expr in
        let bits = if dir_of_pin pin = `Output then sanitize_output_bits pin bits
                   else bits in
        pin, bits_json (pad_bits_to_width pin bits))
        i.port_connections in
    let dirs =
      List.map (fun (pin, _) -> pin, `String (dir_str (dir_of_pin pin)))
        i.port_connections in
    let int_to_bin32 (n : int) =
      String.init 32 (fun i ->
        if (n lsr (31 - i)) land 1 = 1 then '1' else '0') in
    (* A double-quoted value is a STRING-enum parameter (MMCM COMPENSATION,
       BANDWIDTH, GT *_CFG strings, …): emit it BARE, the way yosys' write_json
       does, so nextpnr's `str_or_default` compares the raw token.  Keeping the
       Verilog quotes makes the value literally `"ZHOLD"` (7 chars) which never
       equals nextpnr's `ZHOLD` -> "unsupported COMPENSATION type '"ZHOLD"'".
       Non-quoted values still go through verilog_lit_to_binary (sized literals
       -> bit strings; plain reals/ints pass through). *)
    let param_str_value s =
      let s = String.trim s in
      let n = String.length s in
      if n >= 2 && s.[0] = '"' && s.[n - 1] = '"' then String.sub s 1 (n - 2)
      else verilog_lit_to_binary s in
    let params =
      List.map (fun (k, s) -> k, `String (param_str_value s)) i.param_strs
      @ List.map (fun (k, n) -> k, `String (int_to_bin32 n)) i.param_values
    in
    (* Cells instantiated WITHOUT #() params (Vivado netlists rely on
       defaults) still need them in the json: nextpnr's DRAM packer keys on
       INIT_*/IS_WCLK_INVERTED, and their absence left the RAM64M read-port
       timing arcs unmodelled ("combinatorial loops" abort).  yosys fills
       these defaults when it parses the netlist; mirror that. *)
    let params =
      let zeros n = String.make n '0' in
      let defaults = match i.module_name with
        | "RAM64M" | "RAM32M" ->
            [ "INIT_A", zeros 64; "INIT_B", zeros 64;
              "INIT_C", zeros 64; "INIT_D", zeros 64;
              "IS_WCLK_INVERTED", "0" ]
        | "RAM32X1D" | "RAM32X1S" -> [ "INIT", zeros 32; "IS_WCLK_INVERTED", "0" ]
        | "RAM64X1D" | "RAM64X1S" -> [ "INIT", zeros 64; "IS_WCLK_INVERTED", "0" ]
        | "RAM128X1D" | "RAM128X1S" -> [ "INIT", zeros 128; "IS_WCLK_INVERTED", "0" ]
        | "SRL16E" -> [ "INIT", zeros 16; "IS_CLK_INVERTED", "0" ]
        | "SRLC32E" -> [ "INIT", zeros 32; "IS_CLK_INVERTED", "0" ]
        | _ -> [] in
      params @ List.filter_map (fun (k, v) ->
        if List.mem_assoc k params then None else Some (k, `String v)) defaults
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

  (* nextpnr emits every netname SINGLE-BIT; json2dcp keys each net by bits[0],
     so a multi-bit entry (e.g. a bus collected under one base name) would lose
     its higher bits at import.  Split any multi-bit net into name[i] per bit. *)
  let emit_net nm hide ids acc =
    match ids with
    | [i] -> (nm, `Assoc [ "hide_name", `Int hide;
                           "bits", `List [ `Int i ];
                           "attributes", `Assoc [] ]) :: acc
    | _ ->
        List.fold_left (fun (k, a) i ->
          (k + 1,
           (Printf.sprintf "%s[%d]" nm k, `Assoc [
              "hide_name", `Int hide;
              "bits", `List [ `Int i ];
              "attributes", `Assoc [] ]) :: a)) (0, acc) ids |> snd
  in
  let netnames =
    Hashtbl.fold (fun nm ids acc ->
      emit_net nm (if Hashtbl.mem port_bits nm then 0 else 1) ids acc
    ) ctx.net_names []
  in
  (* nextpnr emits every top-level PORT net in netnames too (not just in
     `ports`).  json2dcp builds its net-id map from netnames only, so a port
     net absent here is "unknown net" at a driver pin (e.g. OBUF.O -> led[i]).
     nextpnr emits ALL netnames SINGLE-BIT (a bus is split into name[i] per
     bit); json2dcp keys each net by bits[0], so a multi-bit bus entry would
     lose its higher bits.  Split each port net into one single-bit netname per
     bit (name for width 1, name[i] for buses) to match nextpnr. *)
  let netnames =
    Hashtbl.fold (fun nm (_dir, bs) acc ->
      if Hashtbl.mem ctx.net_names nm then acc
      else if List.length bs <= 1 then
        (nm, `Assoc [
           "hide_name",  `Int 0;
           "bits",       `List (List.map bit_json bs);
           "attributes", `Assoc [];
         ]) :: acc
      else
        (* bus: one single-bit netname per bit, LSB = name[0] *)
        List.fold_left (fun (i, a) b ->
          (i + 1,
           (Printf.sprintf "%s[%d]" nm i, `Assoc [
              "hide_name",  `Int 0;
              "bits",       `List [ bit_json b ];
              "attributes", `Assoc [];
            ]) :: a)) (0, acc) bs |> snd
    ) port_bits netnames
  in
  (* Emit the constant nets json2dcp expects (maps them to GLOBAL_LOGIC0/1). *)
  let const_net nm id =
    (nm, `Assoc [ "hide_name", `Int 1;
                  "bits", `List [ `Int id ];
                  "attributes", `Assoc [] ]) in
  let netnames =
    (if !(ctx.gnd_used) then [const_net "$PACKER_GND_NET" ctx.gnd_id] else [])
    @ (if !(ctx.vcc_used) then [const_net "$PACKER_VCC_NET" ctx.vcc_id] else [])
    @ netnames
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
