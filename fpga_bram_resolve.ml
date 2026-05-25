(* FPGA block-RAM resolver — the FPGA analogue of [Mem_macro_resolve].
 *
 * Where Mem_macro_resolve maps a (depth,width) memory onto an OpenRAM
 * ASIC macro, this maps it onto a tiling of FIXED-SHAPE Xilinx 7-series
 * block-RAM primitives (RAMB18E1 / RAMB36E1).  behavioral_memlower uses
 * the plan to emit RAMB binstances (+ a library_cell port list for
 * directions); the FPGA flow's Inst lowering then carries those to the
 * nextpnr netlist.
 *
 * Geometry facts (no-parity data widths) we rely on:
 *   RAMB36E1 : 32768 usable bits; widths {1,2,4,8,16,32}; max 32b/port.
 *   RAMB18E1 : 16384 usable bits; widths {1,2,4,8,16};    max 16b/port.
 * A word wider than one tile is split across width-tiles; a memory
 * deeper than one tile's word-count is split across depth-tiles (the
 * read path muxes the depth-tiles by the high address bits).
 *
 * This module is pure (no hardcaml) — the planner + primitive port
 * descriptors only.  Pin wiring + the memlower hook build on top. *)

type prim =
  | RAMB18E1
  | RAMB36E1

let prim_name = function RAMB18E1 -> "RAMB18E1" | RAMB36E1 -> "RAMB36E1"

(* usable (no-parity) capacity in bits and max per-port data width. *)
let prim_capacity = function RAMB18E1 -> 16384 | RAMB36E1 -> 32768
let prim_max_width = function RAMB18E1 -> 16 | RAMB36E1 -> 32

let bits_needed n =
  if n <= 1 then 1
  else
    let rec loop b m = if m >= n then b else loop (b + 1) (m * 2) in
    loop 1 2

(* round a requested data width up to a supported no-parity width. *)
let supported_widths = [ 1; 2; 4; 8; 16; 32 ]

let round_up_width w =
  match List.find_opt (fun s -> s >= w) supported_widths with
  | Some s -> s
  | None -> 32 (* clamp; widths > 32 are handled by width-tiling *)

(* geometry of ONE tile primitive. *)
type tile_geom =
  { prim : prim
  ; tile_width : int (* data bits this tile drives (a supported width) *)
  ; tile_depth : int (* words held at [tile_width] *)
  ; tile_addr_bits : int
  }

type plan =
  { depth : int
  ; width : int
  ; n_width_tiles : int (* tiles across the data word *)
  ; n_depth_tiles : int (* tiles down the address space *)
  ; tile : tile_geom (* every tile shares this geometry *)
  ; addr_bits : int (* total address bits = bits_needed depth *)
  ; total_tiles : int
  }

(* Choose RAMB36E1 by default (best capacity); RAMB18E1 only when the
   whole memory comfortably fits one and is narrow — keeps small RAMs off
   a 36Kb tile.  [prim_hint] can force a primitive. *)
let choose_prim ?prim_hint ~depth ~width () =
  match prim_hint with
  | Some p -> p
  | None ->
    let bits = depth * width in
    if bits <= prim_capacity RAMB18E1 && width <= prim_max_width RAMB18E1
    then RAMB18E1
    else RAMB36E1

(* largest supported width <= [u] (>=1). *)
let largest_supported_le u =
  List.fold_left (fun acc s -> if s <= u && s > acc then s else acc) 1 supported_widths

let plan ?prim_hint ~(depth : int) ~(width : int) () : plan =
  if depth <= 0 || width <= 0 then
    failwith (Printf.sprintf "fpga_bram_resolve.plan: depth=%d width=%d invalid" depth width);
  let prim = choose_prim ?prim_hint ~depth ~width () in
  (* cap tile width at 16: widths 1..16 store data straight into DI[w-1:0]
     (parity lanes unused), so the builder stays interleave-free; a 32-bit
     word becomes 2x16-bit tiles rather than one x36 lane with the awkward
     8-data+1-parity interleave. *)
  let maxw = min (prim_max_width prim) 16 in
  let cap = prim_capacity prim in
  (* Depth expansion is MUX-FREE: a narrower per-tile data width gives the
     same physical BRAM a deeper address space (cap/w words), so pick the
     widest supported width whose tile-depth still covers [depth] (capped
     at the primitive max and the data width).  This turns depth into
     extra WIDTH-tiles instead of an address-mux'd depth cascade — up to
     cap deep (32K for RAMB36) in a single depth-tile.  Only when
     depth > cap (>32K) do we fall back to depth tiles. *)
  let max_for_depth = max 1 (cap / depth) in
  let tile_width = largest_supported_le (min (min maxw width) max_for_depth) in
  let tile_depth = cap / tile_width in
  let n_depth_tiles = (depth + tile_depth - 1) / tile_depth in
  let n_width_tiles = (width + tile_width - 1) / tile_width in
  { depth
  ; width
  ; n_width_tiles
  ; n_depth_tiles
  ; tile = { prim; tile_width; tile_depth; tile_addr_bits = bits_needed tile_depth }
  ; addr_bits = bits_needed depth
  ; total_tiles = n_width_tiles * n_depth_tiles
  }

let string_of_plan p =
  Printf.sprintf
    "%dx%d -> %d x %s [%d wide-tile(s) x %d deep-tile(s)], tile=%dx%d (addr %db), total %d tile(s)"
    p.depth p.width p.total_tiles (prim_name p.tile.prim) p.n_width_tiles
    p.n_depth_tiles p.tile.tile_depth p.tile.tile_width p.tile.tile_addr_bits
    p.total_tiles

(* ── primitive port directions (the wiring contract) ─────────────────
   The SDP (simple-dual-port) read/write subset we actually connect, as
   Behavioral_ir.library_port records so behavioral_memlower can register
   the cell and the Inst lowering can split inputs vs outputs.  Port A is
   the write port, port B the read port; data width = the tile width.
   (RAMB has many more pins; unconnected ones take primitive defaults.) *)
let tile_ports (g : tile_geom) : Behavioral_ir.library_port list =
  let inp n w : Behavioral_ir.library_port =
    { port_name = n; port_direction = `Input; port_width = w }
  in
  let outp n w : Behavioral_ir.library_port =
    { port_name = n; port_direction = `Output; port_width = w }
  in
  let aw = 14 in
  (* RAMB ADDR buses are a fixed 14 bits regardless of geometry. *)
  let dw = g.tile_width in
  [ inp "CLKARDCLK" 1 (* write clock (port A) *)
  ; inp "CLKBWRCLK" 1 (* read clock  (port B) *)
  ; inp "ENARDEN" 1 (* port-A enable *)
  ; inp "ENBWREN" 1 (* port-B enable *)
  ; inp "WEA" 4 (* port-A byte write-enables *)
  ; inp "ADDRARDADDR" aw (* write address *)
  ; inp "ADDRBWRADDR" aw (* read address *)
  ; inp "DIADI" dw (* write data *)
  ; outp "DOBDO" dw (* read data *)
  ]

(* ── single-tile RAMB36E1 binstance builder ──────────────────────────
   Emit the BIR for a sync 1-write/1-read RAM of depth<=1024, width<=32
   on ONE RAMB36E1, matching the yosys synth_xilinx mapping exactly:
     - TDP mode (default); port A write @ WRITE_WIDTH_A=36, port B read @
       READ_WIDTH_B=36; outputs unregistered (DO?_REG=0).
     - address {1'b1, word_addr[9:0], 5'b0} (word addr in ADDR[14:5]).
     - WEA = {4{we}}, EN*=1, all RST/REGCE=0, WEBWE=0.
     - x36 data interleave: 8 data + 1 parity per 9-bit lane, so the 32
       logical bits land in DIADI[28:0] (8+8+8+5) + DIPADIP[2:0].
   Returns (binstance, new internal signals, driver/reassembly stmts,
   read-data signal name) — the caller rewrites read sites to that name. *)
open Behavioral_ir

let uint w = BInt { width = w; signed = Unsigned }
let kconst v w = BConst { value = v; width = w }
let isig name w : bsignal =
  { name; stype = uint w; direction = `Internal; initial_value = None; attrs = [] }

(* zero-extend a [from_w]-bit expr to exactly [to_w] bits (MSB pad). *)
let zext e ~from_w ~to_w =
  if from_w >= to_w then BSlice { signal = e; msb = to_w - 1; lsb = 0 }
  else BConcat [ kconst 0 (to_w - from_w); e ]

let build_single_ramb36 ~(name : string) ~(depth : int) ~(width : int)
    ~(write_clk : bexpr) ~(read_clk : bexpr) ~(we : bexpr) ~(write_addr : bexpr)
    ~(write_data : bexpr) ~(read_addr : bexpr)
    : binstance * bsignal list * bstmt list * string =
  if depth > 1024 || width > 32 then
    failwith
      (Printf.sprintf
         "build_single_ramb36: %dx%d exceeds one RAMB36E1 (use tiling)" depth width);
  let aw = bits_needed depth in
  (* {1'b1, word_addr[9:0], 5'b0} as a 16-bit ADDR expr. *)
  let addr16 a =
    BConcat [ kconst 1 1; zext a ~from_w:aw ~to_w:10; kconst 0 5 ]
  in
  (* widen write data to 32, then x36-interleave into DIADI / DIPADIP. *)
  let d32 = zext write_data ~from_w:width ~to_w:32 in
  let sl hi lo = BSlice { signal = d32; msb = hi; lsb = lo } in
  let diadi = BConcat [ kconst 0 3; sl 31 27; sl 25 18; sl 16 9; sl 7 0 ] in
  let dipadip = BConcat [ kconst 0 1; sl 26 26; sl 17 17; sl 8 8 ] in
  let dobdo = name ^ "_dobdo" and dopbdop = name ^ "_dopbdop" in
  let rdata = name ^ "_rdata" in
  let dob hi lo = BSlice { signal = BVar dobdo; msb = hi; lsb = lo } in
  let dop i = BSlice { signal = BVar dopbdop; msb = i; lsb = i } in
  (* inverse interleave: reassemble the 32-bit read word. *)
  let rdata_full =
    BConcat [ dob 28 24; dop 2; dob 23 16; dop 1; dob 15 8; dop 0; dob 7 0 ]
  in
  let rdata_expr =
    if width >= 32 then rdata_full
    else BSlice { signal = rdata_full; msb = width - 1; lsb = 0 }
  in
  let inst =
    { inst_name = name ^ "_ramb"
    ; module_name = "RAMB36E1"
    ; param_values =
        [ ("WRITE_WIDTH_A", 36); ("READ_WIDTH_B", 36)
        ; ("READ_WIDTH_A", 0); ("WRITE_WIDTH_B", 0)
        ; ("DOA_REG", 0); ("DOB_REG", 0) ]
    ; param_strs = []
    ; port_connections =
        [ ("CLKARDCLK", write_clk); ("CLKBWRCLK", read_clk)
        ; ("ENARDEN", kconst 1 1); ("ENBWREN", kconst 1 1)
        ; ("REGCEAREGCE", kconst 0 1); ("REGCEB", kconst 0 1)
        ; ("RSTRAMARSTRAM", kconst 0 1); ("RSTRAMB", kconst 0 1)
        ; ("RSTREGARSTREG", kconst 0 1); ("RSTREGB", kconst 0 1)
        ; ("WEA", BReplicate { count = 4; value = we }); ("WEBWE", kconst 0 8)
        ; ("ADDRARDADDR", addr16 write_addr); ("ADDRBWRADDR", addr16 read_addr)
        ; ("DIADI", diadi); ("DIPADIP", dipadip)
        ; ("DOBDO", BVar dobdo); ("DOPBDOP", BVar dopbdop) ]
    }
  in
  let signals = [ isig dobdo 32; isig dopbdop 4; isig rdata width ] in
  let stmts = [ BAssign { lhs = rdata; rhs = rdata_expr } ] in
  inst, signals, stmts, rdata

(* ── byte-lane RAMB18E1 builder (the picosoc-proven path) ────────────
   A [width]-bit (width%8=0) sync 1W1R memory of depth<=2048 as width/8
   byte lanes, each a RAMB18E1 in 2K×9 (8-bit) mode — interleave-free, so
   the CPU's per-byte write strobe maps straight to a lane write-enable
   and INIT is plain byte packing.  Matches yosys synth_xilinx's RAMB18E1
   mapping (verified for a 2048×8 RAM): port A write @ WRITE_WIDTH_A=9,
   port B read @ READ_WIDTH_B=9 -> DOBDO[7:0]; ADDR={addr[10:0],3'b0};
   WEA={2{we}}; DIADI={8'b0,byte}.  Ported from ~/tinyllm-fpga bin2init.py
   (kept in OCaml).  [init] (one int per word, LSB = byte lane 0) bakes a
   ROM via INIT_00..INIT_3F; omit it for an uninitialised RAM. *)

(* A true-dual-port port: both A and B can read AND write.  A port reads
   [p_addr] every cycle (-> its rdata net) and writes [p_wdata] there when
   [p_we].  Typical picosoc use: port A = CPU (load+store), port B = host
   overlay (Ethernet-loaded firmware). *)
type ram_port =
  { p_clk : bexpr
  ; p_addr : bexpr
  ; p_we : bexpr (* 1-bit word write enable (used when p_wstrb = None) *)
  ; p_wdata : bexpr (* [width] bits *)
  ; p_wstrb : bexpr option
      (* per-byte write strobe ([width]/8 bits, bit b = byte b). When set,
         forces 8-bit tiles and drives byte tile b's WE from strobe bit b,
         so the CPU's mem_wstrb gives correct sb/sh/sw. *)
  }

(* 64 INIT_xx (256-bit binary strings) for byte [lane] of [words]; entry A
   sits at INIT_(A/32) data bits [(A%32)*8 +: 8]. *)
let lane_init_strings ~(words : int array) ~(lane : int) : (string * string) list =
  List.init 64 (fun xx ->
    let s =
      String.init 256 (fun i ->
        let p = 255 - i in
        (* MSB-first *)
        let k = p / 8 and bit = p mod 8 in
        let wi = (xx * 32) + k in
        let w = if wi < Array.length words then words.(wi) else 0 in
        let byte = (w lsr (lane * 8)) land 0xFF in
        if (byte lsr bit) land 1 = 1 then '1' else '0')
    in
    Printf.sprintf "INIT_%02X" xx, s)

(* RAMB mode width = physical port width incl. parity for a given data
   tile width: 8->9, 16->18, 32->36; 1/2/4 have no parity. *)
let mode_width tw = match tw with 8 -> 9 | 16 -> 18 | 32 -> 36 | w -> w

let rec log2 n = if n <= 1 then 0 else 1 + log2 (n / 2)

(* INIT_xx (256-bit binary strings) for one tile = bits [tile_lo +: slice_w]
   of each word, stored in a [tile_width]-bit-per-entry RAMB (high
   tile_width-slice_w bits zero).  Entry A -> INIT_(A/(256/tw)) bit
   (A mod (256/tw))*tw + bit_in_entry.  Parity (INITP) left 0. *)
let tile_init_strings ~(words : int array) ~(tile_lo : int) ~(slice_w : int)
    ~(tile_width : int) ~(n_init : int) : (string * string) list =
  let eps = 256 / tile_width in
  List.init n_init (fun xx ->
    let s =
      String.init 256 (fun i ->
        let p = 255 - i in
        let entry = p / tile_width and bit = p mod tile_width in
        let a = (xx * eps) + entry in
        let w = if a < Array.length words then words.(a) else 0 in
        let dv = (w lsr tile_lo) land ((1 lsl slice_w) - 1) in
        if bit < slice_w && (dv lsr bit) land 1 = 1 then '1' else '0')
    in
    Printf.sprintf "INIT_%02X" xx, s)

(* Build a [width]-bit sync RAM, depth<=cap (32K on RAMB36, mux-free via
   narrow tiles), as the plan's n_width_tiles of [tile_width]-bit RAMBs
   (RAMB18E1/RAMB36E1, x1..x16 — interleave-free).  [ports] is 1 or 2
   true-dual-port ports (each read+write): port A reads on DOADO, B on
   DOBDO.  Returns (binstances, internal signals, read-reassembly stmts,
   one rdata net name per input port). *)
let build_byte_lane_ram ~(name : string) ~(depth : int) ~(width : int)
    ?(init : int array option) ~(ports : ram_port list) ()
    : binstance list * bsignal list * bstmt list * string list =
  (match ports with [ _ ] | [ _; _ ] -> () | _ -> failwith "build_byte_lane_ram: 1 or 2 ports");
  (* per-byte write strobes force 8-bit (one tile per byte) so each byte
     tile's WE comes from one strobe bit. *)
  let byte_mode = List.exists (fun p -> Option.is_some p.p_wstrb) ports in
  let prim, tw, n =
    if byte_mode then begin
      if width mod 8 <> 0 then failwith "build_byte_lane_ram: byte strobes need width%8=0";
      let prim = if depth <= 2048 then RAMB18E1 else RAMB36E1 in
      if depth > prim_capacity prim / 8 then
        failwith
          (Printf.sprintf "build_byte_lane_ram: byte-write depth %d > %d (deep tiling TODO)"
             depth (prim_capacity prim / 8));
      prim, 8, width / 8
    end
    else begin
      let pl = plan ~depth ~width () in
      if pl.n_depth_tiles > 1 then
        failwith
          (Printf.sprintf "build_byte_lane_ram: depth %d exceeds one tile (%d) — deep tiling TODO"
             depth pl.tile.tile_depth);
      pl.tile.prim, pl.tile.tile_width, pl.n_width_tiles
    end
  in
  (* per-tile WE: a byte strobe bit when present, else the word we. *)
  let we_of (p : ram_port) t =
    match p.p_wstrb with
    | Some ws -> BSlice { signal = ws; msb = t; lsb = t }
    | None -> p.p_we
  in
  let aw = bits_needed depth in
  let cap = prim_capacity prim in
  let n_init = cap / 256 in
  let mw = mode_width tw in
  let shift = log2 tw in
  let addr_total, di_w, dip_w, wea_w, webwe_w, top =
    match prim with
    | RAMB18E1 -> 14, 16, 2, 2, 4, []
    | RAMB36E1 -> 16, 32, 4, 4, 8, [ kconst 1 1 ]
  in
  let word_addr_w = addr_total - shift - List.length top in
  let addr_expr a =
    BConcat
      (top @ [ zext a ~from_w:aw ~to_w:word_addr_w ]
       @ (if shift > 0 then [ kconst 0 shift ] else []))
  in
  let pa = List.nth ports 0 in
  let pb = if List.length ports >= 2 then Some (List.nth ports 1) else None in
  let tiles =
    List.init n (fun t ->
      let lo = t * tw in
      let sw = min tw (width - lo) in
      let tname = Printf.sprintf "%s_t%d" name t in
      let doa = tname ^ "_doa" and dopa = tname ^ "_dopa" in
      let dob = tname ^ "_dob" and dopb = tname ^ "_dopb" in
      let di_of (p : ram_port) =
        let slice = BSlice { signal = p.p_wdata; msb = lo + sw - 1; lsb = lo } in
        if di_w = sw then slice else BConcat [ kconst 0 (di_w - sw); slice ]
      in
      let base_strs =
        [ ("RAM_MODE", "TDP"); ("WRITE_MODE_A", "READ_FIRST")
        ; ("WRITE_MODE_B", "READ_FIRST") ]
      in
      let param_strs =
        base_strs
        @ (match init with
           | Some words -> tile_init_strings ~words ~tile_lo:lo ~slice_w:sw ~tile_width:tw ~n_init
           | None -> [])
      in
      let b_w = if Option.is_some pb then mw else 0 in
      let inst =
        { inst_name = tname ^ "_ramb"
        ; module_name = prim_name prim
        ; param_values =
            [ ("READ_WIDTH_A", mw); ("WRITE_WIDTH_A", mw)
            ; ("READ_WIDTH_B", b_w); ("WRITE_WIDTH_B", b_w)
            ; ("DOA_REG", 0); ("DOB_REG", 0) ]
        ; param_strs
        ; port_connections =
            [ ("CLKARDCLK", pa.p_clk)
            ; ("CLKBWRCLK", (match pb with Some p -> p.p_clk | None -> pa.p_clk))
            ; ("ENARDEN", kconst 1 1); ("ENBWREN", kconst 1 1)
            ; ("REGCEAREGCE", kconst 0 1); ("REGCEB", kconst 0 1)
            ; ("RSTRAMARSTRAM", kconst 0 1); ("RSTRAMB", kconst 0 1)
            ; ("RSTREGARSTREG", kconst 0 1); ("RSTREGB", kconst 0 1)
            ; ("WEA", BReplicate { count = wea_w; value = we_of pa t })
            ; ( "WEBWE"
              , match pb with
                | Some p -> BReplicate { count = webwe_w; value = we_of p t }
                | None -> kconst 0 webwe_w )
            ; ("ADDRARDADDR", addr_expr pa.p_addr)
            ; ( "ADDRBWRADDR"
              , addr_expr (match pb with Some p -> p.p_addr | None -> pa.p_addr) )
            ; ("DIADI", di_of pa); ("DIPADIP", kconst 0 dip_w)
            ; ( "DIBDI"
              , match pb with Some p -> di_of p | None -> kconst 0 di_w )
            ; ("DIPBDIP", kconst 0 dip_w)
            ; ("DOADO", BVar doa); ("DOPADOP", BVar dopa)
            ; ("DOBDO", BVar dob); ("DOPBDOP", BVar dopb) ]
        }
      in
      t, sw, inst, doa, dob)
  in
  let insts = List.map (fun (_, _, i, _, _) -> i) tiles in
  let signals =
    List.concat_map
      (fun (t, _, _, doa, dob) ->
        let tn = Printf.sprintf "%s_t%d" name t in
        [ isig doa di_w; isig (tn ^ "_dopa") dip_w; isig dob di_w; isig (tn ^ "_dopb") dip_w ])
      tiles
  in
  (* reassemble a port's word from its per-tile read slices (MSB-first). *)
  let read_word ~use_a =
    BConcat
      (List.rev
         (List.map
            (fun (_, sw, _, doa, dob) ->
              BSlice { signal = BVar (if use_a then doa else dob); msb = sw - 1; lsb = 0 })
            tiles))
  in
  let rdata_a = name ^ "_rdata_a" in
  let out_sigs = ref [ isig rdata_a width ] in
  let out_stmts = ref [ BAssign { lhs = rdata_a; rhs = read_word ~use_a:true } ] in
  let rdata_names = ref [ rdata_a ] in
  (match pb with
   | Some _ ->
     let rdata_b = name ^ "_rdata_b" in
     out_sigs := !out_sigs @ [ isig rdata_b width ];
     out_stmts := !out_stmts @ [ BAssign { lhs = rdata_b; rhs = read_word ~use_a:false } ];
     rdata_names := !rdata_names @ [ rdata_b ]
   | None -> ());
  insts, signals @ !out_sigs, !out_stmts, !rdata_names
