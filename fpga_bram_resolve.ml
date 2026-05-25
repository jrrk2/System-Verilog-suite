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

let plan ?prim_hint ~(depth : int) ~(width : int) () : plan =
  if depth <= 0 || width <= 0 then
    failwith (Printf.sprintf "fpga_bram_resolve.plan: depth=%d width=%d invalid" depth width);
  let prim = choose_prim ?prim_hint ~depth ~width () in
  let maxw = prim_max_width prim in
  (* width-tiling: split the word into chunks of <= maxw. *)
  let n_width_tiles = (width + maxw - 1) / maxw in
  let tile_width = round_up_width (min width maxw) in
  let tile_depth = prim_capacity prim / tile_width in
  let n_depth_tiles = (depth + tile_depth - 1) / tile_depth in
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
