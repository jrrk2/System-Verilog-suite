(* Synthetic placed netlist generator for a multiply-accumulate
   pipeline of width W, parameterised by multiplier and adder
   architecture.  Used to compare archs head-to-head on real
   LEF-based numbers when we don't have a synth+P&R flow at hand.

   The cell count, cell type, and depth-per-stage are taken from
   textbook references (CSEE adders, Wallace/Dadda multipliers).
   Cells are placed deterministically in a grid:
       column = bit position    (width direction)
       row    = pipeline stage  (depth direction)
   so HPWL between consecutive stages reflects the cell pitch and
   the architecture's natural fan-out.  The arrival numbers then
   come out of the same Placement_timing pipeline as a real
   placed design — no fake delays.

   The MAC under test is
       y_next = y + a * b
   for unsigned W-bit a, b and 2W-bit y. *)

type adder_arch =
  | Ripple_a
  | Kogge_stone_a
  | Brent_kung_a
  | Sklansky_a

type mul_arch =
  | Array_m
  | Wallace_m
  | Dadda_m

let adder_arch_name = function
  | Ripple_a -> "ripple"
  | Kogge_stone_a -> "kogge_stone"
  | Brent_kung_a -> "brent_kung"
  | Sklansky_a -> "sklansky"

let mul_arch_name = function
  | Array_m -> "array"
  | Wallace_m -> "wallace"
  | Dadda_m -> "dadda"

let log2_ceil w =
  let rec loop v acc = if v <= 1 then max 1 acc else loop ((v+1)/2) (acc+1) in
  loop w 0

(* Stages on the longest combinational path through each arch. *)
let adder_depth ~arch ~width =
  match arch with
  | Ripple_a       -> width
  | Sklansky_a     -> log2_ceil width
  | Kogge_stone_a  -> log2_ceil width
  | Brent_kung_a   -> max 1 (2 * log2_ceil width - 1)

let mul_depth ~arch ~width =
  match arch with
  | Array_m              -> 2 * width
  (* log_{1.5}(W) reduction levels, then a width-W ripple final-add. *)
  | Wallace_m | Dadda_m  ->
      let l = log2_ceil width in
      l + width

(* Total cell count — useful as a scale-cost number alongside
   the timing.  Approximate; matches the rough 2-wide accuracy
   you find in arch comparison tables. *)
let adder_cells ~arch ~width =
  match arch with
  | Ripple_a       -> width
  | Sklansky_a     -> (width * log2_ceil width) / 2 + width
  | Kogge_stone_a  -> width * log2_ceil width + width
  | Brent_kung_a   -> 2 * width

let mul_cells ~arch ~width =
  match arch with
  | Array_m              -> width * width + width * (width - 1)
  | Wallace_m | Dadda_m  -> width * width + (3 * width) / 2 + width

(* Cell type used for each role.  Pick small NanGate cells so
   the Liberty delays in the test fixture are realistic. *)
let role_cell = function
  | `And  -> "AND2_X1"
  | `Xor  -> "XOR2_X1"
  | `Inv  -> "INV_X1"
  | `Buf  -> "BUF_X1"
  | `Maj  -> "AOI22_X1"   (* full-adder carry-out approximation *)

(* Pitch of the placement grid (dbu).  Nangate45 places cells on
   a ~190 dbu site, but we use a coarser pitch since each "cell"
   in the synthetic netlist may map to several real gates after
   synthesis. *)
let col_pitch = 1500
let row_pitch = 2800

(* Build the netlist + placements for one arch.  Returns
       (cells, nets, total_cells, depth)
   where [cells] is a Placement.placement list and [nets] is a
   Nets.net list ready to feed Placement_timing. *)
let build ~width ~mul_arch ~add_arch =
  let cells = ref [] in
  let nets = ref [] in
  let cell_count = ref 0 in

  let put ~stage ~bit ~role ~prefix =
    incr cell_count;
    let inst = Printf.sprintf "%s_s%d_b%d_%d" prefix stage bit !cell_count in
    let cell = role_cell role in
    let p =
      { Placement.inst; cell;
        x = bit * col_pitch;
        y = stage * row_pitch;
        orient = Placement.N }
    in
    cells := p :: !cells;
    inst
  in

  (* ── Multiplier stage ─────────────────────────────────────── *)
  let mul_d  = mul_depth  ~arch:mul_arch ~width in
  let prev = Array.make (2 * width) None in

  (* W column of partial-product AND gates at depth 0. *)
  for b = 0 to width - 1 do
    let n = put ~stage:0 ~bit:b ~role:`And ~prefix:"u_mul" in
    prev.(b) <- Some n
  done;

  (* Reduction levels: each level XORs/ANDs the previous one. *)
  for s = 1 to mul_d - 1 do
    for b = 0 to (2 * width - 1) do
      if b <= s + width / 2 then begin
        let role = if s mod 2 = 0 then `Xor else `Maj in
        let n = put ~stage:s ~bit:b ~role ~prefix:"u_mul" in
        let drv = match prev.(b) with
          | Some d -> d
          | None ->
              (* fall back: drive from bit 0 of the previous row *)
              (match prev.(0) with Some d -> d | None -> "0") in
        nets := { Nets.name = Printf.sprintf "n_mul_s%d_b%d" s b;
                  pins = [
                    { Nets.inst = drv; pin = "Z"  };
                    { Nets.inst = n;   pin = "A1" };
                  ] } :: !nets;
        prev.(b) <- Some n
      end
    done
  done;

  let mul_out = Array.copy prev in

  (* ── Adder stage ──────────────────────────────────────────── *)
  let add_d = adder_depth ~arch:add_arch ~width:(2 * width) in
  let aw = 2 * width in
  let prev = Array.make aw None in

  let stage_offset = mul_d in

  (* Stage 0 of the adder: an XOR per bit driven by the
     corresponding mul_out. *)
  for b = 0 to aw - 1 do
    let n = put ~stage:stage_offset ~bit:b ~role:`Xor ~prefix:"u_add" in
    (match mul_out.(b) with
     | Some d ->
         nets := { Nets.name = Printf.sprintf "n_add_in_b%d" b;
                   pins = [
                     { Nets.inst = d; pin = "Z"  };
                     { Nets.inst = n; pin = "A1" };
                   ] } :: !nets
     | None -> ());
    prev.(b) <- Some n
  done;

  (* Carry/prefix-tree levels: depth depends on arch. *)
  for s = 1 to add_d - 1 do
    for b = 0 to aw - 1 do
      let n = put ~stage:(stage_offset + s) ~bit:b ~role:`Maj
                  ~prefix:"u_add" in
      let drv1 = match prev.(b) with Some d -> d | None -> "0" in
      let prev_b =
        match add_arch with
        | Ripple_a -> max 0 (b - 1)
        | _        -> max 0 (b - (1 lsl (s - 1)))
      in
      let drv2 = match prev.(prev_b) with Some d -> d | None -> drv1 in
      nets := { Nets.name = Printf.sprintf "n_add_s%d_b%d" s b;
                pins = [
                  { Nets.inst = drv1; pin = "Z" };
                  { Nets.inst = drv2; pin = "Z" };
                  { Nets.inst = n;    pin = "A1" };
                ] } :: !nets;
      prev.(b) <- Some n
    done
  done;

  let total = !cell_count in
  let depth = mul_d + add_d in
  (List.rev !cells, List.rev !nets, total, depth)
