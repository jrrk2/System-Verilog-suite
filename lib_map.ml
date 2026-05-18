(* Liberty-driven structural tech-mapper.

   Walks a [Hardcaml.Circuit.t] and emits one Liberty cell per
   gate.  The cover is greedy and covers the small bounded
   vocabulary [Behavioral_to_hardcaml] emits:

       And/Or/Xor (Op2)   -> AND2_X1 / OR2_X1 / XOR2_X1   (one per bit)
       Not                -> INV_X1                       (one per bit)
       Mux  (2-input)     -> MUX2_X1                      (one per bit)
       Reg                -> DFF_X1                       (one per bit)
       Const              -> 1'b0 / 1'b1                  (no cell)
       Wire/Select/Cat    -> wire-only, no cell

   Multi-bit ops are bit-blasted: a 4-bit AND becomes four
   single-bit AND2_X1 instances on per-bit slices.

   Hardcoded cell catalogue for Nangate45 right now — that's
   sufficient for the gcd / aes class of designs we're targeting
   first.  When we move to Sky130 or other libs, replace the
   catalogue with one parsed from Liberty (sv_liberty.ml has the
   function-expression parser already).

   Things this mapper deliberately does NOT handle:
     - arithmetic Op2 (add/sub/mul/eq/lt) — emitted as raw
       Verilog operators in the output, yosys techmaps them
       downstream.  Same fall-back the hardcaml-lua reference
       used for unmatched shapes.
     - mux with N>2 cases (rare in hardcaml output)
     - multi-port memories — would need a separate mapper
     - Inst nodes (instantiated submodules) — pass through *)

open Hardcaml.Signal

(* ── Cell catalogue ──────────────────────────────────────────── *)

type cell = {
  cell_name : string;       (* Liberty cell type, e.g. "AND2_X1" *)
  in_pins   : string list;  (* in declaration order *)
  out_pin   : string;       (* the driven pin *)
}

let cell_and  = { cell_name = "AND2_X1"; in_pins = ["A1"; "A2"]; out_pin = "ZN" }
let cell_or   = { cell_name = "OR2_X1";  in_pins = ["A1"; "A2"]; out_pin = "ZN" }
let cell_xor  = { cell_name = "XOR2_X1"; in_pins = ["A";  "B"];  out_pin = "Z"  }
let cell_inv  = { cell_name = "INV_X1";  in_pins = ["A"];        out_pin = "ZN" }
let cell_buf  = { cell_name = "BUF_X1";  in_pins = ["A"];        out_pin = "Z"  }
(* MUX2_X1 in Nangate45: function "(!S & A) | (S & B)". *)
let cell_mux  = { cell_name = "MUX2_X1"; in_pins = ["A"; "B"; "S"]; out_pin = "Z"  }
(* DFF_X1: D, CK → Q.  No reset port; sync reset is data-path. *)
let cell_dff  = { cell_name = "DFF_X1";  in_pins = ["D"; "CK"];    out_pin = "Q"  }
(* DFFR_X1: D, CK, RN → Q.  Async low-active reset. *)
let cell_dffr = { cell_name = "DFFR_X1"; in_pins = ["D"; "CK"; "RN"]; out_pin = "Q"  }

(* Physical tie cells.  Driving a pin from a literal 1'b1 / 1'b0
   makes OpenROAD's read_verilog tag the resulting net as POWER,
   which TritonRoute then refuses to route ("Net foo of signal type
   POWER is not routable").  Instantiate explicit tie cells so the
   driving net is a regular SIGNAL. *)
let cell_logic1 = { cell_name = "LOGIC1_X1"; in_pins = []; out_pin = "Z" }
let cell_logic0 = { cell_name = "LOGIC0_X1"; in_pins = []; out_pin = "Z" }

(* ── Output: list of cell instances + bit-level wire decls ──── *)

type pin_conn = { pin : string; net : string }

type instance = {
  cell      : cell;
  inst_name : string;
  conns     : pin_conn list;        (* including out-pin *)
}

type netlist = {
  inputs    : (string * int) list;  (* port name, width *)
  outputs   : (string * int) list;
  wires     : (string * int) list;  (* internal nets *)
  insts     : instance list;
  assigns   : (string * string) list; (* lhs <- rhs (raw Verilog), for unmapped ops *)
}

(* ── Naming helpers ──────────────────────────────────────────── *)

let next_id = ref 0
let mint prefix =
  incr next_id;
  Printf.sprintf "_%s_%d_" prefix !next_id

(* Identifier-safe rewrite of an arbitrary RTL net name.  Verilog
   identifiers can't carry [, ], {, }, dots, slashes, etc — replace
   those with `_` so [out_name] = "alu_out[15]" or "child.inst.foo"
   becomes a usable inst-name fragment.  Keeps the result short (≤32
   chars) so cell names stay greppable in the placement reports.  *)
let sanitize_for_id s =
  let n = String.length s in
  let n = if n > 32 then 32 else n in
  let b = Bytes.create n in
  for i = 0 to n - 1 do
    let c = s.[i] in
    Bytes.set b i
      (match c with
       | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> c
       | _ -> '_')
  done;
  Bytes.to_string b

(* Parse a legacy free-form context string like "alu_out_bit15" into
   (signal, bit) so we can feed it to [Block_tag.mint_in_scope] and
   produce a structured, reversible cell name.  Falls back to
   (ctx, None) when no _bitN suffix is present.                    *)
let parse_bit_ctx ctx =
  let re = Str.regexp "^\\(.*\\)_bit\\([0-9]+\\)$" in
  if Str.string_match re ctx 0
  then (Str.matched_group 1 ctx,
        try Some (int_of_string (Str.matched_group 2 ctx)) with _ -> None)
  else (ctx, None)

(* Mint a context-bearing name.  When [Block_tag.current_modhash] is
   set (Hier_synth.set_current_module has been called), cells get
   structured Block_tag-encoded names that survive layout transforms
   and let downstream passes recover (module, signal, bit, block,
   role).  When unset (legacy / internal cases), falls back to the
   simple `_<ctx>__<prefix>_<id>_` form.                            *)
let mint_ctx ~ctx prefix =
  if !Block_tag.current_modhash = "" then begin
    if ctx = "" then mint prefix
    else begin
      incr next_id;
      Printf.sprintf "_%s__%s_%d_" (sanitize_for_id ctx) prefix !next_id
    end
  end else begin
    let signal, bit = parse_bit_ctx ctx in
    (* Block_tag.mint_in_scope picks the active block's kind when one
       is set; otherwise we tag this as a stand-alone OP per-bit cell. *)
    Block_tag.mint_in_scope ~kind:Block_tag.OP ~signal ?bit ~role:prefix ()
  end

let net_for_signal s =
  let names = names s in
  match names with
  | n :: _ -> n
  | [] -> Printf.sprintf "_n%d" (Hardcaml.Signal.Type.uid s |> Hardcaml.Signal.Type.Uid.to_int)

let bit_net signal_name bit_idx width =
  if width = 1 then signal_name
  else Printf.sprintf "%s[%d]" signal_name bit_idx

(* ── Walk + map ──────────────────────────────────────────────── *)

(* For each multi-bit op2, bit-blast: emit width copies of the
   per-bit cell, each instance reads bit i of arg_a and arg_b,
   drives bit i of the output. *)
let blast_op2 ~cell ~out_name ~width ~a_name ~b_name ~a_w ~b_w =
  let insts = ref [] in
  for i = 0 to width - 1 do
    let bit_ctx =
      if width = 1 then out_name
      else Printf.sprintf "%s_bit%d" out_name i in
    let inst_name = mint_ctx ~ctx:bit_ctx cell.cell_name in
    let ai = if a_w = 1 then a_name else Printf.sprintf "%s[%d]" a_name i in
    let bi = if b_w = 1 then b_name else Printf.sprintf "%s[%d]" b_name i in
    let oi = if width = 1 then out_name
             else Printf.sprintf "%s[%d]" out_name i in
    insts := { cell; inst_name;
               conns = [
                 { pin = List.nth cell.in_pins 0; net = ai };
                 { pin = List.nth cell.in_pins 1; net = bi };
                 { pin = cell.out_pin; net = oi };
               ] } :: !insts
  done;
  List.rev !insts

let blast_unary ~cell ~out_name ~width ~a_name ~a_w =
  let insts = ref [] in
  for i = 0 to width - 1 do
    let bit_ctx =
      if width = 1 then out_name
      else Printf.sprintf "%s_bit%d" out_name i in
    let inst_name = mint_ctx ~ctx:bit_ctx cell.cell_name in
    let ai = if a_w = 1 then a_name else Printf.sprintf "%s[%d]" a_name i in
    let oi = if width = 1 then out_name
             else Printf.sprintf "%s[%d]" out_name i in
    insts := { cell; inst_name;
               conns = [
                 { pin = List.hd cell.in_pins; net = ai };
                 { pin = cell.out_pin;        net = oi };
               ] } :: !insts
  done;
  List.rev !insts

let blast_mux ~out_name ~width ~sel_name ~a_name ~b_name =
  let insts = ref [] in
  for i = 0 to width - 1 do
    let bit_ctx =
      if width = 1 then out_name
      else Printf.sprintf "%s_bit%d" out_name i in
    let inst_name = mint_ctx ~ctx:bit_ctx cell_mux.cell_name in
    let ai = if width = 1 then a_name else Printf.sprintf "%s[%d]" a_name i in
    let bi = if width = 1 then b_name else Printf.sprintf "%s[%d]" b_name i in
    let oi = if width = 1 then out_name else Printf.sprintf "%s[%d]" out_name i in
    insts := { cell = cell_mux; inst_name;
               conns = [
                 { pin = "A"; net = ai };
                 { pin = "B"; net = bi };
                 { pin = "S"; net = sel_name };
                 { pin = cell_mux.out_pin; net = oi };
               ] } :: !insts
  done;
  List.rev !insts

(* ── Bit-blasted arithmetic / compare (#103b) ─────────────────────

   ORFS's [read_verilog] is structural-only; it can't ingest raw
   `==`, `<`, `+`, `-` operators.  So when [walk] sees an Op2 the
   gate catalogue doesn't cover, we expand it into AND/OR/XOR/INV
   cells right here.  Each helper returns the result-wire name and
   emits the cells/wires into the supplied lists (`insts`, `wires`).

   The conventions:
     - Multi-bit nets follow the [name[i]] convention shared with
       [blast_op2] / [blast_mux] — width-1 nets are bare names.
     - Helpers take the input nets as already-existing names; they
       only mint new wires for their internal nodes and the result.
     - `pad_to` returns a per-bit accessor for any width — when the
       requested bit index is out of range the result is constant 0
       (a fresh wire we drive with a const-0).  This matters for
       [a == b] where [a] is wider than [b]: the upper bits of the
       narrower side become hardcoded zeroes. *)

let gate_inst ~cell ~ipins ~ins ~out =
  let conns =
    List.map2 (fun p n -> { pin = p; net = n }) ipins ins
    @ [{ pin = cell.out_pin; net = out }] in
  { cell; inst_name = mint cell.cell_name; conns }

let bit_at name w idx =
  if w = 1 then name else Printf.sprintf "%s[%d]" name idx

(* Reduce an OR over a list of single-bit nets.  Empty list ⇒ "1'b0".
   Returns the net name carrying the final OR result + the list of
   instances + new wires. *)
let or_reduce nets =
  let insts = ref [] and wires = ref [] in
  let rec loop = function
    | [] -> ("1'b0", List.rev !insts, !wires)
    | [single] -> (single, List.rev !insts, !wires)
    | a :: b :: rest ->
        let out = mint "or" in
        wires := (out, 1) :: !wires;
        insts := gate_inst ~cell:cell_or
                   ~ipins:cell_or.in_pins ~ins:[a; b] ~out :: !insts;
        loop (out :: rest)
  in
  loop nets

(* Equality: a == b.  Width [w] = max widths of a and b.  Returns
   single-bit result wire name + cells/wires to emit. *)
let gen_eq ~a_name ~a_w ~b_name ~b_w =
  let w = max a_w b_w in
  let insts = ref [] and wires = ref [] in
  let zero_for_pad () =
    let z = mint "z" in
    wires := (z, 1) :: !wires;
    (* Constant zero feeds in via a raw assign — emitted later by the
       cell-Verilog stage when the net appears in [wires] without a
       cell driver.  Simpler approach: emit a buffer of 1'b0. *)
    insts := { cell = cell_buf;
               inst_name = mint "buf";
               conns = [{ pin = "A"; net = "1'b0" };
                        { pin = cell_buf.out_pin; net = z }] } :: !insts;
    z in
  let xor_outs = List.init w (fun i ->
    let ai = if i < a_w then bit_at a_name a_w i else zero_for_pad () in
    let bi = if i < b_w then bit_at b_name b_w i else zero_for_pad () in
    let oi = mint "xor" in
    wires := (oi, 1) :: !wires;
    insts := gate_inst ~cell:cell_xor
               ~ipins:cell_xor.in_pins ~ins:[ai; bi] ~out:oi :: !insts;
    oi) in
  let or_out, or_insts, or_wires = or_reduce xor_outs in
  let result = mint "eq" in
  let inv_inst = gate_inst ~cell:cell_inv
                   ~ipins:cell_inv.in_pins ~ins:[or_out] ~out:result in
  let final_wires = !wires @ or_wires @ [(result, 1)] in
  (result, List.rev !insts @ or_insts @ [inv_inst], final_wires)

(* Build a full-adder from XOR/AND/OR gates.  Inputs (a, b, cin),
   outputs (sum, cout).  Returns sum_net, cout_net, instances, wires.
   [~ctx] is woven into mint so the per-bit nets / cells trace back
   to the RTL output (e.g., "alu_out_bit15").                       *)
let gen_fa ?(ctx="") ~a ~b ~cin () =
  let insts = ref [] and wires = ref [] in
  let add_w n = wires := (n, 1) :: !wires in
  let add_i i = insts := i :: !insts in
  let mk = mint_ctx ~ctx in
  let gi cell ins out =
    { cell; inst_name = mk cell.cell_name;
      conns =
        List.map2 (fun p n -> { pin = p; net = n }) cell.in_pins ins
        @ [{ pin = cell.out_pin; net = out }] }
  in
  let ab = mk "ab" in
  add_w ab;
  add_i (gi cell_xor [a; b] ab);
  let sum = mk "sum" in
  add_w sum;
  add_i (gi cell_xor [ab; cin] sum);
  let aandb = mk "aab" in
  add_w aandb;
  add_i (gi cell_and [a; b] aandb);
  let ab_and_cin = mk "abc" in
  add_w ab_and_cin;
  add_i (gi cell_and [ab; cin] ab_and_cin);
  let cout = mk "co" in
  add_w cout;
  add_i (gi cell_or [aandb; ab_and_cin] cout);
  (sum, cout, List.rev !insts, !wires)

(* N-bit subtractor: a - b = a + ~b + 1.  Returns (sum_bits_msb_first,
   final_cout, all_insts, all_wires). *)
let gen_sub ?(ctx="") ~a_name ~a_w ~b_name ~b_w () =
  let w = max a_w b_w in
  let insts = ref [] and wires = ref [] in
  let inv_b_bits = List.init w (fun i ->
    let bi = if i < b_w then bit_at b_name b_w i else "1'b0" in
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let inv = mint_ctx ~ctx:bit_ctx "nb" in
    wires := (inv, 1) :: !wires;
    insts := { cell = cell_inv;
               inst_name = mint_ctx ~ctx:bit_ctx cell_inv.cell_name;
               conns = [
                 { pin = List.hd cell_inv.in_pins; net = bi };
                 { pin = cell_inv.out_pin; net = inv }] } :: !insts;
    inv) in
  let sums = ref [] and cin = ref "1'b1" in
  for i = 0 to w - 1 do
    let ai = if i < a_w then bit_at a_name a_w i else "1'b0" in
    let bi = List.nth inv_b_bits i in
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let s, co, fa_insts, fa_wires =
      gen_fa ~ctx:bit_ctx ~a:ai ~b:bi ~cin:!cin () in
    sums := s :: !sums;
    insts := List.rev_append fa_insts !insts;
    wires := !wires @ fa_wires;
    cin := co
  done;
  (List.rev !sums, !cin, List.rev !insts, !wires)

(* Less-than: a < b ⇔ subtraction borrows out ⇔ ~cout. *)
let gen_lt ?(ctx="") ~a_name ~a_w ~b_name ~b_w () =
  let _sums, cout, insts, wires =
    gen_sub ~ctx ~a_name ~a_w ~b_name ~b_w () in
  let lt = mint_ctx ~ctx "lt" in
  let inv = { cell = cell_inv;
              inst_name = mint_ctx ~ctx cell_inv.cell_name;
              conns = [
                { pin = List.hd cell_inv.in_pins; net = cout };
                { pin = cell_inv.out_pin; net = lt }] } in
  (lt, insts @ [inv], (lt, 1) :: wires)

(* N-bit ripple-carry adder: a + b.  Same FA chain as gen_sub but
   feeds b directly (no inversion) and cin = 0.  Returns
   (sum_bits_msb_first, final_cout, all_insts, all_wires). *)
let gen_add ?(ctx="") ~a_name ~a_w ~b_name ~b_w () =
  let w = max a_w b_w in
  let insts = ref [] and wires = ref [] in
  let sums = ref [] and cin = ref "1'b0" in
  for i = 0 to w - 1 do
    let ai = if i < a_w then bit_at a_name a_w i else "1'b0" in
    let bi = if i < b_w then bit_at b_name b_w i else "1'b0" in
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let s, co, fa_insts, fa_wires =
      gen_fa ~ctx:bit_ctx ~a:ai ~b:bi ~cin:!cin () in
    sums := s :: !sums;
    insts := List.rev_append fa_insts !insts;
    wires := !wires @ fa_wires;
    cin := co
  done;
  (List.rev !sums, !cin, List.rev !insts, !wires)

(* Brent-Kung prefix-sum adder.  O(log W) carry-tree depth instead
   of O(W) for ripple — for picosoc's 32-bit chains, 96 → 11 carry
   stages.

   Structure:
     1.  Pre-stage:  p_i = a_i ^ b_i ;  g_i = a_i & b_i  for each i.
     2.  Forward sweep — at level k = 0 .. log2(W)-1, every position
         i where (i+1) mod 2^(k+1) = 0 combines with position i-2^k:
              p'_i = p_i & p_{i-2^k}
              g'_i = g_i | (p_i & g_{i-2^k})
         After the forward sweep, g_i carries the true carry-out at
         positions 2^L−1, 2*2^L−1, …  (the "powers of 2 minus 1").
     3.  Backward sweep — fills in the carry at positions skipped by
         the forward sweep.  For k = log2(W)-2 .. 0, every position
         i = 3*2^k - 1, 5*2^k - 1, …  combines with i-2^k.
     4.  Final sum: sum_i = p_i_initial ^ c_{i-1}  where c_{-1} = 0
         and c_i = g_i (after the full tree).

   Same return shape as [gen_add]: (sums LSB-first, cout, insts,
   wires).  Each gate is minted via [mint_ctx], so the encoded
   inst names trace back to (signal, bit, role) for STA reports. *)
let gen_add_brent_kung ?(ctx="") ~a_name ~a_w ~b_name ~b_w () =
  let w = max a_w b_w in
  let insts = ref [] and wires = ref [] in
  let add_w n = wires := (n, 1) :: !wires in
  let add_i i = insts := i :: !insts in
  let mk_gate ~bit_ctx cell ins out =
    add_i { cell;
            inst_name = mint_ctx ~ctx:bit_ctx cell.cell_name;
            conns =
              List.map2 (fun p n -> { pin = p; net = n }) cell.in_pins ins
              @ [{ pin = cell.out_pin; net = out }] }
  in
  (* Step 1: pre-stage *)
  let g = Array.make w "" in
  let p = Array.make w "" in
  let p_init = Array.make w "" in     (* original p, kept for sum *)
  for i = 0 to w - 1 do
    let ai = if i < a_w then bit_at a_name a_w i else "1'b0" in
    let bi = if i < b_w then bit_at b_name b_w i else "1'b0" in
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let gi = mint_ctx ~ctx:bit_ctx "g" in
    let pi = mint_ctx ~ctx:bit_ctx "p" in
    add_w gi; add_w pi;
    mk_gate ~bit_ctx cell_and [ai; bi] gi;
    mk_gate ~bit_ctx cell_xor [ai; bi] pi;
    g.(i) <- gi; p.(i) <- pi; p_init.(i) <- pi
  done;
  (* combine helper: replaces (g.(i), p.(i)) with the prefix-merged
     pair, stamping the new gates with [bit_ctx]. *)
  let combine ~bit_ctx i j =
    let pj = p.(j) and gj = g.(j) in
    let pi = p.(i) and gi = g.(i) in
    let new_p = mint_ctx ~ctx:bit_ctx "pp" in
    let pAg   = mint_ctx ~ctx:bit_ctx "pAg" in
    let new_g = mint_ctx ~ctx:bit_ctx "gg" in
    add_w new_p; add_w pAg; add_w new_g;
    mk_gate ~bit_ctx cell_and [pi; pj] new_p;
    mk_gate ~bit_ctx cell_and [pi; gj] pAg;
    mk_gate ~bit_ctx cell_or  [gi; pAg] new_g;
    p.(i) <- new_p; g.(i) <- new_g
  in
  (* log2 ceil *)
  let log2w =
    let rec l n = if n <= 1 then 0 else 1 + l ((n + 1) / 2) in l w in
  (* Step 2: forward sweep *)
  for k = 0 to log2w - 1 do
    let step = 1 lsl (k + 1) in
    let half = 1 lsl k in
    let i = ref (step - 1) in
    while !i < w do
      let j = !i - half in
      if j >= 0 then
        combine ~bit_ctx:(if ctx = "" then ""
                          else Printf.sprintf "%s_bit%d_fwd%d" ctx !i k) !i j;
      i := !i + step
    done
  done;
  (* Step 3: backward sweep *)
  for k = log2w - 2 downto 0 do
    let step = 1 lsl (k + 1) in
    let half = 1 lsl k in
    let i = ref (3 * half - 1) in
    while !i < w do
      let j = !i - half in
      if j >= 0 then
        combine ~bit_ctx:(if ctx = "" then ""
                          else Printf.sprintf "%s_bit%d_bk%d" ctx !i k) !i j;
      i := !i + step
    done
  done;
  (* Step 4: sum_i = p_init.(i) ^ c_{i-1}; c_{-1} = 0; c_i = g.(i) *)
  let sums = Array.make w "" in
  for i = 0 to w - 1 do
    let cin = if i = 0 then "1'b0" else g.(i - 1) in
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let si = mint_ctx ~ctx:bit_ctx "sum" in
    add_w si;
    mk_gate ~bit_ctx cell_xor [p_init.(i); cin] si;
    sums.(i) <- si
  done;
  let cout = if w > 0 then g.(w - 1) else "1'b0" in
  (Array.to_list sums, cout, List.rev !insts, !wires)

(* Brent-Kung subtractor: sums = a - b, cout = ~borrow.  Same prefix
   tree as [gen_add_brent_kung], but with b inverted and cin = 1 so
   the formula a + ~b + 1 = a - b holds.  Used by [gen_lt_brent_kung]
   below — picosoc's WNS path after the add-swap landed on a 26-bit
   LT comparator inside a ripple gen_sub chain.  *)
let gen_sub_brent_kung ?(ctx="") ~a_name ~a_w ~b_name ~b_w () =
  let w = max a_w b_w in
  let insts = ref [] and wires = ref [] in
  let add_w n = wires := (n, 1) :: !wires in
  let add_i i = insts := i :: !insts in
  let mk_gate ~bit_ctx cell ins out =
    add_i { cell;
            inst_name = mint_ctx ~ctx:bit_ctx cell.cell_name;
            conns =
              List.map2 (fun p n -> { pin = p; net = n }) cell.in_pins ins
              @ [{ pin = cell.out_pin; net = out }] }
  in
  (* Step 1: pre-stage — invert b, then compute g/p with cin=1
     folded in for bit 0:
       p_i = a_i ^ ~b_i ;  g_i = a_i & ~b_i              (i ≥ 1)
       p_0 = a_0 ^ ~b_0 ;  g_0 = (a_0 & ~b_0) | p_0      (cin=1)        *)
  let g = Array.make w "" in
  let p = Array.make w "" in
  let p_init = Array.make w "" in
  let nb = Array.make w "" in
  for i = 0 to w - 1 do
    let ai = if i < a_w then bit_at a_name a_w i else "1'b0" in
    let bi = if i < b_w then bit_at b_name b_w i else "1'b0" in
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let nbi = mint_ctx ~ctx:bit_ctx "nb" in
    add_w nbi;
    mk_gate ~bit_ctx cell_inv [bi] nbi;
    nb.(i) <- nbi;
    let gi_raw = mint_ctx ~ctx:bit_ctx "graw" in
    let pi = mint_ctx ~ctx:bit_ctx "p" in
    add_w gi_raw; add_w pi;
    mk_gate ~bit_ctx cell_and [ai; nbi] gi_raw;
    mk_gate ~bit_ctx cell_xor [ai; nbi] pi;
    let gi =
      if i = 0 then begin
        let g0 = mint_ctx ~ctx:bit_ctx "g" in
        add_w g0;
        mk_gate ~bit_ctx cell_or [gi_raw; pi] g0;
        g0
      end else gi_raw
    in
    g.(i) <- gi; p.(i) <- pi; p_init.(i) <- pi
  done;
  let combine ~bit_ctx i j =
    let pj = p.(j) and gj = g.(j) in
    let pi = p.(i) and gi = g.(i) in
    let new_p = mint_ctx ~ctx:bit_ctx "pp" in
    let pAg   = mint_ctx ~ctx:bit_ctx "pAg" in
    let new_g = mint_ctx ~ctx:bit_ctx "gg" in
    add_w new_p; add_w pAg; add_w new_g;
    mk_gate ~bit_ctx cell_and [pi; pj] new_p;
    mk_gate ~bit_ctx cell_and [pi; gj] pAg;
    mk_gate ~bit_ctx cell_or  [gi; pAg] new_g;
    p.(i) <- new_p; g.(i) <- new_g
  in
  let log2w =
    let rec l n = if n <= 1 then 0 else 1 + l ((n + 1) / 2) in l w in
  for k = 0 to log2w - 1 do
    let step = 1 lsl (k + 1) in
    let half = 1 lsl k in
    let i = ref (step - 1) in
    while !i < w do
      let j = !i - half in
      if j >= 0 then
        combine ~bit_ctx:(if ctx = "" then ""
                          else Printf.sprintf "%s_bit%d_fwd%d" ctx !i k) !i j;
      i := !i + step
    done
  done;
  for k = log2w - 2 downto 0 do
    let step = 1 lsl (k + 1) in
    let half = 1 lsl k in
    let i = ref (3 * half - 1) in
    while !i < w do
      let j = !i - half in
      if j >= 0 then
        combine ~bit_ctx:(if ctx = "" then ""
                          else Printf.sprintf "%s_bit%d_bk%d" ctx !i k) !i j;
      i := !i + step
    done
  done;
  (* Sum: bit 0 = ~p_init.0 (cin=1); bit i = p_init.i ^ g.(i-1)        *)
  let sums = Array.make w "" in
  for i = 0 to w - 1 do
    let bit_ctx =
      if ctx = "" then "" else Printf.sprintf "%s_bit%d" ctx i in
    let si = mint_ctx ~ctx:bit_ctx "sum" in
    add_w si;
    if i = 0 then
      mk_gate ~bit_ctx cell_inv [p_init.(0)] si
    else
      mk_gate ~bit_ctx cell_xor [p_init.(i); g.(i - 1)] si;
    sums.(i) <- si
  done;
  let cout = if w > 0 then g.(w - 1) else "1'b1" in
  (Array.to_list sums, cout, List.rev !insts, !wires)

(* Brent-Kung less-than: lt = ~cout(a - b).  Replaces the ripple
   gen_lt → gen_sub when SV_DECOMP_ARCH_SWAP=1 + width threshold.    *)
let gen_lt_brent_kung ?(ctx="") ~a_name ~a_w ~b_name ~b_w () =
  let _sums, cout, insts, wires =
    gen_sub_brent_kung ~ctx ~a_name ~a_w ~b_name ~b_w () in
  let lt = mint_ctx ~ctx "lt" in
  let inv = { cell = cell_inv;
              inst_name = mint_ctx ~ctx cell_inv.cell_name;
              conns = [
                { pin = List.hd cell_inv.in_pins; net = cout };
                { pin = cell_inv.out_pin; net = lt }] } in
  (lt, insts @ [inv], (lt, 1) :: wires)

(* Like [gen_add] but takes pre-built bit lists (LSB-first) instead
   of named-bus operands.  Used by [gen_mul] — partial-product rows
   are sets of fresh 1-bit nets, not bit-selects of a declared bus.
   Returns (sum_bits_lsb_first, final_cout, insts, wires). *)
let gen_add_bits ~(a_bits : string list) ~(b_bits : string list) =
  let w = max (List.length a_bits) (List.length b_bits) in
  let pad bits =
    let n = List.length bits in
    if n >= w then List.filteri (fun i _ -> i < w) bits
    else bits @ List.init (w - n) (fun _ -> "1'b0") in
  let a = Array.of_list (pad a_bits) in
  let b = Array.of_list (pad b_bits) in
  let insts = ref [] and wires = ref [] in
  let sums = ref [] and cin = ref "1'b0" in
  for i = 0 to w - 1 do
    let s, co, fa_insts, fa_wires = gen_fa ~a:a.(i) ~b:b.(i) ~cin:!cin () in
    sums := s :: !sums;
    insts := List.rev_append fa_insts !insts;
    wires := !wires @ fa_wires;
    cin := co
  done;
  (List.rev !sums, !cin, List.rev !insts, !wires)

(* Array multiplier: out = a * b, output width = a_w + b_w.  Builds
   the b_w partial-product rows via AND gates then sums them with
   repeated [gen_add_bits].  Linear-depth (O(b_w)) — the Wallace and
   Dadda alternates reduce that to O(log b_w) and are gated by the
   verify-arch certificates that arch_verify lays down (#80).

   Returns (out_bits_msb_first, ignored_final_cout, insts, wires) so
   the call site can splice it in alongside [gen_add] / [gen_sub]
   without special-casing widths.  The "ignored cout" is always 0 in
   a well-formed multiplier (a*b < 2^(a_w+b_w)). *)
let gen_mul_array_bits ~(a_bits : string list) ~(b_bits : string list) =
  let a_w = List.length a_bits in
  let b_w = List.length b_bits in
  let out_w = a_w + b_w in
  let insts = ref [] and wires = ref [] in
  let a_arr = Array.of_list a_bits in
  let b_arr = Array.of_list b_bits in
  (* Build the b_w partial-product rows, each a list of out_w bit
     names LSB-first. *)
  let pps = List.init b_w (fun i ->
    let bi = b_arr.(i) in
    List.init out_w (fun j ->
      if j < i || j >= i + a_w then "1'b0"
      else
        let aj = a_arr.(j - i) in
        let pij = mint "pp" in
        wires := (pij, 1) :: !wires;
        insts := gate_inst ~cell:cell_and ~ipins:cell_and.in_pins
                   ~ins:[aj; bi] ~out:pij :: !insts;
        pij)
  ) in
  let acc = ref (List.hd pps) in
  List.iter (fun pp ->
    let s, _co, ai, aw = gen_add_bits ~a_bits:!acc ~b_bits:pp in
    insts := !insts @ ai;
    wires := !wires @ aw;
    acc := s) (List.tl pps);
  (!acc, "1'b0", List.rev !insts, !wires)

(* Auto-cascade for wide multipliers (task #126).  Z3's monolithic
   bv-mul SMT performance falls off a cliff past ~16-bit operands;
   manual decompositions like cascade_mac proved we can get arbitrary
   widths through the miter by splitting algebraically:

     A · B  where  A = A_hi · 2^k + A_lo
                   B = B_hi · 2^k + B_lo
                   k = ⌈max(a_w, b_w) / 2⌉
       = A_hi·B_hi · 2^(2k)
       + (A_hi·B_lo + A_lo·B_hi) · 2^k
       + A_lo·B_lo

   Each sub-multiplication has operand widths half as wide; the
   recursion bottoms out at the [gen_mul_array_bits] direct array
   form when both operands fit under SV_DECOMP_MUL_CASCADE_THRESHOLD
   (default 16).  Setting the threshold to 0 disables the cascade
   entirely (forces array everywhere); setting it to ∞ via a large
   value matches the historical pre-#126 behaviour.

   The recursion preserves the same (out_bits_lsb_first, cout, insts,
   wires) interface, so callers don't need to know they're getting a
   cascade.  Each leaf array-mul becomes its own block in [Block_tag],
   so downstream verify-arch / Z3 miter can substitute per-leaf certs.   *)
let cascade_threshold () =
  match Sys.getenv_opt "SV_DECOMP_MUL_CASCADE_THRESHOLD" with
  | Some s -> (try int_of_string s with _ -> 16)
  | None -> 16

(* Bits-form shifter: prepend [shift] copies of "1'b0" then truncate
   or pad to [out_w].  No gates — pure renaming.                     *)
let shift_left_bits ~bits ~shift ~out_w =
  let n = List.length bits in
  let total = shift + n in
  let padded =
    List.init shift (fun _ -> "1'b0") @ bits in
  if total >= out_w then
    List.filteri (fun i _ -> i < out_w) padded
  else
    padded @ List.init (out_w - total) (fun _ -> "1'b0")

let rec gen_mul_bits ~(a_bits : string list) ~(b_bits : string list)
  : string list * string * instance list * (string * int) list =
  let a_w = List.length a_bits and b_w = List.length b_bits in
  let out_w = a_w + b_w in
  let thresh = cascade_threshold () in
  (* Bottom: both operands under threshold → direct array form. *)
  if thresh = 0 || (a_w <= thresh && b_w <= thresh) then
    gen_mul_array_bits ~a_bits ~b_bits
  else begin
    let k_a = (a_w + 1) / 2 and k_b = (b_w + 1) / 2 in
    let k = max k_a k_b in
    let k = min k (min a_w b_w) in
    if k = 0 || k = a_w || k = b_w then
      gen_mul_array_bits ~a_bits ~b_bits
    else begin
      let a_lo = List.filteri (fun i _ -> i < k) a_bits in
      let a_hi = List.filteri (fun i _ -> i >= k) a_bits in
      let b_lo = List.filteri (fun i _ -> i < k) b_bits in
      let b_hi = List.filteri (fun i _ -> i >= k) b_bits in
      (* Four sub-products. *)
      let p_ll, _, i_ll, w_ll = gen_mul_bits ~a_bits:a_lo ~b_bits:b_lo in
      let p_hl, _, i_hl, w_hl = gen_mul_bits ~a_bits:a_hi ~b_bits:b_lo in
      let p_lh, _, i_lh, w_lh = gen_mul_bits ~a_bits:a_lo ~b_bits:b_hi in
      let p_hh, _, i_hh, w_hh = gen_mul_bits ~a_bits:a_hi ~b_bits:b_hi in
      let all_insts = ref (i_ll @ i_hl @ i_lh @ i_hh) in
      let all_wires = ref (w_ll @ w_hl @ w_lh @ w_hh) in
      (* Position each sub-product:
           P_LL at offset 0
           P_HL, P_LH at offset k
           P_HH at offset 2k                                          *)
      let shifted_ll = shift_left_bits ~bits:p_ll ~shift:0 ~out_w in
      let shifted_hl = shift_left_bits ~bits:p_hl ~shift:k ~out_w in
      let shifted_lh = shift_left_bits ~bits:p_lh ~shift:k ~out_w in
      let shifted_hh = shift_left_bits ~bits:p_hh ~shift:(2 * k) ~out_w in
      let add_pair x y =
        let s, _co, is_, ws_ = gen_add_bits ~a_bits:x ~b_bits:y in
        all_insts := !all_insts @ is_;
        all_wires := !all_wires @ ws_;
        s in
      let s1 = add_pair shifted_ll shifted_hl in
      let s2 = add_pair s1 shifted_lh in
      let s3 = add_pair s2 shifted_hh in
      (s3, "1'b0", !all_insts, !all_wires)
    end
  end

let gen_mul ~a_name ~a_w ~b_name ~b_w =
  let a_bits = List.init a_w (fun i -> bit_at a_name a_w i) in
  let b_bits = List.init b_w (fun i -> bit_at b_name b_w i) in
  let out_bits_lsb, cout, insts, wires = gen_mul_bits ~a_bits ~b_bits in
  (List.rev out_bits_lsb, cout, insts, wires)

(* DFF mapping.  hardcaml's Reg has a [register] record with
   clock, optional reset, optional clear, optional enable.  For
   our subset:
     - sync reset (no Reg.reset)            -> DFF_X1
     - async reset (Reg.reset present)     -> DFFR_X1
     - enable (Reg.enable present)         -> emit a mux ahead of D
   per-bit blast as for combinational. *)
let blast_reg ~r_d_name ~r_clk_name ?r_rst_name ~out_name ~width () =
  let insts = ref [] in
  let cell = match r_rst_name with Some _ -> cell_dffr | None -> cell_dff in
  for i = 0 to width - 1 do
    let inst_name = mint cell.cell_name in
    let di = if width = 1 then r_d_name else Printf.sprintf "%s[%d]" r_d_name i in
    let qi = if width = 1 then out_name else Printf.sprintf "%s[%d]" out_name i in
    let conns = [
      { pin = "D"; net = di };
      { pin = "CK"; net = r_clk_name };
      { pin = cell.out_pin; net = qi };
    ] in
    let conns =
      match r_rst_name with
      | Some rn -> conns @ [{ pin = "RN"; net = rn }]
      | None -> conns in
    insts := { cell; inst_name; conns } :: !insts
  done;
  List.rev !insts

(* The main mapper: walk a signal once, emitting cells for every
   Op2/Not/Mux/Reg encountered.  Memoised so a fanout-shared
   subgraph mints one set of cells.

   [ctx.emit] receives each instance.
   [ctx.assign] receives raw assigns for unmapped ops. *)

type emit_ctx = {
  mutable insts   : instance list;
  mutable assigns : (string * string) list;
  mutable wires   : (string * int) list;
  visited : (Hardcaml.Signal.Type.Uid.t, string) Hashtbl.t;
}

let signal_op_kind = function
  | Hardcaml.Signal.Type.Signal_and -> Some cell_and
  | Hardcaml.Signal.Type.Signal_or  -> Some cell_or
  | Hardcaml.Signal.Type.Signal_xor -> Some cell_xor
  | _ -> None

let rec walk ctx sig_ =
  if Hardcaml.Signal.Type.is_empty sig_ then "1'b0"
  else
  let uid = Hardcaml.Signal.Type.uid sig_ in
  match Hashtbl.find_opt ctx.visited uid with
  | Some n -> n
  | None ->
      let out_name =
        try
          match names sig_ with
          | n :: _ -> n
          | [] -> mint "n"
        with _ -> mint "n"
      in
      Hashtbl.add ctx.visited uid out_name;
      let w = width sig_ in
      (match sig_ with
       | Empty -> ()
       | Const { constant; _ } ->
           (* Constants don't need cells; emit as raw Verilog. *)
           let v = Hardcaml.Bits.to_string constant in
           ctx.wires <- (out_name, w) :: ctx.wires;
           ctx.assigns <- (out_name,
             Printf.sprintf "%d'b%s" w v) :: ctx.assigns
       | Op2 { op; arg_a; arg_b; _ } ->
           let an = walk ctx arg_a and bn = walk ctx arg_b in
           let a_w = width arg_a and b_w = width arg_b in
           ctx.wires <- (out_name, w) :: ctx.wires;
           let absorb_helper insts wires_ =
             ctx.insts <- List.rev_append insts ctx.insts;
             ctx.wires <- wires_ @ ctx.wires
           in
           (match signal_op_kind op with
            | Some cell ->
                let inst_list = blast_op2 ~cell ~out_name ~width:w
                                  ~a_name:an ~b_name:bn ~a_w ~b_w in
                ctx.insts <- inst_list @ ctx.insts
            | None ->
                (match op with
                 | Hardcaml.Signal.Type.Signal_eq ->
                     let res, insts, wires_ =
                       Block_tag.with_block ~kind:Block_tag.EQ
                         ~signal:out_name ~width:(max a_w b_w)
                         ~arch:"reduce_or"
                         (fun _ -> gen_eq ~a_name:an ~a_w ~b_name:bn ~b_w) in
                     absorb_helper insts wires_;
                     ctx.assigns <- (out_name, res) :: ctx.assigns
                 | Signal_sub | Signal_add ->
                     let block_kind = match op with
                       | Signal_add -> Block_tag.ADD
                       | _          -> Block_tag.SUB in
                     let want_swap =
                       Sys.getenv_opt "SV_DECOMP_ARCH_SWAP" = Some "1" in
                     let min_w =
                       match Sys.getenv_opt "SV_DECOMP_ARCH_MIN_W" with
                       | Some s -> (try int_of_string s with _ -> 8)
                       | None -> 8 in
                     let use_bk = want_swap && (max a_w b_w) >= min_w in
                     let arch = if use_bk then "brent_kung" else "ripple" in
                     let sums, _co, insts, wires_ =
                       Block_tag.with_block ~kind:block_kind
                         ~signal:out_name ~width:(max a_w b_w) ~arch
                         (fun _ ->
                           match use_bk, op with
                           | true,  Signal_add ->
                               gen_add_brent_kung ~ctx:out_name
                                 ~a_name:an ~a_w ~b_name:bn ~b_w ()
                           | true,  _ (* Signal_sub *) ->
                               gen_sub_brent_kung ~ctx:out_name
                                 ~a_name:an ~a_w ~b_name:bn ~b_w ()
                           | false, Signal_add ->
                               gen_add ~ctx:out_name
                                 ~a_name:an ~a_w ~b_name:bn ~b_w ()
                           | false, _ ->
                               gen_sub ~ctx:out_name
                                 ~a_name:an ~a_w ~b_name:bn ~b_w ()) in
                     absorb_helper insts wires_;
                     let sums = let n = min w (List.length sums) in
                                List.filteri (fun i _ -> i < n) sums in
                     if w = 1 then
                       (match sums with
                        | [s] -> ctx.assigns <- (out_name, s) :: ctx.assigns
                        | _ -> ())
                     else begin
                       List.iter (fun s ->
                         ctx.wires <- (s, 1) :: ctx.wires) sums;
                       let concat_rhs =
                         "{" ^ String.concat ", " (List.rev sums) ^ "}" in
                       ctx.assigns <- (out_name, concat_rhs) :: ctx.assigns
                     end
                 | Signal_lt ->
                     let want_swap =
                       Sys.getenv_opt "SV_DECOMP_ARCH_SWAP" = Some "1" in
                     let min_w =
                       match Sys.getenv_opt "SV_DECOMP_ARCH_MIN_W" with
                       | Some s -> (try int_of_string s with _ -> 8)
                       | None -> 8 in
                     let use_bk = want_swap && (max a_w b_w) >= min_w in
                     let arch =
                       if use_bk then "brent_kung_then_invert"
                       else "sub_then_invert" in
                     let res, insts, wires_ =
                       Block_tag.with_block ~kind:Block_tag.LT
                         ~signal:out_name ~width:(max a_w b_w) ~arch
                         (fun _ ->
                           if use_bk then
                             gen_lt_brent_kung ~ctx:out_name
                               ~a_name:an ~a_w ~b_name:bn ~b_w ()
                           else
                             gen_lt ~ctx:out_name
                               ~a_name:an ~a_w ~b_name:bn ~b_w ()) in
                     absorb_helper insts wires_;
                     ctx.assigns <- (out_name, res) :: ctx.assigns
                 | Signal_mulu | Signal_muls ->
                     (* Array multiplier — Wallace/Dadda are the
                        cert-gated swap targets sitting on top of
                        the same partial-product + gen_add primitives. *)
                     let sums, _co, insts, wires_ =
                       Block_tag.with_block ~kind:Block_tag.MUL
                         ~signal:out_name ~width:(a_w + b_w) ~arch:"array"
                         (fun _ -> gen_mul ~a_name:an ~a_w ~b_name:bn ~b_w) in
                     absorb_helper insts wires_;
                     (* gen_mul returns msb-first; truncate to LHS w. *)
                     let sums =
                       let n = List.length sums in
                       if n <= w then sums
                       else List.filteri (fun i _ -> i >= n - w) sums in
                     if w = 1 then
                       (match sums with
                        | [s] -> ctx.assigns <- (out_name, s) :: ctx.assigns
                        | _ -> ())
                     else begin
                       List.iter (fun s ->
                         ctx.wires <- (s, 1) :: ctx.wires) sums;
                       let concat_rhs =
                         "{" ^ String.concat ", " sums ^ "}" in
                       ctx.assigns <- (out_name, concat_rhs) :: ctx.assigns
                     end
                 | _ ->
                     Printf.eprintf
                       "[lib_map] WARN: unmapped op — emitting raw \
                        Verilog (read_verilog will reject)\n";
                     ctx.assigns <- (out_name,
                       Printf.sprintf "%s %s" an bn) :: ctx.assigns))
       | Not { arg; _ } ->
           let an = walk ctx arg in
           ctx.wires <- (out_name, w) :: ctx.wires;
           ctx.insts <-
             blast_unary ~cell:cell_inv ~out_name ~width:w
               ~a_name:an ~a_w:(width arg) @ ctx.insts
       | Mux { select; cases = [a; b]; _ } ->
           let sn = walk ctx select in
           let an = walk ctx a and bn = walk ctx b in
           ctx.wires <- (out_name, w) :: ctx.wires;
           ctx.insts <-
             blast_mux ~out_name ~width:w
               ~sel_name:sn ~a_name:an ~b_name:bn @ ctx.insts
       | Mux { select; cases; _ } ->
           (* N-way mux per output bit: cascaded chain of 2:1
              muxes, one stage per select bit, using fresh 1-bit
              intermediate wires.  Final stage's `.Z` drives
              `out[j]` directly via per-bit assigns — Verible's
              pre-scan in extract_assign auto-promotes the
              indexed-LHS target into array_names so
              merge_array_writes consolidates them into a single
              full-bus assign for the miter, while OpenROAD's
              read_verilog sees them as ordinary per-bit drives. *)
           let sn = walk ctx select in
           let case_names = List.map (walk ctx) cases in
           let sel_w = width select in
           let n = List.length cases in
           ctx.wires <- (out_name, w) :: ctx.wires;
           let final_bits = ref [] in
           for j = 0 to w - 1 do
             let bit_of_case k =
               let cn = List.nth case_names k in
               let cw = width (List.nth cases k) in
               if cw = 1 then cn else Printf.sprintf "%s[%d]" cn j in
             let level0 = List.init n bit_of_case in
             let rec reduce level lst =
               match lst with
               | [] -> "1'b0"
               | [x] -> x
               | _ ->
                   let sel_bit = if sel_w = 1 then sn
                                 else Printf.sprintf "%s[%d]" sn level in
                   let rec pair = function
                     | [] -> []
                     | [a] -> [a]
                     | a :: b :: rest ->
                         let mid = mint "t" in
                         ctx.wires <- (mid, 1) :: ctx.wires;
                         let inst_list = blast_mux ~out_name:mid ~width:1
                           ~sel_name:sel_bit ~a_name:a ~b_name:b in
                         ctx.insts <- inst_list @ ctx.insts;
                         mid :: pair rest in
                   reduce (level + 1) (pair lst)
             in
             final_bits := reduce 0 level0 :: !final_bits
           done;
           List.iteri (fun i b ->
             let oi = if w = 1 then out_name
                      else Printf.sprintf "%s[%d]" out_name (w - 1 - i) in
             ctx.assigns <- (oi, b) :: ctx.assigns
           ) !final_bits
       | Wire { driver; _ } ->
           (* For input ports, the wire's driver is Empty — leave
              the wire alone, it's driven externally.  Otherwise
              emit a pass-through assign if the names differ. *)
           if not (Hardcaml.Signal.Type.is_empty !driver) then begin
             let dn = walk ctx !driver in
             ctx.wires <- (out_name, w) :: ctx.wires;
             if dn <> out_name then
               ctx.assigns <- (out_name, dn) :: ctx.assigns
           end
       | Cat { args; _ } ->
           let parts = List.map (walk ctx) args in
           ctx.wires <- (out_name, w) :: ctx.wires;
           ctx.assigns <- (out_name,
             "{" ^ String.concat ", " parts ^ "}") :: ctx.assigns
       | Select { arg; high; low; _ } ->
           let an = walk ctx arg in
           ctx.wires <- (out_name, w) :: ctx.wires;
           let sel = if high = low then Printf.sprintf "%s[%d]" an high
                     else Printf.sprintf "%s[%d:%d]" an high low in
           ctx.assigns <- (out_name, sel) :: ctx.assigns
       | Reg { register; d; _ } ->
           let dn = walk ctx d in
           let clk_n = walk ctx register.reg_clock in
           (* Empty signals in [reg_reset] / [reg_clear] mean no
              such port — the BIR's sync resets land in [reg_clear]
              if at all, and we steered them to the data-path
              instead.  Async reset shows up as a non-Empty
              [reg_reset]. *)
           let rst_n =
             if Hardcaml.Signal.Type.is_empty register.reg_reset
             then None
             else Some (walk ctx register.reg_reset) in
           ctx.wires <- (out_name, w) :: ctx.wires;
           ctx.insts <-
             blast_reg ~r_d_name:dn ~r_clk_name:clk_n ?r_rst_name:rst_n
               ~out_name ~width:w () @ ctx.insts
       | Multiport_mem _ | Mem_read_port _ | Inst _ ->
           (* These would each need their own mapper.  Skipped
              for the gcd-class subset. *)
           ());
      out_name

(* ── Top-level: build a netlist for a Hardcaml.Circuit.t. ───── *)

(* Replace constant drivers with explicit tie cells.  Without this,
   OpenROAD's [read_verilog] sees a net driven by `1'b1` / `1'b0`
   (or a wider constant literal), tags the resulting tie net as
   POWER, and TritonRoute then refuses to route it
   ("Net foo of signal type POWER is not routable").

   Two rewrites:
     - Pin connections: literal `1'bN` is replaced by the shared
       `_tie_X_` wire driven by a single LOGIC1/LOGIC0 instance.
     - Assigns whose RHS is a sized constant (e.g. `32'b00..0`):
       expanded to a concat of per-bit tie nets, so the net's
       driver is the LOGIC cell rather than the literal. *)
let bin_const_re = Str.regexp "^\\([0-9]+\\)'b\\([01]+\\)$"
let parse_bin_const s =
  if Str.string_match bin_const_re s 0
  then Some (int_of_string (Str.matched_group 1 s),
             Str.matched_group 2 s)
  else None

let tie_resolve (insts, assigns) =
  let needs_hi = ref false and needs_lo = ref false in
  let scan_pin n =
    if n = "1'b1" then (needs_hi := true; "_tie_hi_")
    else if n = "1'b0" then (needs_lo := true; "_tie_lo_")
    else n in
  let insts' = List.map (fun (i : instance) ->
    let conns = List.map (fun c ->
      { c with net = scan_pin c.net }) i.conns in
    { i with conns }) insts in
  let assigns' = List.map (fun (lhs, rhs) ->
    match parse_bin_const rhs with
    | Some (1, "1") -> needs_hi := true; (lhs, "_tie_hi_")
    | Some (1, "0") -> needs_lo := true; (lhs, "_tie_lo_")
    | Some (_w, bits) ->
        (* Multi-bit constant: emit a concat of tie nets, MSB first
           (matches Verilog's `{}` ordering and the bits string). *)
        let parts = String.to_seq bits |> List.of_seq |> List.map (fun c ->
          if c = '1' then (needs_hi := true; "_tie_hi_")
          else (needs_lo := true; "_tie_lo_")) in
        (lhs, "{" ^ String.concat ", " parts ^ "}")
    | None -> (lhs, rhs)
  ) assigns in
  let extras = ref [] in
  if !needs_hi then
    extras := { cell = cell_logic1;
                inst_name = "_tie_hi_inst_";
                conns = [{ pin = "Z"; net = "_tie_hi_" }] } :: !extras;
  if !needs_lo then
    extras := { cell = cell_logic0;
                inst_name = "_tie_lo_inst_";
                conns = [{ pin = "Z"; net = "_tie_lo_" }] } :: !extras;
  let extra_wires =
    (if !needs_hi then [("_tie_hi_", 1)] else [])
    @ (if !needs_lo then [("_tie_lo_", 1)] else []) in
  (!extras @ insts', assigns', extra_wires)

(* ── Dead-code elimination on the netlist (#111) ────────────────

   Backward reachability from the live root set (module outputs +
   nets used by child instance pins).  Drops cells and assigns
   whose driven net is unreachable.  ORFS's [eliminate_dead_logic]
   already does this downstream, but it's faster, smaller netlists,
   and shorter ORFS time when we don't ship the dead bits in the
   first place.  Typical effect on gcd: 334 → ~240 cells. *)

let const_re = Str.regexp "[0-9]+'[bdhoBDHO][0-9a-fA-FxXzZ_]+"
let id_re    = Str.regexp "[_$a-zA-Z][_$a-zA-Z0-9]*\\(\\[[0-9]+\\(:[0-9]+\\)?\\]\\)?"

let strip_consts s = Str.global_replace const_re " " s

let extract_idents s =
  let s = strip_consts s in
  let acc = ref [] in
  let pos = ref 0 in
  let len = String.length s in
  let rec loop () =
    if !pos >= len then ()
    else
      try
        let _ = Str.search_forward id_re s !pos in
        let full = Str.matched_string s in
        let bare =
          (* Strip any [...] suffix to also yield the base name. *)
          try
            let i = String.index full '[' in
            String.sub full 0 i
          with Not_found -> full in
        acc := full :: !acc;
        if full <> bare then acc := bare :: !acc;
        pos := Str.match_end ();
        loop ()
      with Not_found -> ()
  in
  loop ();
  !acc

let dce ~root_nets (nl : netlist) : netlist =
  let live = Hashtbl.create 256 in
  let mark x = Hashtbl.replace live x () in
  let base_of n =
    try
      let i = String.index n '[' in
      String.sub n 0 i
    with Not_found -> n in
  let live_for n =
    Hashtbl.mem live n || Hashtbl.mem live (base_of n) in
  let bus_widths = Hashtbl.create 64 in
  List.iter (fun (n, w) -> Hashtbl.replace bus_widths n w) nl.wires;
  List.iter (fun (n, w) -> Hashtbl.replace bus_widths n w) nl.inputs;
  List.iter (fun (n, w) -> Hashtbl.replace bus_widths n w) nl.outputs;

  let mark_with_bits x =
    mark x;
    (* If x is a bare bus name, expand to per-bit so cells driving
       individual bits survive even when the upstream reference is
       to the whole bus (e.g. via a concat assign). *)
    let bare = base_of x in
    if bare = x then
      match Hashtbl.find_opt bus_widths bare with
      | Some w when w > 1 ->
          for i = 0 to w - 1 do
            mark (Printf.sprintf "%s[%d]" bare i)
          done
      | _ -> () in
  List.iter mark_with_bits root_nets;
  List.iter (fun (name, w) ->
    if w > 1 then
      for i = 0 to w - 1 do
        mark (Printf.sprintf "%s[%d]" name i)
      done
  ) nl.outputs;

  let by_out_net = Hashtbl.create (List.length nl.insts) in
  List.iter (fun (inst : instance) ->
    List.iter (fun (c : pin_conn) ->
      if c.pin = inst.cell.out_pin then
        Hashtbl.add by_out_net c.net inst
    ) inst.conns
  ) nl.insts;
  (* Build a base-name index so a live bus name finds cells driving
     any of its bits. *)
  let by_base_out_net = Hashtbl.create (List.length nl.insts) in
  List.iter (fun (inst : instance) ->
    List.iter (fun (c : pin_conn) ->
      if c.pin = inst.cell.out_pin then
        Hashtbl.add by_base_out_net (base_of c.net) inst
    ) inst.conns
  ) nl.insts;

  (* Same for assigns — index by base name. *)
  let assigns_by_lhs = Hashtbl.create (List.length nl.assigns) in
  let assigns_by_base = Hashtbl.create (List.length nl.assigns) in
  List.iter (fun (lhs, rhs) ->
    Hashtbl.add assigns_by_lhs lhs rhs;
    Hashtbl.add assigns_by_base (base_of lhs) rhs
  ) nl.assigns;

  let queue = Queue.create () in
  Hashtbl.iter (fun k () -> Queue.push k queue) live;
  while not (Queue.is_empty queue) do
    let net = Queue.pop queue in
    let push x =
      if not (Hashtbl.mem live x) then (mark_with_bits x; Queue.push x queue) in
    let visit_inst (inst : instance) =
      List.iter (fun (c : pin_conn) ->
        if c.pin <> inst.cell.out_pin then
          List.iter push (extract_idents c.net)
      ) inst.conns in
    List.iter visit_inst (Hashtbl.find_all by_out_net net);
    let bare = base_of net in
    if bare <> net then
      List.iter visit_inst (Hashtbl.find_all by_base_out_net bare);
    List.iter (fun rhs -> List.iter push (extract_idents rhs))
      (Hashtbl.find_all assigns_by_lhs net);
    if bare <> net then
      List.iter (fun rhs -> List.iter push (extract_idents rhs))
        (Hashtbl.find_all assigns_by_base bare)
  done;

  let inst_alive (i : instance) =
    List.exists (fun (c : pin_conn) ->
      c.pin = i.cell.out_pin && live_for c.net) i.conns in
  let assign_alive (lhs, _) = live_for lhs in
  let wire_alive (n, _) = live_for n in
  {
    nl with
    insts   = List.filter inst_alive nl.insts;
    assigns = List.filter assign_alive nl.assigns;
    wires   = List.filter wire_alive nl.wires;
  }

(* When the netlist contains `assign port = _n_X_;` where `port` is
   a real output and `_n_X_` is an internal hardcaml-minted alias,
   rewrite all references to `_n_X_` (including per-bit slices) to
   reference `port` directly, then drop the assign.  This makes the
   FF's Q net carry the port name — required by the Z3 miter, which
   matches registers across designs by name.  Inert for other
   downstream tools. *)
let alias_resolve ~outputs (nl : netlist) : netlist =
  let aliases = Hashtbl.create 16 in
  let out_set = Hashtbl.create 16 in
  List.iter (fun (n, _) -> Hashtbl.replace out_set n ()) outputs;
  let kept_assigns = List.filter (fun (lhs, rhs) ->
    if Hashtbl.mem out_set lhs
       (* RHS must be a single bare identifier — not a slice, concat,
          or constant.  Use a strict regex to avoid catching e.g.
          `assign out = {a, b};`. *)
       && Str.string_match
            (Str.regexp "^[_$a-zA-Z][_$a-zA-Z0-9]*$") rhs 0
    then begin
      Hashtbl.replace aliases rhs lhs;
      false
    end else true
  ) nl.assigns in
  if Hashtbl.length aliases = 0 then nl
  else
    let rewrite_id id =
      (* Match `name` or `name[i]` / `name[hi:lo]` and rewrite name. *)
      try
        let bracket = String.index id '[' in
        let bare = String.sub id 0 bracket in
        let suffix = String.sub id bracket (String.length id - bracket) in
        match Hashtbl.find_opt aliases bare with
        | Some new_name -> new_name ^ suffix
        | None -> id
      with Not_found ->
        (match Hashtbl.find_opt aliases id with
         | Some new_name -> new_name
         | None -> id) in
    let rewrite_net s =
      (* Replace each bare identifier matched by id_re with its
         alias rewrite. *)
      let buf = Buffer.create (String.length s) in
      let pos = ref 0 in
      let len = String.length s in
      let rec loop () =
        if !pos >= len then ()
        else
          try
            let mstart = Str.search_forward id_re s !pos in
            Buffer.add_substring buf s !pos (mstart - !pos);
            let full = Str.matched_string s in
            Buffer.add_string buf (rewrite_id full);
            pos := Str.match_end ();
            loop ()
          with Not_found ->
            Buffer.add_substring buf s !pos (len - !pos)
      in
      loop ();
      Buffer.contents buf in
    let rewrite_inst (i : instance) =
      { i with conns = List.map (fun c ->
          { c with net = rewrite_net c.net }) i.conns } in
    let rewrite_assign (lhs, rhs) = (rewrite_id lhs, rewrite_net rhs) in
    {
      nl with
      insts   = List.map rewrite_inst nl.insts;
      assigns = List.map rewrite_assign kept_assigns;
      wires   = List.filter (fun (n, _) -> not (Hashtbl.mem aliases n)) nl.wires;
    }

let map_circuit (circuit : Hardcaml.Circuit.t) =
  next_id := 0;
  let ctx = {
    insts = []; assigns = []; wires = [];
    visited = Hashtbl.create 1024;
  } in
  (* Walk every output, which transitively visits every reachable signal. *)
  let outs = Hardcaml.Circuit.outputs circuit in
  List.iter (fun out ->
    let _ = walk ctx out in ()) outs;
  let inputs = List.map (fun s -> (net_for_signal s, width s))
                 (Hardcaml.Circuit.inputs circuit) in
  let outputs = List.map (fun s -> (net_for_signal s, width s)) outs in
  let raw_insts = List.rev ctx.insts in
  let raw_assigns = List.rev ctx.assigns in
  let final_insts, final_assigns, tie_wires =
    tie_resolve (raw_insts, raw_assigns) in
  let pre_alias = {
    inputs;
    outputs;
    wires =
      List.filter (fun (n, _) ->
        not (List.mem_assoc n inputs) && not (List.mem_assoc n outputs))
        (ctx.wires @ tie_wires);
    insts = final_insts;
    assigns = final_assigns;
  } in
  alias_resolve ~outputs pre_alias
