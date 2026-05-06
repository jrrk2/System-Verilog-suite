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
    let inst_name = mint cell.cell_name in
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
    let inst_name = mint cell.cell_name in
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
    let inst_name = mint cell_mux.cell_name in
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
   outputs (sum, cout).  Returns sum_net, cout_net, instances, wires. *)
let gen_fa ~a ~b ~cin =
  let insts = ref [] and wires = ref [] in
  let add_w n = wires := (n, 1) :: !wires in
  let add_i i = insts := i :: !insts in
  let ab = mint "ab" in
  add_w ab;
  add_i (gate_inst ~cell:cell_xor ~ipins:cell_xor.in_pins
           ~ins:[a; b] ~out:ab);
  let sum = mint "sum" in
  add_w sum;
  add_i (gate_inst ~cell:cell_xor ~ipins:cell_xor.in_pins
           ~ins:[ab; cin] ~out:sum);
  let aandb = mint "aab" in
  add_w aandb;
  add_i (gate_inst ~cell:cell_and ~ipins:cell_and.in_pins
           ~ins:[a; b] ~out:aandb);
  let ab_and_cin = mint "abc" in
  add_w ab_and_cin;
  add_i (gate_inst ~cell:cell_and ~ipins:cell_and.in_pins
           ~ins:[ab; cin] ~out:ab_and_cin);
  let cout = mint "co" in
  add_w cout;
  add_i (gate_inst ~cell:cell_or ~ipins:cell_or.in_pins
           ~ins:[aandb; ab_and_cin] ~out:cout);
  (sum, cout, List.rev !insts, !wires)

(* N-bit subtractor: a - b = a + ~b + 1.  Returns (sum_bits_msb_first,
   final_cout, all_insts, all_wires). *)
let gen_sub ~a_name ~a_w ~b_name ~b_w =
  let w = max a_w b_w in
  let insts = ref [] and wires = ref [] in
  let inv_b_bits = List.init w (fun i ->
    let bi = if i < b_w then bit_at b_name b_w i else "1'b0" in
    let inv = mint "nb" in
    wires := (inv, 1) :: !wires;
    insts := gate_inst ~cell:cell_inv ~ipins:cell_inv.in_pins
               ~ins:[bi] ~out:inv :: !insts;
    inv) in
  let sums = ref [] and cin = ref "1'b1" in
  for i = 0 to w - 1 do
    let ai = if i < a_w then bit_at a_name a_w i else "1'b0" in
    let bi = List.nth inv_b_bits i in
    let s, co, fa_insts, fa_wires = gen_fa ~a:ai ~b:bi ~cin:!cin in
    sums := s :: !sums;
    insts := List.rev_append fa_insts !insts;
    wires := !wires @ fa_wires;
    cin := co
  done;
  (List.rev !sums, !cin, List.rev !insts, !wires)

(* Less-than: a < b ⇔ subtraction borrows out ⇔ ~cout. *)
let gen_lt ~a_name ~a_w ~b_name ~b_w =
  let _sums, cout, insts, wires = gen_sub ~a_name ~a_w ~b_name ~b_w in
  let lt = mint "lt" in
  let inv = gate_inst ~cell:cell_inv ~ipins:cell_inv.in_pins
              ~ins:[cout] ~out:lt in
  (lt, insts @ [inv], (lt, 1) :: wires)

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
                     let res, insts, wires_ = gen_eq ~a_name:an ~a_w
                                                ~b_name:bn ~b_w in
                     absorb_helper insts wires_;
                     ctx.assigns <- (out_name, res) :: ctx.assigns
                 | Signal_sub ->
                     let sums, _co, insts, wires_ =
                       gen_sub ~a_name:an ~a_w ~b_name:bn ~b_w in
                     absorb_helper insts wires_;
                     (* Per-bit alias the requested out_name[i] to sums[i]. *)
                     List.iteri (fun i s ->
                       let oi = if w = 1 then out_name
                                else Printf.sprintf "%s[%d]" out_name i in
                       ctx.assigns <- (oi, s) :: ctx.assigns
                     ) (let n = min w (List.length sums) in
                        List.filteri (fun i _ -> i < n) sums)
                 | Signal_lt ->
                     let res, insts, wires_ =
                       gen_lt ~a_name:an ~a_w ~b_name:bn ~b_w in
                     absorb_helper insts wires_;
                     ctx.assigns <- (out_name, res) :: ctx.assigns
                 | _ ->
                     (* Add/Mul still raw — gcd doesn't need them as
                        gates, but flag them so we notice if a future
                        design hits this path. *)
                     let op_str = match op with
                       | Signal_add -> "+"
                       | Signal_mulu | Signal_muls -> "*"
                       | _ -> "/* op? */" in
                     Printf.eprintf
                       "[lib_map] WARN: unmapped op %s — emitting raw \
                        Verilog (read_verilog will reject)\n" op_str;
                     ctx.assigns <- (out_name,
                       Printf.sprintf "%s %s %s" an op_str bn) :: ctx.assigns))
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
  List.iter mark root_nets;
  (* Expand multi-bit roots so cells driving `q[3]` survive when
     `q` is in the root set. *)
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

  let queue = Queue.create () in
  Hashtbl.iter (fun k () -> Queue.push k queue) live;
  while not (Queue.is_empty queue) do
    let net = Queue.pop queue in
    let push x =
      if not (Hashtbl.mem live x) then (mark x; Queue.push x queue) in
    List.iter (fun (inst : instance) ->
      List.iter (fun (c : pin_conn) ->
        if c.pin <> inst.cell.out_pin then
          List.iter push (extract_idents c.net)
      ) inst.conns
    ) (Hashtbl.find_all by_out_net net);
    List.iter (fun (lhs, rhs) ->
      if lhs = net then List.iter push (extract_idents rhs)
    ) nl.assigns
  done;

  let inst_alive (i : instance) =
    List.exists (fun (c : pin_conn) ->
      c.pin = i.cell.out_pin && Hashtbl.mem live c.net) i.conns in
  let assign_alive (lhs, _) = Hashtbl.mem live lhs in
  let wire_alive (n, _) = Hashtbl.mem live n in
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
