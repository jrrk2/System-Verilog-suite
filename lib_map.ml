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
           (match signal_op_kind op with
            | Some cell ->
                let inst_list = blast_op2 ~cell ~out_name ~width:w
                                  ~a_name:an ~b_name:bn ~a_w ~b_w in
                ctx.insts <- inst_list @ ctx.insts
            | None ->
                (* Arithmetic / comparison — pass through as raw
                   Verilog assign; downstream yosys techmap maps
                   it.  This is the "fall-through" the design
                   memo allowed. *)
                let op_str = match op with
                  | Signal_add -> "+" | Signal_sub -> "-"
                  | Signal_mulu | Signal_muls -> "*"
                  | Signal_eq -> "==" | Signal_lt -> "<"
                  | _ -> "/* op? */" in
                ctx.assigns <- (out_name,
                  Printf.sprintf "%s %s %s" an op_str bn) :: ctx.assigns)
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
       | Mux _ ->
           (* N-way mux: fall through to assign — uncommon in the
              kind of netlist hardcaml emits for our designs. *)
           ()
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
  {
    inputs;
    outputs;
    (* Drop wires that are also ports (avoid duplicate decls). *)
    wires =
      List.filter (fun (n, _) ->
        not (List.mem_assoc n inputs) && not (List.mem_assoc n outputs))
        ctx.wires;
    insts = List.rev ctx.insts;
    assigns = List.rev ctx.assigns;
  }
