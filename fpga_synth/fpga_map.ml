(* Top-level FPGA mapper: a lowered design (Bir_to_aig.lowered) ->
 * a Hardcaml.Circuit of Xilinx primitives (LUTs + FDRE/FDCE).
 *
 * Bir_to_aig split the design into a combinational AIG plus a list of
 * register boundaries (Q = AIG primary input, D = AIG primary output).
 * Here we:
 *   1. give every register Q bit a feedback wire,
 *   2. LUT-map the combinational AIG (Lut_cover.cover), routing AIG
 *      inputs to either the Q wires or real input ports,
 *   3. instantiate one FF per register bit (FDCE if the boundary had an
 *      async reset, else FDRE) with D = the mapped D-cone signal, and
 *      drive the Q wire from the FF output, closing the feedback.
 * The result is a netlist of LUTk + FDRE/FDCE for nextpnr-xilinx. *)

open! Base
open Hardcaml

(* [diff_clocks] maps a top-level clock port name (as it appears as a
 * BIR clock signal) to a pair of differential pad names (P_pin,
 * N_pin) — used for boards like VC707 whose 200 MHz SYSCLK_P/N
 * arrives as an LVDS pair.  When the entry is present and IO buffers
 * are enabled, the clock pad turns into [BUFG (IBUFDS p, n)] instead
 * of [BUFG (IBUF pad)]; the BIR clock name stays the same on the
 * register side so the rest of the netlist is unchanged. *)
let map_lowered ?(io = false) ?(mode : Lut_cover.cost_mode = `Area)
    ?(lutpack = false) ?(mfs2_var_elim = false) ?(mfs2_odc = false)
    ?(diff_clocks : (string * (string * string)) list = [])
    ~(k : int) ~(name : string) (l : Bir_to_aig.lowered)
  : Circuit.t =
  let g = l.Bir_to_aig.graph in
  (* feedback wire per boundary output bit: register Q + instance output
     ports.  signal_of_node routes an AIG input to its wire when present. *)
  let q_wire = Hashtbl.create (module String) in
  List.iter l.regs ~f:(fun r ->
    List.iter r.Bir_to_aig.rb_q_names ~f:(fun nm ->
      (* Name the feedback wire with the register's (source) net name so
         of_circuit emits it as the Q net name — keeps the gate-mapped
         netlist name-correspondent with the source for Z3 LEC. *)
      Hashtbl.set q_wire ~key:nm ~data:(Signal.(wire 1 -- nm))));
  List.iter l.insts ~f:(fun ib ->
    List.iter ib.Bir_to_aig.ib_out_ports ~f:(fun (_, bits) ->
      List.iter bits ~f:(fun nm -> Hashtbl.set q_wire ~key:nm ~data:(Signal.wire 1))));
  (* boundary-output names (drive an FF D or an instance input, not a real
     circuit output): register D-cones + instance input-port cones. *)
  let d_names = Hash_set.create (module String) in
  List.iter l.regs ~f:(fun r ->
    List.iter r.Bir_to_aig.rb_d_names ~f:(Hash_set.add d_names));
  List.iter l.insts ~f:(fun ib ->
    List.iter ib.Bir_to_aig.ib_in_ports ~f:(fun (_, bits) ->
      List.iter bits ~f:(Hash_set.add d_names)));
  (* clock names, for IO-buffer kind selection. *)
  let clock_names = Hash_set.create (module String) in
  List.iter l.regs ~f:(fun r -> Hash_set.add clock_names r.Bir_to_aig.rb_clock);
  (* lazy real input ports.  With [io], wrap each pad in an input buffer:
     a data/reset pad through IBUF, a clock pad through IBUF then BUFG.
     FPGA_NO_BUFG=1 skips the BUFG wrapper on clock pads (the input
     buffer is still inserted) — useful when an upstream tool already
     supplies a global clock buffer or when targeting a flow that
     auto-inserts BUFGs. *)
  let no_bufg =
    match Stdlib.Sys.getenv_opt "FPGA_NO_BUFG" with
    | Some "1" -> true
    | _ -> false
  in
  let ports = Hashtbl.create (module String) in
  let port_input nm =
    Hashtbl.find_or_add ports nm ~default:(fun () ->
      if not io then Signal.input nm 1
      else if Hash_set.mem clock_names nm then
        (let buf =
           match List.Assoc.find diff_clocks nm ~equal:String.equal with
           | Some (p_name, n_name) ->
             (* Differential clock: P/N pads → IBUFDS (→ BUFG).  The
                single pad named [nm] is replaced by two real
                top-level inputs named [p_name] and [n_name];
                downstream consumers (regs) still see [nm] as the
                clock signal because the buffer's output is what we
                return for this port. *)
             let p_pad = Signal.input p_name 1 in
             let n_pad = Signal.input n_name 1 in
             Xil_prim.ibufds ~i:p_pad ~ib:n_pad ()
           | None ->
             let pad = Signal.input nm 1 in
             Xil_prim.ibuf pad
         in
         if no_bufg then buf else Xil_prim.bufg buf)
      else
        let pad = Signal.input nm 1 in
        Xil_prim.ibuf pad)
  in
  (* ---- LUT-map the combinational logic ---- *)
  let chosen = Lut_cover.cover ~mode ~k g in
  (* Convert chosen cuts to packed_luts and optionally lutpack-fuse
     single-fanout parent-child pairs.  Boundary consumers (register Q
     wires, top outputs, instance pins) pin the parent's fanout so we
     don't accidentally absorb a LUT that drives them. *)
  let packed_init = List.map chosen ~f:(fun c -> Lut_cover.cut_to_packed g c) in
  let packed =
    if not lutpack then packed_init
    else begin
      (* Extra consumers per root: register D-cone outputs and instance
         input bits that pass through cover.  These are graph outputs;
         already counted via graph_outputs.                              *)
      let extra_consumers _ = 0 in
      Lut_cover.lutpack ~k ~graph_outputs:g.Lut_cover.outputs ~extra_consumers
        packed_init
    end
  in
  let packed =
    if mfs2_var_elim || mfs2_odc
    then Mfs2.run ~enable_var_elim:mfs2_var_elim ~enable_odc:mfs2_odc packed
    else packed
  in
  (* Inverter folding: an output / FF-D that needs ~root can absorb the
     inversion into the driving LUT's INIT instead of spending a LUT1 —
     but only when [root] is a LUT root with NO positive consumer (LUT
     inputs are always positive references; a positive output ref would
     break if we flipped the INIT). *)
  let pos_ref = Hash_set.create (module Int) in
  List.iter packed ~f:(fun p ->
    List.iter p.Lut_cover.pl_leaves ~f:(Hash_set.add pos_ref));
  let out_pos = Hash_set.create (module Int) in
  let out_inv = Hash_set.create (module Int) in
  List.iter g.Lut_cover.outputs ~f:(fun (_, id, inv) ->
    if inv then Hash_set.add out_inv id else Hash_set.add out_pos id);
  let complement = Hash_set.create (module Int) in
  List.iter packed ~f:(fun p ->
    let r = p.Lut_cover.pl_root in
    if (not (Hash_set.mem pos_ref r))
       && (not (Hash_set.mem out_pos r))
       && Hash_set.mem out_inv r
    then Hash_set.add complement r);
  let n = Array.length g.Lut_cover.nodes in
  let sig_of = Array.create ~len:n None in
  let signal_of_node id =
    match sig_of.(id) with
    | Some s -> s
    | None ->
      let s =
        match g.Lut_cover.nodes.(id).Lut_cover.gate with
        | Lut_cover.Input nm ->
          (match Hashtbl.find q_wire nm with
           | Some w -> w
           | None -> port_input nm)
        | Lut_cover.Const b -> if b then Signal.vdd else Signal.gnd
        | Lut_cover.And2 _ -> failwith "fpga_map: And2 used before its LUT was built"
      in
      sig_of.(id) <- Some s;
      s
  in
  (* Emit each packed LUT in topological order of root id.  After lutpack
     this is still safe: a fused LUT's leaves are unions of the original
     leaves, all of which have lower AIG ids than the root.              *)
  let packed_sorted =
    List.sort packed ~compare:(fun a b ->
      Int.compare a.Lut_cover.pl_root b.Lut_cover.pl_root) in
  List.iter packed_sorted ~f:(fun p ->
    let ins = List.map p.Lut_cover.pl_leaves ~f:signal_of_node in
    let comp = Hash_set.mem complement p.Lut_cover.pl_root in
    sig_of.(p.Lut_cover.pl_root) <-
      Some (Lut_cover.emit_cut_signal ~complement:comp ~ins p.Lut_cover.pl_tt));
  (* ---- route outputs: register D-cones vs real outputs ---- *)
  let d_sig = Hashtbl.create (module String) in
  let real_outs = ref [] in
  List.iter g.Lut_cover.outputs ~f:(fun (nm, id, inv) ->
    let base = signal_of_node id in
    (* if the driving LUT was complemented, it already yields ~node. *)
    let s = if inv && not (Hash_set.mem complement id) then Signal.( ~: ) base else base in
    if Hash_set.mem d_names nm then Hashtbl.set d_sig ~key:nm ~data:s
    else
      (* A `__keep_<clk>` retention output whose <clk> is a register clock is
         an INTERNALLY generated clock (a user BUFG/MMCM O net feeding
         same-module FFs).  Rather than emit it as a driverless top-level IO
         pad, register its on-chip driver under <clk> so the FF-clock
         [port_input] below binds to the buffer output.  Without this the FF
         clock becomes a fresh input pad and the clock net is orphaned
         (Vivado NSTD-1/UCIO-1 on an unconstrained port). *)
      (match String.chop_prefix nm ~prefix:"__keep_" with
       | Some clk when Hash_set.mem clock_names clk ->
         Hashtbl.set ports ~key:clk ~data:s
       | _ ->
         let pad = if io then Xil_prim.obuf s else s in
         real_outs := Signal.output nm pad :: !real_outs));
  (* ---- instantiate one FF per register bit, close feedback ---- *)
  List.iter l.regs ~f:(fun r ->
    let clk = port_input r.Bir_to_aig.rb_clock in
    let rst = Option.map r.Bir_to_aig.rb_reset ~f:port_input in
    (* Per-bit INIT extracted from rb_init.  bit i of rb_init is the
       FDRE INIT for the i-th register bit (LSB first, matching
       rb_q_names ordering). *)
    let init_bit i =
      match r.Bir_to_aig.rb_init with
      | Some v -> (v lsr i) land 1 = 1
      | None -> false
    in
    List.iteri (List.zip_exn r.Bir_to_aig.rb_d_names r.Bir_to_aig.rb_q_names)
      ~f:(fun bit (dn, qn) ->
        let d = Option.value (Hashtbl.find d_sig dn) ~default:Signal.gnd in
        (* Carry the source register net name as the FF INSTANCE name so
           of_circuit can name the emitted Q net after it (Stage-2 of the
           register-name-preservation fix for FPGA Z3 LEC — ffrip matches
           state by name). *)
        let q =
          match rst with
          | Some clr -> (Xil_prim.Fdce.create ~instance:qn { c = clk; ce = Signal.vdd; clr; d }).q
          | None ->
            (Xil_prim.Fdre.create ~init:(init_bit bit) ~instance:qn
               { c = clk; ce = Signal.vdd; r = Signal.gnd; d }).q
        in
        let q = Signal.(q -- qn) in
        let w = Hashtbl.find_exn q_wire qn in
        Signal.(w <== q)));
  (* ---- re-instantiate each black box, wire its boundary buses ---- *)
  let bus_of_bits bit_names =
    match
      List.map bit_names ~f:(fun nm ->
        match Hashtbl.find d_sig nm with
        | Some s -> s
        | None ->
          (* CARRY4 chains use the previous CARRY4's output-port bit name
             (e.g. c4_0_CO_3) as the input-port bit name on the next
             CARRY4's CI port.  That name is in q_wire, not d_sig. *)
          (match Hashtbl.find q_wire nm with
           | Some w -> w
           | None -> Signal.gnd))
    with
    | [] -> Signal.empty
    | bits -> Signal.concat_lsb bits (* bit_names are LSB-first *)
  in
  List.iter l.insts ~f:(fun ib ->
    let inputs =
      List.map ib.Bir_to_aig.ib_in_ports ~f:(fun (p, bits) -> p, bus_of_bits bits)
    in
    let outputs =
      List.map ib.Bir_to_aig.ib_out_ports ~f:(fun (p, bits) -> p, List.length bits)
    in
    let omap =
      Instantiation.create () ~name:ib.Bir_to_aig.ib_name
        ~instance:ib.Bir_to_aig.ib_instance ~parameters:ib.Bir_to_aig.ib_generics
        ~inputs ~outputs
    in
    List.iter ib.Bir_to_aig.ib_out_ports ~f:(fun (p, bits) ->
      let bus = Map.find_exn omap p in
      List.iteri bits ~f:(fun i nm ->
        let wfb = Hashtbl.find_exn q_wire nm in
        Signal.(wfb <== select bus i i))));
  Circuit.create_exn ~name (List.rev !real_outs)
