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

let map_lowered ?(io = false) ~(k : int) ~(name : string) (l : Bir_to_aig.lowered)
  : Circuit.t =
  let g = l.Bir_to_aig.graph in
  (* feedback wire per boundary output bit: register Q + instance output
     ports.  signal_of_node routes an AIG input to its wire when present. *)
  let q_wire = Hashtbl.create (module String) in
  List.iter l.regs ~f:(fun r ->
    List.iter r.Bir_to_aig.rb_q_names ~f:(fun nm ->
      Hashtbl.set q_wire ~key:nm ~data:(Signal.wire 1)));
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
     a data/reset pad through IBUF, a clock pad through IBUF then BUFG. *)
  let ports = Hashtbl.create (module String) in
  let port_input nm =
    Hashtbl.find_or_add ports nm ~default:(fun () ->
      let pad = Signal.input nm 1 in
      if not io then pad
      else if Hash_set.mem clock_names nm then Xil_prim.bufg (Xil_prim.ibuf pad)
      else Xil_prim.ibuf pad)
  in
  (* ---- LUT-map the combinational logic ---- *)
  let chosen = Lut_cover.cover ~k g in
  (* Inverter folding: an output / FF-D that needs ~root can absorb the
     inversion into the driving LUT's INIT instead of spending a LUT1 —
     but only when [root] is a LUT root with NO positive consumer (LUT
     inputs are always positive references; a positive output ref would
     break if we flipped the INIT). *)
  let pos_ref = Hash_set.create (module Int) in
  List.iter chosen ~f:(fun c ->
    List.iter c.Lut_cover.leaves ~f:(Hash_set.add pos_ref));
  let out_pos = Hash_set.create (module Int) in
  let out_inv = Hash_set.create (module Int) in
  List.iter g.Lut_cover.outputs ~f:(fun (_, id, inv) ->
    if inv then Hash_set.add out_inv id else Hash_set.add out_pos id);
  let complement = Hash_set.create (module Int) in
  List.iter chosen ~f:(fun c ->
    let r = c.Lut_cover.root in
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
  List.iter chosen ~f:(fun c ->
    let ins = List.map c.Lut_cover.leaves ~f:signal_of_node in
    let truth = Lut_cover.truth_table_of_cut g c in
    let truth =
      if Hash_set.mem complement c.Lut_cover.root then List.map truth ~f:not
      else truth
    in
    sig_of.(c.Lut_cover.root) <- Some (Xil_prim.lutk ~truth ins));
  (* ---- route outputs: register D-cones vs real outputs ---- *)
  let d_sig = Hashtbl.create (module String) in
  let real_outs = ref [] in
  List.iter g.Lut_cover.outputs ~f:(fun (nm, id, inv) ->
    let base = signal_of_node id in
    (* if the driving LUT was complemented, it already yields ~node. *)
    let s = if inv && not (Hash_set.mem complement id) then Signal.( ~: ) base else base in
    if Hash_set.mem d_names nm then Hashtbl.set d_sig ~key:nm ~data:s
    else (
      let pad = if io then Xil_prim.obuf s else s in
      real_outs := Signal.output nm pad :: !real_outs));
  (* ---- instantiate one FF per register bit, close feedback ---- *)
  List.iter l.regs ~f:(fun r ->
    let clk = port_input r.Bir_to_aig.rb_clock in
    let rst = Option.map r.Bir_to_aig.rb_reset ~f:port_input in
    List.iter2_exn r.Bir_to_aig.rb_d_names r.Bir_to_aig.rb_q_names
      ~f:(fun dn qn ->
        let d = Option.value (Hashtbl.find d_sig dn) ~default:Signal.gnd in
        let q =
          match rst with
          | Some clr -> (Xil_prim.Fdce.create { c = clk; ce = Signal.vdd; clr; d }).q
          | None -> (Xil_prim.Fdre.create { c = clk; ce = Signal.vdd; r = Signal.gnd; d }).q
        in
        let w = Hashtbl.find_exn q_wire qn in
        Signal.(w <== q)));
  (* ---- re-instantiate each black box, wire its boundary buses ---- *)
  let bus_of_bits bit_names =
    match
      List.map bit_names ~f:(fun nm ->
        Option.value (Hashtbl.find d_sig nm) ~default:Signal.gnd)
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
