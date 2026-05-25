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

let map_lowered ~(k : int) ~(name : string) (l : Bir_to_aig.lowered) : Circuit.t =
  let g = l.Bir_to_aig.graph in
  (* feedback wire per register Q bit. *)
  let q_wire = Hashtbl.create (module String) in
  List.iter l.regs ~f:(fun r ->
    List.iter r.Bir_to_aig.rb_q_names ~f:(fun nm ->
      Hashtbl.set q_wire ~key:nm ~data:(Signal.wire 1)));
  (* names that are register D-cone outputs (not real circuit outputs). *)
  let d_names = Hash_set.create (module String) in
  List.iter l.regs ~f:(fun r ->
    List.iter r.Bir_to_aig.rb_d_names ~f:(Hash_set.add d_names));
  (* lazy real input ports (data ports, clocks, resets). *)
  let ports = Hashtbl.create (module String) in
  let port_input nm =
    Hashtbl.find_or_add ports nm ~default:(fun () -> Signal.input nm 1)
  in
  (* ---- LUT-map the combinational logic ---- *)
  let chosen = Lut_cover.cover ~k g in
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
    sig_of.(c.Lut_cover.root) <- Some (Xil_prim.lutk ~truth ins));
  (* ---- route outputs: register D-cones vs real outputs ---- *)
  let d_sig = Hashtbl.create (module String) in
  let real_outs = ref [] in
  List.iter g.Lut_cover.outputs ~f:(fun (nm, id, inv) ->
    let s = signal_of_node id in
    let s = if inv then Signal.( ~: ) s else s in
    if Hash_set.mem d_names nm then Hashtbl.set d_sig ~key:nm ~data:s
    else real_outs := Signal.output nm s :: !real_outs);
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
  Circuit.create_exn ~name (List.rev !real_outs)
