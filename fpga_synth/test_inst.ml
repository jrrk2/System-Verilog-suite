(* Exercise general Inst lowering: a design wrapping a black-box
 * instance.  z = (BLK(a + b)) ^ a, where BLK is an opaque 4-bit cell
 * with input A and output Y plus a parameter.  Confirms the box survives
 * as a cell, its A input is driven by the adder cone, and its Y output
 * feeds the final XOR. *)
open! Base
open Hardcaml
open Fpga_synth

let () =
  let a = Signal.input "a" 4 and b = Signal.input "b" 4 in
  let blk =
    Instantiation.create ()
      ~name:"BLK"
      ~parameters:[ Parameter.create ~name:"MODE" ~value:(Parameter.Value.Int 1) ]
      ~inputs:[ "A", Signal.(a +: b) ]
      ~outputs:[ "Y", 4 ]
  in
  let y = Map.find_exn blk "Y" in
  let circ = Circuit.create_exn ~name:"top" [ Signal.output "z" Signal.(y ^: a) ] in
  let l = Bir_to_aig.lower_circuit circ in
  Stdio.printf "regs=%d  insts=%d  AIG outputs=%d\n" (List.length l.regs)
    (List.length l.insts) (List.length l.graph.outputs);
  List.iter l.insts ~f:(fun ib ->
    Stdio.printf "  inst %s (%s): in_ports=%d out_ports=%d generics=%d\n"
      ib.ib_name ib.ib_instance (List.length ib.ib_in_ports)
      (List.length ib.ib_out_ports) (List.length ib.ib_generics));
  let mapped = Fpga_map.map_lowered ~k:6 ~name:"top" l in
  Fpga_emit.write_yosys_json ~path:"/tmp/inst_top.json" mapped;
  Stdio.print_endline "wrote /tmp/inst_top.json"
