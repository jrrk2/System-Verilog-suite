(* Sonata-board (xc7a50tcsg324-1) validation design for nextpnr-xilinx:
 *   28-bit free-running counter; led[i] = ctr[24+i]  (a blinky).
 * 1-bit ports (clk, led__0..3) so the XDC pins them by exact name.
 * Emitted with IO buffers (IBUF/OBUF/BUFG). *)
open! Base
open Hardcaml
open Fpga_synth

let () =
  let clk = Signal.input "clk" 1 in
  let spec = Reg_spec.create ~clock:clk () in
  let ctr = Signal.wire 28 in
  Signal.(ctr <== reg spec (ctr +:. 1));
  let outs =
    List.init 4 ~f:(fun i ->
      Signal.output (Printf.sprintf "led__%d" i) (Signal.select ctr (24 + i) (24 + i)))
  in
  let circ = Circuit.create_exn ~name:"top" outs in
  let l = Bir_to_aig.lower_circuit circ in
  let mapped = Fpga_map.map_lowered ~io:true ~k:6 ~name:"top" l in
  Fpga_emit.write_yosys_json ~path:"/tmp/sonata_top.json" mapped;
  Stdio.printf "regs=%d  outputs=%d  -> /tmp/sonata_top.json\n" (List.length l.regs)
    (List.length l.graph.outputs)
