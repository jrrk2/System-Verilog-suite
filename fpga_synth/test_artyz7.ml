(* artyz7-style validation design for nextpnr-xilinx (xc7z020):
 *   28-bit free-running counter; led[i] = ctr[24+i] ^ btn[i].
 * Ports are 1-bit (clk, btn__0..3, led__0..3) so a hand-written XDC can
 * pin them by exact name.  Emitted with IO buffers (IBUF/OBUF/BUFG) so
 * nextpnr can place the top-level pads. *)
open! Base
open Hardcaml
open Fpga_synth

let () =
  let clk = Signal.input "clk" 1 in
  let btn = Array.init 4 ~f:(fun i -> Signal.input (Printf.sprintf "btn__%d" i) 1) in
  let spec = Reg_spec.create ~clock:clk () in
  let ctr = Signal.wire 28 in
  Signal.(ctr <== reg spec (ctr +:. 1));
  let outs =
    List.init 4 ~f:(fun i ->
      let cbit = Signal.select ctr (24 + i) (24 + i) in
      Signal.output (Printf.sprintf "led__%d" i) Signal.(cbit ^: btn.(i)))
  in
  let circ = Circuit.create_exn ~name:"top" outs in
  let l = Bir_to_aig.lower_circuit circ in
  let mapped = Fpga_map.map_lowered ~io:true ~k:6 ~name:"top" l in
  let path = "/tmp/artyz7_top.json" in
  Fpga_emit.write_yosys_json ~path mapped;
  Stdio.printf "regs=%d  AIG outputs=%d  -> %s\n" (List.length l.regs)
    (List.length l.graph.outputs) path
