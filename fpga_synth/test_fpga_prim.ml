(* Smoke test: build a tiny primitive netlist with the Xil_prim helpers
 * and emit it, confirming LUT/FDRE/buffer instantiation renders. *)
open! Base
open Hardcaml
open Fpga_synth

let () =
  let a = Signal.input "a" 1 and b = Signal.input "b" 1 in
  let clk = Signal.input "clk" 1 and en = Signal.input "en" 1
  and rst = Signal.input "rst" 1 in
  (* 2-input AND as a LUT2: truth = [f00;f01;f10;f11] = [0;0;0;1]. *)
  let andv = Xil_prim.lutk ~truth:[ false; false; false; true ] [ a; b ] in
  (* register it in an FDRE. *)
  let q = (Xil_prim.Fdre.create { c = clk; ce = en; r = rst; d = andv }).q in
  let o = Xil_prim.obuf q in
  let circ = Circuit.create_exn ~name:"prim_smoke" [ Signal.output "o" o ] in
  Fpga_emit.emit_verilog circ
