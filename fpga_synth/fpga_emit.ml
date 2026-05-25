(* Emit a primitive-mapped netlist for nextpnr-xilinx.
 *
 * Once lut_cover + register/IO/DSP/BRAM inference have rebuilt the
 * design as a hardcaml Circuit of Xilinx primitives (via Xil_prim),
 * render it.  nextpnr consumes yosys `write_json`, but hardcaml emits
 * Verilog natively; the two routes are:
 *   (a) emit Verilog here -> `yosys read_verilog; write_json` (pure
 *       format conversion, no synthesis) -> nextpnr;
 *   (b) a direct Circuit.t -> yosys-JSON writer (mechanical once the
 *       netlist is already primitives) — stubbed below.
 *
 * Scaffold: Verilog emit is real; the JSON writer is a stub. *)

open! Base
open Hardcaml

(* Render the primitive netlist as Verilog (route (a)). *)
let emit_verilog (circ : Circuit.t) : unit = Rtl.print Verilog circ

let write_verilog ~(path : string) (circ : Circuit.t) : unit =
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Rtl.output ~output_mode:(To_channel oc) Verilog circ)

(* Direct yosys write_json emit (route (b)).  The schema is modules ->
 * { ports, cells, netnames }; cells are the Xilinx primitives with their
 * parameters (LUT INIT etc.) and port connections, nets carry integer
 * bit ids.  Mechanical from Circuit.t since it is already a primitive
 * netlist, but not yet implemented. *)
let write_yosys_json ~path:(_ : string) (_circ : Circuit.t) : unit =
  failwith "fpga_emit.write_yosys_json: TODO (Circuit.t -> yosys write_json)"
