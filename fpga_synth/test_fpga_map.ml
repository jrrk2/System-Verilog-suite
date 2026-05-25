(* End-to-end: a sequential Hardcaml circuit -> AIG -> LUT+FF netlist.
 * Exercises feedback (counter) and the FDRE / FDCE choice. *)
open! Base
open Hardcaml
open Fpga_synth

(* 4-bit free-running counter: q <= q + 1 (no reset -> FDRE). *)
let counter () =
  let clk = Signal.input "clk" 1 in
  let spec = Reg_spec.create ~clock:clk () in
  let q = Signal.wire 4 in
  Signal.(q <== reg spec (q +:. 1));
  Circuit.create_exn ~name:"counter4" [ Signal.output "count" q ]

(* 4-bit counter with async reset (-> FDCE). *)
let counter_rst () =
  let clk = Signal.input "clk" 1 and rst = Signal.input "rst" 1 in
  let spec = Reg_spec.create ~clock:clk ~reset:rst () in
  let q = Signal.wire 4 in
  Signal.(q <== reg spec (q +:. 1));
  Circuit.create_exn ~name:"counter4_rst" [ Signal.output "count" q ]

let run label circ =
  let l = Bir_to_aig.lower_circuit circ in
  Stdio.printf "%s: regs=%d  AIG outputs=%d\n" label
    (List.length l.regs) (List.length l.graph.outputs);
  let mapped = Fpga_map.map_lowered ~k:6 ~name:label l in
  let path = Printf.sprintf "/tmp/%s.json" label in
  Fpga_emit.write_yosys_json ~path mapped;
  Stdio.printf "  wrote %s\n" path;
  mapped

let () =
  let c = run "counter4" (counter ()) in
  Stdio.print_endline "---- counter4 yosys-json ----";
  Stdio.print_endline (Fpga_emit.yosys_json_string c);
  ignore (run "counter4_rst" (counter_rst ()))
