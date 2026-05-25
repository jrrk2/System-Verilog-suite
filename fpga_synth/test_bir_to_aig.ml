(* Lower hand-built Hardcaml circuits to an AIG, then LUT-map them, to
 * exercise the bit-blaster (arithmetic + register boundary). *)
open! Base
open Hardcaml
open Fpga_synth

let report name (l : Bir_to_aig.lowered) =
  Stdio.printf "%s: AIG nodes=%d  outputs=%d  regs=%d  ports=%d\n" name
    (Array.length l.graph.nodes)
    (List.length l.graph.outputs)
    (List.length l.regs)
    (List.length l.inputs);
  let chosen = Lut_cover.cover ~k:6 l.graph in
  Stdio.printf "  cover (k=6): %d LUT(s)\n" (List.length chosen);
  List.iter l.regs ~f:(fun r ->
    Stdio.printf "  reg w=%d clk=%s rst=%s\n" r.rb_width r.rb_clock
      (Option.value r.rb_reset ~default:"-"))

let () =
  (* 1) 4-bit adder: pure combinational. *)
  let a = Signal.input "a" 4 and b = Signal.input "b" 4 in
  let add = Circuit.create_exn ~name:"add4" [ Signal.output "sum" Signal.(a +: b) ] in
  let ladd = Bir_to_aig.lower_circuit add in
  report "add4" ladd;
  Stdio.print_endline "---- add4 LUT netlist ----";
  Fpga_emit.emit_verilog (Lut_cover.map_to_luts ~k:6 ~name:"add4_lut" ladd.graph);

  (* 2) registered: q <= d (exercises the Reg boundary). *)
  let clock = Signal.input "clk" 1 in
  let d = Signal.input "d" 4 in
  let spec = Reg_spec.create ~clock () in
  let regc = Circuit.create_exn ~name:"reg4" [ Signal.output "q" (Signal.reg spec d) ] in
  report "reg4" (Bir_to_aig.lower_circuit regc)
