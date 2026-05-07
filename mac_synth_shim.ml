(* MAC synth shim for ORFS hijack.

   Mirrors [synth_orfs_shim] but takes a MAC architecture instead of
   SystemVerilog input.  The args mimic synth_orfs_shim's signature
   (top, output.v, then ignored "input files") so the existing
   USE_DECOMP_SYNTH=1 Makefile patch can invoke us by setting
   DECOMP_SHIM to this binary.  The MAC params come from environment
   variables:

     MAC_WIDTH   - bit width (default 8)
     MAC_MUL     - multiplier arch: array | wallace | dadda
     MAC_ADD     - adder arch:    ripple | brent_kung | sklansky |
                                  kogge_stone

   Emits two files:
     <output.v>            -- structural cell-mapped Verilog
     <output.v>.sdc        -- the design's SDC (clock + io delays)
                              ready to be cp'd to 1_2_yosys.sdc
*)

open Lef_def

let parse_mul = function
  | "array"   -> Synth_mac.Array_m
  | "wallace" -> Synth_mac.Wallace_m
  | "dadda"   -> Synth_mac.Dadda_m
  | s -> failwith ("mac_synth_shim: unknown mul arch " ^ s)

let parse_add = function
  | "ripple"      -> Synth_mac.Ripple_a
  | "kogge_stone" -> Synth_mac.Kogge_stone_a
  | "brent_kung"  -> Synth_mac.Brent_kung_a
  | "sklansky"    -> Synth_mac.Sklansky_a
  | s -> failwith ("mac_synth_shim: unknown adder arch " ^ s)

let env_or k def = try Sys.getenv k with Not_found -> def

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf
      "usage: %s <top> <output.v> [extra-args ignored]\n" Sys.argv.(0);
    Printf.eprintf
      "Reads MAC arch from MAC_WIDTH / MAC_MUL / MAC_ADD env vars.\n";
    exit 1
  end;
  let top   = Sys.argv.(1) in
  let out_v = Sys.argv.(2) in
  let width = int_of_string (env_or "MAC_WIDTH" "8") in
  let mul_str = env_or "MAC_MUL" "array" in
  let add_str = env_or "MAC_ADD" "ripple" in
  let mul = parse_mul mul_str in
  let add = parse_add add_str in
  let clk_ns =
    try float_of_string (Sys.getenv "MAC_CLK_NS") with Not_found -> 4.0
  in
  Printf.eprintf
    "[mac_synth_shim] top=%s width=%d mul=%s add=%s clk=%.2fns -> %s\n"
    top width mul_str add_str clk_ns out_v;

  let nl = Synth_mac.build ~width ~mul_arch:mul ~add_arch:add () in
  Printf.eprintf "[mac_synth_shim] built netlist: %d cells, depth %d\n"
    (List.length nl.cells) nl.depth;

  let oc = open_out out_v in
  Synth_mac.emit_verilog ~module_name:top ~oc nl;
  close_out oc;

  let sdc_path = out_v ^ ".sdc" in
  let oc = open_out sdc_path in
  Synth_mac.emit_sdc ~oc ~clock_period:clk_ns nl;
  close_out oc;

  Printf.printf "[synth_orfs_shim] OK — wrote %s and %s\n" out_v sdc_path
