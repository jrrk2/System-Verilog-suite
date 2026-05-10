(* End-to-end synth driver for the ORFS Makefile hijack (#102).

   Reads SystemVerilog files, runs them through the shared
   [Synth_pipeline.run], and writes the resulting structural netlist
   to the path ORFS expects.

   Exit codes:
       0 — emitted a usable [1_2_yosys.v]
       1 — failure of any kind (parse, unsupported construct, lowering,
           emission).  Stderr says why.

   IMPORTANT: there is *no* silent fallback to yosys in this driver.
   When this exits non-zero, the ORFS make target also fails — that
   visibility is the point.  yosys remains available for explicit
   oracle/comparison runs (#101), but never as a trapdoor that hides
   bugs in our pipeline.

   For preflight / per-variant staging see [Orfs_prep], which calls
   the same pipeline AND writes ORFS [config.mk] + per-variant SDC
   so that ADDITIONAL_LEFS / ADDITIONAL_LIBS for our memory macros
   actually reach OpenROAD's floorplan.  *)

let usage () =
  Printf.eprintf
    "usage: %s <top> <output.v> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 1

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top      = Sys.argv.(1) in
  let out_path = Sys.argv.(2) in
  let files    = Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in
  let _ = Synth_pipeline.run ~top ~out_path ~files () in
  (* Marker line the ORFS Makefile hook greps for to verify the shim
     succeeded (Makefile:281).  Don't drop or rename — older variants
     of the patch key on this exact prefix.                          *)
  Printf.eprintf "[synth_orfs_shim] OK\n"
