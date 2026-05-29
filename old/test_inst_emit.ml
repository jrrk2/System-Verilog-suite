(* Verify behavioral_to_hardcaml emits a hardcaml Inst per binstance when
 * ~emit_instances:true (the FPGA path).  Self-contained direction
 * inference: BLK.A is driven by a process (din = a) so it's an input;
 * BLK.Y drives dout, which no process writes, so it's an output. *)
open Hardcaml
open Behavioral_ir

let () =
  let i4 = BInt { width = 4; signed = Unsigned }
  and i1 = BInt { width = 1; signed = Unsigned } in
  let sg name stype direction =
    { name; stype; direction; initial_value = None; attrs = [] }
  in
  let bmod =
    { name = "top"
    ; params = []
    ; signals =
        [ sg "a" i4 `Input; sg "clk" i1 `Input; sg "z" i4 `Output
        ; sg "din" i4 `Internal; sg "dout" i4 `Internal ]
    ; processes =
        [ BCombinational
            { name = "c"
            ; sensitivity = [ BAny ]
            ; body =
                [ BAssign { lhs = "din"; rhs = BVar "a" }
                ; BAssign { lhs = "z"; rhs = BVar "dout" } ]
            } ]
    ; instances =
        [ { inst_name = "blk0"
          ; module_name = "BLK"
          ; param_values = [ ("MODE", 1) ]
          ; param_strs = []
          ; port_connections = [ ("A", BVar "din"); ("Y", BVar "dout") ]
          } ]
    ; funcs = []
    ; mems = []
    ; attrs = []
    }
  in
  let circ = Behavioral_to_hardcaml.create_circuit ~emit_instances:true bmod in
  let buf = Buffer.create 512 in
  Rtl.output ~output_mode:(To_buffer buf) Verilog circ;
  let s = Buffer.contents buf in
  let has re =
    try ignore (Str.search_forward (Str.regexp re) s 0); true with Not_found -> false
  in
  Printf.printf "BLK instance emitted: %b\nMODE param present: %b\n" (has "BLK")
    (has "MODE");
  (* sanity: with the default (instances dropped) there should be no BLK. *)
  let buf2 = Buffer.create 512 in
  Rtl.output ~output_mode:(To_buffer buf2) Verilog
    (Behavioral_to_hardcaml.create_circuit bmod);
  let dropped =
    not
      (try ignore (Str.search_forward (Str.regexp "BLK") (Buffer.contents buf2) 0); true
       with Not_found -> false)
  in
  Printf.printf "default drops instance: %b\n" dropped;
  if not (has "BLK") then (print_endline "---- emit_instances verilog ----"; print_endline s)
