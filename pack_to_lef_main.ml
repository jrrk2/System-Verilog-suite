(* CLI for the recognition packer (Pack_to_lef): yosys netlist JSON -> packed
   LEF-cell recognition report. *)
let () =
  if Array.length Sys.argv > 1 then begin
    let bmod = Pack_to_lef.bmodule_of_yosys_json Sys.argv.(1) in
    Printf.printf "loaded %d instances\n" (List.length bmod.Behavioral_ir.instances);
    Pack_to_lef.print_result (Pack_to_lef.pack bmod)
  end else
    prerr_endline "usage: pack_to_lef_main <netlist.json>"
