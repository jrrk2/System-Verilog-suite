(* Thin CLI wrapper around the topographical placer core, which now lives in the
   svs_place_core library (Place_lef_core.run) so it is ALSO callable in-process
   from the Lua flow via svd.place_lef.  Behaviour of place_lef.exe is unchanged:
   place_lef <floorplan.json> <netlist.json>, all config via TOPO_* env. *)
let () =
  if Array.length Sys.argv < 3 then
    (prerr_endline "usage: place_lef <floorplan.json> <netlist.json>"; exit 1);
  Place_lef_core.run Sys.argv.(1) Sys.argv.(2)
