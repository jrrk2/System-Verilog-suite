(* Smoke: walk the NanGate LEF and dump cell/pin direction stats. *)
open Lef_def

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "nangate.lef" in
  let entries = Lef_pins.parse path in
  Printf.printf "Pins extracted: %d\n" (List.length entries);
  let by_dir = Hashtbl.create 8 in
  let by_cell = Hashtbl.create 256 in
  List.iter (fun (e : Lef_pins.pin_entry) ->
    let k = Lef_pins.string_of_direction e.dir in
    let n = try Hashtbl.find by_dir k with Not_found -> 0 in
    Hashtbl.replace by_dir k (n+1);
    Hashtbl.replace by_cell e.cell ()) entries;
  Printf.printf "Cells: %d\n" (Hashtbl.length by_cell);
  Hashtbl.iter (fun k v ->
    Printf.printf "  %-8s : %d\n" k v) by_dir;
  Printf.printf "\nFirst few BUF_X1 pins:\n";
  List.iter (fun (e : Lef_pins.pin_entry) ->
    if e.cell = "BUF_X1" then
      Printf.printf "  %s.%s  %s\n" e.cell e.pin
        (Lef_pins.string_of_direction e.dir)) entries
