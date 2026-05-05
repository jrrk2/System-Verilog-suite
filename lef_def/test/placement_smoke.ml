(* Read placements out of a DEF and print summary. *)
open Lef_def

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "real.def" in
  let placements = Placement.parse path in
  Printf.printf "Placements found: %d\n" (List.length placements);
  let by_orient = Hashtbl.create 8 in
  List.iter (fun p ->
    let k = Placement.string_of_orient p.Placement.orient in
    let n = try Hashtbl.find by_orient k with Not_found -> 0 in
    Hashtbl.replace by_orient k (n+1)) placements;
  Hashtbl.iter (fun k v -> Printf.printf "  orient %-3s : %d\n" k v) by_orient;
  Printf.printf "\nAll recovered:\n";
  List.iter (fun p ->
    Printf.printf "  %-30s  %-20s  (%d, %d) %s\n"
      p.Placement.inst p.Placement.cell p.Placement.x p.Placement.y
      (Placement.string_of_orient p.Placement.orient)) placements;
  let xs = List.map (fun p -> p.Placement.x) placements in
  let ys = List.map (fun p -> p.Placement.y) placements in
  let mn = List.fold_left min max_int in
  let mx = List.fold_left max min_int in
  if xs <> [] then
    Printf.printf "\nBBox: x=[%d..%d]  y=[%d..%d]\n"
      (mn xs) (mx xs) (mn ys) (mx ys)
