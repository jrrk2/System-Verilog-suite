(* End-to-end placement+nets+HPWL smoke. *)
open Lef_def

let () =
  let path = if Array.length Sys.argv > 1 then Sys.argv.(1) else "real.def" in
  let placements = Placement.parse path in
  let nets       = Nets.parse path in
  let s = Hpwl.stats placements nets in
  Printf.printf "Placements    : %d\n" (List.length placements);
  Printf.printf "Nets (total)  : %d\n" s.Hpwl.n_nets;
  Printf.printf "Nets (signal) : %d\n" s.Hpwl.n_signal;
  Printf.printf "Total HPWL    : %d dbu\n" s.Hpwl.total_hpwl;
  Printf.printf "Worst net     : %s = %d dbu\n"
    s.Hpwl.max_net_name s.Hpwl.max_net_hpwl;
  let xa, xb = s.Hpwl.bbox_x and ya, yb = s.Hpwl.bbox_y in
  Printf.printf "BBox          : x=[%d..%d] y=[%d..%d]\n" xa xb ya yb;
  let avg = if s.Hpwl.n_signal > 0
            then s.Hpwl.total_hpwl / s.Hpwl.n_signal else 0 in
  Printf.printf "Avg HPWL/net  : %d dbu\n" avg
