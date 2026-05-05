(* Sanity-check Cell_delay arc extraction on a real Liberty.

   Reports how many cells have arc tables, dumps a sample
   AND2_X1 LUT, and shows interpolated delay at a few
   (slew, load) corners.  Useful when bringing up a new
   library — the rewriter has multiple paths for cell_rise/
   cell_fall depending on whether the timing-template name
   was quoted or bare in the source. *)

let () =
  let lib_path =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "lef_def/test/nangate.lib" in
  let pair = Cell_delay.load_arc_table lib_path in
  let tbl, scale = pair in
  Printf.printf "Liberty %s\n" lib_path;
  Printf.printf "  scale = %g (Liberty unit -> ps)\n" scale;
  Printf.printf "  cells in table  : %d\n" (Hashtbl.length tbl);
  let n_with_arcs = ref 0 in
  Hashtbl.iter (fun _ a ->
    if a.Cell_delay.arcs <> [] then incr n_with_arcs) tbl;
  Printf.printf "  cells WITH arcs : %d\n\n" !n_with_arcs;
  match Hashtbl.find_opt tbl "AND2_X1" with
  | None -> print_endline "no AND2_X1 in table"
  | Some a ->
      Printf.printf "AND2_X1: %d arcs\n" (List.length a.arcs);
      (match a.arcs with
       | [] -> ()
       | lut :: _ ->
          Printf.printf "  axis_slew[0..%d]: " (Array.length lut.Cell_delay.axis_slew - 1);
          Array.iter (Printf.printf "%g ") lut.axis_slew;
          Printf.printf "\n  axis_load[0..%d]: " (Array.length lut.axis_load - 1);
          Array.iter (Printf.printf "%g ") lut.axis_load;
          print_newline ());
      Printf.printf "\nInterpolated delay (ps):\n";
      List.iter (fun (s, l) ->
        let v = Cell_delay.at ~slew:s ~load:l (tbl, scale) "AND2_X1" in
        Printf.printf "  AND2_X1 @ slew=%5.4f load=%4.2g  -> %.2f ps\n"
          s l v)
        [ 0.001, 0.5;  0.005, 1.0;  0.01, 1.0;
          0.05, 4.0;   0.1, 8.0 ]
