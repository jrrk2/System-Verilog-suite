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
      Printf.printf "AND2_X1: %d delay arcs, %d slew arcs\n"
        (List.length a.arcs) (List.length a.slew_arcs);
      (match a.arcs with
       | [] -> ()
       | lut :: _ ->
          Printf.printf "  axis_slew[0..%d]: " (Array.length lut.Cell_delay.axis_slew - 1);
          Array.iter (Printf.printf "%g ") lut.axis_slew;
          Printf.printf "\n  axis_load[0..%d]: " (Array.length lut.axis_load - 1);
          Array.iter (Printf.printf "%g ") lut.axis_load;
          print_newline ());
      Printf.printf "\nInterpolated delay+slew (ps, ns):\n";
      List.iter (fun (s, l) ->
        let d, os =
          Cell_delay.delay_and_slew ~slew:s ~load:l (tbl, scale) "AND2_X1" in
        Printf.printf
          "  @ slew=%5.4f load=%4.2g  -> delay %.2f ps, out_slew %.4f ns\n"
          s l d os)
        [ 0.003, 1.0;  0.005, 1.0;  0.005, 2.0;  0.005, 3.0;
          0.01, 1.0;   0.01, 2.0;   0.05, 4.0 ];

      Printf.printf "\nFixed-point under load=2 (typical fanout):\n";
      let s = ref 0.005 in
      for i = 0 to 5 do
        let _, os =
          Cell_delay.delay_and_slew ~slew:!s ~load:2.0 (tbl, scale) "AND2_X1" in
        Printf.printf "  iter %d: slew_in=%.4f -> slew_out=%.4f\n"
          i !s os;
        s := os
      done
