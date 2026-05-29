(* Demo for #94: take a top-ranked predict_swap recommendation
   and emit an OpenROAD-shaped ECO change-list.

   Pipeline:
     1.  Build a synth MAC, run predict_swap to get the best
         certified candidate (re-using #93+#95 wiring).
     2.  Emit the ECO scripts (eco.ys + eco.tcl + eco_swaps.txt)
         to a tmp directory.
     3.  Print the artefact paths and dump each script so the
         user can read the recipe immediately. *)

open Lef_def

let depth_ratio kind ~from_a ~to_a ~width =
  let depth_of arch =
    match kind, arch with
    | `Add, "ripple"      -> width
    | `Add, "brent_kung"  -> max 1 (2 * Synth_mac.log2_ceil width - 1)
    | `Add, "sklansky"    -> Synth_mac.log2_ceil width
    | `Add, "kogge_stone" -> Synth_mac.log2_ceil width
    | `Mul, "array"       -> 2 * width
    | `Mul, "wallace"     -> Synth_mac.log2_ceil width + width
    | `Mul, "dadda"       -> Synth_mac.log2_ceil width + width
    | _ -> width in
  let df = float_of_int (depth_of from_a) in
  let dt = float_of_int (depth_of to_a) in
  if df > 0.0 then dt /. df else 1.0

let () =
  let lib_path = "lef_def/test/nangate.lib" in
  let lef_path = "lef_def/test/nangate.lef" in
  let width = 8 in

  (* timing pipeline → predictions *)
  let pair = Cell_delay.load_arc_table lib_path in
  let dsf cell ~slew ~load = Cell_delay.delay_and_slew ~slew ~load pair cell in
  let pin_dir = Lef_pins.table_of_entries (Lef_pins.parse lef_path) in
  let nl = Synth_mac.build ~width
             ~mul_arch:Synth_mac.Array_m
             ~add_arch:Synth_mac.Ripple_a () in
  let plc_tbl = Hpwl.placement_table nl.cells in
  let cell_of = Hashtbl.create 256 in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) nl.cells;
  let edges = Placement_timing.fanout_edges
                ~cell_of ~pin_dir plc_tbl nl.nets in
  let arrival_tbl = Placement_timing.arrival_table_forward
                      ~edges ~plc_tbl ~delay_slew_fn:dsf nl.cells in
  let fanin = Fanout_cone.invert_edges edges in
  let mul_d = Synth_mac.mul_depth ~arch:Synth_mac.Array_m ~width in
  let bind_by_stage stage_lo stage_hi name =
    let ms = List.filter (fun (p : Placement.placement) ->
      let stage = p.y / 2800 in
      stage >= stage_lo && stage < stage_hi) nl.cells in
    { Bir_def_bind.bir_path = name; members = ms } in
  let bindings = [
    bind_by_stage 0 mul_d "u_mul";
    bind_by_stage mul_d nl.depth "u_add";
  ] in
  let candidates =
    List.filter_map (fun alt ->
      Some {
        Predict_swap.binding_name = "u_add";
        current_arch = "ripple"; to_arch = alt;
        kind = "adder"; width = 2 * width;
        depth_factor = depth_ratio `Add ~from_a:"ripple" ~to_a:alt
                         ~width:(2*width);
      })
      [ "sklansky"; "kogge_stone"; "brent_kung" ] in
  let (_, _, preds) =
    Predict_swap.predict ~arrival_tbl ~fanin ~bindings ~candidates () in
  let proven = Predict_swap.proven_only preds in
  match proven with
  | [] -> print_endline "no proven candidate; aborting"; exit 1
  | top :: _ ->
      Printf.printf "Top recommendation: %s/%s -> %s\n"
        top.cand.binding_name top.cand.current_arch top.cand.to_arch;
      Printf.printf "  predicted savings : %.1f ps\n" top.predicted_savings;
      Printf.printf "  predicted new arr : %.1f ps\n\n" top.predicted_new_arr;

      (* Bbox of the binding for ECO region *)
      let binding =
        List.find (fun b -> b.Bir_def_bind.bir_path = top.cand.binding_name)
          bindings in
      let bbox = Bir_def_bind.bbox binding in

      let out_dir = Filename.temp_file "eco_demo_" "" in
      Sys.remove out_dir;
      let arts = Eco_emit.emit
        ~out_dir
        ~rtl_path:"src/mac.sv"   (* placeholder — would be real RTL path *)
        ~top:"mac"
        ~def_path:"build/mac.def"
        ~lib_path:lib_path
        ~lef_path:lef_path
        ~bbox
        ~prediction:top in

      Printf.printf "Artefacts in %s:\n" out_dir;
      Printf.printf "  %s\n  %s\n  %s\n  (will be written by yosys: %s)\n\n"
        arts.yosys_path arts.openroad_path arts.log_path arts.new_v_path;

      let dump path =
        let ic = open_in path in
        Printf.printf "── %s ────────────────────────────────────────\n" path;
        try while true do print_endline (input_line ic) done
        with End_of_file -> close_in ic; print_newline ()
      in
      dump arts.log_path;
      dump arts.yosys_path;
      dump arts.openroad_path
