(* End-to-end demo for #96.

   1.  Build the baseline MAC, run predict_swap, take the
       top-1 cert-gated recommendation.
   2.  "Apply" the swap by re-synthesising with the new arch
       (Synth_mac.build with the new arch is our stand-in for
        an actual yosys+OpenROAD ECO round-trip).
   3.  Emit both pre- and post-swap Verilog, parse them back
       via Gate_verilog.
   4.  Run post_swap_check: re-prove the arch via Arch_verify,
       and confirm the I/O port signature is unchanged. *)

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

let arch_of_string_add = function
  | "ripple"      -> Synth_mac.Ripple_a
  | "kogge_stone" -> Synth_mac.Kogge_stone_a
  | "brent_kung"  -> Synth_mac.Brent_kung_a
  | "sklansky"    -> Synth_mac.Sklansky_a
  | s -> failwith ("unknown adder: " ^ s)

let dump_v ~module_name nl path =
  let oc = open_out path in
  Synth_mac.emit_verilog ~module_name ~oc nl;
  close_out oc;
  match Gate_verilog.parse_file path with
  | m :: _ -> m
  | [] -> failwith "empty Verilog parse"

let () =
  let lib_path = "lef_def/test/nangate.lib" in
  let lef_path = "lef_def/test/nangate.lef" in
  let width = 8 in

  let pair = Cell_delay.load_arc_table lib_path in
  let dsf cell ~slew ~load = Cell_delay.delay_and_slew ~slew ~load pair cell in
  let pin_dir_lef = Lef_pins.table_of_entries (Lef_pins.parse lef_path) in

  let pre_nl = Synth_mac.build ~width
                 ~mul_arch:Synth_mac.Array_m
                 ~add_arch:Synth_mac.Ripple_a () in
  let plc_tbl = Hpwl.placement_table pre_nl.cells in
  let cell_of = Hashtbl.create 256 in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) pre_nl.cells;
  let edges = Placement_timing.fanout_edges
                ~cell_of ~pin_dir:pin_dir_lef plc_tbl pre_nl.nets in
  let arrival_tbl = Placement_timing.arrival_table_forward
                      ~edges ~plc_tbl ~delay_slew_fn:dsf pre_nl.cells in
  let fanin = Fanout_cone.invert_edges edges in
  let mul_d = Synth_mac.mul_depth ~arch:Synth_mac.Array_m ~width in
  let bind_by_stage stage_lo stage_hi name =
    let ms = List.filter (fun (p : Placement.placement) ->
      let stage = p.y / 2800 in
      stage >= stage_lo && stage < stage_hi) pre_nl.cells in
    { Bir_def_bind.bir_path = name; members = ms } in
  let bindings = [
    bind_by_stage 0 mul_d "u_mul";
    bind_by_stage mul_d pre_nl.depth "u_add";
  ] in
  let candidates =
    List.map (fun alt -> {
      Predict_swap.binding_name = "u_add";
      current_arch = "ripple"; to_arch = alt;
      kind = "adder"; width = 2 * width;
      depth_factor = depth_ratio `Add ~from_a:"ripple" ~to_a:alt
                       ~width:(2*width);
    }) ["sklansky"; "kogge_stone"; "brent_kung"] in
  let (_, _, preds) =
    Predict_swap.predict ~arrival_tbl ~fanin ~bindings ~candidates () in
  let proven = Predict_swap.proven_only preds in
  match proven with
  | [] -> print_endline "no proven candidate"; exit 1
  | top :: _ ->
      Printf.printf "Top recommendation: %s/%s -> %s  (%.1f ps savings)\n\n"
        top.cand.binding_name top.cand.current_arch top.cand.to_arch
        top.predicted_savings;

      (* Stand-in for "user runs yosys + openroad": rebuild
         the MAC with the new arch. *)
      let post_nl = Synth_mac.build ~width
                      ~mul_arch:Synth_mac.Array_m
                      ~add_arch:(arch_of_string_add top.cand.to_arch)
                      () in
      let pre_path  = Filename.temp_file "pre_"  ".v" in
      let post_path = Filename.temp_file "post_" ".v" in
      let pre_m  = dump_v ~module_name:"mac" pre_nl  pre_path  in
      let post_m = dump_v ~module_name:"mac" post_nl post_path in

      Printf.printf "pre  : %s  (%d cells)\n"
        pre_path  (List.length pre_m.cells);
      Printf.printf "post : %s  (%d cells)\n\n"
        post_path (List.length post_m.cells);

      (* Run the post-swap check *)
      Printf.printf "Running post_swap_check...\n%!";
      let r = Post_swap_check.check
                ~pre_module:pre_m ~post_module:post_m
                ~prediction:top in
      Printf.printf "\n%s\n" (Post_swap_check.pp_result r);
      (match r.arch_proof, r.signature with
       | Post_swap_check.Proof_ok _, Post_swap_check.Sig_match ->
           print_endline "\nOK   post-swap formal check PASSED"
       | _ ->
           print_endline "\nFAIL post-swap formal check did not pass";
           exit 1)
