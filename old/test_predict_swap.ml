(* Predict-only resynthesis demo (#93).

   Build a placed MAC with a chosen baseline (mul, adder) arch.
   Identify the worst endpoint and its critical-path cone.  For
   each candidate (multiplier_arch -> alternative, adder_arch ->
   alternative), project what the worst arrival would become if
   the swap were applied.  Sort by savings; that's the
   recommendation list a cert-gated swap would consume.

   Read-only: nothing in the netlist or DEF is modified.  The
   point is to identify which swap is worth attempting before
   we spend the cert-verification cycles. *)

open Lef_def

(* Analytical depth ratios used as the projection factor.
   These are the same numbers Synth_mac uses internally; we
   replicate them here because the prediction layer should be
   independent of the synth fixture (a real placed design has
   no Synth_mac call in its history). *)
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
    | _ -> width
  in
  let df = float_of_int (depth_of from_a) in
  let dt = float_of_int (depth_of to_a) in
  if df > 0.0 then dt /. df else 1.0

let () =
  let lib_path = "lef_def/test/nangate.lib" in
  let lef_path = "lef_def/test/nangate.lef" in
  let width =
    if Array.length Sys.argv > 1 then int_of_string Sys.argv.(1) else 8 in
  let baseline_mul = "array" in
  let baseline_add = "ripple" in

  Printf.printf "Predict-only resynthesis demo\n";
  Printf.printf "  baseline: width=%d  mul=%s  add=%s\n\n"
    width baseline_mul baseline_add;

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

  (* The synth_mac netlist uses inst names "g_s<stage>_b<bit>".
     We bind into two virtual prefixes by stage range:
       u_mul   = stages [0 .. mul_d - 1]
       u_add   = stages [mul_d .. total_d - 1]
     so the prediction layer can suggest swapping each
     independently. *)
  let mul_d = Synth_mac.mul_depth ~arch:Synth_mac.Array_m ~width in
  let bind_by_stage stage_lo stage_hi name =
    let ms = List.filter (fun (p : Placement.placement) ->
      let stage = p.y / 2800 in   (* row_pitch from synth_mac *)
      stage >= stage_lo && stage < stage_hi) nl.cells in
    { Bir_def_bind.bir_path = name; members = ms } in
  let bindings = [
    bind_by_stage 0 mul_d "u_mul";
    bind_by_stage mul_d (nl.depth) "u_add";
  ] in

  Printf.printf "Bindings:\n";
  List.iter (fun b ->
    Printf.printf "  %-8s  %4d cells\n"
      b.Bir_def_bind.bir_path (List.length b.members)) bindings;

  (* Enumerate candidates: every alternative arch for each
     binding, holding the other binding fixed. *)
  let candidates =
    let mul_alts = ["array"; "wallace"; "dadda"] in
    let add_alts = ["ripple"; "brent_kung"; "sklansky"; "kogge_stone"] in
    List.filter_map (fun alt ->
      if alt = baseline_mul then None
      else Some {
        Predict_swap.binding_name = "u_mul";
        current_arch = baseline_mul;
        to_arch = alt;
        kind = "mul";
        width = width;
        depth_factor =
          depth_ratio `Mul ~from_a:baseline_mul ~to_a:alt ~width;
      }) mul_alts
    @
    List.filter_map (fun alt ->
      if alt = baseline_add then None
      else Some {
        Predict_swap.binding_name = "u_add";
        current_arch = baseline_add;
        to_arch = alt;
        kind = "adder";
        width = 2 * width;
        depth_factor =
          depth_ratio `Add ~from_a:baseline_add ~to_a:alt ~width:(2*width);
      }) add_alts
  in

  let (ep, ep_arr), cone, preds =
    Predict_swap.predict ~arrival_tbl ~fanin
      ~bindings ~candidates () in
  Printf.printf "\nWorst endpoint: %s @ %.2f ps  (cone size %d)\n\n"
    ep ep_arr (List.length cone);

  Printf.printf "  %-8s  %-12s -> %-12s  cells  cone_delay   savings   new_arr  cert\n"
    "binding" "current" "to";
  Printf.printf "  %s\n" (String.make 84 '-');
  List.iter (fun p ->
    Printf.printf
      "  %-8s  %-12s -> %-12s  %3d   %8.1f ps %8.1f ps %8.1f ps  %s\n"
      p.Predict_swap.cand.binding_name
      p.cand.current_arch p.cand.to_arch
      p.cells_in_cone
      p.cone_delay_ps
      p.predicted_savings
      p.predicted_new_arr
      (Predict_swap.string_of_cert p.cert)) preds;

  let proven = Predict_swap.proven_only preds in
  Printf.printf "\nBest pick (cert-gated): ";
  (match proven with
   | [] -> print_endline "no PROVEN candidate produces savings"
   | top :: _ ->
       Printf.printf "swap %s from %s to %s -> %.1f ps -> %.1f ps  (saves %.1f ps, cert OK)\n"
         top.cand.binding_name top.cand.current_arch top.cand.to_arch
         ep_arr top.predicted_new_arr top.predicted_savings);

  let needed = Predict_swap.verify_arch_commands_needed preds in
  if needed <> [] then begin
    Printf.printf "\nUnproven candidates — run these to unlock more swaps:\n";
    List.iter (fun s -> Printf.printf "  %s\n" s) needed
  end;

  (* Verification: re-synth with the recommended arch and compare
     against the prediction.  This checks the projection model
     against the actual placement-timing pipeline. *)
  Printf.printf "\nVerification — re-synthesise with the top recommendation\n";
  Printf.printf "and compare predicted vs measured arrival:\n\n";
  let arch_of_string_mul = function
    | "array"   -> Synth_mac.Array_m
    | "wallace" -> Synth_mac.Wallace_m
    | "dadda"   -> Synth_mac.Dadda_m
    | s -> failwith ("unknown mul arch: " ^ s) in
  let arch_of_string_add = function
    | "ripple"      -> Synth_mac.Ripple_a
    | "kogge_stone" -> Synth_mac.Kogge_stone_a
    | "brent_kung"  -> Synth_mac.Brent_kung_a
    | "sklansky"    -> Synth_mac.Sklansky_a
    | s -> failwith ("unknown add arch: " ^ s) in
  Printf.printf "  %-8s %-12s %-12s | %10s | %10s | %5s\n"
    "binding" "from" "to" "predicted" "measured" "err";
  Printf.printf "  %s\n" (String.make 70 '-');
  List.iter (fun p ->
    let ma, aa =
      match p.Predict_swap.cand.binding_name with
      | "u_mul" ->
          (arch_of_string_mul p.cand.to_arch, Synth_mac.Ripple_a)
      | "u_add" ->
          (Synth_mac.Array_m, arch_of_string_add p.cand.to_arch)
      | _ -> (Synth_mac.Array_m, Synth_mac.Ripple_a) in
    let nl' = Synth_mac.build ~width ~mul_arch:ma ~add_arch:aa () in
    let plc' = Hpwl.placement_table nl'.cells in
    let cof' = Hashtbl.create 256 in
    List.iter (fun (q : Placement.placement) ->
      Hashtbl.replace cof' q.Placement.inst q.Placement.cell) nl'.cells;
    let edges' = Placement_timing.fanout_edges
                   ~cell_of:cof' ~pin_dir plc' nl'.nets in
    let arr' = Placement_timing.arrival_table_forward
                 ~edges:edges' ~plc_tbl:plc' ~delay_slew_fn:dsf nl'.cells in
    let m_arr = match Fanout_cone.worst_endpoint arr' with
      | Some (_, a) -> a | None -> 0.0 in
    let err_pct = 100.0 *. (p.predicted_new_arr -. m_arr) /. m_arr in
    Printf.printf "  %-8s %-12s %-12s | %8.1f ps | %8.1f ps | %+4.1f%%\n"
      p.cand.binding_name p.cand.current_arch p.cand.to_arch
      p.predicted_new_arr m_arr err_pct) preds
