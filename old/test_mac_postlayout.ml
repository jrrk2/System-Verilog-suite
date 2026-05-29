(* MAC post-layout critical-path resynthesis demo.

   End-to-end story for a real placed-and-CTS'd MAC:

     1. Load the post-CTS DEF + cell-mapped Verilog produced by ORFS
        for the Synth_mac baseline (Array×Ripple).
     2. Build the placement-aware timing graph; identify the worst
        endpoint and its fanout cone.
     3. Bind the cone's instances to "u_mul" / "u_add" by their
        synth_mac stage prefix (g_s<S>_b<B>) — the multiplier reduction
        lives in stages [0, mul_d) and the final adder in [mul_d, total).
     4. For each candidate (Wallace/Dadda/...; Brent-Kung/Sklansky/
        Kogge-Stone/...) project the new cone arrival via
        Predict_swap.predict.  Each candidate pulls a [verify-arch]
        certificate; report cert state alongside the prediction.
     5. Print the cert-gated best pick and the unproven candidates'
        verify commands.

   The point of running through real ORFS instead of synth_mac's
   built-in placement grid: the predictions go against actual
   placer-driven row/col coordinates and CTS-inserted buffers, which
   is closer to what production deployment would see.

   Usage:
       test_mac_postlayout <def> <verilog> [width]
*)

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
    | _ -> width
  in
  let df = float_of_int (depth_of from_a) in
  let dt = float_of_int (depth_of to_a) in
  if df > 0.0 then dt /. df else 1.0

(* Inst names from synth_mac are g_s<stage>_b<bit>.  Pull the stage
   from the inst name; return -1 if the pattern doesn't match. *)
let stage_of_inst name =
  let re = Str.regexp "^g_s\\([0-9]+\\)_b" in
  if Str.string_match re name 0 then
    int_of_string (Str.matched_group 1 name)
  else -1

let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "usage: %s <def> <verilog> [width]\n" Sys.argv.(0);
    exit 1
  end;
  let def_path = Sys.argv.(1) in
  let verilog_path = Sys.argv.(2) in
  let width =
    if Array.length Sys.argv > 3 then int_of_string Sys.argv.(3) else 8 in
  let lib_path = "lef_def/test/nangate.lib" in
  let lef_path = "lef_def/test/nangate.lef" in

  Printf.printf
    "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  MAC post-layout critical-path resynthesis\n";
  Printf.printf
    "═══════════════════════════════════════════════════════════════\n";
  Printf.printf "  def:     %s\n"  def_path;
  Printf.printf "  verilog: %s\n"  verilog_path;
  Printf.printf "  width:   %d\n\n" width;

  let pair = Cell_delay.load_arc_table lib_path in
  let dsf cell ~slew ~load =
    Cell_delay.delay_and_slew ~slew ~load pair cell in
  let pin_dir = Lef_pins.table_of_entries (Lef_pins.parse lef_path) in

  let placements = Placement.parse def_path in
  let nets       = Nets.parse def_path in
  Printf.printf "Loaded %d cells, %d nets from DEF\n"
    (List.length placements) (List.length nets);
  let _ = verilog_path in

  let plc_tbl = Hpwl.placement_table placements in
  let cell_of = Hashtbl.create (List.length placements) in
  List.iter (fun (p : Placement.placement) ->
    Hashtbl.replace cell_of p.Placement.inst p.Placement.cell) placements;
  let edges = Placement_timing.fanout_edges
                ~cell_of ~pin_dir plc_tbl nets in
  let arr   = Placement_timing.arrival_table_forward
                ~edges ~plc_tbl ~delay_slew_fn:dsf placements in
  let fanin = Fanout_cone.invert_edges edges in

  (match Fanout_cone.worst_endpoint arr with
   | None -> Printf.printf "no endpoint found — empty arrival table\n"
   | Some (ep, ep_arr) ->
       Printf.printf "\nWorst endpoint: %s @ %.2f ps\n" ep ep_arr;
       List.iter (fun b ->
         let cone = Fanout_cone.cone_of_endpoint
                      ~slack_budget:b ~arrival_tbl:arr ~fanin
                      ~endpoint:ep () in
         let hist = Fanout_cone.cone_cell_histogram placements cone in
         Printf.printf "  budget=%4.0f ps  cone=%4d cells   top: " b
           (List.length cone);
         List.iteri (fun i (c, n) ->
           if i < 5 then Printf.printf "%s=%d " c n) hist;
         print_newline ()) [ 0.0; 50.0; 200.0; 500.0 ]);
  Printf.printf "\n";

  (* Bind synth_mac instances to u_mul / u_add by stage. *)
  let mul_d = Synth_mac.mul_depth ~arch:Synth_mac.Array_m ~width in
  let total_d = mul_d + Synth_mac.adder_depth
                          ~arch:Synth_mac.Ripple_a ~width:(2 * width) in
  let in_stage_range s lo hi = s >= lo && s < hi in
  let bind lo hi name =
    let ms = List.filter (fun (p : Placement.placement) ->
      let s = stage_of_inst p.inst in
      s >= 0 && in_stage_range s lo hi) placements in
    { Bir_def_bind.bir_path = name; members = ms } in
  let bindings = [
    bind 0     mul_d   "u_mul";
    bind mul_d total_d "u_add";
  ] in
  Printf.printf "Bindings (by synth_mac stage prefix):\n";
  List.iter (fun b ->
    Printf.printf "  %-8s  %4d cells\n"
      b.Bir_def_bind.bir_path (List.length b.members)) bindings;
  Printf.printf "\n";

  let baseline_mul = "array" in
  let baseline_add = "ripple" in
  let mul_alts = ["wallace"; "dadda"] in
  let add_alts = ["brent_kung"; "sklansky"; "kogge_stone"] in
  let candidates =
    List.map (fun alt -> {
      Predict_swap.binding_name = "u_mul";
      current_arch = baseline_mul;
      to_arch = alt;
      kind = "mul";
      width;
      depth_factor =
        depth_ratio `Mul ~from_a:baseline_mul ~to_a:alt ~width;
    }) mul_alts
    @
    List.map (fun alt -> {
      Predict_swap.binding_name = "u_add";
      current_arch = baseline_add;
      to_arch = alt;
      kind = "adder";
      width = 2 * width;
      depth_factor =
        depth_ratio `Add ~from_a:baseline_add ~to_a:alt ~width:(2 * width);
    }) add_alts in

  let (ep, ep_arr), cone, preds =
    Predict_swap.predict ~arrival_tbl:arr ~fanin
      ~bindings ~candidates () in
  Printf.printf "Worst endpoint considered: %s @ %.2f ps  (cone size %d)\n\n"
    ep ep_arr (List.length cone);

  Printf.printf
    "  %-8s  %-12s -> %-12s  cells  cone_delay   savings   new_arr  cert\n"
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
       Printf.printf
         "swap %s from %s to %s -> %.1f ps -> %.1f ps  (saves %.1f ps, cert OK)\n"
         top.cand.binding_name top.cand.current_arch top.cand.to_arch
         ep_arr top.predicted_new_arr top.predicted_savings);

  let needed = Predict_swap.verify_arch_commands_needed preds in
  if needed <> [] then begin
    Printf.printf "\nUnproven candidates — run these to unlock more swaps:\n";
    List.iter (fun s -> Printf.printf "  %s\n" s) needed
  end
