(* Per-entity FF-set comparator for CVA6.
 *
 * For every entity present in the Vivado-elaborated VHDL, extract the
 * FF set (Q__Q + Q__D signal names from Behavioral_ffrip) on:
 *   - the Vivado side  (cva6_elab.vhd → vhdl_to_ver_front)
 *   - the Verible side (SV files → verible_to_behavioral)
 *   - optionally, the Verilator side (V*.tree.json) for sanity
 *
 * Reports a per-entity similarity score (Jaccard over Q-name set) and
 * lists what's only-in-Vivado / only-in-Verible. Sorted closest-first
 * — the smallest non-zero "only" lists are the best triage candidates
 * for the next elaboration fix.
 *
 * Args:
 *   test_cva6_ff_diff <vivado.vhd> [verilator.json] [filter ...]
 * Env:
 *   MITER_VERIBLE_FILES = colon-separated SV source list
 *   MITER_VERIBLE_TOP   = top entity name for Verible elaboration *)

open Behavioral_ir

let usage () =
  Printf.eprintf
    "usage: %s <vivado.vhd> [verilator.json|-] [filter ...]\n\
     env: MITER_VERIBLE_FILES (colon-sep), MITER_VERIBLE_TOP\n"
    Sys.argv.(0);
  exit 2

let contains substr s =
  let ls = String.length s and lp = String.length substr in
  if lp = 0 then true
  else if lp > ls then false
  else
    let rec loop i =
      if i + lp > ls then false
      else if String.sub s i lp = substr then true
      else loop (i + 1)
    in loop 0

let strip_suffix s sfx =
  let ls = String.length s and lf = String.length sfx in
  if ls > lf && String.sub s (ls - lf) lf = sfx
  then Some (String.sub s 0 (ls - lf))
  else None

let ff_q_set (m : bmodule) =
  let m = Behavioral_ffrip.rip_module m in
  List.filter_map (fun (s : bsignal) ->
    if s.direction = `Input then strip_suffix s.name "__Q" else None
  ) m.signals
  |> List.sort_uniq compare

let jaccard a b =
  let inter = List.filter (fun x -> List.mem x b) a |> List.length in
  let union = List.length a + List.length b - inter in
  if union = 0 then 1.0 else float inter /. float union

let truncate s w =
  if String.length s <= w then s
  else String.sub s 0 (max 0 (w - 1)) ^ "…"

let () =
  if Array.length Sys.argv < 2 then usage ();
  let vhd_file = Sys.argv.(1) in
  let json_file =
    if Array.length Sys.argv >= 3 && Sys.argv.(2) <> "-" then Some Sys.argv.(2)
    else None
  in
  let filter_start = if json_file = None then 2 else 3 in
  let filters =
    if Array.length Sys.argv > filter_start
    then Array.to_list (Array.sub Sys.argv filter_start
                          (Array.length Sys.argv - filter_start))
    else []
  in
  let matches name =
    filters = [] || List.exists (fun s -> contains s name) filters
  in

  Printf.printf "[1/3] Loading Vivado VHDL → BIR ...\n%!";
  let viv =
    match Vhdl_to_ver_front.convert_vhd_file vhd_file with
    | Some p -> p
    | None -> Printf.eprintf "Vivado side load failed\n"; exit 1
  in
  Printf.printf "  %d Vivado modules\n" (List.length viv.modules);

  let verilator =
    match json_file with
    | None -> None
    | Some jf ->
        Printf.printf "[2/3] Loading Verilator JSON → BIR ...\n%!";
        match Verilator_to_behavioral.convert_verilator_json_to_behavioral jf with
        | Some p ->
            Printf.printf "  %d Verilator modules\n" (List.length p.modules);
            Some p
        | None -> Printf.eprintf "  Verilator side load failed (skipping)\n"; None
  in

  Printf.printf "[3/3] Loading Verible SV → BIR ...\n%!";
  let verible =
    match Sys.getenv_opt "MITER_VERIBLE_FILES",
          Sys.getenv_opt "MITER_VERIBLE_TOP" with
    | Some flist, Some top ->
        let files = String.split_on_char ':' flist
                    |> List.filter (fun s -> s <> "") in
        (try
          let p = Verible_to_behavioral.convert_files ~top files in
          (* Same pipeline test_cva6_bottom_up runs on its SV side:
           * unroll genvar bodies, inline functions/tasks, lift if-
           * conditioned blocks, recognise array→memory patterns. The
           * Vivado side already comes from elaborated VHDL so doesn't
           * need this. Without these the FF analysis sees BCalls and
           * unexpanded loops on the Verible side that don't exist on
           * Vivado, inflating the no-match count. *)
          let p =
            p
            |> Behavioral_unroll.unroll_program
            |> Behavioral_inline.inline_program
            |> Behavioral_iflift.lift_program
            |> Behavioral_blocking_subst.blocking_subst_program
            |> Behavioral_meminfer.infer_program
            |> Behavioral_flatten.flatten_program
          in
          Some p
         with e ->
           Printf.eprintf "  Verible failed: %s\n" (Printexc.to_string e);
           None)
    | _ ->
        Printf.eprintf "  MITER_VERIBLE_FILES / MITER_VERIBLE_TOP unset\n";
        None
  in
  (match verible with
   | Some p ->
       Printf.printf "  %d Verible modules\n" (List.length p.modules);
       if Sys.getenv_opt "MITER_DEBUG_NAMES" <> None then
         List.iter (fun (m : bmodule) ->
           Printf.eprintf "  vrb:  %s\n" m.name) p.modules
   | None -> ());

  if Sys.getenv_opt "MITER_DEBUG_NAMES" <> None then
    List.iter (fun (m : bmodule) ->
      Printf.eprintf "  viv:  %s\n" m.name) viv.modules;

  let by_name p =
    List.fold_left (fun acc (m : bmodule) -> (m.name, m) :: acc) [] p.modules
  in
  let viv_idx = by_name viv in
  let vlt_idx = match verilator with Some p -> by_name p | None -> [] in
  let vrb_idx = match verible with Some p -> by_name p | None -> [] in

  (* Pair Vivado entity → other-frontend entity:
   *   1. exact name match
   *   2. base name match after stripping Vivado's `__parameterized<N>`
   *      OR the verible-style suffix __W..._M... etc.
   *   3. port-shape match (same set of {name, width} for I/O signals)
   * Step 3 handles Vivado's anonymous `__parameterized<N>` numbering
   * vs. Verible's parameter-value-encoded names. *)
  let base_of n =
    try
      let i = Str.search_forward (Str.regexp "__") n 0 in
      String.sub n 0 i
    with Not_found -> n
  in
  let port_shape (m : bmodule) =
    List.filter_map (fun (s : bsignal) ->
      match s.direction with
      | `Input | `Output ->
          let w = match s.stype with
            | BInt { width; _ } -> width
            | BArray { size; element = BInt { width; _ }; _ } -> size * width
            | _ -> 0
          in
          Some (s.name, w)
      | _ -> None
    ) m.signals
    |> List.sort compare
  in
  (* Include FF-rip Q__Q widths too — for sequential modules these
   * carry the post-elaboration register-state widths and can differ
   * between Vivado (which always elaborates fully) and Verible (which
   * picks per-spec widths). Used as the SECOND line of defense for
   * pairing, after the bare-port shape. *)
  let ff_widths (m : bmodule) =
    let m = Behavioral_ffrip.rip_module m in
    List.filter_map (fun (s : bsignal) ->
      let n = s.name in
      let l = String.length n in
      let is_qq = l > 3 && String.sub n (l - 3) 3 = "__Q" in
      if is_qq then
        let w = match s.stype with
          | BInt { width; _ } -> width
          | BArray { size; element = BInt { width; _ }; _ } -> size * width
          | _ -> 0
        in
        Some (n, w)
      else None
    ) m.signals
    |> List.sort compare
  in
  (* Internal signals would be too strict — Vivado synth introduces
   * RTL_* decomposition wires that Verible's pre-synth BIR doesn't
   * have, so internal-set equality nukes legitimate matches. Stick
   * with port + FF widths; the residual width-mismatch ⚠s on lfsr
   * etc. are real bugs to chase, not pairing artefacts to hide. *)
  let shapes_match a b =
    port_shape a = port_shape b && ff_widths a = ff_widths b
  in
  let lookup idx (viv_m : bmodule) =
    let name = viv_m.name in
    let exact_match =
      match List.assoc_opt name idx with
      | Some m when shapes_match m viv_m -> Some m
      | _ -> None
    in
    match exact_match with
    | Some m -> Some m
    | None ->
        let base = base_of name in
        let candidates =
          List.filter (fun (n, _) -> base_of n = base) idx in
        (match List.find_opt (fun (_, m) -> shapes_match m viv_m)
                 candidates with
         | Some (_, m) -> Some m
         | None -> None)
  in

  Printf.printf "\n";
  (* Per-entity signal dump for triage. Set
   *   MITER_DUMP_SIGNALS=<substr>
   * and the comparator prints, for every Vivado entity whose name
   * contains the substring, a side-by-side {direction, name, width}
   * table of Vivado-side and Verible-side bsignals. Useful when the
   * Z3 miter errors with a BitVec sort mismatch but the headline
   * port_shape/ff_widths checks pass — the differing signal lives
   * in the internals and the dump shows it. *)
  let dump_filter = Sys.getenv_opt "MITER_DUMP_SIGNALS" in
  let signal_summary (m : bmodule) =
    List.map (fun (s : bsignal) ->
      let dir = match s.direction with
        | `Input -> "in" | `Output -> "out" | `Internal -> "int" in
      let w = match s.stype with
        | BInt { width; _ } -> width
        | BArray { size; element = BInt { width; _ }; _ } -> size * width
        | _ -> 0
      in
      Printf.sprintf "%-3s %-30s w=%d" dir s.name w
    ) m.signals
    |> List.sort compare
  in
  let dump_pair name (viv_m : bmodule) (vrb_m : bmodule option) =
    Printf.printf "── %s ──\n" name;
    let viv_lines = signal_summary viv_m in
    let vrb_lines = match vrb_m with
      | Some m -> signal_summary m | None -> [] in
    let viv_set = List.fold_left (fun acc l ->
      acc |> List.filter (fun x -> x <> l) |> (fun a -> l :: a)) [] viv_lines in
    let vrb_set = List.fold_left (fun acc l ->
      acc |> List.filter (fun x -> x <> l) |> (fun a -> l :: a)) [] vrb_lines in
    let only_viv = List.filter (fun l -> not (List.mem l vrb_set)) viv_set in
    let only_vrb = List.filter (fun l -> not (List.mem l viv_set)) vrb_set in
    if only_viv = [] && only_vrb = [] then
      Printf.printf "  (signal lists match)\n"
    else begin
      List.iter (fun l ->
        Printf.printf "  only-viv: %s\n" l) only_viv;
      List.iter (fun l ->
        Printf.printf "  only-vrb: %s\n" l) only_vrb
    end;
    (match vrb_m with
     | None -> Printf.printf "  (no Verible counterpart paired)\n"
     | Some _ -> ());
    Printf.printf "\n"
  in
  (match dump_filter with
   | None -> ()
   | Some substr ->
       List.iter (fun (name, viv_m) ->
         if contains substr name then
           dump_pair name viv_m (lookup vrb_idx viv_m)
       ) viv_idx);

  let rows = ref [] in
  List.iter (fun (name, viv_m) ->
    if matches name then begin
      let vff = ff_q_set viv_m in
      let vrb_ff = match lookup vrb_idx viv_m with
        | Some m -> Some (ff_q_set m) | None -> None in
      let vlt_ff = match lookup vlt_idx viv_m with
        | Some m -> Some (ff_q_set m) | None -> None in
      rows := (name, vff, vlt_ff, vrb_ff) :: !rows
    end
  ) viv_idx;

  let scored = List.map (fun (name, vff, vlt_ff, vrb_ff) ->
    let s_vlt = match vlt_ff with
      | Some s -> jaccard vff s | None -> -1.0 in
    let s_vrb = match vrb_ff with
      | Some s -> jaccard vff s | None -> -2.0 in
    (name, vff, vlt_ff, vrb_ff, s_vlt, s_vrb)
  ) !rows in

  (* Sort: best Verible match first, then by Vivado FF count (smaller
   * entities are easier to triage). Failures (no Verible module) sink
   * to the bottom. *)
  let scored = List.sort (fun (_,_,_,_,_,a) (_,_,_,_,_,b) ->
    compare b a) scored in

  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  per-entity FF-set diff: vivado vs verible%s\n"
    (if vlt_idx <> [] then " (verilator shown for reference)" else "");
  Printf.printf "═══════════════════════════════════════════════════════\n\n";
  Printf.printf "%-40s %4s %4s %4s  %4s  %s\n"
    "entity" "viv" "vlt" "vrb" "J%" "verdict";
  Printf.printf "%-40s %4s %4s %4s  %4s  %s\n"
    (String.make 40 '-') "----" "----" "----" "----" "-------";

  let fully_match = ref 0 in
  let no_verible = ref 0 in
  let mismatch = ref 0 in

  List.iter (fun (name, vff, vlt_ff, vrb_ff, _, s_vrb) ->
    let nv = List.length vff in
    let nlt = match vlt_ff with Some s -> List.length s | None -> -1 in
    let nrb = match vrb_ff with Some s -> List.length s | None -> -1 in
    let pct, verdict =
      match vrb_ff with
      | None -> "-", "no verible"
      | Some s when s = vff -> "100", "✓ match"
      | Some _ ->
          incr mismatch;
          let p = int_of_float (s_vrb *. 100.0) in
          (string_of_int p,
           let only_v = List.filter (fun x -> not (List.mem x (Option.get vrb_ff))) vff in
           let only_b = List.filter (fun x -> not (List.mem x vff)) (Option.get vrb_ff) in
           Printf.sprintf "only-viv=[%s] only-vrb=[%s]"
             (truncate (String.concat "," only_v) 40)
             (truncate (String.concat "," only_b) 40))
    in
    (match vrb_ff with
     | None -> incr no_verible
     | Some s when s = vff -> incr fully_match
     | _ -> ());
    Printf.printf "%-40s %4d %4d %4d  %4s  %s\n"
      (truncate name 40) nv nlt nrb pct verdict
  ) scored;

  let total = List.length scored in
  Printf.printf "\n═══════════════════════════════════════════════════════\n";
  Printf.printf "  Summary: %d entities, %d ✓ FF-match, %d mismatch, %d no-verible\n"
    total !fully_match !mismatch !no_verible;
  Printf.printf "═══════════════════════════════════════════════════════\n";

  (* Optional Z3 miter pass: only on entities whose Verible FF set
   * already matches Vivado's. Without that match, the miter has zero
   * chance of aligning the state. With Z3 enabled, the comparator
   * upgrades each FF-matching pair to a real combinational
   * equivalence check (after both sides have been ffrip'd). *)
  if Sys.getenv_opt "MITER_Z3" <> None then begin
    Printf.printf "\n═══════════════════════════════════════════════════════\n";
    Printf.printf "  Z3 miter on FF-matching pairs\n";
    Printf.printf "═══════════════════════════════════════════════════════\n";
    let saved = Unix.dup Unix.stdout in
    let z3_pass = ref 0 and z3_fail = ref 0 and z3_err = ref 0 in
    List.iter (fun (name, vff, _, vrb_ff, _, _) ->
      match vrb_ff with
      | Some s when s = vff ->
          let viv_m = List.assoc name viv_idx in
          (match lookup vrb_idx viv_m with
           | None -> ()
           | Some vrb_m ->
               Printf.printf "  %-40s ... %!" (truncate name 40);
               let devnull = Unix.openfile "/dev/null" [Unix.O_WRONLY] 0 in
               Unix.dup2 devnull Unix.stdout;
               Unix.close devnull;
               let result =
                 try if Z3_miter.check_miter_equivalence viv_m vrb_m
                     then `Pass else `Fail
                 with e -> `Error (Printexc.to_string e)
               in
               Unix.dup2 saved Unix.stdout;
               (match result with
                | `Pass -> incr z3_pass; Printf.printf "✅ Z3 PASS\n%!"
                | `Fail -> incr z3_fail; Printf.printf "❌ Z3 FAIL\n%!"
                | `Error e ->
                    incr z3_err;
                    Printf.printf "⚠ Z3 error: %s\n%!"
                      (truncate e 60)))
      | _ -> ()
    ) scored;
    Unix.close saved;
    Printf.printf "\n  Z3 totals: %d ✅  %d ❌  %d ⚠\n" !z3_pass !z3_fail !z3_err
  end
