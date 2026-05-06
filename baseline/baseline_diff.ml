(* Compare two baseline.json files and print a delta table.

   Usage: baseline_diff <baseline_a.json> <baseline_b.json>

   Prints one row per metric:
       metric              A         B         delta     %     status
       wns_setup_ns       -0.034    -0.012    +0.022   +65%   IMPROVED
       core_area_um2       890       925      +35     +3.9%    worse
       n_cells             970       1030     +60     +6.2%    worse
       wirelength_um      4358      4520      +162    +3.7%    worse
       power_total_w     0.0035    0.0038   +0.0003   +9.7%    worse
       fmax_hz       2.02e+09   2.05e+09  +30M       +1.5%    IMPROVED

   "status" is decided per-metric by which direction means
   "better".  Lower is better for area, power, wirelength,
   counts, slack-violation magnitudes.  Higher is better for
   slack (less negative), fmax, ws_hold (less negative).

   Used to compare:
     - stock baseline vs Tier-2.5 baseline (#103)
     - any two stock baselines (when ORFS or yosys updates) *)

let pct a b =
  if a = 0.0 then 0.0
  else 100.0 *. (b -. a) /. (abs_float a)

(* For each metric, "+1" means higher is better, "-1" means
   lower is better.  Skip listed -> not interpreted. *)
let direction = function
  | "wns_setup_ns" | "ws_hold_ns" | "tns_setup_ns"
  | "fmax_hz" | "utilization_pct" -> Some 1
  | "core_area_um2" | "die_area_um2" | "stdcell_area_um2"
  | "n_cells" | "n_seq_cells" | "n_inverter" | "n_buffer"
  | "n_clock_buffer" | "n_combinational"
  | "wirelength_um"
  | "power_total_w" | "power_internal_w" | "power_switching_w" | "power_leakage_w"
  | "drv_setup_violations" | "drv_hold_violations"
  | "flow_errors" | "flow_warnings" -> Some (-1)
  | _ -> None

let to_float = function
  | `Float f -> Some f
  | `Int n -> Some (float_of_int n)
  | _ -> None

let read_metrics path =
  let json = Yojson.Safe.from_file path in
  match Yojson.Safe.Util.member "metrics" json with
  | `Assoc kvs -> kvs
  | _ -> []

let format_value = function
  | `Float f when abs_float f >= 1e6 -> Printf.sprintf "%.2e" f
  | `Float f when abs_float f < 0.001 && f <> 0.0 -> Printf.sprintf "%.3e" f
  | `Float f -> Printf.sprintf "%.4g" f
  | `Int n -> string_of_int n
  | `Null -> "n/a"
  | _ -> "?"

let status delta dir =
  if delta = 0.0 then ""
  else match dir with
    | Some 1  -> if delta > 0.0 then "BETTER" else "worse"
    | Some -1 -> if delta < 0.0 then "BETTER" else "worse"
    | _ -> ""

let () =
  if Array.length Sys.argv < 3 then begin
    prerr_endline "usage: baseline_diff <a.json> <b.json>";
    exit 2
  end;
  let a_path = Sys.argv.(1) and b_path = Sys.argv.(2) in
  let a = read_metrics a_path and b = read_metrics b_path in

  Printf.printf "Compared baselines:\n";
  Printf.printf "  A = %s\n" a_path;
  Printf.printf "  B = %s\n\n" b_path;
  Printf.printf "  %-22s %12s %12s %12s %8s   %s\n"
    "metric" "A" "B" "delta" "%" "status";
  Printf.printf "  %s\n" (String.make 78 '-');

  List.iter (fun (k, va) ->
    let vb = try List.assoc k b with Not_found -> `Null in
    let af = to_float va and bf = to_float vb in
    let delta_str, pct_str, st =
      match af, bf with
      | Some x, Some y ->
          let d = y -. x in
          let p = pct x y in
          (Printf.sprintf "%+.4g" d,
           Printf.sprintf "%+.1f%%" p,
           status d (direction k))
      | _ -> ("", "", "") in
    Printf.printf "  %-22s %12s %12s %12s %8s   %s\n"
      k (format_value va) (format_value vb)
      delta_str pct_str st) a
