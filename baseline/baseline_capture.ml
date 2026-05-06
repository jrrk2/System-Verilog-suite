(* Capture ORFS-flow metrics into a single compact baseline.json.

   ORFS dumps per-stage metrics under
       flow/logs/<platform>/<design>/base/<step>.json
   with keys like [finish__timing__setup__ws] and
   [detailedroute__route__wirelength].  This tool reads the
   relevant per-stage files plus the tool versions, and emits
   one compact baseline.json with provenance and the metrics
   we'll diff against in #103.

   Usage:
     baseline_capture --orfs <ORFS_HOME> --platform <p> --design <d>
                      --out baseline/results/<platform>/<design>/<date>.json

   The output schema is deliberately small: only the metrics
   that mean something at the QoR level (timing, area, power,
   wirelength).  ORFS dumps ~200 keys per stage; most are
   diagnostic noise. *)

let read_json path =
  try Some (Yojson.Safe.from_file path) with _ -> None

let key_opt key = function
  | None -> None
  | Some json ->
      try Some (Yojson.Safe.Util.member key json) with _ -> None

let to_float_opt = function
  | None | Some `Null -> None
  | Some (`Int n)     -> Some (float_of_int n)
  | Some (`Intlit s)  -> (try Some (float_of_string s) with _ -> None)
  | Some (`Float f)   -> Some f
  | Some (`String s)  -> (try Some (float_of_string s) with _ -> None)
  | Some _            -> None

let to_int_opt v =
  match to_float_opt v with
  | Some f -> Some (int_of_float f)
  | None -> None

(* Run a shell command, return its stdout's first line trimmed. *)
let cmd_output cmd =
  try
    let ic = Unix.open_process_in cmd in
    let line = try input_line ic with End_of_file -> "" in
    let _ = Unix.close_process_in ic in
    String.trim line
  with _ -> ""

(* Tool version detection.  ORFS is permissive about path layout
   so we try the standard install locations first. *)
let detect_tool_versions ~orfs_home =
  let or_bin = Filename.concat orfs_home "tools/install/OpenROAD/bin/openroad" in
  let yosys_bin = Filename.concat orfs_home "tools/install/yosys/bin/yosys" in
  let openroad_v = cmd_output (or_bin ^ " -version 2>/dev/null") in
  let yosys_v = cmd_output (yosys_bin ^ " -V 2>/dev/null") in
  let orfs_sha =
    cmd_output (Printf.sprintf "git -C %s rev-parse --short HEAD 2>/dev/null"
                  (Filename.quote orfs_home)) in
  `Assoc [
    "openroad",   `String openroad_v;
    "yosys",      `String yosys_v;
    "orfs_sha",   `String orfs_sha;
  ]

(* Pull the metrics we care about out of the per-stage JSONs. *)
let collect_metrics ~orfs_home ~platform ~design =
  let dir = Filename.concat orfs_home
              (Printf.sprintf "flow/logs/%s/%s/base" platform design) in
  let read step = read_json (Filename.concat dir (step ^ ".json")) in
  let finish = read "6_report" in
  let route  = read "5_2_route" in
  let synth  = read "1_synth" in
  let _ = synth in   (* synth metrics are mostly diagnostic; skip *)

  (* helper: pull [key] from a stage JSON, return JSON value *)
  let g j k = key_opt k j in
  let f j k = to_float_opt (g j k) in
  let i j k = to_int_opt (g j k) in

  (* All in seconds / Hz / square-microns / Watts as ORFS reports. *)
  `Assoc [
    "wns_setup_ns",         (match f finish "finish__timing__setup__ws" with
                              Some v -> `Float (v *. 1.0) | None -> `Null);
    "tns_setup_ns",         (match f finish "finish__timing__setup__tns" with
                              Some v -> `Float v | None -> `Null);
    "ws_hold_ns",           (match f finish "finish__timing__hold__ws" with
                              Some v -> `Float v | None -> `Null);
    "fmax_hz",              (match f finish "finish__timing__fmax" with
                              Some v -> `Float v | None -> `Null);
    "n_cells",              (match i finish "finish__design__instance__count" with
                              Some v -> `Int v | None -> `Null);
    "n_seq_cells",          (match i finish
                                "finish__design__instance__count__class:sequential_cell"
                              with Some v -> `Int v | None -> `Null);
    "n_inverter",           (match i finish
                                "finish__design__instance__count__class:inverter"
                              with Some v -> `Int v | None -> `Null);
    "n_buffer",             (match i finish
                                "finish__design__instance__count__class:timing_repair_buffer"
                              with Some v -> `Int v | None -> `Null);
    "n_clock_buffer",       (match i finish
                                "finish__design__instance__count__class:clock_buffer"
                              with Some v -> `Int v | None -> `Null);
    "n_combinational",      (match i finish
                                "finish__design__instance__count__class:multi_input_combinational_cell"
                              with Some v -> `Int v | None -> `Null);
    (* ORFS emits "finish__design__instance__area" TWICE in 6_report.json
       with two different values (core area vs std-cell sum).  We use the
       *suffixed* keys to disambiguate.  The bare unsuffixed key is unsafe. *)
    "stdcell_area_um2",     (match f finish "finish__design__instance__area__stdcell" with
                              Some v -> `Float v | None -> `Null);
    "core_area_um2",        (match f finish "finish__design__core__area" with
                              Some v -> `Float v | None -> `Null);
    "die_area_um2",         (match f finish "finish__design__die__area" with
                              Some v -> `Float v | None -> `Null);
    "utilization_pct",      (match f finish "finish__design__instance__utilization" with
                              Some v -> `Float (v *. 100.0) | None -> `Null);
    "wirelength_um",        (match f route  "detailedroute__route__wirelength" with
                              Some v -> `Float v | None -> `Null);
    "power_total_w",        (match f finish "finish__power__total" with
                              Some v -> `Float v | None -> `Null);
    "power_internal_w",     (match f finish "finish__power__internal__total" with
                              Some v -> `Float v | None -> `Null);
    "power_switching_w",    (match f finish "finish__power__switching__total" with
                              Some v -> `Float v | None -> `Null);
    "power_leakage_w",      (match f finish "finish__power__leakage__total" with
                              Some v -> `Float v | None -> `Null);
    "n_io",                 (match i finish "finish__design__io" with
                              Some v -> `Int v | None -> `Null);
    "n_nets",               (match i finish "finish__design__nets" with
                              Some v -> `Int v | None -> `Null);
    "drv_setup_violations", (match i finish "finish__timing__drv__setup_violation_count" with
                              Some v -> `Int v | None -> `Null);
    "drv_hold_violations",  (match i finish "finish__timing__drv__hold_violation_count" with
                              Some v -> `Int v | None -> `Null);
    "flow_errors",          (match i finish "finish__flow__errors__count" with
                              Some v -> `Int v | None -> `Null);
    "flow_warnings",        (match i finish "finish__flow__warnings__count" with
                              Some v -> `Int v | None -> `Null);
  ]

let now_iso () =
  let t = Unix.gmtime (Unix.time ()) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
    t.tm_hour t.tm_min t.tm_sec

let () =
  let orfs_home = ref "" in
  let platform  = ref "nangate45" in
  let design    = ref "" in
  let out_path  = ref "" in
  let flow      = ref "stock" in     (* "stock" or "tier25" — labels the row *)
  Arg.parse [
    "--orfs",     Arg.Set_string orfs_home, "ORFS root directory";
    "--platform", Arg.Set_string platform,  "Platform name (e.g. nangate45)";
    "--design",   Arg.Set_string design,    "Design name (e.g. gcd)";
    "--out",      Arg.Set_string out_path,  "Output JSON path";
    "--flow",     Arg.Set_string flow,      "Flow label: stock | tier25 (default stock)";
  ] (fun _ -> ()) "baseline_capture";
  if !orfs_home = "" || !design = "" || !out_path = "" then begin
    prerr_endline
      "usage: baseline_capture --orfs <ORFS_HOME> [--platform p] --design d --out path [--flow stock|tier25]";
    exit 2
  end;
  let baseline = `Assoc [
    "design",        `String !design;
    "platform",      `String !platform;
    "flow",          `String !flow;
    "timestamp",     `String (now_iso ());
    "tool_versions", detect_tool_versions ~orfs_home:!orfs_home;
    "metrics",       collect_metrics
                       ~orfs_home:!orfs_home ~platform:!platform ~design:!design;
  ] in
  (* mkdir -p the output directory *)
  let dir = Filename.dirname !out_path in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir)) in
  Yojson.Safe.to_file !out_path baseline;
  Printf.printf "Wrote %s\n" !out_path
