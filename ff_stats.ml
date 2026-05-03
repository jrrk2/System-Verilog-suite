(* FF / latch matching statistics across frontend flows.
 *
 * For a given testcase, build the BIR via every available frontend:
 *
 *   - Verilator JSON                → Verilator_to_behavioral
 *   - Verible parse-tree            → Verible_to_behavioral
 *   - Yosys RTLIL                   → Rtlil_to_behavioral
 *   - Vivado-elaborated VHDL        → Vhdl_to_ver_front
 *
 * Then run Behavioral_ffrip on each and report the set of Q__Q (FF
 * current-state) and Q__D (next-state) signals each flow emits. The
 * miter relies on these names matching across flows — this tool
 * surfaces the mismatch directly.
 *
 * Usage:
 *   ff_stats <top> <vivado.vhd?> <file.sv> [more.sv ...]
 *
 * Pass `-` for the VHDL slot to skip the Vivado side. *)

open Behavioral_ir

let usage () =
  Printf.eprintf
    "usage: %s <top> <vivado.vhd|-> <file.sv> [more.sv ...]\n" Sys.argv.(0);
  exit 2

(* ─── Helpers shared across the four frontends ──────────────────── *)

let run_verilator ~top ~files =
  let mdir = Filename.concat (Filename.get_temp_dir_name ())
               (Printf.sprintf "ff_stats_vlt_%s_%d" top (Unix.getpid ())) in
  let _ = Sys.command (Printf.sprintf "rm -rf %s" mdir) in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote mdir)) in
  let files_str = String.concat " " (List.map Filename.quote files) in
  let cmd =
    Printf.sprintf
      "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
       -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING \
       -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
       --top-module %s %s --Mdir %s > /dev/null 2>&1"
      (Filename.quote top) files_str (Filename.quote mdir)
  in
  if Sys.command cmd <> 0 then None
  else Some (Filename.concat mdir (Printf.sprintf "V%s.tree.json" top))

let find_yosys () =
  let candidates = [
    "/home/local/f4pga/xc7/conda/envs/xc7/bin/yosys";
    "/usr/local/bin/yosys"; "/usr/bin/yosys"; "yosys";
  ] in
  List.find_opt (fun p ->
    if String.length p > 0 && p.[0] = '/' then Sys.file_exists p
    else (Sys.command (Printf.sprintf "command -v %s > /dev/null" p) = 0)
  ) candidates

let run_yosys ~top ~files =
  match find_yosys () with
  | None -> None
  | Some yosys ->
      let out = Filename.temp_file "ff_stats_yos_" ".il" in
      let script = Filename.temp_file "ff_stats_ys_" ".ys" in
      let oc = open_out script in
      Printf.fprintf oc "read_verilog -sv %s\n" (String.concat " " files);
      Printf.fprintf oc "hierarchy -top %s\n" top;
      Printf.fprintf oc "proc\nopt -fast\nflatten\nopt -fast\n";
      Printf.fprintf oc "write_rtlil %s\n" out;
      close_out oc;
      let cmd = Printf.sprintf "%s -q -s %s > /dev/null 2>&1"
                  (Filename.quote yosys) (Filename.quote script) in
      let rc = Sys.command cmd in
      (try Sys.remove script with _ -> ());
      if rc <> 0 then None else Some out

let pick_top top src =
  List.find_opt (fun (m : bmodule) -> m.name = top) src

(* Run ffrip and collect (Q__Q-style inputs, Q__D-style outputs). The
 * ffrip pass renames internal FF state Q to a primary input named
 * either `Q` (for originally-internal Q's) or `Q__Q` (for FFs whose
 * Q was an output port and got pass-thru'd). The corresponding D-pin
 * always lands as `Q__D`. *)
let ff_signals (m : bmodule) =
  let m = Behavioral_ffrip.rip_module m in
  let q_q = ref [] and q_d = ref [] in
  List.iter (fun (s : bsignal) ->
    let n = s.name in
    let len = String.length n in
    let suffix sfx =
      let l = String.length sfx in
      len > l && String.sub n (len - l) l = sfx
    in
    if s.direction = `Output && suffix "__D" then
      q_d := String.sub n 0 (len - 3) :: !q_d
    else if s.direction = `Input && suffix "__Q" then
      q_q := String.sub n 0 (len - 3) :: !q_q
  ) m.signals;
  (List.sort compare !q_q, List.sort compare !q_d)

(* ─── Per-flow conversion ──────────────────────────────────────── *)

let from_verilator ~top ~files =
  match run_verilator ~top ~files with
  | None -> None
  | Some json ->
      match Verilator_to_behavioral.convert_verilator_json_to_behavioral json
      with
      | Some p -> pick_top top p.modules
      | None -> None

let from_verible ~top ~files =
  let p = Verible_to_behavioral.convert_files ~top files in
  pick_top top p.modules

let from_yosys ~top ~files =
  match run_yosys ~top ~files with
  | None -> None
  | Some il ->
      let p = Rtlil_to_behavioral.convert_file il in
      (try Sys.remove il with _ -> ());
      pick_top top p.modules

let from_vivado ~top ~vhd =
  if vhd = "-" || not (Sys.file_exists vhd) then None
  else match Vhdl_to_ver_front.convert_vhd_file vhd with
    | None -> None
    | Some p -> pick_top top p.modules

(* ─── Reporting ────────────────────────────────────────────────── *)

let label_of n = function
  | None -> Printf.sprintf "%-12s  (no module)" n
  | Some _ -> n

let report flow ?vhd top files =
  let module M = struct
    let go () =
      let vlt = from_verilator ~top ~files in
      let vrb = Some (from_verible ~top ~files) in
      let yos = from_yosys ~top ~files in
      let viv = match vhd with
        | Some f -> from_vivado ~top ~vhd:f
        | None -> None
      in
      let truncate s w =
        if String.length s <= w then s
        else String.sub s 0 (max 0 (w - 1)) ^ "…"
      in
      let pairs = [
        "verilator", vlt;
        "verible",   (match vrb with Some (Some _ as m) -> m | _ -> None);
        "yosys",     yos;
        "vivado",    viv;
      ] in
      let ff_per_flow =
        List.map (fun (lbl, m_opt) ->
          (lbl, Option.map ff_signals m_opt)) pairs
      in
      Printf.printf "─── %s ───\n" flow;
      List.iter (fun (lbl, ff_opt) ->
        match ff_opt with
        | None -> Printf.printf "  %-10s  ⊘ (frontend failed)\n" lbl
        | Some (qs, ds) ->
            Printf.printf "  %-10s  Q__Q=%-3d Q__D=%-3d  Q={%s}  D={%s}\n"
              lbl (List.length qs) (List.length ds)
              (truncate (String.concat ", " qs) 50)
              (truncate (String.concat ", " ds) 80)
      ) ff_per_flow;
      (* Summary: how many FF names overlap pairwise. *)
      let qsets = List.filter_map (fun (lbl, ff_opt) ->
        match ff_opt with Some (qs, _) -> Some (lbl, qs) | None -> None
      ) ff_per_flow in
      let union =
        List.fold_left (fun acc (_, qs) ->
          List.fold_left (fun a q -> if List.mem q a then a else q :: a) acc qs)
          [] qsets
      in
      let intersection = match qsets with
        | [] -> []
        | (_, first) :: rest ->
            List.fold_left (fun acc (_, qs) ->
              List.filter (fun q -> List.mem q qs) acc) first rest
      in
      Printf.printf "  ── union(Q)=%d  intersection(Q)=%d\n"
        (List.length union) (List.length intersection);
      (* Pairwise overlap: how many Q__Q names match between every
       * pair of flows that produced output. The miter only matches
       * when *all* names align, so any < total here pinpoints the
       * pair where collapse breaks. *)
      let dsets = List.filter_map (fun (lbl, ff_opt) ->
        match ff_opt with Some (_, ds) -> Some (lbl, ds) | None -> None
      ) ff_per_flow in
      let combined = List.map (fun (lbl, qs) ->
        match List.assoc_opt lbl dsets with
        | Some ds ->
            let merged = List.sort_uniq compare (qs @ ds) in
            (lbl, merged)
        | None -> (lbl, qs)
      ) qsets in
      let rec pairs = function
        | [] | [_] -> []
        | a :: rest -> List.map (fun b -> (a, b)) rest @ pairs rest
      in
      List.iter (fun ((la, sa), (lb, sb)) ->
        let common = List.filter (fun n -> List.mem n sb) sa in
        let only_a = List.filter (fun n -> not (List.mem n sb)) sa in
        let only_b = List.filter (fun n -> not (List.mem n sa)) sb in
        if only_a <> [] || only_b <> [] then
          Printf.printf "  ⚠ %s≠%s: shared=%d  only-%s=[%s]  only-%s=[%s]\n"
            la lb (List.length common)
            la (truncate (String.concat ", " only_a) 40)
            lb (truncate (String.concat ", " only_b) 40)
        else
          Printf.printf "  ✓ %s=%s: shared=%d (all match)\n"
            la lb (List.length common)
      ) (pairs combined);
      Printf.printf "\n"
  end in M.go ()

let () =
  if Array.length Sys.argv < 4 then usage ();
  let top = Sys.argv.(1) in
  let vhd = Sys.argv.(2) in
  let files = Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in
  let vhd_opt = if vhd = "-" then None else Some vhd in
  report top ?vhd:vhd_opt top files
