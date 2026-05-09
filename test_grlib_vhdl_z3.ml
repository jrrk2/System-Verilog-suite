(* GRLIB bottom-up Z3 sweep: for each leaf entity, compare our
   VHDL→BIR against ghdl-synth→Verilog→verilator→BIR via the Z3
   miter.

   Usage:
     test_grlib_vhdl_z3 <list.tsv>
   where each line is `<top>\t<vhd_path>` (the ghdl side runs
   inline; no pre-built Vivado output needed).

   Reports per-leaf SAT/UNSAT/error and a final tally. *)

open Behavioral_ir

let find_module name (prog : bprogram) =
  List.find_opt (fun (m : bmodule) -> m.name = name) prog.modules

let lower_or_pass s = String.lowercase_ascii s

(* Oracle: parse a Verilog netlist via the requested tool. *)
type oracle_kind = OracVerilator | OracSlang

let oracle_name = function
  | OracVerilator -> "verilator"
  | OracSlang -> "slang"

let oracle_parse ~mdir top kind ghdl_v =
  match kind with
  | OracVerilator ->
      let v_log = Filename.concat mdir "verilator.log" in
      let cmd =
        Printf.sprintf
          "verilator --json-only -Wno-fatal -Wno-WIDTHTRUNC -Wno-WIDTHEXPAND \
           -Wno-UNOPTFLAT -Wno-CASEINCOMPLETE -Wno-CASEX -Wno-PINMISSING \
           -Wno-IMPLICIT -Wno-DECLFILENAME -Wno-MULTITOP \
           --top-module %s %s --Mdir %s > %s 2>&1"
          (Filename.quote top) (Filename.quote ghdl_v)
          (Filename.quote mdir) (Filename.quote v_log)
      in
      let rc = Sys.command cmd in
      let json = Filename.concat mdir (Printf.sprintf "V%s.tree.json" top) in
      if rc <> 0 || not (Sys.file_exists json) then None
      else (try Verilator_to_behavioral.convert_verilator_json_to_behavioral json
            with _ -> None)
  | OracSlang ->
      try Slang_to_behavioral.convert_files ~top [ghdl_v]
      with _ -> None

let compare_one (top, vhd) =
  Printf.printf "\n──── %s ────\n%!" top;
  Printf.printf "  vhd: %s\n%!" vhd;
  let ours = Vhdl_to_behavioral.convert_vhdl_file_to_behavioral vhd in
  match ours with
  | None -> Printf.printf "  result: VHDL→BIR convert failed\n%!"; `ConvertFail
  | Some our_prog ->
      let mdir = Filename.concat "/tmp"
                   (Printf.sprintf "grlib_z3_%s_%d" top (Unix.getpid ())) in
      let _ = Sys.command (Printf.sprintf "rm -rf %s && mkdir -p %s"
                             (Filename.quote mdir) (Filename.quote mdir)) in
      let ghdl_v = Filename.concat mdir "ghdl_synth.v" in
      let ghdl_log = Filename.concat mdir "ghdl.log" in
      (* Pre-built grlib library at ~/ghdl_work/grlib (see
         /tmp/build_grlib_libs.sh).  GHDL_LIB_PATH env var lets the
         caller override or supply additional library dirs as a
         space-separated list.  Each appears with -P. *)
      let lib_paths =
        let home = Sys.getenv "HOME" in
        let default_paths = [
          Filename.concat home "ghdl_work/grlib";
          Filename.concat home "ghdl_work/techmap";
          Filename.concat home "ghdl_work/gaisler";
        ] in
        match Sys.getenv_opt "GHDL_LIB_PATH" with
        | None -> default_paths
        | Some s -> default_paths @
                    List.filter (fun x -> x <> "") (String.split_on_char ' ' s)
      in
      let p_flags = String.concat " "
        (List.filter_map (fun p ->
           if Sys.file_exists p then Some ("-P" ^ Filename.quote p) else None)
         lib_paths) in
      let ghdl_cmd =
        Printf.sprintf "cd %s && ghdl --synth --std=08 %s --out=verilog %s -e %s > %s 2> %s"
          (Filename.quote mdir) p_flags
          (Filename.quote vhd) (Filename.quote top)
          (Filename.quote ghdl_v) (Filename.quote ghdl_log)
      in
      let g_rc = Sys.command ghdl_cmd in
      if g_rc <> 0 then begin
        Printf.printf "  ghdl synth failed (rc=%d), see %s\n%!" g_rc ghdl_log;
        `GhdlFail
      end else begin
      Printf.printf "  ghdl synth → %s\n%!" ghdl_v;
      (* Rename ghdl's anonymous `nXX_q` register names to their source
         signals (per `// (signal)` annotations or aliasing assigns to
         output ports), so the Z3 miter's FF set lines up with what
         our converter produces. *)
      let renamed_v = Filename.concat mdir "ghdl_renamed.v" in
      let _ = Sys.command (Printf.sprintf "/tmp/ghdl_rename_qs.sh %s %s"
                             (Filename.quote ghdl_v) (Filename.quote renamed_v)) in
      let ghdl_v = if Sys.file_exists renamed_v then renamed_v else ghdl_v in
      let oracles = [OracVerilator; OracSlang] in
      let results = List.map (fun kind ->
        let theirs = oracle_parse ~mdir top kind ghdl_v in
        let label = oracle_name kind in
        match theirs with
        | None -> Printf.printf "  [%s] parse failed\n%!" label; (label, `OracleFail)
        | Some their_prog ->
            let pick_top name prog =
              List.find_opt (fun (m : bmodule) ->
                lower_or_pass m.name = lower_or_pass name) prog.modules
            in
            (match pick_top top our_prog, pick_top top their_prog with
             | None, _ -> Printf.printf "  [%s] top %s missing on our side\n%!" label top;
                          (label, `OurMissing)
             | _, None -> Printf.printf "  [%s] top %s missing on oracle side\n%!" label top;
                          (label, `TheirMissing)
             | Some ours_m, Some theirs_m ->
                 Printf.printf "  [%s] ours: %d sigs, %d procs | oracle: %d sigs, %d procs\n%!"
                   label
                   (List.length ours_m.signals) (List.length ours_m.processes)
                   (List.length theirs_m.signals) (List.length theirs_m.processes);
                 (try
                    Z3_miter.clear_miter_caches ();
                    let ok = Z3_miter.check_miter_equivalence ours_m theirs_m in
                    if ok then begin
                      Printf.printf "  [%s] ✅ EQUIVALENT\n%!" label;
                      (label, `Equiv)
                    end else begin
                      Printf.printf "  [%s] ❌ NOT equivalent\n%!" label;
                      (label, `NotEquiv)
                    end
                  with e ->
                    Printf.printf "  [%s] z3 raised: %s\n%!" label (Printexc.to_string e);
                    (label, `Z3Error)))
      ) oracles in
      `Multi results
      end

let () =
  if Array.length Sys.argv < 2 then begin
    prerr_endline "usage: test_grlib_vhdl_z3 <list.tsv>";
    exit 1
  end;
  let listf = Sys.argv.(1) in
  let pairs =
    let ic = open_in listf in
    let acc = ref [] in
    (try
       while true do
         let line = input_line ic in
         match String.split_on_char '\t' line with
         | top :: vhd :: _ -> acc := (top, vhd) :: !acc
         | _ -> ()
       done
     with End_of_file -> close_in ic);
    List.rev !acc
  in
  Printf.printf "GRLIB bottom-up Z3 sweep: %d entries\n%!"
    (List.length pairs);
  let by_oracle : (string, (string, int) Hashtbl.t) Hashtbl.t =
    Hashtbl.create 4 in
  let bump_oracle oracle k =
    let h =
      try Hashtbl.find by_oracle oracle
      with Not_found ->
        let h = Hashtbl.create 8 in
        Hashtbl.add by_oracle oracle h; h
    in
    let n = try Hashtbl.find h k with Not_found -> 0 in
    Hashtbl.replace h k (n + 1)
  in
  let pre_tally = Hashtbl.create 4 in
  let bump_pre k =
    let n = try Hashtbl.find pre_tally k with Not_found -> 0 in
    Hashtbl.replace pre_tally k (n + 1)
  in
  let result_label = function
    | `Equiv -> "Equiv" | `NotEquiv -> "NotEquiv"
    | `Z3Error -> "Z3Error" | `OurMissing -> "OurMissing"
    | `TheirMissing -> "TheirMissing" | `OracleFail -> "OracleFail"
  in
  List.iter (fun t ->
    match compare_one t with
    | `ConvertFail -> bump_pre "ConvertFail"
    | `GhdlFail -> bump_pre "GhdlFail"
    | `Multi rs ->
        List.iter (fun (oracle, r) ->
          bump_oracle oracle (result_label r)) rs
  ) pairs;
  Printf.printf "\n══════ SUMMARY ══════\n";
  Printf.printf "Pre-oracle:\n";
  Hashtbl.iter (fun k n -> Printf.printf "  %-15s %d\n" k n) pre_tally;
  Hashtbl.iter (fun oracle h ->
    Printf.printf "Oracle [%s]:\n" oracle;
    Hashtbl.iter (fun k n -> Printf.printf "  %-15s %d\n" k n) h
  ) by_oracle
