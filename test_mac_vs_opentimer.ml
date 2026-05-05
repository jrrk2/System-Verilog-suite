(* Cross-validate our placement_timing arrival against OpenTimer.

   Input:
     <library.lib>            — Liberty for both tools
     <tech.lef>               — for our pin-direction lookup
     <ot-shell binary>        — path to OpenTimer's ot-shell
     [width=4]                — MAC operand width (kept small for
                                speed and to stay inside the
                                constructs Liberty-readers tolerate)

   For each (mul_arch, add_arch) we:
     - build the synthetic AND2_X1-only netlist
     - dump it to design.v + design.sdc + design.conf in a temp dir
     - run ot-shell < design.conf, parse the worst-path arrival
     - run our Placement_timing.report on the same in-memory netlist
     - tabulate both numbers + ratio
*)

open Lef_def

(* ---- run a subprocess, capture stdout ---------------------- *)
let read_command_output cmd =
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try while true do
     Buffer.add_channel buf ic 4096
   done with End_of_file -> ());
  let _ = Unix.close_process_in ic in
  Buffer.contents buf

(* OpenTimer's report_timing prints "arrival   <number>" near the
   end of each path block.  Take the maximum across all paths.
   The number is in the library's time-unit (1ns for NanGate45),
   so we convert ns → ps for direct comparison. *)
let parse_ot_arrival raw =
  let max_arr = ref neg_infinity in
  let lines = String.split_on_char '\n' raw in
  List.iter (fun line ->
    let line = String.trim line in
    let prefix = "arrival" in
    if String.length line > String.length prefix
       && String.sub line 0 (String.length prefix) = prefix then begin
      let toks =
        String.split_on_char ' ' line
        |> List.filter (fun s -> s <> "") in
      List.iter (fun s ->
        match float_of_string_opt s with
        | Some v -> if v > !max_arr then max_arr := v
        | None -> ()) toks
    end) lines;
  if !max_arr = neg_infinity then None
  else Some (!max_arr *. 1000.0)  (* ns → ps *)

let with_temp_dir f =
  let dir = Filename.temp_file "mac_vs_ot_" "" in
  Sys.remove dir;
  Unix.mkdir dir 0o755;
  let r = try f dir with e ->
    Printf.eprintf "(retaining temp dir %s for inspection)\n" dir;
    raise e in
  (* Keep the dir around so the user can poke at the netlists *)
  Printf.printf "(temp dir: %s)\n" dir;
  r

let run_one ~ot_shell ~lib_path ~temp_dir ~tag nl =
  let v_path   = Filename.concat temp_dir (tag ^ ".v") in
  let sdc_path = Filename.concat temp_dir (tag ^ ".sdc") in
  let conf_path= Filename.concat temp_dir (tag ^ ".conf") in
  let log_path = Filename.concat temp_dir (tag ^ ".log") in

  let oc = open_out v_path in
  Synth_mac.emit_verilog ~module_name:"mac" ~oc nl;
  close_out oc;
  let oc = open_out sdc_path in
  Synth_mac.emit_sdc ~oc ~clock_period:10000.0 nl;
  close_out oc;
  let oc = open_out conf_path in
  Synth_mac.emit_conf ~oc ~lib_path:(Filename.basename lib_path)
    ~v_path:(Filename.basename v_path)
    ~sdc_path:(Filename.basename sdc_path);
  close_out oc;

  (* Run ot-shell in the temp dir so its read_celllib can pick up
     the lib by basename — copy or symlink the lib in. *)
  let lib_link = Filename.concat temp_dir (Filename.basename lib_path) in
  if not (Sys.file_exists lib_link) then
    Unix.symlink (Filename.concat (Sys.getcwd ()) lib_path) lib_link;

  let cmd = Printf.sprintf "cd %s && %s < %s > %s 2>&1"
              (Filename.quote temp_dir)
              (Filename.quote ot_shell)
              (Filename.quote (Filename.basename conf_path))
              (Filename.quote (Filename.basename log_path)) in
  let _ = Sys.command cmd in
  let raw = read_command_output ("cat " ^ Filename.quote log_path) in
  parse_ot_arrival raw

let () =
  if Array.length Sys.argv < 4 then begin
    prerr_endline
      "usage: test_mac_vs_opentimer <library.lib> <tech.lef> \
       <ot-shell> [width=4]";
    exit 2
  end;
  let lib_path = Sys.argv.(1) in
  let lef_path = Sys.argv.(2) in
  let ot_shell = Sys.argv.(3) in
  let width    = if Array.length Sys.argv > 4
                 then int_of_string Sys.argv.(4) else 4 in

  Printf.printf "Loading Liberty %s ...\n%!" lib_path;
  let delay_tbl = Cell_delay.load lib_path in
  Printf.printf "  cells with delay arcs : %d\n" (Hashtbl.length delay_tbl);

  let lef_entries = Lef_pins.parse lef_path in
  let pin_dir = Lef_pins.table_of_entries lef_entries in
  Printf.printf "  LEF pin entries       : %d\n%!"
    (List.length lef_entries);

  with_temp_dir (fun temp_dir ->
    let muls = [ Synth_mac.Array_m; Synth_mac.Wallace_m ] in
    let adds = [ Synth_mac.Ripple_a; Synth_mac.Kogge_stone_a ] in
    let multi_cell = Sys.getenv_opt "MULTI_CELL" <> None in

    Printf.printf "\nMAC width = %d  (cells: %s)\n\n"
      width (if multi_cell then "AND2_X1 + XOR2_X1" else "AND2_X1 only");
    Printf.printf "  %-9s %-12s | %5s %5s | %12s | %12s | %6s\n"
      "mul" "adder" "depth" "cells" "ours (ps)" "OT (ps)" "ratio";
    Printf.printf "  %s\n" (String.make 76 '-');

    List.iter (fun ma ->
      List.iter (fun aa ->
        let nl = Synth_mac.build ~multi_cell ~width
                   ~mul_arch:ma ~add_arch:aa () in
        let r_ours = Placement_timing.report
                       ~delay_of:(Cell_delay.lookup delay_tbl)
                       ~pin_dir nl.cells nl.nets in
        let tag = Printf.sprintf "%s_%s_w%d"
                    (Synth_mac.mul_arch_name ma)
                    (Synth_mac.adder_arch_name aa) width in
        let ot_arr = run_one ~ot_shell ~lib_path ~temp_dir ~tag nl in
        let ratio = match ot_arr with
          | Some o when o > 0.0 -> r_ours.worst_arr_ps /. o
          | _ -> nan in
        Printf.printf
          "  %-9s %-12s | %5d %5d | %12.2f | %12s | %5.2fx\n"
          (Synth_mac.mul_arch_name ma)
          (Synth_mac.adder_arch_name aa)
          nl.depth (List.length nl.cells)
          r_ours.worst_arr_ps
          (match ot_arr with
           | Some v -> Printf.sprintf "%.2f" v
           | None   -> "n/a")
          ratio
      ) adds
    ) muls)
