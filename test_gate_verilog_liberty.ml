(* End-to-end: emit Synth_mac as Verilog, parse it back, and
   build the net structure using pin directions pulled live from
   the NanGate45 Liberty (no hardcoded "magic names" anywhere). *)

let () =
  let lib_path = "lef_def/test/nangate.lib" in

  (* 1. Pin-direction table: built from Liberty by Cell_delay. *)
  let pair = Cell_delay.load_arc_table lib_path in
  let tbl, _ = pair in
  let pin_dir = Cell_delay.pin_dir_table tbl in
  Printf.printf "Liberty pin-directions: %d (cell,pin) entries\n%!"
    (Hashtbl.length pin_dir);

  (* 2. Round-trip a synth netlist through the Verilog parser. *)
  let nl = Lef_def.Synth_mac.build ~width:4
             ~mul_arch:Lef_def.Synth_mac.Wallace_m
             ~add_arch:Lef_def.Synth_mac.Kogge_stone_a () in
  let path = Filename.temp_file "gate_v_" ".v" in
  let oc = open_out path in
  Lef_def.Synth_mac.emit_verilog ~module_name:"mac" ~oc nl;
  close_out oc;
  let modules = Lef_def.Gate_verilog.parse_file path in
  match modules with
  | [] -> print_endline "FAIL no modules"; exit 1
  | m :: _ ->
      Printf.printf "Parsed: %d cells, %d ports\n"
        (List.length m.cells) (List.length m.ports);

      (* 3. Build nets using the Liberty pin-direction table. *)
      let net_pins = Lef_def.Gate_verilog.build_nets ~pin_dir m in
      let n_with_driver = ref 0 in
      let n_orphan_driver = ref 0 in
      Hashtbl.iter (fun _ (drv, _) ->
        match drv with
        | [] -> ()
        | [_] -> incr n_with_driver
        | _ -> incr n_with_driver; incr n_orphan_driver) net_pins;
      Printf.printf "Driven nets: %d  (multi-driver: %d)\n"
        !n_with_driver !n_orphan_driver;
      if !n_with_driver = List.length m.cells
      then print_endline "OK   one driver per cell as expected"
      else Printf.printf "WARN expected %d driven nets, got %d\n"
             (List.length m.cells) !n_with_driver
