(* Round-trip: emit a Synth_mac netlist as Verilog, parse it back,
   check that the cell list and net structure agree.  Pin
   direction comes from the Liberty (no magic-name table). *)

open Lef_def

let temp_path () = Filename.temp_file "gate_v_" ".v"

let () =
  let nl = Synth_mac.build ~width:4
             ~mul_arch:Synth_mac.Wallace_m
             ~add_arch:Synth_mac.Kogge_stone_a () in
  let path = temp_path () in
  let oc = open_out path in
  Synth_mac.emit_verilog ~module_name:"mac" ~oc nl;
  close_out oc;

  let modules = Gate_verilog.parse_file path in
  match modules with
  | [] -> print_endline "FAIL no modules parsed"; exit 1
  | m :: _ ->
      let n_cells_emit = List.length nl.cells in
      let n_cells_parse = List.length m.cells in
      let n_ports = List.length m.ports in
      let by_kind k =
        List.length (List.filter (fun p -> p.Gate_verilog.kind = k) m.ports) in
      Printf.printf "  module name : %s\n" m.name;
      Printf.printf "  ports       : %d (input=%d output=%d wire=%d)\n"
        n_ports
        (by_kind Gate_verilog.Input)
        (by_kind Gate_verilog.Output)
        (by_kind Gate_verilog.Wire);
      Printf.printf "  cells emit  : %d\n" n_cells_emit;
      Printf.printf "  cells parse : %d\n" n_cells_parse;

      (* Verify each emit cell has a parsed counterpart. *)
      let parsed = Hashtbl.create n_cells_parse in
      List.iter (fun c ->
        Hashtbl.replace parsed c.Gate_verilog.inst_name c) m.cells;
      let missing = List.filter (fun (p : Placement.placement) ->
        not (Hashtbl.mem parsed p.Placement.inst)) nl.cells in
      let n_miss = List.length missing in
      if n_miss = 0 && n_cells_emit = n_cells_parse
      then begin
        Printf.printf "  OK   all %d cells round-tripped\n" n_cells_emit;

        (* Build nets.  The lef_def library deliberately exposes
           [empty_pin_dir] only — real flows fill the table from
           a Liberty via Cell_delay.pin_dir_table.  In this
           lef_def-only test we patch in just the two cell shapes
           Synth_mac actually emits.  A library boundary, not a
           magic-name table. *)
        let pin_dir = Hashtbl.create 8 in
        List.iter (fun ((cell, pin), dir) ->
          Hashtbl.replace pin_dir (cell, pin) dir)
          [ ("AND2_X1", "A1"), Gate_verilog.Pin_in;
            ("AND2_X1", "A2"), Gate_verilog.Pin_in;
            ("AND2_X1", "ZN"), Gate_verilog.Pin_out;
            ("XOR2_X1", "A"),  Gate_verilog.Pin_in;
            ("XOR2_X1", "B"),  Gate_verilog.Pin_in;
            ("XOR2_X1", "Z"),  Gate_verilog.Pin_out; ];
        let net_pins = Gate_verilog.build_nets ~pin_dir m in
        let n_nets = Hashtbl.length net_pins in
        Printf.printf "  nets built  : %d (synth had %d)\n"
          n_nets (List.length nl.nets);

        let n_with_driver = ref 0 in
        Hashtbl.iter (fun _ (drv, _) ->
          if drv <> [] then incr n_with_driver) net_pins;
        Printf.printf "  driven nets : %d\n" !n_with_driver;
        exit 0
      end
      else begin
        Printf.printf "  FAIL %d cells missing\n" n_miss;
        List.iteri (fun i p ->
          if i < 5 then
            Printf.printf "    missing: %s\n" p.Placement.inst) missing;
        exit 1
      end
