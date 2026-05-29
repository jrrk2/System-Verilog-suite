(* test_schem_png.ml — render a schematic to PNG without GTK.

   Usage:
     test_schem_png <top> <out.png> <file.sv> [more.sv ...]

   Synthesises through Synth_pipeline, converts the resulting cell
   netlist to BIR, builds a Schematic_layout, and rasterises via Cairo
   to a PNG file.  Useful for debugging the renderer headlessly. *)

let bir_of_synth_netlists
    (netlists : Hier_synth.module_netlist list) : Behavioral_ir.bprogram =
  let mk_signal direction (name, width) : Behavioral_ir.bsignal = {
    name; direction;
    stype = Behavioral_ir.BInt { width; signed = Unsigned };
    initial_value = None;
    attrs = [];
  } in
  let modules = List.map (fun (mn : Hier_synth.module_netlist) ->
    let signals =
      List.map (mk_signal `Input)  mn.mn_real_inputs
      @ List.map (mk_signal `Output) mn.mn_real_outputs
      @ List.map (fun (w, width) -> mk_signal `Internal (w, width))
                 mn.mn_netlist.wires in
    let cell_insts = List.map (fun (i : Lib_map.instance) ->
      { Behavioral_ir.inst_name = i.inst_name;
        module_name = i.cell.cell_name;
        param_values = []; param_strs = [];
        port_connections =
          List.map (fun (pc : Lib_map.pin_conn) ->
            (pc.pin, Behavioral_ir.BVar pc.net)) i.conns }
    ) mn.mn_netlist.insts in
    { Behavioral_ir.name = mn.mn_name;
      params = []; signals; processes = [];
      instances = cell_insts;
      funcs = []; mems = []; attrs = [] }
  ) netlists in
  let cell_tbl : (string, Behavioral_ir.library_port list) Hashtbl.t =
    Hashtbl.create 64 in
  List.iter (fun (mn : Hier_synth.module_netlist) ->
    List.iter (fun (i : Lib_map.instance) ->
      if not (Hashtbl.mem cell_tbl i.cell.cell_name) then
        let ports =
          List.map (fun p ->
            { Behavioral_ir.port_name = p;
              port_direction = `Input;
              port_width = 1 }) i.cell.in_pins
          @ [ { Behavioral_ir.port_name = i.cell.out_pin;
                port_direction = `Output;
                port_width = 1 } ] in
        Hashtbl.add cell_tbl i.cell.cell_name ports
    ) mn.mn_netlist.insts
  ) netlists;
  let library_cells =
    Hashtbl.fold (fun k v acc -> (k, v) :: acc) cell_tbl [] in
  { modules; library_cells }

let () =
  if Array.length Sys.argv < 4 then begin
    Printf.eprintf "usage: %s <top> <out.png> <file.sv> [more...]\n"
      Sys.argv.(0);
    exit 1
  end;
  let top = Sys.argv.(1) in
  let out_png = Sys.argv.(2) in
  let files = Array.to_list (Array.sub Sys.argv 3 (Array.length Sys.argv - 3)) in
  let synth_out = Filename.temp_file "schem_png_" ".v" in
  Printf.eprintf "[test] synth: top=%s, %d file(s) → %s\n%!"
    top (List.length files) synth_out;
  let netlists, _ =
    Synth_pipeline.run ~emit_verilog:true ~top ~out_path:synth_out ~files () in
  let prog = bir_of_synth_netlists netlists in
  Printf.eprintf "[test] BIR: %d module(s), %d cell type(s)\n%!"
    (List.length prog.modules) (List.length prog.library_cells);
  let m = match List.find_opt (fun (m : Behavioral_ir.bmodule) ->
                  m.name = top) prog.modules with
    | Some m -> m
    | None -> List.hd prog.modules in
  Printf.eprintf "[test] top module: %s — %d instances\n%!"
    m.name (List.length m.instances);
  let slib = Symbol_lib.empty () in
  let sc = Schematic_layout.build ~slib ~prog m in
  Printf.eprintf "[test] schematic: %d cells placed, %d nets, %.0f x %.0f canvas\n%!"
    (List.length sc.sc_insts) (List.length sc.sc_nets)
    sc.sc_width sc.sc_height;
  (* Render with a margin around the natural canvas. *)
  let margin = 40 in
  let w = int_of_float sc.sc_width  + 2 * margin in
  let h = int_of_float sc.sc_height + 2 * margin in
  Printf.eprintf "[test] rasterising %dx%d → %s\n%!" w h out_png;
  Schematic_view.save_png sc out_png;
  Printf.eprintf "[test] OK\n%!"
