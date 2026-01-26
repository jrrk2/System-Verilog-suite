let () =
  let content = Edif_parser.read_file "/Users/jonathan/Downloads/uart.edf" in
  let all_cells = Edif_parser.parse_all_netlist_cells content in

  (* Find slib_fifo__parameterized1 and convert *)
  List.iter (fun (cell : Edif_parser.edif_data) ->
    if cell.module_name = "slib_fifo__parameterized1" then begin
      Printf.printf "EDIF Module: %s\n" cell.module_name;
      Printf.printf "EDIF Ports: %d\n" (List.length cell.ports);
      List.iter (fun (p : Edif_parser.port_info) ->
        Printf.printf "  %s: %s [%d]\n"
          p.name
          (match p.direction with
           | Edif_parser.Input -> "input"
           | Edif_parser.Output -> "output"
           | Edif_parser.Inout -> "inout")
          p.width
      ) cell.ports;

      (* Convert to behavioral *)
      let bmod = Edif_to_behavioral.convert_cell cell in
      Printf.printf "\nBehavioral Module: %s\n" bmod.Behavioral_ir.name;
      Printf.printf "Behavioral Signals: %d\n" (List.length bmod.signals);
      Printf.printf "Port signals:\n";
      List.iter (fun (s : Behavioral_ir.bsignal) ->
        if s.direction <> `Internal then
          Printf.printf "  %s: %s width=%d\n"
            s.name
            (match s.direction with
             | `Input -> "input"
             | `Output -> "output"
             | `Internal -> "internal")
            (Behavioral_ir.width_of_type s.stype)
      ) bmod.signals
    end
  ) all_cells
