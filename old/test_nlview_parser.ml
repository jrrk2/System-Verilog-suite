(* Test program for Nlview parser *)

let main () =
  if Array.length Sys.argv < 2 then begin
    Printf.printf "Usage: %s <schematic.sch>\n" Sys.argv.(0);
    exit 1
  end;

  let filename = Sys.argv.(1) in
  Printf.printf "Parsing Nlview schematic: %s\n\n" filename;

  let sch = Nlview_parser.parse_schematic filename in

  (* Print statistics *)
  Nlview_parser.print_stats sch;

  (* Extract and show sample ports *)
  Printf.printf "Extracting port information...\n";
  let ports = Nlview_parser.get_ports filename in
  Printf.printf "\nSample ports (first 10):\n";
  List.iteri (fun i (name, port) ->
    if i < 10 then
      let dir_str = match port.Nlview_parser.port_direction with
        | Nlview_parser.Input -> "input "
        | Nlview_parser.Output -> "output"
        | Nlview_parser.Inout -> "inout "
      in
      let width_str = match port.Nlview_parser.port_range with
        | Some (h, l) -> Printf.sprintf " [%d:%d]" h l
        | None -> ""
      in
      Printf.printf "  %s %s%s\n" dir_str name width_str
  ) ports;

  (* Extract and show sample instances *)
  Printf.printf "\nExtracting instance information...\n";
  let instances = Nlview_parser.get_instances filename in
  Printf.printf "\nSample instances (first 20):\n";
  List.iteri (fun i (name, inst) ->
    if i < 20 then
      Printf.printf "  %s : %s\n"
        name
        inst.Nlview_parser.inst_type
  ) instances;

  Printf.printf "\nParsing complete!\n"

let _ = main ()
