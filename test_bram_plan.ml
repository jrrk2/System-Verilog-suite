(* Exercise the FPGA block-RAM tiling planner. *)
open Fpga_bram_resolve

let () =
  let check label p ~exp_tiles =
    Printf.printf "%-22s %s\n" label (string_of_plan p);
    if p.total_tiles <> exp_tiles then begin
      Printf.printf "  FAIL: expected %d tiles\n" exp_tiles;
      exit 1
    end
  in
  check "progmem 4096x32" (plan ~depth:4096 ~width:32 ()) ~exp_tiles:4;
  check "ram 1024x16" (plan ~depth:1024 ~width:16 ()) ~exp_tiles:1;
  check "regfile 32x32" (plan ~depth:32 ~width:32 ()) ~exp_tiles:1;
  check "wide 1024x64" (plan ~depth:1024 ~width:64 ()) ~exp_tiles:2;
  check "deep+wide 8192x64" (plan ~depth:8192 ~width:64 ()) ~exp_tiles:16;
  (* port directions: write pins are inputs, read data is the only output. *)
  let p = plan ~depth:4096 ~width:32 () in
  let ports = tile_ports p.tile in
  let outs =
    List.filter (fun (lp : Behavioral_ir.library_port) -> lp.port_direction = `Output) ports
  in
  Printf.printf "tile ports: %d (outputs: %s)\n" (List.length ports)
    (String.concat "," (List.map (fun (lp : Behavioral_ir.library_port) -> lp.port_name) outs));
  if List.length outs <> 1 then (print_endline "FAIL: expected exactly 1 output port"; exit 1);
  print_endline "PASS"
