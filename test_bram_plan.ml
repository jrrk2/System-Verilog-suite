(* Exercise the FPGA block-RAM tiling planner + single-tile builder. *)
open Behavioral_ir
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
  (* single-tile RAMB36E1 builder structural check. *)
  let inst, sigs, stmts, rd =
    build_single_ramb36 ~name:"m" ~depth:1024 ~width:32 ~write_clk:(BVar "clk")
      ~read_clk:(BVar "clk") ~we:(BVar "we") ~write_addr:(BVar "waddr")
      ~write_data:(BVar "wdata") ~read_addr:(BVar "raddr")
  in
  Printf.printf "builder: %s module=%s params=%d ports=%d sigs=%d stmts=%d rdata=%s\n"
    inst.inst_name inst.module_name (List.length inst.param_values)
    (List.length inst.port_connections) (List.length sigs) (List.length stmts) rd;
  if inst.module_name <> "RAMB36E1" then (print_endline "FAIL: not RAMB36E1"; exit 1);
  if List.assoc "WRITE_WIDTH_A" inst.param_values <> 36 then (print_endline "FAIL: WW_A"; exit 1);
  if List.assoc "READ_WIDTH_B" inst.param_values <> 36 then (print_endline "FAIL: RW_B"; exit 1);
  if List.length inst.port_connections <> 18 then (print_endline "FAIL: port count"; exit 1);
  print_endline "PASS"
