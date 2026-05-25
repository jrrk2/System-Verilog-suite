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
  (* depth expansion is mux-free: deep memories fit one depth-tile by
     narrowing the per-tile width (up to 32K on RAMB36). *)
  let deep = plan ~depth:8192 ~width:64 () in
  if deep.n_depth_tiles <> 1 then (Printf.printf "FAIL: 8192-deep not mux-free (%d depth tiles)\n" deep.n_depth_tiles; exit 1);
  let deep32k = plan ~depth:32768 ~width:32 () in
  Printf.printf "32K deep: %s\n" (string_of_plan deep32k);
  if deep32k.n_depth_tiles <> 1 then (print_endline "FAIL: 32K not mux-free"; exit 1);
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
  (* byte-lane builder + INIT packing. *)
  let words = [| 0xAB; 0x1234; 0xDEAD; 0x55 |] in
  let lane0 = lane_init_strings ~words ~lane:0 in
  let _, s00 = List.hd lane0 in
  Printf.printf "INIT_00 lane0 low8 = %s (word0 byte0 = 0xAB)\n" (String.sub s00 248 8);
  if String.sub s00 248 8 <> "10101011" then (print_endline "FAIL: INIT byte pack"; exit 1);
  let port a = { p_clk = BVar "clk"; p_addr = BVar a; p_we = BVar "we"; p_wdata = BVar "wd" } in
  let bi, bsig, bst, brd =
    build_byte_lane_ram ~name:"pm" ~depth:4 ~width:32 ~init:words ~ports:[ port "a" ] ()
  in
  Printf.printf "byte-lane ROM: insts=%d (%s) param_strs=%d sigs=%d stmts=%d rports=%d\n"
    (List.length bi) (List.hd bi).module_name (List.length (List.hd bi).param_strs)
    (List.length bsig) (List.length bst) (List.length brd);
  if List.length bi <> 4 then (print_endline "FAIL: lane count"; exit 1);
  if (List.hd bi).module_name <> "RAMB18E1" then (print_endline "FAIL: not RAMB18E1"; exit 1);
  (* 3 base string params (RAM_MODE, WRITE_MODE_A/B) + 64 INIT_xx = 67. *)
  if List.length (List.hd bi).param_strs <> 67 then (print_endline "FAIL: param_strs"; exit 1);
  let _, _, _, brd2 =
    build_byte_lane_ram ~name:"dp" ~depth:8 ~width:32
      ~ports:[ port "ca"; { p_clk = BVar "hclk"; p_addr = BVar "ha"; p_we = BVar "hwe"; p_wdata = BVar "hwd" } ]
      ()
  in
  Printf.printf "dual-port RAM: rdata ports = %d\n" (List.length brd2);
  if List.length brd2 <> 2 then (print_endline "FAIL: dual-port rdata"; exit 1);
  print_endline "PASS"
