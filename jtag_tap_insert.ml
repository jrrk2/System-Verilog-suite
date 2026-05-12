(* IEEE 1149.1 JTAG TAP + Boundary Scan Cells on top-level IO pads.

   For every non-JTAG top-level pad of the chip top module:

     INPUT pad P (width W):
       Allocate fresh nets   P_bsc_po[0..W-1]
       Emit per-bit BSC:
          shift_mux : MUX2 (A=P[i],  B=si,        S=shiftdr  → shmux_q)
          shift_ff  : DFF  (D=shmux_q, CK=clockdr → shift_q)
          update_ff : DFF  (D=shift_q, CK=updatedr → update_q)
          out_mux   : MUX2 (A=P[i],  B=update_q,  S=mode     → P_bsc_po[i])
       Rewrite every consumer of P[i] to use P_bsc_po[i] instead.

     OUTPUT pad P (width W):
       Allocate fresh nets   P_bsc_pi[0..W-1]
       Emit per-bit BSC: same structure, PI=P_bsc_pi[i], PO=P[i].
       Rewrite the original driver of P[i] to drive P_bsc_pi[i] instead.

   Chain order: TAP.tdi → bsc[0].si → bsc[0].so → bsc[1].si → ... →
   bsc[N-1].so → TAP.bsr_so.  TAP top-level pins TCK/TMS/TDI/TDO/TRST_N
   are added to mn_real_inputs/outputs.

   The TAP itself is appended to the output Verilog as a regular RTL
   module — its body is well-known boilerplate (16-state FSM, 4-bit IR,
   IDCODE+BYPASS+SAMPLE+EXTEST opcodes).  gate_sim treats it as a
   black-box child_inst at picosoc level; its own faults are out of
   scope (covered by JTAG self-test in real silicon).

   Env-gated: SV_DECOMP_JTAG=1.                                        *)

open Lib_map

let enabled () = Sys.getenv_opt "SV_DECOMP_JTAG" = Some "1"

(* JTAG signal names — single source of truth. *)
let tck   = "jtag_tck"
let tms   = "jtag_tms"
let tdi   = "jtag_tdi"
let tdo   = "jtag_tdo"
let trst  = "jtag_trst_n"

(* BSR control signals from the TAP. *)
let clockdr  = "_jtag_clockdr_"
let shiftdr  = "_jtag_shiftdr_"
let updatedr = "_jtag_updatedr_"
let mode_n   = "_jtag_mode_"
let bsr_si   = "_jtag_bsr_si_"   (* TAP → first BSC *)
let bsr_so   = "_jtag_bsr_so_"   (* last BSC → TAP *)

let is_jtag_port p =
  p = tck || p = tms || p = tdi || p = tdo || p = trst

let bit_name net i width =
  if width = 1 then net else Printf.sprintf "%s[%d]" net i

(* ── Per-bit BSC: emits 4 cells, returns (insts, si, so).  [si] is
   wired to the previous BSC's so (or tdi for the first cell); [so]
   is what the next BSC consumes (or bsr_so for the last cell).      *)

let bsc_cells ~tag ~pi_net ~po_net ~si_net ~so_net =
  let shmux_q = Printf.sprintf "_bsc_shmux_q_%s_" tag in
  let shift_q = Printf.sprintf "_bsc_shift_q_%s_" tag in
  let update_q = Printf.sprintf "_bsc_update_q_%s_" tag in
  let shift_mux_inst = {
    cell = cell_mux;
    inst_name = Printf.sprintf "_bsc_shmux_%s_" tag;
    conns = [
      { pin = "A"; net = pi_net };
      { pin = "B"; net = si_net };
      { pin = "S"; net = shiftdr };
      { pin = "Z"; net = shmux_q };
    ];
  } in
  let shift_ff_inst = {
    cell = cell_dff;
    inst_name = Printf.sprintf "_bsc_shff_%s_" tag;
    conns = [
      { pin = "D";  net = shmux_q };
      { pin = "CK"; net = clockdr };
      { pin = "Q";  net = shift_q };
    ];
  } in
  let update_ff_inst = {
    cell = cell_dff;
    inst_name = Printf.sprintf "_bsc_upff_%s_" tag;
    conns = [
      { pin = "D";  net = shift_q };
      { pin = "CK"; net = updatedr };
      { pin = "Q";  net = update_q };
    ];
  } in
  let out_mux_inst = {
    cell = cell_mux;
    inst_name = Printf.sprintf "_bsc_omux_%s_" tag;
    conns = [
      { pin = "A"; net = pi_net };
      { pin = "B"; net = update_q };
      { pin = "S"; net = mode_n };
      { pin = "Z"; net = po_net };
    ];
  } in
  ([shift_mux_inst; shift_ff_inst; update_ff_inst; out_mux_inst],
   [(shmux_q, 1); (shift_q, 1); (update_q, 1)],
   (* serial passes through this cell's shift FF Q. *)
   shift_q)

(* ── Per-pad bsc chain: emits W bit-level BSCs for a pad of width W,
   threading si/so so the chain extends across the bits.            *)

let chain_for_pad ~tag ~pi_base ~po_base ~width ~si_in =
  let all_insts = ref [] in
  let all_wires = ref [] in
  let cur_si = ref si_in in
  for i = 0 to width - 1 do
    let pi_b = bit_name pi_base i width in
    let po_b = bit_name po_base i width in
    let t = Printf.sprintf "%s_%d" tag i in
    let insts, wires, so = bsc_cells
      ~tag:t ~pi_net:pi_b ~po_net:po_b ~si_net:!cur_si ~so_net:""
      [@warning "-26"] in
    all_insts := insts @ !all_insts;
    all_wires := wires @ !all_wires;
    cur_si := so
  done;
  (!all_insts, !all_wires, !cur_si)

(* ── Top-level pad rewrite.  For each non-JTAG pad, generate fresh
   internal nets, emit a BSC chain bit-by-bit, and rewire every cell
   conn that referenced the old net to use the new internal net.    *)

(* Rewrite every conn in [insts] that names exactly [old_net] to use
   [new_net] instead.  Bit references like old_net[3] become
   new_net[3].                                                        *)
let rewrite_net_in_insts old_net new_net insts =
  let plain_match s = s = old_net in
  let bit_match s =
    let pre = old_net ^ "[" in
    String.length s > String.length pre
    && String.sub s 0 (String.length pre) = pre
    && s.[String.length s - 1] = ']'
  in
  let map_net s =
    if plain_match s then new_net
    else if bit_match s then
      let inner = String.sub s (String.length old_net)
                   (String.length s - String.length old_net) in
      new_net ^ inner
    else s
  in
  List.map (fun (i : instance) ->
    { i with
      conns = List.map (fun c ->
        { c with net = map_net c.net }) i.conns }
  ) insts

(* Same for assigns. *)
let rewrite_net_in_assigns old_net new_net assigns =
  let map_net s =
    if s = old_net then new_net
    else if String.length s > String.length old_net + 2
         && String.sub s 0 (String.length old_net + 1) = old_net ^ "["
         && s.[String.length s - 1] = ']'
    then new_net ^ String.sub s (String.length old_net)
                    (String.length s - String.length old_net)
    else s
  in
  List.map (fun (l, r) -> (map_net l, map_net r)) assigns

(* Wrap one input pad: cells consuming [pad] are rewired to consume
   [pad_internal].  BSC chain runs from si_in to a new so_out.        *)
let wrap_input_pad (nl : netlist) ~tag ~pad ~width ~si_in =
  let pad_internal = Printf.sprintf "%s_bsc_po" pad in
  let new_insts_pre = rewrite_net_in_insts pad pad_internal nl.insts in
  let new_assigns = rewrite_net_in_assigns pad pad_internal nl.assigns in
  let bsc_insts, bsc_wires, so_out =
    chain_for_pad ~tag ~pi_base:pad ~po_base:pad_internal ~width ~si_in in
  let new_wires = (pad_internal, width) :: bsc_wires @ nl.wires in
  { nl with
    insts = new_insts_pre @ bsc_insts;
    wires = new_wires;
    assigns = new_assigns }, so_out

(* Wrap one output pad: driver of [pad] is rewired to drive
   [pad_internal] instead; BSC ports drive [pad].                   *)
let wrap_output_pad (nl : netlist) ~tag ~pad ~width ~si_in =
  let pad_internal = Printf.sprintf "%s_bsc_pi" pad in
  let new_insts_pre = rewrite_net_in_insts pad pad_internal nl.insts in
  let new_assigns = rewrite_net_in_assigns pad pad_internal nl.assigns in
  let bsc_insts, bsc_wires, so_out =
    chain_for_pad ~tag ~pi_base:pad_internal ~po_base:pad ~width ~si_in in
  let new_wires = (pad_internal, width) :: bsc_wires @ nl.wires in
  { nl with
    insts = new_insts_pre @ bsc_insts;
    wires = new_wires;
    assigns = new_assigns }, so_out

(* ── Top-level entry — modify the chip-top module_netlist.  Returns
   the new module_netlist plus a record describing the BSDL chain.  *)

type bsdl_cell = {
  c_name : string;     (* "ser_tx[0]" etc. *)
  c_dir  : [`In | `Out];
  c_index : int;       (* position in scan chain (TDI side = 0) *)
}

type bsdl_info = {
  b_module    : string;
  b_idcode    : string;     (* "32'h<hex>" or 32-char binary *)
  b_ir_width  : int;
  b_cells     : bsdl_cell list;
}

let default_idcode () =
  match Sys.getenv_opt "SV_DECOMP_JTAG_IDCODE" with
  | Some s -> s
  | None -> "32'h0DEC0DE1"  (* arbitrary default *)

let wrap_top (mn : Hier_synth.module_netlist)
  : Hier_synth.module_netlist * bsdl_info =
  let nl = ref mn.mn_netlist in
  let cells = ref [] in
  let next_index = ref 0 in
  let chain_si = ref bsr_si in   (* TAP's bsr_si pin → first BSC's si *)

  (* Walk inputs first, then outputs — IEEE 1149.1 has no required
     order; we just pick a consistent one.                            *)
  let inputs_to_wrap = List.filter (fun (n, _) -> not (is_jtag_port n))
    mn.mn_real_inputs in
  let outputs_to_wrap = List.filter (fun (n, _) -> not (is_jtag_port n))
    mn.mn_real_outputs in

  List.iter (fun (pad, w) ->
    let tag = Printf.sprintf "i_%s" pad in
    let nl', so = wrap_input_pad !nl ~tag ~pad ~width:w ~si_in:!chain_si in
    nl := nl';
    chain_si := so;
    for i = 0 to w - 1 do
      cells := { c_name = bit_name pad i w; c_dir = `In;
                 c_index = !next_index } :: !cells;
      incr next_index
    done
  ) inputs_to_wrap;

  List.iter (fun (pad, w) ->
    let tag = Printf.sprintf "o_%s" pad in
    let nl', so = wrap_output_pad !nl ~tag ~pad ~width:w ~si_in:!chain_si in
    nl := nl';
    chain_si := so;
    for i = 0 to w - 1 do
      cells := { c_name = bit_name pad i w; c_dir = `Out;
                 c_index = !next_index } :: !cells;
      incr next_index
    done
  ) outputs_to_wrap;

  (* Close the chain: last BSC's so → bsr_so net (TAP consumes it). *)
  let close_buf = {
    cell = cell_buf;
    inst_name = "_jtag_bsr_close_";
    conns = [
      { pin = "A"; net = !chain_si };
      { pin = "Z"; net = bsr_so };
    ];
  } in
  nl := { !nl with insts = !nl.insts @ [close_buf] };

  (* Add JTAG IO pins to module interface — preserve any pre-existing
     ones (e.g. user already declared a tdo).                         *)
  let already_has n lst = List.exists (fun (m, _) -> m = n) lst in
  let new_inputs =
    let extras = List.filter (fun (n, _) ->
      not (already_has n mn.mn_real_inputs))
      [(tck, 1); (tms, 1); (tdi, 1); (trst, 1)] in
    mn.mn_real_inputs @ extras in
  let new_outputs =
    if already_has tdo mn.mn_real_outputs then mn.mn_real_outputs
    else mn.mn_real_outputs @ [(tdo, 1)] in

  (* Add control nets to wires. *)
  let new_wires =
    [(clockdr, 1); (shiftdr, 1); (updatedr, 1); (mode_n, 1);
     (bsr_si, 1); (bsr_so, 1)] @ !nl.wires in

  (* TAP child instance: a black box from gate_sim's view; its body
     gets appended to the emitted Verilog separately.                 *)
  let tap_inst = {
    Hier_synth.ci_module = "jtag_tap";
    ci_inst = "_jtag_tap_inst_";
    ci_conns = [
      ("tck", tck); ("tms", tms); ("tdi", tdi); ("tdo", tdo);
      ("trst_n", trst);
      ("clockdr", clockdr); ("shiftdr", shiftdr);
      ("updatedr", updatedr); ("mode", mode_n);
      ("bsr_si", bsr_si); ("bsr_so", bsr_so);
    ];
  } in
  let new_mn =
    { mn with
      mn_real_inputs = new_inputs;
      mn_real_outputs = new_outputs;
      mn_netlist = { !nl with wires = new_wires };
      mn_child_insts = mn.mn_child_insts @ [tap_inst];
    } in
  let info = {
    b_module = mn.mn_name;
    b_idcode = default_idcode ();
    b_ir_width = 4;
    b_cells = List.rev !cells;
  } in
  new_mn, info

(* ── TAP module Verilog body ─────────────────────────────────────── *)

let tap_verilog_body ~idcode =
  Printf.sprintf {|
// IEEE 1149.1 TAP controller — emitted by jtag_tap_insert.
// IDCODE = %s ; IR_WIDTH = 4.
module jtag_tap (
  input  tck, tms, tdi, trst_n,
  output reg tdo,
  output clockdr, shiftdr, updatedr,
  output mode,
  output bsr_si,
  input  bsr_so
);
  // TAP states (IEEE 1149.1)
  localparam [3:0] S_TLR=0, S_RTI=1, S_SDR=2, S_CDR=3, S_SHR=4, S_E1R=5,
                   S_PDR=6, S_E2R=7, S_UDR=8, S_SIR=9, S_CIR=10, S_SHI=11,
                   S_E1I=12, S_PII=13, S_E2I=14, S_UDI=15;
  reg [3:0] state, n_state;
  always @(posedge tck or negedge trst_n)
    if (!trst_n) state <= S_TLR;
    else         state <= n_state;
  always @* case (state)
    S_TLR: n_state = tms ? S_TLR : S_RTI;
    S_RTI: n_state = tms ? S_SDR : S_RTI;
    S_SDR: n_state = tms ? S_SIR : S_CDR;
    S_CDR: n_state = tms ? S_E1R : S_SHR;
    S_SHR: n_state = tms ? S_E1R : S_SHR;
    S_E1R: n_state = tms ? S_UDR : S_PDR;
    S_PDR: n_state = tms ? S_E2R : S_PDR;
    S_E2R: n_state = tms ? S_UDR : S_SHR;
    S_UDR: n_state = tms ? S_SDR : S_RTI;
    S_SIR: n_state = tms ? S_TLR : S_CIR;
    S_CIR: n_state = tms ? S_E1I : S_SHI;
    S_SHI: n_state = tms ? S_E1I : S_SHI;
    S_E1I: n_state = tms ? S_UDI : S_PII;
    S_PII: n_state = tms ? S_E2I : S_PII;
    S_E2I: n_state = tms ? S_UDI : S_SHI;
    S_UDI: n_state = tms ? S_SDR : S_RTI;
    default: n_state = S_TLR;
  endcase

  // Instruction register
  localparam [3:0] OP_EXTEST=4'b0000, OP_SAMPLE=4'b0001,
                   OP_IDCODE=4'b1110, OP_BYPASS=4'b1111;
  reg [3:0] ir_shift, ir_latch;
  always @(posedge tck or negedge trst_n)
    if (!trst_n) ir_latch <= OP_IDCODE;
    else if (state == S_UDI) ir_latch <= ir_shift;
  always @(posedge tck)
    if (state == S_CIR) ir_shift <= {2'b01, ir_latch[1:0]}; // 01 marker
    else if (state == S_SHI) ir_shift <= {tdi, ir_shift[3:1]};

  // Data registers
  reg        bypass_ff;
  reg [31:0] idcode_ff;
  always @(posedge tck) begin
    if (state == S_CDR) begin
      bypass_ff <= 1'b0;
      idcode_ff <= %s;
    end else if (state == S_SHR) begin
      bypass_ff <= tdi;
      idcode_ff <= {tdi, idcode_ff[31:1]};
    end
  end

  // BSR control fan-out
  assign clockdr  = tck & (state == S_CDR || state == S_SHR);
  assign shiftdr  = (state == S_SHR);
  assign updatedr = (state == S_UDR) & (ir_latch == OP_EXTEST);
  assign mode     = (ir_latch == OP_EXTEST);
  assign bsr_si   = tdi;

  // TDO output mux + enable
  always @(negedge tck) begin
    case (ir_latch)
      OP_BYPASS: tdo <= bypass_ff;
      OP_IDCODE: tdo <= idcode_ff[0];
      OP_SAMPLE, OP_EXTEST: tdo <= bsr_so;
      default:   tdo <= bypass_ff;
    endcase
    if (state == S_SHI) tdo <= ir_shift[0];
  end
endmodule
|} idcode idcode

(* ── BSDL emitter ─────────────────────────────────────────────────── *)

let render_bsdl (info : bsdl_info) : string =
  let buf = Buffer.create 4096 in
  let add = Buffer.add_string buf in
  add (Printf.sprintf "-- BSDL for module %s (auto-generated)\n\n" info.b_module);
  add (Printf.sprintf "entity %s is\n" info.b_module);
  add "  generic (PHYSICAL_PIN_MAP : string := \"DUMMY\");\n";
  add "  port (\n";
  add (Printf.sprintf "    %s : in  bit;\n" tck);
  add (Printf.sprintf "    %s : in  bit;\n" tms);
  add (Printf.sprintf "    %s : in  bit;\n" tdi);
  add (Printf.sprintf "    %s : in  bit;\n" trst);
  add (Printf.sprintf "    %s : out bit;\n" tdo);
  let pads = List.map (fun c ->
    Printf.sprintf "    %s : %s bit"
      c.c_name (match c.c_dir with `In -> "in " | `Out -> "out")
  ) info.b_cells in
  add (String.concat ";\n" pads);
  add ");\n";
  add (Printf.sprintf "end %s;\n\n" info.b_module);
  add (Printf.sprintf "attribute COMPONENT_CONFORMANCE of %s : entity is\n" info.b_module);
  add "  \"STD_1149_1_2001\";\n\n";
  add (Printf.sprintf "attribute TAP_SCAN_IN    of %s : signal is true;\n" tdi);
  add (Printf.sprintf "attribute TAP_SCAN_OUT   of %s : signal is true;\n" tdo);
  add (Printf.sprintf "attribute TAP_SCAN_MODE  of %s : signal is true;\n" tms);
  add (Printf.sprintf "attribute TAP_SCAN_RESET of %s : signal is true;\n" trst);
  add (Printf.sprintf "attribute TAP_SCAN_CLOCK of %s : signal is (1.0e6, BOTH);\n\n" tck);
  add (Printf.sprintf "attribute INSTRUCTION_LENGTH of %s : entity is %d;\n"
    info.b_module info.b_ir_width);
  add (Printf.sprintf "attribute INSTRUCTION_OPCODE of %s : entity is\n" info.b_module);
  add "  \"EXTEST  (0000),\" &\n";
  add "  \"SAMPLE  (0001),\" &\n";
  add "  \"IDCODE  (1110),\" &\n";
  add "  \"BYPASS  (1111)\";\n\n";
  add (Printf.sprintf "attribute IDCODE_REGISTER of %s : entity is %s;\n\n"
    info.b_module info.b_idcode);
  add (Printf.sprintf "attribute BOUNDARY_LENGTH of %s : entity is %d;\n"
    info.b_module (List.length info.b_cells));
  add (Printf.sprintf "attribute BOUNDARY_REGISTER of %s : entity is\n"
    info.b_module);
  let cell_lines = List.map (fun c ->
    let func = match c.c_dir with `In -> "input" | `Out -> "output3" in
    Printf.sprintf "  \"%d (BC_1, %s, %s, X)\""
      c.c_index c.c_name func
  ) info.b_cells in
  add (String.concat " &\n" cell_lines);
  add ";\n";
  Buffer.contents buf
