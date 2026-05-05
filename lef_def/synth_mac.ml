(* Synthetic placed netlist generator for a multiply-accumulate
   pipeline of width W, parameterised by multiplier and adder
   architecture.  Used to compare archs head-to-head on real
   LEF-based numbers when we don't have a synth+P&R flow at hand,
   and to feed an apples-to-apples netlist into OpenTimer for
   cross-validation.

   Every cell is AND2_X1 — fixing the cell type isolates the
   topology variable so any difference between our timing and
   OpenTimer's must come from wire-delay / slew / path-enumeration
   modelling, not from a cell-delay table mismatch.

   Cells are placed deterministically:
       column = bit position    (width direction)
       row    = pipeline stage  (depth direction)
   so HPWL between consecutive stages reflects the cell pitch and
   the architecture's natural fan-out.

   Each cell's inputs come from:
     A1 ← previous stage, same bit
     A2 ← previous stage, bit offset by the arch's prefix
          stride (1 for ripple, 2^(stage-1) for log-prefix archs)
   driving its ZN pin onto a single-driver/single-load net. *)

type adder_arch =
  | Ripple_a
  | Kogge_stone_a
  | Brent_kung_a
  | Sklansky_a

type mul_arch =
  | Array_m
  | Wallace_m
  | Dadda_m

let adder_arch_name = function
  | Ripple_a -> "ripple"
  | Kogge_stone_a -> "kogge_stone"
  | Brent_kung_a -> "brent_kung"
  | Sklansky_a -> "sklansky"

let mul_arch_name = function
  | Array_m -> "array"
  | Wallace_m -> "wallace"
  | Dadda_m -> "dadda"

let log2_ceil w =
  let rec loop v acc = if v <= 1 then max 1 acc else loop ((v+1)/2) (acc+1) in
  loop w 0

(* Stages on the longest combinational path through each arch. *)
let adder_depth ~arch ~width =
  match arch with
  | Ripple_a       -> width
  | Sklansky_a     -> log2_ceil width
  | Kogge_stone_a  -> log2_ceil width
  | Brent_kung_a   -> max 1 (2 * log2_ceil width - 1)

let mul_depth ~arch ~width =
  match arch with
  | Array_m              -> 2 * width
  | Wallace_m | Dadda_m  -> log2_ceil width + width

let adder_cells ~arch ~width =
  match arch with
  | Ripple_a       -> width
  | Sklansky_a     -> (width * log2_ceil width) / 2 + width
  | Kogge_stone_a  -> width * log2_ceil width + width
  | Brent_kung_a   -> 2 * width

let mul_cells ~arch ~width =
  match arch with
  | Array_m              -> width * width + width * (width - 1)
  | Wallace_m | Dadda_m  -> width * width + (3 * width) / 2 + width

(* When [multi_cell] is true the netlist alternates AND2_X1 and
   XOR2_X1 by stage parity — exactly the cell mix you'd see in a
   real full-adder column.  Both have rise/fall arcs in Nangate45
   and both compose into well-formed Verilog without coaxing
   OpenTimer's Liberty reader past edge cases. *)

type cell_kind = And2 | Xor2

let kind_for ~multi_cell ~stage =
  if multi_cell && stage mod 2 = 1 then Xor2 else And2

let cell_name_of = function
  | And2 -> "AND2_X1"
  | Xor2 -> "XOR2_X1"

let in1_pin_of = function
  | And2 -> "A1"
  | Xor2 -> "A"
let in2_pin_of = function
  | And2 -> "A2"
  | Xor2 -> "B"
let out_pin_of = function
  | And2 -> "ZN"
  | Xor2 -> "Z"

let col_pitch = 1500
let row_pitch = 2800

(* The resulting record makes Verilog/SDC emission and our
   placement_timing pipeline share a single source of truth. *)
type netlist = {
  width      : int;
  mul_arch   : mul_arch;
  add_arch   : adder_arch;
  depth      : int;
  cells      : Placement.placement list;
  nets       : Nets.net list;
  inputs     : string list;            (* primary input port names *)
  outputs    : string list;            (* primary output port names *)
  kind_of    : string -> cell_kind;    (* inst -> kind, for emit *)
}

let inst_name s b = Printf.sprintf "g_s%d_b%d" s b

let prefix_stride ~arch ~stage =
  match arch with
  | Ripple_a -> 1
  | _        -> 1 lsl (max 0 (stage - 1))

let build ?(multi_cell=false) ~width ~mul_arch ~add_arch () =
  let cells = ref [] in
  let nets  = ref [] in
  let kind_tbl = Hashtbl.create 1024 in

  let aw    = 2 * width in     (* MAC output is 2W *)
  let mul_d = mul_depth ~arch:mul_arch ~width in
  let add_d = adder_depth ~arch:add_arch ~width:aw in
  let total_d = mul_d + add_d in

  let inputs  = ref [] in
  let outputs = ref [] in

  (* Stage-0 inputs come from primary input ports a[b], b[b]
     for the multiplier, then the chain carries forward. *)
  let put ~stage ~bit =
    let inst = inst_name stage bit in
    let k = kind_for ~multi_cell ~stage in
    Hashtbl.replace kind_tbl inst k;
    let p =
      { Placement.inst; cell = cell_name_of k;
        x = bit  * col_pitch;
        y = stage * row_pitch;
        orient = Placement.N }
    in
    cells := p :: !cells;
    inst
  in
  let in_pin_of stage which =
    let k = kind_for ~multi_cell ~stage in
    if which = 1 then in1_pin_of k else in2_pin_of k
  in
  let out_pin_for stage =
    out_pin_of (kind_for ~multi_cell ~stage)
  in
  let connect ~src_inst ~src_pin ~dst_inst ~dst_pin =
    let nm = Printf.sprintf "n_%s_%s_to_%s_%s"
               src_inst src_pin dst_inst dst_pin in
    nets := { Nets.name = nm;
              pins = [
                { Nets.inst = src_inst; pin = src_pin };
                { Nets.inst = dst_inst; pin = dst_pin };
              ] } :: !nets
  in
  let connect_port ~port ~dst_inst ~dst_pin =
    inputs := port :: !inputs;
    let nm = "n_" ^ port ^ "_" ^ dst_inst ^ "_" ^ dst_pin in
    nets := { Nets.name = nm;
              pins = [
                (* the "PORT" instance is a fictitious source —
                   placement_timing uses [pin_dir] from LEF, which
                   has no entry for it, so it falls back to
                   first-pin-is-driver.  We encode the port as the
                   first pin so that convention works. *)
                { Nets.inst = port;     pin = "Y" };
                { Nets.inst = dst_inst; pin = dst_pin };
              ] } :: !nets
  in
  let connect_to_port ~port ~src_inst ~src_pin =
    outputs := port :: !outputs;
    let nm = "n_" ^ src_inst ^ "_" ^ src_pin ^ "_" ^ port in
    nets := { Nets.name = nm;
              pins = [
                { Nets.inst = src_inst; pin = src_pin };
                { Nets.inst = port;     pin = "A" };
              ] } :: !nets
  in

  (* Stage 0: W cells across, in1 driven by a[b], in2 by b[b]. *)
  for b = 0 to width - 1 do
    let inst = put ~stage:0 ~bit:b in
    connect_port ~port:(Printf.sprintf "a%d" b)
      ~dst_inst:inst ~dst_pin:(in_pin_of 0 1);
    connect_port ~port:(Printf.sprintf "b%d" b)
      ~dst_inst:inst ~dst_pin:(in_pin_of 0 2);
  done;

  (* Stages 1..total_d-1: width up to aw cells per stage.
     Multiplier reduction uses a ripple-style stride (1) for
     simplicity — its DEPTH already differentiates archs (via
     mul_depth).  Final-add stages use the adder arch's stride. *)
  for s = 1 to total_d - 1 do
    let stride =
      if s < mul_d
      then 1                        (* multiplier reduction *)
      else prefix_stride ~arch:add_arch ~stage:(s - mul_d + 1) in
    let stage_w = if s < mul_d then aw else aw in
    for b = 0 to stage_w - 1 do
      let inst = put ~stage:s ~bit:b in
      let prev_b1 = b in
      let prev_b2 = max 0 (b - stride) in
      (* upstream instances: stage s-1, with appropriate bit *)
      let prev_inst1 =
        if prev_b1 < (if s-1 = 0 then width else aw) then
          Some (inst_name (s-1) prev_b1) else None in
      let prev_inst2 =
        if prev_b2 < (if s-1 = 0 then width else aw) then
          Some (inst_name (s-1) prev_b2) else None in
      let src_pin = out_pin_for (s-1) in
      (match prev_inst1 with
       | Some pi -> connect ~src_inst:pi ~src_pin
                            ~dst_inst:inst ~dst_pin:(in_pin_of s 1)
       | None -> ());
      (match prev_inst2 with
       | Some pi when prev_b2 <> prev_b1 ->
           connect ~src_inst:pi ~src_pin
                   ~dst_inst:inst ~dst_pin:(in_pin_of s 2)
       | _ ->
           (* If the offset bit is out of range or coincides with
              prev_b1, drive A2 from the same upstream cell as A1.
              This keeps the chain — falling back to a primary
              input would create an arrival shortcut that isn't
              representative of the real arch. *)
           (match prev_inst1 with
            | Some pi -> connect ~src_inst:pi ~src_pin
                                  ~dst_inst:inst ~dst_pin:(in_pin_of s 2)
            | None -> ()));
    done
  done;

  (* Final stage outputs: connect the topmost cells to y[b] ports. *)
  for b = 0 to aw - 1 do
    let pi = inst_name (total_d - 1) b in
    connect_to_port ~port:(Printf.sprintf "y%d" b)
                    ~src_inst:pi ~src_pin:(out_pin_for (total_d - 1))
  done;

  (* deduplicate input port names — multiple cells may share a
     single primary input *)
  let uniq xs =
    let seen = Hashtbl.create 16 in
    List.filter (fun x ->
      if Hashtbl.mem seen x then false
      else (Hashtbl.add seen x (); true)) xs in
  {
    width; mul_arch; add_arch;
    depth = total_d;
    cells = List.rev !cells;
    nets  = List.rev !nets;
    inputs  = uniq (List.rev !inputs);
    outputs = uniq (List.rev !outputs);
    kind_of = (fun inst ->
      try Hashtbl.find kind_tbl inst with Not_found -> And2);
  }

(* ── Verilog emitter for OpenTimer ─────────────────────────── *)

let emit_verilog ~module_name ~oc nl =
  let p fmt = Printf.fprintf oc fmt in
  p "module %s (\n" module_name;
  let all_ports = nl.inputs @ nl.outputs @ ["clk"] in
  let n = List.length all_ports in
  List.iteri (fun i nm ->
    Printf.fprintf oc "  %s%s\n" nm
      (if i = n-1 then "" else ",")) all_ports;
  p ");\n";
  List.iter (fun i -> p "  input %s;\n" i) nl.inputs;
  p "  input clk;\n";
  List.iter (fun o -> p "  output %s;\n" o) nl.outputs;
  (* internal wires: one per net (excluding port pseudo-nets,
     which use port names directly) *)
  List.iter (fun (n : Nets.net) ->
    let is_port_net pins =
      List.exists (fun (pr : Nets.pin_ref) ->
        pr.pin = "Y" || pr.pin = "A") pins in
    if not (is_port_net n.pins) then
      p "  wire %s;\n" n.name) nl.nets;
  p "\n";
  (* cell instantiations: each cell drives exactly one
     ZN-output net, and reads its A1/A2 from the appropriate
     incoming nets *)
  let net_for_pin = Hashtbl.create 4096 in
  List.iter (fun (n : Nets.net) ->
    List.iter (fun (pr : Nets.pin_ref) ->
      let key = (pr.inst, pr.pin) in
      Hashtbl.replace net_for_pin key n.name) n.pins) nl.nets;
  let port_set = Hashtbl.create 64 in
  List.iter (fun nm -> Hashtbl.replace port_set nm ()) nl.inputs;
  List.iter (fun nm -> Hashtbl.replace port_set nm ()) nl.outputs;
  List.iter (fun (c : Placement.placement) ->
    if not (Hashtbl.mem port_set c.inst) then begin
      let k    = nl.kind_of c.inst in
      let in1  = in1_pin_of k in
      let in2  = in2_pin_of k in
      let outp = out_pin_of k in
      let net_of pin =
        try
          let nm = Hashtbl.find net_for_pin (c.inst, pin) in
          if List.exists (fun s -> nm = "n_" ^ s ^ "_" ^ c.inst ^ "_" ^ pin)
                         nl.inputs
          then List.find (fun s -> nm = "n_" ^ s ^ "_" ^ c.inst ^ "_" ^ pin)
                         nl.inputs
          else nm
        with Not_found -> "1'b0" in
      let out_net =
        try
          let nm = Hashtbl.find net_for_pin (c.inst, outp) in
          if List.exists (fun s -> nm = "n_" ^ c.inst ^ "_" ^ outp ^ "_" ^ s)
                         nl.outputs
          then List.find (fun s -> nm = "n_" ^ c.inst ^ "_" ^ outp ^ "_" ^ s)
                         nl.outputs
          else nm
        with Not_found -> "1'bz" in
      p "  %s %s ( .%s(%s), .%s(%s), .%s(%s) );\n"
        (cell_name_of k) c.inst in1 (net_of in1) in2 (net_of in2)
        outp out_net
    end) nl.cells;
  p "endmodule\n"

(* ── SDC emitter ─────────────────────────────────────────────── *)

let emit_sdc ~oc ~clock_period nl =
  let p fmt = Printf.fprintf oc fmt in
  p "create_clock -period %.1f -name clk [get_ports clk]\n" clock_period;
  List.iter (fun nm ->
    p "set_input_delay 0 -rise [get_ports %s] -clock clk\n" nm;
    p "set_input_delay 0 -fall [get_ports %s] -clock clk\n" nm;
    p "set_input_transition 0.05 -rise [get_ports %s] -clock clk\n" nm;
    p "set_input_transition 0.05 -fall [get_ports %s] -clock clk\n" nm)
    nl.inputs;
  List.iter (fun nm ->
    p "set_load 4 [get_ports %s]\n" nm;
    p "set_output_delay 0 -rise [get_ports %s] -clock clk\n" nm;
    p "set_output_delay 0 -fall [get_ports %s] -clock clk\n" nm)
    nl.outputs

(* ── ot-shell config script ──────────────────────────────────── *)

let emit_conf ~oc ~lib_path ~v_path ~sdc_path =
  Printf.fprintf oc "read_celllib %s\n" lib_path;
  Printf.fprintf oc "read_verilog %s\n" v_path;
  Printf.fprintf oc "read_sdc %s\n" sdc_path;
  Printf.fprintf oc "report_timing -max -num_paths 1\n"
