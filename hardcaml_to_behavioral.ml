(* Hardcaml Circuit.t -> Behavioral_ir.bmodule converter.
 *
 * Inverse of Behavioral_to_hardcaml: lifts a fully gate-mapped Hardcaml
 * circuit (LUTk / FDRE / CARRY4 / IBUF / OBUF / BUFG / IBUFDS / ...)
 * back into structural BIR, preserving bus widths on top-level ports.
 *
 * Motivation (tasks #38 + #39): the existing wrapped_inner_to_nextpnr
 * recipe used to go Circuit.t -> Hardcaml.Rtl write_verilog -> ver_front
 * re-parse, but Hardcaml's Verilog emitter flattens bus ports into
 * scalar `<name>__<i>` declarations.  ver_front then reads them back as
 * scalars and CARRY4.DI/S that should be 4-bit collapse to 1-bit,
 * which crashes nextpnr-xilinx's carry packer.  This module keeps the
 * gate-mapped Circuit.t structural in BIR all the way through the
 * splice step.
 *
 * Output shape: bmodule whose `signals` carry one bsignal per top-level
 * port (multi-bit when the Circuit.t input/output is multi-bit), one
 * width-1 bsignal per internal Hardcaml net (`_n_<id>`), and whose
 * `instances` mirror each Inst node.  Output-port bits that don't fall
 * straight out of an instance pin get an identity LUT1 buffer
 * (#(.INIT(2'b10))) driving the port bit — nextpnr-xilinx happily
 * absorbs identity LUTs during pack.  No BCombinational continuous-
 * assigns: BIR's downstream emitters only understand instance
 * port_connections, so we keep everything structural. *)

open! Base
open Hardcaml
module T = Hardcaml.Signal.Type

(* Pick a stable name for a Hardcaml signal: prefer its first declared
 * name, else fall back to a synthetic `_n<uid>` like fpga_emit does.   *)
let signal_name s =
  match Signal.names s with
  | n :: _ -> n
  | [] -> Printf.sprintf "_n%d" (T.Uid.to_int (T.uid s))

(* Fresh internal-net names. *)
let fresh_net =
  let c = ref 0 in
  fun () -> Int.incr c; Printf.sprintf "_n_%d" !c

(* Translate a Hardcaml Parameter value into the BIR `param_strs`
 * encoding (raw Verilog literal text — same convention ver_front and
 * verible already use).  Same shape as Fpga_synth.Fpga_emit.param_value
 * but keeps a Verilog-literal `<w>'b<bits>` wrapper so ver_front-style
 * consumers parse widths directly.                                    *)
let param_value (p : Parameter.t) : string =
  let open Parameter.Value in
  match p.value with
  | String v -> v
  | Int v ->
    let bits = String.init 32
                 ~f:(fun i -> if (v lsr (31 - i)) land 1 = 1
                              then '1' else '0') in
    "32'b" ^ bits
  | Real v -> Float.to_string v
  | Bool b | Bit b -> if b then "1'b1" else "1'b0"
  | Std_logic v | Std_ulogic v ->
    Printf.sprintf "1'b%c" (Logic.Std_logic.to_char v)
  | Std_logic_vector v | Std_ulogic_vector v ->
    let bits = Logic.Std_logic_vector.to_string v in
    Printf.sprintf "%d'b%s" (String.length bits) bits
  | Bit_vector v ->
    let bits = Logic.Bit_vector.to_string v in
    Printf.sprintf "%d'b%s" (String.length bits) bits

(* Convert a Circuit.t into a structural bmodule.  The circuit MUST be
 * fully techmapped (LUTs / FDREs / CARRY4 / IBUF / OBUF / BUFG /
 * IBUFDS) — Op2 / Mux / Reg / Memory nodes raise. *)
let of_circuit (circ : Circuit.t) : Behavioral_ir.bmodule =
  let memo : (T.Uid.t, string array) Hashtbl.t =
    Hashtbl.create (module T.Uid) in
  let internal_signals : Behavioral_ir.bsignal list ref = ref [] in
  let instances : Behavioral_ir.binstance list ref = ref [] in
  let processes : Behavioral_ir.bprocess list ref = ref [] in

  let add_internal_net () =
    let nm = fresh_net () in
    internal_signals := { Behavioral_ir.name = nm;
                          stype = BInt { width = 1; signed = Unsigned };
                          direction = `Internal;
                          initial_value = None;
                          attrs = [] } :: !internal_signals;
    nm
  in

  let add_const_tie ~value : string =
    let net = add_internal_net () in
    let typ, pin = match value with '1' -> "VCC", "P" | _ -> "GND", "G" in
    instances := {
      Behavioral_ir.inst_name = Printf.sprintf "tie_%s_%s" typ net;
      module_name = typ;
      param_values = []; param_strs = [];
      port_connections = [pin, BVar net];
    } :: !instances;
    net
  in

  let rec bits_of (s : Signal.t) : string array =
    if T.is_empty s then [||]
    else
      let key = T.uid s in
      match Hashtbl.find memo key with
      | Some v -> v
      | None ->
        let w = Signal.width s in
        let v =
          match s with
          | T.Empty -> [||]
          | T.Const { constant; _ } ->
            let s_str = Hardcaml.Bits.to_string constant in
            Array.init w ~f:(fun i ->
              add_const_tie ~value:s_str.[w - 1 - i])
          | T.Wire { driver; _ } ->
            if T.is_empty !driver
            then Array.init w ~f:(fun _ -> add_internal_net ())
            else bits_of !driver
          | T.Select { arg; high; low; _ } ->
            let a = bits_of arg in
            Array.init (high - low + 1) ~f:(fun i -> a.(low + i))
          | T.Cat { args; _ } ->
            Array.concat (List.rev_map args ~f:bits_of)
          | T.Not { arg; _ } ->
            let outs = Array.init w ~f:(fun _ -> add_internal_net ()) in
            Hashtbl.set memo ~key ~data:outs;
            let a = bits_of arg in
            Array.iteri outs ~f:(fun i o ->
              instances := {
                Behavioral_ir.inst_name = "inv_" ^ o;
                module_name = "LUT1";
                param_values = [];
                param_strs = ["INIT", "2'b01"];
                port_connections = ["I0", BVar a.(i); "O", BVar o];
              } :: !instances);
            outs
          | T.Inst { instantiation = inst; _ } ->
            let outs = Array.init w ~f:(fun _ -> add_internal_net ()) in
            Hashtbl.set memo ~key ~data:outs;
            let input_conns = List.map inst.inst_inputs ~f:(fun (p, sig_) ->
              let sw = Signal.width sig_ in
              if sw = 1
              then (p, Behavioral_ir.BVar (bits_of sig_).(0))
              else
                let per_bit = Array.init sw ~f:(fun i ->
                  Behavioral_ir.BVar
                    (bits_of (Signal.select sig_ (sw - 1 - i) (sw - 1 - i))).(0))
                in
                (p, BConcat (Array.to_list per_bit)))
            in
            let output_conns =
              List.map inst.inst_outputs ~f:(fun (p, (pw, off)) ->
                if pw = 1
                then (p, Behavioral_ir.BVar outs.(off))
                else
                  let per_bit = Array.init pw ~f:(fun i ->
                    Behavioral_ir.BVar outs.(off + pw - 1 - i)) in
                  (p, BConcat (Array.to_list per_bit)))
            in
            let params =
              List.map inst.inst_generics ~f:(fun pp ->
                Parameter_name.to_string pp.Parameter.name, param_value pp)
            in
            let cname =
              Printf.sprintf "%s_%d" inst.inst_instance
                (T.Uid.to_int key) in
            instances := {
              Behavioral_ir.inst_name = cname;
              module_name = inst.inst_name;
              param_values = []; param_strs = params;
              port_connections = input_conns @ output_conns;
            } :: !instances;
            outs
          | T.Op2 _ | T.Mux _ | T.Reg _ | T.Multiport_mem _
          | T.Mem_read_port _ ->
            failwith
              "hardcaml_to_behavioral: circuit not fully techmapped \
               (saw Op2/Mux/Reg/Memory) — run fpga_map first"
        in
        Hashtbl.set memo ~key ~data:v;
        v
  in

  let input_sigs = Circuit.inputs circ in
  let output_sigs = Circuit.outputs circ in

  (* Pre-seed memo with per-bit names for input ports.  Each input
   * port is one multi-bit bsignal; the per-bit names are synthetic
   * (`<port>__<i>`) — they appear only inside instance port_connections
   * and never as their own bsignals, so we leave them out of `signals`
   * and let bir_to_nextpnr_json's `bits_of_conn BVar` fallback expand
   * the multi-bit reference into per-bit ids. *)
  let port_signals : Behavioral_ir.bsignal list ref = ref [] in
  List.iter input_sigs ~f:(fun s ->
    let nm = signal_name s in
    let w = Signal.width s in
    let bits = Array.init w ~f:(fun i ->
      Printf.sprintf "%s__%d" nm i) in
    Hashtbl.set memo ~key:(T.uid s) ~data:bits;
    port_signals := { Behavioral_ir.name = nm;
                      stype = BInt { width = w; signed = Unsigned };
                      direction = `Input;
                      initial_value = None;
                      attrs = [] } :: !port_signals);

  (* For each output port, drive each bit with an identity LUT1
   * #(.INIT(2'b10)) (O = I0).  This keeps everything structural —
   * one instance per bit, no BCombinational continuous-assigns — and
   * the bit gets named via BSlice { signal = BVar port; msb=i; lsb=i }
   * which bir_to_nextpnr_json already handles.  nextpnr-xilinx
   * absorbs identity LUT1s during pack.  Multi-bit ports stay
   * multi-bit Output bsignals all the way through. *)
  List.iter output_sigs ~f:(fun s ->
    let nm = signal_name s in
    let w = Signal.width s in
    let inner_bits = bits_of s in
    port_signals := { Behavioral_ir.name = nm;
                      stype = BInt { width = w; signed = Unsigned };
                      direction = `Output;
                      initial_value = None;
                      attrs = [] } :: !port_signals;
    Array.iteri inner_bits ~f:(fun i src ->
      let port_bit =
        if w = 1
        then Behavioral_ir.BVar nm
        else BSlice { signal = BVar nm; msb = i; lsb = i }
      in
      instances := {
        Behavioral_ir.inst_name = Printf.sprintf "obuf_%s_%d" nm i;
        module_name = "LUT1";
        param_values = [];
        param_strs = ["INIT", "2'b10"];
        port_connections = ["I0", BVar src; "O", port_bit];
      } :: !instances));

  {
    Behavioral_ir.name = Circuit.name circ;
    params = [];
    signals = List.rev !port_signals @ List.rev !internal_signals;
    processes = List.rev !processes;
    instances = List.rev !instances;
    funcs = [];
    mems = [];
    attrs = [];
  }
