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

  (* Encode an HDL constant bit as the sentinel net name bir_to_nextpnr
   * _json's `const_of_name` already understands: "VCC" → "1", "GND" →
   * "0".  The emitter skips actual GND/VCC tie cells (nextpnr-xilinx
   * auto-inserts ties), so any FDRE.CE / FDRE.R / CARRY4.CYINIT input
   * that USED to dangle as `tie_N_<id>` now references "VCC"/"GND"
   * directly and resolves to a "1"/"0" string bit token.              *)
  let const_net_name = function '1' -> "VCC" | _ -> "GND" in

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
              const_net_name s_str.[w - 1 - i])
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

  (* Hardcaml's Circuit.inputs / Circuit.outputs return ONE signal per
   * per-bit scalar (`led__0`, `led__1`, …) when the gate mapper drives
   * outputs per-bit.  Regroup these into vector ports — same logic
   * fpga_emit.regroup_bus_ports uses for its EDIF/yosys-JSON emit —
   * so downstream BIR has a single `led[7:0]` Output bsignal and the
   * wrapper's `.led(led)` connection lines up after flatten_struct. *)
  let split_base_idx (nm : string) : (string * int) option =
    let n = String.length nm in
    let rec find_uu i =
      if i + 1 >= n then None
      else if Char.equal nm.[i] '_' && Char.equal nm.[i+1] '_'
      then
        let suf = String.sub nm ~pos:(i+2) ~len:(n - i - 2) in
        if not (String.is_empty suf)
           && String.for_all suf ~f:Char.is_digit
        then Some (String.sub nm ~pos:0 ~len:i, Int.of_string suf)
        else find_uu (i + 1)
      else find_uu (i + 1)
    in
    find_uu 0
  in
  let regroup ~(dir : [`Input | `Output]) (sigs : Signal.t list)
      : (string * int * (int * Signal.t) list) list =
    let groups : (string, (int * Signal.t) list ref) Hashtbl.t =
      Hashtbl.create (module String) in
    let single : (string * Signal.t) list ref = ref [] in
    let order : [`Bus of string | `Single of string] list ref = ref [] in
    List.iter sigs ~f:(fun s ->
      let nm = signal_name s in
      match split_base_idx nm with
      | Some (base, idx) when Signal.width s = 1 ->
        let lst = Hashtbl.find_or_add groups base ~default:(fun () ->
          order := `Bus base :: !order; ref []) in
        lst := (idx, s) :: !lst
      | _ ->
        single := (nm, s) :: !single;
        order := `Single nm :: !order);
    let _ = dir in
    List.rev_map !order ~f:(function
      | `Single nm ->
        let s = List.Assoc.find_exn !single nm ~equal:String.equal in
        nm, Signal.width s, [(0, s)]
      | `Bus base ->
        let lst = Hashtbl.find_exn groups base in
        let by_idx = List.sort !lst ~compare:(fun (a,_) (b,_) ->
          Int.compare a b) in
        let w = List.length by_idx in
        base, w, by_idx)
  in

  let port_signals : Behavioral_ir.bsignal list ref = ref [] in

  (* Inputs: emit ONE Input bsignal per regrouped port.  For width 1
   * the bsignal name IS the bit name — seed memo with [|base|] so
   * inner FDRE.C / LUT.I0 references resolve straight to `BVar base`
   * and after flatten line up with the wrapper's actual input net.
   * For multi-bit inputs the per-bit memo uses the synthetic
   * `<base>__<i>` form; bir_to_nextpnr_json's bare-BVar fallback
   * expands `BVar base` against widths, so consumers that reference
   * the whole bus directly still work. *)
  List.iter (regroup ~dir:`Input input_sigs)
    ~f:(fun (base, w, by_idx) ->
      port_signals := { Behavioral_ir.name = base;
                        stype = BInt { width = w; signed = Unsigned };
                        direction = `Input;
                        initial_value = None;
                        attrs = [] } :: !port_signals;
      List.iter by_idx ~f:(fun (i, s) ->
        let bit_name =
          if w = 1 then base
          else Printf.sprintf "%s__%d" base i in
        Hashtbl.set memo ~key:(T.uid s) ~data:[| bit_name |]));

  (* Outputs: emit ONE multi-bit Output bsignal per regrouped bus.
   * Drive each bit with an identity LUT acting as a structural
   * buffer so the bit has a concrete driver via BSlice {
   * signal = BVar port; msb=i; lsb=i } — flatten_struct's BSlice
   * rewrite then propagates the wrapper's actual when the wrapper
   * instantiates with .port(port).
   *
   * Use **LUT6** (not LUT1) for this buffer.  LUT6 outputs flow via
   * the SLICE's O6 -> x wire path, which nextpnr-xilinx places at
   * a regular *6LUT BEL slot.  A LUT1 acting as a buffer would land
   * at a *5LUT slot whose O5 output exits via xOUTMUX -> xMUX wire;
   * on V7 silicon that path is unreliable when no FF is co-located
   * in the same slot, manifesting as stuck-high LEDs on counter25
   * (see [[project-106-lut5-outmux-diagnosis]]).  yosys's
   * synth_xilinx avoids the same gap by co-locating an FF; using a
   * LUT6 sidesteps the OUTMUX entirely.  Truth table is identity
   * on I0: INIT[i] = (i & 1) -> 64'hAAAAAAAAAAAAAAAA. *)
  List.iter (regroup ~dir:`Output output_sigs)
    ~f:(fun (base, w, by_idx) ->
      port_signals := { Behavioral_ir.name = base;
                        stype = BInt { width = w; signed = Unsigned };
                        direction = `Output;
                        initial_value = None;
                        attrs = [] } :: !port_signals;
      List.iter by_idx ~f:(fun (i, s) ->
        let inner = (bits_of s).(0) in
        let port_bit =
          if w = 1 then Behavioral_ir.BVar base
          else BSlice { signal = BVar base; msb = i; lsb = i }
        in
        instances := {
          Behavioral_ir.inst_name = Printf.sprintf "obuf_%s_%d" base i;
          module_name = "LUT6";
          param_values = [];
          param_strs = ["INIT", "64'hAAAAAAAAAAAAAAAA"];
          port_connections =
            [ "I0", BVar inner
            ; "I1", BConst { value = 0; width = 1 }
            ; "I2", BConst { value = 0; width = 1 }
            ; "I3", BConst { value = 0; width = 1 }
            ; "I4", BConst { value = 0; width = 1 }
            ; "I5", BConst { value = 0; width = 1 }
            ; "O",  port_bit ];
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
