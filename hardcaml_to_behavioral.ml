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

(* Declared INPUT-port widths, keyed by module name, populated by the caller
   (sv_lua.lgate_map) before create_circuit.  of_circuit sizes a regrouped
   input port from the mapped circuit's surviving input bits, but a wide input
   whose high bits' fanout was pruned/renamed narrows (framing_rdata 64->49,
   dropping the ARP target_ip/sender_ip bytes and constant-folding the match).
   Padding each input port back to its declared width -- combined with the
   `<base>__<i>` bitbus_ref resolution -- reconnects those high bits. *)
let declared_input_widths : (string, (string * int) list) Base.Hashtbl.t =
  Hashtbl.create (module String)

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
  (* Output-port base names.  A register that directly feeds an output goes
     through an identity OBUF: register-net and output-port share the source
     name, so naming the register net after it would collide (self-loop
     obuf).  Those FFs already have output-port correspondence for LEC, so we
     leave them anonymous; only INTERNAL registers get the source name. *)
  let output_bases : (string, unit) Hashtbl.t = Hashtbl.create (module String) in
  List.iter (Circuit.outputs circ) ~f:(fun s ->
    match Signal.names s with
    | n :: _ ->
        let base = match String.lsplit2 n ~on:'[' with Some (b, _) -> b | None -> n in
        Hashtbl.set output_bases ~key:base ~data:()
    | [] -> ());
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
            (* Preserve a source net name on the instance output when the
               output signal was explicitly named (fpga_map names FF Q
               outputs with the register net name) — keeps the gate-mapped
               register Q net name-correspondent with the behavioural source
               for Z3 LEC (ffrip matches state by name).  Only single-output
               (width-matched) instances get the name; multi-output boxes
               (GT/MMCM) keep fresh nets. *)
            let is_reg_prim =
              match inst.inst_name with
              | "FDRE" | "FDCE" | "FDPE" | "FDSE" -> true | _ -> false in
            (* Register-name preservation is OPT-IN (FPGA_LEC_NAMES) — it can
               collide a register net with its output port through the identity
               OBUF (self-loop), so the default netlist/bitstream path keeps the
               original anonymous nets.  Z3-LEC runs set the env var. *)
            let lec_names = Option.is_some (Stdlib.Sys.getenv_opt "FPGA_LEC_NAMES") in
            let named_out =
              let nm = inst.inst_instance in
              let base = match String.lsplit2 nm ~on:'[' with Some (b, _) -> b | None -> nm in
              if lec_names
                 && is_reg_prim
                 && String.length nm > 0
                 && not (String.length nm >= 2 && Char.equal nm.[0] '_'
                         && Char.equal nm.[1] 'n')
                 && not (Hashtbl.mem output_bases base)
                 && List.length inst.inst_outputs = 1
                 && (match inst.inst_outputs with [ (_, (pw, _)) ] -> pw = w | _ -> false)
              then Some nm else None in
            let named_net nm =
              internal_signals := { Behavioral_ir.name = nm;
                                    stype = BInt { width = 1; signed = Unsigned };
                                    direction = `Internal;
                                    initial_value = None;
                                    attrs = [] } :: !internal_signals;
              nm in
            let outs =
              match named_out with
              | Some nm when w = 1 -> [| named_net nm |]
              | Some nm ->
                  Array.init w ~f:(fun i -> named_net (Printf.sprintf "%s[%d]" nm i))
              | None -> Array.init w ~f:(fun _ -> add_internal_net ()) in
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
  let declared_w =
    match Hashtbl.find declared_input_widths (Circuit.name circ) with
    | Some l -> (fun base -> Option.value (List.Assoc.find l base ~equal:String.equal) ~default:0)
    | None -> (fun _ -> 0) in
  List.iter (regroup ~dir:`Input input_sigs)
    ~f:(fun (base, w, by_idx) ->
      (* pad to the declared width so high input bits pruned from the mapped
         circuit are still declared on the port and reconnect via bitbus_ref. *)
      let w' = Int.max w (declared_w base) in
      port_signals := { Behavioral_ir.name = base;
                        stype = BInt { width = w'; signed = Unsigned };
                        direction = `Input;
                        initial_value = None;
                        attrs = [] } :: !port_signals;
      List.iter by_idx ~f:(fun (i, s) ->
        let bit_name =
          if w' = 1 then base
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
   * LUT6 sidesteps the OUTMUX entirely.
   *
   * All six LUT inputs are tied to the SAME data signal so
   * nextpnr-xilinx's packer cannot fold a stray LUT5 into the
   * remaining input pins (which it eagerly does when inputs are
   * tied to constants, corrupting the buffer's truth table).
   * With all inputs identical, the address bits reaching the LUT
   * are either 0b000000 or 0b111111, so the only meaningful INIT
   * bits are INIT[0]=0 and INIT[63]=1.  INIT pattern
   * 64'hFFFFFFFF00000000 satisfies that (and is also legible as
   * "high half of the truth table = 1, low half = 0", i.e. the
   * identity function under the all-inputs-tied address scheme).
   *)
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
          param_strs = ["INIT", "64'hFFFFFFFF00000000"];
          port_connections =
            [ "I0", BVar inner
            ; "I1", BVar inner
            ; "I2", BVar inner
            ; "I3", BVar inner
            ; "I4", BVar inner
            ; "I5", BVar inner
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
