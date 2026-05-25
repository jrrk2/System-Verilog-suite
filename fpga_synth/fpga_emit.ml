(* Emit a primitive-mapped netlist for nextpnr-xilinx.
 *
 * Two routes, both implemented:
 *   - emit_verilog: hardcaml's native Verilog (handy for eyeballing /
 *     feeding a Verilog consumer).
 *   - write_yosys_json: a DIRECT Circuit.t -> yosys-`write_json` writer,
 *     so the flow needs no yosys in the loop.  nextpnr-xilinx consumes
 *     this JSON.  Because the input is already a primitive netlist
 *     (LUTk / FDRE / FDCE / IBUF / ...), the conversion is mechanical:
 *     allocate an integer per net bit, walk the Signal graph, and emit
 *     each Instantiation as a cell.  Residual inverters (hardcaml `~:`)
 *     become LUT1 #(.INIT(2'b01)) since nextpnr only accepts techmapped
 *     primitives. *)

open! Base
open Hardcaml
module T = Hardcaml.Signal.Type

(* ---- Verilog (unchanged) ----------------------------------------- *)

let emit_verilog (circ : Circuit.t) : unit = Rtl.print Verilog circ

let write_verilog ~(path : string) (circ : Circuit.t) : unit =
  let oc = Stdlib.open_out path in
  Stdlib.Fun.protect
    ~finally:(fun () -> Stdlib.close_out oc)
    (fun () -> Rtl.output ~output_mode:(To_channel oc) Verilog circ)

(* ---- direct yosys-JSON writer ------------------------------------ *)

(* A net bit is either an allocated integer id or a constant driver. *)
type bit = I of int | C of string

let bit_json = function I i -> `Int i | C s -> `String s
let bits_json arr = `List (Array.to_list (Array.map arr ~f:bit_json))

let uid s = T.Uid.to_int (T.uid s)

let signal_name s =
  match Signal.names s with
  | n :: _ -> n
  | [] -> Printf.sprintf "_n%d" (uid s)

(* yosys writes a bit-vector parameter as its raw bit string (MSB-first);
   nextpnr treats an all-0/1 value as a constant, anything else a string. *)
let param_value (p : Parameter.t) : string =
  let open Parameter.Value in
  match p.value with
  | String v -> v
  | Int v -> Int.to_string v
  | Real v -> Float.to_string v
  | Bool b | Bit b -> if b then "1" else "0"
  | Std_logic v | Std_ulogic v -> String.make 1 (Logic.Std_logic.to_char v)
  | Std_logic_vector v | Std_ulogic_vector v -> Logic.Std_logic_vector.to_string v
  | Bit_vector v -> Logic.Bit_vector.to_string v

let yosys_json (circ : Circuit.t) : Yojson.Safe.t =
  let next_id = ref 2 in
  (* yosys reserves 0/1 for the const strings; nets start at 2. *)
  let fresh () =
    let i = !next_id in
    Int.incr next_id;
    i
  in
  let memo : (int, bit array) Hashtbl.t = Hashtbl.create (module Int) in
  let cells = ref [] in
  let inv_count = ref 0 in
  let lut1_inverter ~(i0 : bit) ~(o : bit) : string * Yojson.Safe.t =
    let nm = Printf.sprintf "inv_%d" !inv_count in
    Int.incr inv_count;
    ( nm
    , `Assoc
        [ "hide_name", `Int 1
        ; "type", `String "LUT1"
        ; "parameters", `Assoc [ "INIT", `String "01" ]
        ; "attributes", `Assoc []
        ; "port_directions", `Assoc [ "I0", `String "input"; "O", `String "output" ]
        ; "connections", `Assoc [ "I0", bits_json [| i0 |]; "O", bits_json [| o |] ]
        ] )
  in
  let rec bits_of sig_ : bit array =
    if T.is_empty sig_ then [||]
    else begin
      let key = uid sig_ in
      match Hashtbl.find memo key with
      | Some v -> v
      | None ->
        let w = Signal.width sig_ in
        let v =
          let open T in
          match sig_ with
          | Empty -> [||]
          | Const { constant; _ } ->
            let s = Hardcaml.Bits.to_string constant in
            Array.init w ~f:(fun i -> C (String.make 1 s.[w - 1 - i]))
          | Wire { driver; _ } ->
            if is_empty !driver then
              Array.init w ~f:(fun _ -> I (fresh ()))
            else bits_of !driver
          | Select { arg; high; low; _ } ->
            let a = bits_of arg in
            Array.init (high - low + 1) ~f:(fun i -> a.(low + i))
          | Cat { args; _ } -> Array.concat (List.rev_map args ~f:bits_of)
          | Not { arg; _ } ->
            let outs = Array.init w ~f:(fun _ -> I (fresh ())) in
            Hashtbl.set memo ~key ~data:outs;
            let a = bits_of arg in
            Array.iteri outs ~f:(fun i o ->
              cells := lut1_inverter ~i0:a.(i) ~o :: !cells);
            outs
          | Inst { instantiation = inst; _ } ->
            let outs = Array.init w ~f:(fun _ -> I (fresh ())) in
            Hashtbl.set memo ~key ~data:outs;
            let conns_in =
              List.map inst.inst_inputs ~f:(fun (p, s) -> p, bits_json (bits_of s))
            in
            let conns_out =
              List.map inst.inst_outputs ~f:(fun (p, (pw, off)) ->
                p, bits_json (Array.sub outs ~pos:off ~len:pw))
            in
            let dirs_in =
              List.map inst.inst_inputs ~f:(fun (p, _) -> p, `String "input")
            in
            let dirs_out =
              List.map inst.inst_outputs ~f:(fun (p, _) -> p, `String "output")
            in
            let params =
              List.map inst.inst_generics ~f:(fun pp ->
                Parameter_name.to_string pp.Parameter.name, `String (param_value pp))
            in
            let cname = Printf.sprintf "%s_%d" inst.inst_instance key in
            cells :=
              ( cname
              , `Assoc
                  [ "hide_name", `Int 1
                  ; "type", `String inst.inst_name
                  ; "parameters", `Assoc params
                  ; "attributes", `Assoc []
                  ; "port_directions", `Assoc (dirs_in @ dirs_out)
                  ; "connections", `Assoc (conns_in @ conns_out)
                  ] )
              :: !cells;
            outs
          | Op2 _ | Mux _ | Reg _ | Multiport_mem _ | Mem_read_port _ ->
            failwith
              "fpga_emit.yosys_json: circuit is not fully techmapped (saw \
               Op2/Mux/Reg/Mem) — run fpga_map first"
        in
        Hashtbl.set memo ~key ~data:v;
        v
    end
  in
  let input_sigs = Circuit.inputs circ in
  let output_sigs = Circuit.outputs circ in
  (* pre-allocate input port nets so they are stable. *)
  List.iter input_sigs ~f:(fun s ->
    let w = Signal.width s in
    Hashtbl.set memo ~key:(uid s) ~data:(Array.init w ~f:(fun _ -> I (fresh ()))));
  let out_ports =
    List.map output_sigs ~f:(fun s -> signal_name s, "output", bits_of s)
  in
  let in_ports =
    List.map input_sigs ~f:(fun s ->
      signal_name s, "input", Hashtbl.find_exn memo (uid s))
  in
  let ports =
    List.map (in_ports @ out_ports) ~f:(fun (nm, dir, bits) ->
      nm, `Assoc [ "direction", `String dir; "bits", bits_json bits ])
  in
  let netnames =
    List.map (in_ports @ out_ports) ~f:(fun (nm, _, bits) ->
      nm, `Assoc [ "hide_name", `Int 0; "bits", bits_json bits; "attributes", `Assoc [] ])
  in
  `Assoc
    [ "creator", `String "fpga_synth"
    ; ( "modules"
      , `Assoc
          [ ( Circuit.name circ
            , `Assoc
                [ "attributes", `Assoc [ "top", `String "00000000000000000000000000000001" ]
                ; "ports", `Assoc ports
                ; "cells", `Assoc (List.rev !cells)
                ; "netnames", `Assoc netnames
                ] )
          ] )
    ]

let yosys_json_string (circ : Circuit.t) : string =
  Yojson.Safe.pretty_to_string (yosys_json circ)

let write_yosys_json ~(path : string) (circ : Circuit.t) : unit =
  Yojson.Safe.to_file path (yosys_json circ)
