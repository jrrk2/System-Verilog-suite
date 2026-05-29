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
  (* yosys/nextpnr read an all-0/1 value as a bit-vector and anything else
     as a string, so an integer param must be emitted as binary (e.g.
     WRITE_WIDTH_A=36 -> "...0100100"), not decimal. *)
  | Int v -> String.init 32 ~f:(fun i -> if (v lsr (31 - i)) land 1 = 1 then '1' else '0')
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
            (* Per-bit Signal.select for input ports so each bit
               position maps cleanly to its net id, independent of
               Hardcaml's Cat args order.  See task #58. *)
            let bits_of_port_input s =
              let sw = Signal.width s in
              if sw = 1 then bits_of s
              else
                Array.init sw ~f:(fun i ->
                  (bits_of (Signal.select s i i)).(0))
            in
            let conns_in =
              List.map inst.inst_inputs ~f:(fun (p, s) ->
                p, bits_json (bits_of_port_input s))
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
  (* Re-group bit-named top-level ports (`led__0`..`led__N-1`) into a
     single vectored port so downstream tools (Vivado XDC, nextpnr,
     hand-written test harnesses) can address the bus by base name.
     fpga_map drives outputs per-bit because the AIG graph carries
     per-bit names from [Bir_to_aig.port_bit_names], but that's an
     internal-representation choice that shouldn't leak into the
     emitted netlist's public interface. *)
  let regroup ports =
    let split_base_idx nm =
      match String.substr_index nm ~pattern:"__" with
      | None -> None
      | Some i ->
        let suf = String.sub nm ~pos:(i + 2) ~len:(String.length nm - i - 2) in
        if String.is_empty suf
           || not (String.for_all suf ~f:Char.is_digit) then None
        else Some (String.sub nm ~pos:0 ~len:i, Int.of_string suf)
    in
    let order = ref [] in
    let groups : (string, (string * (int * bit array) list ref)) Hashtbl.t =
      Hashtbl.create (module String) in
    let single : (string, (string * bit array)) Hashtbl.t =
      Hashtbl.create (module String) in
    List.iter ports ~f:(fun (nm, dir, bits) ->
      match split_base_idx nm with
      | Some (base, idx) when Array.length bits = 1 ->
        let entry =
          Hashtbl.find_or_add groups base ~default:(fun () ->
            order := `Bus base :: !order;
            dir, ref [])
        in
        let _, lst = entry in
        lst := (idx, bits) :: !lst
      | _ ->
        Hashtbl.set single ~key:nm ~data:(dir, bits);
        order := `Single nm :: !order);
    List.rev_map !order ~f:(function
      | `Single nm ->
        let dir, bits = Hashtbl.find_exn single nm in
        nm, dir, bits
      | `Bus base ->
        let dir, bits_ref = Hashtbl.find_exn groups base in
        let by_idx = List.sort !bits_ref ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
        let bits = Array.concat_map (Array.of_list by_idx)
                     ~f:(fun (_, b) -> b)
        in
        base, dir, bits)
  in
  let grouped = regroup (in_ports @ out_ports) in
  let ports =
    List.map grouped ~f:(fun (nm, dir, bits) ->
      nm, `Assoc [ "direction", `String dir; "bits", bits_json bits ])
  in
  let netnames =
    List.map grouped ~f:(fun (nm, _, bits) ->
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

(* ---- EDIF writer -------------------------------------------------- *)

(* Emit the same mapped Circuit.t as EDIF 2 0 0 so Vivado (or any tool
 * with read_edif) can place + route it.  Lets us compare nextpnr-xilinx
 * vs Vivado P&R quality on the suite's identical synth output.
 *
 * The EDIF dialect targets Vivado's read_edif:
 *   (edif <top>)
 *   (Library hdi_primitives  ... cell declarations for LUT*, FDRE, ...)
 *   (Library work            ... top cell with interface + contents)
 *   (Design <top> (cellref ...))
 *
 * Primitive ports are derived from the Inst signals' inst_inputs /
 * inst_outputs lists.  EDIF identifiers must be C-ish; any net or
 * port name with brackets etc. gets a `(rename safe_name "original")`
 * pair.  Each net bit is its own scalar EDIF net (matches the
 * yosys-JSON net-id allocation). *)

(* Safe-ID + rename helper. EDIF identifiers must start with an alpha or
   `_` and contain only alpha/digit/`_`.  When the original name has
   anything else (brackets, slashes, etc.), we emit `(rename sid "orig")`
   so Vivado preserves the source string in its UI but the parser sees a
   clean identifier. *)
let edif_safe_char c =
  let n = Char.to_int c in
  let between a b = n >= Char.to_int a && n <= Char.to_int b in
  between 'a' 'z' || between 'A' 'Z' || between '0' '9' || Char.(=) c '_'

let edif_safe_first c =
  let n = Char.to_int c in
  let between a b = n >= Char.to_int a && n <= Char.to_int b in
  between 'a' 'z' || between 'A' 'Z' || Char.(=) c '_'

let edif_safe_id (s : string) : string =
  if String.is_empty s then "_anon"
  else
    let buf = Buffer.create (String.length s) in
    Buffer.add_char buf (if edif_safe_first s.[0] then s.[0] else '_');
    (* skip index 0: we already wrote the (possibly remapped) first char *)
    for i = 1 to String.length s - 1 do
      let c = s.[i] in
      Buffer.add_char buf (if edif_safe_char c then c else '_')
    done;
    Buffer.contents buf

(* Emit `name` if it's already EDIF-safe, otherwise emit
   `(rename sid "orig")`.  Used for port names, instance names,
   net names.                                                          *)
let edif_id_or_rename (s : string) : string =
  if String.for_all s ~f:edif_safe_char && edif_safe_first s.[0]
  then s
  else Printf.sprintf "(rename %s \"%s\")" (edif_safe_id s) s

(* Walk the Circuit.t in the same way as yosys_json, but collect data
   destined for EDIF: per-instance (type, name, params, conns), per-port
   (name, dir, bits), and the set of distinct primitive types with one
   representative Inst (for the library cell declarations). *)
type edif_inst = {
  e_type : string;
  e_name : string;
  e_params : (string * string) list;
  e_inputs  : (string * bit array) list;   (* port -> per-bit nets *)
  e_outputs : (string * bit array) list;
}

let edif_walk (circ : Circuit.t) =
  let next_id = ref 2 in
  let fresh () = let i = !next_id in Int.incr next_id; i in
  let memo : (int, bit array) Hashtbl.t = Hashtbl.create (module Int) in
  let cells = ref [] in
  let prim_specs : (string,
                    (string * int * [`Input|`Output]) list) Hashtbl.t =
    Hashtbl.create (module String)
  in
  let inv_count = ref 0 in
  let record_prim ~ty ~ins ~outs =
    if not (Hashtbl.mem prim_specs ty) then begin
      let specs =
        List.map ins ~f:(fun (p, bits) -> p, Array.length bits, `Input)
        @ List.map outs ~f:(fun (p, bits) -> p, Array.length bits, `Output)
      in
      Hashtbl.set prim_specs ~key:ty ~data:specs
    end
  in
  let rec bits_of sig_ : bit array =
    if T.is_empty sig_ then [||]
    else
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
            if T.is_empty !driver
            then Array.init w ~f:(fun _ -> I (fresh ()))
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
              let name = Printf.sprintf "inv_%d" !inv_count in
              Int.incr inv_count;
              let i0 = [| a.(i) |] and o' = [| o |] in
              record_prim ~ty:"LUT1" ~ins:[("I0", i0)] ~outs:[("O", o')];
              cells := { e_type = "LUT1"
                       ; e_name = name
                       ; e_params = [ ("INIT", "01") ]
                       ; e_inputs = [ ("I0", i0) ]
                       ; e_outputs = [ ("O", o') ] } :: !cells);
            outs
          | Inst { instantiation = inst; _ } ->
            let outs = Array.init w ~f:(fun _ -> I (fresh ())) in
            Hashtbl.set memo ~key ~data:outs;
            (* Per-bit Signal.select gives explicit position semantics
               that don't depend on whatever order Hardcaml stores Cat
               args in.  Each (port, bit_idx) cleanly maps to the
               correct net id.  See task #58. *)
            let bits_of_port_input s =
              let sw = Signal.width s in
              if sw = 1 then bits_of s
              else
                Array.init sw ~f:(fun i ->
                  (bits_of (Signal.select s i i)).(0))
            in
            let ins =
              List.map inst.inst_inputs ~f:(fun (p, s) ->
                p, bits_of_port_input s) in
            let outps =
              List.map inst.inst_outputs ~f:(fun (p, (pw, off)) ->
                p, Array.sub outs ~pos:off ~len:pw) in
            record_prim ~ty:inst.inst_name ~ins ~outs:outps;
            let params =
              List.map inst.inst_generics ~f:(fun pp ->
                Parameter_name.to_string pp.Parameter.name, param_value pp)
            in
            let cname = Printf.sprintf "%s_%d" inst.inst_instance key in
            cells := { e_type = inst.inst_name
                     ; e_name = cname
                     ; e_params = params
                     ; e_inputs = ins
                     ; e_outputs = outps } :: !cells;
            outs
          | Op2 _ | Mux _ | Reg _ | Multiport_mem _ | Mem_read_port _ ->
            failwith
              "fpga_emit.write_edif: circuit is not fully techmapped \
               (saw Op2/Mux/Reg/Mem) — run fpga_map first"
        in
        Hashtbl.set memo ~key ~data:v;
        v
  in
  let in_sigs = Circuit.inputs circ in
  let out_sigs = Circuit.outputs circ in
  List.iter in_sigs ~f:(fun s ->
    let w = Signal.width s in
    Hashtbl.set memo ~key:(uid s) ~data:(Array.init w ~f:(fun _ -> I (fresh ()))));
  let outs =
    List.map out_sigs ~f:(fun s -> signal_name s, `Output, Signal.width s, bits_of s) in
  let ins =
    List.map in_sigs ~f:(fun s ->
      signal_name s, `Input, Signal.width s,
      Hashtbl.find_exn memo (uid s)) in
  (List.rev !cells, ins @ outs, prim_specs)

(* Format a bit reference for an EDIF (portRef ...).  An integer net id i
   becomes the net name "n<i>"; a constant becomes GND/VCC instance
   reference (we emit one GND and one VCC instance at the top level).   *)
let edif_net_name_of_bit = function
  | I i -> Printf.sprintf "n%d" i
  | C "0" -> "n_GND"
  | C "1" -> "n_VCC"
  | C _   -> "n_GND"

(* Re-group bit-named top-level ports (`led__0`, `led__1`, …) into a
   single vectored port (`led`, width 8).  fpga_map drives top outputs
   per-bit because the AIG graph carries per-bit names from
   [Bir_to_aig.port_bit_names], so a Verilog source `output [7:0] led`
   emerges from the mapper as eight 1-bit Signal.outputs.  XDC and any
   downstream consumer (Vivado especially) needs the bus form, so do
   the inverse here: cluster ports whose names match `<base>__<N>`
   into a single port whose [bits] array is the per-bit nets in
   LSB-first order.  Ports with the same base but inconsistent
   widths/directions are left alone (matches conservative behaviour;
   shouldn't happen in practice).                                      *)
let regroup_bus_ports (ports : (string * [`Input|`Output] * int * bit array) list)
  : (string * [`Input|`Output] * int * bit array) list =
  let split_base_idx nm =
    match String.substr_index nm ~pattern:"__" with
    | None -> None
    | Some i ->
      let base = String.sub nm ~pos:0 ~len:i in
      let suf = String.sub nm ~pos:(i + 2) ~len:(String.length nm - i - 2) in
      if String.is_empty suf
         || not (String.for_all suf ~f:Char.is_digit) then None
      else Some (base, Int.of_string suf)
  in
  let order = ref [] in           (* output order of bases (and standalone ports) *)
  let groups : (string,
                ([`Input|`Output] * (int * bit array) list ref)) Hashtbl.t =
    Hashtbl.create (module String)
  in
  let standalone = ref [] in
  List.iter ports ~f:(fun (nm, dir, w, bits) ->
    if w <> 1 then begin
      standalone := (nm, dir, w, bits) :: !standalone;
      order := `Single nm :: !order
    end
    else
      match split_base_idx nm with
      | None ->
        standalone := (nm, dir, w, bits) :: !standalone;
        order := `Single nm :: !order
      | Some (base, idx) ->
        let entry =
          Hashtbl.find_or_add groups base ~default:(fun () ->
            order := `Bus base :: !order;
            dir, ref [])
        in
        let cur_dir, lst = entry in
        if Poly.(<>) cur_dir dir then begin
          (* direction collision — bail to the standalone form. *)
          standalone := (nm, dir, w, bits) :: !standalone;
          order := `Single nm :: !order
        end
        else lst := (idx, bits) :: !lst);
  let order = List.rev !order in
  (* dedup the order list; if a base also appeared as `Single (unlikely
     given our gating), keep the first occurrence. *)
  let seen = Hashtbl.create (module String) in
  List.filter_map order ~f:(function
    | `Single nm ->
      if Hashtbl.mem seen nm then None
      else (Hashtbl.set seen ~key:nm ~data:(); Some (`Single nm))
    | `Bus base ->
      if Hashtbl.mem seen base then None
      else (Hashtbl.set seen ~key:base ~data:(); Some (`Bus base)))
  |> List.map ~f:(function
    | `Single nm ->
      List.find_exn !standalone ~f:(fun (n, _, _, _) -> String.equal n nm)
    | `Bus base ->
      let dir, bits_ref = Hashtbl.find_exn groups base in
      let by_idx = List.sort !bits_ref ~compare:(fun (a, _) (b, _) -> Int.compare a b) in
      let w = List.length by_idx in
      let bits = Array.concat_map (Array.of_list by_idx) ~f:(fun (_, bits) -> bits) in
      base, dir, w, bits)

let write_edif ~(path : string) (circ : Circuit.t) : unit =
  let cells, ports, prim_specs = edif_walk circ in
  let ports = regroup_bus_ports ports in
  let buf = Buffer.create (256 * 1024) in
  let p fmt = Printf.ksprintf (Buffer.add_string buf) fmt in
  let top_name = Circuit.name circ in
  let now = Unix.gmtime (Unix.time ()) in
  p "(edif %s\n" (edif_id_or_rename top_name);
  p "  (edifversion 2 0 0)\n";
  p "  (edifLevel 0)\n";
  p "  (keywordmap (keywordlevel 0))\n";
  p "  (status (written (timeStamp %d %d %d %d %d %d)\n      (program \"fpga_synth\")))\n"
    (now.tm_year + 1900) (now.tm_mon + 1) now.tm_mday
    now.tm_hour now.tm_min now.tm_sec;

  (* hdi_primitives library: declare every primitive we use. *)
  p "  (Library hdi_primitives\n";
  p "    (edifLevel 0)\n";
  p "    (technology (numberDefinition))\n";
  (* Built-in GND/VCC always declared (we emit them in the top contents
     so constants resolve cleanly).                                     *)
  p "    (cell GND (celltype GENERIC) (view netlist (viewtype NETLIST) (interface (port G (direction OUTPUT)))))\n";
  p "    (cell VCC (celltype GENERIC) (view netlist (viewtype NETLIST) (interface (port P (direction OUTPUT)))))\n";
  Hashtbl.iteri prim_specs ~f:(fun ~key:ty ~data:specs ->
    p "    (cell %s (celltype GENERIC)\n" ty;
    p "      (view netlist (viewtype NETLIST)\n";
    p "        (interface\n";
    List.iter specs ~f:(fun (pname, w, dir) ->
      let dir_s = match dir with `Input -> "INPUT" | `Output -> "OUTPUT" in
      if w = 1 then
        p "          (port %s (direction %s))\n" pname dir_s
      else
        p "          (port (array (rename %s \"%s[%d:0]\") %d) (direction %s))\n"
          pname pname (w - 1) w dir_s);
    p "        )))\n");
  p "  )\n";

  (* work library: the top cell. *)
  p "  (Library work\n";
  p "    (edifLevel 0)\n";
  p "    (technology (numberDefinition))\n";
  p "    (cell %s (celltype GENERIC)\n" (edif_safe_id top_name);
  p "      (view %s (viewtype NETLIST)\n" (edif_safe_id top_name);
  p "        (interface\n";
  List.iter ports ~f:(fun (nm, dir, w, _bits) ->
    let dir_s = match dir with `Input -> "INPUT" | `Output -> "OUTPUT" in
    if w = 1 then
      p "          (port %s (direction %s))\n" (edif_id_or_rename nm) dir_s
    else
      p "          (port (array (rename %s \"%s[%d:0]\") %d) (direction %s))\n"
        (edif_safe_id nm) nm (w - 1) w dir_s);
  p "        )\n";
  p "        (contents\n";
  (* GND / VCC instances so constants 0 / 1 have a driver to wire onto. *)
  p "          (instance n_GND_inst (viewref netlist (cellref GND (libraryref hdi_primitives))))\n";
  p "          (instance n_VCC_inst (viewref netlist (cellref VCC (libraryref hdi_primitives))))\n";
  List.iter cells ~f:(fun c ->
    p "          (instance %s (viewref netlist (cellref %s (libraryref hdi_primitives)))"
      (edif_id_or_rename c.e_name) c.e_type;
    List.iter c.e_params ~f:(fun (k, v) ->
      (* Vivado property literal format depends on type:
         - bit-vector parameters (INIT, INIT_A..D, IS_*_INVERTED) ⇒ "N'bV"
         - enum-string parameters (RAM_MODE="TDP", WRITE_MODE_A="READ_FIRST",
           etc.) ⇒ plain "V" (no bit-width prefix).
         Heuristic: a value made entirely of '0'/'1' characters is a
         bit-vector; anything else is a string. *)
      let is_binary = String.length v > 0
                    && String.for_all v ~f:(fun c -> Char.(=) c '0' || Char.(=) c '1') in
      let value =
        if is_binary then Printf.sprintf "%d'b%s" (String.length v) v
        else v
      in
      p "\n            (property %s (string \"%s\"))" k value);
    p "\n          )\n");
  (* Nets: collect every (port_pin, net_bit) reference and group by net. *)
  let net_uses : (string, string list) Hashtbl.t =
    Hashtbl.create (module String)
  in
  let add_use net portref =
    let cur = match Hashtbl.find net_uses net with Some l -> l | None -> [] in
    Hashtbl.set net_uses ~key:net ~data:(portref :: cur)
  in
  (* Vivado/Xilinx EDIF convention:  for an array port renamed as
     "S[high:0]" (descending), `(member S 0)` corresponds to Verilog
     S[high] (the MSB), not S[0].  bits arrays internally are
     LSB-first (bits[0] = LSB), so we reverse the member index when
     emitting so the LSB lands at `(member S high)` = Verilog S[0].
     Without this, x+1 carry chains end up with the +1 XOR at the MSB
     lane and the prescaler/counter never increments — see task #58. *)
  let mem_idx ~w i = w - 1 - i in
  (* Per-instance pin refs *)
  List.iter cells ~f:(fun c ->
    let inst_id = edif_safe_id c.e_name in
    List.iter (c.e_inputs @ c.e_outputs) ~f:(fun (pin, bits) ->
      let n = Array.length bits in
      Array.iteri bits ~f:(fun bit_i b ->
        let net = edif_net_name_of_bit b in
        let pref =
          if n = 1 then
            Printf.sprintf "(portref %s (instanceref %s))" pin inst_id
          else
            Printf.sprintf "(portref (member %s %d) (instanceref %s))"
              pin (mem_idx ~w:n bit_i) inst_id
        in
        add_use net pref)));
  (* GND/VCC drivers *)
  add_use "n_GND" "(portref G (instanceref n_GND_inst))";
  add_use "n_VCC" "(portref P (instanceref n_VCC_inst))";
  (* Top-level port refs *)
  List.iter ports ~f:(fun (nm, _, w, bits) ->
    let safe = edif_safe_id nm in
    Array.iteri bits ~f:(fun bit_i b ->
      let net = edif_net_name_of_bit b in
      let pref =
        if w = 1 then Printf.sprintf "(portref %s)" safe
        else Printf.sprintf "(portref (member %s %d))" safe (mem_idx ~w bit_i)
      in
      add_use net pref));
  Hashtbl.iteri net_uses ~f:(fun ~key:net ~data:uses ->
    p "          (net %s\n            (joined" net;
    List.iter uses ~f:(fun u -> p "\n              %s" u);
    p ")\n          )\n");
  p "        )\n";   (* end contents *)
  p "      )\n";     (* end view *)
  p "    )\n";       (* end cell *)
  p "  )\n";         (* end Library work *)
  p "  (Design %s (cellref %s (libraryref work)))\n"
    (edif_safe_id top_name) (edif_safe_id top_name);
  p ")\n";
  let oc = Stdlib.open_out path in
  Stdlib.output_string oc (Buffer.contents buf);
  Stdlib.close_out oc
