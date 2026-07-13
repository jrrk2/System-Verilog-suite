(* Read a nextpnr-xilinx "--write" routed JSON (post-pack/place/route) and
 * reconstruct a UNISIM-primitive bmodule Netlist for functional xsim.
 *
 * The inverse of bir_to_nextpnr_json, but it must UN-PACK nextpnr's internal
 * cell types (SLICE_LUTX / SLICE_FFX / SELMUX2_1 / IOB18_*BUF / CARRY4 /
 * RAMB18E1_RAMB18E1 / BUFGCTRL / PSEUDO_VCC|GND / PAD) back to the original
 * Xilinx primitives.  nextpnr stashes everything we need on each cell:
 *   - attribute "X_ORIG_TYPE"          : the UNISIM type to re-instantiate
 *   - attribute "X_ORIG_PORT_<packed>" : the UNISIM pin that packed pin maps to
 *                                        (may carry a [i] bus index)
 *   - the cell's "parameters"          : the POST-LAYOUT INIT etc.
 *   - the cell's "connections"         : the POST-ROUTE nets
 * so the reconstructed netlist reproduces nextpnr's logical view exactly —
 * including any LUT pin-permutation / INIT rewrite.  Pairs with
 * Bir_to_verilog_netlist.write_verilog to get Verilog xsim can run against
 * unisims_ver.  Exposed to Lua as svd.read_nextpnr_json. *)

open Behavioral_ir
module J = Yojson.Safe
module U = Yojson.Safe.Util

(* low [n] bits of an MSB-first binary string (left-pad with 0 if short) *)
let resize_bin (s : string) (n : int) : string =
  let len = String.length s in
  if len = n then s
  else if len > n then String.sub s (len - n) n
  else String.make (n - len) '0' ^ s

let lut_init_width = function
  | "LUT1" -> Some 2  | "LUT2" -> Some 4  | "LUT3" -> Some 8
  | "LUT4" -> Some 16 | "LUT5" -> Some 32 | "LUT6" -> Some 64
  | _ -> None

(* split an orig-pin name "DOADO[11]" -> ("DOADO", Some 11); "I0" -> ("I0",None) *)
let split_pin (p : string) : string * int option =
  match String.index_opt p '[' with
  | None -> (p, None)
  | Some i ->
      let base = String.sub p 0 i in
      let j = try String.index_from p i ']' with Not_found -> String.length p in
      let idx = try int_of_string (String.sub p (i+1) (j-i-1)) with _ -> 0 in
      (base, Some idx)

let skip_type = function
  | "PSEUDO_VCC" | "PSEUDO_GND" | "PAD" | "GND" | "VCC" -> true
  | _ -> false

let read_netlist (path : string)
    : string * bmodule * (string * library_port list) list =
  let j = J.from_file path in
  let mod_name, mj = List.hd (j |> U.member "modules" |> U.to_assoc) in
  let cells    = mj |> U.member "cells"    |> U.to_assoc in
  let netnames = try mj |> U.member "netnames" |> U.to_assoc with _ -> [] in
  let ports    = try mj |> U.member "ports"    |> U.to_assoc with _ -> [] in

  (* bit-id -> (canonical signal base, index); width table.  Seed from ports
     first so a port's name wins as the canonical name for shared ids. *)
  let id2ref : (int, string * int) Hashtbl.t = Hashtbl.create 16384 in
  let width  : (string, int) Hashtbl.t = Hashtbl.create 8192 in
  let bump nm w = let c = try Hashtbl.find width nm with Not_found -> 0 in
                  if w > c then Hashtbl.replace width nm w in
  let add_net nm bits =
    List.iteri (fun idx b -> match b with
      | `Int id -> if not (Hashtbl.mem id2ref id) then
                     Hashtbl.replace id2ref id (nm, idx)
      | _ -> ()) bits;
    bump nm (List.length bits) in
  List.iter (fun (nm,pj) -> add_net nm (pj |> U.member "bits" |> U.to_list)) ports;
  List.iter (fun (nm,nj) -> add_net nm (nj |> U.member "bits" |> U.to_list)) netnames;

  (* constant nets: outputs of PSEUDO_VCC / PSEUDO_GND tie cells *)
  let vcc = Hashtbl.create 16 and gnd = Hashtbl.create 16 in
  List.iter (fun (_inst, cj) ->
    let t = cj |> U.member "type" |> U.to_string in
    let conns = try cj |> U.member "connections" |> U.to_assoc with _ -> [] in
    let mark tbl =
      List.iter (fun (_pin, bj) ->
        List.iter (function `Int id -> Hashtbl.replace tbl id () | _ -> ())
          (U.to_list bj)) conns in
    (match t with "PSEUDO_VCC" -> mark vcc | "PSEUDO_GND" -> mark gnd | _ -> ())
  ) cells;

  let attr cj k = try Some (cj |> U.member "attributes" |> U.member k |> U.to_string)
                  with _ -> None in

  (* SVS_PROMOTE: substrings of net names to expose as top-level OUTPUTs for
     xsim observation.  A promoted net often shares its name with the cell
     instance that drives it (register Q), so we rename any colliding
     instance to keep Verilog identifiers unique. *)
  let promote = match Sys.getenv_opt "SVS_PROMOTE" with
    | None -> []
    | Some s -> List.filter (fun x -> x <> "") (String.split_on_char ',' s) in
  let contains hay needle =
    let lh = String.length hay and ln = String.length needle in
    let rec go i = i + ln <= lh && (String.sub hay i ln = needle || go (i+1)) in
    ln = 0 || go 0 in
  let is_promoted nm = List.exists (fun p -> contains nm p) promote in

  (* SVS_CONST_NETS: instead of inlining VCC/GND sinks as BConst, route them
     through real driven nets (svs_vcc/svs_gnd) fed by VCC/GND UNISIM prims.
     Any const sink the JSON DOESN'T actually connect then shows as undriven
     (X) in xsim instead of being silently forced -> tests const-net
     logical completeness of nextpnr's routeVcc result. *)
  let const_nets = Sys.getenv_opt "SVS_CONST_NETS" <> None in
  if const_nets then (bump "svs_vcc" 1; bump "svs_gnd" 1);
  let ref_of_bit = function
    | `String "1" -> if const_nets then BVar "svs_vcc" else BConst { value = 1; width = 1 }
    | `String "0" -> if const_nets then BVar "svs_gnd" else BConst { value = 0; width = 1 }
    | `Int id ->
        if Hashtbl.mem vcc id then (if const_nets then BVar "svs_vcc" else BConst { value = 1; width = 1 })
        else if Hashtbl.mem gnd id then (if const_nets then BVar "svs_gnd" else BConst { value = 0; width = 1 })
        else (match Hashtbl.find_opt id2ref id with
              | Some (nm, idx) ->
                  BSelect { array = BVar nm; index = BConst { value = idx; width = 32 } }
              | None ->
                  let nm = Printf.sprintf "unnamed_%d" id in
                  bump nm 1;
                  BSelect { array = BVar nm; index = BConst { value = 0; width = 32 } })
    | _ -> BConst { value = 0; width = 1 } in

  (* build one binstance per (non-skipped) cell *)
  let instances = List.filter_map (fun (inst, cj) ->
    let ptype = cj |> U.member "type" |> U.to_string in
    if skip_type ptype then None else begin
      let otype = match attr cj "X_ORIG_TYPE" with Some t -> t | None -> ptype in
      let conns = try cj |> U.member "connections" |> U.to_assoc with _ -> [] in
      (* collect (orig_base, idx_opt, ref) for every packed pin that carries an
         X_ORIG_PORT mapping; drop physical-only pins (routethru taps). *)
      let scalar = Hashtbl.create 32 in           (* base -> ref *)
      let buses  : (string, (int * bexpr) list ref) Hashtbl.t = Hashtbl.create 16 in
      List.iter (fun (ppin, bj) ->
        match attr cj ("X_ORIG_PORT_" ^ ppin) with
        | None -> ()                              (* not a UNISIM pin: skip *)
        | Some opin ->
            (* A disconnected pin carries nextpnr's const-FF-control meaning:
               nextpnr strips an always-enabled CE (=VCC) and an inactive
               SR/R (=GND).  So an empty CE must default to 1 (enabled), every
               other empty control pin to 0 — defaulting CE to 0 froze the
               clkdiv2 divider FF (CE tied low => no toggle => dead cpu_clk). *)
            let r = match U.to_list bj with
              | b :: _ -> ref_of_bit b
              | [] -> BConst { value = (if opin = "CE" then 1 else 0); width = 1 } in
            let base, idx = split_pin opin in
            (match idx with
             | None -> Hashtbl.replace scalar base r
             | Some i ->
                 let l = try Hashtbl.find buses base
                         with Not_found -> let l = ref [] in Hashtbl.add buses base l; l in
                 l := (i, r) :: !l)
      ) conns;
      let port_connections =
        Hashtbl.fold (fun b r acc -> (b, r) :: acc) scalar []
        @ Hashtbl.fold (fun b l acc ->
            let bits = List.sort (fun (a,_) (b,_) -> compare b a) !l (* MSB-first *) in
            (b, BConcat (List.map snd bits)) :: acc) buses [] in
      (* parameters: keep only those valid on the reconstructed UNISIM prim,
         dropping nextpnr-internal attrs (XILINX_TRANSFORM_PINMAP, the
         BUFGCTRL IS_*_INVERTED left on a BUFG, CARRY4 OPT_*, …) that xelab
         rejects.  Resize LUT INIT to the primitive's 2^k width. *)
      let global_drop = [ "XILINX_TRANSFORM_PINMAP"; "XILINX_LEGACY_PRIM";
                          "BOX_TYPE"; "OPT_MODIFIED"; "ADDER_THRESHOLD" ] in
      let keep_param k =
        if List.mem k global_drop then false
        else match otype with
          | _ when lut_init_width otype <> None -> k = "INIT"
          | "FDRE" | "FDSE" | "FDPE" | "FDCE" -> k = "INIT"
          | "BSCANE2" -> k = "JTAG_CHAIN" || k = "DISABLE_JTAG"
          | "BUFG" | "IBUF" | "OBUF" | "IBUFDS"
          | "MUXF7" | "MUXF8" | "CARRY4" -> false
          | _ -> true   (* RAMB18E1 etc.: keep the rest *)
      in
      let params = try cj |> U.member "parameters" |> U.to_assoc with _ -> [] in
      let param_strs = List.filter_map (fun (k, vj) ->
        if not (keep_param k) then None else
        let v = U.to_string vj in
        let v = match lut_init_width otype with
          | Some w when k = "INIT" -> resize_bin v w
          | _ -> v in
        Some (k, v)) params in
      let inst_name = if is_promoted inst then inst ^ "_ri" else inst in
      Some { inst_name; module_name = otype;
             param_values = []; param_strs; port_connections }
    end
  ) cells in
  let instances =
    if const_nets then
      { inst_name = "svs_vcc_drv"; module_name = "VCC"; param_values = [];
        param_strs = []; port_connections = [ ("P", BVar "svs_vcc") ] } ::
      { inst_name = "svs_gnd_drv"; module_name = "GND"; param_values = [];
        param_strs = []; port_connections = [ ("G", BVar "svs_gnd") ] } ::
      instances
    else instances in

  (* signals: one per canonical base in the width table; direction from ports *)
  let pdir = Hashtbl.create 64 in
  List.iter (fun (nm, pj) ->
    let d = match pj |> U.member "direction" |> U.to_string with
      | "input" -> `Input | "output" -> `Output | _ -> `Input in
    Hashtbl.replace pdir nm d) ports;
  let signals = Hashtbl.fold (fun nm w acc ->
    let direction = match Hashtbl.find_opt pdir nm with
      | Some d -> d
      | None -> if is_promoted nm then `Output else `Internal in
    { name = nm;
      stype = (if w <= 1 then BBool else BInt { width = w; signed = Unsigned });
      direction; initial_value = None; attrs = [] } :: acc
  ) width [] in

  let m = { name = mod_name; params = []; signals; processes = [];
            instances; funcs = []; mems = []; attrs = [] } in
  (mod_name, m, [])
