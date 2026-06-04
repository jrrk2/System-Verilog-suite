(* nextpnr_json_to_behavioral.ml — read an ACTUAL nextpnr post-place-and-route
 * netlist (yosys-style JSON, `nextpnr-xilinx --write`) into structural BIR.
 *
 * nextpnr rewrites the design into placed BEL cells (SLICE_LUTX, SLICE_FFX,
 * IOB18_*_DCIEN, BUFGCTRL, PSEUDO_GND/VCC, PAD) but preserves the original
 * primitive identity in attributes:
 *     X_ORIG_TYPE       = "LUT2" / "FDRE" / "BUFG" / "OBUF" / "IBUFDS" ...
 *     X_ORIG_PORT_<bel> = logical pin name  (A3->I0, A1->I1, CK->C, SR->R ...)
 * so we reconstruct a logical-primitive binstance per cell and feed it to the
 * existing Xil_prim_models bodies + the Z3 miter.
 *
 * REGISTER BANKING (for sequential equivalence vs the source RTL).  The Z3
 * miter (Behavioral_ffrip) cuts every FF, exposing its Q as a primary input
 * matched BY NAME across the two designs.  The source has VECTOR registers
 * (`prbs`[24:0], `johnson`[7:0]); the netlist has one scalar FF per bit named
 * `<base>_reg_<i>_`.  To make the state interfaces correspond we GROUP the FF
 * cells back into vector registers: emit a native BSequential assigning the
 * whole vector `<base>`, with bit i's next state taken from FF `<base>_reg_<i>_`,
 * and rewrite every read of an FF-Q net into the vector slice `<base>[i]`.
 *
 * NOTE ON SCOPE: this verifies the PACK/PLACE mapping (pin permutation, INIT,
 * the inserted LUT buffers) against the source.  Routing PIPs are NOT in
 * --write output, so it does NOT verify routing completeness. *)

open Behavioral_ir
module U = Yojson.Safe.Util

let b1 = BInt { width = 1; signed = Unsigned }
let starts_with p s =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p
let netname id = Printf.sprintf "n%d" id
let attr_string = function `String s -> s | `Int i -> string_of_int i | _ -> ""
let bit_id = function `Int id -> Some id | _ -> None

(* "<base>_reg_<i>_" -> (base, i);  "<base>_reg_" -> (base, 0).  None otherwise. *)
let parse_reg_name name =
  let re = Str.regexp "^\\(.*\\)_reg_\\([0-9]+\\)_$" in
  if Str.string_match re name 0 then
    Some (Str.matched_group 1 name, int_of_string (Str.matched_group 2 name))
  else
    let re0 = Str.regexp "^\\(.*\\)_reg_$" in
    if Str.string_match re0 name 0 then Some (Str.matched_group 1 name, 0)
    else None

let is_ff = function "FDRE" | "FDSE" | "FDCE" | "FDPE" -> true | _ -> false

type ffinfo = {
  base : string; idx : int; ty : string;
  init : string;
  d : Yojson.Safe.t option; ce : Yojson.Safe.t option;
  clk : Yojson.Safe.t option; rs : Yojson.Safe.t option;
}

let read_program ?top (path : string) : bprogram =
  let j = Yojson.Safe.from_file path in
  let modules = U.member "modules" j |> U.to_assoc in
  let mname, mj =
    match top, modules with
    | Some t, _ -> t, List.assoc t modules
    | None, (n, v) :: _ -> n, v
    | None, [] -> failwith "nextpnr_json: no modules" in
  let ports = (try U.member "ports" mj |> U.to_assoc with _ -> []) in
  let cells = (try U.member "cells" mj |> U.to_assoc with _ -> []) in

  (* logical type + bel->logical pin map for a cell *)
  let cell_logical cj =
    let ctype = U.member "type" cj |> U.to_string in
    let attrs = (try U.member "attributes" cj |> U.to_assoc with _ -> []) in
    let ltype = match List.assoc_opt "X_ORIG_TYPE" attrs with
      | Some v -> attr_string v | None -> ctype in
    let has_xorig = List.mem_assoc "X_ORIG_TYPE" attrs in
    let pinmap = List.filter_map (fun (k, v) ->
      if starts_with "X_ORIG_PORT_" k
      then Some (String.sub k 12 (String.length k - 12), attr_string v)
      else None) attrs in
    ltype, has_xorig, pinmap in
  (* connections remapped to logical pin names *)
  let logical_conns cj =
    let ltype, has_xorig, pinmap = cell_logical cj in
    let conns = (try U.member "connections" cj |> U.to_assoc with _ -> []) in
    let lc = List.filter_map (fun (belpin, bitsj) ->
      let lp = if has_xorig then List.assoc_opt belpin pinmap else Some belpin in
      match lp with None -> None | Some p -> Some (p, bitsj)) conns in
    ltype, lc in

  (* ---- pass 1: collect FF cells, build the FF-Q-net -> (base,idx) map ---- *)
  let regbit : (int, string * int) Hashtbl.t = Hashtbl.create 64 in
  let ffs = List.filter_map (fun (cname, cj) ->
    let ltype, lc = logical_conns cj in
    if not (is_ff ltype) then None
    else match parse_reg_name cname with
      | None -> None
      | Some (base, idx) ->
          let one p = match List.assoc_opt p lc with
            | Some bitsj -> (match U.to_list bitsj with [b] -> Some b | _ -> None)
            | None -> None in
          (match one "Q" with
           | Some qb -> (match bit_id qb with
                         | Some qid -> Hashtbl.replace regbit qid (base, idx)
                         | None -> ())
           | None -> ());
          let init = match (try U.member "parameters" cj |> U.member "INIT" with _ -> `Null) with
            | `String s -> s | `Int i -> string_of_int i | _ -> "0" in
          Some { base; idx; ty = ltype; init;
                 d = one "D"; ce = one "CE"; clk = one "C";
                 rs = (match one "R" with Some x -> Some x | None -> one "S") }
  ) cells in

  (* net-id universe (everything except FF-Q nets, which become reg slices) *)
  let ids : (int, unit) Hashtbl.t = Hashtbl.create 512 in
  let resolve_bit = function
    | `Int id ->
        (match Hashtbl.find_opt regbit id with
         | Some (base, idx) -> BSlice { signal = BVar base; msb = idx; lsb = idx }
         | None -> Hashtbl.replace ids id (); BVar (netname id))
    | `String "1" -> BConst { value = 1; width = 1 }
    | _ -> BConst { value = 0; width = 1 } in
  let resolve_opt = function Some b -> resolve_bit b | None -> BConst { value = 0; width = 1 } in
  let conn_expr bitsj =
    match U.to_list bitsj with
    | [b] -> resolve_bit b
    | bits -> BConcat (List.rev_map resolve_bit bits) in

  (* ---- pass 2: non-FF cells -> logical-primitive binstances ---- *)
  let instances = List.filter_map (fun (cname, cj) ->
    let ltype, lc = logical_conns cj in
    if ltype = "PAD" || is_ff ltype then None
    else begin
      let params = (try U.member "parameters" cj |> U.to_assoc with _ -> []) in
      let param_strs = List.filter_map (fun (k, v) -> match v with
        | `String s -> Some (k, s) | `Int i -> Some (k, string_of_int i) | _ -> None) params in
      let port_connections = List.map (fun (lp, bitsj) -> lp, conn_expr bitsj) lc in
      Some { inst_name = cname; module_name = ltype;
             param_values = []; param_strs; port_connections }
    end
  ) cells in

  (* ---- banked register processes (one BSequential per <base>) ---- *)
  let bases = List.sort_uniq compare (List.map (fun f -> f.base) ffs) in
  let reg_signals = ref [] in
  let reg_procs = List.filter_map (fun base ->
    let bits = List.filter (fun f -> f.base = base) ffs in
    let width = 1 + List.fold_left (fun m f -> max m f.idx) 0 bits in
    let byidx i = List.find_opt (fun f -> f.idx = i) bits in
    (* clock name from the first FF's C net *)
    let clk_name =
      match List.find_map (fun f -> f.clk) bits with
      | Some (`Int cid) -> Hashtbl.replace ids cid (); netname cid
      | _ -> "clk" in
    (* next-state per bit: ctrl ? setval : (CE ? D : self) *)
    let nextbit i =
      match byidx i with
      | None -> BConst { value = 0; width = 1 }       (* gap -> tie 0 *)
      | Some f ->
          let self = BSlice { signal = BVar base; msb = i; lsb = i } in
          let setv = if f.ty = "FDSE" || f.ty = "FDPE"
                     then BConst { value = 1; width = 1 }
                     else BConst { value = 0; width = 1 } in
          let load = BCond { condition = resolve_opt f.ce;
                             then_val = resolve_opt f.d; else_val = self } in
          BCond { condition = resolve_opt f.rs; then_val = setv; else_val = load }
    in
    let parts = ref [] in           (* MSB-first concat *)
    for i = 0 to width - 1 do parts := nextbit i :: !parts done;
    let rhs = match !parts with [x] -> x | xs -> BConcat xs in
    (* initial value from per-bit INIT (LSB = bit 0) *)
    let init_val = ref 0 in
    List.iter (fun f -> if String.trim f.init = "1" then init_val := !init_val lor (1 lsl f.idx)) bits;
    reg_signals := { name = base; stype = BInt { width; signed = Unsigned };
                     direction = `Internal;
                     initial_value = Some (BConst { value = !init_val; width });
                     attrs = [] } :: !reg_signals;
    Some (BSequential { name = base ^ "_reg"; clock = clk_name; clock_edge = `Pos;
                        reset = None; reset_edge = None; reset_async = false;
                        body = [ BAssign { lhs = base; rhs } ]; blocking_vars = [] })
  ) bases in

  (* port bits are nets too (unless an FF Q, which can't be a top port) *)
  List.iter (fun (_, pj) ->
    List.iter (fun b -> match bit_id b with
      | Some id when not (Hashtbl.mem regbit id) -> Hashtbl.replace ids id ()
      | _ -> ()) (U.member "bits" pj |> U.to_list)) ports;

  let net_signals =
    Hashtbl.fold (fun id () acc ->
      { name = netname id; stype = b1; direction = `Internal;
        initial_value = None; attrs = [] } :: acc) ids [] in
  let port_signals = List.map (fun (pn, pj) ->
    let dir = U.member "direction" pj |> U.to_string in
    let w = List.length (U.member "bits" pj |> U.to_list) in
    { name = pn; stype = BInt { width = w; signed = Unsigned };
      direction = (if dir = "output" then `Output else `Input);
      initial_value = None; attrs = [] }) ports in

  (* io process: bridge vector ports <-> per-bit nets / reg slices *)
  let io_body = List.concat_map (fun (pn, pj) ->
    let dir = U.member "direction" pj |> U.to_string in
    let bits = U.member "bits" pj |> U.to_list in
    if dir = "output" then
      [ BAssign { lhs = pn;
                  rhs = (match bits with [b] -> resolve_bit b
                         | _ -> BConcat (List.rev_map resolve_bit bits)) } ]
    else
      let i = ref (-1) in
      List.filter_map (fun b -> incr i;
        match bit_id b with
        | Some id -> Some (BAssign { lhs = netname id;
                            rhs = BSlice { signal = BVar pn; msb = !i; lsb = !i } })
        | None -> None) bits
  ) ports in
  let io_proc = BCombinational { name = "io"; sensitivity = [BAny]; body = io_body } in

  let m = { name = mname; params = [];
            signals = port_signals @ !reg_signals @ net_signals;
            processes = io_proc :: reg_procs; instances;
            funcs = []; mems = []; attrs = [] } in
  { modules = [m]; library_cells = [] }
