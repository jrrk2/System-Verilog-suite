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
 * miter cuts every FF, exposing its Q as a primary input matched BY NAME.  The
 * source has VECTOR registers (`prbs`,`johnson`); the netlist has one scalar FF
 * per bit named `<base>_reg_<i>_`.  We GROUP those back into vector registers
 * (one BSequential assigning the whole vector) and rewrite reads of FF-Q nets
 * into vector slices, so the state interfaces correspond.
 *
 * PHYSICAL ROUTING CHECK (routing completeness — the bypass-FFMUX defect).
 * nextpnr's `--write` JSON DOES carry routing: each netname has a `ROUTING`
 * attribute listing wires and `src->dst` PIP/SITEPIP arcs.  A flip-flop's
 * D-input sitewire is `SITEWIRE/<site>/<slot>[5]FFMUX_OUT` (derived from its
 * NEXTPNR_BEL).  If the net feeding that D never reaches that sitewire (the
 * FFMUX route-thru was skipped), the D pin physically floats.  We detect this
 * per FF and drive the offending D from a FRESH UNDRIVEN signal (a Z3 free
 * variable) instead of its logical source — so the miter returns a
 * counterexample, exactly reproducing the on-silicon defect.  On a cleanly
 * routed netlist nothing is cut and the result is unchanged.
 *
 * Still out of scope: full functional modelling of the fabric (only FF-D
 * completeness is checked here, where the known defect class lives). *)

open Behavioral_ir
module U = Yojson.Safe.Util
module SS = Set.Make (String)

let b1 = BInt { width = 1; signed = Unsigned }
let starts_with p s =
  String.length s >= String.length p && String.sub s 0 (String.length p) = p
let netname id = Printf.sprintf "n%d" id
let attr_string = function `String s -> s | `Int i -> string_of_int i | _ -> ""
let bit_id = function `Int id -> Some id | _ -> None

let parse_reg_name name =
  let re = Str.regexp "^\\(.*\\)_reg_\\([0-9]+\\)_$" in
  if Str.string_match re name 0 then
    Some (Str.matched_group 1 name, int_of_string (Str.matched_group 2 name))
  else
    let re0 = Str.regexp "^\\(.*\\)_reg_$" in
    if Str.string_match re0 name 0 then Some (Str.matched_group 1 name, 0)
    else None

let is_ff = function "FDRE" | "FDSE" | "FDCE" | "FDPE" -> true | _ -> false

(* ---------------------------------------------------------------- *)
(* ROUTING attribute: parse + reachability                          *)
(* ---------------------------------------------------------------- *)
(* tokens are ';'-separated; arcs are "<src>-><dst>", others are wire
   names or numeric strength markers.  A wire is DRIVEN iff it is the dst
   of an arc whose src is itself reachable from a root (a wire with no
   incoming arc — a BEL output). *)
let parse_routing (r : string) : (string * string) list =
  String.split_on_char ';' r
  |> List.filter_map (fun tok ->
       match Str.bounded_split_delim (Str.regexp_string "->") tok 2 with
       | [src; dst] when src <> "" && dst <> "" -> Some (String.trim src, String.trim dst)
       | _ -> None)

let driven_wires (arcs : (string * string) list) : SS.t =
  let dsts = List.fold_left (fun s (_, d) -> SS.add d s) SS.empty arcs in
  let srcs = List.fold_left (fun s (a, _) -> SS.add a s) SS.empty arcs in
  let roots = SS.diff srcs dsts in
  (* BFS from roots over arcs *)
  let adj = Hashtbl.create 64 in
  List.iter (fun (a, d) ->
    Hashtbl.replace adj a (d :: (try Hashtbl.find adj a with Not_found -> []))) arcs;
  let visited = ref SS.empty in
  let rec go w =
    if not (SS.mem w !visited) then begin
      visited := SS.add w !visited;
      List.iter go (try Hashtbl.find adj w with Not_found -> [])
    end in
  SS.iter go roots;
  (* a wire is driven if visited AND it has an incoming arc (not a root) *)
  SS.filter (fun w -> SS.mem w dsts) !visited

(* expected D-input sitewire of an FF from its NEXTPNR_BEL "SITE/<slot>[5]FF" *)
let ff_d_sitewire (bel : string) : string option =
  match String.split_on_char '/' bel with
  | [site; b] ->
      let re = Str.regexp "^\\([A-D]\\)\\(5?\\)FF$" in
      if Str.string_match re b 0 then
        Some (Printf.sprintf "SITEWIRE/%s/%s%sFFMUX_OUT"
                site (Str.matched_group 1 b) (Str.matched_group 2 b))
      else None
  | _ -> None

type ffinfo = {
  inst : string; base : string; idx : int; ty : string; init : string;
  bel : string;
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
  let netnames = (try U.member "netnames" mj |> U.to_assoc with _ -> []) in

  (* bit id -> driven-wire set of the net carrying that bit *)
  let bit_driven : (int, SS.t) Hashtbl.t = Hashtbl.create 512 in
  List.iter (fun (_nm, nj) ->
    let r = try U.member "attributes" nj |> U.member "ROUTING" |> U.to_string
            with _ -> "" in
    let dw = driven_wires (parse_routing r) in
    List.iter (fun b -> match bit_id b with
      | Some id -> Hashtbl.replace bit_driven id dw | None -> ())
      (U.member "bits" nj |> U.to_list)
  ) netnames;

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
    ltype, has_xorig, pinmap, attrs in
  let logical_conns cj =
    let ltype, has_xorig, pinmap, attrs = cell_logical cj in
    let conns = (try U.member "connections" cj |> U.to_assoc with _ -> []) in
    let lc = List.filter_map (fun (belpin, bitsj) ->
      let lp = if has_xorig then List.assoc_opt belpin pinmap else Some belpin in
      match lp with None -> None | Some p -> Some (p, bitsj)) conns in
    ltype, lc, attrs in

  (* ---- pass 1: FF cells -> regbit map + ffinfo (with routing check) ---- *)
  let regbit : (int, string * int) Hashtbl.t = Hashtbl.create 64 in
  let undriven_ff : (string, unit) Hashtbl.t = Hashtbl.create 16 in
  let ffs = List.filter_map (fun (cname, cj) ->
    let ltype, lc, attrs = logical_conns cj in
    if not (is_ff ltype) then None
    else match parse_reg_name cname with
      | None -> None
      | Some (base, idx) ->
          let one p = match List.assoc_opt p lc with
            | Some bitsj -> (match U.to_list bitsj with [b] -> Some b | _ -> None)
            | None -> None in
          (match one "Q" with
           | Some qb -> (match bit_id qb with
                         | Some qid -> Hashtbl.replace regbit qid (base, idx) | None -> ())
           | None -> ());
          let init = match (try U.member "parameters" cj |> U.member "INIT" with _ -> `Null) with
            | `String s -> s | `Int i -> string_of_int i | _ -> "0" in
          let bel = match List.assoc_opt "NEXTPNR_BEL" attrs with
            | Some v -> attr_string v | None -> "" in
          let d = one "D" in
          (* physical routing-completeness check on the D net *)
          (match ff_d_sitewire bel, d with
           | Some want, Some db ->
               (match bit_id db with
                | Some did ->
                    let dw = try Hashtbl.find bit_driven did with Not_found -> SS.empty in
                    if not (SS.mem want dw) then Hashtbl.replace undriven_ff cname ()
                | None -> ())
           | _ -> ());
          Some { inst = cname; base; idx; ty = ltype; init; bel;
                 d; ce = one "CE"; clk = one "C";
                 rs = (match one "R" with Some x -> Some x | None -> one "S") }
  ) cells in

  let ids : (int, unit) Hashtbl.t = Hashtbl.create 512 in
  let undriven_sigs = ref [] in   (* fresh free-variable nets for cut D-pins *)
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

  (* ---- pass 2: non-FF cells -> binstances ---- *)
  let instances = List.filter_map (fun (cname, cj) ->
    let ltype, lc, _ = logical_conns cj in
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

  (* ---- banked register processes ---- *)
  let bases = List.sort_uniq compare (List.map (fun f -> f.base) ffs) in
  let reg_signals = ref [] in
  let d_expr (f : ffinfo) =
    if Hashtbl.mem undriven_ff f.inst then begin
      (* physically undriven D -> fresh free variable (never assigned) *)
      let nm = "undriven_" ^ f.inst in
      undriven_sigs := { name = nm; stype = b1; direction = `Internal;
                         initial_value = None; attrs = [] } :: !undriven_sigs;
      BVar nm
    end else resolve_opt f.d in
  let reg_procs = List.map (fun base ->
    let bits = List.filter (fun f -> f.base = base) ffs in
    let width = 1 + List.fold_left (fun m f -> max m f.idx) 0 bits in
    let byidx i = List.find_opt (fun f -> f.idx = i) bits in
    let clk_name = match List.find_map (fun f -> f.clk) bits with
      | Some (`Int cid) -> Hashtbl.replace ids cid (); netname cid
      | _ -> "clk" in
    let nextbit i = match byidx i with
      | None -> BConst { value = 0; width = 1 }
      | Some f ->
          let self = BSlice { signal = BVar base; msb = i; lsb = i } in
          let setv = if f.ty = "FDSE" || f.ty = "FDPE"
                     then BConst { value = 1; width = 1 } else BConst { value = 0; width = 1 } in
          let load = BCond { condition = resolve_opt f.ce; then_val = d_expr f; else_val = self } in
          BCond { condition = resolve_opt f.rs; then_val = setv; else_val = load } in
    let parts = ref [] in
    for i = 0 to width - 1 do parts := nextbit i :: !parts done;
    let rhs = match !parts with [x] -> x | xs -> BConcat xs in
    let init_val = ref 0 in
    List.iter (fun f -> if String.trim f.init = "1" then init_val := !init_val lor (1 lsl f.idx)) bits;
    reg_signals := { name = base; stype = BInt { width; signed = Unsigned };
                     direction = `Internal;
                     initial_value = Some (BConst { value = !init_val; width }); attrs = [] }
                   :: !reg_signals;
    BSequential { name = base ^ "_reg"; clock = clk_name; clock_edge = `Pos;
                  reset = None; reset_edge = None; reset_async = false;
                  body = [ BAssign { lhs = base; rhs } ]; blocking_vars = [] }
  ) bases in

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

  if Hashtbl.length undriven_ff > 0 then
    Printf.eprintf "[nextpnr-route-check] %d FF D-pin(s) physically undriven (cut to free vars): %s\n%!"
      (Hashtbl.length undriven_ff)
      (String.concat " " (Hashtbl.fold (fun k () a -> k :: a) undriven_ff []));

  let m = { name = mname; params = [];
            signals = port_signals @ !reg_signals @ !undriven_sigs @ net_signals;
            processes = io_proc :: reg_procs; instances;
            funcs = []; mems = []; attrs = [] } in
  { modules = [m]; library_cells = [] }

(* Standalone routing-completeness report (no BIR build). *)
let route_report ?top (path : string) : string =
  let j = Yojson.Safe.from_file path in
  let modules = U.member "modules" j |> U.to_assoc in
  let _, mj = match top, modules with
    | Some t, _ -> t, List.assoc t modules
    | None, (n, v) :: _ -> n, v
    | None, [] -> failwith "nextpnr_json: no modules" in
  let cells = (try U.member "cells" mj |> U.to_assoc with _ -> []) in
  let netnames = (try U.member "netnames" mj |> U.to_assoc with _ -> []) in
  let bit_driven : (int, SS.t) Hashtbl.t = Hashtbl.create 512 in
  List.iter (fun (_nm, nj) ->
    let r = try U.member "attributes" nj |> U.member "ROUTING" |> U.to_string with _ -> "" in
    let dw = driven_wires (parse_routing r) in
    List.iter (fun b -> match bit_id b with Some id -> Hashtbl.replace bit_driven id dw | None -> ())
      (U.member "bits" nj |> U.to_list)) netnames;
  let undr = ref [] and nff = ref 0 in
  List.iter (fun (cname, cj) ->
    let attrs = (try U.member "attributes" cj |> U.to_assoc with _ -> []) in
    let ltype = match List.assoc_opt "X_ORIG_TYPE" attrs with Some v -> attr_string v
                | None -> U.member "type" cj |> U.to_string in
    if is_ff ltype then begin
      incr nff;
      let bel = match List.assoc_opt "NEXTPNR_BEL" attrs with Some v -> attr_string v | None -> "" in
      let conns = (try U.member "connections" cj |> U.to_assoc with _ -> []) in
      let pinmap = List.filter_map (fun (k, v) ->
        if starts_with "X_ORIG_PORT_" k then Some (String.sub k 12 (String.length k - 12), attr_string v)
        else None) attrs in
      let dpin = List.find_map (fun (bp, l) -> if l = "D" then Some bp else None) pinmap in
      let dbit = match dpin with
        | Some bp -> (match List.assoc_opt bp conns with
                      | Some bj -> (match U.to_list bj with [b] -> bit_id b | _ -> None) | None -> None)
        | None -> None in
      match ff_d_sitewire bel, dbit with
      | Some want, Some did ->
          let dw = try Hashtbl.find bit_driven did with Not_found -> SS.empty in
          if not (SS.mem want dw) then undr := cname :: !undr
      | _ -> ()
    end) cells;
  Printf.sprintf "FFs=%d undriven_D=%d%s" !nff (List.length !undr)
    (if !undr = [] then "" else " [" ^ String.concat " " !undr ^ "]")
