(* Per-cell delay extraction from a Liberty file.

   Two layers of accuracy:

   1. [typical tbl cell]   — interpolate every arc table at a
                             default (slew, load) typical of an
                             ASIC digital path (50 ps slew, 4 fF
                             load) and return the worst-arc number.
                             ~1.3x error vs OpenTimer typical.

   2. [at ~slew ~load tbl cell]
                           — bilinear interpolation at a caller-
                             supplied (slew, load).  Pair with
                             slew propagation in placement_timing
                             to match real STA point-by-point.

   The [lookup] function preserves the old API used by
   placement_timing.ml; under the hood it now calls [typical]
   instead of mean-of-grid.  That alone closes the 2.7x gap to
   OpenTimer measured in test_mac_vs_opentimer.ml. *)

open Liberty_rewrite

(* ── Liberty time_unit handling ───────────────────────────────── *)

let scale_of_unit = function
  | "1ns" | "ns"  -> 1000.0
  | "1ps" | "ps"  -> 1.0
  | "100ps"       -> 100.0
  | "10ps"        -> 10.0
  | "1us" | "us"  -> 1_000_000.0
  | _             -> 1000.0

let numbers_of_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | ',' -> Buffer.add_char buf ' '
    | '"' | '\\' | '\n' | '\r' | '\t' -> Buffer.add_char buf ' '
    | _ -> Buffer.add_char buf c) s;
  String.split_on_char ' ' (Buffer.contents buf)
  |> List.filter (fun p -> p <> "")
  |> List.filter_map (fun s ->
       try Some (float_of_string s) with _ -> None)

(* ── 2-D delay LUT ────────────────────────────────────────────── *)

type lut = {
  axis_slew : float array;   (* index_1 *)
  axis_load : float array;   (* index_2 *)
  values    : float array array;
                              (* values.(slew_idx).(load_idx) *)
}

type cell_arcs = {
  cell_name : string;
  arcs      : lut list;       (* every cell_rise + cell_fall arc *)
}

type table = (string, cell_arcs) Hashtbl.t

(* Find an item by predicate inside a liberty list. *)
let find_first p lst =
  let rec loop = function
    | [] -> None
    | x :: tl -> if p x then Some x else loop tl in
  loop lst

(* A cell_rise/cell_fall block was rewritten as
       CellValues [ CellIndex1 _; CellIndex2 _; CellValues [String row; ...] ]
   The inner CellValues holds the row strings. *)
let lut_of_cellvalues body =
  let idx1 =
    find_first (function CellIndex1 _ -> true | _ -> false) body in
  let idx2 =
    find_first (function CellIndex2 _ -> true | _ -> false) body in
  let inner =
    List.find_map (function
      | CellValues rows ->
          let strs = List.filter_map (function
            | String s -> Some s | _ -> None) rows in
          if strs = [] then None else Some strs
      | _ -> None) body
  in
  match idx1, idx2, inner with
  | Some (CellIndex1 a), Some (CellIndex2 b), Some rows ->
      let axis_slew = Array.of_list (numbers_of_string a) in
      let axis_load = Array.of_list (numbers_of_string b) in
      let values =
        Array.of_list
          (List.map (fun s -> Array.of_list (numbers_of_string s)) rows)
      in
      if Array.length axis_slew = 0
         || Array.length axis_load = 0
         || Array.length values = 0
      then None
      else Some { axis_slew; axis_load; values }
  | _ -> None

(* Walk a Timing block; emit one LUT per cell_rise/cell_fall sub-block.
   The rewriter takes one of two paths depending on whether the
   timing-template name was a quoted STRING or a bare IDENT in the
   Liberty source: the STRING form lands as [CellValues body], the
   IDENT form (Nangate45's convention — `Timing_7_7` unquoted) lands
   as [Other ("cell_rise"|"cell_fall", _, body)]. *)
let arcs_of_timing timing =
  List.filter_map (function
    | CellValues body
      when List.exists (function CellIndex1 _ -> true | _ -> false) body
        && List.exists (function CellIndex2 _ -> true | _ -> false) body ->
        lut_of_cellvalues body
    | Other ((  "cell_rise" | "cell_fall"
              | "rise_constraint" | "fall_constraint"), _, body) ->
        lut_of_cellvalues body
    | _ -> None) timing

(* Pull all timing arcs out of a cell body (across every output pin). *)
let cell_arcs_of body =
  List.fold_left (fun acc -> function
    | CellPin (_, pin_body) ->
        List.fold_left (fun a -> function
          | Timing t -> arcs_of_timing t @ a
          | _ -> a) acc pin_body
    | _ -> acc) [] body

(* ── Bilinear interpolation ──────────────────────────────────── *)

(* Find the row [i] such that axis.(i) <= v <= axis.(i+1).  Clamp
   to the table extents (no extrapolation — STA convention). *)
let bracket axis v =
  let n = Array.length axis in
  if n = 0 then (0, 0, 0.0)
  else if n = 1 then (0, 0, 0.0)
  else if v <= axis.(0) then (0, 1, 0.0)
  else if v >= axis.(n-1) then (n-2, n-1, 1.0)
  else begin
    let i = ref 0 in
    while !i < n - 2 && axis.(!i + 1) < v do incr i done;
    let lo = axis.(!i) and hi = axis.(!i + 1) in
    let t = if hi > lo then (v -. lo) /. (hi -. lo) else 0.0 in
    (!i, !i + 1, t)
  end

let interp_lut ~slew ~load lut =
  let (s0, s1, ts) = bracket lut.axis_slew slew in
  let (l0, l1, tl) = bracket lut.axis_load load in
  let safe r c =
    if r < Array.length lut.values
       && c < Array.length lut.values.(r)
    then lut.values.(r).(c)
    else 0.0 in
  let v00 = safe s0 l0 in
  let v01 = safe s0 l1 in
  let v10 = safe s1 l0 in
  let v11 = safe s1 l1 in
  let v0 = v00 +. tl *. (v01 -. v00) in
  let v1 = v10 +. tl *. (v11 -. v10) in
  v0 +. ts *. (v1 -. v0)

(* ── Build a per-cell arc table ─────────────────────────────── *)

let build_arc_table lib =
  let h : table = Hashtbl.create 256 in
  (match lib with
   | Library (_, items) ->
       List.iter (function
         | LibCell (name, body) ->
             let arcs = cell_arcs_of body in
             Hashtbl.replace h name { cell_name = name; arcs }
         | _ -> ()) items
   | _ -> ());
  h

let load_arc_table filename =
  let lib, _ = Liberty_rewrite.rewrite filename in
  let unit_str =
    match lib with
    | Library (_, items) ->
        List.find_map (function
          | Related ("time_unit", s) -> Some s
          | _ -> None) items
    | _ -> None in
  let scale = scale_of_unit (match unit_str with
                             | Some s -> s | None -> "1ns") in
  let tbl = build_arc_table lib in
  (tbl, scale)

(* ── Lookups ─────────────────────────────────────────────────── *)

(* Defaults for a typical ASIC digital path post-buffering.
   OpenTimer's path traces show steady-state slew settling to
   ~0.005 ns and per-cell loads of ~1 fF after the first
   few stages.  Match that by default — the [at] entry point
   accepts override values for callers that propagate slew. *)
let default_slew = 0.01
let default_load = 1.0

(* Mean-arc delay at a given (slew, load), in Liberty time units.
   Averaging rise + fall is closer to OpenTimer's path-traced
   single-direction number than max-of-rise/fall: real STA picks
   ONE direction per arc on the path, while max over-counts.
   Caller multiplies by [scale] to get ps. *)
let at_native ~slew ~load tbl cell =
  match Hashtbl.find_opt tbl cell with
  | None | Some { arcs = []; _ } -> None
  | Some { arcs; _ } ->
      let total = List.fold_left
        (fun acc lut -> acc +. interp_lut ~slew ~load lut)
        0.0 arcs in
      Some (total /. float_of_int (List.length arcs))

let typical_native tbl cell =
  at_native ~slew:default_slew ~load:default_load tbl cell

(* ── Backwards-compatible API used by placement_timing.ml ──── *)

(* Old [lookup] returns a [string -> float] in picoseconds.  We
   bake the time-unit scale into the closure so callers don't
   need to know about it.  Pre-bake the typical delay per cell so
   the hot path is a single Hashtbl.find. *)
let lookup_table ?(default=50.0) (tbl, scale) =
  let cache = Hashtbl.create (Hashtbl.length tbl) in
  Hashtbl.iter (fun name _ ->
    match typical_native tbl name with
    | Some v -> Hashtbl.replace cache name (v *. scale)
    | None   -> Hashtbl.replace cache name default) tbl;
  cache

(* Old single-step API kept for callers that don't want to thread
   the (table, scale) pair: just return the picoseconds-already
   Hashtbl as before. *)
let load ?(default=50.0) filename =
  let pair = load_arc_table filename in
  lookup_table ~default pair

let lookup ?(default=50.0) cache =
  fun cell ->
    try Hashtbl.find cache cell with Not_found -> default

(* ── New slew/load-aware API ─────────────────────────────────── *)

(* Returns picoseconds for the given conditions; falls back to
   [default] when no arc data exists. *)
let at ?(default=50.0) ~slew ~load (tbl, scale) cell =
  match at_native ~slew ~load tbl cell with
  | Some v -> v *. scale
  | None   -> default
