(* Per-cell typical delay extraction from a Liberty file.

   Walks the parsed Liberty AST (Liberty_rewrite.liberty), finds
   each LibCell's output-pin Timing arcs, and returns a single
   "typical delay" number per cell — the mean of all numeric
   entries across cell_rise/cell_fall tables.  Picoseconds out,
   converted from the Liberty time_unit (we read the unit from
   the library so a `time_unit : "1ns"` library reports ps
   correctly).

   This is a deliberate simplification: real STA picks a delay
   based on input slew + load capacitance from the index_1 /
   index_2 arrays.  For a first-order placement-aware critical
   path this single number is enough to make the model
   relative-correct (cell A faster than cell B), and the wire
   delay still flexes with placement coords.

   Returns a [Hashtbl] keyed by cell name.  Cells with no timing
   arcs (FFs without explicit setup arcs, fillers, tap cells)
   get the [default] value — pass 0. for "ignore them on the
   path" or 50. for the placeholder we use elsewhere. *)

open Liberty_rewrite

(* Default time scale: ns → ps multiplier when [time_unit] in the
   library header is "1ns".  Liberty also allows ps, fs, etc.;
   we recognise the common ones. *)
let scale_of_unit = function
  | "1ns" | "ns"  -> 1000.0     (* values in ns, want ps *)
  | "1ps" | "ps"  -> 1.0
  | "100ps"       -> 100.0
  | "10ps"        -> 10.0
  | "1us" | "us"  -> 1_000_000.0
  | _             -> 1000.0     (* assume ns; matches NanGate *)

(* Pull a comma-separated number list out of a Liberty String
   constructor (the rewriter folds the values-of-a-row into a
   single String like "0.02018,0.02359,...").  Whitespace and
   trailing back-slash continuations are tolerated. *)
let numbers_of_string s =
  let buf = Buffer.create (String.length s) in
  String.iter (fun c ->
    match c with
    | ',' -> Buffer.add_char buf ' '
    | '"' | '\\' | '\n' | '\r' | '\t' -> Buffer.add_char buf ' '
    | _ -> Buffer.add_char buf c) s;
  let parts =
    String.split_on_char ' ' (Buffer.contents buf)
    |> List.filter (fun p -> p <> "")
  in
  List.filter_map (fun s ->
    try Some (float_of_string s) with _ -> None) parts

(* Walk a Timing block, accumulate every numeric value found in
   any CellValues.  This blends rise + fall + every (slew, load)
   entry.  Mean of the bag is the cell's typical delay. *)
let rec walk_for_values acc = function
  | [] -> acc
  | CellValues lst :: tl ->
      let nums = List.fold_left (fun a x ->
        match x with
        | String s -> a @ numbers_of_string s
        | _ -> a) [] lst in
      walk_for_values (nums @ acc) tl
  | Transition (_, _, body) :: tl ->
      walk_for_values (walk_for_values acc [body]) tl
  | Other (_, _, body) :: tl ->
      walk_for_values (walk_for_values acc body) tl
  | _ :: tl -> walk_for_values acc tl

let cell_typical_ns body =
  let timings =
    List.filter_map (function
      | CellPin (_, pin_body) ->
          let ts = List.filter_map (function
            | Timing t -> Some t
            | _ -> None) pin_body in
          if ts = [] then None else Some (List.flatten ts)
      | _ -> None) body
  in
  let all_values = List.fold_left walk_for_values [] timings in
  match all_values with
  | [] -> None
  | _  ->
      let n = List.length all_values in
      let s = List.fold_left (+.) 0.0 all_values in
      Some (s /. float_of_int n)

(* Build the per-cell typical-delay table from a parsed library.
   [unit_str] is the library's time_unit ("1ns" etc.); when
   [None] the function picks NanGate's default of ns. *)
let build ?(unit_str=None) ?(default=50.0) lib =
  let scale = scale_of_unit (match unit_str with
    | Some s -> s | None -> "1ns") in
  let h = Hashtbl.create 256 in
  (match lib with
   | Library (_, items) ->
       List.iter (function
         | LibCell (name, body) ->
             (match cell_typical_ns body with
              | Some ns -> Hashtbl.replace h name (ns *. scale)
              | None    -> Hashtbl.replace h name default)
         | _ -> ()) items
   | _ -> ());
  h

(* Convenience: parse the file then build. *)
let load ?(default=50.0) filename =
  let lib, _ = Liberty_rewrite.rewrite filename in
  (* try to read the time_unit out of the top-level body *)
  let unit_str =
    match lib with
    | Library (_, items) ->
        List.find_map (function
          | Related ("time_unit", s) -> Some s
          | _ -> None) items
    | _ -> None
  in
  build ~unit_str ~default lib

(* Hashtbl-as-function for plugging straight into
   Placement_timing.report. *)
let lookup ?(default=50.0) tbl =
  fun cell ->
    try Hashtbl.find tbl cell with Not_found -> default
