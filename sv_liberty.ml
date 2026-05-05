(* Liberty file parser for gate mapping *)
(* Simplified version for SystemVerilog decompiler *)

(* Liberty cell representation *)
type pin_direction = Input | Output | Inout | Internal
type pin_info = {
  name: string;
  direction: pin_direction;
  function_expr: string option;
  is_clock: bool;
}

(* FF/latch state info, captured from `ff(IQ, IQN) { … }` /
 * `latch(…) { … }` blocks in simcells.lib-style libraries. The
 * pin functions of clocked outputs typically reference the iq /
 * iqn names (e.g. `function: "IQ"` on pin Q), so we keep both. *)
type ff_info = {
  iq_name: string;       (* internal Q name, e.g. "IQ" *)
  iqn_name: string;      (* internal Qbar, e.g. "IQN"; "" if absent *)
  clocked_on: string;    (* Liberty function expr for clock, e.g. "C" or "!C" *)
  next_state: string;    (* Liberty function expr for D, e.g. "D" *)
  clear: string option;  (* async-clear function (forces Q→0) *)
  preset: string option; (* async-preset function (forces Q→1) *)
}

type cell_info = {
  cell_name: string;
  pins: pin_info list;
  cell_type: string;  (* "combinational", "ff", "latch", etc *)
  ff: ff_info option;
}

type library_info = {
  lib_name: string;
  cells: (string, cell_info) Hashtbl.t;
}

(* Helper functions *)
let strip_quotes s =
  let len = String.length s in
  if len >= 2 && s.[0] = '"' && s.[len-1] = '"' then
    String.sub s 1 (len - 2)
  else s

let parse_direction = function
  | "input" -> Input
  | "output" -> Output
  | "inout" -> Inout
  | "internal" -> Internal
  | s -> failwith ("Unknown direction: " ^ s)

(* The hand-rolled tokenizer + recursive-descent parser that previously
 * lived here was replaced by the menhir-based liberty.mly /
 * liberty_lex.mll / liberty_rewrite.ml ported from hardcaml-lua. The
 * adapter from Liberty_rewrite.liberty into our cell_info / pin_info /
 * ff_info records lives below in `parse_liberty_file`. *)

(* Adapter from Liberty_rewrite.liberty (the typed Liberty AST produced
 * by the menhir parser ported from hardcaml-lua) into our existing
 * cell_info / pin_info / ff_info records. The menhir parser handles
 * the full Liberty grammar (Synopsys-style groups, define statements,
 * lu_table_template, operating_conditions, etc.) — replaces the
 * hand-rolled tokenizer + recursive-descent that kept tripping on
 * Nangate's richer constructs. *)

let pin_of_cellpin name attrs =
  let dir = ref Internal in
  let func = ref None in
  let is_clk = ref false in
  List.iter (function
    | Liberty_rewrite.Direction d -> dir := parse_direction d
    | Liberty_rewrite.Function f -> func := Some f
    | Liberty_rewrite.Related ("clock", "true") -> is_clk := true
    | _ -> ()
  ) attrs;
  { name; direction = !dir; function_expr = !func; is_clock = !is_clk }

let ff_of_block oplst body =
  let iq, iqn =
    match oplst with
    | [Liberty_rewrite.String iq; Liberty_rewrite.String iqn] -> (iq, iqn)
    | [Liberty_rewrite.String iq] -> (iq, "")
    | _ -> ("IQ", "IQN")
  in
  let clocked_on = ref "" in
  let next_state = ref "" in
  let clear      = ref None in
  let preset     = ref None in
  List.iter (function
    | Liberty_rewrite.Related ("clocked_on", v) -> clocked_on := v
    | Liberty_rewrite.Related ("next_state", v) -> next_state := v
    | Liberty_rewrite.Related ("clear",      v) -> clear  := Some v
    | Liberty_rewrite.Related ("preset",     v) -> preset := Some v
    | _ -> ()
  ) body;
  { iq_name = iq; iqn_name = iqn;
    clocked_on = !clocked_on; next_state = !next_state;
    clear = !clear; preset = !preset }

let cell_of_libcell name body =
  let pins = ref [] in
  let ff = ref None in
  let kind = ref "combinational" in
  List.iter (function
    | Liberty_rewrite.CellPin (n, attrs) ->
        pins := pin_of_cellpin n attrs :: !pins
    | Liberty_rewrite.FlipFlop (oplst, body) ->
        kind := "ff"; ff := Some (ff_of_block oplst body)
    | Liberty_rewrite.Latch (oplst, body) ->
        kind := "latch"; ff := Some (ff_of_block oplst body)
    | _ -> ()
  ) body;
  { cell_name = name;
    pins = List.rev !pins;
    cell_type = !kind;
    ff = !ff }

let parse_liberty_file filename =
  let (lib, _hash) = Liberty_rewrite.rewrite filename in
  match lib with
  | Liberty_rewrite.Library (lib_name, items) ->
      let cells = Hashtbl.create 256 in
      List.iter (function
        | Liberty_rewrite.LibCell (n, body) ->
            Hashtbl.replace cells n (cell_of_libcell n body)
        | _ -> ()
      ) items;
      { lib_name; cells }
  | _ -> failwith ("Liberty top is not a Library: " ^ filename)

(* Query functions *)
let get_cell lib cell_name =
  try Some (Hashtbl.find lib.cells cell_name)
  with Not_found -> None

let get_cell_pins lib cell_name =
  match get_cell lib cell_name with
  | Some cell -> cell.pins
  | None -> []

let get_cell_function lib cell_name pin_name =
  match get_cell lib cell_name with
  | Some cell ->
      (try
        let pin = List.find (fun p -> p.name = pin_name) cell.pins in
        pin.function_expr
      with Not_found -> None)
  | None -> None

let is_flip_flop lib cell_name =
  match get_cell lib cell_name with
  | Some cell -> cell.cell_type = "ff"
  | None -> false

let is_latch lib cell_name =
  match get_cell lib cell_name with
  | Some cell -> cell.cell_type = "latch"
  | None -> false

(* Pretty printing *)
let string_of_direction = function
  | Input -> "input"
  | Output -> "output"
  | Inout -> "inout"
  | Internal -> "internal"

let print_cell_info cell =
  Printf.printf "Cell: %s (type: %s)\n" cell.cell_name cell.cell_type;
  List.iter (fun pin ->
    Printf.printf "  Pin: %s (%s)" pin.name (string_of_direction pin.direction);
    (match pin.function_expr with
     | Some f -> Printf.printf " function: %s" f
     | None -> ());
    Printf.printf "\n"
  ) cell.pins

let print_library_summary lib =
  Printf.printf "Library: %s\n" lib.lib_name;
  Printf.printf "Cells: %d\n" (Hashtbl.length lib.cells);
  Hashtbl.iter (fun name cell ->
    print_cell_info cell
  ) lib.cells

(* ──────────────────────────────────────────────────────────────────────
 * Liberty function-expression parser.
 *
 * Grammar (loose, matches simcells.lib + most ASIC libs):
 *   expr   := orterm
 *   orterm := xorterm ('+' xorterm)* | xorterm ('|' xorterm)*
 *   xorterm:= andterm ('^' andterm)*
 *   andterm:= notterm (('*' | '&' | <space>) notterm)*
 *   notterm:= atom ("'")*           — postfix NOT
 *   atom   := '(' expr ')' | '!' atom | '~' atom
 *           | IDENT | '0' | '1'
 *
 * Note: Liberty allows whitespace as implicit AND ("A B C" = A&B&C).
 * The output is Behavioral_ir.bexpr at width 1; downstream callers
 * substitute identifiers with the actual wire names.
 *
 * Identifier resolution: when `input_map` carries (formal, bexpr)
 * entries, IDENT lookups produce that bexpr; otherwise the IDENT
 * survives as `BVar formal` for the caller to handle (FF state pins
 * like IQ/IQN do this — they refer to internal cell state, not a
 * port). *)
let parse_function_to_bexpr (input_map : (string * Behavioral_ir.bexpr) list)
                            (s : string) : Behavioral_ir.bexpr =
  let len = String.length s in
  let pos = ref 0 in
  let bool1 = Behavioral_ir.BInt { width = 1; signed = Unsigned } in
  let one  = Behavioral_ir.BConst { value = 1; width = 1 } in
  let zero = Behavioral_ir.BConst { value = 0; width = 1 } in
  let mk_and a b = Behavioral_ir.BBinOp { op = BAnd; lhs = a; rhs = b;
                                          result_type = bool1 } in
  let mk_or  a b = Behavioral_ir.BBinOp { op = BOr;  lhs = a; rhs = b;
                                          result_type = bool1 } in
  let mk_xor a b = Behavioral_ir.BBinOp { op = BXor; lhs = a; rhs = b;
                                          result_type = bool1 } in
  let mk_not a   = Behavioral_ir.BUnOp  { op = BNot; operand = a;
                                          result_type = bool1 } in

  let skip_ws () =
    while !pos < len &&
          (let c = s.[!pos] in
           c = ' ' || c = '\t' || c = '\n' || c = '\r')
    do incr pos done
  in

  let peek () = if !pos < len then Some s.[!pos] else None in

  let lookup name =
    try List.assoc name input_map
    with Not_found -> Behavioral_ir.BVar name
  in

  let rec parse_atom () =
    skip_ws ();
    match peek () with
    | None -> zero
    | Some '(' ->
        incr pos;
        let e = parse_or () in
        skip_ws ();
        (match peek () with
         | Some ')' -> incr pos
         | _ -> ());
        apply_postfix_not e
    | Some '!' | Some '~' ->
        incr pos;
        let e = parse_atom () in
        mk_not e
    | Some c when c = '0' ->
        incr pos; apply_postfix_not zero
    | Some c when c = '1' ->
        incr pos; apply_postfix_not one
    | Some c when (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
               || c = '_' ->
        let start = !pos in
        while !pos < len &&
              (let c = s.[!pos] in
               (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
               (c >= '0' && c <= '9') || c = '_')
        do incr pos done;
        let name = String.sub s start (!pos - start) in
        apply_postfix_not (lookup name)
    | Some _ ->
        (* Unrecognised char — skip and try to recover. Liberty has a
         * few non-standard chars (e.g. backslashes around names) we
         * don't model yet. *)
        incr pos; parse_atom ()

  and apply_postfix_not e =
    skip_ws ();
    match peek () with
    | Some '\'' -> incr pos; apply_postfix_not (mk_not e)
    | _ -> e

  and parse_and () =
    let left = ref (parse_atom ()) in
    let rec loop () =
      skip_ws ();
      match peek () with
      | Some '*' | Some '&' ->
          incr pos;
          left := mk_and !left (parse_atom ());
          loop ()
      | Some c when (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
                 || c = '_' || c = '(' || c = '!' || c = '~'
                 || c = '0' || c = '1' ->
          (* Implicit AND via juxtaposition: "A B" ≡ "A*B" *)
          left := mk_and !left (parse_atom ());
          loop ()
      | _ -> ()
    in
    loop (); !left

  and parse_xor () =
    let left = ref (parse_and ()) in
    let rec loop () =
      skip_ws ();
      match peek () with
      | Some '^' ->
          incr pos;
          left := mk_xor !left (parse_and ());
          loop ()
      | _ -> ()
    in
    loop (); !left

  and parse_or () =
    let left = ref (parse_xor ()) in
    let rec loop () =
      skip_ws ();
      match peek () with
      | Some '+' | Some '|' ->
          incr pos;
          left := mk_or !left (parse_xor ());
          loop ()
      | _ -> ()
    in
    loop (); !left
  in
  parse_or ()

(* Convenience: parse with the pin→bexpr map carried as
 * (string * string) — common when callers only have wire names. *)
let parse_function_with_names
      (input_names : (string * string) list)
      (s : string) : Behavioral_ir.bexpr =
  let m = List.map (fun (k, v) -> (k, Behavioral_ir.BVar v)) input_names in
  parse_function_to_bexpr m s
