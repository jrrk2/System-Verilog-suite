(* Liberty file parser for gate mapping *)
(* Simplified version for SystemVerilog decompiler *)

open Str

(* Liberty cell representation *)
type pin_direction = Input | Output | Inout | Internal
type pin_info = {
  name: string;
  direction: pin_direction;
  function_expr: string option;
}

type cell_info = {
  cell_name: string;
  pins: pin_info list;
  cell_type: string;  (* "combinational", "ff", "latch", etc *)
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

(* Simple tokenizer *)
type token =
  | TIdent of string
  | TString of string
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TColon
  | TSemicolon
  | TComma
  | TEOF

let tokenize content =
  let tokens = ref [] in
  let pos = ref 0 in
  let len = String.length content in

  let skip_whitespace () =
    while !pos < len && (content.[!pos] = ' ' || content.[!pos] = '\t' ||
                         content.[!pos] = '\n' || content.[!pos] = '\r') do
      incr pos
    done
  in

  let skip_comment () =
    if !pos < len - 1 && content.[!pos] = '/' && content.[!pos + 1] = '*' then begin
      pos := !pos + 2;
      while !pos < len - 1 && not (content.[!pos] = '*' && content.[!pos + 1] = '/') do
        incr pos
      done;
      if !pos < len - 1 then pos := !pos + 2;
      true
    end else if !pos < len - 1 && content.[!pos] = '/' && content.[!pos + 1] = '/' then begin
      pos := !pos + 2;
      while !pos < len && content.[!pos] != '\n' do
        incr pos
      done;
      true
    end else
      false
  in

  while !pos < len do
    skip_whitespace ();
    if !pos >= len then ()
    else if skip_comment () then ()
    else match content.[!pos] with
      | '(' -> tokens := TLParen :: !tokens; incr pos
      | ')' -> tokens := TRParen :: !tokens; incr pos
      | '{' -> tokens := TLBrace :: !tokens; incr pos
      | '}' -> tokens := TRBrace :: !tokens; incr pos
      | ':' -> tokens := TColon :: !tokens; incr pos
      | ';' -> tokens := TSemicolon :: !tokens; incr pos
      | ',' -> tokens := TComma :: !tokens; incr pos
      | '"' ->
          incr pos;
          let start = !pos in
          while !pos < len && content.[!pos] != '"' do incr pos done;
          let str = String.sub content start (!pos - start) in
          tokens := TString str :: !tokens;
          if !pos < len then incr pos
      | c when (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' ->
          let start = !pos in
          while !pos < len &&
                let c = content.[!pos] in
                (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
                (c >= '0' && c <= '9') || c = '_' || c = '.' || c = '-' do
            incr pos
          done;
          let ident = String.sub content start (!pos - start) in
          tokens := TIdent ident :: !tokens
      | c when c >= '0' && c <= '9' || c = '-' || c = '+' ->
          let start = !pos in
          incr pos;
          while !pos < len &&
                let c = content.[!pos] in
                (c >= '0' && c <= '9') || c = '.' || c = 'e' || c = 'E' ||
                c = '+' || c = '-' do
            incr pos
          done;
          let num = String.sub content start (!pos - start) in
          tokens := TIdent num :: !tokens
      | _ -> incr pos
  done;
  List.rev !tokens

(* Simple recursive descent parser *)
type parse_node =
  | PNLibrary of string * parse_node list
  | PNCell of string * parse_node list
  | PNPin of string * parse_node list
  | PNAttr of string * string
  | PNFF of parse_node list
  | PNLatch of parse_node list
  | PNTiming of parse_node list
  | PNOther

let rec parse_library tokens =
  let rec skip_to_brace = function
    | TLBrace :: rest -> rest
    | _ :: rest -> skip_to_brace rest
    | [] -> []
  in

  let rec parse_body acc = function
    | TRBrace :: rest -> (List.rev acc, rest)
    | TIdent "cell" :: TLParen :: (TString name | TIdent name) :: TRParen :: TLBrace :: rest ->
        let (cell_body, rest') = parse_body [] rest in
        parse_body (PNCell (name, cell_body) :: acc) rest'
    | TIdent "pin" :: TLParen :: (TString name | TIdent name) :: TRParen :: TLBrace :: rest ->
        let (pin_body, rest') = parse_body [] rest in
        parse_body (PNPin (name, pin_body) :: acc) rest'
    | TIdent "ff" :: TLParen :: rest ->
        let rest' = skip_to_brace rest in
        let (ff_body, rest'') = parse_body [] rest' in
        parse_body (PNFF ff_body :: acc) rest''
    | TIdent "latch" :: TLParen :: rest ->
        let rest' = skip_to_brace rest in
        let (latch_body, rest'') = parse_body [] rest' in
        parse_body (PNLatch latch_body :: acc) rest''
    | TIdent "timing" :: TLParen :: TRParen :: TLBrace :: rest ->
        let (timing_body, rest') = parse_body [] rest in
        parse_body (PNTiming timing_body :: acc) rest'
    | TIdent key :: TColon :: (TString value | TIdent value) :: TSemicolon :: rest ->
        parse_body (PNAttr (key, value) :: acc) rest
    | TIdent _ :: TLParen :: rest ->
        (* Skip function calls we don't care about *)
        let rec skip_parens depth = function
          | TLParen :: rest -> skip_parens (depth + 1) rest
          | TRParen :: rest when depth > 1 -> skip_parens (depth - 1) rest
          | TRParen :: rest -> rest
          | _ :: rest -> skip_parens depth rest
          | [] -> []
        in
        parse_body acc (skip_parens 1 rest)
    | _ :: rest -> parse_body acc rest
    | [] -> (List.rev acc, [])
  in

  match tokens with
  | TIdent "library" :: TLParen :: (TString name | TIdent name) :: TRParen :: TLBrace :: rest ->
      let (body, _) = parse_body [] rest in
      Some (PNLibrary (name, body))
  | _ -> None

(* Extract cell information from parse tree *)
let extract_cell_info node =
  let rec get_pins acc = function
    | [] -> List.rev acc
    | PNPin (name, attrs) :: rest ->
        let dir = ref Internal in
        let func = ref None in
        List.iter (function
          | PNAttr ("direction", d) -> dir := parse_direction d
          | PNAttr ("function", f) -> func := Some f
          | _ -> ()
        ) attrs;
        get_pins ({name; direction = !dir; function_expr = !func} :: acc) rest
    | _ :: rest -> get_pins acc rest
  in

  let rec get_cell_type = function
    | [] -> "combinational"
    | PNFF _ :: _ -> "ff"
    | PNLatch _ :: _ -> "latch"
    | _ :: rest -> get_cell_type rest
  in

  match node with
  | PNCell (name, body) ->
      let pins = get_pins [] body in
      let cell_type = get_cell_type body in
      Some {cell_name = name; pins; cell_type}
  | _ -> None

(* Main parsing function *)
let parse_liberty_file filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;

  let tokens = tokenize content in
  match parse_library tokens with
  | Some (PNLibrary (lib_name, body)) ->
      let cells = Hashtbl.create 256 in
      List.iter (fun node ->
        match extract_cell_info node with
        | Some cell -> Hashtbl.add cells cell.cell_name cell
        | None -> ()
      ) body;
      {lib_name; cells}
  | None -> failwith ("Failed to parse library file: " ^ filename)

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
