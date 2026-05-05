(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Liberty

let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      Liberty.ml_start Liberty_lex.token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "File.parse: parse error at character %d" n);
(*
    | _ ->
      failwith (Printf.sprintf "Parser error at line %d" !Scope.lincnt)
*)
  in
  output

let parse arg =
  let ch = open_in arg in
  let rslt = parse_output_ast_from_chan ch in
  close_in ch;
  rslt

(*
let unhand = ref None
*)

let rec rw = function
| TUPLE2 (arg, (EOF_TOKEN|SEMI)) -> rw arg
| TUPLE2 ((PLUS|MINUS) as op, arg) -> TUPLE2(op, rw arg)
| TUPLE2 (lst, arg) -> (match rw lst with TLIST lst -> TLIST (rw arg :: lst) | oth -> TLIST (rw arg :: oth :: []))
| TUPLE3 (lst, COMMA, arg) -> (match rw lst with TLIST lst -> TLIST (rw arg :: lst) | oth -> TLIST (rw arg :: oth :: []))
| TLIST lst -> TLIST (List.map rw lst)
| ELIST lst -> ELIST (List.map rw lst)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10)
| STRING _ as x -> x
| SLIST _ as x -> x
| NUM _ as x -> x
| IDENT _ as x -> x
| CONS4 _ as x -> x
| CONS3 _ as x -> x
| CONS2 _ as x -> x
| CONS1 _ as x -> x
| VBAR -> VBAR
| UNDERSCORE -> UNDERSCORE
| UNARY -> UNARY
| TILDE -> TILDE
| SEMI -> SEMI
| RPAR -> RPAR
| RCURLY -> RCURLY
| RBRACK -> RBRACK
| RBRACE -> RBRACE
| QUOTE -> QUOTE
| QUERY -> QUERY
| PLUS -> PLUS
| PLING -> PLING
| PERCENT -> PERCENT
| MULT -> MULT
| MINUS -> MINUS
| LPAR -> LPAR
| LINEFEED -> LINEFEED
| LESS -> LESS
| LCURLY -> LCURLY
| LBRACK -> LBRACK
| LBRACE -> LBRACE
| KW_TRUE -> KW_TRUE
| KW_FALSE -> KW_FALSE
| KW_DEFINE_GROUP -> KW_DEFINE_GROUP
| KW_DEFINE -> KW_DEFINE
| HASH -> HASH
| GREATER -> GREATER
| ERROR_TOKEN -> ERROR_TOKEN
| ERROR -> ERROR
| EQ -> EQ
| EOF_TOKEN -> EOF_TOKEN
| END -> END
| EMPTY_TOKEN -> EMPTY_TOKEN
| DOUBLEQUOTE -> DOUBLEQUOTE
| DOT -> DOT
| DOLLAR -> DOLLAR
| DIV -> DIV
| DEFAULT -> DEFAULT
| COMMA -> COMMA
| COLON -> COLON
| CARET -> CARET
| BACKSLASH -> BACKSLASH
| BACKQUOTE -> BACKQUOTE
| AT -> AT
| AMPERSAND -> AMPERSAND
| ACCEPT -> ACCEPT

type liberty =
| Library of string * liberty list
| LibCell of string * liberty list
| CellPin of string * liberty list
| Timing of liberty list
| IPower of liberty list
| LPower of liberty list
| Power of string * string * liberty list
| CellValues of liberty list
| CellIndex1 of string
| CellIndex2 of string
| Other of string * string * liberty list
| String of string
| Sense of string
| Related of string * string
| SDF_cond of string
| Function of string
| Direction of string
| When of string
| Parameter of string *float
| Transition of string * string * liberty
| FlipFlop of liberty list * liberty list
| Latch of liberty list * liberty list
| StateTable of liberty list * string
| WireLoad of string * liberty list
| FanoutLength of float list
| Define of string list
| VoltageMap of liberty list
| CapLoadUnit of liberty list
| LibFeatures of liberty list
| IsolationCell [@@deriving yojson]

let unhand = ref None
let plst = ref []
let truelst = ref []

let scaling = function
| "pW" -> 1e-12
| "kohm" -> 1e3
| "mA" -> 1e-3
| "V" -> 1.0
| "ps" -> 1e-12
| oth -> failwith ("Units: "^oth^" not recognised")

let rec rw' = function
| TUPLE4 (TUPLE4 (IDENT "library", LPAR, (STRING nam | IDENT nam), RPAR), LCURLY, TLIST lst, RCURLY) -> Library (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "cell", LPAR, (STRING nam | IDENT nam), RPAR), LCURLY, TLIST lst, RCURLY) -> LibCell (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE3 (IDENT ("test_cell" as nam), LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> LibCell (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "pg_pin", LPAR, (STRING nam | IDENT nam), RPAR), LCURLY, TLIST lst, RCURLY) -> CellPin (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "pin", LPAR, (STRING nam | IDENT nam), RPAR), LCURLY, TLIST lst, RCURLY) -> CellPin (nam, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "pin", LPAR, (STRING nam | IDENT nam), RPAR), LCURLY, (TUPLE4 _ as itm), RCURLY) -> CellPin (nam, rw' itm :: [])
| TUPLE4 (TUPLE3 (IDENT "timing", LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> Timing (List.rev_map rw' lst)
| TUPLE4 (TUPLE3 (IDENT "internal_power", LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> IPower (List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT ("rise_power"|"fall_power" as p), LPAR, (STRING nam | IDENT nam), RPAR), LCURLY, lst, RCURLY) ->
    Power (p, nam, match lst with TLIST lst -> List.rev_map rw' lst | oth -> rw' oth :: [])
| TUPLE4 (TUPLE4 (IDENT ("rise_transition"|"fall_transition" as x), LPAR, (STRING s | IDENT s), RPAR), LCURLY, values, RCURLY) ->
Transition(x, s, rw' values)
| TUPLE4 (TUPLE4 (IDENT "ff", LPAR, TLIST oplst, RPAR), LCURLY, TLIST lst, RCURLY) -> FlipFlop(List.rev_map rw' oplst, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "latch", LPAR, TLIST oplst, RPAR), LCURLY, TLIST lst, RCURLY) -> Latch(List.rev_map rw' oplst, List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT "statetable", LPAR, TLIST pinlst, RPAR), LCURLY, TUPLE4 (IDENT "table", COLON, STRING s, SEMI), RCURLY) ->
StateTable(List.rev_map rw' pinlst, s)
| TUPLE4 (TUPLE4 (IDENT "wire_load", LPAR, STRING nam, RPAR), LCURLY, TLIST wlst, RCURLY) -> WireLoad(nam, List.rev_map rw' wlst)
| TUPLE4 (TUPLE3 (IDENT "leakage_power", LPAR, RPAR), LCURLY, TLIST lst, RCURLY) -> LPower (List.rev_map rw' lst)
| TUPLE4 (IDENT "fanout_length", LPAR, TLIST lst, RPAR) -> FanoutLength (List.rev_map (function NUM x -> float_of_string x | oth -> unhand := Some oth; failwith "fanout") lst)
| TUPLE4 (IDENT "voltage_map", LPAR, TLIST lst, RPAR) -> VoltageMap (List.rev_map rw' lst)
| TUPLE4 (IDENT "capacitive_load_unit", LPAR, TLIST lst, RPAR) -> CapLoadUnit (List.rev_map rw' lst)
| TUPLE4 (IDENT "library_features", LPAR, TLIST lst, RPAR) -> LibFeatures (List.rev_map rw' lst)
| TUPLE4 (TUPLE4 (IDENT ("rise_constraint"|"fall_constraint"|"cell_rise"|"cell_fall"), LPAR, STRING s, RPAR), LCURLY, TLIST lst, RCURLY) -> CellValues (List.rev_map rw' lst)
| TUPLE4 (IDENT "values", LPAR, TLIST lst, RPAR) -> CellValues (List.rev_map rw' lst)
| TUPLE4 (IDENT "values", LPAR, (STRING _ as x), RPAR) -> CellValues (rw' x :: [])
| TUPLE4 (IDENT "index_1", LPAR, STRING s, RPAR) -> CellIndex1 s
| TUPLE4 (IDENT "index_2", LPAR, STRING s, RPAR) -> CellIndex2 s
| TUPLE4 (IDENT ("technology" as p), LPAR, IDENT s, RPAR) -> Related(p,s)
| TUPLE4 (TUPLE4 (IDENT  oth, LPAR, IDENT nam, RPAR), LCURLY, TLIST lst, RCURLY) -> Other (oth, nam, List.rev_map rw' lst)
| TUPLE4 (IDENT "when", COLON, STRING s, SEMI) -> When s
| TUPLE4 (IDENT "timing_sense", COLON, (STRING s | IDENT s), SEMI) -> Sense s
| TUPLE4 (IDENT ("input_signal_level"|"level_shifter_data_pin"|"related_pin"|"related_output_pin"|"related_bias_pin"|"related_ground_pin"|"related_power_pin"|"physical_connection"|"three_state"|"enable"|"data_in"|"clocked_on"|"next_state"|"preset"|"clear"|"state_function"|"default_wire_load"|"process_corner"|"pulling_resistance_unit"|"current_unit"|"voltage_unit"|"leakage_power_unit"|"time_unit"|"comment"|"revision"|"date"|"signal_type"|"sim_opt" as p), COLON, STRING s, SEMI) -> Related (p, s)
| TUPLE4 (IDENT "sdf_cond", COLON, STRING s, SEMI) -> SDF_cond s
| TUPLE4 (IDENT ("function"), COLON, STRING s, SEMI) -> Function s
| TUPLE4 (IDENT "direction", COLON, (STRING s | IDENT s), SEMI) -> Direction s
| TUPLE4 (IDENT ("cell_leakage_power" as p), COLON, TUPLE3 (NUM mant, PLUS, NUM expo), SEMI) -> Parameter(p, float_of_string (mant^"+"^expo))
| TUPLE4 (IDENT ("default_wire_load_mode"|"output_voltage"|"input_voltage"|"driver_waveform_fall"|"driver_waveform_rise"|"driver_waveform_name" as p), COLON, (STRING _|IDENT _), SEMI) -> Parameter (p, 0.0)
| TUPLE4 (IDENT ("voltage_unit"|"time_unit"|"leakage_power_unit"|"pulling_resistance_unit"|"current_unit" as p), COLON, TLIST [IDENT units; NUM num], SEMI) -> Parameter (p, float_of_string num *. scaling units)
| TUPLE4 (IDENT ("power_down_function"|"pg_type"|"voltage_name"|"timing_type"|"clock"|"nextstate_type"|"clear_preset_var2"|"clear_preset_var1"|"dont_use"|"dont_touch"|"clock_gate_out_pin"|"clock_gate_enable_pin"|"clock_gate_clock_pin"|"internal_node"|"clock_gating_integrated_cell"|"clock_gate_test_pin"|"variable_1"|"variable_2"|"default_operating_conditions"|"tree_type"|"in_place_swap_mode"|"delay_model" as p), COLON, (STRING s | IDENT s), SEMI) -> Related (p, s)
| TUPLE4 (IDENT ("area"|"capacitance"|"cell_leakage_power"|"default_cell_leakage_power"|"default_fanout_load"|"default_inout_pin_cap"|"default_input_pin_cap"|"default_leakage_power_density"|"default_max_fanout"|"default_max_transition"|"default_output_pin_cap"|"drive_strength"|"fall_capacitance"|"input_threshold_pct_fall"|"input_threshold_pct_rise"|"max_capacitance"|"max_transition"|"nom_process"|"nom_temperature"|"nom_voltage"|"output_threshold_pct_fall"|"output_threshold_pct_rise"|"process"|"resistance"|"rise_capacitance"|"slew_derate_from_library"|"slew_lower_threshold_pct_fall"|"slew_lower_threshold_pct_rise"|"slew_upper_threshold_pct_fall"|"slew_upper_threshold_pct_rise"|"slope"|"temperature"|"value"|"vih"|"vil"|"vimax"|"vimin"|"violation_delay_degrade_pct"|"voh"|"vol"|"voltage"|"vomax"|"vomin"|"revision" as p), COLON, (NUM s|STRING s), SEMI) -> if not (List.mem p !plst) then plst := p :: !plst;
Parameter (p, float_of_string s)
| TUPLE4 (IDENT ("library_features"|"technology" as oth), LPAR, STRING nam, RPAR) -> Other(oth, nam, [])
| TUPLE4 (IDENT ("is_isolation_cell"), COLON, STRING "true", SEMI) -> IsolationCell
| TUPLE4 (IDENT ("always_on"|"is_level_shifter"|"isolation_cell_data_pin"|"isolation_cell_enable_pin"|"simulation"|"switching_power_split_model" as p), COLON, STRING "true", SEMI) -> if not (List.mem p !truelst) then truelst := p :: !truelst; Parameter(p, 1.0)
| TUPLE4 (IDENT ("min_pulse_width_mode"|"level_shifter_type"|"driver_model"|"default_constraint_arc_mode"|"default_arc_mode"|"bus_naming_style"|"cell_footprint" as oth), COLON, STRING nam, SEMI) -> Other(oth, nam, [])
| TUPLE4 (IDENT ("input_voltage_range"|"output_voltage_range" as oth), LPAR, TLIST lst, RPAR) -> Other(oth, oth, List.rev_map rw' lst)
| TUPLE4 (IDENT "define", LPAR, TLIST lst, RPAR) -> Define(List.rev_map (function IDENT id -> id | oth -> unhand := Some oth; failwith "define") lst)
| TUPLE4 (TUPLE4 (IDENT ("normalized_driver_waveform"|"lu_table_template"|"power_lut_template"|"operating_conditions" as oth), LPAR, STRING (nam), RPAR), LCURLY, TLIST lst, RCURLY) -> Other(oth, nam, List.rev_map rw' lst)
| STRING s -> String s
| IDENT s -> String s
| NUM n ->  Parameter ("", float_of_string n)
| TLIST (TUPLE4 (IDENT "values", _, _, _) :: _ as lst) -> CellValues(List.rev_map rw' lst)
| oth -> unhand := Some oth; failwith "rw'"

type attr = {cellhash: (string,((string * string) list * (string * string) list * liberty))Hashtbl.t}

let extract_pg_pins = try int_of_string (Sys.getenv "EXTRACT_PG_PINS") > 0 with _ -> false

let rec scan attr = function
| Library (_, lst) -> List.iter (scan attr) lst
| Related (p,q) -> ()
| LibFeatures _ -> ()
| CapLoadUnit _ -> ()
| Parameter (p,f) -> ()
| VoltageMap _ -> ()
| Define _ -> ()
| Other (_, _ , _) -> ()
| WireLoad _ -> ()
| LibCell (nam, lst) -> let pins = ref [] and fn = ref [] and purpose = ref (String nam) in List.iter (function
    | Other ("pg_pin", pin, lst) when extract_pg_pins -> List.iter (function Related ("pg_type", pwr) -> pins := (pin,pwr) :: !pins | _ -> ()) lst
    | CellPin (pin, lst) -> List.iter (function Direction "internal" -> () | Direction dir -> pins := (pin,dir) :: !pins | Function f -> fn := (pin,f) :: !fn | _ -> ()) lst
    | (Latch _ | FlipFlop _ | StateTable _ | Related _ | IsolationCell ) as p -> purpose := p
    | _ -> ()) lst; Hashtbl.add attr.cellhash nam (!pins,!fn,!purpose)
| CellPin (_, _) -> ()
| Timing _ -> ()
| IPower _ -> ()
| LPower _ -> ()
| Power (_, _, _) -> ()
| CellValues _ -> ()
| CellIndex1 _ -> ()
| CellIndex2 _ -> ()
| String _ -> ()
| Sense _ -> ()
| SDF_cond _ -> ()
| Function _ -> ()
| Direction _ -> ()
| When _ -> ()
| Transition (_, _, _) -> ()
| FlipFlop (_, _) -> ()
| Latch (_, _) -> ()
| StateTable (_, _) -> ()
| FanoutLength _ -> ()
| IsolationCell -> ()
let rec strip = function
| TUPLE3(STRING _, arg1, arg2) -> TUPLE2(strip arg1, strip arg2)
| TUPLE4(STRING _, arg1, arg2, arg3) -> TUPLE3(strip arg1, strip arg2, strip arg3)
| TUPLE5(STRING _, arg1, arg2, arg3, arg4) -> TUPLE4(strip arg1, strip arg2, strip arg3, strip arg4)
| TUPLE6(STRING _, arg1, arg2, arg3, arg4, arg5) -> TUPLE5(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5)
| TUPLE7(STRING _, arg1, arg2, arg3, arg4, arg5, arg6) -> TUPLE6(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6)
| TUPLE8(STRING _, arg1, arg2, arg3, arg4, arg5, arg6, arg7) -> TUPLE7(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6, strip arg7)
| TUPLE9(STRING _, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8) -> TUPLE8(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6, strip arg7, strip arg8)
| TUPLE10(STRING _, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9) -> TUPLE9(strip arg1, strip arg2, strip arg3, strip arg4, strip arg5, strip arg6, strip arg7, strip arg8, strip arg9)
| (PLUS|COMMA|LCURLY|RCURLY|EOF_TOKEN|SEMI|COLON|LPAR|RPAR|IDENT _|STRING _|NUM _) as x -> x
| oth -> unhand := Some oth; failwith "strip"

let rewrite arg =
  let p = parse arg in
  let p' = rw (strip p) in
  let rw = rw' p' in
  let cellhash = Hashtbl.create 255 in
  let _ = scan {cellhash} rw in
  rw,cellhash
