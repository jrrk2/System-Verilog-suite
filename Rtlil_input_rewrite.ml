open Rtlil_input_lex
open Rtlil_input_rewrite_types
open Rtlil_input

let unhand = ref None
let othrw = ref (TLIST [])

let rec rw = function
| CONS1 oth -> TLIST (rw oth::[])
| CONS3(lft,_,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | oth -> TLIST (rw rght :: oth :: []))
| CONS4(lft,arg1,arg2,arg3) -> (match rw lft with TLIST arg -> TLIST (rw arg1 :: rw arg2 :: rw arg3 :: arg) | _ -> failwith "CONS4")
| CONS2(lft,rght) -> (match rw lft with TLIST arg -> TLIST (rw rght :: arg) | _ -> failwith "CONS2")
| ELIST lst -> ELIST (List.map rw lst)
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE3 (STRING ("EOL1"|"optional_eol1"), EMPTY_TOKEN, TOK_EOL) -> EMPTY_TOKEN
| TUPLE3(STRING("design1"|"design2"|"design3"|"module_body1"|"case_body1"|"case_body2"|"case_body3"|"sigspec_list_reversed1"),arg2,arg3) -> to_lst arg2 arg3
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,TUPLE3(STRING("EOL1"),_,_)) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,TUPLE3(STRING("EOL1"),_,_)) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(STRING("switch_body1"|"cell_body1"|"cell_body4"|"update_list82"),arg2,arg3,arg4,arg5,TUPLE3(STRING("EOL1"),_,_)) ->
    to_lst arg2 (TUPLE3 (rw arg3, rw arg4, rw arg5))
| TUPLE6(arg1,arg2,arg3,arg4,arg5,TUPLE3(STRING("EOL1"),_,_)) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(STRING("cell_body2"|"cell_body3"),arg2,arg3,arg4,arg5,arg6,TUPLE3(STRING("EOL1"),_,_)) ->
    to_lst arg2 (TUPLE4 (rw arg3, rw arg4, rw arg5, rw arg6))
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,TUPLE3(STRING("EOL1"),_,_)) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,TUPLE3(STRING("EOL1"),_,_)) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,TUPLE3(STRING("EOL1"),_,_)) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10)
(*
| TUPLE11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) ->
   TUPLE11(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11)
| TUPLE12(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) ->
   TUPLE12(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11, rw arg12)
| TUPLE13(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) ->
   TUPLE13(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11, rw arg12, rw arg13)
*)
| ((TOK_ID _ | TOK_INT _ | TOK_STRING _ | TOK_VALUE _ | STRING _
| TOK_ALWAYS
| TOK_ASSIGN
| TOK_ATTRIBUTE
| TOK_AUTOIDX
| TOK_CASE
| TOK_CELL
| TOK_CONNECT
| TOK_EDGE
| TOK_END
| TOK_EOL
| TOK_GLOBAL
| TOK_HIGH
| TOK_INIT
| TOK_INOUT
| TOK_INPUT
| TOK_INVALID
| TOK_LOW
| TOK_MEMORY
| TOK_MEMWR
| TOK_MODULE
| TOK_NEGEDGE
| TOK_OFFSET
| TOK_OUTPUT
| TOK_PARAMETER
| TOK_POSEDGE
| TOK_PROCESS
| TOK_REAL
| TOK_SIGNED
| TOK_SIZE
| TOK_SWITCH
| TOK_SYNC
| TOK_UPDATE
| TOK_UPTO
| TOK_WIDTH
| TOK_WIRE
| AT|EMPTY_TOKEN|LBRACK|RBRACK|LBRACE|RBRACE
| COLON|COMMA|CARET|TILDE|QUERY|QUOTE|EOF_TOKEN|HASH) as x) -> x
| oth -> othrw := oth; failwith ("rw fail: othrw")

and to_lst arg2 arg3 = TLIST (rw arg3 :: (match rw arg2 with EMPTY_TOKEN -> [] | TLIST lst -> lst | oth -> unhand := Some oth; failwith "design"))

let rec rw' = function
| EMPTY_TOKEN -> []
| TOK_POSEDGE -> TokPos :: []
| TOK_NEGEDGE -> TokNeg :: []
| TOK_ID id -> TokID id :: []
| TOK_INT n -> TokInt n :: []
| TOK_VALUE v -> TokVal v :: []
| TOK_STRING s -> TokStr s :: []
| TLIST lst -> List.flatten (List.map rw' lst)
| TUPLE3(STRING("EOL1"),arg1,TOK_EOL) -> rw'' arg1
| TUPLE3(STRING("attr_list56"),arg1,arg2) -> rw'' arg1 @ rw'' arg2
| TUPLE3(STRING("case_body1"),arg1,arg2) -> Case_body63(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("case_body2"),arg1,arg2) -> Case_body64(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("case_body3"),arg1,arg2) -> Case_body65(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("design1"),arg1,arg2) -> Design6(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("design2"),arg1,arg2) -> Design7(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("design3"),arg1,arg2) -> Design8(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("rtlil_input1"),EMPTY_TOKEN,arg2) -> rw'' arg2
| TUPLE3(STRING("ml_start1"),arg1,EOF_TOKEN) -> rw'' arg1
| TUPLE3(STRING("module_body13"),arg1,arg2) -> Module_body13(rw'' arg1, rw'' arg2) :: []
| TUPLE3(STRING("optional_eol1"),arg1,TOK_EOL) -> rw'' arg1
| TUPLE3(STRING("sigspec_list_reversed1"),arg1,arg2) -> Sigspec_list_reversed93(rw'' arg1, rw'' arg2) :: []
| TUPLE4(TOK_CASE, value, EMPTY_TOKEN, lst) -> TokCase(rw' value, rw' lst) :: []
| TUPLE3(TOK_CONNECT, value, value2) -> TokConn(rw' value, rw' value2) :: []
| TUPLE3(TOK_PARAMETER, value, value2) -> TokParam(rw' value, rw' value2) :: []
| TUPLE3(TOK_UPDATE, value, value2) -> TokUpdate(rw' value, rw' value2) :: []
| TUPLE3(STRING("autoidx_stmt1"),TOK_AUTOIDX,TOK_INT arg2) -> Autoidx_stmt26(arg2) :: []
(*
 | TUPLE4(STRING("compare_list61"),arg1,COMMA,arg3) -> Compare_list61(rw'' arg1, rw'' arg3) :: []
*)
| TUPLE4(STRING("memory_options1"),arg1,TOK_WIDTH,TOK_INT arg3) -> Memory_optionswidth(arg3) :: rw'' arg1
| TUPLE4(STRING("memory_options2"),arg1,TOK_SIZE,TOK_INT arg3) -> Memory_optionssize(arg3) :: rw'' arg1
| TUPLE4(STRING("memory_options3"),arg1,TOK_OFFSET,TOK_INT arg3) -> Memory_optionsoffset(arg3) :: rw'' arg1
| TUPLE4(STRING("param_stmt1"),TOK_PARAMETER,TOK_ID arg2,arg3) -> Param_stmt23(arg2,rw'' arg3) :: []
| TUPLE5(STRING("sigspec3"),TOK_ID arg1,LBRACK,TOK_INT arg3,RBRACK) -> Sigspec90(arg1, arg3) :: []
| TUPLE7(STRING("sigspec4"),TOK_ID arg1,LBRACK,TOK_INT arg3,COLON,TOK_INT arg5,RBRACK) -> Sigspecrange(arg1, arg3, arg5) :: []
| TUPLE4(STRING("sigspec5"),LBRACE,arg2,RBRACE) -> Sigspec92(rw'' arg2) :: []
| TUPLE4(STRING("wire_options1"),arg1,TOK_WIDTH,TOK_INT arg3) -> Wire_optionswidth(arg3) :: rw'' arg1
| TUPLE4(STRING("wire_options2"),arg1,TOK_WIDTH,TOK_INVALID) -> Wire_optionsinvalid :: rw'' arg1
| TUPLE3(STRING("wire_options3"),arg1,TOK_UPTO) -> Upto :: rw'' arg1
| TUPLE3(STRING("wire_options4"),arg1,TOK_SIGNED) -> Signed :: rw'' arg1
| TUPLE4(STRING("wire_options5"),arg1,TOK_OFFSET,TOK_INT arg3) -> Wire_optionsoffset(arg3) :: rw'' arg1
| TUPLE4(STRING("wire_options6"),arg1,TOK_INPUT,TOK_INT arg3) -> Wire_optionsinput(arg3) :: rw'' arg1
| TUPLE4(STRING("wire_options7"),arg1,TOK_OUTPUT,TOK_INT arg3) -> Wire_optionsoutput(arg3) :: rw'' arg1
| TUPLE4(STRING("wire_options8"),arg1,TOK_INOUT,TOK_INT arg3) -> Wire_optionsinout(arg3) :: rw'' arg1
| TUPLE4(STRING("assign_stmt1"),TOK_ASSIGN,arg2,arg3) -> Assign_stmt67(rw'' arg2,rw'' arg3) :: []
| TUPLE4(STRING("attr_stmt1"),TOK_ATTRIBUTE,TOK_ID arg2,arg3) -> Attr_stmt(arg2,rw'' arg3) :: []
| TUPLE4(STRING("conn_stmt1"),TOK_CONNECT,arg2,arg3) -> Conn_stmt96(rw'' arg2, rw'' arg3) :: []
| TUPLE4(STRING("memory_stmt1"),TOK_MEMORY,arg2,TOK_ID arg3) -> Memory_stmt39(rw'' arg2,arg3) :: []
| TUPLE4(STRING("param_defval_stmt1"),TOK_PARAMETER,TOK_ID arg2,arg3) -> Param_defval_stmt24(arg2,rw'' arg3) :: []
| TUPLE4(STRING("wire_stmt1"),TOK_WIRE,arg2,TOK_ID arg3) -> Wire_stmt(rw'' arg2,arg3) :: []
| TUPLE6(STRING("cell_body1"),arg1,TOK_PARAMETER,TOK_ID arg3,arg4,arg5) -> Cell_bodyparam(rw'' arg1, arg3, rw'' arg4, rw'' arg5) :: []
| TUPLE7(STRING("cell_body2"),arg1,TOK_PARAMETER,TOK_SIGNED,TOK_ID arg4,arg5,arg6) -> Cell_bodypsigned(rw'' arg1, arg4, rw'' arg5, rw'' arg6) :: []
| TUPLE7(STRING("cell_body3"),arg1,TOK_PARAMETER,TOK_REAL,TOK_ID arg4,arg5,arg6) -> Cell_bodypreal(rw'' arg1, arg4,rw'' arg5,rw'' arg6) :: []
| TUPLE6(STRING("cell_body4"),arg1,TOK_CONNECT,TOK_ID arg3,arg4,arg5) -> Cell_bodyconnect(rw'' arg1, arg3,rw'' arg4,rw'' arg5) :: []
| TUPLE6(STRING("switch_body1"),arg1,TOK_CASE,arg3,arg4,arg5) -> Switch_bodycase(rw'' arg3, rw'' arg4, rw'' arg5) :: rw' arg1
(*
 | TUPLE7(STRING("sync_list69"),arg1,TOK_SYNC,arg3,arg4,arg5,arg6) -> Sync_list69(rw'' arg3, rw'' arg4,rw'' arg5, rw'' arg6) :: rw' arg1
| TUPLE6(STRING("sync_list71"),arg1,TOK_SYNC,TOK_ALWAYS,arg4,arg5) -> Sync_listalways(rw'' arg4, rw'' arg5) :: rw'' arg1
| TUPLE6(STRING("sync_list73"),arg1,TOK_SYNC,TOK_GLOBAL,arg4,arg5) -> Sync_listglobal(rw'' arg4, rw'' arg5) :: rw'' arg1
| TUPLE6(STRING("sync_list75"),arg1,TOK_SYNC,TOK_INIT,arg4,arg5) -> Sync_listinit(rw'' arg4, rw'' arg5) :: rw'' arg1
| TUPLE5(STRING("update_list82"),arg1,TOK_UPDATE,arg3,arg4) -> Update_list82(rw'' arg3, rw'' arg4) :: rw'' arg1
*)
| TUPLE6(STRING("module1"),TOK_MODULE,TOK_ID arg2,(EMPTY_TOKEN|TUPLE3(STRING("EOL1"),_,_)),arg4,TOK_END) -> Module12(arg2, rw'' arg4) :: []
| TUPLE7(STRING("cell_stmt1"),TOK_CELL,TOK_ID arg2,TOK_ID arg3,arg4,arg5,TOK_END) -> Cell_stmt(arg2,arg3,rw'' arg4, rw'' arg5) :: []
| TUPLE7(STRING("proc_stmt1"),TOK_PROCESS,TOK_ID arg2,arg3,arg4,arg5,TOK_END) -> Proc_stmt(arg2,rw'' arg3,rw'' arg4,rw'' arg5) :: []
| TUPLE7(STRING("switch_stmt1"),TOK_SWITCH,arg2,arg3,arg4,arg5,TOK_END) -> Switch_stmt(rw'' arg2,rw'' arg3,rw'' arg4,rw'' arg5) :: []
| TUPLE9(STRING("update_list83"),arg1,arg2,TOK_MEMWR,TOK_ID arg4,arg5,arg6,arg7,arg8) -> Update_listmemwr(arg4, rw'' arg5, rw'' arg6,rw'' arg7,rw'' arg8) :: rw' arg2 @ rw' arg1
| oth -> unhand := Some oth; failwith ("rw' fail: unhand")

and rw'' lst = List.rev (rw' lst)

let parse_output_ast_from_file arg =
  let ch = open_in arg in
  let lb = Lexing.from_channel ch in
  let output = try
      Rtlil_input.ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  close_in ch;
  output

let parse_output_ast_from_rtlil_pipe pipe =
  let ch = Unix.open_process_in pipe in
  let lb = Lexing.from_channel ch in
  let output = try
      Rtlil_input.ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  let _ = Unix.close_process_in ch in  
  output

let parse_output_ast_from_string s =
  let lb = Lexing.from_string s in
  let output = try
      Rtlil_input.ml_start token lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output
  
let parse p =
  let p' = List.rev (rw' (rw p)) in
  let mods = Hashtbl.create 255 in
  let nam = ref "" in
  Hashtbl.add mods !nam [];
  List.iter (function
      | Module12(string,ilang_lst') as x -> nam := string; Hashtbl.add mods !nam [x]
      | oth -> Hashtbl.replace mods !nam (oth :: Hashtbl.find mods !nam)) p';
  let mlst = ref [] in
  Hashtbl.iter (fun k x -> if k <> "" then mlst := (k, List.rev x) :: !mlst) mods;
  p, List.sort compare !mlst

let keyword = function
| oth -> false
