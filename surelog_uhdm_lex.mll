(*
 MIT License

Copyright (c) 2024 Jonathan Kimmitt

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

{
  open Surelog_uhdm

let deflate token = 
  let q = Queue.create () in
  fun lexbuf -> 
    if not (Queue.is_empty q) then Queue.pop q else   
      match token lexbuf with 
        | [   ] -> EOF_TOKEN
        | [tok] -> tok
        | hd::t -> List.iter (fun tok -> Queue.add tok q) t ; hd 

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let oldcnt = ref 1

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
FINISHED, "FINISHED";
INT, "|INT";
KILLED, "KILLED";
Post_Elab, "Post-Elab";
Pre_Elab, "Pre-Elab";
RUNNING, "RUNNING";
Restored, "Restored";
SUSPENDED, "SUSPENDED";
UINT, "|UINT";
WAITING, "WAITING";
Always, "\\_always";
Array_net, "\\_array_net";
Array_var, "\\_array_var";
Array_typespec, "\\_array_typespec";
Assignment, "\\_assignment";
Attribute, "\\_attribute";
Begin, "\\_begin";
Bit_select, "\\_bit_select";
Case_item, "\\_case_item";
Case_stmt, "\\_case_stmt";
Class_defn, "\\_class_defn";
Class_typespec, "\\_class_typespec";
Class_var, "\\_class_var";
Constant, "\\_constant";
Cont_assign, "\\_cont_assign";
Design, "\\_design";
DESIGN, "design";
Enum_const, "\\_enum_const";
Enum_typespec, "\\_enum_typespec";
Enum_var, "\\_enum_var";
Event_control, "\\_event_control";
For_stmt, "\\_for_stmt";
Function, "\\_function";
Gen_if_else, "\\_gen_if_else";
Gen_region, "\\_gen_region";
Gen_scope, "\\_gen_scope";
Gen_case, "\\_gen_case";
Gen_scope_array, "\\_gen_scope_array";
If_else, "\\_if_else";
If_stmt, "\\_if_stmt";
Indexed_part_select, "\\_indexed_part_select";
Initial, "\\_initial";
Int_typespec, "\\_int_typespec";
Int_var, "\\_int_var";
Integer_typespec, "\\_integer_typespec";
Integer_var, "\\_integer_var";
Io_decl, "\\_io_decl";
Logic_net, "\\_logic_net";
Logic_typespec, "\\_logic_typespec";
Logic_var, "\\_logic_var";
Module_inst, "\\_module_inst";
Named_begin, "\\_named_begin";
Operation, "\\_operation";
Package, "\\_package";
Param_assign, "\\_param_assign";
Parameter, "\\_parameter";
Part_select, "\\_part_select";
Port, "\\_port";
Range, "\\_range";
Ref_module, "\\_ref_module";
Ref_obj, "\\_ref_obj";
Ref_typespec, "\\_ref_typespec";
Ref_var, "\\_ref_var";
Sys_func_call, "\\_sys_func_call";
Task, "\\_task";
Task_call, "\\_task_call";
Var_select, "\\_var_select";
Weaklyreferenced, "\\_weaklyReferenced";
Any_sverilog_class, "any_sverilog_class";
Array, "array";
Await, "await";
Bound, "bound";
Builtin, "builtin";
(*
Clk, "clk";
*)
Endln, "endln";
File, "file";
Get, "get";
Keycount, "keyCount";
Kill, "kill";
Line, "line";
Mailbox, "mailbox";
Message, "message";
New, "new";
Num, "num";
Peek, "peek";
Process, "process";
Put, "put";
Queue, "queue";
Resume, "resume";
Self, "self";
Semaphore, "semaphore";
(*
State, "state";
Status, "status";
String, "string";
Suspend, "suspend";
System, "system";
Try_get, "try_get";
Try_peek, "try_peek";
Try_put, "try_put";
*)
Uhdmallclasses, "|uhdmallClasses";
Uhdmallmodules, "|uhdmallModules";
Uhdmallpackages, "|uhdmallPackages";
Uhdmtopmodules, "|uhdmtopModules";
Uhdmtoppackages, "|uhdmtopPackages";
Vpiactual, "|vpiActual";
Vpialwaystype, "|vpiAlwaysType";
Vpiargument, "|vpiArgument";
Vpiarraynet, "|vpiArrayNet";
Vpiarraytype, "|vpiArrayType";
Vpiattribute, "|vpiAttribute";
Vpibaseexpr, "|vpiBaseExpr";
Vpiblocking, "|vpiBlocking";
Vpicaseitem, "|vpiCaseItem";
Vpicasetype, "|vpiCaseType";
Vpiclassdefn, "|vpiClassDefn";
Vpicondition, "|vpiCondition";
Vpiconsttype, "|vpiConstType";
Vpiconstantselect, "|vpiConstantSelect";
Vpicontassign, "|vpiContAssign";
Vpideffile, "|vpiDefFile";
Vpideflineno, "|vpiDefLineNo";
Vpidefname, "|vpiDefName";
Vpidirection, "|vpiDirection";
Vpielsestmt, "|vpiElseStmt";
Vpienumconst, "|vpiEnumConst";
Vpiexpr, "|vpiExpr";
Vpiforincstmt, "|vpiForIncStmt";
Vpiforinitstmt, "|vpiForInitStmt";
Vpifullname, "|vpiFullName";
Vpigenscope, "|vpiGenScope";
Vpigenscopearray, "|vpiGenScopeArray";
Vpigenstmt, "|vpiGenStmt";
Vpihighconn, "|vpiHighConn";
Vpiiodecl, "|vpiIODecl";
Vpiindex, "|vpiIndex";
Vpiindexedpartselecttype, "|vpiIndexedPartSelectType";
Vpiinstance, "|vpiInstance";
Vpileftrange, "|vpiLeftRange";
Vpilhs, "|vpiLhs";
Vpilocalparam, "|vpiLocalParam";
Vpilowconn, "|vpiLowConn";
Vpimethod, "|vpiMethod";
Vpimodule, "|vpiModule";
Vpiname, "|vpiName";
Vpinet, "|vpiNet";
Vpinetdeclassign, "|vpiNetDeclAssign";
Vpinettype, "|vpiNetType";
Vpioptype, "|vpiOpType";
Vpioperand, "|vpiOperand";
Vpioverriden, "|vpiOverriden";
Vpiparamassign, "|vpiParamAssign";
Vpiparameter, "|vpiParameter";
Vpiparent, "|vpiParent";
Vpiport, "|vpiPort";
Vpiprocess, "|vpiProcess";
Vpirandtype, "|vpiRandType";
Vpirange, "|vpiRange";
Vpirefmodule, "|vpiRefModule";
Vpireg, "|vpiReg";
Vpireturn, "|vpiReturn";
Vpirhs, "|vpiRhs";
Vpirightrange, "|vpiRightRange";
Vpisigned, "|vpiSigned";
Vpisize, "|vpiSize";
Vpistmt, "|vpiStmt";
Vpitask, "|vpiTask";
Vpitaskfunc, "|vpiTaskFunc";
Vpitop, "|vpiTop";
Vpitopmodule, "|vpiTopModule";
Vpitypedef, "|vpiTypedef";
Vpitypespec, "|vpiTypespec";
Vpivariables, "|vpiVariables";
Vpivisibility, "|vpiVisibility";
Vpiwidthexpr, "|vpiWidthExpr";
Vpiactual, "|vpiActual";
Vpialwaystype, "|vpiAlwaysType";
Vpiclassdefn, "|vpiClassDefn";
Vpicondition, "|vpiCondition";
Vpiconsttype, "|vpiConstType";
Vpidefname, "|vpiDefName";
Vpidirection, "|vpiDirection";
Vpielaborated, "|vpiElaborated";
Vpielaborated, "|vpiElaborated";
Vpielemtypespec, "|vpiElemTypespec";
Vpiexpr, "|vpiExpr";
Vpifullname, "|vpiFullName";
Vpiindex, "|vpiIndex";
Vpiinstance, "|vpiInstance";
Vpiiodecl, "|vpiIODecl";
Vpileftrange, "|vpiLeftRange";
Vpilhs, "|vpiLhs";
Vpilowconn, "|vpiLowConn";
Vpimethod, "|vpiMethod";
Vpiname, "|vpiName";
Vpinet, "|vpiNet";
Vpinettype, "|vpiNetType";
Vpioperand, "|vpiOperand";
Vpioptype, "|vpiOpType";
Vpiparent, "|vpiParent";
Vpiport, "|vpiPort";
Vpiprocess, "|vpiProcess";
Vpirange, "|vpiRange";
Vpireturn, "|vpiReturn";
Vpirhs, "|vpiRhs";
Vpirightrange, "|vpiRightRange";
Vpisigned, "|vpiSigned";
Vpisize, "|vpiSize";
Vpistmt, "|vpiStmt";
Vpitop, "|vpiTop";
Vpitopmodule, "|vpiTopModule";
Vpitypedef, "|vpiTypedef";
Vpitypespec, "|vpiTypespec";
Vpibasetypespec, "|vpiBaseTypespec";
Vpivariables, "|vpiVariables";
Vpivisibility, "|vpiVisibility";
Work, "work";
      ];
    fun s -> Hashtbl.find h s

let tok arg = [arg]

let vpi_expr = function
|  1 -> Vpiminusop (* unary minus *)
|  2 -> Vpiplusop (* unary plus *)
|  3 -> Vpinotop (* unary not *)
|  4 -> Vpibitnegop (* bitwise negation *)
|  5 -> Vpiunaryandop (* bitwise reduction AND *)
|  6 -> Vpiunarynandop (* bitwise reduction NAND *)
|  7 -> Vpiunaryorop (* bitwise reduction OR *)
|  8 -> Vpiunarynorop (* bitwise reduction NOR *)
|  9 -> Vpiunaryxorop (* bitwise reduction XOR *)
|  10 -> Vpiunaryxnorop (* bitwise reduction XNOR *)
|  11 -> Vpisubop (* binary subtraction *)
|  12 -> Vpidivop (* binary division *)
|  13 -> Vpimodop (* binary modulus *)
|  14 -> Vpieqop (* binary equality *)
|  15 -> Vpineqop (* binary inequality *)
|  16 -> Vpicaseeqop (* case (x and z) equality *)
|  17 -> Vpicaseneqop (* case inequality *)
|  18 -> Vpigtop (* binary greater than *)
|  19 -> Vpigeop (* binary greater than or equal *)
|  20 -> Vpiltop (* binary less than *)
|  21 -> Vpileop (* binary less than or equal *)
|  22 -> Vpilshiftop (* binary left shift *)
|  23 -> Vpirshiftop (* binary right shift *)
|  24 -> Vpiaddop (* binary addition *)
|  25 -> Vpimultop (* binary multiplication *)
|  26 -> Vpilogandop (* binary logical AND *)
|  27 -> Vpilogorop (* binary logical OR *)
|  28 -> Vpibitandop (* binary bitwise AND *)
|  29 -> Vpibitorop (* binary bitwise OR *)
|  30 -> Vpibitxorop (* binary bitwise XOR *)
|  31 -> Vpibitxnorop (* binary bitwise XNOR *)
|  32 -> Vpiconditionop (* ternary conditional *)
|  33 -> Vpiconcatop (* n-ary concatenation *)
|  34 -> Vpimulticoncatop (* repeated concatenation *)
|  35 -> Vpieventorop (* event OR *)
|  36 -> Vpinullop (* null operation *)
|  37 -> Vpilistop (* list of expressions *)
|  38 -> Vpimintypmaxop (* min:typ:max: delay expression *)
|  39 -> Vpiposedgeop (* posedge *)
|  40 -> Vpinegedgeop (* negedge *)
|  41 -> Vpiarithlshiftop (* arithmetic left shift (1364-2001) *)
|  42 -> Vpiarithrshiftop (* arithmetic right shift (1364-2001) *)
|  43 -> Vpipowerop (* arithmetic power op (1364-2001) *)
(*
|  40 -> Vpiconsttype (* constant subtypes: *)
|  41 -> Vpiblocking (* blocking assignment (Boolean) *)
|  42 -> Vpicasetype (* case statement subtypes: *)
|  43 -> Vpinetdeclassign (* assign part of decl (Boolean) *)
*)
|  82 -> Vpirhs (* right-hand side of assignment *)
| oth -> STRING ("Vpioptypeint "^string_of_int oth)
}

let ident = ['a'-'z' 'A'-'Z' '$' '_' '|' '\\'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$' ]*
let number = ['0'-'9' '-']['0'-'9' '_']*
let sized = ['0'-'9']*"'"['b' 'd' 'h' 'o']['0'-'9' 'A'-'F' 'a'-'f' '_' 'x' 'X' 'z' 'Z' '?']+
let hex = "|HEX:"['0'-'9' 'A'-'F' 'a'-'f']+
let dec = "|DEC:"['0'-'9']+
let oct = "|OCT:"['0'-'7']+
let bin = "|BIN:"['0' '1' 'x' 'X' 'z' 'Z' '?']+
let space = [' ' '\t' '\r']+
let newline = ['\n'] [' ']*
let string_const = "|STRING:"[^'\n']*
let qstring = '"'[^'"']*'"'
let vpimethod = "|vpiMethod:"['0'-'9']+
let vpioptype = "|vpiOpType:"['0'-'9']+
let vpidecompile = "|vpiDecompile:"[^'\n']+

rule token = parse
| '#' { tok ( HASH ) }
| ''' { tok ( QUOTE ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| ',' { tok ( COMMA ) }
| ':' { tok ( COLON ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }
| '.' { tok ( DOT ) }
| '(' { tok ( LPAREN ) }
| ')' { tok ( RPAREN ) }
| '@' { tok ( AT ) }
| '-' { tok ( HYPHEN ) }
| '/' { tok ( SLASH ) }
| '\\' { tok ( BACKSLASH ) }
(*
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
*)

  | space
      { token lexbuf }
  | newline as s
      { let old = !oldcnt in oldcnt := String.length s;
      if old < !oldcnt then List.init ((!oldcnt - old)/2) (fun _ -> Indent)
      else if old > !oldcnt then List.init ((old - !oldcnt)/2) (fun _ -> Unindent)
      else token lexbuf }
  | string_const as s
      { tok ( let prefix = String.length "|STRING:" in STRING_CONST (String.sub s prefix (String.length s - prefix))) }
  | sized as n
      { tok ( VpiNum n ) }
  | number as n
      { tok ( try Int (int_of_string n) with _ -> VpiNum n ) }
  | hex as h
      { tok ( HEXS (Scanf.sscanf h "|HEX:%s" (fun h->h) )) }
  | dec as d
      { tok ( DEC (Scanf.sscanf d "|DEC:%s" (fun d->d) )) }
  | oct as o
      { tok ( OCT (Scanf.sscanf o "|OCT:%s" (fun o->o) )) }
  | bin as b
      { tok ( BIN (Scanf.sscanf b "|BIN:%s" (fun b->b) )) }
  | vpidecompile as d
      { tok ( Vpidecompile (Scanf.sscanf d "|vpiDecompile:%s" (fun d->d) )) }
  | vpimethod as s
      { tok ( Vpimethodint (Scanf.sscanf s "|vpiMethod:%d" (fun n -> n))) }
  | vpioptype as s
      { tok ( Scanf.sscanf s "|vpiOpType:%d" vpi_expr) }
  | ident as s
      { tok ( try keyword s with Not_found -> STRING s ) }
  | qstring as s
      { tok ( STRING (String.sub s 1 (String.length s - 2)) ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Input_lex: "^String.make 1 oth) ) }

{
(* *)
let parse_output_ast_from_chan ch =
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start (deflate token) lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  output

let parse_output_ast_from_uhdm_pipe p =
  let ch = Unix.open_process_in p in
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start (deflate token) lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  let _ = Unix.close_process_in ch in
  output


(* The original Input_lex.mll trailer also defined `tranall`/`trantop`
 * helpers that fed the parsed token tree into hardcaml-lua's `top_pat`
 * pattern-matcher. We don't pull those modules in — downstream is the
 * surelog_to_behavioral converter, which walks the raw token tree
 * itself. *)
}
