(*
    <vscr - Verilog converter to abc format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

{
open Lexing
open Vparser
open Setup

let trace = ref false
let psl_nest = ref 0
let logf = ref None

let ksymbols = Hashtbl.create 256;;

let enter_keyword id keyword = 
if Hashtbl.mem ksymbols id then
  Printf.printf "Error: repeated keyword %s\n" id
else begin
(*  Printf.printf "Enter %s\n" id; *)
  Hashtbl.add ksymbols id keyword
  end

let trace_log ktok = if !trace then match !logf with
  | None -> logf := Some (open_out "vlexer.log")
  | Some stream ->
    Printf.fprintf stream "%s\n" (str_token ktok);
    if ktok = ENDOFFILE then ( close_out stream; logf := None )

let hlog lexbuf ktok:token = begin
  let psl = (match ktok with 
    | PSL -> psl_nest := 0; true
    | SEMICOLON -> !psl_nest > 0
    | _ -> history.(!histcnt).psl) in
  let ktok' = if psl || history.(!histcnt).psl then (match ktok with
    | DEFAULT -> PSL_DEFAULT
    | ALWAYS -> PSL_ALWAYS
    | LCURLY -> incr psl_nest; TokenLBr
    | RCURLY -> decr psl_nest; TokenRBr
    | P_ANDAND -> TokenAnd
    | P_OROR -> TokenOr
    | P_ORMINUSGT -> TokenImp
    | PLING -> TokenNeg
    | LPAREN -> TokenLPar
    | RPAREN -> TokenRPar
    | P_EQUAL -> TokenEEQ
    | SEMICOLON -> TokenScol
    | P_MINUSGT -> TokenImp
    | COLON -> TokenCol 
    | TIMES -> TokenStar 
    | PLUS -> TokenPlus 
    | P_AMPERSAND -> TokenAnd 
    | P_VBAR -> TokenOr 
    | P_NOTEQUAL -> TokenNEQ 
    | EQUALS -> TokenEQ 
    | P_EQUAL -> TokenEEQ 
    | P_ANDAND -> TokenCap 
    | P_OROR -> TokenCup 
    | IDSTR "Cl" -> TokenCl
    | IDSTR "F" -> TokenFin
    | IDSTR "G" -> TokenGen
    | IDSTR "H" -> TokenOpH
    | IDSTR "O" -> TokenOpO
    | IDSTR "R" -> TokenRel
    | IDSTR "S" -> TokenOpS
    | IDSTR "T" -> TokenOpT
    | IDSTR "U" -> TokenUnt
    | IDSTR "V" -> TokenRel
    | IDSTR "W" -> TokenOpW
    | IDSTR "X" -> TokenNext
    | IDSTR "Y" -> TokenYest
    | IDSTR "Z" -> TokenZest
    | IDSTR "tt" -> TokenProp("TRUE")    (* true and false in NuSMV *)
    | IDSTR "ff" -> TokenProp("FALSE") 
    | IDSTR "%for" -> PSL_FOR
    | IDSTR "%if" -> PSL_IF
    | IDSTR "abort" -> PSL_ABORT
    | IDSTR "assert" -> PSL_ASSERT
    | IDSTR "assume_guarantee" -> PSL_ASSUME_GUARANTEE
    | IDSTR "before!" -> PSL_BEFORE_PLING
    | IDSTR "before!_" -> PSL_BEFORE_PLING
    | IDSTR "before" -> PSL_BEFORE
    | IDSTR "boolean" -> PSL_BOOLEAN
    | IDSTR "clock" -> PSL_CLOCK
    | IDSTR "const" -> PSL_CONST
    | IDSTR "cover" -> PSL_COVER
    | IDSTR "endpoint" -> PSL_ENDPOINT
    | IDSTR "eventually!" -> PSL_EVENTUALLY_PLING
    | IDSTR "fairness" -> PSL_FAIRNESS
    | IDSTR "fell" -> PSL_FELL
    | IDSTR "forall" -> PSL_FORALL
    | IDSTR "in" -> PSL_IN
    | IDSTR "inf" -> PSL_INF
    | IDSTR "inherit" -> PSL_INHERIT
    | IDSTR "never" -> PSL_NEVER
    | IDSTR "next!" -> PSL_NEXT_PLING
    | IDSTR "next" -> PSL_NEXT
    | IDSTR "next_a!" -> PSL_NEXT_A_PLING
    | IDSTR "next_a" -> PSL_NEXT_A
    | IDSTR "next_e!" -> PSL_NEXT_E_PLING
    | IDSTR "next_e" -> PSL_NEXT_E
    | IDSTR "next_event!" -> PSL_NEXT_EVENT_PLING
    | IDSTR "next_event" -> PSL_NEXT_EVENT
    | IDSTR "next_event_a!" -> PSL_NEXT_EVENT_A_PLING
    | IDSTR "next_event_a" -> PSL_NEXT_EVENT_A
    | IDSTR "next_event_e!" -> PSL_NEXT_EVENT_E_PLING
    | IDSTR "next_event_e" -> PSL_NEXT_EVENT_E
    | IDSTR "prev" -> PSL_PREV
    | IDSTR "property" -> PSL_PROPERTY
    | IDSTR "report" -> PSL_REPORT
    | IDSTR "restrict_guarantee" -> PSL_RESTRICT_GUARANTEE
    | IDSTR "rose" -> PSL_ROSE
    | IDSTR "sequence" -> PSL_SEQUENCE
    | IDSTR "stable" -> PSL_STABLE
    | IDSTR "union" -> PSL_UNION
    | IDSTR "until!" -> PSL_UNTIL
    | IDSTR "until!_" -> PSL_UNTIL_
    | IDSTR "vmode" -> PSL_VMODE
    | IDSTR "vprop" -> PSL_VPROP
    | IDSTR "vunit" -> PSL_VUNIT
    | IDSTR "within" -> PSL_WITHIN
    | IDSTR id -> TokenProp id
    | _ -> ktok)
    else ktok in
histcnt := (!histcnt+1)mod hsiz;
history.(!histcnt) <- {tok=ktok';strt=(Lexing.lexeme_start lexbuf);stop=(Lexing.lexeme_end lexbuf);key=true;psl=psl};
trace_log ktok';
ktok'
end

let _ = List.iter (fun (str,key) -> enter_keyword str key)
[
(  "$attribute",	D_ATTRIBUTE ) ;
(  "$bits",		D_BITS ) ;
(  "$clog2",		D_CLOG2 ) ;
(  "$countdrivers",	D_COUNTDRIVERS ) ;
(  "$countones",	D_COUNTONES ) ;
(  "$display",		D_DISPLAY ) ;
(  "$error",		D_ERROR ) ;
(  "$fatal",		D_FATAL ) ;
(  "$fclose",		D_FCLOSE ) ;
(  "$fdisplay",		D_FDISPLAY ) ;
(  "$feof",		D_FEOF ) ;
(  "$fflush",		D_FFLUSH ) ;
(  "$fgetc",		D_FGETC ) ;
(  "$fgets",		D_FGETS ) ;
(  "$finish",		D_FINISH ) ;
(  "$fopen",		D_FOPEN ) ;
(  "$fscanf",		D_FSCANF ) ;
(  "$fwrite",		D_FWRITE ) ;
(  "$fwriteh",		D_FWRITEH ) ;
(  "$hold",		D_HOLD ) ;
(  "$info",		D_INFO ) ;
(  "$isunknown",	D_ISUNKNOWN ) ;
(  "$monitor",		D_MONITOR ) ;
(  "$onehot",		D_ONEHOT ) ;
(  "$onehot0",		D_ONEHOT0 ) ;
(  "$period",		D_PERIOD ) ;
(  "$readmemb",		D_READMEMB ) ;
(  "$readmemh",		D_READMEMH ) ;
(  "$realtime",		D_TIME ) ;
(  "$recovery",		D_RECOVERY ) ;
(  "$recrem",		D_RECREM ) ;
(  "$removal",		D_REMOVAL ) ;
(  "$setup",		D_SETUP ) ;
(  "$setuphold",	D_SETUPHOLD ) ;
(  "$signed",		D_SIGNED ) ;
(  "$skew",		D_SKEW ) ;
(  "$sscanf",		D_SSCANF ) ;
(  "$stop",		D_STOP ) ;
(  "$test$plusargs",	D_TEST_PLUSARGS ) ;
(  "$time",		D_TIME ) ;
(  "$timeskew",		D_TIMESKEW ) ;
(  "$unsigned",		D_UNSIGNED ) ;
(  "$warning",		D_WARNING ) ;
(  "$width",		D_WIDTH ) ;
(  "$write",		D_WRITE ) ;
(  "always",		ALWAYS ) ;
(  "always_comb",	ALWAYS ) ;
(  "always_ff",		ALWAYS ) ;
(  "always_latch",	ALWAYS ) ;
(  "and",		AND ) ;
(  "assign",		ASSIGN ) ;
(  "automatic",		AUTOMATIC ) ;
(  "begin",		BEGIN ) ;
(  "buf",		BUF ) ;
(  "case",		CASE ) ;
(  "casex",		CASEX ) ;
(  "casez",		CASEZ ) ;
(  "clocking",		CLOCKING ) ;
(  "countones",		D_COUNTONES ) ;
(  "deassign",		DEASSIGN ) ;
(  "default",		DEFAULT ) ;
(  "defparam",		DEFPARAM ) ;
(  "disable",		DISABLE ) ;
(  "do",		DO ) ;
(  "edge",		EDGE ) ;
(  "else",		ELSE ) ;
(  "end",		END ) ;
(  "endcase",		ENDCASE ) ;
(  "endfunction",	ENDFUNCTION ) ;
(  "endgenerate",	ENDGENERATE ) ;
(  "endmodule",		ENDMODULE ) ;
(  "endprimitive",	ENDPRIMITIVE ) ;
(  "endspecify",	ENDSPECIFY ) ;
(  "endtable",		ENDTABLE ) ;
(  "endtask",		ENDTASK ) ;
(  "event",		EVENT ) ;
(  "final",		FINAL ) ;
(  "for",		FOR ) ;
(  "forever",		FOREVER ) ;
(  "function",		FUNCTION ) ;
(  "generate",		GENERATE ) ;
(  "genvar",		GENVAR ) ;
(  "if",		IF ) ;
(  "iff",		IFF ) ;
(  "ifnone",		IF_NONE ) ;
(  "initial",		INITIAL ) ;
(  "inout",		INOUT ) ;
(  "input",		INPUT ) ;
(  "integer",		INTEGER ) ;
(  "isunknown",		D_ISUNKNOWN ) ;
(  "localparam",	LOCALPARAM ) ;
(  "module",		MODULE ) ;
(  "nand",		NAND ) ;
(  "negedge",		NEGEDGE ) ;
(  "nmos",		NMOS ) ;
(  "nor",		NOR ) ;
(  "not",		NOT ) ;
(  "or",		OR ) ;
(  "output",		OUTPUT ) ;
(  "parameter",		PARAMETER ) ;
(  "pmos",		PMOS ) ;
(  "posedge",		POSEDGE ) ;
(  "primitive",		PRIMITIVE ) ;
(  "psl",               PSL ) ;
(  "pullup",		PULLUP ) ;
(  "real",		REAL ) ;
(  "reg",		REG ) ;
(  "repeat",		REPEAT ) ;
(  "signed",		SIGNED ) ;
(  "specify",		SPECIFY ) ;
(  "specparam",		SPECPARAM ) ;
(  "static",            STATIC ) ;
(  "supply0",		SUPPLY0 ) ;
(  "supply1",		SUPPLY1 ) ;
(  "table",		TABLE ) ;
(  "task",		TASK ) ;
(  "time",		TIME ) ;
(  "tran",		TRAN ) ;
(  "tri",		TRI ) ;
(  "tri0",		TRI0 ) ;
(  "tri1",		TRI1 ) ;
(  "true",		P_TRUE ) ;
(  "unsigned",		UNSIGNED ) ;
(  "uwire",		WIRE ) ;
(  "vectored",		VECTORED ) ;
(  "while",		WHILE ) ;
(  "wire",		WIRE ) ;
(  "xnor",		XNOR ) ;
(  "xor",		XOR ) ];;
}

let digit = ['0'-'9']
let state4 = ['0'-'1' 'x' 'z' '?' 'X' 'Z' 'b' 'B']
let ident = ['a'-'z' 'A'-'Z' '_']
let ident_num = ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']
let anything = [
   '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\007' '\008' '\009' '\010' '\011' '\012' '\013' '\014' '\015'
   '\016' '\017' '\018' '\019' '\020' '\021' '\022' '\023' '\024' '\025' '\026' '\027' '\028' '\029' '\030' '\031'
   '\032' '\033' '\034' '\035' '\036' '\037' '\038' '\039' '\040' '\041' '\042' '\043' '\044' '\045' '\046' '\047'
   '\048' '\049' '\050' '\051' '\052' '\053' '\054' '\055' '\056' '\057' '\058' '\059' '\060' '\061' '\062' '\063'
   '\064' '\065' '\066' '\067' '\068' '\069' '\070' '\071' '\072' '\073' '\074' '\075' '\076' '\077' '\078' '\079'
   '\080' '\081' '\082' '\083' '\084' '\085' '\086' '\087' '\088' '\089' '\090' '\091' '\092' '\093' '\094' '\095'
   '\096' '\097' '\098' '\099' '\100' '\101' '\102' '\103' '\104' '\105' '\106' '\107' '\108' '\109' '\110' '\111'
   '\112' '\113' '\114' '\115' '\116' '\117' '\118' '\119' '\120' '\121' '\122' '\123' '\124' '\125' '\126' '\127'
   '\128' '\129' '\130' '\131' '\132' '\133' '\134' '\135' '\136' '\137' '\138' '\139' '\140' '\141' '\142' '\143'
   '\144' '\145' '\146' '\147' '\148' '\149' '\150' '\151' '\152' '\153' '\154' '\155' '\156' '\157' '\158' '\159'
   '\160' '\161' '\162' '\163' '\164' '\165' '\166' '\167' '\168' '\169' '\170' '\171' '\172' '\173' '\174' '\175'
   '\176' '\177' '\178' '\179' '\180' '\181' '\182' '\183' '\184' '\185' '\186' '\187' '\188' '\189' '\190' '\191'
   '\192' '\193' '\194' '\195' '\196' '\197' '\198' '\199' '\200' '\201' '\202' '\203' '\204' '\205' '\206' '\207'
   '\208' '\209' '\210' '\211' '\212' '\213' '\214' '\215' '\216' '\217' '\218' '\219' '\220' '\221' '\222' '\223'
   '\224' '\225' '\226' '\227' '\228' '\229' '\230' '\231' '\232' '\233' '\234' '\235' '\236' '\237' '\238' '\239'
   '\240' '\241' '\242' '\243' '\244' '\245' '\246' '\247' '\248' '\249' '\250' '\251' '\252' '\253' '\254' '\255' ]
let anything_but_blank = [
    '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\007' '\008' '\009' '\010' '\011' '\012' '\013' '\014' '\015'
    '\016' '\017' '\018' '\019' '\020' '\021' '\022' '\023' '\024' '\025' '\026' '\027' '\028' '\029' '\030' '\031'
           '\033' '\034' '\035' '\036' '\037' '\038' '\039' '\040' '\041' '\042' '\043' '\044' '\045' '\046' '\047'
    '\048' '\049' '\050' '\051' '\052' '\053' '\054' '\055' '\056' '\057' '\058' '\059' '\060' '\061' '\062' '\063'
    '\064' '\065' '\066' '\067' '\068' '\069' '\070' '\071' '\072' '\073' '\074' '\075' '\076' '\077' '\078' '\079'
    '\080' '\081' '\082' '\083' '\084' '\085' '\086' '\087' '\088' '\089' '\090' '\091' '\092' '\093' '\094' '\095'
    '\096' '\097' '\098' '\099' '\100' '\101' '\102' '\103' '\104' '\105' '\106' '\107' '\108' '\109' '\110' '\111'
    '\112' '\113' '\114' '\115' '\116' '\117' '\118' '\119' '\120' '\121' '\122' '\123' '\124' '\125' '\126' '\127'
    '\128' '\129' '\130' '\131' '\132' '\133' '\134' '\135' '\136' '\137' '\138' '\139' '\140' '\141' '\142' '\143'
    '\144' '\145' '\146' '\147' '\148' '\149' '\150' '\151' '\152' '\153' '\154' '\155' '\156' '\157' '\158' '\159'
    '\160' '\161' '\162' '\163' '\164' '\165' '\166' '\167' '\168' '\169' '\170' '\171' '\172' '\173' '\174' '\175'
    '\176' '\177' '\178' '\179' '\180' '\181' '\182' '\183' '\184' '\185' '\186' '\187' '\188' '\189' '\190' '\191'
    '\192' '\193' '\194' '\195' '\196' '\197' '\198' '\199' '\200' '\201' '\202' '\203' '\204' '\205' '\206' '\207'
    '\208' '\209' '\210' '\211' '\212' '\213' '\214' '\215' '\216' '\217' '\218' '\219' '\220' '\221' '\222' '\223'
    '\224' '\225' '\226' '\227' '\228' '\229' '\230' '\231' '\232' '\233' '\234' '\235' '\236' '\237' '\238' '\239'
    '\240' '\241' '\242' '\243' '\244' '\245' '\246' '\247' '\248' '\249' '\250' '\251' '\252' '\253' '\254' '\255' ]
let anything_but_newline = [
   '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\007' '\008' '\009'        '\011' '\012' '\013' '\014' '\015'
   '\016' '\017' '\018' '\019' '\020' '\021' '\022' '\023' '\024' '\025' '\026' '\027' '\028' '\029' '\030' '\031'
   '\032' '\033' '\034' '\035' '\036' '\037' '\038' '\039' '\040' '\041' '\042' '\043' '\044' '\045' '\046' '\047'
   '\048' '\049' '\050' '\051' '\052' '\053' '\054' '\055' '\056' '\057' '\058' '\059' '\060' '\061' '\062' '\063'
   '\064' '\065' '\066' '\067' '\068' '\069' '\070' '\071' '\072' '\073' '\074' '\075' '\076' '\077' '\078' '\079'
   '\080' '\081' '\082' '\083' '\084' '\085' '\086' '\087' '\088' '\089' '\090' '\091' '\092' '\093' '\094' '\095'
   '\096' '\097' '\098' '\099' '\100' '\101' '\102' '\103' '\104' '\105' '\106' '\107' '\108' '\109' '\110' '\111'
   '\112' '\113' '\114' '\115' '\116' '\117' '\118' '\119' '\120' '\121' '\122' '\123' '\124' '\125' '\126' '\127'
   '\128' '\129' '\130' '\131' '\132' '\133' '\134' '\135' '\136' '\137' '\138' '\139' '\140' '\141' '\142' '\143'
   '\144' '\145' '\146' '\147' '\148' '\149' '\150' '\151' '\152' '\153' '\154' '\155' '\156' '\157' '\158' '\159'
   '\160' '\161' '\162' '\163' '\164' '\165' '\166' '\167' '\168' '\169' '\170' '\171' '\172' '\173' '\174' '\175'
   '\176' '\177' '\178' '\179' '\180' '\181' '\182' '\183' '\184' '\185' '\186' '\187' '\188' '\189' '\190' '\191'
   '\192' '\193' '\194' '\195' '\196' '\197' '\198' '\199' '\200' '\201' '\202' '\203' '\204' '\205' '\206' '\207'
   '\208' '\209' '\210' '\211' '\212' '\213' '\214' '\215' '\216' '\217' '\218' '\219' '\220' '\221' '\222' '\223'
   '\224' '\225' '\226' '\227' '\228' '\229' '\230' '\231' '\232' '\233' '\234' '\235' '\236' '\237' '\238' '\239'
   '\240' '\241' '\242' '\243' '\244' '\245' '\246' '\247' '\248' '\249' '\250' '\251' '\252' '\253' '\254' '\255' ]
let anything_but_quote = [
   '\000' '\001' '\002' '\003' '\004' '\005' '\006' '\007' '\008' '\009' '\010' '\011' '\012' '\013' '\014' '\015'
   '\016' '\017' '\018' '\019' '\020' '\021' '\022' '\023' '\024' '\025' '\026' '\027' '\028' '\029' '\030' '\031'
   '\032' '\033'        '\035' '\036' '\037' '\038' '\039' '\040' '\041' '\042' '\043' '\044' '\045' '\046' '\047'
   '\048' '\049' '\050' '\051' '\052' '\053' '\054' '\055' '\056' '\057' '\058' '\059' '\060' '\061' '\062' '\063'
   '\064' '\065' '\066' '\067' '\068' '\069' '\070' '\071' '\072' '\073' '\074' '\075' '\076' '\077' '\078' '\079'
   '\080' '\081' '\082' '\083' '\084' '\085' '\086' '\087' '\088' '\089' '\090' '\091' '\092' '\093' '\094' '\095'
   '\096' '\097' '\098' '\099' '\100' '\101' '\102' '\103' '\104' '\105' '\106' '\107' '\108' '\109' '\110' '\111'
   '\112' '\113' '\114' '\115' '\116' '\117' '\118' '\119' '\120' '\121' '\122' '\123' '\124' '\125' '\126' '\127'
   '\128' '\129' '\130' '\131' '\132' '\133' '\134' '\135' '\136' '\137' '\138' '\139' '\140' '\141' '\142' '\143'
   '\144' '\145' '\146' '\147' '\148' '\149' '\150' '\151' '\152' '\153' '\154' '\155' '\156' '\157' '\158' '\159'
   '\160' '\161' '\162' '\163' '\164' '\165' '\166' '\167' '\168' '\169' '\170' '\171' '\172' '\173' '\174' '\175'
   '\176' '\177' '\178' '\179' '\180' '\181' '\182' '\183' '\184' '\185' '\186' '\187' '\188' '\189' '\190' '\191'
   '\192' '\193' '\194' '\195' '\196' '\197' '\198' '\199' '\200' '\201' '\202' '\203' '\204' '\205' '\206' '\207'
   '\208' '\209' '\210' '\211' '\212' '\213' '\214' '\215' '\216' '\217' '\218' '\219' '\220' '\221' '\222' '\223'
   '\224' '\225' '\226' '\227' '\228' '\229' '\230' '\231' '\232' '\233' '\234' '\235' '\236' '\237' '\238' '\239'
   '\240' '\241' '\242' '\243' '\244' '\245' '\246' '\247' '\248' '\249' '\250' '\251' '\252' '\253' '\254' '\255' ]

rule token = parse
|  '\\'anything_but_blank+' ' as word {IDSTR (String.sub word 1 (String.length word -2)) }
(*
|  "//"anything_but_newline* as comment {trace_log (ASCNUM comment); token lexbuf}
| "/*"
    { comment (Lexing.lexeme_start lexbuf) lexbuf; token lexbuf }
| "(*"
    { comment2 (Lexing.lexeme_start lexbuf) lexbuf; token lexbuf }
| "//"
    { comment3 (Lexing.lexeme_start lexbuf) lexbuf; token lexbuf }
|  "//ABC"anything_but_newline*'\n' as abc {hlog lexbuf (ABC (String.sub abc 5 (String.length abc -5))) }
*)
|  "bufif"digit+ as def		{ hlog lexbuf (BUFIF def) }
|  "notif"digit+ as def		{ hlog lexbuf (NOTIF def) }
|  "tranif"digit+ as def	{ hlog lexbuf (TRANIF def) }
|  "(weak"digit+ as strength	{ hlog lexbuf (PWEAK strength ) }
|  "weak"digit+ as strength	{ hlog lexbuf (WEAK strength ) }
|  "(strong"digit+ as strength	{ hlog lexbuf (PSTRONG strength ) }
|  "strong"digit+ as strength	{ hlog lexbuf (STRONG strength ) }
|  "&&"			{ hlog lexbuf (P_ANDAND) }
|  "&&&"		{ hlog lexbuf (P_ANDANDAND) }
|  "&="			{ hlog lexbuf (P_ANDEQ) }
|  "*>"                 { hlog lexbuf (P_ASTGT) }
|  "@@"			{ hlog lexbuf (P_ATAT) }
|  "==="		{ hlog lexbuf (P_CASEEQUAL) }
|  "!=="		{ hlog lexbuf (P_CASENOTEQUAL) }
|  "::"			{ hlog lexbuf (P_COLONCOLON) }
|  ":/"			{ hlog lexbuf (P_COLONDIV) }
|  ":="			{ hlog lexbuf (P_COLONEQ) }
|  "/="			{ hlog lexbuf (P_DIVEQ) }
|  ".*"			{ hlog lexbuf (P_DOTSTAR) }
|  "=>"                 { hlog lexbuf (P_EQGT) }
|  "=="			{ hlog lexbuf (P_EQUAL) }
|  ">="			{ hlog lexbuf (P_GTE) }
|  "<="			{ hlog lexbuf (P_LTE) }
|  "-:"			{ hlog lexbuf (P_MINUSCOLON) }
|  "-="			{ hlog lexbuf (P_MINUSEQ) }
|  "->"			{ hlog lexbuf (P_MINUSGT) }
|  "%="			{ hlog lexbuf (P_MODEQ) }
|  "~&"			{ hlog lexbuf (P_NAND) }
|  "~|"			{ hlog lexbuf (P_NOR) }
|  "!="			{ hlog lexbuf (P_NOTEQUAL) }
|  "|="			{ hlog lexbuf (P_OREQ) }
|  "|=>"		{ hlog lexbuf (P_OREQGT) }
|  "|->"		{ hlog lexbuf (P_ORMINUSGT) }
|  "||"			{ hlog lexbuf (P_OROR) }
|  "+:"			{ hlog lexbuf (P_PLUSCOLON) }
|  "+="			{ hlog lexbuf (P_PLUSEQ) }
|  "##"			{ hlog lexbuf (P_POUNDPOUND) }
|  "**"			{ hlog lexbuf (P_POW) }
|  "<<<"		{ hlog lexbuf (P_SLEFT) }
|  "<<"			{ hlog lexbuf (P_SLEFT) }
|  "<<<="		{ hlog lexbuf (P_SLEFTEQ) }
|  "<<="		{ hlog lexbuf (P_SLEFTEQ) }
|  ">>"			{ hlog lexbuf (P_SRIGHT) }
|  ">>="		{ hlog lexbuf (P_SRIGHTEQ) }
|  ">>>"		{ hlog lexbuf (P_SSRIGHT) }
|  ">>>="		{ hlog lexbuf (P_SSRIGHTEQ) }
|  "*="			{ hlog lexbuf (P_TIMESEQ) }
|  "==?"		{ hlog lexbuf (P_WILDEQUAL) }
|  "!=?"		{ hlog lexbuf (P_WILDNOTEQUAL) }
|  "^~"			{ hlog lexbuf (P_XNOR) }
|  "~^"			{ hlog lexbuf (P_XNOR) }
|  "^="			{ hlog lexbuf (P_XOREQ) }
| "<>->"           { hlog lexbuf (TokenFby ) }
| "[]->"           { hlog lexbuf (TokenTrig ) }
| "<->->"          { hlog lexbuf (TokenBFby ) }
| "[-]->"          { hlog lexbuf (TokenBTrig ) }
| "<->"            { hlog lexbuf (TokenEq ) }
| "&" { hlog lexbuf (P_AMPERSAND) }
| "@" { hlog lexbuf (AT) }
| "^" { hlog lexbuf (P_CARET) }
| ":" { hlog lexbuf (COLON) }
| "," { hlog lexbuf (COMMA) }
| "/" { hlog lexbuf (DIVIDE) }
| "=" { hlog lexbuf (EQUALS) }
| ">" { hlog lexbuf (GREATER) }
| "#" { hlog lexbuf (HASH) }
| "[" { hlog lexbuf (LBRACK) }
| "{" { hlog lexbuf (LCURLY) }
| "<" { hlog lexbuf (LESS) }
| "(" { hlog lexbuf (LPAREN) }
| "-" { hlog lexbuf (MINUS) }
| "%" { hlog lexbuf (MODULO) }
| "." { hlog lexbuf (DOT) }
| "!" { hlog lexbuf (PLING) }
| "+" { hlog lexbuf (PLUS) }
| "?" { hlog lexbuf (QUERY) }
| "]" { hlog lexbuf (RBRACK) }
| "}" { hlog lexbuf (RCURLY) }
| ")" { hlog lexbuf (RPAREN) }
| ";" { hlog lexbuf (SEMICOLON) }
| "~" { hlog lexbuf (P_TILDE) }
| "*" { hlog lexbuf (TIMES) }
| "|" { hlog lexbuf (P_VBAR) }
| "$" ident ident_num* as word {
if Hashtbl.mem ksymbols word then let kw = Hashtbl.find ksymbols word in hlog lexbuf kw else hlog lexbuf (IDSTR word)
}
| "$" { hlog lexbuf (DOLLAR) }
| digit+'.'digit* as floatnum { hlog lexbuf ( FLOATNUM ( float_of_string floatnum ) ) }
| digit*'\''['b' 'B'][' ']*['0' '1' 'x' 'X' 'z' 'Z' '?' '_']+ as bnum { hlog lexbuf (BINNUM bnum ) }
| digit*'\''['o' 'O'][' ']*['0'-'7' 'x' 'X' 'z' 'Z' '?' '_']+ as onum { hlog lexbuf (OCTNUM onum ) }
| digit*'\''['d' 'D'][' ']*digit+ as dnum { hlog lexbuf (DECNUM dnum ) }
| digit*'\''['h' 'H'][' ']*['0'-'9' 'A'-'F' 'a'-'f' 'x' 'X' 'z' 'Z' '?' '_']+ as hnum { hlog lexbuf (HEXNUM hnum ) }
| digit+ as inum { hlog lexbuf (INTNUM inum ) }
| '\"'anything_but_quote*'\"' as asciinum { hlog lexbuf (ASCNUM asciinum ) }
| "`timescale" anything_but_newline+ as preproc { hlog lexbuf (P_TIMESCALE preproc) }
| '`'ident ident_num* as presym' { let presym = Bytes.of_string presym' in
  if Hashtbl.mem Globals.tsymbols presym then hlog lexbuf (Hashtbl.find Globals.tsymbols presym)
  else hlog lexbuf (PREPROC presym')
}
| ident ident_num* as word {
if Hashtbl.mem ksymbols word then hlog lexbuf (Hashtbl.find ksymbols word) else hlog lexbuf (IDSTR word)
}
  | [' ' '\t' ]		{token lexbuf }
  | ['\r' '\n' ]	{token lexbuf }
  | eof		{ hlog lexbuf (ENDOFFILE) }
  | _		{ hlog lexbuf (ILLEGAL ( lexeme_char lexbuf 0 ) ) }

(*
and comment start = parse
  "/*"
    { comment (Lexing.lexeme_start lexbuf) lexbuf; comment start lexbuf }
| "*/"
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated /* comment */ at offset %d." start) }
| _
    { comment start lexbuf }

and comment2 start = parse
  "(*"
    { comment2 (Lexing.lexeme_start lexbuf) lexbuf; comment2 start lexbuf }
| "*)"
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated (* comment *) at offset %d." start) }
| _
    { comment2 start lexbuf }

and comment3 start = parse
  "//"
    { comment3 (Lexing.lexeme_start lexbuf) lexbuf; comment3 start lexbuf }
| '\n'
    { () }
| eof
    { failwith (Printf.sprintf "Unterminated // comment at offset %d." start) }
| _
    { comment3 start lexbuf }
*)
