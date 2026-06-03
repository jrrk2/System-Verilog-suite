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
  open Edif2

  exception Error of string
  
  let hsiz = 64;;

  type hist = {tok:token;strt:int;stop:int;key:bool;};;

  let histcnt = ref 0;;

  let history = Array.init hsiz (fun i-> ref {tok=EMPTYEDIF;strt=0;stop=0;key=false});;

  let hlog lexbuf ktok:token = begin
  history.(!histcnt) := {tok=ktok;strt=(Lexing.lexeme_start lexbuf);stop=(Lexing.lexeme_end lexbuf);key=true};
  ignore(histcnt := (!histcnt+1)mod hsiz);
  (*
  Printf.printf "%s\n" (str_token (ktok));
  *)
  ktok
  end
}

rule token = parse
| [' ' '\t' '\n']
    { token lexbuf }
| ['0'-'9']+ '.' ['0'-'9']+ as f
    { hlog lexbuf ( FLT (float_of_string f) ) }
| ['0'-'9']+ as i
    { hlog lexbuf ( INT (int_of_string i) ) }
| '('
    { hlog lexbuf ( LPAREN ) }
| ')'
    { hlog lexbuf ( RPAREN ) }
| ','
    { hlog lexbuf ( COMMA ) }
| '|'
    { hlog lexbuf ( VBAR ) }
| '&'
    { hlog lexbuf ( AMPERSAND ) }
| ':'
    { hlog lexbuf ( COLON ) }
| '\''
    { hlog lexbuf ( QUOTE ) }
| '/'
    { hlog lexbuf ( SLASH ) }
| '/' '/' [^ '\n'] * as c
    { hlog lexbuf ( COMMENT c ) }
| '\\' [^ ' '] * ' ' as s
    { hlog lexbuf ( ID (let len = String.length s in if s.[len - 2] = '\n' then String.sub s 0 (len-2)^" " else s ) ) }
| '(' '*' 
    { hlog lexbuf ( BEGINPRAGMA ) }
| '*' ')' 
    { hlog lexbuf ( ENDPRAGMA ) }
| '`' [^ '\n'] * as m
    { hlog lexbuf ( MACRO m ) }
| '.'
    { hlog lexbuf ( DOT ) }
| '='
    { hlog lexbuf ( EQUALS ) }
| "<="
    { hlog lexbuf ( BECOMES ) }
| ';'
    { hlog lexbuf ( SEMI ) }
| '#'
    { hlog lexbuf ( HASH_ ) }
| '@'
    { hlog lexbuf ( AT ) }
| '~'
    { hlog lexbuf ( TILDE ) }
| '?'
    { hlog lexbuf ( QUERY ) }
| '['
    { hlog lexbuf ( LBRACK ) }
| ']'
    { hlog lexbuf ( RBRACK ) }
| '{'
    { hlog lexbuf ( LCURLY ) }
| '}'
    { hlog lexbuf ( RCURLY ) }
| '"'[ ' ' '^' '<' '=' '>' '_' '-' ',' ';' ':' '!' '?' '/' '.' '`' '\'' '(' ')' '[' ']' '{' '}' '@' '$' '*' '\\' '#' '%' '+' '0'-'9' 'a'-'z' 'A'-'Z' ]*'"' as ascii { hlog lexbuf (STRING ascii ) }
| ['a'-'z' 'A'-'Z' '_' '&'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']* as word {
(* Case-insensitive EDIF keyword lookup: see eord.ml's case-fold note.   *)
let key = String.lowercase_ascii word in
if Hashtbl.mem Eord.esymbols key then hlog lexbuf (Hashtbl.find Eord.esymbols key)
else hlog lexbuf (ID word)
}
| eof
    { hlog lexbuf ( ENDOFFILE ) }
| _
    { hlog lexbuf ( ILLEGAL (Lexing.lexeme_char lexbuf 0) ) }
