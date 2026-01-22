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

{
  open Rtlil_input

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
ACCEPT, "ACCEPT"; 
AMPERSAND, "AMPERSAND"; 
AT, "AT"; 
BACKQUOTE, "BACKQUOTE"; 
BACKSLASH, "BACKSLASH"; 
CARET, "CARET"; 
COLON, "COLON"; 
COMMA, "COMMA"; 
DEFAULT, "DEFAULT"; 
DOLLAR, "DOLLAR"; 
DOT, "DOT"; 
DOUBLEQUOTE, "DOUBLEQUOTE"; 
EMPTY_TOKEN, "EMPTY_TOKEN"; 
END, "END"; 
EOF_TOKEN, "EOF_TOKEN"; 
ERROR_TOKEN, "ERROR_TOKEN"; 
GREATER, "GREATER"; 
HASH, "HASH"; 
LBRACE, "LBRACE"; 
LBRACK, "LBRACK"; 
LESS, "LESS"; 
LINEFEED, "LINEFEED"; 
PERCENT, "PERCENT"; 
PLING, "PLING"; 
QUERY, "QUERY"; 
QUOTE, "QUOTE"; 
RBRACE, "RBRACE"; 
RBRACK, "RBRACK"; 
TILDE, "TILDE"; 
TOK_ALWAYS, "always"; 
TOK_ASSIGN, "assign"; 
TOK_ATTRIBUTE, "attribute"; 
TOK_AUTOIDX, "autoidx"; 
TOK_CASE, "case"; 
TOK_CELL, "cell"; 
TOK_CONNECT, "connect"; 
TOK_EDGE, "edge"; 
TOK_END, "end"; 
TOK_EOL, "eol"; 
TOK_GLOBAL, "global"; 
TOK_HIGH, "high"; 
TOK_INIT, "init"; 
TOK_INOUT, "inout"; 
TOK_INPUT, "input"; 
TOK_INVALID, "invalid"; 
TOK_LOW, "low"; 
TOK_MEMORY, "memory"; 
TOK_MEMWR, "memwr"; 
TOK_MODULE, "module"; 
TOK_NEGEDGE, "negedge"; 
TOK_OFFSET, "offset"; 
TOK_OUTPUT, "output"; 
TOK_PARAMETER, "parameter"; 
TOK_POSEDGE, "posedge"; 
TOK_PROCESS, "process"; 
TOK_REAL, "real"; 
TOK_SIGNED, "signed"; 
TOK_SIZE, "size"; 
TOK_SWITCH, "switch"; 
TOK_SYNC, "sync"; 
TOK_UPDATE, "update"; 
TOK_UPTO, "upto"; 
TOK_WIDTH, "width"; 
TOK_WIRE, "wire"; 
UNDERSCORE, "UNDERSCORE"; 
VBAR, "VBAR"; 
      ];
    fun s -> Hashtbl.find h s

let tok' x = "\""^Ord_input.getstr x^"\""

let import_seen = ref false

let tok arg =
  arg
}

let ident = ['a'-'z' 'A'-'Z' '$' '_' '\\'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$' '\\' '.' ':' '[' ']' '/' ]*
let sizednumber = ['0'-'9']+'\''[ '0'-'9' 'a'-'f' 'x' 'A'-'F' 'X' '_' '?']*
let number = ['0'-'9']['0'-'9' '_']*
let dfltnum = '''['0'-'9' 'b' 'd' 'h' 'x' 'X']['0'-'9' 'a'-'f' 'A'-'F' 'x' '?']*
let space = [' ' '\t' '\r']+
let newline = ['\n']
let qstring = '"'[^'"']*'"'
let comment = '#'[^'\n']*
   
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
(*
| '-' { tok ( HYPHEN ) }
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| '(' { tok ( LPAREN ) }
| ')' { tok ( RPAREN ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| '.' { tok ( DOT ) }
| '/' { tok ( SLASH ) }
| '\\' { tok ( BACKSLASH ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '@' { tok ( AT ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
*)

| "/*" { comment lexbuf }
| "(* " { comment lexbuf }

  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; tok TOK_EOL }
  | sizednumber as n
      { tok ( TOK_VALUE n) }
  | number as n
      { tok ( TOK_INT (int_of_string n) ) }
  | dfltnum as n
      { tok ( TOK_INT (int_of_string n) ) }
  | ident as s
      { tok ( try keyword s with Not_found -> TOK_ID s ) }
  | qstring as s
      { tok ( TOK_STRING (String.sub s 1 (String.length s - 2)) ) }
  | eof
      { tok ( EOF_TOKEN ) }

| _ as oth
{ tok ( failwith ("Input_lex: "^String.make 1 oth) ) }

and comment = parse
| newline { incr lincnt; comment lexbuf }
| '*''/' as com { if false then print_endline ("/*"^com); token lexbuf }
| '*'')' as com { if false then print_endline ("/*"^com); token lexbuf }
| _ { comment lexbuf }

(* pre-processing should have removed this stuff, but if not, skip over it *)
   
and ifdef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifdef lexbuf }
| _ { ifdef lexbuf }

and ifndef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifndef lexbuf }
| _ { ifndef lexbuf }
