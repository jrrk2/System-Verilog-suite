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
  open Liberty

  let verbose = ref false
  let lincnt = ref 0

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
      KW_DEFINE, "KW_DEFINE"; 
      KW_DEFINE_GROUP, "KW_DEFINE_GROUP"; 
      KW_FALSE, "KW_FALSE"; 
      KW_TRUE, "KW_TRUE"; 
      ];
    fun s -> Hashtbl.find h s

let tok arg = arg
}

let ident = ['a'-'z' 'A'-'Z' '_' ] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let number = ['-']* ['0'-'9'] ['0'-'9' '.' 'e' '-']*
let space = [' ' '\t' '\r']+
let newline = ['\n']
let begincomment = '/' '*'
let endcomment = '*' '/'
let string = '"' [^ '"']* '"'
let escaped = '\\' '\n'

rule token = parse
  | begincomment
      { comment lexbuf }
  | space
      { token lexbuf }
  | newline
      { incr lincnt; token lexbuf }
  | escaped
      { incr lincnt; token lexbuf }
  | '\''
      { tok ( QUOTE ) }
  | number as n
      { tok ( NUM n ) }
  | string as s
      { tok ( STRING (String.sub s 1 (String.length s - 2) ) ) }
  | ident as s
      { tok ( try keyword s with Not_found -> IDENT s ) }
  | eof
      { tok ( EOF_TOKEN ) }
| '!'
{ tok ( PLING ) }

| '"'
{ tok ( DOUBLEQUOTE ) }

| '#'
{ tok ( HASH ) }

| '$'
{ tok ( DOLLAR ) }

| '%'
{ tok ( PERCENT ) }

| '&'
{ tok ( AMPERSAND ) }

| '''
{ tok ( QUOTE ) }

| '('
{ tok ( LPAR ) }

| '['
{ tok ( LBRACK ) }

| '{'
{ tok ( LCURLY ) }

| '<'
{ tok ( LESS ) }

| ')'
{ tok ( RPAR ) }

| ']'
{ tok ( RBRACK ) }

| '}'
{ tok ( RCURLY ) }

| '>'
{ tok ( GREATER ) }

| '*'
{ tok ( MULT ) }

| '+'
{ tok ( PLUS ) }

| ','
{ tok ( COMMA ) }

| '-'
{ tok ( MINUS ) }

| '.'
{ tok ( DOT ) }

| '/'
{ tok ( DIV ) }

| '\\'
{ tok ( BACKSLASH ) }

| ':'
{ tok ( COLON ) }

| ';'
{ tok ( SEMI ) }

| '='
{ tok ( EQ ) }

| '?'
{ tok ( QUERY ) }

| '@'
{ tok ( AT ) }

| '^'
{ tok ( CARET ) }

| '_'
{ tok ( UNDERSCORE ) }

| '`'
{ tok ( BACKQUOTE ) }

| '|'
{ tok ( VBAR ) }

| '~'
{ tok ( TILDE ) }

| _ as c
      { tok ( STRING (String.make 1 c) ) }

and comment = parse
  | endcomment { token lexbuf }
  | _ { comment lexbuf }
