{
(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*     File name:        VhdlLexer.mll                                       *)
(*     Description:      Lexer for the VHDL language.                        *)
(*                       VHDL subset for RTL synthesis + simulation helpers  *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(*   Copyright (C) 2008-2010 TIMA Laboratory - VDS Team                      *)
(*                                                                           *)
(*   This program is free software: you can redistribute it and/or modify    *)
(*   it under the terms of the GNU General Public License as published by    *)
(*   the Free Software Foundation, either version 3 of the License, or       *)
(*   (at your option) any later version.                                     *)
(*                                                                           *)
(*   This program is distributed in the hope that it will be useful,         *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*   GNU General Public License for more details.                            *)
(*                                                                           *)
(*   You should have received a copy of the GNU General Public License       *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.   *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(*     Author:      Florent Ouchet (Intern at TIMA-VDS)                      *)
(*     Version:     1.7 (April 2010)                                         *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(* $Rev:: 1431                                                             $ *)
(* $Author:: ouchet                                                        $ *)
(* $Date:: 2010-04-06 19:28:25 +0200 (Tue, 06 Apr 2010)                    $ *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

(* VhdlLexer.ml is generated from VhdlLexel.mll by ocamllex *)

(* the lexer will identify atomic pattern (reserved keywords or symbols) *)
(* and return an identifier for each known words *)
(* otherwise it will return an OtherString or OtherInt *)
(* these identifiers are declared in VhdlParser.mly *)

  open VhdlParser;;

(* this hashtable will make lexer state table smaller *)
(* it contains a list of conversions string -> lexer identifiers *)
  let vhdl_keywords = Hashtbl.create 111;;
  let initVhdlLexer =
    List.iter (fun (kwd, tok) -> Hashtbl.add vhdl_keywords kwd tok)
              [ "abs", Labs;
                "access", Laccess;
                "across", Lacross; (* might conflict with attribute name *)
                "after", Lafter;
                "alias", Lalias;
                "all", Lall;
                "and", Land;
                "architecture", Larchitecture;
                "array", Larray;
                "assert", Lassert;
                "assume", Lassume;
                "assume_guarantee", Lassumeguarantee;
                "attribute", Lattribute;
                "begin", Lbegin;
                "block", Lblock;
                "body", Lbody;
                "break", Lbreak;
                "buffer", Lbuffer;
                "bus", Lbus;
                "case", Lcase;
                "component", Lcomponent;
                "configuration", Lconfiguration;
                "constant", Lconstant;
                "context", Lcontext;
                "cover", Lcover;
                "disconnect", Ldisconnect;
                "downto", Ldownto;
                "else", Lelse;
                "elsif", Lelsif;
                "end", Lend;
                "entity", Lentity;
                "exit", Lexit;
                "fairness", Lfairness;
                "file", Lfile;
                "for", Lfor;
                "force", Lforce;
                "function", Lfunction;
                "generate", Lgenerate;
                "generic", Lgeneric;
                "group", Lgroup;
                "guarded", Lguarded;
                "if", Lif;
                "impure", Limpure;
                "in", Lin;
                "inertial", Linertial;
                "inout", Linout;
                "is", Lis;
                "label", Llabel;
                "library", Llibrary;
                "limit", Llimit;
                "linkage", Llinkage;
                "literal", Lliteral;
                "loop", Lloop;
                "map", Lmap;
                "mod", Lmod;
                "nand", Lnand;
                "nature", Lnature;
                "new", Lnew;
                "next", Lnext;
                "noise", Lnoise;
                "nor", Lnor;
                "not", Lnot;
                "null", Lnull;
                "of", Lof;
                "on", Lon;
                "open", Lopen;
                "or", Lor;
                "others", Lothers;
                "out", Lout;
                "package", Lpackage;
                "parameter", Lparameter;
                "port", Lport;
                "postponed", Lpostponed;
                "procedural", Lprocedural;
                "procedure", Lprocedure;
                "process", Lprocess;
                "property", Lproperty;
                "protected", Lprotected;
                "pure", Lpure;
                "quantity", Lquantity;
                "range", Lrange; (* might conflict with attribute name *)
                "record", Lrecord;
                "reference", Lreference;
                "register", Lregister;
                "reject", Lreject;
                "release", Lrelease;
                "rem", Lrem;
                "report", Lreport;
                "restrict", Lrestrict;
                "restrict_guarantee", Lrestrictguarantee;
                "return", Lreturn;
                "rol", Lrol;
                "ror", Lror;
                "select", Lselect;
                "sequence", Lsequence;
                "severity", Lseverity;
                "signal", Lsignal;
                "shared", Lshared;
                "sla", Lsla;
                "sll", Lsll;
                "spectrum", Lspectrum;
                "sra", Lsra;
                "srl", Lsrl;
                "strong", Lstrong;
                "subnature", Lsubnature;
                "subtype", Lsubtype;
                "terminal", Lterminal;
                "then", Lthen;
                "through", Lthrough;
                "to", Lto;
                "tolerance", Ltolerance;
                "transport", Ltransport;
                "type", Ltype;
                "unaffected", Lunaffected;
                "units", Lunits;
                "until", Luntil;
                "use", Luse;
                "variable", Lvariable;
                "vmode", Lvmode;
                "vprop", Lvprop;
                "vunit", Lvunit;
                "wait", Lwait;
                "when", Lwhen;
                "while", Lwhile;
                "with", Lwith;
                "xnor", Lxnor;
                "xor", Lxor ];;

  (* this function remove repeated characters in strings *)
  (* "foo" 'o' -> "fo" *)
  (* "foooo" 'o' -> "foo" *)
  (* pour simplifier, on a aussi "foaobc" 'o' -> "fooc" *)
  let strRemoveDup str ch =
    let last = -1 + String.length str in
      let rec strRemoveDupInt acc i =
        if i > last then
          acc
        else
        let currch = String.get str i in
          if (i < last) && (ch = currch) && (ch = String.get str (i+1)) then
            strRemoveDupInt (acc^String.make 1 currch) (i+2)
          else
            strRemoveDupInt (acc^String.make 1 currch) (i+1)
    in strRemoveDupInt "" 0;;
}
     
rule lexer = parse
   (* skip spaces *)
   [' ' '\t']
    { lexer lexbuf }

   (* skip UTF-8 BOM *)
 | '\xef''\xbb''\xbf'
    { Lbomutf8 }

   (* skip UTF-7 BOM *)
 | '\x2b''\x2f''\x76'('\x38'|'\x39'|'\x2B'|'\x2F'|'\x38''\x2D')
    { Lbomutf7 }

   (* skip comment *)
 | "--" [^ '\n']*
    { lexer lexbuf }

   (* skip DOS-style end-of-line *)
 | '\r' '\n'
    { lexer lexbuf }

   (* skip end-of-line *)
 | '\n'
    { lexer lexbuf }

 | '`'"protect begin_protected"
    { Lprotected }

   (* match identifiers and find a reserved keyword *)
   (* identifiers can start with a letter (ISO 8859 - also known as Latin-1): *)
   (*   ABCDEFGHIJKLMNOPQRSTUVWXYZ������������������������������ *)
   (*   abcdefghijklmnopqrstuvwxyz�������������������������������� *)
   (* next characters may be letters or underscore or digits: 0123456789 *)
 | ['A'-'Z' 'a'-'z' '\xC0'-'\xD6' '\xD8'-'\xF6' '\xF8'-'\xFF'] ['A'-'Z' 'a'-'z' '\xC0'-'\xD6' '\xD8'-'\xF6' '\xF8'-'\xFF' '_' '0'-'9']* as id
    { try
        let lowerid = String.lowercase_ascii id in
          Hashtbl.find vhdl_keywords lowerid
      with Not_found ->
        Lword (id, Lexing.lexeme_start lexbuf) }

   (* match escaped identifiers, can contain any characters *)
 | '\\' ([^ '\\' '\n']?"\\\\"?)+ '\\' as id
    { Lword (id, Lexing.lexeme_start lexbuf) }

   (* match integer numbers *)
 | (['0'-'9']['0'-'9' '_']* as strint)
    { Lint ("",strint,"",Lexing.lexeme_start lexbuf) }
 | (['0'-'9']['0'-'9' '_']* as strint)['e' 'E']('+'?)(['0'-'9']['0'-'9' '_']* as strexp)
    { Lint ("",strint,strexp,Lexing.lexeme_start lexbuf) }
 | (['0'-'9']['0'-'9' '_']* as strradix)'#'(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strint)'#'
    { Lint (strradix,strint,"",Lexing.lexeme_start lexbuf) }
 | (['0'-'9']['0'-'9' '_']* as strradix)'#'(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strint)'#'['e' 'E']('+'?)(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strexp)
    { Lint (strradix,strint,strexp,Lexing.lexeme_start lexbuf) }

   (* match float numbers *)
 | (['0'-'9']['0'-'9' '_']* as strint)'.'(['0'-'9']['0'-'9' '_']* as strfrac)
    { Lfloat ("",strint,strfrac,"",Lexing.lexeme_start lexbuf) }
 | (['0'-'9']['0'-'9' '_']* as strint)'.'(['0'-'9']['0'-'9' '_']* as strfrac)['e' 'E'](['+' '-']?['0'-'9']['0'-'9' '_']* as strexp)
    { Lfloat ("",strint,strfrac,strexp,Lexing.lexeme_start lexbuf) }
 | (['0'-'9']['0'-'9' '_']* as strradix)'#'(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strint)'.'(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strfrac)'#'
    { Lfloat (strradix,strint,strfrac,"",Lexing.lexeme_start lexbuf) }
 | (['0'-'9']['0'-'9' '_']* as strradix)'#'(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strint)'.'(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strfrac)'#'['e' 'E'](['+' '-']? as expsign)(['0'-'9' 'a'-'f' 'A'-'F']['0'-'9' '_' 'a'-'f' 'A'-'F']* as strexp)
    { if expsign = "+" then
        Lfloat (strradix,strint,strfrac,strexp,Lexing.lexeme_start lexbuf)
      else
        Lfloat (strradix,strint,strfrac,expsign^strexp,Lexing.lexeme_start lexbuf) }

   (* match string constants *)
 | '"' ( ([^ '"' '\n']?"\"\""?)* as str) '"'
    { let vhdlstr = strRemoveDup str '"' in
        Lstring (vhdlstr,Lexing.lexeme_start lexbuf) }
   (* % is allowed as a string delimiter in VHDL 2007 *)
 | '%' ( ([^ '"' '%' '\n']?"%%"?)* as str) '%'
    { let vhdlstr = strRemoveDup str '%' in
        Lstring (vhdlstr,Lexing.lexeme_start lexbuf) }

   (* match character constants *)
 | ''' ([^ '\n'] as str) '''
    { Lchar (str,Lexing.lexeme_start lexbuf) }

   (* match delimiters *)
 | '&'
    { Lampersand }
 | '''
    { Lquote }
 | '('
    { Lleftparenthesis }
 | ')'
    { Lrightparenthesis }
   (* "**" must be matched before "*" *)
 | "**"
    { Lexponential }
 | '*'
    { Lmultiply }
 | '+'
    { Lplus }
 | '-'
    { Lminus }
 | '.'
    { Ldot }
   (* "/=" must be matched before "/" *)
 | "/="
    { Lnotequal }
 | '/'
    { Lslash }
 | ':'
    { Lcolon }
 | ';'
    { Lsemicolon }
 | '<'
    { Lless }
 | '='
    { Lequal }
 | '>'
    { Lgreater }
 | '|'
    { Lverticalbar }
 | '['
    { Lleftbracket }
 | ']'
    { Lrightbracket }
 | '%'
    { Lpercent }
 | '!'
    { Lexclamationmark }
 | '?'
    { Lquestionmark }
 | ','
    { Lcomma }
 | "=>"
    { Lassociation }
 | ":="
    { Limmediate }
 | eof
    { Leof }
 | "?="
    { Lmatchingequal }
 | "?/="
    { Lmatchingnotequal }
 | "?<"
    { Lmatchingless }
 | "?<="
    { Lmatchinglessequal }
 | "?>"
    { Lmatchinggreater }
 | "?>="
    { Lmatchinggreaterequal }

and protected = parse
   eof
    { Leof }
 | '`'"protect end_protected"
    { Lprotected }

 | [^ '\n']* '\n'
    { protected lexbuf }
