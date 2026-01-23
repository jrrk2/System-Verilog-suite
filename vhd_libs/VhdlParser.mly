%{
(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*     File name:        VhdlParser.mly                                      *)
(*     Description:      Parser for the VHDL language.                       *)
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

  (* VhdlParser.ml is auto-generated from VhdlParser.mly by ocamlyacc *)

  open VhdlTypes;;
(*  open VhdlUtils;; *)

(* create an expression from a relation *)
let relation_of_logical_expression logical_expression =
  AtomRelation (
    AtomShiftExpression (
      AtomSimpleExpression (
        AtomTerm (
          AtomFactor (
	    AtomDotted (
            ParenthesedPrimary (
              AtomExpression logical_expression
  ) ) ) ) ) ) );;


let term_of_simple_expression simple_expression =
  AtomTerm (
    AtomFactor (
      AtomDotted (
      ParenthesedPrimary (
        AtomExpression (
          AtomLogicalExpression (
            AtomRelation (
              AtomShiftExpression simple_expression
  ) ) ) ) ) ) );;

let factor_of_term term =
  AtomFactor (
    AtomDotted (
    ParenthesedPrimary (
      AtomExpression (
        AtomLogicalExpression (
          AtomRelation (
            AtomShiftExpression (
              AtomSimpleExpression term
  ) ) ) ) ) ) );;

let primary_of_factor factor =
  ParenthesedPrimary (
    AtomExpression (
      AtomLogicalExpression (
        AtomRelation (
          AtomShiftExpression (
            AtomSimpleExpression (
              AtomTerm factor
  ) ) ) ) ) );;

let expression_of_primary primary =
  AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm (
              AtomFactor primary
  ) ) ) ) ) );;

let expression_of_factor factor =
  AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression (
            AtomTerm factor
  ) ) ) ) );;

let expression_of_term term =
  AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression (
          AtomSimpleExpression term
  ) ) ) );;

let expression_of_simple_expression simple_expression =
  AtomExpression (
    AtomLogicalExpression (
      AtomRelation (
        AtomShiftExpression simple_expression
  ) ) );;

let expression_of_shift_expression shift_expression =
  AtomExpression (
    AtomLogicalExpression (
      AtomRelation shift_expression
  ) );;

let expression_of_relation relation =
  AtomExpression (
    AtomLogicalExpression relation
  );;

let expression_of_logical_expression logical_expression =
  AtomExpression logical_expression;;

(* extract a position from an identifier *)
let position_of_identifier (identifier: vhdl_identifier) =
  snd identifier;;

(* extract a position from a name *)
let rec position_of_name (name: vhdl_name) =
  match name with
    | SimpleName sn ->
        position_of_identifier sn;
    | OperatorString os ->
        snd os;
    | SelectedName pnlst ->
        List.hd (List.map (function (SuffixSimpleName itm) -> position_of_name itm | _ -> 0) pnlst)
    | AttributeName {attributeprefix=(SuffixSimpleName pn); attribute=a} ->
        position_of_name pn
    | AttributeName {attributeprefix=(SuffixOpSymbol _|SuffixCharLiteral _|SuffixAll)} -> 0
    | SubscriptName(sn,_)
        -> position_of_identifier sn

  (* this function remove underscores from strings *)
  (* 123_345 -> 123456 *)
  let strRemoveUnderscore str =
    let rec strRemoveUnderscoreInt acc str =
      let len = String.length str in
        try
          let posunderscore = (String.index str '_') in
            (*if posunderscore = (len - 1) then
              acc^(String.sub str 0 posunderscore)
            else*)
              strRemoveUnderscoreInt (acc^(String.sub str 0 posunderscore)) (String.sub str (posunderscore+1) (len-posunderscore-1))
        with Not_found ->
          acc^str
      in strRemoveUnderscoreInt "" str;;

  (* this function computes power of base, base being an big_int *)
  let power_big_int (base: Big_int.big_int) (exp: Big_int.big_int) : Big_int.big_int =
    if (Big_int.sign_big_int exp) > 0 then
      let rec power_intp acc count =
        if (Big_int.sign_big_int count) > 0 then
          power_intp (Big_int.mult_big_int acc base) (Big_int.pred_big_int count)
        else
          acc
      in power_intp Big_int.unit_big_int exp
    else
    if (Big_int.sign_big_int exp) < 0 then
      let rec power_intn acc count =
        if (Big_int.sign_big_int count) < 0 then
          power_intn (Big_int.div_big_int acc base) (Big_int.succ_big_int count)
        else
          acc
      in power_intn Big_int.unit_big_int exp
    else
      Big_int.unit_big_int;;

  (* this function computes power of base, base being a float *)
  let power_float (base:float) (exp:Big_int.big_int) =
    if (Big_int.sign_big_int exp) > 0 then
      let rec power_float1 (acc:float) (count:Big_int.big_int) =
        if (Big_int.sign_big_int count) > 0 then
          power_float1 (acc*.base) (Big_int.pred_big_int count)
        else
          acc
      in power_float1 1.0 exp
    else
    if (Big_int.sign_big_int exp) < 0 then
      let rec power_float2 (acc:float) (count:Big_int.big_int) =
        if (Big_int.sign_big_int count) < 0 then
          power_float2 (acc/.base) (Big_int.succ_big_int count)
        else
          acc
      in power_float2 1.0 exp
    else
      1.0;;

  (* this function converts a string representing a number in specified base to a big_int *)
  let big_int_of_string_radix str (radix: Big_int.big_int) =
    let big_int_of_char c = Big_int.big_int_of_int (int_of_string ("0x"^(String.make 1 c))) in
      let rec big_int_of_string_radix_int (acc: Big_int.big_int) str =
        if str = "" then
          acc
        else
          let c = big_int_of_char (String.get str 0) in
            if Big_int.lt_big_int c radix then
              big_int_of_string_radix_int (Big_int.add_big_int (Big_int.mult_big_int acc radix) c) (String.sub str 1 (-1 + String.length str))
            else
              raise (Failure "big_int_of_string_radix")
    in big_int_of_string_radix_int Big_int.zero_big_int str;;

  (* this function converts a string representing a number in base 10 to a big_int *)
  let big_int_of_string str =
    Big_int.big_int_of_string str;;

  (* big_int 2 *)
  let big_int_2 =
    Big_int.big_int_of_int 2;;

  (* this function validates decimal bit strings and converts them to a string *)
  let convert_dec_string str =
    let rec convert_dec_string_int acc num =
      match Big_int.sign_big_int num,acc,Big_int.quomod_big_int num big_int_2 with
      | 0,"",_
          -> "0"; (* at least one digit *)
      | 0,_,_
          -> acc;
      | _,_,(next_num,remainder)
          when (Big_int.sign_big_int remainder) = 0
          -> convert_dec_string_int ("0" ^ acc) next_num;
      | _,_,(next_num,_)
          -> convert_dec_string_int ("1" ^ acc) next_num;
    in convert_dec_string_int "" (Big_int.big_int_of_string str);;

  (* this function validates binary bit strings and converts them to a string *)
  let convert_bin_string str =
    let rec convert_bin_string_int acc index =
      if index < 0 then
        acc
      else
        match String.get str index with
        | '0' -> convert_bin_string_int ("0" ^ acc) (index-1);
        | '1' -> convert_bin_string_int ("1" ^ acc) (index-1);
        | ch  -> convert_bin_string_int ((String.make 1 ch) ^ acc) (index-1);
    in convert_bin_string_int "" (-1 + String.length str);;

  (* this function validates bit strings, 8-based, and converts them to a string *)
  let convert_oct_string str =
    let rec convert_oct_string_int acc index =
      if index < 0 then
        acc
      else
        match String.get str index with
        | '0' -> convert_oct_string_int ("000" ^ acc) (index-1);
        | '1' -> convert_oct_string_int ("001" ^ acc) (index-1);
        | '2' -> convert_oct_string_int ("010" ^ acc) (index-1);
        | '3' -> convert_oct_string_int ("011" ^ acc) (index-1);
        | '4' -> convert_oct_string_int ("100" ^ acc) (index-1);
        | '5' -> convert_oct_string_int ("101" ^ acc) (index-1);
        | '6' -> convert_oct_string_int ("110" ^ acc) (index-1);
        | '7' -> convert_oct_string_int ("111" ^ acc) (index-1);
        | ch  -> convert_oct_string_int ((String.make 3 ch) ^ acc) (index-1);
    in convert_oct_string_int "" (-1 + String.length str);;

  (* this function validates bit strings, 16-based, and converts them to a string *)
  let convert_hex_string str =
    let rec convert_hex_string_int acc index =
      if index < 0 then
        acc
      else
        match String.get str index with
        | '0' -> convert_hex_string_int ("0000" ^ acc) (index-1);
        | '1' -> convert_hex_string_int ("0001" ^ acc) (index-1);
        | '2' -> convert_hex_string_int ("0010" ^ acc) (index-1);
        | '3' -> convert_hex_string_int ("0011" ^ acc) (index-1);
        | '4' -> convert_hex_string_int ("0100" ^ acc) (index-1);
        | '5' -> convert_hex_string_int ("0101" ^ acc) (index-1);
        | '6' -> convert_hex_string_int ("0110" ^ acc) (index-1);
        | '7' -> convert_hex_string_int ("0111" ^ acc) (index-1);
        | '8' -> convert_hex_string_int ("1000" ^ acc) (index-1);
        | '9' -> convert_hex_string_int ("1001" ^ acc) (index-1);
        | 'a'
        | 'A' -> convert_hex_string_int ("1010" ^ acc) (index-1);
        | 'b'
        | 'B' -> convert_hex_string_int ("1011" ^ acc) (index-1);
        | 'c'
        | 'C' -> convert_hex_string_int ("1100" ^ acc) (index-1);
        | 'd'
        | 'D' -> convert_hex_string_int ("1101" ^ acc) (index-1);
        | 'e'
        | 'E' -> convert_hex_string_int ("1110" ^ acc) (index-1);
        | 'f'
        | 'F' -> convert_hex_string_int ("1111" ^ acc) (index-1);
        | ch  -> convert_hex_string_int ((String.make 4 ch) ^ acc) (index-1);
    in convert_hex_string_int "" (-1 + String.length str);;

  let fix_unsigned_bit_string_length str len =
    let str_len =
      String.length str;
    in
      if str_len = len then
        str
      else
      if str_len < len then
        (* '0' padding *)
        (String.make (len-str_len) '0') ^ str
      else
        (* str_len > len, truncation *)
        let rec check_char index =
          if index < 0 then
            (* truncation *)
            String.sub str (str_len - len) len
          else
          (* check *)
          if '0' = String.get str index then
            check_char (index-1)
          else
          (* a character is not '0' *)
            raise Parsing.Parse_error;
        (* check all chars from 0 to str_len - len - 1, they should equal '0' *)
        in check_char (str_len - len - 1);;

  let fix_signed_bit_string_length str len =
    let str_len =
      String.length str;
    in
      if str_len = len then
        str
      else
      if str_len < len then
        (* most significant bit pad padding *)
        (String.make (len-str_len) (String.get str 0)) ^ str
      else
        (* str_len > len, truncation *)
        let rec check_char ch index =
          if index < 0 then
            (* truncation *)
            String.sub str (str_len - len) len
          else
          (* check *)
          if ch = String.get str index then
            check_char ch (index-1)
          else
          (* a character is not 'equal to the most significant bit *)
            raise Parsing.Parse_error;
        (* check all chars from 0 to str_len - len - 1, they should equal to the most significant bit *)
        in check_char (String.get str (str_len - len - 1)) (str_len - len - 1);;

  (* this function compares two identifiers and returns the boolean result of the test *)
  (* if second operand is empty_identifier then returns true *)
  let identifiers_equal id1 id2 =
    if id2 = empty_identifier then
      true
    else
      (String.uppercase_ascii (fst id1)) = (String.uppercase_ascii (fst id2));;

  (* this function compares two identifiers and returns one of them if equals, otherwise it raises an exception *)
  let check_identifiers id1 id2 =
    if identifiers_equal id1 id2 then
      id1
    else
      raise Parsing.Parse_error;;
  let check_labels lab1 lab2 = check_identifiers lab1 lab2;;

  (* this function compares two designators *)
  let designators_equal des1 des2 =
    match des1,des2 with
    | DesignatorIdentifier id1,
      DesignatorIdentifier id2
        -> identifiers_equal id1 id2;
    | DesignatorOperator (sym1,_),
      DesignatorOperator (sym2,_)
        -> sym1 = sym2;
    | DesignatorCharacter (ch1,_),
      DesignatorCharacter (ch2,_)
        -> ch1 = ch2;
    | _ -> false;;

  (* this function will be called in case of parsing error, does nothing *)
  let parse_error (msg: string) = ( );;
%}

/**************************************************************/
/**************************************************************/
/**                                                          **/
/**                   lexer identifiers                      **/
/**                                                          **/
/**************************************************************/
/**                                                          **/
/** the lexer will return these identifiers:                 **/
/**    - VHDL reserved keywords                              **/
/**    - punctuation                                         **/
/**    - word (identifiers, functions, types, objects...     **/
/**    - explicit string ("value" or %value%)                **/
/**    - integers                                            **/
/**                                                          **/
/**************************************************************/
/**************************************************************/

/* VHDL keywords */
%token Labs
%token Laccess
%token Lacross
%token Lafter
%token Lalias
%token Lall
%token Land
%token Larchitecture
%token Larray
%token Lassert
%token Lassume
%token Lassumeguarantee
%token Lattribute
%token Lbegin
%token Lblock
%token Lbody
%token Lbreak
%token Lbuffer
%token Lbus
%token Lcase
%token Lcomponent
%token Lconfiguration
%token Lconstant
%token Lcontext
%token Lcover
%token Ldisconnect
%token Ldownto
%token Lelse
%token Lelsif
%token Lend
%token Lentity
%token Lexit
%token Lfairness
%token Lfile
%token Lfor
%token Lforce
%token Lfunction
%token Lgenerate
%token Lgeneric
%token Lgroup
%token Lguarded
%token Lif
%token Limpure
%token Lin
%token Linertial
%token Linout
%token Lis
%token Llabel
%token Llibrary
%token Llimit
%token Llinkage
%token Lliteral
%token Lloop
%token Lmap
%token Lmod
%token Lnand
%token Lnature
%token Lnew
%token Lnext
%token Lnoise
%token Lnor
%token Lnot
%token Lnull
%token Lof
%token Lon
%token Lopen
%token Lor
%token Lothers
%token Lout
%token Lpackage
%token Lparameter
%token Lport
%token Lpostponed
%token Lprocedural
%token Lprocedure
%token Lprocess
%token Lproperty
%token Lprotected
%token Lpure
%token Lquantity
%token Lrange
%token Lrecord
%token Lreference
%token Lregister
%token Lreject
%token Lrelease
%token Lrem
%token Lreport
%token Lrestrict
%token Lrestrictguarantee
%token Lreturn
%token Lrol
%token Lror
%token Lselect
%token Lsequence
%token Lseverity
%token Lsignal
%token Lshared
%token Lsla
%token Lsll
%token Lspectrum
%token Lsra
%token Lsrl
%token Lstrong
%token Lsubnature
%token Lsubtype
%token Lterminal
%token Lthen
%token Lthrough
%token Lto
%token Ltolerance
%token Ltransport
%token Ltype
%token Lunaffected
%token Lunits
%token Luntil
%token Luse
%token Lvariable
%token Lvmode
%token Lvprop
%token Lvunit
%token Lwait
%token Lwhen
%token Lwhile
%token Lwith
%token Lxnor
%token Lxor

/* punctation */
%token Lampersand            /* &   */
%token Lquote                /* '   */
%token Lleftparenthesis      /* (   */
%token Lrightparenthesis     /* )   */
%token Lexponential          /* **  */
%token Lmultiply             /* *   */
%token Lplus                 /* +   */
%token Lminus                /* -   */
%token Ldot                  /* .   */
%token Lnotequal             /* /=  */
%token Lslash                /* /   */
%token Lcolon                /* :   */
%token Lsemicolon            /* ;   */
%token Lless                 /* <   */
%token Lequal                /* =   */
%token Lgreater              /* >   */
%token Lverticalbar          /* |   */
%token Lleftbracket          /* [   */
%token Lrightbracket         /* ]   */
%token Lpercent              /* %   */
%token Lexclamationmark      /* !   */
%token Lquestionmark         /* ?   */
%token Lcomma                /* ,   */
%token Lassociation          /* =>  */
%token Limmediate            /* :=  */
%token Lmatchingequal        /* ?=  */
%token Lmatchingnotequal     /* ?/= */
%token Lmatchingless         /* ?<  */
%token Lmatchinglessequal    /* ?<= */
%token Lmatchinggreater      /* ?>  */
%token Lmatchinggreaterequal /* ?>= */

/* a string " " or % % literal: stored as string and position*/
%token <string*int> Lstring
/* an integer literal: stored as base, integer, exponent and position */
%token <string*string*string*int> Lint
/* a float literal: stored as base, integer, fraction, exponent and position */
%token <string*string*string*string*int> Lfloat
/* a char literal: stored as char and position */
%token <char*int> Lchar
/* other words: stored as string and position*/
%token <string*int> Lword

/* byte-order-marks */
%token Lbomutf8
%token Lbomutf7

/* end-of-file */
%token Leof

/**************************************************************/
/**************************************************************/
/**                                                          **/
/**             higher level parser types                    **/
/**                                                          **/
/**************************************************************/
/**************************************************************/

/* start point = top level grammar rule */
%start top_level_file
%start top_level_expression

%type <VhdlTypes.vhdl_design_file> top_level_file
%type <VhdlTypes.vhdl_expression> top_level_expression
%type <VhdlTypes.vhdl_suffix> suffix
%type <VhdlTypes.vhdl_suffix> prefix

%%

/* Grammar rules and actions follow */

identifier:
  | Lword
      { $1 }

identifier_list:
  | identifier
      { [$1] }
  | identifier_list Lcomma identifier
      { $1 @ [$3] }

/*
(* label ::= identifier *)
*/
label:
  | identifier
      { $1 }

/*
(* simple_name ::= identifier *)
*/
simple_name:
  | identifier
      { $1 }

/*
(* character_literal ::=   *)
(*   ' graphic_character ' *)
*/
character_literal:
  | Lchar
      { $1 }

/*
(* attribute_designator ::= attribute_simple_name *)
+ some keywords
*/
attribute_designator:
  | simple_name
      { $1 }
  | Lrange
      { ("range",-1) }
  | Lacross
      { ("across",-1) }

/*
(* suffix ::=                      *)
(*     simple_name                 *)
(*   | character_literal           *)
(*   | operator_symbol             *)
(*   | all                         *)
*/
suffix:
  | character_literal
      { SuffixCharLiteral $1 }
  | Lstring
      { SuffixOpSymbol $1 }
  | Lall
      { SuffixAll }
  | simple_name
      { SuffixSimpleName (SimpleName $1) }

/*
(* prefix ::=                      *)
(*    name                         *)
(*  | function_call                *)
*/
prefix:
  | name
      { SuffixSimpleName $1 }

/*
(* selected_name ::= prefix.suffix *)
*/
selected_name:
  | prefix Ldot suffix
      { $3 :: [$1] }
  | selected_name Ldot suffix
      { $3 :: $1 }

selected_name_list:
  | selected_name
      { [ $1 ] }
  | selected_name_list Lcomma selected_name
      { $1 @ [ $3 ] }

/*
(* name ::=           *)
(*    simple_name     *)
(*  | operator_symbol *)
(*  | selected_name   *)
(*  | indexed_name    *)
(*  | slice_name      *)
(*  | attribute_name  *)
*/
name:
  | simple_name
      { SimpleName $1 }
  | selected_name
      { SelectedName $1 }
  | Lstring
      { OperatorString $1 }
  | attribute_name
      { AttributeName $1 }

/*
(* type_mark ::=   *)
(*     typename    *)
(*   | subtypename *)
*/
type_mark:
  | name
      { $1 }

type_mark_list:
  | type_mark
      { [$1] }
  | type_mark_list Lcomma type_mark
      { ($1 @ [$3]) }

/*
(* logical_name ::= identifier                         *)
*/
logical_name:
  | identifier
      { $1 }

/*
(* logical_name_list ::= logical_name {, logical_name} *)
*/
logical_name_list:
  | logical_name
      { [$1] }
  | logical_name_list Lcomma logical_name
      { ($1 @ [$3]) }

/*
(* library_clause ::= library logical_name_list ;      *)
*/
library_clause:
  | Llibrary logical_name_list Lsemicolon
      { $2 }

/*
(*condition ::= expression *)
*/
condition:
  | expression
      { Condition $1 }

/*
(* selector has special restrictions *)
*/
selector:
  | expression
      { Selector $1 }

/*
(* expression ::=                                    *)
(*   logical_expression                              *)
(*   | condition_operator primary                    *)
*/
expression:
  | logical_expression
      { AtomExpression $1 }
  | Lquestionmark Lquestionmark primary
      { ConditionExpression $3 }

expression_list:
  | expression
      { [$1] }
  | expression_list Lcomma expression
      { ($1 @ [$3]) }

/*
(* logical_expression ::=                            *)
(*     relation { and  relation }                    *)
(*   | relation { or   relation }                    *)
(*   | relation { xor  relation }                    *)
(*   | relation [ nand relation ]                    *)
(*   | relation [ nor  relation ]                    *)
(*   | relation { xnor relation }                    *)
*/
logical_expression:
  | relation
      { AtomLogicalExpression $1 }
  | logical_expression Land relation
      { match $1 with
         | AtomLogicalExpression a
             -> AndLogicalExpression (a,$3);
         | _ -> AndLogicalExpression (relation_of_logical_expression $1,$3); }
  | logical_expression Lor relation
      { match $1 with
        | AtomLogicalExpression a
            -> OrLogicalExpression (a,$3);
        | _ -> OrLogicalExpression (relation_of_logical_expression $1,$3); }
  | logical_expression Lxor relation
      { match $1 with
        | AtomLogicalExpression a
            -> XorLogicalExpression (a,$3);
        | _ -> XorLogicalExpression (relation_of_logical_expression $1,$3); }
  | relation Lnand relation
      { NandLogicalExpression ($1,$3) }
  | relation Lnor relation
      { NorLogicalExpression ($1,$3) }
  | logical_expression Lxnor relation
      { match $1 with
        | AtomLogicalExpression a
            -> XnorLogicalExpression (a,$3);
        | _ -> XnorLogicalExpression (relation_of_logical_expression $1,$3); }

/*
(* relation ::=                                      *)
(*   shift_expression                                *)
(*   [ relational_operator shift_expression ]        *)
*/
relation:
  | shift_expression Lequal shift_expression
      { EqualRelation ($1,$3) }
  | shift_expression Lnotequal shift_expression
      { NotEqualRelation ($1,$3) }
  | shift_expression Lless shift_expression
      { LessRelation ($1,$3) }
  | shift_expression Lless Lequal shift_expression
      { LessOrEqualRelation ($1,$4) }
  | shift_expression Lgreater shift_expression
      { GreaterRelation ($1,$3) }
  | shift_expression Lgreater Lequal shift_expression
      { GreaterOrEqualRelation ($1,$4) }
  | shift_expression Lmatchingequal shift_expression
      { MatchingEqualRelation ($1,$3) }
  | shift_expression Lmatchingnotequal shift_expression
      { MatchingNotEqualRelation ($1,$3) }
  | shift_expression Lmatchingless shift_expression
      { MatchingLessRelation ($1,$3) }
  | shift_expression Lmatchinglessequal shift_expression
      { MatchingLessOrEqualRelation ($1,$3) }
  | shift_expression Lmatchinggreater shift_expression
      { MatchingGreaterRelation ($1,$3) }
  | shift_expression Lmatchinggreaterequal shift_expression
      { MatchingGreaterOrEqualRelation ($1,$3) }
  | shift_expression
      { AtomRelation $1 }

/*
(* shift_expression ::=                              *)
(*   simple_expression                               *)
(*   [ shift_operator simple_expression ]            *)
*/
shift_expression:
  | simple_expression Lsll simple_expression
      { ShiftLeftLogicalExpression ($1,$3) }
  | simple_expression Lsrl simple_expression
      { ShiftRightLogicalExpression ($1,$3) }
  | simple_expression Lsla simple_expression
      { ShiftLeftArithmeticExpression ($1,$3) }
  | simple_expression Lsra simple_expression
      { ShiftRightArithmeticExpression ($1,$3) }
  | simple_expression Lrol simple_expression
      { RotateLeftExpression ($1,$3) }
  | simple_expression Lror simple_expression
      { RotateRightExpression ($1,$3) }
  | simple_expression
      { AtomShiftExpression $1 }

/*
(* simple_expression ::=                             *)
(*   [ sign ] term { adding_operator term }          *)
*/
simple_expression:
  | simple_expression Lplus term
      { match $1 with
         | AtomSimpleExpression a -> AddSimpleExpression (a,$3);
         | _                      -> AddSimpleExpression (term_of_simple_expression $1,$3); }
  | simple_expression Lminus term
      { match $1 with
         | AtomSimpleExpression a -> SubSimpleExpression (a,$3);
         | _                      -> SubSimpleExpression (term_of_simple_expression $1,$3); }
  | simple_expression Lampersand term
      { match $1 with
         | ConcatSimpleExpression l -> ConcatSimpleExpression (l @ [$3]);
         | AtomSimpleExpression a   -> ConcatSimpleExpression [a;$3];
         | _                        -> ConcatSimpleExpression [term_of_simple_expression $1;$3]; }
  | Lplus term
      { AtomSimpleExpression $2 }
  | Lminus term
      { NegSimpleExpression $2 }
  | term
      { AtomSimpleExpression $1 }


/*
(* term ::=                                          *)
(*   factor { multiplying_operator factor }          *)
*/

term:
  | term Lmultiply factor
      { match $1 with
         | AtomTerm a -> MultTerm (a,$3);
         | _          -> MultTerm (factor_of_term $1,$3); }
  | term Lslash factor
      { match $1 with
         | AtomTerm a    -> DivTerm (a,$3);
         | _             -> DivTerm (factor_of_term $1,$3); }
  | term Lmod factor
      { match $1 with
         | AtomTerm a    -> ModTerm (a,$3);
         | _             -> ModTerm (factor_of_term $1,$3); }
  | term Lrem factor
      { match $1 with
         | AtomTerm a    -> RemTerm (a,$3);
         | _             -> RemTerm (factor_of_term $1,$3); }
  | factor
      { AtomTerm $1; }


/*
(* factor ::=                                        *)
(*    primary [ ** primary ]                         *)
(*  | abs primary                                    *)
(*  | not primary                                    *)
(*  | logical_operator primary                       *)
*/

factor:
  | dotted Lexponential dotted
      { ExpFactor ($1,$3); }
  | Labs dotted
      { AbsFactor $2; }
  | Lnot dotted
      { NotFactor $2; }
  | Land dotted
      { AndFactor $2; }
  | Lor dotted
      { OrFactor $2; }
  | Lxor dotted
      { XorFactor $2; }
  | Lxnor dotted
      { XnorFactor $2; }
  | Lnand dotted
      { NandFactor $2; }
  | Lnor dotted
      { NorFactor $2; }
  | Lnew dotted
      { NewFactor $2; }
  | dotted
      { AtomFactor $1; }

dotted:
  | primary Ldot primary
      { Ldotted ($1,$3); }
  | primary
      { AtomDotted $1 }

/*
(* primary ::=                                       *)
(*    name                                           *)
(*  | literal                                        *)
(*  | aggregate                                      *)
(*  | function_call                                  *)
(*  | qualified_expression                           *)
(*  | type_conversion                                *)
(*  | allocator                                      *)
(*  | ( expression )                                 *)
type conversions are reported as a function calls
string literals are reported as names (OperatorString)
*/

primary:
  | Lnull
      { VhdSequentialNull }
  | name parameters
      { NameParametersPrimary ($1,$2) }
  | integer_literal
      { IntPrimary $1 }
  | float_literal
      { FloatPrimary $1 }
  | character_literal
      { CharPrimary $1 }
  | physical_literal
      { PhysicalPrimary $1 }
  | aggregate
      /* aggregate primary or parenthesed primary */
      { match $1 with
        | [{elemassocchoices=[];elemassocexpression=expression}]
            -> ParenthesedPrimary expression
        | _ -> AggregatePrimary $1 }
  | qualified_expression
      { QualifiedExpressionPrimary $1 }
  | name
      { NamePrimary $1 }
  | name Lstring
      { match $1 with
        | SimpleName (base,pos)
            -> begin
                 match String.uppercase_ascii base with
                 | "D"
                     -> NamePrimary (OperatorString (convert_dec_string (strRemoveUnderscore (fst $2)),pos))
                 | "B"
                 | "SB"
                 | "UB"
                     -> NamePrimary (OperatorString (convert_bin_string (strRemoveUnderscore (fst $2)),pos))
                 | "O"
                 | "SO"
                 | "UO"
                     -> NamePrimary (OperatorString (convert_oct_string (strRemoveUnderscore (fst $2)),pos))
                 | "X"
                 | "SX"
                 | "UX"
                     -> NamePrimary (OperatorString (convert_hex_string (strRemoveUnderscore (fst $2)),pos))
                 | _ -> raise Parsing.Parse_error;
               end;
        | _ -> raise Parsing.Parse_error; }
  | integer_literal name Lstring
      { let expanded_bit_string,pos,unsigned =
          match $2 with
          | SimpleName ("D",pos)
              -> convert_dec_string (strRemoveUnderscore (fst $3)),
                 pos,
                 true;
          | SimpleName ("SB",pos)
              -> convert_bin_string (strRemoveUnderscore (fst $3)),
                 pos,
                 false;
          | SimpleName ("B",pos)
          | SimpleName ("UB",pos)
              -> convert_bin_string (strRemoveUnderscore (fst $3)),
                 pos,
                 true;
          | SimpleName ("SO",pos)
              -> convert_oct_string (strRemoveUnderscore (fst $3)),
                 pos,
                 false;
          | SimpleName ("O",pos)
          | SimpleName ("UO",pos)
              -> convert_oct_string (strRemoveUnderscore (fst $3)),
                 pos,
                 true;
          | SimpleName ("SX",pos)
              -> convert_hex_string (strRemoveUnderscore (fst $3)),
                 pos,
                 false;
          | SimpleName ("X",pos)
          | SimpleName ("UX",pos)
              -> convert_hex_string (strRemoveUnderscore (fst $3)),
                 pos,
                 true;
          | _ -> raise Parsing.Parse_error;
        and digit_count,_ =
          $1;
        in
          if unsigned then
            NamePrimary (OperatorString (fix_unsigned_bit_string_length expanded_bit_string (Big_int.int_of_big_int digit_count),pos))
          else
            NamePrimary (OperatorString (fix_signed_bit_string_length expanded_bit_string (Big_int.int_of_big_int digit_count),pos)); }

parameters:
  | parameter
      { [$1] }
  | parameters parameter
      { $1 @ [$2] }

parameter:
  | Lleftparenthesis association_list Lrightparenthesis
      { $2 }

/*
(* aggregate ::=                                     *)
(*   ( element_association {, element_association} ) *)
*/
aggregate:
  | Lleftparenthesis element_association_list Lrightparenthesis
      { $2 }

/*
(* element_association ::=                           *)
(*   [ choices => ] expression                       *)
*/
element_association:
  | expression
      { {elemassocchoices=[];
         elemassocexpression=$1} }
  | choices Lassociation expression
      { {elemassocchoices=$1;
         elemassocexpression=$3} }

element_association_list:
  | element_association
      { [$1] }
  | element_association_list Lcomma element_association
      { ($1 @ [$3]) }

/*
(* choices ::= choice { | choice }                   *)
*/
choices:
  | choice
      { [$1] }
  | choices Lverticalbar choice
      { ($1 @ [$3]) }
  | choices Lexclamationmark choice
      { ($1 @ [$3]) }

/*
(* choice ::=                                        *)
(*     simple_expression                             *)
(*   | discrete_range                                *)
(*   | element_simple_name                           *)
(*   | others                                        *)
simple_expression may contain range attribute name
discrete range may be only to/downto expressions
*/
choice:
  | simple_expression
      { ChoiceSimpleExpression $1 }
  | discrete_range_without_attribute
      { ChoiceDiscreteRange $1 }
  | Lothers
      { ChoiceOthers }

/*
(* actual_parameter_part ::=                         *)
(*   parameter_association_list                      *)
never used
parameter_part:
  | association_list
      { $1 }*/

/*
(* qualified_expression ::=                          *)
(*     type_mark'( expression )                      *)
(*   | type_mark'aggregate                           *)
aggregate qualified expression or parenthesed qualified expression
a parenthesed expression is an aggregate with one element without choice
*/
qualified_expression:
  | prefix Lquote aggregate
      { match $3 with
        | [{elemassocchoices=[];elemassocexpression=expression}]
            -> QualifiedExpression ($1,expression)
        | _ -> QualifiedAggregate ($1,$3) }

/*
(* range_constraint ::= range range                  *)
*/
range_constraint:
  | Lrange range
      { $2 }

/*
(* range ::=                                         *)
(*    range_attribute_name                           *)
(*  | simple_expression direction simple_expression  *)
(* direction ::= to | downto                         *)
*/
range:
  | attribute_name
      { AttrNameRange $1 }
  | range_without_attribute
      { $1 }

range_without_attribute:
  | simple_expression Lto simple_expression
      { IncreasingRange ($1,$3) }
  | simple_expression Ldownto simple_expression
      { DecreasingRange ($1,$3) }

/*
(* discrete_range ::=             *)
(*  | discrete_subtype_indication *)
(*  | range                       *)
attributes are reported as subtype_indication
*/
discrete_range:
  | range_without_attribute
      { Range $1 }
  | discrete_subtype_indication
      { SubTypeRange $1 }

discrete_range_without_attribute:
  | range_without_attribute
      { Range $1 }

/*
(* actual_designator ::=                             *)
(*     expression                                    *)
(*   | signal_name                                   *)
(*   | variable_name                                 *)
(*   | file_name (ieee syn ignored)                  *)
(*   | open                                          *)
expression may be an identifier (name)
(* actual_part ::=                                   *)
(*     actual_designator                             *)
(*   | function_name( actual_designator )            *)
(*   | type_mark( actual_designator )                *)
actual_designator may contain an expression of type function call or type conversion
*/
actual_part:
  | Lopen
      { ActualOpen }
  | discrete_range_without_attribute
      { ActualDiscreteRange $1 }
  | expression
      { ActualExpression $1 }
  | type_mark range_constraint
      { ActualDiscreteRange (SubTypeRange {resolutionfunction=empty_name; basetypename=$1; subtypeconstraint=RangeConstraint $2 }) }
  | name type_mark range_constraint
      { ActualDiscreteRange (SubTypeRange {resolutionfunction=$1; basetypename=$2; subtypeconstraint=RangeConstraint $3 }) }

/*
(* formal_designator ::=                             *)
(*     generic_name                                  *)
(*   | port_name                                     *)
(*   | parameter_name                                *)
(* formal_part ::=                                   *)
(*     formal_designator                             *)
(*   | function_name( formal_designator )            *)
(*   | type_mark( formal_designator )                *)
an expression may contain an identifier, a type conversion or a function call
*/
formal_part:
  | expression
      { FormalExpression $1 }

/*
(* association_element ::=                           *)
(*   [formal_part =>] actual_part                    *)
*/
association_element:
  | actual_part
      { { actual=$1; formal=FormalIndexed } }
  | formal_part Lassociation actual_part
      { { actual=$3; formal=$1 } }

/*
(* association_list ::=                              *)
(*   association_element {, association_element}     *)
*/
association_list:
  | association_element
      { [$1] }
  | association_list Lcomma association_element
      { ($1 @ [$3]) }

/*
(* attribute_name ::=                            *)
(*   prefix [ signature ] ' attribute_designator *)
(*   [ ( expression { , expression } ) ]         *)
(* simplified as prefix ' attribute_designator   *)
*/
attribute_name:
  | prefix Lquote attribute_designator
      { {attributeprefix=$1; attribute=$3} }

/*
(* signature ::= [ [ type_mark { , type_mark } ] [ return type_mark ] ] *)
*/
signature:
  | Lleftbracket Lreturn type_mark Lrightbracket
      { {signatureparametertypes=[];
         signaturereturntype=$3} }
  | Lleftbracket type_mark_list Lrightbracket
      { {signatureparametertypes=$2;
         signaturereturntype=empty_type_mark } }
  | Lleftbracket type_mark_list Lreturn type_mark Lrightbracket
      { {signatureparametertypes=$2;
         signaturereturntype=$4 } }

/*
(* decimal_literal ::= integer [ . integer ] [ exponent ] *)
(* integer ::= digit { [ underline ] digit }              *)
(* exponent ::= E [ + ] integer | E - integer             *)
reworked to match caml types
*/
integer_literal:
  | Lint
     { try
         match $1 with
           | ("",intpart,"",pos)
               -> big_int_of_string (strRemoveUnderscore intpart),pos
           | ("",intpart,exppart,pos)
               -> let intvalue = big_int_of_string (strRemoveUnderscore intpart)
                  and expvalue = big_int_of_string (strRemoveUnderscore exppart) in
                    (Big_int.mult_big_int intvalue (power_big_int (Big_int.big_int_of_int 10) expvalue),pos)
           | (radixpart,intpart,"",pos)
               -> let radix = big_int_of_string (strRemoveUnderscore radixpart) in
                    (big_int_of_string_radix (strRemoveUnderscore intpart) radix,pos)
           | (radixpart,intpart,exppart,pos)
               -> let radix = big_int_of_string (strRemoveUnderscore radixpart) in
                    let intvalue = big_int_of_string_radix (strRemoveUnderscore intpart) radix
                    and expvalue = big_int_of_string (strRemoveUnderscore exppart) in
                      (Big_int.mult_big_int intvalue (power_big_int radix expvalue),pos)
       with Failure s -> match $1 with (radixpart,intpart,exppart,pos)
         -> Printf.fprintf stdout "int %s %s %s" radixpart intpart exppart; raise (Failure s) }

float_literal:
  | Lfloat
     { try
         match $1 with
           | ("",intpart,fracpart,"",pos)
               -> (float_of_string (strRemoveUnderscore (intpart^"."^fracpart)),pos);
           | ("",intpart,fracpart,exppart,pos)
               -> (float_of_string (strRemoveUnderscore (intpart^"."^fracpart^"e"^exppart)),pos);
           | (radixpart,intpart,fracpart,"",pos)
               -> let radix = big_int_of_string (strRemoveUnderscore radixpart) in
                    let intvalue = big_int_of_string_radix (strRemoveUnderscore intpart) radix
                    and fracstr = strRemoveUnderscore fracpart in
                      let fracvalue = big_int_of_string_radix fracstr radix in
                        (((Big_int.float_of_big_int intvalue) +. (Big_int.float_of_big_int fracvalue) *. (power_float (Big_int.float_of_big_int radix) (Big_int.minus_big_int (Big_int.big_int_of_int (String.length fracstr))))),pos);
           | (radixpart,intpart,fracpart,exppart,pos)
               -> let radix = big_int_of_string (strRemoveUnderscore radixpart) in
                    let intvalue = big_int_of_string_radix (strRemoveUnderscore intpart) radix
                    and expvalue = big_int_of_string (strRemoveUnderscore exppart)
                    and fracstr = strRemoveUnderscore fracpart in
                      let fracvalue = big_int_of_string_radix fracstr radix in
                        ((((Big_int.float_of_big_int intvalue) +. (Big_int.float_of_big_int fracvalue) *. (power_float (Big_int.float_of_big_int radix) (Big_int.minus_big_int (Big_int.big_int_of_int (String.length fracstr))))) *. (power_float (Big_int.float_of_big_int radix) expvalue)),pos);
       with Failure s -> match $1 with (radixpart,intpart,fracpart,exppart,pos)
         -> Printf.fprintf stdout "int %s %s %s %s" radixpart intpart fracpart exppart; raise (Failure s) }

physical_literal:
  | integer_literal identifier
      { PhysicalInteger ($1, $2) }
  | float_literal identifier
      { PhysicalFloat ($1, $2) }

/*
(* index_constraint ::=                                    *)
(*   ( discrete_range { , discrete_range } )               *)
*/
index_constraint:
  | parameter
     { $1 }

/*
(* subtype_declaration ::=                                 *)
(*   subtype identifier is subtype_indication ;            *)
*/
subtype_declaration:
  | Lsubtype identifier Lis subtype_indication Lsemicolon
     { {subtypename=$2; subtypeindication=$4} }

/*
(* subtype_indication ::=                                  *)
(*   [ resolution_function_name ] type_mark [ constraint ] *)
*/
subtype_indication:
  | type_mark
     { {resolutionfunction=empty_name; basetypename=$1; subtypeconstraint=NoConstraint } }
  | type_mark constraint_
     { {resolutionfunction=empty_name; basetypename=$1; subtypeconstraint=$2 } }
  | name type_mark
     { {resolutionfunction=$1; basetypename=$2; subtypeconstraint=NoConstraint } }
  | name type_mark constraint_
     { {resolutionfunction=$1; basetypename=$2; subtypeconstraint=$3 } }

discrete_subtype_indication:
  | type_mark
     { {resolutionfunction=empty_name; basetypename=$1; subtypeconstraint=NoConstraint } }
  | type_mark range_constraint
     { {resolutionfunction=empty_name; basetypename=$1; subtypeconstraint=RangeConstraint $2 } }
  | name type_mark
     { {resolutionfunction=$1; basetypename=$2; subtypeconstraint=NoConstraint } }
  | name type_mark range_constraint
     { {resolutionfunction=$1; basetypename=$2; subtypeconstraint=RangeConstraint $3 } }

/*
(* constraint ::=                                          *)
(*     range_constraint                                    *)
(*   | array_constraint                                    *)
(*   | record_constraint                                   *)
*/
constraint_:
  | range_constraint
     { RangeConstraint $1 }
  | array_constraint
     { ArrayConstraint $1 }

/*
(* array_constraint ::=                                            *)
(*     index_constraint [array_element_constraint]                 *)
(*   | (open) [array_element_constraint]                           *)
*/
array_constraint:
  | parameters
      { $1 }

/*
(* enumeration_type_definition ::=                        *)
(*   ( enumeration_literal { , enumeration_literal } )    *)
*/
enumeration_type_definition:
  | Lleftparenthesis enumeration_literal_list Lrightparenthesis
     { $2 }

/*
(* enumeration_literal ::= identifier | character_literal *)
*/
enumeration_literal:
  | identifier
     { IdentifierEnumeration $1 }
  | character_literal
     { CharLiteralEnumeration $1 }

enumeration_literal_list:
  | enumeration_literal
     { [$1] }
  | enumeration_literal_list Lcomma enumeration_literal
     { ($1 @ [$3]) }

/*
(*   integer_type_definition ::= range_constraint *)
never used
integer_type_definition:
  | range_constraint
     { $1 }
*/
/*
(* floating_type_definition ::= range_constraint (ignored ieee syn) *)
never used
floating_type_definition:
  | range_constraint
     { $1 }
*/
/*
(* array_type_definition ::=                                           *)
(*     unbounded_array_definition                                      *)
(*   | constrained_array_definition                                    *)
*/
array_type_definition:
  | constrained_array_definition
     { ConstrainedArray $1 }
  | unbounded_array_definition
     { UnboundedArray $1 }

/*
(* unbounded_array_definition ::=                                      *)
(*   array ( index_subtype_definition { , index_subtype_definition } ) *)
(*           of element_subtype_indication                             *)
*/
unbounded_array_definition:
  | Larray Lleftparenthesis index_subtype_definition_list Lrightparenthesis Lof subtype_indication
     { {uarraydimensions=$3; uarrayelementtype=$6} }

/*
(* constrained_array_definition ::=                                    *)
(*   array index_constraint of element_subtype_indication              *)
*/
constrained_array_definition:
  | Larray index_constraint Lof subtype_indication
     { {carraydimensions=$2; carrayelementtype=$4} }

/*
(* index_subtype_definition ::= type_mark range <>                     *)
*/
index_subtype_definition:
  | type_mark Lrange Lless Lgreater
     { $1 }

index_subtype_definition_list:
  | index_subtype_definition
     { [$1] }
  | index_subtype_definition_list Lcomma index_subtype_definition
     { ($1 @ [$3]) }

/*
(* record_type_definition ::=                        *)
(*   record                                          *)
(*     element_declaration                           *)
(*     { element_declaration }                       *)
(* end record [ record_type_simple_name ]            *)
*/

record_type_definition:
  | Lrecord element_declaration_list Lend Lrecord
     { {recordelems=$2; recordname=empty_simple_name} }
  | Lrecord element_declaration_list Lend Lrecord simple_name
     { {recordelems=$2; recordname=$5} }

/*
(* element_declaration ::=                           *)
(*   identifier_list : element_subtype_definition;   *)
*/
element_declaration:
  | identifier_list Lcolon element_subtype_definition Lsemicolon
     { {elementnames=$1; elementsubtype=$3} }

element_declaration_list:
  | element_declaration
     { [$1] }
  | element_declaration_list element_declaration
     { ($1 @ [$2]) }

/*
(* element_subtype_definition ::= subtype_indication *)
*/
element_subtype_definition:
  | subtype_indication
     { $1 }

/*
(* file_type_definition ::= file of type_mark *)
*/
file_type_definition:
  | Lfile Lof type_mark
     { $3 }

/*
(* access_type_definition ::= access subtype_indication *)
(* (ieee syn not supported)                             *)
*/
access_type_definition:
  | Laccess subtype_indication
     { $2 }

/*
(* physical_type_definition ::=                                  *)
(*   range_constraint                                            *)
(*     units                                                     *)
(*       primary_unit_declaration                                *)
(*       { secondary_unit_declaration }                          *)
(*     end units [ physical_type_simple_name ]                   *)
*/
physical_type_definition:
  | range_constraint Lunits physical_unit_declaration Lend Lunits
      { {physicalconstraint=$1; physicalunits=$3; physicalname=empty_simple_name} }
  | range_constraint Lunits physical_unit_declaration Lend Lunits simple_name
      { {physicalconstraint=$1; physicalunits=$3; physicalname=$6} }

/*
(* primary_unit_declaration ::= identifier ;                     *)
(* secondary_unit_declaration ::= identifier = physical_literal; *)
*/
physical_unit_declaration:
  | identifier Lsemicolon
      { [$1, PhysicalEmpty (position_of_identifier $1)] }
  | physical_unit_declaration identifier Lequal physical_literal Lsemicolon
      { ($1 @ [($2,$4)]) }

/*
(* scalar_type_definition ::=                           *)
(*    enumeration_type_definition                       *)
(*  | integer_type_definition                           *)
(*  | physical_type_definition (not supported ieee syn) *)
(*  | floating_type_definition (ignored ieee syn)       *)
(* composite_type_definition ::=                        *)
(*    array_type_definition                             *)
(*  | record_type_definition                            *)
*/
type_definition:
  | enumeration_type_definition
     { EnumerationTypeDefinition $1 }
  | range_constraint
     { RangeTypeDefinition $1 }
  | physical_type_definition
     { PhysicalTypeDefinition $1 }
  | array_type_definition
     { ArrayTypeDefinition $1 }
  | record_type_definition
     { RecordTypeDefinition $1 }
  | file_type_definition
     { FileTypeDefinition $1 }
  | access_type_definition
     { AccessTypeDefinition $1 }

/*
(* full_type_declaration ::=                            *)
(*   type identifier is type_definition ;               *)
*/
full_type_declaration:
  | Ltype identifier Lis type_definition Lsemicolon
     { match $4 with
         | PhysicalTypeDefinition t when not (identifiers_equal $2 t.physicalname) -> raise Parsing.Parse_error
         | RecordTypeDefinition t when not (identifiers_equal $2 t.recordname) -> raise Parsing.Parse_error
         | _ -> {typename=$2; typedefinition=$4} }

incomplete_type_declaration:
  | Ltype identifier Lsemicolon
     { {incompletetypename=$2} }

/*
(* type_declaration ::=                                 *)
(*     full_type_declaration                            *)
(*   | incomplete_type_declaration (ignored ieee syn)   *)
*/
type_declaration:
  | full_type_declaration
     { FullType $1 }
  | incomplete_type_declaration
     { IncompleteType $1 }

/*
(* constant_declaration ::=                                           *)
(*   constant identifier_list : subtype_indication [ := expression ]; *)
*/

constant_declaration:
  | Lconstant identifier_list Lcolon subtype_indication Lsemicolon
     { {constantnames=$2; constantsubtype=$4; constantexpression=empty_expression} }
  | Lconstant identifier_list Lcolon subtype_indication Limmediate expression Lsemicolon
     { {constantnames=$2; constantsubtype=$4; constantexpression=$6 } }

/*
(* signal_declaration ::=                                  *)
(*   signal identifier_list :                              *)
(*     subtype_indication [ signal_kind ] [:= expression]; *)
(*     (expression is ignored ieee syn)                    *)
*/
signal_declaration:
  | Lsignal identifier_list Lcolon subtype_indication Lsemicolon
     { {signalnames=$2; signalsubtype=$4; signalkind=SignalKindDefault; signalexpression=empty_expression} }
  | Lsignal identifier_list Lcolon subtype_indication signal_kind Lsemicolon
     { {signalnames=$2; signalsubtype=$4; signalkind=$5; signalexpression=empty_expression} }
  | Lsignal identifier_list Lcolon subtype_indication Limmediate expression Lsemicolon
     { {signalnames=$2; signalsubtype=$4; signalkind=SignalKindDefault; signalexpression=$6} }
  | Lsignal identifier_list Lcolon subtype_indication signal_kind Limmediate expression Lsemicolon
     { {signalnames=$2; signalsubtype=$4; signalkind=$5; signalexpression=$7} }

/*
(*   signal_kind ::= register | bus                        *)
*/
signal_kind:
  | Lregister
     { SignalKindRegister }
  | Lbus
     { SignalKindBus }

/*
(* variable_declaration ::=               *)
(*  [ shared ] variable identifier_list : *)
(*    subtype_indication [:= expression]; *)
(* (shared not supported ieee syn)        *)
(* (expression ignored ieee syn)          *)
*/
variable_declaration:
  | Lvariable identifier_list Lcolon subtype_indication Lsemicolon
     { {variableshared=false; variablenames=$2; variablesubtype=$4; variableexpression=empty_expression} }
  | Lvariable identifier_list Lcolon subtype_indication Limmediate expression Lsemicolon
     { {variableshared=false; variablenames=$2; variablesubtype=$4; variableexpression=$6} }
  | Lshared Lvariable identifier_list Lcolon subtype_indication Lsemicolon
     { {variableshared=true; variablenames=$3; variablesubtype=$5; variableexpression=empty_expression} }
  | Lshared Lvariable identifier_list Lcolon subtype_indication Limmediate expression Lsemicolon
     { {variableshared=true; variablenames=$3; variablesubtype=$5; variableexpression=$7} }

/*
(* file_declaration ::= (ignored ieee syn)                    *)
(*   file identifier_list :                                   *)
(*     subtype_indication [ file_open_information ] ;         *)
(* file_open_information ::= (ignored ieee syn)               *)
(*   [ open file_open_kind_expression ] is file_logical_name  *)
(* file_logical_name ::= string_expression (ignored ieee syn) *)
*/
file_declaration:
  | Lfile identifier_list Lcolon subtype_indication Lsemicolon
     { {filenames=$2; filesubtype=$4; fileopenkind=empty_expression; filelogicalname=empty_expression} }
  | Lfile identifier_list Lcolon subtype_indication Lis expression Lsemicolon
     { {filenames=$2; filesubtype=$4; fileopenkind=empty_expression; filelogicalname=$6} }
  | Lfile identifier_list Lcolon subtype_indication Lopen expression Lis expression Lsemicolon
     { {filenames=$2; filesubtype=$4; fileopenkind=$6; filelogicalname=$8} }

/*
(* object_declaration ::=                  *)
(*     constant_declaration                *)
(*   | signal_declaration                  *)
(*   | variable_declaration                *)
(*   | file_declaration (ignored ieee syn) *)
never used
object_declaration:
  | constant_declaration
     { ConstantDeclaration $1 }
  | signal_declaration
     { SignalDeclaration $1 }
  | variable_declaration
     { VariableDeclaration $1 }
  | file_declaration
     { FileDeclaration $1 }
*/

/*
(* interface_type_declaration ::=              *)
(*   interface_incomplete_type_declaration     *)
*/
interface_type_declaration:
  | interface_incomplete_type_declaration
      { InterfaceIncompleteTypeDeclaration $1 }

/*
(* interface_incomplete_type_declaration ::=   *)
(*   type identifier                           *)
*/
interface_incomplete_type_declaration:
  | Ltype identifier
      { $2 }

/*
(* interface_declaration ::=                   *)
(*     interface_object_declaration            *)
(*   | interface_type_declaration              *)
(*   | interface_subprogram_declaration        *)
(*   | interface_package_declaration           *)
*/
interface_declaration:
  | interface_object_declaration
      { InterfaceObjectDeclaration $1 }
  | interface_type_declaration
      { InterfaceTypeDeclaration $1 }

/*
(* interface_object_declaration ::=                             *)
(*     interface_constant_declaration                           *)
(*   | interface_signal_declaration                             *)
(*   | interface_variable_declaration                           *)
(*   | interface_file_declaration (ieee syn not supported)      *)
*/
interface_object_declaration:
  | interface_constant_declaration
      { InterfaceConstantDeclaration $1 }
  | interface_signal_declaration
      { InterfaceSignalDeclaration $1 }
  | interface_variable_declaration
      { InterfaceVariableDeclaration $1 }
  | interface_file_declaration
      { InterfaceFileDeclaration $1 }
  | interface_default_declaration
      { InterfaceDefaultDeclaration $1 }

/*
(* interface_constant_declaration ::=                           *)
(*   constant identifier_list :                               *)
(*   [in] subtype_indication [:= static_expression]             *)
(*   (expression ieee syn ignored)                              *)
*/
interface_constant_declaration:
  | Lconstant identifier_list Lcolon subtype_indication
      { {interfaceconstantnames=$2; interfaceconstantsubtype=$4; interfaceconstantexpression=empty_expression} }
  | Lconstant identifier_list Lcolon Lin subtype_indication
      { {interfaceconstantnames=$2; interfaceconstantsubtype=$5; interfaceconstantexpression=empty_expression} }
  | Lconstant identifier_list Lcolon subtype_indication Limmediate expression
      { {interfaceconstantnames=$2; interfaceconstantsubtype=$4; interfaceconstantexpression=$6} }
  | Lconstant identifier_list Lcolon Lin subtype_indication Limmediate expression
      { {interfaceconstantnames=$2; interfaceconstantsubtype=$5; interfaceconstantexpression=$7} }

/*
(* interface_signal_declaration ::=                             *)
(*   signal identifier_list : [mode] subtype_indication [bus] *)
(*   [:= static_expression] (ieee syn ignored)                  *)
*/
interface_signal_declaration:
  | Lsignal identifier_list Lcolon subtype_indication
      { {interfacesignalnames=$2; interfacesignalmode=InterfaceModeIn; interfacesignalsubtype=$4; interfacesignalkind=SignalKindDefault; interfacesignalexpression=empty_expression} }
  | Lsignal identifier_list Lcolon mode subtype_indication
      { {interfacesignalnames=$2; interfacesignalmode=$4; interfacesignalsubtype=$5; interfacesignalkind=SignalKindDefault; interfacesignalexpression=empty_expression} }
  | Lsignal identifier_list Lcolon subtype_indication Lbus
      { {interfacesignalnames=$2; interfacesignalmode=InterfaceModeIn; interfacesignalsubtype=$4; interfacesignalkind=SignalKindBus; interfacesignalexpression=empty_expression} }
  | Lsignal identifier_list Lcolon mode subtype_indication Lbus
      { {interfacesignalnames=$2; interfacesignalmode=$4; interfacesignalsubtype=$5; interfacesignalkind=SignalKindBus; interfacesignalexpression=empty_expression} }
  | Lsignal identifier_list Lcolon subtype_indication Limmediate expression
      { {interfacesignalnames=$2; interfacesignalmode=InterfaceModeIn; interfacesignalsubtype=$4; interfacesignalkind=SignalKindDefault; interfacesignalexpression=$6} }
  | Lsignal identifier_list Lcolon mode subtype_indication Limmediate expression
      { {interfacesignalnames=$2; interfacesignalmode=$4; interfacesignalsubtype=$5; interfacesignalkind=SignalKindDefault; interfacesignalexpression=$7} }
  | Lsignal identifier_list Lcolon subtype_indication Lbus Limmediate expression
      { {interfacesignalnames=$2; interfacesignalmode=InterfaceModeIn; interfacesignalsubtype=$4; interfacesignalkind=SignalKindBus; interfacesignalexpression=$7} }
  | Lsignal identifier_list Lcolon mode subtype_indication Lbus Limmediate expression
      { {interfacesignalnames=$2; interfacesignalmode=$4; interfacesignalsubtype=$5; interfacesignalkind=SignalKindBus; interfacesignalexpression=$8} }

/*
(* interface_default_declaration ::=                             *)
(*   identifier_list : [mode] subtype_indication [bus] *)
(*   [:= static_expression] (ieee syn ignored)                  *)
*/
interface_default_declaration:
  | identifier_list Lcolon subtype_indication
      { {interfacedefaultnames=$1; interfacedefaultmode=InterfaceModeIn; interfacedefaultsubtype=$3; interfacedefaultkind=SignalKindDefault; interfacedefaultexpression=empty_expression} }
  | identifier_list Lcolon mode subtype_indication
      { {interfacedefaultnames=$1; interfacedefaultmode=$3; interfacedefaultsubtype=$4; interfacedefaultkind=SignalKindDefault; interfacedefaultexpression=empty_expression} }
  | identifier_list Lcolon subtype_indication Lbus
      { {interfacedefaultnames=$1; interfacedefaultmode=InterfaceModeIn; interfacedefaultsubtype=$3; interfacedefaultkind=SignalKindBus; interfacedefaultexpression=empty_expression} }
  | identifier_list Lcolon mode subtype_indication Lbus
      { {interfacedefaultnames=$1; interfacedefaultmode=$3; interfacedefaultsubtype=$4; interfacedefaultkind=SignalKindBus; interfacedefaultexpression=empty_expression} }
  | identifier_list Lcolon subtype_indication Limmediate expression
      { {interfacedefaultnames=$1; interfacedefaultmode=InterfaceModeIn; interfacedefaultsubtype=$3; interfacedefaultkind=SignalKindDefault; interfacedefaultexpression=$5} }
  | identifier_list Lcolon mode subtype_indication Limmediate expression
      { {interfacedefaultnames=$1; interfacedefaultmode=$3; interfacedefaultsubtype=$4; interfacedefaultkind=SignalKindDefault; interfacedefaultexpression=$6} }
  | identifier_list Lcolon subtype_indication Lbus Limmediate expression
      { {interfacedefaultnames=$1; interfacedefaultmode=InterfaceModeIn; interfacedefaultsubtype=$3; interfacedefaultkind=SignalKindBus; interfacedefaultexpression=$6} }
  | identifier_list Lcolon mode subtype_indication Lbus Limmediate expression
      { {interfacedefaultnames=$1; interfacedefaultmode=$3; interfacedefaultsubtype=$4; interfacedefaultkind=SignalKindBus; interfacedefaultexpression=$7} }

/*
(* interface_variable_declaration ::=                           *)
(*   variable identifier_list : [mode] subtype_indication     *)
(*   [:= static_expression] (ieee syn ignored)                  *)
*/
interface_variable_declaration:
  | Lvariable identifier_list Lcolon subtype_indication
      { {interfacevariablenames=$2; interfacevariablemode=InterfaceModeIn; interfacevariablesubtype=$4; interfacevariableexpression=empty_expression} }
  | Lvariable identifier_list Lcolon mode subtype_indication
      { {interfacevariablenames=$2; interfacevariablemode=$4; interfacevariablesubtype=$5; interfacevariableexpression=empty_expression} }
  | Lvariable identifier_list Lcolon subtype_indication Limmediate expression
      { {interfacevariablenames=$2; interfacevariablemode=InterfaceModeIn; interfacevariablesubtype=$4; interfacevariableexpression=$6} }
  | Lvariable identifier_list Lcolon mode subtype_indication Limmediate expression
      { {interfacevariablenames=$2; interfacevariablemode=$4; interfacevariablesubtype=$5; interfacevariableexpression=$7} }

/*
(* interface_file_declaration ::= (ieee syn not supported)      *)
(*   file identifier_list : subtype_indication                  *)
*/
interface_file_declaration:
  | Lfile identifier_list Lcolon subtype_indication
      { {interfacefilenames=$2; interfacefilesubtype=$4} }

/*
(* mode ::= in | out | inout | buffer | linkage                 *)
(*   (linkage ieee not supported)                               *)
*/
mode:
  | Lin
      { InterfaceModeIn }
  | Lout
      { InterfaceModeOut }
  | Linout
      { InterfaceModeInOut }
  | Lbuffer
      { InterfaceModeBuffer }

/*
(* interface_list ::=                          *)
(*   interface_element {; interface_element}   *)
*/
interface_list:
  | interface_element
      { [$1] }
  | interface_list Lsemicolon interface_element
      { ($1 @ [$3]) }

/*
(* interface_element ::= interface_declaration *)
*/
interface_element:
  | interface_declaration
      { $1 }

/*
(* alias_declaration ::=                           *)
(*   alias alias_designator [: subtype_indication] *)
(*   is name [signature];                          *)
*/
alias_declaration:
  | Lalias alias_designator Lis expression Lsemicolon
      { {aliasdesignator=$2; aliassubtype=empty_subtype_indication; aliasexpression=$4; aliassignature=None} }
  | Lalias alias_designator Lcolon subtype_indication Lis expression Lsemicolon
      { {aliasdesignator=$2; aliassubtype=$4; aliasexpression=$6; aliassignature=None} }
  | Lalias alias_designator Lis expression signature Lsemicolon
      { {aliasdesignator=$2; aliassubtype=empty_subtype_indication; aliasexpression=$4; aliassignature=Some $5} }
  | Lalias alias_designator Lcolon subtype_indication Lis expression signature Lsemicolon
      { {aliasdesignator=$2; aliassubtype=$4; aliasexpression=$6; aliassignature=Some $7} }

/*
(* alias_designator ::= identifier                 *)
(*   | character_literal | operator_symbol         *)
*/
alias_designator:
  | designator
      { $1 }

/*
(* attribute_specification ::=                        *)
(*   attribute attribute_designator                   *)
(*   of entity_specification is expression;           *)
*/
attribute_specification:
  | Lattribute identifier Lof entity_specification Lis expression Lsemicolon
      { {attributedesignator=$2; attributeentity=$4; attributeexpression=$6} }

/*
(* attribute_declaration ::=             *)
(*   attribute identifier : type_mark ;  *)
*/
attribute_declaration:
  | Lattribute identifier Lcolon type_mark Lsemicolon
      { {attributename=$2; attributetypename=$4} }

/*
(* entity_specification ::=                           *)
(*   entity_name_list : entity_class                  *)
*/
entity_specification:
  | entity_name_list Lcolon entity_class
      { {entitynames=$1; entityclass=$3} }

/*
(* entity_class ::=                                   *)
(*     entity    | architecture | configuration       *)
(*   | procedure | function     | package             *)
(*   | type      | subtype      | constant            *)
(*   | signal    | variable     | component           *)
(*   | label     | literal      | units               *)
(*   | group     | file (both ieee syn not supported) *)
*/
entity_class:
  | Lentity
      { ClassEntity }
  | Larchitecture
      { ClassArchitecture }
  | Lconfiguration
      { ClassConfiguration }
  | Lprocedure
      { ClassProcedure }
  | Lfunction
      { ClassFunction }
  | Lpackage
      { ClassPackage }
  | Ltype
      { ClassType }
  | Lsubtype
      { ClassSubType }
  | Lconstant
      { ClassConstant }
  | Lsignal
      { ClassSignal }
  | Lvariable
      { ClassVariable }
  | Lcomponent
      { ClassComponent }
  | Llabel
      { ClassLabel }
  | Lliteral
      { ClassLiteral }
  | Lunits
      { ClassUnits }
  | Lfile
      { ClassFile }

/*
(* entity_name_list ::=                               *)
(*     entity_designator {, entity_designator}        *)
(*   | others                                         *)
(*   | all                                            *)
*/
entity_name_list:
  | entity_designator
      { EntityDesignator [$1] }
  | entity_name_list Lcomma entity_designator
      { match $1 with
          | EntityDesignator l -> EntityDesignator (l @ [$3]);
          | _                  -> raise Parsing.Parse_error; }
  | Lothers
      { EntityOthers }
  | Lall
      { EntityAll }

/*
(* entity_designator ::= entity_tag [signature]       *)
*/
entity_designator:
  | entity_tag
      { {entitytag=$1; entitysignature=None} }
  | entity_tag signature
      { {entitytag=$1; entitysignature=Some $2 } }

/*
(* entity_tag ::=                                     *)
(*     simple_name                                    *)
(*   | character_literal                              *)
(*   | operator_symbol                                *)
*/
entity_tag:
  | simple_name
      { EntityTagSimpleName $1 }
  | character_literal
      { EntityTagCharacterLiteral $1 }
  | Lstring
      { EntityTagOperatorSymbol $1 }

/*
(* wait_statement ::=                               *)
(*   [label:] wait [sensitivity_clause]             *)
(*   [condition_clause] [timeout_clause] ;          *)
*/
wait_statement:
  | Lwait Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=[]; waitcondition=empty_condition; waittimeout=empty_expression} }
  | label Lcolon Lwait Lsemicolon
      { {waitlabelname=$1; waitsensitivity=[]; waitcondition=empty_condition; waittimeout=empty_expression} }
  | Lwait sensitivity_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=$2; waitcondition=empty_condition; waittimeout=empty_expression} }
  | label Lcolon Lwait sensitivity_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=$4; waitcondition=empty_condition; waittimeout=empty_expression} }
  | Lwait condition_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=[]; waitcondition=$2; waittimeout=empty_expression} }
  | label Lcolon Lwait condition_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=[]; waitcondition=$4; waittimeout=empty_expression} }
  | Lwait sensitivity_clause condition_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=$2; waitcondition=$3; waittimeout=empty_expression} }
  | label Lcolon Lwait sensitivity_clause condition_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=$4; waitcondition=$5; waittimeout=empty_expression} }
  | Lwait timeout_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=[]; waitcondition=empty_condition; waittimeout=$2} }
  | label Lcolon Lwait timeout_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=[]; waitcondition=empty_condition; waittimeout=$4} }
  | Lwait sensitivity_clause timeout_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=$2; waitcondition=empty_condition; waittimeout=$3} }
  | label Lcolon Lwait sensitivity_clause timeout_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=$4; waitcondition=empty_condition; waittimeout=$5} }
  | Lwait condition_clause timeout_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=[]; waitcondition=$2; waittimeout=$3} }
  | label Lcolon Lwait condition_clause timeout_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=[]; waitcondition=$4; waittimeout=$5} }
  | Lwait sensitivity_clause condition_clause timeout_clause Lsemicolon
      { {waitlabelname=empty_label; waitsensitivity=$2; waitcondition=$3; waittimeout=$4} }
  | label Lcolon Lwait sensitivity_clause condition_clause timeout_clause Lsemicolon
      { {waitlabelname=$1; waitsensitivity=$4; waitcondition=$5; waittimeout=$6} }

/*
(* sensitivity_clause ::= on sensitivity_list       *)
*/
sensitivity_clause:
  | Lon sensitivity_list
      { $2 }

/*
(* sensitivity_list ::= signal_name {, signal_name} *)
signals may be indexed or sliced
*/
sensitivity_list:
  | expression_list
      { $1 }

/*
(* condition_clause ::= until condition             *)
(* condition ::= boolean_expression                 *)
*/
condition_clause:
  | Luntil condition
      { $2 }

/*
(* timeout_clause ::= for time_expression           *)
*/
timeout_clause:
  | Lfor expression
      { $2 }

/*
(* assertion_statement ::= [ label: ] assertion ; *)
*/
assertion_statement:
  | assertion Lsemicolon
      { {assertionlabelname=empty_label; assertion=$1} }
  | label Lcolon assertion Lsemicolon
      { {assertionlabelname=$1; assertion=$3} }

/*
(* assertion ::=                                  *)
(*   assert condition [ report expression ]       *)
(*   [ severity expression ]                      *)
(* (ieee syn ignored)                             *)
*/
assertion:
  | Lassert condition
      { {assertioncondition=$2; assertionreport=empty_expression; assertionseverity=empty_expression} }
  | Lassert condition Lreport expression
      { {assertioncondition=$2; assertionreport=$4; assertionseverity=empty_expression} }
  | Lassert condition Lseverity expression
      { {assertioncondition=$2; assertionreport=empty_expression; assertionseverity=$4} }
  | Lassert condition Lreport expression Lseverity expression
      { {assertioncondition=$2; assertionreport=$4; assertionseverity=$6} }

/*
(* report_statement ::=            *)
(*   [label:] report expression    *)
(*   [severity expression] ;       *)
(* (ieee syn ignored)              *)
*/
report_statement:
  | Lreport expression Lsemicolon
      { {reportlabelname=empty_label; reportexpression=$2; reportseverity=empty_expression} }
  | label Lcolon Lreport expression Lsemicolon
      { {reportlabelname=$1; reportexpression=$4; reportseverity=empty_expression} }
  | Lreport expression Lseverity expression Lsemicolon
      { {reportlabelname=empty_label; reportexpression=$2; reportseverity=$4} }
  | label Lcolon Lreport expression Lseverity expression Lsemicolon
      { {reportlabelname=$1; reportexpression=$4; reportseverity=$6} }

/*
(* signal_assignment_statement ::=                      *)
(*     [ label : ] simple_signal_assignment             *)
(*   | [ label : ] conditional_signal_assignment (TODO) *)
(*   | [ label : ] selected_signal_assignment (TODO)    *)
*/
signal_assignment_statement:
  | simple_signal_assignment_statement
      { SimpleSignalAssignment $1 }
  | conditional_signal_assignment_statement
      { ConditionalSignalAssignment $1 }
  | selected_signal_assignment_statement
      { SelectedSignalAssignment $1 }

/*
(* simple_signal_assignment ::=        *)
(*     simple_waveform_assignment      *)
(*  | simple_force_assignment (TODO)   *)
(*  | simple_release_assignment (TODO) *)
*/
simple_signal_assignment_statement:
  | simple_waveform_assignment_statement
      { $1 }

/*
(* simple_waveform_assignment ::=            *)
(*  target <= [ delay_mechanism ] waveform ; *)
*/
simple_waveform_assignment_statement:
  | target Lless Lequal waveform Lsemicolon
      { {simplesignalassignmentlabelname=empty_label;
         simplesignalassignmenttarget=$1;
         simplesignalassignmentdelay=DelayNone;
         simplesignalassignmentwaveform=$4} }
  | label Lcolon target Lless Lequal waveform Lsemicolon
      { {simplesignalassignmentlabelname=$1;
         simplesignalassignmenttarget=$3;
         simplesignalassignmentdelay=DelayNone;
         simplesignalassignmentwaveform=$6} }
  | target Lless Lequal delay_mechanism waveform Lsemicolon
      { {simplesignalassignmentlabelname=empty_label;
         simplesignalassignmenttarget=$1;
         simplesignalassignmentdelay=$4;
         simplesignalassignmentwaveform=$5} }
  | label Lcolon target Lless Lequal delay_mechanism waveform Lsemicolon
      { {simplesignalassignmentlabelname=$1;
         simplesignalassignmenttarget=$3;
         simplesignalassignmentdelay=$6;
         simplesignalassignmentwaveform=$7} }

/*
(* conditional_signal_assignment ::=                       *)
(*     conditional_waveform_assignment                     *)
(*   | conditional_force_assignment (TODO)                 *)
*/
conditional_signal_assignment_statement:
  | conditional_waveform_assignment_statement
      { $1 }

/*
(* conditional_waveform_assignment ::=                     *)
(*   target <= [ delay_mechanism ] conditional_waveforms ; *)
*/
conditional_waveform_assignment_statement:
  | target Lless Lequal conditional_waveforms Lsemicolon
      { {conditionalsignalassignmentlabelname=empty_label;
         conditionalsignalassignmenttarget=$1;
         conditionalsignalassignmentdelay=DelayNone;
         conditionalsignalassignmentwaveforms=$4} }
  | label Lcolon target Lless Lequal conditional_waveforms Lsemicolon
      { {conditionalsignalassignmentlabelname=$1;
         conditionalsignalassignmenttarget=$3;
         conditionalsignalassignmentdelay=DelayNone;
         conditionalsignalassignmentwaveforms=$6} }
  | target Lless Lequal delay_mechanism conditional_waveforms Lsemicolon
      { {conditionalsignalassignmentlabelname=empty_label;
         conditionalsignalassignmenttarget=$1;
         conditionalsignalassignmentdelay=$4;
         conditionalsignalassignmentwaveforms=$5} }
  | label Lcolon target Lless Lequal delay_mechanism conditional_waveforms Lsemicolon
      { {conditionalsignalassignmentlabelname=$1;
         conditionalsignalassignmenttarget=$3;
         conditionalsignalassignmentdelay=$6;
         conditionalsignalassignmentwaveforms=$7} }

/*
(* selected_signal_assignment ::=                         *)
(*     selected_waveform_assignment                       *)
(*   | selected_force_assignment (TODO)                   *)
*/
selected_signal_assignment_statement:
  | selected_waveform_assignment_statement
      { $1 }

/*
(* selected_waveform_assignment ::=                       *)
(*   with expression select [ ? ]                         *)
(*     target <= [ delay_mechanism ] selected_waveforms ; *)
*/
selected_waveform_assignment_statement:
  | Lwith selector Lselect target Lless Lequal selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=empty_label;
         selectedsignalassignmenttarget=$4;
         selectedsignalassignmentselector=$2;
         selectedsignalassignmentkind=OrdinarySelection;
         selectedsignalassignmentdelay=DelayNone;
         selectedsignalassignmentwaveforms=$7} }
  | label Lcolon Lwith selector Lselect target Lless Lequal selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=$1;
         selectedsignalassignmenttarget=$6;
         selectedsignalassignmentselector=$4;
         selectedsignalassignmentkind=OrdinarySelection;
         selectedsignalassignmentdelay=DelayNone;
         selectedsignalassignmentwaveforms=$9} }
  | Lwith selector Lselect target Lless Lequal delay_mechanism selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=empty_label;
         selectedsignalassignmenttarget=$4;
         selectedsignalassignmentselector=$2;
         selectedsignalassignmentkind=OrdinarySelection;
         selectedsignalassignmentdelay=$7;
         selectedsignalassignmentwaveforms=$8} }
  | label Lcolon Lwith selector Lselect target Lless Lequal delay_mechanism selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=$1;
         selectedsignalassignmenttarget=$6;
         selectedsignalassignmentselector=$4;
         selectedsignalassignmentkind=OrdinarySelection;
         selectedsignalassignmentdelay=$9;
         selectedsignalassignmentwaveforms=$10} }
  | Lwith selector Lselect Lquestionmark target Lless Lequal selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=empty_label;
         selectedsignalassignmenttarget=$5;
         selectedsignalassignmentselector=$2;
         selectedsignalassignmentkind=MatchingSelection;
         selectedsignalassignmentdelay=DelayNone;
         selectedsignalassignmentwaveforms=$8} }
  | label Lcolon Lwith selector Lselect Lquestionmark target Lless Lequal selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=$1;
         selectedsignalassignmenttarget=$7;
         selectedsignalassignmentselector=$4;
         selectedsignalassignmentkind=MatchingSelection;
         selectedsignalassignmentdelay=DelayNone;
         selectedsignalassignmentwaveforms=$10} }
  | Lwith selector Lselect Lquestionmark target Lless Lequal delay_mechanism selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=empty_label;
         selectedsignalassignmenttarget=$5;
         selectedsignalassignmentselector=$2;
         selectedsignalassignmentkind=MatchingSelection;
         selectedsignalassignmentdelay=$8;
         selectedsignalassignmentwaveforms=$9} }
  | label Lcolon Lwith selector Lselect Lquestionmark target Lless Lequal delay_mechanism selected_waveforms Lsemicolon
      { {selectedsignalassignmentlabelname=$1;
         selectedsignalassignmenttarget=$7;
         selectedsignalassignmentselector=$4;
         selectedsignalassignmentkind=MatchingSelection;
         selectedsignalassignmentdelay=$10;
         selectedsignalassignmentwaveforms=$11} }

/*
(* delay_mechanism ::= (ieee syn ignored)                *)
(*     transport                                         *)
(*   | [ reject time_expression ] inertial               *)
*/
delay_mechanism:
  | Ltransport
      { DelayTransport }
  | Linertial
      { DelayInertial empty_expression }
  | Lreject expression Linertial
      { DelayInertial $2 }

/*
(* target ::=                                            *)
(*     name                                              *)
(*   | aggregate                                         *)
*/
target:
  | name
      { TargetName $1}
  | dotted
      { TargetDotted $1}
  | name parameters
      { TargetNameParameters ($1,$2) }
/*
  | name Ldot suffix
      { SelectTargetName ($1,$3) }
  | name parameters Ldot suffix
      { SelectTargetNameParameters ($1,$2,$4) }
*/
  | aggregate
      { TargetAggregate $1}

/*
(* waveform ::=                                          *)
(*     waveform_element                                  *)
(*     {, waveform_element} (ieee syn ignored)           *)
(*   | unaffected                                        *)
*/
waveform:
  | waveform_element
      { WaveForms [$1] }
  | waveform Lcomma waveform_element
      { match $1 with
          | WaveForms l -> WaveForms (l @ [$3])
          | _           -> raise Parsing.Parse_error}
  | Lunaffected
      { Unaffected }

/*
(* waveform_element ::=                                  *)
(*     value_expression [after time_expression]          *)
(*   | null [after time_expression]                      *)
*/
waveform_element:
  | expression
      { {valueexpression=$1; timeexpression=empty_expression} }
  | expression Lafter expression
      { {valueexpression=$1; timeexpression=$3} }
  | Lnull
      { {valueexpression=empty_expression; timeexpression=empty_expression} }
  | Lnull Lafter expression
      { {valueexpression=empty_expression; timeexpression=$3} }

/*
(* variable_assignment_statement ::=               *)
(*     [ label : ] simple_variable_assignment      *)
(*   | [ label : ] conditional_variable_assignment *)
(*   | [ label : ] selected_variable_assignment    *)
*/
variable_assignment_statement:
  | simple_variable_assignment
      { SimpleVariableAssignment $1 }
  | conditional_variable_assignment
      { ConditionalVariableAssignment $1 }
  | selected_variable_assignment
      { SelectedVariableAssignment $1 }

/*
(* simple_variable_assignment ::= *)
(*   target := expression ;       *)
*/
simple_variable_assignment:
  | target Limmediate expression Lsemicolon
      { {simplevariableassignmentlabelname=empty_label;
         simplevariableassignmenttarget=$1;
         simplevariableassignmentexpression=$3} }
  | label Lcolon target Limmediate expression Lsemicolon
      { {simplevariableassignmentlabelname=$1;
         simplevariableassignmenttarget=$3;
         simplevariableassignmentexpression=$5} }

/*
(* conditional_variable_assignment ::=   *)
(*   target := conditional_expressions ; *)
*/
conditional_variable_assignment:
  | target Limmediate conditional_expressions Lsemicolon
      { {conditionalvariableassignmentlabelname=empty_label;
         conditionalvariableassignmenttarget=$1;
         conditionalvariableassignmentexpressions=$3} }
  | label Lcolon target Limmediate conditional_expressions Lsemicolon
      { {conditionalvariableassignmentlabelname=$1;
         conditionalvariableassignmenttarget=$3;
         conditionalvariableassignmentexpressions=$5} }

/*
(* conditional_expressions ::=          *)
(*   expression when condition          *)
(*   { else expression when condition } *)
(*   [ else expression ]                *)
*/
conditional_expressions:
  | expression Lwhen condition
      { [{conditionalexpressionvalue=$1;
          conditionalexpressioncondition=$3}] }
  | conditional_expressions Lelse expression
      { if (List.nth $1 (-1 + List.length $1)).conditionalexpressioncondition <> empty_condition then
          ($1 @ [{conditionalexpressionvalue=$3;
                  conditionalexpressioncondition=empty_condition}])
        else
          raise Parsing.Parse_error }
  | conditional_expressions Lelse expression Lwhen condition
      { if (List.nth $1 (-1 + List.length $1)).conditionalexpressioncondition <> empty_condition then
          ($1 @ [{conditionalexpressionvalue=$3;
                  conditionalexpressioncondition=$5}])
        else
          raise Parsing.Parse_error }

/*
(* selected_variable_assignment ::=      *)
(*   with expression select [ ? ]        *)
(*     target := selected_expressions ;  *)
*/
selected_variable_assignment:
  | Lwith selector Lselect target Limmediate selected_expressions Lsemicolon
      { {selectedvariableassignmentlabelname=empty_label;
         selectedvariableassignmenttarget=$4;
         selectedvariableassignmentselector=$2;
         selectedvariableassignmentkind=OrdinarySelection;
         selectedvariableassignmentexpressions=$6} }
  | label Lcolon Lwith selector Lselect target Limmediate selected_expressions Lsemicolon
      { {selectedvariableassignmentlabelname=$1;
         selectedvariableassignmenttarget=$6;
         selectedvariableassignmentselector=$4;
         selectedvariableassignmentkind=OrdinarySelection;
         selectedvariableassignmentexpressions=$8} }
  | Lwith selector Lselect Lquestionmark target Limmediate selected_expressions Lsemicolon
      { {selectedvariableassignmentlabelname=empty_label;
         selectedvariableassignmenttarget=$5;
         selectedvariableassignmentselector=$2;
         selectedvariableassignmentkind=MatchingSelection;
         selectedvariableassignmentexpressions=$7} }
  | label Lcolon Lwith selector Lselect Lquestionmark target Limmediate selected_expressions Lsemicolon
      { {selectedvariableassignmentlabelname=$1;
         selectedvariableassignmenttarget=$7;
         selectedvariableassignmentselector=$4;
         selectedvariableassignmentkind=MatchingSelection;
         selectedvariableassignmentexpressions=$9} }

/*
(* selected_expressions ::=        *)
(*   { expression when choices , } *)
(*   expression when choices       *)
*/
selected_expressions:
  | expression Lwhen choices
      { [{selectedexpressionvalue=$1;
          selectedexpressionchoices=$3}] }
  | selected_expressions Lcomma expression Lwhen choices
      { ($1 @ [{selectedexpressionvalue=$3;
                selectedexpressionchoices=$5}]) }

/*
(* procedure_call_statement ::=                   *)
(*   [ label: ] procedure_call ;                  *)
*/
procedure_call_statement:
  | procedure_call Lsemicolon
      { {procedurecalllabelname=empty_label; procedurecall=$1} }
  | label Lcolon procedure_call Lsemicolon
      { {procedurecalllabelname=$1; procedurecall=$3} }

/*
(* procedure_call ::=                             *)
(*   procedure_name [ ( actual_parameter_part ) ] *)
*/
procedure_call:
  | name
      { {procedurecallname=$1; procedurecallparameter=[]} }
  | name parameter
      { {procedurecallname=$1; procedurecallparameter=$2} }

/*
(* if_statement ::=               *)
(*   [ if_label: ]                *)
(*     if condition then          *)
(*       sequence_of_statements   *)
(*   { elsif condition then       *)
(*       sequence_of_statements } *)
(*   [ else                       *)
(*       sequence_of_statements ] *)
(*     end if [ if_label ] ;      *)
*/
if_statement:
  | Lif condition then_statements Lend Lif Lsemicolon
      { {iflabelname=empty_label; ifcondition=$2; thenstatements=$3; elsestatements=ElseNone} }
  | label Lcolon Lif condition then_statements Lend Lif Lsemicolon
      { {iflabelname=$1; ifcondition=$4; thenstatements=$5; elsestatements=ElseNone} }
  | label Lcolon Lif condition then_statements Lend Lif label Lsemicolon
      { {iflabelname=check_labels $1 $8; ifcondition=$4; thenstatements=$5; elsestatements=ElseNone} }
  | Lif condition then_statements else_statements Lend Lif Lsemicolon
      { {iflabelname=empty_label; ifcondition=$2; thenstatements=$3; elsestatements=$4} }
  | label Lcolon Lif condition then_statements else_statements Lend Lif Lsemicolon
      { {iflabelname=$1; ifcondition=$4; thenstatements=$5; elsestatements=$6} }
  | label Lcolon Lif condition then_statements else_statements Lend Lif label Lsemicolon
      { {iflabelname=check_labels $1 $9; ifcondition=$4; thenstatements=$5; elsestatements=$6} }

then_statements:
  | Lthen
      { [] }
  | Lthen sequence_of_statements
      { $2 }

else_statements:
  | Lelse
      { ElseNone }
  | Lelse sequence_of_statements
      { Else $2 }
  | elsif_statements
      { Elsif $1 }

elsif_statements:
  | Lelsif condition then_statements
      { {iflabelname=empty_label; ifcondition=$2; thenstatements=$3; elsestatements=ElseNone} }
  | Lelsif condition then_statements else_statements
      { {iflabelname=empty_label; ifcondition=$2; thenstatements=$3; elsestatements=$4} }

/*
(* case_statement ::=                 *)
(*   [ case_label: ]                  *)
(*     case expression is             *)
(*       case_statement_alternative   *)
(*     { case_statement_alternative } *)
(*     end case [ case_label ] ;      *)
*/
case_statement:
  | Lcase selector Lis case_statement_alternative_list Lend Lcase Lsemicolon
      { {caselabelname=empty_label;
         caseselector=$2;
         casekind=OrdinarySelection;
         casealternatives=$4} }
  | label Lcolon Lcase selector Lis case_statement_alternative_list Lend Lcase Lsemicolon
      { {caselabelname=$1;
         caseselector=$4;
         casekind=OrdinarySelection;
         casealternatives=$6} }
  | label Lcolon Lcase selector Lis case_statement_alternative_list Lend Lcase label Lsemicolon
      { {caselabelname=check_labels $1 $9;
         caseselector=$4;
         casekind=OrdinarySelection;
         casealternatives=$6} }
  | Lcase Lquestionmark selector Lis case_statement_alternative_list Lend Lcase Lquestionmark Lsemicolon
      { {caselabelname=empty_label;
         caseselector=$3;
         casekind=MatchingSelection;
         casealternatives=$5} }
  | label Lquestionmark Lcolon Lcase selector Lis case_statement_alternative_list Lend Lcase Lquestionmark Lsemicolon
      { {caselabelname=$1;
         caseselector=$5;
         casekind=MatchingSelection;
         casealternatives=$7} }
  | label Lquestionmark Lcolon Lcase selector Lis case_statement_alternative_list Lend Lcase Lquestionmark label Lsemicolon
      { {caselabelname=check_labels $1 $11;
         caseselector=$5;
         casekind=MatchingSelection;
         casealternatives=$7} }

/*
(* case_statement_alternative ::=     *)
(*   when choices =>                  *)
(*     sequence_of_statements         *)
*/
case_statement_alternative:
  | Lwhen choices Lassociation
      { {casechoices=$2; casestatements=[]} }
  | Lwhen choices Lassociation sequence_of_statements
      { {casechoices=$2; casestatements=$4} }

case_statement_alternative_list:
  | case_statement_alternative
      { [$1] }
  | case_statement_alternative_list case_statement_alternative
      { ($1 @ [$2]) }

/*
(* loop_statement ::=                           *)
(*   [ loop_label: ]                            *)
(*   [ iteration_scheme ] loop                  *)
(*     sequence_of_statements                   *)
(*   end loop [ loop_label ] ;                  *)
*/
loop_statement:
  | loop_statements Lend Lloop Lsemicolon
      { {looplabelname=empty_label; loopiteration=AlwaysLoop; loopstatements=$1} }
  | label Lcolon loop_statements Lend Lloop Lsemicolon
      { {looplabelname=$1; loopiteration=AlwaysLoop; loopstatements=$3} }
  | label Lcolon loop_statements Lend Lloop label Lsemicolon
      { {looplabelname=check_labels $1 $6; loopiteration=AlwaysLoop; loopstatements=$3} }
  | iteration_scheme loop_statements Lend Lloop Lsemicolon
      { {looplabelname=empty_label; loopiteration=$1; loopstatements=$2} }
  | label Lcolon iteration_scheme loop_statements Lend Lloop Lsemicolon
      { {looplabelname=$1; loopiteration=$3; loopstatements=$4} }
  | label Lcolon iteration_scheme loop_statements Lend Lloop label Lsemicolon
      { {looplabelname=check_labels $1 $7; loopiteration=$3; loopstatements=$4} }

loop_statements:
  | Lloop
      { [] }
  | Lloop sequence_of_statements
      { $2 }

/*
(* iteration_scheme ::=                         *)
(*     while condition (ieee syn not supported) *)
(*   | for loop_parameter_specification         *)
*/
iteration_scheme:
  | Lwhile condition
      { WhileLoop $2 }
  | Lfor parameter_specification
      { ForLoop $2 }

/*
(* parameter_specification ::=                  *)
(*   identifier in discrete_range               *)
*/
parameter_specification:
  | identifier Lin discrete_range
      { {parameteridentifier=$1; parameterrange=$3} }

/*
(* next_statement ::=                         *)
(*   [ label: ]                               *)
(*   next [ loop_label ] [ when condition ] ; *)
*/
next_statement:
  | Lnext Lsemicolon
      { {nextlabelname=empty_label; nextlooplabelname=empty_label; nextcondition=empty_condition} }
  | label Lcolon Lnext Lsemicolon
      { {nextlabelname=$1; nextlooplabelname=empty_label; nextcondition=empty_condition} }
  | Lnext label Lsemicolon
      { {nextlabelname=empty_label; nextlooplabelname=$2; nextcondition=empty_condition} }
  | label Lcolon Lnext label Lsemicolon
      { {nextlabelname=$1; nextlooplabelname=$4; nextcondition=empty_condition} }
  | Lnext Lwhen condition Lsemicolon
      { {nextlabelname=empty_label; nextlooplabelname=empty_label; nextcondition=$3} }
  | label Lcolon Lnext Lwhen condition Lsemicolon
      { {nextlabelname=$1; nextlooplabelname=empty_label; nextcondition=$5} }
  | Lnext label Lwhen condition Lsemicolon
      { {nextlabelname=empty_label; nextlooplabelname=$2; nextcondition=$4} }
  | label Lcolon Lnext label Lwhen condition Lsemicolon
      { {nextlabelname=$1; nextlooplabelname=$4; nextcondition=$6} }

/*
(* exit_statement ::=                         *)
(*   [ label: ]                               *)
(*   exit [ loop_label ] [ when condition ] ; *)
*/
exit_statement:
  | Lexit Lsemicolon
      { {exitlabelname=empty_label; exitlooplabelname=empty_label; exitcondition=empty_condition} }
  | label Lcolon Lexit Lsemicolon
      { {exitlabelname=$1; exitlooplabelname=empty_label; exitcondition=empty_condition} }
  | Lexit label Lsemicolon
      { {exitlabelname=empty_label; exitlooplabelname=$2; exitcondition=empty_condition} }
  | label Lcolon Lexit label Lsemicolon
      { {exitlabelname=$1; exitlooplabelname=$4; exitcondition=empty_condition} }
  | Lexit Lwhen condition Lsemicolon
      { {exitlabelname=empty_label; exitlooplabelname=empty_label; exitcondition=$3} }
  | label Lcolon Lexit Lwhen condition Lsemicolon
      { {exitlabelname=$1; exitlooplabelname=empty_label; exitcondition=$5} }
  | Lexit label Lwhen condition Lsemicolon
      { {exitlabelname=empty_label; exitlooplabelname=$2; exitcondition=$4} }
  | label Lcolon Lexit label Lwhen condition Lsemicolon
      { {exitlabelname=$1; exitlooplabelname=$4; exitcondition=$6} }

/*
(* return_statement ::=                 *)
(*   [ label: ] return [ expression ] ; *)
*/
return_statement:
  | Lreturn Lsemicolon
      { {returnlabelname=empty_label; returnexpression=empty_expression} }
  | label Lcolon Lreturn Lsemicolon
      { {returnlabelname=$1; returnexpression=empty_expression} }
  | Lreturn expression Lsemicolon
      { {returnlabelname=empty_label; returnexpression=$2} }
  | label Lcolon Lreturn expression Lsemicolon
      { {returnlabelname=$1; returnexpression=$4} }

/*
(* null_statement ::=             *)
(*   [ label: ] null ;            *)
*/
null_statement:
  | Lnull Lsemicolon
      { {nulllabelname=empty_label} }
  | label Lcolon Lnull Lsemicolon
      { {nulllabelname=$1} }

/*
(* sequence_of_statements ::=                 *)
(*   { sequential_statement }                 *)
*/
sequence_of_statements:
  | sequential_statement
      { [$1] }
  | sequence_of_statements sequential_statement
      { ($1 @ [$2]) }

/*
(* sequential_statement ::=                   *)
(*     wait_statement                         *)
(*   | assertion_statement (ieee syn ignored) *)
(*   | report_statement (ieee syn ignored)    *)
(*   | signal_assignment_statement            *)
(*   | variable_assignment_statement          *)
(*   | procedure_call_statement               *)
(*   | if_statement                           *)
(*   | case_statement                         *)
(*   | loop_statement                         *)
(*   | next_statement                         *)
(*   | exit_statement                         *)
(*   | return_statement                       *)
(*   | null_statement                         *)
*/
sequential_statement:
  | wait_statement
      { SequentialWait $1 }
  | assertion_statement
      { SequentialAssertion $1 }
  | report_statement
      { SequentialReport $1 }
  | signal_assignment_statement
      { SequentialSignalAssignment $1 }
  | variable_assignment_statement
      { SequentialVariableAssignment $1 }
  | procedure_call_statement
      { SequentialProcedureCall $1 }
  | if_statement
      { SequentialIf $1 }
  | case_statement
      { SequentialCase $1 }
  | loop_statement
      { SequentialLoop $1 }
  | next_statement
      { SequentialNext $1 }
  | exit_statement
      { SequentialExit $1 }
  | return_statement
      { SequentialReturn $1 }
  | null_statement
      { SequentialNull $1 }

/*
(* subprogram_declaration ::=                                              *)
(*   subprogram_specification ;                                            *)
*/
subprogram_declaration:
  | subprogram_specification Lsemicolon
      { $1 }

/*
(* subprogram_specification ::=                                            *)
(*     procedure_specification                                             *)
(*   | function_specification                                              *)
*/
subprogram_specification:
  | procedure_specification
      { ProcedureSpecification $1 }
  | function_specification
      { FunctionSpecification $1 }

/*
(* procedure_specification ::=                                             *)
(*   procedure designator                                                  *)
(*   subprogram_header                                                     *)
(*   [ [ parameter ] ( formal_parameter_list ) ]                           *)
*/
procedure_specification:
/* conflict with subprogram instantiations
  | Lprocedure designator
      { {procedurespecdesignator=$2;
         procedurespecparameters=[];
         procedurespecgenericlist=[];
         procedurespecgenericmapaspect=[]} }*/
  | Lprocedure designator subprogram_parameters
      { {procedurespecdesignator=$2;
         procedurespecparameters=$3;
         procedurespecgenericlist=[];
         procedurespecgenericmapaspect=[]} }
  | Lprocedure designator subprogram_header
      { match $3 with
        | generic_list,generic_map_aspect
            -> {procedurespecdesignator=$2;
                procedurespecparameters=[];
                procedurespecgenericlist=generic_list;
                procedurespecgenericmapaspect=generic_map_aspect} }
  | Lprocedure designator subprogram_header subprogram_parameters
      { match $3 with
        | generic_list,generic_map_aspect
            -> {procedurespecdesignator=$2;
                procedurespecparameters=$4;
                procedurespecgenericlist=generic_list;
                procedurespecgenericmapaspect=generic_map_aspect} }

/*
(* function_specification ::=                                              *)
(*   [ pure | impure ] function designator                                 *)
(*   subprogram_header                                                     *)
(*   [ [ parameter ] ( formal_parameter_list ) ] return type_mark          *)
*/
function_specification:
  | Lfunction designator Lreturn type_mark
      { {functionspecdesignator=$2;
         functionspecparameters=[];
         functionspecgenericlist=[];
         functionspecgenericmapaspect=[];
         functionspecreturntype=$4;
         functionspecpurity=Unknown} }
  | Lfunction designator subprogram_parameters Lreturn type_mark
      { {functionspecdesignator=$2;
         functionspecparameters=$3;
         functionspecgenericlist=[];
         functionspecgenericmapaspect=[];
         functionspecreturntype=$5;
         functionspecpurity=Unknown} }
  | Lfunction designator subprogram_header Lreturn type_mark
      { match $3 with
        | generic_list,generic_map_aspect
            -> {functionspecdesignator=$2;
                functionspecparameters=[];
                functionspecgenericlist=generic_list;
                functionspecgenericmapaspect=generic_map_aspect;
                functionspecreturntype=$5;
                functionspecpurity=Unknown} }
  | Lfunction designator subprogram_header subprogram_parameters Lreturn type_mark
      { match $3 with
        | generic_list,generic_map_aspect
            -> {functionspecdesignator=$2;
                functionspecparameters=$4;
                functionspecgenericlist=generic_list;
                functionspecgenericmapaspect=generic_map_aspect;
                functionspecreturntype=$6;
                functionspecpurity=Unknown} }
  | function_purity Lfunction designator Lreturn type_mark
      { {functionspecdesignator=$3;
         functionspecparameters=[];
         functionspecgenericlist=[];
         functionspecgenericmapaspect=[];
         functionspecreturntype=$5;
         functionspecpurity=$1} }
  | function_purity Lfunction designator subprogram_parameters Lreturn type_mark
      { {functionspecdesignator=$3;
         functionspecparameters=$4;
         functionspecgenericlist=[];
         functionspecgenericmapaspect=[];
         functionspecreturntype=$6;
         functionspecpurity=$1} }
  | function_purity Lfunction designator subprogram_header Lreturn type_mark
      { match $4 with
        | generic_list,generic_map_aspect
            -> {functionspecdesignator=$3;
                functionspecparameters=[];
                functionspecgenericlist=generic_list;
                functionspecgenericmapaspect=generic_map_aspect;
                functionspecreturntype=$6;
                functionspecpurity=$1} }
  | function_purity Lfunction designator subprogram_header subprogram_parameters Lreturn type_mark
      { match $4 with
        | generic_list,generic_map_aspect
            -> {functionspecdesignator=$3;
                functionspecparameters=$5;
                functionspecgenericlist=generic_list;
                functionspecgenericmapaspect=generic_map_aspect;
                functionspecreturntype=$7;
                functionspecpurity=$1} }

function_purity:
  | Lpure
      { Pure }
  | Limpure
      { Impure }

/*
(* designator ::= identifier | operator_symbol                             *)
(* operator_symbol ::= string_literal                                      *)
*/
designator:
  | identifier
      { DesignatorIdentifier $1 }
  | Lstring
      { DesignatorOperator $1 }
  | Lchar
      { DesignatorCharacter $1 }

/*
(* formal_parameter_list ::= parameter_interface_list *)
*/
subprogram_parameters:
  | Lleftparenthesis formal_parameter_list Lrightparenthesis
      { $2 }
  | Lparameter Lleftparenthesis formal_parameter_list Lrightparenthesis
      { $3 }

formal_parameter_list:
  | interface_list
      { $1 }

/*
(* subprogram_header ::=         *)
(*   [ generic ( generic_list )  *)
(*   [ generic_map_aspect ] ]    *)
*/
subprogram_header:
  | Lgeneric Lleftparenthesis generic_list Lrightparenthesis
      { $3,[] }
  | Lgeneric Lleftparenthesis generic_list Lrightparenthesis generic_map_aspect
      { $3,$5 }

/*
(* subprogram_body ::=                                     *)
(*   subprogram_specification is                           *)
(*   subprogram_declarative_part                           *)
(*   begin                                                 *)
(*     subprogram_statement_part                           *)
(*   end [ subprogram_kind ] [ designator ] ;              *)
(* subprogram_kind ::= procedure | function                *)
*/
subprogram_body:
  | subprogram_specification subprogram_declarations subprogram_statements Lend Lsemicolon
      { {subprogramspecification=$1;
         subprogramdeclarations=$2;
         subprogramstatements=$3} }
  | subprogram_specification subprogram_declarations subprogram_statements Lend Lfunction Lsemicolon
      { match $1 with
        | FunctionSpecification _
            -> {subprogramspecification=$1;
                subprogramdeclarations=$2;
                subprogramstatements=$3};
        | ProcedureSpecification _
            -> raise Parsing.Parse_error }
  | subprogram_specification subprogram_declarations subprogram_statements Lend Lprocedure Lsemicolon
      { match $1 with
        | ProcedureSpecification _
            -> {subprogramspecification=$1;
                subprogramdeclarations=$2;
                subprogramstatements=$3};
        | FunctionSpecification _
            -> raise Parsing.Parse_error }
  | subprogram_specification subprogram_declarations subprogram_statements Lend designator Lsemicolon
      { match $1 with
        | FunctionSpecification {functionspecdesignator=spec_designator}
        | ProcedureSpecification {procedurespecdesignator=spec_designator}
            when designators_equal spec_designator $5
            -> {subprogramspecification=$1;
                subprogramdeclarations=$2;
                subprogramstatements=$3};
        | FunctionSpecification _
        | ProcedureSpecification _
            -> raise Parsing.Parse_error }
  | subprogram_specification subprogram_declarations subprogram_statements Lend Lfunction designator Lsemicolon
      { match $1 with
        | FunctionSpecification {functionspecdesignator=spec_designator}
            when designators_equal spec_designator $6
            -> {subprogramspecification=$1;
                subprogramdeclarations=$2;
                subprogramstatements=$3};
        | FunctionSpecification _
        | ProcedureSpecification _
            -> raise Parsing.Parse_error }
  | subprogram_specification subprogram_declarations subprogram_statements Lend Lprocedure designator Lsemicolon
      { match $1 with
        | ProcedureSpecification {procedurespecdesignator=spec_designator}
            when designators_equal spec_designator $6
            -> {subprogramspecification=$1;
                subprogramdeclarations=$2;
                subprogramstatements=$3};
        | FunctionSpecification _
        | ProcedureSpecification _
            -> raise Parsing.Parse_error }

subprogram_declarations:
  | Lis
      { [] }
  | Lis subprogram_declarative_part
      { $2 }

subprogram_statements:
  | Lbegin
      { [] }
  | Lbegin subprogram_statement_part
      { $2 }

/*
(* subprogram_declarative_part ::=                         *)
(*   { subprogram_declarative_item }                       *)
*/
subprogram_declarative_part:
  | subprogram_declarative_item
      { [$1] }
  | subprogram_declarative_part subprogram_declarative_item
      { ($1 @ [$2]) }

/*
(* subprogram_declarative_item ::=                         *)
(*     subprogram_declaration                              *)
(*   | subprogram_body                                     *)
(*   | subprogram_instantiation                            *)
(*   | type_declaration                                    *)
(*   | subtype_declaration                                 *)
(*   | constant_declaration                                *)
(*   | variable_declaration                                *)
(*   | file_declaration (ieee syn ignored)                 *)
(*   | alias_declaration                                   *)
(*   | attribute_declaration                               *)
(*   | attribute_specification                             *)
(*   | use_clause                                          *)
(*   | group_template_declaration (ieee syn not supported) *)
(*   | group_declaration (ieee syn not supported)          *)
*/
subprogram_declarative_item:
  | subprogram_declaration
      { SubProgramDeclaration $1 }
  | subprogram_body
      { SubProgramBody $1 }
  | subprogram_instantiation
      { SubProgramInstantiation $1 }
  | type_declaration
      { SubProgramTypeDeclaration $1 }
  | subtype_declaration
      { SubProgramSubTypeDeclaration $1 }
  | constant_declaration
      { SubProgramConstantDeclaration $1 }
  | variable_declaration
      { SubProgramVariableDeclaration $1 }
  | file_declaration
      { SubProgramFileDeclaration $1 }
  | alias_declaration
      { SubProgramAliasDeclaration $1 }
  | attribute_declaration
      { SubProgramAttributeDeclaration $1 }
  | attribute_specification
      { SubProgramAttributeSpecification $1 }
  | use_clause
      { SubProgramUseClause $1 }

/*
(* subprogram_statement_part ::=                           *)
(*   { sequential_statement }                              *)
*/
subprogram_statement_part:
  | sequence_of_statements
      { $1 }

/*
(* subprogram_instantiation_declaration ::=                                         *)
(*   subprogram_kind designator is new uninstantiated_subprogram_name [ signature ] *)
(*   [ generic_map_aspect ] ;                                                       *)
*/
subprogram_instantiation:
  | Lfunction designator Lis Lnew name Lsemicolon
      { {subprograminstantiationkind = Function;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = None;
         subprograminstantiationgenericmapaspect = []} }
  | Lfunction designator Lis Lnew name signature Lsemicolon
      { {subprograminstantiationkind = Function;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = Some $6;
         subprograminstantiationgenericmapaspect = []} }
  | Lfunction designator Lis Lnew name generic_map_aspect Lsemicolon
      { {subprograminstantiationkind = Function;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = None;
         subprograminstantiationgenericmapaspect = $6} }
  | Lfunction designator Lis Lnew name signature generic_map_aspect Lsemicolon
      { {subprograminstantiationkind = Function;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = Some $6;
         subprograminstantiationgenericmapaspect = $7} }
  | Lprocedure designator Lis Lnew name Lsemicolon
      { {subprograminstantiationkind = Procedure;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = None;
         subprograminstantiationgenericmapaspect = []} }
  | Lprocedure designator Lis Lnew name signature Lsemicolon
      { {subprograminstantiationkind = Procedure;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = Some $6;
         subprograminstantiationgenericmapaspect = []} }
  | Lprocedure designator Lis Lnew name generic_map_aspect Lsemicolon
      { {subprograminstantiationkind = Procedure;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = None;
         subprograminstantiationgenericmapaspect = $6} }
  | Lprocedure designator Lis Lnew name signature generic_map_aspect Lsemicolon
      { {subprograminstantiationkind = Procedure;
         subprograminstantiationdesignator = $2;
         subprograminstantiationname = $5;
         subprograminstantiationsignature = Some $6;
         subprograminstantiationgenericmapaspect = $7} }

/*
(* component_declaration ::=                 *)
(*   component identifier [is]               *)
(*   [local_generic_clause]                  *)
(*   [local_port_clause]                     *)
(*   end component [component_simple_name] ; *)
*/
component_header:
  | Lcomponent identifier
      { $2 }
  | Lcomponent identifier Lis
      { $2 }

component_trailer:
  | Lend Lcomponent Lsemicolon
      { empty_simple_name }
  | Lend Lcomponent simple_name Lsemicolon
      { $3 }

component_declaration:
  | component_header component_trailer
      { {componentname=check_identifiers $1 $2; componentgeneric=[]; componentport=[]} }
  | component_header generic_clause component_trailer
      { {componentname=check_identifiers $1 $3; componentgeneric=$2; componentport=[]} }
  | component_header port_clause component_trailer
      { {componentname=check_identifiers $1 $3; componentgeneric=[]; componentport=$2} }
  | component_header generic_clause port_clause component_trailer
      { {componentname=check_identifiers $1 $4; componentgeneric=$2; componentport=$3} }

/*
(* generic_clause ::= generic( generic_list ); *)
*/
generic_clause:
  | Lgeneric Lleftparenthesis generic_list Lrightparenthesis Lsemicolon
      { $3 }

/*
(* port_clause ::= port( port_list );          *)
*/
port_clause:
  | Lport Lleftparenthesis port_list Lrightparenthesis Lsemicolon
      { $3 }

/*
(* generic_list ::= generic_interface_list *)
*/
generic_list:
  | interface_list
      { $1 }

/*
(* port_list ::= port_interface_list *)
*/
port_list:
  | interface_list
      { $1 }

/*
(* configuration_specification ::=                   *)
(*   for component_specification binding_indication; *)
*/
configuration_specification:
  | Lfor component_specification binding_indication Lsemicolon
      { {configurationcomponent=$2; configurationbinding=$3} }

/*
(* component_specification ::=                       *)
(*   instantiation_list : component_name             *)
*/
component_specification:
  | instantiation_list Lcolon name
      { {componentinstantiations=$1; componentspecname=$3} }

/*
(* instantiation_list ::=                            *)
(*     instantiation_label {, instantiation_label}   *)
(*   | others                                        *)
(*   | all                                           *)
*/
instantiation_list:
  | label
      { InstantiationLabels [$1] }
  | instantiation_list Lcomma label
      { match $1 with
          | InstantiationLabels l
              -> InstantiationLabels (l @ [$3]);
          | _ -> raise Parsing.Parse_error; }
  | Lothers
      { InstantiationOthers }
  | Lall
      { InstantiationAll }

/*
(* binding_indication ::=                             *)
(*  [ use entity_aspect ]                             *)
(*  [ generic_map_aspect ]                            *)
(*  [ port_map_aspect ]                               *)
*/
binding_indication:
  | Luse entity_aspect
      { {bindingentity=$2; bindinggenericmap=[]; bindingportmap=[]} }
  | generic_map_aspect
      { {bindingentity=BindingOpen; bindinggenericmap=$1; bindingportmap=[]} }
  | port_map_aspect
      { {bindingentity=BindingOpen; bindinggenericmap=[]; bindingportmap=$1} }
  | generic_map_aspect port_map_aspect
      { {bindingentity=BindingOpen; bindinggenericmap=$1; bindingportmap=$2} }
  | Luse entity_aspect generic_map_aspect
      { {bindingentity=$2; bindinggenericmap=$3; bindingportmap=[]} }
  | Luse entity_aspect port_map_aspect
      { {bindingentity=$2; bindinggenericmap=[]; bindingportmap=$3} }
  | Luse entity_aspect generic_map_aspect port_map_aspect
      { {bindingentity=$2; bindinggenericmap=$3; bindingportmap=$4} }

/*
(* entity_aspect ::=                                  *)
(*     entity entity_name [(architecture_identifier)] *)
(*   | configuration configuration_name               *)
(*   | open                                           *)
*/
entity_aspect:
  | Lentity name
      { BindingEntity $2 }
  | Lentity name Lleftparenthesis identifier Lrightparenthesis
      { BindingEntityArchitecture ($2,$4) }
  | Lconfiguration name
      { BindingConfiguration $2 }
  | Lopen
      { BindingOpen }

/*
(* generic_map_aspect ::=                             *)
(*   generic map ( generic_association_list )         *)
*/
generic_map_aspect:
  | Lgeneric Lmap Lleftparenthesis association_list Lrightparenthesis
      { $4 }

/*
(* port_map_aspect ::=                                *)
(*   port map ( port_association_list )               *)
*/
port_map_aspect:
  | Lport Lmap Lleftparenthesis association_list Lrightparenthesis
      { $4 }

/*
(* block_statement ::=                                     *)
(*   block_label:                                          *)
(*   block [ ( guard_expression ) ] [ is ]                 *)
(*     block_header                                        *)
(*     block_declarative_part                              *)
(*   begin                                                 *)
(*     block_statement_part                                *)
(*   end block [ block_label ] ;                           *)
*/
block_statement:
  | label Lcolon block_declarations Lbegin Lend Lblock Lsemicolon
      { { $3 with blocklabelname=$1;
                  blockstatements=[]} }
  | label Lcolon block_declarations Lbegin Lend Lblock label Lsemicolon
      { { $3 with blocklabelname=check_labels $1 $7;
                  blockstatements=[]} }
  | label Lcolon block_declarations Lbegin block_statement_part Lend Lblock Lsemicolon
      { { $3 with blocklabelname=$1;
                  blockstatements=$5} }
  | label Lcolon block_declarations Lbegin block_statement_part Lend Lblock label Lsemicolon
      { { $3 with blocklabelname=check_labels $1 $8;
                  blockstatements=$5} }

block_declarations:
  | Lblock
      { {blocklabelname = empty_identifier;
         blockguardcondition = empty_condition;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = [];
         blockstatements = [] } }
  | Lblock block_header
      { match $2 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = empty_condition;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = [];
                blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis
      { {blocklabelname = empty_identifier;
         blockguardcondition = $3;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = [];
         blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis block_header
      { match $5 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = $3;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = [];
                blockstatements = [] } }
  | Lblock Lis
      { {blocklabelname = empty_identifier;
         blockguardcondition = empty_condition;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = [];
         blockstatements = [] } }
  | Lblock Lis block_header
      { match $3 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = empty_condition;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = [];
                blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis Lis
      { {blocklabelname = empty_identifier;
         blockguardcondition = $3;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = [];
         blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis Lis block_header
      { match $6 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = $3;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = [];
                blockstatements = [] } }
  | Lblock block_declarative_part
      { {blocklabelname = empty_identifier;
         blockguardcondition = empty_condition;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = $2;
         blockstatements = [] } }
  | Lblock block_header block_declarative_part
      { match $2 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = empty_condition;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = $3;
                blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis block_declarative_part
      { {blocklabelname = empty_identifier;
         blockguardcondition = $3;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = $5;
         blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis block_header block_declarative_part
      { match $5 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = $3;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = $6;
                blockstatements = [] } }
  | Lblock Lis block_declarative_part
      { {blocklabelname = empty_identifier;
         blockguardcondition = empty_condition;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = $3;
         blockstatements = [] } }
  | Lblock Lis block_header block_declarative_part
      { match $3 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = empty_condition;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = $4;
                blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis Lis block_declarative_part
      { {blocklabelname = empty_identifier;
         blockguardcondition = $3;
         blockgenericclause = [];
         blockgenericmapaspect = [];
         blockportclause = [];
         blockportmapaspect = [];
         blockdeclarations = $6;
         blockstatements = [] } }
  | Lblock Lleftparenthesis condition Lrightparenthesis Lis block_header block_declarative_part
      { match $6 with
        | generic_clause,generic_map_aspect,port_clause,port_map_aspect
            -> {blocklabelname = empty_identifier;
                blockguardcondition = $3;
                blockgenericclause = generic_clause;
                blockgenericmapaspect = generic_map_aspect;
                blockportclause = port_clause;
                blockportmapaspect = port_map_aspect;
                blockdeclarations = $7;
                blockstatements = [] } }

block_header:
  | generic_clause
      { ($1,[],[],[]) }
  | generic_clause generic_map_aspect Lsemicolon
      { ($1,$2,[],[]) }
  | port_clause
      { ([],[],$1,[]) }
  | port_clause port_map_aspect Lsemicolon
      { ([],[],$1,$2) }
  | generic_clause port_clause
      { ($1,[],$2,[]) }
  | generic_clause generic_map_aspect Lsemicolon port_clause
      { ($1,$2,$4,[]) }
  | generic_clause port_clause port_map_aspect Lsemicolon
      { ($1,[],$2,$3) }
  | generic_clause generic_map_aspect Lsemicolon port_clause port_map_aspect Lsemicolon
      { ($1,$2,$4,$5) }

/*
(* block_declarative_part ::=                              *)
(*   { block_declarative_item }                            *)
*/
block_declarative_part:
  | block_declarative_item
      { [$1] }
  | block_declarative_part block_declarative_item
      { ($1 @ [$2]) }

/*
(* block_statement_part ::=                                *)
(*   { concurrent_statement }                              *)
*/
block_statement_part:
  | concurrent_statement
      { [$1] }
  | block_statement_part concurrent_statement
      { ($1 @ [$2]) }

/*
(* block_declarative_item ::=                              *)
(*    subprogram_declaration                               *)
(*  | subprogram_body                                      *)
(*  | subprogram_instantiation                             *)
(*  | type_declaration                                     *)
(*  | subtype_declaration                                  *)
(*  | constant_declaration                                 *)
(*  | signal_declaration                                   *)
(*  | shared_variable_declaration (ieee syn not supported) *)
(*  | file_declaration (ieee syn ignored)                  *)
(*  | alias_declaration                                    *)
(*  | component_declaration                                *)
(*  | attribute_declaration                                *)
(*  | attribute_specification                              *)
(*  | configuration_specification                          *)
(*  | disconnection_specification (ieee syn not supported) *)
(*  | use_clause                                           *)
(*  | group_template_declaration (ieee syn not supported)  *)
(*  | group_declaration (ieee syn not supported)           *)
*/
block_declarative_item:
  | subprogram_declaration
      { BlockSubProgramDeclaration $1 }
  | subprogram_body
      { BlockSubProgramBody $1 }
  | subprogram_instantiation
      { BlockSubProgramInstantiation $1 }
  | type_declaration
      { BlockTypeDeclaration $1 }
  | subtype_declaration
      { BlockSubTypeDeclaration $1 }
  | constant_declaration
      { BlockConstantDeclaration $1 }
  | signal_declaration
      { BlockSignalDeclaration $1 }
  | file_declaration
      { BlockFileDeclaration $1 }
  | alias_declaration
      { BlockAliasDeclaration $1 }
  | component_declaration
      { BlockComponentDeclaration $1 }
  | attribute_declaration
      { BlockAttributeDeclaration $1 }
  | attribute_specification
      { BlockAttributeSpecification $1 }
  | configuration_specification
      { BlockConfigurationSpecification $1 }
  | use_clause
      { BlockUseClause $1 }

/*
(* process_statement ::=                                   *)
(*   [ process_label: ]                                    *)
(*   [ postponed ] process [ ( sensitivity_list ) ] [ is ] *)
(*     process_declarative_part                            *)
(*   begin                                                 *)
(*     process_statement_part                              *)
(*   end [ postponed ] process [process_label] ;           *)
*/
process_statement:
  | process_declarations process_statements Lend Lprocess Lsemicolon
      { {processlabelname=empty_label; processpostponed=false; processsensitivitylist=fst $1; processdeclarations=snd $1; processstatements=$2} }
  | Lpostponed process_declarations process_statements Lend Lpostponed Lprocess Lsemicolon
      { {processlabelname=empty_label; processpostponed=true; processsensitivitylist=fst $2; processdeclarations=snd $2; processstatements=$3} }
  | label Lcolon process_declarations process_statements Lend Lprocess Lsemicolon
      { {processlabelname=$1; processpostponed=false; processsensitivitylist=fst $3; processdeclarations=snd $3; processstatements=$4} }
  | label Lcolon Lpostponed process_declarations process_statements Lend Lpostponed Lprocess Lsemicolon
      { {processlabelname=$1; processpostponed=true; processsensitivitylist=fst $4; processdeclarations=snd $4; processstatements=$5} }
  | label Lcolon process_declarations process_statements Lend Lprocess label Lsemicolon
      { {processlabelname=check_labels $1 $7; processpostponed=false; processsensitivitylist=fst $3; processdeclarations=snd $3; processstatements=$4} }
  | label Lcolon Lpostponed process_declarations process_statements Lend Lpostponed Lprocess label Lsemicolon
      { {processlabelname=check_labels $1 $9; processpostponed=true; processsensitivitylist=fst $4; processdeclarations=snd $4; processstatements=$5} }

process_declarations:
  | Lprocess
      { (SensitivityExpressionList [],[]) }
  | Lprocess Lis
      { (SensitivityExpressionList [],[]) }
  | Lprocess Lleftparenthesis sensitivity_list Lrightparenthesis
      { (SensitivityExpressionList $3,[]) }
  | Lprocess Lleftparenthesis sensitivity_list Lrightparenthesis Lis
      { (SensitivityExpressionList $3,[]) }
  | Lprocess Lleftparenthesis Lall Lrightparenthesis
      { (SensitivityAll,[]) }
  | Lprocess Lleftparenthesis Lall Lrightparenthesis Lis
      { (SensitivityAll,[]) }
  | Lprocess process_declarative_part
      { (SensitivityExpressionList [],$2) }
  | Lprocess Lis process_declarative_part
      { (SensitivityExpressionList [],$3) }
  | Lprocess Lleftparenthesis sensitivity_list Lrightparenthesis process_declarative_part
      { (SensitivityExpressionList $3,$5) }
  | Lprocess Lleftparenthesis sensitivity_list Lrightparenthesis Lis process_declarative_part
      { (SensitivityExpressionList $3,$6) }
  | Lprocess Lleftparenthesis Lall Lrightparenthesis process_declarative_part
      { (SensitivityAll,$5) }
  | Lprocess Lleftparenthesis Lall Lrightparenthesis Lis process_declarative_part
      { (SensitivityAll,$6) }

process_statements:
  | Lbegin
      { [] }
  | Lbegin process_statement_part
      { $2 }

/*
(* process_declarative_part ::=                            *)
(*   { process_declarative_item }                          *)
*/
process_declarative_part:
  | process_declarative_item
    { [$1] }
  | process_declarative_part process_declarative_item
    { ($1 @ [$2]) }

/*
(* process_declarative_item ::=                            *)
(*     subprogram_declaration                              *)
(*   | subprogram_body                                     *)
(*   | subprogram_instantiation                            *)
(*   | type_declaration                                    *)
(*   | subtype_declaration                                 *)
(*   | constant_declaration                                *)
(*   | variable_declaration                                *)
(*   | file_declaration (ieee syn ignored)                 *)
(*   | alias_declaration                                   *)
(*   | attribute_declaration                               *)
(*   | attribute_specification                             *)
(*   | use_clause                                          *)
(*   | group_template_declaration (ieee syn not supported) *)
(*   | group_declaration (ieee syn not supported)          *)
*/
process_declarative_item:
  | subprogram_declaration
      { ProcessSubProgramDeclaration $1 }
  | subprogram_body
      { ProcessSubProgramBody $1 }
  | subprogram_instantiation
      { ProcessSubProgramInstantiation $1 }
  | type_declaration
      { ProcessTypeDeclaration $1 }
  | subtype_declaration
      { ProcessSubTypeDeclaration $1 }
  | constant_declaration
      { ProcessConstantDeclaration $1 }
  | variable_declaration
      { ProcessVariableDeclaration $1 }
  | file_declaration
      { ProcessFileDeclaration $1 }
  | alias_declaration
      { ProcessAliasDeclaration $1 }
  | attribute_declaration
      { ProcessAttributeDeclaration $1 }
  | attribute_specification
      { ProcessAttributeSpecification $1 }
  | use_clause
      { ProcessUseClause $1 }

/*
(* process_statement_part ::=                              *)
(*   { sequential_statement }                              *)
*/
process_statement_part:
  | sequential_statement
      { [$1] }
  | process_statement_part sequential_statement
      { ($1 @ [$2]) }

/*
(* concurrent_procedure_call_statement ::=            *)
(*   [ label: ] [ postponed ] procedure_call ;        *)
(* (postponed: ieee syn not supported                 *)
*/
concurrent_procedure_call_statement:
  | procedure_call Lsemicolon
      { {concurrentprocedurelabelname=empty_label; concurrentpostponedprocedure=false; concurrentprocedurecall=$1} }
  | label Lcolon procedure_call Lsemicolon
      { {concurrentprocedurelabelname=$1; concurrentpostponedprocedure=false; concurrentprocedurecall=$3} }
  | Lpostponed procedure_call Lsemicolon
      { {concurrentprocedurelabelname=empty_label; concurrentpostponedprocedure=true; concurrentprocedurecall=$2} }
  | label Lcolon Lpostponed procedure_call Lsemicolon
      { {concurrentprocedurelabelname=$1; concurrentpostponedprocedure=true; concurrentprocedurecall=$4} }

/*
(* concurrent_assertion_statement ::=            *)
(*   [ label: ] [ postponed ] assertion ;        *)
(* (postponed: ieee syn not supported)           *)
*/
concurrent_assertion_statement:
  | assertion Lsemicolon
      { {concurrentassertionlabelname=empty_label; concurrentpostponedassertion=false; concurrentassertion=$1} }
  | label Lcolon assertion Lsemicolon
      { {concurrentassertionlabelname=$1; concurrentpostponedassertion=false; concurrentassertion=$3} }
  | Lpostponed assertion Lsemicolon
      { {concurrentassertionlabelname=empty_label; concurrentpostponedassertion=true; concurrentassertion=$2} }
  | label Lcolon Lpostponed assertion Lsemicolon
      { {concurrentassertionlabelname=$1; concurrentpostponedassertion=true; concurrentassertion=$4} }

/*
(* concurrent_signal_assignment_statement ::=                            *)
(*   | [ label: ] [ postponed ] concurrent_simple_signal_assignment      *)
(*   | [ label: ] [ postponed ] concurrent_conditional_signal_assignment *)
(*   | [ label: ] [ postponed ] concurrent_selected_signal_assignment    *)
(* (postponed: ieee syn not supported)                                   *)
*/
concurrent_signal_assignment_statement:
  | concurrent_simple_signal_assignment
      { {concurrentsignallabelname=empty_label;
         concurrentsignalpostponed=false;
         concurrentsignalassignment=ConcurrentSimpleSignalAssignment $1} }
  | label Lcolon concurrent_simple_signal_assignment
      { {concurrentsignallabelname=$1;
         concurrentsignalpostponed=false;
         concurrentsignalassignment=ConcurrentSimpleSignalAssignment $3} }
  | Lpostponed concurrent_simple_signal_assignment
      { {concurrentsignallabelname=empty_label;
         concurrentsignalpostponed=true;
         concurrentsignalassignment=ConcurrentSimpleSignalAssignment $2} }
  | label Lcolon Lpostponed concurrent_simple_signal_assignment
      { {concurrentsignallabelname=$1;
         concurrentsignalpostponed=true;
         concurrentsignalassignment=ConcurrentSimpleSignalAssignment $4} }
  | concurrent_conditional_signal_assignment
      { {concurrentsignallabelname=empty_label;
         concurrentsignalpostponed=false;
         concurrentsignalassignment=ConcurrentConditionalSignalAssignment $1} }
  | label Lcolon concurrent_conditional_signal_assignment
      { {concurrentsignallabelname=$1;
         concurrentsignalpostponed=false;
         concurrentsignalassignment=ConcurrentConditionalSignalAssignment $3} }
  | Lpostponed concurrent_conditional_signal_assignment
      { {concurrentsignallabelname=empty_label;
         concurrentsignalpostponed=true;
         concurrentsignalassignment=ConcurrentConditionalSignalAssignment $2} }
  | label Lcolon Lpostponed concurrent_conditional_signal_assignment
      { {concurrentsignallabelname=$1;
         concurrentsignalpostponed=true;
         concurrentsignalassignment=ConcurrentConditionalSignalAssignment $4} }
  | concurrent_selected_signal_assignment
      { {concurrentsignallabelname=empty_label;
         concurrentsignalpostponed=false;
         concurrentsignalassignment=ConcurrentSelectedSignalAssignment $1} }
  | label Lcolon concurrent_selected_signal_assignment
      { {concurrentsignallabelname=$1;
         concurrentsignalpostponed=false;
         concurrentsignalassignment=ConcurrentSelectedSignalAssignment $3} }
  | Lpostponed concurrent_selected_signal_assignment
      { {concurrentsignallabelname=empty_label;
         concurrentsignalpostponed=true;
         concurrentsignalassignment=ConcurrentSelectedSignalAssignment $2} }
  | label Lcolon Lpostponed concurrent_selected_signal_assignment
      { {concurrentsignallabelname=$1;
         concurrentsignalpostponed=true;
         concurrentsignalassignment=ConcurrentSelectedSignalAssignment $4} }

/*
(* concurrent_simple_signal_assignment ::=                *)
(*   target <= [ guarded ] [ delay_mechanism ] waveform ; *)
*/
concurrent_simple_signal_assignment:
  | target Lless Lequal waveform Lsemicolon
      { {concurrentsimplesignaltarget=$1;
         concurrentsimplesignalguarded=false;
         concurrentsimplesignaldelay=DelayNone;
         concurrentsimplesignalwaveform=$4} }
  | target Lless Lequal Lguarded waveform Lsemicolon
      { {concurrentsimplesignaltarget=$1;
         concurrentsimplesignalguarded=true;
         concurrentsimplesignaldelay=DelayNone;
         concurrentsimplesignalwaveform=$5} }
  | target Lless Lequal delay_mechanism waveform Lsemicolon
      { {concurrentsimplesignaltarget=$1;
         concurrentsimplesignalguarded=false;
         concurrentsimplesignaldelay=$4;
         concurrentsimplesignalwaveform=$5} }
  | target Lless Lequal Lguarded delay_mechanism waveform Lsemicolon
      { {concurrentsimplesignaltarget=$1;
         concurrentsimplesignalguarded=true;
         concurrentsimplesignaldelay=$5;
         concurrentsimplesignalwaveform=$6} }

/*
(* options ::= [ guarded ] [delay_mechanism]                  *)
(* (delay_mechanism: ieee syn ignored)                        *)
(* conditional_signal_assignment ::=                          *)
(*   target <= options conditional_waveforms ;                *)
*/
concurrent_conditional_signal_assignment:
  | target Lless Lequal conditional_waveforms Lsemicolon
      { {concurrentconditionalsignaltarget=$1;
         concurrentconditionalsignalguarded=false;
         concurrentconditionalsignaldelay=DelayNone;
         concurrentconditionalsignalwaveforms=$4} }
  | target Lless Lequal Lguarded conditional_waveforms Lsemicolon
      { {concurrentconditionalsignaltarget=$1;
         concurrentconditionalsignalguarded=true;
         concurrentconditionalsignaldelay=DelayNone;
         concurrentconditionalsignalwaveforms=$5} }
  | target Lless Lequal delay_mechanism conditional_waveforms Lsemicolon
      { {concurrentconditionalsignaltarget=$1;
         concurrentconditionalsignalguarded=false;
         concurrentconditionalsignaldelay=$4;
         concurrentconditionalsignalwaveforms=$5} }
  | target Lless Lequal Lguarded delay_mechanism conditional_waveforms Lsemicolon
      { {concurrentconditionalsignaltarget=$1;
         concurrentconditionalsignalguarded=true;
         concurrentconditionalsignaldelay=$5;
         concurrentconditionalsignalwaveforms=$6} }

/*
(* conditional_waveforms ::=          *)
(*   waveform when condition          *)
(*   { else waveform when condition } *)
(*   [ else waveform ]                *)
*/
conditional_waveforms:
  | waveform Lwhen condition
      { [{conditionalwaveformvalue=$1;
          conditionalwaveformcondition=$3}] }
  | conditional_waveforms Lelse waveform
      { if (List.nth $1 (-1 + List.length $1)).conditionalwaveformcondition <> empty_condition then
          ($1 @ [{conditionalwaveformvalue=$3;
                  conditionalwaveformcondition=empty_condition}])
        else
          raise Parsing.Parse_error }
  | conditional_waveforms Lelse waveform Lwhen condition
      { if (List.nth $1 (-1 + List.length $1)).conditionalwaveformcondition <> empty_condition then
          ($1 @ [{conditionalwaveformvalue=$3;
                  conditionalwaveformcondition=$5}])
        else
          raise Parsing.Parse_error }

/*
(* selected_signal_assignment ::=                             *)
(*   with expression select                                   *)
(*     target <= options selected_waveforms ;                 *)
*/
concurrent_selected_signal_assignment:
  | Lwith selector Lselect target Lless Lequal selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$4;
         concurrentselectedsignalguarded=false;
         concurrentselectedsignaldelay=DelayNone;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=OrdinarySelection;
         concurrentselectedsignalwaveforms=$7} }
  | Lwith selector Lselect target Lless Lequal Lguarded selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$4;
         concurrentselectedsignalguarded=true;
         concurrentselectedsignaldelay=DelayNone;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=OrdinarySelection;
         concurrentselectedsignalwaveforms=$8} }
  | Lwith selector Lselect target Lless Lequal delay_mechanism selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$4;
         concurrentselectedsignalguarded=false;
         concurrentselectedsignaldelay=$7;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=OrdinarySelection;
         concurrentselectedsignalwaveforms=$8} }
  | Lwith selector Lselect target Lless Lequal Lguarded delay_mechanism selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$4;
         concurrentselectedsignalguarded=true;
         concurrentselectedsignaldelay=$8;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=OrdinarySelection;
         concurrentselectedsignalwaveforms=$9} }
  | Lwith selector Lselect Lquestionmark target Lless Lequal selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$5;
         concurrentselectedsignalguarded=false;
         concurrentselectedsignaldelay=DelayNone;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=MatchingSelection;
         concurrentselectedsignalwaveforms=$8} }
  | Lwith selector Lselect Lquestionmark target Lless Lequal Lguarded selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$5;
         concurrentselectedsignalguarded=true;
         concurrentselectedsignaldelay=DelayNone;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=MatchingSelection;
         concurrentselectedsignalwaveforms=$9} }
  | Lwith selector Lselect Lquestionmark target Lless Lequal delay_mechanism selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$5;
         concurrentselectedsignalguarded=false;
         concurrentselectedsignaldelay=$8;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=MatchingSelection;
         concurrentselectedsignalwaveforms=$9} }
  | Lwith selector Lselect Lquestionmark target Lless Lequal Lguarded delay_mechanism selected_waveforms Lsemicolon
      { {concurrentselectedsignaltarget=$5;
         concurrentselectedsignalguarded=true;
         concurrentselectedsignaldelay=$9;
         concurrentselectedsignalselector=$2;
         concurrentselectedsignalkind=MatchingSelection;
         concurrentselectedsignalwaveforms=$10} }

/*
(* selected_waveforms ::=                                     *)
(*   { waveform when choices , }                              *)
(*   waveform when choices                                    *)
*/
selected_waveforms:
  | waveform Lwhen choices
      { [{selectedwaveformvalue=$1;
          selectedwaveformchoices=$3}] }
  | selected_waveforms Lcomma waveform Lwhen choices
      { ($1 @ [{selectedwaveformvalue=$3;
                selectedwaveformchoices=$5}]) }

/*
(* component_instantiation_statement ::=            *)
(*   instantiation_label:                           *)
(*     instantiated_unit                            *)
(*     [ generic_map_aspect ]                       *)
(*     [ port_map_aspect ] ;                        *)
*/
component_instantiation_statement:
/* conflict with procedure calls
  | label Lcolon name Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $3; componentgenericmap=[]; componentportmap=[]} } */
  | label Lcolon Lcomponent name Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $4; componentgenericmap=[]; componentportmap=[]} }
  | label Lcolon Lentity name Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,empty_identifier); componentgenericmap=[]; componentportmap=[]} }
  | label Lcolon Lentity name Lleftparenthesis identifier Lrightparenthesis Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,$6); componentgenericmap=[]; componentportmap=[]} }
  | label Lcolon Lconfiguration name Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedConfiguration $4; componentgenericmap=[]; componentportmap=[]} }

  | label Lcolon name generic_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $3; componentgenericmap=$4; componentportmap=[]} }
  | label Lcolon Lcomponent name generic_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $4; componentgenericmap=$5; componentportmap=[]} }
  | label Lcolon Lentity name generic_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,empty_identifier); componentgenericmap=$5; componentportmap=[]} }
  | label Lcolon Lentity name Lleftparenthesis identifier Lrightparenthesis generic_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,$6); componentgenericmap=$8; componentportmap=[]} }
  | label Lcolon Lconfiguration name generic_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedConfiguration $4; componentgenericmap=$5; componentportmap=[]} }

  | label Lcolon name port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $3; componentgenericmap=[]; componentportmap=$4} }
  | label Lcolon Lcomponent name port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $4; componentgenericmap=[]; componentportmap=$5} }
  | label Lcolon Lentity name port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,empty_identifier); componentgenericmap=[]; componentportmap=$5} }
  | label Lcolon Lentity name Lleftparenthesis identifier Lrightparenthesis port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,$6); componentgenericmap=[]; componentportmap=$8} }
  | label Lcolon Lconfiguration name port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedConfiguration $4; componentgenericmap=[]; componentportmap=$5} }

  | label Lcolon name generic_map_aspect port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $3; componentgenericmap=$4; componentportmap=$5} }
  | label Lcolon Lcomponent name generic_map_aspect port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedComponent $4; componentgenericmap=$5; componentportmap=$6} }
  | label Lcolon Lentity name generic_map_aspect port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,empty_identifier); componentgenericmap=$5; componentportmap=$6} }
  | label Lcolon Lentity name Lleftparenthesis identifier Lrightparenthesis generic_map_aspect port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedEntityArchitecture ($4,$6); componentgenericmap=$8; componentportmap=$9} }
  | label Lcolon Lconfiguration name generic_map_aspect port_map_aspect Lsemicolon
      { {componentlabelname=$1; componentunit=InstantiatedConfiguration $4; componentgenericmap=$5; componentportmap=$6} }

/*
(* instantiated_unit ::=                            *)
(*     [component] component_name                   *)
(*   | entity entity_name [( architecture_name )]   *)
(*   | configuration configuration_name             *)
*/
/*instantiated_unit:
  | name
      { InstantiatedComponent $1 }
  | Lcomponent name
      { InstantiatedComponent $2 }
  | Lentity name
      { InstantiatedEntityArchitecture ($2, empty_identifier) }
  | Lentity name Lleftparenthesis identifier Lrightparenthesis
      { InstantiatedEntityArchitecture ($2, $4) }
  | Lconfiguration name
      { InstantiatedConfiguration $2 }*/

/*
(* generate_statement ::=            *)
(*    for_generate_statement         *)
(*  | if_generate_statement          *)
(*  | case_generate_statement        *)
*/
generate_statement:
  | for_generate_statement
      { ForGenerateStatement $1; }
  | if_generate_statement
      { IfGenerateStatement $1; }
  | case_generate_statement
      { CaseGenerateStatement $1; }

end_generate_statement:
  | Lend Lgenerate Lsemicolon
      { empty_label }
  | Lend Lgenerate label Lsemicolon
      { $3 }

/*
(* if_generate_statement ::=                               *)
(*   generate_label :                                      *)
(*     if [ alternative_label : ] condition generate       *)
(*       generate_statement_body                           *)
(*     { elsif [ alternative_label : ] condition generate  *)
(*       generate_statement_body }                         *)
(*     [ else [ alternative_label : ] generate             *)
(*       generate_statement_body ]                         *)
(*   end generate [ generate_label ] ;                     *)
*/
if_generate_statement:
  | label Lcolon if_generate_label_condition else_generate_statement
      { match $3,$4 with
        | (alternate_label,condition),(end_generate_label,else_generate_statement)
            -> { ifgeneratelabelname=check_labels $1 end_generate_label;
                 ifgeneratealternatelabelname=alternate_label;
                 ifgeneratecondition=condition;
                 ifgeneratedeclarations=[];
                 ifgeneratethenstatements=[];
                 ifgenerateelsestatements=else_generate_statement } }
  | label Lcolon if_generate_label_condition end_generate_statement_body else_generate_statement
      { match $3,$5 with
        | (alternate_label,condition),(end_generate_label,else_generate_statement)
            -> { ifgeneratelabelname=check_labels $1 end_generate_label;
                 ifgeneratealternatelabelname=check_labels alternate_label $4;
                 ifgeneratecondition=condition;
                 ifgeneratedeclarations=[];
                 ifgeneratethenstatements=[];
                 ifgenerateelsestatements=else_generate_statement } }
  | label Lcolon if_generate_label_condition generate_statement_body else_generate_statement
      { match $3,$4,$5 with
        | (alternate_label,condition),(declarations,statements),(end_generate_label,else_generate_statement)
            -> { ifgeneratelabelname=check_labels $1 end_generate_label;
                 ifgeneratealternatelabelname=alternate_label;
                 ifgeneratecondition=condition;
                 ifgeneratedeclarations=declarations;
                 ifgeneratethenstatements=statements;
                 ifgenerateelsestatements=else_generate_statement } }
  | label Lcolon if_generate_label_condition generate_statement_body end_generate_statement_body else_generate_statement
      { match $3,$4,$6 with
        | (alternate_label,condition),(declarations,statements),(end_generate_label,else_generate_statement)
            -> { ifgeneratelabelname=check_labels $1 end_generate_label;
                 ifgeneratealternatelabelname=check_labels alternate_label $5;
                 ifgeneratecondition=condition;
                 ifgeneratedeclarations=declarations;
                 ifgeneratethenstatements=statements;
                 ifgenerateelsestatements=else_generate_statement } }

if_generate_label_condition:
  | Lif condition Lgenerate
      { empty_label,$2 }
  | Lif label Lcolon condition Lgenerate
      { $2, $4 }

else_generate_statement:
  | end_generate_statement
      { $1,GenerateElseNone }
  | else_generate_label end_generate_statement
      { $2,GenerateElse {ifgeneratelabelname=empty_label;
                         ifgeneratealternatelabelname=$1;
                         ifgeneratecondition=empty_condition;
                         ifgeneratedeclarations=[];
                         ifgeneratethenstatements=[];
                         ifgenerateelsestatements=GenerateElseNone} }
  | else_generate_label generate_statement_body end_generate_statement
      { match $2 with
        | declarations,statements
            -> $3,GenerateElse {ifgeneratelabelname=empty_label;
                                ifgeneratealternatelabelname=$1;
                                ifgeneratecondition=empty_condition;
                                ifgeneratedeclarations=declarations;
                                ifgeneratethenstatements=statements;
                                ifgenerateelsestatements=GenerateElseNone} }
  | else_generate_label end_generate_statement_body end_generate_statement
      { $2,GenerateElse {ifgeneratelabelname=empty_label;
                         ifgeneratealternatelabelname=check_labels $1 $2;
                         ifgeneratecondition=empty_condition;
                         ifgeneratedeclarations=[];
                         ifgeneratethenstatements=[];
                         ifgenerateelsestatements=GenerateElseNone} }
  | else_generate_label generate_statement_body end_generate_statement_body end_generate_statement
      { match $2 with
        | declarations,statements
            -> $4,GenerateElse {ifgeneratelabelname=empty_label;
                                ifgeneratealternatelabelname=check_labels $1 $3;
                                ifgeneratecondition=empty_condition;
                                ifgeneratedeclarations=declarations;
                                ifgeneratethenstatements=statements;
                                ifgenerateelsestatements=GenerateElseNone} }
  | elsif_generate_statement
      { $1 }

else_generate_label:
  | Lelse Lgenerate
      { empty_label }
  | Lelse label Lcolon Lgenerate
      { $2 }

elsif_generate_statement:
  | elsif_generate_label_condition else_generate_statement
      { match $1,$2 with
        | (alternate_label,condition),(end_generate_label,else_generate_statement)
            -> end_generate_label,GenerateElsif {ifgeneratelabelname=empty_label;
                                                 ifgeneratealternatelabelname=alternate_label;
                                                 ifgeneratecondition=condition;
                                                 ifgeneratedeclarations=[];
                                                 ifgeneratethenstatements=[];
                                                 ifgenerateelsestatements=else_generate_statement} }
  | elsif_generate_label_condition generate_statement_body else_generate_statement
      { match $1,$2,$3 with
        | (alternate_label,condition),(declarations,statements),(end_generate_label,else_generate_statement)
            -> end_generate_label,GenerateElsif {ifgeneratelabelname=empty_label;
                                                 ifgeneratealternatelabelname=alternate_label;
                                                 ifgeneratecondition=condition;
                                                 ifgeneratedeclarations=declarations;
                                                 ifgeneratethenstatements=statements;
                                                 ifgenerateelsestatements=else_generate_statement} }
  | elsif_generate_label_condition end_generate_statement_body else_generate_statement
      { match $1,$3 with
        | (alternate_label,condition),(end_generate_label,else_generate_statement)
            -> end_generate_label,GenerateElsif {ifgeneratelabelname=empty_label;
                                                 ifgeneratealternatelabelname=check_labels alternate_label $2;
                                                 ifgeneratecondition=condition;
                                                 ifgeneratedeclarations=[];
                                                 ifgeneratethenstatements=[];
                                                 ifgenerateelsestatements=else_generate_statement} }
  | elsif_generate_label_condition generate_statement_body end_generate_statement_body else_generate_statement
      { match $1,$2,$4 with
        | (alternate_label,condition),(declarations,statements),(end_generate_label,else_generate_statement)
            -> end_generate_label,GenerateElsif {ifgeneratelabelname=empty_label;
                                                 ifgeneratealternatelabelname=check_labels alternate_label $3;
                                                 ifgeneratecondition=condition;
                                                 ifgeneratedeclarations=declarations;
                                                 ifgeneratethenstatements=statements;
                                                 ifgenerateelsestatements=else_generate_statement} }

elsif_generate_label_condition:
  | Lelsif condition Lgenerate
      { empty_label,$2 }
  | Lelsif label Lcolon condition Lgenerate
      { $2, $4 }

/*
(* for_generate_statement ::=                         *)
(*   generate_label :                                 *)
(*     for generate_parameter_specification generate  *)
(*       generate_statement_body                      *)
(*   end generate [ generate_label ] ;                *)
*/
for_generate_statement:
  | label Lcolon Lfor parameter_specification Lgenerate end_generate_statement
      { { forgeneratelabelname=check_labels $1 $6;
          forgeneratealternatelabelname=empty_label;
          forgenerateparameter=$4;
          forgeneratedeclarations=[];
          forgeneratestatements=[] } }
  | label Lcolon Lfor parameter_specification Lgenerate end_generate_statement_body end_generate_statement
      { { forgeneratelabelname=check_labels $1 $7;
          forgeneratealternatelabelname=check_labels empty_label $6;
          forgenerateparameter=$4;
          forgeneratedeclarations=[];
          forgeneratestatements=[] } }
  | label Lcolon Lfor parameter_specification Lgenerate generate_statement_body end_generate_statement
      { match $6 with
        | declarations,statements
            -> { forgeneratelabelname=check_labels $1 $7;
                 forgeneratealternatelabelname=empty_label;
                 forgenerateparameter=$4;
                 forgeneratedeclarations=declarations;
                 forgeneratestatements=statements } }
  | label Lcolon Lfor parameter_specification Lgenerate generate_statement_body end_generate_statement_body end_generate_statement
      { match $6 with
        | declarations,statements
            -> { forgeneratelabelname=check_labels $1 $8;
                 forgeneratealternatelabelname=check_labels empty_label $7;
                 forgenerateparameter=$4;
                 forgeneratedeclarations=declarations;
                 forgeneratestatements=statements } }

/*
(* case_generate_statement ::=               *)
(*   generate_label :                        *)
(*     case expression generate              *)
(*       case_generate_alternative           *)
(*       { case_generate_alternative }       *)
(*   end generate [ generate_label ] ;       *)
*/
case_generate_statement:
  | label Lcolon Lcase selector Lgenerate Lwhen case_generate_alternative_list Lsemicolon
      { {casegeneratelabelname=$1;
         casegenerateselector=$4;
         casegeneratekind=OrdinarySelection;
         casegeneratealternatives=$7} }
  | label Lcolon Lcase selector Lgenerate Lwhen case_generate_alternative_list label Lsemicolon
      { {casegeneratelabelname=check_labels $1 $8;
         casegenerateselector=$4;
         casegeneratekind=OrdinarySelection;
         casegeneratealternatives=$7} }

case_generate_alternative_continued:
  | case_generate_alternative Lwhen
      { [$1] }
  | case_generate_alternative end_generate_statement_body Lwhen
      { [{$1 with casegeneratealternatelabelname = check_labels $1.casegeneratealternatelabelname $2}] }
  | case_generate_alternative_continued case_generate_alternative Lwhen
      { $1 @ [$2] }
  | case_generate_alternative_continued case_generate_alternative end_generate_statement_body Lwhen
      { $1 @ [{$2 with casegeneratealternatelabelname = check_labels $2.casegeneratealternatelabelname $3}] }

case_generate_alternative_list:
  | case_generate_alternative Lend Lgenerate
      { [$1] }
  | case_generate_alternative end_generate_statement_body Lend Lgenerate
      { [{$1 with casegeneratealternatelabelname = check_labels $1.casegeneratealternatelabelname $2}] }
  | case_generate_alternative_continued case_generate_alternative Lend Lgenerate
      { $1 @ [$2] }
  | case_generate_alternative_continued case_generate_alternative end_generate_statement_body Lend Lgenerate
      { $1 @ [{$2 with casegeneratealternatelabelname = check_labels $2.casegeneratealternatelabelname $3}] }

/*
(* case_generate_alternative ::=             *)
(*   when [ alternative_label : ] choices => *)
(*     generate_statement_body               *)
*/
case_generate_alternative:
  | case_generate_label_choices
      { match $1 with
        | alternate_label,choices
            -> {casegeneratealternatelabelname=alternate_label;
                casegeneratechoices=choices;
                casegeneratedeclarations=[];
                casegeneratestatements=[]} }
  | case_generate_label_choices generate_statement_body
      { match $1,$2 with
        | (alternate_label,choices),(declarations,statements)
            -> {casegeneratealternatelabelname=alternate_label;
                casegeneratechoices=choices;
                casegeneratedeclarations=declarations;
                casegeneratestatements=statements} }

case_generate_label_choices:
  | choices Lassociation
      { empty_label,$1 }
  | label Lcolon choices Lassociation
      { $1,$3 }

/*
(* generate_statement_body ::=        *)
(*   [ block_declarative_part         *)
(*   begin ]                          *)
(*     { concurrent_statement }       *)
(*   [ end [ alternative_label ] ; ]  *)
*/
generate_statement_body:
  | Lbegin
      { [],[] }
  | block_declarative_part Lbegin
      { $1,[] }
  | concurrent_statements
      { [],$1 }
  | Lbegin concurrent_statements
      { [],$2 }
  | block_declarative_part Lbegin concurrent_statements
      { $1,$3 }

end_generate_statement_body:
  | Lend Lsemicolon
      { empty_label }
  | Lend label Lsemicolon
      { $2 }

/*
(* use_clause ::=                          *)
(*   use selected_name {, selected_name} ; *)
*/
use_clause:
  | Luse selected_name_list Lsemicolon
      { $2 }

use_clause_list:
  | use_clause
      { [$1] }
  | use_clause_list use_clause
      { ($1 @ [$2]) }

/*
(* concurrent_statement ::=                              *)
(*     block_statement                                   *)
(*   | process_statement                                 *)
(*   | concurrent_procedure_call_statement               *)
(*   | concurrent_assertion_statement (ieee syn ignored) *)
(*   | concurrent_signal_assignment_statement            *)
(*   | component_instantiation_statement                 *)
(*   | generate_statement                                *)
*/
concurrent_statement:
  | block_statement
      { ConcurrentBlockStatement $1 }
  | process_statement
      { ConcurrentProcessStatement $1 }
  | concurrent_procedure_call_statement
      { ConcurrentProcedureCallStatement $1 }
  | concurrent_assertion_statement
      { ConcurrentAssertionStatement $1 }
  | concurrent_signal_assignment_statement
      { ConcurrentSignalAssignmentStatement $1 }
  | component_instantiation_statement
      { ConcurrentComponentInstantiationStatement $1 }
  | generate_statement
      { ConcurrentGenerateStatement $1 }

concurrent_statements:
  | concurrent_statement
      { [$1] }
  | concurrent_statements concurrent_statement
      { ($1 @ [$2]) }

/*
(* package_declarative_item ::=                             *)
(*     subprogram_declaration                               *)
(*   | subprogram_instantiation                             *)
(*   | type_declaration                                     *)
(*   | subtype_declaration                                  *)
(*   | constant_declaration                                 *)
(*   | signal_declaration                                   *)
(*   | shared_variable_declaration (not supported ieee syn) *)
(*   | file_declaration (ignored ieee syn)                  *)
(*   | alias_declaration                                    *)
(*   | component_declaration                                *)
(*   | attribute_declaration                                *)
(*   | attribute_specification                              *)
(*   | disconnection_specification (not supported ieee syn) *)
(*   | use_clause                                           *)
(*   | group_template_declaration (not supported ieee syn)  *)
(*   | group_declaration (not supported ieee syn)           *)
*/
package_declarative_item:
  | subprogram_declaration
      { PackageSubProgramDeclaration $1 }
  | subprogram_instantiation
      { PackageSubProgramInstantiation $1 }
  | type_declaration
      { PackageTypeDeclaration $1 }
  | subtype_declaration
      { PackageSubTypeDeclaration $1 }
  | constant_declaration
      { PackageConstantDeclaration $1 }
  | signal_declaration
      { PackageSignalDeclaration $1 }
  | file_declaration
      { PackageFileDeclaration $1 }
  | alias_declaration
      { PackageAliasDeclaration $1 }
  | component_declaration
      { PackageComponentDeclaration $1 }
  | attribute_declaration
      { PackageAttributeDeclaration $1 }
  | attribute_specification
      { PackageAttributeSpecification $1 }
  | use_clause
      { PackageUseClause $1 }

/*
(* package_declarative_part ::=                             *)
(*  { package_declarative_item }                            *)
*/
package_declarative_part:
  | package_declarative_item
      { [$1] }
  | package_declarative_part package_declarative_item
      { ($1 @ [$2]) }

/*
(* package_declaration ::=                                  *)
(*  package identifier is                                   *)
(*    package_header                                        *)
(*    package_declarative_part                              *)
(*  end [ package ] [ package_simple_name ];                *)
*/
package_declaration:
  | Lpackage identifier package_declarations Lend Lsemicolon
      { match $3 with
        | generic_clause,generic_map_aspect,declarations
            -> {packagename=$2;
                packagegenericclause=generic_clause;
                packagegenericmapaspect=generic_map_aspect;
                packagedeclarations=declarations} }
  | Lpackage identifier package_declarations Lend Lpackage Lsemicolon
      { match $3 with
        | generic_clause,generic_map_aspect,declarations
            -> {packagename=$2;
                packagegenericclause=generic_clause;
                packagegenericmapaspect=generic_map_aspect;
                packagedeclarations=declarations} }
  | Lpackage identifier package_declarations Lend identifier Lsemicolon
      { match $3 with
        | generic_clause,generic_map_aspect,declarations
            -> {packagename=check_identifiers $2 $5;
                packagegenericclause=generic_clause;
                packagegenericmapaspect=generic_map_aspect;
                packagedeclarations=declarations} }
  | Lpackage identifier package_declarations Lend Lpackage identifier Lsemicolon
      { match $3 with
        | generic_clause,generic_map_aspect,declarations
            -> {packagename=check_identifiers $2 $6;
                packagegenericclause=generic_clause;
                packagegenericmapaspect=generic_map_aspect;
                packagedeclarations=declarations} }

package_declarations:
  | Lis
      { [],[],[] }
  | Lis package_declarative_part
      { [],[],$2 }
  | Lis package_header
      { match $2 with
        | generic_clause,generic_map_aspect
            -> generic_clause,generic_map_aspect,[] }
  | Lis package_header package_declarative_part
      { match $2 with
        | generic_clause,generic_map_aspect
            -> generic_clause,generic_map_aspect,$3 }

/*
(* package_header ::=                                       *)
(*   [ generic_clause                                       *)
(*   [ generic_map_aspect ; ] ]                             *)
*/
package_header:
  | generic_clause
      { $1,[] }
  | generic_clause generic_map_aspect Lsemicolon
      { $1,$2 }

/*
(* package_body_declarative_item ::=                        *)
(*     subprogram_declaration                               *)
(*   | subprogram_body                                      *)
(*   | subprogram_instantiation                             *)
(*   | type_declaration                                     *)
(*   | subtype_declaration                                  *)
(*   | constant_declaration                                 *)
(*   | shared_variable_declaration (not supported ieee syn) *)
(*   | file_declaration (ignored ieee syn)                  *)
(*   | alias_declaration                                    *)
(*   | use_clause                                           *)
(*   | group_template_declaration (not supported ieee syn)  *)
(*   | group_declaration (not supported ieee syn)           *)
*/
package_body_declarative_item:
  | subprogram_declaration
      { PackageBodySubProgramDeclaration $1 }
  | subprogram_body
      { PackageBodySubProgramBody $1 }
  | subprogram_instantiation
      { PackageBodySubProgramInstantiation $1 }
  | type_declaration
      { PackageBodyTypeDeclaration $1 }
  | subtype_declaration
      { PackageBodySubTypeDeclaration $1 }
  | constant_declaration
      { PackageBodyConstantDeclaration $1 }
  | file_declaration
      { PackageBodyFileDeclaration $1 }
  | alias_declaration
      { PackageBodyAliasDeclaration $1 }
  | use_clause
      { PackageBodyUseClause $1 }

/*
(* package_body_declarative_part ::=                        *)
(*   { package_body_declarative_item }                      *)
*/
package_body_declarative_part:
  | package_body_declarative_item
      { [$1] }
  | package_body_declarative_part package_body_declarative_item
      { ($1 @ [$2]) }

/*
(* package_body ::=                                         *)
(*   package body package_simple_name is                    *)
(*     package_body_declarative_part                        *)
(*   end [ package body ] [ package_simple_name ] ;         *)
*/
package_body:
  | Lpackage Lbody identifier package_body_declarations Lend Lsemicolon
      { {packagebodyname=$3; packagebodydeclarations=$4} }
  | Lpackage Lbody identifier package_body_declarations Lend Lpackage Lbody Lsemicolon
      { {packagebodyname=$3; packagebodydeclarations=$4} }
  | Lpackage Lbody identifier package_body_declarations Lend identifier Lsemicolon
      { {packagebodyname=check_identifiers $3 $6; packagebodydeclarations=$4} }
  | Lpackage Lbody identifier package_body_declarations Lend Lpackage Lbody identifier Lsemicolon
      { {packagebodyname=check_identifiers $3 $8; packagebodydeclarations=$4} }

package_body_declarations:
  | Lis
      { [] }
  | Lis package_body_declarative_part
      { $2 }

/*
(* package_instantiation_declaration ::=                   *)
(*   package identifier is new uninstantiated_package_name *)
(*   [ generic_map_aspect ] ;                              *)
*/

package_instantiation:
  | Lpackage identifier Lis Lnew name Lsemicolon
      { {packageinstantiationidentifier = $2;
         packageinstantiationname = $5;
         packageinstantiationgenericmap = []} }
  | Lpackage identifier Lis Lnew name generic_map_aspect Lsemicolon
      { {packageinstantiationidentifier = $2;
         packageinstantiationname = $5;
         packageinstantiationgenericmap = $6} }

/*
(* entity_declarative_part ::=                              *)
(*   { entity_declarative_item }                            *)
*/
entity_declarative_part:
  | entity_declarative_item
      { [$1] }
  | entity_declarative_part entity_declarative_item
      { ($1 @ [$2]) }

/*
(* entity_declarative_item ::                               *)
(*     subprogram_declaration                               *)
(*   | subprogram_body                                      *)
(*   | subprogram_instantiation                             *)
(*   | type_declaration                                     *)
(*   | subtype_declaration                                  *)
(*   | constant_declaration                                 *)
(*   | signal_declaration                                   *)
(*   | shared_variable_declaration (ieee syn not supported) *)
(*   | file_declaration (ieee syn ignored)                  *)
(*   | alias_declaration                                    *)
(*   | attribute_declaration                                *)
(*   | attribute_specification                              *)
(*   | disconnection_specification (ieee syn not supported) *)
(*   | use_clause                                           *)
(*   | group_template_declaration (ieee syn not supported)  *)
(*   | group_declaration (ieee syn not supported)           *)
*/
entity_declarative_item:
  | subprogram_declaration
      { EntitySubProgramDeclaration $1 }
  | subprogram_body
      { EntitySubProgramBody $1 }
  | subprogram_instantiation
      { EntitySubProgramInstantiation $1 }
  | type_declaration
      { EntityTypeDeclaration $1 }
  | subtype_declaration
      { EntitySubTypeDeclaration $1 }
  | constant_declaration
      { EntityConstantDeclaration $1 }
  | signal_declaration
      { EntitySignalDeclaration $1 }
  | file_declaration
      { EntityFileDeclaration $1 }
  | alias_declaration
      { EntityAliasDeclaration $1 }
  | attribute_declaration
      { EntityAttributeDeclaration $1 }
  | attribute_specification
      { EntityAttributeSpecification $1 }
  | use_clause
      { EntityUseClause $1 }

/*
(* entity_statement_part ::=              *)
(*   { entity_statement }                 *)
*/
entity_statement_part:
  | entity_statement
      { [$1] }
  | entity_statement_part entity_statement
      { ($1 @ [$2]) }

/*
(* entity_statement ::=                   *)
(*     concurrent_assertion_statement     *)
(*   | passive_concurrent_procedure_call  *)
(*   | passive_process_statement          *)
*/
entity_statement:
  | concurrent_assertion_statement
      { EntityConcurrentAssertionStatement $1 }
  | concurrent_procedure_call_statement
      { EntityConcurrentProcedureCallStatement $1 }
  | process_statement
      { EntityProcessStatement $1 }

/*
(* entity_header ::=              *)
(*   [ formal_generic_clause ]    *)
(*   [ formal_port_clause ]       *)
*/
entity_header_helper:
  | Lis
      { ({entitygenerics=[]; entityports=[]},[]) }
  | Lis generic_clause
      { ({entitygenerics=$2; entityports=[]},[]) }
  | Lis port_clause
      { ({entitygenerics=[]; entityports=$2},[]) }
  | Lis generic_clause port_clause
      { ({entitygenerics=$2; entityports=$3},[]) }
  | Lis entity_declarative_part
      { ({entitygenerics=[]; entityports=[]},$2) }
  | Lis generic_clause entity_declarative_part
      { ({entitygenerics=$2; entityports=[]},$3) }
  | Lis port_clause entity_declarative_part
      { ({entitygenerics=[]; entityports=$2},$3) }
  | Lis generic_clause port_clause entity_declarative_part
      { ({entitygenerics=$2; entityports=$3},$4) }

/*
(* entity_declaration ::=                           *)
(*   entity identifier is                           *)
(*     entity_header                                *)
(*     entity_declarative_part                      *)
(*   [ begin                                        *)
(*       entity_statement_part (ieee syn ignored) ] *)
(*     end [ entity ] [ entity_simple_name ] ;      *)
*/
entity_declaration:
  | Lentity identifier entity_header_helper Lend Lsemicolon
      { {entityname=$2; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=[]} }
  | Lentity identifier entity_header_helper Lend Lentity Lsemicolon
      { {entityname=$2; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=[]} }
  | Lentity identifier entity_header_helper Lend identifier Lsemicolon
      { {entityname=$2; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=[]} }
  | Lentity identifier entity_header_helper Lend Lentity identifier Lsemicolon
      { {entityname=$2; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=[]} }
  | Lentity identifier entity_header_helper Lbegin entity_statement_part Lend Lsemicolon
      { {entityname=$2; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=$5} }
  | Lentity identifier entity_header_helper Lbegin entity_statement_part Lend Lentity Lsemicolon
      { {entityname=$2; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=$5} }
  | Lentity identifier entity_header_helper Lbegin entity_statement_part Lend identifier Lsemicolon
      { {entityname=check_identifiers $2 $7; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=$5} }
  | Lentity identifier entity_header_helper Lbegin entity_statement_part Lend Lentity identifier Lsemicolon
      { {entityname=check_identifiers $2 $8; entityheader=fst $3; entitydeclarations=snd $3; entitystatements=$5} }

/*
(* architecture_declarative_part ::=              *)
(*   { block_declarative_item }                   *)
*/
architecture_declarative_part:
  | block_declarative_item
      { [$1] }
  | architecture_declarative_part block_declarative_item
      { ($1 @ [$2]) }

/*
(* architecture_statement_part ::=              *)
(*  { concurrent_statement }                    *)
*/
architecture_statement_part:
  | concurrent_statement
      { [$1] }
  | architecture_statement_part concurrent_statement
      { ($1 @ [$2]) }

/*
(* architecture_body ::=                                 *)
(*   architecture identifier of entity_name is           *)
(*     architecture_declarative_part                     *)
(*   begin                                               *)
(*     architecture_statement_part                       *)
(*   end [ architecture ] [ architecture_simple_name ] ; *)
*/
architecture_body:
  | Larchitecture identifier Lof simple_name architecture_declarations architecture_statements Lend Lsemicolon
      { {archname=$2; archentityname=$4; archdeclarations=$5; archstatements=$6} }
  | Larchitecture identifier Lof simple_name architecture_declarations architecture_statements Lend Larchitecture Lsemicolon
      { {archname=$2; archentityname=$4; archdeclarations=$5; archstatements=$6} }
  | Larchitecture identifier Lof simple_name architecture_declarations architecture_statements Lend identifier Lsemicolon
      { {archname=check_identifiers $2 $8; archentityname=$4; archdeclarations=$5; archstatements=$6} }
  | Larchitecture identifier Lof simple_name architecture_declarations architecture_statements Lend Larchitecture identifier Lsemicolon
      { {archname=check_identifiers $2 $9; archentityname=$4; archdeclarations=$5; archstatements=$6} }

architecture_declarations:
  | Lis
      { [] }
  | Lis architecture_declarative_part
      { $2 }

architecture_statements:
  | Lbegin
      { [] }
  | Lbegin architecture_statement_part
      { $2 }

/*
(* context_clause ::= { context_item } *)
*/
context_clause:
  | context_item
      { [$1] }
  | context_clause context_item
      { ($1 @ [$2]) }

/*
(* context_declaration ::=                       *)
(*   context identifier is                       *)
(*     context_clause                            *)
(*   end [ context ] [ context_simple_name ] ;   *)
*/
context_declaration:
  | Lcontext identifier Lis Lend Lsemicolon
      { {contextname=$2;
         contextclause=[]} }
  | Lcontext identifier Lis Lend Lcontext Lsemicolon
      { {contextname=$2;
         contextclause=[]} }
  | Lcontext identifier Lis Lend identifier Lsemicolon
      { {contextname=check_identifiers $2 $5;
         contextclause=[]} }
  | Lcontext identifier Lis Lend Lcontext identifier Lsemicolon
      { {contextname=check_identifiers $2 $6;
         contextclause=[]} }
  | Lcontext identifier Lis context_clause Lend Lsemicolon
      { {contextname=$2;
         contextclause=$4} }
  | Lcontext identifier Lis context_clause Lend Lcontext Lsemicolon
      { {contextname=$2;
         contextclause=$4} }
  | Lcontext identifier Lis context_clause Lend identifier Lsemicolon
      { {contextname=check_identifiers $2 $6;
         contextclause=$4} }
  | Lcontext identifier Lis context_clause Lend Lcontext identifier Lsemicolon
      { {contextname=check_identifiers $2 $7;
         contextclause=$4} }

/*
(* context_reference ::=                         *)
(*   context selected_name { , selected_name } ; *)
*/
context_reference:
  | Lcontext selected_name_list Lsemicolon
      { $2 }

/*
(* context_item ::=                    *)
(*     library_clause                  *)
(*   | use_clause                      *)
*/
context_item:
  | library_clause
      { ContextLibraryClause $1 }
  | use_clause
      { ContextUseClause $1 }
  | context_reference
      { ContextContextReference $1 }

/*
(* configuration_declaration ::=                      *)
(*   configuration identifier of entity_name is       *)
(*     configuration_declarative_part                 *)
(*     block_configuration                            *)
(*   end [configuration] [configuration_simple_name]; *)
*/
configuration_declaration:
  | Lconfiguration identifier Lof name configuration_declarations block_configuration Lend Lsemicolon
      { {confname=$2; confentityname=$4; confdeclarations=$5; confblock=$6} }
  | Lconfiguration identifier Lof name configuration_declarations block_configuration Lend Lconfiguration Lsemicolon
      { {confname=$2; confentityname=$4; confdeclarations=$5; confblock=$6} }
  | Lconfiguration identifier Lof name configuration_declarations block_configuration Lend simple_name Lsemicolon
      { {confname=check_identifiers $2 $8; confentityname=$4; confdeclarations=$5; confblock=$6} }
  | Lconfiguration identifier Lof name configuration_declarations block_configuration Lend Lconfiguration simple_name Lsemicolon
      { {confname=check_identifiers $2 $9; confentityname=$4; confdeclarations=$5; confblock=$6} }

configuration_declarations:
  | Lis
      { [] }
  | Lis configuration_declarative_part
      { $2 }

/*
(* configuration_declarative_part ::=                 *)
(*   { configuration_declarative_item }               *)
*/
configuration_declarative_part:
  | configuration_declarative_item
      { [$1] }
  | configuration_declarative_part configuration_declarative_item
      { ($1 @ [$2]) }

/*
(* configuration_declarative_item ::=                 *)
(*     use_clause                                     *)
(*   | attribute_specification                        *)
(*   | group_declaration (ieee syn not supported)     *)
*/
configuration_declarative_item:
  | use_clause
      { ConfigurationUseClause $1 }
  | attribute_specification
      { ConfigurationAttributeSpecification $1 }

/*
(* block_configuration ::=                                  *)
(*   for block_specification                                *)
(*     { use_clause }                                       *)
(*     { configuration_item }                               *)
(*   end for ;                                              *)
*/

block_configuration:
  | Lfor block_specification Lend Lfor Lsemicolon
      { {blockspec=$2; blockuses=[]; blockconfigurations=[]} }
  | Lfor block_specification use_clause_list Lend Lfor Lsemicolon
      { {blockspec=$2; blockuses=$3; blockconfigurations=[]} }
  | Lfor block_specification configuration_item_list Lend Lfor Lsemicolon
      { {blockspec=$2; blockuses=[]; blockconfigurations=$3} }
  | Lfor block_specification use_clause_list configuration_item_list Lend Lfor Lsemicolon
      { {blockspec=$2; blockuses=$3; blockconfigurations=$4} }

/*
(* block_specification ::=                                  *)
(*     architecture_name                                    *)
(*   | block_statement_label                                *)
(*   | generate_statement_label [ ( index_specification ) ] *)
*/
block_specification:
  | name
      { BlockSpecificationName $1 }
  | name Lleftparenthesis index_specification Lrightparenthesis
      { BlockSpecificationNameIndex ($1, $3) }

/*
(* index_specification ::=                                  *)
(*     discrete_range                                       *)
(*   | static_expression                                    *)
expression may contain attributes, discrete_range shouldn't
*/
index_specification:
  | discrete_range_without_attribute
      { IndexDiscreteRange $1 }
  | expression
      { IndexStaticExpression $1 }

/*
(* configuration_item ::=                                   *)
(*     block_configuration                                  *)
(*   | component_configuration                              *)
*/
configuration_item:
  | block_configuration
      { BlockConfiguration $1 }
  | component_configuration
      { ComponentConfiguration $1 }

configuration_item_list:
  | configuration_item
      { [$1] }
  | configuration_item_list configuration_item
      { ($1 @ [$2]) }

/*
(* component_configuration ::=              *)
(*   for component_specification            *)
(*     [ binding_indication ; ]             *)
(*     [ block_configuration ]              *)
(*   end for ;                              *)
*/
component_configuration:
  | Lfor component_specification Lend Lfor Lsemicolon
      { {componentspec=$2; componentbinding=empty_binding; componentblockconf=empty_blockconf} }
  | Lfor component_specification block_configuration Lend Lfor Lsemicolon
      { {componentspec=$2; componentbinding=empty_binding; componentblockconf=$3} }
  | Lfor component_specification binding_indication Lsemicolon Lend Lfor Lsemicolon
      { {componentspec=$2; componentbinding=$3; componentblockconf=empty_blockconf} }
  | Lfor component_specification binding_indication Lsemicolon block_configuration Lend Lfor Lsemicolon
      { {componentspec=$2; componentbinding=$3; componentblockconf=$5} }

/*
(* design_file ::= design_unit { design_unit }   *)
*/
design_file:
  | design_unit
      { [$1] }
  | design_file design_unit
      { ($1 @ [$2]) }

/*
(* design_unit ::= context_clause library_unit   *)
*/
design_unit:
  | library_unit
      { {designunitcontextclause=[];
         designunitlibraryunit=$1} }
  | context_clause library_unit
      { {designunitcontextclause=$1;
         designunitlibraryunit=$2} }

/*
(* library_unit ::=                              *)
(*     primary_unit                              *)
(*   | secondary_unit                            *)
*/
library_unit:
  | primary_unit
      { PrimaryUnit $1 }
  | secondary_unit
      { SecondaryUnit $1 }

/*
(* primary_unit ::=                              *)
(*     entity_declaration                        *)
(*   | configuration_declaration                 *)
(*   | package_declaration                       *)
(*   | package_instantiation_declaration (TODO)  *)
(*   | context_declaration                       *)
(*   | PSL_Verification_Unit (TODO)              *)
*/
primary_unit:
  | entity_declaration
      { EntityDeclaration $1 }
  | configuration_declaration
      { ConfigurationDeclaration $1 }
  | package_declaration
      { PackageDeclaration $1 }
  | package_instantiation
      { PackageInstantiation $1 }
  | context_declaration
      { ContextDeclaration $1 }

/*
(* secondary_unit ::=                            *)
(*     architecture_body                         *)
(*   | package_body                              *)
*/
secondary_unit:
  | architecture_body
      { ArchitectureBody $1 }
  | package_body
      { PackageBody $1 }

top_level_file:
  | Leof
      { [] }
  | Lprotected
      { [ {designunitcontextclause = []; designunitlibraryunit = ProtectedUnit} ] }
  | design_file Leof
      { $1 }
  | Lbomutf8 Leof
      { [] }
  | Lbomutf8 design_file Leof
      { $2 }
  | Lbomutf7 Leof
      { [] }
  | Lbomutf7 design_file Leof
      { $2 }

top_level_expression:
  | expression Leof
      { $1 }

unused_tokens:
  |  Lassumeguarantee  { [] }
  |  Lassume  { [] }
  |  Lbreak  { [] }
  |  Lcover  { [] }
  |  Ldisconnect  { [] }
  |  Lfairness  { [] }
  |  Lforce  { [] }
  |  Lgroup  { [] }
  |  Llimit  { [] }
  |  Llinkage  { [] }
  |  Lnature  { [] }
  |  Lnoise  { [] }
  |  Lpercent  { [] }
  |  Lprocedural  { [] }
  |  Lproperty  { [] }
  |  Lprotected  { [] }
  |  Lquantity  { [] }
  |  Lreference  { [] }
  |  Lrelease  { [] }
  |  Lrestrictguarantee  { [] }
  |  Lrestrict  { [] }
  |  Lsequence  { [] }
  |  Lspectrum  { [] }
  |  Lstrong  { [] }
  |  Lsubnature  { [] }
  |  Lterminal  { [] }
  |  Lthrough  { [] }
  |  Ltolerance  { [] }
  |  Lvmode  { [] }
  |  Lvprop  { [] }
  |  Lvunit  { [] }   

%%

