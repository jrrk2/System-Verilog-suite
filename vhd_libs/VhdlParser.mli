
(* The type of tokens. *)

type token = 
  | Lxor
  | Lxnor
  | Lword of (string*int)
  | Lwith
  | Lwhile
  | Lwhen
  | Lwait
  | Lvunit
  | Lvprop
  | Lvmode
  | Lverticalbar
  | Lvariable
  | Luse
  | Luntil
  | Lunits
  | Lunaffected
  | Ltype
  | Ltransport
  | Ltolerance
  | Lto
  | Lthrough
  | Lthen
  | Lterminal
  | Lsubtype
  | Lsubnature
  | Lstrong
  | Lstring of (string*int)
  | Lsrl
  | Lsra
  | Lspectrum
  | Lsll
  | Lslash
  | Lsla
  | Lsignal
  | Lshared
  | Lseverity
  | Lsequence
  | Lsemicolon
  | Lselect
  | Lror
  | Lrol
  | Lrightparenthesis
  | Lrightbracket
  | Lreturn
  | Lrestrictguarantee
  | Lrestrict
  | Lreport
  | Lrem
  | Lrelease
  | Lreject
  | Lregister
  | Lreference
  | Lrecord
  | Lrange
  | Lquote
  | Lquestionmark
  | Lquantity
  | Lpure
  | Lprotected
  | Lproperty
  | Lprocess
  | Lprocedure
  | Lprocedural
  | Lpostponed
  | Lport
  | Lplus
  | Lpercent
  | Lparameter
  | Lpackage
  | Lout
  | Lothers
  | Lor
  | Lopen
  | Lon
  | Lof
  | Lnull
  | Lnotequal
  | Lnot
  | Lnor
  | Lnoise
  | Lnext
  | Lnew
  | Lnature
  | Lnand
  | Lmultiply
  | Lmod
  | Lminus
  | Lmatchingnotequal
  | Lmatchinglessequal
  | Lmatchingless
  | Lmatchinggreaterequal
  | Lmatchinggreater
  | Lmatchingequal
  | Lmap
  | Lloop
  | Lliteral
  | Llinkage
  | Llimit
  | Llibrary
  | Lless
  | Lleftparenthesis
  | Lleftbracket
  | Llabel
  | Lis
  | Lint of (string*string*string*int)
  | Linout
  | Linertial
  | Lin
  | Limpure
  | Limmediate
  | Lif
  | Lguarded
  | Lgroup
  | Lgreater
  | Lgeneric
  | Lgenerate
  | Lfunction
  | Lforce
  | Lfor
  | Lfloat of (string*string*string*string*int)
  | Lfile
  | Lfairness
  | Lexponential
  | Lexit
  | Lexclamationmark
  | Lequal
  | Leof
  | Lentity
  | Lend
  | Lelsif
  | Lelse
  | Ldownto
  | Ldot
  | Ldisconnect
  | Lcover
  | Lcontext
  | Lconstant
  | Lconfiguration
  | Lcomponent
  | Lcomma
  | Lcolon
  | Lchar of (char*int)
  | Lcase
  | Lbus
  | Lbuffer
  | Lbreak
  | Lbomutf8
  | Lbomutf7
  | Lbody
  | Lblock
  | Lbegin
  | Lattribute
  | Lassumeguarantee
  | Lassume
  | Lassociation
  | Lassert
  | Larray
  | Larchitecture
  | Land
  | Lampersand
  | Lall
  | Lalias
  | Lafter
  | Lacross
  | Laccess
  | Labs

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val top_level_file: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (VhdlTypes.vhdl_design_file)

val top_level_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (VhdlTypes.vhdl_expression)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val top_level_file: Lexing.position -> (VhdlTypes.vhdl_design_file) MenhirInterpreter.checkpoint
  
  val top_level_expression: Lexing.position -> (VhdlTypes.vhdl_expression) MenhirInterpreter.checkpoint
  
end
