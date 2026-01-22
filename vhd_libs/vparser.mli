type token =
    EMPTY
  | DOUBLE of (token * token)
  | TRIPLE of (token * token * token)
  | QUADRUPLE of (token * token * token * token)
  | QUINTUPLE of (token * token * token * token * token)
  | SEXTUPLE of (token * token * token * token * token * token)
  | SEPTUPLE of (token * token * token * token * token * token * token)
  | OCTUPLE of
      (token * token * token * token * token * token * token * token)
  | NONUPLE of
      (token * token * token * token * token * token * token * token * token)
  | DECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token)
  | UNDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token)
  | DUODECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token)
  | TREDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token)
  | QUATTUORDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token)
  | QUINDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token)
  | SEXDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token)
  | SEPTENDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token * 
       token)
  | OCTODECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token)
  | NOVEMDECUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token)
  | VIGENUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token * token)
  | UNVIGENUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token * token * token)
  | DUOVIGENUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token * token * token * token)
  | TREVIGENUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token)
  | QUATTUORVIGENUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token)
  | QUINVIGENUPLE of
      (token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token *
       token * token * token * token * token * token * token * token * 
       token)
  | BITSEL
  | PARTSEL
  | IOPORT
  | SUBMODULE
  | SUBCCT
  | MODINST
  | PRIMINST
  | RANGE of (token * token)
  | PLIST of token list
  | TLIST of token list
  | THASH of ((token, unit) Hashtbl.t * (token, unit) Hashtbl.t)
  | IMPLICIT
  | RECEIVER
  | DRIVER
  | BIDIR
  | DOTTED of token list
  | P_CELLDEFINE
  | P_DEFINE
  | P_DELAY_MODE_PATH
  | P_DISABLE_PORTFAULTS
  | P_ELSE
  | P_ENABLE_PORTFAULTS
  | P_ENDCELLDEFINE
  | P_ENDIF
  | P_IFDEF
  | P_IFNDEF
  | P_INCLUDE of string
  | P_NOSUPPRESS_FAULTS
  | P_PROTECT
  | P_ENDPROTECT
  | P_RESETALL
  | P_SUPPRESS_FAULTS
  | P_TIMESCALE of string
  | PREPROC of string
  | NAMED
  | GENITEM
  | UNKNOWN
  | NOTCONST
  | SCALAR
  | VOID
  | SPECIAL
  | PARAMUSED
  | TASKUSED
  | FUNCUSED
  | SENSUSED
  | FUNCASSIGNED
  | MEMORY
  | NMOS
  | PMOS
  | TRAN
  | TRANIF of string
  | TASKREF
  | FUNCREF
  | CELLPIN
  | CASECOND
  | WILDEQUAL
  | GENCASE
  | GENCASECOND
  | MINTYPMAX
  | ENDLABEL
  | CONCAT
  | TEDGE of (char * char)
  | FLOATNUM of float
  | IDSTR of string
  | ID of Idhash.idhash
  | INT of int
  | WIDTHNUM of (int * int * int)
  | INTNUM of string
  | BINNUM of string
  | OCTNUM of string
  | DECNUM of string
  | HEXNUM of string
  | INT64 of int64
  | ASCNUM of string
  | ILLEGAL of char
  | DOLLAR
  | EOF
  | P_AMPERSAND
  | AT
  | P_CARET
  | COLON
  | COMMA
  | DIVIDE
  | EQUALS
  | GREATER
  | HASH
  | LBRACK
  | LCURLY
  | LESS
  | LPAREN
  | MINUS
  | MODULO
  | DOT
  | PLING
  | PLUS
  | QUERY
  | RBRACK
  | RCURLY
  | RPAREN
  | SEMICOLON
  | P_TILDE
  | TIMES
  | P_VBAR
  | ALWAYS
  | AND
  | ASSERT
  | ASSIGN
  | ASSIGNMENT
  | DLYASSIGNMENT
  | AUTOMATIC
  | BEGIN
  | BUF
  | BUFIF of string
  | CASE
  | CASEX
  | CASEZ
  | CLOCKING
  | COVER
  | DEASSIGN
  | DEFAULT
  | DEFPARAM
  | DISABLE
  | DO
  | ELSE
  | END
  | ENDCASE
  | ENDCLOCKING
  | ENDFUNCTION
  | ENDGENERATE
  | ENDMODULE
  | ENDPRIMITIVE
  | ENDSPECIFY
  | ENDTABLE
  | ENDTASK
  | EVENT
  | FINAL
  | FOR
  | FOREVER
  | FUNCTION
  | GENERATE
  | GENVAR
  | IF
  | IFF
  | INITIAL
  | INOUT
  | INPUT
  | INTEGER
  | TIME
  | LOCALPARAM
  | MODULE
  | NAND
  | NEGEDGE
  | NOR
  | NOT
  | NOTIF of string
  | OR
  | OUTPUT
  | PARAMETER
  | POSEDGE
  | PRIMITIVE
  | PROPERTY
  | PULLUP
  | REAL
  | REG
  | REPEAT
  | SCALARED
  | SIGNED
  | SPECIFY
  | STATIC
  | SUPPLY0
  | SUPPLY1
  | TABLE
  | TASK
  | TRI
  | TRI0
  | TRI1
  | UNSIGNED
  | VECTORED
  | WEAK of string
  | PWEAK of string
  | STRONG of string
  | PSTRONG of string
  | WHILE
  | WIRE
  | XNOR
  | XOR
  | D_ATTRIBUTE
  | D_BITS
  | D_C
  | D_CLOG2
  | D_COUNTDRIVERS
  | D_COUNTONES
  | D_DISPLAY
  | D_ERROR
  | D_FATAL
  | D_FCLOSE
  | D_FDISPLAY
  | D_FEOF
  | D_FFLUSH
  | D_FGETC
  | D_FGETS
  | D_FINISH
  | D_FOPEN
  | D_FSCANF
  | D_FWRITE
  | D_FWRITEH
  | D_INFO
  | D_ISUNKNOWN
  | D_MONITOR
  | D_ONEHOT
  | D_ONEHOT0
  | D_RANDOM
  | D_READMEMB
  | D_READMEMH
  | D_SIGNED
  | D_SSCANF
  | D_STIME
  | D_STOP
  | D_TEST_PLUSARGS
  | D_TIME
  | D_UNSIGNED
  | D_WARNING
  | D_WRITE
  | NOCHANGE
  | D_HOLD
  | D_PERIOD
  | D_RECOVERY
  | D_RECREM
  | D_REMOVAL
  | D_SETUPHOLD
  | D_SETUP
  | D_SKEW
  | D_TIMESKEW
  | D_WIDTH
  | SHOWCANCELLED
  | NOSHOWCANCELLED
  | SPECPARAM
  | IF_NONE
  | P_TILDE_VBAR
  | TOKEN_EDGE01
  | TOKEN_EDGE_10
  | TOKEN_ZERO
  | TOKEN_ONE
  | PATHPULSE
  | FULLSKEW
  | PULSESTYLE_ONDETECT
  | PULSESTYLE_ONEVENT
  | EDGE
  | P_TRUE
  | Z_OR_X of string
  | P_NXOR
  | P_OROR
  | P_ANDAND
  | P_NOR
  | P_XNOR
  | P_NAND
  | P_EQUAL
  | P_NOTEQUAL
  | P_CASEEQUAL
  | P_CASENOTEQUAL
  | P_WILDEQUAL
  | P_WILDNOTEQUAL
  | P_GTE
  | P_LTE
  | P_SLEFT
  | P_SRIGHT
  | P_SSRIGHT
  | P_POW
  | P_PLUSCOLON
  | P_MINUSCOLON
  | P_EQGT
  | P_ASTGT
  | P_ANDANDAND
  | P_POUNDPOUND
  | P_DOTSTAR
  | P_ATAT
  | P_COLONCOLON
  | P_COLONEQ
  | P_COLONDIV
  | P_ORMINUSGT
  | P_OREQGT
  | P_PLUSEQ
  | P_MINUSEQ
  | P_TIMESEQ
  | P_DIVEQ
  | P_MODEQ
  | P_ANDEQ
  | P_OREQ
  | P_XOREQ
  | P_SLEFTEQ
  | P_SRIGHTEQ
  | P_SSRIGHTEQ
  | P_MINUSGT
  | PSL
  | PSL_DEFAULT
  | PSL_ALWAYS
  | PSL_ASSERT
  | PSL_CLOCK
  | PSL_COVER
  | PSL_REPORT
  | PSL_FOR
  | PSL_IF
  | PSL_ABORT
  | PSL_ASSUME_GUARANTEE
  | PSL_BEFORE_PLING
  | PSL_BEFORE_
  | PSL_BEFORE
  | PSL_BOOLEAN
  | PSL_CONST
  | PSL_ENDPOINT
  | PSL_EVENTUALLY_PLING
  | PSL_FAIRNESS
  | PSL_FELL
  | PSL_FORALL
  | PSL_IN
  | PSL_INF
  | PSL_INHERIT
  | PSL_NEVER
  | PSL_NEXT_PLING
  | PSL_NEXT
  | PSL_NEXT_A_PLING
  | PSL_NEXT_A
  | PSL_NEXT_E_PLING
  | PSL_NEXT_E
  | PSL_NEXT_EVENT_PLING
  | PSL_NEXT_EVENT
  | PSL_NEXT_EVENT_A_PLING
  | PSL_NEXT_EVENT_A
  | PSL_NEXT_EVENT_E_PLING
  | PSL_NEXT_EVENT_E
  | PSL_PREV
  | PSL_PROPERTY
  | PSL_RESTRICT_GUARANTEE
  | PSL_ROSE
  | PSL_SEQUENCE
  | PSL_STABLE
  | PSL_UNION
  | PSL_UNTIL
  | PSL_UNTIL_
  | PSL_VMODE
  | PSL_VPROP
  | PSL_VUNIT
  | PSL_WITHIN
  | TokenLPar
  | TokenRPar
  | TokenLBr
  | TokenRBr
  | TokenProp of string
  | TokenNEQ
  | TokenEQ
  | TokenEEQ
  | TokenGen
  | TokenFin
  | TokenOpO
  | TokenOpH
  | TokenNeg
  | TokenNext
  | TokenYest
  | TokenZest
  | TokenUnt
  | TokenOpW
  | TokenRel
  | TokenOpS
  | TokenOpT
  | TokenAnd
  | TokenOr
  | TokenImp
  | TokenEq
  | TokenFby
  | TokenBFby
  | TokenTrig
  | TokenBTrig
  | TokenCup
  | TokenCap
  | TokenScol
  | TokenCol
  | TokenStar
  | TokenPlus
  | TokenCl
  | RELATION
  | FINITE
  | SIGN_EXT
  | PROPOSITION
  | SDIV
  | SREM
  | SMOD
  | ENDOFFILE
module type Ordered = sig type t val compare : token -> token -> int end
module OrdTok : sig type t = token val compare : token -> token -> int end
type tset = Set.Make(OrdTok).t
type tsigattr =
    Sigundef
  | Sigarray of tset array
  | Sigparam of token
  | Sigtask of token
  | Sigfunc of token
  | Signamed of token
and symtab = {
  symattr : tset;
  width : token;
  path : Idhash.idhash;
  sigattr : tsigattr;
  localsyms : shash;
}
and sentries = (Idhash.idhash, symtab) Hashtbl.t
and symrec = {
  nxt : shash;
  syms : sentries;
  stabarch : string;
  stabnam : string;
}
and shash = EndShash | Shash of symrec
module TokSet :
  sig
    type elt = OrdTok.t
    type t = Set.Make(OrdTok).t
    val empty : t
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val disjoint : t -> t -> bool
    val diff : t -> t -> t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val min_elt_opt : t -> elt option
    val max_elt : t -> elt
    val max_elt_opt : t -> elt option
    val choose : t -> elt
    val choose_opt : t -> elt option
    val find : elt -> t -> elt
    val find_opt : elt -> t -> elt option
    val find_first : (elt -> bool) -> t -> elt
    val find_first_opt : (elt -> bool) -> t -> elt option
    val find_last : (elt -> bool) -> t -> elt
    val find_last_opt : (elt -> bool) -> t -> elt option
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'acc -> 'acc) -> t -> 'acc -> 'acc
    val map : (elt -> elt) -> t -> t
    val filter : (elt -> bool) -> t -> t
    val filter_map : (elt -> elt option) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val split : elt -> t -> t * bool * t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val subset : t -> t -> bool
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val to_list : t -> elt list
    val of_list : elt list -> t
    val to_seq_from : elt -> t -> elt Seq.t
    val to_seq : t -> elt Seq.t
    val to_rev_seq : t -> elt Seq.t
    val add_seq : elt Seq.t -> t -> t
    val of_seq : elt Seq.t -> t
  end
val enterid : string -> Idhash.idhash
val yytransl_const : int array
val yytransl_block : int array
val yylhs : string
val yylen : string
val yydefred : string
val yydgoto : string
val yysindex : string
val yyrindex : string
val yygindex : string
val yytablesize : int
val yytable : string
val yycheck : string
val yynames_const : string
val yynames_block : string
val yyact : (Parsing.parser_env -> Obj.t) array
val yytables : Parsing.parse_tables
val start : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> token
