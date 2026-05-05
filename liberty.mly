%{

%}

%token  ACCEPT
%token  AMPERSAND
%token  AT
%token  BACKQUOTE
%token  BACKSLASH
%token  CARET
%token  COLON
%token  COMMA
%token <token> CONS1
%token <token*token> CONS2
%token <token*token*token> CONS3
%token <token*token*token*token> CONS4
%token  DEFAULT
%token  DIV
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  EQ
%token  ERROR
%token  ERROR_TOKEN
%token  GREATER
%token  HASH
%token <string> IDENT
%token  KW_DEFINE
%token  KW_DEFINE_GROUP
%token  KW_FALSE
%token  KW_TRUE
%token  LBRACE
%token  LBRACK
%token  LCURLY
%token  LESS
%token  LINEFEED
%token  LPAR
%token  MINUS
%token  MULT
%token <string> NUM
%token  PERCENT
%token  PLING
%token  PLUS
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token  RCURLY
%token  RPAR
%token  SEMI
%token <string list> SLIST
%token <string> STRING
%token  TILDE
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  UNARY
%token  UNDERSCORE
%token  VBAR
%type <token> ml_start
%start ml_start
%%


ml_start: file EOF_TOKEN { TUPLE3(STRING("ml_start0"),$1,EOF_TOKEN) }

file: /* 1 */ group { ($1) }

group: head LCURLY /* 2 */ statements RCURLY { TUPLE5(STRING("group4"),$1,LCURLY,$3,RCURLY) }
	|	head LCURLY /* 3 */ RCURLY { TUPLE4(STRING("group6"),$1,LCURLY,RCURLY) }

statements: statement { ($1) }
	|	statements statement { TUPLE3(STRING("statements8"),$1,$2) }

statement: simple_attr { ($1) }
	|	complex_attr { ($1) }
	|	define { ($1) }
	|	define_group { ($1) }
	|	group { ($1) }

simple_attr: IDENT COLON attr_val_expr /* 4 */ SEMI { TUPLE5(STRING("simple_attr15"),IDENT $1,COLON,$3,SEMI) }
	|	IDENT COLON attr_val_expr { TUPLE4(STRING("simple_attr16"),IDENT $1,COLON,$3) }
	|	IDENT EQ attr_val_expr /* 5 */ SEMI { TUPLE5(STRING("simple_attr18"),IDENT $1,EQ,$3,SEMI) }

complex_attr: head SEMI { TUPLE3(STRING("complex_attr19"),$1,SEMI) }
	|	head { ($1) }

head: IDENT LPAR /* 6 */ param_list RPAR { TUPLE5(STRING("head22"),IDENT $1,LPAR,$3,RPAR) }
	|	IDENT LPAR RPAR { TUPLE4(STRING("head23"),IDENT $1,LPAR,RPAR) }

param_list: attr_val { ($1) }
	|	param_list COMMA attr_val { TUPLE4(STRING("param_list25"),$1,COMMA,$3) }
	|	param_list attr_val { TUPLE3(STRING("param_list26"),$1,$2) }

define: KW_DEFINE LPAR s_or_i COMMA s_or_i COMMA s_or_i RPAR SEMI { TUPLE10(STRING("define27"),KW_DEFINE,LPAR,$3,COMMA,$5,COMMA,$7,RPAR,SEMI) }

define_group: KW_DEFINE_GROUP LPAR s_or_i COMMA s_or_i RPAR SEMI { TUPLE8(STRING("define_group28"),KW_DEFINE_GROUP,LPAR,$3,COMMA,$5,RPAR,SEMI) }

s_or_i: STRING { (STRING $1) }
	|	IDENT { (IDENT $1) }

attr_val: NUM { (NUM $1) }
	|	s_or_i { ($1) }
	|	s_or_i COLON s_or_i { TUPLE4(STRING("attr_val33"),$1,COLON,$3) }
	|	KW_TRUE { (KW_TRUE) }
	|	KW_FALSE { (KW_FALSE) }

attr_val_expr: STRING { (STRING $1) }
	|	KW_TRUE { (KW_TRUE) }
	|	KW_FALSE { (KW_FALSE) }
	|	expr { ($1) }

expr: expr PLUS expr { TUPLE4(STRING("expr40"),$1,PLUS,$3) }
	|	expr MINUS expr { TUPLE4(STRING("expr41"),$1,MINUS,$3) }
	|	expr MULT expr { TUPLE4(STRING("expr42"),$1,MULT,$3) }
	|	expr DIV expr { TUPLE4(STRING("expr43"),$1,DIV,$3) }
	|	LPAR expr RPAR { TUPLE4(STRING("expr44"),LPAR,$2,RPAR) }
	|	MINUS expr { TUPLE3(STRING("expr45"),MINUS,$2) }
	|	PLUS expr { TUPLE3(STRING("expr46"),PLUS,$2) }
	|	NUM { (NUM $1) }
	|	NUM IDENT { TUPLE3 (STRING "units", NUM $1, IDENT $2) }
	|	IDENT { (IDENT $1) }


