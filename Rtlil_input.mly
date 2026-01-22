%{
  open Rtlil_input_types
  let stderr = open_out "parser_stderr.log" (* to capture parser trace mode info *)
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()
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
%token <token*token*token*token*token> CONS5
%token <token*token*token*token*token*token> CONS6
%token <token*token*token*token*token*token*token> CONS7
%token <token*token*token*token*token*token*token*token> CONS8
%token <token*token*token*token*token*token*token*token*token> CONS9
%token  DEFAULT
%token  DOLLAR
%token  DOT
%token  DOUBLEQUOTE
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  END
%token  EOF_TOKEN
%token  ERROR_TOKEN
%token  Error
%token  GREATER
%token  HASH
%token  LBRACE
%token  LBRACK
%token  LESS
%token  LINEFEED
%token  PERCENT
%token  PLING
%token  QUERY
%token  QUOTE
%token  RBRACE
%token  RBRACK
%token <string list> SLIST
%token <string> STRING
%token  TILDE
%token <token list> TLIST
%token  TOK_ALWAYS
%token  TOK_ASSIGN
%token  TOK_ATTRIBUTE
%token  TOK_AUTOIDX
%token  TOK_CASE
%token  TOK_CELL
%token  TOK_CONNECT
%token  TOK_EDGE
%token  TOK_END
%token  TOK_EOL
%token  TOK_GLOBAL
%token  TOK_HIGH
%token <string> TOK_ID
%token  TOK_INIT
%token  TOK_INOUT
%token  TOK_INPUT
%token <int> TOK_INT
%token  TOK_INVALID
%token  TOK_LOW
%token  TOK_MEMORY
%token  TOK_MEMWR
%token  TOK_MODULE
%token  TOK_NEGEDGE
%token  TOK_OFFSET
%token  TOK_OUTPUT
%token  TOK_PARAMETER
%token  TOK_POSEDGE
%token  TOK_PROCESS
%token  TOK_REAL
%token  TOK_SIGNED
%token  TOK_SIZE
%token <string> TOK_STRING
%token  TOK_SWITCH
%token  TOK_SYNC
%token  TOK_UPDATE
%token  TOK_UPTO
%token <string> TOK_VALUE
%token  TOK_WIDTH
%token  TOK_WIRE
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  UNDERSCORE
%token  VBAR
%type <token> ml_start
%start ml_start
%%


ml_start: rtlil_input EOF_TOKEN { TUPLE3(STRING("ml_start1"),$1,EOF_TOKEN) }

rtlil_input: optional_eol /* 1 */ design { TUPLE3(STRING("rtlil_input1"),$1,$2) }

EOL: optional_eol TOK_EOL { TUPLE3(STRING("EOL1"),$1,TOK_EOL) }

optional_eol: optional_eol TOK_EOL { TUPLE3(STRING("optional_eol1"),$1,TOK_EOL) }
	|	/* empty */ { EMPTY_TOKEN }

design: design module_rule { TUPLE3(STRING("design1"),$1,$2) }
	|	design attr_stmt { TUPLE3(STRING("design2"),$1,$2) }
	|	design autoidx_stmt { TUPLE3(STRING("design3"),$1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

module_rule: TOK_MODULE TOK_ID EOL /* 2 */ module_body TOK_END /* 3 */ EOL { TUPLE7(STRING("module1"),TOK_MODULE,TOK_ID $2,$3,$4,TOK_END,$6) }

module_body: module_body module_stmt { TUPLE3(STRING("module_body1"),$1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

module_stmt: param_stmt { ($1) }
	|	param_defval_stmt { ($1) }
	|	attr_stmt { ($1) }
	|	wire_stmt { ($1) }
	|	memory_stmt { ($1) }
	|	cell_stmt { ($1) }
	|	proc_stmt { ($1) }
	|	conn_stmt { ($1) }

param_stmt: TOK_PARAMETER TOK_ID EOL { TUPLE4(STRING("param_stmt1"),TOK_PARAMETER,TOK_ID $2,$3) }

param_defval_stmt: TOK_PARAMETER TOK_ID constant EOL { TUPLE5(STRING("param_defval_stmt1"),TOK_PARAMETER,TOK_ID $2,$3,$4) }

attr_stmt: TOK_ATTRIBUTE TOK_ID constant EOL { TUPLE5(STRING("attr_stmt1"),TOK_ATTRIBUTE,TOK_ID $2,$3,$4) }

autoidx_stmt: TOK_AUTOIDX TOK_INT EOL { TUPLE4(STRING("autoidx_stmt1"),TOK_AUTOIDX,TOK_INT $2,$3) }

wire_stmt: TOK_WIRE /* 4 */ wire_options TOK_ID EOL { TUPLE5(STRING("wire_stmt1"),TOK_WIRE,$2,TOK_ID $3,$4) }

wire_options: wire_options TOK_WIDTH TOK_INT { TUPLE4(STRING("wire_options1"),$1,TOK_WIDTH,TOK_INT $3) }
	|	wire_options TOK_WIDTH TOK_INVALID { TUPLE4(STRING("wire_options2"),$1,TOK_WIDTH,TOK_INVALID) }
	|	wire_options TOK_UPTO { TUPLE3(STRING("wire_options3"),$1,TOK_UPTO) }
	|	wire_options TOK_SIGNED { TUPLE3(STRING("wire_options4"),$1,TOK_SIGNED) }
	|	wire_options TOK_OFFSET TOK_INT { TUPLE4(STRING("wire_options5"),$1,TOK_OFFSET,TOK_INT $3) }
	|	wire_options TOK_INPUT TOK_INT { TUPLE4(STRING("wire_options6"),$1,TOK_INPUT,TOK_INT $3) }
	|	wire_options TOK_OUTPUT TOK_INT { TUPLE4(STRING("wire_options7"),$1,TOK_OUTPUT,TOK_INT $3) }
	|	wire_options TOK_INOUT TOK_INT { TUPLE4(STRING("wire_options8"),$1,TOK_INOUT,TOK_INT $3) }
	|	/* empty */ { EMPTY_TOKEN }

memory_stmt: TOK_MEMORY /* 5 */ memory_options TOK_ID EOL { TUPLE5(STRING("memory_stmt1"),TOK_MEMORY,$2,TOK_ID $3,$4) }

memory_options: memory_options TOK_WIDTH TOK_INT { TUPLE4(STRING("memory_options1"),$1,TOK_WIDTH,TOK_INT $3) }
	|	memory_options TOK_SIZE TOK_INT { TUPLE4(STRING("memory_options2"),$1,TOK_SIZE,TOK_INT $3) }
	|	memory_options TOK_OFFSET TOK_INT { TUPLE4(STRING("memory_options3"),$1,TOK_OFFSET,TOK_INT $3) }
	|	/* empty */ { EMPTY_TOKEN }

cell_stmt: TOK_CELL TOK_ID TOK_ID EOL /* 6 */ cell_body TOK_END EOL { TUPLE8(STRING("cell_stmt1"),TOK_CELL,TOK_ID $2,TOK_ID $3,$4,$5,TOK_END,$7) }

cell_body: cell_body TOK_PARAMETER TOK_ID constant EOL { TUPLE6(STRING("cell_body1"),$1,TOK_PARAMETER,TOK_ID $3,$4,$5) }
	|	cell_body TOK_PARAMETER TOK_SIGNED TOK_ID constant EOL { TUPLE7(STRING("cell_body2"),$1,TOK_PARAMETER,TOK_SIGNED,TOK_ID $4,$5,$6) }
	|	cell_body TOK_PARAMETER TOK_REAL TOK_ID constant EOL { TUPLE7(STRING("cell_body3"),$1,TOK_PARAMETER,TOK_REAL,TOK_ID $4,$5,$6) }
	|	cell_body TOK_CONNECT TOK_ID sigspec EOL { TUPLE6(STRING("cell_body4"),$1,TOK_CONNECT,TOK_ID $3,$4,$5) }
	|	/* empty */ { EMPTY_TOKEN }

proc_stmt: TOK_PROCESS TOK_ID EOL /* 7 */ case_body sync_list TOK_END EOL { TUPLE8(STRING("proc_stmt1"),TOK_PROCESS,TOK_ID $2,$3,$4,$5,TOK_END,$7) }

switch_stmt: TOK_SWITCH sigspec EOL /* 8 */ attr_list switch_body TOK_END EOL { TUPLE8(STRING("switch_stmt1"),TOK_SWITCH,$2,$3,$4,$5,TOK_END,$7) }

attr_list: /* empty */ { EMPTY_TOKEN }
	|	attr_list attr_stmt { CONS2($1,$2) }

switch_body: switch_body TOK_CASE /* 9 */ compare_list EOL case_body { TUPLE6(STRING("switch_body1"),$1,TOK_CASE,$3,$4,$5) }
	|	/* empty */ { EMPTY_TOKEN }

compare_list: sigspec { CONS1 ($1) }
	|	compare_list COMMA sigspec { CONS3($1,COMMA,$3) }
	|	/* empty */ { EMPTY_TOKEN }

case_body: case_body attr_stmt { TUPLE3(STRING("case_body1"),$1,$2) }
	|	case_body switch_stmt { TUPLE3(STRING("case_body2"),$1,$2) }
	|	case_body assign_stmt { TUPLE3(STRING("case_body3"),$1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

assign_stmt: TOK_ASSIGN sigspec sigspec EOL { TUPLE5(STRING("assign_stmt1"),TOK_ASSIGN,$2,$3,$4) }

sync_list: sync_list TOK_SYNC sync_type sigspec EOL /* 10 */ update_list { CONS6($1,TOK_SYNC,$3,$4,$5,$6) }
	|	sync_list TOK_SYNC TOK_ALWAYS EOL /* 11 */ update_list { CONS5($1,TOK_SYNC,TOK_ALWAYS,$4,$5) }
	|	sync_list TOK_SYNC TOK_GLOBAL EOL /* 12 */ update_list { CONS5($1,TOK_SYNC,TOK_GLOBAL,$4,$5) }
	|	sync_list TOK_SYNC TOK_INIT EOL /* 13 */ update_list { CONS5($1,TOK_SYNC,TOK_INIT,$4,$5) }
	|	/* empty */ { EMPTY_TOKEN }

sync_type: TOK_LOW { (TOK_LOW) }
	|	TOK_HIGH { (TOK_HIGH) }
	|	TOK_POSEDGE { (TOK_POSEDGE) }
	|	TOK_NEGEDGE { (TOK_NEGEDGE) }
	|	TOK_EDGE { (TOK_EDGE) }

update_list: update_list TOK_UPDATE sigspec sigspec EOL { CONS5($1,TOK_UPDATE,$3,$4,$5) }
	|	update_list attr_list TOK_MEMWR TOK_ID sigspec sigspec sigspec constant EOL { CONS9($1,$2,TOK_MEMWR,TOK_ID $4,$5,$6,$7,$8,$9) }
	|	/* empty */ { EMPTY_TOKEN }

constant: TOK_VALUE { (TOK_VALUE $1) }
	|	TOK_INT { (TOK_INT $1) }
	|	TOK_STRING { (TOK_STRING $1) }

sigspec: constant { ($1) }
	|	TOK_ID { (TOK_ID $1) }
	|	sigspec LBRACK TOK_INT RBRACK { TUPLE5(STRING("sigspec3"),$1,LBRACK,TOK_INT $3,RBRACK) }
	|	sigspec LBRACK TOK_INT COLON TOK_INT RBRACK { TUPLE7(STRING("sigspec4"),$1,LBRACK,TOK_INT $3,COLON,TOK_INT $5,RBRACK) }
	|	LBRACE sigspec_list RBRACE { TUPLE4(STRING("sigspec5"),LBRACE,$2,RBRACE) }

sigspec_list_reversed: sigspec_list_reversed sigspec { TUPLE3(STRING("sigspec_list_reversed1"),$1,$2) }
	|	/* empty */ { EMPTY_TOKEN }

sigspec_list: sigspec_list_reversed { CONS1 ($1) }

conn_stmt: TOK_CONNECT sigspec sigspec EOL { TUPLE5(STRING("conn_stmt1"),TOK_CONNECT,$2,$3,$4) }


