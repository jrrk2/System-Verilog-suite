%{
  open Source_text_verible_types
  let stderr = open_out "parser_stderr.log" (* to capture parser trace mode info *)
  let declst = ref []
  let packhash_add id_t = Hashtbl.add packhash id_t ()
  let typehash_add id_t = Hashtbl.add typehash id_t ()
%}

%token  ACCEPT
%token  AMPERSAND
%token  AMPERSAND_AMPERSAND
%token  AMPERSAND_AMPERSAND_AMPERSAND
%token  AMPERSAND_EQ
%token  AT
%token  AT_AT
%token  Absdelay
%token  Abstol
%token  Ac_stim
%token  Accept_on
%token  Access
%token  Alias
%token  Aliasparam
%token  Always
%token  Always_comb
%token  Always_ff
%token  Always_latch
%token  Analog
%token  Analysis
%token  And
%token  Assert
%token  Assign
%token  Assume
%token  Automatic
%token  BACKQUOTE
%token  BACKQUOTE_BACKQUOTE
%token  BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____
%token  BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____
%token  BACKQUOTE_begin_keywords
%token  BACKQUOTE_celldefine
%token  BACKQUOTE_default_decay_time
%token  BACKQUOTE_default_nettype
%token  BACKQUOTE_default_trireg_strength
%token  BACKQUOTE_define
%token  BACKQUOTE_delay_mode_distributed
%token  BACKQUOTE_delay_mode_path
%token  BACKQUOTE_delay_mode_unit
%token  BACKQUOTE_delay_mode_zero
%token  BACKQUOTE_disable_portfaults
%token  BACKQUOTE_else
%token  BACKQUOTE_elsif
%token  BACKQUOTE_enable_portfaults
%token  BACKQUOTE_end_keywords
%token  BACKQUOTE_endcelldefine
%token  BACKQUOTE_endif
%token  BACKQUOTE_endprotect
%token  BACKQUOTE_ifdef
%token  BACKQUOTE_ifndef
%token  BACKQUOTE_include
%token  BACKQUOTE_nosuppress_faults
%token  BACKQUOTE_nounconnected_drive
%token  BACKQUOTE_pragma
%token  BACKQUOTE_protect
%token  BACKQUOTE_resetall
%token  BACKQUOTE_suppress_faults
%token  BACKQUOTE_timescale
%token  BACKQUOTE_unconnected_drive
%token  BACKQUOTE_undef
%token  BACKQUOTE_uselib
%token  BACKSLASH
%token  Before
%token  Begin
%token  Bind
%token  Bins
%token  Binsof
%token  Bit
%token  Break
%token  Buf
%token  Bufif0
%token  Bufif1
%token  Byte
%token  CARET
%token  CARET_EQ
%token  CHAR
%token  COLON
%token  COLON_COLON
%token  COLON_EQ
%token  COLON_SLASH
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
%token  Case
%token  Casex
%token  Casez
%token  Cell
%token  Chandle
%token  Checker
%token  Class
%token  Clocking
%token  Cmos
%token  Config
%token  Connect
%token  Connectmodule
%token  Connectrules
%token  Const
%token  Constraint
%token  Context
%token  Continue
%token  Continuous
%token  Cover
%token  Covergroup
%token  Coverpoint
%token  Cross
%token  DEFAULT
%token  DLR_attribute
%token  DLR_fullskew
%token  DLR_hold
%token  DLR_nochange
%token  DLR_period
%token  DLR_recovery
%token  DLR_recrem
%token  DLR_removal
%token  DLR_root
%token  DLR_setup
%token  DLR_setuphold
%token  DLR_skew
%token  DLR_timeskew
%token  DLR_unit
%token  DLR_width
%token  DOLLAR
%token  DOT
%token  DOT_STAR
%token  DOUBLEQUOTE
%token  Ddt_nature
%token  Deassign
%token  Default
%token  Defparam
%token  Design
%token  Disable
%token  Discipline
%token  Discrete
%token  Dist
%token  Do
%token  Domain
%token  Driver_update
%token <token list> ELIST
%token  EMPTY_TOKEN
%token  EOF_TOKEN
%token  EQUALS
%token  EQ_EQ
%token  EQ_EQ_EQ
%token  EQ_EQ_QUERY
%token  EQ_GT
%token  ERROR_TOKEN
%token  Edge
%token  Else
%token  End
%token  End_of_file
%token  Endcase
%token  Endchecker
%token  Endclass
%token  Endclocking
%token  Endconfig
%token  Endconnectrules
%token  Enddiscipline
%token  Endfunction
%token  Endgenerate
%token  Endgroup
%token  Endinterface
%token  Endmodule
%token  Endnature
%token  Endpackage
%token  Endparamset
%token  Endprimitive
%token  Endprogram
%token  Endproperty
%token  Endsequence
%token  Endspecify
%token  Endtable
%token  Endtask
%token  Enum
%token  Error
%token  EscapedIdentifier
%token  Event
%token  Eventually
%token  Exclude
%token  Expect
%token  Export
%token  Extends
%token  Extern
%token  Final
%token  Find
%token  Find_first
%token  Find_first_index
%token  Find_index
%token  Find_last
%token  Find_last_index
%token  First_match
%token  Flicker_noise
%token  Flow
%token  For
%token  Force
%token  Foreach
%token  Forever
%token  Fork
%token  Forkjoin
%token  From
%token  Function
%token  GREATER
%token  GT_EQ
%token  GT_GT
%token  GT_GT_GT
%token  Generate
%token  Genvar
%token  Global
%token  HASH
%token  HASH_EQ_HASH
%token  HASH_HASH
%token  HASH_HYPHEN_HASH
%token  HYPHEN
%token  HYPHEN_COLON
%token  HYPHEN_EQ
%token  HYPHEN_GT
%token  HYPHEN_GT_GT
%token  HYPHEN_GT_LPAREN_constraint_HYPHEN_implies_RPAREN
%token  HYPHEN_GT_LPAREN_logical_HYPHEN_implies_RPAREN
%token  HYPHEN_GT_LPAREN_trigger_RPAREN
%token  HYPHEN_HYPHEN
%token  Highz0
%token  Highz1
%token  Idt_nature
%token  If
%token  Iff
%token  Ifnone
%token  Ignore_bins
%token  Illegal_bins
%token  Implements
%token  Implies
%token  Import
%token  Incdir
%token  Include
%token  Inf
%token  Infinite
%token  Initial
%token  Inout
%token  Input
%token  Inside
%token  Instance
%token  Int
%token  Integer
%token  Interconnect
%token  Interface
%token  Intersect
%token  Join
%token  Join_any
%token  Join_none
%token  LBRACE
%token  LBRACK
%token  LBRACK_EQ
%token  LBRACK_HYPHEN_GT
%token  LBRACK_PLUS_RBRACK
%token  LBRACK_STAR
%token  LBRACK_STAR_RBRACK
%token  LESS
%token  LINEFEED
%token  LPAREN
%token  LPAREN_STAR
%token  LPAREN_STAR_attribute_STAR_RPAREN
%token  LPAREN_timescale_unit_RPAREN
%token  LT_EQ
%token  LT_HYPHEN_GT
%token  LT_LT
%token  LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT
%token  LT_LT_BACKSLASH_BACKSLASH_line_HYPHEN_cont_GT_GT
%token  LT_LT_BACKSLASH_BACKSLASH_n_GT_GT
%token  LT_LT_default_HYPHEN_text_GT_GT
%token  LT_LT_filepath_GT_GT
%token  LT_LT_space_GT_GT
%token  LT_PLUS
%token  Laplace_nd
%token  Laplace_np
%token  Laplace_zd
%token  Laplace_zp
%token  Large
%token  Last_crossing
%token  Less_than_TK_else
%token  Let
%token  Liblist
%token  Library
%token  Limexp
%token  Local
%token  Local_COLON_COLON
%token  Localparam
%token  Logic
%token  Longint
%token  MacroArg
%token  MacroCallCloseToEndLine
%token  MacroCallId
%token  MacroIdItem
%token  MacroIdentifier
%token  MacroNumericWidth
%token  Macromodule
%token  Matches
%token  Max
%token  Medium
%token  Min
%token  Modport
%token  Module
%token  NUMBER_step
%token  Nand
%token  Nature
%token  Negedge
%token  Net_resolution
%token  Nettype
%token  New
%token  Nexttime
%token  Nmos
%token  Noise_table
%token  Nor
%token  Noshowcancelled
%token  Not
%token  Notif0
%token  Notif1
%token  Null
%token  Option
%token  Or
%token  Output
%token  PERCENT
%token  PERCENT_EQ
%token  PLING
%token  PLING_EQ
%token  PLING_EQ_EQ
%token  PLING_EQ_QUERY
%token  PLUS
%token  PLUS_COLON
%token  PLUS_EQ
%token  PLUS_PLUS
%token  PP_Identifier
%token  Package
%token  Packed
%token  Parameter
%token  Paramset
%token  Pmos
%token  Posedge
%token  Potential
%token  Pow
%token  Primitive
%token  Priority
%token  Product
%token  Program
%token  Property
%token  Protected
%token  Pull0
%token  Pull1
%token  Pulldown
%token  Pullup
%token  Pulsestyle_ondetect
%token  Pulsestyle_onevent
%token  Pure
%token  QUERY
%token  QUOTE
%token  QUOTE_LBRACE
%token  RBRACE
%token  RBRACK
%token  RPAREN
%token  Rand
%token  Randc
%token  Randcase
%token  Randomize
%token  Randsequence
%token  Rcmos
%token  Real
%token  Realtime
%token  Ref
%token  Reg
%token  Reject_on
%token  Release
%token  Repeat
%token  Resolveto
%token  Restrict
%token  Return
%token  Reverse
%token  Rnmos
%token  Rpmos
%token  Rsort
%token  Rtran
%token  Rtranif0
%token  Rtranif1
%token  SEMICOLON
%token  SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN
%token  SLASH
%token  SLASH_AMPERSAND_lowast_SEMICOLON_comment_AMPERSAND_lowast_SEMICOLON_SLASH
%token  SLASH_EQ
%token  SLASH_SLASH_end_of_line_comment
%token <string list> SLIST
%token  STAR
%token  STAR_EQ
%token  STAR_GT
%token  STAR_RPAREN
%token  STAR_STAR
%token <string> STRING
%token  S_always
%token  S_eventually
%token  S_nexttime
%token  S_until
%token  S_until_with
%token  Sample
%token  Scalared
%token  Sequence
%token  Shortint
%token  Shortreal
%token  Showcancelled
%token  Shuffle
%token  Signed
%token  Small
%token  Soft
%token  Solve
%token  Sort
%token  Specify
%token  Specparam
%token  Static
%token  String
%token  Strong
%token  Strong0
%token  Strong1
%token  Struct
%token  Sum
%token  Super
%token  Supply0
%token  Supply1
%token <string> SymbolIdentifier
%token  Sync_accept_on
%token  Sync_reject_on
%token <string> SystemTFIdentifier
%token  TILDE
%token  TILDE_AMPERSAND
%token  TILDE_CARET
%token  TILDE_VBAR
%token  TK_AngleBracketInclude
%token <string> TK_BinBase
%token <string> TK_BinDigits
%token <string> TK_DecBase
%token <string> TK_DecDigits
%token <string> TK_DecNumber
%token  TK_EvalStringLiteral
%token <string> TK_HexBase
%token <string> TK_HexDigits
%token  TK_LS_EQ
%token  TK_OTHER
%token <string> TK_OctBase
%token <string> TK_OctDigits
%token  TK_RSS_EQ
%token  TK_RS_EQ
%token <string> TK_RealTime
%token <string> TK_StringLiteral
%token  TK_TimeLiteral
%token <string> TK_UnBasedNumber
%token  TK_XZDigits
%token  TK_edge_descriptor
%token <token list> TLIST
%token <token*token*token*token*token*token*token*token*token*token> TUPLE10
%token <token*token*token*token*token*token*token*token*token*token*token> TUPLE11
%token <token*token*token*token*token*token*token*token*token*token*token*token> TUPLE12
%token <token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE13
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE14
%token <token*token*token*token*token*token*token*token*token*token*token*token*token*token*token> TUPLE15
%token <token*token> TUPLE2
%token <token*token*token> TUPLE3
%token <token*token*token*token> TUPLE4
%token <token*token*token*token*token> TUPLE5
%token <token*token*token*token*token*token> TUPLE6
%token <token*token*token*token*token*token*token> TUPLE7
%token <token*token*token*token*token*token*token*token> TUPLE8
%token <token*token*token*token*token*token*token*token*token> TUPLE9
%token  Table
%token  Tagged
%token  Task
%token  This
%token  Throughout
%token  Time
%token  Timeprecision
%token  Timeprecision_check
%token  Timeunit
%token  Timeunit_check
%token  Tran
%token  Tranif0
%token  Tranif1
%token  Transition
%token  Tri
%token  Tri0
%token  Tri1
%token  Triand
%token  Trior
%token  Trireg
%token  Type
%token  Type_option
%token  Typedef
%token  UNDERSCORE
%token  Union
%token  Unique
%token  Unique0
%token  Unique_index
%token  Units
%token  Unsigned
%token  Until
%token  Until_with
%token  Untyped
%token  Use
%token  Uwire
%token  VBAR
%token  VBAR_EQ
%token  VBAR_EQ_GT
%token  VBAR_HYPHEN_GT
%token  VBAR_VBAR
%token  Var
%token  Vectored
%token  Virtual
%token  Void
%token  Wait
%token  Wait_order
%token  Wand
%token  Weak
%token  Weak0
%token  Weak1
%token  While
%token  White_noise
%token  Wildcard
%token  Wire
%token  With
%token  With_LPAREN_covergroup_RPAREN
%token  Within
%token  Wone
%token  Wor
%token  Wreal
%token  Xnor
%token  Xor
%token  Zi_nd
%token  Zi_np
%token  Zi_zd
%token  Zi_zp
%type <token> ml_start
%start ml_start
%%


ml_start: source_text_verible End_of_file { TUPLE3(STRING("ml_start1"),$1,End_of_file) }

source_text_verible: description_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

GenericIdentifier: SymbolIdentifier { (SymbolIdentifier $1) }
	|	EscapedIdentifier { (EscapedIdentifier) }
	|	MacroIdentifier { (MacroIdentifier) }
	|	KeywordIdentifier { ($1) }

KeywordIdentifier: Access { (Access) }
	|	Exclude { (Exclude) }
	|	Flow { (Flow) }
	|	From { (From) }
	|	Discrete { (Discrete) }
	|	Sample { (Sample) }
	|	Infinite { (Infinite) }
	|	Continuous { (Continuous) }

preprocessor_directive: preprocessor_control_flow { ($1) }
	|	preprocessor_action { ($1) }

preprocessor_if_header: BACKQUOTE_ifdef PP_Identifier { TUPLE3(STRING("preprocessor_if_header1"),BACKQUOTE_ifdef,PP_Identifier) }
	|	BACKQUOTE_ifndef PP_Identifier { TUPLE3(STRING("preprocessor_if_header2"),BACKQUOTE_ifndef,PP_Identifier) }

preprocessor_elsif_header: BACKQUOTE_elsif PP_Identifier { TUPLE3(STRING("preprocessor_elsif_header1"),BACKQUOTE_elsif,PP_Identifier) }

preprocessor_control_flow: preprocessor_if_header { ($1) }
	|	preprocessor_elsif_header { ($1) }
	|	BACKQUOTE_else { (BACKQUOTE_else) }
	|	BACKQUOTE_endif { (BACKQUOTE_endif) }

preprocessor_action: BACKQUOTE_undef PP_Identifier { TUPLE3(STRING("preprocessor_action1"),BACKQUOTE_undef,PP_Identifier) }
	|	BACKQUOTE_include preprocess_include_argument { TUPLE3(STRING("preprocessor_action2"),BACKQUOTE_include,$2) }
	|	BACKQUOTE_define PP_Identifier LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT { TUPLE4(STRING("preprocessor_action3"),BACKQUOTE_define,PP_Identifier,LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT) }
	|	BACKQUOTE_define PP_Identifier LPAREN macro_formals_list_opt RPAREN LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT { TUPLE7(STRING("preprocessor_action4"),BACKQUOTE_define,PP_Identifier,LPAREN,$4,RPAREN,LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT) }

macro_formals_list_opt: macro_formals_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

macro_formals_list: macro_formals_list COMMA macro_formal_parameter { CONS3($1,COMMA,$3) }
	|	macro_formal_parameter { CONS1 ($1) }

macro_formal_parameter: PP_Identifier { (PP_Identifier) }
	|	PP_Identifier EQUALS LT_LT_default_HYPHEN_text_GT_GT { TUPLE4(STRING("macro_formal_parameter2"),PP_Identifier,EQUALS,LT_LT_default_HYPHEN_text_GT_GT) }

preprocess_include_argument: string_literal { ($1) }
	|	TK_AngleBracketInclude { (TK_AngleBracketInclude) }
	|	MacroIdentifier { (MacroIdentifier) }
	|	MacroGenericItem { ($1) }
	|	MacroCall { ($1) }

MacroGenericItem: MacroCallItem { ($1) }
	|	MacroIdItem { (MacroIdItem) }

MacroCallItem: MacroCallId LPAREN macro_args_opt MacroCallCloseToEndLine { TUPLE5(STRING("MacroCallItem1"),MacroCallId,LPAREN,$3,MacroCallCloseToEndLine) }

MacroCall: MacroCallId LPAREN macro_args_opt RPAREN { TUPLE5(STRING("MacroCall1"),MacroCallId,LPAREN,$3,RPAREN) }

macro_args_opt: macro_args_opt COMMA macro_arg_opt { TUPLE4(STRING("macro_args_opt1"),$1,COMMA,$3) }
	|	macro_arg_opt { ($1) }

macro_arg_opt: MacroArg { (MacroArg) }
	|	/* empty */ { EMPTY_TOKEN }

procedural_assertion_statement: concurrent_assertion_statement { ($1) }
	|	immediate_assertion_statement { ($1) }

assertion_item: concurrent_assertion_item { ($1) }
	|	deferred_immediate_assertion_item { ($1) }

assignment_pattern: QUOTE_LBRACE expression_list_proper RBRACE { TUPLE4(STRING("assignment_pattern1"),QUOTE_LBRACE,$2,RBRACE) }
	|	QUOTE_LBRACE structure_or_array_pattern_expression_list RBRACE { TUPLE4(STRING("assignment_pattern2"),QUOTE_LBRACE,$2,RBRACE) }
	|	QUOTE_LBRACE expression LBRACE expression_list_proper RBRACE RBRACE { TUPLE7(STRING("assignment_pattern3"),QUOTE_LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	QUOTE_LBRACE RBRACE { TUPLE3(STRING("assignment_pattern4"),QUOTE_LBRACE,RBRACE) }

assignment_pattern_expression: assignment_pattern { ($1) }
	|	data_type_base assignment_pattern { TUPLE3(STRING("assignment_pattern_expression2"),$1,$2) }
	|	reference assignment_pattern { TUPLE3(STRING("assignment_pattern_expression3"),$1,$2) }
	|	reference call_base assignment_pattern { TUPLE4(STRING("assignment_pattern_expression4"),$1,$2,$3) }

structure_or_array_pattern_expression_list: structure_or_array_pattern_expression_list COMMA structure_or_array_pattern_expression { CONS3($1,COMMA,$3) }
	|	structure_or_array_pattern_expression { CONS1 ($1) }

structure_or_array_pattern_expression: structure_or_array_pattern_key COLON expression { TUPLE4(STRING("structure_or_array_pattern_expression1"),$1,COLON,$3) }

structure_or_array_pattern_key: expression { ($1) }
	|	simple_type { ($1) }
	|	Default { (Default) }

simple_type: integer_atom_type { ($1) }
	|	integer_vector_type { ($1) }

block_identifier_opt: unqualified_id COLON { TUPLE3(STRING("block_identifier_opt1"),$1,COLON) }
	|	/* empty */ { EMPTY_TOKEN }

interface_class_declaration: Interface Class GenericIdentifier module_parameter_port_list_opt declaration_extends_list_opt SEMICOLON interface_class_item_list_opt Endclass label_opt { TUPLE10(STRING("interface_class_declaration1"),Interface,Class,$3,$4,$5,SEMICOLON,$7,Endclass,$9) }

declaration_extends_list_opt: declaration_extends_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

declaration_extends_list: Extends class_id { CONS2(Extends,$2) }
	|	declaration_extends_list COMMA class_id { CONS3($1,COMMA,$3) }

implements_interface_list_opt: implements_interface_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

implements_interface_list: implements_interface_list COMMA class_id { CONS3($1,COMMA,$3) }
	|	Implements class_id { CONS2(Implements,$2) }

interface_class_item_list_opt: interface_class_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

interface_class_item_list: interface_class_item_list interface_class_item { CONS2($1,$2) }
	|	interface_class_item { CONS1 ($1) }

interface_class_item: type_declaration { ($1) }
	|	any_param_declaration { ($1) }
	|	interface_class_method { ($1) }
	|	SEMICOLON { (SEMICOLON) }

interface_class_method: Pure Virtual method_prototype SEMICOLON { TUPLE5(STRING("interface_class_method1"),Pure,Virtual,$3,SEMICOLON) }

method_prototype: task_prototype { ($1) }
	|	function_prototype { ($1) }

class_declaration: TK_virtual_opt Class lifetime_opt GenericIdentifier module_parameter_port_list_opt class_declaration_extends_opt implements_interface_list_opt SEMICOLON class_items_opt Endclass label_opt { TUPLE12(STRING("class_declaration1"),$1,Class,$3,$4,$5,$6,$7,SEMICOLON,$9,Endclass,$11) }

class_constraint: constraint_prototype { ($1) }
	|	constraint_declaration { ($1) }

class_declaration_extends_opt: Extends class_id { TUPLE3(STRING("class_declaration_extends_opt1"),Extends,$2) }
	|	/* empty */ { EMPTY_TOKEN }

unqualified_id: GenericIdentifier parameter_value_opt { TUPLE3(STRING("unqualified_id1"),$1,$2) }

qualified_id: qualified_id COLON_COLON unqualified_id { TUPLE4(STRING("qualified_id1"),$1,COLON_COLON,$3) }
	|	unqualified_id COLON_COLON unqualified_id { TUPLE4(STRING("qualified_id2"),$1,COLON_COLON,$3) }
	|	DLR_unit COLON_COLON unqualified_id { TUPLE4(STRING("qualified_id3"),DLR_unit,COLON_COLON,$3) }
	|	qualified_id COLON_COLON New { TUPLE4(STRING("qualified_id4"),$1,COLON_COLON,New) }
	|	unqualified_id COLON_COLON New { TUPLE4(STRING("qualified_id5"),$1,COLON_COLON,New) }

class_id: qualified_id { ($1) }
	|	unqualified_id { ($1) }

class_items_opt: class_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

class_items: class_items class_item { TUPLE3(STRING("class_items1"),$1,$2) }
	|	class_item { ($1) }

class_constructor_prototype: Function New tf_port_list_paren_opt { TUPLE4(STRING("class_constructor_prototype1"),Function,New,$3) }

class_constructor: class_constructor_prototype SEMICOLON tf_item_or_statement_or_null_list_opt Endfunction endnew_opt { TUPLE6(STRING("class_constructor1"),$1,SEMICOLON,$3,Endfunction,$5) }

class_item: method_property_qualifier_list_not_starting_with_virtual class_constructor { TUPLE3(STRING("class_item1"),$1,$2) }
	|	class_constructor { ($1) }
	|	Virtual method_qualifier_list_opt class_constructor { TUPLE4(STRING("class_item3"),Virtual,$2,$3) }
	|	method_property_qualifier_list_not_starting_with_virtual const_opt var_opt data_type list_of_variable_decl_assignments SEMICOLON { TUPLE7(STRING("class_item4"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	data_type list_of_variable_decl_assignments SEMICOLON { TUPLE4(STRING("class_item5"),$1,$2,SEMICOLON) }
	|	Const class_item_qualifier_list_opt data_type list_of_variable_decl_assignments SEMICOLON { TUPLE6(STRING("class_item6"),Const,$2,$3,$4,SEMICOLON) }
	|	interface_data_declaration { ($1) }
	|	net_type_declaration { ($1) }
	|	package_import_declaration { ($1) }
	|	method_property_qualifier_list_not_starting_with_virtual task_declaration { TUPLE3(STRING("class_item10"),$1,$2) }
	|	task_declaration { ($1) }
	|	Virtual method_qualifier_list_opt task_declaration { TUPLE4(STRING("class_item12"),Virtual,$2,$3) }
	|	method_property_qualifier_list_not_starting_with_virtual function_declaration { TUPLE3(STRING("class_item13"),$1,$2) }
	|	function_declaration { ($1) }
	|	Virtual method_qualifier_list_opt function_declaration { TUPLE4(STRING("class_item15"),Virtual,$2,$3) }
	|	Pure Virtual class_item_qualifier_list_opt method_prototype SEMICOLON { TUPLE6(STRING("class_item16"),Pure,Virtual,$3,$4,SEMICOLON) }
	|	Extern method_qualifier_list_opt method_prototype SEMICOLON { TUPLE5(STRING("class_item17"),Extern,$2,$3,SEMICOLON) }
	|	Extern method_qualifier_list_opt class_constructor_prototype SEMICOLON { TUPLE5(STRING("class_item18"),Extern,$2,$3,SEMICOLON) }
	|	class_declaration { ($1) }
	|	interface_class_declaration { ($1) }
	|	class_constraint { ($1) }
	|	type_declaration { ($1) }
	|	any_param_declaration { ($1) }
	|	covergroup_declaration { ($1) }
	|	SEMICOLON { (SEMICOLON) }
	|	macro_call_or_item { ($1) }
	|	preprocessor_balanced_class_items { ($1) }
	|	preprocessor_action { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("class_item29"),ERROR_TOKEN,SEMICOLON) }
	|	ERROR_TOKEN Endfunction { TUPLE3(STRING("class_item30"),ERROR_TOKEN,Endfunction) }
	|	ERROR_TOKEN Endtask { TUPLE3(STRING("class_item31"),ERROR_TOKEN,Endtask) }
	|	ERROR_TOKEN Endgroup { TUPLE3(STRING("class_item32"),ERROR_TOKEN,Endgroup) }

interface_data_declaration: interface_type list_of_variable_decl_assignments SEMICOLON { TUPLE4(STRING("interface_data_declaration1"),$1,$2,SEMICOLON) }

preprocessor_balanced_class_items: preprocessor_if_header class_items_opt preprocessor_elsif_class_items_opt preprocessor_else_class_item_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_class_items1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_class_items_opt: preprocessor_elsif_class_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_class_items: preprocessor_elsif_class_items preprocessor_elsif_class_item { TUPLE3(STRING("preprocessor_elsif_class_items1"),$1,$2) }
	|	preprocessor_elsif_class_item { ($1) }

preprocessor_elsif_class_item: preprocessor_elsif_header class_items_opt { TUPLE3(STRING("preprocessor_elsif_class_item1"),$1,$2) }

preprocessor_else_class_item_opt: preprocessor_else_class_item { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_class_item: BACKQUOTE_else class_items_opt { TUPLE3(STRING("preprocessor_else_class_item1"),BACKQUOTE_else,$2) }

macro_call_or_item: MacroGenericItem { ($1) }
	|	MacroCall SEMICOLON { TUPLE3(STRING("macro_call_or_item2"),$1,SEMICOLON) }

class_item_qualifier: Static { (Static) }
	|	Protected { (Protected) }
	|	Local { (Local) }

class_item_qualifier_list_opt: class_item_qualifier_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

class_item_qualifier_list: class_item_qualifier_list class_item_qualifier { CONS2($1,$2) }
	|	class_item_qualifier { CONS1 ($1) }

class_new: New call_base { TUPLE3(STRING("class_new1"),New,$2) }
	|	New reference { TUPLE3(STRING("class_new2"),New,$2) }
	|	New { (New) }

action_block: statement_or_null { ($1) }
	|	statement_or_null Else statement_or_null { TUPLE4(STRING("action_block2"),$1,Else,$3) }
	|	Else statement_or_null { TUPLE3(STRING("action_block3"),Else,$2) }

concurrent_assertion_item: block_identifier_opt concurrent_assertion_statement { TUPLE3(STRING("concurrent_assertion_item1"),$1,$2) }

concurrent_assertion_statement: assert_property_statement { ($1) }
	|	assume_property_statement { ($1) }
	|	cover_property_statement { ($1) }
	|	cover_sequence_statement { ($1) }
	|	restrict_property_statement { ($1) }

assert_property_statement: Assert Property LPAREN property_spec RPAREN action_block { TUPLE7(STRING("assert_property_statement1"),Assert,Property,LPAREN,$4,RPAREN,$6) }

assume_property_statement: Assume Property LPAREN property_spec RPAREN action_block { TUPLE7(STRING("assume_property_statement1"),Assume,Property,LPAREN,$4,RPAREN,$6) }

cover_property_statement: Cover Property LPAREN property_spec RPAREN statement_or_null { TUPLE7(STRING("cover_property_statement1"),Cover,Property,LPAREN,$4,RPAREN,$6) }

expect_property_statement: Expect LPAREN property_spec RPAREN action_block { TUPLE6(STRING("expect_property_statement1"),Expect,LPAREN,$3,RPAREN,$5) }

cover_sequence_statement: Cover Sequence LPAREN sequence_spec RPAREN statement_or_null { TUPLE7(STRING("cover_sequence_statement1"),Cover,Sequence,LPAREN,$4,RPAREN,$6) }

restrict_property_statement: Restrict Property LPAREN property_spec RPAREN SEMICOLON { TUPLE7(STRING("restrict_property_statement1"),Restrict,Property,LPAREN,$4,RPAREN,SEMICOLON) }

deferred_immediate_assertion_item: block_identifier_opt deferred_immediate_assertion_statement { TUPLE3(STRING("deferred_immediate_assertion_item1"),$1,$2) }

immediate_assertion_statement: simple_immediate_assertion_statement { ($1) }
	|	deferred_immediate_assertion_statement { ($1) }

simple_immediate_assertion_statement: Assert LPAREN expression RPAREN action_block { TUPLE6(STRING("simple_immediate_assertion_statement1"),Assert,LPAREN,$3,RPAREN,$5) }
	|	Assume LPAREN expression RPAREN action_block { TUPLE6(STRING("simple_immediate_assertion_statement2"),Assume,LPAREN,$3,RPAREN,$5) }
	|	Cover LPAREN expression RPAREN statement_or_null { TUPLE6(STRING("simple_immediate_assertion_statement3"),Cover,LPAREN,$3,RPAREN,$5) }

deferred_immediate_assertion_statement: Assert final_or_zero LPAREN expression RPAREN action_block { TUPLE7(STRING("deferred_immediate_assertion_statement1"),Assert,$2,LPAREN,$4,RPAREN,$6) }
	|	Assume final_or_zero LPAREN expression RPAREN action_block { TUPLE7(STRING("deferred_immediate_assertion_statement2"),Assume,$2,LPAREN,$4,RPAREN,$6) }
	|	Cover final_or_zero LPAREN expression RPAREN statement_or_null { TUPLE7(STRING("deferred_immediate_assertion_statement3"),Cover,$2,LPAREN,$4,RPAREN,$6) }

final_or_zero: Final { (Final) }
	|	HASH TK_DecNumber { TUPLE3(STRING("final_or_zero2"),HASH,TK_DecNumber $2) }

constraint_block: LBRACE constraint_block_item_list_opt RBRACE { TUPLE4(STRING("constraint_block1"),LBRACE,$2,RBRACE) }

constraint_block_item: constraint_expression_no_preprocessor { ($1) }
	|	preprocessor_balanced_constraint_block_item { ($1) }

constraint_primary_list: constraint_primary_list COMMA constraint_primary { CONS3($1,COMMA,$3) }
	|	constraint_primary { CONS1 ($1) }

constraint_block_item_list: constraint_block_item_list constraint_block_item { CONS2($1,$2) }
	|	constraint_block_item { CONS1 ($1) }

constraint_block_item_list_opt: constraint_block_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_balanced_constraint_block_item: preprocessor_if_header constraint_block_item_list_opt preprocessor_elsif_constraint_block_items_opt preprocessor_else_constraint_block_item_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_constraint_block_item1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_constraint_block_items_opt: preprocessor_elsif_constraint_block_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_constraint_block_items: preprocessor_elsif_constraint_block_items preprocessor_elsif_constraint_block_item { TUPLE3(STRING("preprocessor_elsif_constraint_block_items1"),$1,$2) }
	|	preprocessor_elsif_constraint_block_item { ($1) }

preprocessor_elsif_constraint_block_item: preprocessor_elsif_header constraint_block_item_list_opt { TUPLE3(STRING("preprocessor_elsif_constraint_block_item1"),$1,$2) }

preprocessor_else_constraint_block_item_opt: preprocessor_else_constraint_block_item { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_constraint_block_item: BACKQUOTE_else constraint_block_item_list_opt { TUPLE3(STRING("preprocessor_else_constraint_block_item1"),BACKQUOTE_else,$2) }

constraint_declaration_package_item: Constraint class_id constraint_block { TUPLE4(STRING("constraint_declaration_package_item1"),Constraint,$2,$3) }

constraint_declaration: TK_static_opt Constraint GenericIdentifier constraint_block { TUPLE5(STRING("constraint_declaration1"),$1,Constraint,$3,$4) }

constraint_expression_no_preprocessor: Soft expression_or_dist SEMICOLON { TUPLE4(STRING("constraint_expression_no_preprocessor1"),Soft,$2,SEMICOLON) }
	|	expression_or_dist SEMICOLON { TUPLE3(STRING("constraint_expression_no_preprocessor2"),$1,SEMICOLON) }
	|	expression HYPHEN_GT_LPAREN_constraint_HYPHEN_implies_RPAREN constraint_set { TUPLE4(STRING("constraint_expression_no_preprocessor3"),$1,HYPHEN_GT_LPAREN_constraint_HYPHEN_implies_RPAREN,$3) }
	|	If LPAREN expression RPAREN constraint_set { TUPLE6(STRING("constraint_expression_no_preprocessor4"),If,LPAREN,$3,RPAREN,$5) }
	|	If LPAREN expression RPAREN constraint_set Else constraint_set { TUPLE8(STRING("constraint_expression_no_preprocessor5"),If,LPAREN,$3,RPAREN,$5,Else,$7) }
	|	Foreach LPAREN reference RPAREN constraint_set { TUPLE6(STRING("constraint_expression_no_preprocessor6"),Foreach,LPAREN,$3,RPAREN,$5) }
	|	uniqueness_constraint SEMICOLON { TUPLE3(STRING("constraint_expression_no_preprocessor7"),$1,SEMICOLON) }
	|	Disable Soft constraint_primary SEMICOLON { TUPLE5(STRING("constraint_expression_no_preprocessor8"),Disable,Soft,$3,SEMICOLON) }
	|	Solve constraint_primary_list Before constraint_primary_list SEMICOLON { TUPLE6(STRING("constraint_expression_no_preprocessor9"),Solve,$2,Before,$4,SEMICOLON) }

constraint_expression: constraint_expression_no_preprocessor { ($1) }
	|	preprocessor_balanced_constraint_expressions { ($1) }

constraint_primary: reference { ($1) }

uniqueness_constraint: Unique LBRACE open_range_list RBRACE { TUPLE5(STRING("uniqueness_constraint1"),Unique,LBRACE,$3,RBRACE) }

constraint_expression_list: constraint_expression_list constraint_expression { CONS2($1,$2) }
	|	constraint_expression { CONS1 ($1) }

constraint_expression_list_opt: constraint_expression_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_balanced_constraint_expressions: preprocessor_if_header constraint_expression_list_opt preprocessor_elsif_constraint_expressions_opt preprocessor_else_constraint_expression_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_constraint_expressions1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_constraint_expressions_opt: preprocessor_elsif_constraint_expressions { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_constraint_expressions: preprocessor_elsif_constraint_expressions preprocessor_elsif_constraint_expression { TUPLE3(STRING("preprocessor_elsif_constraint_expressions1"),$1,$2) }
	|	preprocessor_elsif_constraint_expression { ($1) }

preprocessor_elsif_constraint_expression: preprocessor_elsif_header constraint_expression_list_opt { TUPLE3(STRING("preprocessor_elsif_constraint_expression1"),$1,$2) }

preprocessor_else_constraint_expression_opt: preprocessor_else_constraint_expression { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_constraint_expression: BACKQUOTE_else constraint_expression_list_opt { TUPLE3(STRING("preprocessor_else_constraint_expression1"),BACKQUOTE_else,$2) }

constraint_prototype: TK_static_opt Constraint GenericIdentifier SEMICOLON { TUPLE5(STRING("constraint_prototype1"),$1,Constraint,$3,SEMICOLON) }

constraint_set: constraint_expression { ($1) }
	|	LBRACE constraint_expression_list RBRACE { TUPLE4(STRING("constraint_set2"),LBRACE,$2,RBRACE) }

const_opt: Const { (Const) }
	|	/* empty */ { EMPTY_TOKEN }

var_opt: Var { (Var) }
	|	/* empty */ { EMPTY_TOKEN }

data_declaration_base: data_type_or_implicit_basic_followed_by_id_and_dimensions_opt trailing_decl_assignment_opt COMMA list_of_variable_decl_assignments SEMICOLON { TUPLE6(STRING("data_declaration_base1"),$1,$2,COMMA,$4,SEMICOLON) }
	|	data_type_or_implicit_basic_followed_by_id_and_dimensions_opt trailing_decl_assignment_opt SEMICOLON { TUPLE4(STRING("data_declaration_base2"),$1,$2,SEMICOLON) }

data_declaration_modifiers_opt: const_opt var_opt lifetime_opt { TUPLE4(STRING("data_declaration_modifiers_opt1"),$1,$2,$3) }

data_declaration: data_declaration_modifiers_opt data_declaration_base { TUPLE3(STRING("data_declaration1"),$1,$2) }

data_type_primitive: data_type_primitive_scalar decl_dimensions_opt { TUPLE3(STRING("data_type_primitive1"),$1,$2) }

data_type_primitive_scalar: integer_vector_type signed_unsigned_opt { TUPLE3(STRING("data_type_primitive_scalar1"),$1,$2) }
	|	non_integer_type { ($1) }
	|	struct_data_type { ($1) }
	|	enum_data_type { ($1) }
	|	integer_atom_type signed_unsigned_opt { TUPLE3(STRING("data_type_primitive_scalar5"),$1,$2) }
	|	Chandle { (Chandle) }
	|	String { (String) }
	|	Event { (Event) }

data_type_base: data_type_primitive { ($1) }
	|	type_reference { ($1) }

type_reference: Type LPAREN expression RPAREN { TUPLE5(STRING("type_reference1"),Type,LPAREN,$3,RPAREN) }

data_type: data_type_base { ($1) }
	|	reference { ($1) }

interface_type: Virtual interface_opt GenericIdentifier parameter_value_opt { TUPLE5(STRING("interface_type1"),Virtual,$2,$3,$4) }
	|	Virtual interface_opt GenericIdentifier parameter_value_opt DOT member_name { TUPLE7(STRING("interface_type2"),Virtual,$2,$3,$4,DOT,$6) }

interface_opt: Interface { (Interface) }
	|	/* empty */ { EMPTY_TOKEN }

delay3_or_drive_opt: delay3 { ($1) }
	|	drive_strength { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

scope_or_if_res: COLON_COLON { (COLON_COLON) }
	|	DOT { (DOT) }

type_identifier_or_implicit_followed_by_id_and_dimensions_opt: GenericIdentifier delay3 decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE6(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt1"),$1,$2,$3,$4,$5) }
	|	GenericIdentifier drive_strength decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE6(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt2"),$1,$2,$3,$4,$5) }
	|	GenericIdentifier decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt3"),$1,$2,$3,$4) }
	|	GenericIdentifier scope_or_if_res GenericIdentifier delay3_or_drive_opt decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE8(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt4"),$1,$2,$3,$4,$5,$6,$7) }
	|	GenericIdentifier decl_dimensions_opt { TUPLE3(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt5"),$1,$2) }
	|	delay3 decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt6"),$1,$2,$3,$4) }
	|	drive_strength decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("type_identifier_or_implicit_followed_by_id_and_dimensions_opt7"),$1,$2,$3,$4) }

type_identifier_followed_by_id: unqualified_id decl_dimensions_opt GenericIdentifier { TUPLE4(STRING("type_identifier_followed_by_id1"),$1,$2,$3) }
	|	qualified_id decl_dimensions_opt GenericIdentifier { TUPLE4(STRING("type_identifier_followed_by_id2"),$1,$2,$3) }
	|	unqualified_id DOT member_name decl_dimensions_opt GenericIdentifier { TUPLE6(STRING("type_identifier_followed_by_id3"),$1,DOT,$3,$4,$5) }
	|	Interface DOT member_name GenericIdentifier { TUPLE5(STRING("type_identifier_followed_by_id4"),Interface,DOT,$3,$4) }
	|	Interface GenericIdentifier { TUPLE3(STRING("type_identifier_followed_by_id5"),Interface,$2) }

type_identifier_or_implicit_basic_followed_by_id: unqualified_id GenericIdentifier { TUPLE3(STRING("type_identifier_or_implicit_basic_followed_by_id1"),$1,$2) }
	|	qualified_id GenericIdentifier { TUPLE3(STRING("type_identifier_or_implicit_basic_followed_by_id2"),$1,$2) }
	|	unqualified_id { ($1) }
	|	unqualified_id DOT member_name GenericIdentifier { TUPLE5(STRING("type_identifier_or_implicit_basic_followed_by_id4"),$1,DOT,$3,$4) }
	|	Interface DOT member_name GenericIdentifier { TUPLE5(STRING("type_identifier_or_implicit_basic_followed_by_id5"),Interface,DOT,$3,$4) }
	|	Interface GenericIdentifier { TUPLE3(STRING("type_identifier_or_implicit_basic_followed_by_id6"),Interface,$2) }

type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt: qualified_id decl_dimensions_opt class_id decl_dimensions_opt { TUPLE5(STRING("type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt1"),$1,$2,$3,$4) }
	|	unqualified_id decl_dimensions_opt class_id decl_dimensions_opt { TUPLE5(STRING("type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2"),$1,$2,$3,$4) }
	|	unqualified_id DOT member_name decl_dimensions_opt class_id decl_dimensions_opt { TUPLE7(STRING("type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt3"),$1,DOT,$3,$4,$5,$6) }
	|	unqualified_id decl_dimensions_opt { TUPLE3(STRING("type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4"),$1,$2) }
	|	qualified_id decl_dimensions_opt { TUPLE3(STRING("type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt5"),$1,$2) }

data_type_or_implicit: decl_dimensions delay3_or_drive_opt { TUPLE3(STRING("data_type_or_implicit1"),$1,$2) }
	|	signing decl_dimensions_opt delay3_or_drive_opt { TUPLE4(STRING("data_type_or_implicit2"),$1,$2,$3) }
	|	GenericIdentifier decl_dimensions_opt delay3_or_drive_opt { TUPLE4(STRING("data_type_or_implicit3"),$1,$2,$3) }
	|	GenericIdentifier COLON_COLON GenericIdentifier decl_dimensions_opt delay3_or_drive_opt { TUPLE6(STRING("data_type_or_implicit4"),$1,COLON_COLON,$3,$4,$5) }

data_type_or_implicit_followed_by_id_and_dimensions_opt: data_type_primitive GenericIdentifier decl_dimensions_opt { TUPLE4(STRING("data_type_or_implicit_followed_by_id_and_dimensions_opt1"),$1,$2,$3) }
	|	type_identifier_or_implicit_followed_by_id_and_dimensions_opt { ($1) }
	|	signing decl_dimensions_opt delay3_or_drive_opt GenericIdentifier decl_dimensions_opt { TUPLE6(STRING("data_type_or_implicit_followed_by_id_and_dimensions_opt3"),$1,$2,$3,$4,$5) }
	|	decl_dimensions delay3_or_drive_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("data_type_or_implicit_followed_by_id_and_dimensions_opt4"),$1,$2,$3,$4) }
	|	Void GenericIdentifier decl_dimensions_opt { TUPLE4(STRING("data_type_or_implicit_followed_by_id_and_dimensions_opt5"),Void,$2,$3) }

data_type_or_implicit_basic_followed_by_id: data_type_primitive GenericIdentifier { TUPLE3(STRING("data_type_or_implicit_basic_followed_by_id1"),$1,$2) }
	|	type_identifier_or_implicit_basic_followed_by_id { ($1) }
	|	signing decl_dimensions_opt GenericIdentifier { TUPLE4(STRING("data_type_or_implicit_basic_followed_by_id3"),$1,$2,$3) }
	|	decl_dimensions GenericIdentifier { TUPLE3(STRING("data_type_or_implicit_basic_followed_by_id4"),$1,$2) }
	|	Void GenericIdentifier { TUPLE3(STRING("data_type_or_implicit_basic_followed_by_id5"),Void,$2) }

data_type_or_implicit_basic_followed_by_id_and_dimensions_opt: data_type_primitive class_id decl_dimensions_opt { TUPLE4(STRING("data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1"),$1,$2,$3) }
	|	type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt { ($1) }
	|	signing decl_dimensions_opt class_id decl_dimensions_opt { TUPLE5(STRING("data_type_or_implicit_basic_followed_by_id_and_dimensions_opt3"),$1,$2,$3,$4) }
	|	decl_dimensions class_id decl_dimensions_opt { TUPLE4(STRING("data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4"),$1,$2,$3) }
	|	Void class_id decl_dimensions_opt { TUPLE4(STRING("data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5"),Void,$2,$3) }

description: module_or_interface_declaration { ($1) }
	|	udp_primitive { ($1) }
	|	config_declaration { ($1) }
	|	nature_declaration { ($1) }
	|	package_declaration { ($1) }
	|	discipline_declaration { ($1) }
	|	package_item_no_pp { ($1) }
	|	DLR_attribute LPAREN GenericIdentifier COMMA TK_StringLiteral COMMA TK_StringLiteral RPAREN { TUPLE9(STRING("description8"),DLR_attribute,LPAREN,$3,COMMA,TK_StringLiteral $5,COMMA,TK_StringLiteral $7,RPAREN) }
	|	bind_directive { ($1) }
	|	preprocessor_balanced_description_items { ($1) }
	|	preprocessor_action { ($1) }
	|	library_source { ($1) }

description_list_opt: description_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

description_list: description { CONS1 ($1) }
	|	description_list description { CONS2($1,$2) }

library_source: BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____ library_description_list_opt BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____ { TUPLE4(STRING("library_source1"),BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____,$2,BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____) }

library_description_list_opt: library_description_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

library_description_list: library_description_list library_description { CONS2($1,$2) }
	|	library_description { CONS1 ($1) }

library_description: library_declaration { ($1) }
	|	include_statement { ($1) }
	|	config_declaration { ($1) }
	|	SEMICOLON { (SEMICOLON) }

preprocessor_balanced_description_items: preprocessor_if_header description_list_opt preprocessor_elsif_description_items_opt preprocessor_else_description_item_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_description_items1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_description_items_opt: preprocessor_elsif_description_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_description_items: preprocessor_elsif_description_items preprocessor_elsif_description_item { TUPLE3(STRING("preprocessor_elsif_description_items1"),$1,$2) }
	|	preprocessor_elsif_description_item { ($1) }

preprocessor_elsif_description_item: preprocessor_elsif_header description_list_opt { TUPLE3(STRING("preprocessor_elsif_description_item1"),$1,$2) }

preprocessor_else_description_item_opt: preprocessor_else_description_item { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_description_item: BACKQUOTE_else description_list_opt { TUPLE3(STRING("preprocessor_else_description_item1"),BACKQUOTE_else,$2) }

endnew_opt: COLON New { TUPLE3(STRING("endnew_opt1"),COLON,New) }
	|	/* empty */ { EMPTY_TOKEN }

dynamic_array_new: New LBRACK expression RBRACK { TUPLE5(STRING("dynamic_array_new1"),New,LBRACK,$3,RBRACK) }
	|	New LBRACK expression RBRACK LPAREN expression RPAREN { TUPLE8(STRING("dynamic_array_new2"),New,LBRACK,$3,RBRACK,LPAREN,$6,RPAREN) }

for_step_opt: for_step { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

for_step: for_step COMMA assignment_statement { TUPLE4(STRING("for_step1"),$1,COMMA,$3) }
	|	assignment_statement { ($1) }

assignment_statement: assignment_statement_no_expr { ($1) }
	|	inc_or_dec_expression { ($1) }

assignment_statement_no_expr: lpvalue EQUALS expression { TUPLE4(STRING("assignment_statement_no_expr1"),$1,EQUALS,$3) }
	|	assign_modify_statement { ($1) }

function_prototype: Function lifetime_opt function_return_type_and_id tf_port_list_paren_opt { TUPLE5(STRING("function_prototype1"),Function,$2,$3,$4) }

function_return_type_and_id: data_type_or_implicit_basic_followed_by_id_and_dimensions_opt { ($1) }
	|	interface_type class_id { TUPLE3(STRING("function_return_type_and_id2"),$1,$2) }

function_declaration: Function lifetime_opt function_return_type_and_id LPAREN tf_port_list_opt RPAREN SEMICOLON block_item_or_statement_or_null_list_opt Endfunction endfunction_label_opt { TUPLE11(STRING("function_declaration1"),Function,$2,$3,LPAREN,$5,RPAREN,SEMICOLON,$8,Endfunction,$10) }
	|	Function lifetime_opt function_return_type_and_id SEMICOLON function_item_list statement_or_null_list_opt Endfunction endfunction_label_opt { TUPLE9(STRING("function_declaration2"),Function,$2,$3,SEMICOLON,$5,$6,Endfunction,$8) }
	|	Function lifetime_opt function_return_type_and_id SEMICOLON statement_or_null_list_opt Endfunction endfunction_label_opt { TUPLE8(STRING("function_declaration3"),Function,$2,$3,SEMICOLON,$5,Endfunction,$7) }

endfunction_label_opt: label_opt { ($1) }
	|	COLON New { TUPLE3(STRING("endfunction_label_opt2"),COLON,New) }

implicit_class_handle: This { (This) }
	|	Super { (Super) }

inc_or_dec_expression: PLUS_PLUS lpvalue { TUPLE3(STRING("inc_or_dec_expression1"),PLUS_PLUS,$2) }
	|	lpvalue PLUS_PLUS { TUPLE3(STRING("inc_or_dec_expression2"),$1,PLUS_PLUS) }
	|	HYPHEN_HYPHEN lpvalue { TUPLE3(STRING("inc_or_dec_expression3"),HYPHEN_HYPHEN,$2) }
	|	lpvalue HYPHEN_HYPHEN { TUPLE3(STRING("inc_or_dec_expression4"),$1,HYPHEN_HYPHEN) }

integer_atom_type: Byte { (Byte) }
	|	Shortint { (Shortint) }
	|	Int { (Int) }
	|	Longint { (Longint) }
	|	Integer { (Integer) }
	|	Time { (Time) }

integer_vector_type: Reg { (Reg) }
	|	Bit { (Bit) }
	|	Logic { (Logic) }

join_keyword: Join { (Join) }
	|	Join_none { (Join_none) }
	|	Join_any { (Join_any) }

jump_statement: Break SEMICOLON { TUPLE3(STRING("jump_statement1"),Break,SEMICOLON) }
	|	Continue SEMICOLON { TUPLE3(STRING("jump_statement2"),Continue,SEMICOLON) }
	|	Return SEMICOLON { TUPLE3(STRING("jump_statement3"),Return,SEMICOLON) }
	|	Return expression SEMICOLON { TUPLE4(STRING("jump_statement4"),Return,$2,SEMICOLON) }

loop_statement: For LPAREN for_initialization_opt SEMICOLON expression_opt SEMICOLON for_step_opt RPAREN statement_or_null { TUPLE10(STRING("loop_statement1"),For,LPAREN,$3,SEMICOLON,$5,SEMICOLON,$7,RPAREN,$9) }
	|	Forever statement_or_null { TUPLE3(STRING("loop_statement2"),Forever,$2) }
	|	repeat_control statement_or_null { TUPLE3(STRING("loop_statement3"),$1,$2) }
	|	While LPAREN expression RPAREN statement_or_null { TUPLE6(STRING("loop_statement4"),While,LPAREN,$3,RPAREN,$5) }
	|	Do statement_or_null While LPAREN expression RPAREN SEMICOLON { TUPLE8(STRING("loop_statement5"),Do,$2,While,LPAREN,$5,RPAREN,SEMICOLON) }
	|	Foreach LPAREN reference RPAREN statement_or_null { TUPLE6(STRING("loop_statement6"),Foreach,LPAREN,$3,RPAREN,$5) }

for_initialization_opt: for_initialization { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

for_initialization: for_initialization COMMA for_init_decl_or_assign { TUPLE4(STRING("for_initialization1"),$1,COMMA,$3) }
	|	for_init_decl_or_assign { ($1) }

for_init_decl_or_assign: lpvalue EQUALS expression { TUPLE4(STRING("for_init_decl_or_assign1"),$1,EQUALS,$3) }
	|	data_type GenericIdentifier EQUALS expression { TUPLE5(STRING("for_init_decl_or_assign2"),$1,$2,EQUALS,$4) }
	|	Var data_type GenericIdentifier EQUALS expression { TUPLE6(STRING("for_init_decl_or_assign3"),Var,$2,$3,EQUALS,$5) }

list_of_variable_decl_assignments: variable_decl_assignment { CONS1 ($1) }
	|	list_of_variable_decl_assignments COMMA variable_decl_assignment { CONS3($1,COMMA,$3) }

variable_decl_assignment: GenericIdentifier decl_dimensions_opt trailing_decl_assignment_opt { TUPLE4(STRING("variable_decl_assignment1"),$1,$2,$3) }

trailing_decl_assignment_opt: trailing_decl_assignment { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

trailing_decl_assignment: EQUALS dynamic_array_new { TUPLE3(STRING("trailing_decl_assignment1"),EQUALS,$2) }
	|	EQUALS expression { TUPLE3(STRING("trailing_decl_assignment2"),EQUALS,$2) }
	|	EQUALS class_new { TUPLE3(STRING("trailing_decl_assignment3"),EQUALS,$2) }

method_qualifier_list_opt: method_qualifier_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

method_qualifier_list: method_qualifier_list method_qualifier { CONS2($1,$2) }
	|	method_qualifier { CONS1 ($1) }

method_property_qualifier_list_not_starting_with_virtual: method_property_qualifier_list_not_starting_with_virtual method_property_qualifier { TUPLE3(STRING("method_property_qualifier_list_not_starting_with_virtual1"),$1,$2) }
	|	property_qualifier { ($1) }

method_qualifier: Virtual { (Virtual) }
	|	Pure Virtual { TUPLE3(STRING("method_qualifier2"),Pure,Virtual) }
	|	class_item_qualifier { ($1) }

method_property_qualifier: Virtual { (Virtual) }
	|	class_item_qualifier { ($1) }
	|	random_qualifier { ($1) }

modport_declaration: Modport modport_item_list SEMICOLON { TUPLE4(STRING("modport_declaration1"),Modport,$2,SEMICOLON) }

modport_item_list: modport_item { CONS1 ($1) }
	|	modport_item_list COMMA modport_item { CONS3($1,COMMA,$3) }

modport_item: GenericIdentifier LPAREN modport_ports_list RPAREN { TUPLE5(STRING("modport_item1"),$1,LPAREN,$3,RPAREN) }

modport_ports_list: modport_simple_ports_declaration_last { CONS1 ($1) }
	|	modport_tf_ports_declaration_last { CONS1 ($1) }
	|	modport_clocking_declaration_last { CONS1 ($1) }

dpi_spec_string: TK_StringLiteral { (TK_StringLiteral $1) }

dpi_import_property_opt: dpi_import_property { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

dpi_import_property: Context { (Context) }
	|	Pure { (Pure) }

dpi_import_export: dpi_import_item { ($1) }
	|	dpi_export_item { ($1) }

dpi_export_item: Export dpi_spec_string GenericIdentifier EQUALS modport_tf_port SEMICOLON { TUPLE7(STRING("dpi_export_item1"),Export,$2,$3,EQUALS,$5,SEMICOLON) }
	|	Export dpi_spec_string modport_tf_port SEMICOLON { TUPLE5(STRING("dpi_export_item2"),Export,$2,$3,SEMICOLON) }

import_export: Export { (Export) }
	|	Import { (Import) }

dpi_import_item: Import dpi_spec_string dpi_import_property_opt GenericIdentifier EQUALS method_prototype SEMICOLON { TUPLE8(STRING("dpi_import_item1"),Import,$2,$3,$4,EQUALS,$6,SEMICOLON) }
	|	Import dpi_spec_string dpi_import_property_opt method_prototype SEMICOLON { TUPLE6(STRING("dpi_import_item2"),Import,$2,$3,$4,SEMICOLON) }

modport_ports_declaration_trailing_comma: modport_simple_ports_declaration_trailing_comma { ($1) }
	|	modport_tf_ports_declaration_trailing_comma { ($1) }
	|	modport_clocking_declaration_trailing_comma { ($1) }

modport_simple_ports_declaration_trailing_comma: modport_simple_ports_declaration_last COMMA { TUPLE3(STRING("modport_simple_ports_declaration_trailing_comma1"),$1,COMMA) }

modport_tf_ports_declaration_trailing_comma: modport_tf_ports_declaration_last COMMA { TUPLE3(STRING("modport_tf_ports_declaration_trailing_comma1"),$1,COMMA) }

modport_clocking_declaration_trailing_comma: modport_clocking_declaration_last COMMA { TUPLE3(STRING("modport_clocking_declaration_trailing_comma1"),$1,COMMA) }

modport_tf_ports_declaration_begin: import_export { ($1) }
	|	modport_ports_declaration_trailing_comma import_export { TUPLE3(STRING("modport_tf_ports_declaration_begin2"),$1,$2) }

modport_tf_ports_declaration_last: modport_tf_ports_declaration_begin modport_tf_port { TUPLE3(STRING("modport_tf_ports_declaration_last1"),$1,$2) }
	|	modport_tf_ports_declaration_trailing_comma modport_tf_port { TUPLE3(STRING("modport_tf_ports_declaration_last2"),$1,$2) }

modport_clocking_declaration_begin: Clocking { (Clocking) }
	|	modport_ports_declaration_trailing_comma Clocking { TUPLE3(STRING("modport_clocking_declaration_begin2"),$1,Clocking) }

modport_clocking_declaration_last: modport_clocking_declaration_begin GenericIdentifier { TUPLE3(STRING("modport_clocking_declaration_last1"),$1,$2) }

modport_simple_ports_declaration_begin: port_direction { ($1) }
	|	modport_ports_declaration_trailing_comma port_direction { TUPLE3(STRING("modport_simple_ports_declaration_begin2"),$1,$2) }

modport_simple_ports_declaration_last: modport_simple_ports_declaration_begin modport_simple_port { TUPLE3(STRING("modport_simple_ports_declaration_last1"),$1,$2) }
	|	modport_simple_ports_declaration_trailing_comma modport_simple_port { TUPLE3(STRING("modport_simple_ports_declaration_last2"),$1,$2) }

modport_simple_port: DOT member_name LPAREN expression RPAREN { TUPLE6(STRING("modport_simple_port1"),DOT,$2,LPAREN,$4,RPAREN) }
	|	GenericIdentifier { ($1) }

modport_tf_port: task_prototype { ($1) }
	|	function_prototype { ($1) }
	|	GenericIdentifier { ($1) }

non_integer_type: Real { (Real) }
	|	Realtime { (Realtime) }
	|	Shortreal { (Shortreal) }
	|	Wreal { (Wreal) }

macro_digits: MacroCall { ($1) }
	|	MacroIdentifier { (MacroIdentifier) }

based_number: dec_based_number { ($1) }
	|	bin_based_number { ($1) }
	|	oct_based_number { ($1) }
	|	hex_based_number { ($1) }

dec_based_number: TK_DecBase TK_DecDigits { TUPLE3(STRING("dec_based_number1"),TK_DecBase $1,TK_DecDigits $2) }
	|	TK_DecBase TK_XZDigits { TUPLE3(STRING("dec_based_number2"),TK_DecBase $1,TK_XZDigits) }
	|	TK_DecBase macro_digits { TUPLE3(STRING("dec_based_number3"),TK_DecBase $1,$2) }

bin_based_number: TK_BinBase TK_BinDigits { TUPLE3(STRING("bin_based_number1"),TK_BinBase $1,TK_BinDigits $2) }
	|	TK_BinBase macro_digits { TUPLE3(STRING("bin_based_number2"),TK_BinBase $1,$2) }

oct_based_number: TK_OctBase TK_OctDigits { TUPLE3(STRING("oct_based_number1"),TK_OctBase $1,TK_OctDigits $2) }
	|	TK_OctBase macro_digits { TUPLE3(STRING("oct_based_number2"),TK_OctBase $1,$2) }

hex_based_number: TK_HexBase TK_HexDigits { TUPLE3(STRING("hex_based_number1"),TK_HexBase $1,TK_HexDigits $2) }
	|	TK_HexBase macro_digits { TUPLE3(STRING("hex_based_number2"),TK_HexBase $1,$2) }

number: based_number { ($1) }
	|	TK_DecNumber { (TK_DecNumber $1) }
	|	constant_dec_number based_number { TUPLE3(STRING("number3"),$1,$2) }
	|	TK_UnBasedNumber { (TK_UnBasedNumber $1) }
	|	constant_dec_number TK_UnBasedNumber { TUPLE3(STRING("number5"),$1,TK_UnBasedNumber $2) }

constant_dec_number: TK_DecNumber { (TK_DecNumber $1) }
	|	MacroNumericWidth { (MacroNumericWidth) }

open_range_list: open_range_list COMMA value_range { CONS3($1,COMMA,$3) }
	|	value_range { CONS1 ($1) }

package_declaration: Package lifetime_opt GenericIdentifier SEMICOLON package_item_list_opt Endpackage label_opt { TUPLE8(STRING("package_declaration1"),Package,$2,$3,SEMICOLON,$5,Endpackage,$7) }

module_package_import_list_opt: package_import_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

package_import_list: package_import_declaration { CONS1 ($1) }
	|	package_import_list package_import_declaration { CONS2($1,$2) }

package_import_declaration: Import package_import_item_list SEMICOLON { TUPLE4(STRING("package_import_declaration1"),Import,$2,SEMICOLON) }

package_export_declaration: Export STAR COLON_COLON STAR SEMICOLON { TUPLE6(STRING("package_export_declaration1"),Export,STAR,COLON_COLON,STAR,SEMICOLON) }
	|	Export package_import_item_list SEMICOLON { TUPLE4(STRING("package_export_declaration2"),Export,$2,SEMICOLON) }

package_import_item: scope_prefix GenericIdentifier { TUPLE3(STRING("package_import_item1"),$1,$2) }
	|	scope_prefix STAR { TUPLE3(STRING("package_import_item2"),$1,STAR) }

package_import_item_list: package_import_item_list COMMA package_import_item { CONS3($1,COMMA,$3) }
	|	package_import_item { CONS1 ($1) }

package_item: package_item_no_pp { ($1) }
	|	preprocessor_balanced_package_items { ($1) }
	|	preprocessor_action { ($1) }

package_item_no_pp: package_or_generate_item_declaration { ($1) }
	|	timeunits_declaration { ($1) }
	|	type_declaration { ($1) }
	|	data_declaration { ($1) }
	|	net_type_declaration { ($1) }
	|	interface_data_declaration { ($1) }
	|	clocking_declaration { ($1) }
	|	let_declaration { ($1) }
	|	constraint_declaration_package_item { ($1) }
	|	package_import_declaration { ($1) }
	|	package_export_declaration { ($1) }
	|	timescale_directive { ($1) }
	|	misc_directive { ($1) }
	|	module_item_directive { ($1) }
	|	macro_call_or_item { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("package_item_no_pp16"),ERROR_TOKEN,SEMICOLON) }
	|	any_param_declaration { ($1) }
	|	Initial statement_item { TUPLE3(STRING("package_item_no_pp18"),Initial,$2) }

preprocessor_balanced_package_items: preprocessor_if_header package_item_list_opt preprocessor_elsif_package_items_opt preprocessor_else_package_item_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_package_items1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_package_items_opt: preprocessor_elsif_package_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_package_items: preprocessor_elsif_package_items preprocessor_elsif_package_item { TUPLE3(STRING("preprocessor_elsif_package_items1"),$1,$2) }
	|	preprocessor_elsif_package_item { ($1) }

preprocessor_elsif_package_item: preprocessor_elsif_header package_item_list_opt { TUPLE3(STRING("preprocessor_elsif_package_item1"),$1,$2) }

preprocessor_else_package_item_opt: preprocessor_else_package_item { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_package_item: BACKQUOTE_else package_item_list_opt { TUPLE3(STRING("preprocessor_else_package_item1"),BACKQUOTE_else,$2) }

package_item_list: package_item_list package_item { CONS2($1,$2) }
	|	package_item { CONS1 ($1) }

package_item_list_opt: package_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

misc_directive: BACKQUOTE_resetall { (BACKQUOTE_resetall) }
	|	BACKQUOTE_celldefine { (BACKQUOTE_celldefine) }
	|	BACKQUOTE_endcelldefine { (BACKQUOTE_endcelldefine) }
	|	BACKQUOTE_unconnected_drive pull01 { TUPLE3(STRING("misc_directive4"),BACKQUOTE_unconnected_drive,$2) }
	|	BACKQUOTE_nounconnected_drive { (BACKQUOTE_nounconnected_drive) }
	|	BACKQUOTE_default_nettype net_type_or_none { TUPLE3(STRING("misc_directive6"),BACKQUOTE_default_nettype,$2) }
	|	BACKQUOTE_suppress_faults { (BACKQUOTE_suppress_faults) }
	|	BACKQUOTE_nosuppress_faults { (BACKQUOTE_nosuppress_faults) }
	|	BACKQUOTE_enable_portfaults { (BACKQUOTE_enable_portfaults) }
	|	BACKQUOTE_disable_portfaults { (BACKQUOTE_disable_portfaults) }
	|	BACKQUOTE_delay_mode_distributed { (BACKQUOTE_delay_mode_distributed) }
	|	BACKQUOTE_delay_mode_path { (BACKQUOTE_delay_mode_path) }
	|	BACKQUOTE_delay_mode_unit { (BACKQUOTE_delay_mode_unit) }
	|	BACKQUOTE_delay_mode_zero { (BACKQUOTE_delay_mode_zero) }
	|	BACKQUOTE_default_decay_time decay_value_simple { TUPLE3(STRING("misc_directive15"),BACKQUOTE_default_decay_time,$2) }
	|	BACKQUOTE_default_trireg_strength TK_DecNumber { TUPLE3(STRING("misc_directive16"),BACKQUOTE_default_trireg_strength,TK_DecNumber $2) }
	|	BACKQUOTE_pragma { (BACKQUOTE_pragma) }
	|	BACKQUOTE_uselib { (BACKQUOTE_uselib) }
	|	BACKQUOTE_begin_keywords TK_StringLiteral { TUPLE3(STRING("misc_directive19"),BACKQUOTE_begin_keywords,TK_StringLiteral $2) }
	|	BACKQUOTE_end_keywords { (BACKQUOTE_end_keywords) }

net_type_or_none: net_type { ($1) }
	|	Trireg { (Trireg) }
	|	GenericIdentifier { ($1) }

module_item_directive: BACKQUOTE_protect { (BACKQUOTE_protect) }
	|	BACKQUOTE_endprotect { (BACKQUOTE_endprotect) }

port_direction: dir { ($1) }
	|	Ref { (Ref) }

tf_port_direction: port_direction { ($1) }
	|	Const Ref { TUPLE3(STRING("tf_port_direction2"),Const,Ref) }

tf_port_direction_opt: tf_port_direction { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

property_qualifier: class_item_qualifier { ($1) }
	|	random_qualifier { ($1) }

property_spec: event_control_opt property_spec_disable_iff_opt property_expr { TUPLE4(STRING("property_spec1"),$1,$2,$3) }

sequence_spec: event_control_opt property_spec_disable_iff_opt sequence_expr { TUPLE4(STRING("sequence_spec1"),$1,$2,$3) }

property_spec_disable_iff: Disable Iff LPAREN expression_or_dist RPAREN { TUPLE6(STRING("property_spec_disable_iff1"),Disable,Iff,LPAREN,$4,RPAREN) }

property_spec_disable_iff_opt: property_spec_disable_iff { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

random_qualifier_opt: random_qualifier { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

random_qualifier: Rand { (Rand) }
	|	Randc { (Randc) }

signing: Signed { (Signed) }
	|	Unsigned { (Unsigned) }

statement: statement_item { ($1) }
	|	unqualified_id COLON statement_item { TUPLE4(STRING("statement2"),$1,COLON,$3) }
	|	reference_or_call SEMICOLON { TUPLE3(STRING("statement3"),$1,SEMICOLON) }
	|	unqualified_id COLON reference_or_call SEMICOLON { TUPLE5(STRING("statement4"),$1,COLON,$3,SEMICOLON) }

statement_or_null: statement { ($1) }
	|	SEMICOLON { (SEMICOLON) }

block_item_or_statement_or_null: block_item_decl { ($1) }
	|	statement_item { ($1) }
	|	unqualified_id COLON statement_item { TUPLE4(STRING("block_item_or_statement_or_null3"),$1,COLON,$3) }
	|	unqualified_id COLON reference call_base { TUPLE5(STRING("block_item_or_statement_or_null4"),$1,COLON,$3,$4) }
	|	SEMICOLON { (SEMICOLON) }
	|	reference SEMICOLON { TUPLE3(STRING("block_item_or_statement_or_null6"),$1,SEMICOLON) }
	|	reference DOT builtin_array_method SEMICOLON { TUPLE5(STRING("block_item_or_statement_or_null7"),$1,DOT,$3,SEMICOLON) }

block_item_or_statement_or_null_list: block_item_or_statement_or_null_list block_item_or_statement_or_null { CONS2($1,$2) }
	|	block_item_or_statement_or_null { CONS1 ($1) }

block_item_or_statement_or_null_list_opt: block_item_or_statement_or_null_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

stream_expression: expression { ($1) }

stream_expression_list: stream_expression_list COMMA stream_expression { CONS3($1,COMMA,$3) }
	|	stream_expression { CONS1 ($1) }

stream_operator: LT_LT { (LT_LT) }
	|	GT_GT { (GT_GT) }

streaming_concatenation: LBRACE stream_operator slice_size_opt LBRACE stream_expression_list RBRACE RBRACE { TUPLE8(STRING("streaming_concatenation1"),LBRACE,$2,$3,LBRACE,$5,RBRACE,RBRACE) }
	|	LBRACE stream_operator slice_size MacroCall RBRACE { TUPLE6(STRING("streaming_concatenation2"),LBRACE,$2,$3,$4,RBRACE) }
	|	LBRACE stream_operator slice_size MacroIdentifier RBRACE { TUPLE6(STRING("streaming_concatenation3"),LBRACE,$2,$3,MacroIdentifier,RBRACE) }
	|	LBRACE stream_operator MacroCall RBRACE { TUPLE5(STRING("streaming_concatenation4"),LBRACE,$2,$3,RBRACE) }
	|	LBRACE stream_operator MacroIdentifier RBRACE { TUPLE5(STRING("streaming_concatenation5"),LBRACE,$2,MacroIdentifier,RBRACE) }

slice_size_opt: slice_size { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

slice_size: expr_primary_no_groups { ($1) }
	|	expr_primary_parens { ($1) }
	|	reference_or_call { ($1) }
	|	data_type_primitive { ($1) }

task_prototype: Task lifetime_opt GenericIdentifier tf_port_list_paren_opt { TUPLE5(STRING("task_prototype1"),Task,$2,$3,$4) }

task_declaration: Task lifetime_opt task_declaration_id tf_port_list_paren_opt SEMICOLON tf_item_or_statement_or_null_list_opt Endtask label_opt { TUPLE9(STRING("task_declaration1"),Task,$2,$3,$4,SEMICOLON,$6,Endtask,$8) }

task_declaration_id: GenericIdentifier scope_or_if_res GenericIdentifier { TUPLE4(STRING("task_declaration_id1"),$1,$2,$3) }
	|	GenericIdentifier { ($1) }

tf_port_declaration: tf_port_direction signed_unsigned_opt qualified_id decl_dimensions_opt list_of_tf_variable_identifiers SEMICOLON { TUPLE7(STRING("tf_port_declaration1"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	tf_port_direction signed_unsigned_opt unqualified_id decl_dimensions_opt list_of_tf_variable_identifiers SEMICOLON { TUPLE7(STRING("tf_port_declaration2"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	tf_port_direction signed_unsigned_opt decl_dimensions list_of_tf_variable_identifiers SEMICOLON { TUPLE6(STRING("tf_port_declaration3"),$1,$2,$3,$4,SEMICOLON) }
	|	tf_port_direction signed_unsigned_opt list_of_tf_variable_identifiers SEMICOLON { TUPLE5(STRING("tf_port_declaration4"),$1,$2,$3,SEMICOLON) }
	|	tf_port_direction data_type_primitive list_of_tf_variable_identifiers SEMICOLON { TUPLE5(STRING("tf_port_declaration5"),$1,$2,$3,SEMICOLON) }

list_of_tf_variable_identifiers: list_of_tf_variable_identifiers COMMA tf_variable_identifier { CONS3($1,COMMA,$3) }
	|	tf_variable_identifier_first { CONS1 ($1) }

tf_variable_identifier_first: unqualified_id decl_dimensions_opt trailing_assign_opt { TUPLE4(STRING("tf_variable_identifier_first1"),$1,$2,$3) }

tf_variable_identifier: GenericIdentifier decl_dimensions_opt trailing_assign_opt { TUPLE4(STRING("tf_variable_identifier1"),$1,$2,$3) }

tf_port_item: tf_port_direction_opt data_type_or_implicit_basic_followed_by_id_and_dimensions_opt tf_port_item_expr_opt { TUPLE4(STRING("tf_port_item1"),$1,$2,$3) }
	|	tf_port_direction_opt interface_type GenericIdentifier decl_dimensions_opt tf_port_item_expr_opt { TUPLE6(STRING("tf_port_item2"),$1,$2,$3,$4,$5) }

tf_port_item_expr_opt: EQUALS expression { TUPLE3(STRING("tf_port_item_expr_opt1"),EQUALS,$2) }
	|	/* empty */ { EMPTY_TOKEN }

tf_port_list: tf_port_list_item_last { CONS1 ($1) }
	|	tf_port_list_preprocessor_last { CONS1 ($1) }

tf_port_list_trailing_comma: tf_port_list COMMA { TUPLE3(STRING("tf_port_list_trailing_comma1"),$1,COMMA) }

tf_port_list_item_last: tf_port_list_trailing_comma tf_port_item { TUPLE3(STRING("tf_port_list_item_last1"),$1,$2) }
	|	tf_port_list_preprocessor_last tf_port_item { TUPLE3(STRING("tf_port_list_item_last2"),$1,$2) }
	|	tf_port_item { ($1) }

tf_port_list_preprocessor_last: tf_port_list preprocessor_directive { TUPLE3(STRING("tf_port_list_preprocessor_last1"),$1,$2) }
	|	tf_port_list_trailing_comma preprocessor_directive { TUPLE3(STRING("tf_port_list_preprocessor_last2"),$1,$2) }
	|	preprocessor_directive { ($1) }

timescale_directive: BACKQUOTE_timescale time_literal SLASH time_literal { TUPLE5(STRING("timescale_directive1"),BACKQUOTE_timescale,$2,SLASH,$4) }
	|	BACKQUOTE_timescale MacroGenericItem { TUPLE3(STRING("timescale_directive2"),BACKQUOTE_timescale,$2) }

time_literal: TK_TimeLiteral { (TK_TimeLiteral) }
	|	TK_DecNumber LPAREN_timescale_unit_RPAREN { TUPLE3(STRING("time_literal2"),TK_DecNumber $1,LPAREN_timescale_unit_RPAREN) }

timeunits_declaration: Timeunit TK_TimeLiteral SEMICOLON { TUPLE4(STRING("timeunits_declaration1"),Timeunit,TK_TimeLiteral,SEMICOLON) }
	|	Timeunit TK_TimeLiteral SLASH TK_TimeLiteral SEMICOLON { TUPLE6(STRING("timeunits_declaration2"),Timeunit,TK_TimeLiteral,SLASH,TK_TimeLiteral,SEMICOLON) }
	|	Timeprecision TK_TimeLiteral SEMICOLON { TUPLE4(STRING("timeunits_declaration3"),Timeprecision,TK_TimeLiteral,SEMICOLON) }
	|	Timeunit_check TK_TimeLiteral SEMICOLON { TUPLE4(STRING("timeunits_declaration4"),Timeunit_check,TK_TimeLiteral,SEMICOLON) }
	|	Timeunit_check TK_TimeLiteral SLASH TK_TimeLiteral SEMICOLON { TUPLE6(STRING("timeunits_declaration5"),Timeunit_check,TK_TimeLiteral,SLASH,TK_TimeLiteral,SEMICOLON) }
	|	Timeprecision_check TK_TimeLiteral SEMICOLON { TUPLE4(STRING("timeunits_declaration6"),Timeprecision_check,TK_TimeLiteral,SEMICOLON) }

value_range: expression { ($1) }
	|	LBRACK expression COLON expression RBRACK { TUPLE6(STRING("value_range2"),LBRACK,$2,COLON,$4,RBRACK) }

select_variable_dimension: LBRACK expression COLON expression RBRACK { TUPLE6(STRING("select_variable_dimension1"),LBRACK,$2,COLON,$4,RBRACK) }
	|	LBRACK expression_or_null_list_opt RBRACK { TUPLE4(STRING("select_variable_dimension2"),LBRACK,$2,RBRACK) }
	|	LBRACK expression PLUS_COLON expression RBRACK { TUPLE6(STRING("select_variable_dimension3"),LBRACK,$2,PLUS_COLON,$4,RBRACK) }
	|	LBRACK expression HYPHEN_COLON expression RBRACK { TUPLE6(STRING("select_variable_dimension4"),LBRACK,$2,HYPHEN_COLON,$4,RBRACK) }

decl_variable_dimension: LBRACK expression COLON expression RBRACK { TUPLE6(STRING("decl_variable_dimension1"),LBRACK,$2,COLON,$4,RBRACK) }
	|	LBRACK expression_or_null_list_opt RBRACK { TUPLE4(STRING("decl_variable_dimension2"),LBRACK,$2,RBRACK) }
	|	LBRACK expression PLUS_COLON expression RBRACK { TUPLE6(STRING("decl_variable_dimension3"),LBRACK,$2,PLUS_COLON,$4,RBRACK) }
	|	LBRACK expression HYPHEN_COLON expression RBRACK { TUPLE6(STRING("decl_variable_dimension4"),LBRACK,$2,HYPHEN_COLON,$4,RBRACK) }
	|	LBRACK data_type_primitive RBRACK { TUPLE4(STRING("decl_variable_dimension5"),LBRACK,$2,RBRACK) }
	|	lb_star_rb { ($1) }

lb_star_rb: LBRACK STAR RBRACK { TUPLE4(STRING("lb_star_rb1"),LBRACK,STAR,RBRACK) }
	|	LBRACK_STAR_RBRACK { (LBRACK_STAR_RBRACK) }
	|	LBRACK_STAR RBRACK { TUPLE3(STRING("lb_star_rb3"),LBRACK_STAR,RBRACK) }

any_param_declaration: Parameter param_type_followed_by_id_and_dimensions_opt trailing_assign COMMA parameter_assign_list SEMICOLON { TUPLE7(STRING("any_param_declaration1"),Parameter,$2,$3,COMMA,$5,SEMICOLON) }
	|	Parameter param_type_followed_by_id_and_dimensions_opt trailing_assign SEMICOLON { TUPLE5(STRING("any_param_declaration2"),Parameter,$2,$3,SEMICOLON) }
	|	Localparam param_type_followed_by_id_and_dimensions_opt trailing_assign COMMA localparam_assign_list SEMICOLON { TUPLE7(STRING("any_param_declaration3"),Localparam,$2,$3,COMMA,$5,SEMICOLON) }
	|	Localparam param_type_followed_by_id_and_dimensions_opt trailing_assign SEMICOLON { TUPLE5(STRING("any_param_declaration4"),Localparam,$2,$3,SEMICOLON) }
	|	Parameter Type type_assignment_list SEMICOLON { TUPLE5(STRING("any_param_declaration5"),Parameter,Type,$3,SEMICOLON) }
	|	Localparam Type type_assignment_list SEMICOLON { TUPLE5(STRING("any_param_declaration6"),Localparam,Type,$3,SEMICOLON) }

instantiation_type: data_type { ($1) }
	|	interface_type { ($1) }

instantiation_base: instantiation_type non_anonymous_gate_instance_or_register_variable_list { TUPLE3(STRING("instantiation_base1"),$1,$2) }
	|	reference_or_call_base { ($1) }

data_declaration_or_module_instantiation: instantiation_base SEMICOLON { TUPLE3(STRING("data_declaration_or_module_instantiation1"),$1,SEMICOLON) }
	|	lifetime const_opt instantiation_base SEMICOLON { TUPLE5(STRING("data_declaration_or_module_instantiation2"),$1,$2,$3,SEMICOLON) }
	|	Var lifetime_opt instantiation_base SEMICOLON { TUPLE5(STRING("data_declaration_or_module_instantiation3"),Var,$2,$3,SEMICOLON) }
	|	Const var_opt lifetime_opt instantiation_base SEMICOLON { TUPLE6(STRING("data_declaration_or_module_instantiation4"),Const,$2,$3,$4,SEMICOLON) }

non_anonymous_gate_instance_or_register_variable: GenericIdentifier decl_dimensions_opt trailing_decl_assignment_opt { TUPLE4(STRING("non_anonymous_gate_instance_or_register_variable1"),$1,$2,$3) }
	|	GenericIdentifier decl_dimensions_opt LPAREN any_port_list_opt RPAREN { TUPLE6(STRING("non_anonymous_gate_instance_or_register_variable2"),$1,$2,LPAREN,$4,RPAREN) }
	|	MacroCall { ($1) }

non_anonymous_gate_instance_or_register_variable_list: non_anonymous_gate_instance_or_register_variable_list COMMA gate_instance_or_register_variable { CONS3($1,COMMA,$3) }
	|	non_anonymous_gate_instance_or_register_variable { CONS1 ($1) }

non_anonymous_instantiation_base: instantiation_type non_anonymous_gate_instance_or_register_variable_list { TUPLE3(STRING("non_anonymous_instantiation_base1"),$1,$2) }

function_item_data_declaration: non_anonymous_instantiation_base SEMICOLON { TUPLE3(STRING("function_item_data_declaration1"),$1,SEMICOLON) }
	|	lifetime const_opt instantiation_base SEMICOLON { TUPLE5(STRING("function_item_data_declaration2"),$1,$2,$3,SEMICOLON) }
	|	Var lifetime_opt instantiation_base SEMICOLON { TUPLE5(STRING("function_item_data_declaration3"),Var,$2,$3,SEMICOLON) }
	|	Const var_opt lifetime_opt instantiation_base SEMICOLON { TUPLE6(STRING("function_item_data_declaration4"),Const,$2,$3,$4,SEMICOLON) }

net_type_declaration: Nettype data_type unqualified_id SEMICOLON { TUPLE5(STRING("net_type_declaration1"),Nettype,$2,$3,SEMICOLON) }
	|	Nettype data_type unqualified_id With class_id SEMICOLON { TUPLE7(STRING("net_type_declaration2"),Nettype,$2,$3,With,$5,SEMICOLON) }

block_item_decl: data_declaration_or_module_instantiation { ($1) }
	|	net_type_declaration { ($1) }
	|	package_import_declaration { ($1) }
	|	any_param_declaration { ($1) }
	|	type_declaration { ($1) }
	|	let_declaration { ($1) }

type_declaration: Typedef data_type GenericIdentifier decl_dimensions_opt SEMICOLON { TUPLE6(STRING("type_declaration1"),Typedef,$2,$3,$4,SEMICOLON) }
	|	Typedef Class GenericIdentifier SEMICOLON { TUPLE5(STRING("type_declaration2"),Typedef,Class,$3,SEMICOLON) }
	|	Typedef Interface Class GenericIdentifier SEMICOLON { TUPLE6(STRING("type_declaration3"),Typedef,Interface,Class,$4,SEMICOLON) }
	|	Typedef interface_type GenericIdentifier SEMICOLON { TUPLE5(STRING("type_declaration4"),Typedef,$2,$3,SEMICOLON) }
	|	Typedef Enum GenericIdentifier SEMICOLON { TUPLE5(STRING("type_declaration5"),Typedef,Enum,$3,SEMICOLON) }
	|	Typedef Struct GenericIdentifier SEMICOLON { TUPLE5(STRING("type_declaration6"),Typedef,Struct,$3,SEMICOLON) }
	|	Typedef Union GenericIdentifier SEMICOLON { TUPLE5(STRING("type_declaration7"),Typedef,Union,$3,SEMICOLON) }
	|	Typedef GenericIdentifier SEMICOLON { TUPLE4(STRING("type_declaration8"),Typedef,$2,SEMICOLON) }

enum_data_type: Enum LBRACE enum_name_list RBRACE { TUPLE5(STRING("enum_data_type1"),Enum,LBRACE,$3,RBRACE) }
	|	Enum data_type LBRACE enum_name_list RBRACE { TUPLE6(STRING("enum_data_type2"),Enum,$2,LBRACE,$4,RBRACE) }

enum_name_list: enum_name_list_preprocessor_last { CONS1 ($1) }
	|	enum_name_list_item_last { CONS1 ($1) }

enum_name_list_trailing_comma: enum_name_list COMMA { TUPLE3(STRING("enum_name_list_trailing_comma1"),$1,COMMA) }

enum_name_list_preprocessor_last: enum_name_list preprocessor_directive { TUPLE3(STRING("enum_name_list_preprocessor_last1"),$1,$2) }
	|	enum_name_list_trailing_comma preprocessor_directive { TUPLE3(STRING("enum_name_list_preprocessor_last2"),$1,$2) }
	|	preprocessor_directive { ($1) }

enum_name_list_item_last: enum_name_list_trailing_comma enum_name { TUPLE3(STRING("enum_name_list_item_last1"),$1,$2) }
	|	enum_name_list_preprocessor_last enum_name { TUPLE3(STRING("enum_name_list_item_last2"),$1,$2) }
	|	enum_name { ($1) }

pos_neg_number: number { ($1) }
	|	HYPHEN number { TUPLE3(STRING("pos_neg_number2"),HYPHEN,$2) }

enum_name: GenericIdentifier { ($1) }
	|	GenericIdentifier LBRACK pos_neg_number RBRACK { TUPLE5(STRING("enum_name2"),$1,LBRACK,$3,RBRACK) }
	|	GenericIdentifier LBRACK pos_neg_number COLON pos_neg_number RBRACK { TUPLE7(STRING("enum_name3"),$1,LBRACK,$3,COLON,$5,RBRACK) }
	|	GenericIdentifier EQUALS expression { TUPLE4(STRING("enum_name4"),$1,EQUALS,$3) }
	|	GenericIdentifier LBRACK pos_neg_number RBRACK EQUALS expression { TUPLE7(STRING("enum_name5"),$1,LBRACK,$3,RBRACK,EQUALS,$6) }
	|	GenericIdentifier LBRACK pos_neg_number COLON pos_neg_number RBRACK EQUALS expression { TUPLE9(STRING("enum_name6"),$1,LBRACK,$3,COLON,$5,RBRACK,EQUALS,$8) }

struct_data_type: Struct packed_signing_opt LBRACE struct_union_member_list RBRACE { TUPLE6(STRING("struct_data_type1"),Struct,$2,LBRACE,$4,RBRACE) }
	|	Union TK_tagged_opt packed_signing_opt LBRACE struct_union_member_list RBRACE { TUPLE7(STRING("struct_data_type2"),Union,$2,$3,LBRACE,$5,RBRACE) }

packed_signing_opt: Packed signed_unsigned_opt { TUPLE3(STRING("packed_signing_opt1"),Packed,$2) }
	|	/* empty */ { EMPTY_TOKEN }

struct_union_member_list: struct_union_member_list struct_union_member { CONS2($1,$2) }
	|	struct_union_member { CONS1 ($1) }

struct_union_member: random_qualifier_opt data_type_or_implicit_followed_by_id_and_dimensions_opt trailing_assign_opt SEMICOLON { TUPLE5(STRING("struct_union_member1"),$1,$2,$3,SEMICOLON) }
	|	random_qualifier_opt data_type_or_implicit_followed_by_id_and_dimensions_opt trailing_assign_opt COMMA list_of_variable_decl_assignments SEMICOLON { TUPLE7(STRING("struct_union_member2"),$1,$2,$3,COMMA,$5,SEMICOLON) }
	|	preprocessor_directive { ($1) }
	|	MacroCall SEMICOLON { TUPLE3(STRING("struct_union_member4"),$1,SEMICOLON) }
	|	MacroCall { ($1) }
	|	MacroCallId LPAREN macro_args_opt MacroCallCloseToEndLine { TUPLE5(STRING("struct_union_member6"),MacroCallId,LPAREN,$3,MacroCallCloseToEndLine) }

case_item: expression_list_proper COLON statement_or_null { TUPLE4(STRING("case_item1"),$1,COLON,$3) }
	|	Default COLON statement_or_null { TUPLE4(STRING("case_item2"),Default,COLON,$3) }
	|	Default statement_or_null { TUPLE3(STRING("case_item3"),Default,$2) }
	|	preprocessor_directive { ($1) }

case_inside_item: open_range_list COLON statement_or_null { TUPLE4(STRING("case_inside_item1"),$1,COLON,$3) }
	|	Default COLON statement_or_null { TUPLE4(STRING("case_inside_item2"),Default,COLON,$3) }
	|	Default statement_or_null { TUPLE3(STRING("case_inside_item3"),Default,$2) }
	|	preprocessor_directive { ($1) }

case_pattern_item: pattern AMPERSAND_AMPERSAND_AMPERSAND expression COLON statement_or_null { TUPLE6(STRING("case_pattern_item1"),$1,AMPERSAND_AMPERSAND_AMPERSAND,$3,COLON,$5) }
	|	pattern COLON statement_or_null { TUPLE4(STRING("case_pattern_item2"),$1,COLON,$3) }
	|	Default COLON statement_or_null { TUPLE4(STRING("case_pattern_item3"),Default,COLON,$3) }
	|	Default statement_or_null { TUPLE3(STRING("case_pattern_item4"),Default,$2) }
	|	preprocessor_directive { ($1) }

case_items: case_items case_item { TUPLE3(STRING("case_items1"),$1,$2) }
	|	case_item { ($1) }

case_inside_items: case_inside_items case_inside_item { TUPLE3(STRING("case_inside_items1"),$1,$2) }
	|	case_inside_item { ($1) }

case_pattern_items: case_pattern_items case_pattern_item { TUPLE3(STRING("case_pattern_items1"),$1,$2) }
	|	case_pattern_item { ($1) }

charge_strength: LPAREN Small RPAREN { TUPLE4(STRING("charge_strength1"),LPAREN,Small,RPAREN) }
	|	LPAREN Medium RPAREN { TUPLE4(STRING("charge_strength2"),LPAREN,Medium,RPAREN) }
	|	LPAREN Large RPAREN { TUPLE4(STRING("charge_strength3"),LPAREN,Large,RPAREN) }

charge_strength_opt: charge_strength { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

defparam_assign: reference EQUALS expression { TUPLE4(STRING("defparam_assign1"),$1,EQUALS,$3) }

defparam_assign_list: defparam_assign { CONS1 ($1) }
	|	decl_dimensions defparam_assign { CONS2($1,$2) }
	|	defparam_assign_list COMMA defparam_assign { CONS3($1,COMMA,$3) }

delay1: HASH delay_value_simple { TUPLE3(STRING("delay11"),HASH,$2) }
	|	HASH LPAREN delay_value RPAREN { TUPLE5(STRING("delay12"),HASH,LPAREN,$3,RPAREN) }

delay3: HASH delay_value_simple { TUPLE3(STRING("delay31"),HASH,$2) }
	|	HASH LPAREN delay_value RPAREN { TUPLE5(STRING("delay32"),HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN delay_value COMMA delay_value RPAREN { TUPLE7(STRING("delay33"),HASH,LPAREN,$3,COMMA,$5,RPAREN) }
	|	HASH LPAREN delay_value COMMA delay_value COMMA delay_value RPAREN { TUPLE9(STRING("delay34"),HASH,LPAREN,$3,COMMA,$5,COMMA,$7,RPAREN) }

delay3_opt: delay3 { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

delay_value_list: delay_value { CONS1 ($1) }
	|	delay_value_list COMMA delay_value { CONS3($1,COMMA,$3) }

delay_value: expression { ($1) }
	|	expression COLON expression COLON expression { TUPLE6(STRING("delay_value2"),$1,COLON,$3,COLON,$5) }

delay_value_simple: TK_DecNumber { (TK_DecNumber $1) }
	|	TK_RealTime { (TK_RealTime $1) }
	|	delay_identifier { ($1) }
	|	TK_TimeLiteral { (TK_TimeLiteral) }
	|	NUMBER_step { (NUMBER_step) }

delay_identifier: delay_identifier DOT GenericIdentifier { TUPLE4(STRING("delay_identifier1"),$1,DOT,$3) }
	|	delay_scope { ($1) }

delay_scope: delay_scope COLON_COLON GenericIdentifier { TUPLE4(STRING("delay_scope1"),$1,COLON_COLON,$3) }
	|	GenericIdentifier { ($1) }

decay_value_simple: TK_DecNumber { (TK_DecNumber $1) }
	|	TK_RealTime { (TK_RealTime $1) }
	|	TK_TimeLiteral { (TK_TimeLiteral) }
	|	Infinite { (Infinite) }

optional_semicolon: SEMICOLON { (SEMICOLON) }
	|	/* empty */ { EMPTY_TOKEN }

discipline_declaration: Discipline GenericIdentifier optional_semicolon discipline_items_opt Enddiscipline { TUPLE6(STRING("discipline_declaration1"),Discipline,$2,$3,$4,Enddiscipline) }

discipline_items_opt: discipline_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

discipline_items: discipline_items discipline_item { TUPLE3(STRING("discipline_items1"),$1,$2) }
	|	discipline_item { ($1) }

discipline_item: Domain Discrete SEMICOLON { TUPLE4(STRING("discipline_item1"),Domain,Discrete,SEMICOLON) }
	|	Domain Continuous SEMICOLON { TUPLE4(STRING("discipline_item2"),Domain,Continuous,SEMICOLON) }
	|	Potential GenericIdentifier SEMICOLON { TUPLE4(STRING("discipline_item3"),Potential,$2,SEMICOLON) }
	|	Flow GenericIdentifier SEMICOLON { TUPLE4(STRING("discipline_item4"),Flow,$2,SEMICOLON) }

nature_declaration: Nature GenericIdentifier optional_semicolon nature_items Endnature { TUPLE6(STRING("nature_declaration1"),Nature,$2,$3,$4,Endnature) }

nature_items: nature_items nature_item { TUPLE3(STRING("nature_items1"),$1,$2) }
	|	nature_item { ($1) }

nature_item: Units EQUALS TK_StringLiteral SEMICOLON { TUPLE5(STRING("nature_item1"),Units,EQUALS,TK_StringLiteral $3,SEMICOLON) }
	|	Abstol EQUALS expression SEMICOLON { TUPLE5(STRING("nature_item2"),Abstol,EQUALS,$3,SEMICOLON) }
	|	Access EQUALS GenericIdentifier SEMICOLON { TUPLE5(STRING("nature_item3"),Access,EQUALS,$3,SEMICOLON) }
	|	Idt_nature EQUALS GenericIdentifier SEMICOLON { TUPLE5(STRING("nature_item4"),Idt_nature,EQUALS,$3,SEMICOLON) }
	|	Ddt_nature EQUALS GenericIdentifier SEMICOLON { TUPLE5(STRING("nature_item5"),Ddt_nature,EQUALS,$3,SEMICOLON) }

library_declaration: Library SymbolIdentifier file_path_spec_list incdir_spec_opt SEMICOLON { TUPLE6(STRING("library_declaration1"),Library,SymbolIdentifier $2,$3,$4,SEMICOLON) }

incdir_spec_opt: incdir_spec { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

incdir_spec: HYPHEN Incdir file_path_spec_list { TUPLE4(STRING("incdir_spec1"),HYPHEN,Incdir,$3) }

include_statement: Include file_path_spec SEMICOLON { TUPLE4(STRING("include_statement1"),Include,$2,SEMICOLON) }

file_path_spec_list: file_path_spec_list COMMA file_path_spec { CONS3($1,COMMA,$3) }
	|	file_path_spec { CONS1 ($1) }

file_path_spec: LT_LT_filepath_GT_GT { (LT_LT_filepath_GT_GT) }

config_declaration: Config GenericIdentifier SEMICOLON design_statement list_of_config_rule_statements_opt Endconfig label_opt { TUPLE8(STRING("config_declaration1"),Config,$2,SEMICOLON,$4,$5,Endconfig,$7) }

design_statement: Design lib_cell_identifiers_opt SEMICOLON { TUPLE4(STRING("design_statement1"),Design,$2,SEMICOLON) }

lib_cell_identifiers_opt: lib_cell_identifiers { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

lib_cell_identifiers: lib_cell_identifiers lib_cell_id { TUPLE3(STRING("lib_cell_identifiers1"),$1,$2) }
	|	lib_cell_id { ($1) }

list_of_config_rule_statements_opt: list_of_config_rule_statements { CONS1 ($1) }
	|	/* empty */ { EMPTY_TOKEN }

list_of_config_rule_statements: list_of_config_rule_statements config_rule_statement { CONS2($1,$2) }
	|	config_rule_statement { CONS1 ($1) }

config_rule_statement: Default liblist_clause SEMICOLON { TUPLE4(STRING("config_rule_statement1"),Default,$2,SEMICOLON) }
	|	inst_clause liblist_clause SEMICOLON { TUPLE4(STRING("config_rule_statement2"),$1,$2,SEMICOLON) }
	|	inst_clause use_clause SEMICOLON { TUPLE4(STRING("config_rule_statement3"),$1,$2,SEMICOLON) }
	|	cell_clause liblist_clause SEMICOLON { TUPLE4(STRING("config_rule_statement4"),$1,$2,SEMICOLON) }
	|	cell_clause use_clause SEMICOLON { TUPLE4(STRING("config_rule_statement5"),$1,$2,SEMICOLON) }
	|	preprocessor_balanced_config_rule_statements { ($1) }

inst_clause: Instance reference { TUPLE3(STRING("inst_clause1"),Instance,$2) }

cell_clause: Cell lib_cell_id { TUPLE3(STRING("cell_clause1"),Cell,$2) }

liblist_clause: Liblist list_of_libraries_opt { TUPLE3(STRING("liblist_clause1"),Liblist,$2) }

use_clause: Use lib_cell_id opt_config { TUPLE4(STRING("use_clause1"),Use,$2,$3) }
	|	Use named_parameter_assignment_list opt_config { TUPLE4(STRING("use_clause2"),Use,$2,$3) }

preprocessor_balanced_config_rule_statements: preprocessor_if_header list_of_config_rule_statements_opt preprocessor_elsif_config_rule_statements_opt preprocessor_else_config_rule_statement_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_config_rule_statements1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_config_rule_statements_opt: preprocessor_elsif_config_rule_statements { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_config_rule_statements: preprocessor_elsif_config_rule_statements preprocessor_elsif_config_rule_statement { TUPLE3(STRING("preprocessor_elsif_config_rule_statements1"),$1,$2) }
	|	preprocessor_elsif_config_rule_statement { ($1) }

preprocessor_elsif_config_rule_statement: preprocessor_elsif_header list_of_config_rule_statements_opt { TUPLE3(STRING("preprocessor_elsif_config_rule_statement1"),$1,$2) }

preprocessor_else_config_rule_statement_opt: preprocessor_else_config_rule_statement { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_config_rule_statement: BACKQUOTE_else list_of_config_rule_statements_opt { TUPLE3(STRING("preprocessor_else_config_rule_statement1"),BACKQUOTE_else,$2) }

named_parameter_assignment_list: named_parameter_assignment_list COMMA named_parameter_assignment { CONS3($1,COMMA,$3) }
	|	named_parameter_assignment { CONS1 ($1) }

named_parameter_assignment: DOT member_name LPAREN parameter_expr RPAREN { TUPLE6(STRING("named_parameter_assignment1"),DOT,$2,LPAREN,$4,RPAREN) }
	|	DOT member_name LPAREN RPAREN { TUPLE5(STRING("named_parameter_assignment2"),DOT,$2,LPAREN,RPAREN) }

opt_config: COLON Config { TUPLE3(STRING("opt_config1"),COLON,Config) }
	|	/* empty */ { EMPTY_TOKEN }

lib_cell_id: GenericIdentifier { ($1) }
	|	GenericIdentifier DOT GenericIdentifier { TUPLE4(STRING("lib_cell_id2"),$1,DOT,$3) }

list_of_libraries_opt: list_of_libraries { CONS1 ($1) }
	|	/* empty */ { EMPTY_TOKEN }

list_of_libraries: list_of_libraries GenericIdentifier { CONS2($1,$2) }
	|	GenericIdentifier { CONS1 ($1) }

drive_strength: LPAREN dr_strength0 COMMA dr_strength1 RPAREN { TUPLE6(STRING("drive_strength1"),LPAREN,$2,COMMA,$4,RPAREN) }
	|	LPAREN dr_strength1 COMMA dr_strength0 RPAREN { TUPLE6(STRING("drive_strength2"),LPAREN,$2,COMMA,$4,RPAREN) }
	|	LPAREN dr_strength0 COMMA Highz1 RPAREN { TUPLE6(STRING("drive_strength3"),LPAREN,$2,COMMA,Highz1,RPAREN) }
	|	LPAREN dr_strength1 COMMA Highz0 RPAREN { TUPLE6(STRING("drive_strength4"),LPAREN,$2,COMMA,Highz0,RPAREN) }
	|	LPAREN Highz1 COMMA dr_strength0 RPAREN { TUPLE6(STRING("drive_strength5"),LPAREN,Highz1,COMMA,$4,RPAREN) }
	|	LPAREN Highz0 COMMA dr_strength1 RPAREN { TUPLE6(STRING("drive_strength6"),LPAREN,Highz0,COMMA,$4,RPAREN) }

drive_strength_opt: drive_strength { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

dr_strength0: Supply0 { (Supply0) }
	|	Strong0 { (Strong0) }
	|	Pull0 { (Pull0) }
	|	Weak0 { (Weak0) }

dr_strength1: Supply1 { (Supply1) }
	|	Strong1 { (Strong1) }
	|	Pull1 { (Pull1) }
	|	Weak1 { (Weak1) }

pull01: Pull0 { (Pull0) }
	|	Pull1 { (Pull1) }

event_control: AT hierarchy_event_identifier { TUPLE3(STRING("event_control1"),AT,$2) }
	|	AT LPAREN event_expression_list RPAREN { TUPLE5(STRING("event_control2"),AT,LPAREN,$3,RPAREN) }
	|	AT LPAREN STAR RPAREN { TUPLE5(STRING("event_control3"),AT,LPAREN,STAR,RPAREN) }
	|	AT STAR { TUPLE3(STRING("event_control4"),AT,STAR) }

event_control_opt: event_control { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

event_expression_list: event_expression { CONS1 ($1) }
	|	event_expression_list Or event_expression { CONS3($1,Or,$3) }
	|	event_expression_list COMMA event_expression { CONS3($1,COMMA,$3) }

event_expression: edge_operator expression { TUPLE3(STRING("event_expression1"),$1,$2) }
	|	expression { ($1) }
	|	edge_operator expression Iff expression { TUPLE5(STRING("event_expression3"),$1,$2,Iff,$4) }
	|	expression Iff expression { TUPLE4(STRING("event_expression4"),$1,Iff,$3) }

branch_probe_expression: GenericIdentifier LPAREN GenericIdentifier COMMA GenericIdentifier RPAREN { TUPLE7(STRING("branch_probe_expression1"),$1,LPAREN,$3,COMMA,$5,RPAREN) }
	|	GenericIdentifier LPAREN GenericIdentifier RPAREN { TUPLE5(STRING("branch_probe_expression2"),$1,LPAREN,$3,RPAREN) }

pattern_opt: pattern { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

pattern: DOT member_name { TUPLE3(STRING("pattern1"),DOT,$2) }
	|	DOT_STAR { (DOT_STAR) }
	|	expr_primary_no_groups { ($1) }
	|	Tagged GenericIdentifier pattern_opt { TUPLE4(STRING("pattern4"),Tagged,$2,$3) }
	|	QUOTE_LBRACE pattern_list RBRACE { TUPLE4(STRING("pattern5"),QUOTE_LBRACE,$2,RBRACE) }
	|	QUOTE_LBRACE member_pattern_list RBRACE { TUPLE4(STRING("pattern6"),QUOTE_LBRACE,$2,RBRACE) }

pattern_list: pattern_list COMMA pattern { CONS3($1,COMMA,$3) }
	|	pattern { CONS1 ($1) }

member_pattern: GenericIdentifier COLON pattern { TUPLE4(STRING("member_pattern1"),$1,COLON,$3) }

member_pattern_list: member_pattern_list COMMA member_pattern { CONS3($1,COMMA,$3) }
	|	member_pattern { CONS1 ($1) }

expression: equiv_impl_expr { ($1) }

equiv_impl_expr: cond_expr { ($1) }
	|	cond_expr HYPHEN_GT_LPAREN_logical_HYPHEN_implies_RPAREN equiv_impl_expr { TUPLE4(STRING("equiv_impl_expr2"),$1,HYPHEN_GT_LPAREN_logical_HYPHEN_implies_RPAREN,$3) }
	|	cond_expr LT_HYPHEN_GT equiv_impl_expr { TUPLE4(STRING("equiv_impl_expr3"),$1,LT_HYPHEN_GT,$3) }

cond_expr: logor_expr { ($1) }
	|	logor_expr QUERY expression COLON cond_expr { TUPLE6(STRING("cond_expr2"),$1,QUERY,$3,COLON,$5) }

inc_or_dec_or_primary_expr: postfix_expression { ($1) }
	|	inc_or_dec_expression { ($1) }

unary_expr: unary_prefix_expr { ($1) }

unary_prefix_expr: inc_or_dec_or_primary_expr { ($1) }
	|	unary_op unary_prefix_expr { TUPLE3(STRING("unary_prefix_expr2"),$1,$2) }

unary_op: PLUS { (PLUS) }
	|	HYPHEN { (HYPHEN) }
	|	TILDE { (TILDE) }
	|	AMPERSAND { (AMPERSAND) }
	|	PLING { (PLING) }
	|	VBAR { (VBAR) }
	|	CARET { (CARET) }
	|	TILDE_AMPERSAND { (TILDE_AMPERSAND) }
	|	TILDE_VBAR { (TILDE_VBAR) }
	|	TILDE_CARET { (TILDE_CARET) }

pow_expr: unary_expr { ($1) }
	|	pow_expr STAR_STAR unary_expr { TUPLE4(STRING("pow_expr2"),$1,STAR_STAR,$3) }

mul_expr: pow_expr { ($1) }
	|	mul_expr STAR pow_expr { TUPLE4(STRING("mul_expr2"),$1,STAR,$3) }
	|	mul_expr SLASH pow_expr { TUPLE4(STRING("mul_expr3"),$1,SLASH,$3) }
	|	mul_expr PERCENT pow_expr { TUPLE4(STRING("mul_expr4"),$1,PERCENT,$3) }

add_expr: mul_expr { ($1) }
	|	add_expr PLUS mul_expr { TUPLE4(STRING("add_expr2"),$1,PLUS,$3) }
	|	add_expr HYPHEN mul_expr { TUPLE4(STRING("add_expr3"),$1,HYPHEN,$3) }

shift_expr: add_expr { ($1) }
	|	shift_expr LT_LT add_expr { TUPLE4(STRING("shift_expr2"),$1,LT_LT,$3) }
	|	shift_expr GT_GT add_expr { TUPLE4(STRING("shift_expr3"),$1,GT_GT,$3) }
	|	shift_expr GT_GT_GT add_expr { TUPLE4(STRING("shift_expr4"),$1,GT_GT_GT,$3) }

comp_expr: shift_expr { ($1) }
	|	comp_expr LESS shift_expr { TUPLE4(STRING("comp_expr2"),$1,LESS,$3) }
	|	comp_expr GREATER shift_expr { TUPLE4(STRING("comp_expr3"),$1,GREATER,$3) }
	|	comp_expr LT_EQ shift_expr { TUPLE4(STRING("comp_expr4"),$1,LT_EQ,$3) }
	|	comp_expr GT_EQ shift_expr { TUPLE4(STRING("comp_expr5"),$1,GT_EQ,$3) }
	|	comp_expr Inside LBRACE open_range_list RBRACE { TUPLE6(STRING("comp_expr6"),$1,Inside,LBRACE,$4,RBRACE) }
	|	comp_expr Inside reference { TUPLE4(STRING("comp_expr7"),$1,Inside,$3) }

logeq_expr: comp_expr { ($1) }
	|	logeq_expr EQ_EQ comp_expr { TUPLE4(STRING("logeq_expr2"),$1,EQ_EQ,$3) }
	|	logeq_expr PLING_EQ comp_expr { TUPLE4(STRING("logeq_expr3"),$1,PLING_EQ,$3) }
	|	logeq_expr EQ_EQ_QUERY comp_expr { TUPLE4(STRING("logeq_expr4"),$1,EQ_EQ_QUERY,$3) }
	|	logeq_expr PLING_EQ_QUERY comp_expr { TUPLE4(STRING("logeq_expr5"),$1,PLING_EQ_QUERY,$3) }

caseeq_expr: logeq_expr { ($1) }
	|	caseeq_expr EQ_EQ_EQ logeq_expr { TUPLE4(STRING("caseeq_expr2"),$1,EQ_EQ_EQ,$3) }
	|	caseeq_expr PLING_EQ_EQ logeq_expr { TUPLE4(STRING("caseeq_expr3"),$1,PLING_EQ_EQ,$3) }

bitand_expr: caseeq_expr { ($1) }
	|	bitand_expr AMPERSAND caseeq_expr { TUPLE4(STRING("bitand_expr2"),$1,AMPERSAND,$3) }
	|	bitand_expr TILDE_AMPERSAND caseeq_expr { TUPLE4(STRING("bitand_expr3"),$1,TILDE_AMPERSAND,$3) }

xor_expr: bitand_expr { ($1) }
	|	xor_expr CARET bitand_expr { TUPLE4(STRING("xor_expr2"),$1,CARET,$3) }
	|	xor_expr TILDE_CARET bitand_expr { TUPLE4(STRING("xor_expr3"),$1,TILDE_CARET,$3) }

bitor_expr: xor_expr { ($1) }
	|	bitor_expr VBAR xor_expr { TUPLE4(STRING("bitor_expr2"),$1,VBAR,$3) }
	|	bitor_expr TILDE_VBAR xor_expr { TUPLE4(STRING("bitor_expr3"),$1,TILDE_VBAR,$3) }

with_exprs_suffix: with_exprs_suffix with_covergroup_expression_in_parens { TUPLE3(STRING("with_exprs_suffix1"),$1,$2) }
	|	bitor_expr { ($1) }

matches_expr: with_exprs_suffix { ($1) }
	|	matches_expr Matches with_exprs_suffix { TUPLE4(STRING("matches_expr2"),$1,Matches,$3) }

logand_expr: matches_expr { ($1) }
	|	logand_expr AMPERSAND_AMPERSAND bitor_expr { TUPLE4(STRING("logand_expr2"),$1,AMPERSAND_AMPERSAND,$3) }

logor_expr: logand_expr { ($1) }
	|	logor_expr VBAR_VBAR logand_expr { TUPLE4(STRING("logor_expr2"),$1,VBAR_VBAR,$3) }

expr_mintypmax: expr_mintypmax_trans_set { ($1) }

expr_mintypmax_trans_set: expr_mintypmax_trans_set EQ_GT expr_mintypmax_generalized { TUPLE4(STRING("expr_mintypmax_trans_set1"),$1,EQ_GT,$3) }
	|	expr_mintypmax_generalized { ($1) }

expr_mintypmax_generalized: expr_mintypmax_generalized COLON property_expr_or_assignment_list { TUPLE4(STRING("expr_mintypmax_generalized1"),$1,COLON,$3) }
	|	property_expr_or_assignment_list { ($1) }

property_expr_or_assignment_list: property_expr_or_assignment_list COMMA property_expr_or_assignment { CONS3($1,COMMA,$3) }
	|	property_expr_or_assignment { CONS1 ($1) }

property_expr_or_assignment: property_expr { ($1) }
	|	LBRACK expression COLON expression RBRACK { TUPLE6(STRING("property_expr_or_assignment2"),LBRACK,$2,COLON,$4,RBRACK) }
	|	assignment_statement_no_expr { ($1) }

argument_list_opt: any_argument_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

any_argument_list: any_argument_list_trailing_comma { CONS1 ($1) }
	|	any_argument_list_preprocessor_last { CONS1 ($1) }
	|	any_argument_list_item_last { CONS1 ($1) }

any_argument_list_trailing_comma: any_argument_list COMMA { TUPLE3(STRING("any_argument_list_trailing_comma1"),$1,COMMA) }
	|	COMMA { (COMMA) }

any_argument_list_item_last: any_argument { ($1) }
	|	any_argument_list_trailing_comma any_argument { TUPLE3(STRING("any_argument_list_item_last2"),$1,$2) }
	|	any_argument_list_preprocessor_last any_argument { TUPLE3(STRING("any_argument_list_item_last3"),$1,$2) }

any_argument_list_preprocessor_last: preprocessor_directive { ($1) }
	|	any_argument_list preprocessor_directive { TUPLE3(STRING("any_argument_list_preprocessor_last2"),$1,$2) }

any_argument: expression { ($1) }
	|	data_type_primitive { ($1) }
	|	event_control { ($1) }
	|	parameter_value_byname { ($1) }

expression_or_null_list_opt: expression_or_null_list_opt COMMA expression_opt { TUPLE4(STRING("expression_or_null_list_opt1"),$1,COMMA,$3) }
	|	expression_opt { ($1) }

expression_opt: expression { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

expression_list_proper: expression_list_proper COMMA expression { TUPLE4(STRING("expression_list_proper1"),$1,COMMA,$3) }
	|	expression { ($1) }

scope_prefix: GenericIdentifier COLON_COLON { TUPLE3(STRING("scope_prefix1"),$1,COLON_COLON) }
	|	DLR_unit COLON_COLON { TUPLE3(STRING("scope_prefix2"),DLR_unit,COLON_COLON) }

postfix_expression: reference_or_call { ($1) }
	|	expr_primary { ($1) }

call_base: LPAREN argument_list_opt RPAREN { TUPLE4(STRING("call_base1"),LPAREN,$2,RPAREN) }

reference_or_call_base: reference call_base { TUPLE3(STRING("reference_or_call_base1"),$1,$2) }
	|	reference_or_call_base hierarchy_or_call_extension { TUPLE3(STRING("reference_or_call_base2"),$1,$2) }

reference_or_call: reference { ($1) }
	|	reference_or_call_base { ($1) }
	|	reference DOT builtin_array_method { TUPLE4(STRING("reference_or_call3"),$1,DOT,$3) }
	|	reference DOT Randomize { TUPLE4(STRING("reference_or_call4"),$1,DOT,Randomize) }
	|	reference call_base select_variable_dimension { TUPLE4(STRING("reference_or_call5"),$1,$2,$3) }

reference: local_root { ($1) }
	|	reference hierarchy_extension { TUPLE3(STRING("reference2"),$1,$2) }
	|	reference select_variable_dimension { TUPLE3(STRING("reference3"),$1,$2) }
	|	MacroCall { ($1) }

expr_primary: expr_primary_no_groups { ($1) }
	|	expr_primary_parens { ($1) }
	|	expr_primary_braces { ($1) }
	|	assignment_pattern_expression { ($1) }

expr_primary_parens: LPAREN expr_mintypmax RPAREN { TUPLE4(STRING("expr_primary_parens1"),LPAREN,$2,RPAREN) }

expr_primary_braces: LBRACE RBRACE { TUPLE3(STRING("expr_primary_braces1"),LBRACE,RBRACE) }
	|	LBRACE value_range LBRACE expression_list_proper RBRACE RBRACE { TUPLE7(STRING("expr_primary_braces2"),LBRACE,$2,LBRACE,$4,RBRACE,RBRACE) }
	|	range_list_in_braces { ($1) }
	|	streaming_concatenation { ($1) }

range_list_in_braces: LBRACE open_range_list RBRACE { TUPLE4(STRING("range_list_in_braces1"),LBRACE,$2,RBRACE) }

type_or_id_root: class_id { ($1) }
	|	implicit_class_handle { ($1) }

local_root: Local_COLON_COLON type_or_id_root { TUPLE3(STRING("local_root1"),Local_COLON_COLON,$2) }
	|	DLR_root DOT type_or_id_root { TUPLE4(STRING("local_root2"),DLR_root,DOT,$3) }
	|	type_or_id_root { ($1) }

string_literal: TK_StringLiteral { (TK_StringLiteral $1) }
	|	TK_EvalStringLiteral { (TK_EvalStringLiteral) }

expr_primary_no_groups: number { ($1) }
	|	TK_RealTime { (TK_RealTime $1) }
	|	TK_TimeLiteral { (TK_TimeLiteral) }
	|	string_literal { ($1) }
	|	cast { ($1) }
	|	randomize_call { ($1) }
	|	select_condition { ($1) }
	|	DOLLAR { (DOLLAR) }
	|	Null { (Null) }
	|	system_tf_call { ($1) }
	|	type_reference { ($1) }
	|	MacroGenericItem { ($1) }

cast: casting_type QUOTE LPAREN expression RPAREN { TUPLE6(STRING("cast1"),$1,QUOTE,LPAREN,$4,RPAREN) }

casting_type: TK_DecNumber { (TK_DecNumber $1) }
	|	expr_primary_parens { ($1) }
	|	system_tf_call { ($1) }
	|	data_type_base { ($1) }
	|	signing { ($1) }
	|	Const { (Const) }
	|	reference { ($1) }

randomize_call: Randomize identifier_list_in_parens_opt with_constraint_block_opt { TUPLE4(STRING("randomize_call1"),Randomize,$2,$3) }

identifier_list_in_parens_opt: LPAREN identifier_list RPAREN { TUPLE4(STRING("identifier_list_in_parens_opt1"),LPAREN,$2,RPAREN) }
	|	LPAREN Null RPAREN { TUPLE4(STRING("identifier_list_in_parens_opt2"),LPAREN,Null,RPAREN) }
	|	LPAREN RPAREN { TUPLE3(STRING("identifier_list_in_parens_opt3"),LPAREN,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

identifier_list: identifier_list COMMA reference { CONS3($1,COMMA,$3) }
	|	reference { CONS1 ($1) }

with_constraint_block_opt: with_constraint_block { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

with_constraint_block: With identifier_list_in_parens_opt constraint_block { TUPLE4(STRING("with_constraint_block1"),With,$2,$3) }

function_item_list: function_item { CONS1 ($1) }
	|	function_item_list function_item { CONS2($1,$2) }

function_item: tf_port_declaration { ($1) }
	|	function_item_data_declaration { ($1) }
	|	net_type_declaration { ($1) }
	|	package_import_declaration { ($1) }
	|	any_param_declaration { ($1) }
	|	type_declaration { ($1) }
	|	let_declaration { ($1) }

primitive_gate_instance: GenericIdentifier decl_dimensions_opt LPAREN any_port_list_opt RPAREN { TUPLE6(STRING("primitive_gate_instance1"),$1,$2,LPAREN,$4,RPAREN) }
	|	LPAREN any_port_list_opt RPAREN { TUPLE4(STRING("primitive_gate_instance2"),LPAREN,$2,RPAREN) }
	|	GenericIdentifier decl_dimensions { TUPLE3(STRING("primitive_gate_instance3"),$1,$2) }

primitive_gate_instance_list: primitive_gate_instance_list COMMA primitive_gate_instance { CONS3($1,COMMA,$3) }
	|	primitive_gate_instance { CONS1 ($1) }

gate_instance_or_register_variable: GenericIdentifier decl_dimensions_opt trailing_decl_assignment_opt { TUPLE4(STRING("gate_instance_or_register_variable1"),$1,$2,$3) }
	|	GenericIdentifier decl_dimensions_opt LPAREN any_port_list_opt RPAREN { TUPLE6(STRING("gate_instance_or_register_variable2"),$1,$2,LPAREN,$4,RPAREN) }
	|	MacroCall { ($1) }
	|	call_base { ($1) }

gate_instance_or_register_variable_list: gate_instance_or_register_variable_list COMMA gate_instance_or_register_variable { CONS3($1,COMMA,$3) }
	|	gate_instance_or_register_variable { CONS1 ($1) }

gatetype: And { (And) }
	|	Nand { (Nand) }
	|	Or { (Or) }
	|	Nor { (Nor) }
	|	Xor { (Xor) }
	|	Xnor { (Xnor) }
	|	Buf { (Buf) }
	|	Bufif0 { (Bufif0) }
	|	Bufif1 { (Bufif1) }
	|	Not { (Not) }
	|	Notif0 { (Notif0) }
	|	Notif1 { (Notif1) }

switchtype: Nmos { (Nmos) }
	|	Rnmos { (Rnmos) }
	|	Pmos { (Pmos) }
	|	Rpmos { (Rpmos) }
	|	Cmos { (Cmos) }
	|	Rcmos { (Rcmos) }
	|	Tran { (Tran) }
	|	Rtran { (Rtran) }
	|	Tranif0 { (Tranif0) }
	|	Tranif1 { (Tranif1) }
	|	Rtranif0 { (Rtranif0) }
	|	Rtranif1 { (Rtranif1) }

hierarchy_extension: DOT unqualified_id { TUPLE3(STRING("hierarchy_extension1"),DOT,$2) }
	|	DOT MacroCall { TUPLE3(STRING("hierarchy_extension2"),DOT,$2) }
	|	DOT New { TUPLE3(STRING("hierarchy_extension3"),DOT,New) }
	|	DOT Randomize call_base with_constraint_block_opt { TUPLE5(STRING("hierarchy_extension4"),DOT,Randomize,$3,$4) }
	|	DOT Randomize with_constraint_block { TUPLE4(STRING("hierarchy_extension5"),DOT,Randomize,$3) }
	|	DOT builtin_array_method call_base array_method_with_predicate_opt { TUPLE5(STRING("hierarchy_extension6"),DOT,$2,$3,$4) }
	|	DOT builtin_array_method array_method_with_predicate { TUPLE4(STRING("hierarchy_extension7"),DOT,$2,$3) }

hierarchy_or_call_extension: DOT unqualified_id { TUPLE3(STRING("hierarchy_or_call_extension1"),DOT,$2) }
	|	DOT unqualified_id call_base { TUPLE4(STRING("hierarchy_or_call_extension2"),DOT,$2,$3) }
	|	DOT MacroCall { TUPLE3(STRING("hierarchy_or_call_extension3"),DOT,$2) }
	|	DOT New { TUPLE3(STRING("hierarchy_or_call_extension4"),DOT,New) }
	|	DOT New call_base { TUPLE4(STRING("hierarchy_or_call_extension5"),DOT,New,$3) }
	|	DOT Randomize with_constraint_block_opt { TUPLE4(STRING("hierarchy_or_call_extension6"),DOT,Randomize,$3) }
	|	DOT builtin_array_method call_base array_method_with_predicate_opt { TUPLE5(STRING("hierarchy_or_call_extension7"),DOT,$2,$3,$4) }
	|	DOT builtin_array_method array_method_with_predicate_opt { TUPLE4(STRING("hierarchy_or_call_extension8"),DOT,$2,$3) }
	|	DOT Randomize call_base { TUPLE4(STRING("hierarchy_or_call_extension9"),DOT,Randomize,$3) }

builtin_array_method: array_locator_method { ($1) }
	|	array_ordering_method { ($1) }
	|	array_reduction_method { ($1) }

array_locator_method: Find { (Find) }
	|	Find_index { (Find_index) }
	|	Find_first { (Find_first) }
	|	Find_first_index { (Find_first_index) }
	|	Find_last { (Find_last) }
	|	Find_last_index { (Find_last_index) }
	|	Unique { (Unique) }
	|	Unique_index { (Unique_index) }
	|	Min { (Min) }
	|	Max { (Max) }

array_ordering_method: Sort { (Sort) }
	|	Rsort { (Rsort) }
	|	Reverse { (Reverse) }
	|	Shuffle { (Shuffle) }

array_reduction_method: Sum { (Sum) }
	|	Product { (Product) }
	|	And { (And) }
	|	Or { (Or) }
	|	Xor { (Xor) }

array_method_with_predicate_opt: array_method_with_predicate { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

array_method_with_predicate: With LPAREN expression RPAREN { TUPLE5(STRING("array_method_with_predicate1"),With,LPAREN,$3,RPAREN) }

hierarchy_event_identifier: hierarchy_event_identifier DOT hierarchy_segment { TUPLE4(STRING("hierarchy_event_identifier1"),$1,DOT,$3) }
	|	hierarchy_segment { ($1) }

hierarchy_segment: GenericIdentifier select_dimensions_opt { TUPLE3(STRING("hierarchy_segment1"),$1,$2) }

list_of_identifiers: GenericIdentifier { CONS1 ($1) }
	|	list_of_identifiers COMMA GenericIdentifier { CONS3($1,COMMA,$3) }

list_of_identifiers_unpacked_dimensions: list_of_identifiers_unpacked_dimensions COMMA identifier_optional_unpacked_dimensions { CONS3($1,COMMA,$3) }
	|	identifier_optional_unpacked_dimensions { CONS1 ($1) }

identifier_optional_unpacked_dimensions: GenericIdentifier decl_dimensions_opt { TUPLE3(STRING("identifier_optional_unpacked_dimensions1"),$1,$2) }

list_of_module_item_identifiers: list_of_module_item_identifiers COMMA identifier_optional_unpacked_dimensions { CONS3($1,COMMA,$3) }
	|	unqualified_id decl_dimensions_opt { CONS2($1,$2) }

list_of_port_identifiers: GenericIdentifier { CONS1 ($1) }
	|	GenericIdentifier EQUALS expression { CONS3($1,EQUALS,$3) }
	|	list_of_port_identifiers COMMA GenericIdentifier { CONS3($1,COMMA,$3) }
	|	list_of_port_identifiers COMMA GenericIdentifier EQUALS expression { CONS5($1,COMMA,$3,EQUALS,$5) }

identifier_opt: GenericIdentifier { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_list_of_ports_or_port_declarations_opt: list_of_ports_or_port_declarations_opt { ($1) }
	|	list_of_ports_or_port_declarations_trailing_comma { ($1) }

list_of_ports_or_port_declarations_opt: list_of_ports_or_port_declarations { CONS1 ($1) }
	|	/* empty */ { EMPTY_TOKEN }

list_of_ports_or_port_declarations: list_of_ports_or_port_declarations_preprocessor_last { CONS1 ($1) }
	|	list_of_ports_or_port_declarations_item_last { CONS1 ($1) }

list_of_ports_or_port_declarations_preprocessor_last: list_of_ports_or_port_declarations preprocessor_balanced_port_declarations { CONS2($1,$2) }
	|	list_of_ports_or_port_declarations_trailing_comma preprocessor_balanced_port_declarations { CONS2($1,$2) }
	|	preprocessor_balanced_port_declarations { CONS1 ($1) }

list_of_ports_or_port_declarations_item_last: list_of_ports_or_port_declarations_preprocessor_last port_or_port_declaration { CONS2($1,$2) }
	|	list_of_ports_or_port_declarations_trailing_comma port_or_port_declaration { CONS2($1,$2) }
	|	port_or_port_declaration { CONS1 ($1) }

list_of_ports_or_port_declarations_trailing_comma: list_of_ports_or_port_declarations COMMA { CONS2($1,COMMA) }

port_or_port_declaration: port { ($1) }
	|	port_declaration { ($1) }

preprocessor_balanced_port_declarations: preprocessor_if_header preprocessor_list_of_ports_or_port_declarations_opt preprocessor_elsif_port_declarations_opt preprocessor_else_port_declarations_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_port_declarations1"),$1,$2,$3,$4,BACKQUOTE_endif) }
	|	MacroGenericItem { ($1) }
	|	preprocessor_action { ($1) }

preprocessor_elsif_port_declarations_opt: preprocessor_elsif_port_declarations { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_port_declarations: preprocessor_elsif_port_declarations preprocessor_elsif_port_declaration { TUPLE3(STRING("preprocessor_elsif_port_declarations1"),$1,$2) }
	|	preprocessor_elsif_port_declaration { ($1) }

preprocessor_elsif_port_declaration: preprocessor_elsif_header preprocessor_list_of_ports_or_port_declarations_opt { TUPLE3(STRING("preprocessor_elsif_port_declaration1"),$1,$2) }

preprocessor_else_port_declarations_opt: preprocessor_else_port_declarations { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_port_declarations: BACKQUOTE_else preprocessor_list_of_ports_or_port_declarations_opt { TUPLE3(STRING("preprocessor_else_port_declarations1"),BACKQUOTE_else,$2) }

dir: Input { (Input) }
	|	Output { (Output) }
	|	Inout { (Inout) }

port_declaration: port_declaration_noattr { ($1) }

port_declaration_noattr: port_direction var_or_net_type_opt data_type_or_implicit_basic_followed_by_id_and_dimensions_opt trailing_assign_opt { TUPLE5(STRING("port_declaration_noattr1"),$1,$2,$3,$4) }
	|	net_type data_type_or_implicit_basic_followed_by_id_and_dimensions_opt trailing_assign_opt { TUPLE4(STRING("port_declaration_noattr2"),$1,$2,$3) }
	|	data_type_primitive GenericIdentifier decl_dimensions_opt trailing_assign_opt { TUPLE5(STRING("port_declaration_noattr3"),$1,$2,$3,$4) }
	|	type_identifier_followed_by_id decl_dimensions_opt trailing_assign_opt { TUPLE4(STRING("port_declaration_noattr4"),$1,$2,$3) }

var_or_net_type_opt: net_type { ($1) }
	|	Var { (Var) }
	|	/* empty */ { EMPTY_TOKEN }

signed_unsigned_opt: signing { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

lpvalue: reference { ($1) }
	|	range_list_in_braces { ($1) }
	|	assignment_pattern { ($1) }
	|	streaming_concatenation { ($1) }

cont_assign: lpvalue EQUALS expression { TUPLE4(STRING("cont_assign1"),$1,EQUALS,$3) }
	|	reference DOT builtin_array_method EQUALS expression { TUPLE6(STRING("cont_assign2"),$1,DOT,$3,EQUALS,$5) }

cont_assign_list: cont_assign_list COMMA cont_assign { CONS3($1,COMMA,$3) }
	|	cont_assign { CONS1 ($1) }

symbol_or_label: GenericIdentifier { ($1) }
	|	MacroIdItem { (MacroIdItem) }

module_or_interface_declaration: module_start lifetime_opt symbol_or_label module_package_import_list_opt module_parameter_port_list_opt module_port_list_opt module_attribute_foreign_opt SEMICOLON module_item_list_opt module_end label_opt { TUPLE12(STRING("module_or_interface_declaration1"),$1,$2,$3,$4,$5,$6,$7,SEMICOLON,$9,$10,$11) }

module_start: Module { (Module) }
	|	Macromodule { (Macromodule) }
	|	Program { (Program) }
	|	Interface { (Interface) }

module_end: Endmodule { (Endmodule) }
	|	Endprogram { (Endprogram) }
	|	Endinterface { (Endinterface) }

label_opt: COLON symbol_or_label { TUPLE3(STRING("label_opt1"),COLON,$2) }
	|	/* empty */ { EMPTY_TOKEN }

module_attribute_foreign: LPAREN_STAR GenericIdentifier Integer GenericIdentifier EQUALS TK_StringLiteral SEMICOLON STAR_RPAREN { TUPLE9(STRING("module_attribute_foreign1"),LPAREN_STAR,$2,Integer,$4,EQUALS,TK_StringLiteral $6,SEMICOLON,STAR_RPAREN) }

module_attribute_foreign_opt: module_attribute_foreign { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

module_port_list_opt: LPAREN list_of_ports_or_port_declarations_opt RPAREN { TUPLE4(STRING("module_port_list_opt1"),LPAREN,$2,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

module_parameter_port_list_opt: HASH LPAREN module_parameter_port_list RPAREN { TUPLE5(STRING("module_parameter_port_list_opt1"),HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN RPAREN { TUPLE4(STRING("module_parameter_port_list_opt2"),HASH,LPAREN,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

parameter_opt: Parameter { (Parameter) }
	|	Localparam { (Localparam) }
	|	/* empty */ { EMPTY_TOKEN }

module_parameter_port: parameter_opt param_type_followed_by_id_and_dimensions_opt trailing_assign_opt { TUPLE4(STRING("module_parameter_port1"),$1,$2,$3) }
	|	parameter_opt Type type_assignment { TUPLE4(STRING("module_parameter_port2"),$1,Type,$3) }

type_assignment_list: type_assignment_list COMMA type_assignment { CONS3($1,COMMA,$3) }
	|	type_assignment { CONS1 ($1) }

type_assignment: GenericIdentifier EQUALS parameter_expr { TUPLE4(STRING("type_assignment1"),$1,EQUALS,$3) }
	|	GenericIdentifier { ($1) }

module_parameter_port_list: module_parameter_port_list_item_last { CONS1 ($1) }
	|	module_parameter_port_list_preprocessor_last { CONS1 ($1) }

module_parameter_port_list_trailing_comma: module_parameter_port_list COMMA { TUPLE3(STRING("module_parameter_port_list_trailing_comma1"),$1,COMMA) }

module_parameter_port_list_preprocessor_last: module_parameter_port_list preprocessor_directive { TUPLE3(STRING("module_parameter_port_list_preprocessor_last1"),$1,$2) }
	|	module_parameter_port_list_trailing_comma preprocessor_directive { TUPLE3(STRING("module_parameter_port_list_preprocessor_last2"),$1,$2) }
	|	preprocessor_directive { ($1) }

module_parameter_port_list_item_last: module_parameter_port_list_trailing_comma module_parameter_port { TUPLE3(STRING("module_parameter_port_list_item_last1"),$1,$2) }
	|	module_parameter_port_list_preprocessor_last module_parameter_port { TUPLE3(STRING("module_parameter_port_list_item_last2"),$1,$2) }
	|	module_parameter_port { ($1) }

net_declaration: net_type net_variable_or_decl_assigns SEMICOLON { TUPLE4(STRING("net_declaration1"),$1,$2,SEMICOLON) }
	|	net_type data_type_or_implicit net_variable_or_decl_assigns SEMICOLON { TUPLE5(STRING("net_declaration2"),$1,$2,$3,SEMICOLON) }
	|	Trireg charge_strength_opt decl_dimensions_opt delay3_opt list_of_identifiers SEMICOLON { TUPLE7(STRING("net_declaration3"),Trireg,$2,$3,$4,$5,SEMICOLON) }
	|	net_type delay3 net_variable_or_decl_assigns SEMICOLON { TUPLE5(STRING("net_declaration4"),$1,$2,$3,SEMICOLON) }

module_port_declaration: port_direction signed_unsigned_opt qualified_id decl_dimensions_opt list_of_identifiers_unpacked_dimensions SEMICOLON { TUPLE7(STRING("module_port_declaration1"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	port_direction signed_unsigned_opt unqualified_id decl_dimensions_opt list_of_identifiers_unpacked_dimensions SEMICOLON { TUPLE7(STRING("module_port_declaration2"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	port_direction signed_unsigned_opt decl_dimensions delay3_opt list_of_identifiers_unpacked_dimensions SEMICOLON { TUPLE7(STRING("module_port_declaration3"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	port_direction signed_unsigned_opt delay3 list_of_identifiers_unpacked_dimensions SEMICOLON { TUPLE6(STRING("module_port_declaration4"),$1,$2,$3,$4,SEMICOLON) }
	|	port_direction signed_unsigned_opt list_of_module_item_identifiers SEMICOLON { TUPLE5(STRING("module_port_declaration5"),$1,$2,$3,SEMICOLON) }
	|	port_direction port_net_type signed_unsigned_opt decl_dimensions_opt list_of_identifiers_unpacked_dimensions SEMICOLON { TUPLE7(STRING("module_port_declaration6"),$1,$2,$3,$4,$5,SEMICOLON) }
	|	dir var_type signed_unsigned_opt decl_dimensions_opt list_of_port_identifiers SEMICOLON { TUPLE7(STRING("module_port_declaration7"),$1,$2,$3,$4,$5,SEMICOLON) }

parameter_override: Defparam defparam_assign_list SEMICOLON { TUPLE4(STRING("parameter_override1"),Defparam,$2,SEMICOLON) }

gate_instantiation: gatetype primitive_gate_instance_list SEMICOLON { TUPLE4(STRING("gate_instantiation1"),$1,$2,SEMICOLON) }
	|	gatetype delay3 primitive_gate_instance_list SEMICOLON { TUPLE5(STRING("gate_instantiation2"),$1,$2,$3,SEMICOLON) }
	|	gatetype drive_strength primitive_gate_instance_list SEMICOLON { TUPLE5(STRING("gate_instantiation3"),$1,$2,$3,SEMICOLON) }
	|	gatetype drive_strength delay3 primitive_gate_instance_list SEMICOLON { TUPLE6(STRING("gate_instantiation4"),$1,$2,$3,$4,SEMICOLON) }
	|	switchtype primitive_gate_instance_list SEMICOLON { TUPLE4(STRING("gate_instantiation5"),$1,$2,SEMICOLON) }
	|	switchtype delay3 primitive_gate_instance_list SEMICOLON { TUPLE5(STRING("gate_instantiation6"),$1,$2,$3,SEMICOLON) }
	|	Pullup primitive_gate_instance_list SEMICOLON { TUPLE4(STRING("gate_instantiation7"),Pullup,$2,SEMICOLON) }
	|	Pulldown primitive_gate_instance_list SEMICOLON { TUPLE4(STRING("gate_instantiation8"),Pulldown,$2,SEMICOLON) }
	|	Pullup LPAREN dr_strength1 RPAREN primitive_gate_instance_list SEMICOLON { TUPLE7(STRING("gate_instantiation9"),Pullup,LPAREN,$3,RPAREN,$5,SEMICOLON) }
	|	Pullup LPAREN dr_strength1 COMMA dr_strength0 RPAREN primitive_gate_instance_list SEMICOLON { TUPLE9(STRING("gate_instantiation10"),Pullup,LPAREN,$3,COMMA,$5,RPAREN,$7,SEMICOLON) }
	|	Pullup LPAREN dr_strength0 COMMA dr_strength1 RPAREN primitive_gate_instance_list SEMICOLON { TUPLE9(STRING("gate_instantiation11"),Pullup,LPAREN,$3,COMMA,$5,RPAREN,$7,SEMICOLON) }
	|	Pulldown LPAREN dr_strength0 RPAREN primitive_gate_instance_list SEMICOLON { TUPLE7(STRING("gate_instantiation12"),Pulldown,LPAREN,$3,RPAREN,$5,SEMICOLON) }
	|	Pulldown LPAREN dr_strength1 COMMA dr_strength0 RPAREN primitive_gate_instance_list SEMICOLON { TUPLE9(STRING("gate_instantiation13"),Pulldown,LPAREN,$3,COMMA,$5,RPAREN,$7,SEMICOLON) }
	|	Pulldown LPAREN dr_strength0 COMMA dr_strength1 RPAREN primitive_gate_instance_list SEMICOLON { TUPLE9(STRING("gate_instantiation14"),Pulldown,LPAREN,$3,COMMA,$5,RPAREN,$7,SEMICOLON) }

specify_block: Specify specify_item_list_opt Endspecify { TUPLE4(STRING("specify_block1"),Specify,$2,Endspecify) }

specparam_declaration: Specparam specparam_decl SEMICOLON { TUPLE4(STRING("specparam_declaration1"),Specparam,$2,SEMICOLON) }

generate_region: Generate generate_item_list_opt Endgenerate { TUPLE4(STRING("generate_region1"),Generate,$2,Endgenerate) }

continuous_assign: Assign drive_strength_opt delay3_opt cont_assign_list SEMICOLON { TUPLE6(STRING("continuous_assign1"),Assign,$2,$3,$4,SEMICOLON) }
	|	Assign drive_strength_opt delay3_opt macro_call_or_item { TUPLE5(STRING("continuous_assign2"),Assign,$2,$3,$4) }

net_alias_assign_lvalue_list: net_alias_assign_lvalue_list EQUALS lpvalue { CONS3($1,EQUALS,$3) }
	|	lpvalue EQUALS lpvalue { CONS3($1,EQUALS,$3) }

net_alias: Alias net_alias_assign_lvalue_list SEMICOLON { TUPLE4(STRING("net_alias1"),Alias,$2,SEMICOLON) }

loop_generate_construct: For LPAREN genvar_opt GenericIdentifier EQUALS expression SEMICOLON expression_opt SEMICOLON for_step_opt RPAREN generate_item { TUPLE13(STRING("loop_generate_construct1"),For,LPAREN,$3,$4,EQUALS,$6,SEMICOLON,$8,SEMICOLON,$10,RPAREN,$12) }

conditional_generate_construct: generate_if generate_item Else generate_item { TUPLE5(STRING("conditional_generate_construct1"),$1,$2,Else,$4) }
	|	generate_if generate_item { TUPLE3(STRING("conditional_generate_construct2"),$1,$2) }
	|	Case LPAREN expression RPAREN generate_case_items Endcase { TUPLE7(STRING("conditional_generate_construct3"),Case,LPAREN,$3,RPAREN,$5,Endcase) }

always_construct: always_any statement { TUPLE3(STRING("always_construct1"),$1,$2) }

initial_construct: Initial statement { TUPLE3(STRING("initial_construct1"),Initial,$2) }

final_construct: Final statement { TUPLE3(STRING("final_construct1"),Final,$2) }

analog_construct: Analog analog_statement { TUPLE3(STRING("analog_construct1"),Analog,$2) }

module_common_item: module_or_generate_item_declaration { ($1) }
	|	always_construct { ($1) }
	|	initial_construct { ($1) }
	|	final_construct { ($1) }
	|	analog_construct { ($1) }
	|	assertion_item { ($1) }
	|	bind_directive { ($1) }
	|	continuous_assign { ($1) }
	|	net_alias { ($1) }
	|	loop_generate_construct { ($1) }
	|	conditional_generate_construct { ($1) }
	|	system_tf_call SEMICOLON { TUPLE3(STRING("module_common_item12"),$1,SEMICOLON) }

genvar_declaration: Genvar list_of_identifiers SEMICOLON { TUPLE4(STRING("genvar_declaration1"),Genvar,$2,SEMICOLON) }

module_or_generate_item_declaration: package_or_generate_item_declaration { ($1) }
	|	clocking_declaration { ($1) }
	|	Default Clocking GenericIdentifier SEMICOLON { TUPLE5(STRING("module_or_generate_item_declaration3"),Default,Clocking,$3,SEMICOLON) }
	|	Default Disable Iff expression_or_dist SEMICOLON { TUPLE6(STRING("module_or_generate_item_declaration4"),Default,Disable,Iff,$4,SEMICOLON) }
	|	genvar_declaration { ($1) }

package_or_generate_item_declaration: class_declaration { ($1) }
	|	interface_class_declaration { ($1) }
	|	net_declaration { ($1) }
	|	task_declaration { ($1) }
	|	function_declaration { ($1) }
	|	covergroup_declaration { ($1) }
	|	assertion_item_declaration { ($1) }
	|	modport_declaration { ($1) }
	|	specparam_declaration { ($1) }
	|	dpi_import_export { ($1) }
	|	SEMICOLON { (SEMICOLON) }

module_or_generate_item: parameter_override { ($1) }
	|	gate_instantiation { ($1) }
	|	data_declaration_or_module_instantiation { ($1) }
	|	net_type_declaration { ($1) }
	|	package_import_declaration { ($1) }
	|	any_param_declaration { ($1) }
	|	type_declaration { ($1) }
	|	let_declaration { ($1) }
	|	module_common_item { ($1) }

module_item: module_port_declaration { ($1) }
	|	non_port_module_item { ($1) }
	|	module_block { ($1) }
	|	macro_call_or_item { ($1) }
	|	preprocessor_balanced_module_items { ($1) }
	|	preprocessor_action { ($1) }
	|	module_item_directive { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("module_item8"),ERROR_TOKEN,SEMICOLON) }

module_block: begin_rule module_item_list_opt end_rule { TUPLE4(STRING("module_block1"),$1,$2,$3) }

preprocessor_balanced_module_items: preprocessor_if_header module_item_list_opt preprocessor_elsif_module_items_opt preprocessor_else_module_item_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_module_items1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_module_items_opt: preprocessor_elsif_module_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_module_items: preprocessor_elsif_module_items preprocessor_elsif_module_item { TUPLE3(STRING("preprocessor_elsif_module_items1"),$1,$2) }
	|	preprocessor_elsif_module_item { ($1) }

preprocessor_elsif_module_item: preprocessor_elsif_header module_item_list_opt { TUPLE3(STRING("preprocessor_elsif_module_item1"),$1,$2) }

preprocessor_else_module_item_opt: preprocessor_else_module_item { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_module_item: BACKQUOTE_else module_item_list_opt { TUPLE3(STRING("preprocessor_else_module_item1"),BACKQUOTE_else,$2) }

non_port_module_item: generate_region { ($1) }
	|	module_or_generate_item { ($1) }
	|	specify_block { ($1) }
	|	timeunits_declaration { ($1) }
	|	module_or_interface_declaration { ($1) }
	|	DLR_attribute LPAREN GenericIdentifier COMMA TK_StringLiteral COMMA TK_StringLiteral RPAREN SEMICOLON { TUPLE10(STRING("non_port_module_item6"),DLR_attribute,LPAREN,$3,COMMA,TK_StringLiteral $5,COMMA,TK_StringLiteral $7,RPAREN,SEMICOLON) }

always_any: Always { (Always) }
	|	Always_ff { (Always_ff) }
	|	Always_comb { (Always_comb) }
	|	Always_latch { (Always_latch) }

generate_if: If expression_in_parens { TUPLE3(STRING("generate_if1"),If,$2) }

generate_case_items: generate_case_items generate_case_item { TUPLE3(STRING("generate_case_items1"),$1,$2) }
	|	generate_case_item { ($1) }

generate_case_item: expression_list_proper COLON generate_item { TUPLE4(STRING("generate_case_item1"),$1,COLON,$3) }
	|	Default COLON generate_item { TUPLE4(STRING("generate_case_item2"),Default,COLON,$3) }

generate_item: module_or_generate_item { ($1) }
	|	generate_block { ($1) }
	|	macro_call_or_item { ($1) }
	|	preprocessor_balanced_generate_items { ($1) }
	|	preprocessor_action { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("generate_item6"),ERROR_TOKEN,SEMICOLON) }

preprocessor_balanced_generate_items: preprocessor_if_header generate_item_list_opt preprocessor_elsif_generate_items_opt preprocessor_else_generate_item_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_generate_items1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_generate_items_opt: preprocessor_elsif_generate_items { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_generate_items: preprocessor_elsif_generate_items preprocessor_elsif_generate_item { TUPLE3(STRING("preprocessor_elsif_generate_items1"),$1,$2) }
	|	preprocessor_elsif_generate_item { ($1) }

preprocessor_elsif_generate_item: preprocessor_elsif_header generate_item_list_opt { TUPLE3(STRING("preprocessor_elsif_generate_item1"),$1,$2) }

preprocessor_else_generate_item_opt: preprocessor_else_generate_item { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_generate_item: BACKQUOTE_else generate_item_list_opt { TUPLE3(STRING("preprocessor_else_generate_item1"),BACKQUOTE_else,$2) }

begin_rule: Begin label_opt { TUPLE3(STRING("begin1"),Begin,$2) }

end_rule: End label_opt { TUPLE3(STRING("end1"),End,$2) }

generate_block: begin_rule generate_item_list_opt end_rule { TUPLE4(STRING("generate_block1"),$1,$2,$3) }
	|	unqualified_id COLON Begin generate_item_list_opt end_rule { TUPLE6(STRING("generate_block2"),$1,COLON,Begin,$4,$5) }

generate_item_list: generate_item_list generate_item { CONS2($1,$2) }
	|	generate_item { CONS1 ($1) }

generate_item_list_opt: generate_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

module_item_list: module_item_list module_item { CONS2($1,$2) }
	|	module_item { CONS1 ($1) }

module_item_list_opt: module_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

genvar_opt: Genvar { (Genvar) }
	|	/* empty */ { EMPTY_TOKEN }

net_decl_assign: GenericIdentifier EQUALS expression { TUPLE4(STRING("net_decl_assign1"),$1,EQUALS,$3) }

net_variable_or_decl_assign: net_variable { ($1) }
	|	net_decl_assign { ($1) }

net_variable_or_decl_assigns: net_variable_or_decl_assigns COMMA net_variable_or_decl_assign { TUPLE4(STRING("net_variable_or_decl_assigns1"),$1,COMMA,$3) }
	|	net_variable_or_decl_assign { ($1) }

bit_logic: Logic { (Logic) }
	|	Bit { (Bit) }

bit_logic_opt: bit_logic { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

port_net_type: net_type { ($1) }
	|	Logic { (Logic) }

net_type: Wire { (Wire) }
	|	Tri { (Tri) }
	|	Tri1 { (Tri1) }
	|	Supply0 { (Supply0) }
	|	Wand { (Wand) }
	|	Triand { (Triand) }
	|	Tri0 { (Tri0) }
	|	Supply1 { (Supply1) }
	|	Wor { (Wor) }
	|	Trior { (Trior) }
	|	Wone { (Wone) }
	|	Uwire { (Uwire) }

var_type: Reg { (Reg) }

param_type_followed_by_id_and_dimensions_opt: bit_logic_opt signed_unsigned_opt qualified_id decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE7(STRING("param_type_followed_by_id_and_dimensions_opt1"),$1,$2,$3,$4,$5,$6) }
	|	bit_logic_opt signed_unsigned_opt unqualified_id decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE7(STRING("param_type_followed_by_id_and_dimensions_opt2"),$1,$2,$3,$4,$5,$6) }
	|	bit_logic_opt signed_unsigned_opt unqualified_id decl_dimensions_opt { TUPLE5(STRING("param_type_followed_by_id_and_dimensions_opt3"),$1,$2,$3,$4) }
	|	bit_logic_opt signed_unsigned_opt decl_dimensions GenericIdentifier decl_dimensions_opt { TUPLE6(STRING("param_type_followed_by_id_and_dimensions_opt4"),$1,$2,$3,$4,$5) }
	|	integer_atom_type signed_unsigned_opt decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE6(STRING("param_type_followed_by_id_and_dimensions_opt5"),$1,$2,$3,$4,$5) }
	|	non_integer_type decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("param_type_followed_by_id_and_dimensions_opt6"),$1,$2,$3,$4) }
	|	Reg decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("param_type_followed_by_id_and_dimensions_opt7"),Reg,$2,$3,$4) }
	|	String decl_dimensions_opt GenericIdentifier decl_dimensions_opt { TUPLE5(STRING("param_type_followed_by_id_and_dimensions_opt8"),String,$2,$3,$4) }

parameter_assign_list: parameter_assign { CONS1 ($1) }
	|	parameter_assign_list COMMA parameter_assign { CONS3($1,COMMA,$3) }

localparam_assign_list: localparam_assign { CONS1 ($1) }
	|	localparam_assign_list COMMA localparam_assign { CONS3($1,COMMA,$3) }

parameter_assign: GenericIdentifier EQUALS expression parameter_value_ranges_opt { TUPLE5(STRING("parameter_assign1"),$1,EQUALS,$3,$4) }

localparam_assign: GenericIdentifier EQUALS expression { TUPLE4(STRING("localparam_assign1"),$1,EQUALS,$3) }

trailing_assign: EQUALS parameter_expr parameter_value_ranges_opt { TUPLE4(STRING("trailing_assign1"),EQUALS,$2,$3) }

trailing_assign_opt: trailing_assign { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

parameter_value_ranges_opt: parameter_value_ranges { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

parameter_value_ranges: parameter_value_ranges parameter_value_range { TUPLE3(STRING("parameter_value_ranges1"),$1,$2) }
	|	parameter_value_range { ($1) }

parameter_value_range: from_exclude LBRACK value_range_expression COLON value_range_expression RBRACK { TUPLE7(STRING("parameter_value_range1"),$1,LBRACK,$3,COLON,$5,RBRACK) }
	|	from_exclude LBRACK value_range_expression COLON value_range_expression RPAREN { TUPLE7(STRING("parameter_value_range2"),$1,LBRACK,$3,COLON,$5,RPAREN) }
	|	from_exclude LPAREN value_range_expression COLON value_range_expression RBRACK { TUPLE7(STRING("parameter_value_range3"),$1,LPAREN,$3,COLON,$5,RBRACK) }
	|	from_exclude LPAREN value_range_expression COLON value_range_expression RPAREN { TUPLE7(STRING("parameter_value_range4"),$1,LPAREN,$3,COLON,$5,RPAREN) }
	|	Exclude expression { TUPLE3(STRING("parameter_value_range5"),Exclude,$2) }

value_range_expression: expression { ($1) }
	|	Inf { (Inf) }
	|	PLUS Inf { TUPLE3(STRING("value_range_expression3"),PLUS,Inf) }
	|	HYPHEN Inf { TUPLE3(STRING("value_range_expression4"),HYPHEN,Inf) }

from_exclude: From { (From) }
	|	Exclude { (Exclude) }

parameter_value_opt: parameters { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

parameters: HASH LPAREN parameter_expr_list RPAREN { TUPLE5(STRING("parameters1"),HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN parameter_value_byname_list RPAREN { TUPLE5(STRING("parameters2"),HASH,LPAREN,$3,RPAREN) }
	|	HASH LPAREN RPAREN { TUPLE4(STRING("parameters3"),HASH,LPAREN,RPAREN) }
	|	HASH TK_DecNumber { TUPLE3(STRING("parameters4"),HASH,TK_DecNumber $2) }
	|	HASH TK_RealTime { TUPLE3(STRING("parameters5"),HASH,TK_RealTime $2) }

parameter_expr_list: parameter_expr_list COMMA parameter_expr { CONS3($1,COMMA,$3) }
	|	parameter_expr { CONS1 ($1) }

parameter_value_byname: DOT member_name LPAREN parameter_expr RPAREN { TUPLE6(STRING("parameter_value_byname1"),DOT,$2,LPAREN,$4,RPAREN) }
	|	DOT member_name LPAREN RPAREN { TUPLE5(STRING("parameter_value_byname2"),DOT,$2,LPAREN,RPAREN) }
	|	DOT member_name { TUPLE3(STRING("parameter_value_byname3"),DOT,$2) }

parameter_value_byname_list: parameter_value_byname_list_item_last { CONS1 ($1) }
	|	parameter_value_byname_list_preprocessor_last { CONS1 ($1) }
	|	parameter_value_byname_list_trailing_comma { CONS1 ($1) }

parameter_value_byname_list_trailing_comma: parameter_value_byname_list COMMA { TUPLE3(STRING("parameter_value_byname_list_trailing_comma1"),$1,COMMA) }
	|	COMMA { (COMMA) }

parameter_value_byname_list_preprocessor_last: parameter_value_byname_list preprocessor_directive { TUPLE3(STRING("parameter_value_byname_list_preprocessor_last1"),$1,$2) }
	|	preprocessor_directive { ($1) }

parameter_value_byname_list_item_last: parameter_value_byname { ($1) }
	|	parameter_value_byname_list_trailing_comma parameter_value_byname { TUPLE3(STRING("parameter_value_byname_list_item_last2"),$1,$2) }
	|	parameter_value_byname_list_preprocessor_last parameter_value_byname { TUPLE3(STRING("parameter_value_byname_list_item_last3"),$1,$2) }

parameter_expr: expression { ($1) }
	|	data_type_primitive { ($1) }
	|	interface_type { ($1) }

port: port_expression trailing_assign_opt { TUPLE3(STRING("port1"),$1,$2) }
	|	DOT member_name LPAREN port_expression_opt RPAREN { TUPLE6(STRING("port2"),DOT,$2,LPAREN,$4,RPAREN) }

any_port_list_opt: any_port_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

any_port_list: any_port_list_item_last { CONS1 ($1) }
	|	any_port_list_preprocessor_last { CONS1 ($1) }
	|	any_port_list_trailing_comma { CONS1 ($1) }

any_port_list_trailing_comma: any_port_list COMMA { TUPLE3(STRING("any_port_list_trailing_comma1"),$1,COMMA) }
	|	COMMA { (COMMA) }

any_port_list_item_last: any_port_list_trailing_comma any_port { TUPLE3(STRING("any_port_list_item_last1"),$1,$2) }
	|	any_port_list_preprocessor_last any_port { TUPLE3(STRING("any_port_list_item_last2"),$1,$2) }
	|	any_port { ($1) }

any_port_list_preprocessor_last: any_port_list preprocessor_directive { TUPLE3(STRING("any_port_list_preprocessor_last1"),$1,$2) }
	|	preprocessor_directive { ($1) }

any_port: port_named { ($1) }
	|	expression { ($1) }

port_named: DOT member_name LPAREN expression RPAREN { TUPLE6(STRING("port_named1"),DOT,$2,LPAREN,$4,RPAREN) }
	|	DOT member_name LPAREN RPAREN { TUPLE5(STRING("port_named2"),DOT,$2,LPAREN,RPAREN) }
	|	DOT member_name { TUPLE3(STRING("port_named3"),DOT,$2) }
	|	DOT_STAR { (DOT_STAR) }

member_name: GenericIdentifier { ($1) }
	|	builtin_array_method { ($1) }

port_expression_opt: port_expression { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

port_expression: port_reference { ($1) }
	|	LBRACE port_reference_list RBRACE { TUPLE4(STRING("port_expression2"),LBRACE,$2,RBRACE) }

port_reference: unqualified_id decl_dimensions_opt { TUPLE3(STRING("port_reference1"),$1,$2) }

port_reference_list: port_reference { CONS1 ($1) }
	|	port_reference_list COMMA port_reference { CONS3($1,COMMA,$3) }

select_dimensions_opt: select_dimensions { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

select_dimensions: select_variable_dimension { ($1) }
	|	select_dimensions select_variable_dimension { TUPLE3(STRING("select_dimensions2"),$1,$2) }

decl_dimensions_opt: decl_dimensions { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

decl_dimensions: decl_variable_dimension { ($1) }
	|	decl_dimensions decl_variable_dimension { TUPLE3(STRING("decl_dimensions2"),$1,$2) }

net_variable: GenericIdentifier decl_dimensions_opt { TUPLE3(STRING("net_variable1"),$1,$2) }

specify_item: Specparam specparam_decl SEMICOLON { TUPLE4(STRING("specify_item1"),Specparam,$2,SEMICOLON) }
	|	specify_simple_path_decl SEMICOLON { TUPLE3(STRING("specify_item2"),$1,SEMICOLON) }
	|	specify_edge_path_decl SEMICOLON { TUPLE3(STRING("specify_item3"),$1,SEMICOLON) }
	|	If LPAREN expression RPAREN specify_simple_path_decl SEMICOLON { TUPLE7(STRING("specify_item4"),If,LPAREN,$3,RPAREN,$5,SEMICOLON) }
	|	If LPAREN expression RPAREN specify_edge_path_decl SEMICOLON { TUPLE7(STRING("specify_item5"),If,LPAREN,$3,RPAREN,$5,SEMICOLON) }
	|	Ifnone specify_simple_path_decl SEMICOLON { TUPLE4(STRING("specify_item6"),Ifnone,$2,SEMICOLON) }
	|	Ifnone specify_edge_path_decl SEMICOLON { TUPLE4(STRING("specify_item7"),Ifnone,$2,SEMICOLON) }
	|	DLR_fullskew LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE13(STRING("specify_item8"),DLR_fullskew,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,$10,RPAREN,SEMICOLON) }
	|	DLR_nochange LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE13(STRING("specify_item9"),DLR_nochange,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,$10,RPAREN,SEMICOLON) }
	|	DLR_recrem LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE13(STRING("specify_item10"),DLR_recrem,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,$10,RPAREN,SEMICOLON) }
	|	DLR_setuphold LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE13(STRING("specify_item11"),DLR_setuphold,LPAREN,$3,COMMA,$5,COMMA,$7,COMMA,$9,$10,RPAREN,SEMICOLON) }
	|	DLR_period LPAREN spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE9(STRING("specify_item12"),DLR_period,LPAREN,$3,COMMA,$5,$6,RPAREN,SEMICOLON) }
	|	DLR_width LPAREN spec_reference_event COMMA delay_value RPAREN SEMICOLON { TUPLE8(STRING("specify_item13"),DLR_width,LPAREN,$3,COMMA,$5,RPAREN,SEMICOLON) }
	|	DLR_hold LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item14"),DLR_hold,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	DLR_recovery LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item15"),DLR_recovery,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	DLR_removal LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item16"),DLR_removal,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	DLR_setup LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item17"),DLR_setup,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	DLR_skew LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item18"),DLR_skew,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	DLR_timeskew LPAREN spec_reference_event COMMA spec_reference_event COMMA delay_value spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item19"),DLR_timeskew,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	DLR_width LPAREN spec_reference_event COMMA delay_value COMMA expression spec_notifier_opt RPAREN SEMICOLON { TUPLE11(STRING("specify_item20"),DLR_width,LPAREN,$3,COMMA,$5,COMMA,$7,$8,RPAREN,SEMICOLON) }
	|	Pulsestyle_onevent specify_path_identifiers SEMICOLON { TUPLE4(STRING("specify_item21"),Pulsestyle_onevent,$2,SEMICOLON) }
	|	Pulsestyle_ondetect specify_path_identifiers SEMICOLON { TUPLE4(STRING("specify_item22"),Pulsestyle_ondetect,$2,SEMICOLON) }
	|	Showcancelled specify_path_identifiers SEMICOLON { TUPLE4(STRING("specify_item23"),Showcancelled,$2,SEMICOLON) }
	|	Noshowcancelled specify_path_identifiers SEMICOLON { TUPLE4(STRING("specify_item24"),Noshowcancelled,$2,SEMICOLON) }
	|	preprocessor_directive { ($1) }

specify_item_list: specify_item { CONS1 ($1) }
	|	specify_item_list specify_item { CONS2($1,$2) }

specify_item_list_opt: /* empty */ { EMPTY_TOKEN }
	|	specify_item_list { ($1) }

specify_edge_path_decl: specify_edge_path EQUALS LPAREN delay_value_list RPAREN { TUPLE6(STRING("specify_edge_path_decl1"),$1,EQUALS,LPAREN,$4,RPAREN) }
	|	specify_edge_path EQUALS delay_value_simple { TUPLE4(STRING("specify_edge_path_decl2"),$1,EQUALS,$3) }

edge_operator: Posedge { (Posedge) }
	|	Negedge { (Negedge) }
	|	Edge { (Edge) }

specify_edge_path: LPAREN specify_path_identifiers spec_polarity EQ_GT LPAREN specify_path_identifiers polarity_operator expression RPAREN RPAREN { TUPLE11(STRING("specify_edge_path1"),LPAREN,$2,$3,EQ_GT,LPAREN,$6,$7,$8,RPAREN,RPAREN) }
	|	LPAREN specify_path_identifiers spec_polarity STAR_GT LPAREN specify_path_identifiers polarity_operator expression RPAREN RPAREN { TUPLE11(STRING("specify_edge_path2"),LPAREN,$2,$3,STAR_GT,LPAREN,$6,$7,$8,RPAREN,RPAREN) }
	|	LPAREN edge_operator specify_path_identifiers spec_polarity EQ_GT LPAREN specify_path_identifiers polarity_operator expression RPAREN RPAREN { TUPLE12(STRING("specify_edge_path3"),LPAREN,$2,$3,$4,EQ_GT,LPAREN,$7,$8,$9,RPAREN,RPAREN) }
	|	LPAREN edge_operator specify_path_identifiers spec_polarity STAR_GT LPAREN specify_path_identifiers polarity_operator expression RPAREN RPAREN { TUPLE12(STRING("specify_edge_path4"),LPAREN,$2,$3,$4,STAR_GT,LPAREN,$7,$8,$9,RPAREN,RPAREN) }

polarity_operator: PLUS_COLON { (PLUS_COLON) }
	|	HYPHEN_COLON { (HYPHEN_COLON) }
	|	COLON { (COLON) }

specify_simple_path_decl: specify_simple_path EQUALS LPAREN delay_value_list RPAREN { TUPLE6(STRING("specify_simple_path_decl1"),$1,EQUALS,LPAREN,$4,RPAREN) }
	|	specify_simple_path EQUALS delay_value_simple { TUPLE4(STRING("specify_simple_path_decl2"),$1,EQUALS,$3) }

specify_simple_path: LPAREN specify_path_identifiers spec_polarity EQ_GT specify_path_identifiers RPAREN { TUPLE7(STRING("specify_simple_path1"),LPAREN,$2,$3,EQ_GT,$5,RPAREN) }
	|	LPAREN specify_path_identifiers spec_polarity STAR_GT specify_path_identifiers RPAREN { TUPLE7(STRING("specify_simple_path2"),LPAREN,$2,$3,STAR_GT,$5,RPAREN) }

specify_path_identifiers: GenericIdentifier { ($1) }
	|	GenericIdentifier LBRACK expr_primary RBRACK { TUPLE5(STRING("specify_path_identifiers2"),$1,LBRACK,$3,RBRACK) }
	|	specify_path_identifiers COMMA GenericIdentifier { TUPLE4(STRING("specify_path_identifiers3"),$1,COMMA,$3) }
	|	specify_path_identifiers COMMA GenericIdentifier LBRACK expr_primary RBRACK { TUPLE7(STRING("specify_path_identifiers4"),$1,COMMA,$3,LBRACK,$5,RBRACK) }

specparam: GenericIdentifier EQUALS expression { TUPLE4(STRING("specparam1"),$1,EQUALS,$3) }
	|	GenericIdentifier EQUALS expression COLON expression COLON expression { TUPLE8(STRING("specparam2"),$1,EQUALS,$3,COLON,$5,COLON,$7) }

specparam_list: specparam { CONS1 ($1) }
	|	specparam_list COMMA specparam { CONS3($1,COMMA,$3) }

specparam_decl: specparam_list { ($1) }
	|	decl_dimensions specparam_list { TUPLE3(STRING("specparam_decl2"),$1,$2) }

spec_polarity: PLUS { (PLUS) }
	|	HYPHEN { (HYPHEN) }
	|	/* empty */ { EMPTY_TOKEN }

spec_reference_event: edge_operator specify_terminal_descriptor { TUPLE3(STRING("spec_reference_event1"),$1,$2) }
	|	edge_operator specify_terminal_descriptor AMPERSAND_AMPERSAND_AMPERSAND expression { TUPLE5(STRING("spec_reference_event2"),$1,$2,AMPERSAND_AMPERSAND_AMPERSAND,$4) }
	|	Edge LBRACK edge_descriptor_list RBRACK specify_terminal_descriptor { TUPLE6(STRING("spec_reference_event3"),Edge,LBRACK,$3,RBRACK,$5) }
	|	Edge LBRACK edge_descriptor_list RBRACK specify_terminal_descriptor AMPERSAND_AMPERSAND_AMPERSAND expression { TUPLE8(STRING("spec_reference_event4"),Edge,LBRACK,$3,RBRACK,$5,AMPERSAND_AMPERSAND_AMPERSAND,$7) }
	|	specify_terminal_descriptor AMPERSAND_AMPERSAND_AMPERSAND expression { TUPLE4(STRING("spec_reference_event5"),$1,AMPERSAND_AMPERSAND_AMPERSAND,$3) }
	|	specify_terminal_descriptor { ($1) }

edge_descriptor_list: edge_descriptor_list COMMA TK_edge_descriptor { CONS3($1,COMMA,TK_edge_descriptor) }
	|	TK_edge_descriptor { CONS1 (TK_edge_descriptor) }

specify_terminal_descriptor: reference { ($1) }

spec_notifier_opt: spec_notifier { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

spec_notifier: COMMA { (COMMA) }
	|	COMMA reference { TUPLE3(STRING("spec_notifier2"),COMMA,$2) }
	|	spec_notifier COMMA { TUPLE3(STRING("spec_notifier3"),$1,COMMA) }
	|	spec_notifier COMMA reference { TUPLE4(STRING("spec_notifier4"),$1,COMMA,$3) }
	|	GenericIdentifier { ($1) }

case_any: Case { (Case) }
	|	Casex { (Casex) }
	|	Casez { (Casez) }

blocking_assignment: lpvalue EQUALS delay_or_event_control expression SEMICOLON { TUPLE6(STRING("blocking_assignment1"),$1,EQUALS,$3,$4,SEMICOLON) }
	|	lpvalue EQUALS dynamic_array_new SEMICOLON { TUPLE5(STRING("blocking_assignment2"),$1,EQUALS,$3,SEMICOLON) }
	|	lpvalue EQUALS class_new SEMICOLON { TUPLE5(STRING("blocking_assignment3"),$1,EQUALS,$3,SEMICOLON) }
	|	reference DOT builtin_array_method EQUALS delay_or_event_control expression SEMICOLON { TUPLE8(STRING("blocking_assignment4"),$1,DOT,$3,EQUALS,$5,$6,SEMICOLON) }
	|	reference DOT builtin_array_method EQUALS expression SEMICOLON { TUPLE7(STRING("blocking_assignment5"),$1,DOT,$3,EQUALS,$5,SEMICOLON) }

nonblocking_assignment: lpvalue LT_EQ delay_or_event_control_opt expression SEMICOLON { TUPLE6(STRING("nonblocking_assignment1"),$1,LT_EQ,$3,$4,SEMICOLON) }

clocking_drive_only: lpvalue LT_EQ cycle_delay expression SEMICOLON { TUPLE6(STRING("clocking_drive_only1"),$1,LT_EQ,$3,$4,SEMICOLON) }

procedural_continuous_assignment: Assign lpvalue EQUALS expression SEMICOLON { TUPLE6(STRING("procedural_continuous_assignment1"),Assign,$2,EQUALS,$4,SEMICOLON) }
	|	Assign macro_call_or_item { TUPLE3(STRING("procedural_continuous_assignment2"),Assign,$2) }
	|	Deassign lpvalue SEMICOLON { TUPLE4(STRING("procedural_continuous_assignment3"),Deassign,$2,SEMICOLON) }
	|	Force lpvalue EQUALS expression SEMICOLON { TUPLE6(STRING("procedural_continuous_assignment4"),Force,$2,EQUALS,$4,SEMICOLON) }
	|	Release lpvalue SEMICOLON { TUPLE4(STRING("procedural_continuous_assignment5"),Release,$2,SEMICOLON) }

case_statement: unique_priority_opt case_any LPAREN expression RPAREN case_items Endcase { TUPLE8(STRING("case_statement1"),$1,$2,LPAREN,$4,RPAREN,$6,Endcase) }
	|	unique_priority_opt case_any LPAREN expression RPAREN Matches case_pattern_items Endcase { TUPLE9(STRING("case_statement2"),$1,$2,LPAREN,$4,RPAREN,Matches,$7,Endcase) }
	|	unique_priority_opt case_any LPAREN expression RPAREN Inside case_inside_items Endcase { TUPLE9(STRING("case_statement3"),$1,$2,LPAREN,$4,RPAREN,Inside,$7,Endcase) }
	|	unique_priority_opt Randcase case_items Endcase { TUPLE5(STRING("case_statement4"),$1,Randcase,$3,Endcase) }

conditional_statement: unique_priority_opt If expression_in_parens statement_or_null { TUPLE5(STRING("conditional_statement1"),$1,If,$3,$4) }
	|	unique_priority_opt If expression_in_parens statement_or_null Else statement_or_null { TUPLE7(STRING("conditional_statement2"),$1,If,$3,$4,Else,$6) }

event_trigger: HYPHEN_GT_LPAREN_trigger_RPAREN reference SEMICOLON { TUPLE4(STRING("event_trigger1"),HYPHEN_GT_LPAREN_trigger_RPAREN,$2,SEMICOLON) }
	|	HYPHEN_GT_GT delay_or_event_control_opt reference SEMICOLON { TUPLE5(STRING("event_trigger2"),HYPHEN_GT_GT,$2,$3,SEMICOLON) }

repeat_control: Repeat LPAREN expression RPAREN { TUPLE5(STRING("repeat_control1"),Repeat,LPAREN,$3,RPAREN) }

delay_or_event_control: delay1 { ($1) }
	|	event_control { ($1) }
	|	repeat_control event_control { TUPLE3(STRING("delay_or_event_control3"),$1,$2) }

delay_or_event_control_opt: delay_or_event_control { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

par_block: Fork label_opt block_item_or_statement_or_null_list_opt join_keyword label_opt { TUPLE6(STRING("par_block1"),Fork,$2,$3,$4,$5) }

procedural_timing_control_statement: delay1 statement_or_null { TUPLE3(STRING("procedural_timing_control_statement1"),$1,$2) }
	|	event_control statement_or_null { TUPLE3(STRING("procedural_timing_control_statement2"),$1,$2) }
	|	cycle_delay statement_or_null { TUPLE3(STRING("procedural_timing_control_statement3"),$1,$2) }

seq_block: begin_rule block_item_or_statement_or_null_list_opt end_rule { TUPLE4(STRING("seq_block1"),$1,$2,$3) }

wait_statement: Wait LPAREN expression RPAREN statement_or_null { TUPLE6(STRING("wait_statement1"),Wait,LPAREN,$3,RPAREN,$5) }
	|	Wait Fork SEMICOLON { TUPLE4(STRING("wait_statement2"),Wait,Fork,SEMICOLON) }

statement_item: blocking_assignment { ($1) }
	|	nonblocking_assignment { ($1) }
	|	procedural_continuous_assignment { ($1) }
	|	case_statement { ($1) }
	|	conditional_statement { ($1) }
	|	assignment_statement SEMICOLON { TUPLE3(STRING("statement_item6"),$1,SEMICOLON) }
	|	disable_statement { ($1) }
	|	event_trigger { ($1) }
	|	loop_statement { ($1) }
	|	jump_statement { ($1) }
	|	par_block { ($1) }
	|	procedural_timing_control_statement { ($1) }
	|	seq_block { ($1) }
	|	wait_statement { ($1) }
	|	procedural_assertion_statement { ($1) }
	|	expect_property_statement { ($1) }
	|	clocking_drive_only { ($1) }
	|	randsequence_statement { ($1) }
	|	subroutine_call SEMICOLON { TUPLE3(STRING("statement_item19"),$1,SEMICOLON) }
	|	randomize_call SEMICOLON { TUPLE3(STRING("statement_item20"),$1,SEMICOLON) }
	|	Void QUOTE LPAREN expression RPAREN SEMICOLON { TUPLE7(STRING("statement_item21"),Void,QUOTE,LPAREN,$4,RPAREN,SEMICOLON) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("statement_item22"),ERROR_TOKEN,SEMICOLON) }
	|	MacroGenericItem { ($1) }
	|	preprocessor_balanced_statements { ($1) }
	|	preprocessor_action { ($1) }

preprocessor_balanced_statements: preprocessor_if_header block_item_or_statement_or_null_list_opt preprocessor_elsif_statements_opt preprocessor_else_statement_opt BACKQUOTE_endif { TUPLE6(STRING("preprocessor_balanced_statements1"),$1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_statements_opt: preprocessor_elsif_statements { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_statements: preprocessor_elsif_statements preprocessor_elsif_statement { TUPLE3(STRING("preprocessor_elsif_statements1"),$1,$2) }
	|	preprocessor_elsif_statement { ($1) }

preprocessor_elsif_statement: preprocessor_elsif_header block_item_or_statement_or_null_list_opt { TUPLE3(STRING("preprocessor_elsif_statement1"),$1,$2) }

preprocessor_else_statement_opt: preprocessor_else_statement { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_statement: BACKQUOTE_else block_item_or_statement_or_null_list_opt { TUPLE3(STRING("preprocessor_else_statement1"),BACKQUOTE_else,$2) }

disable_statement: Disable reference SEMICOLON { TUPLE4(STRING("disable_statement1"),Disable,$2,SEMICOLON) }
	|	Disable Fork SEMICOLON { TUPLE4(STRING("disable_statement2"),Disable,Fork,SEMICOLON) }

assign_modify_statement: lpvalue PLUS_EQ expression { TUPLE4(STRING("assign_modify_statement1"),$1,PLUS_EQ,$3) }
	|	lpvalue HYPHEN_EQ expression { TUPLE4(STRING("assign_modify_statement2"),$1,HYPHEN_EQ,$3) }
	|	lpvalue STAR_EQ expression { TUPLE4(STRING("assign_modify_statement3"),$1,STAR_EQ,$3) }
	|	lpvalue SLASH_EQ expression { TUPLE4(STRING("assign_modify_statement4"),$1,SLASH_EQ,$3) }
	|	lpvalue PERCENT_EQ expression { TUPLE4(STRING("assign_modify_statement5"),$1,PERCENT_EQ,$3) }
	|	lpvalue AMPERSAND_EQ expression { TUPLE4(STRING("assign_modify_statement6"),$1,AMPERSAND_EQ,$3) }
	|	lpvalue VBAR_EQ expression { TUPLE4(STRING("assign_modify_statement7"),$1,VBAR_EQ,$3) }
	|	lpvalue CARET_EQ expression { TUPLE4(STRING("assign_modify_statement8"),$1,CARET_EQ,$3) }
	|	lpvalue TK_LS_EQ expression { TUPLE4(STRING("assign_modify_statement9"),$1,TK_LS_EQ,$3) }
	|	lpvalue TK_RS_EQ expression { TUPLE4(STRING("assign_modify_statement10"),$1,TK_RS_EQ,$3) }
	|	lpvalue TK_RSS_EQ expression { TUPLE4(STRING("assign_modify_statement11"),$1,TK_RSS_EQ,$3) }

unique_priority_opt: Unique { (Unique) }
	|	Unique0 { (Unique0) }
	|	Priority { (Priority) }
	|	/* empty */ { EMPTY_TOKEN }

statement_or_null_list_opt: statement_or_null_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

statement_or_null_list: statement_or_null_list statement_or_null { CONS2($1,$2) }
	|	statement_or_null { CONS1 ($1) }

analog_statement: branch_probe_expression LT_PLUS expression SEMICOLON { TUPLE5(STRING("analog_statement1"),$1,LT_PLUS,$3,SEMICOLON) }

task_item: function_item_data_declaration { ($1) }
	|	net_type_declaration { ($1) }
	|	package_import_declaration { ($1) }
	|	any_param_declaration { ($1) }
	|	type_declaration { ($1) }
	|	let_declaration { ($1) }
	|	tf_port_declaration { ($1) }

tf_item_or_statement_or_null: task_item { ($1) }
	|	statement_or_null { ($1) }

tf_item_or_statement_or_null_list: tf_item_or_statement_or_null { CONS1 ($1) }
	|	tf_item_or_statement_or_null_list tf_item_or_statement_or_null { CONS2($1,$2) }

tf_item_or_statement_or_null_list_opt: tf_item_or_statement_or_null_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

tf_port_list_paren_opt: LPAREN tf_port_list_opt RPAREN { TUPLE4(STRING("tf_port_list_paren_opt1"),LPAREN,$2,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

tf_port_list_opt: tf_port_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

udp_body: Table udp_entry_list Endtable { TUPLE4(STRING("udp_body1"),Table,$2,Endtable) }
	|	Table Endtable { TUPLE3(STRING("udp_body2"),Table,Endtable) }

udp_entry_list: udp_comb_entry_list { CONS1 ($1) }
	|	udp_sequ_entry_list { CONS1 ($1) }
	|	udp_unknown_list { CONS1 ($1) }

udp_unknown_list: udp_unknown_list preprocessor_directive { CONS2($1,$2) }
	|	preprocessor_directive { CONS1 ($1) }

udp_comb_entry: udp_input_list COLON udp_output_sym SEMICOLON { TUPLE5(STRING("udp_comb_entry1"),$1,COLON,$3,SEMICOLON) }

udp_comb_entry_list: udp_comb_entry { CONS1 ($1) }
	|	udp_comb_entry_list udp_comb_entry { CONS2($1,$2) }
	|	udp_comb_entry_list preprocessor_directive { CONS2($1,$2) }
	|	udp_unknown_list udp_comb_entry { CONS2($1,$2) }

udp_sequ_entry_list: udp_sequ_entry { CONS1 ($1) }
	|	udp_sequ_entry_list udp_sequ_entry { CONS2($1,$2) }
	|	udp_sequ_entry_list preprocessor_directive { CONS2($1,$2) }
	|	udp_unknown_list udp_sequ_entry { CONS2($1,$2) }

udp_sequ_entry: udp_input_list COLON udp_input_sym COLON udp_output_sym SEMICOLON { TUPLE7(STRING("udp_sequ_entry1"),$1,COLON,$3,COLON,$5,SEMICOLON) }

udp_initial: Initial GenericIdentifier EQUALS number SEMICOLON { TUPLE6(STRING("udp_initial1"),Initial,$2,EQUALS,$4,SEMICOLON) }

udp_init_opt: udp_initial { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

udp_input_list: udp_input_sym { CONS1 ($1) }
	|	udp_input_list udp_input_sym { CONS2($1,$2) }

udp_input_sym: CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	QUERY { (QUERY) }
	|	CHAR { (CHAR) }
	|	STAR { (STAR) }
	|	PERCENT { (PERCENT) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	UNDERSCORE { (UNDERSCORE) }
	|	PLUS { (PLUS) }
	|	TK_DecNumber { (TK_DecNumber $1) }

udp_output_sym: CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	CHAR { (CHAR) }
	|	HYPHEN { (HYPHEN) }
	|	TK_DecNumber { (TK_DecNumber $1) }

udp_port_decl: Input list_of_identifiers SEMICOLON { TUPLE4(STRING("udp_port_decl1"),Input,$2,SEMICOLON) }
	|	Output GenericIdentifier SEMICOLON { TUPLE4(STRING("udp_port_decl2"),Output,$2,SEMICOLON) }
	|	Reg GenericIdentifier SEMICOLON { TUPLE4(STRING("udp_port_decl3"),Reg,$2,SEMICOLON) }
	|	Reg Output GenericIdentifier SEMICOLON { TUPLE5(STRING("udp_port_decl4"),Reg,Output,$3,SEMICOLON) }

udp_port_decls: udp_port_decl { ($1) }
	|	udp_port_decls udp_port_decl { TUPLE3(STRING("udp_port_decls2"),$1,$2) }

udp_port_list: GenericIdentifier { CONS1 ($1) }
	|	udp_port_list COMMA GenericIdentifier { CONS3($1,COMMA,$3) }

udp_initial_expr_opt: EQUALS expression { TUPLE3(STRING("udp_initial_expr_opt1"),EQUALS,$2) }
	|	/* empty */ { EMPTY_TOKEN }

udp_input_declaration_list: Input GenericIdentifier { CONS2(Input,$2) }
	|	udp_input_declaration_list COMMA Input GenericIdentifier { CONS4($1,COMMA,Input,$4) }

udp_primitive: Primitive GenericIdentifier LPAREN udp_port_list RPAREN SEMICOLON udp_port_decls udp_init_opt udp_body Endprimitive label_opt { TUPLE12(STRING("udp_primitive1"),Primitive,$2,LPAREN,$4,RPAREN,SEMICOLON,$7,$8,$9,Endprimitive,$11) }
	|	Primitive GenericIdentifier LPAREN Output TK_reg_opt GenericIdentifier udp_initial_expr_opt COMMA udp_input_declaration_list RPAREN SEMICOLON udp_body Endprimitive label_opt { TUPLE15(STRING("udp_primitive2"),Primitive,$2,LPAREN,Output,$5,$6,$7,COMMA,$9,RPAREN,SEMICOLON,$12,Endprimitive,$14) }

lifetime: Automatic { (Automatic) }
	|	Static { (Static) }

lifetime_opt: lifetime { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

TK_reg_opt: Reg { (Reg) }
	|	/* empty */ { EMPTY_TOKEN }

TK_static_opt: Static { (Static) }
	|	/* empty */ { EMPTY_TOKEN }

TK_tagged_opt: Tagged { (Tagged) }
	|	/* empty */ { EMPTY_TOKEN }

TK_virtual_opt: Virtual { (Virtual) }
	|	/* empty */ { EMPTY_TOKEN }

bind_directive: Bind reference COLON bind_target_instance_list bind_instantiation SEMICOLON { TUPLE7(STRING("bind_directive1"),Bind,$2,COLON,$4,$5,SEMICOLON) }
	|	Bind reference bind_instantiation SEMICOLON { TUPLE5(STRING("bind_directive2"),Bind,$2,$3,SEMICOLON) }

bind_target_instance_list: bind_target_instance_list COMMA bind_target_instance { CONS3($1,COMMA,$3) }
	|	bind_target_instance { CONS1 ($1) }

bind_target_instance: reference { ($1) }

bind_instantiation: class_id gate_instance_or_register_variable_list { TUPLE3(STRING("bind_instantiation1"),$1,$2) }

clocking_declaration: Default Clocking identifier_opt event_control SEMICOLON clocking_item_list_opt Endclocking label_opt { TUPLE9(STRING("clocking_declaration1"),Default,Clocking,$3,$4,SEMICOLON,$6,Endclocking,$8) }
	|	Clocking identifier_opt event_control SEMICOLON clocking_item_list_opt Endclocking label_opt { TUPLE8(STRING("clocking_declaration2"),Clocking,$2,$3,SEMICOLON,$5,Endclocking,$7) }
	|	Global Clocking identifier_opt event_control SEMICOLON Endclocking label_opt { TUPLE8(STRING("clocking_declaration3"),Global,Clocking,$3,$4,SEMICOLON,Endclocking,$7) }

clocking_item_list_opt: clocking_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

clocking_item_list: clocking_item_list clocking_item { CONS2($1,$2) }
	|	clocking_item { CONS1 ($1) }

clocking_item: Default default_skew SEMICOLON { TUPLE4(STRING("clocking_item1"),Default,$2,SEMICOLON) }
	|	clocking_direction list_of_clocking_decl_assign SEMICOLON { TUPLE4(STRING("clocking_item2"),$1,$2,SEMICOLON) }
	|	assertion_item_declaration { ($1) }
	|	let_declaration { ($1) }

default_skew: Input clocking_skew { TUPLE3(STRING("default_skew1"),Input,$2) }
	|	Input clocking_skew Output clocking_skew { TUPLE5(STRING("default_skew2"),Input,$2,Output,$4) }
	|	Output clocking_skew { TUPLE3(STRING("default_skew3"),Output,$2) }

clocking_skew_opt: clocking_skew { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

clocking_skew: edge_operator delay3_opt { TUPLE3(STRING("clocking_skew1"),$1,$2) }
	|	delay3 { ($1) }

clocking_direction: Input clocking_skew_opt { TUPLE3(STRING("clocking_direction1"),Input,$2) }
	|	Input clocking_skew_opt Output clocking_skew_opt { TUPLE5(STRING("clocking_direction2"),Input,$2,Output,$4) }
	|	Output clocking_skew_opt { TUPLE3(STRING("clocking_direction3"),Output,$2) }
	|	Inout { (Inout) }

list_of_clocking_decl_assign: list_of_clocking_decl_assign COMMA clocking_decl_assign { CONS3($1,COMMA,$3) }
	|	clocking_decl_assign { CONS1 ($1) }

clocking_decl_assign: GenericIdentifier trailing_assign_opt { TUPLE3(STRING("clocking_decl_assign1"),$1,$2) }

assertion_item_declaration: property_declaration { ($1) }
	|	sequence_declaration { ($1) }

let_declaration: Let GenericIdentifier let_port_list_in_parens_opt EQUALS expression SEMICOLON { TUPLE7(STRING("let_declaration1"),Let,$2,$3,EQUALS,$5,SEMICOLON) }

let_port_list_in_parens_opt: LPAREN let_port_list RPAREN { TUPLE4(STRING("let_port_list_in_parens_opt1"),LPAREN,$2,RPAREN) }
	|	LPAREN RPAREN { TUPLE3(STRING("let_port_list_in_parens_opt2"),LPAREN,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

let_port_list: let_port_list COMMA let_port_item { CONS3($1,COMMA,$3) }
	|	let_port_item { CONS1 ($1) }

let_port_item: let_formal_type_followed_by_id decl_dimensions_opt { TUPLE3(STRING("let_port_item1"),$1,$2) }
	|	let_formal_type_followed_by_id decl_dimensions_opt EQUALS expression { TUPLE5(STRING("let_port_item2"),$1,$2,EQUALS,$4) }

let_formal_type_followed_by_id: data_type_or_implicit_basic_followed_by_id { ($1) }
	|	Untyped GenericIdentifier { TUPLE3(STRING("let_formal_type_followed_by_id2"),Untyped,$2) }

sequence_declaration: Sequence GenericIdentifier sequence_port_list_in_parens_opt SEMICOLON assertion_variable_declaration_list SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN sequence_expr optional_semicolon Endsequence label_opt { TUPLE11(STRING("sequence_declaration1"),Sequence,$2,$3,SEMICOLON,$5,SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN,$7,$8,Endsequence,$10) }
	|	Sequence GenericIdentifier sequence_port_list_in_parens_opt SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN sequence_expr optional_semicolon Endsequence label_opt { TUPLE9(STRING("sequence_declaration2"),Sequence,$2,$3,SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN,$5,$6,Endsequence,$8) }

sequence_port_list_in_parens_opt: LPAREN sequence_port_list_opt RPAREN { TUPLE4(STRING("sequence_port_list_in_parens_opt1"),LPAREN,$2,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

sequence_port_list_opt: sequence_port_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

sequence_port_list: sequence_port_list COMMA sequence_port_item { CONS3($1,COMMA,$3) }
	|	sequence_port_item { CONS1 ($1) }

sequence_port_item: local_sequence_lvar_port_direction_opt sequence_port_type_followed_by_id decl_dimensions_opt trailing_assign_opt { TUPLE5(STRING("sequence_port_item1"),$1,$2,$3,$4) }

local_sequence_lvar_port_direction_opt: Local dir { TUPLE3(STRING("local_sequence_lvar_port_direction_opt1"),Local,$2) }
	|	Local { (Local) }
	|	/* empty */ { EMPTY_TOKEN }

sequence_port_type_followed_by_id: data_type_or_implicit_basic_followed_by_id { ($1) }
	|	Sequence GenericIdentifier { TUPLE3(STRING("sequence_port_type_followed_by_id2"),Sequence,$2) }
	|	Untyped GenericIdentifier { TUPLE3(STRING("sequence_port_type_followed_by_id3"),Untyped,$2) }

property_declaration: Property GenericIdentifier property_port_list_in_parens_opt SEMICOLON assertion_variable_declaration_list SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN property_spec optional_semicolon Endproperty label_opt { TUPLE11(STRING("property_declaration1"),Property,$2,$3,SEMICOLON,$5,SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN,$7,$8,Endproperty,$10) }
	|	Property GenericIdentifier property_port_list_in_parens_opt SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN property_spec optional_semicolon Endproperty label_opt { TUPLE9(STRING("property_declaration2"),Property,$2,$3,SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN,$5,$6,Endproperty,$8) }

property_port_list_in_parens_opt: LPAREN property_port_list RPAREN { TUPLE4(STRING("property_port_list_in_parens_opt1"),LPAREN,$2,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

property_port_list: property_port_list COMMA property_port_item { CONS3($1,COMMA,$3) }
	|	property_port_item { CONS1 ($1) }

property_port_item: property_port_modifiers_opt property_formal_type_followed_by_id decl_dimensions_opt property_actual_arg_opt { TUPLE5(STRING("property_port_item1"),$1,$2,$3,$4) }

property_port_modifiers_opt: Local Input { TUPLE3(STRING("property_port_modifiers_opt1"),Local,Input) }
	|	Local { (Local) }
	|	/* empty */ { EMPTY_TOKEN }

property_formal_type_followed_by_id: data_type_or_implicit_basic_followed_by_id { ($1) }
	|	Sequence GenericIdentifier { TUPLE3(STRING("property_formal_type_followed_by_id2"),Sequence,$2) }
	|	Untyped GenericIdentifier { TUPLE3(STRING("property_formal_type_followed_by_id3"),Untyped,$2) }
	|	Property GenericIdentifier { TUPLE3(STRING("property_formal_type_followed_by_id4"),Property,$2) }

property_actual_arg_opt: EQUALS property_actual_arg { TUPLE3(STRING("property_actual_arg_opt1"),EQUALS,$2) }
	|	/* empty */ { EMPTY_TOKEN }

assertion_variable_declaration_list: assertion_variable_declaration_list SEMICOLON assertion_variable_declaration { CONS3($1,SEMICOLON,$3) }
	|	assertion_variable_declaration { CONS1 ($1) }

assertion_variable_declaration: var_opt data_type_or_implicit_basic_followed_by_id_and_dimensions_opt trailing_assign_opt { TUPLE4(STRING("assertion_variable_declaration1"),$1,$2,$3) }
	|	var_opt data_type_or_implicit_basic_followed_by_id_and_dimensions_opt trailing_assign_opt COMMA net_variable_or_decl_assigns { TUPLE6(STRING("assertion_variable_declaration2"),$1,$2,$3,COMMA,$5) }

property_actual_arg: edge_operator expression { TUPLE3(STRING("property_actual_arg1"),$1,$2) }
	|	property_expr { ($1) }

property_expr: sequence_expr { ($1) }

sequence_expr: property_implication_expr { ($1) }

property_prefix_expr: Accept_on LPAREN expression_or_dist RPAREN property_prefix_expr { TUPLE6(STRING("property_prefix_expr1"),Accept_on,LPAREN,$3,RPAREN,$5) }
	|	Reject_on LPAREN expression_or_dist RPAREN property_prefix_expr { TUPLE6(STRING("property_prefix_expr2"),Reject_on,LPAREN,$3,RPAREN,$5) }
	|	Sync_accept_on LPAREN expression_or_dist RPAREN property_prefix_expr { TUPLE6(STRING("property_prefix_expr3"),Sync_accept_on,LPAREN,$3,RPAREN,$5) }
	|	Sync_reject_on LPAREN expression_or_dist RPAREN property_prefix_expr { TUPLE6(STRING("property_prefix_expr4"),Sync_reject_on,LPAREN,$3,RPAREN,$5) }
	|	Nexttime property_prefix_expr { TUPLE3(STRING("property_prefix_expr5"),Nexttime,$2) }
	|	Nexttime LBRACK expression RBRACK property_prefix_expr { TUPLE6(STRING("property_prefix_expr6"),Nexttime,LBRACK,$3,RBRACK,$5) }
	|	S_nexttime property_prefix_expr { TUPLE3(STRING("property_prefix_expr7"),S_nexttime,$2) }
	|	S_nexttime LBRACK expression RBRACK property_prefix_expr { TUPLE6(STRING("property_prefix_expr8"),S_nexttime,LBRACK,$3,RBRACK,$5) }
	|	Always property_prefix_expr { TUPLE3(STRING("property_prefix_expr9"),Always,$2) }
	|	Always LBRACK cycle_range RBRACK property_prefix_expr { TUPLE6(STRING("property_prefix_expr10"),Always,LBRACK,$3,RBRACK,$5) }
	|	S_always LBRACK cycle_range RBRACK property_prefix_expr { TUPLE6(STRING("property_prefix_expr11"),S_always,LBRACK,$3,RBRACK,$5) }
	|	S_eventually property_prefix_expr { TUPLE3(STRING("property_prefix_expr12"),S_eventually,$2) }
	|	Eventually LBRACK cycle_range RBRACK property_prefix_expr { TUPLE6(STRING("property_prefix_expr13"),Eventually,LBRACK,$3,RBRACK,$5) }
	|	S_eventually LBRACK cycle_range RBRACK property_prefix_expr { TUPLE6(STRING("property_prefix_expr14"),S_eventually,LBRACK,$3,RBRACK,$5) }
	|	property_if_else_expr { ($1) }

property_if_else_expr: If LPAREN expression_or_dist RPAREN property_prefix_expr Else property_prefix_expr { TUPLE8(STRING("property_if_else_expr1"),If,LPAREN,$3,RPAREN,$5,Else,$7) }
	|	If LPAREN expression_or_dist RPAREN property_prefix_expr { TUPLE6(STRING("property_if_else_expr2"),If,LPAREN,$3,RPAREN,$5) }
	|	simple_sequence_expr { ($1) }

simple_sequence_expr: First_match LPAREN sequence_expr_match_item_list RPAREN { TUPLE5(STRING("simple_sequence_expr1"),First_match,LPAREN,$3,RPAREN) }
	|	property_case_statement { ($1) }
	|	Strong LPAREN sequence_expr RPAREN { TUPLE5(STRING("simple_sequence_expr3"),Strong,LPAREN,$3,RPAREN) }
	|	Weak LPAREN sequence_expr RPAREN { TUPLE5(STRING("simple_sequence_expr4"),Weak,LPAREN,$3,RPAREN) }
	|	sequence_or_expr { ($1) }

property_case_statement: Case LPAREN expression_or_dist RPAREN property_case_item_list optional_semicolon Endcase { TUPLE8(STRING("property_case_statement1"),Case,LPAREN,$3,RPAREN,$5,$6,Endcase) }

property_case_item_list: property_case_item_list SEMICOLON property_case_item { CONS3($1,SEMICOLON,$3) }
	|	property_case_item { CONS1 ($1) }

property_case_item: expression_or_dist_list COLON property_expr { TUPLE4(STRING("property_case_item1"),$1,COLON,$3) }
	|	Default COLON property_expr { TUPLE4(STRING("property_case_item2"),Default,COLON,$3) }
	|	Default property_expr { TUPLE3(STRING("property_case_item3"),Default,$2) }

expression_or_dist_list: expression_or_dist_list COMMA expression_or_dist { CONS3($1,COMMA,$3) }
	|	expression_or_dist { CONS1 ($1) }

property_implication_expr: property_implication_expr property_operator property_prefix_expr { TUPLE4(STRING("property_implication_expr1"),$1,$2,$3) }
	|	property_prefix_expr { ($1) }

property_operator: implication_operator { ($1) }
	|	until_operator { ($1) }
	|	followed_by_operator { ($1) }

implication_operator: VBAR_HYPHEN_GT { (VBAR_HYPHEN_GT) }
	|	VBAR_EQ_GT { (VBAR_EQ_GT) }
	|	Implies { (Implies) }
	|	Iff { (Iff) }

until_operator: Until { (Until) }
	|	Until_with { (Until_with) }
	|	S_until { (S_until) }
	|	S_until_with { (S_until_with) }

followed_by_operator: HASH_EQ_HASH { (HASH_EQ_HASH) }
	|	HASH_HYPHEN_HASH { (HASH_HYPHEN_HASH) }

system_tf_call: SystemTFIdentifier call_base { TUPLE3(STRING("system_tf_call1"),SystemTFIdentifier $1,$2) }
	|	SystemTFIdentifier { (SystemTFIdentifier $1) }

sequence_match_item_list: sequence_match_item_list COMMA sequence_match_item { CONS3($1,COMMA,$3) }
	|	sequence_match_item { CONS1 ($1) }

sequence_expr_match_item_list: property_expr COMMA sequence_match_item_list { CONS3($1,COMMA,$3) }
	|	property_expr { CONS1 ($1) }

sequence_match_item: assignment_statement { ($1) }
	|	subroutine_call { ($1) }
	|	reference_or_call { ($1) }

subroutine_call: system_tf_call { ($1) }

sequence_or_expr: sequence_or_expr Or sequence_and_expr { TUPLE4(STRING("sequence_or_expr1"),$1,Or,$3) }
	|	sequence_and_expr { ($1) }

sequence_and_expr: sequence_and_expr And sequence_unary_expr { TUPLE4(STRING("sequence_and_expr1"),$1,And,$3) }
	|	sequence_unary_expr { ($1) }

sequence_unary_expr: sequence_intersect_expr { ($1) }
	|	Not sequence_intersect_expr { TUPLE3(STRING("sequence_unary_expr2"),Not,$2) }

sequence_intersect_expr: sequence_within_expr { ($1) }
	|	sequence_intersect_expr Intersect sequence_within_expr { TUPLE4(STRING("sequence_intersect_expr2"),$1,Intersect,$3) }

sequence_within_expr: sequence_throughout_expr { ($1) }
	|	sequence_within_expr Within sequence_throughout_expr { TUPLE4(STRING("sequence_within_expr2"),$1,Within,$3) }

sequence_throughout_expr: sequence_delay_range_expr { ($1) }
	|	sequence_throughout_expr Throughout sequence_delay_range_expr { TUPLE4(STRING("sequence_throughout_expr2"),$1,Throughout,$3) }

sequence_delay_range_expr: sequence_delay_repetition_list { ($1) }
	|	cycle_delay_range sequence_delay_repetition_list { TUPLE3(STRING("sequence_delay_range_expr2"),$1,$2) }

sequence_delay_repetition_list: sequence_delay_repetition_list cycle_delay_range sequence_expr_primary { CONS3($1,$2,$3) }
	|	sequence_expr_primary { CONS1 ($1) }

cycle_delay: HASH_HASH TK_DecNumber { TUPLE3(STRING("cycle_delay1"),HASH_HASH,TK_DecNumber $2) }
	|	HASH_HASH GenericIdentifier { TUPLE3(STRING("cycle_delay2"),HASH_HASH,$2) }
	|	HASH_HASH LPAREN expression RPAREN { TUPLE5(STRING("cycle_delay3"),HASH_HASH,LPAREN,$3,RPAREN) }

cycle_delay_range: HASH_HASH LBRACK cycle_range RBRACK { TUPLE5(STRING("cycle_delay_range1"),HASH_HASH,LBRACK,$3,RBRACK) }
	|	HASH_HASH LBRACK_STAR_RBRACK { TUPLE3(STRING("cycle_delay_range2"),HASH_HASH,LBRACK_STAR_RBRACK) }
	|	HASH_HASH LBRACK_PLUS_RBRACK { TUPLE3(STRING("cycle_delay_range3"),HASH_HASH,LBRACK_PLUS_RBRACK) }
	|	HASH_HASH LPAREN expression RPAREN { TUPLE5(STRING("cycle_delay_range4"),HASH_HASH,LPAREN,$3,RPAREN) }
	|	HASH_HASH GenericIdentifier { TUPLE3(STRING("cycle_delay_range5"),HASH_HASH,$2) }
	|	HASH_HASH TK_DecNumber { TUPLE3(STRING("cycle_delay_range6"),HASH_HASH,TK_DecNumber $2) }

cycle_range_or_expr: cycle_range { ($1) }
	|	expression { ($1) }

cycle_range: expression COLON expression { TUPLE4(STRING("cycle_range1"),$1,COLON,$3) }

dist_opt: Dist LBRACE dist_list RBRACE { TUPLE5(STRING("dist_opt1"),Dist,LBRACE,$3,RBRACE) }
	|	/* empty */ { EMPTY_TOKEN }

dist_list: dist_item { CONS1 ($1) }
	|	dist_list COMMA dist_item { CONS3($1,COMMA,$3) }

dist_item: value_range dist_weight { TUPLE3(STRING("dist_item1"),$1,$2) }
	|	value_range { ($1) }

dist_weight: COLON_EQ expression { TUPLE3(STRING("dist_weight1"),COLON_EQ,$2) }
	|	COLON_SLASH expression { TUPLE3(STRING("dist_weight2"),COLON_SLASH,$2) }

sequence_expr_primary: sequence_repetition_expr { ($1) }

sequence_repetition_expr: expression_or_dist boolean_abbrev_opt { TUPLE3(STRING("sequence_repetition_expr1"),$1,$2) }

expression_or_dist: expression dist_opt { TUPLE3(STRING("expression_or_dist1"),$1,$2) }

boolean_abbrev_opt: boolean_abbrev { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

boolean_abbrev: consecutive_repetition { ($1) }
	|	nonconsecutive_repetition { ($1) }
	|	goto_repetition { ($1) }

consecutive_repetition: LBRACK_STAR cycle_range_or_expr RBRACK { TUPLE4(STRING("consecutive_repetition1"),LBRACK_STAR,$2,RBRACK) }
	|	LBRACK_STAR_RBRACK { (LBRACK_STAR_RBRACK) }
	|	LBRACK_PLUS_RBRACK { (LBRACK_PLUS_RBRACK) }

nonconsecutive_repetition: LBRACK_EQ cycle_range_or_expr RBRACK { TUPLE4(STRING("nonconsecutive_repetition1"),LBRACK_EQ,$2,RBRACK) }

goto_repetition: LBRACK_HYPHEN_GT cycle_range_or_expr RBRACK { TUPLE4(STRING("goto_repetition1"),LBRACK_HYPHEN_GT,$2,RBRACK) }

covergroup_declaration: Covergroup GenericIdentifier tf_port_list_paren_opt coverage_event_opt SEMICOLON coverage_spec_or_option_list_opt Endgroup label_opt { TUPLE9(STRING("covergroup_declaration1"),Covergroup,$2,$3,$4,SEMICOLON,$6,Endgroup,$8) }

coverage_spec_or_option_list_opt: coverage_spec_or_option_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

coverage_spec_or_option_list: coverage_spec_or_option_list coverage_spec_or_option { CONS2($1,$2) }
	|	coverage_spec_or_option { CONS1 ($1) }

coverage_spec_or_option: coverage_spec { ($1) }
	|	coverage_option SEMICOLON { TUPLE3(STRING("coverage_spec_or_option2"),$1,SEMICOLON) }
	|	preprocessor_directive { ($1) }
	|	macro_call_or_item { ($1) }

coverage_option: Option DOT GenericIdentifier EQUALS expression { TUPLE6(STRING("coverage_option1"),Option,DOT,$3,EQUALS,$5) }
	|	Type_option DOT GenericIdentifier EQUALS expression { TUPLE6(STRING("coverage_option2"),Type_option,DOT,$3,EQUALS,$5) }

coverage_spec: cover_point { ($1) }
	|	cover_cross { ($1) }

coverage_event_opt: coverage_event { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

coverage_event: event_control { ($1) }
	|	With_LPAREN_covergroup_RPAREN Function Sample LPAREN tf_port_list_opt RPAREN { TUPLE7(STRING("coverage_event2"),With_LPAREN_covergroup_RPAREN,Function,Sample,LPAREN,$5,RPAREN) }
	|	AT_AT LPAREN block_event_expression RPAREN { TUPLE5(STRING("coverage_event3"),AT_AT,LPAREN,$3,RPAREN) }

block_event_expression: block_event_or_expr { ($1) }

block_event_or_expr: block_event_or_expr Or block_event_expr_primary { TUPLE4(STRING("block_event_or_expr1"),$1,Or,$3) }
	|	block_event_expr_primary { ($1) }

block_event_expr_primary: Begin reference { TUPLE3(STRING("block_event_expr_primary1"),Begin,$2) }
	|	End reference { TUPLE3(STRING("block_event_expr_primary2"),End,$2) }

cover_cross: data_type_or_implicit_basic_followed_by_id COLON Cross cross_item_list iff_expr_opt cross_body { TUPLE7(STRING("cover_cross1"),$1,COLON,Cross,$4,$5,$6) }
	|	Cross cross_item_list iff_expr_opt cross_body { TUPLE5(STRING("cover_cross2"),Cross,$2,$3,$4) }

cross_body: LBRACE cross_body_item_list_opt RBRACE { TUPLE4(STRING("cross_body1"),LBRACE,$2,RBRACE) }
	|	SEMICOLON { (SEMICOLON) }

cross_body_item_list_opt: cross_body_item_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

cross_body_item_list: cross_body_item_list cross_body_item { CONS2($1,$2) }
	|	cross_body_item { CONS1 ($1) }

cross_item_list: cross_item_list COMMA cross_item { CONS3($1,COMMA,$3) }
	|	cross_item COMMA cross_item { CONS3($1,COMMA,$3) }

cross_body_item: function_declaration { ($1) }
	|	bins_selection SEMICOLON { TUPLE3(STRING("cross_body_item2"),$1,SEMICOLON) }
	|	coverage_option SEMICOLON { TUPLE3(STRING("cross_body_item3"),$1,SEMICOLON) }

cross_item: reference { ($1) }

cover_point: data_type_or_implicit_basic_followed_by_id COLON Coverpoint property_expr bins_or_options_list_opt { TUPLE6(STRING("cover_point1"),$1,COLON,Coverpoint,$4,$5) }
	|	Coverpoint property_expr bins_or_options_list_opt { TUPLE4(STRING("cover_point2"),Coverpoint,$2,$3) }

iff_expr_opt: Iff LPAREN expression RPAREN { TUPLE5(STRING("iff_expr_opt1"),Iff,LPAREN,$3,RPAREN) }
	|	/* empty */ { EMPTY_TOKEN }

bins_or_options_list_opt: LBRACE bins_or_options_list RBRACE { TUPLE4(STRING("bins_or_options_list_opt1"),LBRACE,$2,RBRACE) }
	|	SEMICOLON { (SEMICOLON) }
	|	LBRACE ERROR_TOKEN RBRACE { TUPLE4(STRING("bins_or_options_list_opt3"),LBRACE,ERROR_TOKEN,RBRACE) }

bins_or_options_list: bins_or_options_list bins_or_options { CONS2($1,$2) }
	|	bins_or_options { CONS1 ($1) }

bins_or_options: coverage_option SEMICOLON { TUPLE3(STRING("bins_or_options1"),$1,SEMICOLON) }
	|	coverage_bin SEMICOLON { TUPLE3(STRING("bins_or_options2"),$1,SEMICOLON) }
	|	preprocessor_balanced_bins_or_options_list { ($1) }
	|	macro_call_or_item { ($1) }
	|	ERROR_TOKEN SEMICOLON { TUPLE3(STRING("bins_or_options5"),ERROR_TOKEN,SEMICOLON) }

bins_or_options_list_opt_pp: bins_or_options_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_balanced_bins_or_options_list: preprocessor_if_header bins_or_options_list_opt_pp preprocessor_elsif_bins_or_options_list_opt preprocessor_else_bins_or_options_opt BACKQUOTE_endif { CONS5($1,$2,$3,$4,BACKQUOTE_endif) }

preprocessor_elsif_bins_or_options_list_opt: preprocessor_elsif_bins_or_options_list { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_elsif_bins_or_options_list: preprocessor_elsif_bins_or_options_list preprocessor_elsif_bins_or_options { CONS2($1,$2) }
	|	preprocessor_elsif_bins_or_options { CONS1 ($1) }

preprocessor_elsif_bins_or_options: preprocessor_elsif_header bins_or_options_list_opt_pp { TUPLE3(STRING("preprocessor_elsif_bins_or_options1"),$1,$2) }

preprocessor_else_bins_or_options_opt: preprocessor_else_bins_or_options { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

preprocessor_else_bins_or_options: BACKQUOTE_else bins_or_options_list_opt_pp { TUPLE3(STRING("preprocessor_else_bins_or_options1"),BACKQUOTE_else,$2) }

coverage_bin: wildcard_opt bins_keyword GenericIdentifier covergroup_expression_bracketed_opt EQUALS coverage_bin_rhs iff_expr_opt { TUPLE8(STRING("coverage_bin1"),$1,$2,$3,$4,EQUALS,$6,$7) }

coverage_bin_rhs: set_covergroup_expression_or_covergroup_range_list_or_trans_list { ($1) }
	|	Default { (Default) }
	|	Default Sequence { TUPLE3(STRING("coverage_bin_rhs3"),Default,Sequence) }

wildcard_opt: Wildcard { (Wildcard) }
	|	/* empty */ { EMPTY_TOKEN }

bins_keyword: Bins { (Bins) }
	|	Illegal_bins { (Illegal_bins) }
	|	Ignore_bins { (Ignore_bins) }

covergroup_expression_bracketed_opt: LBRACK expression RBRACK { TUPLE4(STRING("covergroup_expression_bracketed_opt1"),LBRACK,$2,RBRACK) }
	|	LBRACK RBRACK { TUPLE3(STRING("covergroup_expression_bracketed_opt2"),LBRACK,RBRACK) }
	|	/* empty */ { EMPTY_TOKEN }

set_covergroup_expression_or_covergroup_range_list_or_trans_list: expression_list_proper { CONS1 ($1) }

bins_selection: bins_keyword GenericIdentifier EQUALS select_expression { TUPLE5(STRING("bins_selection1"),$1,$2,EQUALS,$4) }

select_expression: property_expr { ($1) }

select_condition: Binsof LPAREN bins_expression RPAREN { TUPLE5(STRING("select_condition1"),Binsof,LPAREN,$3,RPAREN) }

bins_expression: reference { ($1) }

with_covergroup_expression_in_parens: With_LPAREN_covergroup_RPAREN LPAREN with_covergroup_expression RPAREN { TUPLE5(STRING("with_covergroup_expression_in_parens1"),With_LPAREN_covergroup_RPAREN,LPAREN,$3,RPAREN) }

with_covergroup_expression: expression { ($1) }

randsequence_statement: Randsequence LPAREN identifier_opt RPAREN production_list Endsequence { TUPLE7(STRING("randsequence_statement1"),Randsequence,LPAREN,$3,RPAREN,$5,Endsequence) }

production_list: production_list production { CONS2($1,$2) }
	|	production { CONS1 ($1) }

production: data_type_or_void_with_id tf_port_list_paren_opt COLON rs_rule_list SEMICOLON { TUPLE6(STRING("production1"),$1,$2,COLON,$4,SEMICOLON) }

data_type_or_void_with_id: data_type_or_implicit_basic_followed_by_id_and_dimensions_opt { ($1) }

rs_rule_list: rs_rule_list VBAR rs_rule { CONS3($1,VBAR,$3) }
	|	rs_rule { CONS1 ($1) }

rs_rule: rs_production_list_or_rand_join { ($1) }
	|	rs_production_list_or_rand_join COLON_EQ weight_specification { TUPLE4(STRING("rs_rule2"),$1,COLON_EQ,$3) }
	|	rs_production_list_or_rand_join COLON_EQ weight_specification rs_code_block { TUPLE5(STRING("rs_rule3"),$1,COLON_EQ,$3,$4) }

rs_production_list_or_rand_join: rs_production_list { ($1) }
	|	Rand Join expression_in_parens_opt production_items_list { TUPLE5(STRING("rs_production_list_or_rand_join2"),Rand,Join,$3,$4) }

expression_in_parens: LPAREN expression RPAREN { TUPLE4(STRING("expression_in_parens1"),LPAREN,$2,RPAREN) }
	|	LPAREN ERROR_TOKEN RPAREN { TUPLE4(STRING("expression_in_parens2"),LPAREN,ERROR_TOKEN,RPAREN) }

expression_in_parens_opt: expression_in_parens { ($1) }
	|	/* empty */ { EMPTY_TOKEN }

rs_production_list: rs_production_list rs_prod { CONS2($1,$2) }
	|	rs_prod { CONS1 ($1) }

weight_specification: number { ($1) }
	|	GenericIdentifier { ($1) }
	|	LPAREN expression RPAREN { TUPLE4(STRING("weight_specification3"),LPAREN,$2,RPAREN) }

rs_code_block: LBRACE block_item_or_statement_or_null_list_opt RBRACE { TUPLE4(STRING("rs_code_block1"),LBRACE,$2,RBRACE) }

production_items_list: production_items_list production_item { CONS2($1,$2) }
	|	production_item production_item { CONS2($1,$2) }

rs_prod: production_item { ($1) }
	|	rs_code_block { ($1) }
	|	rs_if_else { ($1) }
	|	rs_repeat { ($1) }
	|	rs_case { ($1) }

production_item: GenericIdentifier LPAREN any_port_list RPAREN { TUPLE5(STRING("production_item1"),$1,LPAREN,$3,RPAREN) }
	|	GenericIdentifier { ($1) }

rs_if_else: If LPAREN expression RPAREN production_item Else production_item { TUPLE8(STRING("rs_if_else1"),If,LPAREN,$3,RPAREN,$5,Else,$7) }
	|	If LPAREN expression RPAREN production_item { TUPLE6(STRING("rs_if_else2"),If,LPAREN,$3,RPAREN,$5) }

rs_repeat: repeat_control production_item { TUPLE3(STRING("rs_repeat1"),$1,$2) }

rs_case: Case LPAREN expression RPAREN rs_case_item_list Endcase { TUPLE7(STRING("rs_case1"),Case,LPAREN,$3,RPAREN,$5,Endcase) }

rs_case_item_list: rs_case_item_list rs_case_item { CONS2($1,$2) }
	|	rs_case_item { CONS1 ($1) }

rs_case_item: case_item_expression_list COLON production_item SEMICOLON { TUPLE5(STRING("rs_case_item1"),$1,COLON,$3,SEMICOLON) }
	|	Default COLON production_item SEMICOLON { TUPLE5(STRING("rs_case_item2"),Default,COLON,$3,SEMICOLON) }
	|	Default production_item SEMICOLON { TUPLE4(STRING("rs_case_item3"),Default,$2,SEMICOLON) }

case_item_expression_list: case_item_expression_list COMMA case_item_expression { CONS3($1,COMMA,$3) }
	|	case_item_expression { CONS1 ($1) }

case_item_expression: expression { ($1) }


