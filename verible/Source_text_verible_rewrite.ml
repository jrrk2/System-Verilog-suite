open Source_text_verible_rewrite_types
open Source_text_verible

let othcons = ref End_of_file

let verbose = try int_of_string (Sys.getenv ("DESCEND_VERBOSE")) > 0 with _ -> false

let trim s = if s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s

let notsupp kind lst = failwith ("Not supported: "^kind)

let getstr = function
  | Add _ -> "Add"
  | AlwaysComb _ -> "AlwaysComb"
  | AlwaysComb2 _ -> "AlwaysComb2"
  | AlwaysFF _ -> "AlwaysFF"
  | AlwaysLatch _ -> "AlwaysLatch"
  | AlwaysLegacy _ -> "AlwaysLegacy"
  | AlwaysSync -> "AlwaysSync"
  | And _ -> "And"
  | And2 _ -> "And2"
  | And3 _ -> "And3"
  | AnyRange _ -> "AnyRange"
  | Asgn1 _ -> "Asgn1"
  | AsgnPat _ -> "AsgnPat"
  | Assert -> "Assert"
  | AssertProperty -> "AssertProperty"
  | At _ -> "At"
  | AtStar
  | Atom _ -> "Atom"
  | AutoFunDecl _ -> "AutoFunDecl"
  | BeginBlock _ -> "BeginBlock"
  | Bitlst _ -> "Bitlst"
  | BlockItem _ -> "BlockItem"
  | BreakSemi -> "BreakSemi"
  | CaretTilde _ -> "CaretTilde"
  | CaseItm _ -> "CaseItm"
  | CaseStart _ -> "CaseStart"
  | CaseStart1 _ -> "CaseStart1"
  | CaseStartInside _ -> "CaseStartInside"
  | CaseStartUniq _ -> "CaseStartUniq"
  | CaseStartUniqInside _ -> "CaseStartUniqInside"
  | CaseStmt _ -> "CaseStmt"
  | Cast _ -> "Cast"
  | CellParamItem2 _ -> "CellParamItem2"
  | CellParamItem3 _ -> "CellParamItem3"
  | CellPinItem2 _ -> "CellPinItem2"
  | CellPinItemImplied _ -> "CellPinItemImplied"
  | CellPinItemNC _ -> "CellPinItemNC"
  | Concat _ -> "Concat"
  | CondGen1 _ -> "CondGen1"
  | ContAsgn _ -> "ContAsgn"
  | DeclAsgn _ -> "DeclAsgn"
  | DeclData _ -> "DeclData"
  | DeclInt2 _ -> "DeclInt2"
  | DeclLogic _ -> "DeclLogic"
  | DeclLogic2 _ -> "DeclLogic2"
  | DeclModPort _ -> "DeclModPort"
  | DeclReg _ -> "DeclReg"
  | Deflt
  | Div _ -> "Div"
  | Dot1 _ -> "Dot1"
  | Dot3 _ -> "Dot3"
  | DotBus _ -> "DotBus"
  | Edge _ -> "Edge"
  | ElabTask _ -> "ElabTask"
  | ElseStmt _ -> "ElseStmt"
  | EnumInit _ -> "EnumInit"
  | Equals _ -> "Equals"
  | Equals3 _ -> "Equals3"
  | EqualsQuery _ -> "EqualsQuery"
  | Equate _ -> "Equate"
  | EquateArrayField _ -> "EquateArrayField"
  | EquateConcat _ -> "EquateConcat"
  | EquateField _ -> "EquateField"
  | EquateSelect _ -> "EquateSelect"
  | EquateSelect2 _ -> "EquateSelect2"
  | EquateSlice _ -> "EquateSlice"
  | EquateSlicePlus _ -> "EquateSlicePlus"
  | EventOr _ -> "EventOr"
  | ExprOKL _ -> "ExprOKL"
  | ExprQuote1 _ -> "ExprQuote1"
  | Expression _ -> "Expression"
  | Final _ -> "Final"
  | Float _ -> "Float"
  | FopAsgn _ -> "FopAsgn"
  | FopAsgn1 _ -> "FopAsgn1"
  | FopAsgnArrayField _ -> "FopAsgnArrayField"
  | FopAsgnArrayField2 _ -> "FopAsgnArrayField2"
  | FopAsgnArrayField3 _ -> "FopAsgnArrayField3"
  | FopAsgnArrayField4 _ -> "FopAsgnArrayField4"
  | FopAsgnArrayField5 _ -> "FopAsgnArrayField5"
  | FopAsgnArrayField6 _ -> "FopAsgnArrayField6"
  | FopAsgnArrayField7 _ -> "FopAsgnArrayField7"
  | FopAsgnArrayField8 _ -> "FopAsgnArrayField8"
  | FopAsgnArrayField9 _ -> "FopAsgnArrayField9"
  | FopAsgnArrayMemSel _ -> "FopAsgnArrayMemSel"
  | FopAsgnArrayRange _ -> "FopAsgnArrayRange"
  | FopAsgnArrayRange2 _ -> "FopAsgnArrayRange2"
  | FopAsgnArraySel _ -> "FopAsgnArraySel"
  | FopAsgnArrayWid _ -> "FopAsgnArrayWid"
  | FopAsgnConcat _ -> "FopAsgnConcat"
  | ForEach _ -> "ForEach"
  | ForLoop _ -> "ForLoop"
  | FunDecl _ -> "FunDecl"
  | FunGuts _ -> "FunGuts"
  | FunRef _ -> "FunRef"
  | FunRef2 _ -> "FunRef2"
  | GenBlock _ -> "GenBlock"
  | GenItem _ -> "GenItem"
  | Genvar _ -> "GenItem"
  | Greater _ -> "Greater"
  | GtEq _ -> "GtEq"
  | HyphenGt _ -> "HyphenGt"
  | Id _ -> "Id"
  | IdArrayed1 _ -> "IdArrayed1"
  | IdArrayed2 _ -> "IdArrayed2"
  | IdArrayed3 _ -> "IdArrayed3"
  | IdArrayedColon _ -> "IdArrayedColon"
  | IdArrayedHyphenColon _ -> "IdArrayedHyphenColon"
  | IdArrayedPlusColon _ -> "IdArrayedPlusColon"
  | If1 _ -> "If1"
  | If2 _ -> "If2"
  | Iff _ -> "Iff"
  | Import _ -> "Import"
  | In
  | Inc _ -> "Inc"
  | InitPair _ -> "InitPair"
  | InitPat _ -> "InitPat"
  | InitSig _ -> "InitSig"
  | Initial _ -> "Initial"
  | Inout
  | InsideCase _ -> "InsideCase"
  | InsideRange _ -> "InsideRange"
  | InstArrayDecl _ -> "InstArrayDecl"
  | InstDecl _ -> "InstDecl"
  | InstNameParen1 _ -> "InstNameParen1"
  | InstNameParen2 _ -> "InstNameParen2"
  | InstRange _ -> "InstRange"
  | IntfDecl _ -> "IntfDecl"
  | Intgr _ -> "Intgr"
  | ItemAsgn _ -> "ItemAsgn"
  | Itmlst _ -> "Itmlst"
  | Less _ -> "Less"
  | LocalParamTyp _ -> "LocalParamTyp"
  | Logic _ -> "Logic"
  | LoopGen1 _ -> "LoopGen1"
  | LtEq _ -> "LtEq"
  | LtGt _ -> "LtGt"
  | Mod _ -> "Mod"
  | ModPortItm _ -> "ModPortItm"
  | Modul _ -> "Modul"
  | Mult _ -> "Mult"
  | Nand _ -> "Nand"
  | Neg _ -> "Neg"
  | NetDecl _ -> "NetDecl"
  | NonBlocking _ -> "NonBlocking"
  | Nor _ -> "Nor"
  | NotEq _ -> "NotEq"
  | NotEq3 _ -> "NotEq3"
  | NotEqQuery _ -> "NotEqQuery"
  | Number _ -> "Number"
  | OpenRange _ -> "OpenRange"
  | Or _ -> "Or"
  | Or2 _ -> "Or2"
  | Out
  | PackageBody _ -> "PackageBody"
  | PackageParam _ -> "PackageParam"
  | PackageParam2 _ -> "PackageParam2"
  | Param _ -> "Param"
  | ParamAsgn1 _ -> "ParamAsgn1"
  | ParamAsgn2 _ -> "ParamAsgn2"
  | ParamDecl _ -> "ParamDecl"
  | ParamPort _ -> "ParamPort"
  | PatMember1 _ -> "PatMember1"
  | PatMemberDflt _ -> "PatMemberDflt"
  | PkgImport _ -> "PkgImport"
  | PkgImportItm _ -> "PkgImportItm"
  | Pling _ -> "Pling"
  | Port _ -> "Port"
  | PortDir _ -> "PortDir"
  | PortFront _ -> "PortFront"
  | PortItem _ -> "PortItem"
  | PortItemFront _ -> "PortItemFront"
  | PortItemFront2 _ -> "PortItemFront2"
  | PortsStar _ -> "PortsStar"
  | Pos _ -> "Pos"
  | PropertySpec -> "PropertySpec"
  | Query _ -> "Query"
  | RedAnd _ -> "RedAnd"
  | RedOr _ -> "RedOr"
  | RedXor _ -> "RedXor"
  | Repl _ -> "Repl"
  | Return _ -> "Return"
  | SUDecl _ -> "SUDecl"
  | SUMember _ -> "SUMember"
  | Seq _ -> "Seq"
  | Shiftl _ -> "Shiftl"
  | Shiftr _ -> "Shiftr"
  | Shiftr3 _ -> "Shiftr3"
  | SideEffect _ -> "SideEffect"
  | Signed _ -> "Signed"
  | StarStar _ -> "StarStar"
  | Blocking _ -> "Blocking"
  | String _ -> "String"
  | Sub _ -> "Sub"
  | Sys _ -> "Sys"
  | SysFuncCall _ -> "SysFuncCall"
  | SysTaskCall _ -> "SysTaskCall"
  | TFBody _ -> "TFBody"
  | TF_port_decl _ -> "TF_port_decl"
  | TF_variable _ -> "TF_variable"
  | TaskDecl _ -> "TaskDecl"
  | TaskRef _ -> "TaskRef"
  | SysTaskRef _ -> "SysTaskRef"
  | Tilde _ -> "Tilde"
  | TildeAnd _ -> "TildeAnd"
  | TildeOr _ -> "TildeOr"
  | Typ1 _ -> "Typ1"
  | Typ2 _ -> "Typ2"
  | Typ3 _ -> "Typ3"
  | Typ4 _ -> "Typ4"
  | Typ5 _ -> "Typ5"
  | Typ6 _ -> "Typ6"
  | Typ7 _ -> "Typ7"
  | Typ8 _ -> "Typ8"
  | Typ9 _ -> "Typ9"
  | Typ10 _ -> "Typ10"
  | TypEnum3 _ -> "TypEnum3"
  | TypEnum4 _ -> "TypEnum4"
  | TypEnum5 _ -> "TypEnum5"
  | TypEnum6 _ -> "TypEnum6"
  | TypParam _ -> "TypParam"
  | UMinus _ -> "UMinus"
  | UPlus _ -> "UPlus"
  | Unimplemented _ -> "Unimplemented"
  | Union _ -> "Union"
  | Unknown _ -> "Unknown"
  | Unsigned _ -> "Unsigned"
  | VNum _ -> "VNum"
  | ValueRange _ -> "ValueRange"
  | VarDeclAsgn _ -> "VarDeclAsgn"
  | VarDim _ -> "VarDim"
  | While _ -> "While"
  | WireExpr _ -> "WireExpr"
  | Xnor _ -> "Xnor"
  | Xor _ -> "Xor"

let rec exp_lst_proper = function
| TUPLE4 (STRING "expression_list_proper1", (TUPLE4 (STRING "expression_list_proper1", _, COMMA, _) as lst), COMMA, expr) ->
    expr :: exp_lst_proper lst
| TUPLE4 (STRING "expression_list_proper1", expr, COMMA, expr') -> expr :: expr' :: []
| oth ->failwith "exp_lst_proper"

let rec rw = function
| STRING _ as str -> str
| TK_BinBase _ as b -> b
| TK_BinDigits _ as b -> b
| TK_OctBase _ as o -> o
| TK_OctDigits _ as o -> o
| TK_DecBase _ as d -> d
| TK_DecDigits _ as d -> d
| TK_HexBase _ as h -> h
| TK_HexDigits _ as h -> h
| TK_DecNumber _ as d -> d
| TK_StringLiteral _ as s -> s
| TK_UnBasedNumber _ as d -> d
| TK_RealTime _ as f -> f
| SymbolIdentifier _ as sym -> sym
| SystemTFIdentifier _ as sym -> sym
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, lst, RBRACE) ->
  TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST (exp_lst_proper lst), RBRACE)
| CONS1 (TUPLE3 (STRING "enum_name_list_item_last1", TUPLE3 (STRING "enum_name_list_trailing_comma1", op, COMMA), id)) ->
  (match rw op with TLIST oth -> TLIST (rw id :: oth) | oth -> othcons := oth; failwith "enum")
| CONS1 (TUPLE3 (STRING "enum_name_list_item_last1", _, _)) as oth -> othcons := oth; failwith "enum"
| CONS1 x -> (match rw x with TLIST x -> TLIST x | oth -> TLIST [oth])
| CONS2 (CONS1 a, b) -> (match rw a with TLIST lst -> TLIST (rw b :: lst) | oth -> TLIST (rw b :: oth :: []))
| CONS2 (CONS2 (a, b), c) -> (match rw a with TLIST lst -> TLIST (rw c :: rw b :: lst) | oth -> TLIST (rw c :: rw b :: oth :: []))
| CONS2 (a, b) -> (match rw a with TLIST lst -> TLIST (rw b :: lst) | oth -> TLIST (rw b :: oth :: []))
| CONS3 (CONS1 a, (COMMA|Or), b) -> (match rw a with TLIST lst -> TLIST (rw b :: lst) | oth -> TLIST (rw b :: oth :: []))
| CONS3 (CONS3 (a, (COMMA|Or), b), (COMMA|Or), c) -> (match rw a with TLIST lst -> TLIST (rw c :: rw b :: lst) | oth -> TLIST (rw c :: rw b :: oth :: []))
| TLIST lst -> TLIST (List.map rw lst)
| TUPLE2(arg1,arg2) -> TUPLE2 (rw arg1, rw arg2)
| TUPLE4(STRING _ as arg0,LPAREN,arg,RPAREN) -> TUPLE4(arg0,LPAREN,rw arg,RPAREN)
| TUPLE3(STRING "case_items1", (TUPLE3(STRING "case_items1",_,_) as lft), rght) -> (match rw lft with
    | TLIST lst -> TLIST (lst @ [rw rght])
    | oth -> othcons := oth; failwith "case_items1")
| TUPLE3(STRING "case_items1", lft, rght) -> TLIST (rw lft :: rw rght :: [])
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw arg1, rw arg2, rw arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw arg1, rw arg2, rw arg3, rw arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10)
| TUPLE11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) ->
   TUPLE11(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11)
| TUPLE12(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) ->
   TUPLE12(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11, rw arg12)
| TUPLE13(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) ->
   TUPLE13(rw arg1, rw arg2, rw arg3, rw arg4, rw arg5, rw arg6, rw arg7, rw arg8, rw arg9, rw arg10, rw arg11, rw arg12, rw arg13)
| ((AT|EMPTY_TOKEN|LPAREN|RPAREN|LBRACK|RBRACK|LBRACE|RBRACE
| COLON|SEMICOLON|COMMA|CARET|TILDE|QUERY|QUOTE
| PLUS|HYPHEN|STAR|SLASH|HASH|PLING
| AMPERSAND|AMPERSAND_AMPERSAND|AMPERSAND_EQ
| GT_GT_GT|PLUS_COLON|PLUS_PLUS|COLON_COLON
| EQUALS|LT_EQ|VBAR_VBAR|LT_LT|GT_GT|GT_EQ|EQ_EQ|LESS|GREATER|VBAR
| TILDE_VBAR|TILDE_AMPERSAND
| TILDE_CARET
| HYPHEN_HYPHEN
| VBAR_EQ|PLUS_EQ
| PLING_EQ|DOT_STAR|STAR_STAR
| QUOTE_LBRACE
| Module|Always|Assign|Reg|Wire|Logic|Bit|Int|Integer
| Unsigned|Signed
| Output|Input|Posedge|Negedge|Or|DOT
| Parameter|Localparam|Initial
| If|Else|Modport|For
| Begin|End|Endmodule
| Interface|Endinterface
| Task|Endtask
| Package|Endpackage
| Generate|Endgenerate
| Function|Endfunction
| Struct
| Typedef
| Enum
| Case|Casez|Casex|Default|Endcase
| Packed
| Import
| Genvar
| Always_comb|Always_ff|Always_latch
| Return
| Automatic
| Union
| Assert
| Const
| Inout
| Break
| Type
| Property
| While
| String
| Byte
| Longint
| Void
| Inside
| Unique
| Foreach
| Final
| HYPHEN_COLON
| HYPHEN_EQ
| End_of_file
|Zi_zp|Zi_zd|Zi_np|Zi_nd|Xor|Xnor|Wreal|Wor|Wone|Within|
With_LPAREN_covergroup_RPAREN|With|Wildcard|White_noise|Weak1|Weak0|Weak|
Wand|Wait_order|Wait|Virtual|Vectored|Var|VBAR_HYPHEN_GT|VBAR_EQ_GT|Uwire|
Use|Untyped|Until_with|Until|Units|Unique_index|Unique0|UNDERSCORE|
Type_option|Trireg|Trior|Triand|Tri1|Tri0|Tri|Transition|Tranif1|Tranif0|
Tran|Timeunit_check|Timeunit|Timeprecision_check|Timeprecision|Time|
Throughout|This|Tagged|Table|TK_edge_descriptor|TK_XZDigits|
TK_TimeLiteral|TK_RS_EQ|TK_RSS_EQ|
TK_OTHER|TK_LS_EQ|TK_EvalStringLiteral|
TK_AngleBracketInclude|
Sync_reject_on|Sync_accept_on|Supply1|Supply0|Super|Sum|Strong1|Strong0|
Strong|Static|Specparam|Specify|Sort|Solve|Soft|Small|Shuffle|Showcancelled|
Shortreal|Shortint|Sequence|Scalared|Sample|S_until_with|S_until|S_nexttime|
S_eventually|S_always|STAR_RPAREN|STAR_GT|STAR_EQ|
SLASH_SLASH_end_of_line_comment|SLASH_EQ|
SLASH_AMPERSAND_lowast_SEMICOLON_comment_AMPERSAND_lowast_SEMICOLON_SLASH|
SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN|
Rtranif1|Rtranif0|Rtran|Rsort|Rpmos|Rnmos|Reverse|Restrict|Resolveto|Repeat|
Release|Reject_on|Ref|Realtime|Real|Rcmos|Randsequence|Randomize|Randcase|
Randc|Rand|Pure|Pulsestyle_onevent|Pulsestyle_ondetect|Pullup|Pulldown|Pull1|
Pull0|Protected|Program|Product|Priority|Primitive|Pow|Potential|Pmos|
Paramset|PP_Identifier|PLING_EQ_QUERY|PLING_EQ_EQ|PERCENT_EQ|PERCENT|Option|
Null|Notif1|Notif0|Not|Noshowcancelled|Nor|Noise_table|Nmos|Nexttime|New|
Nettype|Net_resolution|Nature|Nand|NUMBER_step|Min|Medium|Max|Matches|
Macromodule|MacroNumericWidth|MacroIdentifier|MacroIdItem|MacroCallId|
MacroCallCloseToEndLine|MacroArg|Local_COLON_COLON|Local|Limexp|Library|
Liblist|Let|Less_than_TK_else|Last_crossing|Large|Laplace_zp|Laplace_zd|
Laplace_np|Laplace_nd|LT_PLUS|LT_LT_space_GT_GT|LT_LT_filepath_GT_GT|
LT_LT_default_HYPHEN_text_GT_GT|LT_LT_BACKSLASH_BACKSLASH_n_GT_GT|
LT_LT_BACKSLASH_BACKSLASH_line_HYPHEN_cont_GT_GT|
LT_LT_BACKQUOTE_define_HYPHEN_tokens_GT_GT|LT_HYPHEN_GT|
LPAREN_timescale_unit_RPAREN|LPAREN_STAR_attribute_STAR_RPAREN|LPAREN_STAR|
LINEFEED|LBRACK_STAR_RBRACK|LBRACK_STAR|LBRACK_PLUS_RBRACK|LBRACK_HYPHEN_GT|
LBRACK_EQ|Join_none|Join_any|Join|Intersect|Interconnect|Instance|Infinite|
Inf|Include|Incdir|Implies|Implements|Illegal_bins|Ignore_bins|Ifnone|Iff|
Idt_nature|Highz1|Highz0|HYPHEN_GT_LPAREN_trigger_RPAREN|
HYPHEN_GT_LPAREN_logical_HYPHEN_implies_RPAREN|
HYPHEN_GT_LPAREN_constraint_HYPHEN_implies_RPAREN|HYPHEN_GT_GT|HYPHEN_GT|
HASH_HYPHEN_HASH|HASH_HASH|HASH_EQ_HASH|Global|From|Forkjoin|Fork|Forever|
Force|Flow|Flicker_noise|First_match|Find_last_index|Find_last|Find_index|
Find_first_index|Find_first|Find|Extern|Extends|Export|Expect|Exclude|
Eventually|Event|EscapedIdentifier|Error|Endtable|Endspecify|Endsequence|
Endproperty|Endprogram|Endprimitive|Endparamset|Endnature|Endgroup|
Enddiscipline|Endconnectrules|Endconfig|Endclocking|Endclass|Endchecker|Edge|
ERROR_TOKEN|EQ_GT|EQ_EQ_QUERY|EQ_EQ_EQ|EOF_TOKEN|Driver_update|Domain|Do|
Dist|Discrete|Discipline|Disable|Design|Defparam|Deassign|Ddt_nature|
DOUBLEQUOTE|DOLLAR|DLR_width|DLR_unit|DLR_timeskew|DLR_skew|DLR_setuphold|
DLR_setup|DLR_root|DLR_removal|DLR_recrem|DLR_recovery|DLR_period|
DLR_nochange|DLR_hold|DLR_fullskew|DLR_attribute|DEFAULT|Cross|Coverpoint|
Covergroup|Cover|Continuous|Continue|Context|Constraint|Connectrules|
Connectmodule|Connect|Config|Cmos|Clocking|Class|Checker|Chandle|Cell|
COLON_SLASH|COLON_EQ|CHAR|CARET_EQ|Bufif1|Bufif0|Buf|Binsof|Bins|Bind|Before|
BACKSLASH|BACKQUOTE_uselib|BACKQUOTE_undef|BACKQUOTE_unconnected_drive|
BACKQUOTE_timescale|BACKQUOTE_suppress_faults|BACKQUOTE_resetall|
BACKQUOTE_protect|BACKQUOTE_pragma|BACKQUOTE_nounconnected_drive|
BACKQUOTE_nosuppress_faults|BACKQUOTE_include|BACKQUOTE_ifndef|
BACKQUOTE_ifdef|BACKQUOTE_endprotect|BACKQUOTE_endif|BACKQUOTE_endcelldefine|
BACKQUOTE_end_keywords|BACKQUOTE_enable_portfaults|BACKQUOTE_elsif|
BACKQUOTE_else|BACKQUOTE_disable_portfaults|BACKQUOTE_delay_mode_zero|
BACKQUOTE_delay_mode_unit|BACKQUOTE_delay_mode_path|
BACKQUOTE_delay_mode_distributed|BACKQUOTE_define|
BACKQUOTE_default_trireg_strength|BACKQUOTE_default_nettype|
BACKQUOTE_default_decay_time|BACKQUOTE_celldefine|BACKQUOTE_begin_keywords|
BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____|
BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____|
BACKQUOTE_BACKQUOTE|BACKQUOTE|Assume|And|Analysis|Analog|Aliasparam|Alias|
Access|Accept_on|Ac_stim|Abstol|Absdelay|AT_AT|AMPERSAND_AMPERSAND_AMPERSAND|
ACCEPT|TUPLE15 _|TUPLE14 _|SLIST _|
CONS5 _
) as oth) -> oth
| oth -> othcons := oth; failwith "CONS1"

let rec rw' = function
| TLIST lst -> TLIST (rw'' (List.rev (List.map rw' lst)))
| TUPLE2(arg1,arg2) -> TUPLE2 (rw' arg1, rw' arg2)
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 (rw' arg1, rw' arg2, rw' arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 (rw' arg1, rw' arg2, rw' arg3, rw' arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 (rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 (rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 (rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8(rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7, rw' arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9(rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7, rw' arg8, rw' arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10(rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7, rw' arg8, rw' arg9, rw' arg10)
| TUPLE11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) ->
   TUPLE11(rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7, rw' arg8, rw' arg9, rw' arg10, rw' arg11)
| TUPLE12(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) ->
   TUPLE12(rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7, rw' arg8, rw' arg9, rw' arg10, rw' arg11, rw' arg12)
| TUPLE13(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12,arg13) ->
   TUPLE13(rw' arg1, rw' arg2, rw' arg3, rw' arg4, rw' arg5, rw' arg6, rw' arg7, rw' arg8, rw' arg9, rw' arg10, rw' arg11, rw' arg12, rw' arg13)
| oth -> oth

and rw'' = function
| [] -> []
| TUPLE5 (STRING "port_declaration_noattr1", (Input|Output as dir), (Wire|EMPTY_TOKEN as kind),
    TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK),
      TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id1, EMPTY_TOKEN),
      EMPTY_TOKEN),
    EMPTY_TOKEN) ::
  COMMA ::
  TUPLE3 (STRING "port1",
    TUPLE3 (STRING "port_reference1",
      TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id2,
	EMPTY_TOKEN),
      EMPTY_TOKEN),
    EMPTY_TOKEN) :: tl ->
  TUPLE5 (STRING "port_declaration_noattr1", dir, kind,
    TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK),
      TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id1, EMPTY_TOKEN),
      EMPTY_TOKEN),
    EMPTY_TOKEN) ::
  COMMA :: rw'' (
  TUPLE5 (STRING "port_declaration_noattr1", dir, kind,
    TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
      TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK),
      TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id2, EMPTY_TOKEN),
      EMPTY_TOKEN),
    EMPTY_TOKEN) :: tl)
| TUPLE5 (STRING "port_declaration_noattr1", (Input|Output as dir), (Wire|EMPTY_TOKEN as kind),
    TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
       TUPLE3 (STRING "data_type_primitive1",
	 TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
	 TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK)),
       TUPLE3 (STRING "unqualified_id1", SymbolIdentifier a, EMPTY_TOKEN),
       EMPTY_TOKEN),
     EMPTY_TOKEN) ::
   COMMA ::
   TUPLE3 (STRING "port1",
     TUPLE3 (STRING "port_reference1",
       TUPLE3 (STRING "unqualified_id1", SymbolIdentifier b, EMPTY_TOKEN), EMPTY_TOKEN), EMPTY_TOKEN) :: tl ->
  TUPLE5 (STRING "port_declaration_noattr1", dir, kind,
    TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
       TUPLE3 (STRING "data_type_primitive1",
	 TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
	 TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK)),
       TUPLE3 (STRING "unqualified_id1", SymbolIdentifier a, EMPTY_TOKEN),
       EMPTY_TOKEN),
     EMPTY_TOKEN) ::
   COMMA :: rw'' (
  TUPLE5 (STRING "port_declaration_noattr1", dir, kind,
    TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
       TUPLE3 (STRING "data_type_primitive1",
	 TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
	 TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK)),
       TUPLE3 (STRING "unqualified_id1", SymbolIdentifier b, EMPTY_TOKEN),
       EMPTY_TOKEN),
     EMPTY_TOKEN) :: tl)
| TUPLE5 (STRING "port_declaration_noattr1", (Input|Output as dir), (Wire|EMPTY_TOKEN as kind),
    TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
      TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id1,
	EMPTY_TOKEN),
      EMPTY_TOKEN),
    EMPTY_TOKEN) :: COMMA ::
    TUPLE3 (STRING "port1", TUPLE3 (STRING "port_reference1",
                TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id2, EMPTY_TOKEN),
                EMPTY_TOKEN),
		   EMPTY_TOKEN) :: tl ->
    TUPLE5 (STRING "port_declaration_noattr1", dir, kind,
      TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id1,
	  EMPTY_TOKEN),
      EMPTY_TOKEN),
    EMPTY_TOKEN) :: COMMA :: rw'' (
    TUPLE5 (STRING "port_declaration_noattr1", dir, kind,
      TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id2,
	  EMPTY_TOKEN),
      EMPTY_TOKEN),
      EMPTY_TOKEN) :: tl)
| oth :: tl -> oth :: rw'' tl

open Source_text_verible_lex
open Source_text_verible
open Verible_pat
open Verible_typ

let othrawp = ref End_of_file
let othp = ref End_of_file
let othp' = ref End_of_file
let othp'' = ref Vempty
let othitms = ref (Input_dump.empty_itms [])

let tranlst v =
  let rawp = parse_output_ast_from_file v in
  othrawp := rawp;
  let p = rw rawp in
  othp := p;
  let p' = rw' p in
  othp' := p';
  let p'' = pat p' in
  othp'' := p'';
  print_endline "verible_pat_cnv";
  Verible_pat.cnv' othitms p''
