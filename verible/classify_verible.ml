open Source_text_verible
open Source_text_verible_rewrite_types

let rec classify chash = function
| STRING _ as itm -> itm
| SymbolIdentifier _ -> SymbolIdentifier ""
| SystemTFIdentifier _ -> SystemTFIdentifier ""
| TK_DecNumber _ -> TK_DecNumber ""
| TK_BinBase _ -> TK_BinBase ""
| TK_BinDigits _ -> TK_BinDigits ""
| TK_OctBase _ -> TK_OctBase ""
| TK_OctDigits _ -> TK_OctDigits ""
| TK_DecBase _ -> TK_DecBase ""
| TK_DecDigits _ -> TK_DecDigits ""
| TK_HexBase _ -> TK_HexBase ""
| TK_HexDigits _ -> TK_HexDigits ""
| TK_StringLiteral _ -> TK_StringLiteral ""
| TK_UnBasedNumber _ -> TK_UnBasedNumber ""
| TK_RealTime _ -> TK_RealTime ""
| ELIST lst -> ELIST []
| TLIST lst -> List.iter (fun itm -> Hashtbl.replace chash (classify chash itm) ()) lst; TLIST []
| TUPLE2(arg1,arg2) -> TUPLE2 ((classify chash) arg1, (classify chash) arg2)
| TUPLE3(STRING (
 "instantiation_base1"
|"bin_based_number1"
|"begin1"
|"procedural_timing_control_statement2"
|"reference3"|"case_items1"|"reference_or_call_base1"|"assignment_pattern_expression2"
|"assignment_pattern_expression3"
|"block_event_expr_primary1"
|"block_event_expr_primary2"
|"constraint_expression_no_preprocessor2"
|"constraint_expression_no_preprocessor7"
|"covergroup_expression_bracketed_opt2"
|"event_expression1"
|"expr_primary_braces1"
|"expression_or_dist1"
|"inc_or_dec_expression1"
|"inc_or_dec_expression2"
|"inc_or_dec_expression3"
|"inc_or_dec_expression4"
|"preprocessor_else_constraint_expression1"
|"preprocessor_elsif_constraint_expression1"
|"preprocessor_elsif_constraint_expressions1"
|"property_prefix_expr12"
|"property_prefix_expr5"
|"property_prefix_expr7"
|"property_prefix_expr9"
|"sequence_delay_range_expr2"
|"sequence_repetition_expr1"
|"sequence_unary_expr2"
|"tf_port_item_expr_opt1"
|"udp_initial_expr_opt1"
|"unary_prefix_expr2"
|"value_range_expression3"
|"value_range_expression4"
|"with_exprs_suffix1"
 as pat), arg2, arg3) ->
    let arg2' = classify chash arg2 in
    let arg3' = classify chash arg3 in
    Hashtbl.replace chash (arg2') ();
    Hashtbl.replace chash (arg3') ();
    Hashtbl.replace chash (TUPLE3 (STRING pat, arg2', arg3')) ();
    STRING pat
| TUPLE4(STRING (
 "generate_block1"
|"seq_block1"		 
|"case_item1"
|"add_expr2"
|"add_expr3"
|"assignment_pattern_expression4"
|"assignment_statement_no_expr1"
|"bitand_expr2"
|"bitand_expr3"
|"bitor_expr2"
|"bitor_expr3"
|"block_event_or_expr1"
|"caseeq_expr2"
|"caseeq_expr3"
|"comp_expr2"
|"comp_expr3"
|"comp_expr4"
|"comp_expr5"
|"comp_expr7"
|"constraint_expression_no_preprocessor1"
|"constraint_expression_no_preprocessor3"
|"covergroup_expression_bracketed_opt1"
|"equiv_impl_expr2"
|"equiv_impl_expr3"
|"event_expression4"
|"expr_mintypmax_generalized1"
|"expr_mintypmax_trans_set1"
|"expr_primary_parens1"
|"expression_in_parens1"
|"expression_in_parens2"
|"expression_list_proper1"
|"expression_or_null_list_opt1"
|"logand_expr2"
|"logeq_expr2"
|"logeq_expr3"
|"logeq_expr4"
|"logeq_expr5"
|"logor_expr2"
|"matches_expr2"
|"mul_expr2"
|"mul_expr3"
|"mul_expr4"
|"port_expression2"
|"pow_expr2"
|"property_implication_expr1"
|"sequence_and_expr1"
|"sequence_intersect_expr2"
|"sequence_or_expr1"
|"sequence_throughout_expr2"
|"sequence_within_expr2"
|"shift_expr2"
|"shift_expr3"
|"shift_expr4"
|"structure_or_array_pattern_expression1"
|"xor_expr2"
|"xor_expr3"
 as pat),arg2,arg3,arg4) ->
    Hashtbl.replace chash (classify chash arg2) ();
    Hashtbl.replace chash (classify chash arg3) ();
    Hashtbl.replace chash (classify chash arg4) ();
    Hashtbl.replace chash (TUPLE4 (STRING pat, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4)) ();
    STRING pat
| TUPLE5(STRING (
"conditional_generate_construct1"
|"conditional_statement1"
|"branch_probe_expression2"
|"constraint_expression_no_preprocessor8"
|"event_expression3"
|"iff_expr_opt1"
|"simple_sequence_expr1"
|"simple_sequence_expr3"
|"simple_sequence_expr4"
|"with_covergroup_expression_in_parens1"
 as pat),arg2,arg3,arg4,arg5) ->
    Hashtbl.replace chash (classify chash arg2) ();
    Hashtbl.replace chash (classify chash arg3) ();
    Hashtbl.replace chash (classify chash arg4) ();
    Hashtbl.replace chash (classify chash arg5) ();
    Hashtbl.replace chash (TUPLE5 (STRING pat, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5)) ();
    STRING pat
| TUPLE6(STRING ("cond_expr2"|"param_type_followed_by_id_and_dimensions_opt4"|"nonblocking_assignment1" as pat),arg2,arg3,arg4,arg5,arg6) ->
    Hashtbl.replace chash (classify chash arg2) ();
    Hashtbl.replace chash (classify chash arg3) ();
    Hashtbl.replace chash (classify chash arg4) ();
    Hashtbl.replace chash (classify chash arg5) ();
    Hashtbl.replace chash (classify chash arg6) ();
    Hashtbl.replace chash (TUPLE6 (STRING pat, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6)) ();
    STRING pat
| TUPLE7(STRING ("conditional_statement2" as pat),arg2,arg3,arg4,arg5,arg6,arg7) ->
    Hashtbl.replace chash (classify chash arg2) ();
    Hashtbl.replace chash (classify chash arg3) ();
    Hashtbl.replace chash (classify chash arg4) ();
    Hashtbl.replace chash (classify chash arg5) ();
    Hashtbl.replace chash (classify chash arg6) ();
    Hashtbl.replace chash (classify chash arg7) ();
    Hashtbl.replace chash (TUPLE7 (STRING pat, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7)) ();
    STRING pat
| TUPLE8(STRING ("case_statement1" as pat),arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
    Hashtbl.replace chash (classify chash arg2) ();
    Hashtbl.replace chash (classify chash arg3) ();
    Hashtbl.replace chash (classify chash arg4) ();
    Hashtbl.replace chash (classify chash arg5) ();
    Hashtbl.replace chash (classify chash arg6) ();
    Hashtbl.replace chash (classify chash arg7) ();
    Hashtbl.replace chash (classify chash arg8) ();
    Hashtbl.replace chash (TUPLE8 (STRING pat, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8)) ();
    STRING pat
| TUPLE3(STRING pat, (SymbolIdentifier id as arg2), arg3) -> Hashtbl.replace chash (TUPLE3 (STRING pat, (classify chash) arg2, (classify chash) arg3)) (); STRING pat
| TUPLE3(arg1,arg2,arg3) -> TUPLE3 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3)
| TUPLE4(arg1,arg2,arg3,arg4) -> TUPLE4 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4)
| TUPLE5(arg1,arg2,arg3,arg4,arg5) -> TUPLE5 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5)
| TUPLE6(arg1,arg2,arg3,arg4,arg5,arg6) -> TUPLE6 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6)
| TUPLE7(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> TUPLE7 ((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7)
| TUPLE8(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) ->
   TUPLE8((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8)
| TUPLE9(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9) ->
   TUPLE9((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8, (classify chash) arg9)
| TUPLE10(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10) ->
   TUPLE10((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8, (classify chash) arg9, (classify chash) arg10)
| TUPLE11(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11) ->
   TUPLE11((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8, (classify chash) arg9, (classify chash) arg10, (classify chash) arg11)
| TUPLE12(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12) ->
   TUPLE12((classify chash) arg1, (classify chash) arg2, (classify chash) arg3, (classify chash) arg4, (classify chash) arg5, (classify chash) arg6, (classify chash) arg7, (classify chash) arg8, (classify chash) arg9, (classify chash) arg10, (classify chash) arg11, (classify chash) arg12)
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
ACCEPT|TUPLE15 _|TUPLE14 _|TUPLE13 _|SLIST _|
CONS5 _|CONS4 _|CONS3 _|CONS2 _|CONS1 _
) as oth) -> oth
(*
| oth -> failwith ("classify fail: "^Source_text_verible_tokens.getstr oth)
*)
   
let chash = Hashtbl.create 255;;

let classify'' p' =
  Hashtbl.replace chash (classify chash p') ();
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  Hashtbl.iter (fun k _ -> Hashtbl.replace chash (classify chash k) ()) chash;
  ()

let classify' () =
  let lst = ref [] in
  Hashtbl.iter (fun k _ -> lst := k :: !lst) chash;
  List.sort compare !lst
