open Source_text_verible
open Verible_typ

let seqlst = ref []
let othpat' = ref End_of_file
let othpar' = ref End_of_file
let othloop = ref []
let othpatlst = ref []
let othexp = ref []
let othexp' = ref []
let othseq = ref []
let othseq' = ref []
let othcnst = ref (Input_types.CNST (32, ERR ""), Input_types.CNST (32, ERR ""))
let otha = ref (None, None)
let othitms' = ref (Input_dump.empty_itms [])
let othparam = ref None
let othparamlst = ref None

let rec pat' = function
| TK_StringLiteral s -> Vstring_literal s
| TLIST lst -> Vtlist (List.map pat lst)
| TUPLE9 (STRING "task_declaration1", Task, EMPTY_TOKEN, SymbolIdentifier id,
   EMPTY_TOKEN, SEMICOLON, TLIST lst, Endtask, EMPTY_TOKEN) -> (Vtask_declaration1 (patlst lst))
| TUPLE7 (STRING "conditional_statement2", EMPTY_TOKEN, If,
       expression_in_parens1,
       then_statement,
   Else, else_statement) -> Vconditional_statement2 (pat expression_in_parens1, pat then_statement, pat else_statement)
| TUPLE7 (STRING "expr_primary_braces2", LBRACE, braced, LBRACE,
  reference3, RBRACE, RBRACE) -> Vexpr_primary_braces2 (pat braced, pat reference3)
| TUPLE6 (STRING "cond_expr2", expr_primary, QUERY, expr_true, COLON, expr_false) ->
  Vcond_expr (pat expr_primary, pat expr_true, pat expr_false)
| TUPLE6 (STRING "continuous_assign1", Assign, EMPTY_TOKEN, dly, TLIST lst,
  SEMICOLON) -> (Vcontinuous_assign1 (patlst lst))
| TUPLE6 (STRING "decl_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK) -> Vdecl_variable_dimension1 (pat hi, pat lo)
| TUPLE6 (STRING "non_anonymous_gate_instance_or_register_variable2",
  SymbolIdentifier id, EMPTY_TOKEN, LPAREN, TLIST lst, RPAREN) -> (Vnon_anonymous_gate_instance_or_register_variable2 (patlst lst))
| TUPLE6 (STRING "nonblocking_assignment1", lhs, LT_EQ, EMPTY_TOKEN, rhs, SEMICOLON) -> (Vnonblocking_assignment1 (pat lhs, pat rhs))
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", (Logic|EMPTY_TOKEN),
       EMPTY_TOKEN, TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
         TK_DecNumber lo, RBRACK),
   SymbolIdentifier id, EMPTY_TOKEN) -> Vparam_type_followed_by_id_and_dimensions_opt4
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
  TK_DecNumber num, RPAREN) -> Vparameter_value_byname1
| TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
   unqualified_id1, RPAREN) -> Vparameter_value_byname1
| TUPLE7 (STRING "param_type_followed_by_id_and_dimensions_opt2", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN, SymbolIdentifier id,
         EMPTY_TOKEN) -> Vparam_type_followed_by_id_and_dimensions_opt2 (pat unqualified_id1, id)
| TUPLE5 (STRING "any_param_declaration4", Localparam,
       param_type_followed_by_id_and_dimensions_opt2,
       TUPLE4 (STRING "trailing_assign1", EQUALS, expr', EMPTY_TOKEN),
   SEMICOLON) -> Vany_param_declaration4 (pat param_type_followed_by_id_and_dimensions_opt2, pat expr')
| TUPLE5 (STRING "param_type_followed_by_id_and_dimensions_opt3", EMPTY_TOKEN,
         EMPTY_TOKEN, unqualified_id1, EMPTY_TOKEN)  -> (Vparam_type_followed_by_id_and_dimensions_opt3 (pat unqualified_id1))
| TUPLE5 (STRING "conditional_generate_construct1", expr, generate_block1, Else, conditional_generate_construct1) ->
  Vconditional_generate_construct1 (pat expr, pat generate_block1, pat conditional_generate_construct1)
| TUPLE3 (STRING "conditional_generate_construct2", expr, generate_block1) -> Vconditional_generate_construct2 (pat expr, pat generate_block1)
| TUPLE7 (STRING "conditional_generate_construct3", Case, LPAREN, expr, RPAREN, generate_case_items1, Endcase) ->
  Vconditional_generate_construct3 (pat expr, pat generate_case_items1)
| TUPLE3 (STRING "generate_case_items1",
    TUPLE3 (STRING "generate_case_items1",
		   TUPLE4 (STRING "generate_case_item1", caselbl, COLON, generate_block1), arg3),
		   TUPLE4 (STRING "generate_case_item2", Default, COLON, generate_dflt)) ->
  Vgenerate_case_items1 (pat generate_block1)
| TUPLE5 (STRING "conditional_statement1", EMPTY_TOKEN, If, expression_in_parens1, seq_block1) -> 
  Vconditional_statement1 (pat expression_in_parens1, pat seq_block1)
| TUPLE5 (STRING "event_control2", AT, LPAREN, TLIST lst, RPAREN) -> (Vevent_control2 (patlst lst))
| TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
       TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
         unqualified_id1),
   SEMICOLON) -> Vnet_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
       TUPLE3
       (STRING "data_type_or_implicit1",
         TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber hi, COLON,
           TK_DecNumber lo, RBRACK),
         EMPTY_TOKEN),
    net_variable1, SEMICOLON) -> Vnet_declaration2
| TUPLE5 (STRING "port_declaration_noattr1", dir, EMPTY_TOKEN, typ, EMPTY_TOKEN) -> Vport_declaration_noattr1 (pat dir, pat typ)
| TUPLE4 (STRING "assignment_statement_no_expr1", range_list_in_braces1, EQUALS, expr') ->
    Vassignment_statement_no_expr1 (pat range_list_in_braces1, pat expr')
| TUPLE4 (STRING "assign_modify_statement1", lhs, PLUS_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vadd_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement2", lhs, HYPHEN_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vsub_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement3", lhs, STAR_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vmul_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement4", lhs, SLASH_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vdiv_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement5", lhs, PERCENT_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vmod_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement6", lhs, AMPERSAND_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vbitand_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement7", lhs, VBAR_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vbitor_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement8", lhs, CARET_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vxor_expr (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement9", lhs, TK_LS_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vshift_expr2 (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement10", lhs, TK_RS_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vshift_expr3 (lhs', pat rhs))
| TUPLE4 (STRING "assign_modify_statement11", lhs, TK_RSS_EQ, rhs) -> let lhs' = pat lhs in
    Vassignment_statement_no_expr1 (lhs', Vshift_expr4 (lhs', pat rhs))
| TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN) -> (Vcall_base1 (patlst lst))
| TUPLE4 (STRING "case_item1", sel, COLON, seq_block) -> Vcase_item1 (pat sel, pat seq_block)
| TUPLE4 (STRING "case_item2", Default, COLON, SEMICOLON) -> Vcase_item2 Vempty
| TUPLE4 (STRING "case_item2", Default, COLON, seq_block) -> Vcase_item2 (pat seq_block)
| TUPLE4 (STRING "cont_assign1", unqualified_id1, EQUALS, expr) -> Vcont_assign1 (pat unqualified_id1, pat expr)
| TUPLE4 (STRING "expr_primary_parens1", LPAREN, TLIST lst, RPAREN) -> (Vexpr_primary_parens1 (patlst lst))
| TUPLE4 (STRING "expression_in_parens1", LPAREN, cond_expr2, RPAREN) -> (Vexpression_in_parens1 (pat cond_expr2))
| TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier id,
        TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> (Vsystem_tf_call1 (id, patlst lst))
| TUPLE4 (STRING "expression_list_proper1", unqualified_id1, COMMA, expr') -> Vexpression_list_proper1
| TUPLE4 (STRING "gate_instance_or_register_variable1", id, EMPTY_TOKEN, EMPTY_TOKEN) ->
  Vgate_instance_or_register_variable1 (pat id)
| TUPLE4 (STRING "generate_block1", begin1,
  TLIST lst, TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> (Vgenerate_block1 (pat begin1, (patlst lst)))
| TUPLE4 (STRING "generate_region1", Generate, TLIST lst, Endgenerate) -> (Vgenerate_region1 (patlst lst))
| TUPLE4 (STRING "net_declaration1", Wire,
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier id, EQUALS,
        unary_prefix_expr2),
    SEMICOLON) -> Vnet_declaration1
| TUPLE4 (STRING "net_declaration1", Wire, net_variable1, SEMICOLON) -> Vnet_declaration1
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
  SymbolIdentifier id, EMPTY_TOKEN, asgn) -> Vnon_anonymous_gate_instance_or_register_variable1 (id, pat asgn)
| TUPLE3 (STRING "trailing_decl_assignment2", EQUALS, rhs) -> Vtrailing_decl_assignment2
| TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1",
      SymbolIdentifier id, dim,
    EMPTY_TOKEN) -> Vnon_anonymous_gate_instance_or_register_variable1 (id, pat dim)
| TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST lst, RBRACE) -> (Vrange_list_in_braces1 (patlst lst))
| TUPLE4 (STRING "seq_block1", begin1, EMPTY_TOKEN, TUPLE3 (end1, End, EMPTY_TOKEN)) -> (Vseq_block1 (Vtlist []))
| TUPLE4 (STRING "seq_block1", begin1, TLIST lst, TUPLE3 (end1, End, EMPTY_TOKEN)) -> (Vseq_block1 (patlst lst))
| TUPLE3 (STRING "always_construct1", Always, procedural_timing_control_statement2) -> (Valways_construct1 (pat procedural_timing_control_statement2))
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE3 (STRING "reference2", unqualified_id1,
    TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2))) -> (Vany_argument_list_item_last2 (patlst lst))
| TUPLE3 (STRING "any_argument_list_item_last2",
      TUPLE3 (STRING "any_argument_list_trailing_comma1", TLIST lst, COMMA),
   cond_expr2) -> (Vany_argument_list_item_last2 (patlst lst))
| TUPLE3 (STRING "begin1", Begin, EMPTY_TOKEN) -> Vbegin1 Vempty
| TUPLE3 (STRING "bin_based_number1", TK_BinBase base, TK_BinDigits digits) -> Vbin_based_number1 (base, digits)
| TUPLE3 (STRING "block_item_or_statement_or_null6", unqualified_id1, SEMICOLON) -> Vblock_item_or_statement_or_null6
| TUPLE3 (STRING "case_items1", item1, item2) -> Vcase_items1 (pat item1, pat item2)
| TUPLE3 (STRING "data_declaration_or_module_instantiation1",
   instantiation_base1, SEMICOLON) -> Vdata_declaration_or_module_instantiation1 (pat instantiation_base1)
| TUPLE3 (STRING "end1", End, EMPTY_TOKEN) -> Vend1
| TUPLE3 (STRING "event_control4", AT, STAR) -> Vevent_control4
| TUPLE3 (STRING "event_expression1", Posedge, unqualified_id1) -> Vevent_expression_posedge (pat unqualified_id1)
| TUPLE3 (STRING "event_expression1", Negedge, unqualified_id1) -> Vevent_expression_negedge (pat unqualified_id1)
| TUPLE3 (STRING "expression_or_dist1", cond_expr2, EMPTY_TOKEN) -> Vexpression_or_dist1 (pat cond_expr2)
| TUPLE3 (STRING "initial_construct1", Initial, seq_block1) -> Vinitial_construct1 (pat seq_block1)
   | TUPLE3 (STRING "instantiation_base1",
      TUPLE3 (STRING "data_type_primitive1",
        TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN),
        EMPTY_TOKEN),
   TLIST lst) -> (Vinstantiation_base1_reg (patlst lst))
| TUPLE3 (STRING "instantiation_base1", unqualified_id1, TLIST lst) -> Vinstantiation_base1 (pat unqualified_id1, patlst' lst)
| TUPLE3 (STRING "ml_start1", TLIST lst, End_of_file) -> (Vml_start1 (patlst lst))
| TUPLE3 (STRING "module_parameter_port_list_item_last1",
TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA), nxt) ->
  Vmodule_parameter_port_list_item_last1 (patlst (nxt :: lst))
(*
 | TUPLE3 (STRING "module_parameter_port_list_item_last1",
      TUPLE3 (STRING "module_parameter_port_list_trailing_comma1", TLIST lst, COMMA), nxt) ->
  Vmodule_parameter_port_list_item_last1 (patlst (nxt :: lst))
*)
| TUPLE3 (STRING "net_variable1", SymbolIdentifier id, EMPTY_TOKEN) -> Vnet_variable1
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
    TK_DecNumber num, RPAREN)) -> (Vparameter_value_byname_list_item_last2 (patlst lst))
| TUPLE3 (STRING "parameter_value_byname_list_item_last2",
      TUPLE3 (STRING "parameter_value_byname_list_trailing_comma1", TLIST lst, COMMA),
      TUPLE6 (STRING "parameter_value_byname1", DOT, SymbolIdentifier id, LPAREN,
     unqualified_id1, RPAREN)) -> (Vparameter_value_byname_list_item_last2 (patlst lst))
| TUPLE3 (STRING "port1", TUPLE3 (STRING "port_reference1", unqualified_id1, EMPTY_TOKEN), EMPTY_TOKEN) -> Vport1 (pat unqualified_id1)
| TUPLE3 (STRING "procedural_timing_control_statement2", stmt, seq_block) -> (Vprocedural_timing_control_statement2 (pat stmt, pat seq_block))
| TUPLE6 (STRING "select_variable_dimension1", LBRACK, hi, COLON, lo, RBRACK) -> Vselect_variable_dimension1 (pat hi, pat lo)
| TUPLE4 (STRING "select_variable_dimension2", LBRACK, ix, RBRACK) -> Vselect_variable_dimension2 (pat ix)
| TUPLE6 (STRING "select_variable_dimension3", LBRACK, unqualified_id2, PLUS_COLON, unqualified_id3, RBRACK) -> Vselect_variable_dimension3 (pat unqualified_id2, pat unqualified_id3)
| TUPLE3 (STRING "reference_or_call_base1", unqualified_id1,
  TUPLE4 (STRING "call_base1", LPAREN, TLIST lst, RPAREN)) -> (Vreference_or_call_base1 (patlst lst))
| TUPLE3 (STRING "sequence_repetition_expr1", expression_or_dist1,
   EMPTY_TOKEN) -> Vsequence_repetition_expr1 (pat expression_or_dist1)
| TUPLE3 (STRING "statement3", reference, SEMICOLON) -> Vstatement3 (pat reference)
| TUPLE3 (STRING "statement_item6", assignment_statement_no_expr1, SEMICOLON) -> (Vstatement_item6 (pat assignment_statement_no_expr1))
| TUPLE3 (STRING "unary_prefix_expr2", PLUS, reference) -> (Vunary_prefix_expr_plus (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", VBAR, reference) -> (Vunary_prefix_expr_or (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", CARET, reference) -> (Vunary_prefix_expr_xor (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE_CARET, reference) -> (Vunary_prefix_expr_xnor (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE_VBAR, reference) -> (Vunary_prefix_expr_nor (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE, reference) -> (Vunary_prefix_expr_tilde (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", PLING, reference) -> (Vunary_prefix_expr_not (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", HYPHEN, reference) -> (Vunary_prefix_expr_negate (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", AMPERSAND, reference) -> (Vunary_prefix_expr_and (pat reference))
| TUPLE3 (STRING "unary_prefix_expr2", TILDE_AMPERSAND, reference) -> (Vunary_prefix_expr_nand (pat reference))
| TUPLE3 (STRING "unqualified_id1", SymbolIdentifier id, param) -> (Vunqualified_id1 (id, pat param))
| TUPLE5 (STRING "parameters2", HASH, LPAREN, TLIST lst, RPAREN) -> (Vparameters2 (patlst lst))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endmodule, EMPTY_TOKEN) -> (Vmodule_or_interface_declaration1 (id, Vempty, patlst lst, Vtlist []))
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, EMPTY_TOKEN, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> Vmodule_or_interface_declaration1 (id, Vempty, Vtlist [], patlst lst')
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, EMPTY_TOKEN,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) -> Vmodule_or_interface_declaration1 (id, Vempty, patlst lst, patlst lst')
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, param,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule, EMPTY_TOKEN) ->
  othpar' := param;
  Vmodule_or_interface_declaration1 (id, pat param, patlst lst, patlst lst')
| TUPLE5 (STRING "module_parameter_port_list_opt1", HASH, LPAREN, TLIST lst, RPAREN) ->
  let lst' = patlst lst in
  othparamlst := Some lst';
  Vmodule_parameter_port_list_opt1 (lst')
| TUPLE6 (STRING "cast1", data_type_primitive1, QUOTE, LPAREN, reference3, RPAREN) -> Vcast1
| TK_DecNumber num -> Vdec_num num
| SymbolIdentifier id -> Vident id
| STRING _ -> Vstring
| COMMA -> Vcomma
| LESS -> Vless
| TUPLE6 (STRING "param_type_followed_by_id_and_dimensions_opt4", Logic,
      EMPTY_TOKEN,
      TUPLE3 (STRING "decl_dimensions2",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK)),
   SymbolIdentifier _, EMPTY_TOKEN) -> Vparam_type_followed_by_id_and_dimensions_opt4
| TUPLE6
     (STRING "simple_immediate_assertion_statement1", Assert, LPAREN,
      reference3, RPAREN,
      TUPLE3
       (STRING "action_block3", Else,
        TUPLE3
      (STRING "statement3", reference_or_call_base1, SEMICOLON))) -> Vsimple_immediate_assertion_statement1
| TUPLE5
     (STRING "data_declaration_or_module_instantiation2", Automatic,
   EMPTY_TOKEN, instantiation_base1, SEMICOLON) -> Vdata_declaration_or_module_instantiation2
| TUPLE5
     (STRING "net_declaration2", Wire,
      TUPLE3
       (STRING "data_type_or_implicit1",
        TUPLE6
         (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE3 (STRING "delay31", HASH, TK_RealTime _)),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        TUPLE4 (STRING "range_list_in_braces1", LBRACE, TLIST _, RBRACE)),
   SEMICOLON) -> Vnet_declaration2
| TUPLE5 (STRING "net_declaration2", Wire,
      TUPLE3 (STRING "data_type_or_implicit1",
        TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
          TK_DecNumber _, RBRACK),
        TUPLE3 (STRING "delay31", HASH, TK_RealTime _)),
      TUPLE4 (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS, unqualified_id1),
   SEMICOLON) -> Vnet_declaration2
| TUPLE5
     (STRING "net_declaration4", Wire,
      TUPLE3 (STRING "delay31", HASH, TK_RealTime _),
      TUPLE4
       (STRING "net_decl_assign1", SymbolIdentifier _, EQUALS,
        cond_expr2),
   SEMICOLON) -> Vnet_declaration4
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
         arg2, arg3, EMPTY_TOKEN) -> Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1 (pat arg2, pat arg3)
| TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
        decl_variable_dimension1,
        unqualified_id1, EMPTY_TOKEN) ->
  Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4 (pat decl_variable_dimension1, pat unqualified_id1)
| TUPLE5 (STRING "port_declaration_noattr1", dir, Wire, typ, EMPTY_TOKEN) -> Vport_declaration_noattr1 (pat dir, pat typ)
| TUPLE6 (STRING "type_declaration1", Typedef, data_type_primitive1, id_t, EMPTY_TOKEN, SEMICOLON) ->
  Vtype_declaration1 (pat data_type_primitive1, pat id_t)
| TUPLE3 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4", unqualified_id1, EMPTY_TOKEN) ->
   Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4 (pat unqualified_id1)
| TUPLE5 (STRING "type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2",
		 unqualified_id1, EMPTY_TOKEN, unqualified_id2, EMPTY_TOKEN) ->
		 Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2 (pat unqualified_id1, pat unqualified_id2)
| TUPLE4 (STRING "generate_block1", begin1, EMPTY_TOKEN,
   TUPLE3 (STRING "end1", End, EMPTY_TOKEN)) -> (Vgenerate_block1 (pat begin1, Vtlist []))
| TUPLE4 (STRING "jump_statement4", Return, unqualified_id1, SEMICOLON) -> Vjump_statement4
| TUPLE4 (STRING "module_parameter_port1", Parameter, param,
    TUPLE4 (STRING "trailing_assign1", EQUALS, rhs, EMPTY_TOKEN)) -> Vmodule_parameter_port1 (pat param, pat rhs)
| TUPLE4 (STRING "module_parameter_port2", Parameter, Type,
      TUPLE4 (STRING "type_assignment1", lhs, EQUALS, rhs)) -> Vmodule_parameter_port2 (pat lhs, pat rhs)
| TUPLE4 (STRING "port_declaration_noattr4",
      TUPLE6 (STRING "type_identifier_followed_by_id3", unqualified_id1, DOT,
        SymbolIdentifier _, EMPTY_TOKEN, SymbolIdentifier _),
   EMPTY_TOKEN, EMPTY_TOKEN) -> Vport_declaration_noattr4
   | TUPLE4 (STRING "qualified_id2", unqualified_id1, COLON_COLON,
   unqualified_id2) -> Vqualified_id2
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6 (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> Vtf_port_item1
   | TUPLE4 (STRING "tf_port_item1", EMPTY_TOKEN,
      TUPLE4 (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Byte, EMPTY_TOKEN),
          EMPTY_TOKEN),
        unqualified_id1, EMPTY_TOKEN),
   EMPTY_TOKEN) -> Vtf_port_item1
| TUPLE3 (STRING "always_construct1", Always_latch, seq_block1) -> (Valways_construct1 (pat seq_block1))
| TUPLE3 (STRING "always_construct1", Always_ff,
   procedural_timing_control_statement2) -> (Valways_construct1 (pat procedural_timing_control_statement2))
| TUPLE3 (STRING "always_construct1", Always_comb, seq_block1) -> (Valways_construct1 (pat seq_block1))
| TUPLE3 (STRING "any_port_list_trailing_comma1", TLIST lst, COMMA) -> Vany_port_list_trailing_comma1 (patlst lst)
| TUPLE3 (STRING "any_port_list_item_last1", trailing, named) -> Vany_port_list_item_last1 (pat trailing, pat named)
| TUPLE6 (STRING "port_named1", DOT, SymbolIdentifier id, LPAREN, unqualified_id1, RPAREN) ->  Vport_named1 (id, pat unqualified_id1)
| TUPLE5 (STRING "port_named2", DOT, SymbolIdentifier id, LPAREN, RPAREN) -> Vport_named2 id
| TUPLE3 (STRING "port_named3", DOT, SymbolIdentifier id) -> Vport_named3 id
| TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier id) -> Vlabel_opt1 id
| TUPLE3 (STRING "begin1", Begin, label) -> Vbegin1 (pat label)
| TUPLE3 (STRING "generate_if1", If, expression_in_parens1) -> Vgenerate_if1 (pat expression_in_parens1)
| TUPLE3 (STRING "data_type_primitive_scalar1", Reg, EMPTY_TOKEN) -> Vdata_type_primitive_scalar1_reg
| TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN) -> Vdata_type_primitive_scalar1_logic
| TUPLE3 (STRING "data_type_primitive1", prim, dim) -> Vdata_type_primitive1 (pat prim, pat dim)
| TUPLE5 (STRING "enum_data_type1", Enum, LBRACE, TLIST enum_name_list_item, RBRACE) ->
  Venum_data_type1 (patlst enum_name_list_item)
| TUPLE6 (STRING "enum_data_type2", Enum, data_type_primitive1, LBRACE, TLIST enum_name_list_item, RBRACE) ->
  Venum_data_type2 (pat data_type_primitive1, patlst enum_name_list_item)
| TUPLE3 (STRING "enum_name_list_item_last1", enum_name_list_trailing_comma1, id) ->
  Venum_name_list_item_last1 (pat enum_name_list_trailing_comma1, pat id)
| TUPLE3 (STRING "enum_name_list_trailing_comma1", TLIST lst, COMMA) -> Venum_name_list_trailing_comma1 (patlst lst)
| TUPLE3 (STRING "dec_based_number1", TK_DecBase b, TK_DecDigits d) -> Vdec_based_number1 (b, d)
| TUPLE3 (STRING "decl_dimensions2", dim1, dim2) -> Vdecl_dimensions2 (pat dim1, pat dim2)
| TUPLE3 (STRING "hex_based_number1", TK_HexBase b, TK_HexDigits d) -> Vhex_based_number1 (b, d)
| TUPLE3 (STRING "inc_or_dec_expression2", unqualified_id1, PLUS_PLUS) -> Vinc_or_dec_expression2
| TUPLE3 (STRING "reference2", unqualified_id1, ext) -> Vreference2 (pat unqualified_id1, pat ext)
| TUPLE3 (STRING "hierarchy_extension1", DOT, unqualified_id2) -> Vhierarchy_extension1 (pat unqualified_id2)
| TUPLE3 (STRING "reference3", unqualified_id1, dim) -> Vreference3 (pat unqualified_id1, pat dim)
| TUPLE12 (STRING "module_or_interface_declaration1", Module, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, params,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
      EMPTY_TOKEN, SEMICOLON, TLIST lst', Endmodule,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> Vmodule_or_interface_declaration1 (id, pat params, patlst lst, patlst lst')
| TUPLE12 (STRING "module_or_interface_declaration1", Interface, EMPTY_TOKEN,
      SymbolIdentifier id, EMPTY_TOKEN, params,
      TUPLE4 (STRING "module_port_list_opt1", LPAREN, TLIST lst, RPAREN),
   EMPTY_TOKEN, SEMICOLON, EMPTY_TOKEN, Endinterface, EMPTY_TOKEN) -> Vmodule_or_interface_declaration1 (id, pat params, patlst lst, patlst [])
| TUPLE11 (STRING "function_declaration1", Function, EMPTY_TOKEN,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt5",
        Void, unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> Vfunction_declaration1
| TUPLE11 (STRING "function_declaration1", Function, Automatic,
      TUPLE4
       (STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt1",
        TUPLE3
         (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar1", Logic, EMPTY_TOKEN),
          TUPLE6
           (STRING "decl_variable_dimension1", LBRACK, TK_DecNumber _, COLON,
            TK_DecNumber _, RBRACK)),
        unqualified_id1, EMPTY_TOKEN),
      LPAREN, TLIST _, RPAREN, SEMICOLON, TLIST _, Endfunction,
   TUPLE3 (STRING "label_opt1", COLON, SymbolIdentifier _)) -> Vfunction_declaration1
(*
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE4 (STRING "for_init_decl_or_assign1", unqualified_id1, EQUALS, TK_DecNumber num),
      SEMICOLON, expr', SEMICOLON, assignment_statement_no_expr1,
   RPAREN, seq_block) -> Vloop_statement1 (pat expr', pat seq_block)
*)
| TUPLE10 (STRING "loop_statement1", For, LPAREN,
      TUPLE5 (STRING "for_init_decl_or_assign2",
        TUPLE3 (STRING "data_type_primitive1",
          TUPLE3 (STRING "data_type_primitive_scalar5", Int, (Unsigned|EMPTY_TOKEN)),
          EMPTY_TOKEN),
        SymbolIdentifier ix, EQUALS, init),
      SEMICOLON, cond, SEMICOLON, inc_or_dec_expression2, RPAREN,
   seq_block) -> Vloop_statement1 (ix, pat init, pat cond, pat inc_or_dec_expression2, pat seq_block)
| TUPLE4 (STRING "assignment_pattern1", QUOTE_LBRACE, TLIST lst, RBRACE) -> (Vassignment_pattern1 (patlst lst))
| TUPLE3 (STRING "case_inside_items1",
        TUPLE4 (STRING "case_inside_item1", TLIST _, COLON,
          TUPLE4 (STRING "jump_statement4", Return, STRING "bin_based_number1", SEMICOLON)),
        TUPLE4 (STRING "case_inside_item2", Default, COLON,
          TUPLE4 (STRING "jump_statement4", Return, STRING "bin_based_number1", SEMICOLON))) -> Vcase_inside_items1
| TUPLE9 (STRING "case_statement3", Unique, Case, LPAREN, unqualified_id1, RPAREN, Inside, items, Endcase) ->
    Vcase_statement3
| TUPLE9 (STRING "case_statement3", EMPTY_TOKEN, Case, LPAREN, unqualified_id1, RPAREN, Inside, case_inside_items1, Endcase) ->
    Vcase_statement3
| TUPLE4 (STRING "case_inside_item1", TLIST _, COLON, STRING "seq_block1") -> Vcase_inside_item1
| TUPLE4 (STRING "case_inside_item2", Default, COLON, stmt) -> Vcase_inside_item2
| TUPLE8 (STRING "case_statement1", unique, Case, LPAREN, expr, RPAREN, items, Endcase) ->
    Vcase_statement1 (pat unique, pat expr, pat items)
| TUPLE4 (STRING "comp_expr2", expr1, LESS, expr2) -> Vlt_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "comp_expr3", expr1, GREATER, expr2) -> Vgt_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "comp_expr4", expr1, LT_EQ, expr2) -> Vlteq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "comp_expr5", expr1, GT_EQ, expr2) -> Vgteq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logand_expr2", expr1, AMPERSAND_AMPERSAND, expr2) -> Vlogand_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logor_expr2", expr1, VBAR_VBAR, expr2) -> Vlogor_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logeq_expr2", expr1, EQ_EQ, expr2) -> Vlogeq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "logeq_expr3", expr1, PLING_EQ, expr2) -> Vplingeq_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "bitand_expr2", expr1, AMPERSAND, expr2) -> Vbitand_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "bitor_expr2", expr1, VBAR, expr2) -> Vbitor_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "xor_expr2", expr1, CARET, expr2) -> Vxor_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "add_expr2", expr1, PLUS, expr2) -> Vadd_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "add_expr3", expr1, HYPHEN, expr2) -> Vsub_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr2", expr1, STAR, expr2) -> Vmul_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr3", expr1, SLASH, expr2) -> Vdiv_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "mul_expr4", expr1, PERCENT, expr2) -> Vmod_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "pow_expr2", expr1, STAR_STAR, expr2) -> Vpow_expr (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr2", expr1, LT_LT, expr2) -> Vshift_expr2 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr3", expr1, GT_GT, expr2) -> Vshift_expr3 (pat expr1, pat expr2)
| TUPLE4 (STRING "shift_expr4", expr1, GT_GT_GT, expr2) -> Vshift_expr4 (pat expr1, pat expr2)
| TUPLE7 (STRING "module_port_declaration3", dir, signed, dim, EMPTY_TOKEN, TLIST dimlst, SEMICOLON) ->
    Vmodule_port_declaration3 (pat dir, pat signed, pat dim, patlst dimlst)
| TUPLE5 (STRING "module_port_declaration5", dir, signed, TLIST idlst, SEMICOLON) ->
    Vmodule_port_declaration5 (pat dir, pat signed, patlst idlst)
| TUPLE7 (STRING "module_port_declaration7", dir, Reg, EMPTY_TOKEN, dim, TLIST [SymbolIdentifier id], SEMICOLON) ->
    Vmodule_port_declaration7_reg (pat dir, pat dim, id)
| TUPLE3 (STRING "identifier_optional_unpacked_dimensions1", SymbolIdentifier id, EMPTY_TOKEN) ->
Videntifier_optional_unpacked_dimensions1 id
| EMPTY_TOKEN -> Vempty
| Signed -> Vsigned
| Unique -> Vunique
| Input -> Vinput
| Output -> Voutput
| TUPLE5 (
      STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt3", 
      Signed, 
      decl_variable_dimension1,
      unqualified_id1,
      EMPTY_TOKEN) -> Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt3 (Vsigned, pat decl_variable_dimension1, pat unqualified_id1)
| oth -> othpat' := oth; (* failtoken oth; *) try failwith "pat" with _ -> Printexc.print_backtrace stdout; Vempty

and patlst lst = Vtlist (List.map pat lst)

and patlst' lst = Vtlist (List.map (function
  | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1", SymbolIdentifier s, EMPTY_TOKEN, EMPTY_TOKEN) -> Vident s
  | TUPLE4 (STRING "non_anonymous_gate_instance_or_register_variable1", SymbolIdentifier mem, decl_variable_dimension1, EMPTY_TOKEN) ->
    Vident mem
  | TUPLE4 (STRING "gate_instance_or_register_variable1", SymbolIdentifier s, EMPTY_TOKEN, EMPTY_TOKEN) -> Vident s
  | oth -> othpat' := oth; failwith "patlst'") lst)

and pat itm = othpatlst := itm :: !othpatlst; let rslt = pat' itm in othpatlst := List.tl !othpatlst; rslt

let rec bin_to_native x = match String.length x with
| 0 -> 0
| 1 -> Char.code (x.[0]) - Char.code '0'
| n -> bin_to_native (String.sub x 0 (n-1)) * 2 + bin_to_native (String.sub x (n-1) 1)

let trandir  = function
| Vinput -> Dump_types.Dinput
| Voutput -> Doutput
| oth -> fail "trandir" oth

let addio (itms:Input_types.itms) (id, itm) = if not (List.mem_assoc id !(itms.io)) then itms.io := (id, itm) :: !(itms.io)

let param' (itms:Input_types.itms) id =
let eval = ref None in
  List.iter (function (id_t, (_, (w, Dump_types.HEX n))) ->
    if id = id_t then eval := Some (w, Dump_types.HEX n)
   | oth -> ()) !(itms.cnst);
  !eval

let enum' (itms:Input_types.itms) id =
let eval = ref None in
  List.iter (function (id_t, ("", (_, "logic", rng, []), enumlst, len)) ->
  List.iteri (fun ix -> function
    | Input_types.SCOPE itm ->
    if false then print_endline itm;
    if id = itm then eval := Some (rng, Dump_types.HEX ix)
    | oth -> ()) enumlst
   | oth -> ()) !(itms.iv);
  !eval

let enum'' itms id = match enum' itms id with
  | None -> None
  | Some (TYPRNG(HEX hi,HEX lo),x) -> Some (Input_types.CNST(hi-lo+1, x))
  | Some (TYPNONE,x) -> Some (Input_types.CNST(1, x))
  | oth -> None

let rec tran (itms:Input_types.itms) modnam = function
| Vml_start1 (Vtlist modlst) -> List.iter (tran itms modnam) modlst 
| Vmodule_or_interface_declaration1 (modnam', params, Vtlist declst, Vtlist bodylst) ->
    othparam := Some params;
    if params <> Vempty then tran itms modnam params;
    modnam := modnam';
    List.iter (tran itms modnam) (List.rev declst);
    List.iter (tran itms modnam) bodylst
| Vport_declaration_noattr1 (dir, Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1
       (Vdata_type_primitive1 ((Vdata_type_primitive_scalar1_reg|Vdata_type_primitive_scalar1_logic), typ),
        Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "reg", trantyp itms typ, []), trandir dir, "wire", []))
| Vport_declaration_noattr1 (dir, Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt3
       (Vsigned, Vdecl_variable_dimension1 (hi, lo), Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "reg", TYPRNG (tran' itms  hi, tran' itms  lo), [TYPSIGNED]), trandir dir, "wire", []))
| Vport_declaration_noattr1 (dir, Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4
       (Vdecl_variable_dimension1 (hi, lo), Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "reg", TYPRNG (tran' itms  hi, tran' itms  lo), []), trandir dir, "wire", []))
| Vport_declaration_noattr1 (dir, Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4
       (Vunqualified_id1 (id, Vempty))) ->
    addio itms (id, ("", (BASDTYP, "wire", TYPNONE, []), trandir dir, "wire", []))
| Valways_construct1 (Vprocedural_timing_control_statement2 (Vevent_control2 (Vtlist
             [Vevent_expression_posedge (Vunqualified_id1 (clk, Vempty))]), body)) -> 
    itms.alwys := ("", Input_types.POSEDGE clk, tranlst'' itms body) :: !(itms.alwys)
| Valways_construct1 (Vprocedural_timing_control_statement2 (Vevent_control2 (Vtlist
             (Vevent_expression_posedge (Vunqualified_id1 (clk, Vempty)) ::
              Vevent_expression_posedge (Vunqualified_id1 (rst, Vempty)) :: [])), body)) -> 
    itms.alwys := ("", Input_types.POSPOS (clk, rst), tranlst'' itms body) :: !(itms.alwys)
| Valways_construct1 (Vprocedural_timing_control_statement2 (Vevent_control4, Vseq_block1 (Vtlist seq))) ->
    
    itms.alwys := ("", Input_types.COMB, (SNTRE [] :: List.map (tran'' itms) (transeqlst seq))) :: !(itms.alwys)
| Valways_construct1 (Vseq_block1 (Vtlist seq)) ->
    itms.alwys := ("", Input_types.COMB, (SNTRE [] :: List.map (tran'' itms) (transeqlst seq))) :: !(itms.alwys)
| Valways_construct1 stmt ->
    itms.alwys := ("", Input_types.COMB, (SNTRE [] :: (tran'' itms) stmt :: [])) :: !(itms.alwys)
| Vcomma -> ()
| Vport1 (Vunqualified_id1 (id, Vempty)) -> print_endline ("Vport1: "^id)
| Vmodule_port_declaration7_reg (dir, typ, id) ->
    addio itms (id, ("", (BASDTYP, "reg", trantyp itms typ, []), trandir dir, "wire", []))
| Vmodule_port_declaration3 (dir, signed, Vdecl_variable_dimension1 (hi, lo), Vtlist iolst) -> List.iter (function
    | Videntifier_optional_unpacked_dimensions1 id -> 
        addio itms (id, ("", (BASDTYP, "reg", TYPRNG (tran' itms  hi, tran' itms  lo), transign signed), trandir  dir, "wire", []))
    | oth -> fail "tran iolst" oth) iolst
| Vmodule_port_declaration5 (dir, Vempty, Vtlist lst) ->
    List.iter (function
      | Vempty -> ()
      | Vunqualified_id1 (id, Vempty) -> addio itms (id, ("", (BASDTYP, "logic", TYPNONE, []), trandir dir, "logic", []))
      | oth -> fail "module_port_declaration5" oth) lst
| Vcontinuous_assign1 (Vtlist lst) -> List.iter (function
    | Vcont_assign1 (lhs, rhs) -> itms.ca := ("", (tran'' itms) lhs, (tran'' itms) rhs) :: !(itms.ca)
    | oth -> fail "cont_assign" oth) lst
| Vtype_declaration1 (Vdata_type_primitive1 (Venum_data_type1 (Vtlist lst), Vempty), Vident id_t) ->
  itms.iv := (id_t, ("", (BASDTYP, "logic", TYPNONE, []), List.map tranenum lst, List.length lst)) :: !(itms.iv)
| Vtype_declaration1 (Vdata_type_primitive1 (Venum_data_type2
       (Vdata_type_primitive1 (Vdata_type_primitive_scalar1_logic, decl_variable_dimension1), Vtlist lst), Vempty),
   Vident id_t) -> itms.iv := (id_t, ("", (BASDTYP, "logic", trantyp itms decl_variable_dimension1, []), List.map tranenum lst, List.length lst)) :: !(itms.iv)
| Vdata_declaration_or_module_instantiation1 (Vinstantiation_base1 (Vunqualified_id1 (id_t, Vempty), Vtlist lst)) ->
let (_, typ, _, _) = List.assoc id_t !(itms.iv) in
List.iter (function 
  | Vident nam ->  itms.v := (nam, ("", typ, id_t, (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
  | oth -> fail "tran'" oth) lst
| Vdata_declaration_or_module_instantiation1
   (Vinstantiation_base1 (Vdata_type_primitive1 (Vdata_type_primitive_scalar1_logic, typ), Vtlist lst)) ->
List.iter (function 
  | Vident nam ->  itms.v := (nam, ("", (BASDTYP, "logic", trantyp itms typ, []), nam, (UNKDTYP, "", TYPNONE, []))) :: !(itms.v)
  | oth -> fail "tran'" oth) lst
| Vmodule_parameter_port_list_opt1 (Vtlist lst) -> List.iter (tran itms modnam) lst
| Vmodule_parameter_port_list_item_last1 (Vtlist lst) -> List.iter (tran itms modnam) lst
| Vmodule_parameter_port1 (Vparam_type_followed_by_id_and_dimensions_opt3 (Vunqualified_id1 (id, Vempty)), rhs) ->
  print_endline ("param: "^id);
  itms.cnst := (id, (false, tranp' itms rhs)) :: !(itms.cnst);
  othitms' := itms;
| Vconditional_generate_construct3 (expr, generate_case_items1) -> print_endline "conditional_generate_construct3"
| oth -> fail "tran" oth

and transubst subst = function
| Vbin_based_number1 (radix, bin) -> Vbin_based_number1 (radix, bin)
| Vdec_based_number1 (radix, bin) -> Vdec_based_number1 (radix, bin)
| Vhex_based_number1 (radix, bin) -> Vhex_based_number1 (radix, bin)
| Vadd_expr (lhs, rhs) -> Vadd_expr (transubst subst lhs, transubst subst rhs)
| Vsub_expr (lhs, rhs) -> Vsub_expr (transubst subst lhs, transubst subst rhs)
| Vmul_expr (lhs, rhs) -> Vmul_expr (transubst subst lhs, transubst subst rhs)
| Vdiv_expr (lhs, rhs) -> Vdiv_expr (transubst subst lhs, transubst subst rhs)
| Vmod_expr (lhs, rhs) -> Vmod_expr (transubst subst lhs, transubst subst rhs)
| Vpow_expr (lhs, rhs) -> Vpow_expr (transubst subst lhs, transubst subst rhs)
| Vshift_expr2 (lhs, rhs) -> Vshift_expr2 (transubst subst lhs, transubst subst rhs)
| Vshift_expr3 (lhs, rhs) -> Vshift_expr3 (transubst subst lhs, transubst subst rhs)
| Vshift_expr4 (lhs, rhs) -> Vshift_expr4 (transubst subst lhs, transubst subst rhs)
| Vlogand_expr (lhs, rhs) -> Vlogand_expr (transubst subst lhs, transubst subst rhs)
| Vbitand_expr (lhs, rhs) -> Vbitand_expr (transubst subst lhs, transubst subst rhs)
| Vbitor_expr (lhs, rhs) -> Vbitor_expr (transubst subst lhs, transubst subst rhs)
| Vxor_expr (lhs, rhs) -> Vxor_expr (transubst subst lhs, transubst subst rhs)
| Vlogeq_expr (lhs, rhs) -> Vlogeq_expr (transubst subst lhs, transubst subst rhs)
| Vlt_expr (lhs, rhs) -> Vlt_expr (transubst subst lhs, transubst subst rhs)
| Vgt_expr (lhs, rhs) -> Vgt_expr (transubst subst lhs, transubst subst rhs)
| Vgteq_expr (lhs, rhs) -> Vgteq_expr (transubst subst lhs, transubst subst rhs)
| Vlteq_expr (lhs, rhs) -> Vlteq_expr (transubst subst lhs, transubst subst rhs)
| Vplingeq_expr (lhs, rhs) -> Vplingeq_expr (transubst subst lhs, transubst subst rhs)
| Vunqualified_id1 (_, Vempty) as id -> if Hashtbl.mem subst id then Hashtbl.find subst id else id
| Vconditional_statement1 (cond, then_) -> Vconditional_statement1 (transubst subst cond, transubst subst then_)
| Vconditional_statement2 (cond, then_, else_) -> Vconditional_statement2 (transubst subst cond, transubst subst then_, transubst subst else_)
| Vstatement_item6 stmt -> Vstatement_item6 (transubst subst stmt)
| Vassignment_statement_no_expr1 (rhs, lhs) -> Vassignment_statement_no_expr1 (transubst subst rhs, transubst subst lhs)
| Vcase_statement1 (Vempty, sel, Vtlist itmlst) -> Vcase_statement1 (Vempty, transubst subst sel, transubstlst subst itmlst)
| Vcase_item1 (sel, stmt) -> Vcase_item1 (transubst subst sel, transubst subst stmt)
| Vnonblocking_assignment1 (lhs, rhs) -> Vnonblocking_assignment1 (transubst subst lhs, transubst subst rhs)
| Vunary_prefix_expr_and othtran -> Vunary_prefix_expr_and (transubst subst othtran)
| Vunary_prefix_expr_nand othtran -> Vunary_prefix_expr_nand (transubst subst othtran)
| Vunary_prefix_expr_or othtran -> Vunary_prefix_expr_or (transubst subst othtran)
| Vunary_prefix_expr_nor othtran -> Vunary_prefix_expr_nor (transubst subst othtran)
| Vunary_prefix_expr_xor othtran -> Vunary_prefix_expr_xor (transubst subst othtran)
| Vunary_prefix_expr_xnor othtran -> Vunary_prefix_expr_xnor (transubst subst othtran)
| Vunary_prefix_expr_tilde othtran -> Vunary_prefix_expr_tilde (transubst subst othtran)
| Vunary_prefix_expr_negate othtran -> Vunary_prefix_expr_negate (transubst subst othtran)
| Vunary_prefix_expr_not othtran -> Vunary_prefix_expr_not (transubst subst othtran)
| Vunary_prefix_expr_plus othtran -> Vunary_prefix_expr_plus (transubst subst othtran)
| Vexpr_primary_parens1 (Vtlist lst) -> Vexpr_primary_parens1 ( transubstlst subst lst )
| Vexpression_or_dist1 othtran -> Vexpression_or_dist1 (transubst subst othtran)
| Vcond_expr (cond, true_, false_) -> Vcond_expr (transubst subst cond, transubst subst true_, transubst subst false_)
| Vdec_num ix -> Vdec_num ix
| Vreference3 (vector, Vselect_variable_dimension2 ix) ->
  Vreference3 (transubst subst vector, Vselect_variable_dimension2 (transubst subst ix))
| Vreference3 (vector, Vselect_variable_dimension1 (hi,lo)) ->
  Vreference3 (vector, Vselect_variable_dimension1 (transubst subst hi,transubst subst lo))
| Vrange_list_in_braces1 (Vtlist lst) -> Vrange_list_in_braces1 (Vtlist lst)
| Vexpr_primary_braces2 (Vdec_num repl, reference3) -> Vexpr_primary_braces2 (Vdec_num repl, reference3)
| Vsequence_repetition_expr1 othtran -> Vsequence_repetition_expr1 (transubst subst othtran)
| oth -> fail "transubst" oth

and transubstlst subst lst = Vtlist (List.map (transubst subst) lst)

and tranexpand exattr = function
| Vbin_based_number1 (radix, bin) -> Vbin_based_number1 (radix, bin)
| Vdec_based_number1 (radix, bin) -> Vdec_based_number1 (radix, bin)
| Vhex_based_number1 (radix, bin) -> Vhex_based_number1 (radix, bin)
| Vadd_expr (lhs, rhs) -> Vadd_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vsub_expr (lhs, rhs) -> Vsub_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vmul_expr (lhs, rhs) -> Vmul_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vdiv_expr (lhs, rhs) -> Vdiv_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vmod_expr (lhs, rhs) -> Vmod_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vpow_expr (lhs, rhs) -> Vpow_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vshift_expr2 (lhs, rhs) -> Vshift_expr2 (tranexpand exattr lhs, tranexpand exattr rhs)
| Vshift_expr3 (lhs, rhs) -> Vshift_expr3 (tranexpand exattr lhs, tranexpand exattr rhs)
| Vshift_expr4 (lhs, rhs) -> Vshift_expr4 (tranexpand exattr lhs, tranexpand exattr rhs)
| Vlogand_expr (lhs, rhs) -> Vlogand_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vbitand_expr (lhs, rhs) -> Vbitand_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vbitor_expr (lhs, rhs) -> Vbitor_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vxor_expr (lhs, rhs) -> Vxor_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vlogeq_expr (lhs, rhs) -> Vlogeq_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vlt_expr (lhs, rhs) -> Vlt_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vgt_expr (lhs, rhs) -> Vgt_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vgteq_expr (lhs, rhs) -> Vgteq_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vlteq_expr (lhs, rhs) -> Vlteq_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vplingeq_expr (lhs, rhs) -> Vplingeq_expr (tranexpand exattr lhs, tranexpand exattr rhs)
| Vunqualified_id1 (id, Vempty) -> Vunqualified_id1 (id, Vempty)
| Vconditional_statement1 (cond, then_) -> Vconditional_statement1 (tranexpand exattr cond, tranexpand exattr then_)
| Vconditional_statement2 (cond, then_, else_) -> Vconditional_statement2 (tranexpand exattr cond, tranexpand exattr then_, tranexpand exattr else_)
| Vstatement_item6 stmt -> Vstatement_item6 (tranexpand exattr stmt)
| Vassignment_statement_no_expr1 (rhs, lhs) -> Vassignment_statement_no_expr1 (tranexpand exattr rhs, tranexpand exattr lhs)
| Vcase_statement1 (Vempty, sel, Vtlist itmlst) -> Vcase_statement1 (Vempty, tranexpand exattr sel, tranexpandlst exattr itmlst)
| Vcase_item1 (sel, stmt) -> Vcase_item1 (tranexpand exattr sel, tranexpand exattr stmt)
| Vnonblocking_assignment1 (lhs, rhs) -> Vnonblocking_assignment1 (tranexpand exattr lhs, tranexpand exattr rhs)
| Vunary_prefix_expr_and othtran -> Vunary_prefix_expr_and (tranexpand exattr othtran)
| Vunary_prefix_expr_nand othtran -> Vunary_prefix_expr_nand (tranexpand exattr othtran)
| Vunary_prefix_expr_or othtran -> Vunary_prefix_expr_or (tranexpand exattr othtran)
| Vunary_prefix_expr_nor othtran -> Vunary_prefix_expr_nor (tranexpand exattr othtran)
| Vunary_prefix_expr_xor othtran -> Vunary_prefix_expr_xor (tranexpand exattr othtran)
| Vunary_prefix_expr_xnor othtran -> Vunary_prefix_expr_xnor (tranexpand exattr othtran)
| Vunary_prefix_expr_tilde othtran -> Vunary_prefix_expr_tilde (tranexpand exattr othtran)
| Vunary_prefix_expr_negate othtran -> Vunary_prefix_expr_negate (tranexpand exattr othtran)
| Vunary_prefix_expr_not othtran -> Vunary_prefix_expr_not (tranexpand exattr othtran)
| Vunary_prefix_expr_plus othtran -> Vunary_prefix_expr_plus (tranexpand exattr othtran)
| Vexpr_primary_parens1 (Vtlist lst) -> Vexpr_primary_parens1 ( tranexpandlst exattr lst )
| Vexpression_or_dist1 othtran -> Vexpression_or_dist1 (tranexpand exattr othtran)
| Vcond_expr (cond, true_, false_) -> Vcond_expr (tranexpand exattr cond, tranexpand exattr true_, tranexpand exattr false_)
| Vdec_num ix -> Vdec_num ix
| Vreference3 (vector, Vselect_variable_dimension2 ix) ->
  Vreference3 (tranexpand exattr vector, Vselect_variable_dimension2 (tranexpand exattr ix))
| Vreference3 (vector, Vselect_variable_dimension1 (hi,lo)) ->
  Vreference3 (vector, Vselect_variable_dimension1 (tranexpand exattr hi,tranexpand exattr lo))
| Vrange_list_in_braces1 (Vtlist lst) -> Vrange_list_in_braces1 (Vtlist lst)
| Vexpr_primary_braces2 (Vdec_num repl, reference3) -> Vexpr_primary_braces2 (Vdec_num repl, reference3)
| Vsequence_repetition_expr1 othtran -> Vsequence_repetition_expr1 (tranexpand exattr othtran)
| Vloop_statement1 (ix, Vdec_num lo, Vlt_expr (Vunqualified_id1 (ix', Vempty) as id, Vdec_num hi),
   Vassignment_statement_no_expr1 (Vunqualified_id1 (ix'', Vempty),
    Vadd_expr (Vunqualified_id1 (ix''', Vempty), Vdec_num inc)),
   Vseq_block1 (Vtlist itmlst)) when ix=ix' && ix'=ix'' && ix''=ix''' ->
   Vseq_block1 (Vtlist (tranloop id (int_of_string lo) (int_of_string hi) (int_of_string inc) itmlst))
| oth -> fail "tranexpand" oth

and tranexpandlst exattr lst = Vtlist (List.map (tranexpand exattr) lst)

and tranexpandlst' = function
| [] -> []
| stmt :: Vseq_block1 (Vtlist lst) :: tl -> stmt :: tranexpandlst' (lst @ tl)
| oth :: tl -> oth :: tranexpandlst' tl

and transeq subh = function
| [] -> let lst = ref [] in Hashtbl.iter (fun c a -> lst := Vstatement_item6 (Vassignment_statement_no_expr1 (c, a)) :: !lst) subh; !lst
| Vstatement_item6 (Vassignment_statement_no_expr1 (Vunqualified_id1 (_, Vempty) as c, a)) :: tl ->
  let a' = transubst subh a in
  Hashtbl.replace subh c a'; seqlst := a' :: !seqlst; transeq subh tl
| oth :: tl -> transubst subh oth :: transeq subh tl

and transeqlst seq =
print_endline "transeq1";
othseq := seq;
let exp = List.map (tranexpand {exp=Vempty}) seq in
othexp := exp;
let exp' = tranexpandlst' exp in
othexp' := exp';
let subh = Hashtbl.create 257 in
let seq' = transeq subh exp' in
othseq' := seq';
seq'

and transign = function
| Vsigned -> [TYPSIGNED]
| Vempty -> []
| oth -> fail "transign" oth

and trantyp itms = function
| Vdecl_variable_dimension1 (hi, lo) -> TYPRNG (tran' itms  hi, tran' itms  lo)
| Vempty -> TYPNONE
| oth -> fail "trantyp" oth

and tranenum = function
| Vident id -> SCOPE id
| oth -> fail "trantyp" oth

and tranp' itms = function
| Vdec_num s -> 32, HEX (int_of_string s)
| Vunqualified_id1 (id, Vempty) -> (match enum' itms id, param' itms id with
    | None, None -> failwith ("tranp': None "^id)
    | Some (TYPRNG(HEX hi,HEX lo),x), _ -> hi-lo+1, x
    | _, Some (w, n) -> w, n
    | _ -> failwith ("tranp': oth "^id))
| Vsub_expr (lhs, rhs) -> (match tranp' itms lhs, tranp' itms rhs with
  | (wlhs, HEX lhs), (wrhs, HEX rhs) -> max wlhs wrhs, HEX (lhs-rhs)
  | lhs, rhs -> otha := (Some lhs, Some rhs); failwith "tranp' expr")
| oth -> fail "tranp'" oth

and tran' itms x = snd (tranp' itms x)

and tranw' itms = function
| Vdec_num s -> Input_types.CNST(32, HEX (int_of_string s))
| Vunqualified_id1 (id, Vempty) -> (match enum'' itms id with
					  | None -> failwith "tranw'"
                                          | Some x -> x)
| Vbin_based_number1 (radix, bin) -> Scanf.sscanf radix "%d'b" (fun rad -> Input_types.CNST(rad, HEX (bin_to_native bin)))
| Vdec_based_number1 (radix, bin) -> Scanf.sscanf radix "%d'd" (fun rad -> Input_types.CNST(rad, HEX (int_of_string bin)))
| Vhex_based_number1 (radix, bin) -> Scanf.sscanf (radix^bin) "%d'h%x" (fun rad dig -> Input_types.CNST(rad, HEX dig))
| oth -> fail "tranw'" oth

and tran'' itms = function
| Vbin_based_number1 (radix, bin) -> Scanf.sscanf radix "%d'b" (fun rad -> Input_types.CNST(rad, HEX (bin_to_native bin)))
| Vdec_based_number1 (radix, bin) -> Scanf.sscanf radix "%d'd" (fun rad -> Input_types.CNST(rad, HEX (int_of_string bin)))
| Vhex_based_number1 (radix, bin) -> Scanf.sscanf (radix^bin) "%d'h%x" (fun rad dig -> Input_types.CNST(rad, HEX dig))
| Vadd_expr (lhs, rhs) -> ARITH (Aadd "fastest", (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vsub_expr (lhs, rhs) -> ARITH (Asub "fastest", (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vmul_expr (lhs, rhs) -> ARITH (Amul "fastest", (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vdiv_expr (lhs, rhs) -> ARITH (Adiv, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vmod_expr (lhs, rhs) -> ARITH (Amod, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vpow_expr (lhs, rhs) -> ARITH (Apow, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vshift_expr2 (lhs, rhs) -> LOGIC (Lshiftl, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vshift_expr3 (lhs, rhs) -> LOGIC (Lshiftr, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vshift_expr4 (lhs, rhs) -> LOGIC (Lshiftrs, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vlogand_expr (lhs, rhs) -> LOGIC (Land, (LOGIC (Lredor, (tran'' itms) lhs :: [])) :: (LOGIC (Lredor, (tran'' itms) rhs :: [])) :: [])
| Vbitand_expr (lhs, rhs) -> LOGIC (Land, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vbitor_expr (lhs, rhs) -> LOGIC (Lor, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vxor_expr (lhs, rhs) -> LOGIC (Lxor, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vlogeq_expr (lhs, rhs) -> CMP (Ceq, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vlt_expr (lhs, rhs) -> CMP (Clt, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vgt_expr (lhs, rhs) -> CMP (Cgt, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vgteq_expr (lhs, rhs) -> CMP (Cgte, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vlteq_expr (lhs, rhs) -> CMP (Clte, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vplingeq_expr (lhs, rhs) -> CMP (Cneq, (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vunqualified_id1 (id, Vempty) ->
  (match enum' itms id, param' itms id with
    | None, None -> VRF (id, (BASDTYP, "wire", TYPNONE, []), [])
    | Some (TYPRNG(HEX hi,HEX lo),x), _ -> CNST(hi-lo+1, x)
    | _, Some (w, n) -> CNST(w, n)
    | _ -> failwith ("tran'': oth "^id))
| Vconditional_statement1 (Vexpression_in_parens1 cond, then_) ->
  Input_types.IF ("", (tran'' itms) cond :: (tran'' itms) then_ :: [])
| Vconditional_statement2 (Vexpression_in_parens1 cond, then_, else_) ->
  Input_types.IF ("", (tran'' itms) cond :: (tran'' itms) then_ :: (tran'' itms) else_ :: [])
| Vstatement_item6 (Vassignment_statement_no_expr1 (rhs, lhs)) -> ASGN( false, "", (tran'' itms) lhs :: (tran'' itms) rhs :: [])
| Vseq_block1 (Vtlist itmlst) -> BGN (None, tranflatten (List.map (tran'' itms) itmlst))
| Vloop_statement1 (ix, Vdec_num lo, Vlt_expr (Vunqualified_id1 (ix', Vempty) as id, Vdec_num hi),
   Vassignment_statement_no_expr1 (Vunqualified_id1 (ix'', Vempty),
    Vadd_expr (Vunqualified_id1 (ix''', Vempty), Vdec_num inc)),
   Vseq_block1 (Vtlist itmlst)) when ix=ix' && ix'=ix'' && ix''=ix''' ->
   Input_types.BGN(None, List.map (tran'' itms) (tranloop id (int_of_string lo) (int_of_string hi) (int_of_string inc) itmlst))
| Vcase_statement1 (Vempty, sel, Vtlist itmlst) -> CS ("", (tran'' itms) sel :: List.map ((tran'' itms) ) itmlst)
| Vcase_item1 (sel, stmt) -> CSITM ("", tranw' itms sel :: tran'' itms stmt :: [])
| Vcase_item2 (stmt) -> CSITM ("", tran'' itms stmt :: [])
| Vnonblocking_assignment1 (lhs, rhs) -> ASGN (true, "", (tran'' itms) rhs :: (tran'' itms) lhs :: [])
| Vunary_prefix_expr_and othtran -> LOGIC(Lredand, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_nand othtran -> LOGIC(Lrednand, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_or othtran -> LOGIC(Lredor, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_nor othtran -> LOGIC(Lrednor, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_xor othtran -> LOGIC(Lredxor, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_xnor othtran -> LOGIC(Lredxnor, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_tilde othtran -> UNRY(Ulognot, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_negate othtran -> UNRY(Unegate, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_not othtran -> UNRY(Unot, (tran'' itms) othtran :: [])
| Vunary_prefix_expr_plus othtran -> (tran'' itms) othtran
| Vexpr_primary_parens1 (Vtlist [Vsequence_repetition_expr1 arg]) -> (tran'' itms) arg
| Vexpression_or_dist1 othtran -> (tran'' itms) othtran
| Vcond_expr (cond, true_, false_) -> CND ("", (tran'' itms) cond :: (tran'' itms) true_ :: (tran'' itms) false_ :: [])
| Vdec_num ix -> Input_types.CNST(32, HEX (int_of_string ix))
| Vreference3 (vector, Vselect_variable_dimension2 ix) -> SEL ("", (tran'' itms) vector :: (tran'' itms) ix :: (tran'' itms) (Vdec_num "1") :: [])
| Vreference3 (vector, Vselect_variable_dimension1 (hi,lo)) ->
  SEL ("", (tran'' itms) vector :: (tran'' itms) lo :: trancnst itms (Vsub_expr (Vadd_expr (hi,Vdec_num "1"), lo)) :: [])
| Vrange_list_in_braces1 (Vtlist lst) -> CAT ("", List.map (tran'' itms) lst)
| Vexpr_primary_braces2 (Vdec_num repl, reference3) -> let r = (tran'' itms) reference3 in CAT ("", List.init (int_of_string repl) (fun _ -> r))
| oth -> fail "tran''" oth

and tranflatten = function
  | [] -> []
  | BGN (None, lst) :: tl -> tranflatten (lst @ tl)
  | oth :: tl -> oth :: tranflatten tl

and tranloop id lo hi inc lst = let loop = (List.flatten (List.init ((hi-lo)/inc) (fun ix' -> let ix = lo + ix' * inc in
       print_endline (string_of_int (ix));
       let subh = Hashtbl.create 257 in
       Hashtbl.add subh id (Vdec_num (string_of_int ix));
       List.map (transubst subh) lst))) in
       othloop := loop;
       loop
       
and trancnst itms = function
| Vdec_num n -> (tran'' itms) (Vdec_num n)
| Vadd_expr (Vdec_num lhs, Vdec_num rhs) -> (tran'' itms) (Vdec_num (string_of_int (int_of_string lhs + int_of_string rhs)))
| Vadd_expr (lft, rght) -> (match trancnst itms lft, trancnst itms rght with
    | CNST (wlhs, HEX lhs), CNST (wrhs, HEX rhs) -> CNST (max wlhs wrhs, HEX (lhs+rhs))
    | lft', rght' -> othcnst := (lft', rght'); failwith "addtrancnst")
| Vsub_expr (Vdec_num lhs, Vdec_num rhs) -> (tran'' itms) (Vdec_num (string_of_int (int_of_string lhs - int_of_string rhs)))
| Vsub_expr (lft, rght) -> (match trancnst itms lft, trancnst itms rght with
    | CNST (wlhs, HEX lhs), CNST (wrhs, HEX rhs) -> CNST (max wlhs wrhs, HEX (lhs-rhs))
    | lft', rght' -> othcnst := (lft', rght'); failwith "subtrancnst'")
| Vunqualified_id1 _ as v -> let w,x = tranp' itms v in CNST (w, x)
| oth -> othtran := oth; fail "trancnst" oth

and tranlst'' itms = function
| Vseq_block1 (Vtlist lst) -> List.map ((tran'' itms) ) lst
| oth -> (tran'' itms) oth :: []

let cnv' othitms = function
| Vml_start1 (Vtlist lst) -> List.map (fun itm ->
let modnam = ref "" in
let uitms = Input_dump.empty_itms [] in
let _ = tran uitms modnam itm in
othitms := uitms;
let _ = Input_dump.dump' "_check" (!modnam, ((), uitms)) in
let rtl = Input_hardcaml.cnv (!modnam, uitms) in
let fd = open_out "rtl.v" in output_string fd rtl; close_out fd;
(*
let tree = Source_text_rewrite.parse_output_ast_from_string rtl in
othrtl := Some tree;
Input_dump.dump' "_tmp" cnvrted;
*)
(!modnam, uitms)) lst
| oth -> fail "cnv'" oth
