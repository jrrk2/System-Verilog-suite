(*
 open Source_text_verible
 *)

open Source_text_rewrite_types

type othtran =
          | Vadd_expr of othtran * othtran
          | Valways_construct1 of othtran
          | Vany_argument_list_item_last2 of othtran
          | Vany_param_declaration4 of othtran * othtran
          | Vany_port_list_item_last1 of othtran * othtran
          | Vany_port_list_trailing_comma1 of othtran
          | Vassignment_pattern1 of othtran
          | Vassignment_statement_no_expr1 of othtran * othtran
          | Vbegin1 of othtran
          | Vbin_based_number1 of string * string
          | Vbitand_expr of othtran * othtran
          | Vbitor_expr of othtran * othtran
          | Vblock_item_or_statement_or_null6
          | Vcall_base1 of othtran
          | Vcase_inside_item1
          | Vcase_inside_item2
          | Vcase_inside_items1
          | Vcase_item1 of othtran * othtran
          | Vcase_item2 of othtran
          | Vcase_items1 of othtran * othtran
          | Vcase_statement1 of othtran * othtran * othtran
          | Vcase_statement3
          | Vcast1
          | Vcomma
          | Vcond_expr of othtran * othtran * othtran
          | Vconditional_generate_construct1 of othtran * othtran * othtran
          | Vconditional_generate_construct2 of othtran * othtran
          | Vconditional_generate_construct3 of othtran * othtran
          | Vgenerate_case_items1 of othtran
          | Vconditional_statement1 of othtran * othtran
          | Vconditional_statement2 of othtran * othtran * othtran
          | Vcont_assign1 of othtran * othtran
          | Vcontinuous_assign1 of othtran
          | Vdata_declaration_or_module_instantiation1 of othtran
          | Vdata_declaration_or_module_instantiation2
          | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt1 of othtran * othtran
          | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt2 of othtran * othtran
          | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt3 of othtran * othtran * othtran
          | Vdata_type_or_implicit_basic_followed_by_id_and_dimensions_opt4 of othtran * othtran
          | Vdata_type_primitive1 of othtran * othtran
          | Vdata_type_primitive_scalar1_logic
          | Vdata_type_primitive_scalar1_reg
          | Vdec_based_number1 of string * string
          | Vdec_num of string
          | Vdecl_dimensions2 of othtran * othtran
          | Vdecl_variable_dimension1 of othtran * othtran
          | Vdiv_expr of othtran * othtran
          | Vempty
          | Vend1
          | Vevent_control2 of othtran
          | Vevent_control4
          | Vevent_expression_negedge of othtran
          | Vevent_expression_posedge of othtran
          | Vexpr_primary_braces2 of othtran * othtran
          | Vexpr_primary_parens1 of othtran
          | Vexpression_in_parens1 of othtran
          | Vexpression_list_proper1
          | Vexpression_or_dist1 of othtran
          | Vfunction_declaration1
          | Vgate_instance_or_register_variable1 of othtran
          | Vgenerate_block1 of othtran * othtran
          | Vgenerate_if1 of othtran
          | Vgenerate_region1 of othtran
          | Vgt_expr of othtran * othtran
          | Vgteq_expr of othtran * othtran
          | Vhex_based_number1 of string * string
          | Vhierarchy_extension1 of othtran
          | Vident of string
          | Videntifier_optional_unpacked_dimensions1 of string
          | Vinc_or_dec_expression2
          | Vinitial_construct1 of othtran
          | Vinput
          | Vinstantiation_base1 of othtran * othtran
          | Vinstantiation_base1_reg of othtran
          | Vjump_statement4
          | Vlabel_opt1 of string
          | Vless
          | Vlogand_expr of othtran * othtran
          | Vlogeq_expr of othtran * othtran
          | Vlogor_expr of othtran * othtran
          | Vloop_statement1 of string * othtran * othtran * othtran * othtran
          | Vlt_expr of othtran * othtran
          | Vlteq_expr of othtran * othtran
          | Vml_start1 of othtran
          | Vmod_expr of othtran * othtran
          | Vmodule_or_interface_declaration1 of string * othtran * othtran * othtran
          | Vmodule_parameter_port1 of othtran * othtran
          | Vmodule_parameter_port2 of othtran * othtran
          | Vmodule_parameter_port_list_item_last1 of othtran
          | Vmodule_port_declaration3 of othtran * othtran * othtran * othtran
          | Vmodule_port_declaration5 of othtran * othtran * othtran
          | Vmodule_port_declaration7_reg of othtran * othtran * string
          | Vmodule_parameter_port_list_opt1 of othtran
          | Vmul_expr of othtran * othtran
          | Vnet_declaration1
          | Vnet_declaration2
          | Vnet_declaration4
          | Vnet_variable1
          | Vnon_anonymous_gate_instance_or_register_variable1 of string * othtran
          | Vnon_anonymous_gate_instance_or_register_variable2 of othtran
          | Vnonblocking_assignment1 of othtran * othtran
          | Voutput
          | Vparam_type_followed_by_id_and_dimensions_opt2 of othtran * string
          | Vparam_type_followed_by_id_and_dimensions_opt3 of othtran
          | Vparam_type_followed_by_id_and_dimensions_opt4
          | Vparameter_value_byname1
          | Vparameter_value_byname_list_item_last2 of othtran
          | Vparameters2 of othtran
          | Vplingeq_expr of othtran * othtran
          | Vport1 of othtran
          | Vport_declaration_noattr1 of othtran * othtran
          | Vport_declaration_noattr4
          | Vport_named1 of string * othtran
          | Vport_named2 of string
          | Vport_named3 of string
          | Vpow_expr of othtran * othtran
          | Vprocedural_timing_control_statement2 of othtran * othtran
          | Vqualified_id2
          | Vrange_list_in_braces1 of othtran
          | Vreference2 of othtran * othtran
          | Vreference3 of othtran * othtran
          | Vreference_or_call_base1 of othtran
          | Vselect_variable_dimension1 of othtran * othtran
          | Vselect_variable_dimension2 of othtran
          | Vselect_variable_dimension3 of othtran * othtran
          | Vseq_block1 of othtran
          | Vsequence_repetition_expr1 of othtran
          | Vshift_expr2 of othtran * othtran
          | Vshift_expr3 of othtran * othtran
          | Vshift_expr4 of othtran * othtran
          | Vsigned
          | Vsimple_immediate_assertion_statement1
          | Vstatement3 of othtran
          | Vstatement_item6 of othtran
          | Vstring
          | Vstring_literal of string
          | Vsub_expr of othtran * othtran
          | Vsystem_tf_call1 of string * othtran
          | Vtask_declaration1 of othtran
          | Vtf_port_item1
          | Vtlist of othtran list
          | Vtrailing_decl_assignment2
          | Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt2 of othtran * othtran
          | Vtype_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4 of othtran
          | Vunary_prefix_expr_and of othtran
          | Vunary_prefix_expr_nand of othtran
          | Vunary_prefix_expr_negate of othtran
          | Vunary_prefix_expr_nor of othtran
          | Vunary_prefix_expr_not of othtran
          | Vunary_prefix_expr_or of othtran
          | Vunary_prefix_expr_plus of othtran
          | Vunary_prefix_expr_tilde of othtran
          | Vunary_prefix_expr_xnor of othtran
          | Vunary_prefix_expr_xor of othtran
          | Vunique
          | Vunqualified_id1 of string * othtran
          | Vxor_expr of othtran * othtran
          | Vtype_declaration1 of othtran * othtran
          | Venum_data_type1 of othtran
          | Venum_data_type2 of othtran * othtran
          | Venum_name_list_trailing_comma1 of othtran
          | Venum_name_list_item_last1 of othtran * othtran [@@deriving yojson]

type expand = {exp:othtran}

type strip =
    | Null
    | Bool of bool
    | Int of int
    | Intlit of string
    | Float of float
    | String of string
    | Assoc of (string * strip) list
    | List of strip list
    | Tuple of strip list
    | Variant of (string * strip option)

let othyt = ref Null
let othytok = ref Null
let othtran = ref Vempty

let rec strip_json_typ = function
    | `Null -> Null
    | `Bool x -> Bool x
    | `Int x -> Int x
    | `Intlit x -> Intlit x
    | `Float x -> Float x
    | `String x -> String x
    | `Assoc lst -> Assoc (List.map (fun (str, x) -> (str,  strip_json_typ x)) lst)
    | `List lst -> List (List.map (fun x -> strip_json_typ x) lst)
    | `Tuple lst -> Tuple (List.map (fun x -> strip_json_typ x) lst)
    | `Variant (str, x) -> Variant (str, match x with Some o -> Some (strip_json_typ o) | None -> None)

let rec pp fmt = function
  | List (String ("Vunqualified_id1") as tup :: (String _ :: _ as arg)) -> pp fmt ( List [tup; List arg] )
  | List [String kind; String s] -> Format.fprintf fmt "%s %S" kind s
  | List [String tup; List lst] ->
      Format.fprintf fmt "%s@[<hov> " tup;
      let sep = ref "(" in
      List.iter (fun itm -> Format.fprintf fmt "%s@ " !sep; sep := ", "; pp fmt itm) lst; Format.fprintf fmt ")@]"
  | List (String tup :: (List _ :: _ as tl)) ->
      Format.fprintf fmt "%s@[<hov> " tup;
      let sep = ref "(" in
      List.iter (fun itm -> Format.fprintf fmt "%s@ " !sep; sep := ", "; pp fmt itm) tl; Format.fprintf fmt ")@]"
  | List [String s] -> Format.pp_print_string fmt s
  | Null -> Format.pp_print_string fmt "Null"
  | Bool x ->
    Format.fprintf fmt "Bool (@[<hov>";
    Format.fprintf fmt "%B" x;
    Format.fprintf fmt "@])"
  | Int x ->
    Format.fprintf fmt "Int (@[<hov>";
    Format.fprintf fmt "%d" x;
    Format.fprintf fmt "@])"
  | Intlit x ->
    Format.fprintf fmt "Intlit (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  | Float x ->
    Format.fprintf fmt "Float (@[<hov>";
    Format.fprintf fmt "%F" x;
    Format.fprintf fmt "@])"
  | String x ->
    Format.fprintf fmt "String (@[<hov>";
    Format.fprintf fmt "%S" x;
    Format.fprintf fmt "@])"
  | Assoc xs ->
    Format.fprintf fmt "Assoc (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep (key, value) ->
        if sep then
          Format.fprintf fmt ";@ ";
          Format.fprintf fmt "(@[";
          Format.fprintf fmt "%S" key;
          Format.fprintf fmt ",@ ";
          pp fmt value;
          Format.fprintf fmt "@])";
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | List xs ->
    Format.fprintf fmt "List (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep x ->
        if sep then
          Format.fprintf fmt ";@ ";
          pp fmt x;
          true) false xs);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | Tuple tup ->
    Format.fprintf fmt "Tuple (@[<hov>";
    Format.fprintf fmt "@[<2>[";
    ignore (List.fold_left
      (fun sep e ->
         if sep then
           Format.fprintf fmt ";@ ";
           pp fmt e;
           true) false tup);
    Format.fprintf fmt "@,]@]";
    Format.fprintf fmt "@])"
  | Variant (name, value) ->
    Format.fprintf fmt "Variant (@[<hov>";
    Format.fprintf fmt "(@[";
    Format.fprintf fmt "%S" name;
    Format.fprintf fmt ",@ ";
    (match value with
      | None -> Format.pp_print_string fmt "None"
      | Some x ->
        Format.pp_print_string fmt "(Some ";
        pp fmt x;
        Format.pp_print_string fmt ")");
    Format.fprintf fmt "@])";
    Format.fprintf fmt "@])"

let py () = print_endline (Format.asprintf "%a" pp !othytok);;

let fail_json to_yojson x = let y = strip_json_typ (to_yojson x) in othyt := y; print_endline (Format.asprintf "%a" pp y)

let failothtran = fail_json othtran_to_yojson
(*
 let failtoken = fail_json token_to_yojson
 *)
let failind = fail_json ind_to_yojson

let fail' msg x = failothtran x; failwith msg
let fail msg x = othtran := x; fail' msg x
