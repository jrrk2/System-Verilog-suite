(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*     File name:        VhdlTypes.ml                                        *)
(*     Description:      Type for storing VHDL language patterns.            *)
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

(* value and position pairs *)
type vhdl_identifier = string*int
and vhdl_string = string*int
and vhdl_int = Big_int.big_int*int
and vhdl_float = float*int
and vhdl_char = char*int

(* identifier_list ::= identifier {, identifier} *)
and vhdl_identifier_list = vhdl_identifier list

(* label ::= identifier *)
and vhdl_label = vhdl_identifier

(* simple_name ::= identifier *)
and vhdl_simple_name = vhdl_identifier

(* attribute_designator ::= attribute_simple_name *)
and vhdl_attribute_designator = vhdl_simple_name

(* suffix ::=                      *)
(*     simple_name                 *)
(*   | character_literal           *)
(*   | operator_symbol             *)
(*   | all                         *)

and vhdl_suffix =
  | SuffixSimpleName of vhdl_name
  | SuffixCharLiteral of vhdl_char
  | SuffixOpSymbol of vhdl_string
  | SuffixAll

(* prefix ::=                      *)
(*    name                         *)
(*  | function_call                *)
(*TODO*)
and vhdl_prefix =
  | PrefixName of vhdl_name

(* selected_name ::= prefix.suffix *)
and vhdl_selected_name = vhdl_suffix list

and vhdl_selected_name_list = vhdl_selected_name list

(* name ::=           *)
(*    simple_name     *)
(*  | operator_symbol *)
(*  | selected_name   *)
(*  | indexed_name    *)
(*  | slice_name      *)
(*  | attribute_name  *)
(* attribute names will not have parameters *)
(* indexed name, slice name, and complex    *)
(* expressions like name( ... ) will        *)
(* be parsed at an higher level             *)
and vhdl_name =
  | SimpleName of vhdl_simple_name
  | OperatorString of vhdl_string
  | SelectedName of vhdl_selected_name
  | AttributeName of vhdl_attribute_name
  | SubscriptName of vhdl_simple_name * vhdl_selector

and vhdl_name_list = vhdl_name list

(* type_mark ::=   *)
(*     typename    *)
(*   | subtypename *)
and vhdl_type_mark = vhdl_name

and vhdl_type_mark_list = vhdl_type_mark list

(* signature ::= [ [ type_mark { , type_mark } ] [ return type_mark ] ] *)
and vhdl_signature = {
  signatureparametertypes: vhdl_type_mark_list;
  signaturereturntype: vhdl_type_mark
  }

(* library_clause ::= library logical_name_list ;      *)
(* logical_name_list ::= logical_name {, logical_name} *)
(* logical_name ::= identifier                         *)

and vhdl_logical_name = vhdl_identifier
and vhdl_logical_name_list = vhdl_logical_name list
and vhdl_library_clause = vhdl_logical_name_list

(* attribute_name ::=                                *)
(*   prefix [ signature ] ' attribute_designator     *)
(*   [ ( expression { , expression } ) ]             *)
and vhdl_attribute_name = {
  attributeprefix: vhdl_suffix;
  attribute: vhdl_attribute_designator;
  }

(* designator ::= identifier | operator_symbol *)
(* extended to store characters too *)
and vhdl_designator =
  | DesignatorIdentifier of vhdl_identifier
  | DesignatorOperator of vhdl_string
  | DesignatorCharacter of vhdl_char

(* expression ::=                                    *)
(*     logical_expression                            *)
(*   | condition_operator primary                    *)
(* logical_expression ::=                            *)
(*     relation { and  relation }                    *)
(*   | relation { or   relation }                    *)
(*   | relation { xor  relation }                    *)
(*   | relation [ nand relation ]                    *)
(*   | relation [ nor  relation ]                    *)
(*   | relation { xnor relation }                    *)
(* relation ::=                                      *)
(*   shift_expression                                *)
(*   [ relational_operator shift_expression ]        *)
(* shift_expression ::=                              *)
(*   simple_expression                               *)
(*   [ shift_operator simple_expression ]            *)
(* simple_expression ::=                             *)
(*   [ sign ] term { adding_operator term }          *)
(* term ::=                                          *)
(*   factor { multiplying_operator factor }          *)
(* factor ::=                                        *)
(*    primary [ ** primary ]                         *)
(*  | abs primary                                    *)
(*  | not primary                                    *)
(*  | logical_operator primary                       *)
(* primary ::=                                       *)
(*    name                                           *)
(*  | literal                                        *)
(*  | aggregate                                      *)
(*  | function_call                                  *)
(*  | qualified_expression                           *)
(*  | type_conversion                                *)
(*  | allocator                                      *)
(*  | ( expression )                                 *)

(* aggregate ::=                                     *)
(*   ( element_association {, element_association} ) *)
(* element_association ::=                           *)
(*   [ choices => ] expression                       *)
(* choices ::= choice { | choice }                   *)
(* choice ::=                                        *)
(*     simple_expression                             *)
(*   | discrete_range                                *)
(*   | element_simple_name                           *)
(*   | others                                        *)

(* condition ::= boolean_expression                 *)
and vhdl_condition =
  | Condition of vhdl_expression

(* selectors have special limitations *)
and vhdl_selector =
  | Selector of vhdl_expression
and vhdl_selection_kind =
  | OrdinarySelection
  | MatchingSelection

(* function_call ::=                                 *)
(*   function_name [ ( actual_parameter_part ) ]     *)
(* actual_parameter_part ::=                         *)
(*   parameter_association_list                      *)

(* qualified_expression ::=                          *)
(*     type_mark'( expression )                      *)
(*   | type_mark'aggregate                           *)

(* type_conversion ::= type_mark( expression )       *)
(* range_constraint ::= range range                  *)
(* range ::=                                         *)
(*    range_attribute_name                           *)
(*  | simple_expression direction simple_expression  *)
(* direction ::= to | downto                         *)
(* association_list ::=                              *)
(*   association_element {, association_element}     *)
(* association_element ::=                           *)
(*   [formal_part =>] actual_part                    *)
(* formal_part ::=                                   *)
(*     formal_designator                             *)
(*   | function_name( formal_designator )            *)
(*   | type_mark( formal_designator )                *)
(* formal_designator ::=                             *)
(*     generic_name                                  *)
(*   | port_name                                     *)
(*   | parameter_name                                *)
(* actual_part ::=                                   *)
(*     actual_designator                             *)
(*   | function_name( actual_designator )            *)
(*   | type_mark( actual_designator )                *)
(* actual_designator ::=                             *)
(*     expression                                    *)
(*   | signal_name                                   *)
(*   | variable_name                                 *)
(*   | file_name (ieee syn ignored)                  *)
(*   | subtype_indication                            *)
(*   | subprogram_name                               *)
(*   | instantiated_package_name                     *)
(*   | open                                          *)

and vhdl_expression =
  | AtomExpression of vhdl_logical_expression
  | ConditionExpression of vhdl_primary

and vhdl_expression_list =
  vhdl_expression list

and vhdl_logical_expression =
  | AtomLogicalExpression of vhdl_relation
  | AndLogicalExpression of (vhdl_relation * vhdl_relation)
  | OrLogicalExpression of (vhdl_relation * vhdl_relation)
  | XorLogicalExpression of (vhdl_relation * vhdl_relation)
  | NandLogicalExpression of (vhdl_relation * vhdl_relation)
  | NorLogicalExpression of (vhdl_relation * vhdl_relation)
  | XnorLogicalExpression of (vhdl_relation * vhdl_relation)

and vhdl_relation =
  | AtomRelation of vhdl_shift_expression
  | EqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | NotEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | LessRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | LessOrEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | GreaterRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | GreaterOrEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | MatchingEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | MatchingNotEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | MatchingLessRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | MatchingLessOrEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | MatchingGreaterRelation of (vhdl_shift_expression * vhdl_shift_expression)
  | MatchingGreaterOrEqualRelation of (vhdl_shift_expression * vhdl_shift_expression)
and vhdl_relation_list = vhdl_relation list

and vhdl_shift_expression =
  | AtomShiftExpression of vhdl_simple_expression
  | ShiftLeftLogicalExpression of (vhdl_simple_expression * vhdl_simple_expression)
  | ShiftRightLogicalExpression of (vhdl_simple_expression * vhdl_simple_expression)
  | ShiftLeftArithmeticExpression of (vhdl_simple_expression * vhdl_simple_expression)
  | ShiftRightArithmeticExpression of (vhdl_simple_expression * vhdl_simple_expression)
  | RotateLeftExpression of (vhdl_simple_expression * vhdl_simple_expression)
  | RotateRightExpression of (vhdl_simple_expression * vhdl_simple_expression)

and vhdl_simple_expression =
  | AtomSimpleExpression of vhdl_term
  | AddSimpleExpression of (vhdl_term * vhdl_term)
  | SubSimpleExpression of (vhdl_term * vhdl_term)
  | NegSimpleExpression of vhdl_term
  | ConcatSimpleExpression of vhdl_term_list

and vhdl_term =
  | AtomTerm of vhdl_factor
  | MultTerm of (vhdl_factor * vhdl_factor)
  | DivTerm of (vhdl_factor * vhdl_factor)
  | ModTerm of (vhdl_factor * vhdl_factor)
  | RemTerm of (vhdl_factor * vhdl_factor)
and vhdl_term_list = vhdl_term list

and vhdl_factor =
  | AtomFactor of vhdl_dotted
  | ExpFactor of (vhdl_dotted * vhdl_dotted)
  | AbsFactor of vhdl_dotted
  | NotFactor of vhdl_dotted
  | AndFactor of vhdl_dotted
  | OrFactor of vhdl_dotted
  | XorFactor of vhdl_dotted
  | XnorFactor of vhdl_dotted
  | NandFactor of vhdl_dotted
  | NorFactor of vhdl_dotted
  | NewFactor of vhdl_dotted
and vhdl_factor_list = vhdl_factor list

and vhdl_dotted =
  | AtomDotted of vhdl_primary
  | Ldotted of (vhdl_primary * vhdl_primary)

(* NameParamPrimary represents the following syntax:        *)
(* name(...) it's basically a sliced name, a function call, *)
(*   an indexed name, a qualified expression or             *)
(* an attribute with parameters.                            *)
and vhdl_primary =
  | VhdSequentialNull
  | NamePrimary of vhdl_name
  | NameParametersPrimary of (vhdl_name * vhdl_parameters)
  | IntPrimary of vhdl_int
  | CharPrimary of vhdl_char
  | FloatPrimary of vhdl_float
  | PhysicalPrimary of vhdl_physical_literal
  | AggregatePrimary of vhdl_aggregate
  | QualifiedExpressionPrimary of vhdl_qualified_expression
  | ParenthesedPrimary of vhdl_expression
and vhdl_parameter = vhdl_association_list
and vhdl_parameters =
  vhdl_parameter list
and vhdl_physical_literal =
  | PhysicalEmpty of int (* position *)
  | PhysicalInteger of (vhdl_int*vhdl_identifier)
  | PhysicalFloat of (vhdl_float*vhdl_identifier)
and vhdl_aggregate = vhdl_element_association_list
and vhdl_element_association_list = vhdl_element_association list
and vhdl_element_association = {
  elemassocchoices: vhdl_choices;
  elemassocexpression: vhdl_expression
  }
and vhdl_choices = vhdl_choice list
and vhdl_choice =
  | ChoiceSimpleExpression of vhdl_simple_expression
  | ChoiceDiscreteRange of vhdl_discrete_range
  (* expression may be identifiers *)
  (*| ChoiceElementSimpleName of vhdl_simple_name*)
  | ChoiceOthers
and vhdl_qualified_expression =
  | QualifiedExpression of (vhdl_suffix * vhdl_expression)
  | QualifiedAggregate of (vhdl_suffix * vhdl_aggregate)
and vhdl_range =
  | AttrNameRange of vhdl_attribute_name
  | IncreasingRange of (vhdl_simple_expression * vhdl_simple_expression)
  | DecreasingRange of (vhdl_simple_expression * vhdl_simple_expression)
and vhdl_range_constraint = vhdl_range
and vhdl_discrete_range =
  | Range of vhdl_range
  | SubTypeRange of vhdl_subtype_indication
and vhdl_discrete_range_list = vhdl_discrete_range list
and vhdl_actual_part =
  | ActualExpression of vhdl_expression
  | ActualDiscreteRange of vhdl_discrete_range
  | ActualOpen
and vhdl_formal_designator = vhdl_name
and vhdl_formal_part =
  | FormalIndexed
  | FormalExpression of vhdl_expression
and vhdl_association_element = {
  formal: vhdl_formal_part;
  actual: vhdl_actual_part
  }
and vhdl_association_list = vhdl_association_element list

(* subtype_declaration ::=                                         *)
(*   subtype identifier is subtype_indication ;                    *)
(* subtype_indication ::=                                          *)
(*   [ resolution_function_name ] type_mark [ constraint ]         *)
(* constraint ::=                                                  *)
(*     range_constraint                                            *)
(*   | array_constraint                                            *)
(*   | record_constraint                                           *)
(* array_constraint ::=                                            *)
(*     index_constraint [array_element_constraint]                 *)
(*   | (open) [array_element_constraint]                           *)
(* array_element_constraint ::=                                    *)
(*   element_constraint                                            *)
(* element_constraint ::=                                          *)
(*     array_constraint                                            *)
(*   | record_constraint                                           *)
(* record_constraint ::=                                           *)
(*   ( record_element_constraint { , record_element_constraint } ) *)
(* record_element_constraint ::=                                   *)
(*   record_element_simple_name element_constraint                 *)
(* index_constraint ::=                                            *)
(*   ( discrete_range { , discrete_range } )                       *)

and vhdl_array_constraint =
  vhdl_parameters
and vhdl_array_element_constraint = vhdl_parameter
and vhdl_index_constraint = vhdl_parameter
and vhdl_constraint =
  | RangeConstraint of vhdl_range_constraint
  | ArrayConstraint of vhdl_array_constraint
  | NoConstraint
and vhdl_subtype_indication = {
    resolutionfunction: vhdl_name;
    basetypename: vhdl_type_mark;
    subtypeconstraint: vhdl_constraint
  }
and vhdl_subtype_declaration = {
    subtypename: vhdl_identifier;
    subtypeindication: vhdl_subtype_indication
  }

(* enumeration_type_definition ::=                        *)
(*   ( enumeration_literal { , enumeration_literal } )    *)
(* enumeration_literal ::= identifier | character_literal *)

and vhdl_enumeration_literal =
  | IdentifierEnumeration of vhdl_identifier
  | CharLiteralEnumeration of vhdl_char
and vhdl_enumeration_type_definition = vhdl_enumeration_literal list

(*   integer_type_definition ::= range_constraint *)

and vhdl_integer_type_definition = vhdl_range_constraint

(* floating_type_definition ::= range_constraint (ignored ieee syn) *)

and vhdl_floating_type_definition = vhdl_range_constraint

(* array_type_definition ::=                                           *)
(*     unbounded_array_definition                                      *)
(*   | constrained_array_definition                                    *)
(* unbounded_array_definition ::=                                      *)
(*   array ( index_subtype_definition { , index_subtype_definition } ) *)
(*           of element_subtype_indication                             *)
(* constrained_array_definition ::=                                    *)
(*   array index_constraint of element_subtype_indication              *)
(* index_subtype_definition ::= type_mark range <>                     *)
(* index_constraint ::= ( discrete_range { , discrete_range } )        *)

and vhdl_constrained_array_definition = {
  carraydimensions: vhdl_index_constraint;
  carrayelementtype: vhdl_subtype_indication;
  }

and vhdl_index_subtype_definition = vhdl_type_mark
and vhdl_unbounded_array_definition = {
  uarraydimensions: vhdl_index_subtype_definition list;
  uarrayelementtype: vhdl_subtype_indication
  }

and vhdl_array_type_definition =
  | ConstrainedArray of vhdl_constrained_array_definition
  | UnboundedArray of vhdl_unbounded_array_definition


(* record_type_definition ::=                        *)
(*   record                                          *)
(*     element_declaration                           *)
(*     { element_declaration }                       *)
(* end record [ record_type_simple_name ]            *)
(* element_declaration ::=                           *)
(*   identifier_list : element_subtype_definition;   *)
(* element_subtype_definition ::= subtype_indication *)

and vhdl_element_declaration = {
  elementnames: vhdl_identifier_list;
  elementsubtype: vhdl_subtype_indication
  }
and vhdl_element_declaration_list = vhdl_element_declaration list
and vhdl_record_type_definition = {
  recordelems: vhdl_element_declaration_list;
  recordname: vhdl_simple_name
  }

(* file_type_definition ::= file of type_mark *)

and vhdl_file_type_definition = vhdl_type_mark

(* access_type_definition ::= access subtype_indication *)

and vhdl_access_type_definition = vhdl_subtype_indication

(* physical_type_definition ::=                                  *)
(*   range_constraint                                            *)
(*   units                                                       *)
(*     primary_unit_declaration                                  *)
(*     { secondary_unit_declaration }                            *)
(*   end units [ physical_type_simple_name ]                     *)
(* primary_unit_declaration ::= identifier ;                     *)
(* secondary_unit_declaration ::= identifier = physical_literal; *)

and vhdl_physical_unit_declaration = (vhdl_identifier*vhdl_physical_literal)
and vhdl_physical_type_definition = {
    physicalconstraint: vhdl_range_constraint;
    physicalunits: vhdl_physical_unit_declaration list;
    physicalname: vhdl_identifier
  }

(* scalar_type_definition ::=                           *)
(*    enumeration_type_definition                       *)
(*  | integer_type_definition                           *)
(*  | physical_type_definition (not supported ieee syn) *)
(*  | floating_type_definition (ignored ieee syn)       *)
(* composite_type_definition ::=                        *)
(*    array_type_definition                             *)
(*  | record_type_definition                            *)
(* type_declaration ::=                                 *)
(*     full_type_declaration                            *)
(*   | incomplete_type_declaration (ignored ieee syn)   *)
(* full_type_declaration ::=                            *)
(*   type identifier is type_definition ;               *)

and vhdl_type_definition =
  | EnumerationTypeDefinition of vhdl_enumeration_type_definition
  (* integer and floating point numbers *)
  | RangeTypeDefinition of vhdl_range_constraint
  | ArrayTypeDefinition of vhdl_array_type_definition
  | RecordTypeDefinition of vhdl_record_type_definition
  | FileTypeDefinition of vhdl_file_type_definition
  | AccessTypeDefinition of vhdl_access_type_definition
  | PhysicalTypeDefinition of vhdl_physical_type_definition

and vhdl_full_type_declaration = {
  typename: vhdl_identifier;
  typedefinition: vhdl_type_definition
  }
and vhdl_incomplete_type_declaration = {
  incompletetypename: vhdl_identifier;
  }
and vhdl_type_declaration =
  | FullType of vhdl_full_type_declaration
  | IncompleteType of vhdl_incomplete_type_declaration

(* constant_declaration ::=                                           *)
(*   constant identifier_list : subtype_indication [ := expression ]; *)

and vhdl_constant_declaration = {
  constantnames: vhdl_identifier_list;
  constantsubtype: vhdl_subtype_indication;
  constantexpression: vhdl_expression
  }

(* signal_declaration ::=                                  *)
(*   signal identifier_list :                              *)
(*     subtype_indication [ signal_kind ] [:= expression]; *)
(*     (expression is ignored ieee syn)                    *)
(*   signal_kind ::= register | bus                        *)

and vhdl_signal_kind =
  | SignalKindRegister
  | SignalKindBus
  | SignalKindDefault

and vhdl_signal_declaration = {
  signalnames: vhdl_identifier_list;
  signalsubtype: vhdl_subtype_indication;
  signalkind: vhdl_signal_kind;
  signalexpression: vhdl_expression
  }

(* variable_declaration ::=               *)
(*  [ shared ] variable identifier_list : *)
(*    subtype_indication [:= expression]; *)
(* (shared not supported ieee syn)        *)
(* (expression ignored ieee syn)          *)

and vhdl_variable_declaration = {
  variableshared: bool;
  variablenames: vhdl_identifier_list;
  variablesubtype: vhdl_subtype_indication;
  variableexpression: vhdl_expression
  }

(* file_declaration ::= (ignored ieee syn)                    *)
(*   file identifier_list :                                   *)
(*     subtype_indication [ file_open_information ] ;         *)
(* file_open_information ::= (ignored ieee syn)               *)
(*   [ open file_open_kind_expression ] is file_logical_name  *)
(* file_logical_name ::= string_expression (ignored ieee syn) *)

and vhdl_file_declaration = {
  filenames: vhdl_identifier_list;
  filesubtype: vhdl_subtype_indication;
  fileopenkind: vhdl_expression;
  filelogicalname: vhdl_expression
  }

(* object_declaration ::=                  *)
(*     constant_declaration                *)
(*   | signal_declaration                  *)
(*   | variable_declaration                *)
(*   | file_declaration (ignored ieee syn) *)

and vhdl_object_declaration =
  | ConstantDeclaration of vhdl_constant_declaration
  | SignalDeclaration of vhdl_signal_declaration
  | VariableDeclaration of vhdl_variable_declaration
  | FileDeclaration of vhdl_file_declaration

(* interface_object_declaration ::=                             *)
(*     interface_constant_declaration                           *)
(*   | interface_signal_declaration                             *)
(*   | interface_variable_declaration                           *)
(*   | interface_file_declaration (ieee syn not supported)      *)
(* interface_constant_declaration ::=                           *)
(*   [constant] identifier_list :                               *)
(*   [in] subtype_indication [:= static_expression]             *)
(*   (expression ieee syn ignored)                              *)
(* interface_signal_declaration ::=                             *)
(*   [signal] identifier_list : [mode] subtype_indication [bus] *)
(*   [:= static_expression] (ieee syn ignored)                  *)
(* interface_variable_declaration ::=                           *)
(*   [variable] identifier_list : [mode] subtype_indication     *)
(*   [:= static_expression] (ieee syn ignored)                  *)
(* interface_file_declaration ::= (ieee syn not supported)      *)
(*   file identifier_list : subtype_indication                  *)
(* mode ::= in | out | inout | buffer | linkage                 *)
(*   (linkage ieee not supported)                               *)

and vhdl_mode =
  | InterfaceModeIn
  | InterfaceModeOut
  | InterfaceModeInOut
  | InterfaceModeBuffer

and vhdl_interface_variable_declaration = {
  interfacevariablenames: vhdl_identifier_list;
  interfacevariablemode: vhdl_mode;
  interfacevariablesubtype: vhdl_subtype_indication;
  interfacevariableexpression: vhdl_expression
  }
and vhdl_interface_signal_declaration = {
  interfacesignalnames: vhdl_identifier_list;
  interfacesignalmode: vhdl_mode;
  interfacesignalsubtype: vhdl_subtype_indication;
  interfacesignalkind: vhdl_signal_kind;
  interfacesignalexpression: vhdl_expression
  }
and vhdl_interface_constant_declaration = {
  interfaceconstantnames: vhdl_identifier_list;
  interfaceconstantsubtype: vhdl_subtype_indication;
  interfaceconstantexpression: vhdl_expression
  }
and vhdl_interface_file_declaration = {
  interfacefilenames: vhdl_identifier_list;
  interfacefilesubtype: vhdl_subtype_indication
  }
and vhdl_interface_default_declaration = {
  interfacedefaultnames: vhdl_identifier_list;
  interfacedefaultmode: vhdl_mode;
  interfacedefaultsubtype: vhdl_subtype_indication;
  interfacedefaultkind: vhdl_signal_kind;
  interfacedefaultexpression: vhdl_expression
  }
and vhdl_interface_object_declaration =
  | InterfaceVariableDeclaration of vhdl_interface_variable_declaration
  | InterfaceSignalDeclaration of vhdl_interface_signal_declaration
  | InterfaceConstantDeclaration of vhdl_interface_constant_declaration
  | InterfaceFileDeclaration of vhdl_interface_file_declaration
  | InterfaceDefaultDeclaration of vhdl_interface_default_declaration

(* interface_type_declaration ::=              *)
(*   interface_incomplete_type_declaration     *)
(* interface_incomplete_type_declaration ::=   *)
(*   type identifier                           *)
and vhdl_interface_type_declaration =
  | InterfaceIncompleteTypeDeclaration of vhdl_identifier

(* interface_declaration ::=                   *)
(*     interface_object_declaration            *)
(*   | interface_type_declaration              *)
(*   | interface_subprogram_declaration        *)
(*   | interface_package_declaration           *)

and vhdl_interface_declaration =
  | InterfaceObjectDeclaration of vhdl_interface_object_declaration
  | InterfaceTypeDeclaration of vhdl_interface_type_declaration
  (*| InterfaceSubprogramDeclaration of vhdl_interface_subprogram_declaration
  | InterfacePackageDeclaration of vhdl_interface_package_declaration*)

(* interface_list ::=                          *)
(*   interface_element {; interface_element}   *)
(* interface_element ::= interface_declaration *)

and vhdl_interface_element = vhdl_interface_declaration
and vhdl_interface_list = vhdl_interface_element list

(* alias_declaration ::=                           *)
(*   alias alias_designator [: subtype_indication] *)
(*   is name [signature];                          *)
(* alias_designator ::= identifier                 *)
(*   | character_literal | operator_symbol         *)

and vhdl_alias_designator = vhdl_designator

and vhdl_alias_declaration = {
  aliasdesignator: vhdl_alias_designator;
  aliassubtype: vhdl_subtype_indication;
  aliasexpression: vhdl_expression;
  aliassignature: vhdl_signature option
  }

(* attribute_specification ::=                        *)
(*   attribute attribute_designator                   *)
(*   of entity_specification is expression;           *)
(* entity_specification ::=                           *)
(*   entity_name_list : entity_class                  *)
(* entity_class ::=                                   *)
(*     entity    | architecture | configuration       *)
(*   | procedure | function     | package             *)
(*   | type      | subtype      | constant            *)
(*   | signal    | variable     | component           *)
(*   | label     | literal      | units               *)
(*   | group     | file (both ieee syn not supported) *)
(* entity_name_list ::=                               *)
(*     entity_designator {, entity_designator}        *)
(*   | others                                         *)
(*   | all                                            *)
(* entity_designator ::= entity_tag [signature]       *)
(* entity_tag ::=                                     *)
(*     simple_name                                    *)
(*   | character_literal                              *)
(*   | operator_symbol                                *)

and vhdl_entity_tag =
  | EntityTagSimpleName of vhdl_simple_name
  | EntityTagCharacterLiteral of vhdl_char
  | EntityTagOperatorSymbol of vhdl_string

and vhdl_entity_designator = {
  entitytag: vhdl_entity_tag;
  entitysignature: vhdl_signature option
  }
and vhdl_entity_name_list =
  | EntityDesignator of vhdl_entity_designator list
  | EntityOthers
  | EntityAll

and vhdl_entity_class =
  | ClassEntity | ClassArchitecture | ClassConfiguration
  | ClassProcedure | ClassFunction | ClassPackage
  | ClassType | ClassSubType | ClassConstant
  | ClassSignal | ClassVariable | ClassComponent
  | ClassLabel | ClassLiteral | ClassUnits | ClassFile
and vhdl_entity_specification = {
  entitynames: vhdl_entity_name_list;
  entityclass: vhdl_entity_class
  }

(* attribute_declaration ::=             *)
(*   attribute identifier : type_mark ;  *)

and vhdl_attribute_declaration = {
  attributename: vhdl_identifier;
  attributetypename: vhdl_type_mark
  }

and vhdl_attribute_specification = {
  attributedesignator: vhdl_identifier;
  attributeentity: vhdl_entity_specification;
  attributeexpression: vhdl_expression
  }

(* wait_statement ::=                               *)
(*   [label:] wait [sensitivity_clause]             *)
(*   [condition_clause] [timeout_clause] ;          *)
(* sensitivity_clause ::= on sensitivity_list       *)
(* sensitivity_list ::= signal_name {, signal_name} *)
(* condition_clause ::= until condition             *)
(* timeout_clause ::= for time_expression           *)

and vhdl_wait_statement = {
  waitlabelname: vhdl_label;
  waitsensitivity: vhdl_expression_list;
  waitcondition: vhdl_condition;
  waittimeout: vhdl_expression
  }

(* assertion_statement ::= [ label: ] assertion ; *)
(* assertion ::=                                  *)
(*   assert condition [ report expression ]       *)
(*   [ severity expression ]                      *)
(* (ieee syn ignored)                             *)

and vhdl_assertion = {
  assertioncondition: vhdl_condition;
  assertionreport: vhdl_expression;
  assertionseverity: vhdl_expression
  }
and vhdl_assertion_statement = {
  assertionlabelname: vhdl_label;
  assertion: vhdl_assertion
  }

(* report_statement ::=            *)
(*   [label:] report expression    *)
(*   [severity expression] ;       *)
(* (ieee syn ignored)              *)

and vhdl_report_statement = {
  reportlabelname: vhdl_label;
  reportexpression: vhdl_expression;
  reportseverity: vhdl_expression
  }

(* delay_mechanism ::= (ieee syn ignored)                *)
(*     transport                                         *)
(*   | [ reject time_expression ] inertial               *)
(* target ::=                                            *)
(*     name                                              *)
(*   | aggregate                                         *)
(* waveform ::=                                          *)
(*     waveform_element                                  *)
(*     {, waveform_element} (ieee syn ignored)           *)
(*   | unaffected                                        *)
(* waveform_element ::=                                  *)
(*     value_expression [after time_expression]          *)
(*   | null [after time_expression]                      *)

and vhdl_waveform_element = {
  valueexpression: vhdl_expression;
  timeexpression: vhdl_expression
  }
and vhdl_waveform =
  | WaveForms of vhdl_waveform_element list
  | Unaffected

and vhdl_target =
  | TargetName of vhdl_name
  | TargetDotted of vhdl_dotted
  | TargetNameParameters of (vhdl_name * vhdl_parameters)
  | TargetAggregate of vhdl_aggregate
  | TargetInvalid of int (* error position *)
  | SelectTargetName of vhdl_name * vhdl_suffix
  | SelectTargetNameParameters of (vhdl_name * vhdl_parameters * vhdl_suffix)

and vhdl_delay_mechanism =
  | DelayNone
  | DelayTransport
  | DelayInertial of vhdl_expression

(* signal_assignment_statement ::=                      *)
(*     [ label : ] simple_signal_assignment             *)
(*   | [ label : ] conditional_signal_assignment        *)
(*   | [ label : ] selected_signal_assignment           *)
and vhdl_signal_assignment_statement =
  | SimpleSignalAssignment of vhdl_simple_signal_assignment_statement
  | ConditionalSignalAssignment of vhdl_conditional_signal_assignment_statement
  | SelectedSignalAssignment of vhdl_selected_signal_assignment_statement

(* simple_signal_assignment ::=              *)
(*     simple_waveform_assignment            *)
(*  | simple_force_assignment (TODO)         *)
(*  | simple_release_assignment (TODO)       *)
(* simple_waveform_assignment ::=            *)
(*  target <= [ delay_mechanism ] waveform ; *)
and vhdl_simple_signal_assignment_statement = {
  simplesignalassignmentlabelname: vhdl_label;
  simplesignalassignmenttarget: vhdl_target;
  simplesignalassignmentdelay: vhdl_delay_mechanism;
  simplesignalassignmentwaveform: vhdl_waveform
  }

(* conditional_signal_assignment ::=                       *)
(*     conditional_waveform_assignment                     *)
(*   | conditional_force_assignment (TODO)                 *)
(* conditional_waveform_assignment ::=                     *)
(*   target <= [ delay_mechanism ] conditional_waveforms ; *)
and vhdl_conditional_signal_assignment_statement = {
  conditionalsignalassignmentlabelname: vhdl_label;
  conditionalsignalassignmenttarget: vhdl_target;
  conditionalsignalassignmentdelay: vhdl_delay_mechanism;
  conditionalsignalassignmentwaveforms: vhdl_conditional_waveforms
  }

(* selected_signal_assignment ::=                         *)
(*     selected_waveform_assignment                       *)
(*   | selected_force_assignment (TODO)                   *)
(* selected_waveform_assignment ::=                       *)
(*   with expression select [ ? ]                         *)
(*     target <= [ delay_mechanism ] selected_waveforms ; *)
and vhdl_selected_signal_assignment_statement = {
  selectedsignalassignmentlabelname: vhdl_label;
  selectedsignalassignmenttarget: vhdl_target;
  selectedsignalassignmentselector: vhdl_selector;
  selectedsignalassignmentkind: vhdl_selection_kind;
  selectedsignalassignmentdelay: vhdl_delay_mechanism;
  selectedsignalassignmentwaveforms: vhdl_selected_waveforms
  }

(* variable_assignment_statement ::=               *)
(*     [ label : ] simple_variable_assignment      *)
(*   | [ label : ] conditional_variable_assignment *)
(*   | [ label : ] selected_variable_assignment    *)
and vhdl_variable_assignment_statement =
  | SimpleVariableAssignment of vhdl_simple_variable_assignment
  | ConditionalVariableAssignment of vhdl_conditional_variable_assignment
  | SelectedVariableAssignment of vhdl_selected_variable_assignment

(* simple_variable_assignment ::= *)
(*   target := expression ;       *)
and vhdl_simple_variable_assignment = {
  simplevariableassignmentlabelname: vhdl_label;
  simplevariableassignmenttarget: vhdl_target;
  simplevariableassignmentexpression: vhdl_expression
  }

(* conditional_variable_assignment ::=   *)
(*   target := conditional_expressions ; *)
and vhdl_conditional_variable_assignment = {
  conditionalvariableassignmentlabelname: vhdl_label;
  conditionalvariableassignmenttarget: vhdl_target;
  conditionalvariableassignmentexpressions: vhdl_conditional_expressions
  }

(* conditional_expressions ::=          *)
(*   expression when condition          *)
(*   { else expression when condition } *)
(*   [ else expression ]                *)
and vhdl_conditional_expressions =
  vhdl_conditional_expression list
and vhdl_conditional_expression = {
  conditionalexpressionvalue: vhdl_expression;
  conditionalexpressioncondition: vhdl_condition
  }

(* selected_variable_assignment ::=      *)
(*   with expression select [ ? ]        *)
(*     target := selected_expressions ;  *)
and vhdl_selected_variable_assignment = {
  selectedvariableassignmentlabelname: vhdl_label;
  selectedvariableassignmenttarget: vhdl_target;
  selectedvariableassignmentselector: vhdl_selector;
  selectedvariableassignmentkind: vhdl_selection_kind;
  selectedvariableassignmentexpressions: vhdl_selected_expressions
  }

(* selected_expressions ::=        *)
(*   { expression when choices , } *)
(*   expression when choices       *)
and vhdl_selected_expressions =
  vhdl_selected_expression list
and vhdl_selected_expression = {
  selectedexpressionvalue: vhdl_expression;
  selectedexpressionchoices: vhdl_choices
  }

(* procedure_call_statement ::=                   *)
(*   [ label: ] procedure_call ;                  *)
(* procedure_call ::=                             *)
(*   procedure_name [ ( actual_parameter_part ) ] *)

and vhdl_procedure_call = {
    procedurecallname: vhdl_name;
    procedurecallparameter: vhdl_parameter
  }

and vhdl_procedure_call_statement = {
  procedurecalllabelname: vhdl_label;
  procedurecall: vhdl_procedure_call
  }

(* if_statement ::=               *)
(*   [ if_label: ]                *)
(*     if condition then          *)
(*       sequence_of_statements   *)
(*   { elsif condition then       *)
(*       sequence_of_statements } *)
(*   [ else                       *)
(*       sequence_of_statements ] *)
(*     end if [ if_label ] ;      *)

and vhdl_else_statements =
  | ElseNone
  | Else of vhdl_sequence_of_statements
  | Elsif of vhdl_if_statement
and vhdl_if_statement = {
  iflabelname: vhdl_label;
  ifcondition: vhdl_condition;
  thenstatements: vhdl_sequence_of_statements;
  elsestatements: vhdl_else_statements
  }

(* case_statement ::=                 *)
(*   [ case_label: ]                  *)
(*     case expression is             *)
(*       case_statement_alternative   *)
(*     { case_statement_alternative } *)
(*     end case [ case_label ] ;      *)
(* case_statement_alternative ::=     *)
(*   when choices =>                  *)
(*     sequence_of_statements         *)

and vhdl_case_statement = {
  caselabelname: vhdl_label;
  caseselector: vhdl_selector;
  casekind: vhdl_selection_kind;
  casealternatives: vhdl_case_statement_alternative list
  }
and vhdl_case_statement_alternative = {
  casechoices: vhdl_choices;
  casestatements: vhdl_sequence_of_statements
  }

(* loop_statement ::=                           *)
(*   [ loop_label: ]                            *)
(*   [ iteration_scheme ] loop                  *)
(*     sequence_of_statements                   *)
(*   end loop [ loop_label ] ;                  *)
(* iteration_scheme ::=                         *)
(*     while condition (ieee syn not supported) *)
(*   | for loop_parameter_specification         *)
(* parameter_specification ::=                  *)
(*   identifier in discrete_range               *)

and vhdl_parameter_specification = {
  parameteridentifier: vhdl_identifier;
  parameterrange: vhdl_discrete_range
  }
and vhdl_iteration_scheme =
  | AlwaysLoop
  | WhileLoop of vhdl_condition
  | ForLoop of vhdl_parameter_specification
and vhdl_loop_statement = {
  looplabelname: vhdl_label;
  loopiteration: vhdl_iteration_scheme;
  loopstatements: vhdl_sequence_of_statements
  }

(* next_statement ::=                         *)
(*   [ label: ]                               *)
(*   next [ loop_label ] [ when condition ] ; *)

and vhdl_next_statement = {
  nextlabelname: vhdl_label;
  nextlooplabelname: vhdl_label;
  nextcondition: vhdl_condition
  }

(* exit_statement ::=                         *)
(*   [ label: ]                               *)
(*   exit [ loop_label ] [ when condition ] ; *)

and vhdl_exit_statement = {
  exitlabelname: vhdl_label;
  exitlooplabelname: vhdl_label;
  exitcondition: vhdl_condition
  }

(* return_statement ::=                 *)
(*   [ label: ] return [ expression ] ; *)

and vhdl_return_statement = {
  returnlabelname: vhdl_label;
  returnexpression: vhdl_expression
  }

(* null_statement ::=             *)
(*   [ label: ] null ;            *)

and vhdl_null_statement = {
  nulllabelname: vhdl_label
  }

(* sequence_of_statements ::=                 *)
(*   { sequential_statement }                 *)
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

and vhdl_sequential_statement =
  | SequentialWait of vhdl_wait_statement
  | SequentialAssertion of vhdl_assertion_statement
  | SequentialReport of vhdl_report_statement
  | SequentialSignalAssignment of vhdl_signal_assignment_statement
  | SequentialVariableAssignment of vhdl_variable_assignment_statement
  | SequentialProcedureCall of vhdl_procedure_call_statement
  | SequentialIf of vhdl_if_statement
  | SequentialCase of vhdl_case_statement
  | SequentialLoop of vhdl_loop_statement
  | SequentialNext of vhdl_next_statement
  | SequentialExit of vhdl_exit_statement
  | SequentialReturn of vhdl_return_statement
  | SequentialNull of vhdl_null_statement
and vhdl_sequence_of_statements = vhdl_sequential_statement list

(* subprogram_declaration ::=                                              *)
(*   subprogram_specification ;                                            *)
(* subprogram_specification ::=                                            *)
(*     procedure_specification                                             *)
(*   | function_specification                                              *)
(* procedure_specification ::=                                             *)
(*   procedure designator                                                  *)
(*   subprogram_header                                                     *)
(*   [ [ parameter ] ( formal_parameter_list ) ]                           *)
(* function_specification ::=                                              *)
(*   [ pure | impure ] function designator                                 *)
(*   subprogram_header                                                     *)
(*   [ [ parameter ] ( formal_parameter_list ) ] return type_mark          *)
(* subprogram_header ::=                                                   *)
(*   [ generic ( generic_list )                                            *)
(*   [ generic_map_aspect ] ]                                              *)
(* operator_symbol ::= string_literal                                      *)

and vhdl_subprogram_declaration =
  vhdl_subprogram_specification
and vhdl_subprogram_specification =
  | ProcedureSpecification of vhdl_procedure_specification
  | FunctionSpecification of vhdl_function_specification
and vhdl_procedure_specification = {
  procedurespecdesignator: vhdl_designator;
  procedurespecparameters: vhdl_formal_parameter_list;
  procedurespecgenericlist: vhdl_generic_list;
  procedurespecgenericmapaspect: vhdl_generic_map_aspect;
  }
and vhdl_function_specification = {
  functionspecdesignator: vhdl_designator;
  functionspecparameters: vhdl_formal_parameter_list;
  functionspecgenericlist: vhdl_generic_list;
  functionspecgenericmapaspect: vhdl_generic_map_aspect;
  functionspecreturntype: vhdl_type_mark;
  functionspecpurity: vhdl_function_purity;
  }
and vhdl_function_purity =
  | Unknown
  | Pure
  | Impure

(* formal_parameter_list ::= parameter_interface_list *)

and vhdl_formal_parameter_list = vhdl_interface_list

(* subprogram_body ::=                                     *)
(*   subprogram_specification is                           *)
(*   subprogram_declarative_part                           *)
(*   begin                                                 *)
(*     subprogram_statement_part                           *)
(*   end [ subprogram_kind ] [ designator ] ;              *)
(* subprogram_declarative_part ::=                         *)
(*   { subprogram_declarative_item }                       *)
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
(* subprogram_statement_part ::=                           *)
(*   { sequential_statement }                              *)

and vhdl_subprogram_body = {
  subprogramspecification: vhdl_subprogram_specification;
  subprogramdeclarations: vhdl_subprogram_declarative_part;
  subprogramstatements: vhdl_subprogram_statement_part
  }
and vhdl_subprogram_declarative_part = vhdl_subprogram_declarative_item list
and vhdl_subprogram_declarative_item =
  | SubProgramDeclaration of vhdl_subprogram_declaration
  | SubProgramBody of vhdl_subprogram_body
  | SubProgramInstantiation of vhdl_subprogram_instantiation
  | SubProgramTypeDeclaration of vhdl_type_declaration
  | SubProgramSubTypeDeclaration of vhdl_subtype_declaration
  | SubProgramConstantDeclaration of vhdl_constant_declaration
  | SubProgramVariableDeclaration of vhdl_variable_declaration
  | SubProgramFileDeclaration of vhdl_file_declaration
  | SubProgramAliasDeclaration of vhdl_alias_declaration
  | SubProgramAttributeDeclaration of vhdl_attribute_declaration
  | SubProgramAttributeSpecification of vhdl_attribute_specification
  | SubProgramUseClause of vhdl_use_clause
and vhdl_subprogram_statement_part = vhdl_sequence_of_statements

(* subprogram_instantiation_declaration ::=                                         *)
(*   subprogram_kind designator is new uninstantiated_subprogram_name [ signature ] *)
(*   [ generic_map_aspect ] ;                                                       *)

and vhdl_subprogram_instantiation = {
  subprograminstantiationkind: vhdl_subprogram_kind;
  subprograminstantiationdesignator: vhdl_designator;
  subprograminstantiationname: vhdl_name;
  subprograminstantiationsignature: vhdl_signature option;
  subprograminstantiationgenericmapaspect: vhdl_generic_map_aspect;
  }

and vhdl_subprogram_kind =
  | Function
  | Procedure

(* component_declaration ::=                 *)
(*   component identifier [is]               *)
(*   [local_generic_clause]                  *)
(*   [local_port_clause]                     *)
(*   end component [component_simple_name] ; *)

and vhdl_component_declaration = {
  componentname: vhdl_identifier;
  componentgeneric: vhdl_generic_clause;
  componentport: vhdl_port_clause
  }

(* generic_clause ::= generic( generic_list ); *)
(* port_clause ::= port( port_list );          *)

and vhdl_generic_clause = vhdl_generic_list
and vhdl_port_clause = vhdl_port_list

(* generic_list ::= generic_interface_list *)

and vhdl_generic_list = vhdl_interface_list

(* port_list ::= port_interface_list *)

and vhdl_port_list = vhdl_interface_list

(* configuration_specification ::=                   *)
(*   for component_specification binding_indication; *)
(* component_specification ::=                       *)
(*   instantiation_list : component_name             *)
(* instantiation_list ::=                            *)
(*     instantiation_label {, instantiation_label}   *)
(*   | others                                        *)
(*   | all                                           *)

and vhdl_instantiation_list =
  | InstantiationLabels of vhdl_label list
  | InstantiationOthers
  | InstantiationAll
and vhdl_component_specification = {
  componentinstantiations: vhdl_instantiation_list;
  componentspecname: vhdl_name
  }
and vhdl_configuration_specification = {
  configurationcomponent: vhdl_component_specification;
  configurationbinding: vhdl_binding_indication
  }

(* binding_indication ::=                             *)
(*  [ use entity_aspect ]                             *)
(*  [ generic_map_aspect ]                            *)
(*  [ port_map_aspect ]                               *)
(* entity_aspect ::=                                  *)
(*     entity entity_name [(architecture_identifier)] *)
(*   | configuration configuration_name               *)
(*   | open                                           *)
(* generic_map_aspect ::=                             *)
(*   generic map ( generic_association_list )         *)
(* port_map_aspect ::=                                *)
(*   port map ( port_association_list )               *)


and vhdl_binding_indication = {
  bindingentity: vhdl_entity_aspect;
  bindinggenericmap: vhdl_generic_map_aspect;
  bindingportmap: vhdl_port_map_aspect
  }
and vhdl_entity_aspect =
  | BindingEntity of vhdl_name
  | BindingEntityArchitecture of (vhdl_name * vhdl_identifier)
  | BindingConfiguration of vhdl_name
  | BindingOpen
and vhdl_generic_map_aspect = vhdl_association_list
and vhdl_port_map_aspect = vhdl_association_list

(* block_statement ::=                                     *)
(*   block_label:                                          *)
(*   block [ ( guard_expression ) ] [ is ]                 *)
(*     block_header                                        *)
(*     block_declarative_part                              *)
(*   begin                                                 *)
(*     block_statement_part                                *)
(*   end block [ block_label ] ;                           *)
(* block_header ::= (ieee syn not supported)               *)
(*   [ generic_clause                                      *)
(*   [ generic_map_aspect ;] ]                             *)
(*   [ port_clause                                         *)
(*   [ port_map_aspect ;] ]                                *)
(* block_declarative_part ::=                              *)
(*   { block_declarative_item }                            *)
(* block_statement_part ::=                                *)
(*   { concurrent_statement }                              *)
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

and vhdl_block_statement = {
  blocklabelname: vhdl_label;
  blockguardcondition: vhdl_condition;
  blockgenericclause: vhdl_generic_clause;
  blockgenericmapaspect: vhdl_generic_map_aspect;
  blockportclause: vhdl_port_clause;
  blockportmapaspect: vhdl_port_map_aspect;
  blockdeclarations: vhdl_block_declarative_part;
  blockstatements: vhdl_block_statement_part
  }
and vhdl_block_declarative_part = vhdl_block_declarative_item list
and vhdl_block_statement_part = vhdl_concurrent_statement list
and vhdl_block_declarative_item =
  | BlockSubProgramDeclaration of vhdl_subprogram_declaration
  | BlockSubProgramBody of vhdl_subprogram_body
  | BlockSubProgramInstantiation of vhdl_subprogram_instantiation
  | BlockTypeDeclaration of vhdl_type_declaration
  | BlockSubTypeDeclaration of vhdl_subtype_declaration
  | BlockConstantDeclaration of vhdl_constant_declaration
  | BlockSignalDeclaration of vhdl_signal_declaration
  | BlockFileDeclaration of vhdl_file_declaration
  | BlockAliasDeclaration of vhdl_alias_declaration
  | BlockComponentDeclaration of vhdl_component_declaration
  | BlockAttributeDeclaration of vhdl_attribute_declaration
  | BlockAttributeSpecification of vhdl_attribute_specification
  | BlockConfigurationSpecification of vhdl_configuration_specification
  | BlockUseClause of vhdl_use_clause

(* process_statement ::=                                   *)
(*   [ process_label: ]                                    *)
(*   [ postponed ] process [ ( sensitivity_list ) ] [ is ] *)
(*     process_declarative_part                            *)
(*   begin                                                 *)
(*     process_statement_part                              *)
(*   end [ postponed ] process [process_label] ;           *)
(* process_declarative_part ::=                            *)
(*   { process_declarative_item }                          *)
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
(* process_statement_part ::=                              *)
(*   { sequential_statement }                              *)

and vhdl_process_statement = {
  processlabelname: vhdl_label;
  processpostponed: bool;
  processsensitivitylist: vhdl_process_sensitivity_list;
  processdeclarations: vhdl_process_declarative_part;
  processstatements: vhdl_process_statement_part;
  }
and vhdl_process_sensitivity_list =
  | SensitivityAll
  | SensitivityExpressionList of vhdl_sensitivity_list
and vhdl_sensitivity_list = vhdl_expression_list
and vhdl_process_declarative_part = vhdl_process_declarative_item list
and vhdl_process_declarative_item =
  | ProcessSubProgramDeclaration of vhdl_subprogram_declaration
  | ProcessSubProgramBody of vhdl_subprogram_body
  | ProcessSubProgramInstantiation of vhdl_subprogram_instantiation
  | ProcessTypeDeclaration of vhdl_type_declaration
  | ProcessSubTypeDeclaration of vhdl_subtype_declaration
  | ProcessConstantDeclaration of vhdl_constant_declaration
  | ProcessVariableDeclaration of vhdl_variable_declaration
  | ProcessFileDeclaration of vhdl_file_declaration
  | ProcessAliasDeclaration of vhdl_alias_declaration
  | ProcessAttributeDeclaration of vhdl_attribute_declaration
  | ProcessAttributeSpecification of vhdl_attribute_specification
  | ProcessUseClause of vhdl_use_clause
and vhdl_process_statement_part = vhdl_sequential_statement list

(* concurrent_procedure_call_statement ::=            *)
(*   [ label: ] [ postponed ] procedure_call ;        *)
(* (postponed: ieee syn not supported                 *)

and vhdl_concurrent_procedure_call_statement = {
  concurrentprocedurelabelname: vhdl_label;
  concurrentpostponedprocedure: bool;
  concurrentprocedurecall: vhdl_procedure_call
  }

(* concurrent_assertion_statement ::=            *)
(*   [ label: ] [ postponed ] assertion ;        *)
(* (postponed: ieee syn not supported)           *)

and vhdl_concurrent_assertion_statement = {
  concurrentassertionlabelname: vhdl_label;
  concurrentpostponedassertion: bool;
  concurrentassertion: vhdl_assertion
  }

(* concurrent_signal_assignment_statement ::=                 *)
(*     [ label: ] [ postponed ] conditional_signal_assignment *)
(*   | [ label: ] [ postponed ] selected_signal_assignment    *)
(* options ::= [ guarded ] [delay_mechanism]                  *)
(* (postponed: ieee syn not supported)                        *)
(* (delay_mechanism: ieee syn ignored)                        *)
(* conditional_signal_assignment ::=                          *)
(*   target <= options conditional_waveforms ;                *)
(* conditional_waveforms ::=                                  *)
(*   { waveform when condition else }                         *)
(*     waveform [ when condition ]                            *)
(* selected_signal_assignment ::=                             *)
(*   with expression select                                   *)
(*     target <= options selected_waveforms ;                 *)
(* select_waveforms ::=                                       *)
(*   { waveform when choices , }                              *)
(*   waveform when choices                                    *)

and vhdl_concurrent_selected_signal_assignment = {
  concurrentselectedsignaltarget: vhdl_target;
  concurrentselectedsignalguarded: bool;
  concurrentselectedsignaldelay: vhdl_delay_mechanism;
  concurrentselectedsignalselector: vhdl_selector;
  concurrentselectedsignalkind: vhdl_selection_kind;
  concurrentselectedsignalwaveforms: vhdl_selected_waveforms
  }
and vhdl_selected_waveforms =
  vhdl_selected_waveform list
and vhdl_selected_waveform = {
  selectedwaveformvalue: vhdl_waveform;
  selectedwaveformchoices: vhdl_choices
  }
and vhdl_concurrent_conditional_signal_assignment = {
  concurrentconditionalsignaltarget: vhdl_target;
  concurrentconditionalsignalguarded: bool;
  concurrentconditionalsignaldelay: vhdl_delay_mechanism;
  concurrentconditionalsignalwaveforms: vhdl_conditional_waveforms
  }
and vhdl_conditional_waveforms =
  vhdl_conditional_waveform list
and vhdl_conditional_waveform = {
  conditionalwaveformvalue: vhdl_waveform;
  conditionalwaveformcondition: vhdl_condition
  }
and vhdl_concurrent_simple_signal_assignment = {
  concurrentsimplesignaltarget: vhdl_target;
  concurrentsimplesignalguarded: bool;
  concurrentsimplesignaldelay: vhdl_delay_mechanism;
  concurrentsimplesignalwaveform: vhdl_waveform
  }
and vhdl_concurrent_signal_assignment =
  | ConcurrentSimpleSignalAssignment of vhdl_concurrent_simple_signal_assignment
  | ConcurrentConditionalSignalAssignment of vhdl_concurrent_conditional_signal_assignment
  | ConcurrentSelectedSignalAssignment of vhdl_concurrent_selected_signal_assignment
and vhdl_concurrent_signal_assignment_statement = {
  concurrentsignallabelname: vhdl_label;
  concurrentsignalpostponed: bool;
  concurrentsignalassignment: vhdl_concurrent_signal_assignment;
  }

(* component_instantiation_statement ::=            *)
(*   instantiation_label:                           *)
(*     instantiated_unit                            *)
(*     [ generic_map_aspect ]                       *)
(*     [ port_map_aspect ] ;                        *)
(* instantiated_unit ::=                            *)
(*     [component] component_name                   *)
(*   | entity entity_name [( architecture_name )]   *)
(*   | configuration configuration_name             *)

and vhdl_instantiated_unit =
  | InstantiatedComponent of vhdl_name
  | InstantiatedEntityArchitecture of (vhdl_name * vhdl_identifier)
  | InstantiatedConfiguration of vhdl_name
and vhdl_component_instantiation_statement = {
  componentlabelname: vhdl_label;
  componentunit: vhdl_instantiated_unit;
  componentgenericmap: vhdl_generic_map_aspect;
  componentportmap: vhdl_port_map_aspect
  }

(* generate_statement ::=                                  *)
(*    for_generate_statement                               *)
(*  | if_generate_statement                                *)
(*  | case_generate_statement                              *)
(* generate_statement_body ::=                             *)
(*   [ block_declarative_part                              *)
(*   begin ]                                               *)
(*     { concurrent_statement }                            *)
(*   [ end [ alternative_label ] ; ]                       *)
and vhdl_generate_statement =
  | IfGenerateStatement of vhdl_if_generate_statement
  | ForGenerateStatement of vhdl_for_generate_statement
  | CaseGenerateStatement of vhdl_case_generate_statement

(* if_generate_statement ::=                               *)
(*   generate_label :                                      *)
(*     if [ alternative_label : ] condition generate       *)
(*       generate_statement_body                           *)
(*     { elsif [ alternative_label : ] condition generate  *)
(*       generate_statement_body }                         *)
(*     [ else [ alternative_label : ] generate             *)
(*       generate_statement_body ]                         *)
(*   end generate [ generate_label ] ;                     *)
and vhdl_if_generate_statement = {
  ifgeneratelabelname: vhdl_label;
  ifgeneratealternatelabelname: vhdl_label;
  ifgeneratecondition: vhdl_condition;
  ifgeneratedeclarations: vhdl_block_declarative_part;
  ifgeneratethenstatements: vhdl_concurrent_statement list;
  ifgenerateelsestatements: vhdl_else_generate_statement
  }
and vhdl_else_generate_statement =
  | GenerateElseNone
  | GenerateElse of vhdl_if_generate_statement
  | GenerateElsif of vhdl_if_generate_statement

(* for_generate_statement ::=                              *)
(*   generate_label :                                      *)
(*     for generate_parameter_specification generate       *)
(*       generate_statement_body                           *)
(*   end generate [ generate_label ] ;                     *)
and vhdl_for_generate_statement = {
  forgeneratelabelname: vhdl_label;
  forgeneratealternatelabelname: vhdl_label;
  forgenerateparameter: vhdl_parameter_specification;
  forgeneratedeclarations: vhdl_block_declarative_part;
  forgeneratestatements: vhdl_concurrent_statement list
  }

(* case_generate_statement ::=               *)
(*   generate_label :                        *)
(*     case expression generate              *)
(*       case_generate_alternative           *)
(*       { case_generate_alternative }       *)
(*   end generate [ generate_label ] ;       *)
and vhdl_case_generate_statement = {
  casegeneratelabelname: vhdl_label;
  casegenerateselector: vhdl_selector;
  casegeneratekind: vhdl_selection_kind;
  casegeneratealternatives: vhdl_case_generate_alternative list;
  }

(* case_generate_alternative ::=             *)
(*   when [ alternative_label : ] choices => *)
(*     generate_statement_body               *)
and vhdl_case_generate_alternative = {
  casegeneratealternatelabelname: vhdl_label;
  casegeneratechoices: vhdl_choices;
  casegeneratedeclarations: vhdl_block_declarative_part;
  casegeneratestatements: vhdl_concurrent_statement list;
  }


(* use_clause ::=                          *)
(*   use selected_name {, selected_name} ; *)
and vhdl_use_clause = vhdl_selected_name_list

(* concurrent_statement ::=                              *)
(*     block_statement                                   *)
(*   | process_statement                                 *)
(*   | concurrent_procedure_call_statement               *)
(*   | concurrent_assertion_statement (ieee syn ignored) *)
(*   | concurrent_signal_assignment_statement            *)
(*   | component_instantiation_statement                 *)
(*   | generate_statement                                *)

and vhdl_concurrent_statement =
  | ConcurrentBlockStatement of vhdl_block_statement
  | ConcurrentProcessStatement of vhdl_process_statement
  | ConcurrentProcedureCallStatement of vhdl_concurrent_procedure_call_statement
  | ConcurrentAssertionStatement of vhdl_concurrent_assertion_statement
  | ConcurrentSignalAssignmentStatement of vhdl_concurrent_signal_assignment_statement
  | ConcurrentComponentInstantiationStatement of vhdl_component_instantiation_statement
  | ConcurrentGenerateStatement of vhdl_generate_statement

and vhdl_concurrent_statements = vhdl_concurrent_statement list

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
(* package_declarative_part ::=                             *)
(*  { package_declarative_item }                            *)
(* package_declaration ::=                                  *)
(*  package identifier is                                   *)
(*    package_header                                        *)
(*    package_declarative_part                              *)
(*  end [ package ] [ package_simple_name ];                *)
(* package_header ::=                                       *)
(*   [ generic_clause                                       *)
(*   [ generic_map_aspect ; ] ]                             *)

and vhdl_package_declarative_item =
  | PackageSubProgramDeclaration of vhdl_subprogram_declaration
  | PackageSubProgramInstantiation of vhdl_subprogram_instantiation
  | PackageTypeDeclaration of vhdl_type_declaration
  | PackageSubTypeDeclaration of vhdl_subtype_declaration
  | PackageConstantDeclaration of vhdl_constant_declaration
  | PackageSignalDeclaration of vhdl_signal_declaration
  | PackageFileDeclaration of vhdl_file_declaration
  | PackageAliasDeclaration of vhdl_alias_declaration
  | PackageComponentDeclaration of vhdl_component_declaration
  | PackageAttributeDeclaration of vhdl_attribute_declaration
  | PackageAttributeSpecification of vhdl_attribute_specification
  | PackageUseClause of vhdl_use_clause

and vhdl_package_declarative_part = vhdl_package_declarative_item list

and vhdl_package_declaration = {
  packagename: vhdl_identifier;
  packagegenericclause: vhdl_generic_clause;
  packagegenericmapaspect: vhdl_generic_map_aspect;
  packagedeclarations: vhdl_package_declarative_part
  }

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
(* package_body_declarative_part ::=                        *)
(*   { package_body_declarative_item }                      *)
(* package_body ::=                                         *)
(*   package body package_simple_name is                    *)
(*     package_body_declarative_part                        *)
(*   end [ package body ] [ package_simple_name ] ;         *)

and vhdl_package_body_declarative_item =
  | PackageBodySubProgramDeclaration of vhdl_subprogram_declaration
  | PackageBodySubProgramBody of vhdl_subprogram_body
  | PackageBodySubProgramInstantiation of vhdl_subprogram_instantiation
  | PackageBodyTypeDeclaration of vhdl_type_declaration
  | PackageBodySubTypeDeclaration of vhdl_subtype_declaration
  | PackageBodyConstantDeclaration of vhdl_constant_declaration
  | PackageBodyFileDeclaration of vhdl_file_declaration
  | PackageBodyAliasDeclaration of vhdl_alias_declaration
  | PackageBodyUseClause of vhdl_use_clause

and vhdl_package_body_declarative_part = vhdl_package_body_declarative_item list

and vhdl_package_body = {
  packagebodyname: vhdl_simple_name;
  packagebodydeclarations: vhdl_package_body_declarative_part
  }

(* package_instantiation_declaration ::=                   *)
(*   package identifier is new uninstantiated_package_name *)
(*   [ generic_map_aspect ] ;                              *)

and vhdl_package_instantiation = {
  packageinstantiationidentifier: vhdl_identifier;
  packageinstantiationname: vhdl_name;
  packageinstantiationgenericmap: vhdl_generic_map_aspect
  }

(* entity_header ::=              *)
(*   [ formal_generic_clause ]    *)
(*   [ formal_port_clause ]       *)

and vhdl_entity_header = {
  entitygenerics: vhdl_generic_clause;
  entityports: vhdl_port_clause
  }

(* entity_declarative_part ::=                              *)
(*   { entity_declarative_item }                            *)
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

and vhdl_entity_declarative_item =
  | EntitySubProgramDeclaration of vhdl_subprogram_declaration
  | EntitySubProgramBody of vhdl_subprogram_body
  | EntitySubProgramInstantiation of vhdl_subprogram_instantiation
  | EntityTypeDeclaration of vhdl_type_declaration
  | EntitySubTypeDeclaration of vhdl_subtype_declaration
  | EntityConstantDeclaration of vhdl_constant_declaration
  | EntitySignalDeclaration of vhdl_signal_declaration
  | EntityFileDeclaration of vhdl_file_declaration
  | EntityAliasDeclaration of vhdl_alias_declaration
  | EntityAttributeDeclaration of vhdl_attribute_declaration
  | EntityAttributeSpecification of vhdl_attribute_specification
  | EntityUseClause of vhdl_use_clause

and vhdl_entity_declarative_part = vhdl_entity_declarative_item list

(* entity_statement_part ::=              *)
(*   { entity_statement }                 *)
(* entity_statement ::=                   *)
(*     concurrent_assertion_statement     *)
(*   | passive_concurrent_procedure_call  *)
(*   | passive_process_statement          *)

and vhdl_entity_statement =
  | EntityConcurrentAssertionStatement of vhdl_concurrent_assertion_statement
  | EntityConcurrentProcedureCallStatement of vhdl_concurrent_procedure_call_statement
  | EntityProcessStatement of vhdl_process_statement

and vhdl_entity_statement_part = vhdl_entity_statement list

(* entity_declaration ::=                           *)
(*   entity identifier is                           *)
(*     entity_header                                *)
(*     entity_declarative_part                      *)
(*   [ begin                                        *)
(*       entity_statement_part (ieee syn ignored) ] *)
(*     end [ entity ] [ entity_simple_name ] ;      *)

and vhdl_entity_declaration = {
  entityname: vhdl_identifier;
  entityheader: vhdl_entity_header;
  entitydeclarations: vhdl_entity_declarative_part;
  entitystatements: vhdl_entity_statement_part
  }

(* architecture_declarative_part ::=              *)
(*   { block_declarative_item }                   *)

and vhdl_architecture_declarative_part = vhdl_block_declarative_item list

(* architecture_statement_part ::=              *)
(*  { concurrent_statement }                    *)

and vhdl_architecture_statement_part = vhdl_concurrent_statement list

(* architecture_body ::=                                 *)
(*   architecture identifier of entity_name is           *)
(*     architecture_declarative_part                     *)
(*   begin                                               *)
(*     architecture_statement_part                       *)
(*   end [ architecture ] [ architecture_simple_name ] ; *)

and vhdl_architecture_body = {
  archname: vhdl_identifier;
  archentityname: vhdl_simple_name;
  archdeclarations: vhdl_architecture_declarative_part;
  archstatements: vhdl_architecture_statement_part
  }

(* context_declaration ::=                       *)
(*   context identifier is                       *)
(*     context_clause                            *)
(*   end [ context ] [ context_simple_name ] ;   *)

and vhdl_context_declaration = {
  contextname: vhdl_identifier;
  contextclause: vhdl_context_clause
  }

(* context_item ::=        *)
(*     library_clause      *)
(*   | use_clause          *)
(*   | context_reference   *)

and vhdl_context_item =
  | ContextLibraryClause of vhdl_library_clause
  | ContextUseClause of vhdl_use_clause
  | ContextContextReference of vhdl_context_reference

(* context_clause ::= { context_item } *)

and vhdl_context_clause = vhdl_context_item list

(* context_reference ::=                         *)
(*   context selected_name { , selected_name } ; *)

and vhdl_context_reference = vhdl_selected_name_list

(* configuration_declaration ::=                      *)
(*   configuration identifier of entity_name is       *)
(*     configuration_declarative_part                 *)
(*     block_configuration                            *)
(*   end [configuration] [configuration_simple_name]; *)
(* configuration_declarative_part ::=                 *)
(*   { configuration_declarative_item }               *)
(* configuration_declarative_item ::=                 *)
(*     use_clause                                     *)
(*   | attribute_specification                        *)
(*   | group_declaration (ieee syn not supported)     *)

and vhdl_configuration_declaration = {
  confname: vhdl_identifier;
  confentityname: vhdl_name;
  confdeclarations: vhdl_configuration_declarative_part;
  confblock: vhdl_block_configuration
  }
and vhdl_configuration_declarative_part = vhdl_configuration_declarative_item list
and vhdl_configuration_declarative_item =
  | ConfigurationUseClause of vhdl_use_clause
  | ConfigurationAttributeSpecification of vhdl_attribute_specification

(* block_configuration ::=                                  *)
(*   for block_specification                                *)
(*     { use_clause }                                       *)
(*     { configuration_item }                               *)
(*   end for ;                                              *)
(* block_specification ::=                                  *)
(*     architecture_name                                    *)
(*   | block_statement_label                                *)
(*   | generate_statement_label [ ( index_specification ) ] *)
(* index_specification ::=                                  *)
(*     discrete_range                                       *)
(*   | static_expression                                    *)
(* configuration_item ::=                                   *)
(*     block_configuration                                  *)
(*   | component_configuration                              *)

and vhdl_block_configuration = {
  blockspec: vhdl_block_specification;
  blockuses: vhdl_use_clause list;
  blockconfigurations: vhdl_configuration_item list
  }
and vhdl_block_specification =
  | BlockSpecificationEmpty
  | BlockSpecificationName of vhdl_name
  | BlockSpecificationNameIndex of (vhdl_name * vhdl_index_specification)
and vhdl_index_specification =
  | IndexDiscreteRange of vhdl_discrete_range
  | IndexStaticExpression of vhdl_expression
and vhdl_configuration_item =
  | BlockConfiguration of vhdl_block_configuration
  | ComponentConfiguration of vhdl_component_configuration

(* component_configuration ::=              *)
(*   for component_specification            *)
(*     [ binding_indication ; ]             *)
(*     [ block_configuration ]              *)
(*   end for ;                              *)

and vhdl_component_configuration = {
  componentspec: vhdl_component_specification;
  componentbinding: vhdl_binding_indication;
  componentblockconf: vhdl_block_configuration
  }

(* design_file ::= design_unit { design_unit }   *)
(* design_unit ::= context_clause library_unit   *)
(* library_unit ::=                              *)
(*     primary_unit                              *)
(*   | secondary_unit                            *)
(* primary_unit ::=                              *)
(*     entity_declaration                        *)
(*   | configuration_declaration                 *)
(*   | package_declaration                       *)
(*   | package_instantiation_declaration (TODO)  *)
(*   | context_declaration                       *)
(*   | PSL_Verification_Unit (TODO)              *)
(* secondary_unit ::=                            *)
(*     architecture_body                         *)
(*   | package_body                              *)

and vhdl_secondary_unit =
  | ArchitectureBody of vhdl_architecture_body
  | PackageBody of vhdl_package_body

and vhdl_primary_unit =
  | EntityDeclaration of vhdl_entity_declaration
  | ConfigurationDeclaration of vhdl_configuration_declaration
  | PackageDeclaration of vhdl_package_declaration
  | PackageInstantiation of vhdl_package_instantiation
  | ContextDeclaration of vhdl_context_declaration

and vhdl_library_unit =
  | PrimaryUnit of vhdl_primary_unit
  | SecondaryUnit of vhdl_secondary_unit
  | ProtectedUnit

and vhdl_design_unit = {
  designunitcontextclause: vhdl_context_clause;
  designunitlibraryunit: vhdl_library_unit
  }

and vhdl_design_file = vhdl_design_unit list

and vhdl_parsed_file = {
  parsedfilename: string;
  parsedfilelibrary: string;
  parsedfileunits: vhdl_design_file
  }
and vhdl_parsed_file_list = vhdl_parsed_file list;;

(* simple constants *)
let empty_identifier = ("",-1);;
let empty_label = empty_identifier;;
let empty_simple_name = empty_identifier;;
let empty_name = SimpleName empty_simple_name;;
let empty_type_mark = empty_name;;
let empty_expression = AtomExpression (AtomLogicalExpression (AtomRelation (AtomShiftExpression (AtomSimpleExpression (AtomTerm (AtomFactor (AtomDotted (NamePrimary empty_name))))))));;
let empty_condition = Condition empty_expression;;
let empty_subtype_indication = {resolutionfunction=empty_name; basetypename=empty_type_mark; subtypeconstraint=NoConstraint};;
let empty_binding = {bindingentity=BindingOpen; bindinggenericmap=[]; bindingportmap=[]};;
let empty_blockconf = {blockspec=BlockSpecificationEmpty; blockuses=[]; blockconfigurations=[]};;
