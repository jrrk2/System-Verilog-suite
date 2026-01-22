(*
    <vscr - Verilog converter to abc format.>
    Copyright (C) <2011,2012>  <Jonathan Richard Robert Kimmitt>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)

open VhdlTypes

type vhdintf = 
  | VhdNone
  | Str of string
  | Char of char
  | Real of float
  | Num of string
  | List of vhdintf list
  | Double of vhdintf * vhdintf
  | Triple of vhdintf * vhdintf * vhdintf
  | Quadruple of vhdintf * vhdintf * vhdintf * vhdintf
  | Quintuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Sextuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Septuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Octuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Nonuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Decuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Undecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Duodecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Tredecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Quattuordecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Quindecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Sexdecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Septendecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Octodecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Novemdecuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Vigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Unvigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Duovigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Trevigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Quattuorvigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | Quinvigenuple of vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf * vhdintf
  | VhdAbsFactor
  | VhdAccessTypeDefinition
  | VhdActualDiscreteRange
  | VhdActualExpression
  | VhdActualOpen
  | VhdAddSimpleExpression
  | VhdAggregatePrimary
  | VhdAlwaysLoop
  | VhdAndFactor
  | VhdAndLogicalExpression
  | VhdArchitectureBody
  | VhdArrayConstraint
  | VhdArrayTypeDefinition
  | VhdAttrNameRange
  | VhdAttributeName
  | VhdBindingConfiguration
  | VhdBindingEntity
  | VhdBindingEntityArchitecture
  | VhdBindingOpen
  | VhdBlockAliasDeclaration
  | VhdBlockAttributeDeclaration
  | VhdBlockAttributeSpecification
  | VhdBlockComponentDeclaration
  | VhdBlockConfiguration
  | VhdBlockConfigurationSpecification
  | VhdBlockConstantDeclaration
  | VhdBlockFileDeclaration
  | VhdBlockSignalDeclaration
  | VhdBlockSpecificationEmpty
  | VhdBlockSpecificationName
  | VhdBlockSpecificationNameIndex
  | VhdBlockSubProgramBody
  | VhdBlockSubProgramDeclaration
  | VhdBlockSubProgramInstantiation
  | VhdBlockSubTypeDeclaration
  | VhdBlockTypeDeclaration
  | VhdBlockUseClause
  | VhdCaseGenerateStatement
  | VhdCharLiteralEnumeration
  | VhdCharPrimary
  | VhdChoiceDiscreteRange
  | VhdChoiceOthers
  | VhdChoiceSimpleExpression
  | VhdClassArchitecture
  | VhdClassComponent
  | VhdClassConfiguration
  | VhdClassConstant
  | VhdClassEntity
  | VhdClassFile
  | VhdClassFunction
  | VhdClassLabel
  | VhdClassLiteral
  | VhdClassPackage
  | VhdClassProcedure
  | VhdClassSignal
  | VhdClassSubType
  | VhdClassType
  | VhdClassUnits
  | VhdClassVariable
  | VhdComponentConfiguration
  | VhdConcatSimpleExpression
  | VhdConcurrentAssertionStatement
  | VhdConcurrentBlockStatement
  | VhdConcurrentComponentInstantiationStatement
  | VhdConcurrentConditionalSignalAssignment
  | VhdConcurrentGenerateStatement
  | VhdConcurrentProcedureCallStatement
  | VhdConcurrentProcessStatement
  | VhdConcurrentSelectedSignalAssignment
  | VhdConcurrentSignalAssignmentStatement
  | VhdConcurrentSimpleSignalAssignment
  | VhdCondition
  | VhdConditionExpression
  | VhdConditionalSignalAssignment
  | VhdConditionalVariableAssignment
  | VhdConfigurationAttributeSpecification
  | VhdConfigurationDeclaration
  | VhdConfigurationUseClause
  | VhdConstantDeclaration
  | VhdConstrainedArray
  | VhdContextContextReference
  | VhdContextDeclaration
  | VhdContextLibraryClause
  | VhdContextUseClause
  | VhdDecreasingRange
  | VhdDelayInertial
  | VhdDelayNone
  | VhdDelayTransport
  | VhdDesignatorCharacter
  | VhdDesignatorIdentifier
  | VhdDesignatorOperator
  | VhdDivTerm
  | VhdElse
  | VhdElseNone
  | VhdElsif
  | VhdEntityAliasDeclaration
  | VhdEntityAll
  | VhdEntityAttributeDeclaration
  | VhdEntityAttributeSpecification
  | VhdEntityConcurrentAssertionStatement
  | VhdEntityConcurrentProcedureCallStatement
  | VhdEntityConstantDeclaration
  | VhdEntityDeclaration
  | VhdEntityFileDeclaration
  | VhdEntityOthers
  | VhdEntityProcessStatement
  | VhdEntitySignalDeclaration
  | VhdEntitySubProgramBody
  | VhdEntitySubProgramDeclaration
  | VhdEntitySubProgramInstantiation
  | VhdEntitySubTypeDeclaration
  | VhdEntityTagCharacterLiteral
  | VhdEntityTagOperatorSymbol
  | VhdEntityTagSimpleName
  | VhdEntityTypeDeclaration
  | VhdEntityUseClause
  | VhdEnumerationTypeDefinition
  | VhdEqualRelation
  | VhdExpFactor
  | VhdFileDeclaration
  | VhdFileTypeDefinition
  | VhdFloatPrimary
  | VhdForGenerateStatement
  | VhdForLoop
  | VhdFormalExpression
  | VhdFormalIndexed
  | VhdFullType
  | VhdFunction
  | VhdFunctionSpecification
  | VhdGenerateElse
  | VhdGenerateElseNone
  | VhdGenerateElsif
  | VhdGreaterOrEqualRelation
  | VhdGreaterRelation
  | VhdIdentifierEnumeration
  | VhdIfGenerateStatement
  | VhdImpure
  | VhdIncompleteType
  | VhdIncreasingRange
  | VhdIndexDiscreteRange
  | VhdIndexStaticExpression
  | VhdInstantiatedComponent
  | VhdInstantiatedConfiguration
  | VhdInstantiatedEntityArchitecture
  | VhdInstantiationAll
  | VhdInstantiationOthers
  | VhdIntPrimary
  | VhdInterfaceConstantDeclaration
  | VhdInterfaceDefaultDeclaration
  | VhdInterfaceFileDeclaration
  | VhdInterfaceIncompleteTypeDeclaration
  | VhdInterfaceModeBuffer
  | VhdInterfaceModeIn
  | VhdInterfaceModeInOut
  | VhdInterfaceModeOut
  | VhdInterfaceObjectDeclaration
  | VhdInterfaceSignalDeclaration
  | VhdInterfaceTypeDeclaration
  | VhdInterfaceVariableDeclaration
  | VhdLdotted
  | VhdLessOrEqualRelation
  | VhdLessRelation
  | VhdMatchingEqualRelation
  | VhdMatchingGreaterOrEqualRelation
  | VhdMatchingGreaterRelation
  | VhdMatchingLessOrEqualRelation
  | VhdMatchingLessRelation
  | VhdMatchingNotEqualRelation
  | VhdMatchingSelection
  | VhdModTerm
  | VhdMultTerm
  | VhdNameParametersPrimary
  | VhdNamePrimary
  | VhdNandFactor
  | VhdNandLogicalExpression
  | VhdNegSimpleExpression
  | VhdNewFactor
  | VhdNoConstraint
  | VhdNorFactor
  | VhdNorLogicalExpression
  | VhdNotEqualRelation
  | VhdNotFactor
  | VhdOperatorString
  | VhdOrFactor
  | VhdOrLogicalExpression
  | VhdOrdinarySelection
  | VhdPackageAliasDeclaration
  | VhdPackageAttributeDeclaration
  | VhdPackageAttributeSpecification
  | VhdPackageBody
  | VhdPackageBodyAliasDeclaration
  | VhdPackageBodyConstantDeclaration
  | VhdPackageBodyFileDeclaration
  | VhdPackageBodySubProgramBody
  | VhdPackageBodySubProgramDeclaration
  | VhdPackageBodySubProgramInstantiation
  | VhdPackageBodySubTypeDeclaration
  | VhdPackageBodyTypeDeclaration
  | VhdPackageBodyUseClause
  | VhdPackageComponentDeclaration
  | VhdPackageConstantDeclaration
  | VhdPackageDeclaration
  | VhdPackageFileDeclaration
  | VhdPackageInstantiation
  | VhdPackageSignalDeclaration
  | VhdPackageSubProgramDeclaration
  | VhdPackageSubProgramInstantiation
  | VhdPackageSubTypeDeclaration
  | VhdPackageTypeDeclaration
  | VhdPackageUseClause
  | VhdParenthesedPrimary
  | VhdPhysicalEmpty
  | VhdPhysicalFloat
  | VhdPhysicalInteger
  | VhdPhysicalPrimary
  | VhdPhysicalTypeDefinition
  | VhdPrefixName
  | VhdPrimaryUnit
  | VhdProcedure
  | VhdProcedureSpecification
  | VhdProcessAliasDeclaration
  | VhdProcessAttributeDeclaration
  | VhdProcessAttributeSpecification
  | VhdProcessConstantDeclaration
  | VhdProcessFileDeclaration
  | VhdProcessSubProgramBody
  | VhdProcessSubProgramDeclaration
  | VhdProcessSubProgramInstantiation
  | VhdProcessSubTypeDeclaration
  | VhdProcessTypeDeclaration
  | VhdProcessUseClause
  | VhdProcessVariableDeclaration
  | VhdProtectedUnit
  | VhdPure
  | VhdQualifiedAggregate
  | VhdQualifiedExpression
  | VhdQualifiedExpressionPrimary
  | VhdRange
  | VhdRangeConstraint
  | VhdRangeTypeDefinition
  | VhdRecordTypeDefinition
  | VhdRemTerm
  | VhdRotateLeftExpression
  | VhdRotateRightExpression
  | VhdSecondaryUnit
  | VhdSelectTargetName
  | VhdSelectTargetNameParameters
  | VhdSelectedName
  | VhdSelectedSignalAssignment
  | VhdSelectedVariableAssignment
  | VhdSelector
  | VhdSensitivityAll
  | VhdSensitivityExpressionList
  | VhdSequentialAssertion
  | VhdSequentialCase
  | VhdSequentialExit
  | VhdSequentialIf
  | VhdSequentialLoop
  | VhdSequentialNext
  | VhdSequentialNull
  | VhdSequentialProcedureCall
  | VhdSequentialReport
  | VhdSequentialReturn
  | VhdSequentialSignalAssignment
  | VhdSequentialVariableAssignment
  | VhdSequentialWait
  | VhdShiftLeftArithmeticExpression
  | VhdShiftLeftLogicalExpression
  | VhdShiftRightArithmeticExpression
  | VhdShiftRightLogicalExpression
  | VhdSignalDeclaration
  | VhdSignalKindBus
  | VhdSignalKindDefault
  | VhdSignalKindRegister
  | VhdSimpleName
  | VhdSimpleSignalAssignment
  | VhdSimpleVariableAssignment
  | VhdSubProgramAliasDeclaration
  | VhdSubProgramAttributeDeclaration
  | VhdSubProgramAttributeSpecification
  | VhdSubProgramBody
  | VhdSubProgramConstantDeclaration
  | VhdSubProgramDeclaration
  | VhdSubProgramFileDeclaration
  | VhdSubProgramInstantiation
  | VhdSubProgramSubTypeDeclaration
  | VhdSubProgramTypeDeclaration
  | VhdSubProgramUseClause
  | VhdSubProgramVariableDeclaration
  | VhdSubSimpleExpression
  | VhdSubTypeRange
  | VhdSubscriptName
  | VhdSuffixAll
  | VhdSuffixCharLiteral
  | VhdSuffixOpSymbol
  | VhdSuffixSimpleName
  | VhdTargetAggregate
  | VhdTargetInvalid
  | VhdTargetName
  | VhdTargetDotted
  | VhdTargetNameParameters
  | VhdUnaffected
  | VhdUnboundedArray
  | VhdUnknown
  | VhdVariableDeclaration
  | VhdWhileLoop
  | VhdXnorFactor
  | VhdXnorLogicalExpression
  | VhdXorFactor
  | VhdXorLogicalExpression
  | Vhdalias_declaration
  | Vhdarchitecture_body
  | Vhdassertion
  | Vhdassertion_statement
  | Vhdassociation_element
  | Vhdattribute_declaration
  | Vhdattribute_name
  | Vhdattribute_specification
  | Vhdbinding_indication
  | Vhdblock_configuration
  | Vhdblock_statement
  | Vhdcase_generate_alternative
  | Vhdcase_generate_statement
  | Vhdcase_statement
  | Vhdcase_statement_alternative
  | Vhdcomponent_configuration
  | Vhdcomponent_declaration
  | Vhdcomponent_instantiation_statement
  | Vhdcomponent_specification
  | Vhdconcurrent_assertion_statement
  | Vhdconcurrent_conditional_signal_assignment
  | Vhdconcurrent_procedure_call_statement
  | Vhdconcurrent_selected_signal_assignment
  | Vhdconcurrent_signal_assignment_statement
  | Vhdconcurrent_simple_signal_assignment
  | Vhdconditional_expression
  | Vhdconditional_signal_assignment_statement
  | Vhdconditional_variable_assignment
  | Vhdconditional_waveform
  | Vhdconfiguration_declaration
  | Vhdconfiguration_specification
  | Vhdconstant_declaration
  | Vhdconstrained_array_definition
  | Vhdcontext_declaration
  | Vhddesign_unit
  | Vhdelement_association
  | Vhdelement_declaration
  | Vhdentity_declaration
  | Vhdentity_designator
  | Vhdentity_header
  | Vhdentity_specification
  | Vhdexit_statement
  | Vhdfile_declaration
  | Vhdfor_generate_statement
  | Vhdfull_type_declaration
  | Vhdfunction_specification
  | Vhdif_generate_statement
  | Vhdif_statement
  | Vhdincomplete_type_declaration
  | Vhdinterface_constant_declaration
  | Vhdinterface_default_declaration
  | Vhdinterface_file_declaration
  | Vhdinterface_signal_declaration
  | Vhdinterface_variable_declaration
  | Vhdloop_statement
  | Vhdnext_statement
  | Vhdnull_statement
  | Vhdpackage_body
  | Vhdpackage_declaration
  | Vhdpackage_instantiation
  | Vhdparameter_specification
  | Vhdparsed_file
  | Vhdphysical_type_definition
  | Vhdprocedure_call
  | Vhdprocedure_call_statement
  | Vhdprocedure_specification
  | Vhdprocess_statement
  | Vhdrecord_type_definition
  | Vhdreport_statement
  | Vhdreturn_statement
  | Vhdselected_expression
  | Vhdselected_signal_assignment_statement
  | Vhdselected_variable_assignment
  | Vhdselected_waveform
  | Vhdsignal_declaration
  | Vhdsignature
  | Vhdsimple_signal_assignment_statement
  | Vhdsimple_variable_assignment
  | Vhdsubprogram_body
  | Vhdsubprogram_instantiation
  | Vhdsubtype_declaration
  | Vhdsubtype_indication
  | Vhdunbounded_array_definition
  | Vhdvariable_declaration
  | Vhdwait_statement
  | Vhdwaveform_element


let rec dump_vhd_int x = Str (string_of_int x)
        and dump_vhd_bool x = Str (string_of_bool x)
        and dump_vhd_string x = Str x
(*0*)   and dump_identifier (str,_) = Str str
(*1*)   and dump_string (str,_) = Str str
(*2*)   and dump_int (n,_) = Num (Big_int.string_of_big_int n)
(*3*)   and dump_float (f,_) = Real f
(*4*)   and dump_char (ch,_) = Char ch
(*5*)   and dump_identifier_list x = List (List.map dump_identifier x)
(*6*)   and dump_label x = dump_identifier x
(*7*)   and dump_simple_name x = dump_identifier x
(*8*)   and dump_attribute_designator x = dump_simple_name x
(*9*)   and dump_suffix = function
  | SuffixSimpleName name -> Double(VhdSuffixSimpleName,
	dump_name name)
  | SuffixCharLiteral char -> Double(VhdSuffixCharLiteral,
	dump_char char)
  | SuffixOpSymbol string -> Double(VhdSuffixOpSymbol,
	dump_string string)
  | SuffixAll -> VhdSuffixAll
(*10*)   and dump_prefix = function
  | PrefixName name -> Double(VhdPrefixName,
	dump_name name)
(*11*)   and dump_selected_name x = List (List.map dump_suffix x)
(*12*)   and dump_selected_name_list x = List (List.map dump_selected_name x)
(*13*)   and dump_name = function
  | SimpleName simple_name -> Double(VhdSimpleName,
	dump_simple_name simple_name)
  | OperatorString string -> Double(VhdOperatorString,
	dump_string string)
  | SelectedName selected_name -> Double(VhdSelectedName,
	dump_selected_name selected_name)
  | AttributeName attribute_name -> Double(VhdAttributeName,
	dump_attribute_name attribute_name)
  | SubscriptName (simple_name0,selector1) -> Triple(VhdSubscriptName, dump_simple_name simple_name0
	, dump_selector selector1
	)
(*14*)   and dump_name_list x = List (List.map dump_name x)
(*15*)   and dump_type_mark x = dump_name x
(*16*)   and dump_type_mark_list x = List (List.map dump_type_mark x)
(*17*)   and dump_signature {
  signatureparametertypes=type_mark_list0;
  signaturereturntype=type_mark1;
} = Triple(Vhdsignature,
	dump_type_mark_list type_mark_list0,
	dump_type_mark type_mark1)
(*18*)   and dump_logical_name x = dump_identifier x
(*19*)   and dump_logical_name_list x = List (List.map dump_logical_name x)
(*20*)   and dump_library_clause x = dump_logical_name_list x
(*21*)   and dump_attribute_name {
  attributeprefix=suffix0;
  attribute=attribute_designator1;
} = Triple(Vhdattribute_name,
	dump_suffix suffix0,
	dump_attribute_designator attribute_designator1)
(*22*)   and dump_designator = function
  | DesignatorIdentifier identifier -> Double(VhdDesignatorIdentifier,
	dump_identifier identifier)
  | DesignatorOperator string -> Double(VhdDesignatorOperator,
	dump_string string)
  | DesignatorCharacter char -> Double(VhdDesignatorCharacter,
	dump_char char)
(*23*)   and dump_condition = function
  | Condition expression -> Double(VhdCondition,
	dump_expression expression)
(*24*)   and dump_selector = function
  | Selector expression -> Double(VhdSelector,
	dump_expression expression)
(*25*)   and dump_selection_kind = function
  | OrdinarySelection -> VhdOrdinarySelection
  | MatchingSelection -> VhdMatchingSelection
(*26*)   and dump_expression = function
  | AtomExpression logical_expression -> dump_logical_expression logical_expression
  | ConditionExpression primary -> Double(VhdConditionExpression,
	dump_primary primary)
(*27*)   and dump_expression_list x = List (List.map dump_expression x)
(*28*)   and dump_logical_expression = function
  | AtomLogicalExpression relation -> dump_relation relation
  | AndLogicalExpression (relation0,relation1) -> Triple(VhdAndLogicalExpression, dump_relation relation0
	, dump_relation relation1
	)
  | OrLogicalExpression (relation0,relation1) -> Triple(VhdOrLogicalExpression, dump_relation relation0
	, dump_relation relation1
	)
  | XorLogicalExpression (relation0,relation1) -> Triple(VhdXorLogicalExpression, dump_relation relation0
	, dump_relation relation1
	)
  | NandLogicalExpression (relation0,relation1) -> Triple(VhdNandLogicalExpression, dump_relation relation0
	, dump_relation relation1
	)
  | NorLogicalExpression (relation0,relation1) -> Triple(VhdNorLogicalExpression, dump_relation relation0
	, dump_relation relation1
	)
  | XnorLogicalExpression (relation0,relation1) -> Triple(VhdXnorLogicalExpression, dump_relation relation0
	, dump_relation relation1
	)
(*29*)   and dump_relation = function
  | AtomRelation shift_expression -> dump_shift_expression shift_expression
  | EqualRelation (shift_expression0,shift_expression1) -> Triple(VhdEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | NotEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdNotEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | LessRelation (shift_expression0,shift_expression1) -> Triple(VhdLessRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | LessOrEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdLessOrEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | GreaterRelation (shift_expression0,shift_expression1) -> Triple(VhdGreaterRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | GreaterOrEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdGreaterOrEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | MatchingEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdMatchingEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | MatchingNotEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdMatchingNotEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | MatchingLessRelation (shift_expression0,shift_expression1) -> Triple(VhdMatchingLessRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | MatchingLessOrEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdMatchingLessOrEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | MatchingGreaterRelation (shift_expression0,shift_expression1) -> Triple(VhdMatchingGreaterRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
  | MatchingGreaterOrEqualRelation (shift_expression0,shift_expression1) -> Triple(VhdMatchingGreaterOrEqualRelation, dump_shift_expression shift_expression0
	, dump_shift_expression shift_expression1
	)
(*30*)   and dump_relation_list x = List (List.map dump_relation x)
(*31*)   and dump_shift_expression = function
  | AtomShiftExpression simple_expression -> dump_simple_expression simple_expression
  | ShiftLeftLogicalExpression (simple_expression0,simple_expression1) -> Triple(VhdShiftLeftLogicalExpression, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
  | ShiftRightLogicalExpression (simple_expression0,simple_expression1) -> Triple(VhdShiftRightLogicalExpression, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
  | ShiftLeftArithmeticExpression (simple_expression0,simple_expression1) -> Triple(VhdShiftLeftArithmeticExpression, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
  | ShiftRightArithmeticExpression (simple_expression0,simple_expression1) -> Triple(VhdShiftRightArithmeticExpression, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
  | RotateLeftExpression (simple_expression0,simple_expression1) -> Triple(VhdRotateLeftExpression, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
  | RotateRightExpression (simple_expression0,simple_expression1) -> Triple(VhdRotateRightExpression, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
(*32*)   and dump_simple_expression = function
  | AtomSimpleExpression term -> dump_term term
  | AddSimpleExpression (term0,term1) -> Triple(VhdAddSimpleExpression, dump_term term0
	, dump_term term1
	)
  | SubSimpleExpression (term0,term1) -> Triple(VhdSubSimpleExpression, dump_term term0
	, dump_term term1
	)
  | NegSimpleExpression term -> Double(VhdNegSimpleExpression,
	dump_term term)
  | ConcatSimpleExpression term_list -> Double(VhdConcatSimpleExpression,
	dump_term_list term_list)
(*33*)   and dump_term = function
  | AtomTerm factor -> dump_factor factor
  | MultTerm (factor0,factor1) -> Triple(VhdMultTerm, dump_factor factor0
	, dump_factor factor1
	)
  | DivTerm (factor0,factor1) -> Triple(VhdDivTerm, dump_factor factor0
	, dump_factor factor1
	)
  | ModTerm (factor0,factor1) -> Triple(VhdModTerm, dump_factor factor0
	, dump_factor factor1
	)
  | RemTerm (factor0,factor1) -> Triple(VhdRemTerm, dump_factor factor0
	, dump_factor factor1
	)
(*34*)   and dump_term_list x = List (List.map dump_term x)
(*35*)   and dump_factor = function
  | AtomFactor dotted -> dump_dotted dotted
  | ExpFactor (dotted0,dotted1) -> Triple(VhdExpFactor, dump_dotted dotted0
	, dump_dotted dotted1
	)
  | AbsFactor dotted -> Double(VhdAbsFactor,
	dump_dotted dotted)
  | NotFactor dotted -> Double(VhdNotFactor,
	dump_dotted dotted)
  | AndFactor dotted -> Double(VhdAndFactor,
	dump_dotted dotted)
  | OrFactor dotted -> Double(VhdOrFactor,
	dump_dotted dotted)
  | XorFactor dotted -> Double(VhdXorFactor,
	dump_dotted dotted)
  | XnorFactor dotted -> Double(VhdXnorFactor,
	dump_dotted dotted)
  | NandFactor dotted -> Double(VhdNandFactor,
	dump_dotted dotted)
  | NorFactor dotted -> Double(VhdNorFactor,
	dump_dotted dotted)
  | NewFactor dotted -> Double(VhdNewFactor,
	dump_dotted dotted)
(*36*)   and dump_factor_list x = List (List.map dump_factor x)
(*37*)   and dump_dotted = function
  | AtomDotted primary -> dump_primary primary
  | Ldotted (primary0,primary1) -> Triple(VhdLdotted, dump_primary primary0
	, dump_primary primary1
	)
(*38*)   and dump_primary = function
  | VhdSequentialNull -> VhdSequentialNull
  | NamePrimary name -> Double(VhdNamePrimary,
	dump_name name)
  | NameParametersPrimary (name0,parameters1) -> Triple(VhdNameParametersPrimary, dump_name name0
	, dump_parameters parameters1
	)
  | IntPrimary int -> Double(VhdIntPrimary,
	dump_int int)
  | CharPrimary char -> Double(VhdCharPrimary,
	dump_char char)
  | FloatPrimary float -> Double(VhdFloatPrimary,
	dump_float float)
  | PhysicalPrimary physical_literal -> Double(VhdPhysicalPrimary,
	dump_physical_literal physical_literal)
  | AggregatePrimary aggregate -> Double(VhdAggregatePrimary,
	dump_aggregate aggregate)
  | QualifiedExpressionPrimary qualified_expression -> Double(VhdQualifiedExpressionPrimary,
	dump_qualified_expression qualified_expression)
  | ParenthesedPrimary expression -> Double(VhdParenthesedPrimary,
	dump_expression expression)
(*39*)   and dump_parameter x = dump_association_list x
(*40*)   and dump_parameters x = List (List.map dump_parameter x)
(*41*)   and dump_physical_literal = function
  | PhysicalEmpty vhd_int -> Double(VhdPhysicalEmpty,
	dump_vhd_int vhd_int)
  | PhysicalInteger (int0,identifier1) -> Triple(VhdPhysicalInteger, dump_int int0
	, dump_identifier identifier1
	)
  | PhysicalFloat (float0,identifier1) -> Triple(VhdPhysicalFloat, dump_float float0
	, dump_identifier identifier1
	)
(*42*)   and dump_aggregate x = dump_element_association_list x
(*43*)   and dump_element_association_list x = List (List.map dump_element_association x)
(*44*)   and dump_element_association {
  elemassocchoices=choices0;
  elemassocexpression=expression1;
} = Triple(Vhdelement_association,
	dump_choices choices0,
	dump_expression expression1)
(*45*)   and dump_choices x = List (List.map dump_choice x)
(*46*)   and dump_choice = function
  | ChoiceSimpleExpression simple_expression -> Double(VhdChoiceSimpleExpression,
	dump_simple_expression simple_expression)
  | ChoiceDiscreteRange discrete_range -> Double(VhdChoiceDiscreteRange,
	dump_discrete_range discrete_range)
  | ChoiceOthers -> VhdChoiceOthers
(*47*)   and dump_qualified_expression = function
  | QualifiedExpression (suffix0,expression1) -> Triple(VhdQualifiedExpression, dump_suffix suffix0
	, dump_expression expression1
	)
  | QualifiedAggregate (suffix0,aggregate1) -> Triple(VhdQualifiedAggregate, dump_suffix suffix0
	, dump_aggregate aggregate1
	)
(*48*)   and dump_range = function
  | AttrNameRange attribute_name -> Double(VhdAttrNameRange,
	dump_attribute_name attribute_name)
  | IncreasingRange (simple_expression0,simple_expression1) -> Triple(VhdIncreasingRange, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
  | DecreasingRange (simple_expression0,simple_expression1) -> Triple(VhdDecreasingRange, dump_simple_expression simple_expression0
	, dump_simple_expression simple_expression1
	)
(*49*)   and dump_range_constraint x = dump_range x
(*50*)   and dump_discrete_range = function
  | Range range -> Double(VhdRange,
	dump_range range)
  | SubTypeRange subtype_indication -> Double(VhdSubTypeRange,
	dump_subtype_indication subtype_indication)
(*51*)   and dump_discrete_range_list x = List (List.map dump_discrete_range x)
(*52*)   and dump_actual_part = function
  | ActualExpression expression -> Double(VhdActualExpression,
	dump_expression expression)
  | ActualDiscreteRange discrete_range -> Double(VhdActualDiscreteRange,
	dump_discrete_range discrete_range)
  | ActualOpen -> VhdActualOpen
(*53*)   and dump_formal_designator x = dump_name x
(*54*)   and dump_formal_part = function
  | FormalIndexed -> VhdFormalIndexed
  | FormalExpression expression -> Double(VhdFormalExpression,
	dump_expression expression)
(*55*)   and dump_association_element {
  formal=formal_part0;
  actual=actual_part1;
} = Triple(Vhdassociation_element,
	dump_formal_part formal_part0,
	dump_actual_part actual_part1)
(*56*)   and dump_association_list x = List (List.map dump_association_element x)
(*57*)   and dump_array_constraint x = dump_parameters x
(*58*)   and dump_array_element_constraint x = dump_parameter x
(*59*)   and dump_index_constraint x = dump_parameter x
(*60*)   and dump_constraint = function
  | RangeConstraint range_constraint -> Double(VhdRangeConstraint,
	dump_range_constraint range_constraint)
  | ArrayConstraint array_constraint -> Double(VhdArrayConstraint,
	dump_array_constraint array_constraint)
  | NoConstraint -> VhdNoConstraint
(*61*)   and dump_subtype_indication {
  resolutionfunction=name0;
  basetypename=type_mark1;
  subtypeconstraint=constraint2;
} = Quadruple(Vhdsubtype_indication,
	dump_name name0,
	dump_type_mark type_mark1,
	dump_constraint constraint2)
(*62*)   and dump_subtype_declaration {
  subtypename=identifier0;
  subtypeindication=subtype_indication1;
} = Triple(Vhdsubtype_declaration,
	dump_identifier identifier0,
	dump_subtype_indication subtype_indication1)
(*63*)   and dump_enumeration_literal = function
  | IdentifierEnumeration identifier -> Double(VhdIdentifierEnumeration,
	dump_identifier identifier)
  | CharLiteralEnumeration char -> Double(VhdCharLiteralEnumeration,
	dump_char char)
(*64*)   and dump_enumeration_type_definition x = List (List.map dump_enumeration_literal x)
(*65*)   and dump_integer_type_definition x = dump_range_constraint x
(*66*)   and dump_floating_type_definition x = dump_range_constraint x
(*67*)   and dump_constrained_array_definition {
  carraydimensions=index_constraint0;
  carrayelementtype=subtype_indication1;
} = Triple(Vhdconstrained_array_definition,
	dump_index_constraint index_constraint0,
	dump_subtype_indication subtype_indication1)
(*68*)   and dump_index_subtype_definition x = dump_type_mark x
(*69*)   and dump_unbounded_array_definition {
  uarraydimensions=index_subtype_definition0;
  uarrayelementtype=subtype_indication1;
} = Triple(Vhdunbounded_array_definition,
	List (List.map dump_index_subtype_definition index_subtype_definition0),
	dump_subtype_indication subtype_indication1)
(*70*)   and dump_array_type_definition = function
  | ConstrainedArray constrained_array_definition -> Double(VhdConstrainedArray,
	dump_constrained_array_definition constrained_array_definition)
  | UnboundedArray unbounded_array_definition -> Double(VhdUnboundedArray,
	dump_unbounded_array_definition unbounded_array_definition)
(*71*)   and dump_element_declaration {
  elementnames=identifier_list0;
  elementsubtype=subtype_indication1;
} = Triple(Vhdelement_declaration,
	dump_identifier_list identifier_list0,
	dump_subtype_indication subtype_indication1)
(*72*)   and dump_element_declaration_list x = List (List.map dump_element_declaration x)
(*73*)   and dump_record_type_definition {
  recordelems=element_declaration_list0;
  recordname=simple_name1;
} = Triple(Vhdrecord_type_definition,
	dump_element_declaration_list element_declaration_list0,
	dump_simple_name simple_name1)
(*74*)   and dump_file_type_definition x = dump_type_mark x
(*75*)   and dump_access_type_definition x = dump_subtype_indication x
(*76*)   and dump_physical_unit_declaration (identifier,physical_literal) = Double(dump_identifier identifier,
	dump_physical_literal physical_literal)
(*77*)   and dump_physical_type_definition {
  physicalconstraint=range_constraint0;
  physicalunits=physical_unit_declaration1;
  physicalname=identifier2;
} = Quadruple(Vhdphysical_type_definition,
	dump_range_constraint range_constraint0,
	List (List.map dump_physical_unit_declaration physical_unit_declaration1),
	dump_identifier identifier2)
(*78*)   and dump_type_definition = function
  | EnumerationTypeDefinition enumeration_type_definition -> Double(VhdEnumerationTypeDefinition,
	dump_enumeration_type_definition enumeration_type_definition)
  | RangeTypeDefinition range_constraint -> Double(VhdRangeTypeDefinition,
	dump_range_constraint range_constraint)
  | ArrayTypeDefinition array_type_definition -> Double(VhdArrayTypeDefinition,
	dump_array_type_definition array_type_definition)
  | RecordTypeDefinition record_type_definition -> Double(VhdRecordTypeDefinition,
	dump_record_type_definition record_type_definition)
  | FileTypeDefinition file_type_definition -> Double(VhdFileTypeDefinition,
	dump_file_type_definition file_type_definition)
  | AccessTypeDefinition access_type_definition -> Double(VhdAccessTypeDefinition,
	dump_access_type_definition access_type_definition)
  | PhysicalTypeDefinition physical_type_definition -> Double(VhdPhysicalTypeDefinition,
	dump_physical_type_definition physical_type_definition)
(*79*)   and dump_full_type_declaration {
  typename=identifier0;
  typedefinition=type_definition1;
} = Triple(Vhdfull_type_declaration,
	dump_identifier identifier0,
	dump_type_definition type_definition1)
(*80*)   and dump_incomplete_type_declaration {
  incompletetypename=identifier0;
} = Double(Vhdincomplete_type_declaration,
	dump_identifier identifier0)
(*81*)   and dump_type_declaration = function
  | FullType full_type_declaration -> Double(VhdFullType,
	dump_full_type_declaration full_type_declaration)
  | IncompleteType incomplete_type_declaration -> Double(VhdIncompleteType,
	dump_incomplete_type_declaration incomplete_type_declaration)
(*82*)   and dump_constant_declaration {
  constantnames=identifier_list0;
  constantsubtype=subtype_indication1;
  constantexpression=expression2;
} = Quadruple(Vhdconstant_declaration,
	dump_identifier_list identifier_list0,
	dump_subtype_indication subtype_indication1,
	dump_expression expression2)
(*83*)   and dump_signal_kind = function
  | SignalKindRegister -> VhdSignalKindRegister
  | SignalKindBus -> VhdSignalKindBus
  | SignalKindDefault -> VhdSignalKindDefault
(*84*)   and dump_signal_declaration {
  signalnames=identifier_list0;
  signalsubtype=subtype_indication1;
  signalkind=signal_kind2;
  signalexpression=expression3;
} = Quintuple(Vhdsignal_declaration,
	dump_identifier_list identifier_list0,
	dump_subtype_indication subtype_indication1,
	dump_signal_kind signal_kind2,
	dump_expression expression3)
(*85*)   and dump_variable_declaration {
  variableshared=vhd_bool0;
  variablenames=identifier_list1;
  variablesubtype=subtype_indication2;
  variableexpression=expression3;
} = Quintuple(Vhdvariable_declaration,
	dump_vhd_bool vhd_bool0,
	dump_identifier_list identifier_list1,
	dump_subtype_indication subtype_indication2,
	dump_expression expression3)
(*86*)   and dump_file_declaration {
  filenames=identifier_list0;
  filesubtype=subtype_indication1;
  fileopenkind=expression2;
  filelogicalname=expression3;
} = Quintuple(Vhdfile_declaration,
	dump_identifier_list identifier_list0,
	dump_subtype_indication subtype_indication1,
	dump_expression expression2,
	dump_expression expression3)
(*87*)   and dump_object_declaration = function
  | ConstantDeclaration constant_declaration -> Double(VhdConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | SignalDeclaration signal_declaration -> Double(VhdSignalDeclaration,
	dump_signal_declaration signal_declaration)
  | VariableDeclaration variable_declaration -> Double(VhdVariableDeclaration,
	dump_variable_declaration variable_declaration)
  | FileDeclaration file_declaration -> Double(VhdFileDeclaration,
	dump_file_declaration file_declaration)
(*88*)   and dump_mode = function
  | InterfaceModeIn -> VhdInterfaceModeIn
  | InterfaceModeOut -> VhdInterfaceModeOut
  | InterfaceModeInOut -> VhdInterfaceModeInOut
  | InterfaceModeBuffer -> VhdInterfaceModeBuffer
(*89*)   and dump_interface_variable_declaration {
  interfacevariablenames=identifier_list0;
  interfacevariablemode=mode1;
  interfacevariablesubtype=subtype_indication2;
  interfacevariableexpression=expression3;
} = Quintuple(Vhdinterface_variable_declaration,
	dump_identifier_list identifier_list0,
	dump_mode mode1,
	dump_subtype_indication subtype_indication2,
	dump_expression expression3)
(*90*)   and dump_interface_signal_declaration {
  interfacesignalnames=identifier_list0;
  interfacesignalmode=mode1;
  interfacesignalsubtype=subtype_indication2;
  interfacesignalkind=signal_kind3;
  interfacesignalexpression=expression4;
} = Sextuple(Vhdinterface_signal_declaration,
	dump_identifier_list identifier_list0,
	dump_mode mode1,
	dump_subtype_indication subtype_indication2,
	dump_signal_kind signal_kind3,
	dump_expression expression4)
(*91*)   and dump_interface_constant_declaration {
  interfaceconstantnames=identifier_list0;
  interfaceconstantsubtype=subtype_indication1;
  interfaceconstantexpression=expression2;
} = Quadruple(Vhdinterface_constant_declaration,
	dump_identifier_list identifier_list0,
	dump_subtype_indication subtype_indication1,
	dump_expression expression2)
(*92*)   and dump_interface_file_declaration {
  interfacefilenames=identifier_list0;
  interfacefilesubtype=subtype_indication1;
} = Triple(Vhdinterface_file_declaration,
	dump_identifier_list identifier_list0,
	dump_subtype_indication subtype_indication1)
(*93*)   and dump_interface_default_declaration {
  interfacedefaultnames=identifier_list0;
  interfacedefaultmode=mode1;
  interfacedefaultsubtype=subtype_indication2;
  interfacedefaultkind=signal_kind3;
  interfacedefaultexpression=expression4;
} = Sextuple(Vhdinterface_default_declaration,
	dump_identifier_list identifier_list0,
	dump_mode mode1,
	dump_subtype_indication subtype_indication2,
	dump_signal_kind signal_kind3,
	dump_expression expression4)
(*94*)   and dump_interface_object_declaration = function
  | InterfaceVariableDeclaration interface_variable_declaration -> Double(VhdInterfaceVariableDeclaration,
	dump_interface_variable_declaration interface_variable_declaration)
  | InterfaceSignalDeclaration interface_signal_declaration -> Double(VhdInterfaceSignalDeclaration,
	dump_interface_signal_declaration interface_signal_declaration)
  | InterfaceConstantDeclaration interface_constant_declaration -> Double(VhdInterfaceConstantDeclaration,
	dump_interface_constant_declaration interface_constant_declaration)
  | InterfaceFileDeclaration interface_file_declaration -> Double(VhdInterfaceFileDeclaration,
	dump_interface_file_declaration interface_file_declaration)
  | InterfaceDefaultDeclaration interface_default_declaration -> Double(VhdInterfaceDefaultDeclaration,
	dump_interface_default_declaration interface_default_declaration)
(*95*)   and dump_interface_type_declaration = function
  | InterfaceIncompleteTypeDeclaration identifier -> Double(VhdInterfaceIncompleteTypeDeclaration,
	dump_identifier identifier)
(*96*)   and dump_interface_declaration = function
  | InterfaceObjectDeclaration interface_object_declaration -> Double(VhdInterfaceObjectDeclaration,
	dump_interface_object_declaration interface_object_declaration)
  | InterfaceTypeDeclaration interface_type_declaration -> Double(VhdInterfaceTypeDeclaration,
	dump_interface_type_declaration interface_type_declaration)
(*97*)   and dump_interface_element x = dump_interface_declaration x
(*98*)   and dump_interface_list x = List (List.map dump_interface_element x)
(*99*)   and dump_alias_designator x = dump_designator x
(*100*)   and dump_alias_declaration {
  aliasdesignator=alias_designator0;
  aliassubtype=subtype_indication1;
  aliasexpression=expression2;
  aliassignature=signature3;
} = Quintuple(Vhdalias_declaration,
	dump_alias_designator alias_designator0,
	dump_subtype_indication subtype_indication1,
	dump_expression expression2,
	(match signature3 with Some signature -> dump_signature signature | None -> VhdNone))
(*101*)   and dump_entity_tag = function
  | EntityTagSimpleName simple_name -> Double(VhdEntityTagSimpleName,
	dump_simple_name simple_name)
  | EntityTagCharacterLiteral char -> Double(VhdEntityTagCharacterLiteral,
	dump_char char)
  | EntityTagOperatorSymbol string -> Double(VhdEntityTagOperatorSymbol,
	dump_string string)
(*102*)   and dump_entity_designator {
  entitytag=entity_tag0;
  entitysignature=signature1;
} = Triple(Vhdentity_designator,
	dump_entity_tag entity_tag0,
	(match signature1 with Some signature -> dump_signature signature | None -> VhdNone))
(*103*)   and dump_entity_name_list = function
  | EntityDesignator entity_designator_lst -> List (List.map dump_entity_designator entity_designator_lst)
  | EntityOthers -> VhdEntityOthers
  | EntityAll -> VhdEntityAll
(*104*)   and dump_entity_class = function
  | ClassEntity -> VhdClassEntity
  | ClassArchitecture -> VhdClassArchitecture
  | ClassConfiguration -> VhdClassConfiguration
  | ClassProcedure -> VhdClassProcedure
  | ClassFunction -> VhdClassFunction
  | ClassPackage -> VhdClassPackage
  | ClassType -> VhdClassType
  | ClassSubType -> VhdClassSubType
  | ClassConstant -> VhdClassConstant
  | ClassSignal -> VhdClassSignal
  | ClassVariable -> VhdClassVariable
  | ClassComponent -> VhdClassComponent
  | ClassLabel -> VhdClassLabel
  | ClassLiteral -> VhdClassLiteral
  | ClassUnits -> VhdClassUnits
  | ClassFile -> VhdClassFile
(*105*)   and dump_entity_specification {
  entitynames=entity_name_list0;
  entityclass=entity_class1;
} = Triple(Vhdentity_specification,
	dump_entity_name_list entity_name_list0,
	dump_entity_class entity_class1)
(*106*)   and dump_attribute_declaration {
  attributename=identifier0;
  attributetypename=type_mark1;
} = Triple(Vhdattribute_declaration,
	dump_identifier identifier0,
	dump_type_mark type_mark1)
(*107*)   and dump_attribute_specification {
  attributedesignator=identifier0;
  attributeentity=entity_specification1;
  attributeexpression=expression2;
} = Quadruple(Vhdattribute_specification,
	dump_identifier identifier0,
	dump_entity_specification entity_specification1,
	dump_expression expression2)
(*108*)   and dump_wait_statement {
  waitlabelname=label0;
  waitsensitivity=expression_list1;
  waitcondition=condition2;
  waittimeout=expression3;
} = Quintuple(Vhdwait_statement,
	dump_label label0,
	dump_expression_list expression_list1,
	dump_condition condition2,
	dump_expression expression3)
(*109*)   and dump_assertion {
  assertioncondition=condition0;
  assertionreport=expression1;
  assertionseverity=expression2;
} = Quadruple(Vhdassertion,
	dump_condition condition0,
	dump_expression expression1,
	dump_expression expression2)
(*110*)   and dump_assertion_statement {
  assertionlabelname=label0;
  assertion=assertion1;
} = Triple(Vhdassertion_statement,
	dump_label label0,
	dump_assertion assertion1)
(*111*)   and dump_report_statement {
  reportlabelname=label0;
  reportexpression=expression1;
  reportseverity=expression2;
} = Quadruple(Vhdreport_statement,
	dump_label label0,
	dump_expression expression1,
	dump_expression expression2)
(*112*)   and dump_waveform_element {
  valueexpression=expression0;
  timeexpression=expression1;
} = Triple(Vhdwaveform_element,
	dump_expression expression0,
	dump_expression expression1)
(*113*)   and dump_waveform = function
  | WaveForms waveform_element_lst -> List (List.map dump_waveform_element waveform_element_lst)
  | Unaffected -> VhdUnaffected
(*114*)   and dump_target = function
  | TargetName name -> Double(VhdTargetName,
	dump_name name)
  | TargetDotted name -> Double(VhdTargetDotted,
	dump_dotted name)
  | TargetNameParameters (name0,parameters1) -> Triple(VhdTargetNameParameters, dump_name name0
	, dump_parameters parameters1
	)
  | TargetAggregate aggregate -> Double(VhdTargetAggregate,
	dump_aggregate aggregate)
  | TargetInvalid vhd_int -> Double(VhdTargetInvalid,
	dump_vhd_int vhd_int)
  | SelectTargetName (name0,suffix1) -> Triple(VhdSelectTargetName, dump_name name0
	, dump_suffix suffix1
	)
  | SelectTargetNameParameters (name0,parameters1,suffix2) -> Quadruple(VhdSelectTargetNameParameters, dump_name name0
	, dump_parameters parameters1
	, dump_suffix suffix2
	)
(*115*)   and dump_delay_mechanism = function
  | DelayNone -> VhdDelayNone
  | DelayTransport -> VhdDelayTransport
  | DelayInertial expression -> Double(VhdDelayInertial,
	dump_expression expression)
(*116*)   and dump_signal_assignment_statement = function
  | SimpleSignalAssignment simple_signal_assignment_statement -> Double(VhdSimpleSignalAssignment,
	dump_simple_signal_assignment_statement simple_signal_assignment_statement)
  | ConditionalSignalAssignment conditional_signal_assignment_statement -> Double(VhdConditionalSignalAssignment,
	dump_conditional_signal_assignment_statement conditional_signal_assignment_statement)
  | SelectedSignalAssignment selected_signal_assignment_statement -> Double(VhdSelectedSignalAssignment,
	dump_selected_signal_assignment_statement selected_signal_assignment_statement)
(*117*)   and dump_simple_signal_assignment_statement {
  simplesignalassignmentlabelname=label0;
  simplesignalassignmenttarget=target1;
  simplesignalassignmentdelay=delay_mechanism2;
  simplesignalassignmentwaveform=waveform3;
} = Quintuple(Vhdsimple_signal_assignment_statement,
	dump_label label0,
	dump_target target1,
	dump_delay_mechanism delay_mechanism2,
	dump_waveform waveform3)
(*118*)   and dump_conditional_signal_assignment_statement {
  conditionalsignalassignmentlabelname=label0;
  conditionalsignalassignmenttarget=target1;
  conditionalsignalassignmentdelay=delay_mechanism2;
  conditionalsignalassignmentwaveforms=conditional_waveforms3;
} = Quintuple(Vhdconditional_signal_assignment_statement,
	dump_label label0,
	dump_target target1,
	dump_delay_mechanism delay_mechanism2,
	dump_conditional_waveforms conditional_waveforms3)
(*119*)   and dump_selected_signal_assignment_statement {
  selectedsignalassignmentlabelname=label0;
  selectedsignalassignmenttarget=target1;
  selectedsignalassignmentselector=selector2;
  selectedsignalassignmentkind=selection_kind3;
  selectedsignalassignmentdelay=delay_mechanism4;
  selectedsignalassignmentwaveforms=selected_waveforms5;
} = Septuple(Vhdselected_signal_assignment_statement,
	dump_label label0,
	dump_target target1,
	dump_selector selector2,
	dump_selection_kind selection_kind3,
	dump_delay_mechanism delay_mechanism4,
	dump_selected_waveforms selected_waveforms5)
(*120*)   and dump_variable_assignment_statement = function
  | SimpleVariableAssignment simple_variable_assignment -> Double(VhdSimpleVariableAssignment,
	dump_simple_variable_assignment simple_variable_assignment)
  | ConditionalVariableAssignment conditional_variable_assignment -> Double(VhdConditionalVariableAssignment,
	dump_conditional_variable_assignment conditional_variable_assignment)
  | SelectedVariableAssignment selected_variable_assignment -> Double(VhdSelectedVariableAssignment,
	dump_selected_variable_assignment selected_variable_assignment)
(*121*)   and dump_simple_variable_assignment {
  simplevariableassignmentlabelname=label0;
  simplevariableassignmenttarget=target1;
  simplevariableassignmentexpression=expression2;
} = Quadruple(Vhdsimple_variable_assignment,
	dump_label label0,
	dump_target target1,
	dump_expression expression2)
(*122*)   and dump_conditional_variable_assignment {
  conditionalvariableassignmentlabelname=label0;
  conditionalvariableassignmenttarget=target1;
  conditionalvariableassignmentexpressions=conditional_expressions2;
} = Quadruple(Vhdconditional_variable_assignment,
	dump_label label0,
	dump_target target1,
	dump_conditional_expressions conditional_expressions2)
(*123*)   and dump_conditional_expressions x = List (List.map dump_conditional_expression x)
(*124*)   and dump_conditional_expression {
  conditionalexpressionvalue=expression0;
  conditionalexpressioncondition=condition1;
} = Triple(Vhdconditional_expression,
	dump_expression expression0,
	dump_condition condition1)
(*125*)   and dump_selected_variable_assignment {
  selectedvariableassignmentlabelname=label0;
  selectedvariableassignmenttarget=target1;
  selectedvariableassignmentselector=selector2;
  selectedvariableassignmentkind=selection_kind3;
  selectedvariableassignmentexpressions=selected_expressions4;
} = Sextuple(Vhdselected_variable_assignment,
	dump_label label0,
	dump_target target1,
	dump_selector selector2,
	dump_selection_kind selection_kind3,
	dump_selected_expressions selected_expressions4)
(*126*)   and dump_selected_expressions x = List (List.map dump_selected_expression x)
(*127*)   and dump_selected_expression {
  selectedexpressionvalue=expression0;
  selectedexpressionchoices=choices1;
} = Triple(Vhdselected_expression,
	dump_expression expression0,
	dump_choices choices1)
(*128*)   and dump_procedure_call {
  procedurecallname=name0;
  procedurecallparameter=parameter1;
} = Triple(Vhdprocedure_call,
	dump_name name0,
	dump_parameter parameter1)
(*129*)   and dump_procedure_call_statement {
  procedurecalllabelname=label0;
  procedurecall=procedure_call1;
} = Triple(Vhdprocedure_call_statement,
	dump_label label0,
	dump_procedure_call procedure_call1)
(*130*)   and dump_else_statements = function
  | ElseNone -> VhdElseNone
  | Else sequence_of_statements -> Double(VhdElse,
	dump_sequence_of_statements sequence_of_statements)
  | Elsif if_statement -> Double(VhdElsif,
	dump_if_statement if_statement)
(*131*)   and dump_if_statement {
  iflabelname=label0;
  ifcondition=condition1;
  thenstatements=sequence_of_statements2;
  elsestatements=else_statements3;
} = Quintuple(Vhdif_statement,
	dump_label label0,
	dump_condition condition1,
	dump_sequence_of_statements sequence_of_statements2,
	dump_else_statements else_statements3)
(*132*)   and dump_case_statement {
  caselabelname=label0;
  caseselector=selector1;
  casekind=selection_kind2;
  casealternatives=case_statement_alternative3;
} = Quintuple(Vhdcase_statement,
	dump_label label0,
	dump_selector selector1,
	dump_selection_kind selection_kind2,
	List (List.map dump_case_statement_alternative case_statement_alternative3))
(*133*)   and dump_case_statement_alternative {
  casechoices=choices0;
  casestatements=sequence_of_statements1;
} = Triple(Vhdcase_statement_alternative,
	dump_choices choices0,
	dump_sequence_of_statements sequence_of_statements1)
(*134*)   and dump_parameter_specification {
  parameteridentifier=identifier0;
  parameterrange=discrete_range1;
} = Triple(Vhdparameter_specification,
	dump_identifier identifier0,
	dump_discrete_range discrete_range1)
(*135*)   and dump_iteration_scheme = function
  | AlwaysLoop -> VhdAlwaysLoop
  | WhileLoop condition -> Double(VhdWhileLoop,
	dump_condition condition)
  | ForLoop parameter_specification -> Double(VhdForLoop,
	dump_parameter_specification parameter_specification)
(*136*)   and dump_loop_statement {
  looplabelname=label0;
  loopiteration=iteration_scheme1;
  loopstatements=sequence_of_statements2;
} = Quadruple(Vhdloop_statement,
	dump_label label0,
	dump_iteration_scheme iteration_scheme1,
	dump_sequence_of_statements sequence_of_statements2)
(*137*)   and dump_next_statement {
  nextlabelname=label0;
  nextlooplabelname=label1;
  nextcondition=condition2;
} = Quadruple(Vhdnext_statement,
	dump_label label0,
	dump_label label1,
	dump_condition condition2)
(*138*)   and dump_exit_statement {
  exitlabelname=label0;
  exitlooplabelname=label1;
  exitcondition=condition2;
} = Quadruple(Vhdexit_statement,
	dump_label label0,
	dump_label label1,
	dump_condition condition2)
(*139*)   and dump_return_statement {
  returnlabelname=label0;
  returnexpression=expression1;
} = Triple(Vhdreturn_statement,
	dump_label label0,
	dump_expression expression1)
(*140*)   and dump_null_statement {
  nulllabelname=label0;
} = Double(Vhdnull_statement,
	dump_label label0)
(*141*)   and dump_sequential_statement = function
  | SequentialWait wait_statement -> Double(VhdSequentialWait,
	dump_wait_statement wait_statement)
  | SequentialAssertion assertion_statement -> Double(VhdSequentialAssertion,
	dump_assertion_statement assertion_statement)
  | SequentialReport report_statement -> Double(VhdSequentialReport,
	dump_report_statement report_statement)
  | SequentialSignalAssignment signal_assignment_statement -> Double(VhdSequentialSignalAssignment,
	dump_signal_assignment_statement signal_assignment_statement)
  | SequentialVariableAssignment variable_assignment_statement -> Double(VhdSequentialVariableAssignment,
	dump_variable_assignment_statement variable_assignment_statement)
  | SequentialProcedureCall procedure_call_statement -> Double(VhdSequentialProcedureCall,
	dump_procedure_call_statement procedure_call_statement)
  | SequentialIf if_statement -> Double(VhdSequentialIf,
	dump_if_statement if_statement)
  | SequentialCase case_statement -> Double(VhdSequentialCase,
	dump_case_statement case_statement)
  | SequentialLoop loop_statement -> Double(VhdSequentialLoop,
	dump_loop_statement loop_statement)
  | SequentialNext next_statement -> Double(VhdSequentialNext,
	dump_next_statement next_statement)
  | SequentialExit exit_statement -> Double(VhdSequentialExit,
	dump_exit_statement exit_statement)
  | SequentialReturn return_statement -> Double(VhdSequentialReturn,
	dump_return_statement return_statement)
  | SequentialNull null_statement -> Double(VhdSequentialNull,
	dump_null_statement null_statement)
(*142*)   and dump_sequence_of_statements x = List (List.map dump_sequential_statement x)
(*143*)   and dump_subprogram_declaration x = dump_subprogram_specification x
(*144*)   and dump_subprogram_specification = function
  | ProcedureSpecification procedure_specification -> Double(VhdProcedureSpecification,
	dump_procedure_specification procedure_specification)
  | FunctionSpecification function_specification -> Double(VhdFunctionSpecification,
	dump_function_specification function_specification)
(*145*)   and dump_procedure_specification {
  procedurespecdesignator=designator0;
  procedurespecparameters=formal_parameter_list1;
  procedurespecgenericlist=generic_list2;
  procedurespecgenericmapaspect=generic_map_aspect3;
} = Quintuple(Vhdprocedure_specification,
	dump_designator designator0,
	dump_formal_parameter_list formal_parameter_list1,
	dump_generic_list generic_list2,
	dump_generic_map_aspect generic_map_aspect3)
(*146*)   and dump_function_specification {
  functionspecdesignator=designator0;
  functionspecparameters=formal_parameter_list1;
  functionspecgenericlist=generic_list2;
  functionspecgenericmapaspect=generic_map_aspect3;
  functionspecreturntype=type_mark4;
  functionspecpurity=function_purity5;
} = Septuple(Vhdfunction_specification,
	dump_designator designator0,
	dump_formal_parameter_list formal_parameter_list1,
	dump_generic_list generic_list2,
	dump_generic_map_aspect generic_map_aspect3,
	dump_type_mark type_mark4,
	dump_function_purity function_purity5)
(*147*)   and dump_function_purity = function
  | Unknown -> VhdUnknown
  | Pure -> VhdPure
  | Impure -> VhdImpure
(*148*)   and dump_formal_parameter_list x = dump_interface_list x
(*149*)   and dump_subprogram_body {
  subprogramspecification=subprogram_specification0;
  subprogramdeclarations=subprogram_declarative_part1;
  subprogramstatements=subprogram_statement_part2;
} = Quadruple(Vhdsubprogram_body,
	dump_subprogram_specification subprogram_specification0,
	dump_subprogram_declarative_part subprogram_declarative_part1,
	dump_subprogram_statement_part subprogram_statement_part2)
(*150*)   and dump_subprogram_declarative_part x = List (List.map dump_subprogram_declarative_item x)
(*151*)   and dump_subprogram_declarative_item = function
  | SubProgramDeclaration subprogram_declaration -> Double(VhdSubProgramDeclaration,
	dump_subprogram_declaration subprogram_declaration)
  | SubProgramBody subprogram_body -> Double(VhdSubProgramBody,
	dump_subprogram_body subprogram_body)
  | SubProgramInstantiation subprogram_instantiation -> Double(VhdSubProgramInstantiation,
	dump_subprogram_instantiation subprogram_instantiation)
  | SubProgramTypeDeclaration type_declaration -> Double(VhdSubProgramTypeDeclaration,
	dump_type_declaration type_declaration)
  | SubProgramSubTypeDeclaration subtype_declaration -> Double(VhdSubProgramSubTypeDeclaration,
	dump_subtype_declaration subtype_declaration)
  | SubProgramConstantDeclaration constant_declaration -> Double(VhdSubProgramConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | SubProgramVariableDeclaration variable_declaration -> Double(VhdSubProgramVariableDeclaration,
	dump_variable_declaration variable_declaration)
  | SubProgramFileDeclaration file_declaration -> Double(VhdSubProgramFileDeclaration,
	dump_file_declaration file_declaration)
  | SubProgramAliasDeclaration alias_declaration -> Double(VhdSubProgramAliasDeclaration,
	dump_alias_declaration alias_declaration)
  | SubProgramAttributeDeclaration attribute_declaration -> Double(VhdSubProgramAttributeDeclaration,
	dump_attribute_declaration attribute_declaration)
  | SubProgramAttributeSpecification attribute_specification -> Double(VhdSubProgramAttributeSpecification,
	dump_attribute_specification attribute_specification)
  | SubProgramUseClause use_clause -> Double(VhdSubProgramUseClause,
	dump_use_clause use_clause)
(*152*)   and dump_subprogram_statement_part x = dump_sequence_of_statements x
(*153*)   and dump_subprogram_instantiation {
  subprograminstantiationkind=subprogram_kind0;
  subprograminstantiationdesignator=designator1;
  subprograminstantiationname=name2;
  subprograminstantiationsignature=signature3;
  subprograminstantiationgenericmapaspect=generic_map_aspect4;
} = Sextuple(Vhdsubprogram_instantiation,
	dump_subprogram_kind subprogram_kind0,
	dump_designator designator1,
	dump_name name2,
	(match signature3 with Some signature -> dump_signature signature | None -> VhdNone),
	dump_generic_map_aspect generic_map_aspect4)
(*154*)   and dump_subprogram_kind = function
  | Function -> VhdFunction
  | Procedure -> VhdProcedure
(*155*)   and dump_component_declaration {
  componentname=identifier0;
  componentgeneric=generic_clause1;
  componentport=port_clause2;
} = Quadruple(Vhdcomponent_declaration,
	dump_identifier identifier0,
	dump_generic_clause generic_clause1,
	dump_port_clause port_clause2)
(*156*)   and dump_generic_clause x = dump_generic_list x
(*157*)   and dump_port_clause x = dump_port_list x
(*158*)   and dump_generic_list x = dump_interface_list x
(*159*)   and dump_port_list x = dump_interface_list x
(*160*)   and dump_instantiation_list = function
  | InstantiationLabels label_lst -> List (List.map dump_label label_lst)
  | InstantiationOthers -> VhdInstantiationOthers
  | InstantiationAll -> VhdInstantiationAll
(*161*)   and dump_component_specification {
  componentinstantiations=instantiation_list0;
  componentspecname=name1;
} = Triple(Vhdcomponent_specification,
	dump_instantiation_list instantiation_list0,
	dump_name name1)
(*162*)   and dump_configuration_specification {
  configurationcomponent=component_specification0;
  configurationbinding=binding_indication1;
} = Triple(Vhdconfiguration_specification,
	dump_component_specification component_specification0,
	dump_binding_indication binding_indication1)
(*163*)   and dump_binding_indication {
  bindingentity=entity_aspect0;
  bindinggenericmap=generic_map_aspect1;
  bindingportmap=port_map_aspect2;
} = Quadruple(Vhdbinding_indication,
	dump_entity_aspect entity_aspect0,
	dump_generic_map_aspect generic_map_aspect1,
	dump_port_map_aspect port_map_aspect2)
(*164*)   and dump_entity_aspect = function
  | BindingEntity name -> Double(VhdBindingEntity,
	dump_name name)
  | BindingEntityArchitecture (name0,identifier1) -> Triple(VhdBindingEntityArchitecture, dump_name name0
	, dump_identifier identifier1
	)
  | BindingConfiguration name -> Double(VhdBindingConfiguration,
	dump_name name)
  | BindingOpen -> VhdBindingOpen
(*165*)   and dump_generic_map_aspect x = dump_association_list x
(*166*)   and dump_port_map_aspect x = dump_association_list x
(*167*)   and dump_block_statement {
  blocklabelname=label0;
  blockguardcondition=condition1;
  blockgenericclause=generic_clause2;
  blockgenericmapaspect=generic_map_aspect3;
  blockportclause=port_clause4;
  blockportmapaspect=port_map_aspect5;
  blockdeclarations=block_declarative_part6;
  blockstatements=block_statement_part7;
} = Nonuple(Vhdblock_statement,
	dump_label label0,
	dump_condition condition1,
	dump_generic_clause generic_clause2,
	dump_generic_map_aspect generic_map_aspect3,
	dump_port_clause port_clause4,
	dump_port_map_aspect port_map_aspect5,
	dump_block_declarative_part block_declarative_part6,
	dump_block_statement_part block_statement_part7)
(*168*)   and dump_block_declarative_part x = List (List.map dump_block_declarative_item x)
(*169*)   and dump_block_statement_part x = List (List.map dump_concurrent_statement x)
(*170*)   and dump_block_declarative_item = function
  | BlockSubProgramDeclaration subprogram_declaration -> Double(VhdBlockSubProgramDeclaration,
	dump_subprogram_declaration subprogram_declaration)
  | BlockSubProgramBody subprogram_body -> Double(VhdBlockSubProgramBody,
	dump_subprogram_body subprogram_body)
  | BlockSubProgramInstantiation subprogram_instantiation -> Double(VhdBlockSubProgramInstantiation,
	dump_subprogram_instantiation subprogram_instantiation)
  | BlockTypeDeclaration type_declaration -> Double(VhdBlockTypeDeclaration,
	dump_type_declaration type_declaration)
  | BlockSubTypeDeclaration subtype_declaration -> Double(VhdBlockSubTypeDeclaration,
	dump_subtype_declaration subtype_declaration)
  | BlockConstantDeclaration constant_declaration -> Double(VhdBlockConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | BlockSignalDeclaration signal_declaration -> Double(VhdBlockSignalDeclaration,
	dump_signal_declaration signal_declaration)
  | BlockFileDeclaration file_declaration -> Double(VhdBlockFileDeclaration,
	dump_file_declaration file_declaration)
  | BlockAliasDeclaration alias_declaration -> Double(VhdBlockAliasDeclaration,
	dump_alias_declaration alias_declaration)
  | BlockComponentDeclaration component_declaration -> Double(VhdBlockComponentDeclaration,
	dump_component_declaration component_declaration)
  | BlockAttributeDeclaration attribute_declaration -> Double(VhdBlockAttributeDeclaration,
	dump_attribute_declaration attribute_declaration)
  | BlockAttributeSpecification attribute_specification -> Double(VhdBlockAttributeSpecification,
	dump_attribute_specification attribute_specification)
  | BlockConfigurationSpecification configuration_specification -> Double(VhdBlockConfigurationSpecification,
	dump_configuration_specification configuration_specification)
  | BlockUseClause use_clause -> Double(VhdBlockUseClause,
	dump_use_clause use_clause)
(*171*)   and dump_process_statement {
  processlabelname=label0;
  processpostponed=vhd_bool1;
  processsensitivitylist=process_sensitivity_list2;
  processdeclarations=process_declarative_part3;
  processstatements=process_statement_part4;
} = Sextuple(Vhdprocess_statement,
	dump_label label0,
	dump_vhd_bool vhd_bool1,
	dump_process_sensitivity_list process_sensitivity_list2,
	dump_process_declarative_part process_declarative_part3,
	dump_process_statement_part process_statement_part4)
(*172*)   and dump_process_sensitivity_list = function
  | SensitivityAll -> VhdSensitivityAll
  | SensitivityExpressionList sensitivity_list -> Double(VhdSensitivityExpressionList,
	dump_sensitivity_list sensitivity_list)
(*173*)   and dump_sensitivity_list x = dump_expression_list x
(*174*)   and dump_process_declarative_part x = List (List.map dump_process_declarative_item x)
(*175*)   and dump_process_declarative_item = function
  | ProcessSubProgramDeclaration subprogram_declaration -> Double(VhdProcessSubProgramDeclaration,
	dump_subprogram_declaration subprogram_declaration)
  | ProcessSubProgramBody subprogram_body -> Double(VhdProcessSubProgramBody,
	dump_subprogram_body subprogram_body)
  | ProcessSubProgramInstantiation subprogram_instantiation -> Double(VhdProcessSubProgramInstantiation,
	dump_subprogram_instantiation subprogram_instantiation)
  | ProcessTypeDeclaration type_declaration -> Double(VhdProcessTypeDeclaration,
	dump_type_declaration type_declaration)
  | ProcessSubTypeDeclaration subtype_declaration -> Double(VhdProcessSubTypeDeclaration,
	dump_subtype_declaration subtype_declaration)
  | ProcessConstantDeclaration constant_declaration -> Double(VhdProcessConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | ProcessVariableDeclaration variable_declaration -> Double(VhdProcessVariableDeclaration,
	dump_variable_declaration variable_declaration)
  | ProcessFileDeclaration file_declaration -> Double(VhdProcessFileDeclaration,
	dump_file_declaration file_declaration)
  | ProcessAliasDeclaration alias_declaration -> Double(VhdProcessAliasDeclaration,
	dump_alias_declaration alias_declaration)
  | ProcessAttributeDeclaration attribute_declaration -> Double(VhdProcessAttributeDeclaration,
	dump_attribute_declaration attribute_declaration)
  | ProcessAttributeSpecification attribute_specification -> Double(VhdProcessAttributeSpecification,
	dump_attribute_specification attribute_specification)
  | ProcessUseClause use_clause -> Double(VhdProcessUseClause,
	dump_use_clause use_clause)
(*176*)   and dump_process_statement_part x = List (List.map dump_sequential_statement x)
(*177*)   and dump_concurrent_procedure_call_statement {
  concurrentprocedurelabelname=label0;
  concurrentpostponedprocedure=vhd_bool1;
  concurrentprocedurecall=procedure_call2;
} = Quadruple(Vhdconcurrent_procedure_call_statement,
	dump_label label0,
	dump_vhd_bool vhd_bool1,
	dump_procedure_call procedure_call2)
(*178*)   and dump_concurrent_assertion_statement {
  concurrentassertionlabelname=label0;
  concurrentpostponedassertion=vhd_bool1;
  concurrentassertion=assertion2;
} = Quadruple(Vhdconcurrent_assertion_statement,
	dump_label label0,
	dump_vhd_bool vhd_bool1,
	dump_assertion assertion2)
(*179*)   and dump_concurrent_selected_signal_assignment {
  concurrentselectedsignaltarget=target0;
  concurrentselectedsignalguarded=vhd_bool1;
  concurrentselectedsignaldelay=delay_mechanism2;
  concurrentselectedsignalselector=selector3;
  concurrentselectedsignalkind=selection_kind4;
  concurrentselectedsignalwaveforms=selected_waveforms5;
} = Septuple(Vhdconcurrent_selected_signal_assignment,
	dump_target target0,
	dump_vhd_bool vhd_bool1,
	dump_delay_mechanism delay_mechanism2,
	dump_selector selector3,
	dump_selection_kind selection_kind4,
	dump_selected_waveforms selected_waveforms5)
(*180*)   and dump_selected_waveforms x = List (List.map dump_selected_waveform x)
(*181*)   and dump_selected_waveform {
  selectedwaveformvalue=waveform0;
  selectedwaveformchoices=choices1;
} = Triple(Vhdselected_waveform,
	dump_waveform waveform0,
	dump_choices choices1)
(*182*)   and dump_concurrent_conditional_signal_assignment {
  concurrentconditionalsignaltarget=target0;
  concurrentconditionalsignalguarded=vhd_bool1;
  concurrentconditionalsignaldelay=delay_mechanism2;
  concurrentconditionalsignalwaveforms=conditional_waveforms3;
} = Quintuple(Vhdconcurrent_conditional_signal_assignment,
	dump_target target0,
	dump_vhd_bool vhd_bool1,
	dump_delay_mechanism delay_mechanism2,
	dump_conditional_waveforms conditional_waveforms3)
(*183*)   and dump_conditional_waveforms x = List (List.map dump_conditional_waveform x)
(*184*)   and dump_conditional_waveform {
  conditionalwaveformvalue=waveform0;
  conditionalwaveformcondition=condition1;
} = Triple(Vhdconditional_waveform,
	dump_waveform waveform0,
	dump_condition condition1)
(*185*)   and dump_concurrent_simple_signal_assignment {
  concurrentsimplesignaltarget=target0;
  concurrentsimplesignalguarded=vhd_bool1;
  concurrentsimplesignaldelay=delay_mechanism2;
  concurrentsimplesignalwaveform=waveform3;
} = Quintuple(Vhdconcurrent_simple_signal_assignment,
	dump_target target0,
	dump_vhd_bool vhd_bool1,
	dump_delay_mechanism delay_mechanism2,
	dump_waveform waveform3)
(*186*)   and dump_concurrent_signal_assignment = function
  | ConcurrentSimpleSignalAssignment concurrent_simple_signal_assignment -> Double(VhdConcurrentSimpleSignalAssignment,
	dump_concurrent_simple_signal_assignment concurrent_simple_signal_assignment)
  | ConcurrentConditionalSignalAssignment concurrent_conditional_signal_assignment -> Double(VhdConcurrentConditionalSignalAssignment,
	dump_concurrent_conditional_signal_assignment concurrent_conditional_signal_assignment)
  | ConcurrentSelectedSignalAssignment concurrent_selected_signal_assignment -> Double(VhdConcurrentSelectedSignalAssignment,
	dump_concurrent_selected_signal_assignment concurrent_selected_signal_assignment)
(*187*)   and dump_concurrent_signal_assignment_statement {
  concurrentsignallabelname=label0;
  concurrentsignalpostponed=vhd_bool1;
  concurrentsignalassignment=concurrent_signal_assignment2;
} = Quadruple(Vhdconcurrent_signal_assignment_statement,
	dump_label label0,
	dump_vhd_bool vhd_bool1,
	dump_concurrent_signal_assignment concurrent_signal_assignment2)
(*188*)   and dump_instantiated_unit = function
  | InstantiatedComponent name -> Double(VhdInstantiatedComponent,
	dump_name name)
  | InstantiatedEntityArchitecture (name0,identifier1) -> Triple(VhdInstantiatedEntityArchitecture, dump_name name0
	, dump_identifier identifier1
	)
  | InstantiatedConfiguration name -> Double(VhdInstantiatedConfiguration,
	dump_name name)
(*189*)   and dump_component_instantiation_statement {
  componentlabelname=label0;
  componentunit=instantiated_unit1;
  componentgenericmap=generic_map_aspect2;
  componentportmap=port_map_aspect3;
} = Quintuple(Vhdcomponent_instantiation_statement,
	dump_label label0,
	dump_instantiated_unit instantiated_unit1,
	dump_generic_map_aspect generic_map_aspect2,
	dump_port_map_aspect port_map_aspect3)
(*190*)   and dump_generate_statement = function
  | IfGenerateStatement if_generate_statement -> Double(VhdIfGenerateStatement,
	dump_if_generate_statement if_generate_statement)
  | ForGenerateStatement for_generate_statement -> Double(VhdForGenerateStatement,
	dump_for_generate_statement for_generate_statement)
  | CaseGenerateStatement case_generate_statement -> Double(VhdCaseGenerateStatement,
	dump_case_generate_statement case_generate_statement)
(*191*)   and dump_if_generate_statement {
  ifgeneratelabelname=label0;
  ifgeneratealternatelabelname=label1;
  ifgeneratecondition=condition2;
  ifgeneratedeclarations=block_declarative_part3;
  ifgeneratethenstatements=concurrent_statement4;
  ifgenerateelsestatements=else_generate_statement5;
} = Septuple(Vhdif_generate_statement,
	dump_label label0,
	dump_label label1,
	dump_condition condition2,
	dump_block_declarative_part block_declarative_part3,
	List (List.map dump_concurrent_statement concurrent_statement4),
	dump_else_generate_statement else_generate_statement5)
(*192*)   and dump_else_generate_statement = function
  | GenerateElseNone -> VhdGenerateElseNone
  | GenerateElse if_generate_statement -> Double(VhdGenerateElse,
	dump_if_generate_statement if_generate_statement)
  | GenerateElsif if_generate_statement -> Double(VhdGenerateElsif,
	dump_if_generate_statement if_generate_statement)
(*193*)   and dump_for_generate_statement {
  forgeneratelabelname=label0;
  forgeneratealternatelabelname=label1;
  forgenerateparameter=parameter_specification2;
  forgeneratedeclarations=block_declarative_part3;
  forgeneratestatements=concurrent_statement4;
} = Sextuple(Vhdfor_generate_statement,
	dump_label label0,
	dump_label label1,
	dump_parameter_specification parameter_specification2,
	dump_block_declarative_part block_declarative_part3,
	List (List.map dump_concurrent_statement concurrent_statement4))
(*194*)   and dump_case_generate_statement {
  casegeneratelabelname=label0;
  casegenerateselector=selector1;
  casegeneratekind=selection_kind2;
  casegeneratealternatives=case_generate_alternative3;
} = Quintuple(Vhdcase_generate_statement,
	dump_label label0,
	dump_selector selector1,
	dump_selection_kind selection_kind2,
	List (List.map dump_case_generate_alternative case_generate_alternative3))
(*195*)   and dump_case_generate_alternative {
  casegeneratealternatelabelname=label0;
  casegeneratechoices=choices1;
  casegeneratedeclarations=block_declarative_part2;
  casegeneratestatements=concurrent_statement3;
} = Quintuple(Vhdcase_generate_alternative,
	dump_label label0,
	dump_choices choices1,
	dump_block_declarative_part block_declarative_part2,
	List (List.map dump_concurrent_statement concurrent_statement3))
(*196*)   and dump_use_clause x = dump_selected_name_list x
(*197*)   and dump_concurrent_statement = function
  | ConcurrentBlockStatement block_statement -> Double(VhdConcurrentBlockStatement,
	dump_block_statement block_statement)
  | ConcurrentProcessStatement process_statement -> Double(VhdConcurrentProcessStatement,
	dump_process_statement process_statement)
  | ConcurrentProcedureCallStatement concurrent_procedure_call_statement -> Double(VhdConcurrentProcedureCallStatement,
	dump_concurrent_procedure_call_statement concurrent_procedure_call_statement)
  | ConcurrentAssertionStatement concurrent_assertion_statement -> Double(VhdConcurrentAssertionStatement,
	dump_concurrent_assertion_statement concurrent_assertion_statement)
  | ConcurrentSignalAssignmentStatement concurrent_signal_assignment_statement -> Double(VhdConcurrentSignalAssignmentStatement,
	dump_concurrent_signal_assignment_statement concurrent_signal_assignment_statement)
  | ConcurrentComponentInstantiationStatement component_instantiation_statement -> Double(VhdConcurrentComponentInstantiationStatement,
	dump_component_instantiation_statement component_instantiation_statement)
  | ConcurrentGenerateStatement generate_statement -> Double(VhdConcurrentGenerateStatement,
	dump_generate_statement generate_statement)
(*198*)   and dump_concurrent_statements x = List (List.map dump_concurrent_statement x)
(*199*)   and dump_package_declarative_item = function
  | PackageSubProgramDeclaration subprogram_declaration -> Double(VhdPackageSubProgramDeclaration,
	dump_subprogram_declaration subprogram_declaration)
  | PackageSubProgramInstantiation subprogram_instantiation -> Double(VhdPackageSubProgramInstantiation,
	dump_subprogram_instantiation subprogram_instantiation)
  | PackageTypeDeclaration type_declaration -> Double(VhdPackageTypeDeclaration,
	dump_type_declaration type_declaration)
  | PackageSubTypeDeclaration subtype_declaration -> Double(VhdPackageSubTypeDeclaration,
	dump_subtype_declaration subtype_declaration)
  | PackageConstantDeclaration constant_declaration -> Double(VhdPackageConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | PackageSignalDeclaration signal_declaration -> Double(VhdPackageSignalDeclaration,
	dump_signal_declaration signal_declaration)
  | PackageFileDeclaration file_declaration -> Double(VhdPackageFileDeclaration,
	dump_file_declaration file_declaration)
  | PackageAliasDeclaration alias_declaration -> Double(VhdPackageAliasDeclaration,
	dump_alias_declaration alias_declaration)
  | PackageComponentDeclaration component_declaration -> Double(VhdPackageComponentDeclaration,
	dump_component_declaration component_declaration)
  | PackageAttributeDeclaration attribute_declaration -> Double(VhdPackageAttributeDeclaration,
	dump_attribute_declaration attribute_declaration)
  | PackageAttributeSpecification attribute_specification -> Double(VhdPackageAttributeSpecification,
	dump_attribute_specification attribute_specification)
  | PackageUseClause use_clause -> Double(VhdPackageUseClause,
	dump_use_clause use_clause)
(*200*)   and dump_package_declarative_part x = List (List.map dump_package_declarative_item x)
(*201*)   and dump_package_declaration {
  packagename=identifier0;
  packagegenericclause=generic_clause1;
  packagegenericmapaspect=generic_map_aspect2;
  packagedeclarations=package_declarative_part3;
} = Quintuple(Vhdpackage_declaration,
	dump_identifier identifier0,
	dump_generic_clause generic_clause1,
	dump_generic_map_aspect generic_map_aspect2,
	dump_package_declarative_part package_declarative_part3)
(*202*)   and dump_package_body_declarative_item = function
  | PackageBodySubProgramDeclaration subprogram_declaration -> Double(VhdPackageBodySubProgramDeclaration,
	dump_subprogram_declaration subprogram_declaration)
  | PackageBodySubProgramBody subprogram_body -> Double(VhdPackageBodySubProgramBody,
	dump_subprogram_body subprogram_body)
  | PackageBodySubProgramInstantiation subprogram_instantiation -> Double(VhdPackageBodySubProgramInstantiation,
	dump_subprogram_instantiation subprogram_instantiation)
  | PackageBodyTypeDeclaration type_declaration -> Double(VhdPackageBodyTypeDeclaration,
	dump_type_declaration type_declaration)
  | PackageBodySubTypeDeclaration subtype_declaration -> Double(VhdPackageBodySubTypeDeclaration,
	dump_subtype_declaration subtype_declaration)
  | PackageBodyConstantDeclaration constant_declaration -> Double(VhdPackageBodyConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | PackageBodyFileDeclaration file_declaration -> Double(VhdPackageBodyFileDeclaration,
	dump_file_declaration file_declaration)
  | PackageBodyAliasDeclaration alias_declaration -> Double(VhdPackageBodyAliasDeclaration,
	dump_alias_declaration alias_declaration)
  | PackageBodyUseClause use_clause -> Double(VhdPackageBodyUseClause,
	dump_use_clause use_clause)
(*203*)   and dump_package_body_declarative_part x = List (List.map dump_package_body_declarative_item x)
(*204*)   and dump_package_body {
  packagebodyname=simple_name0;
  packagebodydeclarations=package_body_declarative_part1;
} = Triple(Vhdpackage_body,
	dump_simple_name simple_name0,
	dump_package_body_declarative_part package_body_declarative_part1)
(*205*)   and dump_package_instantiation {
  packageinstantiationidentifier=identifier0;
  packageinstantiationname=name1;
  packageinstantiationgenericmap=generic_map_aspect2;
} = Quadruple(Vhdpackage_instantiation,
	dump_identifier identifier0,
	dump_name name1,
	dump_generic_map_aspect generic_map_aspect2)
(*206*)   and dump_entity_header {
  entitygenerics=generic_clause0;
  entityports=port_clause1;
} = Triple(Vhdentity_header,
	dump_generic_clause generic_clause0,
	dump_port_clause port_clause1)
(*207*)   and dump_entity_declarative_item = function
  | EntitySubProgramDeclaration subprogram_declaration -> Double(VhdEntitySubProgramDeclaration,
	dump_subprogram_declaration subprogram_declaration)
  | EntitySubProgramBody subprogram_body -> Double(VhdEntitySubProgramBody,
	dump_subprogram_body subprogram_body)
  | EntitySubProgramInstantiation subprogram_instantiation -> Double(VhdEntitySubProgramInstantiation,
	dump_subprogram_instantiation subprogram_instantiation)
  | EntityTypeDeclaration type_declaration -> Double(VhdEntityTypeDeclaration,
	dump_type_declaration type_declaration)
  | EntitySubTypeDeclaration subtype_declaration -> Double(VhdEntitySubTypeDeclaration,
	dump_subtype_declaration subtype_declaration)
  | EntityConstantDeclaration constant_declaration -> Double(VhdEntityConstantDeclaration,
	dump_constant_declaration constant_declaration)
  | EntitySignalDeclaration signal_declaration -> Double(VhdEntitySignalDeclaration,
	dump_signal_declaration signal_declaration)
  | EntityFileDeclaration file_declaration -> Double(VhdEntityFileDeclaration,
	dump_file_declaration file_declaration)
  | EntityAliasDeclaration alias_declaration -> Double(VhdEntityAliasDeclaration,
	dump_alias_declaration alias_declaration)
  | EntityAttributeDeclaration attribute_declaration -> Double(VhdEntityAttributeDeclaration,
	dump_attribute_declaration attribute_declaration)
  | EntityAttributeSpecification attribute_specification -> Double(VhdEntityAttributeSpecification,
	dump_attribute_specification attribute_specification)
  | EntityUseClause use_clause -> Double(VhdEntityUseClause,
	dump_use_clause use_clause)
(*208*)   and dump_entity_declarative_part x = List (List.map dump_entity_declarative_item x)
(*209*)   and dump_entity_statement = function
  | EntityConcurrentAssertionStatement concurrent_assertion_statement -> Double(VhdEntityConcurrentAssertionStatement,
	dump_concurrent_assertion_statement concurrent_assertion_statement)
  | EntityConcurrentProcedureCallStatement concurrent_procedure_call_statement -> Double(VhdEntityConcurrentProcedureCallStatement,
	dump_concurrent_procedure_call_statement concurrent_procedure_call_statement)
  | EntityProcessStatement process_statement -> Double(VhdEntityProcessStatement,
	dump_process_statement process_statement)
(*210*)   and dump_entity_statement_part x = List (List.map dump_entity_statement x)
(*211*)   and dump_entity_declaration {
  entityname=identifier0;
  entityheader=entity_header1;
  entitydeclarations=entity_declarative_part2;
  entitystatements=entity_statement_part3;
} = Quintuple(Vhdentity_declaration,
	dump_identifier identifier0,
	dump_entity_header entity_header1,
	dump_entity_declarative_part entity_declarative_part2,
	dump_entity_statement_part entity_statement_part3)
(*212*)   and dump_architecture_declarative_part x = List (List.map dump_block_declarative_item x)
(*213*)   and dump_architecture_statement_part x = List (List.map dump_concurrent_statement x)
(*214*)   and dump_architecture_body {
  archname=identifier0;
  archentityname=simple_name1;
  archdeclarations=architecture_declarative_part2;
  archstatements=architecture_statement_part3;
} = Quintuple(Vhdarchitecture_body,
	dump_identifier identifier0,
	dump_simple_name simple_name1,
	dump_architecture_declarative_part architecture_declarative_part2,
	dump_architecture_statement_part architecture_statement_part3)
(*215*)   and dump_context_declaration {
  contextname=identifier0;
  contextclause=context_clause1;
} = Triple(Vhdcontext_declaration,
	dump_identifier identifier0,
	dump_context_clause context_clause1)
(*216*)   and dump_context_item = function
  | ContextLibraryClause library_clause -> Double(VhdContextLibraryClause,
	dump_library_clause library_clause)
  | ContextUseClause use_clause -> Double(VhdContextUseClause,
	dump_use_clause use_clause)
  | ContextContextReference context_reference -> Double(VhdContextContextReference,
	dump_context_reference context_reference)
(*217*)   and dump_context_clause x = List (List.map dump_context_item x)
(*218*)   and dump_context_reference x = dump_selected_name_list x
(*219*)   and dump_configuration_declaration {
  confname=identifier0;
  confentityname=name1;
  confdeclarations=configuration_declarative_part2;
  confblock=block_configuration3;
} = Quintuple(Vhdconfiguration_declaration,
	dump_identifier identifier0,
	dump_name name1,
	dump_configuration_declarative_part configuration_declarative_part2,
	dump_block_configuration block_configuration3)
(*220*)   and dump_configuration_declarative_part x = List (List.map dump_configuration_declarative_item x)
(*221*)   and dump_configuration_declarative_item = function
  | ConfigurationUseClause use_clause -> Double(VhdConfigurationUseClause,
	dump_use_clause use_clause)
  | ConfigurationAttributeSpecification attribute_specification -> Double(VhdConfigurationAttributeSpecification,
	dump_attribute_specification attribute_specification)
(*222*)   and dump_block_configuration {
  blockspec=block_specification0;
  blockuses=use_clause1;
  blockconfigurations=configuration_item2;
} = Quadruple(Vhdblock_configuration,
	dump_block_specification block_specification0,
	List (List.map dump_use_clause use_clause1),
	List (List.map dump_configuration_item configuration_item2))
(*223*)   and dump_block_specification = function
  | BlockSpecificationEmpty -> VhdBlockSpecificationEmpty
  | BlockSpecificationName name -> Double(VhdBlockSpecificationName,
	dump_name name)
  | BlockSpecificationNameIndex (name0,index_specification1) -> Triple(VhdBlockSpecificationNameIndex, dump_name name0
	, dump_index_specification index_specification1
	)
(*224*)   and dump_index_specification = function
  | IndexDiscreteRange discrete_range -> Double(VhdIndexDiscreteRange,
	dump_discrete_range discrete_range)
  | IndexStaticExpression expression -> Double(VhdIndexStaticExpression,
	dump_expression expression)
(*225*)   and dump_configuration_item = function
  | BlockConfiguration block_configuration -> Double(VhdBlockConfiguration,
	dump_block_configuration block_configuration)
  | ComponentConfiguration component_configuration -> Double(VhdComponentConfiguration,
	dump_component_configuration component_configuration)
(*226*)   and dump_component_configuration {
  componentspec=component_specification0;
  componentbinding=binding_indication1;
  componentblockconf=block_configuration2;
} = Quadruple(Vhdcomponent_configuration,
	dump_component_specification component_specification0,
	dump_binding_indication binding_indication1,
	dump_block_configuration block_configuration2)
(*227*)   and dump_secondary_unit = function
  | ArchitectureBody architecture_body -> Double(VhdArchitectureBody,
	dump_architecture_body architecture_body)
  | PackageBody package_body -> Double(VhdPackageBody,
	dump_package_body package_body)
(*228*)   and dump_primary_unit = function
  | EntityDeclaration entity_declaration -> Double(VhdEntityDeclaration,
	dump_entity_declaration entity_declaration)
  | ConfigurationDeclaration configuration_declaration -> Double(VhdConfigurationDeclaration,
	dump_configuration_declaration configuration_declaration)
  | PackageDeclaration package_declaration -> Double(VhdPackageDeclaration,
	dump_package_declaration package_declaration)
  | PackageInstantiation package_instantiation -> Double(VhdPackageInstantiation,
	dump_package_instantiation package_instantiation)
  | ContextDeclaration context_declaration -> Double(VhdContextDeclaration,
	dump_context_declaration context_declaration)
(*229*)   and dump_library_unit = function
  | PrimaryUnit primary_unit -> Double(VhdPrimaryUnit,
	dump_primary_unit primary_unit)
  | SecondaryUnit secondary_unit -> Double(VhdSecondaryUnit,
	dump_secondary_unit secondary_unit)
  | ProtectedUnit -> VhdProtectedUnit
(*230*)   and dump_design_unit {
  designunitcontextclause=context_clause0;
  designunitlibraryunit=library_unit1;
} = Triple(Vhddesign_unit,
	dump_context_clause context_clause0,
	dump_library_unit library_unit1)
(*231*)   and dump_design_file x = List (List.map dump_design_unit x)
(*232*)   and dump_parsed_file {
  parsedfilename=vhd_string0;
  parsedfilelibrary=vhd_string1;
  parsedfileunits=design_file2;
} = Quadruple(Vhdparsed_file,
	dump_vhd_string vhd_string0,
	dump_vhd_string vhd_string1,
	dump_design_file design_file2)
(*233*)   and dump_parsed_file_list x = List (List.map dump_parsed_file x)
