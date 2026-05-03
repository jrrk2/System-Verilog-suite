(* vhdl_to_ver_front.ml
 *
 * Convert Vivado-emitted structural VHDL into the ver_front token-tree
 * shape (Vparser.token), then insert into Globals.modprims so that
 * Ver_front_to_behavioral.module_to_bmodule processes it. This gets the
 * best of both worlds:
 *
 *   - VHDL preserves vector ports (no bit-blasted PRDATA outputs, no
 *     .NAME named-port shorthand to handle).
 *   - All the per-bit register grouping, RTL_* mapping, and bit-select
 *     handling already done in ver_front_to_behavioral is reused
 *     without modification.
 *
 * Vivado's `write_vhdl` always produces purely structural code (entity
 * + architecture with components and component instantiations only),
 * so the translation is mechanical — there are no behavioural
 * processes to worry about.
 *)

open Vhd_front.VhdlTree
open Ver_front
open Vparser

(* Strict mode (env MITER_STRICT=1): refuse to silently fall back to a
 * placeholder value for an unrecognised VHDL tree shape — instead raise
 * so the offending pattern can be added to the converter explicitly.
 * Permissive (default) keeps existing tests passing. *)
let strict_mode = lazy (Sys.getenv_opt "MITER_STRICT" <> None)

let vhd_kind = function
  | VhdNone -> "VhdNone" | Str _ -> "Str" | Char _ -> "Char"
  | Real _ -> "Real" | Num _ -> "Num" | List _ -> "List"
  | Double _ -> "Double" | Triple _ -> "Triple"
  | Quadruple _ -> "Quadruple" | Quintuple _ -> "Quintuple"
  | Sextuple _ -> "Sextuple" | Septuple _ -> "Septuple"
  | Octuple _ -> "Octuple" | _ -> "(other)"

(* Print the Vhd* constructor inside a Double/Triple/... node by name.
 *
 * Exhaustive: every constant constructor of vhdintf is listed, so when
 * vhd_libs/VhdlTree.ml grows a new one OCaml's pattern-match warning
 * gives us a printed catalogue of what to add. Generated mechanically
 * from VhdlTree.ml; do not edit by hand. To regenerate the cases:
 *
 *   grep -E '^  [|] Vhd[A-Z][a-zA-Z_]*' vhd_libs/VhdlTree.ml | ... etc
 *
 * Non-constant constructors are handled by vhd_kind / vhd_inner_kind
 * dispatch above. *)
let vhd_named_constant = function
  | VhdNone -> "VhdNone"
  | VhdAbsFactor -> "VhdAbsFactor"
  | VhdAccessTypeDefinition -> "VhdAccessTypeDefinition"
  | VhdActualDiscreteRange -> "VhdActualDiscreteRange"
  | VhdActualExpression -> "VhdActualExpression"
  | VhdActualOpen -> "VhdActualOpen"
  | VhdAddSimpleExpression -> "VhdAddSimpleExpression"
  | VhdAggregatePrimary -> "VhdAggregatePrimary"
  | VhdAlwaysLoop -> "VhdAlwaysLoop"
  | VhdAndFactor -> "VhdAndFactor"
  | VhdAndLogicalExpression -> "VhdAndLogicalExpression"
  | VhdArchitectureBody -> "VhdArchitectureBody"
  | VhdArrayConstraint -> "VhdArrayConstraint"
  | VhdArrayTypeDefinition -> "VhdArrayTypeDefinition"
  | VhdAttrNameRange -> "VhdAttrNameRange"
  | VhdAttributeName -> "VhdAttributeName"
  | VhdBindingConfiguration -> "VhdBindingConfiguration"
  | VhdBindingEntity -> "VhdBindingEntity"
  | VhdBindingEntityArchitecture -> "VhdBindingEntityArchitecture"
  | VhdBindingOpen -> "VhdBindingOpen"
  | VhdBlockAliasDeclaration -> "VhdBlockAliasDeclaration"
  | VhdBlockAttributeDeclaration -> "VhdBlockAttributeDeclaration"
  | VhdBlockAttributeSpecification -> "VhdBlockAttributeSpecification"
  | VhdBlockComponentDeclaration -> "VhdBlockComponentDeclaration"
  | VhdBlockConfiguration -> "VhdBlockConfiguration"
  | VhdBlockConfigurationSpecification -> "VhdBlockConfigurationSpecification"
  | VhdBlockConstantDeclaration -> "VhdBlockConstantDeclaration"
  | VhdBlockFileDeclaration -> "VhdBlockFileDeclaration"
  | VhdBlockSignalDeclaration -> "VhdBlockSignalDeclaration"
  | VhdBlockSpecificationEmpty -> "VhdBlockSpecificationEmpty"
  | VhdBlockSpecificationName -> "VhdBlockSpecificationName"
  | VhdBlockSpecificationNameIndex -> "VhdBlockSpecificationNameIndex"
  | VhdBlockSubProgramBody -> "VhdBlockSubProgramBody"
  | VhdBlockSubProgramDeclaration -> "VhdBlockSubProgramDeclaration"
  | VhdBlockSubProgramInstantiation -> "VhdBlockSubProgramInstantiation"
  | VhdBlockSubTypeDeclaration -> "VhdBlockSubTypeDeclaration"
  | VhdBlockTypeDeclaration -> "VhdBlockTypeDeclaration"
  | VhdBlockUseClause -> "VhdBlockUseClause"
  | VhdCaseGenerateStatement -> "VhdCaseGenerateStatement"
  | VhdCharLiteralEnumeration -> "VhdCharLiteralEnumeration"
  | VhdCharPrimary -> "VhdCharPrimary"
  | VhdChoiceDiscreteRange -> "VhdChoiceDiscreteRange"
  | VhdChoiceOthers -> "VhdChoiceOthers"
  | VhdChoiceSimpleExpression -> "VhdChoiceSimpleExpression"
  | VhdClassArchitecture -> "VhdClassArchitecture"
  | VhdClassComponent -> "VhdClassComponent"
  | VhdClassConfiguration -> "VhdClassConfiguration"
  | VhdClassConstant -> "VhdClassConstant"
  | VhdClassEntity -> "VhdClassEntity"
  | VhdClassFile -> "VhdClassFile"
  | VhdClassFunction -> "VhdClassFunction"
  | VhdClassLabel -> "VhdClassLabel"
  | VhdClassLiteral -> "VhdClassLiteral"
  | VhdClassPackage -> "VhdClassPackage"
  | VhdClassProcedure -> "VhdClassProcedure"
  | VhdClassSignal -> "VhdClassSignal"
  | VhdClassSubType -> "VhdClassSubType"
  | VhdClassType -> "VhdClassType"
  | VhdClassUnits -> "VhdClassUnits"
  | VhdClassVariable -> "VhdClassVariable"
  | VhdComponentConfiguration -> "VhdComponentConfiguration"
  | VhdConcatSimpleExpression -> "VhdConcatSimpleExpression"
  | VhdConcurrentAssertionStatement -> "VhdConcurrentAssertionStatement"
  | VhdConcurrentBlockStatement -> "VhdConcurrentBlockStatement"
  | VhdConcurrentComponentInstantiationStatement -> "VhdConcurrentComponentInstantiationStatement"
  | VhdConcurrentConditionalSignalAssignment -> "VhdConcurrentConditionalSignalAssignment"
  | VhdConcurrentGenerateStatement -> "VhdConcurrentGenerateStatement"
  | VhdConcurrentProcedureCallStatement -> "VhdConcurrentProcedureCallStatement"
  | VhdConcurrentProcessStatement -> "VhdConcurrentProcessStatement"
  | VhdConcurrentSelectedSignalAssignment -> "VhdConcurrentSelectedSignalAssignment"
  | VhdConcurrentSignalAssignmentStatement -> "VhdConcurrentSignalAssignmentStatement"
  | VhdConcurrentSimpleSignalAssignment -> "VhdConcurrentSimpleSignalAssignment"
  | VhdCondition -> "VhdCondition"
  | VhdConditionExpression -> "VhdConditionExpression"
  | VhdConditionalSignalAssignment -> "VhdConditionalSignalAssignment"
  | VhdConditionalVariableAssignment -> "VhdConditionalVariableAssignment"
  | VhdConfigurationAttributeSpecification -> "VhdConfigurationAttributeSpecification"
  | VhdConfigurationDeclaration -> "VhdConfigurationDeclaration"
  | VhdConfigurationUseClause -> "VhdConfigurationUseClause"
  | VhdConstantDeclaration -> "VhdConstantDeclaration"
  | VhdConstrainedArray -> "VhdConstrainedArray"
  | VhdContextContextReference -> "VhdContextContextReference"
  | VhdContextDeclaration -> "VhdContextDeclaration"
  | VhdContextLibraryClause -> "VhdContextLibraryClause"
  | VhdContextUseClause -> "VhdContextUseClause"
  | VhdDecreasingRange -> "VhdDecreasingRange"
  | VhdDelayInertial -> "VhdDelayInertial"
  | VhdDelayNone -> "VhdDelayNone"
  | VhdDelayTransport -> "VhdDelayTransport"
  | VhdDesignatorCharacter -> "VhdDesignatorCharacter"
  | VhdDesignatorIdentifier -> "VhdDesignatorIdentifier"
  | VhdDesignatorOperator -> "VhdDesignatorOperator"
  | VhdDivTerm -> "VhdDivTerm"
  | VhdElse -> "VhdElse"
  | VhdElseNone -> "VhdElseNone"
  | VhdElsif -> "VhdElsif"
  | VhdEntityAliasDeclaration -> "VhdEntityAliasDeclaration"
  | VhdEntityAll -> "VhdEntityAll"
  | VhdEntityAttributeDeclaration -> "VhdEntityAttributeDeclaration"
  | VhdEntityAttributeSpecification -> "VhdEntityAttributeSpecification"
  | VhdEntityConcurrentAssertionStatement -> "VhdEntityConcurrentAssertionStatement"
  | VhdEntityConcurrentProcedureCallStatement -> "VhdEntityConcurrentProcedureCallStatement"
  | VhdEntityConstantDeclaration -> "VhdEntityConstantDeclaration"
  | VhdEntityDeclaration -> "VhdEntityDeclaration"
  | VhdEntityFileDeclaration -> "VhdEntityFileDeclaration"
  | VhdEntityOthers -> "VhdEntityOthers"
  | VhdEntityProcessStatement -> "VhdEntityProcessStatement"
  | VhdEntitySignalDeclaration -> "VhdEntitySignalDeclaration"
  | VhdEntitySubProgramBody -> "VhdEntitySubProgramBody"
  | VhdEntitySubProgramDeclaration -> "VhdEntitySubProgramDeclaration"
  | VhdEntitySubProgramInstantiation -> "VhdEntitySubProgramInstantiation"
  | VhdEntitySubTypeDeclaration -> "VhdEntitySubTypeDeclaration"
  | VhdEntityTagCharacterLiteral -> "VhdEntityTagCharacterLiteral"
  | VhdEntityTagOperatorSymbol -> "VhdEntityTagOperatorSymbol"
  | VhdEntityTagSimpleName -> "VhdEntityTagSimpleName"
  | VhdEntityTypeDeclaration -> "VhdEntityTypeDeclaration"
  | VhdEntityUseClause -> "VhdEntityUseClause"
  | VhdEnumerationTypeDefinition -> "VhdEnumerationTypeDefinition"
  | VhdEqualRelation -> "VhdEqualRelation"
  | VhdExpFactor -> "VhdExpFactor"
  | VhdFileDeclaration -> "VhdFileDeclaration"
  | VhdFileTypeDefinition -> "VhdFileTypeDefinition"
  | VhdFloatPrimary -> "VhdFloatPrimary"
  | VhdForGenerateStatement -> "VhdForGenerateStatement"
  | VhdForLoop -> "VhdForLoop"
  | VhdFormalExpression -> "VhdFormalExpression"
  | VhdFormalIndexed -> "VhdFormalIndexed"
  | VhdFullType -> "VhdFullType"
  | VhdFunction -> "VhdFunction"
  | VhdFunctionSpecification -> "VhdFunctionSpecification"
  | VhdGenerateElse -> "VhdGenerateElse"
  | VhdGenerateElseNone -> "VhdGenerateElseNone"
  | VhdGenerateElsif -> "VhdGenerateElsif"
  | VhdGreaterOrEqualRelation -> "VhdGreaterOrEqualRelation"
  | VhdGreaterRelation -> "VhdGreaterRelation"
  | VhdIdentifierEnumeration -> "VhdIdentifierEnumeration"
  | VhdIfGenerateStatement -> "VhdIfGenerateStatement"
  | VhdImpure -> "VhdImpure"
  | VhdIncompleteType -> "VhdIncompleteType"
  | VhdIncreasingRange -> "VhdIncreasingRange"
  | VhdIndexDiscreteRange -> "VhdIndexDiscreteRange"
  | VhdIndexStaticExpression -> "VhdIndexStaticExpression"
  | VhdInstantiatedComponent -> "VhdInstantiatedComponent"
  | VhdInstantiatedConfiguration -> "VhdInstantiatedConfiguration"
  | VhdInstantiatedEntityArchitecture -> "VhdInstantiatedEntityArchitecture"
  | VhdInstantiationAll -> "VhdInstantiationAll"
  | VhdInstantiationOthers -> "VhdInstantiationOthers"
  | VhdIntPrimary -> "VhdIntPrimary"
  | VhdInterfaceConstantDeclaration -> "VhdInterfaceConstantDeclaration"
  | VhdInterfaceDefaultDeclaration -> "VhdInterfaceDefaultDeclaration"
  | VhdInterfaceFileDeclaration -> "VhdInterfaceFileDeclaration"
  | VhdInterfaceIncompleteTypeDeclaration -> "VhdInterfaceIncompleteTypeDeclaration"
  | VhdInterfaceModeBuffer -> "VhdInterfaceModeBuffer"
  | VhdInterfaceModeIn -> "VhdInterfaceModeIn"
  | VhdInterfaceModeInOut -> "VhdInterfaceModeInOut"
  | VhdInterfaceModeOut -> "VhdInterfaceModeOut"
  | VhdInterfaceObjectDeclaration -> "VhdInterfaceObjectDeclaration"
  | VhdInterfaceSignalDeclaration -> "VhdInterfaceSignalDeclaration"
  | VhdInterfaceTypeDeclaration -> "VhdInterfaceTypeDeclaration"
  | VhdInterfaceVariableDeclaration -> "VhdInterfaceVariableDeclaration"
  | VhdLdotted -> "VhdLdotted"
  | VhdLessOrEqualRelation -> "VhdLessOrEqualRelation"
  | VhdLessRelation -> "VhdLessRelation"
  | VhdMatchingEqualRelation -> "VhdMatchingEqualRelation"
  | VhdMatchingGreaterOrEqualRelation -> "VhdMatchingGreaterOrEqualRelation"
  | VhdMatchingGreaterRelation -> "VhdMatchingGreaterRelation"
  | VhdMatchingLessOrEqualRelation -> "VhdMatchingLessOrEqualRelation"
  | VhdMatchingLessRelation -> "VhdMatchingLessRelation"
  | VhdMatchingNotEqualRelation -> "VhdMatchingNotEqualRelation"
  | VhdMatchingSelection -> "VhdMatchingSelection"
  | VhdModTerm -> "VhdModTerm"
  | VhdMultTerm -> "VhdMultTerm"
  | VhdNameParametersPrimary -> "VhdNameParametersPrimary"
  | VhdNamePrimary -> "VhdNamePrimary"
  | VhdNandFactor -> "VhdNandFactor"
  | VhdNandLogicalExpression -> "VhdNandLogicalExpression"
  | VhdNegSimpleExpression -> "VhdNegSimpleExpression"
  | VhdNewFactor -> "VhdNewFactor"
  | VhdNoConstraint -> "VhdNoConstraint"
  | VhdNorFactor -> "VhdNorFactor"
  | VhdNorLogicalExpression -> "VhdNorLogicalExpression"
  | VhdNotEqualRelation -> "VhdNotEqualRelation"
  | VhdNotFactor -> "VhdNotFactor"
  | VhdOperatorString -> "VhdOperatorString"
  | VhdOrFactor -> "VhdOrFactor"
  | VhdOrLogicalExpression -> "VhdOrLogicalExpression"
  | VhdOrdinarySelection -> "VhdOrdinarySelection"
  | VhdPackageAliasDeclaration -> "VhdPackageAliasDeclaration"
  | VhdPackageAttributeDeclaration -> "VhdPackageAttributeDeclaration"
  | VhdPackageAttributeSpecification -> "VhdPackageAttributeSpecification"
  | VhdPackageBody -> "VhdPackageBody"
  | VhdPackageBodyAliasDeclaration -> "VhdPackageBodyAliasDeclaration"
  | VhdPackageBodyConstantDeclaration -> "VhdPackageBodyConstantDeclaration"
  | VhdPackageBodyFileDeclaration -> "VhdPackageBodyFileDeclaration"
  | VhdPackageBodySubProgramBody -> "VhdPackageBodySubProgramBody"
  | VhdPackageBodySubProgramDeclaration -> "VhdPackageBodySubProgramDeclaration"
  | VhdPackageBodySubProgramInstantiation -> "VhdPackageBodySubProgramInstantiation"
  | VhdPackageBodySubTypeDeclaration -> "VhdPackageBodySubTypeDeclaration"
  | VhdPackageBodyTypeDeclaration -> "VhdPackageBodyTypeDeclaration"
  | VhdPackageBodyUseClause -> "VhdPackageBodyUseClause"
  | VhdPackageComponentDeclaration -> "VhdPackageComponentDeclaration"
  | VhdPackageConstantDeclaration -> "VhdPackageConstantDeclaration"
  | VhdPackageDeclaration -> "VhdPackageDeclaration"
  | VhdPackageFileDeclaration -> "VhdPackageFileDeclaration"
  | VhdPackageInstantiation -> "VhdPackageInstantiation"
  | VhdPackageSignalDeclaration -> "VhdPackageSignalDeclaration"
  | VhdPackageSubProgramDeclaration -> "VhdPackageSubProgramDeclaration"
  | VhdPackageSubProgramInstantiation -> "VhdPackageSubProgramInstantiation"
  | VhdPackageSubTypeDeclaration -> "VhdPackageSubTypeDeclaration"
  | VhdPackageTypeDeclaration -> "VhdPackageTypeDeclaration"
  | VhdPackageUseClause -> "VhdPackageUseClause"
  | VhdParenthesedPrimary -> "VhdParenthesedPrimary"
  | VhdPhysicalEmpty -> "VhdPhysicalEmpty"
  | VhdPhysicalFloat -> "VhdPhysicalFloat"
  | VhdPhysicalInteger -> "VhdPhysicalInteger"
  | VhdPhysicalPrimary -> "VhdPhysicalPrimary"
  | VhdPhysicalTypeDefinition -> "VhdPhysicalTypeDefinition"
  | VhdPrefixName -> "VhdPrefixName"
  | VhdPrimaryUnit -> "VhdPrimaryUnit"
  | VhdProcedure -> "VhdProcedure"
  | VhdProcedureSpecification -> "VhdProcedureSpecification"
  | VhdProcessAliasDeclaration -> "VhdProcessAliasDeclaration"
  | VhdProcessAttributeDeclaration -> "VhdProcessAttributeDeclaration"
  | VhdProcessAttributeSpecification -> "VhdProcessAttributeSpecification"
  | VhdProcessConstantDeclaration -> "VhdProcessConstantDeclaration"
  | VhdProcessFileDeclaration -> "VhdProcessFileDeclaration"
  | VhdProcessSubProgramBody -> "VhdProcessSubProgramBody"
  | VhdProcessSubProgramDeclaration -> "VhdProcessSubProgramDeclaration"
  | VhdProcessSubProgramInstantiation -> "VhdProcessSubProgramInstantiation"
  | VhdProcessSubTypeDeclaration -> "VhdProcessSubTypeDeclaration"
  | VhdProcessTypeDeclaration -> "VhdProcessTypeDeclaration"
  | VhdProcessUseClause -> "VhdProcessUseClause"
  | VhdProcessVariableDeclaration -> "VhdProcessVariableDeclaration"
  | VhdProtectedUnit -> "VhdProtectedUnit"
  | VhdPure -> "VhdPure"
  | VhdQualifiedAggregate -> "VhdQualifiedAggregate"
  | VhdQualifiedExpression -> "VhdQualifiedExpression"
  | VhdQualifiedExpressionPrimary -> "VhdQualifiedExpressionPrimary"
  | VhdRange -> "VhdRange"
  | VhdRangeConstraint -> "VhdRangeConstraint"
  | VhdRangeTypeDefinition -> "VhdRangeTypeDefinition"
  | VhdRecordTypeDefinition -> "VhdRecordTypeDefinition"
  | VhdRemTerm -> "VhdRemTerm"
  | VhdRotateLeftExpression -> "VhdRotateLeftExpression"
  | VhdRotateRightExpression -> "VhdRotateRightExpression"
  | VhdSecondaryUnit -> "VhdSecondaryUnit"
  | VhdSelectTargetName -> "VhdSelectTargetName"
  | VhdSelectTargetNameParameters -> "VhdSelectTargetNameParameters"
  | VhdSelectedName -> "VhdSelectedName"
  | VhdSelectedSignalAssignment -> "VhdSelectedSignalAssignment"
  | VhdSelectedVariableAssignment -> "VhdSelectedVariableAssignment"
  | VhdSelector -> "VhdSelector"
  | VhdSensitivityAll -> "VhdSensitivityAll"
  | VhdSensitivityExpressionList -> "VhdSensitivityExpressionList"
  | VhdSequentialAssertion -> "VhdSequentialAssertion"
  | VhdSequentialCase -> "VhdSequentialCase"
  | VhdSequentialExit -> "VhdSequentialExit"
  | VhdSequentialIf -> "VhdSequentialIf"
  | VhdSequentialLoop -> "VhdSequentialLoop"
  | VhdSequentialNext -> "VhdSequentialNext"
  | VhdSequentialNull -> "VhdSequentialNull"
  | VhdSequentialProcedureCall -> "VhdSequentialProcedureCall"
  | VhdSequentialReport -> "VhdSequentialReport"
  | VhdSequentialReturn -> "VhdSequentialReturn"
  | VhdSequentialSignalAssignment -> "VhdSequentialSignalAssignment"
  | VhdSequentialVariableAssignment -> "VhdSequentialVariableAssignment"
  | VhdSequentialWait -> "VhdSequentialWait"
  | VhdShiftLeftArithmeticExpression -> "VhdShiftLeftArithmeticExpression"
  | VhdShiftLeftLogicalExpression -> "VhdShiftLeftLogicalExpression"
  | VhdShiftRightArithmeticExpression -> "VhdShiftRightArithmeticExpression"
  | VhdShiftRightLogicalExpression -> "VhdShiftRightLogicalExpression"
  | VhdSignalDeclaration -> "VhdSignalDeclaration"
  | VhdSignalKindBus -> "VhdSignalKindBus"
  | VhdSignalKindDefault -> "VhdSignalKindDefault"
  | VhdSignalKindRegister -> "VhdSignalKindRegister"
  | VhdSimpleName -> "VhdSimpleName"
  | VhdSimpleSignalAssignment -> "VhdSimpleSignalAssignment"
  | VhdSimpleVariableAssignment -> "VhdSimpleVariableAssignment"
  | VhdSubProgramAliasDeclaration -> "VhdSubProgramAliasDeclaration"
  | VhdSubProgramAttributeDeclaration -> "VhdSubProgramAttributeDeclaration"
  | VhdSubProgramAttributeSpecification -> "VhdSubProgramAttributeSpecification"
  | VhdSubProgramBody -> "VhdSubProgramBody"
  | VhdSubProgramConstantDeclaration -> "VhdSubProgramConstantDeclaration"
  | VhdSubProgramDeclaration -> "VhdSubProgramDeclaration"
  | VhdSubProgramFileDeclaration -> "VhdSubProgramFileDeclaration"
  | VhdSubProgramInstantiation -> "VhdSubProgramInstantiation"
  | VhdSubProgramSubTypeDeclaration -> "VhdSubProgramSubTypeDeclaration"
  | VhdSubProgramTypeDeclaration -> "VhdSubProgramTypeDeclaration"
  | VhdSubProgramUseClause -> "VhdSubProgramUseClause"
  | VhdSubProgramVariableDeclaration -> "VhdSubProgramVariableDeclaration"
  | VhdSubSimpleExpression -> "VhdSubSimpleExpression"
  | VhdSubTypeRange -> "VhdSubTypeRange"
  | VhdSubscriptName -> "VhdSubscriptName"
  | VhdSuffixAll -> "VhdSuffixAll"
  | VhdSuffixCharLiteral -> "VhdSuffixCharLiteral"
  | VhdSuffixOpSymbol -> "VhdSuffixOpSymbol"
  | VhdSuffixSimpleName -> "VhdSuffixSimpleName"
  | VhdTargetAggregate -> "VhdTargetAggregate"
  | VhdTargetInvalid -> "VhdTargetInvalid"
  | VhdTargetName -> "VhdTargetName"
  | VhdTargetDotted -> "VhdTargetDotted"
  | VhdTargetNameParameters -> "VhdTargetNameParameters"
  | VhdUnaffected -> "VhdUnaffected"
  | VhdUnboundedArray -> "VhdUnboundedArray"
  | VhdUnknown -> "VhdUnknown"
  | VhdVariableDeclaration -> "VhdVariableDeclaration"
  | VhdWhileLoop -> "VhdWhileLoop"
  | VhdXnorFactor -> "VhdXnorFactor"
  | VhdXnorLogicalExpression -> "VhdXnorLogicalExpression"
  | VhdXorFactor -> "VhdXorFactor"
  | VhdXorLogicalExpression -> "VhdXorLogicalExpression"
  (* Block (payload-bearing) constructors of vhdintf. These shouldn't
   * normally appear as the head of a Double/Triple — vhd_inner_kind
   * extracts the head expecting a constant — but OCaml's exhaustiveness
   * check insists we list them, which is the whole point: the day
   * VhdlTree.ml grows a new constructor we'll fail to compile and have
   * to add the case. Better than a silent fall-through. *)
  | Str _ -> "Str" | Char _ -> "Char" | Real _ -> "Real"
  | Num _ -> "Num" | List _ -> "List"
  | Double _ -> "Double" | Triple _ -> "Triple"
  | Quadruple _ -> "Quadruple" | Quintuple _ -> "Quintuple"
  | Sextuple _ -> "Sextuple" | Septuple _ -> "Septuple"
  | Octuple _ -> "Octuple" | Nonuple _ -> "Nonuple"
  | Decuple _ -> "Decuple" | Undecuple _ -> "Undecuple"
  | Duodecuple _ -> "Duodecuple" | Tredecuple _ -> "Tredecuple"
  | Quattuordecuple _ -> "Quattuordecuple" | Quindecuple _ -> "Quindecuple"
  | Sexdecuple _ -> "Sexdecuple" | Septendecuple _ -> "Septendecuple"
  | Octodecuple _ -> "Octodecuple" | Novemdecuple _ -> "Novemdecuple"
  | Vigenuple _ -> "Vigenuple" | Unvigenuple _ -> "Unvigenuple"
  | Duovigenuple _ -> "Duovigenuple" | Trevigenuple _ -> "Trevigenuple"
  | Quattuorvigenuple _ -> "Quattuorvigenuple"
  | Quinvigenuple _ -> "Quinvigenuple"
  | Vhdalias_declaration -> "Vhdalias_declaration"
  | Vhdarchitecture_body -> "Vhdarchitecture_body"
  | Vhdassertion -> "Vhdassertion"
  | Vhdassertion_statement -> "Vhdassertion_statement"
  | Vhdassociation_element -> "Vhdassociation_element"
  | Vhdattribute_declaration -> "Vhdattribute_declaration"
  | Vhdattribute_name -> "Vhdattribute_name"
  | Vhdattribute_specification -> "Vhdattribute_specification"
  | Vhdbinding_indication -> "Vhdbinding_indication"
  | Vhdblock_configuration -> "Vhdblock_configuration"
  | Vhdblock_statement -> "Vhdblock_statement"
  | Vhdcase_generate_alternative -> "Vhdcase_generate_alternative"
  | Vhdcase_generate_statement -> "Vhdcase_generate_statement"
  | Vhdcase_statement -> "Vhdcase_statement"
  | Vhdcase_statement_alternative -> "Vhdcase_statement_alternative"
  | Vhdcomponent_configuration -> "Vhdcomponent_configuration"
  | Vhdcomponent_declaration -> "Vhdcomponent_declaration"
  | Vhdcomponent_instantiation_statement -> "Vhdcomponent_instantiation_statement"
  | Vhdcomponent_specification -> "Vhdcomponent_specification"
  | Vhdconcurrent_assertion_statement -> "Vhdconcurrent_assertion_statement"
  | Vhdconcurrent_conditional_signal_assignment -> "Vhdconcurrent_conditional_signal_assignment"
  | Vhdconcurrent_procedure_call_statement -> "Vhdconcurrent_procedure_call_statement"
  | Vhdconcurrent_selected_signal_assignment -> "Vhdconcurrent_selected_signal_assignment"
  | Vhdconcurrent_signal_assignment_statement -> "Vhdconcurrent_signal_assignment_statement"
  | Vhdconcurrent_simple_signal_assignment -> "Vhdconcurrent_simple_signal_assignment"
  | Vhdconditional_expression -> "Vhdconditional_expression"
  | Vhdconditional_signal_assignment_statement -> "Vhdconditional_signal_assignment_statement"
  | Vhdconditional_variable_assignment -> "Vhdconditional_variable_assignment"
  | Vhdconditional_waveform -> "Vhdconditional_waveform"
  | Vhdconfiguration_declaration -> "Vhdconfiguration_declaration"
  | Vhdconfiguration_specification -> "Vhdconfiguration_specification"
  | Vhdconstant_declaration -> "Vhdconstant_declaration"
  | Vhdconstrained_array_definition -> "Vhdconstrained_array_definition"
  | Vhdcontext_declaration -> "Vhdcontext_declaration"
  | Vhddesign_unit -> "Vhddesign_unit"
  | Vhdelement_association -> "Vhdelement_association"
  | Vhdelement_declaration -> "Vhdelement_declaration"
  | Vhdentity_declaration -> "Vhdentity_declaration"
  | Vhdentity_designator -> "Vhdentity_designator"
  | Vhdentity_header -> "Vhdentity_header"
  | Vhdentity_specification -> "Vhdentity_specification"
  | Vhdexit_statement -> "Vhdexit_statement"
  | Vhdfile_declaration -> "Vhdfile_declaration"
  | Vhdfor_generate_statement -> "Vhdfor_generate_statement"
  | Vhdfull_type_declaration -> "Vhdfull_type_declaration"
  | Vhdfunction_specification -> "Vhdfunction_specification"
  | Vhdif_generate_statement -> "Vhdif_generate_statement"
  | Vhdif_statement -> "Vhdif_statement"
  | Vhdincomplete_type_declaration -> "Vhdincomplete_type_declaration"
  | Vhdinterface_constant_declaration -> "Vhdinterface_constant_declaration"
  | Vhdinterface_default_declaration -> "Vhdinterface_default_declaration"
  | Vhdinterface_file_declaration -> "Vhdinterface_file_declaration"
  | Vhdinterface_signal_declaration -> "Vhdinterface_signal_declaration"
  | Vhdinterface_variable_declaration -> "Vhdinterface_variable_declaration"
  | Vhdloop_statement -> "Vhdloop_statement"
  | Vhdnext_statement -> "Vhdnext_statement"
  | Vhdnull_statement -> "Vhdnull_statement"
  | Vhdpackage_body -> "Vhdpackage_body"
  | Vhdpackage_declaration -> "Vhdpackage_declaration"
  | Vhdpackage_instantiation -> "Vhdpackage_instantiation"
  | Vhdparameter_specification -> "Vhdparameter_specification"
  | Vhdparsed_file -> "Vhdparsed_file"
  | Vhdphysical_type_definition -> "Vhdphysical_type_definition"
  | Vhdprocedure_call -> "Vhdprocedure_call"
  | Vhdprocedure_call_statement -> "Vhdprocedure_call_statement"
  | Vhdprocedure_specification -> "Vhdprocedure_specification"
  | Vhdprocess_statement -> "Vhdprocess_statement"
  | Vhdrecord_type_definition -> "Vhdrecord_type_definition"
  | Vhdreport_statement -> "Vhdreport_statement"
  | Vhdreturn_statement -> "Vhdreturn_statement"
  | Vhdselected_expression -> "Vhdselected_expression"
  | Vhdselected_signal_assignment_statement -> "Vhdselected_signal_assignment_statement"
  | Vhdselected_variable_assignment -> "Vhdselected_variable_assignment"
  | Vhdselected_waveform -> "Vhdselected_waveform"
  | Vhdsignal_declaration -> "Vhdsignal_declaration"
  | Vhdsignature -> "Vhdsignature"
  | Vhdsimple_signal_assignment_statement -> "Vhdsimple_signal_assignment_statement"
  | Vhdsimple_variable_assignment -> "Vhdsimple_variable_assignment"
  | Vhdsubprogram_body -> "Vhdsubprogram_body"
  | Vhdsubprogram_instantiation -> "Vhdsubprogram_instantiation"
  | Vhdsubtype_declaration -> "Vhdsubtype_declaration"
  | Vhdsubtype_indication -> "Vhdsubtype_indication"
  | Vhdunbounded_array_definition -> "Vhdunbounded_array_definition"
  | Vhdvariable_declaration -> "Vhdvariable_declaration"
  | Vhdwait_statement -> "Vhdwait_statement"
  | Vhdwaveform_element -> "Vhdwaveform_element"
let vhd_inner_kind = function
  | Double (a, _) | Triple (a, _, _) | Quadruple (a, _, _, _)
  | Quintuple (a, _, _, _, _) | Sextuple (a, _, _, _, _, _)
  | Septuple (a, _, _, _, _, _, _) | Octuple (a, _, _, _, _, _, _, _) ->
      vhd_named_constant a
  | other -> vhd_kind other

let strict_bail kind context node =
  let msg =
    Printf.sprintf "[vhdl_to_ver_front] unrecognised %s in %s: %s (%s)\n\
                    set MITER_STRICT=0 (or unset) to suppress"
      kind context (vhd_kind node) (vhd_inner_kind node)
  in
  if Lazy.force strict_mode then failwith msg
  else if Sys.getenv_opt "MITER_VERBOSE" <> None then
    Printf.eprintf "Warning: %s\n" msg

(* ─── Port direction translation ──────────────────────────────────────── *)

let port_direction = function
  | VhdInterfaceModeIn     -> INPUT
  | VhdInterfaceModeOut    -> OUTPUT
  | VhdInterfaceModeInOut  -> INOUT
  | VhdInterfaceModeBuffer -> OUTPUT
  | _                      -> INPUT

(* ─── Width / range translation ───────────────────────────────────────── *)

let mk_id name = ID { Idhash.id = name }

(* Recover the integer literal value buried in a VHDL expression node. *)
let rec extract_int = function
  | Num s -> (try Some (int_of_string s) with _ -> None)
  | Double (_, x) -> extract_int x
  | Triple (_, x, _) -> extract_int x
  | _ -> None

(* Deep-search the VHDL subtype tree for the first range-style Triple
 * (VhdIncreasingRange / VhdDecreasingRange) and return (msb, lsb). *)
let rec find_range = function
  | Triple (VhdDecreasingRange, hi, lo)
  | Triple (VhdIncreasingRange, lo, hi) ->
      (match extract_int hi, extract_int lo with
       | Some h, Some l -> Some (max h l, min h l)
       | _ -> None)
  | Double (_, x) -> find_range x
  | Triple (_, a, b) ->
      (match find_range a with Some r -> Some r | None -> find_range b)
  | Quadruple (_, a, b, c) ->
      (match find_range a with
       | Some r -> Some r
       | None ->
           (match find_range b with
            | Some r -> Some r
            | None -> find_range c))
  | List xs ->
      List.fold_left (fun acc x ->
        match acc with Some _ -> acc | None -> find_range x
      ) None xs
  | _ -> None

let range_of_subtype subtype =
  match find_range subtype with
  | Some (msb, lsb) -> RANGE (INT msb, INT lsb)
  | None -> EMPTY

(* ─── Name extraction ─────────────────────────────────────────────────── *)

(* VHDL escaped identifiers are written `\name\` — the parser preserves
 * the leading and trailing backslashes inside the Str token. Strip them
 * so downstream matchers see plain names. *)
let unescape s =
  let n = String.length s in
  if n >= 2 && s.[0] = '\\' && s.[n - 1] = '\\'
  then String.sub s 1 (n - 2)
  else s

(* VHDL names come in many wrapped forms; peel off the layers and return
 * the bare string. For a dotted name like `unisim.vcomponents.IBUF` we
 * want the last segment (the actual cell name), not the library prefix. *)
let rec name_string = function
  | Str s -> unescape s
  | Double (VhdSimpleName, n) -> name_string n
  | Double (VhdSelectedName, List xs) ->
      (* Dotted name like `unisim.vcomponents.IBUF`. The vhd_front parser
       * stores suffixes leaf-first (innermost qualified name comes first
       * in the list), so the leaf cell name is the head. *)
      (match xs with
       | first :: _ -> name_string first
       | [] -> "")
  | Double (VhdSelectedName, n) -> name_string n
  | Double (VhdSuffixSimpleName, n) -> name_string n
  | Double (_, n) -> name_string n
  | Triple (_, n, _) -> name_string n
  | _ -> ""

let identifier_string = function
  | Str s -> unescape s
  | other -> name_string other

(* ─── Expression translation ──────────────────────────────────────────── *)

(* Translate a VHDL "actual part" / expression to a Vparser.token suitable
 * as a cell-pin connection. Handles plain names, bit-select indexed
 * names, and the rewrite-normalised forms (where a name reference is
 * wrapped in NameParametersPrimary with an association_element). *)
let rec actual_to_vparser = function
  | Str s -> mk_id (unescape s)
  | Double (VhdSimpleName, Str s) -> mk_id (unescape s)
  | Double (VhdActualExpression, e) -> actual_to_vparser e
  | Double (VhdAggregatePrimary, e) -> actual_to_vparser e
  | Double (VhdNamePrimary, e) -> actual_to_vparser e
  (* Waveform element: `expr [after time]`. Vivado emits these for
   * default-driven concurrent assignments. The waveform body is just
   * an expression at the actual_part level — recurse. *)
  | Double (Vhdwaveform_element, e) -> actual_to_vparser e
  | Double (VhdActualDiscreteRange, _) -> EMPTY
  (* Character literal `'0'`/`'1'` — Vivado uses these for tied-low/high
   * pin connections (e.g., the I2 signedness flag of RTL_ARSHIFT,
   * tied-zero data inputs to RAMB36E1, etc.). Convert to a 1-bit
   * BINNUM that ver_front_to_behavioral.expr_to_bexpr handles. *)
  | Double (VhdCharPrimary, Char c) ->
      BINNUM (Printf.sprintf "1'b%c" c)
  (* Integer literal — emit as DECNUM for downstream BIR encoding. *)
  | Double (VhdIntPrimary, Num s) -> DECNUM s
  (* Bit-string literal like `8'h00` — Vivado emits these for wide
   * tied-zero inputs. Comes through as VhdOperatorString of a
   * "0"/"1" string of N chars (treated as N-bit binary). *)
  | Double (VhdOperatorString, Str s) ->
      BINNUM (Printf.sprintf "%d'b%s" (String.length s) s)
  (* Bare aggregate `(others => '0')` for wide constants. Approximate
   * as an unsized zero — z3_miter's BConst with width=1 will widen at
   * the assignment boundary. *)
  | VhdAggregatePrimary -> BINNUM "1'b0"
  | VhdActualOpen -> EMPTY
  (* Indexed name: q(0), x(3). The pre-rewrite shape is
   *   Triple(VhdSubscriptName, simple_name, Double(VhdSelector, expr))
   * Post-rewrite (after abstraction) a function-style indexed access
   * shows up as
   *   Triple(VhdNameParametersPrimary, Str base,
   *          Triple(Vhdassociation_element, VhdFormalIndexed,
   *                 Double(VhdActualExpression, idx_expr)))
   *)
  | Triple (VhdSubscriptName, prefix,
            Double (VhdSelector, idx_expr)) ->
      let base = name_string prefix in
      (match extract_int idx_expr with
       | Some i -> TRIPLE (BITSEL, ID { Idhash.id = base }, INT i)
       | None -> mk_id base)
  | Triple (VhdNameParametersPrimary, Str base,
            Triple (Vhdassociation_element, _,
                    Double (VhdActualExpression, idx_expr))) ->
      let base = unescape base in
      (match extract_int idx_expr with
       | Some i -> TRIPLE (BITSEL, ID { Idhash.id = base }, INT i)
       | None -> mk_id base)
  | Triple (VhdNameParametersPrimary, Str base,
            Triple (Vhdassociation_element, _,
                    Double (VhdActualDiscreteRange, _))) ->
      mk_id (unescape base)
  | Double (VhdSelectedName, n) -> mk_id (name_string n)
  | other ->
      (* Recover *some* identifier if possible — many wrapper shapes
       * still resolve to a meaningful name via name_string. Only bail
       * when we genuinely have nothing. *)
      let s = name_string other in
      if s <> "" then mk_id s
      else begin
        strict_bail "actual_part" "actual_to_vparser" other;
        EMPTY
      end

(* ─── Building ver_front tree fragments ───────────────────────────────── *)

(* A port declaration in ver_front looks like:
 *   QUINTUPLE(direction, EMPTY, EMPTY, range, TLIST [TRIPLE(ID name, EMPTY, EMPTY)])
 *)
let mk_port_decl direction range name =
  QUINTUPLE (
    direction,
    EMPTY, EMPTY, range,
    TLIST [TRIPLE (ID { Idhash.id = name }, EMPTY, EMPTY)])

(* A wire declaration in ver_front looks like:
 *   QUADRUPLE(WIRE, EMPTY, TRIPLE(EMPTY, range, EMPTY), TLIST [DOUBLE(ID name, EMPTY)])
 *)
let mk_wire_decl range name =
  QUADRUPLE (
    WIRE,
    EMPTY,
    TRIPLE (EMPTY, range, EMPTY),
    TLIST [DOUBLE (ID { Idhash.id = name }, EMPTY)])

(* A cell instantiation in ver_front looks like:
 *   QUADRUPLE(MODINST, ID cell_type, EMPTY, TLIST [
 *     TRIPLE(ID inst_name, SCALAR, TLIST [
 *       TRIPLE(CELLPIN, ID pin_name, expr); ...
 *     ])
 *   ])
 *)
let mk_modinst cell_type inst_name pins =
  let pin_tokens = List.map (fun (pin_name, expr) ->
    TRIPLE (CELLPIN, ID { Idhash.id = pin_name }, expr)
  ) pins in
  QUADRUPLE (
    MODINST,
    ID { Idhash.id = cell_type },
    EMPTY,
    TLIST [TRIPLE (ID { Idhash.id = inst_name }, SCALAR, TLIST pin_tokens)])

(* ─── VHDL tree walk: collect entities and architectures ─────────────── *)

type entity_info = {
  ent_name: string;
  ent_ports: (string * Vparser.token * Vparser.token) list;
    (* (port_name, direction_token, range_token) *)
}

(* Recursively walk a port-clause list and extract each port. *)
let rec extract_ports_from = function
  | List xs -> List.concat_map extract_ports_from xs
  (* Vhd_front rewrite normalises ports to Vhdinterface_default_declaration. *)
  | Double (VhdInterfaceObjectDeclaration,
            Double (VhdInterfaceDefaultDeclaration,
                    Sextuple (Vhdinterface_default_declaration,
                              Str name, mode, subtype, _kind, _default))) ->
      [(name, port_direction mode, range_of_subtype subtype)]
  (* Raw signal-declaration form (in case rewrite hasn't run). *)
  | Double (VhdInterfaceSignalDeclaration,
            Sextuple (Vhdinterface_signal_declaration,
                      List names, mode, subtype, _kind, _expr)) ->
      let dir = port_direction mode in
      let range = range_of_subtype subtype in
      List.map (fun n -> (identifier_string n, dir, range)) names
  | _ -> []

let extract_entity_ports header =
  match header with
  | Triple (Vhdentity_header, _generic_clause, port_clause) ->
      extract_ports_from port_clause
  | _ -> []

let collect_entity = function
  | Triple (Vhddesign_unit, _,
            Double (VhdPrimaryUnit,
                    Double (VhdEntityDeclaration,
                            Quintuple (Vhdentity_declaration,
                                       Str ent_name, header, _, _)))) ->
      Some { ent_name = unescape ent_name;
             ent_ports = extract_entity_ports header }
  | _ -> None

(* Walk an architecture body's declarations and pull out internal
 * (non-port) signal declarations as wire decls. Component declarations
 * are skipped — only the instantiations matter for our purposes. *)
let extract_arch_signals decls =
  let items = match decls with List xs -> xs | _ -> [] in
  List.concat_map (fun d ->
    match d with
    (* `VhdSignalDeclaration` is the bare form; `VhdBlockSignalDeclaration`
     * is the form Vivado emits when the architecture has attribute
     * specifications interspersed with signal decls (which it does for
     * any non-trivial design). Both wrap the same inner Quintuple. *)
    | Double ((VhdSignalDeclaration | VhdBlockSignalDeclaration),
              Quintuple (Vhdsignal_declaration,
                         List names, subtype, _kind, _init)) ->
        let range = range_of_subtype subtype in
        List.map (fun n -> mk_wire_decl range (identifier_string n)) names
    (* Single-name shape Vivado often uses: name is a bare Str rather
     * than a List of names. *)
    | Double ((VhdSignalDeclaration | VhdBlockSignalDeclaration),
              Quintuple (Vhdsignal_declaration,
                         (Str _ as n), subtype, _kind, _init)) ->
        let range = range_of_subtype subtype in
        [mk_wire_decl range (identifier_string n)]
    | _ -> []
  ) items

(* Walk an architecture body's statement part, picking up component
 * instantiation statements and translating each to a MODINST quad. *)
(* Concurrent VHDL signal assignment `target <= expr` translates to a
 * single-cell MODINST that we encode as the IBUF passthrough family —
 * O = I gives target = expr. This catches Vivado's bit-blasted assigns
 * like `\a[2]\ <= a(2)` that sit alongside the IBUF/OBUF chain. *)
let mk_passthrough_inst lhs_name rhs_token =
  mk_modinst "WIRE_ASSIGN" (lhs_name ^ "_assign")
    [("I", rhs_token); ("O", ID { Idhash.id = lhs_name })]

let extract_concurrent_assigns stmts =
  let items = match stmts with
    | List xs -> xs
    | other -> [other]
  in
  List.filter_map (function
    (* Vivado-emitted concurrent assignments come wrapped as:
     *   Double(VhdConcurrentSignalAssignmentStatement,
     *     Quadruple(Vhdconcurrent_signal_assignment_statement,
     *       label, bool,
     *       Double(VhdConcurrentSimpleSignalAssignment,
     *         Quintuple(Vhdconcurrent_simple_signal_assignment,
     *           target, bool, delay, waveform))))
     *)
    | Double (VhdConcurrentSignalAssignmentStatement,
              Quadruple (Vhdconcurrent_signal_assignment_statement,
                         _label, _post,
                         Double (VhdConcurrentSimpleSignalAssignment,
                                 Quintuple (Vhdconcurrent_simple_signal_assignment,
                                            target, _bool, _delay, waveform)))) ->
        let lhs = name_string target in
        let rhs = actual_to_vparser waveform in
        if lhs = "" then None else Some (mk_passthrough_inst lhs rhs)
    | _ -> None
  ) items

let extract_arch_instances stmts =
  let items = match stmts with
    | List xs -> xs
    | other -> [other]  (* abstraction collapses single-element lists *)
  in
  List.filter_map (fun s ->
    match s with
    | Double (VhdConcurrentComponentInstantiationStatement,
              Quintuple (Vhdcomponent_instantiation_statement,
                         label, instantiated_unit,
                         _generic_map, port_map)) ->
        let inst_name = identifier_string label in
        let cell_type =
          match instantiated_unit with
          | Double (VhdInstantiatedComponent, name) -> name_string name
          | Triple (VhdInstantiatedEntityArchitecture, name, _) ->
              name_string name
          | Double (VhdInstantiatedConfiguration, name) -> name_string name
          | _ -> "?unknown_cell?"
        in
        let pins =
          let assoc_list = match port_map with List xs -> xs | _ -> [] in
          (* Vivado emits per-bit port maps when the entity port has
           * STD_LOGIC_VECTOR(0 to N) ascending range, e.g. for
           *   I0 : in STD_LOGIC_VECTOR (2 downto 0)
           *   port map (I0(2) => A(0), I0(1) => A(1), I0(0) => A(2), ...)
           * The pin name `name_string formal` collapses to "I0" for all
           * three associations, so a naive walk emits "I0" three times
           * and `pin_expr "I0"` (in cell_to_bprocess) only sees one bit.
           *
           * Detect subscripted formals, group by base name. If every
           * actual in the group is a BITSEL of the same base signal
           * covering the full range 0..N-1, that's just a whole-vector
           * mapping — emit the bare ID. Otherwise emit a Concat of
           * the per-bit actuals (MSB first). *)
          let extract_index = function
            | Double (VhdFormalExpression,
                      Triple (VhdNameParametersPrimary, _,
                              Triple (Vhdassociation_element, _,
                                      Double (VhdActualExpression, idx_expr))))
            | Triple (VhdNameParametersPrimary, _,
                      Triple (Vhdassociation_element, _,
                              Double (VhdActualExpression, idx_expr))) ->
                extract_int idx_expr
            | _ -> None
          in
          (* Walk once, partition subscripted from plain. *)
          let plain = ref [] in
          let subscripted : (string, (int * Vparser.token) list ref)
                              Hashtbl.t = Hashtbl.create 8
          in
          List.iter (fun a ->
            match a with
            | Triple (Vhdassociation_element, formal, actual) ->
                let pin_name = name_string formal in
                if pin_name = "" then ()
                else begin
                  let expr = actual_to_vparser actual in
                  match extract_index formal with
                  | Some i ->
                      let bucket =
                        try Hashtbl.find subscripted pin_name
                        with Not_found ->
                          let r = ref [] in
                          Hashtbl.add subscripted pin_name r; r
                      in
                      bucket := (i, expr) :: !bucket
                  | None ->
                      plain := (pin_name, expr) :: !plain
                end
            | _ -> ()
          ) assoc_list;
          let plain_pins = List.rev !plain in
          let merged_pins = Hashtbl.fold (fun pin_name bucket acc ->
            (* Sort by index descending — MSB first for ver_front concat. *)
            let bits =
              List.sort (fun (i, _) (j, _) -> compare j i) !bucket
            in
            let exprs = List.map snd bits in
            (* Whole-vector mapping: every per-bit actual is a BITSEL of
             * the SAME base signal AND covers the full set of indices
             * 0..N-1 (in any order — VHDL `(0 to N-1)` ascending vs
             * `(N-1 downto 0)` descending makes the natural ordering
             * differ). When that holds, the cell input is just the
             * bare signal — emit the ID directly so the BIR side sees
             * a wide expression rather than a Concat of bit-selects
             * that may have inverted bit indexing. *)
            let same_base_consecutive () =
              match exprs with
              | [] -> None
              | TRIPLE (BITSEL, ID id, INT _) :: _ ->
                  let base = id.Idhash.id in
                  let n = List.length exprs in
                  let indices = List.filter_map (function
                    | TRIPLE (BITSEL, ID id', INT i)
                      when id'.Idhash.id = base -> Some i
                    | _ -> None
                  ) exprs in
                  if List.length indices = n then begin
                    let sorted = List.sort compare indices in
                    let expected = List.init n (fun k -> k) in
                    if sorted = expected then Some (ID id) else None
                  end else None
              | _ -> None
            in
            let combined = match exprs with
              | [single] -> single
              | _ ->
                  (match same_base_consecutive () with
                   | Some bare -> bare
                   | None -> DOUBLE (CONCAT, TLIST exprs))
            in
            (pin_name, combined) :: acc
          ) subscripted [] in
          plain_pins @ merged_pins
        in
        Some (mk_modinst cell_type inst_name pins)
    | _ -> None
  ) items

(* Build the ver_front QUINTUPLE(MODULE, ...) for one entity+arch. *)
let build_module entity arch_decls arch_stmts =
  let port_names = List.map (fun (n, _, _) ->
    ID { Idhash.id = n }
  ) entity.ent_ports in
  let port_decls =
    List.map (fun (n, dir, range) -> mk_port_decl dir range n)
      entity.ent_ports
  in
  let signal_wires = extract_arch_signals arch_decls in
  let assigns = extract_concurrent_assigns arch_stmts in
  let instances = extract_arch_instances arch_stmts @ assigns in

  let decls_h = Hashtbl.create 32 in
  List.iter (fun d -> Hashtbl.add decls_h d ()) (port_decls @ signal_wires);
  let body_h = Hashtbl.create 32 in
  List.iter (fun i -> Hashtbl.add body_h i ()) instances;

  QUINTUPLE (
    MODULE,
    ID { Idhash.id = entity.ent_name },
    TLIST [],          (* no params *)
    TLIST port_names,  (* port-name list *)
    THASH (decls_h, body_h))

(* ─── Top-level conversion ────────────────────────────────────────────── *)

let convert_vhd_file filename =
  let fresh = Hashtbl.create 256 in
  let old_hash = !Vhd_front.Vabstraction.vhdlhash in
  Vhd_front.Vabstraction.vhdlhash := fresh;
  let old_settings = !Vhd_front.VhdlSettings.settings in
  Vhd_front.VhdlSettings.settings :=
    { !Vhd_front.VhdlSettings.settings with
      fileparsedlist = []; filefailedlist = [] };

  let ok = ref true in
  (try Vhd_front.VhdlMain.main ok [filename]
   with _ -> ok := false);

  if not !ok then begin
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Vhd_front.VhdlSettings.settings := old_settings;
    None
  end else begin
    (* Collect entities and architectures from the parsed trees. *)
    let entities : (string, entity_info) Hashtbl.t = Hashtbl.create 16 in
    let architectures : (string, vhdintf * vhdintf) Hashtbl.t =
      Hashtbl.create 16
    in
    Hashtbl.iter (fun (k, _) _ ->
      (* Rewrite normalises interface declarations to the form our
       * extractors expect. *)
      let k = Vhd_front.Rewrite.abstraction (Vhd_front.Rewrite.abstraction k) in
      (match collect_entity k with
       | Some e -> Hashtbl.replace entities e.ent_name e
       | None -> ());
      (match k with
       | Triple (Vhddesign_unit, _,
                 Double (VhdSecondaryUnit,
                         Double (VhdArchitectureBody,
                                 Quintuple (Vhdarchitecture_body,
                                            _arch_name, ent_ref,
                                            decls, stmts)))) ->
           let entity_name = identifier_string ent_ref in
           Hashtbl.replace architectures entity_name (decls, stmts)
       | _ -> ())
    ) fresh;

    (* For each entity that has a matching architecture, build the
     * ver_front tree and insert into Globals.modprims so that
     * Ver_front_to_behavioral picks it up. *)
    Hashtbl.clear Globals.modprims;
    Hashtbl.iter (fun ent_name (decls, stmts) ->
      match Hashtbl.find_opt entities ent_name with
      | Some entity ->
          let module_tree = build_module entity decls stmts in
          let mt : Globals.modtree = {
            tree = module_tree;
            symbols = EndShash;
            unresolved = [];
            is_netlist = true;
            is_behav = false;
            is_seq = false;
            is_hier = false;
            is_top = true;
            arch = "";
            comment = "";
            datestamp = Unix.gettimeofday ();
          } in
          Hashtbl.add Globals.modprims ent_name mt
      | None -> ()
    ) architectures;

    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Vhd_front.VhdlSettings.settings := old_settings;

    (* Now run the ver_front BIR converter over the populated modprims. *)
    let modules = Hashtbl.fold (fun name mt acc ->
      Ver_front_to_behavioral.module_to_bmodule name mt :: acc
    ) Globals.modprims [] in
    (* Vivado's elaborated VHDL emits internal aliases of output-port
     * signals as `\^name\` (escaped VHDL identifier). The pattern is:
     *   signal \^foo\ : ...;
     *   foo <= \^foo\;             -- pass-through to the port
     *   <FF cell>.Q => \^foo\;     -- the actual storage
     * Yosys/Verilator both name the FF's Q after the source signal
     * (`foo`), so the `^` prefix produces a phantom Q__Q after ffrip.
     * Rename every `^foo` reference to `foo` and drop the now-trivial
     * `foo <= foo` pass-through process. Output ports retain their
     * original direction; we just remove the `^foo` Internal signal
     * since `foo` already covers it. *)
    let collapse_caret_alias (m : Behavioral_ir.bmodule) : Behavioral_ir.bmodule =
      let port_outputs =
        List.fold_left (fun acc (s : Behavioral_ir.bsignal) ->
          if s.direction = `Output then s.name :: acc else acc)
          [] m.signals
      in
      let rename n =
        if String.length n > 0 && n.[0] = '^'
           && List.mem (String.sub n 1 (String.length n - 1)) port_outputs
        then String.sub n 1 (String.length n - 1)
        else n
      in
      let rec rename_e = function
        | Behavioral_ir.BVar n -> Behavioral_ir.BVar (rename n)
        | (BConst _) as e -> e
        | BBinOp r -> BBinOp { r with lhs = rename_e r.lhs;
                                       rhs = rename_e r.rhs }
        | BUnOp r -> BUnOp { r with operand = rename_e r.operand }
        | BSelect r -> BSelect { array = rename_e r.array;
                                 index = rename_e r.index }
        | BSlice r -> BSlice { r with signal = rename_e r.signal }
        | BConcat es -> BConcat (List.map rename_e es)
        | BReplicate r -> BReplicate { r with value = rename_e r.value }
        | BCond r -> BCond { condition = rename_e r.condition;
                             then_val = rename_e r.then_val;
                             else_val = rename_e r.else_val }
        | BCall r -> BCall { r with args = List.map rename_e r.args }
      in
      let rec rename_s = function
        | Behavioral_ir.BAssign { lhs; rhs } ->
            Behavioral_ir.BAssign { lhs = rename lhs; rhs = rename_e rhs }
        | BIf r -> BIf { condition = rename_e r.condition;
                          then_stmts = List.map rename_s r.then_stmts;
                          else_stmts = List.map rename_s r.else_stmts }
        | BCase r -> BCase {
            selector = rename_e r.selector;
            cases = List.map (fun (k, ss) ->
              (rename_e k, List.map rename_s ss)) r.cases;
            default = List.map rename_s r.default;
          }
        | BBlock ss -> BBlock (List.map rename_s ss)
        | BWhile { condition; body } ->
            BWhile { condition = rename_e condition;
                     body = List.map rename_s body }
        | BFor { init; condition; update; body } ->
            BFor { init = rename_s init;
                   condition = rename_e condition;
                   update = rename_s update;
                   body = List.map rename_s body }
        | BCallStmt r -> BCallStmt { r with args = List.map rename_e r.args }
        | BReturn (Some e) -> BReturn (Some (rename_e e))
        | BReturn None -> BReturn None
      in
      let rename_proc = function
        | Behavioral_ir.BCombinational c ->
            Behavioral_ir.BCombinational
              { c with body = List.map rename_s c.body }
        | BSequential s ->
            BSequential { s with body = List.map rename_s s.body }
      in
      let signals =
        List.filter_map (fun (s : Behavioral_ir.bsignal) ->
          if String.length s.name > 0 && s.name.[0] = '^'
             && List.mem (String.sub s.name 1 (String.length s.name - 1))
                  port_outputs
          then None  (* drop the alias; the output port covers it *)
          else Some s
        ) m.signals
      in
      let processes =
        List.filter_map (fun p ->
          let p' = rename_proc p in
          (* Drop the trivial `foo <= foo` pass-through that the rename
           * just produced. *)
          match p' with
          | BCombinational { body = [BAssign { lhs;
              rhs = BVar n }]; _ } when lhs = n -> None
          | _ -> Some p'
        ) m.processes
      in
      { m with signals; processes }
    in
    let modules = List.map collapse_caret_alias modules in
    Some Behavioral_ir.{ modules; library_cells = [] }
  end
