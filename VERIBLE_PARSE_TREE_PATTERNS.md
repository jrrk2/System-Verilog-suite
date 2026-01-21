# Verible Parse Tree Patterns

Based on parsing `simple_add.v`, here are the key patterns:

## Module Declaration
```
TUPLE12(
  STRING "module_or_interface_declaration1",
  Module,                    // keyword
  EMPTY_TOKEN,              // lifetime
  SymbolIdentifier "name",  // module name
  EMPTY_TOKEN,              // package imports
  EMPTY_TOKEN or params,    // parameter port list
  port_list,                // module ports
  EMPTY_TOKEN,              // foreign attributes
  SEMICOLON,
  module_item_list,         // statements
  Endmodule,
  EMPTY_TOKEN               // label
)
```

## Port Declaration
```
TUPLE5(
  STRING "port_declaration_noattr1",
  Input/Output,             // direction
  EMPTY_TOKEN,              // net_type
  TUPLE4(                   // data_type_or_implicit...
    STRING "data_type_or_implicit_basic_followed_by_id_and_dimensions_opt4",
    TUPLE6(                 // decl_variable_dimension1
      STRING "decl_variable_dimension1",
      LBRACK,
      TK_DecNumber "3",     // high bit
      COLON,
      TK_DecNumber "0",     // low bit
      RBRACK
    ),
    TUPLE3(                 // unqualified_id1
      STRING "unqualified_id1",
      SymbolIdentifier "name",
      EMPTY_TOKEN
    ),
    EMPTY_TOKEN             // unpacked dimensions
  ),
  EMPTY_TOKEN               // initialization
)
```

## Continuous Assignment
```
TUPLE6(
  STRING "continuous_assign1",
  Assign,
  EMPTY_TOKEN,              // drive strength
  EMPTY_TOKEN,              // delay
  CONS1(
    TUPLE4(
      STRING "cont_assign1",
      lhs_identifier,       // left-hand side
      EQUALS,
      expression            // right-hand side
    )
  ),
  SEMICOLON
)
```

## Expression - Addition
```
TUPLE4(
  STRING "add_expr2",
  left_expr,                // left operand
  PLUS,
  right_expr                // right operand
)
```

## Identifier Reference
```
TUPLE3(
  STRING "unqualified_id1",
  SymbolIdentifier "name",
  EMPTY_TOKEN
)
```

## Key Observations

1. **STRING tags identify node types**: e.g., "module_or_interface_declaration1", "add_expr2"
2. **EMPTY_TOKEN** used for optional/missing elements
3. **CONS1/CONS2** used for lists (descriptions, port lists, etc.)
4. **COMMA** tokens separate list items
5. **Widths** are in TUPLE6 with format `[high:low]`
6. **Expressions** use STRING tags like "add_expr2", "mul_expr2", etc.
