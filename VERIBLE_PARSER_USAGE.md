# Using the Verible OCaml Parser

## How It Works

The Verible parser from hardcaml-lua has TWO stages:

### Stage 1: Lexer (`Source_text_verible_lex.mll`)
- Input: Verilog source file
- Output: **Flat token list** `[Module; SymbolIdentifier "simple_add"; LPAREN; ...]`
- Each call to `token lexbuf` returns `token list` (usually `[tok]`, sometimes `[tok1; tok2]`)
- Must be called repeatedly until `End_of_file`

### Stage 2: Parser (`Source_text_verible.mly`)
- Input: Token stream function `(Lexing.lexbuf -> token)`
- Output: **Parse tree** with TUPLE nodes
- Uses the `deflate` function to convert list-returning lexer to streaming

## Example Token Stream

For `simple_add.v`:
```
Module
SymbolIdentifier "simple_add"
LPAREN
Input
LBRACK
TK_DecNumber "3"
COLON
TK_DecNumber "0"
RBRACK
SymbolIdentifier "a"
...
Endmodule
End_of_file
```

## Example Parse Tree (Expected)

After parsing, should produce:
```ocaml
TUPLE3(
  STRING "ml_start1",
  TUPLE... (description_list containing module_declaration),
  End_of_file
)
```

Where module_declaration contains:
- module_header with ports
- module_body with statements
- Each structured as nested TUPLEs

## To Complete Implementation

Need to:
1. Use the `deflate` wrapper from lexer to convert token list to stream
2. Feed to parser: `Source_text_verible.ml_start deflated_lexer lexbuf`
3. Walk resulting TUPLE tree to extract:
   - Parameters
   - Port widths
   - Assign statements
4. Convert to IR operations

## Next Step

Create a helper function that properly uses both lexer and parser together to produce the parse tree.
