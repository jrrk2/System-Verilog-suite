# SystemVerilog Elaboration Implementation Status

## What We've Accomplished

### 1. Integrated Verible Parser (✓ Complete)
- **Files copied from hardcaml-lua:**
  - `Source_text_verible.mly` - Full Verilog/SystemVerilog parser (2917 lines)
  - `Source_text_verible_lex.mll` - Lexer
  - `Source_text_verible_types.ml` - Type definitions
  - `Source_text_verible_tokens.ml` - Token utilities

- **Build system updated:**
  - Added menhir support to `dune-project`
  - Added ocamllex and menhir rules to `dune`
  - Successfully compiles (with warnings)

### 2. Created Elaboration Framework (✓ Structure Ready)
- **File:** `sv_elaborate.ml`
- **Capabilities:**
  - Parameter/constant evaluation framework
  - Expression evaluation (arithmetic operations)
  - Width resolution framework
  - Context management (scopes, types, parameters)

### 3. Created Verible→IR Converter (✓ Stub)
- **File:** `sv_verible_to_ir.ml`
- **Current status:** Parses Verilog, creates empty IR
- **Next step:** Implement AST walking to populate IR

## The Challenge: Understanding Verible's AST

The Verible parser produces a token tree with TUPLE nodes:
```ocaml
TUPLE4(STRING "add_expr2", left_expr, PLUS, right_expr)
TUPLE3(STRING "module_declaration1", header, body)
```

To implement elaboration, we need to:

1. **Understand the parse tree structure** for:
   - Module declarations
   - Parameter declarations
   - Port declarations (with ranges)
   - Assign statements
   - Expressions

2. **Walk the tree** to extract:
   - Parameters and their values
   - Signal widths from range expressions
   - Logic operations

3. **Convert to IR** operations with resolved widths

## Recommended Next Steps

### Option A: Study Verible Output (Quick Start)
```bash
# See what Verible actually produces
verible-verilog-syntax --printtree test_elab.v > verible_tree.txt

# Then write pattern matchers for common constructs:
# - TUPLE patterns for module headers
# - TUPLE patterns for parameter declarations
# - TUPLE patterns for port declarations with ranges
# - TUPLE patterns for continuous assignments
```

### Option B: Reference sv-elaborator Implementation
The Rust sv-elaborator at `/Users/jonathan/Downloads/sv-elaborator-master/` shows the full elaboration algorithm:

**Key files to study:**
- `src/elaborate/elaborate.rs` (lines 1-200) - Main elaboration logic
- `src/elaborate/expr.rs` - Expression evaluation
- `src/elaborate/ty.rs` - Type resolution
- `src/lowering/*.rs` - Generate block expansion

**Elaboration phases:**
1. **Resolve** - Name resolution, scope building
2. **Elaborate** - Parameter substitution, constant evaluation
3. **Lower** - Generate expansion, instance arrays

### Option C: Incremental Implementation Plan

**Phase 1: Basic Parameter Elaboration**
- Parse module with `parameter WIDTH = 8;`
- Extract parameter name → value mapping
- Evaluate constant expressions in ranges: `[WIDTH-1:0]` → `[7:0]`
- **Test:** Verify parameters resolve correctly

**Phase 2: Port Width Resolution**
- Parse port declarations
- Resolve width expressions using parameter context
- Create IR inputs/outputs with correct widths
- **Test:** Compare with Verilator widths

**Phase 3: Assign Statement Conversion**
- Parse `assign` statements
- Convert expressions to IR operations
- Resolve operation widths
- **Test:** Compare simple_add.v with Yosys/Verilator

**Phase 4: Generate Block Expansion**
- Detect `generate for` loops
- Unroll with parameter values
- Create multiple instances
- **Test:** Parameterized designs

## Current Verification Status

| Design | Yosys | Verilator | Verible | Status |
|--------|-------|-----------|---------|--------|
| simple_add.v (4+4=4) | ✓ | ✓ | Parser only | **PASS** |
| simple_mult.v (4×4=8) | ✓ | ✓ | Parser only | **FAIL** (width mismatch) |
| simple_logic.v | ✓ | ✓ | Parser only | **PASS** |
| test_elab.v (params) | ✓ | ✓ | **Needs elaboration** | N/A |

## Technical Debt

1. **Verible lexer integration** - Returns `token list` not streaming tokens
2. **Parse tree documentation** - Need to document TUPLE patterns
3. **Width extension normalization** - Still needed for Yosys↔Verilator comparison
4. **Generate blocks** - Not yet supported

## Files Created

```
sv_elaborate.ml (176 lines)           - Elaboration framework
sv_verible_to_ir.ml (60 lines)        - Verible→IR converter stub
test_verible_elab.ml (40 lines)       - Test harness
ELABORATION_ANALYSIS.md               - Problem analysis
WIDTH_EXTENSION_ISSUE.md              - Width mismatch analysis
ELABORATION_STATUS.md (this file)     - Status summary
```

## Estimated Work Remaining

- **Parameter extraction**: 2-4 hours (parse TUPLE patterns)
- **Width resolution**: 2-3 hours (evaluate range expressions)
- **Assign conversion**: 3-5 hours (expression → IR ops)
- **Testing & debugging**: 3-4 hours
- **Generate blocks** (optional): 5-8 hours

**Total**: ~10-15 hours for basic elaboration
**With generate blocks**: ~20 hours

## Conclusion

We have:
- ✅ Full Verible parser integrated and compiling
- ✅ Elaboration framework structure ready
- ✅ Test infrastructure in place

We need:
- 📝 Document Verible's TUPLE parse tree structure
- 🔨 Implement pattern matching for module/parameter/port extraction
- 🔨 Implement expression evaluation with parameter context
- 🔨 Implement AST→IR conversion

The hardest part (parser integration) is done. The remaining work is systematic AST traversal and pattern matching, which can be done incrementally with tests at each step.
