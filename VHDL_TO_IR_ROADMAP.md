# VHDL to IR Conversion Roadmap

## Current Status: ✅ VHDL Parser Integrated, AST Structure Understood

### Completed ✅

1. **VHDL Parser Integration** - `vhdl_parse.ml`, `vhdl_elaborate.ml`
   - Successfully parses all 4 UART VHDL modules
   - Extracts entity and architecture
   - Returns VHDL AST

2. **AST Exploration** - `vhdl_dump.ml`, `test_vhdl_dump.ml`
   - Walks VHDL AST structure
   - Extracts process statements
   - Shows if/elsif/else hierarchies
   - Displays signal assignment targets

3. **Pattern Understanding** - From VHDL source analysis
   - Pattern A: Unconditional + Conditional (slib_clock_div, uart_baudgen)
   - Pattern B: Mutually Exclusive (slib_input_filter)
   - Pattern C: Sequential Independent If (slib_mv_filter)

## VHDL AST Structure (Discovered)

### Process Statement
```
ConcurrentProcessStatement(vhdl_process_statement)
  ├─ processlabelname: string
  ├─ processsensitivitylist: SensitivityAll | SensitivityExpressionList
  ├─ processdeclarations: declarations
  └─ processstatements: vhdl_sequential_statement list
```

### Sequential Statements
```
SequentialSignalAssignment(vhdl_signal_assignment_statement)
  └─ SimpleSignalAssignment
      ├─ target: TargetDotted(vhdl_dotted) → signal name
      └─ waveform: WaveForms([expression]) → RHS value

SequentialIf(vhdl_if_statement)
  ├─ ifcondition: vhdl_condition
  ├─ thenstatements: vhdl_sequential_statement list
  └─ elsestatements: ElseNone | Else(statements) | Elsif(if_statement)
```

### Example: slib_clock_div

**VHDL Source (lines 48-64)**:
```vhdl
CD_PROC: process (RST, CLK)
begin
    if (RST = '1') then
        iCounter <= 0;
        iQ       <= '0';
    elsif (CLK'event and CLK='1') then
        iQ <= '0';                      -- Line 54: unconditional
        if (CE = '1') then
            if (iCounter = (RATIO-1)) then
                iQ <= '1';              -- Line 57: conditional override
                iCounter <= 0;
            else
                iCounter <= iCounter + 1;
            end if;
        end if;
    end if;
end process;
```

**AST Structure** (from vhdl_dump.ml):
```
Process: CD_PROC
  Statements:
    If <RST = '1'>:
      Then:
        Target: iCounter  (reset)
        Target: iQ        (reset)
      Elsif:
        If <CLK'event and CLK='1'>:
          Then:
            Target: iQ  (unconditional default, line 54)
            If <CE = '1'>:
              Then:
                If <iCounter = (RATIO-1)>:
                  Then:
                    Target: iQ        (conditional override, line 57)
                    Target: iCounter
                  Else:
                    Target: iCounter
```

## Next Steps: VHDL→IR Converter

### Phase 1: Expression Conversion

**Task**: Convert VHDL expressions to IR operations

**Key Types**:
- `vhdl_expression` → IR expression
- `vhdl_condition` → IR boolean expression
- `vhdl_primary` → IR value/identifier

**Operations Needed**:
- Comparison: `=`, `/=`, `<`, `>`, `<=`, `>=`
- Logical: `and`, `or`, `not`, `xor`
- Arithmetic: `+`, `-`, `*`, `/`
- Attributes: `'event`, `'stable`

**Example Mappings**:
```
VHDL: CE = '1'              → IR: Compare(Equal, Wire("CE"), Constant(1, 1))
VHDL: iCounter + 1          → IR: Add(Wire("iCounter"), Constant(1, width))
VHDL: CLK'event and CLK='1' → IR: (rising edge detected, handled specially)
```

### Phase 2: Sensitivity List Analysis

**Task**: Identify clock, reset, and enable signals

**Patterns**:
```
Sensitivity: (RST, CLK)     → Async reset, posedge CLK
Sensitivity: (CLK)          → Sync reset, posedge CLK
Sensitivity: all            → Combinational (not typical for UART)
```

**Extract**:
- Clock signal: Look for `CLK'event and CLK='1'`
- Reset signal: First `if` condition in process
- Reset type: Async (in sensitivity) vs Sync (not in sensitivity)

### Phase 3: Assignment Grouping

**Task**: Group all assignments to same signal across conditions

**Algorithm**:
1. Walk all sequential statements recursively
2. Track current condition path (AND of nested conditions)
3. Accumulate assignments: `signal → [(condition, value)]`
4. Build priority MUX tree (latest assignment wins)

**Example**: slib_clock_div `iQ` signal
```
Assignments:
  1. condition: RST='1'                          → value: '0'  (reset)
  2. condition: CLK'event ∧ true                → value: '0'  (default)
  3. condition: CLK'event ∧ CE='1' ∧ (cnt==max) → value: '1'  (override)

MUX Tree:
  iQ_next = Mux(RST='1',
                '0',
                Mux(CE='1' ∧ (cnt==max),
                    '1',
                    '0'))
```

### Phase 4: Register Inference

**Task**: Create register nodes for signals assigned in clocked process

**For each signal**:
```
If assigned in process with clock:
  1. Create register: Register(clock, reset_signal, enable, data)
  2. data = MUX tree from Phase 3
  3. enable = OR of all non-reset conditions
  4. Add to IR: ir_nodes[signal_id] = Register(...)
```

### Phase 5: IR Construction

**Task**: Build complete IR matching sv_ast.ml format

**Create**:
- `ir_inputs`: Clock, reset, control signals
- `ir_wires`: Internal signals (iCounter, iQ)
- `ir_outputs`: Module outputs
- `ir_nodes`: All operations (MUX, Register, Add, etc.)

**Example IR** for slib_clock_div `iQ`:
```ocaml
{
  ir_inputs: {
    "CLK" → Input { id=0; width=1 };
    "RST" → Input { id=1; width=1 };
    "CE"  → Input { id=2; width=1 };
  };
  ir_wires: {
    "iQ" → Wire { id=10; width=1 };
    "iCounter" → Wire { id=14; width=3 };
  };
  ir_outputs: {
    "Q" → Wire { id=10; width=1 };  (* Same as iQ *)
  };
  ir_nodes: {
    6: Compare(Equal, iCounter, RATIO-1);     (* iCounter == max *)
    7: And(CE, node_6);                       (* CE && (cnt==max) *)
    8: Mux(node_7, Constant(1,1), Constant(0,1));  (* Conditional *)
    9: Not(RST);                              (* ~RST for enable *)
    10: Register(CLK, RST, node_9, node_8);   (* iQ register *)
  };
}
```

## Implementation Files Needed

### 1. `vhdl_expr_to_ir.ml`
Convert VHDL expressions to IR operations
- `convert_expression: vhdl_expression → ir_expr`
- `convert_condition: vhdl_condition → ir_expr`
- Handle operators, constants, signals

### 2. `vhdl_process_extract.ml`
Extract information from process statements
- `extract_sensitivity: vhdl_process_statement → clock * reset * type`
- `extract_assignments: vhdl_sequential_statement list → (signal × [(condition, value)]) list`
- Track condition contexts through if/elsif/else

### 3. `vhdl_to_ir.ml`
Main converter
- `convert_architecture: vhdl_architecture_body → sv_ast.ir`
- Build complete IR structure
- Handle all signals and operations

### 4. `test_vhdl_to_ir.ml`
Test conversion
- Parse VHDL → Convert to IR
- Print IR structure
- Verify against expected

## Testing Strategy

### Unit Tests
1. **Expression Conversion**: Simple expressions → IR
2. **Assignment Grouping**: Multiple assignments → MUX tree
3. **Register Inference**: Process → Register nodes

### Integration Tests
1. **slib_clock_div**: Pattern A (unconditional + conditional)
2. **slib_input_filter**: Pattern B (mutually exclusive)
3. **slib_mv_filter**: Pattern C (sequential independent if)
4. **uart_baudgen**: Pattern A variant

### Verification Tests
1. **VHDL vs SV**: Compare VHDL-derived IR against SV-derived IR using Z3
2. **Ground Truth**: VHDL is authoritative, proves SV decompiler correct
3. **Pattern Validation**: Confirms our MUX tree generation strategy

## Expected Outcomes

### Success Metrics
- ✅ All 4 UART modules convert VHDL → IR without errors
- ✅ Generated IR matches expected structure
- ✅ Z3 proves VHDL IR ≡ SystemVerilog IR
- ✅ Validates SystemVerilog decompiler against ground truth

### Benefits
1. **Ground Truth Comparison**: Compare against original VHDL, not buggy Verilator
2. **Pattern Validation**: Confirm our MUX tree generation is correct
3. **Debugging Tool**: When SV decompiler fails, compare against VHDL IR
4. **Documentation**: Clear examples of expected IR for each pattern

## Timeline Estimate

- Phase 1 (Expressions): 2-3 hours
- Phase 2 (Sensitivity): 1 hour
- Phase 3 (Grouping): 2-3 hours (most complex)
- Phase 4 (Registers): 1-2 hours
- Phase 5 (IR Build): 1-2 hours
- Testing: 2-3 hours

**Total**: ~10-15 hours of focused development

## Current Commit

```
Commit: "Add VHDL AST dumper to explore process structure"
- vhdl_dump.ml: Walks AST, prints structure
- test_vhdl_dump.ml: Tests on UART modules
- Output confirms AST structure matches VHDL source
```

## Next Immediate Step

Start with Phase 1: Create `vhdl_expr_to_ir.ml` to convert simple expressions.

**Priority**: Medium-High
**Complexity**: High (but well-defined)
**Value**: Very High (ground truth validation)
