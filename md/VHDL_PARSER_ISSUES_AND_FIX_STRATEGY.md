# VHDL Parser Issues and Fix Strategy

## Date: 2026-01-22

## Executive Summary

Analysis of 12 VHDL/SystemVerilog module pairs reveals **critical bugs in the VHDL parser** that prevent proper IR generation. The most severe issue is **complete failure to extract entity ports**, resulting in IRs that only show internal signals (CLK, RST) while missing all data inputs and outputs.

## Differences Found

### 1. Port Extraction Failure (CRITICAL)

**Issue:** VHDL parser extracts only process-internal signals, not entity ports.

**Evidence from UART modules:**

| Module | VHDL Ports | SV Ports | Status |
|--------|------------|----------|--------|
| uart_transmitter | CLK, RST | DIN, WLS, SOUT, TXFINISHED | ❌ Missing all data ports |
| uart_baudgen | CLK, RST | DIVIDER, BAUDTICK | ❌ Missing all data ports |
| uart_interrupt | CLK, RST | (multiple) | ❌ Missing all data ports |
| uart_receiver | CLK, RST | (multiple) | ❌ Missing all data ports |
| apb_uart | CLK, RST | PADDR, PWDATA, PRDATA, PREADY, PSLVERR, INT, OUT1N, OUT2N, RTSN, DTRN, SOUT | ❌ Missing 11 ports |

**Pattern:** VHDL consistently shows only 2 inputs (CLK, RST) regardless of actual entity interface.

**VHDL Source Example (uart_transmitter.vhd):**
```vhdl
entity uart_transmitter is
  port (
    CLK : in std_logic;
    RST : in std_logic;
    DIN : in std_logic_vector(7 downto 0);  -- MISSING FROM IR
    WLS : in std_logic_vector(1 downto 0);  -- MISSING FROM IR
    STB : in std_logic;                     -- MISSING FROM IR
    SOUT : out std_logic;                   -- MISSING FROM IR
    TXFINISHED : out std_logic              -- MISSING FROM IR
  );
end entity;
```

**Generated IR (wrong):**
```verilog
module uart_transmitter (
  input CLK,
  input RST
);
```

**Should be:**
```verilog
module uart_transmitter (
  input CLK,
  input RST,
  input [7:0] DIN,
  input [1:0] WLS,
  input STB,
  output SOUT,
  output TXFINISHED
);
```

**Impact:** IR is fundamentally incomplete and cannot represent the actual circuit.

### 2. IR Complexity Differences

**Issue:** VHDL generates dramatically simpler IR than equivalent SV.

**Evidence:**

| Module | VHDL Lines | VHDL Nodes | SV Lines | SV Nodes | Ratio |
|--------|------------|------------|----------|----------|-------|
| apb_uart | 35 | 8 | 699 | ~200+ | 19.97x |
| uart_receiver | 28 | 8 | 144 | ~50+ | 5.14x |
| uart_interrupt | 46 | ~15 | 92 | ~30+ | 2.00x |
| uart_baudgen | 51 | ~20 | 68 | ~25 | 1.33x |
| uart_transmitter | 83 | ~30 | 65 | ~20 | 0.78x (SV smaller!) |

**Pattern:**
- Simple modules (uart_baudgen, uart_transmitter): Similar complexity
- Complex modules (apb_uart, uart_receiver): VHDL drastically simplified
- Suggests VHDL parser giving up or filtering too aggressively on complex designs

**Root causes:**
1. Incomplete process extraction
2. Missing state machine elaboration
3. Partial signal assignment capture
4. Early termination on complex logic

### 3. Width Inference Issues

**Issue:** VHDL defaults to 32-bit for most signals instead of actual widths.

**Evidence from uart_baudgen:**

**VHDL IR:**
```verilog
logic [31:0] mux_n14_n15_n31_reg_plus_n19;  // Should be [15:0]
logic [31:0] mux_n16_eq_n17_n20_n30;        // Should be [15:0]
```

**SV IR:**
```verilog
logic [15:0] n43_reg;                        // Correct width
logic [15:0] mux_n41_n39_n3_plus_const_1;  // Correct width
```

**Pattern:**
- VHDL uses 32-bit default for unknowns
- SV properly infers widths from declarations
- 1-bit signals handled correctly by both

**VHDL Source:**
```vhdl
signal counter : std_logic_vector(15 downto 0);  -- Should be 16-bit
```

**VHDL IR (wrong):**
```verilog
logic [31:0] counter;  -- Generated as 32-bit!
```

### 4. Undefined Signal References

**Issue:** Many signals referenced but never defined (n0, n1, n4, etc.).

**Evidence from uart_baudgen VHDL:**
```verilog
assign clk_eq_n5 = clk == n5;           // What is n5?
assign mux_n14_n15_n31_reg_eq_n1 = ... == n1;  // What is n1?
assign n16_eq_n17 = n16 == n17;         // What are n16, n17?
```

**Evidence from uart_baudgen SV:**
```verilog
always @(posedge n0 or posedge n0)      // n0 = undefined
assign n3_plus_const_1 = n3 + const_1;  // n3 = undefined
```

**Pattern:**
- Undefined signals are missing:
  - Port connections (DIN, DIVIDER, etc.)
  - Constants (1, 0, max values)
  - Intermediate wires not properly extracted
- Both parsers have this issue, but VHDL worse due to missing ports

**Expected vs Actual:**

| Reference | Should Be | Current |
|-----------|-----------|---------|
| n0 | CLK or RST | Undefined |
| n1 | Constant 1 or 0 | Undefined |
| n5 | Constant value | Undefined |
| n3 | Input port (DIN, DIVIDER) | Undefined |
| n6 | Comparison result | Defined (partial) |

### 5. Missing Constants

**Issue:** Constants not extracted from VHDL source.

**VHDL Source:**
```vhdl
constant BAUD_MAX : integer := 100;
constant IDLE : std_logic_vector(1 downto 0) := "00";
```

**VHDL IR:**
```verilog
// Constants not defined anywhere
assign counter_eq_max = counter == n100;  // n100 undefined!
```

**SV IR (correct):**
```verilog
assign const_0 = 32'd0;
assign const_1 = 32'd1;
assign counter_eq_max = counter == const_100;
```

**Impact:** Impossible to understand or simulate the IR without constant values.

### 6. Signal Naming Inconsistencies

**Issue:** Different naming between VHDL and SV even for equivalent signals.

**VHDL:**
```verilog
logic mux_n14_n15_n31_reg;
logic mux_n14_n15_n31_reg_plus_n19;
```

**SV:**
```verilog
logic n43_reg;
logic n3_plus_const_1;
```

**Pattern:**
- Both use nXX for node IDs
- But numbering is completely different
- Can't align equivalent signals between IRs
- Meaningful names help, but underlying IDs don't match

### 7. Register vs Wire Classification

**Issue:** Some confusion between registers and wires.

**Evidence from uart_transmitter:**

**VHDL:**
```verilog
logic n47_reg;                           // Correctly identified as register
logic mux_n30_and_n31_n7_n49_reg;       // Correctly identified
```

**SV:**
```verilog
logic n13_reg;                           // Correctly identified
logic txfinished;                        // Output register (correct)
logic mux_n18_and_n22_const_1_n21_reg;  // Correctly identified
```

**Mostly correct** - Both parsers handle register identification reasonably well.

### 8. Always Block vs Combinational

**Issue:** Some confusion about what should be in always blocks.

**Evidence:**

Both VHDL and SV correctly separate:
- Sequential logic → `always @(posedge clk)` blocks
- Combinational logic → `assign` statements

**No major issues** in this area - both parsers handle it correctly.

## Root Cause Analysis

### VHDL Parser Architecture (vhdl_to_ir.ml, vhdl_elaborate.ml)

Current pipeline:
```
VHDL Source → VhdlParser (AST) → vhdl_elaborate → vhdl_to_ir → opt_ir
```

**Problem points identified:**

#### 1. Entity Port Extraction (vhdl_elaborate.ml)
```ocaml
(* Current: Only extracts process signals *)
let extract_signals ast =
  match ast with
  | Process { signals; ... } ->
      (* Only gets signals from process, NOT entity ports *)
      List.iter add_signal signals
```

**Missing:**
```ocaml
(* Needed: Extract from entity declaration first *)
let extract_entity_ports ast =
  match ast with
  | Entity { ports; ... } ->
      List.iter (fun port ->
        add_input_or_output port.name port.direction port.type
      ) ports
```

#### 2. Width Inference (vhdl_expr_to_ir.ml)
```ocaml
(* Current: Defaults to 32 *)
let infer_width signal =
  match signal.type with
  | StdLogic -> 1
  | StdLogicVector _ -> 32  (* WRONG - should parse range *)
  | Integer -> 32
```

**Should be:**
```ocaml
let infer_width signal =
  match signal.type with
  | StdLogic -> 1
  | StdLogicVector (high, low) -> (high - low + 1)
  | Integer -> 32
```

#### 3. Constant Extraction (vhdl_elaborate.ml)
```ocaml
(* Current: Constants not extracted *)
(* Missing entire constant handling *)
```

**Needed:**
```ocaml
let extract_constants ast =
  match ast with
  | ConstDecl { name; value; type } ->
      add_constant name value type
```

#### 4. Signal Connection (vhdl_to_ir.ml)
```ocaml
(* Current: Only connects process-local signals *)
let connect_signals process =
  (* Missing: Connect to entity ports *)
  (* Missing: Connect to constants *)
```

**Needed:**
```ocaml
let connect_signals process entity_ports constants =
  (* Connect process signals to entity ports *)
  (* Connect references to constants *)
  (* Build complete signal graph *)
```

## Fix Strategy

### Phase 1: Entity Port Extraction (HIGH PRIORITY)

**Goal:** Extract complete port list from entity declaration.

**Files to modify:**
- `vhdl_elaborate.ml` - Add entity port extraction
- `vhdl_to_ir.ml` - Connect ports to IR

**Implementation:**

1. **Parse entity declaration first**
```ocaml
let extract_entity_ports entity =
  match entity with
  | Entity { name; ports; ... } ->
      List.map (fun port ->
        match port with
        | PortDecl { name; direction; dtype } ->
            let width = parse_width dtype in
            let dir = match direction with
              | In -> "input"
              | Out -> "output"
              | InOut -> "inout"
            in
            (name, dir, width)
      ) ports
```

2. **Add ports to IR inputs/outputs**
```ocaml
let add_entity_ports_to_ir ir entity_ports =
  List.iter (fun (name, direction, width) ->
    match direction with
    | "input" ->
        let input = Input { id = get_next_id (); name; width } in
        Hashtbl.add ir.ir_inputs name input
    | "output" ->
        let output = Output { id = get_next_id (); name; width } in
        Hashtbl.add ir.ir_outputs name output
    | _ -> ()
  ) entity_ports
```

3. **Connect process signals to ports**
```ocaml
let connect_process_to_ports process_signals entity_ports =
  List.iter (fun signal ->
    (* If signal name matches port name, connect them *)
    match List.find_opt (fun (pname, _, _) -> pname = signal.name) entity_ports with
    | Some (port_name, direction, width) ->
        connect_signal signal port_name
    | None -> ()
  ) process_signals
```

**Testing:**
- Run on uart_transmitter.vhd
- Verify all 7 ports extracted (CLK, RST, DIN, WLS, STB, SOUT, TXFINISHED)
- Check widths correct (DIN=8, WLS=2, others=1)

**Expected result:**
```verilog
module uart_transmitter (
  input CLK,
  input [7:0] DIN,        // ← Fixed!
  input RST,
  input STB,              // ← Fixed!
  input [1:0] WLS,        // ← Fixed!
  output SOUT,            // ← Fixed!
  output TXFINISHED       // ← Fixed!
);
```

### Phase 2: Width Inference (HIGH PRIORITY)

**Goal:** Parse actual widths from std_logic_vector declarations.

**Files to modify:**
- `vhdl_expr_to_ir.ml` - Fix width inference
- `vhdl_elaborate.ml` - Parse vector ranges

**Implementation:**

1. **Parse std_logic_vector range**
```ocaml
let parse_vector_width dtype =
  match dtype with
  | StdLogicVector { high; low; ... } ->
      (* Handle "15 downto 0" → width = 16 *)
      (* Handle "0 to 15" → width = 16 *)
      if high >= low then
        high - low + 1
      else
        low - high + 1
  | StdLogic -> 1
  | Integer -> 32  (* Keep 32 for integers *)
```

2. **Apply to all signal declarations**
```ocaml
let elaborate_signal_decl decl =
  match decl with
  | SignalDecl { name; dtype } ->
      let width = parse_vector_width dtype in
      { name; width; dtype }
```

3. **Propagate through IR generation**
```ocaml
let signal_to_ir_wire signal =
  Wire {
    id = get_next_id ();
    name = signal.name;
    width = signal.width;  (* Use parsed width *)
  }
```

**Testing:**
- Run on uart_baudgen.vhd
- Verify counter signals use correct width (15 downto 0 = 16 bits)
- Check all widths match source declarations

**Expected result:**
```verilog
module uart_baudgen (
  ...
  logic [15:0] counter_reg;         // ← Fixed! Was [31:0]
  logic [15:0] counter_plus_1;      // ← Fixed! Was [31:0]
);
```

### Phase 3: Constant Extraction (MEDIUM PRIORITY)

**Goal:** Extract constant declarations and use them in IR.

**Files to modify:**
- `vhdl_elaborate.ml` - Extract constant declarations
- `vhdl_to_ir.ml` - Add constants to IR

**Implementation:**

1. **Extract constant declarations**
```ocaml
let extract_constants ast =
  let constants = ref [] in
  let rec find_constants = function
    | ConstDecl { name; value; dtype } ->
        let parsed_value = parse_constant_value value dtype in
        constants := (name, parsed_value) :: !constants
    | Architecture { decls; ... } ->
        List.iter find_constants decls
    | _ -> ()
  in
  find_constants ast;
  !constants
```

2. **Parse constant values**
```ocaml
let parse_constant_value value dtype =
  match value with
  | IntLiteral n -> n
  | HexLiteral s -> int_of_string ("0x" ^ s)
  | BinLiteral s -> int_of_string ("0b" ^ s)
  | _ -> 0  (* Default for complex constants *)
```

3. **Add to IR constants table**
```ocaml
let add_constants_to_ir ir constants =
  List.iter (fun (name, value) ->
    let const_id = get_next_id () in
    Hashtbl.add ir.ir_constants value const_id;
    (* Create name mapping *)
    add_name_mapping name const_id
  ) constants
```

**Testing:**
- Run on modules with constants
- Verify constants defined in IR
- Check constant references resolved

**Expected result:**
```verilog
module uart_baudgen (
  ...
  assign counter_eq_max = counter == const_100;  // ← Fixed!
  ...
  assign const_100 = 32'd100;  // ← Added!
);
```

### Phase 4: Signal Connection (MEDIUM PRIORITY)

**Goal:** Build complete signal dependency graph.

**Files to modify:**
- `vhdl_to_ir.ml` - Improve signal connection logic

**Implementation:**

1. **Build symbol table**
```ocaml
let build_symbol_table entity_ports process_signals constants =
  let symbols = Hashtbl.create 100 in
  (* Add entity ports *)
  List.iter (fun (name, _, width) ->
    Hashtbl.add symbols name (Port { name; width })
  ) entity_ports;
  (* Add constants *)
  List.iter (fun (name, value) ->
    Hashtbl.add symbols name (Const { name; value })
  ) constants;
  (* Add process signals *)
  List.iter (fun signal ->
    Hashtbl.add symbols signal.name (Signal signal)
  ) process_signals;
  symbols
```

2. **Resolve references**
```ocaml
let resolve_signal_ref symbols ref_name =
  match Hashtbl.find_opt symbols ref_name with
  | Some (Port p) -> PortRef p.name
  | Some (Const c) -> ConstRef c.value
  | Some (Signal s) -> SignalRef s.name
  | None -> UndefRef ref_name  (* Track undefined refs *)
```

3. **Connect in expressions**
```ocaml
let convert_expression symbols expr =
  match expr with
  | VarRef name ->
      resolve_signal_ref symbols name
  | BinOp (op, left, right) ->
      let left' = convert_expression symbols left in
      let right' = convert_expression symbols right in
      BinOp (op, left', right')
  | _ -> expr
```

**Testing:**
- Run on all modules
- Count undefined references (should be 0)
- Verify all signals connected properly

### Phase 5: Process Completeness (MEDIUM PRIORITY)

**Goal:** Ensure all process statements converted to IR.

**Files to modify:**
- `vhdl_process_extract.ml` - Improve process analysis

**Implementation:**

1. **Extract all assignments**
```ocaml
let extract_all_assignments process =
  let assignments = ref [] in
  let rec extract = function
    | Assignment { lhs; rhs } ->
        assignments := (lhs, rhs) :: !assignments
    | IfStmt { branches; else_branch } ->
        List.iter (fun (cond, stmts) ->
          List.iter extract stmts
        ) branches;
        Option.iter (List.iter extract) else_branch
    | CaseStmt { cases; default } ->
        List.iter (fun (_, stmts) ->
          List.iter extract stmts
        ) cases;
        Option.iter (List.iter extract) default
    | _ -> ()
  in
  extract process;
  !assignments
```

2. **Verify completeness**
```ocaml
let verify_all_signals_assigned signals assignments =
  List.iter (fun signal ->
    let assigned = List.exists (fun (lhs, _) ->
      lhs = signal.name
    ) assignments in
    if not assigned then
      Printf.eprintf "Warning: Signal %s never assigned\n" signal.name
  ) signals
```

**Testing:**
- Run on complex modules (apb_uart, uart_receiver)
- Compare IR node count to expected
- Verify no missing logic

### Phase 6: State Machine Elaboration (LOW PRIORITY)

**Goal:** Properly elaborate VHDL case statements to IR.

**Implementation:**

1. **Detect state machine pattern**
```ocaml
let is_state_machine process =
  (* Check for case statement on signal *)
  (* Check for state <= next_state pattern *)
  ...
```

2. **Convert case to mux tree**
```ocaml
let case_to_mux_tree case_stmt =
  (* Convert case branches to nested muxes *)
  (* Preserve all branches *)
  ...
```

**Testing:**
- Run on uart_receiver, uart_transmitter
- Compare IR complexity to SV
- Verify all states represented

## Implementation Plan

### Week 1: Critical Fixes
- **Day 1-2:** Phase 1 - Entity Port Extraction
  - Implement entity port parsing
  - Add ports to IR
  - Test on uart_transmitter

- **Day 3-4:** Phase 2 - Width Inference
  - Parse std_logic_vector ranges
  - Apply to all signals
  - Test on uart_baudgen

- **Day 5:** Integration Testing
  - Run on all 12 modules
  - Measure improvement
  - Document results

### Week 2: Important Fixes
- **Day 1-2:** Phase 3 - Constant Extraction
  - Extract constants from declarations
  - Add to IR
  - Test resolution

- **Day 3-4:** Phase 4 - Signal Connection
  - Build symbol table
  - Resolve all references
  - Verify completeness

- **Day 5:** Integration Testing
  - Verify undefined refs = 0
  - Check signal connectivity
  - Document results

### Week 3: Completeness
- **Day 1-3:** Phase 5 - Process Completeness
  - Verify all assignments extracted
  - Check complex logic
  - Test on apb_uart

- **Day 4-5:** Phase 6 - State Machine Elaboration
  - Improve case statement handling
  - Test on state machine modules
  - Final integration test

## Success Metrics

### Phase 1 Success (Port Extraction)
- ✅ All entity ports appear in IR
- ✅ Correct directions (input/output)
- ✅ Port count matches entity declaration
- ✅ uart_transmitter shows 7 ports (not just 2)

### Phase 2 Success (Width Inference)
- ✅ No signals defaulting to 32-bit unless integer
- ✅ All std_logic_vector widths correct
- ✅ uart_baudgen counter is 16-bit (not 32-bit)

### Phase 3 Success (Constants)
- ✅ All constants extracted and defined
- ✅ Constant references resolved
- ✅ No undefined constant refs (nXX for constants)

### Phase 4 Success (Signal Connection)
- ✅ Zero undefined signal references
- ✅ All process signals connected to ports
- ✅ Complete signal dependency graph

### Phase 5 Success (Process Completeness)
- ✅ IR node count comparable to SV
- ✅ apb_uart: >100 nodes (not 8)
- ✅ All assignments captured

### Phase 6 Success (State Machines)
- ✅ Case statements fully elaborated
- ✅ All states represented in IR
- ✅ Comparable complexity to SV

## Expected Final Results

After all fixes:

| Module | Current VHDL | Expected VHDL | SV Lines | Status |
|--------|--------------|---------------|----------|--------|
| apb_uart | 35 (8 nodes) | ~400-500 | 699 | Much improved |
| uart_baudgen | 51 | ~60-70 | 68 | Nearly matched |
| uart_interrupt | 46 | ~80-90 | 92 | Nearly matched |
| uart_receiver | 28 (8 nodes) | ~120-140 | 144 | Much improved |
| uart_transmitter | 83 | ~65-75 | 65 | Already close |

**Overall goal:** VHDL and SV IRs of similar complexity for equivalent designs.

## Testing Procedure

For each phase:

1. **Unit test:** Test fix on single simple module
2. **Integration test:** Run on all 12 modules
3. **Comparison:** Diff against SV IR
4. **Metrics:** Count ports, signals, nodes, undefined refs
5. **Visual check:** Review generated behavioral Verilog
6. **Documentation:** Update IR dumps and comparisons

## Risk Mitigation

### Risk 1: Breaking existing functionality
- **Mitigation:** Keep git branches for each phase
- **Rollback:** Can revert to working version
- **Testing:** Run regression tests after each change

### Risk 2: Incomplete VHDL AST
- **Mitigation:** May need to enhance VhdlParser.mly
- **Workaround:** Add parser debugging output
- **Escalation:** Examine vhd_front library source

### Risk 3: Complex VHDL semantics
- **Mitigation:** Start with simple test cases
- **Incremental:** Add complexity gradually
- **Reference:** Compare against known-good VHDL synthesizers

## Conclusion

The VHDL parser has **critical bugs** that prevent proper IR generation:

**Top 3 Issues:**
1. ❌ **Entity port extraction completely broken** (Priority 1)
2. ❌ **Width inference defaulting to 32-bit** (Priority 1)
3. ❌ **Missing constants causing undefined refs** (Priority 2)

**Fix strategy:**
- 6 phases, prioritized by impact
- Incremental with testing after each phase
- 3 weeks to complete all fixes

**Expected outcome:**
- VHDL IRs with complete port lists
- Correct signal widths
- No undefined references
- Comparable complexity to SV IRs
- Successful equivalence checking possible

With these fixes, the VHDL parser will generate accurate IR representations that can be properly compared and verified against SystemVerilog equivalents.
