# Language-Neutral IR Architecture Proposal

## Current Problem

Each language frontend (VHDL, SystemVerilog) independently converts to the hardware dataflow IR (`opt_ir`), which means:

❌ **Redundant logic** - Each frontend reimplements optimization/lowering
❌ **Inconsistent behavior** - VHDL creates 6 registers, SV creates 2 registers
❌ **Hard to maintain** - Bug fixes need to be duplicated
❌ **Can't share optimizations** - SSA, CSE, DCE must be per-language

### Current Architecture

```
VHDL AST ──────────┐
                   ├──> opt_ir (dataflow) ──> Backend
SystemVerilog AST ─┘
```

Each frontend tries to do everything at once:
- Parse source
- Resolve names/types
- Optimize (group registers, build MUX trees, etc.)
- Generate hardware dataflow graph
- Apply SSA, sharing, etc.

**Result:** The VHDL register bug we just fixed - each frontend has different logic for when to create registers vs wires.

## Proposed Architecture (LLVM-Style)

```
┌──────────────┐  ┌──────────────┐
│  VHDL        │  │ SystemVerilog│
│  Frontend    │  │  Frontend    │
└──────┬───────┘  └──────┬───────┘
       │                 │
       └────────┬────────┘
                ↓
    ╔═══════════════════════════╗
    ║  Behavioral IR            ║  ← Language-neutral
    ║  (High-level, SSA form)   ║  ← Statement-based
    ╚═══════════════════════════╝
                ↓
    ╔═══════════════════════════╗
    ║  Optimization Passes      ║  ← Shared across languages
    ║  - SSA construction       ║
    ║  - Dead code elimination  ║
    ║  - Common subexpr elim    ║
    ║  - Constant propagation   ║
    ║  - Operator sharing       ║
    ╚═══════════════════════════╝
                ↓
    ╔═══════════════════════════╗
    ║  Dataflow IR (opt_ir)     ║  ← Hardware-specific
    ║  - Register inference     ║  ← Low-level graph
    ║  - MUX tree generation    ║
    ╚═══════════════════════════╝
                ↓
    ┌───────────┴─────────────┐
    ↓                         ↓
┌────────┐  ┌────────┐  ┌─────────┐
│Verilog │  │  VHDL  │  │Hardcaml │
│Backend │  │Backend │  │ Backend │
└────────┘  └────────┘  └─────────┘
```

## Layer 1: Behavioral IR (Language-Neutral)

### Design Goals

✅ **Language-independent** - No VHDL-isms or SV-isms
✅ **High-level** - Close to source semantics
✅ **SSA-friendly** - Easy to convert to SSA form
✅ **Typed** - Explicit widths, signed/unsigned
✅ **Hierarchical** - Modules, processes, functions

### Type Definition

```ocaml
(* Behavioral IR - Language-neutral intermediate representation *)

type width = int

type signedness = Signed | Unsigned

type btype =
  | BInt of { width: width; signed: signedness }
  | BBool
  | BArray of { element: btype; size: int }
  | BStruct of { fields: (string * btype) list }

type bexpr =
  | BVar of string
  | BConst of { value: int; width: width }
  | BBinOp of { op: binop; lhs: bexpr; rhs: bexpr; result_type: btype }
  | BUnOp of { op: unop; operand: bexpr; result_type: btype }
  | BSelect of { array: bexpr; index: bexpr }
  | BSlice of { signal: bexpr; msb: int; lsb: int }
  | BConcat of bexpr list
  | BReplicate of { count: int; value: bexpr }
  | BCond of { condition: bexpr; then_val: bexpr; else_val: bexpr }
  | BCall of { func: string; args: bexpr list }

and binop =
  | BAdd | BSub | BMul | BDiv | BMod
  | BAnd | BOr | BXor
  | BShl | BShr | BAshr
  | BEq | BNe | BLt | BLe | BGt | BGe

and unop =
  | BNot | BNeg
  | BRedAnd | BRedOr | BRedXor

type bstmt =
  | BAssign of { lhs: string; rhs: bexpr }
  | BIf of { condition: bexpr; then_stmts: bstmt list; else_stmts: bstmt list }
  | BCase of { selector: bexpr; cases: (bexpr * bstmt list) list; default: bstmt list }
  | BWhile of { condition: bexpr; body: bstmt list }
  | BFor of { init: bstmt; condition: bexpr; update: bstmt; body: bstmt list }
  | BBlock of bstmt list
  | BCall of { func: string; args: bexpr list }
  | BReturn of bexpr option

type sensitivity =
  | BPosEdge of string
  | BNegEdge of string
  | BLevel of string
  | BAny

type bprocess =
  | BCombinational of {
      name: string;
      sensitivity: sensitivity list;
      body: bstmt list;
    }
  | BSequential of {
      name: string;
      clock: string;
      clock_edge: [`Pos | `Neg];
      reset: string option;
      reset_edge: [`Pos | `Neg] option;
      reset_async: bool;
      body: bstmt list;
    }

type bsignal = {
  name: string;
  stype: btype;
  direction: [`Input | `Output | `Internal];
  initial_value: bexpr option;
}

type bmodule = {
  name: string;
  params: (string * int) list;  (* Parameters with values *)
  signals: bsignal list;
  processes: bprocess list;
  instances: binstance list;
}

and binstance = {
  inst_name: string;
  module_name: string;
  param_values: (string * int) list;
  port_connections: (string * bexpr) list;
}

type bprogram = {
  modules: bmodule list;
}
```

### Key Features

1. **No language-specific constructs**
   - No "process" vs "always" distinction
   - Unified sequential/combinational model
   - Generic operators (not VHDL "std_logic_arith" or SV "$clog2")

2. **Explicit types**
   - Every expression has a type
   - Width inference already done
   - Signed vs unsigned explicit

3. **Normalized structure**
   - Sequential blocks have explicit clock/reset
   - All edges specified (posedge, negedge)
   - Sensitivity lists standardized

4. **Ready for SSA**
   - Assignment targets are variable names (strings)
   - Easy to rename variables for SSA: `x` → `x_1`, `x_2`, etc.

## Layer 2: Optimization Passes (Shared)

### SSA Construction

**Input:** Behavioral IR with multiple assignments to same variable
**Output:** SSA form where each variable assigned once

```ocaml
(* Before SSA *)
if condition then
  x := a + b
else
  x := c + d
use x

(* After SSA *)
if condition then
  x_1 := a + b
else
  x_2 := c + d
x_3 := φ(x_1, x_2)  (* Phi node *)
use x_3
```

### Common Subexpression Elimination

```ocaml
(* Before *)
y := a + b
z := a + b  (* Duplicate computation *)

(* After *)
temp := a + b
y := temp
z := temp
```

### Constant Propagation

```ocaml
(* Before *)
x := 5
y := x + 3  (* x is known to be 5 *)

(* After *)
x := 5
y := 8  (* Folded at compile time *)
```

### Dead Code Elimination

```ocaml
(* Before *)
x := a + b
y := c + d  (* y never used *)
use x

(* After *)
x := a + b
use x
```

### Register Inference

**THIS IS WHERE THE BUG FIX BELONGS!**

```ocaml
(* Input: Multiple assignments in sequential block *)
process (clock, reset)
  if reset then
    q <= '0'
  elsif clock'event then
    q <= '0'
    if enable then
      q <= '1'

(* Output: Single register + MUX tree *)
mux1 := if enable then 1 else 0
mux2 := if reset then 0 else mux1
register q (clock, mux2)
```

This pass runs **once** on behavioral IR, not separately in each frontend!

## Layer 3: Dataflow IR (opt_ir) - Hardware Specific

This is what we currently have - a dataflow graph of operations:

```ocaml
type operation =
  | Add of { width: int; signed: bool }
  | Mux of { width: int }
  | Register of { width: int; clock: value_id; reset: value_id option; ... }
  (* ... existing definition ... *)

type node = {
  node_id: value_id;
  node_op: operation;
  node_inputs: value_id list;
}
```

**Purpose:**
- Dataflow analysis
- Scheduling
- Technology mapping
- Backend code generation

## Migration Path

### Phase 1: Define Behavioral IR

1. Create `behavioral_ir.ml` with type definitions above
2. Add pretty-printer for debugging
3. Add JSON serialization

### Phase 2: Convert Frontends

#### VHDL Frontend
```ocaml
(* vhdl_to_behavioral.ml *)
let convert_vhdl_to_behavioral vhdl_ast =
  (* Convert VHDL processes to BProcess *)
  (* Convert signals to BSignal *)
  (* Normalize types to BInt/BBool *)
  ...
```

#### SystemVerilog Frontend
```ocaml
(* sv_to_behavioral.ml *)
let convert_sv_to_behavioral sv_ast =
  (* Convert always blocks to BProcess *)
  (* Convert wires/regs to BSignal *)
  (* Normalize operators *)
  ...
```

### Phase 3: Implement Optimization Passes

```ocaml
(* behavioral_ssa.ml *)
let construct_ssa behavioral_ir = ...

(* behavioral_cse.ml *)
let eliminate_common_subexpressions behavioral_ir = ...

(* behavioral_dce.ml *)
let eliminate_dead_code behavioral_ir = ...

(* behavioral_registers.ml *)
let infer_registers behavioral_ir = ...
  (* This is where we fix "one register per signal" *)
```

### Phase 4: Lower to Dataflow IR

```ocaml
(* behavioral_to_dataflow.ml *)
let lower_to_dataflow behavioral_ir =
  (* Convert BProcess to Register + Mux nodes *)
  (* Generate operation graph *)
  (* Already SSA, so easy! *)
  ...
```

## Benefits

### 1. Consistency

✅ Both VHDL and SV produce identical behavioral IR for same hardware
✅ Optimizations apply uniformly
✅ Register inference happens once, correctly

### 2. Maintainability

✅ Bug fixes apply to all languages
✅ Clear separation of concerns
✅ Each layer has single responsibility

### 3. Extensibility

✅ Easy to add new languages (Chisel, Bluespec, etc.)
✅ Easy to add new optimizations
✅ Easy to add new backends

### 4. Verification

✅ Can verify behavioral IR equivalence (easier than dataflow)
✅ Each pass can be tested independently
✅ Optimization passes are testable units

## Example: slib_clock_div

### VHDL Input
```vhdl
process (CLK, RST)
begin
  if RST = '1' then
    iCounter <= 0;
    iQ <= '0';
  elsif CLK'event and CLK='1' then
    iQ <= '0';
    if CE = '1' then
      if iCounter = (RATIO-1) then
        iQ <= '1';
        iCounter <= 0;
      else
        iCounter <= iCounter + 1;
      end if;
    end if;
  end if;
end process;
```

### Behavioral IR (Normalized)
```ocaml
BModule {
  name = "slib_clock_div";
  signals = [
    { name = "CLK"; stype = BBool; direction = `Input; ... };
    { name = "RST"; stype = BBool; direction = `Input; ... };
    { name = "CE"; stype = BBool; direction = `Input; ... };
    { name = "iCounter"; stype = BInt { width = 2; signed = Unsigned }; ... };
    { name = "iQ"; stype = BBool; ... };
  ];
  processes = [
    BSequential {
      name = "CD_PROC";
      clock = "CLK";
      clock_edge = `Pos;
      reset = Some "RST";
      reset_edge = Some `Pos;
      reset_async = true;
      body = [
        BIf {
          condition = BVar "CE";
          then_stmts = [
            BIf {
              condition = BBinOp { op = BEq; lhs = BVar "iCounter"; rhs = BConst { value = 3; width = 2 }; ... };
              then_stmts = [
                BAssign { lhs = "iQ"; rhs = BConst { value = 1; width = 1 } };
                BAssign { lhs = "iCounter"; rhs = BConst { value = 0; width = 2 } };
              ];
              else_stmts = [
                BAssign { lhs = "iQ"; rhs = BConst { value = 0; width = 1 } };
                BAssign { lhs = "iCounter"; rhs = BBinOp { op = BAdd; lhs = BVar "iCounter"; rhs = BConst { value = 1; width = 2 }; ... } };
              ];
            }
          ];
          else_stmts = [
            BAssign { lhs = "iQ"; rhs = BConst { value = 0; width = 1 } };
          ];
        }
      ];
    }
  ];
}
```

### After SSA + Register Inference
```ocaml
(* SSA form - each variable assigned once per path *)
iCounter_reset = 0
iQ_reset = 0

if CE then
  if iCounter == 3 then
    iQ_1 = 1
    iCounter_1 = 0
  else
    iQ_2 = 0
    iCounter_2 = iCounter + 1
  iQ_3 = φ(iQ_1, iQ_2)
  iCounter_3 = φ(iCounter_1, iCounter_2)
else
  iQ_3 = 0
  iCounter_3 = iCounter

(* Register inference identifies state elements *)
Register(iCounter) <- φ(reset ? iCounter_reset : iCounter_3)
Register(iQ) <- φ(reset ? iQ_reset : iQ_3)
```

### Dataflow IR (opt_ir)
```ocaml
(* Now we have 2 registers with MUX trees - correct! *)
node_1: Compare(iCounter, 3, Eq) -> cmp_result
node_2: Mux(cmp_result, 1, 0) -> iQ_val_1
node_3: Add(iCounter, 1) -> counter_inc
node_4: Mux(cmp_result, 0, counter_inc) -> counter_val_1
node_5: Mux(CE, iQ_val_1, 0) -> iQ_next
node_6: Mux(CE, counter_val_1, iCounter) -> counter_next
node_7: Mux(RST, 0, iQ_next) -> iQ_d
node_8: Mux(RST, 0, counter_next) -> counter_d
node_9: Register(clock=CLK, data=iQ_d) -> iQ
node_10: Register(clock=CLK, data=counter_d) -> iCounter
```

**Result:** 2 registers (correct!), generated by shared optimization pass.

## Implementation Priority

### Immediate (Phase 1)
1. Define behavioral IR types in `behavioral_ir.ml`
2. Add JSON serialization for debugging

### Short-term (Phase 2)
1. Implement `vhdl_to_behavioral.ml` converter
2. Implement `sv_to_behavioral.ml` converter
3. Verify both produce identical IR for equivalent hardware

### Medium-term (Phase 3)
1. Implement SSA construction pass
2. Implement register inference pass (fixes the bug once and for all!)
3. Implement basic optimizations (CSE, DCE, constant folding)

### Long-term (Phase 4)
1. Implement `behavioral_to_dataflow.ml` lowering pass
2. Update verification to work on behavioral IR (easier!)
3. Add more sophisticated optimizations

## Comparison to Current Approach

### Current
```
VHDL → process_to_ir() → [tries to create registers correctly]
                       → [builds MUX trees]
                       → [does SSA manually]
                       → opt_ir

SV → always_to_ir() → [tries to create registers correctly]
                    → [builds MUX trees]
                    → [does SSA manually]
                    → opt_ir
```

Each frontend reinvents the wheel!

### Proposed
```
VHDL → behavioral_ir ─┐
                      ├→ SSA pass → Register inference → CSE → DCE → opt_ir
SV → behavioral_ir ───┘
```

Register inference happens **once**, in a **shared pass**, that is **thoroughly tested**.

## References

- **LLVM Architecture:** https://llvm.org/docs/LangRef.html
- **SSA Book:** "SSA-based Compiler Design" by Rastello & Bouchez
- **GHC Core:** Haskell's intermediate representation
- **FIRM:** Academic IR for optimization research

## Next Steps

1. Review and refine behavioral IR type definitions
2. Create `behavioral_ir.ml` with types and pretty-printer
3. Start with simple VHDL→Behavioral converter for one module
4. Verify it produces sensible output
5. Implement SV→Behavioral converter for same module
6. Compare outputs - should be identical!

Then build optimization passes incrementally, testing each one.
