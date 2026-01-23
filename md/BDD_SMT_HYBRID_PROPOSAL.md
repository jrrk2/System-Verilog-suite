# BDD + SMT Hybrid Verification Architecture

## Motivation

Binary Decision Diagrams (BDDs) and SMT solvers have complementary strengths:

| Tool | Strengths | Weaknesses |
|------|-----------|------------|
| **BDDs** | • Canonical representation<br>• Fast for Boolean logic<br>• Excellent for control logic<br>• Good for medium state spaces | • Explode on arithmetic<br>• Require variable ordering<br>• Boolean domain only |
| **SMT (Z3)** | • Word-level reasoning<br>• Bitvector arithmetic<br>• Arrays, quantifiers<br>• Multiple theories | • No canonical form<br>• Can be slow on pure Boolean<br>• Requires encoding |

**Key Insight:** Use BDDs for control logic, Z3 for datapath arithmetic.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│  Behavioral IR (from VHDL or SystemVerilog)                 │
└────────────┬────────────────────────────────────────────────┘
             │
             ↓
     ┌───────────────┐
     │  Partitioning  │ ← Separate control from datapath
     └───────┬───────┘
             │
        ┌────┴────┐
        ↓         ↓
   ┌─────────┐  ┌──────────┐
   │ Control │  │ Datapath │
   │ Logic   │  │ (Arith)  │
   └────┬────┘  └────┬─────┘
        │            │
        ↓            ↓
   ┌─────────┐  ┌──────────┐
   │   BDD   │  │ Z3 SMT   │
   │ Verify  │  │ Verify   │
   └────┬────┘  └────┬─────┘
        │            │
        └────┬───────┘
             ↓
     ┌───────────────┐
     │   Combined    │
     │ Equivalence   │
     └───────────────┘
```

## Use Cases

### 1. Post-Synthesis Equivalence Checking

After generating gate-level netlists, verify VHDL and SV are equivalent:

**For slib_clock_div:**

```ocaml
(* Control logic: Reset, enable conditions *)
let control_signals = [
  "RST";    (* Reset signal - Boolean *)
  "CE";     (* Chip enable - Boolean *)
  "is_max"; (* iCounter == RATIO-1 - Boolean result *)
]

let verify_control_logic vhdl_ir sv_ir =
  (* Extract Boolean control expressions *)
  let vhdl_control = extract_control_logic vhdl_ir control_signals in
  let sv_control = extract_control_logic sv_ir control_signals in

  (* Convert to BDDs *)
  let vhdl_bdd = build_bdd vhdl_control in
  let sv_bdd = build_bdd sv_control in

  (* Check equivalence (canonical representation!) *)
  Bdd.equal vhdl_bdd sv_bdd

(* Datapath: Counter arithmetic *)
let datapath_signals = [
  "iCounter";     (* 2-bit counter *)
  "iCounter_next"; (* Next state value *)
]

let verify_datapath vhdl_ir sv_ir =
  (* Extract arithmetic expressions *)
  let vhdl_arith = extract_datapath vhdl_ir datapath_signals in
  let sv_arith = extract_datapath sv_ir datapath_signals in

  (* Use Z3 for word-level reasoning *)
  let z3_ctx = Z3.mk_context [] in
  let vhdl_formula = encode_z3 z3_ctx vhdl_arith in
  let sv_formula = encode_z3 z3_ctx sv_arith in

  (* Check equivalence *)
  Z3_verify.check_equivalent z3_ctx vhdl_formula sv_formula

(* Combined verification *)
let verify_module vhdl_ir sv_ir =
  let control_ok = verify_control_logic vhdl_ir sv_ir in
  let datapath_ok = verify_datapath vhdl_ir sv_ir in
  control_ok && datapath_ok
```

**Benefits:**
- BDD handles Boolean conditions efficiently
- Z3 handles `iCounter + 1` and `iCounter == RATIO-1` at word-level
- Best of both worlds!

### 2. Behavioral IR Optimization

Optimize MUX trees in the behavioral IR using BDDs:

```ocaml
(* Complex nested conditions in VHDL/SV *)
let optimize_conditional_tree stmt =
  match stmt with
  | BIf { condition = c1;
          then_stmts = [BIf { condition = c2; then_stmts = t2; else_stmts = e2 }];
          else_stmts = e1 } ->
      (* Build BDD for nested conditions *)
      let bdd = Bdd.and_ (expr_to_bdd c1) (expr_to_bdd c2) in

      (* Minimize Boolean expression *)
      let minimized = Bdd.minimize bdd in

      (* Check if conditions can be simplified *)
      if Bdd.is_equivalent bdd (expr_to_bdd (BBinOp { op = BAnd; lhs = c1; rhs = c2 }))
      then
        (* Flatten nested ifs *)
        BIf { condition = BBinOp { op = BAnd; lhs = c1; rhs = c2 };
              then_stmts = t2;
              else_stmts = merge_else e1 e2 }
      else stmt
  | _ -> stmt
```

**Example optimization:**
```vhdl
-- Before
if CE = '1' then
  if enable_flag = '1' then
    output <= '1';
  end if;
end if;

-- BDD minimization
-- After (if enable_flag is always '1' when CE is '1')
if CE = '1' then
  output <= '1';
end if;
```

### 3. State Space Exploration

For larger designs with FSMs, BDDs can explore reachable states:

```ocaml
(* Find all reachable states *)
let compute_reachable_states fsm initial_state =
  let rec fixed_point visited frontier =
    if Bdd.is_empty frontier then visited
    else
      let new_states = Bdd.and_not (compute_successors frontier) visited in
      let visited' = Bdd.or_ visited new_states in
      fixed_point visited' new_states
  in
  fixed_point (Bdd.var initial_state) (Bdd.var initial_state)

(* Verify FSM equivalence *)
let fsm_bisimilar vhdl_fsm sv_fsm =
  let vhdl_reach = compute_reachable_states vhdl_fsm vhdl_fsm.initial in
  let sv_reach = compute_reachable_states sv_fsm sv_fsm.initial in
  Bdd.equal vhdl_reach sv_reach
```

## Implementation Plan

### Phase 1: BDD Library Integration

```bash
opam install mlbdd
```

Add to `dune`:
```dune
(libraries str yojson unix hardcaml z3 mlbdd vhd_front ver_front)
```

### Phase 2: Control/Datapath Partitioning

Create `behavioral_partition.ml`:
```ocaml
(* Partition behavioral IR into control and datapath *)
type partition = {
  control: bstmt list;   (* Boolean conditions, FSM logic *)
  datapath: bstmt list;  (* Arithmetic operations *)
}

let partition_module bmod =
  (* Identify arithmetic vs Boolean expressions *)
  let rec classify_expr = function
    | BVar _ | BConst _ -> `Unknown
    | BBinOp { op = BAdd | BSub | BMul | BDiv; _ } -> `Datapath
    | BBinOp { op = BAnd | BOr | BXor; _ } -> `Control
    | BBinOp { op = BEq | BNe | BLt | BLe | BGt | BGe; lhs; rhs; _ } ->
        (* Comparison results are Boolean (control) *)
        (* But operands might be arithmetic (datapath) *)
        `Both
    | _ -> `Unknown
  in

  (* Split statements by classification *)
  ...
```

### Phase 3: BDD Verification Backend

Create `behavioral_bdd_verify.ml`:
```ocaml
open Mlbdd

(* Convert behavioral IR expression to BDD *)
let rec expr_to_bdd ctx = function
  | BVar name -> Bdd.var (get_var_id ctx name)
  | BConst { value = 0; _ } -> Bdd.false_
  | BConst { value = 1; _ } -> Bdd.true_
  | BBinOp { op = BAnd; lhs; rhs; _ } ->
      Bdd.and_ (expr_to_bdd ctx lhs) (expr_to_bdd ctx rhs)
  | BBinOp { op = BOr; lhs; rhs; _ } ->
      Bdd.or_ (expr_to_bdd ctx lhs) (expr_to_bdd ctx rhs)
  | BBinOp { op = BXor; lhs; rhs; _ } ->
      Bdd.xor (expr_to_bdd ctx lhs) (expr_to_bdd ctx rhs)
  | BUnOp { op = BNot; operand; _ } ->
      Bdd.not_ (expr_to_bdd ctx operand)
  | _ -> failwith "Not a Boolean expression"

(* Verify equivalence using BDDs *)
let verify_control_equivalent vhdl_expr sv_expr =
  let ctx = create_bdd_context () in
  let vhdl_bdd = expr_to_bdd ctx vhdl_expr in
  let sv_bdd = expr_to_bdd ctx sv_expr in
  Bdd.equal vhdl_bdd sv_bdd
```

### Phase 4: Hybrid Verification

Create `behavioral_hybrid_verify.ml`:
```ocaml
(* Combined BDD + SMT verification *)
let verify_behavioral_equivalent vhdl_bir sv_bir =
  (* Partition into control and datapath *)
  let vhdl_parts = Behavioral_partition.partition_module vhdl_bir in
  let sv_parts = Behavioral_partition.partition_module sv_bir in

  (* Verify control logic with BDDs *)
  let control_ok = Behavioral_bdd_verify.verify_control_equivalent
    vhdl_parts.control sv_parts.control in

  (* Verify datapath with Z3 *)
  let datapath_ok = Behavioral_z3_verify.verify_datapath_equivalent
    vhdl_parts.datapath sv_parts.datapath in

  match (control_ok, datapath_ok) with
  | (true, true) ->
      Printf.printf "✅ Modules are equivalent (BDD + SMT verified)\n";
      true
  | (false, _) ->
      Printf.printf "❌ Control logic differs\n";
      false
  | (_, false) ->
      Printf.printf "❌ Datapath differs\n";
      false
```

## Expected Performance

For **slib_clock_div**:

| Component | Tool | Expected Performance |
|-----------|------|---------------------|
| Reset logic (RST = '1') | BDD | < 1ms (2 variables) |
| Enable logic (CE = '1') | BDD | < 1ms (1 variable) |
| Comparison (iCounter == 3) | Z3 | ~10ms (2-bit bitvector) |
| Increment (iCounter + 1) | Z3 | ~10ms (2-bit arithmetic) |
| **Total** | **Hybrid** | **~20ms** |

Compare to current Z3-only approach: ~50ms

**Speedup:** 2.5x for this simple module

For larger modules with complex control logic: **10-100x speedup possible**

## When to Use BDDs vs Z3

| Use BDDs When: | Use Z3 When: |
|----------------|--------------|
| • Pure Boolean logic | • Arithmetic operations |
| • Control FSMs | • Bitvector operations |
| • Small-medium state space | • Arrays, memory |
| • Canonical form needed | • Quantifiers |
| • < 1000 Boolean variables | • Mixed theories |

## Limitations and Risks

### Variable Ordering Problem

BDDs are sensitive to variable ordering:

```ocaml
(* Good ordering: related variables together *)
let good_order = ["clk"; "rst"; "enable"; "state0"; "state1"]

(* Bad ordering: interleaved *)
let bad_order = ["clk"; "state0"; "rst"; "state1"; "enable"]
```

**Solution:** Use heuristics (e.g., input variables first, then state variables).

### Arithmetic Explosion

Don't use BDDs for:
- Multiplication
- Division
- Large adders (> 32 bits)

**Solution:** Partition and use Z3 for these.

### Memory Overhead

BDDs can consume significant memory:
- 1000 Boolean variables ≈ 100MB BDD size (typical)
- Worst case: exponential blowup

**Solution:** Set memory limits and fall back to Z3 if BDD grows too large.

## Conclusion

**Recommendation:** Implement BDD verification as an **optional optimization**:

```ocaml
let verify_behavioral_ir vhdl_bir sv_bir use_bdds =
  if use_bdds then
    (* Try hybrid BDD + SMT approach *)
    try
      verify_hybrid vhdl_bir sv_bir
    with Bdd.Memory_limit_exceeded ->
      (* Fall back to Z3 only *)
      Printf.printf "⚠️  BDD memory limit exceeded, using Z3 only\n";
      verify_z3_only vhdl_bir sv_bir
  else
    (* Z3 only (current approach) *)
    verify_z3_only vhdl_bir sv_bir
```

**Benefits:**
- 2-100x speedup for Boolean-heavy designs
- Canonical representation for equivalence checking
- Complements existing Z3 infrastructure

**Non-benefits:**
- Won't help with the register inference bug (architectural fix needed)
- Won't help with IR lowering
- Adds complexity and dependencies

**Overall:** Worth implementing for **post-synthesis verification** and **optimization**, but not essential for the behavioral IR layer itself.
