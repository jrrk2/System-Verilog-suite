# HardCaml Sequential Circuit Generation - FIXED!

## Summary

Successfully debugged and fixed the HardCaml backend to generate sequential circuits with proper register handling. The traffic signal controller now generates valid Verilog with registers and always blocks.

## Issues Fixed

### 1. Invalid LHS Expression Errors

**Problem**: SSA variables like `delay_3s_1`, `delay_3s_2_3` were not being created, causing "LHS expression is Invalid" errors.

**Root Cause**: The `extract_signals()` function only looked at top-level `Sv_ast.Var` declarations, missing SSA variables created by Verilator's transformation.

**Solution**: Added `extract_lhs_variables()` function that scans ALL assignments to find every LHS variable:

```ocaml
let extract_lhs_variables stmts =
  let vars = Hashtbl.create 64 in
  let rec extract_from_expr = function
    | Sv_ast.VarRef { name; dtype_ref; _ } ->
        let width = extract_width dtype_ref in
        Hashtbl.replace vars name width
    | Sv_ast.VarRef' { name; _ } ->
        Hashtbl.replace vars name 1  (* VarRef' doesn't have width *)
    | _ -> ()
  in
  let rec scan_stmt = function
    | Sv_ast.Assign { lhs; _ } | Sv_ast.AssignW { lhs; _ } ->
        extract_from_expr lhs
    | Sv_ast.Always { stmts; _ } -> List.iter scan_stmt stmts
    | ...
  in
  List.iter scan_stmt stmts;
  Hashtbl.fold (fun name width acc -> (name, width, false) :: acc) vars []
```

### 2. Blocking vs Non-Blocking Assignment Confusion

**Initial Mistake**: I thought blocking and non-blocking assignments should be handled differently - blocking as combinational Signals, non-blocking as sequential Variables.

**User Correction**: "Blocking and non-blocking assignments are identical except that the blocking assignment creates a temporary expression that will be used whenever that variable is referred to up to the next clock cycle."

**Key Insight**: Verilator has already transformed the code! Original blocking assignments like:
```verilog
always @(posedge clk) begin
  x = x + 2;  // blocking
  y = x;
end
```

Are transformed to SSA form:
```verilog
always @(posedge clk) begin
  x_1 = x + 2;   // blocking (temporary)
  y <= x_1;      // non-blocking
  x <= x_1;      // non-blocking
end
```

**Solution**: Treat ALL assignments in always blocks the same way - use the Always API with Variables:
- State elements (non-blocking targets): `Variable.reg spec ~width`
- SSA temporaries (blocking targets): `Variable.wire ~default:signal`

### 3. Continuous Assignments Creating Unassigned Variables

**Problem**: "circuit input signal must have a port name (unassigned wire?)" error. Variable `enable_clk` was created but never assigned in always blocks.

**Root Cause**: `enable_clk` was assigned with `AssignW` (continuous assignment), but continuous assignments should NOT use the Always API.

**Solution**: Handle continuous assignments separately - store them as Signals, not Variables:

```ocaml
(* Process continuous assignments *)
List.iter (fun (lhs, rhs) ->
  match lhs with
  | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
      let rhs_sig = sig' (expr_to_remap decls rhs) in
      Hashtbl.replace decls name (Sig rhs_sig);
      Printf.eprintf "        Continuous: %s = <expr> (stored as Sig)\n%!" name
  | _ -> ()
) cont_assigns;
```

## Test Results

### Traffic Signal Controller

**Input**: `sysver_tests/Controller_for_traffic_signal.sv`
- 3 inputs: sensor, clk, rst
- 2 outputs: highway_signal[2], lane_signal[2]
- State machine with 4 states
- 3 state elements: count[33], count_delay[33], current_state[2]

**Generated Output**: `results/decompile_VController_for_traffic_signal.tree.json.sv`

```verilog
module traffic_signal (
    sensor,
    clk,
    rst,
    highway_signal,
    lane_signal
);

    input sensor;
    input clk;
    input rst;
    output [1:0] highway_signal;
    output [1:0] lane_signal;

    reg [32:0] _47;  // Register 1: count
    reg [32:0] _41;  // Register 2: count_delay
    reg [1:0] _20;   // Register 3: current_state

    // ... 100+ lines of combinational logic ...

    always @(posedge clk) begin
        _47 <= _8;   // Non-blocking assignment
    end

    always @(posedge clk) begin
        _41 <= _10;
    end

    always @(posedge clk) begin
        _20 <= _12;
    end

    assign highway_signal = _14;
    assign lane_signal = _1;

endmodule
```

### Validation

✅ **Verilator Lint**: PASS (no warnings or errors)
✅ **Registers**: 3 proper `reg` declarations
✅ **Always Blocks**: 3 `always @(posedge clk)` blocks
✅ **Non-Blocking Assignments**: All use `<=`
✅ **Total Lines**: 203 lines of valid Verilog

## Code Changes

### Modified Files

1. **sv_gen_hardcaml.ml**:
   - Added `extract_lhs_variables()` function to find all LHS variables including SSA
   - Modified variable creation to use Variables for ALL LHS targets (both blocking and non-blocking)
   - Separated continuous assignment handling from always block handling
   - Added debugging output for tracking assignments

### Key Functions

**Extract LHS Variables** (lines 102-131):
```ocaml
let extract_lhs_variables stmts =
  (* Scans all assignments to find every LHS variable *)
```

**Build Circuit** (lines 548-573):
```ocaml
(* Create Variables for ALL LHS targets *)
(* State elements (non-blocking) use Variable.reg *)
(* SSA temporaries (blocking) use Variable.wire *)
List.iter (fun (name, width, _signed) ->
  if not (Hashtbl.mem decls name) then begin
    let is_state = Hashtbl.mem state_elements name in
    if is_state && clock_signal <> None then begin
      let clk = Option.get clock_signal in
      let spec = Reg_spec.create ~clock:clk () in
      Hashtbl.add decls name (Var (Variable.reg spec ~width))
    end else begin
      let default_sig = Signal.of_int ~width 0 in
      let wire_var = Variable.wire ~default:default_sig in
      Hashtbl.add decls name (Var wire_var)
    end
  end
) all_vars;
```

**Continuous Assignments** (lines 581-591):
```ocaml
(* Continuous assignments don't use Always API *)
List.iter (fun (lhs, rhs) ->
  match lhs with
  | Sv_ast.VarRef { name; _ } | Sv_ast.VarRef' { name; _ } ->
      let rhs_sig = sig' (expr_to_remap decls rhs) in
      Hashtbl.replace decls name (Sig rhs_sig);
  | _ -> ()
) cont_assigns;
```

## What Was Learned

1. **SSA Transformation**: Verilator automatically transforms blocking assignments into SSA form with temporary variables (`x_1`, `x_2`, etc.)

2. **Always API Usage**: HardCaml's Always API is for ALL assignments within always blocks, not just sequential logic

3. **Variable Types**:
   - `Variable.reg spec ~width`: For state elements (registers/flip-flops)
   - `Variable.wire ~default:signal`: For temporary SSA variables within always blocks
   - `Sig s`: For continuous assignments and combinational logic

4. **Assignment Types**:
   - `Sv_ast.Assign { is_blocking=false }`: Non-blocking (<=) - state elements
   - `Sv_ast.Assign { is_blocking=true }`: Blocking (=) - SSA temporaries
   - `Sv_ast.AssignW`: Continuous (assign) - pure combinational

## Next Steps

1. **Run Z3 Verification**: Compare original vs generated circuit with state as pseudo-inputs
2. **Test More Designs**: Try other sequential circuits (counters, shift registers, FSMs)
3. **Reset Support**: Add reset signal handling to Reg_spec
4. **Multiple Clock Domains**: Handle designs with multiple clocks
