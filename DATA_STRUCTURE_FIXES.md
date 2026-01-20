# HardCaml Backend - Data Structure Analysis & Fixes

## Problem: No Significant Conversion

Your JSON dump revealed several mismatches between the actual AST structure and what the HardCaml backend was expecting.

## Data Structure Analysis from dump.json

### 1. **Direction Strings - UPPERCASE** ❌→✅

**Actual data**:
```json
{
  "name": "a",
  "direction": "INPUT",    ← UPPERCASE
  "var_type": "PORT"
}
```

**Original code (incorrect)**:
```ocaml
| Sv_ast.Var { name; dtype_ref; direction = "input"; _ } ->  (* lowercase *)
```

**Fixed code**:
```ocaml
| Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ } 
  when direction = "INPUT" || direction = "input" ->
```

### 2. **Operators - UPPERCASE** ❌→✅

**Actual data**:
```json
{
  "op": "ADD",    ← UPPERCASE
  "lhs": [...],
  "rhs": [...]
}
```

**Original code (incorrect)**:
```ocaml
match op with
| "Add" -> ...      (* Mixed case *)
| "Sub" -> ...
```

**Fixed code**:
```ocaml
match String.uppercase_ascii op with
| "ADD" | "VADD" -> Sig (width_match (+:) lhs_sig rhs_sig)
| "SUB" | "VSUB" -> Sig (width_match (-:) lhs_sig rhs_sig)
```

### 3. **Constants Use `Const` Node** ❌→✅

**Actual data**:
```json
{
  "conditions": [
    ["Const", { "name": "4'h0", "dtype_ref": [...] }]
  ]
}
```

**Original code (missing)**:
```ocaml
(* Only handled Text nodes, not Const nodes *)
| Sv_ast.Text { text } -> ...
```

**Fixed code**:
```ocaml
| Sv_ast.Const { name; dtype_ref } ->
    let width = extract_width dtype_ref in
    let (parsed_width, value) = parse_const_value name in
    let final_width = if width > 1 then width else parsed_width in
    Con (Constant.of_int ~width:final_width value)
```

### 4. **Constant Format Parsing** ❌→✅

**Examples from your data**:
- `"4'h0"` - 4-bit hex value 0
- `"4'h1"` - 4-bit hex value 1
- `"32'sh8"` - 32-bit signed hex value 8

**Parser implementation**:
```ocaml
let parse_const_value name =
  try
    (* Parse: <width>'<format><value> *)
    let parts = String.split_on_char '\'' name in
    match parts with
    | [width_str; format_value] ->
        let width = int_of_string width_str in
        let is_signed = format_value.[0] = 's' in
        let fmt_start = if is_signed then 1 else 0 in
        let format_char = format_value.[fmt_start] in
        let value_str = String.sub format_value (fmt_start + 1) ... in
        
        let value = match format_char with
          | 'h' -> int_of_string ("0x" ^ value_str)
          | 'd' -> int_of_string value_str
          | 'b' -> int_of_string ("0b" ^ value_str)
          | 'o' -> int_of_string ("0o" ^ value_str)
        in
        (width, value)
```

### 5. **Port Detection** ❌→✅

**Actual data shows**:
```json
{
  "name": "WIDTH",
  "var_type": "GPARAM",     ← Parameter, not port
  "direction": "NONE",
  "is_param": true
}
vs
{
  "name": "a",
  "var_type": "PORT",       ← This is a port
  "direction": "INPUT"
}
```

**Fixed code checks var_type**:
```ocaml
| Sv_ast.Var { name; dtype_ref; direction; var_type = "PORT"; _ } 
  when direction = "INPUT" || direction = "input" ->
```

### 6. **Output Port Handling** ❌→✅

**Problem**: Outputs need to be Variables so they can be assigned in always blocks.

**Fixed code**:
```ocaml
| `Output ->
    (* Create output as a variable that can be assigned *)
    if not (Hashtbl.mem decls name) then
      Hashtbl.add decls name (Var (Variable.wire ~default:(zero width)))
```

## Complete Module Example from Your Data

```json
{
  "name": "alu",
  "stmts": [
    // Ports
    { "name": "a", "direction": "INPUT", "var_type": "PORT", "range": "7:0" },
    { "name": "b", "direction": "INPUT", "var_type": "PORT", "range": "7:0" },
    { "name": "op", "direction": "INPUT", "var_type": "PORT", "range": "3:0" },
    { "name": "y", "direction": "OUTPUT", "var_type": "PORT", "range": "7:0" },
    
    // Always block
    {
      "always": "always_comb",
      "stmts": [
        {
          "Case": {
            "expr": ["VarRef", { "name": "op" }],
            "items": [
              {
                "conditions": [["Const", { "name": "4'h0" }]],
                "statements": [
                  {
                    "Assign": {
                      "lhs": ["VarRef", { "name": "y" }],
                      "rhs": [
                        "BinaryOp",
                        {
                          "op": "ADD",
                          "lhs": ["VarRef", { "name": "a" }],
                          "rhs": ["VarRef", { "name": "b" }]
                        }
                      ]
                    }
                  }
                ]
              }
            ]
          }
        }
      ]
    }
  ]
}
```

## How the Fixed Backend Processes This

### Step 1: Extract Ports
```ocaml
ports = [
  ("a", 8, `Input);
  ("b", 8, `Input);
  ("op", 4, `Input);
  ("y", 8, `Output);
]
```

### Step 2: Create Declarations
```ocaml
decls["a"] = Sig (Signal.input "a" 8)
decls["b"] = Sig (Signal.input "b" 8)
decls["op"] = Sig (Signal.input "op" 4)
decls["y"] = Var (Variable.wire ~default:(zero 8))  (* Output as Variable *)
```

### Step 3: Process Always Block
```ocaml
(* Case statement *)
expr_sig = decls["op"]  (* 4-bit signal *)

(* Case item 0: op == 4'h0 *)
condition = Const "4'h0" → parse to Constant.of_int ~width:4 0
                         → convert to Signal.of_constant
statements = [
  y <-- (a +:. b)  (* Blocking assignment *)
]

(* Case item 1: op == 4'h1 *)
condition = Const "4'h1" → Signal.of_constant (Constant.of_int ~width:4 1)
statements = [
  y <-- (a -:. b)
]
```

### Step 4: Compile Always Block
```ocaml
switch expr_sig [
  (constant_0, [y <-- (a +:. b)]);
  (constant_1, [y <-- (a -:. b)]);
  ...
]
Always.compile [switch_block]
```

### Step 5: Build Circuit
```ocaml
outputs = [output "y" decls["y"].value]
Circuit.create_exn ~name:"alu" outputs
```

### Step 6: Generate Verilog
```verilog
module alu (
  a,
  b,
  op,
  y
);
  input [7:0] a;
  input [7:0] b;
  input [3:0] op;
  output [7:0] y;
  
  reg [7:0] _y;
  
  always @(*) begin
    case (op)
      4'h0: _y = a + b;
      4'h1: _y = a - b;
      ...
    endcase
  end
  
  assign y = _y;
endmodule
```

## Testing the Fixes

### Debug Output You'll See

```
HardCaml backend: Starting processing
  Processing Netlist with 1 nodes
  Processing Module: alu with 5 statements
    Building circuit for alu
      Found 4 ports: a(in:8) b(in:8) op(in:4) y(out:8) 
      Found 0 internal signals
HardCaml backend: Processed 1 circuits
```

### Verification

Run the unified tool:
```bash
./sv_main_unified scan hardcaml output/
```

Expected:
- Should process ALU module successfully
- Generate Verilog with case statement
- No "Invalid" remap warnings

## Summary of Fixes Applied

| Issue | Original | Fixed | Impact |
|-------|----------|-------|--------|
| Direction case | `"input"` | `"INPUT"` or `"input"` | ✅ Ports now detected |
| Operator case | `"Add"` | `"ADD"` (uppercase) | ✅ Operations now work |
| Const nodes | Not handled | Parse `"4'h0"` format | ✅ Constants now work |
| Const parsing | N/A | Handle h/d/b/o formats | ✅ All formats supported |
| Port detection | direction only | var_type + direction | ✅ No false positives |
| Output ports | Not created | Create as Variables | ✅ Can be assigned |
| Debugging | None | Detailed traces | ✅ Easy to diagnose |

## Next Steps

1. **Test with your ALU module**:
   ```bash
   cd unified-decompiler
   dune build sv_main_unified.exe
   ./_build/default/sv_main_unified.exe scan hardcaml output/
   ```

2. **Check debug output** - Should show:
   - Module detection
   - Port extraction
   - Circuit count

3. **Verify Verilog output** - Should contain:
   - Proper module declaration
   - Case statement with operations
   - Correct bit widths

4. **Report any remaining issues** with:
   - Module name
   - Expected vs actual output
   - Error messages

The backend should now correctly process your ALU and other modules!
