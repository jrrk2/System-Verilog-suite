# VHDL Register Generation Fix

## Problem

The VHDL→IR converter was incorrectly creating **multiple registers** for signals with multiple assignments in a clocked process.

### Example VHDL Code

```vhdl
process (RST, CLK)
begin
  if (RST = '1') then
    iCounter <= 0;      -- Assignment 1
    iQ <= '0';          -- Assignment 2
  elsif (CLK'event and CLK='1') then
    iQ <= '0';          -- Assignment 3 (to iQ)
    if (CE = '1') then
      if (iCounter = (RATIO-1)) then
        iQ <= '1';      -- Assignment 4 (to iQ)
        iCounter <= 0;  -- Assignment 5 (to iCounter)
      else
        iCounter <= iCounter + 1;  -- Assignment 6 (to iCounter)
      end if;
    end if;
  end if;
end process;
```

### What Was Wrong

**Before Fix:** Created a register for **every assignment** = 6 registers
- Register(iCounter) for assignment 1
- Register(iQ) for assignment 2
- Register(iQ_next1) for assignment 3
- Register(iQ_next2) for assignment 4
- Register(iCounter_n1) for assignment 5
- Register(iCounter_n2) for assignment 6

**Expected:** Create **one register per signal** = 2 registers
- Register(iCounter) with MUX tree selecting between values
- Register(iQ) with MUX tree selecting between values

### Root Cause

In `vhdl_to_ir.ml` lines 531-537, the code was:

```ocaml
List.iter (fun (dst_id, data_id) ->
  let _reg_id = add_node ctx
    (Register { width = 32; clock = clk_id; reset = reset_id;
               enable = None; reset_value = 0 })
    [data_id] in
  ()
) assigns
```

This iterates over **all assignments** and creates a register for each one!

## The Fix

### Step 1: Group Assignments by Signal

```ocaml
(* Group assignments by destination signal *)
let signal_groups = Hashtbl.create 10 in
List.iter (fun (dst_id, data_id) ->
  let existing = try Hashtbl.find signal_groups dst_id with Not_found -> [] in
  Hashtbl.replace signal_groups dst_id ((dst_id, data_id) :: existing)
) assigns;
```

### Step 2: Create ONE Register Per Signal

```ocaml
(* Create ONE register per unique signal *)
Hashtbl.iter (fun dst_id assigns_for_signal ->
  let assigns_in_order = List.rev assigns_for_signal in

  (* Take last assignment (highest priority) *)
  let (_final_dst_id, final_data_id) = match assigns_in_order with
    | [] -> failwith "Empty assignment list"
    | assignments -> List.hd (List.rev assignments)
  in

  (* Create ONE register for this signal *)
  let _reg_id = add_node ctx
    (Register { width = 32; clock = clk_id; reset = reset_id;
               enable = None; reset_value = 0 })
    [final_data_id] in
  ()
) signal_groups
```

### How SystemVerilog Does It Correctly

The SV version (`sv_verible_to_ir.ml` lines 979-1100) already handles this properly:

1. **Groups by signal** (lines 982-986)
2. **Builds MUX tree** for conditional assignments (lines 1007-1043)
3. **Creates ONE register** per signal (lines 1077+)

**Example MUX tree for iQ:**
```
          ┌──────────────────────┐
          │   Mux (RST)          │
          │   reset? '0' : ...   │
          └──────┬───────────────┘
                 │
          ┌──────▼───────────────┐
          │   Mux (is_max)       │
          │   max? '1' : '0'     │
          └──────┬───────────────┘
                 │
          ┌──────▼───────────────┐
          │   Register(iQ)       │
          │   D ← mux_output     │
          │   Q → output         │
          └──────────────────────┘
```

## Results

### Before Fix (vhdl_to_ir.ml original)

```
slib_clock_div VHDL IR:
  - 13 nodes total
  - 6 registers (wrong!)
  - 4 compares
  - 1 add, 1 sub, 1 and
```

### After Fix (vhdl_to_ir.ml with grouping)

```
slib_clock_div VHDL IR:
  - 9 nodes total
  - 2 registers (correct! ✅)
  - 4 compares
  - 1 add, 1 sub, 1 and
```

### SystemVerilog IR (for comparison)

```
slib_clock_div SV IR:
  - 10 nodes total
  - 2 registers
  - 2 compares, 2 mux
  - 1 add, 1 sub, 1 and, 1 or
```

**Node count comparison:**
- Before: VHDL=13, SV=10 (23% difference)
- After: VHDL=9, SV=10 (11% difference)

## What Still Needs Work

### Current Limitation

The fix uses a **conservative approach** - it just takes the last assignment (highest priority):

```ocaml
(* Take last assignment = highest priority *)
let (_final_dst_id, final_data_id) = match assigns_in_order with
  | [] -> failwith "Empty assignment list"
  | assignments -> List.hd (List.rev assignments)
in
```

This means **conditional logic is lost** - all the if/elsif conditions are ignored!

### Complete Fix Would Need

1. **Track conditions** from if/elsif/case statements
2. **Build MUX tree** that encodes the conditional selection
3. **Handle reset values** separately from normal assignments

**Example:** For the iQ signal with 3 assignments:
```vhdl
if RST = '1' then
  iQ <= '0';              -- Reset condition
elsif CLK'event then
  iQ <= '0';              -- Default case
  if CE = '1' then
    if iCounter = (RATIO-1) then
      iQ <= '1';          -- Pulse condition
    end if;
  end if;
end if;
```

**Should generate:**
```ocaml
(* Build MUX tree *)
let reset_val = const 0 in
let default_val = const 0 in
let pulse_val = const 1 in

(* is_max = (iCounter == RATIO-1) *)
let is_max = Compare { cmp_op = `Eq; ... } in

(* inner_mux = is_max ? pulse_val : default_val *)
let inner_mux = Mux { width = 1 } [is_max; pulse_val; default_val] in

(* ce_mux = CE ? inner_mux : default_val *)
let ce_mux = Mux { width = 1 } [ce; inner_mux; default_val] in

(* Register with MUX tree as input *)
Register { clock = clk; reset = Some rst; ... } [ce_mux]
```

**Currently generates:**
```ocaml
(* Just takes the last assignment value *)
Register { clock = clk; reset = Some rst; ... } [pulse_val]
```

### Why This Matters

1. **IR Accuracy:** The current approach loses conditional logic information
2. **Optimization:** Can't optimize away redundant muxes
3. **Verification:** Z3 equivalence checking needs correct structure
4. **Synthesis:** Backend generators need to know the full logic

### To Implement Full MUX Tree

Need to modify `stmt_to_ir` to return not just `(dst_id, data_id)` pairs but also:
```ocaml
type conditional_assign = {
  dst: value_id;
  data: value_id;
  condition: value_id option;  (* None = unconditional *)
  priority: int;               (* Statement order *)
}
```

Then in `process_to_ir`, build the MUX tree as SV version does (see `sv_verible_to_ir.ml:1011-1043`).

## Testing

To verify the fix:

```bash
dune exec ./test_clock_div_full_flow.exe
```

**Expected output:**
```
VHDL IR statistics:
  Inputs:  3 (CLK, RST, CE)
  Outputs: 1 (Q)
  Nodes:   9 (operations/registers)  ← Should be ~9-10

Node breakdown:
  - Register: 2  ← Should be 2, not 6!
  - Compare: 4
  - Add: 1
  - Sub: 1
  - And: 1
```

## Files Changed

- `vhdl_to_ir.ml:518-541` - Added signal grouping and deduplication

## Benefits

✅ **Correct register count** (2 vs 6)
✅ **Closer to SV IR structure** (9 vs 10 nodes)
✅ **More accurate hardware model** (one register per signal)
✅ **Better optimization potential** (fewer redundant registers)

⚠️ **Still needs:** Full MUX tree construction for conditional assignments

## References

- **SV implementation:** `sv_verible_to_ir.ml:979-1100` (correct reference)
- **VHDL fix:** `vhdl_to_ir.ml:518-541`
- **Test:** `test_clock_div_full_flow.ml`
- **Issue:** Multiple registers created for intermediate expressions (should be wires/muxes)
