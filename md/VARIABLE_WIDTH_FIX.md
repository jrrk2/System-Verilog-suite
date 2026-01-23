# HardCaml Variable Width Issue - Analysis & Fix

## Error Message

```
Warning: Circuit build failed for alu: ("[select] got [hi < lo]" (hi 0) (lo 8))
```

## Root Cause

The error `(hi 0) (lo 8)` indicates HardCaml tried to select bits with `hi < lo`, specifically trying to do `select signal 0 0` on an 8-bit signal.

### The Problem: Variable.wire Width Behavior

```ocaml
let v = Variable.wire ~default:(zero 8) in
(* v.value initially has width 8 from the default *)
(* BUT the width tracking in HardCaml is complex *)
```

### What Was Happening

1. Output port "y" created as: `Variable.wire ~default:(zero 8)`
2. Try to assign to it: `v <-- rhs_sig`
3. Width matching code: 
   ```ocaml
   let wlhs = width v.value  (* This could return unexpected width *)
   let wrhs = width rhs_sig  (* This was 8 *)
   ```

## The Fix

### Before (Broken)
```ocaml
(* Create outputs as Variables in separate loop *)
| `Output ->
    Hashtbl.add decls name (Var (Variable.wire ~default:(zero width)))
```

### After (Fixed)
```ocaml
(* Create ALL variables (internal + outputs) together *)
let all_vars = signals @ (output_vars_from_ports) in
List.iter (fun (name, width, _) ->
  Hashtbl.add decls name (Var (Variable.wire ~default:(zero width)))
) all_vars;
```

## Why This Fixes It

1. **Variables created with their target width** from port/signal declarations
2. **All variables created before any Always blocks run**
3. **Width is established consistently**

## Additional Safety: Width Checks

Added explicit width validation and debugging.

## Testing the Fix

Run with your ALU module - should now work without width errors!
