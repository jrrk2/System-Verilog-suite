# HardCaml select() API Issue - Fixed

## The Error

```
Downsizing RHS from 16 to 8
Warning: Circuit build failed for alu: ("[select] got [hi < lo]" (hi 0) (lo 8))
```

## Root Cause: Wrong select() Usage

### HardCaml's select() Signature

```ocaml
val select : Signal.t -> int -> int -> Signal.t
(*           signal    low   high *)
```

**Key**: The parameters are `low` and `high` bit positions, NOT `lsb` and `width`!

### What We Were Doing (WRONG)

```ocaml
(* Trying to select 8 bits from bit 0 *)
select rhs_sig 0 8   (* WRONG! This means bits[8:0] which is backwards! *)
```

**Error interpretation**:
- HardCaml saw: `select signal 0 8`
- Interpreted as: "select bits from 0 (lo) to 8 (hi)"
- But in HardCaml, this means bits[0:8] which is 9 bits starting at 0
- Wait no - the error says `(hi 0) (lo 8)` which means it swapped them!
- So HardCaml interpreted our call as: hi=0, lo=8
- This is invalid: hi < lo!

### The Fix: Use sel_bottom()

```ocaml
val sel_bottom : Signal.t -> int -> Signal.t
(*               signal    width *)
```

**Correct usage**:
```ocaml
(* Select 8 bits from the bottom (LSB) *)
sel_bottom rhs_sig 8   (* CORRECT! *)
```

## Code Changes

### Before (Broken)
```ocaml
else begin
  Printf.eprintf "Downsizing RHS from %d to %d\n" wrhs wlhs;
  select rhs_sig 0 wlhs  (* WRONG *)
end
```

### After (Fixed)
```ocaml
else begin
  Printf.eprintf "Downsizing RHS from %d to %d\n" wrhs wlhs;
  sel_bottom rhs_sig wlhs  (* CORRECT *)
end
```

## Where This Was Applied

1. **process_assign** - blocking assignments
2. **Non-blocking assignments** - Assign with is_blocking=false
3. **Sel node handling** - bit/part select in expressions

## Alternative: Correct select() Usage

If you want to use `select` instead of `sel_bottom`, you must:

```ocaml
(* To select bits [0:7] (8 bits) *)
select signal 0 7   (* NOT 0 8! *)
(*             ^   ^
               lo  hi = lo + width - 1 *)
```

For a general case:
```ocaml
(* Select 'width' bits starting at 'lsb' *)
let msb = lsb + width - 1 in
select signal lsb msb
```

## Testing

Your ALU should now work:

```
Processing Module: alu
  Assignment: lhs_width=8 rhs_width=16
  Downsizing RHS from 16 to 8 (select bits 0 to 7)
  ✅ Success!
```

## HardCaml Signal Selection API Summary

| Function | Signature | Description |
|----------|-----------|-------------|
| `sel_bottom` | `signal -> width -> signal` | Select width bits from LSB |
| `sel_top` | `signal -> width -> signal` | Select width bits from MSB |
| `select` | `signal -> low -> high -> signal` | Select bits [low:high] inclusive |
| `bit` | `signal -> int -> signal` | Select single bit |

**Recommendation**: Use `sel_bottom` and `sel_top` when you know the width. Use `select` only when you have explicit bit positions.
