# Hashtable Isolation - Implementation Complete

## Changes Made

### 1. Made vhdlhash a Mutable Reference

**File:** `vhd_libs/vabstraction.ml`
```ocaml
(* Before *)
let (vhdlhash:(vhdintf*string, vhdintf) Hashtbl.t) = Hashtbl.create 256;;

(* After *)
let vhdlhash = ref (Hashtbl.create 256 : (vhdintf*string, vhdintf) Hashtbl.t);;
```

### 2. Updated All References to Dereference

**Files Updated:**
- `vhd_libs/vabstraction.ml` line 190: `!vhdlhash`
- `vhd_libs/VhdlMain.ml` line 330: `!Vabstraction.vhdlhash`
- `vhdl_to_ir.ml` lines 637, 745: `!Vhd_front.Vabstraction.vhdlhash`
- `vhdl_to_ir_iterate.ml` lines 637, 745: `!Vhd_front.Vabstraction.vhdlhash`

### 3. Implemented Swap-In/Swap-Out Pattern

**Both `vhdl_to_ir.ml` and `vhdl_to_ir_iterate.ml`:**
```ocaml
let convert_vhdl_file_to_ir filename =
  (* Create fresh hashtable for isolation *)
  let fresh_hash = Hashtbl.create 256 in
  let old_hash = !Vhd_front.Vabstraction.vhdlhash in
  Vhd_front.Vabstraction.vhdlhash := fresh_hash;

  try
    (* Parse with fresh hashtable *)
    Vhd_front.VhdlMain.main succ [filename];

    (* Extract from fresh_hash *)
    Hashtbl.iter ... fresh_hash;

    (* Restore old hashtable *)
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    Some ir
  with e ->
    (* Restore on error too *)
    Vhd_front.Vabstraction.vhdlhash := old_hash;
    None
```

## Result

✅ **Build Successful** - All code compiles

## Remaining Issue: Settings Global State

The VHDL parser also has a global `settings` ref that accumulates `fileparsedlist` across multiple parses. When line 330 of VhdlMain.ml executes:

```ocaml
List.iter (fun r -> List.iter (fun itm ->
  Hashtbl.add !Vabstraction.vhdlhash (dump_design_unit itm,"") VhdNone
) r.parsedfileunits) !settings.fileparsedlist
```

It iterates over ALL files in settings.fileparsedlist, not just the current one. Even with our fresh hashtable, it's being populated with accumulated files from all previous parses.

### Evidence

When parsing `uart_baudgen.vhd` (which has 2 units: entity + architecture):
```
parsing file sysver_tests/uart_baudgen.vhd: (success) 2 units were parsed

9 item(s) were parsed with success.  ← Should be 2, not 9!

Architecture: rtl of slib_edge_detect  ← Wrong module!
```

The 9 items are accumulated from all previous test runs in the same process.

### Solution Options

**Option A: Reset fileparsedlist** (simple)
Before calling VhdlMain.main, clear the settings filelists. This requires accessing VhdlMain internals.

**Option B: Track and filter** (cleaner)
Count fileparsedlist length before parse, only process new entries after parse.

**Option C: Process isolation** (foolproof)
Run each conversion in a fresh subprocess to guarantee clean state.

## Summary

✅ Hashtable isolation: **DONE**
⏸️ Settings isolation: **NEEDED**

The approach of converting global mutable state to parameters (or refs that can be swapped) is the correct solution. We just need to apply it to one more global: the settings record.
