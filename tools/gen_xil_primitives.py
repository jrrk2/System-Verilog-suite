#!/usr/bin/env python3
"""One-shot converter from Vivado's unisim_v.v files to a static OCaml
table of Xilinx primitive port metadata.

Reads every `module <NAME>` declaration in
/opt/Xilinx/Vivado/2020.1/data/verilog/src/unisims/ and emits
xil_primitives.ml — a Behavioral_ir.bprogram-friendly description that
gives every downstream consumer (Verible_to_behavioral,
Ver_front_to_behavioral, Bir_to_nextpnr_json, …) consistent port-
direction and bus-width metadata for any user-instantiated primitive.

Run:
    tools/gen_xil_primitives.py > xil_primitives.ml

Idempotent — the generated file is deterministic given the same Vivado
install.  Re-run when the Vivado version changes.

Per the user direction (2026-05-29 PM): one-time conversion of the
primitive library so it lives as OCaml frames in advance rather than
each emitter rolling its own fallback table.
"""

import os, re, sys

UNISIMS = "/opt/Xilinx/Vivado/2020.1/data/verilog/src/unisims"

# Subset filter: ignore very-large or AI-engine-only / vendor-specific
# primitives we don't expect to encounter in 7-series user RTL.  None
# of these prefixes match Xilinx 7-series cells like LUT*/FDRE/CARRY4/
# IBUFDS/BUFG/MMCM/DSP/RAM*.  Keep the filter narrow so we don't
# accidentally drop something a user designs around.
SKIP_PREFIXES = ("AIE_", "BUFG_GT", "OBUF_DUAL_BUF",)

def parse_module(path, name):
    """Returns list of (port_name, direction, width) or None.

    Handles both port-declaration styles:
      Verilog-2001 (CARRY4.v): directions inline in port list
      Verilog-1995 (IBUFDS_GTE2.v): bare names in port list, then
          separate `input X;` / `output Y;` statements outside.

    Width 1 means scalar; >1 means a bus."""
    src = open(path).read()
    # Strip comments file-wide
    src_nc = re.sub(r"//[^\n]*", "", src)
    src_nc = re.sub(r"/\*.*?\*/", "", src_nc, flags=re.DOTALL)

    m = re.search(r"module\s+" + re.escape(name) + r"\b[^;]*?\((.*?)\)\s*;",
                  src_nc, re.DOTALL)
    if not m:
        return None
    ports_text = m.group(1)

    # Style 1: ANSI inline directions in the port list.
    inline = re.findall(
        r"(input|output|inout)\s*(?:\[\s*(\d+)\s*:\s*(\d+)\s*\])?"
        r"\s*([A-Za-z_][A-Za-z0-9_]*)",
        ports_text)
    if inline:
        ports = []
        for direction, msb, lsb, port_name in inline:
            width = int(msb) - int(lsb) + 1 if msb and lsb else 1
            ports.append((port_name, direction, width))
        return ports

    # Style 2: bare port names in header, separate decl statements.
    # First gather the port-name order from the header.
    port_order = re.findall(r"([A-Za-z_][A-Za-z0-9_]*)\s*(?:,|$|\))",
                            ports_text)
    # Then scan the file body for `input/output/inout [range] X[, Y, …];`
    decl_map = {}
    # Take everything between the `(...);` and `endmodule` once
    body_start = m.end()
    body_end = src_nc.find("endmodule", body_start)
    body = src_nc[body_start:body_end] if body_end != -1 else \
           src_nc[body_start:]
    for line_m in re.finditer(
            r"^\s*(input|output|inout)\s*(?:\[\s*(\d+)\s*:\s*(\d+)\s*\])?"
            r"\s*([^;]+);",
            body, re.MULTILINE):
        direction, msb, lsb, names_blob = line_m.groups()
        width = int(msb) - int(lsb) + 1 if msb and lsb else 1
        for nm in re.findall(r"[A-Za-z_][A-Za-z0-9_]*", names_blob):
            decl_map[nm] = (direction, width)

    ports = []
    for nm in port_order:
        if nm in decl_map:
            d, w = decl_map[nm]
            ports.append((nm, d, w))
    return ports if ports else None

def main():
    if not os.path.isdir(UNISIMS):
        sys.stderr.write(f"unisims dir not found: {UNISIMS}\n")
        sys.exit(1)

    primitives = []
    skipped = 0
    for f in sorted(os.listdir(UNISIMS)):
        if not f.endswith(".v"):
            continue
        name = f[:-2]
        if any(name.startswith(p) for p in SKIP_PREFIXES):
            skipped += 1
            continue
        path = os.path.join(UNISIMS, f)
        ports = parse_module(path, name)
        if ports is None or not ports:
            skipped += 1
            continue
        primitives.append((name, ports))

    sys.stderr.write(
        f"parsed {len(primitives)} primitives; skipped {skipped}\n")

    print("(* Auto-generated from Vivado's unisim_v.v files by")
    print(" * tools/gen_xil_primitives.py.  Do not edit by hand —")
    print(" * re-run the generator and commit the result. *)")
    print()
    print("(* Per-port metadata: name, direction, width-in-bits.")
    print(" * Width 1 = scalar; >1 = vector [width-1:0]. *)")
    print("type port_dir = [`Input | `Output | `Inout]")
    print()
    print("type port_meta = {")
    print("  name      : string;")
    print("  direction : port_dir;")
    print("  width     : int;")
    print("}")
    print()
    print(f"(* {len(primitives)} primitives covering every user-")
    print(" * instantiable Xilinx 7-series cell. *)")
    print("let primitives : (string * port_meta list) list = [")
    for name, ports in primitives:
        print(f"  \"{name}\", [")
        for pn, pd, pw in ports:
            d = {"input": "`Input", "output": "`Output", "inout": "`Inout"}[pd]
            print(f"    {{ name = \"{pn}\"; direction = {d}; width = {pw} }};")
        print("  ];")
    print("]")
    print()
    print("(* O(1) lookup table built lazily at first use. *)")
    print("let port_table : (string, port_meta list) Hashtbl.t Lazy.t =")
    print("  lazy (let t = Hashtbl.create 512 in")
    print("        List.iter (fun (n, ps) -> Hashtbl.replace t n ps) primitives;")
    print("        t)")
    print()
    print("(* Look up a primitive by cell-type name; None when unknown. *)")
    print("let find name : port_meta list option =")
    print("  Hashtbl.find_opt (Lazy.force port_table) name")

if __name__ == "__main__":
    main()
