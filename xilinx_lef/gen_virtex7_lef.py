#!/usr/bin/env python3
"""Generate a Virtex-7 physical LEF library for topographical (ASIC-style)
placement of Xilinx designs, to be legalised afterwards by nextpnr-xilinx.

The FPGA fabric is modelled as an ASIC floorplan:

  * The placement unit is one SLICE.  Coordinates are in "SLICE units":
    1.0 um in X == one SLICE column, 1.0 um in Y == one CLB row.  HPWL and
    Elmore wire delay then read directly as SLICE-hop Manhattan distance,
    which is the quantity the router actually pays for on the Xilinx INT
    fabric.  (Real INT is H metal1 + V metal2, so we give the tech LEF one
    horizontal and one vertical routing layer -- enough for a wirelength /
    congestion model, not a sign-off RC deck.)

  * Each MACRO is a *recognised* tile configuration -- a whole SLICE (or
    BRAM/DSP/IO/clock site) pre-packed for one role.  This is the "higher
    level recognition" a soup-of-cells placer lacks: a CARRY cell exposes
    CI (bottom) / CO (top) pins so the placer chains it vertically like
    Vivado does; a LOGIC cell clusters its LUT I/O; a BRAM/DSP is a 5-row
    macro pinned to its column.  The packer maps BIR primitives onto these
    configs; the placer moves them route-length-aware; nextpnr unpacks to
    real BELs + routes.

  * A config is a SUPERSET of pins for its role, so one MACRO covers a
    family of real packings (e.g. SLICE_LOGIC covers 1..4 LUT6 + 1..4 FF).
    The packer only wires the pins it uses; unused pins float (legal in LEF).

Emits:  virtex7.tech.lef   (units, grid, layers, sites)
        virtex7_cells.lef  (the MACRO cell library)
"""
import sys

# ---- physical model (SLICE units; 1.0 um == 1 SLICE pitch) -----------------
DB = 2000                     # DATABASE MICRONS
SLICE_W, SLICE_H = 1.0, 1.0   # one SLICE
BRAM_H = 5.0                  # RAMB36 spans 5 CLB rows
DSP_H  = 5.0                  # DSP48 spans 5 CLB rows
IO_H   = 1.0

# ---- cell library: name -> (site, w, h, [(pin, dir), ...]) -----------------
# dir: 'I' input, 'O' output, 'B' inout (clocks/power treated as I).
def lut_ins(letters):        # A1..A6 for each LUT letter
    return [("%s%d" % (L, i), "I") for L in letters for i in range(1, 7)]

CELLS = {}

# --- SLICE configurations (site SLICE, 1x1) ---------------------------------
# LOGIC: up to 4 (LUT6 + FF).  Superset pins for lanes A..D.
CELLS["SLICE_LOGIC"] = ("SLICE", SLICE_W, SLICE_H,
    lut_ins("ABCD")
    + [("%sO" % L, "O") for L in "ABCD"]          # LUT outputs
    + [("CLK", "I"), ("CE", "I"), ("SR", "I")]
    + [("%sQ" % L, "O") for L in "ABCD"])         # FF outputs

# CARRY: CARRY4 + up to 4 S-LUTs + 4 sum FFs.  CI/CO drive vertical chaining.
CELLS["SLICE_CARRY"] = ("SLICE", SLICE_W, SLICE_H,
    [("CI", "I"), ("CO", "O")]                    # carry chain (bottom in, top out)
    + [("CYINIT", "I")]
    + [("S%d" % i, "I") for i in range(4)]        # propagate selects
    + [("DI%d" % i, "I") for i in range(4)]       # generate data-in
    + [("O%d" % i, "O") for i in range(4)]        # sum outputs
    + [("CLK", "I"), ("CE", "I"), ("SR", "I")]
    + [("Q%d" % i, "O") for i in range(4)])       # sum FF outputs

# MUX: wide MUXF7/F8 built from the slice LUTs.
CELLS["SLICE_MUX"] = ("SLICE", SLICE_W, SLICE_H,
    lut_ins("ABCD")
    + [("F7A", "O"), ("F7B", "O"), ("F8", "O"),
       ("SEL7A", "I"), ("SEL7B", "I"), ("SEL8", "I")])

# FF-only register bank (retiming / pipeline stage): 8 FFs.
CELLS["SLICE_FF"] = ("SLICE", SLICE_W, SLICE_H,
    [("D%d" % i, "I") for i in range(8)]
    + [("Q%d" % i, "O") for i in range(8)]
    + [("CLK", "I"), ("CE", "I"), ("SR", "I")])

# SLICEM-only: SRL shift register (Q31 cascade out for chaining).
CELLS["SLICEM_SRL"] = ("SLICE", SLICE_W, SLICE_H,
    [("D", "I"), ("CLK", "I"), ("CE", "I")]
    + [("A%d" % i, "I") for i in range(5)]
    + [("Q", "O"), ("Q31", "O"), ("MC31", "O")])

# SLICEM-only: distributed RAM (RAM32M/RAM64M).
CELLS["SLICEM_DRAM"] = ("SLICE", SLICE_W, SLICE_H,
    [("WCLK", "I"), ("WE", "I")]
    + [("A%d" % i, "I") for i in range(6)]
    + [("DI%d" % i, "I") for i in range(4)]
    + [("DO%d" % i, "O") for i in range(4)])

# --- hard blocks ------------------------------------------------------------
# BRAM (RAMB36): wide address/data; 5-row macro.  Superset A/B ports.
CELLS["RAMB36"] = ("BRAM", 1.0, BRAM_H,
    [("CLKA", "I"), ("CLKB", "I"), ("ENA", "I"), ("ENB", "I"),
     ("WEA", "I"), ("WEB", "I"), ("REGCEA", "I"), ("REGCEB", "I"),
     ("RSTA", "I"), ("RSTB", "I")]
    + [("ADDRA%d" % i, "I") for i in range(16)]
    + [("ADDRB%d" % i, "I") for i in range(16)]
    + [("DIA%d" % i, "I") for i in range(32)]
    + [("DIB%d" % i, "I") for i in range(32)]
    + [("DOA%d" % i, "O") for i in range(32)]
    + [("DOB%d" % i, "O") for i in range(32)]
    + [("CASCADEINA", "I"), ("CASCADEINB", "I"),
       ("CASCADEOUTA", "O"), ("CASCADEOUTB", "O")])

# BRAM half (RAMB18) -- 2.5-row; round to 3 for the grid.
CELLS["RAMB18"] = ("BRAM", 1.0, 3.0,
    [("CLKA", "I"), ("CLKB", "I"), ("ENA", "I"), ("ENB", "I"),
     ("WEA", "I"), ("WEB", "I")]
    + [("ADDRA%d" % i, "I") for i in range(14)]
    + [("ADDRB%d" % i, "I") for i in range(14)]
    + [("DIA%d" % i, "I") for i in range(16)]
    + [("DIB%d" % i, "I") for i in range(16)]
    + [("DOA%d" % i, "O") for i in range(16)]
    + [("DOB%d" % i, "O") for i in range(16)])

# DSP48E1: 5-row macro with PCIN/PCOUT cascade for vertical chaining.
CELLS["DSP48"] = ("DSP", 1.0, DSP_H,
    [("CLK", "I"), ("CE", "I")]
    + [("A%d" % i, "I") for i in range(30)]
    + [("B%d" % i, "I") for i in range(18)]
    + [("C%d" % i, "I") for i in range(48)]
    + [("P%d" % i, "O") for i in range(48)]
    + [("OPMODE%d" % i, "I") for i in range(7)]
    + [("PCIN", "I"), ("PCOUT", "O")])            # DSP cascade chain

# IOB: bidirectional pad site (in/out/tri).
CELLS["IOB"] = ("IO", 1.0, IO_H,
    [("I", "I"), ("T", "I"), ("O", "O"), ("PAD", "B")])

# Clock resources (placed on their dedicated columns; fixed by legaliser).
CELLS["BUFG"] = ("CLK", 1.0, 1.0, [("I", "I"), ("O", "O")])
CELLS["BUFH"] = ("CLK", 1.0, 1.0, [("I", "I"), ("O", "O")])
CELLS["MMCM"] = ("CLK", 1.0, 4.0,
    [("CLKIN1", "I"), ("CLKIN2", "I"), ("CLKFBIN", "I"), ("RST", "I")]
    + [("CLKOUT%d" % i, "O") for i in range(7)]
    + [("CLKFBOUT", "O"), ("LOCKED", "O")])

DIRW = {"I": "INPUT", "O": "OUTPUT", "B": "INOUT"}


def emit_tech(f):
    f.write("VERSION 5.8 ;\n")
    f.write('BUSBITCHARS "[]" ;\n')
    f.write('DIVIDERCHAR "/" ;\n\n')
    f.write("UNITS\n  DATABASE MICRONS %d ;\nEND UNITS\n\n" % DB)
    f.write("MANUFACTURINGGRID 0.0010 ;\n\n")
    # One H + one V routing layer -- models the INT fabric's H/V tracks.
    f.write("LAYER metal1\n  TYPE ROUTING ;\n  DIRECTION HORIZONTAL ;\n"
            "  WIDTH 0.10 ;\n  SPACING 0.10 ;\n  PITCH 0.20 ;\n"
            "  RESISTANCE RPERSQ 0.10 ;\n  CAPACITANCE CPERSQDIST 1.0e-04 ;\nEND metal1\n\n")
    f.write("LAYER via1\n  TYPE CUT ;\nEND via1\n\n")
    f.write("LAYER metal2\n  TYPE ROUTING ;\n  DIRECTION VERTICAL ;\n"
            "  WIDTH 0.10 ;\n  SPACING 0.10 ;\n  PITCH 0.20 ;\n"
            "  RESISTANCE RPERSQ 0.10 ;\n  CAPACITANCE CPERSQDIST 1.0e-04 ;\nEND metal2\n\n")
    # Sites -- the placement rows.  SLICE is the core row; BRAM/DSP/IO/CLK are
    # taller/dedicated sites the floorplan pins to their columns.
    for site, w, h, cls in [("SLICE", SLICE_W, SLICE_H, "CORE"),
                            ("BRAM", 1.0, 1.0, "CORE"),
                            ("DSP", 1.0, 1.0, "CORE"),
                            ("IO", 1.0, IO_H, "PAD"),
                            ("CLK", 1.0, 1.0, "CORE")]:
        f.write("SITE %s\n  CLASS %s ;\n  SYMMETRY Y ;\n  SIZE %.3f BY %.3f ;\nEND %s\n\n"
                % (site, cls, w, h, site))


def emit_macro(f, name, site, w, h, pins):
    f.write("MACRO %s\n  CLASS CORE ;\n  ORIGIN 0 0 ;\n  SYMMETRY X Y ;\n"
            "  SITE %s ;\n  SIZE %.3f BY %.3f ;\n" % (name, site, w, h))
    # Lay pins out along the cell edges as small rects so tools that need
    # geometry get a legal port; the model only relies on name + direction.
    n = len(pins)
    for idx, (pin, d) in enumerate(pins):
        # distribute pins up the left (inputs) / right (outputs) edges
        edge_x = 0.05 if d != "O" else (w - 0.15)
        y = 0.05 + (h - 0.1) * (idx / max(1, n))
        f.write("  PIN %s\n    DIRECTION %s ;\n" % (pin, DIRW[d]))
        if d == "B":
            f.write("    USE SIGNAL ;\n")
        f.write("    PORT\n      LAYER metal1 ;\n        RECT %.3f %.3f %.3f %.3f ;\n"
                "    END\n  END %s\n" % (edge_x, y, edge_x + 0.10, y + 0.06, pin))
    f.write("END %s\n\n" % name)


def main():
    outdir = sys.argv[1] if len(sys.argv) > 1 else "."
    with open(outdir + "/virtex7.tech.lef", "w") as f:
        emit_tech(f)
    with open(outdir + "/virtex7_cells.lef", "w") as f:
        f.write("VERSION 5.8 ;\n\n")
        for name in sorted(CELLS):
            site, w, h, pins = CELLS[name]
            emit_macro(f, name, site, w, h, pins)
    npins = sum(len(v[3]) for v in CELLS.values())
    print("wrote virtex7.tech.lef + virtex7_cells.lef: %d MACROs, %d pins"
          % (len(CELLS), npins))
    for name in sorted(CELLS):
        s, w, h, p = CELLS[name]
        print("  %-14s site=%-6s %.0fx%.0f  %d pins" % (name, s, w, h, len(p)))


if __name__ == "__main__":
    main()
