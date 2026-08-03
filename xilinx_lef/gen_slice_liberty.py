#!/usr/bin/env python3
"""Liberty for the PACKED virtex7 LEF macros, so OpenROAD can run
`global_placement -timing_driven` on a place_lef DEF.

Why a NEW liberty and not the existing openflow/opentimer/arp_gold.lib: that one
describes PRIMITIVES (LUT6, FDRE, CARRY4...), but the DEF handed to OpenROAD is
at PACKED granularity (SLICE_LOGIC, SLICE_CARRY, SLICE_FF...).  OpenSTA matches
liberty cells to LEF macros by name, so the packed macros need their own model.

Delays are the measured Vivado arc values for the dominant primitive inside each
pack (ethsoc/extract_liberty_probe.tcl on a routed DCP):
    LUT6 I->O    0.043 (fast input) .. 0.123 (slow input)   -> 0.100 typical
    FDRE C->Q    0.223 slow / 0.100 fast
    RAMB36 setup 0.493, hold 0.083, CLK->DO ~0.5
These are FIXED per corner on 7-series -- there is no slew/load surface to model
(the load-dependent part is interconnect, which OpenROAD estimates via
set_wire_rc), so scalar templates are the faithful representation, not a
shortcut.

The absolute values matter less than their RATIO to interconnect delay: what
timing-driven placement needs is a sane relative criticality ordering.

usage: gen_slice_liberty.py virtex7_cells.lef out.lib
"""
import re, sys

LUT_DELAY   = 0.100   # LUT input -> LUT output
FF_CLK_Q    = 0.223   # FDRE C -> Q
FF_SETUP    = 0.100
FF_HOLD     = 0.020
RAM_CLK_Q   = 0.500
RAM_SETUP   = 0.493
RAM_HOLD    = 0.083
CARRY_DELAY = 0.050   # CI -> CO is the fast dedicated path
PIN_CAP     = 0.001
SLEW        = 0.050   # output transition; 7-series arcs are fixed, slew is nominal
CAP_SLOPE   = 0.02    # ns per pF: near-flat, only to make driveResistance finite   # output transition; 7-series arcs are fixed, slew is nominal

def parse_lef(path):
    """macro -> [(pin, direction)] in declaration order"""
    macros, cur, pin = {}, None, None
    for ln in open(path):
        s = ln.strip()
        m = re.match(r'^MACRO\s+(\S+)', s)
        if m:
            cur = m.group(1); macros[cur] = []; continue
        m = re.match(r'^PIN\s+(\S+)', s)
        if m and cur:
            pin = m.group(1); macros[cur].append([pin, "INPUT"]); continue
        m = re.match(r'^DIRECTION\s+(\S+)', s)
        if m and cur and macros[cur]:
            macros[cur][-1][1] = m.group(1).rstrip(';').strip()
    return macros

def is_clock(p):   return p == "CLK" or p.startswith("CLKA") or p.startswith("CLKB")
def is_ff_out(p):  return re.fullmatch(r'[A-D]Q', p) or re.fullmatch(r'Q\d+', p)

def emit(macro, pins, out):
    ins  = [p for p, d in pins if d == "INPUT"  and not is_clock(p)]
    outs = [p for p, d in pins if d == "OUTPUT"]
    clks = [p for p, d in pins if is_clock(p)]
    ram  = macro.startswith("RAMB")
    seq  = bool(clks)
    clk_q = RAM_CLK_Q if ram else FF_CLK_Q
    setup = RAM_SETUP if ram else FF_SETUP
    hold  = RAM_HOLD  if ram else FF_HOLD
    comb  = CARRY_DELAY if macro == "SLICE_CARRY" else LUT_DELAY

    out.write("  cell (%s) {\n" % macro)
    out.write("    area : 1;\n")
    for c in clks:
        out.write("    pin (%s) { direction : input; clock : true; capacitance : %s; }\n"
                  % (c, PIN_CAP))
    for p in ins:
        out.write("    pin (%s) {\n      direction : input; capacitance : %s;\n" % (p, PIN_CAP))
        if seq:
            # data pins are captured by the clock -- gives OpenSTA real endpoints
            for c in clks:
                for kind, val in (("setup_rising", setup), ("hold_rising", hold)):
                    out.write("      timing () { related_pin : \"%s\"; timing_type : %s;\n"
                              "        rise_constraint (cst) { values (\"%s\"); }\n"
                              "        fall_constraint (cst) { values (\"%s\"); } }\n"
                              % (c, kind, val, val))
        out.write("    }\n")
    for o in outs:
        out.write("    pin (%s) {\n      direction : output; max_capacitance : 10000;\n" % o)
        if seq and is_ff_out(o):
            for c in clks:
                out.write("      timing () { related_pin : \"%s\"; timing_type : rising_edge;\n"
                          "        cell_rise (load) { values (\"%s, %s\"); }\n"
                          "        cell_fall (load) { values (\"%s, %s\"); }\n"
                          "        rise_transition (load) { values (\"%s, %s\"); }\n"
                          "        fall_transition (load) { values (\"%s, %s\"); } }\n"
                          % (c, clk_q, clk_q+CAP_SLOPE, clk_q, clk_q+CAP_SLOPE, SLEW, SLEW+CAP_SLOPE, SLEW, SLEW+CAP_SLOPE))
        else:
            for p in ins:
                out.write("      timing () { related_pin : \"%s\"; timing_sense : non_unate;\n"
                          "        cell_rise (load) { values (\"%s, %s\"); }\n"
                          "        cell_fall (load) { values (\"%s, %s\"); }\n"
                          "        rise_transition (load) { values (\"%s, %s\"); }\n"
                          "        fall_transition (load) { values (\"%s, %s\"); } }\n"
                          % (p, comb, comb+CAP_SLOPE, comb, comb+CAP_SLOPE, SLEW, SLEW+CAP_SLOPE, SLEW, SLEW+CAP_SLOPE))
        out.write("    }\n")
    out.write("  }\n")

def main():
    lef, outp = sys.argv[1], sys.argv[2]
    macros = parse_lef(lef)
    with open(outp, "w") as o:
        o.write("library (virtex7_packed) {\n"
                "  delay_model : table_lookup;\n"
                "  time_unit : \"1ns\"; voltage_unit : \"1V\"; current_unit : \"1mA\";\n"
                "  capacitive_load_unit (1,pf); pulling_resistance_unit : \"1kohm\";\n"
                # NO artificial electrical limits.  These are what make the resizer
                # insert buffers: it repairs max_transition/max_capacitance
                # violations, and with set_wire_rc over long nets a 1.0 ns slew
                # cap is violated constantly -- but that limit is an artifact of
                # this model, not a device constraint.  A 7-series routing
                # architecture has no equivalent, so the limits are set out of
                # reach and the resizer has nothing to "fix".
                "  default_max_transition : 1000.0;\n"
                "  default_max_fanout : 100000;\n"
                # A SINGLE-POINT load axis makes drive resistance (d delay /
                # d cap) a 0/0 -- OpenSTA computes it in
                # GateTableModel::driveResistance while sorting cells for
                # EquivCells, so timing-driven placement dies on FE_INVALID long
                # before the NaN surfaces as "parasitic Pi model has NaNs".
                # Two points keep the delay essentially fixed (correct for
                # 7-series, whose arcs do not vary with load) while giving a
                # finite, well-defined slope.
                "  lu_table_template (load) { variable_1 : total_output_net_capacitance;\n"
                "    index_1 (\"0.0010, 0.0100\"); }\n"
                "  lu_table_template (cst) { variable_1 : constrained_pin_transition;\n"
                "    variable_2 : related_pin_transition; index_1 (\"0.0\"); index_2 (\"0.0\"); }\n"
                "  input_threshold_pct_rise : 50; input_threshold_pct_fall : 50;\n"
                "  output_threshold_pct_rise : 50; output_threshold_pct_fall : 50;\n"
                "  slew_lower_threshold_pct_rise : 20; slew_upper_threshold_pct_rise : 80;\n"
                "  slew_lower_threshold_pct_fall : 20; slew_upper_threshold_pct_fall : 80;\n")
        # OpenROAD's resizer refuses to initialise without a buffer cell
        # ("[ERROR RSZ-0022] no buffers found"), and timing-driven global
        # placement brings the resizer up for parasitic estimation.  This cell
        # exists only to satisfy that: gpl does not insert buffers (only
        # repair_design does, which we never call), so it never reaches the DEF
        # and cannot confuse the import back into place_lef.
        o.write("  cell (LOGIC_BUF) {\n"
                "    area : 1;\n"
                "    pin (A) { direction : input; capacitance : %s; }\n"
                "    pin (Z) {\n"
                "      direction : output; max_capacitance : 10000;\n"
                "      function : \"A\";\n"
                "      timing () { related_pin : \"A\"; timing_sense : positive_unate;\n"
                "        cell_rise (load) { values (\"%s, %s\"); }\n"
                "        cell_fall (load) { values (\"%s, %s\"); }\n"
                "        rise_transition (load) { values (\"%s, %s\"); }\n"
                "        fall_transition (load) { values (\"%s, %s\"); } }\n"
                "    }\n  }\n" % (PIN_CAP, LUT_DELAY, LUT_DELAY+CAP_SLOPE, LUT_DELAY, LUT_DELAY+CAP_SLOPE, SLEW, SLEW+CAP_SLOPE, SLEW, SLEW+CAP_SLOPE))
        for m in sorted(macros):
            emit(m, macros[m], o)
        o.write("}\n")
    n_seq = sum(1 for m, p in macros.items() if any(is_clock(x) for x, _ in p))
    print("wrote %s: %d cells (%d sequential)" % (outp, len(macros), n_seq))

main()
