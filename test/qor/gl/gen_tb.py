# gen_tb.py ports.txt top cycles > tb.sv
# A testbench driving the source module <top> and the netlist wrapper <top>_gl
# with the same random inputs every clock, comparing every output at negedge.
import sys
ports = [l.split() for l in open(sys.argv[1]) if l.strip()]
top, cycles = sys.argv[2], int(sys.argv[3])
ins = [(n, int(w)) for d, n, w in ports if d == 'input']
outs = [(n, int(w)) for d, n, w in ports if d == 'output']
# clocks.txt (svd.clocks): "clock <name> pos|neg" / "reset <name> pos|neg async|sync"
clocks, resets = [], {}
if len(sys.argv) > 4:
    try:
        for l in open(sys.argv[4]):
            f = l.split()
            if f and f[0] == 'clock': clocks.append(f[1])
            if f and f[0] == 'reset': resets[f[1]] = f[2]
    except OSError:
        pass
innames = [n for n, _ in ins]
clk = next((c for c in clocks if c in innames), None)
if clk is None:  # no clocked process named one: fall back to the usual names
    clk = next((n for n, _ in ins if n in ('clk', 'clk_i', 'clock')), None)
rst = [n for n, _ in ins if n in resets or n in ('reset', 'rst', 'rst_n', 'rst_ni', 'resetn', 'rst_i')]
def rng(w): return '' if w == 1 else '[%d:0] ' % (w - 1)
L = ['`timescale 1ns/1ps', 'module tb;', '  glbl glbl();']
for n, w in ins: L.append('  logic %s%s = 0;' % (rng(w), n))
for n, w in outs: L.append('  wire %s%s_rtl, %s_gl;' % (rng(w), n, n))
L.append('  %s rtl(%s);' % (top, ', '.join(['.%s(%s)' % (n, n) for n, _ in ins] + ['.%s(%s_rtl)' % (n, n) for n, _ in outs])))
L.append('  %s_gl gl(%s);' % (top, ', '.join(['.%s(%s)' % (n, n) for n, _ in ins] + ['.%s(%s_gl)' % (n, n) for n, _ in outs])))
L.append('  int cyc = 0, errs = 0;')
L.append('  initial if ($test$plusargs("vcd")) begin $dumpfile("dump.vcd"); $dumpvars(0, tb); end')
# glbl holds GSR for the first 100 ns: start the clock after it, so both sides leave their initial state together
if clk is None:
    # purely combinational module: a testbench-only clock paces the stimulus
    clk = 'tb_clk'; L.append('  logic tb_clk;')
L.append('  initial begin %s = 0; #120; forever #5 %s = ~%s; end' % (clk, clk, clk))
L.append('  always @(posedge %s) begin' % clk)
for n, w in ins:
    if n == clk: continue
    if n in rst:
        if n in resets: act = "1'b0" if resets[n] == 'neg' else "1'b1"
        else: act = "1'b0" if n.endswith('_n') or n.endswith('n') and n != 'reset' and n != 'rst' else "1'b1"
        ina = "1'b1" if act == "1'b0" else "1'b0"
        L.append("    %s <= (cyc < 4) ? %s : (($urandom %% 101) == 0 ? %s : %s);" % (n, act, act, ina))
    else:
        for lo in range(0, w, 32):
            hi = min(w, lo + 32) - 1
            L.append('    %s[%d:%d] <= $urandom;' % (n, hi, lo) if w > 1 else '    %s <= $urandom;' % n)
L.append('    cyc <= cyc + 1;')
L.append('  end')
# the previous cycle's outputs, printed with the first mismatch: the state the
# diverging transition started from
for n, w in outs: L.append('  logic %s%s_p1, %s_p2;' % (rng(w), n, n))
L.append('  always @(posedge %s) begin' % clk)
for n, w in outs: L.append('    %s_p2 <= %s_p1; %s_p1 <= %s_rtl;' % (n, n, n, n))
L.append('  end')
ctx = ' '.join('%s=%%h' % n for n, _ in outs)
vals = lambda sfx: ', '.join('%s%s' % (n, sfx) for n, _ in outs)
L.append('  task show_context; begin')
L.append('    $display("CONTEXT cyc-2: %s", %s);' % (ctx, vals('_p2')))
L.append('    $display("CONTEXT cyc-1: %s", %s);' % (ctx, vals('_p1')))
L.append('    $display("CONTEXT rtl  : %s", %s);' % (ctx, vals('_rtl')))
L.append('    $display("CONTEXT gl   : %s", %s);' % (ctx, vals('_gl')))
L.append('  end endtask')
L.append('  always @(negedge %s) if (cyc > 1) begin' % clk)
L.append('    if (errs == 0 && (%s)) show_context;' % ' || '.join('%s_rtl !== %s_gl' % (n, n) for n, _ in outs))
for n, w in outs:
    L.append('    if (%s_rtl !== %s_gl) begin errs++; if (errs <= 8) $display("MISMATCH cyc %%0d %s rtl=%%h gl=%%h", cyc, %s_rtl, %s_gl); end' % (n, n, n, n, n))
L.append('    if (cyc >= %d) begin $display("GL-COSIM: %%s (%%0d cycles, %%0d mismatches)", errs == 0 ? "MATCH" : "DIFFER", cyc, errs); $finish; end' % cycles)
L.append('  end')
L.append('endmodule')
print('\n'.join(L))
