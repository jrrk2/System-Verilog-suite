# Rename the bit-blasted netlist module to <top>_net and emit a <top> wrapper
# with vector ports, so the RTL testbench binds to it unchanged.
import re, sys
src, out_net, out_wrap, top = sys.argv[1:5]
wrapname = sys.argv[5] if len(sys.argv) > 5 else top
glbl = '--glbl' in sys.argv
# --ports <file>: the module's real ports ("dir name width" lines); every one is
# declared at that width, whether or not the netlist kept it
plist = None
if '--ports' in sys.argv:
    plist = [l.split() for l in open(sys.argv[sys.argv.index('--ports') + 1]) if l.strip()]
txt = open(src).read()
ports = {}  # name -> (dir, maxbit or None)
bits = {}   # name -> set of bit indices the netlist has (an unread input bit has no port)
for d, n in re.findall(r'^\s*(input|output)\s+(\w+)\s*;', txt, re.M):
    m = re.match(r'(.*)__(\d+)$', n)
    if m:
        b, i = m.group(1), int(m.group(2))
        ports[b] = (d, max(i, ports.get(b, (d, -1))[1] or 0))
        bits.setdefault(b, set()).add(i)
    else:
        ports[n] = (d, None)
assert not re.search(r'^\s*(input|output)\s*\[', txt, re.M), "vector port in netlist"
txt = re.sub(r'^module\s+%s\b' % top, 'module %s_net' % top, txt, count=1, flags=re.M)
open(out_net, 'w').write(txt)
w = ['`timescale 1ps/1ps', 'module %s (' % wrapname]
decl, conns = [], []
if plist is not None:
    order = [(n, (d, None if int(wd) == 1 else int(wd) - 1)) for d, n, wd in plist]
else:
    order = sorted(ports.items())
for n, (d, mx) in order:
    rng = '' if mx is None else '[%d:0] ' % mx
    decl.append('  %s logic %s%s' % (d, rng, n))
    if n not in ports:
        continue                      # the netlist has no such port: left open
    nmx = ports[n][1]
    if nmx is None and mx is None:
        conns.append('.%s(%s)' % (n, n))
    elif nmx is None:
        conns.append('.%s(%s[0])' % (n, n))
    else:
        conns += ['.%s__%d(%s[%d])' % (n, i, n, i) for i in range(nmx + 1) if i in bits[n] and (mx is None or i <= mx)]
w.append(',\n'.join(decl) + ');')
if glbl: w.append('  glbl glbl();')
w.append('  %s_net u (%s);' % (top, ',\n    '.join(conns)))
w.append('endmodule')
open(out_wrap, 'w').write('\n'.join(w) + '\n')
for n, (d, mx) in sorted(ports.items()):
    print(d, n, 1 if mx is None else mx + 1)
