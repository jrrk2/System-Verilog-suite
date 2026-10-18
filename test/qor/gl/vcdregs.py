# vcdregs.py dump.vcd <period ps> <from> <to> <reg>...   (netlist side: tb.gl.u.<reg>__b<i>_<n>.Q)
# Reassembles bit-blasted registers from the FDRE instance names and prints them per cycle,
# plus any RAMB36E1 instance pins named with --pins inst.PIN
import sys, re, os
path, period, c0, c1 = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4])
regs = [a for a in sys.argv[5:] if not a.startswith('--')]
pins = [a[6:] for a in sys.argv[5:] if a.startswith('--pin=')]
want = {}   # code -> list of (regname, bit)
pinw = {}   # code -> pinname
scope = []
with open(path) as f:
    for line in f:
        l = line.lstrip()
        if l.startswith('$scope'): scope.append(l.split()[2])
        elif l.startswith('$upscope'): scope.pop()
        elif l.startswith('$var'):
            p = l.split(); code = p[3]; name = p[4]
            # VCDREGS_SCOPE: the dotted path of the netlist module (default tb.gl.u)
            import os
            pre = os.environ.get('VCDREGS_SCOPE', 'tb.gl.u').split('.')
            if len(scope) >= len(pre) + 1 and scope[:len(pre)] == pre and name == 'Q':
                m = re.match(r'^(.*)__b(\d+)_\d+$', scope[len(pre)]) or re.match(r'^(.*)_\d+$', scope[len(pre)])
                if m:
                    base = m.group(1); bit = int(m.group(2)) if m.lastindex == 2 else 0
                    if base in regs: want.setdefault(code, []).append((base, bit))
            full = '.'.join(scope[len(pre):] + [name])
            if full in pins: pinw[code] = full
        elif l.startswith('$enddefinitions'): break
    vals = {}
    def emit(cyc):
        out = {}
        for c, lst in want.items():
            v = vals.get(c, 'x')
            for base, bit in lst:
                out.setdefault(base, {})[bit] = v
        row = []
        for base in regs:
            bits = out.get(base, {})
            if not bits: continue
            s = ''.join(bits.get(i, 'x') for i in range(max(bits) , -1, -1))
            try: row.append('%s=%x' % (base, int(s, 2)))
            except ValueError: row.append('%s=%s' % (base, s))
        for c, n in pinw.items():
            v = vals.get(c, 'x')
            try: row.append('%s=%x' % (n, int(v, 2)))
            except ValueError: row.append('%s=%s' % (n, v))
        print('cyc %d: %s' % (cyc, ' '.join(row)))
    nextc = c0
    for line in f:
        if line.startswith('#'):
            t = int(line[1:])
            off = int(os.environ.get('VCDREGS_T0', '125000'))
            while nextc <= c1 and t > off + nextc * period - 1:
                emit(nextc); nextc += 1
            if nextc > c1: break
        elif line[0] == 'b':
            v, c = line[1:].split()
            if c in want or c in pinw: vals[c] = v
        elif line[0] in '01xz':
            c = line[1:].strip()
            if c in want or c in pinw: vals[c] = line[0]
