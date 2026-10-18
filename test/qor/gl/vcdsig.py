# vcdsig.py dump.vcd <clk period ps> <from_cycle> <to_cycle> <regex of signal paths>...
# prints the selected signals' values at each clock cycle in the window (value sampled just before the posedge)
import sys, re
path, period, c0, c1 = sys.argv[1], int(sys.argv[2]), int(sys.argv[3]), int(sys.argv[4])
pats = [re.compile(p) for p in sys.argv[5:]]
ids = {}; scope = []
with open(path) as f:
    for line in f:
        line = line.lstrip()
        if line.startswith('$scope'): scope.append(line.split()[2])
        elif line.startswith('$upscope'): scope.pop()
        elif line.startswith('$var'):
            p = line.split(); code = p[3]; name = p[4]; full = '.'.join(scope + [name])
            if any(r.search(full) for r in pats): ids.setdefault(code, []).append((full, int(p[2])))
        elif line.startswith('$enddefinitions'): break
    vals = {c: 'x' for c in ids}; t = 0; nextc = c0; out = []
    def emit(cyc):
        row = []
        for c, lst in ids.items():
            for full, w in lst:
                v = vals[c]
                try: row.append('%s=%x' % (full.split('tb.')[-1], int(v, 2)))
                except ValueError: row.append('%s=%s' % (full.split('tb.')[-1], v))
        print('cyc %d: %s' % (cyc, ' '.join(sorted(row))))
    for line in f:
        if line.startswith('#'):
            t = int(line[1:])
            # the testbench clock starts at 120 ns; posedges at 120ns + 5ns + k*10ns
            while nextc <= c1 and t > 120000 + 5000 + nextc * period - 1:
                emit(nextc); nextc += 1
            if nextc > c1: break
        elif line[0] in 'b':
            v, c = line[1:].split(); 
            if c in ids: vals[c] = v
        elif line[0] in '01xz':
            c = line[1:].strip()
            if c in ids: vals[c] = line[0]
