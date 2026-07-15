#!/usr/bin/env python3
"""Lift enable/sync-reset feedback muxes from the D-LUT onto the FDRE CE/R pins.

A naive lowering realises `if (en) q<=d` as D = mux(en, d, Q) baked into the
D-LUT (Q fed back), with CE tied high.  This wastes LUT inputs, routes the
q-feedback in fabric, and turns the shared enable net into per-FF D-LUT fanout
(the control-net routing pressure we hit).  Detect the pattern in the LUT INIT
and rewrite: FDRE.CE = en, D = the residual function; likewise a const-0
cofactor -> synchronous R.  Bounded truth-table analysis (LUT<=6).
Usage: ce_lift.py <in.json> <out.json>
"""
import json, sys

def lut_inputs(c):
    order=[]
    for k in ("I0","I1","I2","I3","I4","I5"):
        v=c["connections"].get(k)
        if v: order.append(v[0])
    return order

def init_bits(c, n):
    raw=c.get("parameters",{}).get("INIT","0")
    # yosys INIT is an unsigned bit-string, MSB-first, width 2^n; or a plain int-string
    s=raw
    val=int(s,2) if set(s)<=set("01") and len(s)>1 else int(s,0) if s.isdigit() or s.startswith("0x") else int(s,2)
    return [(val>>i)&1 for i in range(1<<n)]   # bits[i] = output for input-combo i (I0=LSB)

def evaluate(bits, n):
    return bits  # bits[assignment] with I_k = bit k of assignment

def try_lift(bits, n, qi):
    """Return (en_pos, resid_bits, resid_ins, is_reset0) if f == mux over some
    input between (pass Q) and (a function independent of Q), else None."""
    for en in range(n):
        if en==qi: continue
        # partition assignments by en bit
        ok_en0=True; ok_en1_indepQ=True
        for a in range(1<<n):
            qbit=(a>>qi)&1; ebit=(a>>en)&1
            if ebit==0:
                # en=0 -> output must equal Q
                if bits[a]!=qbit: ok_en0=False; break
        if not ok_en0: continue
        # en=1 -> output independent of Q
        for a in range(1<<n):
            if (a>>en)&1 != 1: continue
            a_flipQ=a ^ (1<<qi)
            if bits[a]!=bits[a_flipQ]: ok_en1_indepQ=False; break
        if not ok_en1_indepQ: continue
        # residual function of the remaining inputs (drop en and qi), taken at en=1
        rem=[i for i in range(n) if i!=en and i!=qi]
        rbits=[]
        for ra in range(1<<len(rem)):
            a=(1<<en)  # en=1
            for bitpos,src in enumerate(rem):
                if (ra>>bitpos)&1: a|=(1<<src)
            rbits.append(bits[a])
        is_reset0 = all(b==0 for b in rbits)  # en=1 forces 0 -> it's a sync reset, not enable
        return en, rbits, rem, is_reset0
    return None

def main():
    inp,out=sys.argv[1],sys.argv[2]
    j=json.load(open(inp)); m=max(j["modules"].values(),key=lambda x:len(x.get("cells",{})))
    cells=m["cells"]
    drv={}
    for cn,c in cells.items():
        for p,nets in c.get("connections",{}).items():
            if c.get("port_directions",{}).get(p)=="output":
                for b in nets:
                    if isinstance(b,int): drv[b]=cn
    maxbit=1
    for c in cells.values():
        for nets in c.get("connections",{}).values():
            for b in nets:
                if isinstance(b,int): maxbit=max(maxbit,b)
    def is_const1(b):  # CE tied high?
        if not isinstance(b,int): return b in ("1",)   # literal const bit
        d=drv.get(b)
        return d is not None and cells[d]["type"]=="VCC"
    n_ce=0; n_r=0; newcells={}; dropped=[]
    for cn,c in list(cells.items()):
        if c["type"] not in ("FDRE","FDCE","FDSE","FDPE"): continue
        ce=c["connections"].get("CE",[None])[0]
        if not is_const1(ce): continue           # only FFs with CE currently tied high
        d=c["connections"].get("D",[None])[0]
        q=c["connections"].get("Q",[None])[0]
        if not (isinstance(d,int) and isinstance(q,int)): continue
        ld=drv.get(d)
        if ld is None or not cells[ld]["type"].startswith("LUT"): continue
        lut=cells[ld]
        if list(lut["connections"].get("O",[None]))[0]!=d: continue
        if len([u for u in cells.values() if any(d in v for k,v in u.get("connections",{}).items() if k!="O")])!=1:
            continue  # D-LUT drives only this FF (safe to rewrite)
        ins=lut_inputs(lut); n=len(ins)
        if n==0 or q not in ins: continue
        qi=ins.index(q)
        try:
            bits=init_bits(lut,n)
        except Exception:
            continue
        res=try_lift(bits,n,qi)
        if res is None: continue
        en,rbits,rem,is_reset0=res
        en_net=ins[en]
        # build residual D
        if len(rem)==0:
            newd_net = None  # constant
            const_val=rbits[0]
        else:
            maxbit+=1; newd_net=maxbit
            rinit=0
            for i,b in enumerate(rbits):
                if b: rinit|=(1<<i)
            rconns={f"I{k}":[ins[src]] for k,src in enumerate(rem)}
            rconns["O"]=[newd_net]
            rdirs={f"I{k}":"input" for k in range(len(rem))}; rdirs["O"]="output"
            newcells[f"{ld}$resid"]={"type":f"LUT{len(rem)}","parameters":{"INIT":bin(rinit)[2:].zfill(1<<len(rem))},
                "port_directions":rdirs,"connections":rconns}
        # rewrite the FF
        if is_reset0:
            # en=1 forces D=0 -> this is really a sync reset: R=en, D stays Q-hold path... 
            # conservative: only lift as CE (skip R re-encode to stay safe)
            continue
        c["connections"]["CE"]=[en_net]
        if newd_net is not None:
            c["connections"]["D"]=[newd_net]
        dropped.append(ld)   # old D-LUT now unused (will be GC'd by opt_clean later)
        n_ce+=1
    cells.update(newcells)
    json.dump(j,open(out,"w"))
    print(f"CE-lifted {n_ce} FFs (enable moved D-LUT->CE); residual LUTs added {len(newcells)}; old D-LUTs freed {len(dropped)}")

if __name__=="__main__": main()
