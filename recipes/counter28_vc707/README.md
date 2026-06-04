# counter28_vc707 — SVS-flow hardware regression on VC707

A slowed-LFSR sibling of the upstream `counter25` VC707 example, used
to confirm the SVS-frontend fixes for issues #65 (FDSE INIT
propagation) and #106 (LUT5-output OBUF buffer routed via xOUTMUX)
on real silicon at a Johnson advance rate the human eye can resolve.

## Why a 28-bit LFSR

The reference `counter25` design ticks the 8-bit Johnson counter
once per `(2^25 − 1)` clocks (~6 Hz at 200 MHz).  On the
VC707 + V7 open-flow combination, a residual silicon-level rate
glitch on OLOGIC OQ pads pushes the visible LED rate above
persistence of vision — the LEDs *look* uniformly on/off rather
than visibly walking the Johnson sequence.  Widening to 28 bits
shifts the period to `~(2^28 − 1)` clocks (~1 Hz), slow enough
that the Johnson pattern is unambiguously visible while the
underlying rate glitch is being separately investigated.

The 28-bit LFSR uses the primitive polynomial `x^28 + x^3 + 1`.

## Hardware regression target

This design is the end-to-end SVS-flow correctness test:

1. **#65 — FDSE INIT preservation.**  `reg [27:0] prbs = 28'h1;` only
   reaches FDSE inference if Verible's `trailing_decl_assignment2` is
   captured into BIR's `initial_value`.  Without that fix, the LFSR
   locks at the all-zero forbidden state and the LEDs stay at the
   power-on residue (`0x11` on this VC707 build).
2. **#106 — OBUF buffer placement.**  The structural buffer that
   `hardcaml_to_behavioral` inserts at every output bit must be a
   LUT6 with all six inputs tied to the data signal.  A LUT1 buffer
   lands at a `*5LUT` BEL slot whose `O5 → xOUTMUX → xMUX` path is
   unreliable on V7 silicon when no FF is co-located; the LUT6 lands
   at a `*6LUT` slot whose `O6 → x` path is reliable.

If either fix regresses, the symptom on this bit is unambiguous and
visible from across the room.

## Run

```bash
sv_suite script /home/jonathan/System-Verilog-suite/recipes/counter28_vc707/recipe.lua
```

That writes `top.json` (nextpnr-xilinx input) and `top.edif` (golden
reference) into this directory.  Then the standard downstream
pipeline:

```bash
NEXTPNR_ARC_MAX_VISIT=2000000 NEXTPNR_SKIP_FAILED_ARCS=1 \
  nextpnr-xilinx --router router1 \
    --chipdb ~/nextpnr-xilinx/xilinx/xc7vx485t.bin \
    --xdc top.xdc --json top.json --fasm counter28.fasm --freq 200

XRAY_ALLOW_MISSING_FEATURES=1 PATH=~/prjxray/env/bin:$PATH \
  python3 ~/prjxray/utils/fasm2frames.py \
    --db-root ~/prjxray/database/virtex7 --part xc7vx485tffg1761-2 \
    counter28.fasm counter28.frames

~/prjxray/build/tools/xc7frames2bit \
    --part_file ~/prjxray/database/virtex7/xc7vx485tffg1761-2/part.yaml \
    --part_name xc7vx485tffg1761-2 \
    --frm_file counter28.frames --output_file counter28.bit

openFPGALoader --cable digilent --freq 15000000 counter28.bit
```

Expected: all eight VC707 LEDs walk the 8-bit Johnson sequence
(`0x00 → 0x01 → 0x03 → 0x07 → ... → 0xFF → 0xFE → ... → 0x00`)
at ~1 Hz, with `rst` (CPU_RESET, AV40) clearing back to `0x00`.
