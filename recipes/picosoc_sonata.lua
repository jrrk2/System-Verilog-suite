-- recipes/picosoc_sonata.lua
--
-- Port of picosoc_build_sonata.sh: build the PicoSoC bitstream for the
-- Sonata board (xc7a50tcsg324-1) and package it as a UF2.
--
-- Pipeline:
--   1. SVS native synthesis      -> $BUILD/picosoc_fpga.json
--   2. nextpnr-xilinx            -> $BUILD/pnr/top.fasm
--   3. fasm2frames + xc7frames2bit -> top.bit
--   4. uf2conv                   -> top.slot1.uf2
--
-- Caller globals (all optional, defaults below):
--   HOME, PICO, BUILD, NEXTPNR, CHIPDB, PRJXRAY, PRJXRAY_DB,
--   PART, XDC, TOP, WRAPPER, UF2_FAMILY, UF2_SLOT.

-- ── Defaults (mirror the shell script's ${VAR:-default} pattern) ──
HOME       = HOME       or "/home/jonathan"
PICO       = PICO       or HOME .. "/f4pga-examples/xc7/picosoc_demo"
BUILD      = BUILD      or HOME .. "/picosoc_build"
NEXTPNR    = NEXTPNR    or HOME .. "/nextpnr-xilinx/build/nextpnr-xilinx"
CHIPDB     = CHIPDB     or HOME .. "/nextpnr-xilinx/xilinx/xc7a50t.bin"
PRJXRAY    = PRJXRAY    or HOME .. "/prjxray"
PRJXRAY_DB = PRJXRAY_DB or HOME .. "/nextpnr-xilinx/xilinx/external/prjxray-db"
PART       = PART       or "xc7a50tcsg324-1"
XDC        = XDC        or BUILD .. "/sonata_top.xdc"
TOP        = TOP        or "sonata_top"
WRAPPER    = WRAPPER    or BUILD .. "/sonata_top.v"
UF2_FAMILY = UF2_FAMILY or "0x6ce29e6b"
UF2_SLOT   = UF2_SLOT   or "slot1"

print("recipe: picosoc_sonata  top=" .. TOP .. "  part=" .. PART)
print("  BUILD=" .. BUILD)

execute("mkdir -p " .. BUILD .. "/pnr")

JSON = BUILD .. "/picosoc_fpga.json"
FASM = BUILD .. "/pnr/top.fasm"
FRM  = BUILD .. "/pnr/top.frames"
BIT  = BUILD .. "/pnr/top.bit"
UF2  = BUILD .. "/pnr/top." .. UF2_SLOT .. ".uf2"

-- ── [1/4] SVS native synthesis ──
print(">> [1/4] SVS: " .. TOP .. " -> " .. JSON)

-- Tell Behavioral_memlower to target FPGA BRAMs (was Unix.putenv in the
-- old test_picosoc_fpga.ml). Unset before svd.meminfer in an ASIC recipe.
execute("true")  -- placeholder; env-set helper would go here
prog = svd.parse("verible", TOP,
    { WRAPPER,
      PICO .. "/picosoc_noflash.v",
      PICO .. "/picorv32.v",
      PICO .. "/progmem.v",
      PICO .. "/simpleuart.v" })
print("  parsed: " .. svd.module_names(prog))

-- flatten_for_z3 returns a Mod handle wrapping a singleton program.
flat = svd.flatten_z3(prog, TOP)
prog = svd.owner(flat)

prog = svd.unroll(prog)
prog = svd.inline(prog)
prog = svd.iflift(prog)
prog = svd.blocking_subst(prog)
prog = svd.meminfer(prog)
prog = svd.memlower(prog)
prog = svd.ssa(prog)
print("  pipeline done")

m = svd.pick(prog, TOP)
mapped = svd.gate_map(m, 6, 1)        -- k_lut=6, io=true
svd.write_mapped_json(mapped, JSON)
print("  wrote " .. JSON)

-- ── [2/4] nextpnr-xilinx ──
print(">> [2/4] nextpnr-xilinx (" .. PART .. ")")
rc = execute(NEXTPNR ..
    " --chipdb " .. CHIPDB ..
    " --xdc "    .. XDC    ..
    " --json "   .. JSON   ..
    " --write "  .. BUILD .. "/pnr/routed.json" ..
    " --fasm "   .. FASM)
if rc ~= 0 then print("nextpnr-xilinx FAILED rc=" .. rc); return end

-- ── [3/4] FASM → frames → bit ──
print(">> [3/4] fasm2frames + xc7frames2bit")
PY = PRJXRAY .. "/env/bin/python3"
rc = execute(PY .. " " .. PRJXRAY .. "/utils/fasm2frames.py" ..
    " --part "    .. PART ..
    " --db-root " .. PRJXRAY_DB .. "/artix7 " ..
    FASM .. " " .. FRM)
if rc ~= 0 then print("fasm2frames FAILED rc=" .. rc); return end
rc = execute(PRJXRAY .. "/build/tools/xc7frames2bit" ..
    " --part_file "   .. PRJXRAY_DB .. "/artix7/" .. PART .. "/part.yaml" ..
    " --part_name "   .. PART ..
    " --frm_file "    .. FRM ..
    " --output_file " .. BIT)
if rc ~= 0 then print("xc7frames2bit FAILED rc=" .. rc); return end

-- ── [4/4] uf2conv packaging ──
print(">> [4/4] uf2conv (family " .. UF2_FAMILY .. ", " .. UF2_SLOT .. ")")
rc = execute("uf2conv -b 0x00000000 -f " .. UF2_FAMILY ..
             " " .. BIT .. " -co " .. UF2)
if rc ~= 0 then print("uf2conv FAILED rc=" .. rc); return end

print("")
print("── PASS ──")
execute("ls -la " .. BIT .. " " .. UF2)
print("")
print("Flash to Sonata: copy " .. UF2)
print("onto the Sonata board's USB mass-storage drive.")
