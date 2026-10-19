#!/usr/bin/env python3
# Generate the LiteX CVA6 SoC for the VC707 with examples/vc707-litex/vc707_litex.py,
# using upstream LiteX's CVA6 port instead of the one in litex-deps.
#
# litex-deps' litex (the jrrk2 fork) carries an older cpu/cva6: its core.py asks
# for core/Flist.cv64a6_imafdc_sv39 (gone from CVA6 since #1012) and its wrapper
# manifest for corev_apu/src/axi_riscv_atomics (vendored in #3183, removed again
# since).  No pythondata-cpu-cva6 commit satisfies both.  Upstream's port
# (b78494107, TARGET_CFGS + core/Flist.cva6) matches pythondata-cpu-cva6 HEAD,
# so register that class under "cva6" -- the same hook vc707_litex.py uses for
# BOOM -- and run the board script unchanged.
#
#   gen_soc.py <vc707_litex.py args...>
#   LITEX_CVA6_CORE  directory holding upstream's cva6/core.py
#                    (default ~/trixium-litex/litex/litex/soc/cores/cpu/cva6)
#   VC707_LITEX      the board script (default ~/xc7-bitstream-tools/examples/vc707-litex/vc707_litex.py)

import importlib.util
import os
import sys

home = os.path.expanduser("~")
core_dir = os.environ.get("LITEX_CVA6_CORE",
                          os.path.join(home, "trixium-litex/litex/litex/soc/cores/cpu/cva6"))
board = os.environ.get("VC707_LITEX",
                       os.path.join(home, "xc7-bitstream-tools/examples/vc707-litex/vc707_litex.py"))

from litex.soc.cores import cpu as litex_cpu

spec = importlib.util.spec_from_file_location("upstream_cva6_core", os.path.join(core_dir, "core.py"))
mod = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = mod     # the builder finds crt0.S etc. via inspect.getfile
spec.loader.exec_module(mod)
litex_cpu.CPUS["cva6"] = mod.CVA6
print(f"[gen_soc] cva6 from {core_dir}")

sys.path.insert(0, os.path.dirname(board))
spec = importlib.util.spec_from_file_location("vc707_litex", board)
board_mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(board_mod)
sys.argv = [board] + sys.argv[1:]
board_mod.main()
