#!/bin/bash
# Run picorv32 regression. Each .srcs file just lists picorv32.v;
# the top is the basename of the test (e.g., picorv32_pcpi_mul).
set -e
here=$(cd "$(dirname "$0")" && pwd)
cd "$here"

backend=${1:-xilinx_rtl}
tests="picorv32_regs picorv32_pcpi_mul picorv32_pcpi_fast_mul picorv32_pcpi_div picorv32"

for t in $tests; do
    bash compare.sh "$t" "$backend"
done
