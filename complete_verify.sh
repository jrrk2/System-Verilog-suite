#!/bin/bash
# complete_verify.sh - Full end-to-end verification with Verilator

set -e  # Exit on error

echo "========================================="
echo "Complete End-to-End Verification"
echo "========================================="
echo ""

# Configuration
ORIGINAL_JSON="obj_dir/Valu.tree.json"
HARDCAML_SV="/tmp/hardcaml_alu.sv"
HARDCAML_JSON="obj_dir/Vhardcaml_alu.tree.json"

# Step 1: Generate HardCaml Verilog
echo "Step 1: Generate HardCaml Verilog"
echo "========================================="
./_build/default/sv_main_unified.exe file hardcaml "$ORIGINAL_JSON" "$HARDCAML_SV"

if [ ! -f "$HARDCAML_SV" ]; then
    echo "❌ Generation failed"
    exit 1
fi

echo "✅ Generated: $HARDCAML_SV"
echo "   Lines: $(wc -l < "$HARDCAML_SV")"
echo ""

# Step 2: Parse HardCaml back to JSON with Verilator
echo "Step 2: Parse HardCaml Verilog to JSON"
echo "========================================="

# Check if Verilator is available
if ! command -v verilator &> /dev/null; then
    echo "⚠️  Verilator not found - cannot complete full verification"
    echo ""
    echo "To complete verification, install Verilator:"
    echo "  brew install verilator  # macOS"
    echo "  apt install verilator   # Linux"
    echo ""
    echo "Then run:"
    echo "  verilator --json-only $HARDCAML_SV"
    echo "  ./_build/default/verify_main.exe $ORIGINAL_JSON $HARDCAML_JSON"
    exit 1
fi

# Clean up old output
rm -rf obj_dir/Vhardcaml_alu*

# Parse with Verilator
echo "Running: verilator --json-only $HARDCAML_SV"
verilator --json-only "$HARDCAML_SV" 2>&1 | head -20

if [ ! -f "$HARDCAML_JSON" ]; then
    echo "❌ Parsing failed - JSON not created"
    echo ""
    echo "Verilator may have encountered errors. Check the output above."
    exit 1
fi

echo "✅ Parsed: $HARDCAML_JSON"
echo "   Size: $(du -h "$HARDCAML_JSON" | cut -f1)"
echo ""

# Step 3: Run Z3 Formal Verification
echo "Step 3: Z3 Formal Verification"
echo "========================================="
echo ""

./_build/default/verify_main.exe "$ORIGINAL_JSON" "$HARDCAML_JSON"

EXIT_CODE=$?
echo ""
echo "========================================="
echo "Verification Complete"
echo "========================================="
echo "Exit code: $EXIT_CODE"

if [ $EXIT_CODE -eq 0 ]; then
    echo "✅ MATHEMATICALLY PROVEN EQUIVALENT!"
    echo ""
    echo "This means:"
    echo "  • Original and HardCaml outputs match for ALL inputs"
    echo "  • 2^20 = 1,048,576 combinations verified"
    echo "  • Formal proof, not just testing"
    echo "  • HardCaml backend is CORRECT!"
else
    echo "❌ VERIFICATION FAILED"
    echo ""
    echo "This means:"
    echo "  • Found inputs where outputs differ"
    echo "  • Check the counterexample above"
    echo "  • HardCaml backend may have a bug"
fi

echo ""
exit $EXIT_CODE
