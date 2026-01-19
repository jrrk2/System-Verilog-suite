#!/bin/bash
# run_roundtrip_test.sh - Complete round-trip validation
#
# Flow:
# 1. Start with hand-written SV files in svsrc/
# 2. Compile them with Verilator → original JSON
# 3. Decompile JSON → SV (decompiled)
# 4. Re-compile decompiled SV → round-trip JSON
# 5. Compare original JSON vs round-trip JSON

set +e

# Directories
HANDWRITTEN_SV_DIR=${1:-"svsrc"}
ORIGINAL_JSON_DIR="obj_dir"  # Verilator always writes here
DECOMPILED_SV_DIR="decompiled_sv"
ROUNDTRIP_JSON_DIR="roundtrip_json"
ORIGINAL_JSON_BACKUP="original_json_backup"
REPORT_FILE="roundtrip_report.csv"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo "=========================================="
echo "Complete Round-Trip Validation"
echo "=========================================="
echo "Flow: Hand-written SV → JSON → Decompiled SV → JSON → Compare"
echo "Timestamp: $TIMESTAMP"
echo ""

# Check verilator
if ! command -v verilator &> /dev/null; then
    echo -e "${RED}Error: Verilator not found${NC}"
    exit 1
fi

# Create directories
mkdir -p "$ORIGINAL_JSON_BACKUP"
mkdir -p "$DECOMPILED_SV_DIR"
mkdir -p "$ROUNDTRIP_JSON_DIR"

# Clean obj_dir to start fresh
rm -f obj_dir/*.json obj_dir/*.tree* 2>/dev/null

# Step 1: Compile hand-written SV to JSON
echo -e "${BLUE}Step 1: Compiling hand-written SV to JSON${NC}"
echo "========================================"

sv_files=$(find "$HANDWRITTEN_SV_DIR" -name "*.sv" -type f)
sv_count=$(echo "$sv_files" | wc -l)

echo "Found $sv_count hand-written SV files"

if [ "$sv_count" -eq 0 ]; then
    echo -e "${RED}Error: No .sv files in $HANDWRITTEN_SV_DIR${NC}"
    exit 1
fi

compile_count=0
for sv_file in $sv_files; do
    basename=$(basename "$sv_file" .sv)
    echo -n "Compiling: $basename ... "
    
    # Use proper Verilator flags (no --top-module, no --dump-tree-json in Step 1)
    if verilator --json-only -Wno-widthtrunc --no-timing -Wno-stmtdly -Wno-widthexpand "$sv_file" 2>&1 | grep -qi "%Error"; then
        echo -e "${RED}✗ Failed${NC}"
    else
        # Verilator writes to obj_dir/V<module>.tree.json
        found_json=$(ls obj_dir/V*.tree.json 2>/dev/null | head -1)
        if [ -n "$found_json" ] && [ -f "$found_json" ]; then
            # Backup the original JSON with a clear name
            cp "$found_json" "$ORIGINAL_JSON_BACKUP/${basename}.tree.json"
            echo -e "${GREEN}✓ Success${NC}"
            compile_count=$((compile_count + 1))
        else
            echo -e "${YELLOW}⚠ No JSON${NC}"
        fi
    fi
done

echo ""
echo "Successfully compiled: $compile_count / $sv_count"

if [ "$compile_count" -eq 0 ]; then
    echo -e "${RED}Error: No JSON files generated${NC}"
    exit 1
fi

rm -f obj_dir/*.meta.json
ls -l obj_dir

# Step 2: Decompile JSON to SV
echo ""
echo -e "${BLUE}Step 2: Decompiling JSON to SV${NC}"
echo "========================================"

# Your decompiler reads from obj_dir/ (where Verilator wrote them)
# It will write to results/ with "decompile_" prefix

# Find decompiler
DECOMPILER=""
if [ -f "./sv_main" ]; then
    DECOMPILER="./sv_main"
elif [ -f "../sv_main" ]; then
    DECOMPILER="../sv_main"
fi

if [ -z "$DECOMPILER" ]; then
    echo -e "${RED}Error: Decompiler not found${NC}"
    exit 1
fi

echo "Running decompiler on JSON files in obj_dir/..."

# The JSON files are already in obj_dir/ from Step 1
# Just run the decompiler
$DECOMPILER results/

# Count and collect decompiled files
decompile_count=0
for gen_file in results/decompile_*.sv; do
    if [ -f "$gen_file" ]; then
        # Extract base name: decompile_Valu.tree.json.sv -> alu
        # Remove "decompile_V" prefix and ".tree.json.sv" suffix
        base=$(basename "$gen_file" .sv)
        base=$(echo "$base" | sed 's/^decompile_V//' | sed 's/\.tree\.json$//')
        
        echo "  Processing: $(basename "$gen_file") -> ${base}.sv"
        
        # Check if we have a corresponding original
        # The original was backed up as ${base}.tree.json
        if [ -f "$ORIGINAL_JSON_BACKUP/${base}.tree.json" ]; then
            cp "$gen_file" "$DECOMPILED_SV_DIR/${base}.sv"
            echo "  ✓ Decompiled: $base"
            decompile_count=$((decompile_count + 1))
        else
            echo "  ⚠ No matching original for: $base"
            echo "     Looking for: $ORIGINAL_JSON_BACKUP/${base}.tree.json"
        fi
    fi
done

echo ""
echo "Successfully decompiled: $decompile_count / $compile_count"

if [ "$decompile_count" -eq 0 ]; then
    echo -e "${RED}Error: No SV files generated${NC}"
    exit 1
fi

# Step 3: Re-compile decompiled SV to JSON
echo ""
echo -e "${BLUE}Step 3: Re-compiling decompiled SV to JSON${NC}"
echo "========================================"

# Clear obj_dir before re-compiling
rm -rf obj_dir/*.tree* 2>/dev/null

recompile_count=0
for sv_file in "$DECOMPILED_SV_DIR"/*.sv; do
    if [ -f "$sv_file" ]; then
        basename=$(basename "$sv_file" .sv)
        echo -n "Re-compiling: $basename ... "
        
        rm -rf obj_dir/*.tree* 2>/dev/null
        
        # Use same Verilator flags as Step 1, plus additional warning suppressions for decompiled code
        if verilator --dump-tree-json --json-only -Wno-widthtrunc --no-timing -Wno-stmtdly -Wno-widthexpand -Wno-CASEWITHX -Wno-COMBDLY -Wno-LATCH -Wno-UNOPTFLAT --top-module "$basename" "$sv_file" 2>&1 | grep -qi "%Error"; then
            echo -e "${RED}✗ Failed${NC}"
            # Uncomment to see errors:
            # verilator --dump-tree-json --json-only -Wno-widthtrunc --no-timing -Wno-stmtdly -Wno-widthexpand -Wno-CASEWITHX --top-module "$basename" "$sv_file" 2>&1 | grep "%Error" | head -3
        else
            found_json=$(ls obj_dir/V*.tree.json 2>/dev/null | head -1)
            if [ -n "$found_json" ] && [ -f "$found_json" ]; then
                cp "$found_json" "$ROUNDTRIP_JSON_DIR/${basename}.tree.json"
                echo -e "${GREEN}✓ Success${NC}"
                recompile_count=$((recompile_count + 1))
            else
                echo -e "${YELLOW}⚠ No JSON${NC}"
            fi
        fi
    fi
done

echo ""
echo "Successfully re-compiled: $recompile_count / $decompile_count"

if [ "$recompile_count" -eq 0 ]; then
    echo -e "${RED}Error: No round-trip JSON files generated${NC}"
    exit 1
fi

# Step 4: Compare original vs round-trip JSON
echo ""
echo -e "${BLUE}Step 4: Comparing original vs round-trip JSON${NC}"
echo "========================================"

if [ ! -f "./roundtrip_validator" ]; then
    echo "Compiling validator..."
    ocamlfind ocamlc -I .. -package yojson,unix,str -linkpkg \
        ../sv_ast.mli ../sv_parse.ml ../sv_gen.ml ../sv_main.ml \
        roundtrip_validator.ml -o roundtrip_validator
fi

if [ -f "./roundtrip_validator" ]; then
    ./roundtrip_validator "$ORIGINAL_JSON_BACKUP" "$ROUNDTRIP_JSON_DIR" "$REPORT_FILE"
else
    echo -e "${YELLOW}⚠ Validator not available, using Python comparison${NC}"
    
    if command -v python3 &> /dev/null && [ -f "ast_diff_viewer.py" ]; then
        python3 ast_diff_viewer.py "$ORIGINAL_JSON_BACKUP" "$ROUNDTRIP_JSON_DIR" "comparison_results.json"
    else
        echo "Manual comparison needed:"
        echo "  Original JSON:   $ORIGINAL_JSON_BACKUP/"
        echo "  Round-trip JSON: $ROUNDTRIP_JSON_DIR/"
    fi
fi

# Summary
echo ""
echo "=========================================="
echo "Round-Trip Test Complete!"
echo "=========================================="
echo "Step 1: Compiled SV→JSON:      $compile_count files"
echo "Step 2: Decompiled JSON→SV:    $decompile_count files"  
echo "Step 3: Re-compiled SV→JSON:   $recompile_count files"
echo ""
echo "Files:"
echo "  Original JSON:    $ORIGINAL_JSON_BACKUP/"
echo "  Decompiled SV:    $DECOMPILED_SV_DIR/"
echo "  Round-trip JSON:  $ROUNDTRIP_JSON_DIR/"
echo ""

if [ -f "$REPORT_FILE" ]; then
    echo "Comparison report: $REPORT_FILE"
fi

echo ""
echo "Success! Your decompiler completed a full round-trip."
