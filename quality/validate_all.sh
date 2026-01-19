#!/bin/bash
# validate_all.sh - Master validation script
# Runs all quality checks and round-trip validation

set -e

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
REPORT_DIR="validation_reports"
mkdir -p "$REPORT_DIR"

# Colors
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo "=========================================="
echo "Complete Decompiler Validation Suite"
echo "=========================================="
echo "Timestamp: $TIMESTAMP"
echo ""

# Step 1: Basic Quality Checks
echo -e "${BLUE}Step 1: Basic Quality Assessment${NC}"
echo "=========================================="
if [ -f "stream_quality_analyzer.py" ]; then
    python3 stream_quality_analyzer.py results/ \
        "$REPORT_DIR/quality_$TIMESTAMP.json" \
        "$REPORT_DIR/quality_$TIMESTAMP.csv"
    echo -e "${GREEN}✓ Quality assessment complete${NC}"
else
    echo -e "${YELLOW}⚠ stream_quality_analyzer.py not found, skipping${NC}"
fi
echo ""

# Step 2: Round-Trip Validation
echo -e "${BLUE}Step 2: Round-Trip Validation${NC}"
echo "=========================================="
if [ -f "run_roundtrip_test.sh" ]; then
    ./run_roundtrip_test.sh obj_dir/
    mv roundtrip_report.csv "$REPORT_DIR/roundtrip_$TIMESTAMP.csv"
    echo -e "${GREEN}✓ Round-trip validation complete${NC}"
else
    echo -e "${YELLOW}⚠ run_roundtrip_test.sh not found, skipping${NC}"
fi
echo ""

# Step 3: Detailed AST Comparison
echo -e "${BLUE}Step 3: Detailed AST Analysis${NC}"
echo "=========================================="
if [ -f "ast_diff_viewer.py" ] && [ -d "obj_dir" ] && [ -d "roundtrip_json" ]; then
    python3 ast_diff_viewer.py obj_dir/ roundtrip_json/ \
        "$REPORT_DIR/ast_comparison_$TIMESTAMP.json"
    echo -e "${GREEN}✓ AST analysis complete${NC}"
else
    echo -e "${YELLOW}⚠ Skipping AST analysis (missing files/dirs)${NC}"
fi
echo ""

# Step 4: Generate Summary Report
echo -e "${BLUE}Step 4: Generating Summary${NC}"
echo "=========================================="

SUMMARY_FILE="$REPORT_DIR/summary_$TIMESTAMP.txt"

{
    echo "=========================================="
    echo "Validation Summary - $TIMESTAMP"
    echo "=========================================="
    echo ""
    
    # Quality metrics
    if [ -f "$REPORT_DIR/quality_$TIMESTAMP.json" ]; then
        echo "Quality Assessment:"
        echo "------------------"
        AVG_QUALITY=$(python3 -c "import json; data=json.load(open('$REPORT_DIR/quality_$TIMESTAMP.json')); print(f\"{data['summary']['avg_score']*100:.1f}\")" 2>/dev/null || echo "N/A")
        TOTAL_FILES=$(python3 -c "import json; data=json.load(open('$REPORT_DIR/quality_$TIMESTAMP.json')); print(data['summary']['total_files'])" 2>/dev/null || echo "N/A")
        PASSED=$(python3 -c "import json; data=json.load(open('$REPORT_DIR/quality_$TIMESTAMP.json')); print(data['summary']['passed'])" 2>/dev/null || echo "N/A")
        
        echo "  Files analyzed: $TOTAL_FILES"
        echo "  Passed quality checks: $PASSED"
        echo "  Average quality score: $AVG_QUALITY%"
        echo ""
    fi
    
    # Round-trip metrics
    if [ -f "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" ]; then
        echo "Round-Trip Validation:"
        echo "---------------------"
        RT_TOTAL=$(tail -n +2 "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" | wc -l)
        RT_PERFECT=$(tail -n +2 "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" | awk -F',' '$2=="true" {count++} END {print count+0}')
        RT_AVG=$(tail -n +2 "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" | awk -F',' '{sum+=$3} END {printf "%.1f", sum/NR*100}')
        
        echo "  Files compared: $RT_TOTAL"
        echo "  Perfect matches: $RT_PERFECT ($((RT_PERFECT*100/RT_TOTAL))%)"
        echo "  Average similarity: $RT_AVG%"
        echo ""
        
        echo "  Files with differences:"
        tail -n +2 "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" | \
            awk -F',' '$2=="false" {printf "    %-40s %5.1f%% (%3d diffs)\n", $1, $3*100, $4}' | \
            head -10
        echo ""
    fi
    
    # Overall assessment
    echo "Overall Assessment:"
    echo "------------------"
    
    QUALITY_PASS=false
    ROUNDTRIP_PASS=false
    
    if [ "$AVG_QUALITY" != "N/A" ] && (( $(echo "$AVG_QUALITY >= 80" | bc -l 2>/dev/null) )); then
        echo "  ✓ Quality checks: PASS (≥80%)"
        QUALITY_PASS=true
    else
        echo "  ✗ Quality checks: FAIL (<80%)"
    fi
    
    if [ "$RT_AVG" != "" ] && (( $(echo "$RT_AVG >= 85" | bc -l 2>/dev/null) )); then
        echo "  ✓ Round-trip validation: PASS (≥85%)"
        ROUNDTRIP_PASS=true
    else
        echo "  ✗ Round-trip validation: FAIL (<85%)"
    fi
    
    echo ""
    
    if $QUALITY_PASS && $ROUNDTRIP_PASS; then
        echo "🎉 Overall Result: PASS"
        echo ""
        echo "Your decompiler is producing high-quality, lossless conversions!"
    elif $QUALITY_PASS || $ROUNDTRIP_PASS; then
        echo "⚠️  Overall Result: PARTIAL PASS"
        echo ""
        echo "Some aspects are good, but improvements needed."
    else
        echo "❌ Overall Result: FAIL"
        echo ""
        echo "Significant issues detected. Review the detailed reports."
    fi
    
    echo ""
    echo "Detailed reports available in: $REPORT_DIR/"
    echo "=========================================="
    
} | tee "$SUMMARY_FILE"

echo ""
echo -e "${GREEN}✓ Summary saved to: $SUMMARY_FILE${NC}"
echo ""

# Step 5: Generate HTML Report (optional)
echo -e "${BLUE}Step 5: Generate HTML Report${NC}"
echo "=========================================="

HTML_REPORT="$REPORT_DIR/report_$TIMESTAMP.html"

cat > "$HTML_REPORT" << 'EOF'
<!DOCTYPE html>
<html>
<head>
    <title>Decompiler Validation Report</title>
    <style>
        body {
            font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
            max-width: 1200px;
            margin: 0 auto;
            padding: 20px;
            background: #f5f5f5;
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 10px;
            margin-bottom: 30px;
        }
        .section {
            background: white;
            padding: 25px;
            margin-bottom: 20px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .metric {
            display: inline-block;
            margin: 10px 20px 10px 0;
        }
        .metric-value {
            font-size: 2em;
            font-weight: bold;
            color: #667eea;
        }
        .metric-label {
            font-size: 0.9em;
            color: #666;
            text-transform: uppercase;
        }
        .status-pass {
            color: #10b981;
            font-weight: bold;
        }
        .status-fail {
            color: #ef4444;
            font-weight: bold;
        }
        .status-warn {
            color: #f59e0b;
            font-weight: bold;
        }
        table {
            width: 100%;
            border-collapse: collapse;
            margin-top: 15px;
        }
        th, td {
            padding: 12px;
            text-align: left;
            border-bottom: 1px solid #e5e7eb;
        }
        th {
            background: #f9fafb;
            font-weight: 600;
        }
        .progress-bar {
            width: 100%;
            height: 30px;
            background: #e5e7eb;
            border-radius: 15px;
            overflow: hidden;
            margin: 10px 0;
        }
        .progress-fill {
            height: 100%;
            background: linear-gradient(90deg, #10b981 0%, #059669 100%);
            transition: width 0.3s ease;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-weight: bold;
        }
    </style>
</head>
<body>
    <div class="header">
        <h1>🔍 Decompiler Validation Report</h1>
        <p>Generated: TIMESTAMP_PLACEHOLDER</p>
    </div>
EOF

# Add quality metrics
if [ -f "$REPORT_DIR/quality_$TIMESTAMP.json" ]; then
    cat >> "$HTML_REPORT" << EOF
    <div class="section">
        <h2>📊 Quality Assessment</h2>
        <div class="metric">
            <div class="metric-value">$AVG_QUALITY%</div>
            <div class="metric-label">Average Quality</div>
        </div>
        <div class="metric">
            <div class="metric-value">$PASSED/$TOTAL_FILES</div>
            <div class="metric-label">Files Passed</div>
        </div>
        <div class="progress-bar">
            <div class="progress-fill" style="width: $AVG_QUALITY%">$AVG_QUALITY%</div>
        </div>
    </div>
EOF
fi

# Add round-trip metrics
if [ -f "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" ]; then
    cat >> "$HTML_REPORT" << EOF
    <div class="section">
        <h2>🔄 Round-Trip Validation</h2>
        <div class="metric">
            <div class="metric-value">$RT_AVG%</div>
            <div class="metric-label">Average Similarity</div>
        </div>
        <div class="metric">
            <div class="metric-value">$RT_PERFECT/$RT_TOTAL</div>
            <div class="metric-label">Perfect Matches</div>
        </div>
        <div class="progress-bar">
            <div class="progress-fill" style="width: $RT_AVG%">$RT_AVG%</div>
        </div>
        
        <h3>Files with Differences</h3>
        <table>
            <tr>
                <th>Filename</th>
                <th>Similarity</th>
                <th>Differences</th>
                <th>Status</th>
            </tr>
EOF
    
    # Add table rows
    tail -n +2 "$REPORT_DIR/roundtrip_$TIMESTAMP.csv" | head -20 | while IFS=',' read -r filename identical similarity diffs rest; do
        sim_pct=$(echo "$similarity * 100" | bc -l | xargs printf "%.1f")
        if [ "$identical" = "true" ]; then
            status="<span class='status-pass'>✓ PERFECT</span>"
        elif (( $(echo "$similarity >= 0.9" | bc -l) )); then
            status="<span class='status-warn'>⚠ MINOR</span>"
        else
            status="<span class='status-fail'>✗ MAJOR</span>"
        fi
        
        echo "            <tr>" >> "$HTML_REPORT"
        echo "                <td>$filename</td>" >> "$HTML_REPORT"
        echo "                <td>$sim_pct%</td>" >> "$HTML_REPORT"
        echo "                <td>$diffs</td>" >> "$HTML_REPORT"
        echo "                <td>$status</td>" >> "$HTML_REPORT"
        echo "            </tr>" >> "$HTML_REPORT"
    done
    
    cat >> "$HTML_REPORT" << EOF
        </table>
    </div>
EOF
fi

# Close HTML
cat >> "$HTML_REPORT" << EOF
    <div class="section">
        <h2>📁 Detailed Reports</h2>
        <ul>
            <li><a href="quality_$TIMESTAMP.csv">Quality Assessment CSV</a></li>
            <li><a href="roundtrip_$TIMESTAMP.csv">Round-Trip Validation CSV</a></li>
            <li><a href="summary_$TIMESTAMP.txt">Text Summary</a></li>
        </ul>
    </div>
</body>
</html>
EOF

sed -i "s/TIMESTAMP_PLACEHOLDER/$TIMESTAMP/g" "$HTML_REPORT"

echo -e "${GREEN}✓ HTML report saved to: $HTML_REPORT${NC}"
echo ""

# Final summary
echo "=========================================="
echo "Validation Complete!"
echo "=========================================="
echo ""
echo "📊 Reports generated:"
echo "  • Summary:    $SUMMARY_FILE"
echo "  • HTML:       $HTML_REPORT"
echo "  • Quality:    $REPORT_DIR/quality_$TIMESTAMP.csv"
echo "  • Round-trip: $REPORT_DIR/roundtrip_$TIMESTAMP.csv"
echo ""
echo "Open the HTML report in your browser:"
echo "  open $HTML_REPORT  # macOS"
echo "  xdg-open $HTML_REPORT  # Linux"
echo ""

# Exit with appropriate code
if $QUALITY_PASS && $ROUNDTRIP_PASS; then
    exit 0
else
    exit 1
fi