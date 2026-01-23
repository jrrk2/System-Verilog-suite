#!/bin/bash
# Analyze all unhandled JSON patterns

echo "Analyzing unhandled VHDL patterns..."
echo "========================================="
echo ""

echo "Summary by context:"
for ctx in "extract_entity" "extract_ports" "expr_to_ir" "stmt_to_ir" "concurrent_stmt" "convert_architecture"; do
  count=$(ls unhandled_*.json 2>/dev/null | xargs grep -l "\"context\": \"$ctx\"" 2>/dev/null | wc -l)
  if [ "$count" -gt 0 ]; then
    echo "  $ctx: $count unhandled cases"
  fi
done

echo ""
echo "First unhandled expression:"
grep -A 30 "\"context\": \"expr_to_ir\"" unhandled_*.json | head -35 | jq .

echo ""
echo "First unhandled statement:"
grep -A 30 "\"context\": \"stmt_to_ir\"" unhandled_*.json | head -35 | jq .

echo ""
echo "First unhandled concurrent statement:"
grep -A 30 "\"context\": \"concurrent_stmt\"" unhandled_*.json | head -35 | jq .
