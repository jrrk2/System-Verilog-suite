#!/bin/bash
# Quick diagnostic script using the existing test infrastructure

JSON_FILE="${1:-obj_dir/Variane.tree.json}"

if [ ! -f "$JSON_FILE" ]; then
  echo "Error: JSON file not found: $JSON_FILE"
  echo "Usage: $0 [json_file]"
  exit 1
fi

echo "═══════════════════════════════════════════════════════════════"
echo "  Quick Diagnostic: What modules are in the JSON?"
echo "═══════════════════════════════════════════════════════════════"
echo ""

echo "Modules in Verilator JSON:"
jq -r '.modulesp[] | .name' "$JSON_FILE" | nl
echo ""

echo "Total modules: $(jq '.modulesp | length' "$JSON_FILE")"
echo ""

echo "═══════════════════════════════════════════════════════════════"
echo "  std_icache Detailed Analysis"
echo "═══════════════════════════════════════════════════════════════"
echo ""

echo "Checking if std_icache exists in JSON..."
STD_ICACHE_EXISTS=$(jq -r '.modulesp[] | select(.name == "std_icache") | .name' "$JSON_FILE")

if [ -n "$STD_ICACHE_EXISTS" ]; then
  echo "✓ std_icache found in JSON"
  echo ""

  echo "std_icache structure:"
  jq '.modulesp[] | select(.name == "std_icache") | {
    name,
    num_stmts: (.stmtsp | length),
    num_inlines: (.inlinesp | length // 0)
  }' "$JSON_FILE"
  echo ""

  echo "Variables (showing first 15):"
  jq -r '.modulesp[] | select(.name == "std_icache") |
    .stmtsp[] | select(.type == "VAR") | .name' "$JSON_FILE" | head -15 | nl
  echo ""

  echo "_q variables (registers):"
  jq -r '.modulesp[] | select(.name == "std_icache") |
    .stmtsp[] | select(.type == "VAR") |
    select(.name | contains("_q")) | .name' "$JSON_FILE"
  echo ""

  echo "Always blocks:"
  jq '.modulesp[] | select(.name == "std_icache") |
    [.stmtsp[] | select(.type == "ALWAYS")] |
    length' "$JSON_FILE"
  echo ""

  echo "Always block types:"
  jq -r '.modulesp[] | select(.name == "std_icache") |
    [.stmtsp[] | select(.type == "ALWAYS")] |
    .[] | .keyword' "$JSON_FILE"
else
  echo "✗ std_icache NOT found in JSON"
  echo ""
  echo "Modules containing 'cache':"
  jq -r '.modulesp[] | select(.name | contains("cache")) | .name' "$JSON_FILE"
fi

echo ""
echo "═══════════════════════════════════════════════════════════════"
