#!/bin/bash
# Compare all VHDL vs SystemVerilog IR pairs

echo "═══════════════════════════════════════════════════════════════"
echo "  IR Pair Comparison Report"
echo "  VHDL vs SystemVerilog"
echo "═══════════════════════════════════════════════════════════════"
echo ""

modules=(
  "apb_uart"
  "slib_clock_div"
  "slib_counter"
  "slib_edge_detect"
  "slib_fifo"
  "slib_input_filter"
  "slib_input_sync"
  "slib_mv_filter"
  "uart_baudgen"
  "uart_interrupt"
  "uart_receiver"
  "uart_transmitter"
)

for module in "${modules[@]}"; do
  vhdl_file="ir_dumps/${module}_vhdl_ir.v"
  sv_file="ir_dumps/${module}_sv_ir.v"

  if [ ! -f "$vhdl_file" ] || [ ! -f "$sv_file" ]; then
    echo "❌ $module: Missing files"
    continue
  fi

  echo "───────────────────────────────────────────────────────────────"
  echo "Module: $module"
  echo "───────────────────────────────────────────────────────────────"

  # Get file sizes
  vhdl_size=$(wc -l < "$vhdl_file")
  sv_size=$(wc -l < "$sv_file")

  echo "  VHDL IR: $vhdl_size lines"
  echo "  SV IR:   $sv_size lines"

  # Count diff lines
  diff_lines=$(diff -u "$vhdl_file" "$sv_file" | wc -l)

  if [ "$diff_lines" -eq 0 ]; then
    echo "  ✅ IDENTICAL"
  else
    echo "  ⚠️  DIFFERENT ($diff_lines diff lines)"

    # Show brief diff summary
    echo ""
    echo "  Diff summary:"
    diff -u "$vhdl_file" "$sv_file" | grep -E "^[\+\-]" | grep -v "^[\+\-][\+\-][\+\-]" | head -20 | sed 's/^/    /'
    echo ""
  fi
  echo ""
done

echo "═══════════════════════════════════════════════════════════════"
echo ""
echo "For detailed comparison, run:"
echo "  diff -u ir_dumps/<module>_vhdl_ir.v ir_dumps/<module>_sv_ir.v"
echo ""
