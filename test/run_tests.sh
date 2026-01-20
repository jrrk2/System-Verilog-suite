#!/bin/bash
# Test suite for sv_main_unified
# Tests various backends and features

set -e  # Exit on error

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
TESTS_RUN=0
TESTS_PASSED=0
TESTS_FAILED=0

# Test directories
TEST_DIR="."
TEST_INPUT="$TEST_DIR/input"
TEST_EXPECTED="$TEST_DIR/expected"
TEST_OUTPUT="$TEST_DIR/output"
TEST_OBJ="$TEST_DIR/obj_dir"

# Executable (relative to test directory)
SV_UNIFIED="../_build/default/sv_main_unified.exe"

# Initialize test environment
init_tests() {
    echo -e "${BLUE}=== SystemVerilog Decompiler Test Suite ===${NC}"
    echo ""

    # Create directories
    mkdir -p "$TEST_INPUT" "$TEST_EXPECTED" "$TEST_OUTPUT" "$TEST_OBJ"

    # Check if executable exists
    if [ ! -f "$SV_UNIFIED" ]; then
        echo -e "${RED}Error: $SV_UNIFIED not found${NC}"
        echo "Run: dune build"
        exit 1
    fi

    # Check for verilator
    if ! command -v verilator &> /dev/null; then
        echo -e "${RED}Error: verilator not found${NC}"
        echo "Please install Verilator"
        exit 1
    fi

    echo -e "${GREEN}✓ Environment setup complete${NC}"
    echo ""
}

# Run a test
run_test() {
    local test_name="$1"
    local backend="$2"
    local input_sv="$3"
    local should_pass="$4"  # "pass" or "fail"

    TESTS_RUN=$((TESTS_RUN + 1))

    echo -e "${BLUE}Test $TESTS_RUN: $test_name [$backend]${NC}"

    # Generate JSON with Verilator
    local module_name=$(basename "$input_sv" .sv)
    verilator --json-only --quiet -Wno-fatal "$input_sv" -o "$TEST_OBJ/V$module_name" 2>&1 > /dev/null

    if [ ! -f "$TEST_OBJ/V${module_name}.tree.json" ]; then
        echo -e "${RED}  ✗ FAILED: Verilator did not produce JSON${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    # Run decompiler
    local output_file="$TEST_OUTPUT/${module_name}_${backend}.sv"
    if $SV_UNIFIED file "$backend" "$TEST_OBJ/V${module_name}.tree.json" "$output_file" 2>&1 | grep -q "Error\|FAILED\|Exception"; then
        if [ "$should_pass" = "fail" ]; then
            echo -e "${GREEN}  ✓ PASSED: Expected failure detected${NC}"
            TESTS_PASSED=$((TESTS_PASSED + 1))
            return 0
        else
            echo -e "${RED}  ✗ FAILED: Unexpected error${NC}"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            return 1
        fi
    fi

    if [ ! -f "$output_file" ]; then
        echo -e "${RED}  ✗ FAILED: No output file generated${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    # Check output is valid Verilog
    if ! grep -q "module" "$output_file"; then
        echo -e "${RED}  ✗ FAILED: Output does not contain module${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    if [ "$should_pass" = "fail" ]; then
        echo -e "${RED}  ✗ FAILED: Should have failed but passed${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        return 1
    fi

    echo -e "${GREEN}  ✓ PASSED${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    return 0
}

# Run scan mode test
run_scan_test() {
    local test_name="$1"
    local backend="$2"
    local verify="$3"  # "verify" or ""

    TESTS_RUN=$((TESTS_RUN + 1))

    echo -e "${BLUE}Test $TESTS_RUN: $test_name [scan mode]${NC}"

    # Copy obj_dir for scan test (to parent directory)
    rm -rf ../obj_dir
    cp -r "$TEST_OBJ" ../obj_dir

    # Run scan
    local output_dir="$TEST_OUTPUT/scan_${backend}/"
    rm -rf "$output_dir"

    if [ "$verify" = "verify" ]; then
        if $SV_UNIFIED scan "$backend" "$output_dir" --verify 2>&1 | grep -q "Error\|FAILED"; then
            echo -e "${RED}  ✗ FAILED: Scan with verification failed${NC}"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            rm -rf ../obj_dir
            return 1
        fi
    else
        if $SV_UNIFIED scan "$backend" "$output_dir" 2>&1 | grep -q "Error\|FAILED"; then
            echo -e "${RED}  ✗ FAILED: Scan failed${NC}"
            TESTS_FAILED=$((TESTS_FAILED + 1))
            rm -rf ../obj_dir
            return 1
        fi
    fi

    # Check output directory
    if [ ! -d "$output_dir" ] || [ -z "$(ls -A $output_dir)" ]; then
        echo -e "${RED}  ✗ FAILED: No output generated${NC}"
        TESTS_FAILED=$((TESTS_FAILED + 1))
        rm -rf ../obj_dir
        return 1
    fi

    rm -rf ../obj_dir
    echo -e "${GREEN}  ✓ PASSED${NC}"
    TESTS_PASSED=$((TESTS_PASSED + 1))
    return 0
}

# Create test input files
create_test_files() {
    # Test 1: Simple counter
    cat > "$TEST_INPUT/counter.sv" << 'EOF'
module counter (
  input logic clk,
  input logic reset,
  output logic [7:0] count
);
  logic [7:0] count_reg;

  always_ff @(posedge clk) begin
    if (reset)
      count_reg <= 8'h0;
    else
      count_reg <= count_reg + 1;
  end

  assign count = count_reg;
endmodule
EOF

    # Test 2: Register pipeline
    cat > "$TEST_INPUT/pipeline.sv" << 'EOF'
module pipeline (
  input logic clk,
  input logic [31:0] data_in,
  output logic [31:0] data_out
);
  logic [31:0] stage1, stage2, stage3;

  always_ff @(posedge clk) begin
    stage1 <= data_in;
    stage2 <= stage1;
    stage3 <= stage2;
  end

  assign data_out = stage3;
endmodule
EOF

    # Test 3: Small memory (should expand)
    cat > "$TEST_INPUT/small_memory.sv" << 'EOF'
module small_memory (
  input logic clk,
  input logic [1:0] addr,
  input logic [7:0] wdata,
  input logic we,
  output logic [7:0] rdata
);
  logic [7:0] mem [0:3];  // 32 bits total

  always_ff @(posedge clk) begin
    if (we)
      mem[addr] <= wdata;
    rdata <= mem[addr];
  end
endmodule
EOF

    # Test 4: Large memory (should use primitive)
    cat > "$TEST_INPUT/large_memory.sv" << 'EOF'
module large_memory (
  input logic clk,
  input logic [4:0] addr,
  input logic [31:0] wdata,
  input logic we,
  output logic [31:0] rdata
);
  logic [31:0] mem [0:31];  // 1024 bits total

  always_ff @(posedge clk) begin
    if (we)
      mem[addr] <= wdata;
    rdata <= mem[addr];
  end
endmodule
EOF

    # Test 5: Combinational logic
    cat > "$TEST_INPUT/combinational.sv" << 'EOF'
module combinational (
  input logic [7:0] a,
  input logic [7:0] b,
  input logic sel,
  output logic [7:0] result
);
  assign result = sel ? a : b;
endmodule
EOF

    # Test 6: Mixed sequential and combinational
    cat > "$TEST_INPUT/mixed.sv" << 'EOF'
module mixed (
  input logic clk,
  input logic [7:0] a,
  input logic [7:0] b,
  output logic [7:0] sum,
  output logic [7:0] sum_reg
);
  assign sum = a + b;

  always_ff @(posedge clk) begin
    sum_reg <= a + b;
  end
endmodule
EOF

    # Test 7: 4-state values
    cat > "$TEST_INPUT/fourstate.sv" << 'EOF'
module fourstate (
  input logic clk,
  input logic reset,
  output logic [31:0] data
);
  logic [31:0] reg1;

  always_ff @(posedge clk) begin
    if (reset)
      reg1 <= 32'bxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx;
    else
      reg1 <= reg1 + 1;
  end

  assign data = reg1;
endmodule
EOF

    echo -e "${GREEN}✓ Test files created${NC}"
    echo ""
}

# Print summary
print_summary() {
    echo ""
    echo "========================================"
    echo "Test Summary"
    echo "========================================"
    echo "Total tests:  $TESTS_RUN"
    echo -e "${GREEN}Passed:       $TESTS_PASSED${NC}"
    if [ $TESTS_FAILED -gt 0 ]; then
        echo -e "${RED}Failed:       $TESTS_FAILED${NC}"
    else
        echo "Failed:       $TESTS_FAILED"
    fi
    echo "========================================"

    if [ $TESTS_FAILED -eq 0 ]; then
        echo -e "${GREEN}All tests passed!${NC}"
        return 0
    else
        echo -e "${RED}Some tests failed!${NC}"
        return 1
    fi
}

# Main test execution
main() {
    init_tests
    create_test_files

    echo -e "${BLUE}=== Backend Tests ===${NC}"
    echo ""

    # Test each backend with simple counter
    run_test "Counter - Standard backend" "standard" "$TEST_INPUT/counter.sv" "pass"
    run_test "Counter - Structural backend" "structural" "$TEST_INPUT/counter.sv" "pass"
    run_test "Counter - Yosys backend" "yosys" "$TEST_INPUT/counter.sv" "pass"
    run_test "Counter - HardCaml backend" "hardcaml" "$TEST_INPUT/counter.sv" "pass"

    echo ""
    echo -e "${BLUE}=== Register Inference Tests ===${NC}"
    echo ""

    run_test "Pipeline - Structural backend" "structural" "$TEST_INPUT/pipeline.sv" "pass"
    run_test "Mixed - Structural backend" "structural" "$TEST_INPUT/mixed.sv" "pass"

    echo ""
    echo -e "${BLUE}=== Memory Tests ===${NC}"
    echo ""

    run_test "Small memory - Structural" "structural" "$TEST_INPUT/small_memory.sv" "pass"
    run_test "Large memory - Structural" "structural" "$TEST_INPUT/large_memory.sv" "pass"

    echo ""
    echo -e "${BLUE}=== Combinational Tests ===${NC}"
    echo ""

    run_test "Combinational - Standard" "standard" "$TEST_INPUT/combinational.sv" "pass"
    run_test "Combinational - Yosys" "yosys" "$TEST_INPUT/combinational.sv" "pass"

    echo ""
    echo -e "${BLUE}=== 4-State Value Tests ===${NC}"
    echo ""

    run_test "4-state - HardCaml" "hardcaml" "$TEST_INPUT/fourstate.sv" "pass"
    run_test "4-state - Structural" "structural" "$TEST_INPUT/fourstate.sv" "pass"

    echo ""
    echo -e "${BLUE}=== Scan Mode Tests ===${NC}"
    echo ""

    run_scan_test "Scan all - Standard" "standard" ""
    run_scan_test "Scan all - Structural" "structural" ""
    run_scan_test "Scan all - HardCaml" "hardcaml" ""

    print_summary
}

# Run tests
main
exit $?
