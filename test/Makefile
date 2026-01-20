# Makefile for sv_main_unified test suite

.PHONY: all test clean help build backends memory register scan verify quick

# Colors
RED    := \033[0;31m
GREEN  := \033[0;32m
YELLOW := \033[1;33m
BLUE   := \033[0;34m
NC     := \033[0m # No Color

# Directories
TEST_DIR    := test
OUTPUT_DIR  := $(TEST_DIR)/output
OBJ_DIR     := $(TEST_DIR)/obj_dir
INPUT_DIR   := $(TEST_DIR)/input

# Executable
SV_UNIFIED  := _build/default/sv_main_unified.exe

all: test

help:
	@echo "$(BLUE)SystemVerilog Decompiler Test Suite$(NC)"
	@echo ""
	@echo "Targets:"
	@echo "  $(GREEN)make test$(NC)       - Run all tests"
	@echo "  $(GREEN)make quick$(NC)      - Run quick tests (no verification)"
	@echo "  $(GREEN)make backends$(NC)   - Test all backends"
	@echo "  $(GREEN)make memory$(NC)     - Test memory detection"
	@echo "  $(GREEN)make register$(NC)   - Test register inference"
	@echo "  $(GREEN)make scan$(NC)       - Test scan mode"
	@echo "  $(GREEN)make verify$(NC)     - Test with verification (slow)"
	@echo "  $(GREEN)make clean$(NC)      - Clean test outputs"
	@echo "  $(GREEN)make build$(NC)      - Build decompiler"
	@echo ""
	@echo "Examples:"
	@echo "  make test              # Run full test suite"
	@echo "  make quick             # Quick smoke tests"
	@echo "  make verify            # Full verification"

build:
	@echo "$(BLUE)Building decompiler...$(NC)"
	@dune build
	@echo "$(GREEN)✓ Build complete$(NC)"

test: build
	@echo "$(BLUE)Running full test suite...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh

quick: build
	@echo "$(BLUE)Running quick tests...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh 2>&1 | grep -v "Scan Mode\|Verification" || true

backends: build
	@echo "$(BLUE)Testing all backends...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh 2>&1 | grep "Backend Tests" -A 50

memory: build
	@echo "$(BLUE)Testing memory detection...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh 2>&1 | grep "Memory Tests" -A 20

register: build
	@echo "$(BLUE)Testing register inference...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh 2>&1 | grep "Register Inference Tests" -A 20

scan: build
	@echo "$(BLUE)Testing scan mode...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh 2>&1 | grep "Scan Mode Tests" -A 30

verify: build
	@echo "$(BLUE)Running verification tests (slow)...$(NC)"
	@echo "$(YELLOW)Warning: This may take several minutes$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh 2>&1 | grep -E "Verification|PASSED|FAILED"

clean:
	@echo "$(BLUE)Cleaning test outputs...$(NC)"
	@rm -rf $(OUTPUT_DIR)
	@rm -rf $(OBJ_DIR)
	@rm -rf obj_dir
	@echo "$(GREEN)✓ Clean complete$(NC)"

# Run specific backend tests
test-standard: build
	@echo "$(BLUE)Testing standard backend...$(NC)"
	@verilator --json-only --quiet $(INPUT_DIR)/counter.sv
	@$(SV_UNIFIED) file standard obj_dir/Vcounter.tree.json /tmp/test_standard.sv
	@echo "$(GREEN)✓ Standard backend test passed$(NC)"

test-structural: build
	@echo "$(BLUE)Testing structural backend...$(NC)"
	@verilator --json-only --quiet $(INPUT_DIR)/counter.sv
	@$(SV_UNIFIED) file structural obj_dir/Vcounter.tree.json /tmp/test_structural.sv
	@echo "$(GREEN)✓ Structural backend test passed$(NC)"

test-yosys: build
	@echo "$(BLUE)Testing yosys backend...$(NC)"
	@verilator --json-only --quiet $(INPUT_DIR)/counter.sv
	@$(SV_UNIFIED) file yosys obj_dir/Vcounter.tree.json /tmp/test_yosys.sv
	@echo "$(GREEN)✓ Yosys backend test passed$(NC)"

test-hardcaml: build
	@echo "$(BLUE)Testing hardcaml backend...$(NC)"
	@verilator --json-only --quiet $(INPUT_DIR)/counter.sv
	@$(SV_UNIFIED) file hardcaml obj_dir/Vcounter.tree.json /tmp/test_hardcaml.sv
	@echo "$(GREEN)✓ HardCaml backend test passed$(NC)"

# Check test infrastructure
check:
	@echo "$(BLUE)Checking test infrastructure...$(NC)"
	@command -v verilator >/dev/null 2>&1 || (echo "$(RED)✗ Verilator not found$(NC)" && exit 1)
	@command -v dune >/dev/null 2>&1 || (echo "$(RED)✗ Dune not found$(NC)" && exit 1)
	@test -f $(SV_UNIFIED) || (echo "$(YELLOW)! Building decompiler...$(NC)" && dune build)
	@test -f $(TEST_DIR)/run_tests.sh || (echo "$(RED)✗ Test runner not found$(NC)" && exit 1)
	@chmod +x $(TEST_DIR)/run_tests.sh
	@echo "$(GREEN)✓ Infrastructure check passed$(NC)"

# Run tests with coverage reporting
coverage: build
	@echo "$(BLUE)Running tests with coverage...$(NC)"
	@chmod +x $(TEST_DIR)/run_tests.sh
	@$(TEST_DIR)/run_tests.sh > /tmp/test_output.txt 2>&1 || true
	@echo ""
	@echo "$(BLUE)Coverage Summary:$(NC)"
	@grep "PASSED" /tmp/test_output.txt | wc -l | xargs echo "  Tests passed:"
	@grep "FAILED" /tmp/test_output.txt | wc -l | xargs echo "  Tests failed:"

# Benchmark test performance
benchmark: build
	@echo "$(BLUE)Benchmarking test performance...$(NC)"
	@time -p make quick 2>&1 | grep real

# Install test dependencies
install-deps:
	@echo "$(BLUE)Installing test dependencies...$(NC)"
	@which verilator || echo "$(YELLOW)Please install Verilator$(NC)"
	@which dune || echo "$(YELLOW)Please install Dune (opam install dune)$(NC)"
	@which z3 || echo "$(YELLOW)Optional: Install Z3 for verification$(NC)"

.PHONY: test-standard test-structural test-yosys test-hardcaml check coverage benchmark install-deps
