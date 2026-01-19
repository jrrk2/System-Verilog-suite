# Makefile for SystemVerilog Decompiler

.PHONY: all build clean test install help unified legacy

# Default target
all: unified

# Build unified interface (recommended)
unified:
	@echo "Building unified decompiler..."
	dune build sv_main_unified.exe
	@echo "Built: _build/default/sv_main_unified.exe"
	@echo "Usage: _build/default/sv_main_unified.exe scan <backend> <output_dir>"

# Build legacy interfaces
legacy:
	@echo "Building legacy executables..."
	dune build sv_main.exe
	@echo "Built: _build/default/sv_main.exe"

# Build everything
build:
	@echo "Building all executables..."
	dune build @all
	@ls -lh _build/default/*.exe 2>/dev/null || echo "No executables built"

# Clean build artifacts
clean:
	@echo "Cleaning build artifacts..."
	dune clean
	rm -rf test_output_*
	@echo "Clean complete"

# Run tests
test: unified
	@echo "Running tests..."
	./test_unified.sh

# Install to system (requires opam)
install:
	@echo "Installing to opam environment..."
	dune install

# Display help
help:
	@echo "SystemVerilog Decompiler - Makefile"
	@echo "===================================="
	@echo ""
	@echo "Targets:"
	@echo "  all (default) - Build unified interface (recommended)"
	@echo "  unified       - Build sv_main_unified.exe"
	@echo "  legacy        - Build original sv_main.exe"
	@echo "  build         - Build all executables"
	@echo "  clean         - Remove build artifacts"
	@echo "  test          - Run test suite"
	@echo "  install       - Install to opam"
	@echo "  help          - Show this help"
	@echo ""
	@echo "Backends:"
	@echo "  standard      - Original SystemVerilog output"
	@echo "  structural    - Structural with primitives"
	@echo "  yosys         - Yosys-compatible output"
	@echo "  hardcaml      - HardCaml OCaml output (NEW!)"
	@echo ""
	@echo "Examples:"
	@echo "  make unified"
	@echo "  ./_build/default/sv_main_unified.exe scan yosys results/"
	@echo "  ./_build/default/sv_main_unified.exe file hardcaml input.json output.ml"
	@echo ""
	@echo "For more information, see README_UNIFIED.md"

# Quick build and run with hardcaml backend
quicktest-hardcaml: unified
	@echo "Quick test with HardCaml backend..."
	@if [ -d "obj_dir" ]; then \
		mkdir -p test_hardcaml_output; \
		./_build/default/sv_main_unified.exe scan hardcaml test_hardcaml_output/; \
		echo "Output in test_hardcaml_output/"; \
	else \
		echo "No obj_dir/ found. Place Verilator JSON files there first."; \
	fi

# Quick build and run with yosys backend
quicktest-yosys: unified
	@echo "Quick test with Yosys backend..."
	@if [ -d "obj_dir" ]; then \
		mkdir -p test_yosys_output; \
		./_build/default/sv_main_unified.exe scan yosys test_yosys_output/; \
		echo "Output in test_yosys_output/"; \
	else \
		echo "No obj_dir/ found. Place Verilator JSON files there first."; \
	fi
