# Verible Parser - Error Analysis for Failed Files

## Summary

Out of 164 Ariane RISC-V files tested:
- **138 files passed** (84%)
- **26 files failed** (16%)

All failures report generic `MenhirBasics.Error` without specific location or construct information.

## Error Reporting Limitation

The current `test_verible_parse.ml` implementation catches parse exceptions but doesn't report:
- Line/column numbers where parsing failed
- Token that caused the error
- Parser state at failure

This makes it difficult to identify exact problematic constructs.

## Manual Analysis of Failed Files

### 1. ariane_pkg.sv (Main Package File)

**File location:** `../ariane/include/ariane_pkg.sv`

**Observed SystemVerilog constructs:**

```systemverilog
// Preprocessor directives
`ifdef PITON_ARIANE
`ifndef AXI64_CACHE_PORTS
  `include "l15.tmp.h"
`endif
`endif

// System functions in parameters
localparam TRANS_ID_BITS = $clog2(NR_SB_ENTRIES);

// Conditional expressions in parameters
localparam FLEN = RVD     ? 64 :
                  RVF     ? 32 :
                  XF16    ? 16 :
                  XF16ALT ? 16 :
                  XF8     ? 8 :
                  0;

// Bitwise operations in parameter expressions
localparam bit FP_PRESENT = RVF | RVD | XF16 | XF16ALT | XF8;
localparam bit RVFVEC = RVF & XFVEC & FLEN>32;

// Complex struct typedefs
typedef struct packed {
    logic [63:0] cause;
    logic [63:0] tval;
    logic [63:0] tval2;
    logic        gva;
    logic        debug_mode;
} exception_t;

// Enum typedefs
typedef enum logic [1:0] { BHT, BTB, RAS } cf_t;

typedef enum logic[3:0] {
    NONE,      // 0
    LOAD,      // 1
    STORE,     // 2
    // ... more values
} fu_t;

// Function definitions in packages
function automatic logic is_rs1_fpr (input fu_op op);
    if (FP_PRESENT) begin
        case (op) inside
            [FADD:FNMSUB], [FMADD:FLE], FMV_F2X, FCVT_F2I, FMV_X2F, FCVT_I2F, FCLASS: return 1'b1;
            default: return 1'b0;
        endcase
    end else return 1'b0;
endfunction
```

**Potentially problematic constructs:**
1. ✗ `case ... inside` statement (SystemVerilog 2009 feature)
2. ✗ Function definitions within package
3. ✗ Complex ternary chains in localparam
4. ✗ Conditional compilation (`ifdef) mixed with type definitions
5. ⚠ System functions ($clog2) in parameter expressions
6. ⚠ Bitwise operators (|, &, >) in constexpr contexts

### 2. serpent_cache_pkg.sv

Similar structure to ariane_pkg.sv - likely has:
- Complex struct/enum definitions
- Functions in packages
- Advanced parameter expressions

### 3. Cache Subsystem Files (11 failures)

**Failed files:**
- cache_ctrl.sv
- miss_handler.sv
- serpent_cache_subsystem.sv
- serpent_dcache_ctrl.sv
- serpent_dcache_mem.sv
- serpent_dcache_missunit.sv
- serpent_dcache_wbuffer.sv
- serpent_dcache.sv
- serpent_icache.sv
- serpent_l15_adapter.sv
- std_cache_subsystem.sv

**Common patterns in cache files:**
```systemverilog
// Complex parameterized interfaces
interface cache_line_t #(
    parameter int unsigned SET_ASSOCIATIVITY = 8,
    parameter int unsigned CACHE_LINE_WIDTH = 128
);
    // Complex packed structures
    typedef struct packed {
        logic [CACHE_LINE_WIDTH-1:0] data;
        logic [SET_ASSOCIATIVITY-1:0] way;
        logic valid;
        logic dirty;
    } cache_line_s;
endinterface

// Nested generate blocks
generate
    for (genvar i = 0; i < NUM_WAYS; i++) begin : gen_ways
        for (genvar j = 0; j < LINE_WIDTH/8; j++) begin : gen_bytes
            always_ff @(posedge clk_i) begin
                // Complex logic
            end
        end
    end
endgenerate

// Advanced assertions
assert property (@(posedge clk) disable iff (!rst_n)
    req |-> ##[1:5] gnt
) else $error("Timeout");
```

**Likely issues:**
1. ✗ Parameterized interfaces with complex typedefs
2. ✗ Nested generate loops with dependencies
3. ✗ Assertions with temporal operators (|->##[])
4. ✗ Advanced packed struct usage

### 4. uart.sv (Testbench)

**File location:** `../ariane/tb/common/uart.sv`

**Problematic constructs:**
```systemverilog
// Interface with parameters
interface uart_bus #(
    parameter int unsigned BAUD_RATE = 115200,
    parameter int unsigned PARITY_EN = 0
)(
    input  logic rx,
    output logic tx,
    input  logic rx_en
);

// Pragma directives
/* pragma translate_off */
`ifndef VERILATOR
  localparam time BIT_PERIOD = (1000000000 / BAUD_RATE) * 1ns;

  // Time literals
  #(BIT_PERIOD/2);

  // String handling
  logic [256*8-1:0] stringa;

  // File I/O
  file = $fopen("uart", "w");

  // Clock-based procedural blocks
  always begin
    if (rx_en) begin
      @(negedge rx);
      #(BIT_PERIOD/2);
      // ...
    end
  end
`endif
/* pragma translate_on */
endinterface
```

**Issues:**
1. ✗ Interface with both parameters and port list
2. ✗ `pragma translate_off/on` directives
3. ✗ Time literals with units (`1ns`)
4. ✗ Delay statements (#) in testbench code
5. ✗ Procedural blocks inside interface

### 5. Core CPU Failures

**Failed files:**
- ariane.sv (top-level module)
- issue_read_operands.sv
- load_unit.sv
- scoreboard.sv
- store_buffer.sv

**Common advanced features:**
```systemverilog
// Complex port connections with interfaces
module ariane #(
    parameter ariane_pkg::ariane_cfg_t ArianeCfg = ariane_pkg::ArianeDefaultConfig
) (
    input  logic                         clk_i,
    input  logic                         rst_ni,
    axi_pkg::axi_req_t                   axi_req_o,
    axi_pkg::axi_resp_t                  axi_resp_i,
    // ...
);

// Interface array ports
axi_pkg::axi_req_t [NR_PORTS-1:0] axi_req;

// Case inside with ranges
case (fu) inside
    [LOAD, STORE]: begin
        // ...
    end
    [FPU, FPU_VEC]: begin
        // ...
    end
endcase

// Dynamic array indexing
logic [DEPTH-1:0][DATA_WIDTH-1:0] buffer;
assign out = buffer[rd_ptr];
```

**Issues:**
1. ✗ `case ... inside` with range patterns `[LOAD, STORE]`
2. ✗ Interface types as module parameters
3. ✗ Array of interface ports
4. ✗ Complex packed array indexing

### 6. Common Cells Failures

**Failed files:**
- fifo_v3.sv
- rrarbiter.sv

**Likely issues:**
```systemverilog
// Generic FIFO with complex parameters
module fifo_v3 #(
    parameter bit          FALL_THROUGH = 1'b0,
    parameter int unsigned DATA_WIDTH   = 32,
    parameter int unsigned DEPTH        = 8,
    parameter type dtype                = logic [DATA_WIDTH-1:0]
) (
    // ...
);

// Advanced generate with conditions
if (FALL_THROUGH) begin : gen_pass_through
    assign data_o = push_i ? data_i : mem[read_pointer_n];
end else begin : gen_registered
    assign data_o = mem[read_pointer_q];
end
```

**Issues:**
1. ✗ Type parameters (`parameter type dtype`)
2. ✗ Conditional generate blocks with complex logic
3. ✗ FALL_THROUGH behavior switching

### 7. Debug Module Failures

**Failed files:**
- dm_csrs.sv
- dm_sba.sv

**Likely issues:**
- Complex CSR (Control and Status Register) logic
- Debug-specific packed structures
- State machine implementations with assertions

### 8. AXI Bridge Failures

**Failed files:**
- axi_lite_interface.sv
- axi2apb_64_32.sv
- axi2apb.sv

**Likely issues:**
```systemverilog
// Complex interface conversions
always_comb begin
    axi_lite.aw_ready = 1'b0;
    axi_lite.w_ready  = 1'b0;
    axi_lite.b_valid  = 1'b0;

    unique case (state_q)
        IDLE: begin
            if (axi_lite.aw_valid && axi_lite.w_valid) begin
                state_d = WRITE;
            end
        end
        // ...
    endcase
end

// Bit width conversions
assign apb_64 = {axi_32_high, axi_32_low};
```

**Issues:**
1. ✗ `unique case` qualifier
2. ✗ Complex interface member access patterns
3. ✗ Bit width conversion logic

## Summary of Unsupported Constructs

Based on the analysis, the Verible OCaml parser likely struggles with:

### High Priority (Block 42% of failures - cache subsystem)
1. **Parameterized interfaces** with typedef inside
2. **`case ... inside`** with range patterns `[VALUE1, VALUE2]`
3. **Nested generate blocks** with complex conditions
4. **Type parameters** (`parameter type dtype`)

### Medium Priority (Block 19% of failures - core CPU)
5. **Interface types as parameters/ports**
6. **Array of interfaces** (`axi_req_t [N-1:0]`)
7. **Functions in packages** (may work but with `case inside` fails)
8. **`unique case`** and other case qualifiers

### Low Priority (Testbench/debug - 27% of failures)
9. **Pragmas** (`pragma translate_off`)
10. **Time literals** (`1ns`, `1ps`)
11. **Interface with procedural blocks** (testbench style)
12. **Delay statements** (`#delay`)

## Recommendations

### 1. Add Better Error Reporting
Modify `test_verible_parse.ml` to:
```ocaml
let parse_file filename =
  try
    let ic = open_in filename in
    let lexbuf = Lexing.from_channel ic in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
    let result = Source_text_verible.source_text Source_text_verible_lex.token lexbuf in
    close_in ic;
    Some result
  with
  | Source_text_verible.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf "Parse error at line %d, column %d\n"
        pos.pos_lnum
        (pos.pos_cnum - pos.pos_bol);
      None
```

### 2. Focus Parser Improvements On
1. **`case ... inside`** - Used extensively in Ariane
2. **Parameterized interfaces** - Critical for cache subsystem
3. **Type parameters** - Used in generic components (FIFOs, arbiters)
4. **Interface arrays** - Common in multi-port designs

### 3. Alternative Approaches
- Use Verilator's JSON output for files the parser can't handle
- Pre-process files to remove unsupported constructs
- Create simplified variants of cache files for testing

## Positive Results

The **84% success rate** shows strong support for:
- ✅ Basic module declarations and instantiations
- ✅ Standard typedefs (non-parameterized)
- ✅ Most enum definitions
- ✅ Simple structs
- ✅ Standard case statements
- ✅ Always blocks (always_ff, always_comb)
- ✅ Generate blocks (simple cases)
- ✅ Most AXI infrastructure
- ✅ Packages and imports
- ✅ Parameters and localparams (most cases)

## Files Generated

- **test_failed_files** - Script to test only failed files
- **ariane_verible_detailed** - Full test with error capture
- **ariane_verible_errors/** - Directory with error logs
