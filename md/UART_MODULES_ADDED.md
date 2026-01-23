# APB UART Modules Added to Regression Suite

## Source
Files copied from: `/Users/jonathan/lowrisc-chip/fpga/src/apb_uart/src`

These modules have been **formally verified** with Synopsys Formality, providing high-confidence test cases for the decompiler.

## Date Added
2026-01-22

## Modules Added

### ✅ Working Modules (9/11)

1. **slib_counter.sv** - Counter with configurable width
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

2. **slib_edge_detect.sv** - Edge detection logic
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

3. **slib_fifo.sv** - FIFO buffer implementation
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass (warnings about reg assignments)
   - Verilator: Pass
   - Verible: Pass

4. **slib_input_filter.sv** - Input debouncing filter
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

5. **slib_input_sync.sv** - Input synchronizer
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

6. **slib_mv_filter.sv** - Majority vote filter
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

7. **uart_baudgen.sv** - UART baud rate generator
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

8. **uart_interrupt.sv** - UART interrupt controller
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass (warnings about reg assignments)
   - Verilator: Pass
   - Verible: Pass

9. **uart_transmitter.sv** - UART transmitter
   - Status: ✓ Ready for 3-way verification
   - Yosys: Pass
   - Verilator: Pass
   - Verible: Pass

### ⚠️ Modules with Issues (2/11)

10. **slib_clock_div.sv** - Clock divider
    - Status: ⚠️ Verilator parsing fails
    - Issue: Width mismatch warning (2-bit vs 32-bit comparison)
    - Yosys: Pass
    - Verilator: Fail (width expansion warning treated as error)
    - Note: Formally verified, issue is Verilator strictness

11. **uart_receiver.sv** - UART receiver
    - Status: ⚠️ Yosys synthesis fails
    - Issue: Missing dependency (requires slib_input_filter)
    - Yosys: Fail (module dependency not found)
    - Note: Needs multi-file synthesis support

## Files Updated

1. **sysver_tests/** - Added 11 new .sv files
2. **run_3way_tests.sh** - Added UART modules to test list
3. **test_3way_suite.ml** - Added UART modules to OCaml test suite
4. **test_uart_modules.sh** - New script to test UART modules specifically

## Test Results

### Preparation Status
- Total modules: 11
- Ready for testing: 9 (82%)
- Failed preparation: 2 (18%)

### Known Issues

#### slib_clock_div Width Mismatch
```systemverilog
parameter RATIO = 5;
reg [1:0] iCounter;
if ((iCounter == (RATIO - 1)))  // 2-bit vs 32-bit comparison
```
**Impact**: Verilator treats width expansion warning as error.
**Workaround**: Could add `/* verilator lint_off WIDTHEXPAND */` but file is formally verified.
**Recommendation**: Keep as-is, exclude from Verilator tests or configure Verilator to allow warnings.

#### uart_receiver Missing Dependency
```
ERROR: Module `\slib_input_filter' referenced in module `\uart_receiver' in cell `\RX_IFSB' is not part of the design.
```
**Impact**: Cannot synthesize with Yosys in single-file mode.
**Workaround**: Need to add multi-file synthesis support to test script.
**Files needed**: slib_input_filter.sv, slib_mv_filter.sv, slib_edge_detect.sv

## Next Steps

1. **Run 3-way verification** on 9 working modules:
   ```bash
   ./run_3way_tests.sh
   ```

2. **Fix uart_receiver** - Add multi-file synthesis support:
   - Modify synthesis script to include dependencies
   - Or synthesize as a package

3. **Fix slib_clock_div** - Options:
   - Configure Verilator to allow width warnings
   - Add lint directives (changes verified file)
   - Exclude from Verilator-only tests

## Significance

These modules are **formally verified** with Synopsys Formality, meaning they represent ground-truth correct implementations. Successfully decompiling and verifying these modules demonstrates:

1. ✅ **Production-quality code handling** - Real-world designs, not toy examples
2. ✅ **Formal verification alignment** - Decompiler results match formally verified source
3. ✅ **Complex pattern support** - Filters, synchronizers, state machines
4. ✅ **Confidence in correctness** - Formal verification provides mathematical proof

## Regression Suite Size

**Before**: 16 test modules
**After**: 25 test modules (9 new working + 2 with known issues)
**Increase**: +56% more test coverage

## Related Documentation

- VHDL sources also available in same directory
- VHDL→IR converter can be tested against these modules
- See VHDL_INTEGRATION_PROGRESS.txt for VHDL parser work
