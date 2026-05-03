// Blackbox stubs for Xilinx primitives directly instantiated by the
// apb_uart sources. Vivado knows these natively; we just need a port-
// compatible declaration so verilator --json-only can elaborate.
//
// Only the I/O signature matters; behavior is irrelevant for netlist
// comparison.
/* verilator lint_off DECLFILENAME */

module RAMB16_S9_S9 (
    output [7:0]  DOA,
    output        DOPA,
    output [7:0]  DOB,
    output        DOPB,
    input  [10:0] ADDRA,
    input  [10:0] ADDRB,
    input         CLKA,
    input         CLKB,
    input  [7:0]  DIA,
    input         DIPA,
    input  [7:0]  DIB,
    input         DIPB,
    input         ENA,
    input         ENB,
    input         SSRA,
    input         SSRB,
    input         WEA,
    input         WEB
);
    parameter INIT_A = 9'b0;
    parameter INIT_B = 9'b0;
    parameter SRVAL_A = 9'b0;
    parameter SRVAL_B = 9'b0;
    parameter WRITE_MODE_A = "WRITE_FIRST";
    parameter WRITE_MODE_B = "WRITE_FIRST";
    parameter SIM_COLLISION_CHECK = "ALL";
endmodule

/* verilator lint_on DECLFILENAME */
