// Behavioural models of Vivado RTL elaboration primitives.
// See README in this directory for the rationale.
/* verilator lint_off DECLFILENAME */
/* verilator lint_off UNUSED */
/* verilator lint_off WIDTHEXPAND */
/* verilator lint_off WIDTHTRUNC */

`define RTL_W 64

// ─── Combinational ──────────────────────────────────────────────────────

module RTL_INV (input [`RTL_W-1:0] I0, output [`RTL_W-1:0] O);
  assign O = ~I0;
endmodule

module RTL_AND (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                output [`RTL_W-1:0] O);
  assign O = I0 & I1;
endmodule

module RTL_OR (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
               output [`RTL_W-1:0] O);
  assign O = I0 | I1;
endmodule

module RTL_XOR (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                output [`RTL_W-1:0] O);
  assign O = I0 ^ I1;
endmodule

module RTL_ADD (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                output [`RTL_W-1:0] O);
  assign O = I0 + I1;
endmodule

module RTL_SUB (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                output [`RTL_W-1:0] O);
  assign O = I0 - I1;
endmodule

module RTL_MUL (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                output [`RTL_W-1:0] O);
  assign O = I0 * I1;
endmodule

module RTL_LSHIFT (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                   output [`RTL_W-1:0] O);
  assign O = I0 << I1;
endmodule

module RTL_RSHIFT (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                   output [`RTL_W-1:0] O);
  assign O = I0 >> I1;
endmodule

module RTL_EQ (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1, output O);
  assign O = (I0 == I1);
endmodule

module RTL_NEQ (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1, output O);
  assign O = (I0 != I1);
endmodule

module RTL_LT (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1, output O);
  assign O = (I0 < I1);
endmodule

module RTL_LEQ (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1, output O);
  assign O = (I0 <= I1);
endmodule

module RTL_GT (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1, output O);
  assign O = (I0 > I1);
endmodule

module RTL_GEQ (input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1, output O);
  assign O = (I0 >= I1);
endmodule

// 2:1 mux with select S, inputs I0 (S=0) and I1 (S=1).
module RTL_MUX (input S, input [`RTL_W-1:0] I0, input [`RTL_W-1:0] I1,
                output [`RTL_W-1:0] O);
  assign O = S ? I1 : I0;
endmodule

// ─── Registers ──────────────────────────────────────────────────────────
// RTL_REG: plain register, no reset.
module RTL_REG (input C, input [`RTL_W-1:0] D, output reg [`RTL_W-1:0] Q);
  always @(posedge C) Q <= D;
endmodule

// RTL_REG_SYNC: synchronous reset (active-high RST clears Q to 0).
module RTL_REG_SYNC (input C, input RST, input [`RTL_W-1:0] D,
                     output reg [`RTL_W-1:0] Q);
  always @(posedge C)
    if (RST) Q <= '0;
    else     Q <= D;
endmodule

// RTL_REG_ASYNC: asynchronous reset (active-high RST clears Q to 0).
module RTL_REG_ASYNC (input C, input RST, input [`RTL_W-1:0] D,
                      output reg [`RTL_W-1:0] Q);
  always @(posedge C or posedge RST)
    if (RST) Q <= '0;
    else     Q <= D;
endmodule

// RTL_REG_CE: plain register with clock-enable (no reset).  When CE is
// high the FF samples D; when CE is low the FF holds.  The variant exists
// so cell-mapped output can route Vivado-RTL's CE-bearing register cells
// through the converter without a special case.
module RTL_REG_CE (input C, input CE, input [`RTL_W-1:0] D,
                   output reg [`RTL_W-1:0] Q);
  always @(posedge C)
    if (CE) Q <= D;
endmodule

// RTL_REG_SYNC_CE: synchronous reset + clock-enable.  Reset wins over CE.
module RTL_REG_SYNC_CE (input C, input CE, input RST, input [`RTL_W-1:0] D,
                        output reg [`RTL_W-1:0] Q);
  always @(posedge C)
    if (RST)     Q <= '0;
    else if (CE) Q <= D;
endmodule

// RTL_REG_ASYNC_CE: asynchronous reset + clock-enable (e.g. Yosys
// `$adffe` lowers to this shape).  Reset still wins over CE.
module RTL_REG_ASYNC_CE (input C, input CE, input RST, input [`RTL_W-1:0] D,
                         output reg [`RTL_W-1:0] Q);
  always @(posedge C or posedge RST)
    if (RST)     Q <= '0;
    else if (CE) Q <= D;
endmodule

/* verilator lint_on DECLFILENAME */
