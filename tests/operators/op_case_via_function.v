// Distilled from random_sv_gen ssa_stress seed=8 after
// yosys/proc/opt/flatten/opt - yosys lowers a multi-arm `case (sel)`
// inside an always_ff into a synthesisable function-of-case form:
//
//   function [3:0] mux4;
//     input [3:0] a; input [7:0] b; input [1:0] s;
//     case (s)
//       2'b01: mux4 = b[3:0];
//       2'b10: mux4 = b[7:4];
//       2'b00: mux4 = a;
//       default: mux4 = 4'bx;
//     endcase
//   endfunction
//
// Verible's BIR-after-inline produces a nested-BCond ITE that the
// Z3 miter evaluates differently from the source semantics —
// Design1=0x2 where 0x9 is correct for sel=0/a=9/b=2.  Held here as
// a permanent regression target for the Z3 miter's BConcat+BSlice +
// BCond-chain encoding.
module op_case_via_function (
    input  wire        clk,
    input  wire        rst_n,
    input  wire [1:0]  sel,
    input  wire [3:0]  a,
    input  wire [3:0]  b,
    output reg  [3:0]  y
);
  wire        n_00_ = ~b[1];
  wire [3:0]  n_04_ = a ^ b;
  wire        n_02_ = sel == 2'b01;
  wire        n_03_ = !sel;
  wire [7:0]  rhs = { a, b[3:2], n_00_, b[0] };
  reg  [3:0]  n_01_;
  always @* begin
    case ({n_03_, n_02_})
      2'b01: n_01_ = rhs[3:0];
      2'b10: n_01_ = rhs[7:4];
      2'b00: n_01_ = n_04_;
      default: n_01_ = 4'd0;
    endcase
  end
  always @(posedge clk)
    if (!rst_n) y <= 4'd0;
    else       y <= n_01_;
endmodule
