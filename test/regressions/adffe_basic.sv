// Async reset + clock enable — Yosys folds this into `$adffe`. The
// pending RTL_REG-with-CE work; before the fix this FF disappeared
// from Yosys-side BIR and signals like cva6's `iCounter` showed as
// zero-extended garbage in the miter counterexample.
module adffe_basic (
  input  logic       clk,
  input  logic       rst,
  input  logic       en,
  input  logic [3:0] d,
  output logic [3:0] q
);
  always @(posedge clk or posedge rst)
    if (rst)      q <= 0;
    else if (en)  q <= d;
endmodule
