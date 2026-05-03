// `if (en) q <= d;` inside a posedge-clk always block — Yosys folds
// this into a `$dffe` cell (clock-enable FF). The RTLIL converter
// must recognise it; otherwise the FF disappears from Yosys-side BIR.
module dffe_basic (
  input  logic       clk,
  input  logic       en,
  input  logic [3:0] d,
  output logic [3:0] q
);
  always @(posedge clk) if (en) q <= d;
endmodule
