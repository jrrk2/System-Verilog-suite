// `always @(level signals)` should be classified combinational, not
// sequential. Both Verilator and Verible converters must agree:
//   - Verilator's `is_edge_triggered` only true for posedge/negedge
//     (was true for any non-empty edge_str like LEVEL/ANYEDGE).
//   - Verible's `extract_always` looks for Posedge/Negedge tokens,
//     and the must-assign analysis confirms there's no latch.
module level_always_comb (
  input  logic [3:0] a,
  input  logic       s,
  output logic [3:0] y
);
  reg [3:0] y_r;
  always @(a or s) y_r = s ? a : 4'b0;
  assign y = y_r;
endmodule
