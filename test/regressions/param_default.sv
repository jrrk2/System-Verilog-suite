// Header `#(parameter W=4)` default substituted into port widths.
// Without the body-params extraction in verible_to_behavioral, port
// `[W-1:0]` falls back to width 1 and disagrees with Verilator.
module param_default #(parameter W = 4) (
  input  logic [W-1:0] a,
  output logic [W-1:0] y
);
  assign y = a;
endmodule
