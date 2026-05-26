// SV: `lhs(32) = slice(20) << 12` — RHS must be evaluated in LHS context.
module op_shl_widen (input [31:0] data, output reg [31:0] y);
  always @* y = data[31:12] << 12;
endmodule
