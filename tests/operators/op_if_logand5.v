// SV: `if (cw && rd) x <= val;` — mimics picorv32's cpuregs writeback gate
// where cw is 1-bit and rd is 5-bit.  Logical && must reduce rd to
// non-zero, so a write fires for ANY rd!=0 including rd=5'b01010.
module op_if_logand5 (input clk, input cw, input [4:0] rd,
                     input [31:0] val, output reg [31:0] x);
  always @(posedge clk) begin
    if (cw && rd) x <= val;
  end
endmodule
