// 96-bit register with a wide initial value + wide add: guards the init-value
// and arithmetic constant paths through gate_map.
module wreg (input clk, input en, output reg [95:0] q);
  initial q = 96'hFEDCBA9876543210DEADBEEF;
  always @(posedge clk) if (en) q <= q ^ 96'h1122334455667788990011AA;
endmodule
