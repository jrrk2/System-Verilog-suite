// Test case for register file - should be mapped to memory module
// 32 registers × 8 bits = 256 bits (above typical flip-flop threshold)

module test_regfile (
  input logic clk,
  input logic rst,
  input logic [4:0] raddr1,
  input logic [4:0] raddr2,
  input logic [4:0] waddr,
  input logic [7:0] wdata,
  input logic we,
  output logic [7:0] rdata1,
  output logic [7:0] rdata2
);

  // Register file: 32 entries × 8 bits
  logic [7:0] mem [31:0];

  // Write port
  always_ff @(posedge clk) begin
    if (rst) begin
      mem <= '{default: '0};
    end else if (we) begin
      mem[waddr] <= wdata;
    end
  end

  // Read ports (combinational)
  assign rdata1 = mem[raddr1];
  assign rdata2 = mem[raddr2];

endmodule
