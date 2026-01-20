module large_memory (
  input logic clk,
  input logic [4:0] addr,
  input logic [31:0] wdata,
  input logic we,
  output logic [31:0] rdata
);
  logic [31:0] mem [0:31];  // 1024 bits total

  always_ff @(posedge clk) begin
    if (we)
      mem[addr] <= wdata;
    rdata <= mem[addr];
  end
endmodule
