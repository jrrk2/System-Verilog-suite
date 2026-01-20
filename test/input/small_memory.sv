module small_memory (
  input logic clk,
  input logic [1:0] addr,
  input logic [7:0] wdata,
  input logic we,
  output logic [7:0] rdata
);
  logic [7:0] mem [0:3];  // 32 bits total

  always_ff @(posedge clk) begin
    if (we)
      mem[addr] <= wdata;
    rdata <= mem[addr];
  end
endmodule
