module a #(parameter int W = 64) (
  input logic clk_i, rst_ni, flush_i,
  input logic [2:0] rr_i,
  input logic [4:0] req_i, input logic [4:0][W-1:0] data_i,
  output logic [4:0] gnt_o, output logic req_o, input logic gnt_i,
  output logic [W-1:0] data_o, output logic [2:0] idx_o);
  rr_arb_tree #(.NumIn(5), .DataType(logic [W-1:0]), .AxiVldRdy(1'b1), .LockIn(1'b1)) u (
    .clk_i, .rst_ni, .flush_i, .rr_i, .req_i, .gnt_o, .data_i, .req_o, .gnt_i, .data_o, .idx_o);
endmodule
