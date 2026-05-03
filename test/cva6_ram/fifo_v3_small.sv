// Concrete instance of cva6's fifo_v3 (pulp-platform common_cells).
//
// fifo_v3 is the standard FIFO used throughout cva6 — exercises register-
// file-style memory inference (the `mem_q` array becomes a small
// distributed RAM or a register array depending on geometry).
//
// 8 entries × 16 bits → likely register array on -rtl, possibly a
// shift-register chain or LUTRAM on full synthesis.
module fifo_v3_small (
  input  logic        clk_i,
  input  logic        rst_ni,
  input  logic        flush_i,
  input  logic        testmode_i,
  output logic        full_o,
  output logic        empty_o,
  output logic [2:0]  usage_o,
  input  logic [15:0] data_i,
  input  logic        push_i,
  output logic [15:0] data_o,
  input  logic        pop_i
);
  fifo_v3 #(
    .FALL_THROUGH (1'b0),
    .DATA_WIDTH   (16),
    .DEPTH        (8)
  ) i_fifo (
    .clk_i,
    .rst_ni,
    .flush_i,
    .testmode_i,
    .full_o,
    .empty_o,
    .usage_o,
    .data_i,
    .push_i,
    .data_o,
    .pop_i
  );
endmodule
