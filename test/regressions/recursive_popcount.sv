// popcount-style recursive instantiation, simplified to use a plain
// integer parameter (no struct). Tests that the const-fn evaluator's
// scope-aware override resolution unrolls the self-instantiation
// chain through localparam-derived widths.
//
// Expected: with a working evaluator, Verible-side specialise_design
// produces popcount__W8, popcount__W4, popcount__W2 (the chain
// 8→4→2). Without it, only popcount__W8 appears.

module pop #(
    parameter int unsigned W = 8
) (
    input  logic [W-1:0] data_i,
    output logic [$clog2(W)+1-1:0] cnt_o
);
  localparam int unsigned PaddedW = 1 << $clog2(W);
  if (W == 1) begin : g_leaf
    assign cnt_o = data_i[0];
  end else if (W == 2) begin : g_two
    assign cnt_o = data_i[0] + data_i[1];
  end else begin : g_split
    logic [$clog2(PaddedW/2)+1-1:0] lo_cnt, hi_cnt;
    pop #(.W(PaddedW/2)) u_lo (
        .data_i(data_i[PaddedW/2-1:0]),
        .cnt_o (lo_cnt)
    );
    pop #(.W(PaddedW/2)) u_hi (
        .data_i({{(PaddedW-W){1'b0}}, data_i[W-1:PaddedW/2]}),
        .cnt_o (hi_cnt)
    );
    assign cnt_o = lo_cnt + hi_cnt;
  end
endmodule

module top (
    input  logic [7:0] din,
    output logic [3:0] cnt
);
  pop #(.W(8)) u (.data_i(din), .cnt_o(cnt));
endmodule
