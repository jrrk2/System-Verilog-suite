// The loop shape the behavioural-Verilog writer emits for every for-loop
// (`init; while (cond) begin body; step end`, step written `k + i`, bound
// written `N > i`), preceded by another statement so the init is not the
// first statement of the block: the unroller must find the pair anywhere in
// a statement list, and verible must read `while` at all.
module while_loop_reread(input logic [15:0] a, output logic [15:0] y, output logic [4:0] cnt);
  logic [31:0] i;
  always_comb begin
    y = 16'd0;
    cnt = 5'd0;
    i = 32'd0;
    while ((32'd16 > i)) begin
      y[i[3:0] +: 1] = a[15 - i[3:0]];
      cnt = cnt + {4'd0, a[i[3:0]]};
      i = (32'd1 + i);
    end
  end
endmodule
