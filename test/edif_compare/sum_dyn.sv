module sum_dyn(input logic [3:0] mask, output logic [2:0] cnt);
  always_comb begin
    cnt = 0;
    for (int i = 0; i < 4; i++) cnt = cnt + mask[i];
  end
endmodule
