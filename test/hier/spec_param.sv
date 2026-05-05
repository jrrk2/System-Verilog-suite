// Parameterised child module instantiated at two different widths.
// After Verible's elaboration, the bprogram contains specialised
// siblings `popcount__IW2` (4-bit input) and `popcount__IW16`
// (16-bit input). The parent's binstances carry the BASE name
// `popcount` — Behavioral_hier.flatten_for_z3 needs to disambiguate
// via port-shape matching.

module popcount #(parameter int IW = 8) (
  input  [IW-1:0] data_i,
  output [$clog2(IW+1)-1:0] popcount_o
);
  localparam int OW = $clog2(IW+1);
  logic [OW-1:0] sum;
  always_comb begin
    sum = '0;
    for (int j = 0; j < IW; j = j + 1)
      sum = sum + {{(OW-1){1'b0}}, data_i[j]};
  end
  assign popcount_o = sum;
endmodule

module multi_pop (
  input  [3:0]  small_data,
  input  [15:0] big_data,
  output [2:0]  small_count,
  output [4:0]  big_count
);
  popcount #(.IW(4))  u_small (.data_i(small_data), .popcount_o(small_count));
  popcount #(.IW(16)) u_big   (.data_i(big_data),   .popcount_o(big_count));
endmodule
