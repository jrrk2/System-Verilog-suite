// Behavioural reference for spec_param.sv — both popcount calls
// inlined as a comb sum. The hier miter must reach the same answer
// after picking the right specialised popcount__IW2 / popcount__IW16
// for each instance.
module multi_pop (
  input  [3:0]  small_data,
  input  [15:0] big_data,
  output [2:0]  small_count,
  output [4:0]  big_count
);
  logic [2:0] s4;
  logic [4:0] s16;
  always_comb begin
    s4 = '0;
    for (int j = 0; j < 4; j = j + 1)
      s4 = s4 + {2'b0, small_data[j]};
  end
  always_comb begin
    s16 = '0;
    for (int j = 0; j < 16; j = j + 1)
      s16 = s16 + {4'b0, big_data[j]};
  end
  assign small_count = s4;
  assign big_count   = s16;
endmodule
