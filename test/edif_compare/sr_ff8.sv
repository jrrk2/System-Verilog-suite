// 8-bit set/reset flip-flop. Wider data exercises whether Verilator preserves
// the if/else-if structure (or a Cond chain) that the structural classifier
// can recognise. The 1-bit version (sr_ff.sv) gets fully boolean-folded
// because each output bit reduces to `preset | (~clear & d_bit)`.
module sr_ff8 (
  input  logic        clk,
  input  logic        preset,
  input  logic        clear,
  input  logic [7:0]  d,
  output logic [7:0]  q
);
  always @(posedge clk or posedge clear or posedge preset)
    if (preset)     q <= 8'hFF;
    else if (clear) q <= 8'h00;
    else            q <= d;
endmodule
