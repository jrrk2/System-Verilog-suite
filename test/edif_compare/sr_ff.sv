// Set/reset flip-flop: three-edge always block. Both preset and clear are
// async. Vivado elaborates this as a single RTL register with set+reset
// inputs. This exercises the structural classifier's N-edge handling.
module sr_ff (
  input  logic clk,
  input  logic preset,
  input  logic clear,
  input  logic d,
  output logic q
);
  always @(posedge clk or posedge clear or posedge preset)
    if (preset)     q <= 1'b1;
    else if (clear) q <= 1'b0;
    else            q <= d;
endmodule
