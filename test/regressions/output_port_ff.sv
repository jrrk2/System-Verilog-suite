// Output-port flip-flop. The FF rip needs to expose the FF's input
// cone as a primary output (Q__D) AND keep the original output port
// (Q) driven, by inserting a fresh primary input Q__Q for the
// current state and a combinational Q = Q__Q pass-through.
module output_port_ff (
  input  logic       clk,
  input  logic [3:0] d,
  output logic [3:0] q
);
  always @(posedge clk) q <= d;
endmodule
