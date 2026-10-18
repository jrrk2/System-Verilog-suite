// A case arm whose only write is dead (scratch is never read) next to a
// non-empty default.  Dead-code elimination used to drop the emptied arm,
// so op==0 fell through to the default and y took b instead of holding.
module case_dead_arm (
    input  logic       clk,
    input  logic [1:0] op,
    input  logic [7:0] a, b,
    output logic [7:0] y
);
  logic [7:0] scratch;
  always_ff @(posedge clk) begin
    case (op)
      2'd0: scratch <= a;
      2'd1: y <= a;
      default: y <= b;
    endcase
  end
endmodule
