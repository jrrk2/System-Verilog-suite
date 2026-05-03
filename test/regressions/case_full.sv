// Combinational `case (sel)` with a `default` arm that fully assigns
// the lhs — every code path drives `y_r`, so the must-assign
// analysis must see "case-with-default" as covering.
module case_full (
  input  logic [1:0] sel,
  input  logic [3:0] a,
  output logic [3:0] y
);
  reg [3:0] y_r;
  always @(*) begin
    case (sel)
      2'b00:  y_r = a;
      2'b01:  y_r = a + 1;
      2'b10:  y_r = a - 1;
      default: y_r = 0;
    endcase
  end
  assign y = y_r;
endmodule
