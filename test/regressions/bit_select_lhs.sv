// LHS bit-select inside a sequential always block. lhs_name_of must
// return the underlying signal name (`iD`), not the index identifier.
// This was the slib_input_sync shape — without correct LHS extraction
// the BAssign target was wrong and the FF rip exposed the wrong Q.
module bit_select_lhs (
  input  logic       clk,
  input  logic       d,
  output logic [1:0] q
);
  reg [1:0] iD;
  always @(posedge clk) begin
    iD[0] <= d;
    iD[1] <= iD[0];
  end
  assign q = iD;
endmodule
