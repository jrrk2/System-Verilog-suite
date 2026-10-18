// `$signed(imm) < Int_val(accu)` -- both operands signed, so the compare is
// signed (the VM's BLTINT..BGEINT); a `>>>` of a $signed slice is signed too.
module signed_cmp_branch (
    input  logic        clk,
    input  logic [31:0] imm,
    input  logic [35:0] accu,
    output logic        lt, le, gt, ge, ult
);
  function automatic logic signed [31:0] Int_val(input logic [35:0] v);
    Int_val = $signed(v[31:0]) >>> 1;
  endfunction
  always_ff @(posedge clk) begin
    lt  <= $signed(imm) <  Int_val(accu);
    le  <= $signed(imm) <= Int_val(accu);
    gt  <= $signed(imm) >  Int_val(accu);
    ge  <= $signed(imm) >= Int_val(accu);
    ult <= imm < Int_val(accu);              // mixed: unsigned per the LRM
  end
endmodule
