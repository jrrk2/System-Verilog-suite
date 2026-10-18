// The VM's conditional branches: a signed compare of imm against a value
// derived from accu decides whether pc takes `pc + $signed(offset) - 1`.
// Several opcodes write pc with the same adder (group sharing) and the
// compare results are mutually exclusive path conditions (AND-OR muxes).
module branch_pc_share (
    input  logic        clk, reset,
    input  logic [7:0]  opcode,
    input  logic [31:0] imm, offset,
    input  logic [35:0] accu,
    output logic [23:0] pc
);
  function automatic integer Int_val(input logic [35:0] v);
    Int_val = $signed(v[31:0]) >>> 1;
  endfunction
  always_ff @(posedge clk) begin
    if (reset) pc <= 0;
    else begin
      case (opcode)
        8'd84: pc <= pc + $signed(offset) - 1;
        8'd85: if (accu != 1) pc <= pc + $signed(offset) - 1;
        8'd86: if (accu == 1) pc <= pc + $signed(offset) - 1;
        8'd131: if ($signed(imm) == Int_val(accu)) pc <= pc + $signed(offset) - 1;
        8'd133: if ($signed(imm) <  Int_val(accu)) pc <= pc + $signed(offset) - 1;
        8'd134: if ($signed(imm) <= Int_val(accu)) pc <= pc + $signed(offset) - 1;
        8'd135: if ($signed(imm) >  Int_val(accu)) pc <= pc + $signed(offset) - 1;
        8'd136: if ($signed(imm) >= Int_val(accu)) pc <= pc + $signed(offset) - 1;
        8'd139: if (imm < Int_val(accu)) pc <= pc + $signed(offset) - 1;   // unsigned
        8'd1:   pc <= pc + 1;
        8'd2:   pc <= pc + 2;
        8'd3:   pc <= pc + imm[15:0];
        default: ;
      endcase
    end
  end
endmodule
