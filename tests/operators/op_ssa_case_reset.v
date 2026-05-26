// Multi-arm case in always_ff with synchronous reset — picorv32 pattern
// at small scale.  After SSA each case arm pushes a new version of `r`;
// the BCase phi merges them with the entering version; the synchronous
// reset in the outer `if (!resetn)` sets `r` to 0.  The surviving FF
// after hardcaml's register-forwarding optimization must:
//   (1) reset to 0 in one clock (not N), and
//   (2) advance to 0x11 / 0x22 / 0x33 in the next clock based on sel.
module op_ssa_case_reset (
    input            clk,
    input            resetn,
    input      [1:0] sel,
    output reg [7:0] r
);
  always @(posedge clk) begin
    if (!resetn)
      r <= 8'h00;
    else
      case (sel)
        2'd0: r <= 8'h11;
        2'd1: r <= 8'h22;
        2'd2: r <= 8'h33;
        default: r <= 8'hff;
      endcase
  end
endmodule
