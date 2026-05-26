// Single-bit select from a 32-bit reg.  Lowering must preserve the
// LSB-index semantics (bit 0 = pc[0], NOT pc[31]).  Picorv32's
// MISALIGNED-INSTRUCTION trap fires on `reg_pc[0]`, and a mis-lowered
// pc[31:31] would trap whenever the upper address bits are set.
module op_bit_select (
    input  [31:0] pc,
    output        y0,
    output        y31
);
  assign y0  = pc[0];
  assign y31 = pc[31];
endmodule
