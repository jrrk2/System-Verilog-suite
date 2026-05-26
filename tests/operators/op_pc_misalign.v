// Picorv32 MISALIGNED-INSTRUCTION pattern.  The ternary's then-branch
// is `pc[0]` (single-bit, constant index 0); verible emits this as a
// 32-bit-index BSelect in this context, which the hardcaml lowering
// then mis-renders as `pc[31:31]`.  Outer OR-reduce in picorv32 hides
// the failure under normal traffic but at boot with pc=0x00100000
// the rogue MSB of (0x00100000) is 0 -> looks fine, except the
// trap-target mux upstream uses the (resetn & mem_do_rinst & cond)
// combination and a stale mux selector triggers fetch->trap.
module op_pc_misalign (
    input         cpr,            // COMPRESSED_ISA equivalent
    input  [31:0] pc,
    output        y
);
  assign y = cpr ? pc[0] : |pc[1:0];
endmodule
