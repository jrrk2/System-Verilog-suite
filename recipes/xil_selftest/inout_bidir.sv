// Bidirectional (inout) pin handling: the inout `io` must become a PRIMARY I/O
// (input) — a linked variable in the miter (constrained equal across designs,
// never in the output cone) — not a lost \`Internal signal.  The tristate driver
// and the read both resolve; behavioral == gate-mapped.
module inout_bidir (input clk, input oe, input din, inout io, output dout);
  assign io = oe ? din : 1'bz;
  assign dout = io;
endmodule
