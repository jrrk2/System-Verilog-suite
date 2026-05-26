// Slice-write chain — distilled picorv32 pcpi_mul carry-save shape.
// Each statement writes a 2-bit slice of `r` and reads the *same*
// slice via direct slice-expression syntax (not `r >> j` — that path
// trips the SV operator-context-width gap, task #24, which is a
// separate seed).  After lowering, the BIR has four @slice_write
// calls on `r` that each read their own slice via BSlice; without
// SSA versioning, the merged BAssign would close a self-reference
// loop through Always.Variable.value r — yosys-scc reported 16 SCCs
// on picosoc before the fix; this seed exercises the same pattern
// at small scale.
module op_unrolled_slice_chain (
    input  [7:0] init,
    output [7:0] out
);
  reg [7:0] r;
  always @* begin
    r       = init;
    r[1:0]  = r[1:0] ^ 2'b11;
    r[3:2]  = r[3:2] ^ 2'b11;
    r[5:4]  = r[5:4] ^ 2'b11;
    r[7:6]  = r[7:6] ^ 2'b11;
  end
  assign out = r;
endmodule
