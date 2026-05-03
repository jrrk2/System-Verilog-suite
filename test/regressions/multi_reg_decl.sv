// `reg [W-1:0] r1, r2;` — the second reg in a comma-separated decl
// is tagged `gate_instance_or_register_variable1` (no
// "non_anonymous_" prefix). Both vars must inherit the same width
// from the parent `instantiation_base`.
module multi_reg_decl (
  input  logic [3:0] a,
  output logic [3:0] x,
  output logic [3:0] y
);
  reg [3:0] r1, r2;
  always_comb begin
    r1 = a;
    r2 = a + 1;
  end
  assign x = r1;
  assign y = r2;
endmodule
