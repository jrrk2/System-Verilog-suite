// Body-declared `parameter W = 4;` substituted into a literal
// expression. Tests `extract_body_params` for `any_param_declaration*`.
module param_body (
  input  logic [3:0] a,
  output logic [3:0] y
);
  parameter W = 3;
  assign y = a + W;
endmodule
