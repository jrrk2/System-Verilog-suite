// `typedef enum logic [N-1:0] {…} t;` — width N must propagate to
// any reg of type `t`. Two converters must agree:
//   - Verible: `extract_typedefs` walks `type_declaration1`, then
//     `width_of` falls back to typedef lookup when no packed dim is
//     present at the reg-decl site.
//   - Verilator: `EnumType` carries its base type (resolved from
//     refDTypep), and `get_width_from_dtype` recurses through it.
module enum_width (
  input  logic       clk,
  input  logic       sel,
  output logic [2:0] s
);
  typedef enum logic [2:0] { A, B, C, D, E } st_t;
  st_t cs;
  always @(posedge clk) cs <= sel ? B : A;
  assign s = cs;
endmodule
