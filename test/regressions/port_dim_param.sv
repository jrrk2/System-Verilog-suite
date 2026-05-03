// `output [WIDTH-1:0] q` — WIDTH is a parameter, not a port.
// extract_port_decl walks the port-decl token tree for SymbolIdentifiers;
// without skipping subtrees inside `decl_variable_dimension*`, the
// `WIDTH` reference inside `[WIDTH-1:0]` would be extracted as a port
// of the same direction as `q`.
module port_dim_param #(parameter WIDTH = 8) (
  input  logic [WIDTH-1:0] a,
  output logic [WIDTH-1:0] y
);
  assign y = a;
endmodule
