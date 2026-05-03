// `{a, b}` concatenation — Verible parses as `range_list_in_braces1`
// (TUPLE4 with LBRACE + open_range_list + RBRACE). Open_range_list
// is a TLIST; it's reverse source order, so we have to reverse on
// conversion to keep MSB→LSB ordering of BConcat.
module concat (
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [7:0] y
);
  assign y = {a, b};
endmodule
