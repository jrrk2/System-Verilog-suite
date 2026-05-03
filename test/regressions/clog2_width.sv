// `[$clog2(N)-1:0]` in port width. Verible's lexer treats `$clog2`
// as a SymbolIdentifier (only `$test$plusargs` is whitelisted as
// SystemTFIdentifier), so it parses through `reference_or_call_base1`.
// eval_int detects the function name and computes ceil(log2(N)).
module clog2_width #(parameter N = 8) (
  input  logic [$clog2(N)-1:0] a,
  output logic [$clog2(N)-1:0] y
);
  assign y = a;
endmodule
