// Blocking-vs-nonblocking semantics test.
//
// `tmp = a + b` is a blocking assignment. By the user's principle, all
// assignments inside an always block are candidate flip-flops; the only
// difference is read-resolution. After `tmp = a+b`, reads of `tmp` in the
// same block see the just-computed (a+b), not a delayed FF output.
//
// Since `tmp` is read only once (in `q <= tmp`) and the read is fully
// substituted away, `tmp` is a pure alias and DCE should drop its FF.
//
// Expected after substitution + DCE:
//   q's FF has D = (a+b)        — one register cell for q
//   one adder for a+b
//   no register for tmp         — purely an alias
//
// The textbook (incorrect) approach would either give q a 2-cycle delay
// (no substitution, q's D = tmp_FF.Q) or just decline to register tmp at
// all even when externally observed. We do neither.
module blocking (
  input  logic       clk,
  input  logic [3:0] a,
  input  logic [3:0] b,
  output logic [3:0] q
);
  logic [3:0] tmp;
  always @(posedge clk) begin
    tmp = a + b;
    q <= tmp;
  end
endmodule
