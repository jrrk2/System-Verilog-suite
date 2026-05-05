// Per-signal `(* sv_decomp_adder = "..." *)` annotations drive each
// operator's analytical depth in the timing report. Total arrival
// is the sum of per-stage depths along the path.
module mixed (
  input  [7:0] a, input [7:0] b, input [7:0] c, input [7:0] d,
  output [7:0] s
);
  (* sv_decomp_adder = "ripple"      *) wire [7:0] t1;
  (* sv_decomp_adder = "brent_kung"  *) wire [7:0] t2;
  (* sv_decomp_adder = "kogge_stone" *) wire [7:0] s_int;
  assign t1    = a + b;
  assign t2    = t1 + c;
  assign s_int = t2 + d;
  assign s     = s_int;
endmodule
