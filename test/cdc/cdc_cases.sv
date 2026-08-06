// Negative and positive controls for the clock-domain boundary checker.
//
// The exemplar (ethsoc/eth_macro.sv) can only show that the checker stays
// QUIET on a good boundary.  Quiet is also what a checker that looks at
// nothing does -- and that is exactly how the first version of this pass
// passed its calibration.  These four cuts pin down the other half: each one
// is a boundary the checker must REJECT, or a boundary it must accept for a
// stated reason, one construct at a time.

// ---------------------------------------------------------------------------
// 1. bad: a same-clock FF->FF arc straight across the interface, both ways.
//    The minimum the checker must catch.
// ---------------------------------------------------------------------------
module m_bad (input wire clk, input wire d, output wire q);
    reg r;
    always @(posedge clk) r <= d;
    assign q = r;
endmodule

module t_bad (input wire clk, input wire x, output wire y);
    reg a, b;
    wire qi;
    always @(posedge clk) a <= x;
    m_bad u (.clk(clk), .d(a), .q(qi));
    always @(posedge clk) b <= qi;
    assign y = b;
endmodule

// ---------------------------------------------------------------------------
// 2. good: the same shape, but the macro is in a different clock domain and
//    takes the signal through a two-flop synchroniser.  A genuine CDC, which
//    the checker must NOT flag -- otherwise it rejects every real cut.
// ---------------------------------------------------------------------------
module m_good (input wire clk_b, input wire d, output wire q);
    reg s1, s2;
    always @(posedge clk_b) begin
        s1 <= d;
        s2 <= s1;
    end
    assign q = s2;
endmodule

module t_good (input wire clk_a, input wire clk_b, input wire x, output wire y);
    reg a, c;
    wire qi;
    always @(posedge clk_a) a <= x;
    m_good u (.clk_b(clk_b), .d(a), .q(qi));
    always @(posedge clk_a) c <= qi;
    assign y = c;
endmodule

// ---------------------------------------------------------------------------
// 3. thru: no register inside the macro at all -- a combinational feed-through
//    with the launching AND capturing flops both outside.  The arc is still a
//    same-clock FF->FF path that nobody times, and an endpoint-only analysis
//    ("is this port driven by a register in this module") sees two ports with
//    no register on either end and says fine.
// ---------------------------------------------------------------------------
module m_thru (input wire a, output wire b);
    assign b = ~a;
endmodule

module t_thru (input wire clk, input wire x, output wire y);
    reg p, q;
    wire w;
    always @(posedge clk) p <= x;
    m_thru u (.a(p), .b(w));
    always @(posedge clk) q <= w;
    assign y = q;
endmodule

// ---------------------------------------------------------------------------
// 4. deep: the offending register is not in the macro but in a CHILD of the
//    macro -- which is where every register in eth_macro actually lives.  This
//    is the case the process-local version of the checker could not see.
// ---------------------------------------------------------------------------
module leaf (input wire clk, input wire d, output wire q);
    reg r;
    always @(posedge clk) r <= d;
    assign q = r;
endmodule

module m_deep (input wire clk, input wire d, output wire q);
    leaf l (.clk(clk), .d(d), .q(q));
endmodule

module t_deep (input wire clk, input wire x, output wire y);
    reg a, b;
    wire qi;
    always @(posedge clk) a <= x;
    m_deep u (.clk(clk), .d(a), .q(qi));
    always @(posedge clk) b <= qi;
    assign y = b;
endmodule
