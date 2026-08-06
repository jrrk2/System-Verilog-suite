// Designs for the per-clock-domain flattener.
//
// Small enough to check by eye, and each one exercises a decision the split
// has to make rather than just "does it run".

// ---------------------------------------------------------------------------
// s_two: two domains joined the way a correct design joins them -- a two-flop
// synchroniser.  The split must produce exactly two modules, and the checker
// must then prove the boundary it just drew.
//
// `toggle` is combinational and feeds only clk_b registers, so it belongs to
// the clk_b module even though every one of its inputs comes from clk_a: logic
// is timed by the flop it drives, not by the flop that drove it.
// ---------------------------------------------------------------------------
module s_two (
    input  wire       clk_a,
    input  wire       clk_b,
    input  wire       rst,
    input  wire       x,
    output wire       y,
    output wire       z
);
    reg [7:0] cnt_a;
    reg [7:0] cnt_b;
    reg       s1, s2;
    wire      toggle;

    assign toggle = cnt_a[7] & x;

    always @(posedge clk_a)
        if (rst) cnt_a <= 8'd0;
        else     cnt_a <= cnt_a + 8'd1;

    always @(posedge clk_b) begin
        s1 <= toggle;
        s2 <= s1;
    end

    always @(posedge clk_b)
        if (rst) cnt_b <= 8'd0;
        else if (s2) cnt_b <= cnt_b + 8'd1;

    assign y = cnt_a[0];
    assign z = cnt_b[0];
endmodule

// ---------------------------------------------------------------------------
// s_shared: the same two domains, but a combinational cone feeding BOTH.  The
// split must replicate it, not pick a side -- picking one strands a same-clock
// arc across the cut, which is what the checker would then reject.
// ---------------------------------------------------------------------------
module s_shared (
    input  wire       clk_a,
    input  wire       clk_b,
    input  wire [3:0] d,
    output wire       y,
    output wire       z
);
    wire      parity;
    reg       ra, rb;

    assign parity = ^d;

    always @(posedge clk_a) ra <= parity;
    always @(posedge clk_b) rb <= parity;

    assign y = ra;
    assign z = rb;
endmodule

// ---------------------------------------------------------------------------
// s_multi: a register bank written from two clocks.  There is no module to put
// the driver in, so the split must REFUSE rather than silently pick one.
// ---------------------------------------------------------------------------
module s_multi (
    input  wire clk_a,
    input  wire clk_b,
    input  wire sel,
    output wire y
);
    reg q;
    always @(posedge clk_a) if (sel)  q <= 1'b1;
    always @(posedge clk_b) if (!sel) q <= 1'b0;
    assign y = q;
endmodule
