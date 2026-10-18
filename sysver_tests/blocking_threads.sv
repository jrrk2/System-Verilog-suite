// Blocking `=` in always_ff: a later read in the same body sees the
// expression being built for D, an earlier read (or another process) sees Q.
// blocking_subst threads those expressions; this covers the shapes it must
// keep exact.
module blocking_threads (
    input  logic       clk,
    input  logic [1:0] st,
    input  logic       c,
    input  logic [7:0] a, b,
    output logic [7:0] y, z, w, q,
    output logic [7:0] rd,
    output logic [3:0] ra
);
  logic [7:0] t, u, v;
  logic [3:0] addr;
  logic [7:0] wd;
  logic       we;

  always_ff @(posedge clk) begin
    // default, then per-state overrides, consumed at the bottom (RAM-port idiom)
    we = 1'b0; addr = '0; wd = '0;
    // read before write: Q of t from the previous cycle
    z <= t;
    t = a;
    if (c) t = t + b;            // reads the pending t
    case (st)
      2'd0: begin we = 1'b1; addr = a[3:0]; wd = t; end
      2'd1: addr = b[3:0];
      2'd2: begin u = t ^ b; t = u + 8'd1; end   // t rewritten after being read
      default: ;
    endcase
    y <= t;
    w <= u;                      // u is not written on every path: Q survives
    // a RAM port would read these here; registered so the miter sees them
    if (we) rd <= wd;
    ra <= addr;
    v = y + 8'd3;                // reads y's Q (non-blocking)
    q <= v;
  end

  // another process reads a blocking variable: it sees the register
  logic [7:0] t_seen;
  always_ff @(posedge clk) t_seen <= t;
endmodule
