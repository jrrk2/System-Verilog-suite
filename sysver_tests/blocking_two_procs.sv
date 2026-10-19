// A blocking temp in one clocked block read by ANOTHER clocked block (it sees
// the register).  The front end merges same-clock processes, and the miter
// must stay coherent on the result: this design mitered DIFFER against its
// own re-parse while the register-sharing pass aliased the merged-away
// output ports to a `__Q` name only port registers have, and ffrip read the
// blocking `t` as a register's Q in `y <= t` (now threaded first).
module blocking_two_procs(input logic clk, input logic [7:0] a, output logic [7:0] y, z);
  logic [7:0] t, t_seen;
  always_ff @(posedge clk) begin
    z <= t;
    t = a;
    y <= t;
  end
  always_ff @(posedge clk) t_seen <= t;
endmodule
