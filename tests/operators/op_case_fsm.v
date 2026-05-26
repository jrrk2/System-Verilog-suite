// Single-cycle FSM: case statement choosing the next state.
// Each branch is reached only when (state == that_branch_label) holds —
// our lowering must not let a downstream branch's RHS "leak" into the
// register update for a different branch.  This is the picorv32 pattern
// that produced fetch -> trap one cycle out of reset, where the
// `cpu_state_trap` branch's `trap <= 1` assignment was firing under
// the `cpu_state_fetch` branch's enable.
module op_case_fsm (
    input             clk,
    input             resetn,
    input             go,
    output reg [1:0]  state
);
  always @(posedge clk or negedge resetn) begin
    if (!resetn)
      state <= 2'd0;
    else begin
      case (state)
        2'd0: state <= go ? 2'd1 : 2'd0;
        2'd1: state <= 2'd2;
        2'd2: state <= 2'd0;
        default: state <= 2'd3;  // unreachable from reset
      endcase
    end
  end
endmodule
