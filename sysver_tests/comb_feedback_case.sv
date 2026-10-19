// A combinational case that leaves its targets alone in one arm.  The
// source marks the arm unreachable (full_case; sel is never 3); yosys
// renders the hole as a bit-mask mux that feeds y back to itself, verible
// reads the case as-is with no default.  Either way the miter saw y = f(y)
// and was free to pick a different fixed point per side, so the yosys
// roundtrip of picorv32 (mem_la_wdata at mem_wordsize == 3) DIFFERed on a
// state the design cannot reach.  The miter now reads the pre-value as a
// state element tied by name across the two designs.
module comb_feedback_case(input logic clk, input logic resetn, input logic [7:0] d,
                          input logic step, output logic [7:0] y, output logic [1:0] strb);
  logic [1:0] sel;
  always @(posedge clk)
    if (!resetn) sel <= 2'd0;
    else if (step) sel <= (sel == 2'd2) ? 2'd0 : sel + 2'd1;
  always @* begin
    (* full_case *)
    case (sel)
      2'd0: begin y = d;               strb = 2'b11; end
      2'd1: begin y = {d[3:0], d[3:0]}; strb = 2'b01; end
      2'd2: begin y = {2{d[3:0]}} ^ 8'h0f; strb = 2'b10; end
    endcase
  end
endmodule
