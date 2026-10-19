// A function whose result is set by `return` inside case arms after a default
// assignment (the verilator lowering of ariane_pkg::op_is_branch): the inliner
// must see a `return` as yielding the result, or the call survives to the gate
// lowering as an unknown function and is lowered to a constant 0.
module func_return_case(input logic [7:0] op, input logic [7:0] x, output logic hit, output logic [7:0] y);
  function automatic logic is_branch(input logic [7:0] o);
    is_branch = 1'b0;
    case (o)
      8'd17: return 1'b1;
      8'd19: return 1'b1;
      default: return 1'b0;
    endcase
  endfunction
  function automatic logic [7:0] bump(input logic [7:0] v);
    if (v == 8'hff) return 8'd0;
    return v + 8'd1;
  endfunction
  assign hit = is_branch(op);
  assign y = hit ? bump(x) : x;
endmodule
