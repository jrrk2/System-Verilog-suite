// Test case for function inlining with case statements
// Based on patterns from Ariane ariane_pkg.sv

module test_function_case (
  input  logic [2:0] op,
  input  logic [2:0] addr,
  input  logic [1:0] size,
  output logic result,
  output logic [7:0] be_out
);

  // Simple function with case inside
  function automatic logic is_special(logic [2:0] op);
    case (op) inside
      [3'b001:3'b011]: begin
        return 1'b1;
      end
      default: return 1'b0;
    endcase
  endfunction

  // Nested case function (simplified from be_gen)
  function automatic logic [7:0] gen_mask(logic [1:0] size, logic [2:0] addr);
    case (size)
      2'b11: begin
        return 8'b1111_1111;
      end
      2'b10: begin
        case (addr[1:0])
          2'b00: return 8'b0000_1111;
          2'b01: return 8'b0011_1100;
          2'b10: return 8'b1111_0000;
        endcase
      end
      2'b01: begin
        case (addr[0])
          1'b0: return 8'b0000_0011;
          1'b1: return 8'b0000_1100;
        endcase
      end
      2'b00: begin
        return 8'b0000_0001;
      end
    endcase
  endfunction

  assign result = is_special(op);
  assign be_out = gen_mask(size, addr);

endmodule
