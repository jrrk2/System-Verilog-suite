// GETSTRINGCHAR's address: the block's heap index, plus one for the header,
// plus the byte index divided by four -- an 18-bit sum of two slices and a
// constant, next to other sums of the same slices (adder sharing / fusion).
module strget_addr (
    input  logic        clk,
    input  logic [1:0]  op,
    input  logic [35:0] accu, st_rd_a,
    output logic [17:0] temp_heap_addr,
    output logic [1:0]  str_byte
);
  localparam int HEAP_AW = 18;
  function automatic logic [HEAP_AW-1:0] Heap_index_of_ptr(input logic [35:0] ptr);
    Heap_index_of_ptr = ptr[HEAP_AW+1:2];
  endfunction
  always_ff @(posedge clk) begin
    case (op)
      2'd0: begin
        temp_heap_addr <= Heap_index_of_ptr(accu) + 1 + st_rd_a[HEAP_AW+2:3];
        str_byte <= st_rd_a[2:1];
      end
      2'd1: temp_heap_addr <= Heap_index_of_ptr(accu) + 1;
      2'd2: temp_heap_addr <= Heap_index_of_ptr(accu) + st_rd_a[HEAP_AW+2:3];
      2'd3: temp_heap_addr <= Heap_index_of_ptr(st_rd_a) + 1 + 1;
    endcase
  end
endmodule
