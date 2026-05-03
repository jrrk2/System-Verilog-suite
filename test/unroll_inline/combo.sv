// Combined test: function + task + constant-bound for loop in one
// always block. Exercises all three transformations together.

package combo_pkg;
    function automatic logic [7:0] dbl(input logic [7:0] a);
        return a + a;
    endfunction
    task automatic clear(output logic [7:0] v);
        v = 0;
    endtask
endpackage

module combo (
    input  logic [7:0] in_data,
    input  logic [3:0] sel,
    output logic [7:0] out_data
);
    import combo_pkg::*;
    always_comb begin
        clear(out_data);
        // Add `dbl(in_data)` `sel`-times — but bound the loop by a
        // constant 4 so the unroller can fully expand it.
        for (int i = 0; i < 4; i++)
            if (i < sel) out_data = out_data + dbl(in_data);
    end
endmodule
