// ============================================================================
// function_task_test.sv - Test cases for function and task inlining
// ============================================================================

// Test 1: Simple function - returns max of two values
module test_function_simple(
  input  logic [7:0] a,
  input  logic [7:0] b,
  output logic [7:0] result
);
  function automatic logic [7:0] max;
    input logic [7:0] x;
    input logic [7:0] y;
    begin
      if (x > y)
        max = x;
      else
        max = y;
    end
  endfunction
  
  always_comb begin
    result = max(a, b);
  end
endmodule

// Expected after inlining:
// always_comb begin
//   logic [7:0] max_1_x;
//   logic [7:0] max_1_y;
//   max_1_x = a;
//   max_1_y = b;
//   if (max_1_x > max_1_y)
//     result = max_1_x;
//   else
//     result = max_1_y;
// end

// Test 2: Function with multiple calls
module test_function_multiple(
  input  logic [7:0] a,
  input  logic [7:0] b,
  input  logic [7:0] c,
  output logic [7:0] result
);
  function automatic logic [7:0] add;
    input logic [7:0] x;
    input logic [7:0] y;
    begin
      add = x + y;
    end
  endfunction
  
  always_comb begin
    result = add(add(a, b), c);  // Nested calls
  end
endmodule

// Test 3: Simple task - sets output
module test_task_simple(
  input  logic clk,
  input  logic [7:0] data_in,
  output logic [7:0] data_out
);
  task write_data;
    input logic [7:0] value;
    begin
      data_out = value;
    end
  endtask
  
  always_ff @(posedge clk) begin
    write_data(data_in);
  end
endmodule

// Test 4: Task with multiple statements
module test_task_complex(
  input  logic clk,
  input  logic [7:0] addr,
  input  logic [7:0] data,
  output logic [7:0] mem [0:255],
  output logic write_done
);
  task mem_write;
    input logic [7:0] a;
    input logic [7:0] d;
    begin
      mem[a] = d;
      write_done = 1'b1;
    end
  endtask
  
  always_ff @(posedge clk) begin
    write_done = 1'b0;
    mem_write(addr, data);
  end
endmodule

// Test 5: Function used in loop
module test_function_in_loop(
  input  logic [7:0] data [0:7],
  output logic [7:0] result
);
  function automatic logic [7:0] abs;
    input logic [7:0] x;
    begin
      abs = (x[7]) ? (~x + 1) : x;  // Two's complement if negative
    end
  endfunction
  
  always_comb begin
    result = 0;
    for (int i = 0; i < 8; i = i + 1) begin
      result = result + abs(data[i]);
    end
  end
endmodule

// Test 6: Recursive function (should NOT inline - detect and warn)
module test_function_recursive(
  input  logic [7:0] n,
  output logic [7:0] result
);
  function automatic logic [7:0] factorial;
    input logic [7:0] x;
    begin
      if (x <= 1)
        factorial = 1;
      else
        factorial = x * factorial(x - 1);  // Recursive call!
    end
  endfunction
  
  always_comb begin
    result = factorial(n);
  end
endmodule

// Test 7: Task calling another task (nested)
module test_task_nested(
  input  logic clk,
  input  logic [7:0] data,
  output logic [7:0] reg1,
  output logic [7:0] reg2
);
  task update_reg1;
    input logic [7:0] value;
    begin
      reg1 = value;
    end
  endtask
  
  task update_both;
    input logic [7:0] value;
    begin
      update_reg1(value);
      reg2 = value + 1;
    end
  endtask
  
  always_ff @(posedge clk) begin
    update_both(data);
  end
endmodule

// Test 8: Function with local variables
module test_function_locals(
  input  logic [7:0] x,
  input  logic [7:0] y,
  output logic [7:0] result
);
  function automatic logic [7:0] compute;
    input logic [7:0] a;
    input logic [7:0] b;
    logic [7:0] temp;
    begin
      temp = a + b;
      compute = temp * 2;
    end
  endfunction
  
  always_comb begin
    result = compute(x, y);
  end
endmodule

// Test 9: Multiple functions
module test_multiple_functions(
  input  logic [7:0] a,
  input  logic [7:0] b,
  output logic [7:0] sum,
  output logic [7:0] prod
);
  function automatic logic [7:0] add;
    input logic [7:0] x;
    input logic [7:0] y;
    begin
      add = x + y;
    end
  endfunction
  
  function automatic logic [7:0] multiply;
    input logic [7:0] x;
    input logic [7:0] y;
    begin
      multiply = x * y;
    end
  endfunction
  
  always_comb begin
    sum = add(a, b);
    prod = multiply(a, b);
  end
endmodule

// Test 10: Combined - loop unrolling + function inlining
module test_combined(
  input  logic [7:0] data [0:7],
  output logic [7:0] checksum
);
  function automatic logic [7:0] reduce_xor;
    input logic [7:0] x;
    begin
      reduce_xor = ^x;  // XOR all bits
    end
  endfunction
  
  always_comb begin
    checksum = 0;
    for (int i = 0; i < 8; i = i + 1) begin
      checksum = checksum ^ reduce_xor(data[i]);
    end
  end
endmodule

// Expected: Loop unrolled to 8 iterations, function inlined in each

// ============================================================================
// NEGATIVE TEST CASES - Should NOT inline or should warn
// ============================================================================

// Test 11: Function with system task (not synthesizable)
module test_function_system_task(
  input  logic [7:0] data,
  output logic [7:0] result
);
  function automatic logic [7:0] debug_add;
    input logic [7:0] x;
    begin
      $display("Adding: %d", x);  // System task!
      debug_add = x + 1;
    end
  endfunction
  
  always_comb begin
    result = debug_add(data);
  end
endmodule

// Test 12: Task with delay (not synthesizable)
module test_task_delay(
  input  logic clk,
  input  logic [7:0] data,
  output logic [7:0] result
);
  task delayed_write;
    input logic [7:0] value;
    begin
      #10 result = value;  // Delay!
    end
  endtask
  
  always_ff @(posedge clk) begin
    delayed_write(data);
  end
endmodule

// Test 13: Function with non-constant array access
module test_function_dynamic_array(
  input  logic [7:0] data [0:255],
  input  logic [7:0] index,
  output logic [7:0] result
);
  function automatic logic [7:0] lookup;
    input logic [7:0] addr;
    begin
      lookup = data[addr];  // Dynamic indexing
    end
  endfunction
  
  always_comb begin
    result = lookup(index);
  end
endmodule