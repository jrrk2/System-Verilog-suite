// Test cases demonstrating assignment ordering issues in MUX tree generation

// Test 1: Unconditional default followed by conditional override
// This is the pattern that failed in slib_clock_div
module test_unconditional_then_conditional (
    input wire CLK,
    input wire RST,
    input wire ENABLE,
    input wire COND,
    output logic Q
);

reg iQ;

always @(posedge CLK or posedge RST) begin
    if (RST)
        iQ <= 1'b0;
    else begin
        iQ <= 1'b0;           // Assignment 1: unconditional default
        if (ENABLE) begin
            if (COND)
                iQ <= 1'b1;   // Assignment 2: conditional override
        end
    end
end

assign Q = iQ;

endmodule

// Test 2: Multiple conditional assignments with priority
// Later assignments should override earlier ones
module test_multiple_conditionals (
    input wire CLK,
    input wire RST,
    input wire COND1,
    input wire COND2,
    input wire COND3,
    output logic Q
);

reg iQ;

always @(posedge CLK or posedge RST) begin
    if (RST)
        iQ <= 1'b0;
    else begin
        if (COND1)
            iQ <= 1'b1;       // Assignment 1: priority 3 (lowest)
        if (COND2)
            iQ <= 2'b10;      // Assignment 2: priority 2
        if (COND3)
            iQ <= 2'b11;      // Assignment 3: priority 1 (highest)
    end
end

assign Q = iQ;

endmodule

// Test 3: Sequential if statements (non-nested)
// Each assignment happens in program order
module test_sequential_ifs (
    input wire CLK,
    input wire RST,
    input wire A,
    input wire B,
    output logic [1:0] Q
);

reg [1:0] iQ;

always @(posedge CLK or posedge RST) begin
    if (RST)
        iQ <= 2'b00;
    else begin
        iQ <= 2'b00;          // Assignment 1: default
        if (A)
            iQ <= 2'b01;      // Assignment 2: override if A
        if (B)
            iQ <= 2'b10;      // Assignment 3: override if B (highest priority)
    end
end

assign Q = iQ;

// Expected behavior:
// RST=1: Q=00
// RST=0, A=0, B=0: Q=00 (default)
// RST=0, A=1, B=0: Q=01 (A wins)
// RST=0, A=0, B=1: Q=10 (B wins)
// RST=0, A=1, B=1: Q=10 (B wins - later assignment has priority)

endmodule

// Test 4: Nested if-else with unconditional in outer block
// This pattern was broken by incorrect List.rev
module test_nested_with_outer_unconditional (
    input wire CLK,
    input wire RST,
    input wire EN,
    input wire SEL,
    output logic [1:0] Q
);

reg [1:0] iQ;

always @(posedge CLK or posedge RST) begin
    if (RST)
        iQ <= 2'b00;
    else begin
        iQ <= 2'b00;                    // Assignment 1: unconditional (first)
        if (EN) begin
            if (SEL)
                iQ <= 2'b11;            // Assignment 2: EN && SEL
            else
                iQ <= 2'b01;            // Assignment 3: EN && !SEL
        end
    end
end

assign Q = iQ;

// Expected MUX tree (correct chronological order):
// iQ = (EN && SEL) ? 2'b11 : ((EN && !SEL) ? 2'b01 : 2'b00)
//
// With INCORRECT ordering (after extra List.rev):
// iQ = (EN && !SEL) ? 2'b01 : ((EN && SEL) ? 2'b11 : 2'b00)
// This would give wrong results when EN=1, SEL=1

endmodule

// Test 5: The actual slib_clock_div pattern (simplified)
module test_clock_div_pattern (
    input wire CLK,
    input wire RST,
    input wire CE,
    input wire AT_MAX,
    output logic Q
);

reg iQ;

always @(posedge CLK or posedge RST) begin
    if (RST)
        iQ <= 1'b0;
    else begin
        iQ <= 1'b0;              // Unconditional: always clear first
        if (CE) begin
            if (AT_MAX)
                iQ <= 1'b1;      // Conditional: set only if CE && AT_MAX
        end
    end
end

assign Q = iQ;

// Expected behavior:
// RST=1: Q=0
// RST=0, CE=0: Q=0 (unconditional assignment)
// RST=0, CE=1, AT_MAX=0: Q=0 (unconditional assignment wins)
// RST=0, CE=1, AT_MAX=1: Q=1 (conditional override wins)
//
// Correct MUX tree: (CE && AT_MAX) ? 1'b1 : 1'b0
// Wrong if ordered incorrectly!

endmodule

// Test 6: Assignment order matters with overlapping conditions
module test_overlapping_conditions (
    input wire CLK,
    input wire RST,
    input wire A,
    input wire B,
    output logic [1:0] Q
);

reg [1:0] iQ;

always @(posedge CLK or posedge RST) begin
    if (RST)
        iQ <= 2'b00;
    else begin
        if (A)
            iQ <= 2'b01;      // Assignment 1: if A
        if (A && B)
            iQ <= 2'b11;      // Assignment 2: if A && B (more specific, later)
    end
end

assign Q = iQ;

// Expected behavior when A=1, B=1:
// With correct ordering: Q=11 (assignment 2 wins - later in program order)
// With wrong ordering: Q=01 (assignment 1 would win incorrectly)

endmodule
