module nor6(input a,b,c,d,e,f, output y); assign y = ~(a|b|c|d|e|f); endmodule
module and6(input a,b,c,d,e,f, output y); assign y =  (a&b&c&d&e&f); endmodule
