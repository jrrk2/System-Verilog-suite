(* test_decompiler.ml - Test cases for the decompiler *)

open Sv_ast

(* Test case 1: Simple module with if-else *)
let test_if_else () =
  let module_ast = Module {
    name = "test_if";
    stmts = [
      Var { 
        name = "clk"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = None });
        var_type = "PORT"; 
        direction = "INPUT"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "data"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "PORT"; 
        direction = "OUTPUT"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "sel"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = None });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Always {
        always = "always_comb";
        senses = [];
        stmts = [
          If {
            condition = VarRef { name = "sel"; access = "RD" };
            then_stmt = Assign {
              lhs = VarRef { name = "data"; access = "WR" };
              rhs = Const { name = "8'hAA"; dtype_ref = None };
              is_blocking = true
            };
            else_stmt = Some (Assign {
              lhs = VarRef { name = "data"; access = "WR" };
              rhs = Const { name = "8'h55"; dtype_ref = None };
              is_blocking = true
            })
          }
        ]
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 2: Case statement *)
let test_case () =
  let module_ast = Module {
    name = "test_case";
    stmts = [
      Var { 
        name = "state"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "1:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "output_val"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Always {
        always = "always_comb";
        senses = [];
        stmts = [
          Case {
            expr = VarRef { name = "state"; access = "RD" };
            items = [
              { 
                conditions = [Const { name = "2'h0"; dtype_ref = None }];
                statements = [
                  Assign {
                    lhs = VarRef { name = "output_val"; access = "WR" };
                    rhs = Const { name = "8'h00"; dtype_ref = None };
                    is_blocking = true
                  }
                ]
              };
              { 
                conditions = [Const { name = "2'h1"; dtype_ref = None }];
                statements = [
                  Assign {
                    lhs = VarRef { name = "output_val"; access = "WR" };
                    rhs = Const { name = "8'hFF"; dtype_ref = None };
                    is_blocking = true
                  }
                ]
              };
              { 
                conditions = [];  (* default case *)
                statements = [
                  Assign {
                    lhs = VarRef { name = "output_val"; access = "WR" };
                    rhs = Const { name = "8'hAA"; dtype_ref = None };
                    is_blocking = true
                  }
                ]
              }
            ]
          }
        ]
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 3: For loop (using While) *)
let test_while_loop () =
  let module_ast = Module {
    name = "test_loop";
    stmts = [
      Initial {
        suspend = false;
        stmts = [
          Begin {
            name = "";
            stmts = [
              Var { 
                name = "i"; 
                dtype_ref = Some (BasicType { keyword = "integer"; range = None });
                var_type = "VAR"; 
                direction = "NONE"; 
                value = None; 
                dtype_name = "integer"; 
                is_param = false 
              };
              Assign {
                lhs = VarRef { name = "i"; access = "WR" };
                rhs = Const { name = "0"; dtype_ref = None };
                is_blocking = true
              };
              While {
                condition = BinaryOp {
                  op = "LT";
                  lhs = VarRef { name = "i"; access = "RD" };
                  rhs = Const { name = "10"; dtype_ref = None }
                };
                stmts = [
                  TaskRef {
                    name = "$display";
                    args = [
                      Const { name = "\"Loop iteration %0d\""; dtype_ref = None };
                      VarRef { name = "i"; access = "RD" }
                    ]
                  }
                ];
                incs = [
                  Assign {
                    lhs = VarRef { name = "i"; access = "WR" };
                    rhs = BinaryOp {
                      op = "ADD";
                      lhs = VarRef { name = "i"; access = "RD" };
                      rhs = Const { name = "1"; dtype_ref = None }
                    };
                    is_blocking = true
                  }
                ]
              }
            ];
            is_generate = false
          }
        ]
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 4: Array operations *)
let test_array_ops () =
  let module_ast = Module {
    name = "test_arrays";
    stmts = [
      Var { 
        name = "mem"; 
        dtype_ref = Some (ArrayType {
          base = BasicType { keyword = "logic"; range = Some "7:0" };
          range = "0:255"
        });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "addr"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "data"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Always {
        always = "always_comb";
        senses = [];
        stmts = [
          Assign {
            lhs = VarRef { name = "data"; access = "WR" };
            rhs = ArraySel {
              expr = VarRef { name = "mem"; access = "RD" };
              index = VarRef { name = "addr"; access = "RD" }
            };
            is_blocking = true
          }
        ]
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 5: Bit selection and slicing *)
let test_bit_select () =
  let module_ast = Module {
    name = "test_bitsel";
    stmts = [
      Var { 
        name = "data_in"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "15:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "bit_out"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = None });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "slice_out"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Always {
        always = "always_comb";
        senses = [];
        stmts = [
          Assign {
            lhs = VarRef { name = "bit_out"; access = "WR" };
            rhs = Sel {
              expr = VarRef { name = "data_in"; access = "RD" };
              lsb = Some (Const { name = "5"; dtype_ref = None });
              width = None;
              range = ""
            };
            is_blocking = true
          };
          Assign {
            lhs = VarRef { name = "slice_out"; access = "WR" };
            rhs = Sel {
              expr = VarRef { name = "data_in"; access = "RD" };
              lsb = Some (Const { name = "0"; dtype_ref = None });
              width = Some (Const { name = "8"; dtype_ref = None });
              range = ""
            };
            is_blocking = true
          }
        ]
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 6: Ternary operator (Cond) *)
let test_ternary () =
  let module_ast = Module {
    name = "test_ternary";
    stmts = [
      Var { 
        name = "sel"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = None });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "result"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      AssignW {
        lhs = VarRef { name = "result"; access = "WR" };
        rhs = Cond {
          condition = VarRef { name = "sel"; access = "RD" };
          then_val = Const { name = "8'hFF"; dtype_ref = None };
          else_val = Const { name = "8'h00"; dtype_ref = None }
        }
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 7: Concatenation *)
let test_concat () =
  let module_ast = Module {
    name = "test_concat";
    stmts = [
      Var { 
        name = "a"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "3:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "b"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "3:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "result"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      AssignW {
        lhs = VarRef { name = "result"; access = "WR" };
        rhs = Concat {
          parts = [
            VarRef { name = "a"; access = "RD" };
            VarRef { name = "b"; access = "RD" }
          ]
        }
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Test case 8: Function definition *)
let test_function () =
  let pkg_ast = Package {
    name = "my_pkg";
    stmts = [
      Func {
        name = "add_one";
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        vars = [
          Var { 
            name = "value"; 
            dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
            var_type = "PORT"; 
            direction = "INPUT"; 
            value = None; 
            dtype_name = "logic"; 
            is_param = false 
          }
        ];
        stmts = [
          Assign {
            lhs = VarRef { name = "add_one"; access = "WR" };
            rhs = BinaryOp {
              op = "ADD";
              lhs = VarRef { name = "value"; access = "RD" };
              rhs = Const { name = "8'h01"; dtype_ref = None }
            };
            is_blocking = true
          }
        ]
      }
    ]
  } in
  Sv_gen.generate_sv pkg_ast 0

(* Test case 9: Typedef with enum *)
let test_typedef_enum () =
  let pkg_ast = Package {
    name = "state_pkg";
    stmts = [
      Typedef {
        name = "state_t";
        dtype_ref = Some (EnumType {
          name = "state_t";
          items = [
            ("IDLE", "2'h0");
            ("ACTIVE", "2'h1");
            ("DONE", "2'h2")
          ]
        })
      }
    ]
  } in
  Sv_gen.generate_sv pkg_ast 0

(* Test case 10: Unary reduction operators *)
let test_unary_ops () =
  let module_ast = Module {
    name = "test_unary";
    stmts = [
      Var { 
        name = "data"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = Some "7:0" });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      Var { 
        name = "parity"; 
        dtype_ref = Some (BasicType { keyword = "logic"; range = None });
        var_type = "VAR"; 
        direction = "NONE"; 
        value = None; 
        dtype_name = "logic"; 
        is_param = false 
      };
      AssignW {
        lhs = VarRef { name = "parity"; access = "WR" };
        rhs = UnaryOp {
          op = "REDXOR";
          operand = VarRef { name = "data"; access = "RD" }
        }
      }
    ]
  } in
  Sv_gen.generate_sv module_ast 0

(* Main test runner *)
let () =
  Printf.printf "=== Test 1: If-Else ===\n%s\n\n" (test_if_else ());
  Printf.printf "=== Test 2: Case Statement ===\n%s\n\n" (test_case ());
  Printf.printf "=== Test 3: While Loop ===\n%s\n\n" (test_while_loop ());
  Printf.printf "=== Test 4: Array Operations ===\n%s\n\n" (test_array_ops ());
  Printf.printf "=== Test 5: Bit Selection ===\n%s\n\n" (test_bit_select ());
  Printf.printf "=== Test 6: Ternary Operator ===\n%s\n\n" (test_ternary ());
  Printf.printf "=== Test 7: Concatenation ===\n%s\n\n" (test_concat ());
  Printf.printf "=== Test 8: Function ===\n%s\n\n" (test_function ());
  Printf.printf "=== Test 9: Typedef Enum ===\n%s\n\n" (test_typedef_enum ());
  Printf.printf "=== Test 10: Unary Operators ===\n%s\n\n" (test_unary_ops ())
