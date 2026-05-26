(* End-to-end smoke for #99 + #100:
   build a BIR module, lower to hardcaml, lib-map to cell instances,
   emit cell-mapped Verilog, and report the cell histogram.

   Two test modules:
     - counter_sync (4-bit registered, sync reset)
     - simple_logic (combinational and/or/not on 4-bit busses)
   to exercise both DFF mapping and combinational mapping. *)

open Behavioral_ir

let counter ~name ~reset_async : bmodule = {
  name;
  params = [];
  signals = [
    { name = "clk"; stype = BBool;  direction = `Input;
      initial_value = None; attrs = [] };
    { name = "rst"; stype = BBool;  direction = `Input;
      initial_value = None; attrs = [] };
    { name = "q";   stype = BInt { width = 4; signed = Unsigned };
      direction = `Output; initial_value = None; attrs = [] };
  ];
  processes = [
    BSequential {
      name = "count"; clock = "clk"; clock_edge = `Pos;
      reset = Some "rst"; reset_edge = Some `Pos; reset_async;
      body = [
        BIf {
          condition = BVar "rst";
          then_stmts = [BAssign { lhs = "q";
                                  rhs = BConst { value = 0; width = 4 } }];
          else_stmts = [BAssign { lhs = "q";
                                  rhs = BBinOp {
                                    op = BAdd;
                                    lhs = BVar "q";
                                    rhs = BConst { value = 1; width = 4 };
                                    result_type = BInt { width = 4; signed = Unsigned }
                                  } }]
        }
      ];
      blocking_vars = []
    }
  ];
  instances = []; funcs = []; mems = []; attrs = [];
}

let simple_logic : bmodule = {
  name = "simple_logic";
  params = [];
  signals = [
    { name = "a"; stype = BInt { width = 4; signed = Unsigned };
      direction = `Input;  initial_value = None; attrs = [] };
    { name = "b"; stype = BInt { width = 4; signed = Unsigned };
      direction = `Input;  initial_value = None; attrs = [] };
    { name = "y"; stype = BInt { width = 4; signed = Unsigned };
      direction = `Output; initial_value = None; attrs = [] };
  ];
  processes = [
    BCombinational {
      name = "f"; sensitivity = [BAny];
      body = [
        BAssign { lhs = "y";
                  rhs = BBinOp {
                    op = BAnd;
                    lhs = BVar "a";
                    rhs = BUnOp { op = BNot;
                                  operand = BVar "b";
                                  result_type = BInt { width = 4; signed = Unsigned } };
                    result_type = BInt { width = 4; signed = Unsigned }
                  } }
      ]
    }
  ];
  instances = []; funcs = []; mems = []; attrs = [];
}

let run_case (m : bmodule) =
  Printf.printf "═══ %s ═══\n" m.name;
  let circuit = Behavioral_to_hardcaml.create_circuit m in
  let netlist = Lib_map.map_circuit circuit in
  Printf.printf "%s" (Cell_verilog_emit.summary netlist);
  let path = Filename.temp_file (m.name ^ "_") ".v" in
  let _ = Cell_verilog_emit.emit_to_file ~module_name:m.name netlist path in
  Printf.printf "  wrote %s\n" path;
  let ic = open_in path in
  let rec loop n =
    if n = 0 then ()
    else try
      Printf.printf "  %s\n" (input_line ic);
      loop (n-1)
    with End_of_file -> () in
  loop 50;
  close_in ic;
  Printf.printf "\n";
  netlist

let cell_count_of nm (n : Lib_map.netlist) =
  List.fold_left (fun acc (i : Lib_map.instance) ->
    if i.cell.cell_name = nm then acc + 1 else acc) 0 n.insts

let () =
  let nl_logic = run_case simple_logic in
  let nl_sync  = run_case (counter ~name:"counter_sync"  ~reset_async:false) in
  let nl_async = run_case (counter ~name:"counter_async" ~reset_async:true)  in

  let n_total = List.length nl_logic.insts
              + List.length nl_sync.insts
              + List.length nl_async.insts in
  Printf.printf "Combined: %d cell instances\n" n_total;

  let sync_dff   = cell_count_of "DFF_X1"  nl_sync in
  let sync_dffr  = cell_count_of "DFFR_X1" nl_sync in
  let async_dff  = cell_count_of "DFF_X1"  nl_async in
  let async_dffr = cell_count_of "DFFR_X1" nl_async in
  Printf.printf "  counter_sync : DFF_X1=%d  DFFR_X1=%d\n" sync_dff sync_dffr;
  Printf.printf "  counter_async: DFF_X1=%d  DFFR_X1=%d\n" async_dff async_dffr;

  let ok =
    n_total > 0
    && sync_dff = 4 && sync_dffr = 0       (* sync reset → no FF reset port *)
    && async_dffr = 4 && async_dff = 0     (* async reset → DFFR_X1 *)
  in
  if ok then
    Printf.printf "OK   sync→DFF_X1, async→DFFR_X1, structural Verilog produced\n"
  else (
    Printf.printf "FAIL cell mix unexpected\n";
    exit 1
  )
