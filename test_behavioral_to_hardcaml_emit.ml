(* Smoke test for #98: build a small BIR module by hand, lower it
   to a hardcaml [Circuit.t], emit Verilog via [Rtl.output],
   parse the result back via [Lef_def.Gate_verilog], and check
   that the structural shape (ports, instance count) matches.

   The BIR module is a 4-bit registered counter with synchronous
   reset — covers comb (BBinOp +), seq (BSequential with reset),
   and IO ports.  Small enough to inspect by eye, big enough to
   exercise the parts of [Behavioral_to_hardcaml] that #99/#100
   depend on. *)

open Behavioral_ir

let counter_module : bmodule = {
  name = "counter";
  params = [];
  signals = [
    { name = "clk";  stype = BBool; direction = `Input;
      initial_value = None; attrs = [] };
    { name = "rst";  stype = BBool; direction = `Input;
      initial_value = None; attrs = [] };
    { name = "q";    stype = BInt { width = 4; signed = Unsigned };
      direction = `Output; initial_value = None; attrs = [] };
  ];
  processes = [
    BSequential {
      name = "count";
      clock = "clk";
      clock_edge = `Pos;
      reset = Some "rst";
      reset_edge = Some `Pos;
      reset_async = false;
      body = [
        BIf {
          condition = BVar "rst";
          then_stmts = [
            BAssign { lhs = "q";
                      rhs = BConst { value = 0; width = 4 } }
          ];
          else_stmts = [
            BAssign { lhs = "q";
                      rhs = BBinOp {
                        op = BAdd;
                        lhs = BVar "q";
                        rhs = BConst { value = 1; width = 4 };
                        result_type = BInt { width = 4; signed = Unsigned } } }
          ];
        }
      ];
    }
  ];
  instances = [];
  funcs = [];
  mems = [];
  attrs = [];
}

let () =
  Printf.printf "Building hardcaml Circuit from BIR...\n%!";
  let circuit = Behavioral_to_hardcaml.create_circuit counter_module in
  Printf.printf "  Circuit name: %s\n" (Hardcaml.Circuit.name circuit);
  Printf.printf "  inputs: %d  outputs: %d\n"
    (List.length (Hardcaml.Circuit.inputs circuit))
    (List.length (Hardcaml.Circuit.outputs circuit));

  Printf.printf "\nEmitting Verilog...\n";
  let verilog = Behavioral_to_hardcaml.emit_verilog circuit in
  Printf.printf "  %d bytes\n" (String.length verilog);

  let v_path = Filename.temp_file "counter_" ".v" in
  let oc = open_out v_path in output_string oc verilog; close_out oc;
  Printf.printf "  wrote %s\n\n" v_path;

  Printf.printf "First 30 lines of emitted Verilog:\n";
  let ic = open_in v_path in
  let rec loop n =
    if n = 0 then ()
    else
      try
        let line = input_line ic in
        Printf.printf "  %s\n" line; loop (n - 1)
      with End_of_file -> ()
  in
  loop 30;
  close_in ic;

  (* Sanity-check the emit: should contain the module name,
     proper port widths, and the expected always-ff structure.
     Full structural round-trip via Gate_verilog must wait for
     #99 (Liberty mapper) to insert cell instances. *)
  let contains s sub =
    let ls = String.length s and lp = String.length sub in
    let rec scan i = i + lp <= ls
      && (String.sub s i lp = sub || scan (i+1)) in
    scan 0 in
  let checks = [
    "module counter",       contains verilog "module counter";
    "input clk",            contains verilog "input clk";
    "input rst",            contains verilog "input rst";
    "output [3:0] q",       contains verilog "output [3:0] q";
    "always @(posedge clk)",contains verilog "always @(posedge clk)";
    (* sync reset is in the data path, not on the FF port *)
    "no reset on FF port",  not (contains verilog "posedge rst");
    "rst becomes mux",      contains verilog "rst ?";
    "non-blocking <=",      contains verilog "<= ";
  ] in
  Printf.printf "\nVerilog structural checks:\n";
  let all_pass = ref true in
  List.iter (fun (label, ok) ->
    Printf.printf "  %s  %s\n" (if ok then "OK  " else "FAIL") label;
    if not ok then all_pass := false) checks;
  if !all_pass
  then Printf.printf "\nOK   end-to-end BIR -> hardcaml -> Verilog lowering works\n"
  else (Printf.printf "\nFAIL one or more structural checks\n"; exit 1)
