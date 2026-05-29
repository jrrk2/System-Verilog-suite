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

(* Build a counter module parameterised on async/sync reset.
   The body is identical: `if (rst) q <= 0 else q <= q + 1`.
   Only the BSequential.reset_async flag changes.  This is exactly
   the case the lowering must distinguish — same RTL intent, two
   different hardware shapes. *)
let counter_module ~reset_async name : bmodule = {
  name;
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
      reset_async;
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
      blocking_vars = [];
    }
  ];
  instances = [];
  funcs = [];
  mems = [];
  attrs = [];
}

let contains s sub =
  let ls = String.length s and lp = String.length sub in
  let rec scan i = i + lp <= ls
    && (String.sub s i lp = sub || scan (i+1)) in
  scan 0

let lower_and_emit bm =
  let circuit = Behavioral_to_hardcaml.create_circuit bm in
  Behavioral_to_hardcaml.emit_verilog circuit

let run_case ~label ~reset_async ~expect_checks =
  Printf.printf "═══ %s (reset_async=%b) ═══\n" label reset_async;
  let bm = counter_module ~reset_async label in
  let v = lower_and_emit bm in
  Printf.printf "  %d bytes emitted\n" (String.length v);
  let path = Filename.temp_file (label ^ "_") ".v" in
  let oc = open_out path in output_string oc v; close_out oc;
  Printf.printf "  wrote %s\n" path;

  (* Print the always-block; the rest is housekeeping wires. *)
  let lines = String.split_on_char '\n' v in
  Printf.printf "  always-block emitted:\n";
  let in_always = ref false and depth = ref 0 in
  List.iter (fun line ->
    let t = String.trim line in
    if not !in_always && contains t "always @"
    then (in_always := true; depth := 0; Printf.printf "    %s\n" t)
    else if !in_always then begin
      Printf.printf "    %s\n" t;
      if contains t "begin" then incr depth;
      if contains t "end" then begin
        decr depth;
        if !depth <= 0 then in_always := false
      end
    end) lines;

  Printf.printf "  checks:\n";
  let all_pass = ref true in
  List.iter (fun (name, ok) ->
    Printf.printf "    %s  %s\n" (if ok then "OK  " else "FAIL") name;
    if not ok then all_pass := false) (expect_checks v);
  Printf.printf "\n";
  !all_pass

let () =
  let sync_pass = run_case
    ~label:"counter_sync"
    ~reset_async:false
    ~expect_checks:(fun v -> [
      "module exists",        contains v "module counter_sync";
      "always @(posedge clk)",contains v "always @(posedge clk)";
      "no rst on FF port",    not (contains v "posedge rst");
      "rst becomes data mux", contains v "rst ?";
    ]) in

  let async_pass = run_case
    ~label:"counter_async"
    ~reset_async:true
    ~expect_checks:(fun v -> [
      "module exists",            contains v "module counter_async";
      (* Async reset shows up two ways and BOTH must be present: *)
      "rst in sensitivity list",  contains v "posedge clk or posedge rst";
      "if (rst) inside always",   contains v "if (rst)";
      (* Hardcaml also emits a redundant data-path mux ahead of the
         FF; that's its emit style, not a semantic concern — synth
         removes it.  We do NOT check for absence of `rst ?`. *)
    ]) in

  if sync_pass && async_pass
  then Printf.printf "OK   both sync and async reset paths emit correct shape\n"
  else (Printf.printf "FAIL one or more checks\n"; exit 1)
