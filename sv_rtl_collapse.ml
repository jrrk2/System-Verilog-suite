(* RTL Collapse - converts gate-level netlist back to behavioral RTL *)
(* Pattern recognition and reconstruction of high-level constructs *)

open Sv_netlist_reader

(* High-level RTL constructs *)
type rtl_operation =
  | RtlAnd of string * string * string      (* output = a & b *)
  | RtlOr of string * string * string       (* output = a | b *)
  | RtlXor of string * string * string      (* output = a ^ b *)
  | RtlNot of string * string               (* output = !a *)
  | RtlAdd of string * string * string      (* output = a + b *)
  | RtlSub of string * string * string      (* output = a - b *)
  | RtlMux of string * string * string * string  (* output = sel ? a : b *)
  | RtlAssign of string * expr              (* output = expr *)

type rtl_module = {
  rtl_name: string;
  rtl_inputs: (string * int) list;
  rtl_outputs: (string * int) list;
  rtl_ops: rtl_operation list;
}

(* Pattern matchers *)

(* Match AND gate pattern *)
let match_and_pattern lib netlist signal_name =
  match build_expr_for_signal lib netlist signal_name with
  | Some (EAnd (EVar a, EVar b)) ->
      Some (RtlAnd (signal_name, a, b))
  | _ -> None

(* Match OR gate pattern *)
let match_or_pattern lib netlist signal_name =
  match build_expr_for_signal lib netlist signal_name with
  | Some (EOr (EVar a, EVar b)) ->
      Some (RtlOr (signal_name, a, b))
  | _ -> None

(* Match XOR gate pattern *)
let match_xor_pattern lib netlist signal_name =
  match build_expr_for_signal lib netlist signal_name with
  | Some (EXor (EVar a, EVar b)) ->
      Some (RtlXor (signal_name, a, b))
  | _ -> None

(* Match NOT gate pattern *)
let match_not_pattern lib netlist signal_name =
  match build_expr_for_signal lib netlist signal_name with
  | Some (ENot (EVar a)) ->
      Some (RtlNot (signal_name, a))
  | _ -> None

(* Match MUX pattern: (s & a) | (!s & b) *)
let match_mux_pattern lib netlist signal_name =
  match build_expr_for_signal lib netlist signal_name with
  | Some (EOr (EAnd (EVar s1, EVar a), EAnd (ENot (EVar s2), EVar b))) when s1 = s2 ->
      Some (RtlMux (signal_name, s1, a, b))
  | Some (EOr (EAnd (ENot (EVar s1), EVar b), EAnd (EVar s2, EVar a))) when s1 = s2 ->
      Some (RtlMux (signal_name, s1, a, b))
  | _ -> None

(* Match multi-bit AND: multiple single-bit AND gates on corresponding bits *)
let match_multibit_and lib netlist output_base inputs =
  (* Look for pattern: out[i] = a[i] & b[i] for all i *)
  let patterns = ref [] in
  let width = ref 0 in

  (* Try to find consecutive bit operations *)
  List.iter (fun inst ->
    match Sv_liberty.get_cell lib inst.cell_type with
    | Some cell when cell.cell_type = "combinational" ->
        (* Check if it's an AND operation *)
        let output_pins = List.filter (fun p -> p.Sv_liberty.direction = Sv_liberty.Output) cell.pins in
        List.iter (fun out_pin ->
          match out_pin.Sv_liberty.function_expr with
          | Some func when String.contains (String.lowercase_ascii func) '&' ->
              (* This is an AND gate *)
              List.iter (fun conn ->
                if conn.pin_name = out_pin.Sv_liberty.name then begin
                  (* Check if output signal follows pattern *)
                  let sig_len = String.length conn.signal.sig_name in
                  let base_len = String.length output_base in
                  if sig_len >= base_len && String.sub conn.signal.sig_name 0 base_len = output_base then
                    patterns := inst :: !patterns
                end
              ) inst.conns
          | _ -> ()
        ) output_pins
    | _ -> ()
  ) netlist.net_instances;

  if List.length !patterns > 1 then
    Some (List.length !patterns)
  else
    None

(* Collapse netlist to RTL operations *)
let collapse_netlist lib netlist =
  let operations = ref [] in

  (* Try to match patterns for each output *)
  List.iter (fun output ->
    let matched = ref false in

    (* Try MUX first (more complex pattern) *)
    if not !matched then
      match match_mux_pattern lib netlist output.sig_name with
      | Some op -> operations := op :: !operations; matched := true
      | None -> ();

    (* Try basic gates *)
    if not !matched then
      match match_and_pattern lib netlist output.sig_name with
      | Some op -> operations := op :: !operations; matched := true
      | None -> ();

    if not !matched then
      match match_or_pattern lib netlist output.sig_name with
      | Some op -> operations := op :: !operations; matched := true
      | None -> ();

    if not !matched then
      match match_xor_pattern lib netlist output.sig_name with
      | Some op -> operations := op :: !operations; matched := true
      | None -> ();

    if not !matched then
      match match_not_pattern lib netlist output.sig_name with
      | Some op -> operations := op :: !operations; matched := true
      | None -> ();

    (* Fallback: generic assign *)
    if not !matched then
      match build_expr_for_signal lib netlist output.sig_name with
      | Some expr -> operations := RtlAssign (output.sig_name, expr) :: !operations
      | None -> ()
  ) netlist.net_outputs;

  {
    rtl_name = netlist.top_module;
    rtl_inputs = List.map (fun s -> (s.sig_name, s.sig_width)) netlist.net_inputs;
    rtl_outputs = List.map (fun s -> (s.sig_name, s.sig_width)) netlist.net_outputs;
    rtl_ops = List.rev !operations;
  }

(* Generate behavioral Verilog from RTL module *)
let verilog_of_rtl_operation = function
  | RtlAnd (out, a, b) ->
      Printf.sprintf "  assign %s = %s & %s;" out a b
  | RtlOr (out, a, b) ->
      Printf.sprintf "  assign %s = %s | %s;" out a b
  | RtlXor (out, a, b) ->
      Printf.sprintf "  assign %s = %s ^ %s;" out a b
  | RtlNot (out, a) ->
      Printf.sprintf "  assign %s = ~%s;" out a
  | RtlAdd (out, a, b) ->
      Printf.sprintf "  assign %s = %s + %s;" out a b
  | RtlSub (out, a, b) ->
      Printf.sprintf "  assign %s = %s - %s;" out a b
  | RtlMux (out, sel, a, b) ->
      Printf.sprintf "  assign %s = %s ? %s : %s;" out sel a b
  | RtlAssign (out, expr) ->
      Printf.sprintf "  assign %s = %s;" out (string_of_expr expr)

let verilog_of_rtl_module rtl =
  let buf = Buffer.create 1024 in

  (* Module header *)
  Buffer.add_string buf (Printf.sprintf "module %s (\n" rtl.rtl_name);

  (* Ports *)
  let all_ports = rtl.rtl_inputs @ rtl.rtl_outputs in
  let port_names = String.concat ",\n  " (List.map fst all_ports) in
  Buffer.add_string buf (Printf.sprintf "  %s\n);\n\n" port_names);

  (* Input declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  input %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  input [%d:0] %s;\n" (width-1) name)
  ) rtl.rtl_inputs;

  (* Output declarations *)
  List.iter (fun (name, width) ->
    if width = 1 then
      Buffer.add_string buf (Printf.sprintf "  output %s;\n" name)
    else
      Buffer.add_string buf (Printf.sprintf "  output [%d:0] %s;\n" (width-1) name)
  ) rtl.rtl_outputs;

  Buffer.add_string buf "\n";

  (* Operations *)
  List.iter (fun op ->
    Buffer.add_string buf (verilog_of_rtl_operation op);
    Buffer.add_string buf "\n"
  ) rtl.rtl_ops;

  Buffer.add_string buf "\nendmodule\n";
  Buffer.contents buf

(* Print RTL summary *)
let print_rtl_summary rtl =
  Printf.printf "RTL Module: %s\n" rtl.rtl_name;
  Printf.printf "Operations:\n";
  List.iter (fun op ->
    Printf.printf "  %s\n" (verilog_of_rtl_operation op)
  ) rtl.rtl_ops
