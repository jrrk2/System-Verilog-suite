(* Convert EDIF to Behavioral IR *)

open Behavioral_ir
open Edif_parser

(* ── Verilog literal → bit list (LSB first) ───────────────────────────
   Parses "1'b0", "4'h9", "64'h9009000000009009", "8'b00010110", etc.
   Returns (width, [bit0; bit1; ...; bit(width-1)]).  *)
let parse_verilog_literal (s : string) : int * int list =
  (* Strip whitespace *)
  let s = String.trim s in
  match String.index_opt s '\'' with
  | None -> (1, [0])  (* no width prefix — treat as 1-bit zero *)
  | Some apos ->
      let width = try int_of_string (String.sub s 0 apos) with _ -> 1 in
      let base_char = if apos + 1 < String.length s then s.[apos + 1] else 'h' in
      let digits =
        if apos + 2 < String.length s
        then String.sub s (apos + 2) (String.length s - apos - 2)
        else "" in
      let bits = ref [] in
      (match Char.lowercase_ascii base_char with
       | 'h' | 'x' ->
           String.iter (fun c ->
             let nib =
               match c with
               | '_' -> -1
               | '0'..'9' -> Char.code c - Char.code '0'
               | 'a'..'f' -> 10 + Char.code c - Char.code 'a'
               | 'A'..'F' -> 10 + Char.code c - Char.code 'A'
               | _ -> -1 in
             if nib >= 0 then
               for i = 3 downto 0 do
                 bits := ((nib lsr i) land 1) :: !bits
               done
           ) digits
       | 'b' ->
           String.iter (fun c ->
             match c with
             | '0' -> bits := 0 :: !bits
             | '1' -> bits := 1 :: !bits
             | _ -> ()
           ) digits
       | 'd' ->
           let v = try int_of_string digits with _ -> 0 in
           for i = 0 to max width 32 - 1 do
             bits := ((v lsr i) land 1) :: !bits
           done
       | _ -> ());
      (* bits is currently MSB-first reversed; flip to LSB-first then truncate/pad. *)
      let bs = List.rev !bits in
      let bs = List.rev bs in  (* nothing — kept for clarity *)
      let lsb_first = List.rev bs in
      let n = List.length lsb_first in
      if n >= width then
        (width, List.filteri (fun i _ -> i < width) lsb_first)
      else
        (width, lsb_first @ List.init (width - n) (fun _ -> 0))

(* Build a mux tree from a LUT INIT bit list and an input list.
   [inputs] is LSB-first (input 0 controls bit-0 selection).
   For k inputs, INIT has 2^k bits; bit j is the output when the inputs
   form the integer j (LSB = input 0).                                  *)
let rec lut_mux_tree (inputs : bexpr list) (init_bits : int list) : bexpr =
  match inputs with
  | [] ->
      let b = match init_bits with [b] -> b | _ -> 0 in
      BConst { value = Z.of_int b; width = 1 }
  | hi :: rest ->
      (* Split init_bits in half: lower half = output when hi=0, upper
         half = output when hi=1. *)
      let n = List.length init_bits in
      let half = n / 2 in
      let lo_bits = List.filteri (fun i _ -> i < half) init_bits in
      let hi_bits = List.filteri (fun i _ -> i >= half) init_bits in
      BCond {
        condition = hi;
        then_val  = lut_mux_tree rest hi_bits;
        else_val  = lut_mux_tree rest lo_bits;
      }

(* Decode a LUTk's INIT into a behavioral expression given the input
   pins in order I0, I1, …, I(k-1).  Returns None if the INIT property
   couldn't be parsed.                                                  *)
let lut_to_bexpr (init : string option) (inputs : bexpr list) : bexpr option =
  match init with
  | None -> None
  | Some s ->
      let width, bits = parse_verilog_literal s in
      let k = List.length inputs in
      if width <> 1 lsl k then None
      else Some (lut_mux_tree inputs bits)

(* Map RTL primitives to behavioral expressions *)
let map_rtl_primitive cell_type inputs =
  match cell_type with
  | "RTL_AND" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BAnd; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_OR" | "RTL_OR4" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BOr; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_XOR" | "RTL_XOR2" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BXor; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_INV" ->
      (match inputs with
       | [a] -> Some (BUnOp { op = BNot; operand = a;
                              result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_ADD" | "RTL_ADD2" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BAdd; lhs = a; rhs = b;
                                  result_type = BInt { width = 32; signed = Unsigned } })
       | _ -> None)

  | "RTL_SUB" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BSub; lhs = a; rhs = b;
                                  result_type = BInt { width = 32; signed = Unsigned } })
       | _ -> None)

  | "RTL_EQ" | "RTL_EQ2" | "RTL_EQ25" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BEq; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_NEQ" | "RTL_NEQ3" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BNe; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_LT" ->
      (match inputs with
       | [a; b] -> Some (BBinOp { op = BLt; lhs = a; rhs = b;
                                  result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "RTL_MUX" | "RTL_MUX1" | "RTL_MUX5" | "RTL_MUX11" | "RTL_MUX16" | "RTL_MUX86"
  | "RTL_MUX167" | "RTL_MUX180" | "RTL_MUX189" | "RTL_MUX198" ->
      (match inputs with
       | s :: i0 :: i1 :: _ ->
           Some (BCond { condition = s; then_val = i1; else_val = i0 })
       | _ -> None)

  | "OBUF" | "IBUF" | "BUFG" | "BUF" ->
      (match inputs with
       | [a] -> Some a  (* Passthrough *)
       | _ -> None)

  | "INV" ->
      (match inputs with
       | [a] -> Some (BUnOp { op = BNot; operand = a;
                              result_type = BInt { width = 1; signed = Unsigned } })
       | _ -> None)

  | "GND" -> Some (BConst { value = Z.zero; width = 1 })
  | "VCC" -> Some (BConst { value = Z.one; width = 1 })

  | _ -> None

(* Xilinx full-synth primitives with parameters (INIT).  Looks up the
   instance's INIT property for LUTs; returns None for primitives that
   need other handling (FFs → BSequential, CARRY4 → multi-output).    *)
let map_xilinx_comb (cell_type : string) (init : string option)
    (inputs : bexpr list) : bexpr option =
  match cell_type with
  | "LUT1" | "LUT2" | "LUT3" | "LUT4" | "LUT5" | "LUT6" ->
      lut_to_bexpr init inputs
  | "MUXF7" | "MUXF8" | "MUXF9" ->
      (* MUXF{N}(I0, I1, S) → S ? I1 : I0 *)
      (match inputs with
       | [i0; i1; s] -> Some (BCond { condition = s; then_val = i1; else_val = i0 })
       | _ -> None)
  | _ -> None

(* Is this cell type a Xilinx full-synth primitive?  Used to decide
   between "lower to gates" and "treat as a hierarchical child".      *)
let is_xilinx_primitive cell_type =
  match cell_type with
  | "LUT1" | "LUT2" | "LUT3" | "LUT4" | "LUT5" | "LUT6"
  | "MUXF7" | "MUXF8" | "MUXF9"
  | "FD" | "FDR" | "FDS" | "FDC" | "FDP"
  | "FDE" | "FDRE" | "FDSE" | "FDCE" | "FDPE"
  | "FDR_1" | "FDS_1" | "FDC_1" | "FDP_1"
  | "CARRY4" | "CARRY8" | "MUXCY" | "XORCY"
  | "INV" | "BUFG" | "BUFGCE" | "BUF" -> true
  | _ -> false

(* Xilinx flip-flop classification.
   sync_rst   : sync reset to 0 (R pin)
   sync_set   : sync set to 1 (S pin)
   async_rst  : async clear to 0 (CLR pin)
   async_set  : async preset to 1 (PRE pin)
   has_ce     : has CE pin
   The pin name lists below capture what Vivado uses post-`synth_design`. *)
type ff_kind = {
  sync_rst   : bool;
  sync_set   : bool;
  async_clr  : bool;
  async_pre  : bool;
  has_ce     : bool;
}

let xilinx_ff_kind = function
  | "FD"    -> Some { sync_rst=false; sync_set=false; async_clr=false;
                      async_pre=false; has_ce=false }
  | "FDE"   -> Some { sync_rst=false; sync_set=false; async_clr=false;
                      async_pre=false; has_ce=true  }
  | "FDR"   -> Some { sync_rst=true;  sync_set=false; async_clr=false;
                      async_pre=false; has_ce=false }
  | "FDRE"  -> Some { sync_rst=true;  sync_set=false; async_clr=false;
                      async_pre=false; has_ce=true  }
  | "FDS"   -> Some { sync_rst=false; sync_set=true;  async_clr=false;
                      async_pre=false; has_ce=false }
  | "FDSE"  -> Some { sync_rst=false; sync_set=true;  async_clr=false;
                      async_pre=false; has_ce=true  }
  | "FDC" | "FDC_1" ->
                Some { sync_rst=false; sync_set=false; async_clr=true;
                       async_pre=false; has_ce=false }
  | "FDCE"  -> Some { sync_rst=false; sync_set=false; async_clr=true;
                      async_pre=false; has_ce=true  }
  | "FDP" | "FDP_1" ->
                Some { sync_rst=false; sync_set=false; async_clr=false;
                       async_pre=true;  has_ce=false }
  | "FDPE"  -> Some { sync_rst=false; sync_set=false; async_clr=false;
                      async_pre=true;  has_ce=true  }
  | _ -> None

(* Convert a single EDIF cell to a Behavioral IR module *)
let convert_cell (edif : edif_data) =

  (* Helper: parse signal name to extract base and bit index *)
  let parse_signal_name name =
    try
      let bracket_pos = String.rindex name '[' in
      let close_pos = String.rindex name ']' in
      if close_pos = String.length name - 1 then
        let base = String.sub name 0 bracket_pos in
        let idx_str = String.sub name (bracket_pos + 1) (close_pos - bracket_pos - 1) in
        let idx = int_of_string idx_str in
        (base, Some idx)
      else
        (name, None)
    with _ -> (name, None)
  in

  (* Helper: convert net name to expression *)
  let net_to_expr net_name =
    let (base, idx_opt) = parse_signal_name net_name in
    match idx_opt with
    | Some idx -> BSelect { array = BVar base; index = BConst { value = Z.of_int idx; width = 32 } }
    | None -> BVar base
  in

  (* Build connectivity map: (inst, pin) -> net *)
  let pin_to_net = Hashtbl.create 1024 in
  List.iter (fun (net : net_info) ->
    List.iter (fun (pin : net_pin) ->
      match pin.inst with
      | Some inst_name ->
          let pin_name = match pin.index with
            | Some idx -> Printf.sprintf "%s[%d]" pin.pin idx
            | None -> pin.pin
          in
          Hashtbl.add pin_to_net (inst_name, pin_name) net.name
      | None -> ()  (* Top-level port *)
    ) net.connections
  ) edif.nets;

  (* Helper: get input nets for an instance *)
  let get_inputs inst_name =
    let pins = ref [] in
    (* Try common input pin names *)
    List.iter (fun pin_name ->
      try
        let net = Hashtbl.find pin_to_net (inst_name, pin_name) in
        pins := net :: !pins
      with Not_found -> ()
    ) ["I0"; "I1"; "I"; "D"; "S"];
    List.map net_to_expr (List.rev !pins)
  in

  (* Helper: get output net for an instance *)
  let get_output inst_name =
    try
      Some (Hashtbl.find pin_to_net (inst_name, "O"))
    with Not_found ->
      try Some (Hashtbl.find pin_to_net (inst_name, "Q"))
      with Not_found ->
        try Some (Hashtbl.find pin_to_net (inst_name, "G"))
        with Not_found ->
          try Some (Hashtbl.find pin_to_net (inst_name, "P"))
          with Not_found -> None
  in

  (* Group signals with indices into vectors *)
  let group_into_vectors signal_list =
    let groups : (string, (int * bsignal) list) Hashtbl.t = Hashtbl.create 256 in
    List.iter (fun (sig_ : bsignal) ->
      let (base, idx_opt) = parse_signal_name sig_.name in
      match idx_opt with
      | Some idx ->
          let existing = try Hashtbl.find groups base with Not_found -> [] in
          Hashtbl.replace groups base ((idx, sig_) :: existing)
      | None ->
          Hashtbl.replace groups sig_.name [(0, sig_)]
    ) signal_list;

    Hashtbl.fold (fun base indices acc ->
      let sorted = List.sort (fun (i1, _) (i2, _) -> compare i1 i2) indices in
      match sorted with
      | [(0, sig_)] when not (List.exists (fun (i, _) -> i > 0) sorted) ->
          sig_ :: acc
      | indices ->
          let max_idx = List.fold_left (fun m (i, _) -> max m i) 0 indices in
          let (_, template) = List.hd sorted in
          { template with
            name = base;
            stype = BInt { width = max_idx + 1; signed = Unsigned };
          } :: acc
    ) groups []
  in

  (* Create signals from ports and nets *)
  let ungrouped_signals =
    (* Ports *)
    List.map (fun (p : port_info) ->
      let direction = match p.direction with
        | Input -> `Input
        | Output -> `Output
        | Inout -> `Internal  (* Treat inout as internal for now *)
      in
      {
        name = p.name;
        stype = BInt { width = p.width; signed = Unsigned };
        direction;
        initial_value = None; attrs = []; 
      }
    ) edif.ports
    @
    (* Internal nets *)
    List.filter_map (fun (n : net_info) ->
      (* Skip constant nets *)
      if n.name = "<const0>" || n.name = "<const1>" then None
      else
        (* Check if it's a port or matches a port's indexed name *)
        let (base, _) = parse_signal_name n.name in
        let is_port = List.exists (fun (p : port_info) ->
          p.name = n.name || p.name = base
        ) edif.ports in
        if not is_port then
          Some {
            name = n.name;
            stype = BInt { width = 1; signed = Unsigned };
            direction = `Internal;
            initial_value = None; attrs = []; 
          }
        else None
    ) edif.nets
  in

  (* Group internal signals into vectors, but keep ports as-is since they
     already have correct widths from EDIF *)
  let (port_signals, internal_signals) = List.partition (fun (s : bsignal) ->
    s.direction <> `Internal
  ) ungrouped_signals in

  let grouped_internals = group_into_vectors internal_signals in
  let signals = port_signals @ grouped_internals in

  (* Helper: check if instance is a known combinational primitive *)
  let is_known_primitive cell_type =
    match cell_type with
    | "RTL_AND" | "RTL_OR" | "RTL_OR4" | "RTL_XOR" | "RTL_XOR2"
    | "RTL_INV" | "RTL_ADD" | "RTL_ADD2" | "RTL_SUB"
    | "RTL_EQ" | "RTL_EQ2" | "RTL_EQ25" | "RTL_NEQ" | "RTL_NEQ3" | "RTL_LT"
    | "RTL_MUX" | "RTL_MUX1" | "RTL_MUX5" | "RTL_MUX11" | "RTL_MUX16" | "RTL_MUX86"
    | "RTL_MUX167" | "RTL_MUX180" | "RTL_MUX189" | "RTL_MUX198"
    | "GND" | "VCC" | "OBUF" | "IBUF" | "BUFG" | "BUF" | "INV" -> true
    | s when is_xilinx_primitive s -> true
    | _ -> false
  in

  (* Lookup a single pin's net for an instance, returning a bexpr.       *)
  let get_pin_net inst_name pin_name : bexpr option =
    try Some (net_to_expr (Hashtbl.find pin_to_net (inst_name, pin_name)))
    with Not_found -> None
  in
  let pin_or_zero inst pin =
    match get_pin_net inst pin with
    | Some e -> e
    | None -> BConst { value = Z.zero; width = 1 } in

  (* Create continuous assignments for primitives.  Sequential elements
     (LUT/MUXF outputs that are direct combinational, plus Xilinx FFs)
     accumulate separately so they can be emitted as proper processes. *)
  let assignments = ref [] in
  let sequential_processes = ref [] in
  List.iter (fun (inst : instance_info) ->
    let ct = inst.cell_type in

    (* Combinational Xilinx primitives — LUT*, MUXF*, INV, BUFG, IBUF, OBUF. *)
    (if is_xilinx_primitive ct then begin
      match get_output inst.name, xilinx_ff_kind ct with
      | Some output, None when output <> "<const0>" && output <> "<const1>" ->
          (* Collect inputs in fixed pin-name order. *)
          let in_pins = match ct with
            | "LUT1" -> ["I0"]
            | "LUT2" -> ["I0"; "I1"]
            | "LUT3" -> ["I0"; "I1"; "I2"]
            | "LUT4" -> ["I0"; "I1"; "I2"; "I3"]
            | "LUT5" -> ["I0"; "I1"; "I2"; "I3"; "I4"]
            | "LUT6" -> ["I0"; "I1"; "I2"; "I3"; "I4"; "I5"]
            | "MUXF7" | "MUXF8" | "MUXF9" -> ["I0"; "I1"; "S"]
            | "INV" | "BUFG" | "BUFGCE" | "BUF" -> ["I"]
            | _ -> []
          in
          let inputs = List.filter_map (get_pin_net inst.name) in_pins in
          let expr_opt =
            if List.length inputs = List.length in_pins then
              match ct with
              | "INV" | "BUFG" | "BUFGCE" | "BUF" ->
                  map_rtl_primitive (if ct = "INV" then "INV" else "IBUF") inputs
              | _ -> map_xilinx_comb ct inst.init inputs
            else None
          in
          (match expr_opt with
           | Some expr ->
               assignments := BAssign { lhs = output; rhs = expr } :: !assignments
           | None -> ())
      | _ -> ()
    end
    else if is_known_primitive ct then begin
      (* Legacy RTL_* + GND/VCC path — unchanged from before. *)
      match get_output inst.name with
      | Some output when output <> "<const0>" && output <> "<const1>" ->
          let inputs = get_inputs inst.name in
          (match map_rtl_primitive ct inputs with
           | Some expr ->
               assignments := BAssign { lhs = output; rhs = expr } :: !assignments
           | None -> ())
      | _ -> ()
    end);

    (* Xilinx FF — emit a BSequential. *)
    (match xilinx_ff_kind ct, get_pin_net inst.name "Q" with
     | Some k, Some q_expr ->
         let q_name = match q_expr with BVar n -> n | _ -> inst.name ^ "_Q" in
         let d_e = pin_or_zero inst.name "D" in
         let clk_name = match get_pin_net inst.name "C" with
           | Some (BVar n) -> n | _ -> "clk" in
         let ce_e = if k.has_ce then get_pin_net inst.name "CE" else None in
         (* Body: optionally gated by CE.  When the FF has a sync R or
            S, that takes priority inside the clocked branch. *)
         let core_assign =
           let d_with_sync_rst_set =
             if k.sync_rst then
               BCond { condition = pin_or_zero inst.name "R";
                       then_val  = BConst { value = Z.zero; width = 1 };
                       else_val  = d_e }
             else if k.sync_set then
               BCond { condition = pin_or_zero inst.name "S";
                       then_val  = BConst { value = Z.one; width = 1 };
                       else_val  = d_e }
             else d_e in
           let rhs = match ce_e with
             | Some ce -> BCond { condition = ce;
                                  then_val  = d_with_sync_rst_set;
                                  else_val  = BVar q_name }
             | None -> d_with_sync_rst_set in
           [BAssign { lhs = q_name; rhs }] in
         let body =
           if k.async_clr then
             [BIf { condition = pin_or_zero inst.name "CLR";
                    then_stmts = [BAssign { lhs = q_name;
                                           rhs = BConst { value = Z.zero; width = 1 } }];
                    else_stmts = core_assign }]
           else if k.async_pre then
             [BIf { condition = pin_or_zero inst.name "PRE";
                    then_stmts = [BAssign { lhs = q_name;
                                           rhs = BConst { value = Z.one; width = 1 } }];
                    else_stmts = core_assign }]
           else core_assign in
         let reset, reset_async =
           if k.async_clr then
             (Some (match get_pin_net inst.name "CLR" with
                    | Some (BVar n) -> n | _ -> "clr"), true)
           else if k.async_pre then
             (Some (match get_pin_net inst.name "PRE" with
                    | Some (BVar n) -> n | _ -> "pre"), true)
           else (None, false) in
         sequential_processes := BSequential {
           name = inst.name ^ "_ff";
           clock = clk_name;
           clock_edge = `Pos;
           reset;
           reset_edge = (if Option.is_some reset then Some `Pos else None);
           reset_async;
           body;
           blocking_vars = [];
         } :: !sequential_processes
     | _ -> ())
  ) edif.instances;

  (* Create a combinational process *)
  let main_process = BCombinational {
    name = "main_logic";
    sensitivity = [BAny];
    body = List.rev !assignments;
  } in

  (* Helper: normalize port names from EDIF to Verilog array notation *)
  (* Converts "I0_0_" to "I0", "I1_5_" to "I1", etc. *)
  let normalize_port_name port_name =
    try
      (* Check if port name ends with _N_ pattern *)
      let len = String.length port_name in
      if len > 3 && port_name.[len - 1] = '_' then
        (* Find last underscore before the trailing one *)
        let rec find_last_underscore pos =
          if pos < 0 then None
          else if port_name.[pos] = '_' then
            (* Check if everything after this underscore (except last char) is digits *)
            let suffix_start = pos + 1 in
            let suffix_end = len - 1 in
            let is_numeric = ref true in
            for i = suffix_start to suffix_end - 1 do
              if not (port_name.[i] >= '0' && port_name.[i] <= '9') then
                is_numeric := false
            done;
            if !is_numeric && suffix_end > suffix_start then
              Some pos
            else
              find_last_underscore (pos - 1)
          else
            find_last_underscore (pos - 1)
        in
        match find_last_underscore (len - 2) with
        | Some pos -> String.sub port_name 0 pos
        | None -> port_name
      else
        port_name
    with _ -> port_name
  in

  (* Helper: get all port connections for an instance *)
  let get_port_connections inst_name =
    let connections = ref [] in
    List.iter (fun (net : net_info) ->
      List.iter (fun (pin : net_pin) ->
        match pin.inst with
        | Some inst when inst = inst_name ->
            let normalized_pin = normalize_port_name pin.pin in
            connections := (normalized_pin, net_to_expr net.name) :: !connections
        | _ -> ()
      ) net.connections
    ) edif.nets;
    List.rev !connections
  in

  (* Identify hierarchical instances *)
  (* No need to parse parameterized names - EDIF is post-elaboration,
     so parameterized variants are separate modules (e.g., slib_input_filter__parameterized2) *)
  let hier_instances = List.filter_map (fun (inst : instance_info) ->
    if not (is_known_primitive inst.cell_type) then
      Some {
        inst_name = inst.name;
        module_name = inst.cell_type;  (* Use full name as-is *)
        param_values = []; param_strs = [];  (* No parameters - already elaborated *)
        port_connections = get_port_connections inst.name;
      }
    else None
  ) edif.instances in

  (* Create module *)
  {
    name = edif.module_name;
    params = [];
    signals;
    processes = main_process :: List.rev !sequential_processes;
    instances = hier_instances;
    funcs = [];
    mems = []; attrs = [];
  }

(* Convert EDIF to Behavioral IR *)
let convert filename =
  let content = Edif_parser.read_file filename in

  (* Parse all netlist cells (including top-level) *)
  let all_cells = Edif_parser.parse_all_netlist_cells content in

  (* Also get the top-level module with library cells *)
  let top_level = Edif_parser.parse_schematic filename in

  Printf.printf "Converting EDIF: %s\n" top_level.module_name;
  Printf.printf "  Found %d netlist cells to convert\n" (List.length all_cells);

  (* Convert all cells to modules *)
  let all_modules = List.map convert_cell all_cells in

  Printf.printf "  Converted %d modules\n" (List.length all_modules);

  (* Convert library cells to Behavioral IR format *)
  let library_cells_list =
    Hashtbl.fold (fun cell_name ports acc ->
      let lib_ports = List.map (fun (p : Edif_parser.port_info) ->
        let port_dir = match p.direction with
          | Edif_parser.Input -> `Input
          | Edif_parser.Output -> `Output
          | Edif_parser.Inout -> `Input  (* Treat inout as input for now *)
        in
        {
          Behavioral_ir.port_name = p.name;
          port_direction = port_dir;
          port_width = p.width;
        }
      ) ports in
      (cell_name, lib_ports) :: acc
    ) top_level.library_cells []
  in

  (* Create program with all modules *)
  {
    modules = all_modules;
    library_cells = library_cells_list;
  }
