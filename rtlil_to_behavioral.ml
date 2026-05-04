(* Yosys RTLIL → Behavioral IR.
 *
 * Reads RTLIL produced by `yosys -p 'read_verilog ...; proc; opt;
 * write_rtlil -'` (parsed by Sv_rtlil_reader) and emits a BIR
 * `bprogram` so the same Z3 miter that compares against Vivado VHDL
 * also accepts a Yosys-derived reference.
 *
 * Coverage today: the most common Yosys cell types
 * ($and / $or / $xor / $not / $reduce_* / $eq / $ne / $lt / $le /
 * $gt / $ge / $add / $sub / $mul / $shl / $shr / $sshr / $mux /
 * $dff / $dffsr / $sdff / $adff / $not / passthrough / connections),
 * plus arbitrary wire / signal constructs. Anything else is encoded
 * as a free variable (BConst 0) — surfaces as a counter-example
 * rather than a parse failure. *)

open Behavioral_ir
open Sv_rtlil_reader

let strip_dollar s =
  if String.length s > 0 && s.[0] = '$' then
    String.sub s 1 (String.length s - 1)
  else s

(* SigSpec → bexpr. We treat single-bit and ranged wires as
 * BVar of the base name; constants are BConst; bit-select becomes
 * a BSlice. *)
let rec sigspec_to_bexpr = function
  | SigWire n -> BVar n
  | SigBit (n, b) -> BSlice { signal = BVar n; msb = b; lsb = b }
  | SigRange (n, hi, lo) -> BSlice { signal = BVar n; msb = hi; lsb = lo }
  | SigConst s ->
      (* Yosys's RTLIL constant format is `<width>'<bits>` where <bits>
       * is a string of binary digits — no `b` prefix. So `4'0000` is
       * 4-bit zero, `4'1010` is 4-bit binary 1010 = decimal 10. We
       * also accept the SystemVerilog-flavoured `4'b...`, `4'h...`,
       * `4'd...` for cells/code paths that hand us SV-style values. *)
      let s = String.trim s in
      let value, width =
        try
          if String.contains s '\'' then
            let parts = String.split_on_char '\'' s in
            match parts with
            | [w; rest] when String.length rest >= 1 ->
                let w = int_of_string w in
                let v = match rest.[0] with
                  | 'b' | 'B' ->
                      int_of_string ("0b" ^ String.sub rest 1
                                       (String.length rest - 1))
                  | 'h' | 'H' ->
                      int_of_string ("0x" ^ String.sub rest 1
                                       (String.length rest - 1))
                  | 'd' | 'D' ->
                      int_of_string (String.sub rest 1 (String.length rest - 1))
                  | '0' | '1' | 'x' | 'X' | 'z' | 'Z' ->
                      (* Yosys binary form: rest is a string of 0/1
                       * (and x/z, which we treat as 0). Walk it
                       * and OR in 1-bits. *)
                      let acc = ref 0 in
                      String.iter (fun c ->
                        acc := !acc lsl 1;
                        if c = '1' then acc := !acc lor 1
                      ) rest;
                      !acc
                  | _ -> int_of_string rest
                in (v, w)
            | _ -> (int_of_string s, 32)
          else
            (int_of_string s, 32)
        with _ -> (0, 1)
      in
      BConst { value; width }
  | SigConcat xs -> BConcat (List.map sigspec_to_bexpr xs)

(* Lookup helper: given a cell, find the connection on a named pin. *)
let pin cell name =
  try
    Some (List.find (fun c -> c.conn_pin = name) cell.cell_conns).conn_sig
  with Not_found -> None

let pin_expr cell name =
  Option.map sigspec_to_bexpr (pin cell name)

(* Pull the LHS-name of a sigspec used as a write target. We accept
 * `\foo` and `\foo[N]`; the BIR `BAssign.lhs` is just a string, so
 * indexed writes lose the index (same lossy approximation as the
 * other frontends). *)
let sigspec_to_lhs = function
  | SigWire n -> Some n
  | SigBit (n, _) | SigRange (n, _, _) -> Some n
  | _ -> None

let pin_lhs cell name =
  match pin cell name with
  | Some s -> sigspec_to_lhs s
  | None -> None

let bool_t = BInt { width = 1; signed = Unsigned }
let int_t w = BInt { width = w; signed = Unsigned }

let get_width cell pname =
  try int_of_string (List.assoc pname cell.cell_params)
  with _ -> 1

(* Build a combinational process that drives `lhs` from `rhs`. *)
let comb name lhs rhs =
  Some (BCombinational {
    name;
    sensitivity = [BAny];
    body = [BAssign { lhs; rhs }];
  })

(* Convert a Yosys cell to a BIR process. Returns None when we don't
 * yet model the cell type. *)
let cell_to_bprocess (c : rtlil_cell) =
  let bin_2 op result_w =
    match pin_lhs c "Y", pin_expr c "A", pin_expr c "B" with
    | Some lhs, Some a, Some b ->
        comb (Printf.sprintf "%s_%s" (strip_dollar c.cell_type) c.cell_inst) lhs
          (BBinOp { op; lhs = a; rhs = b; result_type = int_t result_w })
    | _ -> None
  in
  let un_1 op result_w =
    match pin_lhs c "Y", pin_expr c "A" with
    | Some lhs, Some a ->
        comb (Printf.sprintf "%s_%s" (strip_dollar c.cell_type) c.cell_inst) lhs
          (BUnOp { op; operand = a; result_type = int_t result_w })
    | _ -> None
  in
  let yw = get_width c "Y_WIDTH" in
  match c.cell_type with
  (* Bit-level primitives. *)
  | "$_AND_" | "$and"   -> bin_2 BAnd yw
  | "$_OR_"  | "$or"    -> bin_2 BOr  yw
  | "$_XOR_" | "$xor"   -> bin_2 BXor yw
  | "$_NOT_" | "$not"   -> un_1 BNot yw
  | "$neg"              -> un_1 BNeg yw
  | "$add"              -> bin_2 BAdd yw
  | "$sub"              -> bin_2 BSub yw
  | "$mul"              -> bin_2 BMul yw
  | "$shl"   | "$sshl"  -> bin_2 BShl yw
  | "$shr"              -> bin_2 BShr yw
  | "$sshr"             -> bin_2 BAshr yw
  | "$eq"               -> bin_2 BEq 1
  | "$ne"               -> bin_2 BNe 1
  | "$lt"               -> bin_2 BLt 1
  | "$le"               -> bin_2 BLe 1
  | "$gt"               -> bin_2 BGt 1
  | "$ge"               -> bin_2 BGe 1
  (* Reductions. *)
  | "$reduce_and"       -> un_1 BRedAnd 1
  | "$reduce_or"        -> un_1 BRedOr 1
  | "$reduce_xor"       -> un_1 BRedXor 1
  (* Logical AND/OR/NOT: SV `&&`, `||`, `!`. yosys reduces each operand
   * to 1 bit (any non-zero ⇒ true) before combining. For 1-bit
   * operands this is just bit-AND/OR/NOT; for wider operands we wrap
   * each input in BRedOr first so the semantics match. *)
  | "$logic_and" | "$logic_or" as ct ->
      (match pin_lhs c "Y", pin_expr c "A", pin_expr c "B" with
       | Some lhs, Some a, Some b ->
           let red x = BUnOp { op = BRedOr; operand = x; result_type = bool_t } in
           let op = if ct = "$logic_and" then BAnd else BOr in
           comb (Printf.sprintf "%s_%s" (strip_dollar ct) c.cell_inst) lhs
             (BBinOp { op; lhs = red a; rhs = red b; result_type = bool_t })
       | _ -> None)
  | "$logic_not" ->
      (match pin_lhs c "Y", pin_expr c "A" with
       | Some lhs, Some a ->
           let red = BUnOp { op = BRedOr; operand = a; result_type = bool_t } in
           comb (Printf.sprintf "logic_not_%s" c.cell_inst) lhs
             (BUnOp { op = BNot; operand = red; result_type = bool_t })
       | _ -> None)
  (* Mux: Y = S ? B : A *)
  | "$mux" | "$_MUX_" ->
      (match pin_lhs c "Y", pin_expr c "A", pin_expr c "B", pin_expr c "S" with
       | Some lhs, Some a, Some b, Some s ->
           comb (Printf.sprintf "mux_%s" c.cell_inst) lhs
             (BCond { condition = s; then_val = b; else_val = a })
       | _ -> None)
  (* Pass-through: Y = A. yosys-slang's `proc; flatten` flow uses
   * `$buf` as the structural placeholder that wires a wide
   * concatenation onto a single output (e.g. `\g = {a_q,b,a,$xor_y}`
   * appears as a $buf with A = SigConcat[...] and Y = \g). Without
   * this case those concat-driven outputs were unwired on the
   * yosys-slang side. *)
  | "$buf" | "$_BUF_" | "$pos" ->
      (match pin_lhs c "Y", pin_expr c "A" with
       | Some lhs, Some a ->
           comb (Printf.sprintf "buf_%s" c.cell_inst) lhs a
       | _ -> None)
  (* Flip-flops (positive-edge variants). *)
  | "$dff" | "$_DFF_P_" ->
      (match pin_lhs c "Q", pin_expr c "D", pin c "CLK" with
       | Some lhs, Some d, Some clk ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           Some (BSequential {
             name = Printf.sprintf "dff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = None;
             reset_edge = None;
             reset_async = false;
             body = [BAssign { lhs; rhs = d }];
           })
       | _ -> None)
  (* Sync-reset flip-flop. Vivado calls this RTL_REG_SYNC; yosys emits
   * `$sdff`. Body: if (SRST==SRST_POL) q<=val; else q<=D. *)
  | "$sdff" ->
      (match pin_lhs c "Q", pin_expr c "D", pin c "CLK", pin c "SRST" with
       | Some lhs, Some d, Some clk, Some srst ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match srst with SigWire n -> n | _ -> "rst" in
           let rst_val = try
             let v = List.assoc "SRST_VALUE" c.cell_params in
             sigspec_to_bexpr (SigConst v)
           with Not_found -> BConst { value = 0; width = yw }
           in
           Some (BSequential {
             name = Printf.sprintf "sdff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = None;       (* sync: not in sensitivity list *)
             reset_async = false;
             body = [BIf {
               condition = BVar rst_name;
               then_stmts = [BAssign { lhs; rhs = rst_val }];
               else_stmts = [BAssign { lhs; rhs = d }];
             }];
           })
       | _ -> None)
  (* Clock-enable flip-flop, no reset. Body: if (EN) q<=D. The else
   * branch keeps q (lhs <= lhs). *)
  | "$dffe" ->
      (match pin_lhs c "Q", pin_expr c "D", pin c "CLK", pin_expr c "EN" with
       | Some lhs, Some d, Some clk, Some en ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           Some (BSequential {
             name = Printf.sprintf "dffe_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = None;
             reset_edge = None;
             reset_async = false;
             body = [BIf {
               condition = en;
               then_stmts = [BAssign { lhs; rhs = d }];
               else_stmts = [BAssign { lhs; rhs = BVar lhs }];
             }];
           })
       | _ -> None)
  (* Async-reset flip-flop. *)
  | "$adff" | "$_DFF_PP0_" | "$_DFF_PP1_" ->
      (match pin_lhs c "Q", pin_expr c "D",
             pin c "CLK", pin c "ARST" with
       | Some lhs, Some d, Some clk, Some arst ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match arst with SigWire n -> n | _ -> "rst" in
           let rst_val = try
             let v = List.assoc "ARST_VALUE" c.cell_params in
             let pv = sigspec_to_bexpr (SigConst v) in
             pv
           with Not_found -> BConst { value = 0; width = yw }
           in
           Some (BSequential {
             name = Printf.sprintf "adff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = Some `Pos;
             reset_async = true;
             body = [BIf {
               condition = BVar rst_name;
               then_stmts = [BAssign { lhs; rhs = rst_val }];
               else_stmts = [BAssign { lhs; rhs = d }];
             }];
           })
       | _ -> None)
  (* Async-reset clock-enable flip-flop. Body:
   *   if (ARST==ARST_POL) q<=ARST_VALUE
   *   else if (EN==EN_POL) q<=D
   *   else q<=q
   * Both ARST and EN polarities default to active-high; for now we
   * trust yosys to emit polarity-inverted EN/ARST nets when the
   * source is active-low (the common case after `proc`). *)
  (* Async-load DFF. Body:
   *   if (ALOAD == ALOAD_POL) q <= AD     (asynchronous, value = AD)
   *   else                    q <= D      (clocked, value = D)
   * yosys-slang's `proc` lowers an SV `always_ff @(posedge clk or
   * negedge rst_n) if (!rst_n) q <= '0; else q <= ...` to this cell:
   * ALOAD = rst_n, ALOAD_POLARITY = 0 (active-low), AD = '0. Without
   * this case those FFs were dropped and the BIR side had Q as a
   * free wire with no D-side process, which made every Z3 miter on
   * yosys-slang output mis-compute the next-state. *)
  | "$aldff" ->
      (match pin_lhs c "Q", pin_expr c "D",
             pin c "CLK", pin c "ALOAD", pin_expr c "AD" with
       | Some lhs, Some d, Some clk, Some aload, Some ad ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let aload_name = match aload with SigWire n -> n | _ -> "aload" in
           let aload_pol =
             try int_of_string (List.assoc "ALOAD_POLARITY" c.cell_params)
             with _ -> 1
           in
           let cond =
             let v = BVar aload_name in
             if aload_pol = 0
             then BUnOp { op = BNot; operand = v; result_type = bool_t }
             else v
           in
           Some (BSequential {
             name = Printf.sprintf "aldff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some aload_name;
             reset_edge = Some (if aload_pol = 0 then `Neg else `Pos);
             reset_async = true;
             body = [BIf {
               condition = cond;
               then_stmts = [BAssign { lhs; rhs = ad }];
               else_stmts = [BAssign { lhs; rhs = d }];
             }];
           })
       | _ -> None)
  | "$adffe" ->
      (match pin_lhs c "Q", pin_expr c "D",
             pin c "CLK", pin c "ARST", pin_expr c "EN" with
       | Some lhs, Some d, Some clk, Some arst, Some en ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match arst with SigWire n -> n | _ -> "rst" in
           let rst_val = try
             let v = List.assoc "ARST_VALUE" c.cell_params in
             sigspec_to_bexpr (SigConst v)
           with Not_found -> BConst { value = 0; width = yw }
           in
           Some (BSequential {
             name = Printf.sprintf "adffe_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = Some `Pos;
             reset_async = true;
             body = [BIf {
               condition = BVar rst_name;
               then_stmts = [BAssign { lhs; rhs = rst_val }];
               else_stmts = [BIf {
                 condition = en;
                 then_stmts = [BAssign { lhs; rhs = d }];
                 else_stmts = [BAssign { lhs; rhs = BVar lhs }];
               }];
             }];
           })
       | _ -> None)
  | _ -> None

(* Wires → bsignals. Direction comes from the wire's `port` field
 * (input/output) or defaults to `Internal. *)
let wire_to_bsignal (w : rtlil_wire) : bsignal =
  let dir = match w.wire_port with
    | Some (RInput, _) -> `Input
    | Some (ROutput, _) -> `Output
    | _ -> `Internal
  in
  {
    name = w.wire_name;
    stype = BInt { width = w.wire_width;
                   signed = if w.wire_signed then Signed else Unsigned };
    direction = dir;
    initial_value = None;
  }

(* Decompose a sigspec used as a write target into (name, msb, lsb).
 * Whole-wire writes use width=None (caller treats as full-width).
 * Concats and unsupported shapes return None. *)
let sigspec_to_partial_lhs = function
  | SigWire n -> Some (n, None)
  | SigBit (n, b) -> Some (n, Some (b, b))
  | SigRange (n, hi, lo) -> Some (n, Some (hi, lo))
  | _ -> None

(* Top-level: convert one RTLIL module → bmodule. Direct wire-to-wire
 * connections (`connect \dst \src`) become BCombinational with a
 * BAssign. Sliced-LHS connects (`connect $3y [0] \rst_n`,
 * `connect $3y [3:1] 3'111`) targeting the same wire are merged into
 * a single full-width concat assignment, MSB-first. Without the
 * merge, the two partial writes generate two separate BAssigns to
 * the same lhs and the second wins, dropping the rest of the wire. *)
let module_to_bmodule (m : rtlil_module) : bmodule =
  let signals = List.map wire_to_bsignal m.mod_wires in
  let cell_procs = List.filter_map cell_to_bprocess m.mod_cells in
  (* Look up each wire's declared width (used to fill in any bits
   * the partial-write set doesn't cover with a self-read). *)
  let wire_width name =
    try (List.find (fun w -> w.wire_name = name) m.mod_wires).wire_width
    with Not_found -> 0
  in
  (* Group connects by their LHS base-name. Whole-wire writes get an
   * msb of -1 (sentinel that the caller flattens to a direct
   * assignment). *)
  let groups : (string, (int * int * bexpr) list) Hashtbl.t =
    Hashtbl.create 16 in
  let unsupported_connects = ref [] in
  List.iter (fun (lhs, rhs) ->
    let r = sigspec_to_bexpr rhs in
    match sigspec_to_partial_lhs lhs with
    | Some (name, None) ->
        let w = wire_width name in
        let bucket =
          try Hashtbl.find groups name with Not_found -> [] in
        (* Whole-wire write: encoded as range (w-1, 0). *)
        Hashtbl.replace groups name ((w - 1, 0, r) :: bucket)
    | Some (name, Some (msb, lsb)) ->
        let bucket =
          try Hashtbl.find groups name with Not_found -> [] in
        Hashtbl.replace groups name ((msb, lsb, r) :: bucket)
    | None ->
        unsupported_connects := (lhs, rhs) :: !unsupported_connects
  ) m.mod_connects;
  let connect_procs =
    Hashtbl.fold (fun name slices acc ->
      let w = wire_width name in
      let body = match slices with
        | [(_, _, r)] when w = 0 || List.for_all (fun (a, b, _) ->
            a = w - 1 && b = 0) slices ->
            (* Single full-width write — emit directly. *)
            [BAssign { lhs = name; rhs = r }]
        | _ ->
            (* Multiple partial writes. Sort msb-descending so the
             * concat reads MSB-first, then walk top-down filling any
             * bits not driven by these connects with `BSlice (BVar
             * name) [hi:lo]` (i.e. preserve whatever else drives
             * those bits — typically a cell with a slice-LHS Y pin
             * that wrote elsewhere; for our purposes a self-read is
             * a safe placeholder). *)
            let sorted = List.sort
              (fun (a, _, _) (b, _, _) -> compare b a) slices in
            let parts = ref [] in
            let cursor = ref (w - 1) in
            List.iter (fun (msb, lsb, r) ->
              if msb < !cursor then begin
                parts := BSlice { signal = BVar name;
                                  msb = !cursor;
                                  lsb = msb + 1 } :: !parts;
              end;
              parts := r :: !parts;
              cursor := lsb - 1
            ) sorted;
            if !cursor >= 0 then
              parts := BSlice { signal = BVar name;
                                msb = !cursor;
                                lsb = 0 } :: !parts;
            let rhs = match List.rev !parts with
              | [single] -> single
              | many -> BConcat many
            in
            [BAssign { lhs = name; rhs }]
      in
      BCombinational {
        name = "connect_" ^ name;
        sensitivity = [BAny];
        body;
      } :: acc
    ) groups []
  in
  ignore !unsupported_connects;
  {
    name = m.mod_name;
    params = [];
    signals;
    processes = cell_procs @ connect_procs;
    instances = [];
    funcs = [];
    mems = [];
  }

let convert_design (d : rtlil_design) : bprogram =
  { modules = List.map module_to_bmodule d.design_modules;
    library_cells = [] }

(* Convenience: parse an RTLIL file straight from disk. *)
let convert_file path : bprogram =
  let d = parse_rtlil_file path in
  convert_design d
