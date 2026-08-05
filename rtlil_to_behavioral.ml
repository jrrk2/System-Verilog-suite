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
      BConst { value = Z.of_int value; width }
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

(* Slice-aware lvalue: returns (name, Some (msb, lsb)) when the pin
 * targets a wire slice (yosys splits multi-bit FFs across cells, each
 * driving a different slice of the same parent wire), or
 * (name, None) for whole-wire writes.  Concats/unsupported sigspecs
 * return None.  Callers use this to decide between a plain BAssign
 * (whole-wire) and a @slice_write BCallStmt (partial). *)
let pin_lhs_slice cell name =
  match pin cell name with
  | Some (SigWire n) -> Some (n, None)
  | Some (SigBit (n, b)) -> Some (n, Some (b, b))
  | Some (SigRange (n, hi, lo)) -> Some (n, Some (hi, lo))
  | _ -> None

(* Build either a plain `lhs := rhs` or, when the cell drives a slice
 * of the parent wire, `@slice_write(lhs, hi, lo, rhs)` so multiple
 * sliced drivers can coexist without clobbering each other.
 *
 * When the rhs is a bare `BVar name` (self-read, used by FFs in
 * their "keep" branches) and the cell writes only a slice, the rhs
 * width must shrink to the slice width — otherwise the merge pass
 * concats a wider value than the slice it represents.  Wrap with
 * BSlice in that case. *)
let assign_or_slice_write name slice rhs =
  match slice with
  | None -> BAssign { lhs = name; rhs }
  | Some (hi, lo) ->
      let rhs = match rhs with
        | BVar n when n = name ->
            BSlice { signal = BVar n; msb = hi; lsb = lo }
        | _ -> rhs
      in
      BCallStmt {
        func = "@slice_write";
        args = [BVar name;
                BConst { value = Z.of_int hi; width = 32 };
                BConst { value = Z.of_int lo; width = 32 };
                rhs] }

let bool_t = BInt { width = 1; signed = Unsigned }
let int_t w = BInt { width = w; signed = Unsigned }

let get_width cell pname =
  try int_of_string (List.assoc pname cell.cell_params)
  with _ -> 1

(* Build a combinational process that drives `lhs` from `rhs`.  When
 * the cell's Y pin is sliced, emit @slice_write so the cross-cell
 * merger can stitch sibling cells writing other slices of the same
 * parent wire (the alternative — multiple BCombinationals all
 * writing the full parent — clobbers via last-write-wins). *)
let comb_sliced name (lhs, slice) rhs =
  Some (BCombinational {
    name;
    sensitivity = [BAny];
    body = [assign_or_slice_write lhs slice rhs];
  })

(* Bit-width of one concat chunk (for placing it in the RHS value). *)
let sigchunk_width = function
  | SigBit _ -> Some 1
  | SigRange (_, hi, lo) -> Some (abs (hi - lo) + 1)
  | SigConst s ->
      let s = String.trim s in
      (match String.index_opt s '\'' with
       | Some i -> (try Some (int_of_string (String.sub s 0 i)) with _ -> None)
       | None -> None)
  | SigWire _ -> None            (* unknown width without the wire table *)
  | SigConcat _ -> None          (* nested concat: unsupported *)

(* Distribute a computed `rhs` value across an output that is a
 * concatenation of scattered bit-selects.  Yosys emits this after
 * const-cell replacement splits e.g. `a ^ 8'ha5` into a $not on the
 * set-bits with Y = {q[7],q[5],q[2],q[0]}; the old reader returned
 * None here (SigConcat unsupported by pin_lhs_slice) and silently
 * DROPPED the cell, zeroing those bits.  RTLIL concat is MSB-first,
 * so we walk the reversed list assigning bit offsets from the LSB and
 * slice-write each chunk from the matching bits of `rhs`.  Returns the
 * per-chunk writes (LSB first) or None if any chunk width is unknown. *)
let concat_out_writes chunks rhs =
  let rec go off acc = function
    | [] -> Some (List.rev acc)
    | ch :: rest ->
        (match sigchunk_width ch with
         | None -> None
         | Some w ->
             let src = BSlice { signal = rhs; msb = off + w - 1; lsb = off } in
             (match ch with
              | SigBit (n, b) ->
                  go (off + w) (assign_or_slice_write n (Some (b, b)) src :: acc) rest
              | SigRange (n, hi, lo) ->
                  go (off + w) (assign_or_slice_write n (Some (hi, lo)) src :: acc) rest
              | SigConst _ -> go (off + w) acc rest   (* const target: consume, don't write *)
              | SigWire _ | SigConcat _ -> None))
  in
  go 0 [] (List.rev chunks)

(* Emit the combinational process driving cell `c`'s output pin `pn`
 * from `rhs`, transparently handling a single (possibly sliced) wire
 * or a concat of scattered slices. *)
let comb_out c pn label rhs =
  match pin c pn with
  | Some (SigConcat chunks) ->
      (match concat_out_writes chunks rhs with
       | Some ((_ :: _) as stmts) ->
           Some (BCombinational { name = label; sensitivity = [BAny]; body = stmts })
       | _ -> None)
  | Some _ ->
      (match pin_lhs_slice c pn with
       | Some lhs_slice -> comb_sliced label lhs_slice rhs
       | None -> None)
  | None -> None

(* Convert a Yosys cell to a BIR process. Returns None when we don't
 * yet model the cell type. *)
let cell_to_bprocess (c : rtlil_cell) =
  let bin_2 op result_w =
    match pin_expr c "A", pin_expr c "B" with
    | Some a, Some b ->
        comb_out c "Y" (Printf.sprintf "%s_%s" (strip_dollar c.cell_type) c.cell_inst)
          (BBinOp { op; lhs = a; rhs = b; result_type = int_t result_w })
    | _ -> None
  in
  let un_1 op result_w =
    match pin_expr c "A" with
    | Some a ->
        comb_out c "Y" (Printf.sprintf "%s_%s" (strip_dollar c.cell_type) c.cell_inst)
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
  (* Reductions.  `$reduce_bool` is yosys's "is any bit set" cell —
     semantically equivalent to a 1-bit OR-reduction. *)
  | "$reduce_and"       -> un_1 BRedAnd 1
  | "$reduce_or" | "$reduce_bool" -> un_1 BRedOr 1
  | "$reduce_xor"       -> un_1 BRedXor 1
  (* Logical AND/OR/NOT: SV `&&`, `||`, `!`. yosys reduces each operand
   * to 1 bit (any non-zero ⇒ true) before combining. For 1-bit
   * operands this is just bit-AND/OR/NOT; for wider operands we wrap
   * each input in BRedOr first so the semantics match. *)
  | "$logic_and" | "$logic_or" as ct ->
      (match pin_expr c "A", pin_expr c "B" with
       | Some a, Some b ->
           let red x = BUnOp { op = BRedOr; operand = x; result_type = bool_t } in
           let op = if ct = "$logic_and" then BAnd else BOr in
           comb_out c "Y" (Printf.sprintf "%s_%s" (strip_dollar ct) c.cell_inst)
             (BBinOp { op; lhs = red a; rhs = red b; result_type = bool_t })
       | _ -> None)
  | "$logic_not" ->
      (match pin_expr c "A" with
       | Some a ->
           let red = BUnOp { op = BRedOr; operand = a; result_type = bool_t } in
           comb_out c "Y" (Printf.sprintf "logic_not_%s" c.cell_inst)
             (BUnOp { op = BNot; operand = red; result_type = bool_t })
       | _ -> None)
  (* Mux: Y = S ? B : A *)
  | "$mux" | "$_MUX_" ->
      (match pin_expr c "A", pin_expr c "B", pin_expr c "S" with
       | Some a, Some b, Some s ->
           comb_out c "Y" (Printf.sprintf "mux_%s" c.cell_inst)
             (BCond { condition = s; then_val = b; else_val = a })
       | _ -> None)
  (* Parallel mux: case-statement lowering.  yosys emits $pmux for
     `case (...) ... endcase`.  Pins:
       A: default value (WIDTH bits, used when no S bit is set)
       B: concatenation of S_WIDTH chunks, each WIDTH bits — case-arm values
       S: S_WIDTH one-hot select bits — chunk i is taken when S[i]=1
     Semantics: Y = S[0] ? B[WIDTH-1:0] : S[1] ? B[2*WIDTH-1:WIDTH] : ... : A *)
  | "$pmux" ->
      (match pin_expr c "A", pin_expr c "B", pin_expr c "S" with
       | Some a, Some b, Some s ->
           let yw = get_width c "WIDTH" in
           let sw = get_width c "S_WIDTH" in
           if yw <= 0 || sw <= 0 then None
           else
             let acc = ref a in
             for i = sw - 1 downto 0 do
               let s_i = BSlice { signal = s; msb = i; lsb = i } in
               let b_i = BSlice { signal = b;
                                  msb = (i + 1) * yw - 1;
                                  lsb = i * yw } in
               acc := BCond { condition = s_i;
                              then_val = b_i;
                              else_val = !acc }
             done;
             comb_out c "Y" (Printf.sprintf "pmux_%s" c.cell_inst) !acc
       | _ -> None)
  (* Pass-through: Y = A. yosys-slang's `proc; flatten` flow uses
   * `$buf` as the structural placeholder that wires a wide
   * concatenation onto a single output (e.g. `\g = {a_q,b,a,$xor_y}`
   * appears as a $buf with A = SigConcat[...] and Y = \g). Without
   * this case those concat-driven outputs were unwired on the
   * yosys-slang side. *)
  | "$buf" | "$_BUF_" | "$pos" ->
      (match pin_expr c "A" with
       | Some a ->
           comb_out c "Y" (Printf.sprintf "buf_%s" c.cell_inst) a
       | _ -> None)
  (* Flip-flops (positive-edge variants). *)
  | "$dff" | "$_DFF_P_" ->
      (match pin_lhs_slice c "Q", pin_expr c "D", pin c "CLK" with
       | Some (lhs, slice), Some d, Some clk ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           Some (BSequential {
             name = Printf.sprintf "dff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = None;
             reset_edge = None;
             reset_async = false;
             body = [assign_or_slice_write lhs slice d];
             blocking_vars = [];
           })
       | _ -> None)
  (* Sync-reset flip-flop. Vivado calls this RTL_REG_SYNC; yosys emits
   * `$sdff`. Body: if (SRST==SRST_POL) q<=val; else q<=D.
   * SRST_POLARITY=0 means active-low — invert the condition. *)
  | "$sdff" ->
      (match pin_lhs_slice c "Q", pin_expr c "D", pin c "CLK", pin c "SRST" with
       | Some (lhs, slice), Some d, Some clk, Some srst ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match srst with SigWire n -> n | _ -> "rst" in
           let rst_val = try
             let v = List.assoc "SRST_VALUE" c.cell_params in
             sigspec_to_bexpr (SigConst v)
           with Not_found -> BConst { value = Z.zero; width = yw }
           in
           let rst_pol =
             try int_of_string (List.assoc "SRST_POLARITY" c.cell_params)
             with _ -> 1
           in
           let rst_cond =
             let v = BVar rst_name in
             if rst_pol = 0
             then BUnOp { op = BNot; operand = v; result_type = bool_t }
             else v
           in
           Some (BSequential {
             name = Printf.sprintf "sdff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = None;       (* sync: not in sensitivity list *)
             reset_async = false;
             body = [BIf {
               condition = rst_cond;
               then_stmts = [assign_or_slice_write lhs slice rst_val];
               else_stmts = [assign_or_slice_write lhs slice d];
             }];
             blocking_vars = [];
           })
       | _ -> None)
  (* Synchronous reset WITH clock enable.  Two cells, differing only in
   * PRIORITY -- and getting that backwards silently yields a register that is
   * wrong only in the cycle where reset and enable disagree, which is the
   * hardest kind of bug to see.  From yosys simlib.v verbatim:
   *
   *   $sdffe   always @(posedge CLK) if (SRST) Q <= VAL;
   *                                  else if (EN) Q <= D;      reset > enable
   *   $sdffce  always @(posedge CLK) if (EN) begin
   *                                    if (SRST) Q <= VAL; else Q <= D; end
   *                                                                enable > reset
   *
   * Neither was handled, so every $sdffe in the design was DROPPED: reading
   * axis_gmii_rx through the RTLIL front ends gave 16 registers where verible,
   * slang, Vivado -rtl and the RTLIL itself all say 27 -- the missing 11 being
   * exactly the $sdffe count.  That made synlig (Surelog) and the yosys front
   * end useless as register-correspondence peers for a reason that was in our
   * importer, not in either reader.
   *
   * EN_POLARITY is honoured here (the plain $dffe handler above assumes active
   * high); yosys emits active-low enables after some optimisation passes. *)
  | "$sdffe" | "$sdffce" ->
      (match pin_lhs_slice c "Q", pin_expr c "D", pin c "CLK",
             pin c "SRST", pin_expr c "EN" with
       | Some (lhs, slice), Some d, Some clk, Some srst, Some en ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match srst with SigWire n -> n | _ -> "rst" in
           let rst_val =
             try sigspec_to_bexpr (SigConst (List.assoc "SRST_VALUE" c.cell_params))
             with Not_found -> BConst { value = Z.zero; width = yw } in
           let pol name dflt =
             try int_of_string (List.assoc name c.cell_params) with _ -> dflt in
           let apply_pol p e =
             if p = 0 then BUnOp { op = BNot; operand = e; result_type = bool_t }
             else e in
           let rst_cond = apply_pol (pol "SRST_POLARITY" 1) (BVar rst_name) in
           let en_cond  = apply_pol (pol "EN_POLARITY" 1) en in
           let put e  = assign_or_slice_write lhs slice e in
           let keep   = assign_or_slice_write lhs slice (BVar lhs) in
           let body =
             if c.cell_type = "$sdffe" then
               [BIf { condition = rst_cond;
                      then_stmts = [put rst_val];
                      else_stmts = [BIf { condition = en_cond;
                                          then_stmts = [put d];
                                          else_stmts = [keep] }] }]
             else
               [BIf { condition = en_cond;
                      then_stmts = [BIf { condition = rst_cond;
                                          then_stmts = [put rst_val];
                                          else_stmts = [put d] }];
                      else_stmts = [keep] }] in
           Some (BSequential {
             name = Printf.sprintf "sdffe_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = None;       (* sync: not in the sensitivity list *)
             reset_async = false;
             body;
             blocking_vars = [];
           })
       | _ -> None)
  (* Clock-enable flip-flop, no reset. Body: if (EN) q<=D. The else
   * branch keeps q (lhs <= lhs). *)
  | "$dffe" ->
      (match pin_lhs_slice c "Q", pin_expr c "D", pin c "CLK", pin_expr c "EN" with
       | Some (lhs, slice), Some d, Some clk, Some en ->
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
               then_stmts = [assign_or_slice_write lhs slice d];
               else_stmts = [assign_or_slice_write lhs slice (BVar lhs)];
             }];
             blocking_vars = [];
           })
       | _ -> None)
  (* Async-reset flip-flop. *)
  | "$adff" | "$_DFF_PP0_" | "$_DFF_PP1_" ->
      (match pin_lhs_slice c "Q", pin_expr c "D",
             pin c "CLK", pin c "ARST" with
       | Some (lhs, slice), Some d, Some clk, Some arst ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match arst with SigWire n -> n | _ -> "rst" in
           let rst_val = try
             let v = List.assoc "ARST_VALUE" c.cell_params in
             let pv = sigspec_to_bexpr (SigConst v) in
             pv
           with Not_found -> BConst { value = Z.zero; width = yw }
           in
           let rst_pol =
             try int_of_string (List.assoc "ARST_POLARITY" c.cell_params)
             with _ -> 1
           in
           let rst_cond =
             let v = BVar rst_name in
             if rst_pol = 0
             then BUnOp { op = BNot; operand = v; result_type = bool_t }
             else v
           in
           Some (BSequential {
             name = Printf.sprintf "adff_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = Some (if rst_pol = 0 then `Neg else `Pos);
             reset_async = true;
             body = [BIf {
               condition = rst_cond;
               then_stmts = [assign_or_slice_write lhs slice rst_val];
               else_stmts = [assign_or_slice_write lhs slice d];
             }];
             blocking_vars = [];
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
      (match pin_lhs_slice c "Q", pin_expr c "D",
             pin c "CLK", pin c "ALOAD", pin_expr c "AD" with
       | Some (lhs, slice), Some d, Some clk, Some aload, Some ad ->
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
               then_stmts = [assign_or_slice_write lhs slice ad];
               else_stmts = [assign_or_slice_write lhs slice d];
             }];
             blocking_vars = [];
           })
       | _ -> None)
  | "$adffe" ->
      (match pin_lhs_slice c "Q", pin_expr c "D",
             pin c "CLK", pin c "ARST", pin_expr c "EN" with
       | Some (lhs, slice), Some d, Some clk, Some arst, Some en ->
           let clk_name = match clk with SigWire n -> n | _ -> "clk" in
           let rst_name = match arst with SigWire n -> n | _ -> "rst" in
           let rst_val = try
             let v = List.assoc "ARST_VALUE" c.cell_params in
             sigspec_to_bexpr (SigConst v)
           with Not_found -> BConst { value = Z.zero; width = yw }
           in
           let rst_pol =
             try int_of_string (List.assoc "ARST_POLARITY" c.cell_params)
             with _ -> 1
           in
           let rst_cond =
             let v = BVar rst_name in
             if rst_pol = 0
             then BUnOp { op = BNot; operand = v; result_type = bool_t }
             else v
           in
           let en_pol =
             try int_of_string (List.assoc "EN_POLARITY" c.cell_params)
             with _ -> 1
           in
           let en_cond =
             if en_pol = 0
             then BUnOp { op = BNot; operand = en; result_type = bool_t }
             else en
           in
           Some (BSequential {
             name = Printf.sprintf "adffe_%s" c.cell_inst;
             clock = clk_name;
             clock_edge = `Pos;
             reset = Some rst_name;
             reset_edge = Some (if rst_pol = 0 then `Neg else `Pos);
             reset_async = true;
             body = [BIf {
               condition = rst_cond;
               then_stmts = [assign_or_slice_write lhs slice rst_val];
               else_stmts = [BIf {
                 condition = en_cond;
                 then_stmts = [assign_or_slice_write lhs slice d];
                 else_stmts = [assign_or_slice_write lhs slice (BVar lhs)];
               }];
             }];
             blocking_vars = [];
           })
       | _ -> None)
  (* D-latch. `$dlatch` is a level-sensitive transparent latch:
       Q := EN_active ? D : prev_Q
     Yosys emits these for SV `always_latch` blocks.  Model in BIR
     as a BCombinational with an if-then keeping the prior value in
     the else — Z3 sees q_next = en ? d : q.  Latches in SV-RTL are
     usually unintentional; treating them this way lets the miter
     match other frontends that lower the same construct. *)
  | "$dlatch" ->
      (match pin_lhs_slice c "Q", pin_expr c "D", pin_expr c "EN" with
       | Some (lhs, slice), Some d, Some en ->
           let en_pol =
             try int_of_string (List.assoc "EN_POLARITY" c.cell_params)
             with _ -> 1
           in
           let en_cond =
             if en_pol = 0
             then BUnOp { op = BNot; operand = en; result_type = bool_t }
             else en
           in
           Some (BCombinational {
             name = Printf.sprintf "dlatch_%s" c.cell_inst;
             sensitivity = [BAny];
             body = [BIf {
               condition = en_cond;
               then_stmts = [assign_or_slice_write lhs slice d];
               else_stmts = [assign_or_slice_write lhs slice (BVar lhs)];
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
    initial_value = None; attrs = []; 
  }

(* Decompose a sigspec used as a write target into (name, msb, lsb).
 * Whole-wire writes use width=None (caller treats as full-width).
 * Concats and unsupported shapes return None. *)
let sigspec_to_partial_lhs = function
  | SigWire n -> Some (n, None)
  | SigBit (n, b) -> Some (n, Some (b, b))
  | SigRange (n, hi, lo) -> Some (n, Some (hi, lo))
  | _ -> None

(* ── Cross-cell sliced-FF merge ─────────────────────────────────────
   yosys's `opt -fast` (and the underlying opt_dff sub-pass) splits a
   single multi-bit always-block into one $adff/$adffe/$dff/$dffe per
   slice of the parent wire — e.g. iCounter[3:0] driven by an $adffe,
   iCounter[4] driven by an $adff.  Our cell handlers honour the Q
   pin's slice info and emit @slice_write inside the BSequential body,
   but behavioral_to_z3 doesn't model @slice_write — without merging,
   the Z3 lowering silently drops both slices and iCounter has no
   driver, mis-computing every next-state.

   Strategy: group BSequentials by clocking signature (clock name +
   edge, reset name + edge + sync/async); for each group, gather every
   @slice_write leaf with its target name, slice bounds, and full
   cond-chain from the body's root.  If a group writes a single target
   with disjoint slices that together cover the wire's width, rebuild
   the body as a single full-width BAssign whose RHS is the BConcat of
   each cell's contribution per cond-chain.  Other groups are left
   intact (the per-cell processes pass through unchanged).             *)

type clock_sig = {
  clock : string;
  clock_edge : [`Pos | `Neg];
  reset : string option;
  reset_edge : [`Pos | `Neg] option;
  reset_async : bool;
}

let clock_sig_of (s : sensitivity list) = ignore s; ()
let clock_sig_of_seq (clock, clock_edge, reset, reset_edge, reset_async) =
  { clock; clock_edge; reset; reset_edge; reset_async }

(* Walk a stmt list, accumulating @slice_write leaves with the
   sequence of (cond, is_then) branches taken to reach each one. *)
type slice_leaf = {
  cond_chain : (bexpr * bool) list;
  target : string;
  hi : int;
  lo : int;
  rhs : bexpr;
}

let rec collect_slices chain acc = function
  | [] -> acc
  | s :: rest ->
      let acc = collect_one chain acc s in
      collect_slices chain acc rest

and collect_one chain acc = function
  | BCallStmt { func = "@slice_write";
                args = [BVar t;
                        BConst { value = hi; _ };
                        BConst { value = lo; _ };
                        rhs] } ->
      { cond_chain = List.rev chain; target = t; hi = Z.to_int hi; lo = Z.to_int lo; rhs } :: acc
  | BBlock ss -> collect_slices chain acc ss
  | BIf { condition; then_stmts; else_stmts } ->
      let acc = collect_slices ((condition, true) :: chain) acc then_stmts in
      collect_slices ((condition, false) :: chain) acc else_stmts
  | _ -> acc

(* Evaluate a body under a cond-chain, returning the slice-write rhs
   the cell would commit, or None if the chain doesn't lead to a
   slice-write leaf.  Used to fill in a cell's value at a refined
   cond-chain produced by another cell's branching. *)
let rec eval_body_at chain stmts =
  let rec walk = function
    | [] -> None
    | s :: rest ->
        (match walk_one s with
         | Some r -> Some r
         | None -> walk rest)
  and walk_one = function
    | BCallStmt { func = "@slice_write";
                  args = [_; _; _; rhs] } -> Some rhs
    | BBlock ss -> walk ss
    | BIf { condition; then_stmts; else_stmts } ->
        (match List.assoc_opt condition chain with
         | Some true -> walk then_stmts
         | Some false -> walk else_stmts
         | None ->
             (* Cell doesn't case-split on this condition; if both
                branches yield the same value, take it.  Otherwise
                we can't summarise. *)
             (match walk then_stmts, walk else_stmts with
              | Some a, Some b when a = b -> Some a
              | _ -> None))
    | _ -> None
  in
  walk stmts

(* Given a list of leaves from one cell, return the unique cond-chains
   (in body order). *)
let unique_chains leaves =
  let seen = Hashtbl.create 8 in
  List.fold_left (fun acc l ->
    if Hashtbl.mem seen l.cond_chain then acc
    else (Hashtbl.add seen l.cond_chain (); l.cond_chain :: acc))
    [] leaves
  |> List.rev

(* Reconstruct a BIf-tree from a list of (cond_chain, leaf_stmt)
   pairs.  Walks the chains as a decision tree, building BIf nodes
   for each split point.  Assumes chains share a common prefix
   structure (the first differing condition becomes the BIf). *)
let rec rebuild_tree groups =
  match groups with
  | [] -> []
  | [(([], stmt))] -> [stmt]
  | _ ->
      (* All entries should start with the same head condition; split
         by then/else and recurse. *)
      let heads = List.map (fun (chain, _) -> List.nth_opt chain 0) groups in
      let common_cond = match heads with
        | Some (c, _) :: _ -> Some c
        | _ -> None
      in
      (match common_cond with
       | None ->
           (* Mixed empty + non-empty: just emit the first stmt; loses
              info but is safe. *)
           (match groups with
            | (_, s) :: _ -> [s]
            | [] -> [])
       | Some c ->
           let strip = List.map (fun (chain, s) ->
             match chain with
             | (c0, _) :: rest when c0 = c -> rest, s
             | _ -> chain, s) groups in
           let then_g = List.filter (fun (chain, _) ->
             match chain with (_, true) :: _ -> false | _ -> true) groups in
           let else_g = List.filter (fun (chain, _) ->
             match chain with (_, false) :: _ -> false | _ -> true) groups in
           ignore strip;
           let then_stmts = rebuild_tree (List.map (fun (chain, s) ->
             match chain with (_, true) :: rest -> rest, s | _ -> chain, s)
             then_g) in
           let else_stmts = rebuild_tree (List.map (fun (chain, s) ->
             match chain with (_, false) :: rest -> rest, s | _ -> chain, s)
             else_g) in
           [BIf { condition = c;
                  then_stmts; else_stmts }])

(* Convert a cell's body (a BIf tree whose leaves are @slice_write)
   into a single bexpr that returns the slice's next-state value.
   Each BIf becomes a BCond.  Returns None when the body has shape
   we don't recognise (multiple leaves at the same conditional path,
   non-slice-write leaves, etc.). *)
let rec body_to_slice_expr stmts =
  match stmts with
  | [BCallStmt { func = "@slice_write";
                 args = [_; _; _; rhs] }] -> Some rhs
  | [BBlock ss] -> body_to_slice_expr ss
  | [BIf { condition; then_stmts; else_stmts }] ->
      (match body_to_slice_expr then_stmts,
             body_to_slice_expr else_stmts with
       | Some t, Some e ->
           Some (BCond { condition; then_val = t; else_val = e })
       | _ -> None)
  | _ -> None

(* Build the merged body of a group of co-clocked BSequentials that
   together write disjoint slices of a single target signal.  Returns
   None when slices aren't a clean disjoint cover or any cell body
   doesn't reduce to a single BCond chain. *)
let merge_one_group ~width ~target processes =
  (* Each cell: extract its slice span (from its first leaf) and its
     body-as-expression.  Synth tools always emit one slice per
     cell, so all leaves of one cell share (hi, lo). *)
  let cell_info = List.map (fun proc ->
    let body = match proc with
      | BSequential { body; _ } -> body
      | BCombinational { body; _ } -> body
    in
    let leaves = collect_slices [] [] body in
    match leaves with
    | [] -> None
    | l0 :: _ ->
        if List.for_all (fun l ->
              l.target = target && l.hi = l0.hi && l.lo = l0.lo) leaves
        then
          (match body_to_slice_expr body with
           | Some expr -> Some (l0.hi, l0.lo, expr)
           | None -> None)
        else None) processes
  in
  if List.exists Option.is_none cell_info then None
  else
    let cells = List.map Option.get cell_info in
    (* Sort by descending hi (msb first for concat). *)
    let sorted = List.sort (fun (a, _, _) (b, _, _) -> compare b a) cells in
    (* Disjoint-cover check from msb down to 0. *)
    let rec check_cover cursor = function
      | [] -> cursor = -1
      | (hi, lo, _) :: rest ->
          if hi = cursor && lo <= hi
          then check_cover (lo - 1) rest
          else false
    in
    if not (check_cover (width - 1) sorted) then None
    else
      let parts = List.map (fun (_, _, expr) -> expr) sorted in
      let rhs = match parts with
        | [single] -> single
        | many -> BConcat many in
      Some [BAssign { lhs = target; rhs }]

let merge_sliced_ff_processes ~wire_widths processes =
  let seq_key = function
    | BSequential { clock; clock_edge; reset; reset_edge; reset_async; _ } ->
        Some (clock_sig_of_seq (clock, clock_edge, reset, reset_edge, reset_async))
    | BCombinational _ -> None
  in
  (* Bucket BSequentials by clocking signature.  BCombinationals
     don't share clocks, so group them by target instead — combine
     all combinational processes whose @slice_write writes touch the
     same wire.  We tag the comb bucket with a synthetic clock_sig
     where `clock` carries the target name; that's only used as a
     hash key, not interpreted as a signal. *)
  let by_clock : (clock_sig, bprocess list) Hashtbl.t = Hashtbl.create 8 in
  let body_of = function
    | BSequential { body; _ } | BCombinational { body; _ } -> body
  in
  let target_of_proc p =
    let leaves = collect_slices [] [] (body_of p) in
    match leaves with
    | [] -> None
    | l0 :: _ ->
        if List.for_all (fun l -> l.target = l0.target) leaves
        then Some l0.target else None
  in
  List.iter (fun p ->
    match seq_key p with
    | Some k ->
        let cur = try Hashtbl.find by_clock k with Not_found -> [] in
        Hashtbl.replace by_clock k (p :: cur)
    | None ->
        (* Combinational: only consider for merging if its body has
           a @slice_write leaf with a recoverable target. *)
        (match target_of_proc p with
         | None -> ()
         | Some t ->
             let synthetic = {
               clock = "@comb:" ^ t; clock_edge = `Pos;
               reset = None; reset_edge = None; reset_async = false } in
             let cur = try Hashtbl.find by_clock synthetic with Not_found -> [] in
             Hashtbl.replace by_clock synthetic (p :: cur))) processes;
  (* Find groups whose every member's @slice_write leaves all reference
     the same target.  Single-process buckets are still pushed through
     to normalise their bodies (replacing @slice_write with an
     RMW-style BAssign so behavioral_to_z3 sees a concrete driver). *)
  let merged : (bprocess, bprocess) Hashtbl.t = Hashtbl.create 8 in
  let dropped : bprocess list ref = ref [] in
  Hashtbl.iter (fun k procs ->
    if List.length procs < 2 then ()
    else begin
      ignore k;
      (* All procs must have a single target; collect the target sets
         per proc; if they all share exactly one target, merge. *)
      let targets = List.map (fun p ->
        let body = match p with
          | BSequential { body; _ } | BCombinational { body; _ } -> body in
        let leaves = collect_slices [] [] body in
        match leaves with
        | [] -> None
        | l0 :: _ ->
            if List.for_all (fun l -> l.target = l0.target) leaves
            then Some l0.target else None) procs in
      let common_target = match targets with
        | Some t :: rest when List.for_all (fun o -> o = Some t) rest -> Some t
        | _ -> None
      in
      (match common_target with
      | None -> ()
      | Some target ->
          let w = try List.assoc target wire_widths with Not_found -> 0 in
          if w = 0 then ()
          else
            (match merge_one_group ~width:w ~target procs with
            | None -> ()
            | Some body ->
                let template = List.hd procs in
                let merged_proc =
                  (match template with
                  | BSequential s ->
                      BSequential { s with
                        name = Printf.sprintf "merged_%s" target;
                        body }
                  | BCombinational c ->
                      BCombinational { c with
                        name = Printf.sprintf "merged_%s" target;
                        body })
                in
                Hashtbl.add merged template merged_proc;
                List.iter (fun p -> dropped := p :: !dropped)
                  (List.tl procs)))
    end) by_clock;
  (* Walk the original list, replacing merged-template procs and
     dropping the rest of each merged group. *)
  let dropset = !dropped in
  List.filter_map (fun p ->
    if List.exists (fun d -> d == p) dropset then None
    else match Hashtbl.find_opt merged p with
      | Some replacement -> Some replacement
      | None -> Some p) processes

(* Top-level: convert one RTLIL module → bmodule. Direct wire-to-wire
 * connections (`connect \dst \src`) become BCombinational with a
 * BAssign. Sliced-LHS connects (`connect $3y [0] \rst_n`,
 * `connect $3y [3:1] 3'111`) targeting the same wire are merged into
 * a single full-width concat assignment, MSB-first. Without the
 * merge, the two partial writes generate two separate BAssigns to
 * the same lhs and the second wins, dropping the rest of the wire. *)
(* Clean a $memrd/$memwr MEMID param ("\\mem" — quoted, backslash-escaped) down
   to the bare memory name ("mem"). *)
let clean_memid s =
  let s = String.trim s in
  let s = if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"'
          then String.sub s 1 (String.length s - 2) else s in
  let strip1 s = if String.length s >= 1 && s.[0] = '\\'
                 then String.sub s 1 (String.length s - 1) else s in
  strip1 (strip1 s)

(* $memrd / $memwr ports → BIR.  The memory is a BArray signal (created from the
   $mem_decl synthetic cell); a write is a clocked @mem_write with the port's
   per-bit EN folded into a read-modify-write mask (so `if (wen) m[a] <= d`
   lowers correctly — EN = {W{wen}} makes it a no-op when wen=0); an async read
   ($memrd CLK_ENABLE=0) is a combinational `data := mem[addr]`. *)
let mem_cell_to_bprocess mems_tbl (c : rtlil_cell) =
  let is_pfx p = let lp = String.length p in
    String.length c.cell_type >= lp && String.sub c.cell_type 0 lp = p in
  let memid () = clean_memid (try List.assoc "MEMID" c.cell_params with Not_found -> "") in
  if is_pfx "$memwr" then
    match pin_expr c "ADDR", pin_expr c "DATA" with
    | Some addr, Some data ->
        let mem = memid () in
        let w = (try fst (Hashtbl.find mems_tbl mem) with Not_found -> 0) in
        let u = BInt { width = (if w > 0 then w else 32); signed = Unsigned } in
        let value = match pin_expr c "EN" with
          | Some en ->
              let old = BSelect { array = BVar mem; index = addr } in
              BBinOp { op = BOr;
                lhs = BBinOp { op = BAnd; lhs = old;
                               rhs = BUnOp { op = BNot; operand = en; result_type = u };
                               result_type = u };
                rhs = BBinOp { op = BAnd; lhs = data; rhs = en; result_type = u };
                result_type = u }
          | None -> data in
        let write = BCallStmt { func = "@mem_write"; args = [BVar mem; addr; value] } in
        (match pin c "CLK" with
         | Some (SigWire clk) ->
             Some (BSequential { name = "memwr_" ^ c.cell_inst; clock = clk;
               clock_edge = `Pos; reset = None; reset_edge = None; reset_async = false;
               body = [write]; blocking_vars = [] })
         | _ -> Some (BCombinational { name = "memwr_" ^ c.cell_inst;
                                       sensitivity = [BAny]; body = [write] }))
    | _ -> None
  else if is_pfx "$memrd" then
    match pin_expr c "ADDR", pin_lhs c "DATA" with
    | Some addr, Some dname ->
        let read = BAssign { lhs = dname;
                             rhs = BSelect { array = BVar (memid ()); index = addr } } in
        let clk_en = (try List.assoc "CLK_ENABLE" c.cell_params with Not_found -> "0") in
        (match (clk_en = "1'1" || clk_en = "1"), pin c "CLK" with
         | true, Some (SigWire clk) ->
             Some (BSequential { name = "memrd_" ^ c.cell_inst; clock = clk;
               clock_edge = `Pos; reset = None; reset_edge = None; reset_async = false;
               body = [read]; blocking_vars = [] })
         | _ -> Some (BCombinational { name = "memrd_" ^ c.cell_inst;
                                       sensitivity = [BAny]; body = [read] }))
    | _ -> None
  else None

let module_to_bmodule (m : rtlil_module) : bmodule =
  (* Memory declarations arrive as synthetic $mem_decl cells: create one BArray
     signal each and index (width,size) by name for the port handlers. *)
  let mem_param c k = try int_of_string (List.assoc k c.cell_params) with _ -> 0 in
  let mems_tbl : (string, int * int) Hashtbl.t = Hashtbl.create 8 in
  let mem_signals = List.filter_map (fun (c : rtlil_cell) ->
    if c.cell_type = "$mem_decl" then begin
      let w = mem_param c "WIDTH" and sz = mem_param c "SIZE" in
      Hashtbl.replace mems_tbl c.cell_inst (w, sz);
      Some { name = c.cell_inst;
             stype = BArray { element = BInt { width = w; signed = Unsigned }; size = sz };
             direction = `Internal; initial_value = None; attrs = [] }
    end else None) m.mod_cells in
  let mem_procs = List.filter_map (mem_cell_to_bprocess mems_tbl) m.mod_cells in
  let signals = List.map wire_to_bsignal m.mod_wires @ mem_signals in
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
        (* Scattered-concat LHS: `connect { a[hi:lo], b[i], ... } = rhs`.
           Yosys ties a signal's undriven bits this way — e.g. picorv32_pcpi_mul's
           next_rdx has its non-carry bits zeroed via
           `connect { next_rdx[63:61], ..., next_rdx[3:0] } = 49'0`.  The old code
           dropped the whole connect (unsupported), leaving those bits undriven so
           the slice-merge filled them with SELF-READS — a false combinational loop
           that broke Cyclesim/BMC and corrupted the Z3 comb-cone.  Distribute rhs
           across the chunks (RTLIL concat is MSB-first; walk LSB-first assigning
           bit offsets) so each lands in `groups` and merges with the cell-driven
           slices. *)
        (match lhs with
         | SigConcat chunks ->
             let add name msb lsb src =
               let bucket = try Hashtbl.find groups name with Not_found -> [] in
               Hashtbl.replace groups name ((msb, lsb, src) :: bucket) in
             let off = ref 0 and ok = ref true in
             List.iter (fun ch ->
               let w = match ch with
                 | SigBit _ -> 1
                 | SigRange (_, hi, lo) -> abs (hi - lo) + 1
                 | SigWire n -> wire_width n
                 | SigConst s -> (match sigchunk_width (SigConst s) with Some w -> w | None -> -1)
                 | SigConcat _ -> -1 in
               if w <= 0 then ok := false
               else begin
                 let src = BSlice { signal = r; msb = !off + w - 1; lsb = !off } in
                 (match ch with
                  | SigBit (n, b) -> add n b b src
                  | SigRange (n, hi, lo) -> add n (max hi lo) (min hi lo) src
                  | SigWire n -> add n (w - 1) 0 src
                  | SigConst _ | SigConcat _ -> ());   (* const target: consume, don't write *)
                 off := !off + w
               end) (List.rev chunks);
             if not !ok then
               unsupported_connects := (lhs, rhs) :: !unsupported_connects
         | _ -> unsupported_connects := (lhs, rhs) :: !unsupported_connects)
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
            (* Partial / multiple writes: emit one @slice_write per slice so
             * they MERGE with any cell-driven slices of the same wire (via
             * behavioral_to_hardcaml's slice merger).  The old code instead
             * built a full-width concat and filled the uncovered bits with a
             * self-read `BSlice (BVar name)`; when those bits are actually
             * driven by a CELL (yosys drives a signal's carry bits by cells and
             * zeroes the gaps with a scattered-concat connect — picorv32_pcpi_mul's
             * next_rdx), the self-read created a false combinational loop that
             * broke Cyclesim/BMC and corrupted the Z3 comb-cone.  @slice_writes
             * carry no self-read, so every bit is driven exactly once. *)
            List.map (fun (msb, lsb, r) ->
              assign_or_slice_write name (Some (msb, lsb)) r) slices
      in
      BCombinational {
        name = "connect_" ^ name;
        sensitivity = [BAny];
        body;
      } :: acc
    ) groups []
  in
  ignore !unsupported_connects;
  (* Build a lookup of every wire's declared width — feed to the
     cross-cell sliced-FF merger so it can verify disjoint coverage. *)
  let wire_widths = List.map (fun w -> w.wire_name, w.wire_width) m.mod_wires in
  let cell_procs = merge_sliced_ff_processes ~wire_widths cell_procs in
  {
    name = m.mod_name;
    params = [];
    signals;
    processes = cell_procs @ mem_procs @ connect_procs;
    instances = [];
    funcs = [];
    mems = []; attrs = [];
  }

(* Inline single-definition internal-wire aliases. yosys-slang emits
 * a fresh internal wire for every cell output, including chains of
 * `$logic_not` cells whose outputs (`$1y`, `$5y`, …) all compute the
 * SAME function `!rst_n` of the same input. Verible's BIR has no
 * such intermediates — both FFs reference `rst_n` directly. After
 * Behavioral_ffrip the two designs' D-cones diverge purely on the
 * names of these intermediates, so Behavioral_share's structural
 * equality miss merging them, leaving Verible with one ripped FF
 * and yosys-slang with two — interface mismatch.
 *
 * This pass folds each non-port wire whose value is given by a
 * single BCombinational/BAssign of pure expressions (no calls, no
 * inter-process driven LHS) into every reader. Iterates to
 * fixed-point so chains like `b_q$6 → mux_Y → ...` collapse all the
 * way through. *)
let copyprop_module (m : bmodule) : bmodule =
  let internal_names =
    List.filter_map (fun (s : bsignal) ->
      if s.direction = `Internal then Some s.name else None
    ) m.signals
    |> List.fold_left (fun acc n -> n :: acc) []
  in
  let is_internal name = List.mem name internal_names in
  let rec free_vars acc = function
    | BVar n -> if List.mem n acc then acc else n :: acc
    | BConst _ -> acc
    | BBinOp { lhs; rhs; _ } -> free_vars (free_vars acc lhs) rhs
    | BUnOp { operand; _ } -> free_vars acc operand
    | BCond { condition; then_val; else_val } ->
        free_vars (free_vars (free_vars acc condition) then_val) else_val
    | BConcat es -> List.fold_left free_vars acc es
    | BReplicate { value; _ } -> free_vars acc value
    | BSelect { array; index } -> free_vars (free_vars acc array) index
    | BSlice { signal; _ } -> free_vars acc signal
    | BCall { args; _ } -> List.fold_left free_vars acc args
  in
  let rec subst env = function
    | BVar n as e ->
        (match List.assoc_opt n env with
         | Some e' -> e'
         | None -> e)
    | BConst _ as e -> e
    | BBinOp r ->
        BBinOp { r with lhs = subst env r.lhs; rhs = subst env r.rhs }
    | BUnOp r -> BUnOp { r with operand = subst env r.operand }
    | BCond r -> BCond { condition = subst env r.condition;
                         then_val = subst env r.then_val;
                         else_val = subst env r.else_val }
    | BConcat es -> BConcat (List.map (subst env) es)
    | BReplicate r -> BReplicate { r with value = subst env r.value }
    | BSelect r -> BSelect { array = subst env r.array;
                             index = subst env r.index }
    | BSlice r -> BSlice { r with signal = subst env r.signal }
    | BCall r -> BCall { r with args = List.map (subst env) r.args }
  in
  let rec subst_stmt env = function
    | BAssign { lhs; rhs } -> BAssign { lhs; rhs = subst env rhs }
    | BIf r ->
        BIf { condition = subst env r.condition;
              then_stmts = List.map (subst_stmt env) r.then_stmts;
              else_stmts = List.map (subst_stmt env) r.else_stmts }
    | BCase r ->
        BCase { selector = subst env r.selector;
                cases = List.map (fun (e, ss) ->
                  (subst env e, List.map (subst_stmt env) ss)) r.cases;
                default = List.map (subst_stmt env) r.default }
    | BBlock ss -> BBlock (List.map (subst_stmt env) ss)
    | BCallStmt r -> BCallStmt { r with args = List.map (subst env) r.args }
    | BReturn (Some e) -> BReturn (Some (subst env e))
    | other -> other
  in
  let subst_proc env = function
    | BCombinational c ->
        BCombinational { c with body = List.map (subst_stmt env) c.body }
    | BSequential s ->
        BSequential { s with body = List.map (subst_stmt env) s.body }
  in
  (* Find all whole-wire combinational defs of internal wires. Each
   * wire is allowed at most one defining BAssign — multi-driver
   * wires are skipped (their semantics aren't a simple alias). *)
  let defs : (string, bexpr) Hashtbl.t = Hashtbl.create 32 in
  let dup : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun p ->
    match p with
    | BCombinational { body = [BAssign { lhs; rhs }]; _ }
      when is_internal lhs ->
        if Hashtbl.mem defs lhs then Hashtbl.add dup lhs ()
        else Hashtbl.add defs lhs rhs
    | _ -> ()
  ) m.processes;
  Hashtbl.iter (fun n () -> Hashtbl.remove defs n) dup;
  (* Build the substitution to fixed-point: each entry's RHS is
   * itself substituted using the rest of the env. Stops when an
   * iteration changes nothing. Bounded by max_iter to break cycles
   * (shouldn't happen in well-formed RTLIL but cheap insurance). *)
  let env = Hashtbl.fold (fun k v acc -> (k, v) :: acc) defs [] in
  let max_iter = 10 in
  let rec close iter env =
    if iter >= max_iter then env
    else begin
      let changed = ref false in
      let env' = List.map (fun (k, v) ->
        let v' = subst (List.filter (fun (k', _) -> k' <> k) env) v in
        if v' <> v then changed := true;
        (k, v')
      ) env in
      if !changed then close (iter + 1) env' else env'
    end
  in
  let env = close 0 env in
  (* Substitute env throughout every process. Then drop any
   * combinational process that defined an internal wire — its
   * readers no longer reference it. Keep the alias if the wire is
   * read by anything OTHER than a now-dead process. *)
  let processes' = List.map (subst_proc env) m.processes in
  let inlined_names =
    List.filter_map (fun (k, _) -> Some k) env in
  let still_referenced =
    let acc = ref [] in
    List.iter (fun p ->
      match p with
      | BCombinational { body = [BAssign { lhs; _ }]; _ }
        when List.mem lhs inlined_names -> ()
      | BCombinational c ->
          List.iter (function
            | BAssign { rhs; _ } -> acc := free_vars !acc rhs
            | _ -> ()) c.body
      | BSequential s ->
          List.iter (function
            | BAssign { rhs; _ } -> acc := free_vars !acc rhs
            | _ -> ()) s.body
    ) processes';
    !acc
  in
  let processes' = List.filter (fun p ->
    match p with
    | BCombinational { body = [BAssign { lhs; _ }]; _ }
      when List.mem lhs inlined_names ->
        List.mem lhs still_referenced
    | _ -> true
  ) processes' in
  { m with processes = processes' }

let convert_design (d : rtlil_design) : bprogram =
  { modules = List.map (fun m -> copyprop_module (module_to_bmodule m))
                d.design_modules;
    library_cells = [] }

(* Convenience: parse an RTLIL file straight from disk. *)
let convert_file path : bprogram =
  let d = parse_rtlil_file path in
  convert_design d
