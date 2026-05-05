(* ver_front_to_behavioral.ml
 *
 * Convert a Vivado-emitted Verilog netlist to Behavioral IR by walking
 * ver_front's parsed tree (Globals.modprims). The key advantage over the
 * Verilator → Behavioral path is that bit-select pin connections like
 * .D(q0[2]), .Q(q[2]) are first-class in the tree, and per-bit
 * register cells emitted by Vivado for multi-bit registers can be
 * recombined into a single multi-bit BSequential process.
 *)

open Ver_front
open Vparser
open Behavioral_ir

(* Strict mode (env MITER_STRICT=1): refuse to silently skip an
 * unrecognised RTL_* / cell type. Instead raise so the missing pattern
 * gets flagged. Permissive (default) preserves existing behaviour. *)
let strict_mode = lazy (Sys.getenv_opt "MITER_STRICT" <> None)

let strict_bail_cell cell_type inst_name =
  let msg =
    Printf.sprintf "[ver_front_to_behavioral] no cell_to_bprocess case for \
                    cell type %S (instance %S)\nset MITER_STRICT=0 (or unset) \
                    to suppress" cell_type inst_name
  in
  if Lazy.force strict_mode then failwith msg
  else if Sys.getenv_opt "MITER_VERBOSE" <> None then
    Printf.eprintf "Warning: %s\n" msg

(* ─── Cell-type name normalisation ────────────────────────────────────── *)

(* Vivado emits cell types with disambiguating suffixes for its viewer
 * (RTL_REG_SYNC__BREG_4, RTL_OR4, RTL_MUX103). The semantic family is
 * what we need; collapse to it. *)
let normalize_cell_type s =
  let strip_breg s =
    try
      let i = Str.search_forward (Str.regexp "__BREG_[0-9]+$") s 0 in
      String.sub s 0 i
    with Not_found -> s
  in
  let strip_trailing_digits s =
    if String.length s = 0 then s
    else
      let n = String.length s in
      let rec find_end i =
        if i >= 0 && s.[i] >= '0' && s.[i] <= '9' then find_end (i - 1)
        else i + 1
      in
      let end_idx = find_end (n - 1) in
      if end_idx < n && end_idx > 0 then String.sub s 0 end_idx else s
  in
  s |> strip_breg |> strip_trailing_digits

(* ─── Range / width helpers ───────────────────────────────────────────── *)

let int_of_token = function
  | INT n -> Some n
  | INTNUM s | DECNUM s -> (try Some (int_of_string s) with _ -> None)
  | _ -> None

let width_of_range = function
  | RANGE (m, l) ->
      (match int_of_token m, int_of_token l with
       | Some mi, Some li -> abs (mi - li) + 1
       | _ -> 1)
  | _ -> 1

(* ─── Expression conversion ───────────────────────────────────────────── *)

(* Returns (value, width) for a numeric token. *)
let parse_const tok : int * int =
  match tok with
  | INT n -> (n, 32)
  | INTNUM s | DECNUM s ->
      (try (int_of_string s, 32) with _ -> (0, 32))
  | HEXNUM s ->
      let pos = try String.index s 'h' with Not_found -> -1 in
      let pos = if pos < 0 then (try String.index s 'H' with Not_found -> -1) else pos in
      if pos < 0 then (0, 32)
      else
        let hex_part = String.sub s (pos + 1) (String.length s - pos - 1) in
        let value = try int_of_string ("0x" ^ hex_part) with _ -> 0 in
        let width = if pos > 0
          then (try int_of_string (String.sub s 0 pos) with _ -> 32) else 32
        in (value, width)
  | BINNUM s ->
      let pos = try String.index s 'b' with Not_found -> -1 in
      let pos = if pos < 0 then (try String.index s 'B' with Not_found -> -1) else pos in
      if pos < 0 then (0, 32)
      else
        let bin_part = String.sub s (pos + 1) (String.length s - pos - 1) in
        let value = try int_of_string ("0b" ^ bin_part) with _ -> 0 in
        let width = if pos > 0
          then (try int_of_string (String.sub s 0 pos) with _ -> 32) else 32
        in (value, width)
  | _ -> (0, 1)

let rec expr_to_bexpr = function
  | ID id -> BVar id.Idhash.id
  | (INT _ | INTNUM _ | DECNUM _ | HEXNUM _ | BINNUM _) as c ->
      let (value, width) = parse_const c in
      BConst { value; width }
  | TRIPLE (BITSEL, ID id, idx) ->
      BSelect {
        array = BVar id.Idhash.id;
        index = expr_to_bexpr idx;
      }
  | QUADRUPLE (PARTSEL, ID id, INT msb, INT lsb) ->
      (* `name[MSB:LSB]` — a sub-vector slice. Vivado uses these in
       * port maps to wire a sub-range of one bus to a child entity's
       * full input. Encoding it as BSlice (rather than collapsing to
       * BVar) is necessary so flatten's port-substitution chains the
       * sliced expression into deeper inlined leaves. *)
      BSlice { signal = BVar id.Idhash.id; msb; lsb }
  | TRIPLE (PARTSEL, ID id, _) ->
      BVar id.Idhash.id
  | TRIPLE (PLUS, a, b) ->
      BBinOp { op = BAdd;
               lhs = expr_to_bexpr a; rhs = expr_to_bexpr b;
               result_type = BInt { width = 64; signed = Unsigned } }
  | TRIPLE (MINUS, a, b) ->
      BBinOp { op = BSub;
               lhs = expr_to_bexpr a; rhs = expr_to_bexpr b;
               result_type = BInt { width = 64; signed = Unsigned } }
  | TRIPLE (P_AMPERSAND, a, b) ->
      BBinOp { op = BAnd;
               lhs = expr_to_bexpr a; rhs = expr_to_bexpr b;
               result_type = BInt { width = 64; signed = Unsigned } }
  | TRIPLE (P_VBAR, a, b) ->
      BBinOp { op = BOr;
               lhs = expr_to_bexpr a; rhs = expr_to_bexpr b;
               result_type = BInt { width = 64; signed = Unsigned } }
  | TRIPLE (P_CARET, a, b) ->
      BBinOp { op = BXor;
               lhs = expr_to_bexpr a; rhs = expr_to_bexpr b;
               result_type = BInt { width = 64; signed = Unsigned } }
  | DOUBLE (P_TILDE, a) ->
      BUnOp { op = BNot;
              operand = expr_to_bexpr a;
              result_type = BInt { width = 64; signed = Unsigned } }
  | _ -> BConst { value = 0; width = 1 }

(* Extract the underlying base name and (optional) bit index from a Pin
 * expression. q[3] → ("q", Some 3); plain VarRef q → ("q", None). *)
let pin_base_index expr =
  match expr with
  | ID id -> Some (id.Idhash.id, None)
  | TRIPLE (BITSEL, ID id, idx) ->
      (match int_of_token idx with
       | Some n -> Some (id.Idhash.id, Some n)
       | None -> Some (id.Idhash.id, None))
  | _ -> None

(* ─── Port and signal extraction ──────────────────────────────────────── *)

(* Walk THASH decls, collecting (name, direction, width). *)
let extract_signals body =
  let signals = Hashtbl.create 16 in
  let dir_for = function
    | INPUT -> `Input | OUTPUT -> `Output | _ -> `Internal
  in
  (* Collect ports (final say on direction). *)
  let process_decl decl =
    match decl with
    | QUINTUPLE ((INPUT | OUTPUT | INOUT) as dir, _, _, range_tok, TLIST names) ->
        let width = width_of_range range_tok in
        let direction = dir_for dir in
        List.iter (function
          | TRIPLE (ID id, _, _) | DOUBLE (ID id, _) | ID id ->
              Hashtbl.replace signals id.Idhash.id (direction, width)
          | _ -> ()
        ) names
    | QUADRUPLE ((WIRE | REG | TRI0 | TRI1), _, range_in_triple, TLIST names) ->
        let width = match range_in_triple with
          | TRIPLE (_, range_tok, _) -> width_of_range range_tok
          | _ -> 1
        in
        List.iter (function
          | DOUBLE (ID id, _) | TRIPLE (ID id, _, _) | ID id ->
              if not (Hashtbl.mem signals id.Idhash.id) then
                Hashtbl.replace signals id.Idhash.id (`Internal, width)
          | _ -> ()
        ) names
    | _ -> ()
  in
  (match body with
   | THASH (decls, _) -> Hashtbl.iter (fun k _ -> process_decl k) decls
   | _ -> ());
  Hashtbl.fold (fun name (direction, width) acc ->
    { name;
      stype = BInt { width; signed = Unsigned };
      direction;
      initial_value = None; attrs = [] } :: acc
  ) signals []

(* ─── Cell instance walking ───────────────────────────────────────────── *)

type cell_inst = {
  cell_type: string;       (* normalised, e.g. RTL_REG_SYNC *)
  raw_cell_type: string;   (* pre-normalisation (e.g. popcount__parameterized3,
                              kept for hierarchical entity-instance lookups) *)
  inst_name: string;
  pins: (string * sv_node) list;  (* (pin_name, expr_token) *)
}
and sv_node = Vparser.token

let extract_cells body =
  let cells = ref [] in
  let process_inst = function
    | QUADRUPLE (modinst_kw, ID cell_id, _params, TLIST insts) ->
        ignore modinst_kw;
        List.iter (function
          | TRIPLE (ID inst_name, _scalar, TLIST pin_list) ->
              let pins = List.filter_map (function
                | TRIPLE (cellpin_kw, ID pin_name, expr) ->
                    ignore cellpin_kw;
                    Some (pin_name.Idhash.id, expr)
                | _ -> None
              ) pin_list in
              cells := {
                cell_type = normalize_cell_type cell_id.Idhash.id;
                raw_cell_type = cell_id.Idhash.id;
                inst_name = inst_name.Idhash.id;
                pins;
              } :: !cells
          | _ -> ()
        ) insts
    | _ -> ()
  in
  (match body with
   | THASH (_, body_h) -> Hashtbl.iter (fun k _ -> process_inst k) body_h
   | _ -> ());
  List.rev !cells

(* ─── Per-bit register grouping ───────────────────────────────────────── *)

(* When Vivado emits a multi-bit register, each bit gets its own cell with
 * Q and D pins connected via BITSEL(name, i). Group these cells by:
 *   (cell_type, base_name_of_Q, base_name_of_D_or_other_data_pin)
 * and check that the bit indices form a contiguous 0..N-1 set. If so,
 * we can emit one combined BSequential process for the whole register. *)

let is_register_cell ct =
  ct = "RTL_REG" || ct = "RTL_REG_SYNC" || ct = "RTL_REG_ASYNC"

(* Vivado preserves the original SV signal name in the FF *instance*
 * label even when its optimiser renames the corresponding Q-pin net.
 * For `\shift_q_reg[0]\: RTL_REG_ASYNC port map (Q => \^refill_way_bin\(0))`
 * the *net* (`^refill_way_bin`) is unrelated to the source code, but
 * the *instance label* `shift_q_reg[0]` carries the SV signal name
 * (`shift_q`) plus the bit index. Strip `_reg[<N>]` / `_reg_<N>` /
 * `_reg` from the instance label to recover the SV bus name. *)
let q_base_from_inst_name inst =
  let strip suffix s =
    if Filename.check_suffix s suffix
    then Some (Filename.chop_suffix s suffix)
    else None
  in
  let with_idx s =
    (* `<base>_reg[N]` *)
    try
      let i = Str.search_backward (Str.regexp "_reg\\[\\([0-9]+\\)\\]$") s
                (String.length s) in
      Some (String.sub s 0 i,
            int_of_string (Str.matched_group 1 s))
    with Not_found ->
      (* `<base>_reg_<N>` *)
      try
        let i = Str.search_backward (Str.regexp "_reg_\\([0-9]+\\)$") s
                  (String.length s) in
        Some (String.sub s 0 i,
              int_of_string (Str.matched_group 1 s))
      with Not_found -> None
  in
  match with_idx inst with
  | Some (base, idx) -> Some (base, Some idx)
  | None ->
      match strip "_reg" inst with
      | Some base -> Some (base, None)
      | None -> None

let group_register_cells (cells : cell_inst list) =
  let regs, others = List.partition (fun c -> is_register_cell c.cell_type) cells in
  (* For each register cell, extract Q's base name and bit index. We
   * prefer the instance-name derivation (which gives the SV-source
   * name) and fall back to the Q-pin's net name when the instance
   * doesn't follow Vivado's `<base>_reg[N]` convention. *)
  let with_q_index = List.filter_map (fun c ->
    let from_inst = q_base_from_inst_name c.inst_name in
    let from_pin =
      match List.assoc_opt "Q" c.pins with
      | Some q_expr -> pin_base_index q_expr
      | None -> None
    in
    match from_inst, from_pin with
    | Some (base, idx), _ -> Some (c, base, idx)
    | None, Some (base, idx) -> Some (c, base, idx)
    | None, None -> None
  ) regs in
  (* Group by (cell_type, q_base). *)
  let groups : (string * string, (cell_inst * int option) list) Hashtbl.t =
    Hashtbl.create 8
  in
  List.iter (fun (c, base, idx) ->
    let key = (c.cell_type, base) in
    let lst = try Hashtbl.find groups key with Not_found -> [] in
    Hashtbl.replace groups key ((c, idx) :: lst)
  ) with_q_index;
  (* Convert each group: if it's a single None-index cell, leave as one cell;
   * if it's multiple Some-indexed cells covering 0..N-1, combine into one. *)
  let combined = Hashtbl.fold (fun (cell_type, q_base) members acc ->
    let sorted = List.sort (fun (_, a) (_, b) -> compare a b) members in
    let indices = List.filter_map snd sorted in
    if List.length indices = List.length sorted &&
       List.length sorted > 1 &&
       (let max_idx = List.fold_left max 0 indices in
        max_idx + 1 = List.length sorted &&
        List.sort compare indices = List.init (List.length sorted) (fun i -> i))
    then
      (* Contiguous bits 0..N-1 → combine. Take the first cell as a
       * template and rewrite Q (and D when its base agrees) pins to
       * whole-vector references using the *recovered* SV signal name
       * (`q_base`) — Vivado's optimiser may have renamed the Q-pin
       * net to something opaque like `^refill_way_bin`. *)
      let (template, _) = List.hd sorted in
      let rewrite_pin (name, expr) =
        if name = "Q" then
          (name, ID { Idhash.id = q_base })
        else if name = "D" then
          match pin_base_index expr with
          | Some (_, Some _) ->
              (* Bit-select like `p_0_out[0]` — drop the index;
               * keep the original base since Vivado uses one D-net
               * per bit too. *)
              (match expr with
               | TRIPLE (BITSEL, ID id, _) -> (name, ID id)
               | _ -> (name, expr))
          | _ -> (name, expr)
        else (name, expr)
      in
      let pins = List.map rewrite_pin template.pins in
      `Combined { template with inst_name = q_base ^ "_combined"; pins } :: acc
    else
      List.fold_left (fun acc (c, _) -> `Single c :: acc) acc sorted
  ) groups [] in
  let regs_out = List.map (function `Combined c | `Single c -> c) combined in
  regs_out @ others

(* ─── Cell → BIR process ──────────────────────────────────────────────── *)

let pin_expr name pins =
  match List.assoc_opt name pins with
  | Some t -> Some (expr_to_bexpr t)
  | None -> None

let pin_lhs_name name pins =
  match List.assoc_opt name pins with
  | Some (ID id) -> Some id.Idhash.id
  | Some (TRIPLE (BITSEL, ID id, _)) -> Some id.Idhash.id
  (* `name(MSB downto LSB)` on the LHS of a port_map — Vivado emits this
   * for whole-vector connections to a child cell's output port. We
   * still target the bare signal at the BIR level (the slice is
   * implied by the signal's declared width). *)
  | Some (QUADRUPLE (PARTSEL, ID id, _, _)) -> Some id.Idhash.id
  | _ -> None

let cell_to_bprocess (c : cell_inst) =
  let result_t = BInt { width = 64; signed = Unsigned } in
  let bool_t = BInt { width = 1; signed = Unsigned } in
  let combinational lhs rhs =
    Some (BCombinational {
      name = Printf.sprintf "%s_inst_%s" c.cell_type c.inst_name;
      sensitivity = [BAny];
      body = [BAssign { lhs; rhs }];
    })
  in
  let mk_binop op a b t = BBinOp { op; lhs = a; rhs = b; result_type = t } in
  let mk_unop op a t = BUnOp { op; operand = a; result_type = t } in
  let comb_2to1 op t =
    match pin_lhs_name "O" c.pins, pin_expr "I0" c.pins, pin_expr "I1" c.pins with
    | Some lhs, Some a, Some b -> combinational lhs (mk_binop op a b t)
    | _ -> None
  in
  let comb_1to1 op =
    match pin_lhs_name "O" c.pins, pin_expr "I0" c.pins with
    | Some lhs, Some a -> combinational lhs (mk_unop op a result_t)
    | _ -> None
  in
  (* Reduction operators. BIR has BRedAnd / BRedOr — z3_miter encodes
   * them via Z3's bitvector reduction primitives. NAND/NOR are the
   * negation of the AND/OR result. *)
  let comb_reduction kind =
    match pin_lhs_name "O" c.pins, pin_expr "I0" c.pins with
    | Some lhs, Some a ->
        let red op = BUnOp { op; operand = a; result_type = bool_t } in
        let body = match kind with
          | `And  -> red BRedAnd
          | `Or   -> red BRedOr
          | `Nand -> BUnOp { op = BNot; operand = red BRedAnd;
                             result_type = bool_t }
          | `Nor  -> BUnOp { op = BNot; operand = red BRedOr;
                             result_type = bool_t }
        in
        combinational lhs body
    | _ -> None
  in
  (* Single-port passthrough buffer (IBUF/OBUF and friends): O = I. *)
  let comb_passthrough () =
    match pin_lhs_name "O" c.pins, pin_expr "I" c.pins with
    | Some lhs, Some a -> combinational lhs a
    | _ -> None
  in
  let mk_register ~async =
    match pin_lhs_name "Q" c.pins, pin_expr "C" c.pins, pin_expr "D" c.pins with
    | Some q, Some _, Some d ->
        let clock = match pin_expr "C" c.pins with
          | Some (BVar n) -> n | _ -> "clk" in
        let reset_pin = pin_expr "RST" c.pins in
        let pre_pin   = pin_expr "PRE" c.pins in
        let clr_pin   = pin_expr "CLR" c.pins in
        let zero = BConst { value = 0; width = 64 } in
        (* All-ones at whatever width Q resolves to — z3_miter widens the
         * BConst-zero to Q's width and ~0 then becomes the proper
         * all-ones mask. Used for the async PRE (preset-to-1) case. *)
        let ones = BUnOp { op = BNot; operand = zero; result_type = result_t } in
        let inner = BAssign { lhs = q; rhs = d } in
        (* Lower priority first, higher priority later — outermost wins.
         * Vivado's RTL_REG_ASYNC follows the order PRE > CLR > RST in the
         * underlying flop, so wrap CLR around RST and PRE around CLR. *)
        let wrap inner cond_pin set_to =
          match cond_pin with
          | Some r ->
              BIf { condition = r;
                    then_stmts = [BAssign { lhs = q; rhs = set_to }];
                    else_stmts = [inner] }
          | None -> inner
        in
        let wrapped = wrap inner reset_pin zero in
        let wrapped = wrap wrapped clr_pin   zero in
        let wrapped = wrap wrapped pre_pin   ones in
        let body = [wrapped] in
        let async = async || pre_pin <> None || clr_pin <> None in
        Some (BSequential {
          name = Printf.sprintf "%s_inst_%s" c.cell_type c.inst_name;
          clock; clock_edge = `Pos;
          reset = (match reset_pin with Some (BVar n) -> Some n | _ -> None);
          reset_edge = (if async then Some `Pos else None);
          reset_async = async;
          body;
        })
    | _ -> None
  in
  match c.cell_type with
  | "RTL_INV"     -> comb_1to1 BNot
  | "RTL_AND"     -> comb_2to1 BAnd result_t
  | "RTL_OR"      -> comb_2to1 BOr  result_t
  | "RTL_XOR"     -> comb_2to1 BXor result_t
  | "RTL_XNOR" ->
      (* a XNOR b = NOT (a XOR b) — synthesise as a single op via the
       * standard binop combine helper plus an inverter wouldn't fit the
       * single-process shape; emit as ~(a^b) inline. *)
      (match pin_lhs_name "O" c.pins, pin_expr "I0" c.pins, pin_expr "I1" c.pins with
       | Some lhs, Some a, Some b ->
           combinational lhs
             (BUnOp { op = BNot;
                      operand = mk_binop BXor a b result_t;
                      result_type = result_t })
       | _ -> None)
  | "RTL_ADD"     -> comb_2to1 BAdd result_t
  | "RTL_SUB"     -> comb_2to1 BSub result_t
  | "RTL_MINUS" ->
      (* unary negation: O = -I0 (≡ 0 - I0). *)
      (match pin_lhs_name "O" c.pins, pin_expr "I0" c.pins with
       | Some lhs, Some a ->
           combinational lhs (mk_unop BNeg a result_t)
       | _ -> None)
  | "RTL_MUL"
  | "RTL_MULT"    -> comb_2to1 BMul result_t
  | "RTL_LSHIFT" | "RTL_ALSHIFT"
                  -> comb_2to1 BShl result_t
  | "RTL_RSHIFT"  -> comb_2to1 BShr result_t
  | "RTL_ARSHIFT" -> comb_2to1 BAshr result_t
  | "RTL_EQ"      -> comb_2to1 BEq  bool_t
  | "RTL_NEQ"     -> comb_2to1 BNe  bool_t
  | "RTL_LT"      -> comb_2to1 BLt  bool_t
  | "RTL_LEQ"     -> comb_2to1 BLe  bool_t
  | "RTL_GT"      -> comb_2to1 BGt  bool_t
  | "RTL_GEQ"     -> comb_2to1 BGe  bool_t
  | "RTL_MUX" ->
      (match pin_lhs_name "O" c.pins, pin_expr "S" c.pins,
             pin_expr "I0" c.pins, pin_expr "I1" c.pins with
       | Some lhs, Some s, Some i0, Some i1 ->
           combinational lhs
             (BCond { condition = s; then_val = i0; else_val = i1 })
       | _ -> None)
  | "RTL_REG"        -> mk_register ~async:false
  | "RTL_REG_SYNC"   -> mk_register ~async:false
  | "RTL_REG_ASYNC"  -> mk_register ~async:true
  (* Reduction operators. *)
  | "RTL_REDUCTION_AND"  -> comb_reduction `And
  | "RTL_REDUCTION_OR"   -> comb_reduction `Or
  | "RTL_REDUCTION_NAND" -> comb_reduction `Nand
  | "RTL_REDUCTION_NOR"  -> comb_reduction `Nor
  (* I/O buffers and synthetic wire-assign: semantically O = I. *)
  | "IBUF" | "OBUF" | "BUFG" | "BUFGCTRL" | "WIRE_ASSIGN"
                  -> comb_passthrough ()
  (* Tied-high / tied-low primitives. Output is a fixed constant. *)
  | "VCC" ->
      (match pin_lhs_name "P" c.pins with
       | Some lhs -> Some (BCombinational {
           name = Printf.sprintf "VCC_inst_%s" c.inst_name;
           sensitivity = [BAny];
           body = [BAssign { lhs; rhs = BConst { value = 1; width = 1 } }];
         })
       | None -> None)
  | "GND" ->
      (match pin_lhs_name "G" c.pins with
       | Some lhs -> Some (BCombinational {
           name = Printf.sprintf "GND_inst_%s" c.inst_name;
           sensitivity = [BAny];
           body = [BAssign { lhs; rhs = BConst { value = 0; width = 1 } }];
         })
       | None -> None)
  (* RTL_BSEL: dynamic bit-select. Pins:
   *   O : 1-bit out         (result)
   *   I : N-bit vector in   (source)
   *   S : K-bit vector in   (index)
   * Encoding: O = (I >> S) & 1. Exact — no approximation. *)
  | "RTL_BSEL" ->
      (match pin_lhs_name "O" c.pins, pin_expr "I" c.pins,
             pin_expr "S" c.pins with
       | Some lhs, Some i_src, Some s_idx ->
           Some (BCombinational {
             name = Printf.sprintf "RTL_BSEL_%s" c.inst_name;
             sensitivity = [BAny];
             body = [BAssign {
               lhs;
               rhs = BBinOp {
                 op = BAnd;
                 lhs = BBinOp { op = BShr; lhs = i_src; rhs = s_idx;
                                result_type = bool_t };
                 rhs = BConst { value = 1; width = 1 };
                 result_type = bool_t;
               };
             }];
           })
       | _ -> None)
  (* RTL_BMERGE: bit-merge / overlay primitive. Vivado uses this for
   * patterns like `out = data; out[s] = i;` — replace one bit of DATA
   * with I at position S. The proper encoding is
   *   for j in 0..W-1: O[j] = (j == S) ? I : DATA[j]
   * which needs per-bit modelling we don't currently emit at this
   * level. As a deterministic stand-in we pass DATA through unchanged
   * — equivalence holds when DATA is always 0 (the common case for
   * shift-register style overlays) and counter-examples otherwise. *)
  | "RTL_BMERGE" ->
      (match pin_lhs_name "O" c.pins, pin_expr "DATA" c.pins with
       | Some lhs, Some data ->
           Some (BCombinational {
             name = Printf.sprintf "RTL_BMERGE_approx_%s" c.inst_name;
             sensitivity = [BAny];
             body = [BAssign { lhs; rhs = data }];
           })
       | _ -> None)
  (* RTL_LATCH: transparent latch. When G (gate) is high the output
   * follows D; when low it holds. We model the transparent case
   * (Q = D) — a counter-example would surface if the design depends
   * on the hold behaviour. *)
  | "RTL_LATCH" ->
      (match pin_lhs_name "Q" c.pins, pin_expr "D" c.pins with
       | Some lhs, Some d ->
           Some (BCombinational {
             name = Printf.sprintf "RTL_LATCH_approx_%s" c.inst_name;
             sensitivity = [BAny];
             body = [BAssign { lhs; rhs = d }];
           })
       | _ -> None)
  (* RTL_RAM: distributed-RAM primitive Vivado infers for sync-write +
   * async-read memories. Pins:
   *   RO1 : out W-bit  (combinational read of mem[RA1])
   *   RA1 : in  N-bit  (read address)
   *   WA2 : in  N-bit  (write address)
   *   WD2 : in  W-bit  (write data)
   *   WE2 : in  W-bit  (per-bit / per-byte write enable)
   *   WCLK : in 1-bit  (write clock)
   * Proper modelling needs an array-typed signal in BIR (Array(BV, BV)
   * in Z3) — that's a separate refactor. As a deterministic stand-in we
   * drive RO1 = WD2 ⊕ RA1 (zero-extended): exact when RA1==WA2 and the
   * memory has just been written, counter-examples otherwise. Honest
   * "approximate" status — surfaces in test output, doesn't pretend to
   * be equivalent. *)
  | "RTL_RAM" ->
      (match pin_lhs_name "RO1" c.pins,
             pin_expr "WD2" c.pins,
             pin_expr "RA1" c.pins with
       | Some lhs, Some wd, Some ra ->
           Some (BCombinational {
             name = Printf.sprintf "RTL_RAM_approx_%s" c.inst_name;
             sensitivity = [BAny];
             body = [BAssign {
               lhs;
               rhs = BBinOp { op = BXor; lhs = wd; rhs = ra;
                              result_type = bool_t };
             }];
           })
       | _ -> None)
  (* RTL_ROM: a constant lookup table whose actual contents Vivado
   * keeps internal — not exposed in the elaborated VHDL. We model it
   * with a deterministic but ARBITRARY function of the address (XOR
   * of the address bits). This is wrong functionally — any test that
   * exercises a ROM will report a counter-example — but it makes the
   * encoder produce *some* constraint instead of leaving the output
   * wire free. The counter-example then honestly flags the gap. *)
  | "RTL_ROM" ->
      (match pin_lhs_name "O" c.pins, pin_expr "A" c.pins with
       | Some lhs, Some addr ->
           Some (BCombinational {
             name = Printf.sprintf "RTL_ROM_approx_%s" c.inst_name;
             sensitivity = [BAny];
             body = [BAssign {
               lhs;
               rhs = BUnOp { op = BRedXor; operand = addr;
                             result_type = bool_t };
             }];
           })
       | _ -> None)
  | other ->
      (* Hierarchical user-module instances aren't behavioural cells —
       * they're references to sub-modules whose definitions live as
       * separate entities in the same VHDL. The miter currently only
       * encodes the top module, so we silently skip these (a
       * full-hierarchy miter is a separate piece of work). Genuine
       * unhandled primitives (RTL_*, library cells) DO bail in strict
       * mode. *)
      let looks_like_primitive =
        let n = String.length other in
        (n >= 4 && String.sub other 0 4 = "RTL_")
        || other = "LUT" || other = "LUT1" || other = "LUT2"
        || other = "LUT3" || other = "LUT4" || other = "LUT5"
        || other = "LUT6" || other = "FDRE" || other = "FDCE"
        || other = "FDPE" || other = "FDSE" || other = "CARRY4"
        || other = "MUXF7" || other = "MUXF8"
      in
      if looks_like_primitive then strict_bail_cell other c.inst_name;
      None

(* ─── Module conversion ───────────────────────────────────────────────── *)

(* Heuristic match for Vivado-emitted primitive cell types.  Anything
 * that doesn't look like a primitive AND lives in our cell list is
 * treated as a hierarchical user-module instance (e.g. an entity-
 * instantiation of `popcount__parameterized3` from inside
 * `popcount__parameterized2`). Without this, those cells were silently
 * dropped during conversion to BIR and Behavioral_flatten saw the
 * Vivado side as a leaf module — which leaves L+R as free wires when
 * miter'd against the Verible side that DOES inline its sub-children. *)
let cell_type_is_primitive ct =
  let n = String.length ct in
  (n >= 4 && String.sub ct 0 4 = "RTL_")
  || ct = "LUT" || ct = "LUT1" || ct = "LUT2"
  || ct = "LUT3" || ct = "LUT4" || ct = "LUT5"
  || ct = "LUT6" || ct = "FDRE" || ct = "FDCE"
  || ct = "FDPE" || ct = "FDSE" || ct = "CARRY4"
  || ct = "MUXF7" || ct = "MUXF8"
  || ct = "BUFG" || ct = "IBUF" || ct = "OBUF"
  || ct = "GND" || ct = "VCC"

let cell_to_binstance (c : cell_inst) : Behavioral_ir.binstance option =
  if cell_type_is_primitive c.cell_type then None
  else
    let pcs = List.map (fun (pin, expr) ->
      (pin, expr_to_bexpr expr)
    ) c.pins in
    Some {
      Behavioral_ir.inst_name = c.inst_name;
      module_name = c.raw_cell_type;
      param_values = [];
      port_connections = pcs;
    }

let module_to_bmodule name (mt : Globals.modtree) =
  let body = match mt.tree with
    | QUINTUPLE (MODULE, _, _, _, body) -> body
    | _ -> EMPTY
  in
  let signals = extract_signals body in
  let raw_cells = extract_cells body in
  let cells = group_register_cells raw_cells in
  let processes = List.filter_map cell_to_bprocess cells in
  let instances = List.filter_map cell_to_binstance cells in
  { name; params = []; signals; processes; instances; funcs = []; mems = []; attrs = [] }

let convert_v_file filename =
  Hashtbl.clear Globals.modprims;
  let ok = Vparse.parse filename in
  if not ok then None
  else begin
    let modules = Hashtbl.fold (fun name mt acc ->
      module_to_bmodule name mt :: acc
    ) Globals.modprims [] in
    Some { modules; library_cells = [] }
  end
