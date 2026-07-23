(* rtl_prim.ml — Vivado RTL_* elaboration primitives -> Behavioral IR processes.
 *
 * Vivado `synth_design -rtl` writes a structural netlist over generic RTL
 * primitives (RTL_INV/AND/OR/ADD/MUX/REG/…).  `write_vhdl` emits each as a
 * `component` instance WITH its interface (checked by the VHDL frontend) plus,
 * for the mux family, a `SEL_VAL` attribute giving the select->input map.  This
 * module turns those instances into behavioural processes so a Vivado netlist can
 * serve as a gold reference in the cross-tool miter.
 *
 * The combinational/register models are ported from verilator_to_behavioral's
 * inline cell handler and augmented: the MUX family parses SEL_VAL for N inputs
 * (RTL_MUX0 is a 2-input EQUALITY-select mux — a multi-bit S compared against the
 * SEL_VAL value, not a binary selector), and the per-BIT RTL_REG cells Vivado
 * emits are grouped by (base signal, clock, enable) back into one multi-bit
 * register via @slice_write. *)
open Behavioral_ir

let strip_bs s =
  let s = if String.length s > 0 && s.[0] = '\\' then String.sub s 1 (String.length s - 1) else s in
  if String.length s > 0 && s.[String.length s - 1] = '\\'
  then String.sub s 0 (String.length s - 1) else s

let contains_sub sub s =
  let ls = String.length sub and lm = String.length s in
  let rec go k = k + ls <= lm && (String.sub s k ls = sub || go (k + 1)) in
  ls = 0 || go 0

(* Vivado names an output port's internal net `\^q\` (escaped, `^`-prefixed) and
   ties it to the port with a concurrent `q <= \^q\` the VHDL frontend drops.
   Canonicalise `\^<name>\` -> `<name>` so the driver lands on the port directly. *)
let canon_net n =
  let l = String.length n in
  if l >= 3 && n.[0] = '\\' && n.[1] = '^' && n.[l - 1] = '\\'
  then String.sub n 2 (l - 3)   (* \^q\ -> q *)
  else n

(* "2'b11" / "3'd5" / "8'hA" -> Some (width, value). *)
let parse_based (v : string) : (int * int) option =
  let v = String.trim v in
  match String.index_opt v '\'' with
  | Some q when q + 1 < String.length v ->
      let w = try int_of_string (String.sub v 0 q) with _ -> 0 in
      let digits = String.sub v (q + 2) (String.length v - q - 2) in
      (try
         let value = match Char.lowercase_ascii v.[q + 1] with
           | 'b' -> int_of_string ("0b" ^ digits)
           | 'h' -> int_of_string ("0x" ^ digits)
           | _   -> int_of_string digits in
         Some ((if w > 0 then w else 32), value)
       with _ -> None)
  | _ -> (try Some (32, int_of_string v) with _ -> None)

(* "I0:S=2'b11,I1:S=default" -> [("I0", Some (2,3)); ("I1", None)]. *)
let parse_sel_val (s : string) : (string * (int * int) option) list =
  String.split_on_char ',' s
  |> List.filter_map (fun arm ->
       match String.index_opt arm ':' with
       | Some c ->
           let inp = String.trim (String.sub arm 0 c) in
           let rest = String.sub arm (c + 1) (String.length arm - c - 1) in
           (match String.index_opt rest '=' with
            | Some e ->
                let v = String.trim (String.sub rest (e + 1) (String.length rest - e - 1)) in
                if v = "default" then Some (inp, None) else Some (inp, parse_based v)
            | None -> None)
       | None -> None)

(* An output pin drives either a whole signal or a constant bit-slice of one.
   `write_out lhs_pin rhs` builds the matching BAssign / @slice_write. *)
let out_target pin_conn = match pin_conn with
  | Some (BVar n) -> Some (n, None)
  | Some (BSlice { signal = BVar n; msb; lsb }) -> Some (n, Some (msb, lsb))
  | _ -> None

let write_out target rhs = match target with
  | (n, None) -> BAssign { lhs = n; rhs }
  | (n, Some (msb, lsb)) ->
      BCallStmt { func = "@slice_write";
                  args = [BVar n; BConst { value = Z.of_int msb; width = 32 };
                          BConst { value = Z.of_int lsb; width = 32 }; rhs] }

let self_read = function
  | (n, None) -> BVar n
  | (n, Some (msb, lsb)) -> BSlice { signal = BVar n; msb; lsb }

(* Combinational RTL_* cell (logic / mux) -> process.  Registers are handled by
   the grouping pass below. *)
let comb_cell (sel_val : string option) (i : binstance) : bprocess option =
  let pin name = List.assoc_opt name i.port_connections in
  let ut = BInt { width = 64; signed = Unsigned }
  and bool_t = BInt { width = 1; signed = Unsigned } in
  let emit rhs = match out_target (pin "O") with
    | Some t -> Some (BCombinational { name = i.inst_name ^ "_rtl"; sensitivity = [BAny];
                                       body = [write_out t rhs] })
    | None -> None in
  let bin op rt = match pin "I0", pin "I1" with
    | Some a, Some b -> emit (BBinOp { op; lhs = a; rhs = b; result_type = rt })
    | _ -> None in
  let un op = match pin "I0" with Some a -> emit (BUnOp { op; operand = a; result_type = ut }) | _ -> None in
  let mux () = match pin "S" with
    | Some s ->
        let arms = match sel_val with Some sv -> parse_sel_val sv | None -> [] in
        let base = List.find_map (fun (inp, v) -> if v = None then pin inp else None) arms in
        let base = match base with Some e -> e | None -> BConst { value = Z.zero; width = 1 } in
        let rhs = List.fold_left (fun acc (inp, v) ->
          match v, pin inp with
          | Some (w, sel), Some ie ->
              BCond { condition = BBinOp { op = BEq; lhs = s;
                        rhs = BConst { value = Z.of_int sel; width = (if w > 0 then w else 32) };
                        result_type = bool_t };
                      then_val = ie; else_val = acc }
          | _ -> acc) base (List.rev arms) in
        emit rhs
    | None -> None in
  match strip_bs i.module_name with
  | "RTL_INV" -> un BNot
  | "RTL_AND" -> bin BAnd ut | "RTL_OR" -> bin BOr ut | "RTL_XOR" -> bin BXor ut
  | "RTL_ADD" -> bin BAdd ut | "RTL_SUB" -> bin BSub ut | "RTL_MUL" -> bin BMul ut
  | "RTL_LSHIFT" -> bin BShl ut | "RTL_RSHIFT" -> bin BShr ut
  | "RTL_EQ" -> bin BEq bool_t | "RTL_NEQ" -> bin BNe bool_t
  | "RTL_LT" -> bin BLt bool_t | "RTL_LEQ" -> bin BLe bool_t
  | "RTL_GT" -> bin BGt bool_t | "RTL_GEQ" -> bin BGe bool_t
  | m when String.length m >= 7 && String.sub m 0 7 = "RTL_MUX" -> mux ()
  | _ -> None

(* Group per-bit RTL_REG cells writing the same (base signal, clock, enable) into
   ONE sequential process (@slice_write per bit; merge_slice_writes_deep then packs
   them into one bus register).  Async-reset variants add the reset branch. *)
let register_processes (regs : binstance list) : bprocess list =
  let key_of (i : binstance) =
    let pin n = List.assoc_opt n i.port_connections in
    match out_target (pin "Q"), pin "C" with
    | Some (base, _), Some clk ->
        let clk_n = match clk with BVar n -> n | _ -> "clk" in
        let ce_s = match pin "CE" with Some e -> Behavioral_ir.string_of_bexpr e | None -> "" in
        let mn = strip_bs i.module_name in
        let rst =
          if contains_sub "SYNC" mn || contains_sub "ASYNC" mn
          then (match pin "RST" with Some e -> Behavioral_ir.string_of_bexpr e | None -> "")
          else "" in
        Some (base, clk_n, ce_s, rst)
    | _ -> None in
  let tbl : (string * string * string * string, binstance list) Hashtbl.t = Hashtbl.create 16 in
  let order = ref [] in
  List.iter (fun i -> match key_of i with
    | Some k ->
        if not (Hashtbl.mem tbl k) then order := k :: !order;
        Hashtbl.replace tbl k (i :: (try Hashtbl.find tbl k with Not_found -> []))
    | None -> ()) regs;
  List.rev_map (fun ((base, clk, _ce, rst) as k) ->
    let insts = Hashtbl.find tbl k in
    let pin (i : binstance) n = List.assoc_opt n i.port_connections in
    (* Assemble the per-bit D's into ONE clean multi-bit assignment `base <= {…}`
       (MSB-first, gaps self-read) instead of per-bit @slice_writes — a single
       bus register that ffrip/ssa keep whole (else the register bit-blasts into a
       per-bit SSA explosion the miter can't correspond to a source array). *)
    let bit_of (i : binstance) = match out_target (pin i "Q") with
      | Some (b, Some (msb, lsb)) when b = base -> Some (msb, lsb, pin i "D")
      | Some (b, None) when b = base -> Some (0, 0, pin i "D")
      | _ -> None in
    let slices = List.filter_map (fun i ->
      match bit_of i with Some (m, l, Some d) -> Some (m, l, d) | _ -> None) insts in
    let maxmsb = List.fold_left (fun a (m, _, _) -> max a m) 0 slices in
    let sorted = List.sort (fun (a, _, _) (b, _, _) -> compare b a) slices in
    let parts = ref [] and cursor = ref maxmsb in
    List.iter (fun (msb, lsb, d) ->
      if msb < !cursor then
        parts := BSlice { signal = BVar base; msb = !cursor; lsb = msb + 1 } :: !parts;
      parts := d :: !parts;
      cursor := lsb - 1) sorted;
    if !cursor >= 0 then
      parts := BSlice { signal = BVar base; msb = !cursor; lsb = 0 } :: !parts;
    let data = match List.rev !parts with [x] -> x | xs -> BConcat xs in
    let ce = match insts with i :: _ -> pin i "CE" | [] -> None in
    let assign = BAssign { lhs = base; rhs = data } in
    let gated = match ce with
      | Some ce -> BIf { condition = ce; then_stmts = [assign];
                         else_stmts = [BAssign { lhs = base; rhs = BVar base }] }
      | None -> assign in
    let rst_pin = match insts with i :: _ -> pin i "RST" | [] -> None in
    let body =
      if rst <> "" then
        let w = maxmsb + 1 in
        (match rst_pin with
         | Some r -> [BIf { condition = r;
                            then_stmts = [BAssign { lhs = base; rhs = BConst { value = Z.zero; width = w } }];
                            else_stmts = [gated] }]
         | None -> [gated])
      else [gated] in
    let is_async = match insts with i :: _ -> contains_sub "ASYNC" (strip_bs i.module_name) | _ -> false in
    BSequential { name = base ^ "_rtlreg"; clock = clk; clock_edge = `Pos;
                  reset = (if rst <> "" then match rst_pin with Some (BVar n) -> Some n | _ -> None else None);
                  reset_edge = (if is_async then Some `Pos else None);
                  reset_async = is_async; body; blocking_vars = [] })
    !order

(* Canonicalise \^port\ nets -> port throughout a module (bexpr, stmt, signals,
   instance connections), then drop the now-duplicate \^port\ signal decl. *)
let canon_ports_module (m : bmodule) : bmodule =
  let rec re_e e = match e with
    | BVar n -> BVar (canon_net n)
    | BConst _ -> e
    | BBinOp r -> BBinOp { r with lhs = re_e r.lhs; rhs = re_e r.rhs }
    | BUnOp r -> BUnOp { r with operand = re_e r.operand }
    | BSelect r -> BSelect { array = re_e r.array; index = re_e r.index }
    | BSlice r -> BSlice { r with signal = re_e r.signal }
    | BConcat es -> BConcat (List.map re_e es)
    | BReplicate r -> BReplicate { r with value = re_e r.value }
    | BCond r -> BCond { condition = re_e r.condition; then_val = re_e r.then_val; else_val = re_e r.else_val }
    | BCall r -> BCall { r with args = List.map re_e r.args } in
  let rec re_s s = match s with
    | BAssign r -> BAssign { lhs = canon_net r.lhs; rhs = re_e r.rhs }
    | BIf r -> BIf { condition = re_e r.condition; then_stmts = List.map re_s r.then_stmts; else_stmts = List.map re_s r.else_stmts }
    | BCase r -> BCase { selector = re_e r.selector; cases = List.map (fun (g, b) -> (re_e g, List.map re_s b)) r.cases; default = List.map re_s r.default }
    | BWhile r -> BWhile { condition = re_e r.condition; body = List.map re_s r.body }
    | BFor r -> BFor { init = re_s r.init; condition = re_e r.condition; update = re_s r.update; body = List.map re_s r.body }
    | BBlock b -> BBlock (List.map re_s b)
    | BCallStmt r -> BCallStmt { r with args = List.map re_e r.args }
    | BReturn eo -> BReturn (Option.map re_e eo) in
  let re_proc = function
    | BCombinational r -> BCombinational { r with body = List.map re_s r.body }
    | BSequential r -> BSequential { r with clock = canon_net r.clock;
                                            reset = Option.map canon_net r.reset;
                                            body = List.map re_s r.body } in
  let seen = Hashtbl.create 64 in
  let signals = List.filter_map (fun (s : bsignal) ->
    let n = canon_net s.name in
    if Hashtbl.mem seen n then None    (* \^q\ collapsed onto the port q *)
    else (Hashtbl.replace seen n (); Some { s with name = n })) m.signals in
  { m with signals;
    processes = List.map re_proc m.processes;
    instances = List.map (fun (i : binstance) ->
      { i with port_connections = List.map (fun (p, e) -> (p, re_e e)) i.port_connections })
      m.instances }

let resolve_rtl_instances (sel_val_of : string -> string option) (m : bmodule) : bmodule =
  let m = canon_ports_module m in
  let is_reg i = let mn = strip_bs i.module_name in
    String.length mn >= 7 && String.sub mn 0 7 = "RTL_REG" in
  let is_rtl i = let mn = strip_bs i.module_name in
    String.length mn >= 4 && String.sub mn 0 4 = "RTL_" in
  let regs = List.filter is_reg m.instances in
  let combs, kept = List.partition is_rtl (List.filter (fun i -> not (is_reg i)) m.instances) in
  let comb_procs = List.filter_map (fun i -> comb_cell (sel_val_of i.inst_name) i) combs in
  let reg_procs = register_processes regs in
  { m with instances = kept;
           processes = m.processes @ comb_procs @ reg_procs }
