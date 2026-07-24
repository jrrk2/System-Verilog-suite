(* Verible → Behavioral IR — a Verilator replacement frontend.
 *
 * Takes a list of Verible-elaborated specialised modules (from
 * Verible_elaborate.specialise_design) and produces a Behavioral_ir
 * `bprogram`, equivalent to what
 * Verilator_to_behavioral.convert_verilator_json_to_behavioral
 * builds from Verilator's JSON output. The downstream miter pipeline
 * (test_xilinx_rtl_miter, test_cva6_bottom_up) accepts either
 * source.
 *
 * Coverage is incremental — what's handled today:
 *   - module port declarations (input/output, scalar + [N-1:0])
 *   - net/reg declarations as internal signals
 *   - continuous `assign lhs = rhs;`
 *   - `always_comb` and `always_ff @(posedge clk)` blocks (best-effort)
 *   - expressions: identifier, decimal/sized number, binary op,
 *     unary op, ternary, paren, bit-select, slice
 *
 * Anything we don't recognise is replaced with a 1-bit zero so the
 * pipeline keeps running. The miter then surfaces the gap as a
 * counter-example, not a parse failure. *)

open Behavioral_ir
open Source_text_verible
open Verible_elaborate

(* ─── Width / range evaluation ───────────────────────────────────── *)

(* Walk an expression token tree, fold to an int when every leaf is
 * a literal or a known package constant. Returns None when the
 * expression has a free identifier. *)
(* Width prefix from a sized literal subtree (e.g. `4'b0001` → 4).
   Used by eval_int's concat handler to know how many bits each part
   contributes when folding {a, b, c} into an integer.              *)
let width_of_sized_literal tok =
  match tok with
  | TUPLE3 (STRING tag, base_token, _digits)
    when prefix_is "bin_based_number" tag
      || prefix_is "hex_based_number" tag
      || prefix_is "dec_based_number" tag
      || prefix_is "oct_based_number" tag ->
      let bs = match base_token with
        | TK_BinBase s | TK_HexBase s | TK_DecBase s | TK_OctBase s -> s
        | _ -> ""
      in
      (try
         let i = String.index bs '\'' in
         Some (int_of_string (String.sub bs 0 i))
       with _ -> None)
  | _ -> None

(* Forward-declared module-scope hashtable consulted by eval_int for
 * struct-typed parameter field lookups (task #141).  Populated by
 * extract_body_params when it sees a `parameter T X = '{F: v, ...}`
 * default; cleared per-module by convert_module.  See the full doc
 * comment at the real definition site below. *)
let cur_struct_params : (string, (string * int) list) Hashtbl.t =
  Hashtbl.create 4

(* Type name -> bit width, summed from every packed struct / enum / typedef
 * (module bodies + package bodies).  Populated during conversion (a pre-pass
 * over all modules/packages, refined per-module with full params) and queried
 * by the `$bits(<type>)` evaluator.  Without this, `.Width($bits(dcsr_t))`
 * folded to 0 -> the specialized ibex_csr got a 1-bit rd_data_o while the
 * parent wired a 26-bit concat, leaving 25 bits/26 undriven (EDIF ERC). *)
let type_widths : (string, int) Hashtbl.t = Hashtbl.create 128

(* SystemVerilog interfaces are elaborated as PACKED STRUCTS: an interface's
 * signals become the struct's fields, so member access (`p.data`) reuses the
 * existing struct-slice machinery and a whole-interface port connection carries
 * every member.  Populated once from the interface declarations before the user
 * modules are converted (see register_interfaces / convert_files_inner).
 *   iface_reg : interface name -> [(member, width)]  (declaration order = MSB..LSB)
 * The total width is also registered in [type_widths] so extract_port_decl treats
 * an interface port like any struct-typed port. *)
let iface_reg : (string, (string * int) list) Hashtbl.t = Hashtbl.create 16

(* Modport member directions: "iface$modport" -> [(member, `Input|`Output)].
 * An interface PORT `if2.wr p` exposes only the modport's members and with the
 * modport's per-member direction — clk is an INPUT of drv, data an OUTPUT — so
 * the scalarized port members (p$clk, p$data) get the right formal directions
 * and the Vivado-flattened miter matches.  Populated in convert_files_inner. *)
let iface_modports : (string, (string * [ `Input | `Output ]) list) Hashtbl.t =
  Hashtbl.create 16

(* Interface ports of the module currently being converted:
 * (port_name, iface_name, modport_name).  extract_port_decl records each one;
 * convert_module replays it after scalarize_module to promote the scalarized
 * members (p$clk, p$data, …) to formals with their modport directions.
 * Reset per module. *)
let cur_iface_ports : (string * string * string) list ref = ref []

(* Interface header ports: iface name -> [(port, `Input|`Output)] — the ports on
 * the interface DECLARATION itself (e.g. `interface if2(input logic clk)` → clk).
 * Used to wire an interface INSTANCE's own connections (`if2 u(clk)` → u.clk=clk)
 * during interface-instance elaboration.  Populated in convert_files_inner. *)
let iface_hdr_ports : (string, (string * [ `Input | `Output ]) list) Hashtbl.t =
  Hashtbl.create 16

(* Interface ports of each module: (port_name, iface).  The elaboration pass uses
 * this to rebuild source-order port SLOTS from the FINAL scalarized signal order,
 * collapsing each interface port's per-member formals (p$clk, p$data) back into a
 * single slot so positional connections resolve as they do for scalar ports. *)
let module_iface_ports : (string, (string * string) list) Hashtbl.t =
  Hashtbl.create 32

(* Interface INSTANCES declared inside each module: (inst_name, iface, param
 * overrides).  Captured from the struct-typed local decls (an interface instance
 * `if2 u(clk)` — or even a port-less `hs_if h()` that never surfaces as a
 * binstance).  The overrides (`axi_if #(.ID_WIDTH(4)) lsu_if()`) let the
 * elaboration pass size each bundle's member nets per-instance, not by the
 * interface's default params. *)
let module_iface_insts : (string, (string * string * (string * string) list) list) Hashtbl.t =
  Hashtbl.create 32

(* Interface declarations (name -> module_decl), kept so per-instance member
 * widths can be recomputed with the instance's parameter overrides. *)
let iface_decls : (string, Verible_elaborate.module_decl) Hashtbl.t =
  Hashtbl.create 16

(* Per-(iface, canonical-params) specialised member widths, memoised. *)
let iface_spec_members : (string, (string * int) list) Hashtbl.t =
  Hashtbl.create 32

(* SIGNAL name -> width, for `$bits(<signal>)` ONLY.  Kept SEPARATE from
 * type_widths so registering signal widths does NOT pollute the phantom-instance
 * drop guard (which treats type_widths membership as "this is a type name") or
 * any other type_widths consumer.  Cleared+repopulated per module. *)
let signal_widths : (string, int) Hashtbl.t = Hashtbl.create 128

(* Enum member name -> the enum's declared base width (`enum logic [1:0]` -> 2).
 * A member used in a WIDTH-sensitive context (a concat) must carry the enum
 * width, not the default 32: dmi_jtag's `{address_q, data_q, DMINoError}` (op
 * status = 2-bit enum) rendered DMINoError as 32'0 -> 71-bit concat truncated to
 * 41 -> the DMI op-status field corrupted -> openocd sees perpetual "busy" ->
 * "DMI operation didn't complete".  Populated by extract_enum_items. *)
let enum_member_widths : (string, int) Hashtbl.t = Hashtbl.create 128

(* Recursion guard for compile-time evaluation of user function bodies. *)
let eval_fn_depth = ref 0

(* Package function table (fname, formals, body), built once from the package
   bodies for eval_user_fn — the whole design shares one package set. *)
let cached_pkg_fns : Verible_elaborate.sv_function list option ref = ref None

(* Actual argument expressions of a call, in source order, from the
   `call_base1(LPAREN, argument_list, RPAREN)` node — mirrors the argument-spine
   walk used by expr_to_bexpr's user-call handling. *)
let call_args call_base =
  let arg_list = match call_base with
    | TUPLE4 (STRING t, _, body, _) when prefix_is "call_base" t -> body
    | _ -> call_base in
  let rec collect = function
    | TLIST xs -> List.concat_map collect xs
    | TUPLE3 (STRING t, prev, last)
      when prefix_is "any_argument_list_item_last" t -> collect prev @ collect last
    | TUPLE3 (STRING t, prev, _)
      when prefix_is "any_argument_list_trailing_comma" t -> collect prev
    | EMPTY_TOKEN -> []
    | other -> [other]
  in
  collect arg_list

let rec eval_int ~pkgs ~params tok =
  let lookup name =
    match List.assoc_opt name params with
    | Some v -> int_of_pvalue (PStr v)
    | None ->
        (* Search all packages — first match wins. *)
        List.find_map (fun (p : package_decl) ->
          List.assoc_opt name p.pkg_params |> Option.map (fun v ->
            match int_of_pvalue v with
            | Some n -> Some n | None -> None)
          |> Option.value ~default:None
        ) pkgs
  in
  match tok with
  | TK_DecNumber n | TK_UnBasedNumber n ->
      (try Some (int_of_string n) with _ -> None)
  (* Sized literals: `1'b0`, `4'hA`, `8'd255`. Verible parses each as
   * `<base>_based_number` wrapping the digits — for our purposes the
   * width prefix doesn't change the integer value. *)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "bin_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_BinDigits n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string ("0b" ^ !s)) with _ -> None)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "hex_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_HexDigits n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string ("0x" ^ !s)) with _ -> None)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "dec_based_number" tag ->
      let s = ref "" in
      (* A SIZED decimal literal (`5'd10`) carries its digits as TK_DecDigits,
         parallel to the TK_{Bin,Hex,Oct}Digits the other bases use — only an
         UNSIZED decimal is TK_DecNumber.  Walking for TK_DecNumber alone left
         `5'd10` folding to "" → None, so dm_mem's `LoadBaseAddr` ternary
         branches never resolved. *)
      walk (function
        | TK_DecDigits n | TK_DecNumber n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string !s) with _ -> None)
  | TUPLE3 (STRING tag, _base, digits) when prefix_is "oct_based_number" tag ->
      let s = ref "" in
      walk (function
        | TK_OctDigits n -> s := !s ^ n
        | _ -> ()) digits;
      (try Some (int_of_string ("0o" ^ !s)) with _ -> None)
  | SymbolIdentifier id -> lookup id
  (* `pkg::name` — a package-scoped constant (e.g. dm::Idle, an enum member
   * now in the package's params via convert_files_inner's augmentation).
   * Resolve against the NAMED package first, then fall back to the all-package
   * lookup for bare uniqueness. *)
  | TUPLE4 (STRING tag,
            TUPLE3 (STRING tg1, SymbolIdentifier pkg, _),
            _,
            TUPLE3 (STRING tg2, SymbolIdentifier name, _))
    when prefix_is "qualified_id" tag
      && prefix_is "unqualified_id" tg1
      && prefix_is "unqualified_id" tg2 ->
      (match resolve_pkg_ref pkgs ~pkg ~name with
       | Some v -> int_of_pvalue v
       | None -> lookup name)
  | TUPLE4 (STRING "add_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a + b)
       | _ -> None)
  | TUPLE4 (STRING "add_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a - b)
       | _ -> None)
  | TUPLE4 (STRING "mul_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a * b)
       | _ -> None)
  (* Integer `/` and `%`.  When the divisor is *concretely zero* (from
   * a fold that produced 0 — usually a struct-typed param's empty-config
   * default; see the reference2/hierarchy_extension1 case below) we
   * return Some 0 rather than None.  Without this, derived localparams
   * like `NR_ROWS = NR_ENTRIES / CVA6Cfg.INSTR_PER_FETCH` would fail to
   * register and downstream `$clog2(NR_ROWS)` references unresolvable,
   * even though slang's standalone empty-config elaboration produces
   * the same degenerate-but-tractable BIR.  Sticking with `None` for
   * the genuinely-unresolved-divisor case keeps the diagnostic loud
   * elsewhere. *)
  | TUPLE4 (STRING "mul_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some _, Some 0 -> Some 0
       | Some a, Some b -> Some (a / b)
       | _ -> None)
  | TUPLE4 (STRING "mul_expr4", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some _, Some 0 -> Some 0
       | Some a, Some b -> Some (a mod b)
       | _ -> None)
  | TUPLE4 (STRING "shift_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lsl b)
       | _ -> None)
  | TUPLE4 (STRING "shift_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lsr b)
       | _ -> None)
  (* `**` SystemVerilog power. Right-associative, parsed as
   * `pow_expr STAR_STAR unary_expr`. Required for packed-array
   * dimensions like `[2**NumLevels-1:0]` in lzc/cf_math_pkg. *)
  | TUPLE4 (STRING "pow_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some e when e >= 0 ->
           let rec p b e = if e = 0 then 1 else b * p b (e - 1) in
           Some (p a e)
       | _ -> None)
  (* Bitwise / logical / comparison operators on constants — extends
     eval_int's coverage so picorv32-style body localparams like
       localparam WITH_PCPI = ENABLE_PCPI || ENABLE_MUL || …;
       localparam irqregs_offset = ENABLE_REGS_16_31 ? 32 : 16;
     resolve at elaboration time instead of leaking through as bare
     identifiers downstream.                                          *)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "and_expr"    tag
      || prefix_is "bitand_expr" tag
      || prefix_is "logand_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a land b)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "or_expr"    tag
      || prefix_is "bitor_expr" tag
      || prefix_is "logor_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lor b)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "xor_expr"    tag
      || prefix_is "bitxor_expr" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (a lxor b)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr2", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a <  b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr3", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a >  b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr4", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a <= b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING "comp_expr5", lhs, _op, rhs) ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a >= b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "logeq_expr2"     tag
      || prefix_is "binary_eq_expr1" tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a = b then 1 else 0)
       | _ -> None)
  | TUPLE4 (STRING tag, lhs, _op, rhs)
    when prefix_is "logneq_expr"     tag
      || prefix_is "binary_neq_expr" tag
      (* Verible emits `!=` as `logeq_expr3` (the `==` form is `logeq_expr2`).
         Matching only logneq/binary_neq left `(DmBaseAddress != 0)` unfolded,
         so dm_mem's `HasSndScratch` localparam stayed a bare identifier. *)
      || prefix_is "logeq_expr3"     tag ->
      (match eval_int ~pkgs ~params lhs, eval_int ~pkgs ~params rhs with
       | Some a, Some b -> Some (if a <> b then 1 else 0)
       | _ -> None)
  (* Ternary `cond ? t : e` — used by irqregs_offset and many similar
     control-knob localparams.                                         *)
  | TUPLE6 (STRING tag, cond, _, t, _, e) when prefix_is "cond_expr" tag ->
      (match eval_int ~pkgs ~params cond with
       | Some 0 -> eval_int ~pkgs ~params e
       | Some _ -> eval_int ~pkgs ~params t
       | None -> None)
  (* Unary prefix `!x` / `-x` / `+x`.  Without this, ibex's
       MISA_VALUE = … | (32'(!RV32E) << 8) | …
     failed to fold (`!` unhandled) and, since eval_int returns None on the
     whole `|`-chain if ANY operand is None, the entire localparam leaked. *)
  | TUPLE3 (STRING tag, op_tok, operand) when prefix_is "unary_prefix_expr" tag ->
      (match eval_int ~pkgs ~params operand with
       | None -> None
       | Some v ->
           (match op_tok with
            | PLING  -> Some (if v = 0 then 1 else 0)
            | HYPHEN -> Some (- v)
            | PLUS   -> Some v
            (* bitwise `~`: width-agnostic here, so mask to 32 bits (the common
               `logic [31:0]` mask width, e.g. ibex DEBUG_MASK = ~(DEBUG_SIZE-1))
               and let the use-site width truncate further if narrower. *)
            | TILDE  -> Some (lnot v land 0xFFFFFFFF)
            | _      -> None))
  (* Concat `{a, b, c}` — fold to integer when every part is a sized
     literal (so we know its width).  picorv32-style trace-flag
     localparams like
       localparam [35:0] TRACE_BRANCH = {4'b0001, 32'b0};
     are this shape exactly.  Body is a TLIST in reverse source
     order; reverse for MSB → LSB.                                   *)
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let parts = match body with
        | TLIST xs ->
            List.filter (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN -> false | _ -> true) (List.rev xs)
        | other -> [other]
      in
      let folded =
        List.fold_left (fun acc part ->
          match acc with
          | None -> None
          | Some acc_val ->
              (match eval_int ~pkgs ~params part with
               | None -> None
               | Some v ->
                 (match width_of_sized_literal part with
                  | Some w ->
                      let mask = (1 lsl w) - 1 in
                      Some ((acc_val lsl w) lor (v land mask))
                  | None ->
                      (* Non-literal part (package ref / derived param, e.g.
                         `dm::DataCount` in `{4'h0, dm::DataCount}`): its width
                         isn't a sized literal we can read.  When the MSBs
                         accumulated so far are 0 the shift distance is
                         irrelevant, so fold it as a zero-extension.  This lets
                         DataEnd/ProgBufEnd (`Data0 + {4'h0, DataCount} - 1`)
                         resolve instead of leaving the [Data0:DataEnd] address
                         range bound unfolded -> mis-decoding dmcontrol writes.
                         If the MSBs are non-zero we can't place it -> None. *)
                      if acc_val = 0 then Some v else None))
        ) (Some 0) parts
      in
      folded
  (* Replication `{N{expr}}` — expr_primary_braces2(LBRACE, count, LBRACE,
     body, RBRACE, RBRACE).  Fold to N copies of expr's bits.  dm_csrs'
     `parameter SelectableHarts = {NrHarts{1'b1}}` needs this or it ties to 0,
     which corrupts the per-hart selection/alignment logic. *)
  | TUPLE7 (STRING tag, _, count, _, body, _, _)
    when prefix_is "expr_primary_braces2" tag ->
      (match eval_int ~pkgs ~params count with
       | Some n when n > 0 && n <= 62 ->
           let inner = match body with TLIST (x :: _) -> x | x -> x in
           (match eval_int ~pkgs ~params inner with
            | Some v ->
                let w = match width_of_sized_literal inner with
                  | Some w when w > 0 -> w | _ -> 1 in
                let mask = if w >= 62 then -1 else (1 lsl w) - 1 in
                let vv = v land mask in
                let rec rep i acc =
                  if i >= n then acc else rep (i + 1) ((acc lsl w) lor vv) in
                Some (rep 0 0)
            | None -> None)
       | _ -> None)
  (* `system_tf_call1(SystemTFIdentifier "$name", call_base)` — emitted
   * for elaboration system tasks whitelisted in
   * Source_text_verible_lex.mll's `systask` hashtable ($clog2, $bits,
   * $signed, $unsigned, …).  Whitelisting them in the lexer lets
   * `$clog2(X)'(Y)` size casts parse via `casting_type | system_tf_call`.
   * Inner-arg extraction uses the same predicate as the
   * reference_or_call_base1 fallback below. *)
  | TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier name, call_base) ->
      let inner_arg () =
        (* A package-qualified arg like `dm::ProgBufSize` is a `qualified_id`
           whose FIRST inner `unqualified_id` is the PACKAGE name (`dm`).
           Collecting unqualified_id sub-nodes then taking the first wrongly
           picks `dm` (not a param → eval_int None → $clog2 collapses to 0,
           giving `addr[-1:0]`).  So, unless the arg is an arithmetic
           expression (handled recursively), prefer a whole qualified_id. *)
        let has_arith =
          collect_by (function
            | TUPLE4 (STRING t, _, _, _)
              when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
            | _ -> false) call_base <> [] in
        let quals =
          if has_arith then []
          else collect_by (function
            | TUPLE4 (STRING t, _, _, _) when prefix_is "qualified_id" t -> true
            | _ -> false) call_base in
        match quals with
        | q :: _ -> Some q
        | [] ->
          let cands = collect_by (function
            | TUPLE4 (STRING t, _, _, _)
              when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
            | TK_DecNumber _ -> true
            | TUPLE3 (STRING t, _, _)
              when prefix_is "reference_or_call_base" t
                || prefix_is "unqualified_id" t
                || prefix_is "reference2" t -> true
            | _ -> false) call_base in
          match cands with first :: _ -> Some first | [] -> None
      in
      (match name with
       | "$clog2" ->
           (match inner_arg () with
            | Some e ->
                (match eval_int ~pkgs ~params e with
                 | Some n when n > 1 ->
                     let rec lg n acc =
                       if n <= 1 then acc else lg ((n + 1) / 2) (acc + 1)
                     in
                     Some (lg n 0)
                 | _ -> Some 0)
            | None -> Some 0)
       | "$unsigned" | "$signed" ->
           (match inner_arg () with
            | Some e -> eval_int ~pkgs ~params e
            | None -> None)
       | "$bits" ->
           (* $bits(<type>) or $bits(pkg::<type>): the LAST identifier is the
              type name; look it up in the type-width registry.  Falls back to
              Some 0 (previous behaviour) for $bits of an unknown type. *)
           let last = ref None in
           walk (function SymbolIdentifier id -> last := Some id | _ -> ()) call_base;
           (match !last with
            | Some id -> (match Hashtbl.find_opt type_widths id with Some w -> Some w | None -> (match Hashtbl.find_opt signal_widths id with Some w -> Some w | None -> Some 0))
            | None -> Some 0)
       | _ -> None)
  (* Function-like call wrapper: `reference_or_call_base1(reference,
   * call_base)`. The lexer treats most `$name` tokens as
   * SystemTFIdentifier (see the systask whitelist), so the common
   * elaboration calls ($clog2, $bits, …) go through system_tf_call1
   * above.  Names NOT in the whitelist (user functions, $readmemh, …)
   * still arrive here as SymbolIdentifier-wrapped reference_or_call_base1. *)
  | TUPLE3 (STRING "reference_or_call_base1", ref_node, call_base) ->
      let fname = ref None and last_id = ref None in
      walk (function
        | SymbolIdentifier id ->
            if !fname = None then fname := Some id;
            last_id := Some id
        | _ -> ()) ref_node;
      let inner_arg () =
        let cands = collect_by (function
          | TUPLE4 (STRING t, _, _, _)
            when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
          | TK_DecNumber _ -> true
          | TUPLE3 (STRING t, _, _)
            when prefix_is "reference_or_call_base" t
              || prefix_is "unqualified_id" t
              (* `reference2` is struct-member access `X.Y` — needed so
               * $clog2(cfg.BHTEntries) finds its inner-arg subtree.
               * Task #141. *)
              || prefix_is "reference2" t -> true
          | _ -> false) call_base in
        match cands with first :: _ -> Some first | [] -> None
      in
      (match !fname with
       | Some "$clog2" ->
           (* Inner-arg unresolvable → fold to Some 0 rather than None.
            * Aligns with the divide-by-zero handling above and the
            * struct-member-access fold below: under empty-config
            * standalone elaboration the operand chain ends in zeros,
            * and we'd rather emit a degenerate-but-comparable BIR
            * than a hard raise — slang's standalone elaboration
            * produces the same shape. *)
           (match inner_arg () with
            | Some e ->
                (match eval_int ~pkgs ~params e with
                 | Some n when n > 1 ->
                     let rec lg n acc =
                       if n <= 1 then acc else lg ((n + 1) / 2) (acc + 1)
                     in
                     Some (lg n 0)
                 | Some _ -> Some 0
                 | None -> Some 0)
            | None -> Some 0)
       (* `$unsigned(x)` / `$signed(x)` — sign-cast, no value change. *)
       | Some ("$unsigned" | "$signed") ->
           (match inner_arg () with
            | Some e -> eval_int ~pkgs ~params e
            | None -> None)
       (* `$bits(T)` / `$bits(pkg::T)` — the LAST identifier is the type name;
        * look it up in the type-width registry (summed struct/enum/typedef
        * widths).  Falls back to Some 0 for an unknown type. *)
       | Some "$bits" ->
           let last = ref None in
           walk (function SymbolIdentifier id -> last := Some id | _ -> ()) call_base;
           (match !last with
            | Some id -> (match Hashtbl.find_opt type_widths id with Some w -> Some w | None -> (match Hashtbl.find_opt signal_widths id with Some w -> Some w | None -> Some 0))
            | None -> Some 0)
       | _ ->
           (* General compile-time evaluation of a USER function call (e.g.
              prim_util_pkg::vbits): look the callee up by its member name in the
              package function table, bind its formals to the evaluated actual
              args, and evaluate its `return <expr>` body — no per-function
              special-casing.  Falls back to resolving ref_node as a plain
              reference when it is not a callable const function. *)
           (match !last_id with
            | Some fn ->
                (match eval_user_fn ~pkgs ~params fn call_base with
                 | Some v -> Some v
                 | None -> eval_int ~pkgs ~params ref_node)
            | None -> eval_int ~pkgs ~params ref_node))
  | TUPLE6 (STRING tag, _casting_type, _, _, inner, _) when prefix_is "cast" tag ->
      (* `T'(expr)` type/size cast — cast1(casting_type, ', (, expr, )).
         For constant folding the cast value IS the inner value (width
         truncation is irrelevant to the small register-index localparams
         that use this, e.g. `DataEnd = dm::dm_csr_e'(dm::Data0 +
         {4'h0, dm::DataCount} - 8'h1)`).  Without this the whole localparam
         failed to fold and stayed unbound, so dm_csrs' `[(Data0):DataEnd]`
         range bound tied to 0. *)
      eval_int ~pkgs ~params inner
  (* SVA-grammar transparent wrappers Verible inserts inside `( … )`: a
     parenthesised expression nests as
       expr_primary_parens → sequence_repetition_expr → expression_or_dist → <expr>
     so the comparison inside `(DmBaseAddress != 0)` was never reached.  Strip
     them so eval_int recurses to the real operand. *)
  | TUPLE3 (STRING tag, inner, _)
    when prefix_is "sequence_repetition_expr" tag
      || prefix_is "expression_or_dist"       tag ->
      eval_int ~pkgs ~params inner
  | TUPLE4 (STRING tag, _, inner, _) when prefix_is "expr_primary_parens" tag ->
      (* `( <expr> )` — TUPLE4(tag, LPAREN, inner_expression, RPAREN).
         The grammar wraps the body via expr_mintypmax →
         property_expr_or_assignment_list which collects elements
         into a singleton TLIST even for a plain parenthesised
         expression, so unwrap that before recursing. The previous
         collect_by heuristic skipped past wrappers and picked the
         first leaf SymbolIdentifier / TK_DecNumber, which made e.g.
         `(VOCAB_SIZE * 16)` evaluate to 27 instead of 432 because
         the mul_expr2 wrapper wasn't in the predicate. *)
      let rec unwrap = function
        | TLIST [single] -> unwrap single
        | TLIST (single :: _) -> single
        | other -> other
      in
      eval_int ~pkgs ~params (unwrap inner)
  (* Struct-member access `X.Y` on a parameter — Verible parses as
   * `reference2(reference, hierarchy_extension1(DOT, unqualified_id))`.
   * Used inside compile-time expressions like `$clog2(CVA6Cfg.INSTR_PER_FETCH)`
   * or signal widths `[CVA6Cfg.VLEN-1:0]`.  Lookup order:
   *   1. cur_struct_params (task #141) — built from `'{F: v, ...}`
   *      named-key defaults at extract_body_params time.  This is the
   *      *correct* semantics path; produces real per-field values.
   *   2. eval_int on X itself — for flat int params we treat any field
   *      as the param's value (lossy but produces non-zero output for
   *      simple cases).
   *   3. Fold to Some 0 — empty-config default matching slang's
   *      standalone elaboration for `<struct_t>'(0)` patterns.  This
   *      is the degenerate path and the reason the cva6 sweep
   *      progresses even without full struct elaboration. *)
  | TUPLE3 (STRING t, ref_node, TUPLE3 (STRING ht, _, ext_id))
    when prefix_is "reference2" t
      && prefix_is "hierarchy_extension1" ht ->
      let lhs_name = ref None in
      walk (function
        | SymbolIdentifier id when !lhs_name = None ->
            lhs_name := Some id
        | _ -> ()) ref_node;
      let field_name = ref None in
      walk (function
        | SymbolIdentifier id when !field_name = None ->
            field_name := Some id
        | _ -> ()) ext_id;
      (match !lhs_name, !field_name with
       | Some lhs, Some fld ->
           (match Hashtbl.find_opt cur_struct_params lhs with
            | Some pairs ->
                (match List.assoc_opt fld pairs with
                 | Some n -> Some n
                 | None -> Some 0)
            | None ->
                (match eval_int ~pkgs ~params ref_node with
                 | Some _ as some_int -> some_int
                 | None -> Some 0))
       | _ ->
           (match eval_int ~pkgs ~params ref_node with
            | Some _ as some_int -> some_int
            | None -> Some 0))
  | TUPLE2 (a, _) | TUPLE3 (_, a, _) -> eval_int ~pkgs ~params a
  | _ -> None

(* Compile-time evaluation of a USER function call by its DEFINITION (not by a
   hard-coded name): find the function among the package function tables, bind
   its formals to the evaluated actual args, and evaluate its `return <expr>`
   body.  Handles simple single-return const functions (prim_util_pkg::vbits,
   etc.); returns None for anything it cannot fold so the caller falls back. *)
and eval_user_fn ~pkgs ~params fname call_base =
  if !eval_fn_depth > 16 then None
  else begin
    incr eval_fn_depth;
    let fns =
      match !cached_pkg_fns with
      | Some f -> f
      | None ->
          let f = List.concat_map (fun (p : Verible_elaborate.package_decl) ->
            Verible_elaborate.extract_functions p.pkg_body) pkgs in
          cached_pkg_fns := Some f; f in
    let result =
      match List.find_opt (fun (f : Verible_elaborate.sv_function) ->
        f.Verible_elaborate.fn_name = fname) fns with
      | None -> None
      | Some f ->
          let ret =
            match collect_by (has_tag (prefix_is "jump_statement")) f.fn_body with
            | TUPLE4 (_, _, e, _) :: _ -> Some e
            | _ -> None in
          let formals = f.fn_args in
          (match ret with
           | None -> None
           | Some ret_e ->
               let actuals = call_args call_base in
               if List.length actuals <> List.length formals then None
               else
                 let rec bind fs acts acc = match fs, acts with
                   | [], [] -> Some acc
                   | fm :: fs', a :: acts' ->
                       (match eval_int ~pkgs ~params a with
                        | Some v -> bind fs' acts' ((fm, string_of_int v) :: acc)
                        | None -> None)
                   | _ -> None in
                 (match bind formals actuals [] with
                  | Some binds -> eval_int ~pkgs ~params:(binds @ params) ret_e
                  | None -> None))
    in
    decr eval_fn_depth;
    result
  end

(* `decl_variable_dimension1`: TUPLE6(tag, LBRACK, msb_expr, COLON,
 * lsb_expr, RBRACK). Pull the msb/lsb subtrees directly — collect_by
 * would also descend into them and pick up nested expressions, which
 * shifts the lsb. *)
let extract_range ~pkgs ~params tok =
  let pairs = collect_by (has_tag (prefix_is "decl_variable_dimension")) tok in
  match pairs with
  | (TUPLE6 (STRING _, _lb, msb, _colon, lsb, _rb)) :: _ ->
      let m = eval_int ~pkgs ~params msb in
      let l = eval_int ~pkgs ~params lsb in
      (match m, l with
       | Some mi, Some li -> Some (mi, li)
       | _ -> None)
  (* `[N]` single-value SIZE form (unpacked `foo [N]` / `foo [PARAM]`):
     grammar `decl_variable_dimension2` = TUPLE4(tag, LBRACK, size, RBRACK).
     Size N means indices [N-1:0].  Without this the unpacked array
     dimension is dropped and `logic [W:0] arr [N]` collapses to a scalar
     BInt[W] — arr[k>0] then reads/writes out of range, leaving driverless
     element wires. *)
  | (TUPLE4 (STRING tag, _lb, size_e, _rb)) :: _
    when prefix_is "decl_variable_dimension2" tag ->
      (match eval_int ~pkgs ~params size_e with
       | Some n when n >= 1 -> Some (n - 1, 0)
       | _ -> None)
  | _ -> None

(* Return every packed dimension as `(msb, lsb)`, in declaration
 * order (outermost first). For `logic [WIDTH-1:0][NumLevels-1:0]
 * index_lut`, returns [(WIDTH-1, 0); (NumLevels-1, 0)] — the outer
 * dim is the array index, the inner is the per-element width. The
 * grammar shape for chained dimensions is left-recursive
 * `decl_dimensions2`, so collect_by (prefix_is "decl_variable_dimension")
 * already finds them all; this helper just evaluates each. *)
let extract_packed_dims ~pkgs ~params tok =
  let pairs = collect_by
    (has_tag (prefix_is "decl_variable_dimension")) tok in
  List.filter_map (fun n ->
    match n with
    | TUPLE6 (STRING _, _, msb, _, lsb, _) ->
        let m = eval_int ~pkgs ~params msb in
        let l = eval_int ~pkgs ~params lsb in
        (match m, l with
         | Some mi, Some li -> Some (mi, li)
         | _ -> None)
    (* `[N]` single-value size form (unpacked array dim `arr [N]`) →
       [N-1:0]; see extract_range.  Keeps arrayed ports (`logic [W:0] p [N]`)
       as BArray[N x (W+1)] rather than collapsing to BInt[W+1]. *)
    | TUPLE4 (STRING tag, _, size_e, _)
      when prefix_is "decl_variable_dimension2" tag ->
        (match eval_int ~pkgs ~params size_e with
         | Some n when n >= 1 -> Some (n - 1, 0)
         | _ -> None)
    | _ -> None
  ) pairs

(* Extract every `typedef <data_type> <name>;` from the module body and
 * return [(name, width)]. Verible parses these as `type_declaration1`
 * = TUPLE6(tag, Typedef, data_type, GenericIdentifier, decl_dimensions_opt, ;).
 * Width comes from the inner data_type subtree (we just reuse
 * extract_range, which finds the packed dimension wherever it is). *)
let extract_typedefs ~pkgs ~params tok =
  let nodes = collect_by (has_tag (prefix_is "type_declaration")) tok in
  (* Local width map, so a struct field typed by an enum/typedef declared in
     the SAME scope resolves regardless of declaration order (dmi_req_t.op :
     dtm_op_e).  Consult local first, then the global type_widths. *)
  let local : (string, int) Hashtbl.t = Hashtbl.create 32 in
  let width_of_node n = match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _) ->
        (* Check struct first — extract_range would otherwise dive
         * into the first field's packed dim and short-circuit. *)
        let members = collect_by
          (has_tag (prefix_is "struct_union_member")) data_type in
        if members <> [] then
          let total = List.fold_left (fun acc m ->
            match extract_range ~pkgs ~params m with
            | Some (mb, lb) -> acc + abs (mb - lb) + 1
            | None ->
                (* enum/typedef-typed field (`dtm_op_e op`): width is the field
                   TYPE's, not 1.  This feeds type_widths → the struct PORT
                   width; dmi_req_t must be 41 (op=2) so dm_csrs' dmi_req_i port
                   aligns with the 41-bit JTAG DR (else DTM_WRITE mis-decodes). *)
                let tw = ref None in
                walk (function
                  | SymbolIdentifier id when !tw = None ->
                      (match Hashtbl.find_opt local id with
                       | Some w -> tw := Some w
                       | None ->
                         (match Hashtbl.find_opt type_widths id with
                          | Some w -> tw := Some w | None -> ()))
                  | _ -> ()) m;
                acc + (match !tw with Some w -> w | None -> 1)
          ) 0 members in
          if total > 0 then Some (nm, total) else None
        else
          (match extract_range ~pkgs ~params data_type with
           | Some (m, l) -> Some (nm, abs (m - l) + 1)
           | None -> None)
    | _ -> None in
  (* Pass 1: populate `local` (enums/scalars resolve immediately; structs whose
     enum fields aren't in `local` yet get a provisional width). *)
  List.iter (fun n -> match width_of_node n with
    | Some (nm, w) -> Hashtbl.replace local nm w | None -> ()) nodes;
  (* Pass 2: recompute — struct fields now see their enum types in `local`. *)
  List.filter_map width_of_node nodes

(* For each `typedef struct packed { ... } t;`, extract an ordered
 * list of (field_name, width). Field declaration order is MSB →
 * LSB per SV semantics: `'{f1: x, f2: y}` packs f1 in the high
 * bits, f2 in the low bits. The struct_union_member nodes appear in
 * source (= MSB-first) order in Verible's AST. *)
let extract_struct_defs ~pkgs ~params tok
    : (string * (string * int) list) list =
  let nodes = collect_by (has_tag (prefix_is "type_declaration")) tok in
  List.filter_map (fun n -> match n with
    | TUPLE6 (_, _, data_type, SymbolIdentifier nm, _, _)
      when (let ms = collect_by
              (has_tag (prefix_is "struct_union_member")) data_type in
            ms <> []) ->
        (* The struct_union_member_list TLIST is left-recursive, so
         * collect_by ends up walking it in reverse source order; the
         * final List.rev in collect_by un-reverses *that* but leaves
         * the original reversal in place. Reverse here to recover
         * source (= MSB-first) order. *)
        let members = List.rev (collect_by
          (has_tag (prefix_is "struct_union_member")) data_type) in
        let fields = List.filter_map (fun m ->
          let w = match extract_range ~pkgs ~params m with
            | Some (mb, lb) -> abs (mb - lb) + 1
            | None ->
                (* No packed dim — an enum/typedef-typed field (`dtm_op_e op`).
                   Its width is that of the field's TYPE, not 1: find the first
                   identifier that is a known type width (the type precedes the
                   field name, so it's matched first).  Without this a struct
                   with an enum field (dmi_req_t.op : dtm_op_e) sums to the
                   wrong total width and every field slice on it is corrupt. *)
                let tw = ref None in
                walk (function
                  | SymbolIdentifier id when !tw = None ->
                      (match Hashtbl.find_opt type_widths id with
                       | Some w -> tw := Some w | None -> ())
                  | _ -> ()) m;
                (match !tw with Some w -> w | None -> 1)
          in
          (* Field name is the DECLARATOR.  A packed-struct member is
             `<type> <name>`, so the declarator is the LAST identifier that is
             NOT a known type name (the type — possibly `pkg::enum_t` — precedes
             it; range/param identifiers like `[FOO:0]` are not types either but
             also precede the name).  Picking the FIRST identifier wrongly named
             a typedef/enum-typed field after its TYPE (`dm::dtm_op_e op` ->
             `dtm_op_e`), so `.op` member access never resolved; picking the
             plain LAST could grab a trailing type token.  Last-non-type is the
             declarator in both `logic [W] addr` and `dm::dtm_op_e op`. *)
          let fname = ref None in
          walk (function
            | SymbolIdentifier id when not (Hashtbl.mem type_widths id) ->
                fname := Some id
            | _ -> ()) m;
          (* Degenerate: every identifier was a type name — fall back to last. *)
          if !fname = None then
            walk (function SymbolIdentifier id -> fname := Some id | _ -> ()) m;
          match !fname with
          | Some id -> Some (id, w)
          | None -> None
        ) members in
        if fields <> [] then Some (nm, fields) else None
    | _ -> None
  ) nodes

let width_of ?(typedefs = []) ~pkgs ~params tok =
  match extract_range ~pkgs ~params tok with
  | Some (m, l) -> abs (m - l) + 1
  | None ->
      (* No explicit packed dimension — try integer_atom_type next:
       * `integer i`, `int x`, `byte b`, … carry their bit-width
       * implicitly. Verible's grammar wraps the atom token inside
       * `data_type_primitive_scalar5`, so we search for it before
       * falling back to typedef lookup. *)
      let atom_w = ref None in
      let rec scan = function
        | TUPLE3 (STRING t, atom, _)
          when prefix_is "data_type_primitive_scalar5" t
               && !atom_w = None ->
            (match atom with
             | Byte -> atom_w := Some 8
             | Shortint -> atom_w := Some 16
             | Int | Integer | Time -> atom_w := Some 32
             | Longint -> atom_w := Some 64
             | _ -> ())
        | TUPLE2 (a, b) -> scan a; scan b
        | TUPLE3 (a, b, c) -> scan a; scan b; scan c
        | TUPLE4 (a, b, c, d) -> scan a; scan b; scan c; scan d
        | TUPLE5 (a, b, c, d, e) -> List.iter scan [a; b; c; d; e]
        | TUPLE6 (a, b, c, d, e, f) -> List.iter scan [a; b; c; d; e; f]
        | TUPLE7 (a, b, c, d, e, f, g) -> List.iter scan [a; b; c; d; e; f; g]
        | TLIST xs -> List.iter scan xs
        | _ -> ()
      in
      scan tok;
      (match !atom_w with
       | Some w -> w
       | None ->
           (* No atom either — the type might be a typedef reference like
            * `state_type CState` or a package-qualified struct `dm::dtmcs_t`.
            * Take the LAST SymbolIdentifier (the type name AFTER any `pkg::`
            * qualifier) and look it up in the local typedef map, then the
            * global type_widths (packed struct/enum widths from all modules +
            * packages).  Without the global fallback, a struct-typed signal
            * `dm::dtmcs_t dtmcs_d` got width 1 -> the 32-bit dtmcs literal
            * truncated to 1 bit -> JTAG DTM dead (dtmcs.abits reads 0). *)
           let nm = ref None in
           walk (function SymbolIdentifier id -> nm := Some id | _ -> ()) tok;
           (match !nm with
            | Some id ->
                (match List.assoc_opt id typedefs with
                 | Some w -> w
                 | None ->
                     (match Hashtbl.find_opt type_widths id with
                      | Some w -> w | None -> 1))
            | None -> 1))

(* `typedef enum logic [N-1:0] { A, B = 5, C, … } t;` — every enum
 * item folds to its integer value. Default sequential numbering
 * starts at 0 and increments; an explicit `= expr` resets the
 * counter. Returns [(item_name, decimal_string)] which can be merged
 * into params so expr_to_bexpr substitutes uses inline. *)
let extract_enum_items ~pkgs ~params tok =
  let nodes = collect_by (has_tag (prefix_is "enum_data_type")) tok in
  List.concat_map (fun n ->
    let list_node = match n with
      | TUPLE5 (_, _, _, ln, _) -> ln  (* enum_data_type1: no base type *)
      | TUPLE6 (_, _, _, _, ln, _) -> ln  (* enum_data_type2: with base *)
      | _ -> EMPTY_TOKEN
    in
    (* The enum's declared base width (`enum logic [N-1:0]`) — from the packed
       dim on the base type, BEFORE the `{...}` member list.  Members inherit it
       so they carry the right width in concats (default 32 corrupts op fields). *)
    let enum_w =
      match extract_range ~pkgs ~params n with
      | Some (m, l) -> Some (abs (m - l) + 1)
      | None -> None in
    (* enum_name_list is built left-recursively through
     * `enum_name_list_item_last` nodes that nest each item inside the
     * previous list. A simple TLIST iteration only sees the outermost
     * item. Walk the whole list_node and collect every name plus its
     * (optional) explicit value, then process in source order. *)
    let raw = ref [] in
    let consume_item item =
      let nm = ref None in
      walk (function
        | SymbolIdentifier id when !nm = None -> nm := Some id
        | _ -> ()) item;
      let value_node = match item with
        | TUPLE4 (STRING t, _, _, expr) when prefix_is "enum_name" t ->
            Some expr
        | _ -> None
      in
      (match !nm with
       | Some id -> raw := (id, value_node) :: !raw
       | None -> ())
    in
    (* Walk the list_node — every enum_name-tagged node is a wrapped
     * variant; a passthrough bare GenericIdentifier appears as a
     * SymbolIdentifier outside any enum_name wrapper. The flat-list
     * trick: collect items by matching either the enum_name shapes or
     * bare SymbolIdentifiers that aren't already inside one. *)
    (* Strict tag match — `prefix_is "enum_name"` would also catch
     * `enum_name_list_item_last1` and friends. *)
    let is_enum_name_tag t =
      t = "enum_name1" || t = "enum_name2" || t = "enum_name3"
      || t = "enum_name4" || t = "enum_name5" || t = "enum_name6"
    in
    let rec scan t =
      match t with
      | TUPLE4 (STRING tag, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | TUPLE5 (STRING tag, _, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | TUPLE7 (STRING tag, _, _, _, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | TUPLE9 (STRING tag, _, _, _, _, _, _, _, _) when is_enum_name_tag tag ->
          consume_item t
      | SymbolIdentifier id ->
          raw := (id, None) :: !raw
      | TUPLE2 (a, b) -> scan a; scan b
      | TUPLE3 (_, a, b) -> scan a; scan b
      | TUPLE4 (_, a, b, c) -> scan a; scan b; scan c
      | TUPLE5 (_, a, b, c, d) -> scan a; scan b; scan c; scan d
      | TUPLE6 (_, a, b, c, d, e) -> List.iter scan [a; b; c; d; e]
      | TUPLE7 (_, a, b, c, d, e, f) -> List.iter scan [a; b; c; d; e; f]
      | TLIST xs -> List.iter scan xs
      | _ -> ()
    in
    scan list_node;
    let items_in_source_order = List.rev !raw in
    let counter = ref 0 in
    List.filter_map (fun (id, value_node) ->
      let v = match value_node with
        | Some expr ->
            (match eval_int ~pkgs ~params expr with
             | Some n -> counter := n + 1; n
             | None -> let n = !counter in counter := !counter + 1; n)
        | None ->
            let n = !counter in counter := !counter + 1; n
      in
      (match enum_w with
       | Some w when w > 0 && not (Hashtbl.mem enum_member_widths id) ->
           Hashtbl.replace enum_member_widths id w
       | _ -> ());
      Some (id, string_of_int v)
    ) items_in_source_order
  ) nodes

(* ─── Module-scoped state for struct typedef lookup ──────────────── *)
(* Set by convert_module before walking each module body.  Read by
 * expr_to_bexpr's `'{f1: x, ...}` and `p.field` handlers and by the
 * signal-table builder when it sees a struct-typed declaration. *)
let cur_struct_defs : (string * (string * int) list) list ref = ref []
(* Map of in-module signal names → their struct typedef name (for
 * member-select on bare references like `p.field`). *)
let cur_signal_struct : (string * string) list ref = ref []

(* Per-module signal name → declared width. Populated by
 * `convert_module` before any `expr_to_bexpr` call so the operator
 * builders can compute a real `result_type` width instead of falling
 * back to dummy_bool. *)
let cur_signal_widths : (string * int) list ref = ref []

(* Per-module table of signals declared with a NON-ZERO packed LSB, e.g.
 * `logic [31:1] instr_addr_q`.  The emitted declaration is normalised to
 * zero-based `[width-1:0]` (BInt keeps only width), so every bit/part-
 * select written against the source indices must be REBASED by the
 * declared LSB to stay consistent with the zero-based declaration —
 * `instr_addr_q[31:1]` -> `[30:0]`, `instr_addr_q[1:1]` -> `[0:0]`.
 * Without this, ibex_fetch_fifo reads out-of-range/off-by-one bits and
 * the fetch aligner picks the wrong half-word.  Maps name -> lsb. *)
let cur_signal_lsb : (string * int) list ref = ref []

(* Build a BSlice, rebasing the indices when the base is a plain signal
 * declared with a non-zero LSB (see cur_signal_lsb).  A no-op for the
 * common zero-LSB case. *)
let mk_bslice signal msb lsb =
  match signal with
  | BVar name ->
      (match List.assoc_opt name !cur_signal_lsb with
       | Some l when l <> 0 -> BSlice { signal; msb = msb - l; lsb = lsb - l }
       | _ -> BSlice { signal; msb; lsb })
  | _ -> BSlice { signal; msb; lsb }

(* Per-module table of array-typed localparams from .svh includes /
   inline `'{e1, e2, …}` initialisers.  Maps the localparam name to
   the list of element bexprs in source order (index 0 = first elem).
   Consulted by the reference3 walker: `LUT[i]` with i a known
   integer literal returns the i-th element directly; with i a
   runtime signal returns a balanced BCond mux tree over the
   elements.  Task #139's ROM-promotion path.                       *)
let cur_array_params : (string, bexpr list) Hashtbl.t = Hashtbl.create 4

(* Real declaration site for `cur_struct_params` (forward-declared
 * above so eval_int's `reference2 + hierarchy_extension1` arm can
 * see it).  Maps a parameter name to its struct field bindings (task
 * #141), where the bindings come from a named-key
 * assignment-pattern default `'{F1: v1, F2: v2}` in
 * extract_body_params.  The `<struct_t>'(0)` cast case is handled
 * by the Some 0 fallback in eval_int — no entry needed here. *)

(* ─── Expression conversion ──────────────────────────────────────── *)

let dummy_bool = BInt { width = 1; signed = Unsigned }

(* Parse a parameter value string that came in via specialise_design /
 * extract_body_params. Handles plain integers (`255`, `-1`), SV-style
 * sized literals (`8'd255`, `64'hFF`, `1'b1`), and the all-ones short-
 * hand `'1`. Falls back to `BVar id` if the value isn't recognisable
 * as a numeric constant — that way `parameter type T = …` keeps its
 * symbolic name. *)
(* Params whose value could not be folded to a constant, already warned about
   (dedup so one unresolved param does not spam per reference). *)
let unresolved_param_warned : (string, unit) Hashtbl.t = Hashtbl.create 16

let param_value_to_bexpr id v =
  let v = String.trim v in
  if v = "" then BVar id
  else if String.length v >= 2 && v.[0] = '"' && v.[String.length v - 1] = '"'
  then begin
    (* STRING parameter value ("GALOIS"): pack as ASCII byte vector, the
       same encoding TK_StringLiteral uses, so `LFSR_CONFIG == "GALOIS"`
       folds to a constant compare instead of leaving an unbound BVar
       (which read as 0 and disabled rgmii_lfsr's CRC mask generation). *)
    let s = String.sub v 1 (String.length v - 2) in
    let z = ref Z.zero in
    String.iter (fun c ->
      z := Z.logor (Z.shift_left !z 8) (Z.of_int (Char.code c land 0xff))) s;
    BConst { value = !z; width = 8 * String.length s }
  end
  else
    let parse_radix base s =
      try Some (int_of_string ("0" ^ base ^ s))
      with _ -> None in
    let parse_dec s = try Some (int_of_string s) with _ -> None in
    (* Sized literal: <w>'<base><digits>. Width determines bit-width. *)
    (match String.index_opt v '\'' with
     | Some i when i > 0 && i + 1 < String.length v ->
         let w_str = String.sub v 0 i in
         let base = v.[i + 1] in
         let digits = String.sub v (i + 2) (String.length v - i - 2) in
         let w = try int_of_string w_str with _ -> 32 in
         let n = match base with
           | 'd' | 'D' -> parse_dec digits
           | 'h' | 'H' -> parse_radix "x" digits
           | 'b' | 'B' -> parse_radix "b" digits
           | 'o' | 'O' -> parse_radix "o" digits
           | _ -> None in
         (match n with
          | Some n -> BConst { value = Z.of_int n; width = w }
          | None -> BVar id)
     | Some 0 ->
         (* `'1` / `'0` — width inferred elsewhere; use 32 as default *)
         (try
           let bit = String.sub v 1 (String.length v - 1) in
           if bit = "1" then BConst { value = Z.minus_one; width = 32 }
           else if bit = "0" then BConst { value = Z.zero; width = 32 }
           else BVar id
          with _ -> BVar id)
     | _ ->
         (match parse_dec v with
          | Some n ->
              (* An ENUM MEMBER (DMINoError, …) carries its enum's declared
                 width so it packs correctly in a concat — default 32 corrupts
                 e.g. the 2-bit DMI op-status field.  Otherwise: wide unbased-
                 decimal params (48-bit FPGA_MAC) size to the value. *)
              let w =
                match Hashtbl.find_opt enum_member_widths id with
                | Some ew when ew >= Z.numbits (Z.of_int n) -> ew
                | _ -> max 32 (Z.numbits (Z.of_int n)) in
              BConst { value = Z.of_int n; width = w }
          | None -> BVar id))

(* Best-effort expr → bexpr translator. Walks one level at a time,
 * recursing where the shape is recognised. Anything else becomes a
 * 1-bit zero with a stderr note (when MITER_VERIBLE_DEBUG is set). *)
let debug_expr = lazy (Sys.getenv_opt "MITER_VERIBLE_DEBUG" <> None)

(* Unhandled-syntax safety net.  Each "fall back to BConst 0" path
 * in the expression walker is routed through [silent_zero], which by
 * default raises [Silent_zero_substitution] so we see immediately
 * which shapes we don't handle.  Setting SV_DECOMP_LENIENT=1 reverts
 * to the historical "log + return BConst 0" behaviour for situations
 * where surfacing the bug isn't an option (e.g. a downstream consumer
 * still depends on the wrong-but-survivable BIR).  The flip from
 * default-lenient to default-strict was task #139: the rope ORFS
 * layout produced an all-zero data path because Verible silently
 * substituted 0 for an .svh-defined array localparam; making the
 * default loud means we catch the next instance at parse time
 * instead of post-route.                                              *)
exception Silent_zero_substitution of string

let lenient_mode = lazy (Sys.getenv_opt "SV_DECOMP_LENIENT" = Some "1")

let silent_zero ~reason ~width =
  if Lazy.force lenient_mode then begin
    if Lazy.force debug_expr then
      Printf.eprintf "[verible_to_bir] silent zero: %s\n" reason;
    BConst { value = Z.zero; width }
  end else
    raise (Silent_zero_substitution reason)

(* One-level shape introspection for the generic catch-all in the
 * expression walker.  Returns e.g. "TUPLE4(STRING(\"foo\"), Bar,
 * TLIST[3], Baz)" — outermost constructor plus the constructor name
 * of each child (and the embedded STRING tag for tagged tuples).
 * Lets us see *which* CST shape the converter is missing instead of
 * a blanket "no matching pattern" message. *)
let shape_of_tok (t : Source_text_verible.token) : string =
  let open Source_text_verible in
  let str = Source_text_verible_tokens.getstr in
  let child_desc c =
    match c with
    | STRING s -> Printf.sprintf "STRING(%S)" s
    | TLIST xs -> Printf.sprintf "TLIST[%d]" (List.length xs)
    | _ -> str c
  in
  match t with
  | TUPLE2 (a, b)                -> Printf.sprintf "TUPLE2(%s, %s)" (child_desc a) (child_desc b)
  | TUPLE3 (a, b, c)             -> Printf.sprintf "TUPLE3(%s, %s, %s)" (child_desc a) (child_desc b) (child_desc c)
  | TUPLE4 (a, b, c, d)          -> Printf.sprintf "TUPLE4(%s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d)
  | TUPLE5 (a, b, c, d, e)       -> Printf.sprintf "TUPLE5(%s, %s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d) (child_desc e)
  | TUPLE6 (a, b, c, d, e, f)    -> Printf.sprintf "TUPLE6(%s, %s, %s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d) (child_desc e) (child_desc f)
  | TUPLE7 (a, b, c, d, e, f, g) -> Printf.sprintf "TUPLE7(%s, %s, %s, %s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d) (child_desc e) (child_desc f) (child_desc g)
  | TUPLE8 (a, b, c, d, e, f, g, h)             -> Printf.sprintf "TUPLE8(%s, %s, %s, %s, %s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d) (child_desc e) (child_desc f) (child_desc g) (child_desc h)
  | TUPLE9 (a, b, c, d, e, f, g, h, i)          -> Printf.sprintf "TUPLE9(%s, %s, %s, %s, %s, %s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d) (child_desc e) (child_desc f) (child_desc g) (child_desc h) (child_desc i)
  | TUPLE10 (a, b, c, d, e, f, g, h, i, j)      -> Printf.sprintf "TUPLE10(%s, %s, %s, %s, %s, %s, %s, %s, %s, %s)" (child_desc a) (child_desc b) (child_desc c) (child_desc d) (child_desc e) (child_desc f) (child_desc g) (child_desc h) (child_desc i) (child_desc j)
  | TLIST xs                     -> Printf.sprintf "TLIST[%d](%s)" (List.length xs) (String.concat "," (List.map child_desc xs))
  | STRING s                     -> Printf.sprintf "STRING(%S)" s
  | _ -> str t

(* Recursive width computation over a bexpr against `cur_signal_widths`.
 * Returns `Some w` when computable; `None` when we can't tell (caller
 * falls back to `dummy_bool`, and z3_miter's fixed-point inference
 * picks it up from operands later). *)
let rec width_of_bexpr_ctx widths = function
  | BVar n -> List.assoc_opt n widths
  | BConst { width; _ } -> Some width
  | BBinOp { op = (BEq|BNe|BLt|BLe|BGt|BGe); _ } -> Some 1
  | BBinOp { op = _; lhs; rhs; result_type } ->
      let dw = match result_type with BInt { width; _ } -> width | _ -> 0 in
      if dw > 1 then Some dw
      else
        (match width_of_bexpr_ctx widths lhs,
               width_of_bexpr_ctx widths rhs with
         | Some a, Some b -> Some (max a b)
         | Some a, None | None, Some a -> Some a
         | None, None -> None)
  | BUnOp { op = (BRedAnd|BRedOr|BRedXor); _ } -> Some 1
  | BUnOp { operand; _ } -> width_of_bexpr_ctx widths operand
  | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
  | BConcat es ->
      let ws = List.map (width_of_bexpr_ctx widths) es in
      if List.for_all Option.is_some ws then
        Some (List.fold_left (+) 0
                (List.map (Option.value ~default:0) ws))
      else None
  | BReplicate { count; value } ->
      Option.map (fun w -> count * w) (width_of_bexpr_ctx widths value)
  | BCond { then_val; else_val; _ } ->
      (match width_of_bexpr_ctx widths then_val,
             width_of_bexpr_ctx widths else_val with
       | Some a, Some b -> Some (max a b)
       | Some a, None | None, Some a -> Some a
       | _ -> None)
  | BSelect _ | BCall _ -> None

let result_type_for op lhs rhs =
  let comparison = match op with
    | BEq | BNe | BLt | BLe | BGt | BGe -> true
    | _ -> false in
  if comparison then BInt { width = 1; signed = Unsigned }
  else
    let widths = !cur_signal_widths in
    match width_of_bexpr_ctx widths lhs,
          width_of_bexpr_ctx widths rhs with
    | Some a, Some b ->
        BInt { width = max a b; signed = Unsigned }
    | Some a, None | None, Some a ->
        BInt { width = a; signed = Unsigned }
    | None, None ->
        BInt { width = 1; signed = Unsigned }   (* legacy dummy_bool fallback *)

let result_type_for_un op operand =
  let reduction = match op with
    | BRedAnd | BRedOr | BRedXor -> true
    | _ -> false in
  if reduction then BInt { width = 1; signed = Unsigned }
  else
    match width_of_bexpr_ctx !cur_signal_widths operand with
    | Some w -> BInt { width = w; signed = Unsigned }
    | None -> BInt { width = 1; signed = Unsigned }

(* Pack a struct-typed localparam into a single BConst: field VALUES from
   cur_struct_params, field WIDTHS/order from cur_struct_defs (via the
   name->type map cur_signal_struct).  Lets a whole-struct or bit-sliced use
   (dm_top's `localparam dm::hartinfo_t DebugHartInfo = '{...}` referenced as
   DebugHartInfo[31:24]) fold instead of leaking as an undeclared identifier.
   MSB-first packing (SV packed-struct layout); unspecified fields default to 0
   per `'{...}` semantics.  None if the type or its layout is unknown. *)
let pack_struct_const id : bexpr option =
  match Hashtbl.find_opt cur_struct_params id,
        List.assoc_opt id !cur_signal_struct with
  | Some field_vals, Some sty ->
      (match List.assoc_opt sty !cur_struct_defs with
       | Some layout when layout <> [] ->
           let total = List.fold_left (fun a (_, w) -> a + max 0 w) 0 layout in
           let value = List.fold_left (fun acc (fname, w) ->
             let w = max 0 w in
             let fv = match List.assoc_opt fname field_vals with
               | Some v -> Z.of_int v | None -> Z.zero in
             let masked = Z.logand fv (Z.sub (Z.shift_left Z.one w) Z.one) in
             Z.logor (Z.shift_left acc w) masked) Z.zero layout in
           Some (BConst { value; width = total })
       | _ -> None)
  | _ -> None

let rec expr_to_bexpr ~pkgs ~params ~arrays tok =
  let recurse = expr_to_bexpr ~pkgs ~params ~arrays in
  let bin op a b =
    let lhs = recurse a in
    let rhs = recurse b in
    BBinOp { op; lhs; rhs; result_type = result_type_for op lhs rhs }
  in
  let un op a =
    let operand = recurse a in
    BUnOp { op; operand; result_type = result_type_for_un op operand }
  in
  (* Sized literal: TUPLE3(STRING "bin_based_number1",
   *   TK_BinBase "<W>'b", TK_BinDigits "<bits>")
   *   etc. Decode <W> and the digits. *)
  let parse_sized prefix base_token digits_token =
    let width =
      try
        let bs = match base_token with
          | TK_BinBase s | TK_HexBase s | TK_DecBase s | TK_OctBase s -> s
          | _ -> ""
        in
        let i = String.index bs '\'' in
        int_of_string (String.sub bs 0 i)
      with _ -> 32
    in
    let digits = match digits_token with
      | TK_BinDigits s | TK_HexDigits s
      | TK_DecDigits s | TK_OctDigits s -> s
      | _ -> "0"
    in
    (* Casez/casex/inside patterns use `?`, `x`, `z` as wildcard digits
     * (`53'b1???…?`).  int_of_string can't parse those.  Substitute `0`
     * for the wildcard bits to get a representative integer value —
     * sound for non-wildcard comparison ops; for wildcard ops (`==?`,
     * `casez`) the wildcard semantics is lost but the operand width
     * survives, which is what downstream width inference needs. *)
    let digits_clean =
      String.map (function
        | '?' | 'x' | 'X' | 'z' | 'Z' -> '0'
        | c -> c)
        (String.concat ""
           (String.split_on_char '_' digits))
    in
    (* OCaml's `int` is 63-bit (one bit reserved by the runtime), so
       any sized literal with more than ~62 effective bits overflows
       `int_of_string`.  Data-table .svh files use
       `128'h<128 hex digits>` patterns routinely (one entry per
       weight row).  Drop the high-order digits and keep the low-order
       portion that fits — this gives a BConst with the *correct
       declared width* but a representative low-bits value, which is
       enough for Z3 sort matching and lowering correctness on every
       op that doesn't actually depend on the high bits.  Math against
       the high bits is the rare case; if it comes up we'd need an
       arbitrary-precision BConst, which is a separate refactor.    *)
    (* Exact arbitrary-precision parse: BConst.value is Z.t, so wide literals
       (64'hFFFF..., 128'h...) are preserved verbatim -- no truncation.  prefix
       is the base char ("b"/"o"/"x"/"" for dec); Z.of_string wants "0x"/etc. *)
    let value =
      try Z.of_string ("0" ^ prefix ^ digits_clean)
      with _ ->
        if Lazy.force lenient_mode then Z.zero
        else raise (Silent_zero_substitution
          (Printf.sprintf "sized literal Z.of_string %S%S failed" prefix digits))
    in
    BConst { value; width }
  in
  match tok with
  | SymbolIdentifier id ->
      (match List.assoc_opt id params with
       | Some v ->
           (match param_value_to_bexpr id v with
            | BVar _ ->
                (* A PARAMETER whose value could not be folded to a constant
                   (e.g. an ibex_csr `.ResetValue({MSTATUS_RST_VAL})` struct-
                   localparam override that specialise_design cannot pack).  A
                   param is always a constant, so fold to 0 rather than emit an
                   undeclared identifier that xvlog rejects.  Warn (deduped). *)
                (if not (Hashtbl.mem unresolved_param_warned id) then begin
                   Hashtbl.replace unresolved_param_warned id ();
                   Printf.eprintf
                     "[verible_to_bir] WARN: param %s value %S did not fold to a \
                      constant — using 0\n%!" id v
                 end);
                BConst { value = Z.zero; width = 32 }
            | b -> b)
       | None -> (match pack_struct_const id with Some c -> c | None -> BVar id))
  | TK_DecNumber n | TK_UnBasedNumber n ->
      (* SV unbased-unsized literals: `'0`, `'1`, `'x`, `'z`. The bit
       * pattern broadcasts to the LHS width.  We don't have the LHS
       * width here, so tag with width=0 as a "fill at context width"
       * sentinel.  A later pass (Behavioral_const.expand_fill_consts)
       * walks the IR and rewrites these to BConsts of the appropriate
       * width based on the enclosing BAssign / BConcat sibling, so
       * `'1` becomes `64'hFFFFFFFFFFFFFFFF` on a 64-bit LHS rather
       * than the silently-truncated `32'hFFFFFFFF`. *)
      let n2 = String.trim n in
      if n2 = "'1" then BConst { value = Z.minus_one; width = 0 }
      else if n2 = "'0" then BConst { value = Z.zero; width = 0 }
      else if n2 = "'x" || n2 = "'z" || n2 = "'X" || n2 = "'Z" then
        BConst { value = Z.zero; width = 0 }
      else
        (try BConst { value = Z.of_string n; width = 32 }
         with _ ->
           silent_zero ~width:32
             ~reason:(Printf.sprintf "TK_DecNumber int_of_string %S failed" n))
  | TK_StringLiteral s ->
      (* SV string literal in an expression: a packed byte vector,
       * first character in the MSB, 8 bits per char (1800-2017
       * §5.9).  Pack the bytes into the BConst value (mod 2^63 for
       * strings longer than 7 chars — only equality against another
       * identically-packed literal needs to hold, which it does since
       * both operands flow through this same encoding) and set the
       * width to 8·len.  Most occurrences are `$error`/`$display`
       * arguments (irrelevant to synthesis) or string-parameter
       * comparisons like `WRITE_MODE_A == "WRITE_FIRST"`. *)
      let v = ref Z.zero in
      String.iter (fun c -> v := Z.logor (Z.shift_left !v 8) (Z.of_int (Char.code c land 0xff))) s;
      BConst { value = !v; width = 8 * String.length s }
  | TUPLE3 (STRING tag, base, digits) when prefix_is "bin_based_number" tag ->
      parse_sized "b" base digits
  | TUPLE3 (STRING tag, base, digits) when prefix_is "hex_based_number" tag ->
      parse_sized "x" base digits
  | TUPLE3 (STRING tag, base, digits) when prefix_is "dec_based_number" tag ->
      parse_sized "" base digits
  | TUPLE3 (STRING tag, base, digits) when prefix_is "oct_based_number" tag ->
      parse_sized "o" base digits
  | TUPLE3 (STRING t, SymbolIdentifier id, _) when prefix_is "unqualified_id" t ->
      (* If the identifier is a known parameter, fold to its constant
       * value here so downstream Z3 sees a concrete int rather than a
       * free variable. *)
      (match List.assoc_opt id params with
       | Some v ->
           (match param_value_to_bexpr id v with
            | BVar _ ->
                (if not (Hashtbl.mem unresolved_param_warned id) then begin
                   Hashtbl.replace unresolved_param_warned id ();
                   Printf.eprintf
                     "[verible_to_bir] WARN: param %s value %S did not fold to a \
                      constant — using 0\n%!" id v
                 end);
                BConst { value = Z.zero; width = 32 }
            | b -> b)
       | None -> (match pack_struct_const id with Some c -> c | None -> BVar id))
  (* Array assignment pattern `'{e1, e2, …}` — Verible's
     `assignment_pattern1` wraps a TLIST of bare expressions (no key
     prefix, unlike the struct variant).  Distilled use case:
     array-typed localparam initialiser from an included .svh, as in
     test/regressions/svh_array_localparam.svh.  Without this case
     the walker fell into the silent-zero fallback and the whole
     array localparam value became 0 (task #139).  Emit BConcat of
     elements in source order; index lookups (`LUT[sel]`) are
     handled separately by the reference3 → BSelect path.            *)
  | TUPLE4 (STRING tag, _, body, _)
    when prefix_is "assignment_pattern1" tag ->
      (* The body is a left-recursive cons of `expression_list_proper`
         nodes:  TUPLE4(expression_list_proper, prev_list, comma, new_elem).
         The grammar grows the list to the LEFT (Verible parses the
         comma-separated list bottom-up), so the outermost node carries
         the LAST element on its right, with everything before it
         nested in the left child.  Walk the left spine accumulating
         right-side elements, then add the final non-list node
         (the leftmost element) at the front.                          *)
      let rec flatten_explist acc t =
        match t with
        | TUPLE4 (STRING tag', lhs, _comma, rhs)
          when prefix_is "expression_list_proper" tag' ->
            flatten_explist (rhs :: acc) lhs
        | TLIST xs ->
            (* Older / different shape — bare TLIST.  Append as-is. *)
            xs @ acc
        | other -> other :: acc
      in
      let elems = flatten_explist [] body in
      let exprs = List.map recurse elems in
      (match exprs with
       | [] -> BConst { value = Z.zero; width = 1 }  (* `'{}` — empty *)
       | [single] -> single
       | _ -> BConcat exprs)
  (* Struct assignment pattern `'{f1: x, f2: y}`. The Verible parse
   * is `assignment_pattern2` wrapping a TLIST of
   * `structure_or_array_pattern_expression1` nodes — each carries a
   * key (the field name) and a value expression. We emit a BConcat
   * in the field's DECLARED order (MSB-first). The struct typedef
   * is looked up via `cur_struct_defs`; if no typedef matches we
   * fall back to source-order BConcat.

     Special-case `'{default: <expr>}` (also accepts the wildcard
     pattern `'{default: '0}` which is by far the common form): the
     `default` keyword is a bare `Default` token, not a
     SymbolIdentifier, so the per-key walker would miss it and we'd
     return an empty pair list — historically emitted as `mem := {}`
     instead of `mem := 0`.  When any key is `Default` we take that
     entry's value as the broadcast fill and emit it directly; both
     slang and verilator lower `'{default:'0}` to a single-bit zero
     and rely on the array-write path to broadcast it.            *)
  | TUPLE4 (STRING tag, _, body, _)
    when prefix_is "assignment_pattern2" tag ->
      let is_default_key k =
        match k with
        | Default -> true
        | _ ->
            let saw_default = ref false in
            walk (function
              | Default -> saw_default := true
              | _ -> ()) k;
            !saw_default
      in
      let default_value = ref None in
      let pairs = match body with
        | TLIST xs ->
            List.filter_map (fun e -> match e with
              | TUPLE4 (STRING t, key, _, value)
                when prefix_is "structure_or_array_pattern_expression" t ->
                  if is_default_key key then begin
                    if !default_value = None then
                      default_value := Some (recurse value);
                    None
                  end else
                  let kname = ref None in
                  walk (function
                    | SymbolIdentifier id when !kname = None ->
                        kname := Some id
                    | _ -> ()) key;
                  (match !kname with
                   | Some k -> Some (k, recurse value)
                   | None -> None)
              | _ -> None) (List.rev xs)
        | _ -> []
      in
      (* A `'{default: v}` with no other keys → broadcast fill.
         Both slang and verilator lower this to the bare value (typically
         `'0` → `1'b0`) and rely on the array-write path to broadcast it
         across all elements. *)
      (match !default_value, pairs with
       | Some v, [] -> v
       | _ ->
          (* Pick a typedef whose field set EXACTLY matches the keys —
           * this is how we link `'{hi: x, lo: y}` back to `pair_t`. *)
          let key_set = List.map fst pairs |> List.sort compare in
          let matching_def =
            List.find_opt (fun (_, fields) ->
              List.map fst fields |> List.sort compare = key_set
            ) !cur_struct_defs
          in
          (match matching_def with
           | Some (_, fields) ->
               (* Size each field value to the field's DECLARED width, so an
                  unsized `'0` fill (which recurse gives a 32-bit BConst) or an
                  over-wide signal doesn't inflate/mis-align the packed struct.
                  Const -> re-width (masked); other -> truncate via BSlice
                  (out-of-range high bits zero-pad downstream). *)
               let size_field w e =
                 if w <= 0 then e else
                 match e with
                 | BConst { value; width } when width <> w ->
                     let mask =
                       if w >= 62 then Z.minus_one
                       else Z.sub (Z.shift_left Z.one w) Z.one in
                     BConst { value = Z.logand value mask; width = w }
                 | BConst _ -> e
                 | _ -> BSlice { signal = e; msb = w - 1; lsb = 0 }
               in
               let in_order =
                 List.map (fun (fname, w) ->
                   let v =
                     try List.assoc fname pairs
                     with Not_found ->
                       (match !default_value with
                        | Some v -> v
                        | None -> BConst { value = Z.zero; width = 1 })
                   in
                   size_field w v
                 ) fields
               in
               (match in_order with
                | [single] -> single
                | _ -> BConcat in_order)
           | None ->
               let exprs = List.map snd pairs in
               (match exprs with
                | [] ->
                    (match !default_value with
                     | Some v -> v
                     | None -> BConst { value = Z.zero; width = 1 })
                | [single] -> single
                | _ -> BConcat exprs)))
  (* Replication `{N{value}}` — Verible parses as `expr_primary_braces2`:
   *   TUPLE7(tag, LBRACE, value_range, LBRACE, expression_list_proper,
   *          RBRACE, RBRACE)
   * value_range is the count N (constant), and expression_list_proper
   * is the inner value to be replicated. Both ride on the standard
   * eval_int / recurse paths. *)
  | TUPLE7 (STRING tag, _, count_node, _, value_node, _, _)
    when prefix_is "expr_primary_braces2" tag ->
      let n = match eval_int ~pkgs ~params count_node with
        | Some n -> n
        | None -> 1
      in
      let value = recurse value_node in
      BReplicate { count = n; value }
  (* Concatenation `{a, b, c}` — Verible parses as
   * `range_list_in_braces1`: TUPLE4(tag, LBRACE, open_range_list, RBRACE).
   * The open_range_list is a TLIST of expressions (with COMMA separators
   * folded in); collect every expression-shaped child and concat them.
   * Note: SV concat orders MSB → LSB, which matches BConcat semantics. *)
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let exprs = match body with
        | TLIST xs ->
            (* TLIST is reverse source order (parser uses left recursion).
             * Reverse to preserve MSB → LSB ordering. *)
            List.filter_map (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN -> None
              | _ -> Some (recurse e)) (List.rev xs)
        | other -> [recurse other]
      in
      (match exprs with
       | [single] -> single
       | _ -> BConcat exprs)
  (* `system_tf_call1`: whitelisted elaboration helpers (see lexer
   * systask hashtable).  Route through eval_int to fold to a constant;
   * if eval_int can't fold ($bits of unknown type, etc.) fall back to
   * BConst 0 with width 1 — matches the empty-config strategy used
   * for $clog2 inner-arg failure. *)
  | TUPLE3 (STRING "system_tf_call1", SystemTFIdentifier name, call_base) as tok ->
      (* For value-preserving sign casts ($unsigned, $signed), recurse
         into the argument expression rather than collapsing to a
         constant.  Without this, `$unsigned({1'b0, D})` lost both
         operands of the concat and became a 1-bit zero — matching
         slang's old behaviour, but masking a real semantic divergence
         the moment slang's Call handler started producing BCall. *)
      (match name with
       | "$unsigned" | "$signed" ->
           let cands = collect_by (function
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "range_list_in_braces" t -> true
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "reference_or_call_base" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "reference" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "unqualified_id" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "bin_based_number" t
                 || prefix_is "hex_based_number" t
                 || prefix_is "dec_based_number" t
                 || prefix_is "oct_based_number" t -> true
             | TK_DecNumber _ | TK_UnBasedNumber _ -> true
             | _ -> false) call_base in
           let inner_b = match cands with
             | first :: _ -> recurse first
             | [] -> recurse call_base
           in
           (* Tag $signed(x) with an @signed marker — the downstream
              Behavioral_const.normalize_bcall_args pass uses it to
              pick sign-extension over zero-extension when widening
              the value to a signed formal's width, and to push the
              widening into the operands of a containing BBinOp so
              the arithmetic happens at the wide width rather than
              at the source-operand width.  $unsigned is already a
              no-op at the BV level. *)
           if name = "$signed" then
             BCall { func = "@signed"; args = [inner_b] }
           else
             inner_b
       | _ ->
           (match eval_int ~pkgs ~params tok with
            | Some n -> BConst { value = Z.of_int n; width = 32 }
            | None -> BConst { value = Z.zero; width = 1 }))
  (* Function-like call: `reference_or_call_base1`. The lexer treats
   * `$unsigned`/`$signed` as ordinary SymbolIdentifier (not
   * SystemTFIdentifier), so they parse through this shape too — both
   * are sign-cast no-ops at the Z3 BV level. Other recognised system
   * tasks: $clog2 (folded to a constant by eval_int — see above). *)
  | TUPLE3 (STRING "reference_or_call_base1", ref_node, call_base) ->
      (* A package-qualified callee `pkg::func(args)` (dm::nop(), dm::auipc(),
         the pulp-debug abstract-command instruction encoders) parses as a
         qualified_id whose FIRST identifier is the package and LAST is the
         function.  Taking the first id named the call `dm` and dropped the
         member, so Behavioral_inline could never substitute the constant
         function body — the whole abstract_cmd ROM stayed as opaque `dm()`
         calls.  For a qualified callee use the MEMBER (last id); otherwise the
         plain first id. *)
      let is_qualified = ref false in
      let first_id = ref None and last_id = ref None in
      walk (function
        | TUPLE4 (STRING t, _, _, _) when prefix_is "qualified_id" t ->
            is_qualified := true
        | SymbolIdentifier id ->
            if !first_id = None then first_id := Some id;
            last_id := Some id
        | _ -> ()) ref_node;
      let fname = ref (if !is_qualified then !last_id else !first_id) in
      (match !fname with
       | Some (("$unsigned" | "$signed") as cast_name) ->
           (* Find the inner expression argument.  `reference` matches
              indexed/struct/bit-select forms (reference2/3/...), and
              `reference_or_call_base` matches nested calls — without
              these the argument's index/slice is silently dropped
              (e.g. `$signed(x_mem[i])` would collapse to
              `BVar x_mem` when only `unqualified_id` was matched). *)
           let cands = collect_by (function
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "range_list_in_braces" t -> true
             | TUPLE4 (STRING t, _, _, _)
               when prefix_is "add_expr" t || prefix_is "mul_expr" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "reference_or_call_base" t -> true
             | TUPLE3 (STRING t, _, _)
               when prefix_is "reference" t -> true
             | TUPLE3 (STRING t, _, _) when prefix_is "unqualified_id" t -> true
             | TK_DecNumber _ -> true
             | _ -> false) call_base in
           let inner_b = match cands with
             | first :: _ -> recurse first
             | [] -> recurse call_base
           in
           (* Mark `$signed(x)` with a BCall sentinel so downstream
              passes (normalize_bcall_args) can sign-extend instead of
              zero-extend when widening the value to a wider context
              (e.g. the formal of a signed [31:0] function input).
              $unsigned is structurally a no-op for BV semantics — its
              value already lives in two's complement form. *)
           if cast_name = "$signed" then
             BCall { func = "@signed"; args = [inner_b] }
           else
             inner_b
       | Some name when (try name.[0] = '$' with _ -> false) ->
           (* Other $foo() — fall back to evaluating the integer
            * literal (works for $clog2 et al). *)
           (match eval_int ~pkgs ~params tok with
            | Some n -> BConst { value = Z.of_int n; width = 32 }
            | None ->
                let inner_shape =
                  try shape_of_tok call_base
                  with _ -> "<shape-print failed>" in
                let known_params =
                  String.concat ","
                    (List.map fst params |> List.sort compare) in
                silent_zero ~width:1
                  ~reason:(Printf.sprintf
                    "%s arg unresolvable: arg_shape=%s; known params=[%s]"
                    name inner_shape known_params))
       | Some fname ->
           (* Plain user function call. Emit BCall so the inline pass
            * (Behavioral_inline) can substitute the body in.
            *
            * Walk the call_base argument list at its structural
            * boundaries — `call_base1(LPAREN, argument_list_opt,
            * RPAREN)` carries a TLIST built left-recursively by
            * any_argument_list_item_last / trailing_comma rules. A
            * previous version used `collect_by` with a loose-leaf
            * predicate, which descended INTO each argument and
            * collected its sub-expressions as additional args (so
            * `apply_temperature_delta(a - b, c)` produced a 4-arg
            * call `(a - b, a, b, c)` instead of 2 args).
            *
            * The unfold below visits the list spine via the chained
            * any_argument_list_* tags, pushing the leaf any_argument
            * payloads to the front in source order. *)
           let arg_exprs =
             let arg_list_node = match call_base with
               | TUPLE4 (STRING t, _, body, _)
                 when prefix_is "call_base" t -> body
               | _ -> EMPTY_TOKEN
             in
             let rec collect_args = function
               | TLIST xs -> List.concat_map collect_args xs
               | TUPLE3 (STRING t, prev, last)
                 when prefix_is "any_argument_list_item_last" t ->
                   collect_args prev @ collect_args last
               | TUPLE3 (STRING t, prev, _)
                 when prefix_is "any_argument_list_trailing_comma" t ->
                   collect_args prev
               | TUPLE3 (STRING t, prev, last)
                 when prefix_is "any_argument_list_preprocessor_last" t ->
                   collect_args prev @ collect_args last
               | EMPTY_TOKEN -> []
               | other -> [other]
             in
             List.map recurse (collect_args arg_list_node)
           in
           BCall { func = fname; args = arg_exprs }
       | None ->
           silent_zero ~width:1
             ~reason:"function call with no resolvable callee name")
  (* Struct member-select `p.field` — Verible parses as `reference2`:
   *   TUPLE3(tag, reference, hierarchy_extension)
   * with `hierarchy_extension1: TUPLE3(tag, DOT, unqualified_id)`.
   * The signal name (typically a BVar) must be a struct-typed signal
   * in `cur_signal_struct`, and we look up the field's bit range in
   * `cur_struct_defs` to emit a BSlice. *)
  | TUPLE3 (STRING t, ref_node,
            TUPLE3 (STRING ht, _, ext_id))
    when prefix_is "reference2" t
      && prefix_is "hierarchy_extension1" ht ->
      let signal = recurse ref_node in
      let field_name = ref None in
      walk (function
        | SymbolIdentifier id when !field_name = None ->
            field_name := Some id
        | _ -> ()) ext_id;
      (match signal, !field_name with
       | BVar sig_name, Some fname ->
           (if Sys.getenv_opt "FIELD_DEBUG" <> None
               && String.length sig_name >= 7 && String.sub sig_name 0 7 = "dmi_req" then
              Printf.eprintf "[field] %s.%s in_signal_struct=%b in_struct_defs=%b\n%!"
                sig_name fname (List.mem_assoc sig_name !cur_signal_struct)
                (match List.assoc_opt sig_name !cur_signal_struct with
                 | Some sn -> List.mem_assoc sn !cur_struct_defs | None -> false));
           (match List.assoc_opt sig_name !cur_signal_struct with
            | Some struct_name ->
                (match List.assoc_opt struct_name !cur_struct_defs with
                 | Some fields ->
                     (* Field declared first is in the MSBs.  Bit
                      * range = [W - prefix_w - 1 : W - prefix_w - field_w]. *)
                     let total_w = List.fold_left (fun a (_, w) -> a + w)
                                     0 fields in
                     let rec find_bit_range pos = function
                       | [] -> None
                       | (n, w) :: rest ->
                           if n = fname then
                             Some (total_w - pos - 1,
                                   total_w - pos - w)
                           else find_bit_range (pos + w) rest
                     in
                     (match find_bit_range 0 fields with
                      | Some (msb, lsb) ->
                          BSlice { signal; msb; lsb }
                      | None -> signal)
                 | None -> signal)
            | None -> signal)
       | _ -> signal)
  (* Bit-select / part-select on a reference: `reference3` is
   *   TUPLE3(tag, reference, select_variable_dimension).
   * The inner dimension is either select_variable_dimension2 (single
   * index `[N]`, TUPLE4) or select_variable_dimension1 (range `[M:N]`,
   * TUPLE6). For packed regs we emit BSlice; for memory arrays
   * (signal name in `arrays`) the single-index case must emit
   * BSelect so meminfer recognises the read. *)
  | TUPLE3 (STRING t, ref_node, dim_node) when prefix_is "reference" t
                                              && t <> "reference1" ->
      let signal = recurse ref_node in
      (* Array-typed localparam ROM lookup (task #139).  If `signal`
         names a localparam whose RHS was an `'{e1, …, eN}` initialiser
         that we registered into [cur_array_params], and `dim_node` is
         a single-index `select_variable_dimension2`, return either the
         constant-folded element (if the index evaluates to an integer)
         or a balanced BCond mux tree over the elements (runtime index).
         Short-circuits before the rest of reference3 so the lookup
         doesn't fall through to the BVar fallback that drops [sel].   *)
      let array_param_elems = match signal with
        | BVar n -> Hashtbl.find_opt cur_array_params n
        | _ -> None in
      if Sys.getenv_opt "TRACE_ARRAY_PARAMS" = Some "1" then
        Printf.eprintf "[ref3] signal=%s array_param_elems=%s table_size=%d\n%!"
          (match signal with BVar n -> n | _ -> "<other>")
          (match array_param_elems with
           | Some es -> Printf.sprintf "Some[%d]" (List.length es)
           | None -> "None")
          (Hashtbl.length cur_array_params);
      (match array_param_elems, dim_node with
       | Some elems, TUPLE4 (STRING dt, _, idx, _)
         when prefix_is "select_variable_dimension" dt ->
           let idx_e = recurse idx in
           (match eval_int ~pkgs ~params idx with
            | Some i ->
                (try List.nth elems i
                 with _ -> BConst { value = Z.zero; width = 1 })
            | None ->
                let n = List.length elems in
                (* Balanced BCond mux: (idx == 0) ? e0 : (idx == 1) ? e1 : … *)
                let rec build i =
                  if i >= n - 1 then List.nth elems i
                  else BCond {
                    condition = BBinOp {
                      op = BEq;
                      lhs = idx_e;
                      rhs = BConst { value = Z.of_int i; width = 32 };
                      result_type = BInt { width = 1; signed = Unsigned } };
                    then_val = List.nth elems i;
                    else_val = build (i + 1) }
                in build 0)
       | _ ->
      let is_array_sig = match signal with
        | BVar n -> List.mem n arrays
        | _ -> false
      in
      (match dim_node with
       | TUPLE6 (STRING dt, _, e1, _, e2, _)
         when prefix_is "select_variable_dimension" dt ->
           let signal_width () = match signal with
             | BVar n -> List.assoc_opt n !cur_signal_widths
             | _ -> None in
           (* Distinguish dimension subtype:
                "1" → range  [m:l]   (e1=msb,  e2=lsb)
                "3" → up     [b +: w] (e1=base, e2=width)
                "4" → down   [b -: w] (e1=base, e2=width) *)
           let tag = String.sub dt
               (String.length "select_variable_dimension")
               (String.length dt
                - String.length "select_variable_dimension") in
           (match tag with
            | "3" ->
                (* `signal[base +: width]`: bits [base+width-1 : base].
                   For dynamic base we lower to `(signal >> base) &
                   {width{1'b1}}` so Verilator's pre-folded shift+mask
                   shape and our verible-side encoding land on the
                   same Z3 expression. A previous version emitted an
                   `@part_select_up` BCall and relied on a downstream
                   folder once the base went constant, but the BCall
                   became an uninterpreted function on the verible
                   side while Verilator's IR carried explicit
                   shift+mask, and the two miter sides diverged. *)
                (match eval_int ~pkgs ~params e1,
                       eval_int ~pkgs ~params e2 with
                 | Some b, Some w when w > 0 ->
                     mk_bslice signal (b + w - 1) b
                 | _, Some w when w > 0 ->
                     let base = recurse e1 in
                     let result_t = BInt { width = w; signed = Unsigned } in
                     let shifted = BBinOp {
                       op = BShr; lhs = signal; rhs = base;
                       result_type = result_t } in
                     let mask = BConst {
                       value = Z.sub (Z.shift_left Z.one w) Z.one;
                       width = w } in
                     BBinOp { op = BAnd; lhs = shifted; rhs = mask;
                              result_type = result_t }
                 | _ -> signal)
            | "4" ->
                (* `signal[base -: width]`: bits [base : base-width+1]. *)
                (match eval_int ~pkgs ~params e1,
                       eval_int ~pkgs ~params e2 with
                 | Some b, Some w when w > 0 ->
                     mk_bslice signal b (b - w + 1)
                 | _, Some w when w > 0 ->
                     (* signal[base -: width] = bits [base : base-w+1]
                        = (signal >> (base - w + 1)) & {w{1'b1}}. *)
                     let base = recurse e1 in
                     let result_t = BInt { width = w; signed = Unsigned } in
                     let base_lsb = BBinOp {
                       op = BSub;
                       lhs = base;
                       rhs = BConst { value = Z.of_int (w - 1); width = 32 };
                       result_type = BInt { width = 32; signed = Unsigned } } in
                     let shifted = BBinOp {
                       op = BShr; lhs = signal; rhs = base_lsb;
                       result_type = result_t } in
                     let mask = BConst {
                       value = Z.sub (Z.shift_left Z.one w) Z.one;
                       width = w } in
                     BBinOp { op = BAnd; lhs = shifted; rhs = mask;
                              result_type = result_t }
                 | _ -> signal)
            | _ ->
                (* dimension 1 (range) or unknown → existing range
                   logic with $high-style fallback. *)
                (match eval_int ~pkgs ~params e1,
                       eval_int ~pkgs ~params e2 with
                 | Some m, Some l -> mk_bslice signal m l
                 | None, Some l ->
                     (match signal_width () with
                      | Some w when w > 0 ->
                          BSlice { signal; msb = w - 1; lsb = l }
                      | _ -> signal)
                 | Some m, None ->
                     BSlice { signal; msb = m; lsb = 0 }
                 | _ -> signal))
       | TUPLE4 (STRING dt, _, idx, _)
         when prefix_is "select_variable_dimension" dt ->
           if is_array_sig then
             BSelect { array = signal; index = recurse idx }
           else
             (match eval_int ~pkgs ~params idx with
              | Some i -> mk_bslice signal i i
              | None ->
                  (* Dynamic-index single-bit select on a packed signal:
                     SV `signal[idx]` returns one bit.  Lower to
                     `(signal >> idx[ceil(log2 w)-1:0]) & 1` to match
                     Verilator, which pre-slices the index to the
                     minimum width that can address the signal.  Without
                     the slice an out-of-range index on the verible side
                     shifts everything out (→ 0) while verilator's
                     low-bits-only path returns the in-range bit, and
                     Z3 picks an out-of-range index as the
                     counterexample.

                     If the signal width is unknown, skip the slice;
                     same-shape encoding on both sides still beats
                     dropping the index entirely (the previous bug). *)
                  let sig_w = match signal with
                    | BVar n -> List.assoc_opt n !cur_signal_widths
                    | _ -> None in
                  let idx_e = recurse idx in
                  let idx_e =
                    match sig_w with
                    | Some w when w > 1 ->
                        (* Bit-length of (w-1) is the number of bits
                           needed to index a w-element signal:
                             w=64 → max idx 63 = 0b111111 → 6 bits. *)
                        let rec bit_length v =
                          if v = 0 then 0
                          else 1 + bit_length (v lsr 1)
                        in
                        let need = bit_length (w - 1) in
                        if need >= 1
                        then BSlice { signal = idx_e;
                                      msb = need - 1; lsb = 0 }
                        else idx_e
                    | _ -> idx_e
                  in
                  let one_bit = BConst { value = Z.one; width = 1 } in
                  let res_t = BInt { width = 1; signed = Unsigned } in
                  let shifted = BBinOp {
                    op = BShr;
                    lhs = signal;
                    rhs = idx_e;
                    result_type = res_t } in
                  BBinOp { op = BAnd;
                           lhs = shifted;
                           rhs = one_bit;
                           result_type = res_t })
       | _ -> signal))
  (* `unary_prefix_expr2`: TUPLE3(tag, unary_op_token, operand).
   * Must come before the generic TUPLE3 wrapper below — otherwise the
   * fallback recurses into the operator token and discards the operand.
   * unary_op variants we recognise: TILDE → bitwise NOT,
   * HYPHEN → arithmetic negation, AMPERSAND/VBAR/CARET → reductions
   * (AND/OR/XOR), TILDE_AMPERSAND/_VBAR/_CARET → reduce-then-NOT,
   * PLING → logical NOT (= reduce-OR + NOT), PLUS → no-op. *)
  | TUPLE3 (STRING tag, op_tok, operand) when prefix_is "unary_prefix_expr" tag ->
      let inner = recurse operand in
      let result_t = dummy_bool in
      let red op = BUnOp { op; operand = inner; result_type = result_t } in
      (match op_tok with
       | TILDE                -> BUnOp { op = BNot; operand = inner; result_type = result_t }
       | HYPHEN               -> BUnOp { op = BNeg; operand = inner; result_type = result_t }
       | PLUS                 -> inner
       | AMPERSAND            -> red BRedAnd
       | VBAR                 -> red BRedOr
       | CARET                -> red BRedXor
       | TILDE_AMPERSAND      ->
           BUnOp { op = BNot; operand = red BRedAnd; result_type = result_t }
       | TILDE_VBAR           ->
           BUnOp { op = BNot; operand = red BRedOr; result_type = result_t }
       | TILDE_CARET          ->
           BUnOp { op = BNot; operand = red BRedXor; result_type = result_t }
       | PLING                ->
           BUnOp { op = BNot; operand = red BRedOr; result_type = result_t }
       | _                    ->
           BUnOp { op = BNot; operand = inner; result_type = result_t })
  | TUPLE3 (STRING _, a, b) ->
      (* Generic single-content wrapper. Verible puts the meaningful
       * subtree in slot 1 most of the time (slot 2 is usually
       * EMPTY_TOKEN or punctuation). Prefer slot 1; fall back to
       * slot 2 if slot 1 isn't a tree. *)
      (match a with
       | EMPTY_TOKEN -> recurse b
       | _ -> recurse a)
  (* `**` constant power. BIR has no native power op, so we evaluate
   * eagerly. Common case after genvar-substitution is `2 ** 0` etc.
   * which folds to a small constant. *)
  | TUPLE4 (STRING tag, lhs, _op, rhs) when prefix_is "pow_expr2" tag ->
      (match eval_int ~pkgs ~params tok with
       | Some n ->
           (* Width: at minimum cover the value. 32-bit by default
            * matches what SV unsized-int literals get. *)
           BConst { value = Z.of_int n; width = 32 }
       | None ->
           (* No fold — best-effort: encode as repeated mul if rhs
            * is small. For the unsupported general case, fall back
            * to lhs (drops the power). *)
           ignore rhs; recurse lhs)
  | TUPLE4 (STRING tag, lhs, _op, rhs) ->
      (* Binary expressions: add_expr, mul_expr, comp_expr, and_expr,
       * or_expr, xor_expr, shift_expr, logeq_expr, etc. *)
      if prefix_is "add_expr2" tag then bin BAdd lhs rhs
      else if prefix_is "add_expr3" tag then bin BSub lhs rhs
      else if prefix_is "mul_expr2" tag then bin BMul lhs rhs
      else if prefix_is "mul_expr3" tag then bin BDiv lhs rhs
      else if prefix_is "mul_expr4" tag then bin BMod lhs rhs
      else if prefix_is "and_expr" tag || prefix_is "bitand_expr" tag
        then bin BAnd lhs rhs
      else if prefix_is "or_expr" tag || prefix_is "bitor_expr" tag
        then bin BOr lhs rhs
      else if prefix_is "xor_expr" tag || prefix_is "bitxor_expr" tag
        then bin BXor lhs rhs
      (* `&&` and `||` — logical AND/OR.  SV reduces each operand to
       * "non-zero" before combining (`cpuregs_write && latched_rd` with
       * latched_rd=5'b01010 yields true).  Lowering as bitwise BAnd/BOr
       * at mixed widths zero-extends the 1-bit operand, so the AND's
       * high bits become zero and a downstream if-condition OR-reduce
       * collapses to just bit 0 of the wider operand — exactly the
       * picorv32 cpuregs_write gate bug.  Wrap each operand in BRedOr
       * (if wider than 1) so both sides are 1-bit booleans before the
       * bitwise combine.  Caught by tests/operators/op_if_logand5. *)
      else if prefix_is "logand_expr" tag || prefix_is "logor_expr" tag then begin
        let op = if prefix_is "logand_expr" tag then BAnd else BOr in
        let reduce_to_bool e =
          let ew = match e with
            | BConst { width; _ } -> width
            | _ -> 32 in
          if ew <= 1 then e
          else BUnOp { op = BRedOr; operand = e;
                       result_type = BInt { width = 1; signed = Unsigned } } in
        let lb = reduce_to_bool (recurse lhs) in
        let rb = reduce_to_bool (recurse rhs) in
        BBinOp { op; lhs = lb; rhs = rb;
                 result_type = BInt { width = 1; signed = Unsigned } }
      end
      else if prefix_is "shift_expr2" tag then bin BShl lhs rhs
      else if prefix_is "shift_expr3" tag then bin BShr lhs rhs
      else if prefix_is "shift_expr4" tag then
        (* SV `>>>` is arithmetic right-shift but the LRM-spec'd fill
         * depends on the LHS type: signed operands fill with the
         * sign bit, unsigned operands fill with 0 (i.e. behave like
         * `>>`). Verilator pre-applies this distinction; on the
         * verible side our only signedness signal is whether the
         * LHS got tagged with `$signed(...)` (an @signed BCall). If
         * so emit BAshr, otherwise fall back to logical BShr so the
         * two miter sides match on the common unsigned-RHS shift
         * idiom `(unsigned_expr >>> k)`. *)
        let lhs_bexpr = recurse lhs in
        let is_signed_lhs = match lhs_bexpr with
          | BCall { func = "@signed"; _ } -> true
          | _ -> false
        in
        let op = if is_signed_lhs then BAshr else BShr in
        let rhs_bexpr = recurse rhs in
        BBinOp { op; lhs = lhs_bexpr; rhs = rhs_bexpr;
                 result_type = result_type_for op lhs_bexpr rhs_bexpr }
      else if prefix_is "comp_expr2" tag then bin BLt lhs rhs
      else if prefix_is "comp_expr3" tag then bin BGt lhs rhs
      else if prefix_is "comp_expr4" tag then bin BLe lhs rhs
      else if prefix_is "comp_expr5" tag then bin BGe lhs rhs
      else if prefix_is "logeq_expr2" tag || prefix_is "binary_eq_expr1" tag
        then bin BEq lhs rhs
      else if prefix_is "logeq_expr3" tag then bin BNe lhs rhs
      (* Case equality `===`/`!==` (caseeq_expr2/3) and wildcard
       * equality `==?`/`!=?` (logeq_expr4/5) degenerate to plain
       * `==`/`!=` in synthesis — X/Z don't exist in the synthesizable
       * subset, so the 4-state distinction collapses.  Xilinx unisim
       * sim models lean on `=== 1'b1` heavily. *)
      else if prefix_is "caseeq_expr2" tag || prefix_is "logeq_expr4" tag
        then bin BEq lhs rhs
      else if prefix_is "caseeq_expr3" tag || prefix_is "logeq_expr5" tag
        then bin BNe lhs rhs
      else if prefix_is "expr_primary_parens" tag then recurse _op
      (* `qualified_id2`: scope-qualified reference `scope::name`
       * (or `inst.field` — same shape, different separator).
       * Verilator -E flattening doesn't resolve `pkg::CONST` so the
       * flat cva6 source still has these.  Try the constant-folder
       * first; on miss, fall back to the rightmost name as a BVar,
       * matching how slang's flat elaboration renames bare `name`
       * after import resolution. *)
      else if prefix_is "qualified_id" tag then
        (match eval_int ~pkgs ~params
                 (TUPLE4 (STRING tag, lhs, _op, rhs)) with
         | Some n -> BConst { value = Z.of_int n; width = 32 }
         | None -> recurse rhs)
      (* `expression_list_proper`: left-recursive cons of a
       * comma-separated list.  Reaching this node in expression
       * context means an upstream handler forwarded a raw list
       * (e.g. a function-call argument list slipped through).
       * Flatten the spine and lower to BConcat — matches SV semantics
       * if the list came from a `{a, b, c}` context; for arg-list
       * misroutes it at least preserves all elements rather than
       * silently zeroing the expression. *)
      else if prefix_is "expression_list_proper" tag then begin
        let rec flatten acc t = match t with
          | TUPLE4 (STRING tag', x, _comma, y)
            when prefix_is "expression_list_proper" tag' ->
              flatten (y :: acc) x
          | other -> other :: acc
        in
        let raw = flatten [rhs] lhs in
        let exprs = List.map recurse raw in
        match exprs with
        | [] -> BConst { value = Z.zero; width = 1 }
        | [single] -> single
        | xs -> BConcat xs
      end
      else
        silent_zero ~width:1
          ~reason:(Printf.sprintf "unhandled TUPLE4 expression tag %s" tag)
  (* unary_prefix_expr handled above the generic TUPLE3 fallback. *)
  (* `cond_expr2`: TUPLE6(tag, cond, QUERY, then_expr, COLON, else_expr).
   * The ternary `?:` operator. *)
  | TUPLE6 (STRING tag, cond, _, t, _, e) when prefix_is "cond_expr" tag ->
      BCond { condition = recurse cond;
              then_val = recurse t;
              else_val = recurse e }
  (* `<type>'(<expr>)` cast — value-preserving for our integer
   * arithmetic. cast1: TUPLE6(tag, casting_type, QUOTE, LPAREN,
   * expression, RPAREN). Drop the cast and recurse on the inner
   * expression. lzc.sv uses `(NumLevels)'(unsigned'(j))` to
   * size-extend the genvar index — without unwrapping these the
   * expression falls to the BConst{0,1} default.
   *
   * When the casting_type evaluates to a positive integer (a size
   * cast like `LfsrWidth'(RstVal)` or `64'(x)`) and the inner
   * lowers to a literal constant, stamp the cast width onto the
   * constant.  Otherwise the unbased `'1` / `'0` shortcut would
   * keep its 32-bit default and produce the wrong reset value on
   * any LHS wider than 32 bits.  Z3's BV semantics interpret the
   * stored integer mod 2^width, so value-truncation falls out
   * naturally and no manual masking is needed. *)
  | TUPLE6 (STRING tag, ct, _quote, _lp, expr, _rp) when prefix_is "cast" tag ->
      let inner = recurse expr in
      (match eval_int ~pkgs ~params ct, inner with
       | Some w, BConst { value; _ } when w > 0 ->
           BConst { value; width = w }
       | Some w, _ when w > 0 ->
           (* Size cast `w'(expr)` on a NON-constant.  The cast is NOT a no-op in
              a width-sensitive context: `{td_i, 31'(idcode_q >> 1)}` must stay
              32 bits.  Dropping it left a 32-bit shift, making a 33-bit concat
              whose MSB (td_i) is silently truncated — this broke dmi_jtag_tap's
              IDCODE/BYPASS shift chain (JTAG chain-validation failed).  Model the
              cast as the low w bits so the width is preserved for every backend
              (Z3/gate_map already handle BSlice; the Verilog emitter renders the
              expression case as a real `w'(...)` size cast). *)
           BSlice { signal = inner; msb = w - 1; lsb = 0 }
       | _ -> inner)
  | TLIST [single] -> recurse single
  (* Streaming concatenation `{<<N{body}}` / `{>>N{body}}` —
   *   TUPLE8(tag, LBRACE, <<|>>, N_decimal, LBRACE, body, RBRACE, RBRACE).
   * Lowers to the body's plain BConcat: this preserves the bit count
   * (which downstream width inference needs) but loses the
   * reordering.  Acceptable interim semantics — the miter will flag
   * any real bit-order disagreement vs slang. *)
  | TUPLE8 (STRING tag, _lb1, _stream_op, _n_dec, _lb2, body, _rb1, _rb2)
    when prefix_is "streaming_concatenation" tag ->
      let exprs = match body with
        | TLIST xs ->
            List.filter_map (function
              | TLIST [] | EMPTY_TOKEN | COMMA -> None
              | e -> Some (recurse e)) xs
        | other -> [recurse other]
      in
      (match exprs with
       | [] -> BConst { value = Z.zero; width = 1 }
       | [single] -> single
       | xs -> BConcat xs)
  (* `x inside { a, b, c, [lo:hi], ... }` set-membership.
   * Verible parses as `comp_expr6`:
   *   TUPLE6(tag, lhs_expr, Inside, LBRACE, value_list, RBRACE)
   * where value_list is a TLIST whose non-separator children are
   * either expressions or `value_range` tuples `[lo:hi]`.
   * Lowering: OR-reduce equalities (`x == a`) for plain values;
   * range members become `(x >= lo) && (x <= hi)`.  An empty set
   * lowers to literal 0 (no value matches).  Used heavily in cva6
   * state-machine guards: `if (op inside {OP_ADD, OP_SUB})`. *)
  | TUPLE6 (STRING tag, lhs_tok, Inside, _, body, _)
    when prefix_is "comp_expr6" tag ->
      let lhs_bir = recurse lhs_tok in
      let cmp_type = BInt { width = 1; signed = Unsigned } in
      let eq r = BBinOp { op = BEq; lhs = lhs_bir; rhs = r;
                          result_type = cmp_type } in
      let arm e =
        match e with
        (* value_range: TUPLE5(STRING "value_range1", LBRACKET,
         *                     lo, COLON, hi, RBRACKET) — five or six
         * children depending on Verible build.  Match a tagged
         * tuple whose first child is the value_range tag and pick
         * out the two expression-shaped children. *)
        | TUPLE5 (STRING t, _, lo, _, hi)
        | TUPLE6 (STRING t, _, lo, _, hi, _)
          when prefix_is "value_range" t ->
            let lo_b = recurse lo and hi_b = recurse hi in
            let ge = BBinOp { op = BGe; lhs = lhs_bir; rhs = lo_b;
                              result_type = cmp_type } in
            let le = BBinOp { op = BLe; lhs = lhs_bir; rhs = hi_b;
                              result_type = cmp_type } in
            BBinOp { op = BAnd; lhs = ge; rhs = le;
                     result_type = cmp_type }
        | _ -> eq (recurse e)
      in
      let elems = match body with
        | TLIST xs ->
            List.filter_map (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN | COMMA -> None
              | _ -> Some (arm e)) xs
        | other -> [arm other]
      in
      (match elems with
       | [] -> BConst { value = Z.zero; width = 1 }
       | first :: rest ->
           List.fold_left (fun acc e ->
             BBinOp { op = BOr; lhs = acc; rhs = e;
                      result_type = cmp_type }) first rest)
  | other ->
      silent_zero ~width:1
        ~reason:(Printf.sprintf
                   "unhandled expression shape: %s"
                   (shape_of_tok other))

(* ─── Module port + signal extraction ────────────────────────────── *)

(* Pull the SymbolIdentifiers out of a `module_port_declaration*`
 * subtree so each name becomes one signal. *)
let extract_port_decl ~pkgs ~params tok =
  (* Interface port `if2.wr p` / `if2 p` — parsed by verible as
   * `type_identifier_followed_by_id` (or interface_port_header) nodes with the
   * identifiers in order [iface; modport?; portname].  Only the LAST id is the
   * port name; the iface and modport names must NOT leak as ports.  Emit ONE
   * signal (the port) with the interface's packed width, direction `Internal so
   * scalarize_module splits it into members; record (port, iface, modport) so
   * convert_module promotes the members to formals with modport directions. *)
  let iface_port =
    let hdr = collect_by (has_tag (fun t ->
              prefix_is "type_identifier_followed_by_id" t
              || prefix_is "interface_port_header" t)) tok in
    if hdr = [] then None
    else begin
      let ids = ref [] in
      walk (function SymbolIdentifier id -> ids := id :: !ids | _ -> ()) tok;
      let ids = List.rev !ids in
      (* iface = first id that is a registered interface *)
      match List.find_opt (fun id -> Hashtbl.mem iface_reg id) ids with
      | Some iface ->
          (match List.rev ids with
           | portname :: _ when portname <> iface ->
               (* modport = an id strictly between iface and portname *)
               let modport =
                 List.find_opt (fun id -> id <> iface && id <> portname) ids in
               Some (portname, iface, modport)
           | _ -> None)
      | None -> None
    end
  in
  match iface_port with
  | Some (portname, iface, modport) ->
      cur_iface_ports :=
        (portname, iface, Option.value ~default:"" modport) :: !cur_iface_ports;
      let w = try Hashtbl.find type_widths iface with Not_found -> 1 in
      [ { name = portname; stype = BInt { width = w; signed = Unsigned };
          direction = `Internal; initial_value = None; attrs = [] } ]
  | None ->
  let dir = ref `Internal in
  walk (function
    | Input -> dir := `Input
    | Output -> dir := `Output
    | Inout -> dir := `Input  (* bidirectional -> primary I/O linked var *)
    | _ -> ()
  ) tok;
  (* Collect port names — but skip identifiers that live inside a
   * packed/select dimension subtree (e.g. the `WIDTH` inside
   * `output [WIDTH-1:0] Q`). Without this filter `WIDTH` would be
   * extracted as a port of the same direction as `Q`. *)
  let names = ref [] in
  let rec walk_skip t =
    match t with
    | TUPLE6 (STRING t', _, _, _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    | TUPLE4 (STRING t', _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    (* `wire [W:0] foo = expr;` parses as net_declaration2 holding a
       net_decl_assign1 whose first SymbolIdentifier is the LHS name
       and whose third slot is the RHS expression.  Take the LHS,
       skip the RHS — without this every identifier appearing inside
       the initialiser would be wrongly promoted to a declaration with
       the enclosing wire's width. *)
    | TUPLE4 (STRING t', SymbolIdentifier id, _, _)
      when prefix_is "net_decl_assign" t' ->
        names := id :: !names
    | TUPLE3 (STRING t', SymbolIdentifier id, _)
      when prefix_is "unqualified_id" t' ->
        names := id :: !names
    | TUPLE2 (a, b) -> walk_skip a; walk_skip b
    | TUPLE3 (a, b, c) -> walk_skip a; walk_skip b; walk_skip c
    | TUPLE4 (a, b, c, d) -> walk_skip a; walk_skip b; walk_skip c; walk_skip d
    | TUPLE5 (a, b, c, d, e) ->
        walk_skip a; walk_skip b; walk_skip c; walk_skip d; walk_skip e
    | TUPLE6 (a, b, c, d, e, f) ->
        List.iter walk_skip [a; b; c; d; e; f]
    | TUPLE7 (a, b, c, d, e, f, g) ->
        List.iter walk_skip [a; b; c; d; e; f; g]
    | TUPLE8 (a, b, c, d, e, f, g, h) ->
        List.iter walk_skip [a; b; c; d; e; f; g; h]
    | TLIST xs -> List.iter walk_skip xs
    | _ -> ()
  in
  walk_skip tok;
  (* Always run a permissive sweep to pick up bare SymbolIdentifier
     port names that walk_skip missed.  K&R `input clk, rst;` parses
     such that only the first name is wrapped in `unqualified_id`;
     subsequent comma-separated names are bare SymbolIdentifier
     leaves.  Without the loose sweep, the second name is silently
     lost and ends up classified as Internal/Output by the
     downstream port-list merge.  Dedup later via sort_uniq. *)
  let rec walk_loose t =
    match t with
    | TUPLE6 (STRING t', _, _, _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    | TUPLE4 (STRING t', _, _, _)
      when prefix_is "decl_variable_dimension" t'
        || prefix_is "select_variable_dimension" t' -> ()
    (* As walk_skip: take the LHS of net_decl_assign, skip the RHS.
       Without this, identifiers inside the initialiser
       (`wire [W:0] foo = mem[i];` → `mem`, `i`) are wrongly added
       as declaration names with the wrapping wire's width. *)
    | TUPLE4 (STRING t', SymbolIdentifier id, _, _)
      when prefix_is "net_decl_assign" t' ->
        names := id :: !names
    | SymbolIdentifier id -> names := id :: !names
    | TUPLE2 (a, b) -> walk_loose a; walk_loose b
    | TUPLE3 (a, b, c) -> walk_loose a; walk_loose b; walk_loose c
    | TUPLE4 (a, b, c, d) -> walk_loose a; walk_loose b; walk_loose c; walk_loose d
    | TUPLE5 (a, b, c, d, e) ->
        List.iter walk_loose [a; b; c; d; e]
    | TUPLE6 (a, b, c, d, e, f) ->
        List.iter walk_loose [a; b; c; d; e; f]
    | TUPLE7 (a, b, c, d, e, f, g) ->
        List.iter walk_loose [a; b; c; d; e; f; g]
    | TUPLE8 (a, b, c, d, e, f, g, h) ->
        List.iter walk_loose [a; b; c; d; e; f; g; h]
    | TLIST xs -> List.iter walk_loose xs
    | _ -> ()
  in
  walk_loose tok;
  (* Struct/enum-typed port (`dm::dmi_req_t jtag_dmi_req_i` or bare
     `dmi_req_t p`): the type identifiers (the package qualifier `dm` and the
     type name `dmi_req_t`) are NOT port names, and the width is the struct
     width — not width_of's default 1.  Without this the whole struct port
     collapses to 1 bit AND the type names leak in as bogus 1-bit ports (the
     dmi_cdc `dm::dmi_req_t`/`dm::dmi_resp_t` CDC-datapath killer). *)
  let type_ids : (string, unit) Hashtbl.t = Hashtbl.create 4 in
  let struct_w = ref None in
  walk (function
    | SymbolIdentifier id when Hashtbl.mem type_widths id ->
        Hashtbl.replace type_ids id ();
        if !struct_w = None then struct_w := Hashtbl.find_opt type_widths id
    | _ -> ()) tok;
  (* exclude every identifier inside a `pkg::type` qualified_id (both the
     package and the type name) from the port-name set. *)
  let rec collect_qual t =
    match t with
    | TUPLE4 (STRING t', _, _, _) when prefix_is "qualified_id" t' ->
        walk (function SymbolIdentifier id -> Hashtbl.replace type_ids id () | _ -> ()) t
    | TUPLE2 (a, b) -> collect_qual a; collect_qual b
    | TUPLE3 (a, b, c) -> collect_qual a; collect_qual b; collect_qual c
    | TUPLE4 (a, b, c, d) -> List.iter collect_qual [a; b; c; d]
    | TUPLE5 (a, b, c, d, e) -> List.iter collect_qual [a; b; c; d; e]
    | TUPLE6 (a, b, c, d, e, f) -> List.iter collect_qual [a; b; c; d; e; f]
    | TUPLE7 (a, b, c, d, e, f, g) -> List.iter collect_qual [a; b; c; d; e; f; g]
    | TLIST xs -> List.iter collect_qual xs
    | _ -> () in
  collect_qual tok;
  (* a `pkg::type` qualifier the AST shape above didn't wrap in qualified_id
     still leaks the bare package name — exclude every known package name. *)
  List.iter (fun p -> Hashtbl.replace type_ids p.pkg_name ()) pkgs;
  names := List.filter (fun n -> not (Hashtbl.mem type_ids n)) !names;
  let w = match !struct_w with Some sw -> sw | None -> width_of ~pkgs ~params tok in
  (* Detect unpacked-array net decls (`wire [W:0] arr[D:0]`):
     two decl_variable_dimensions, first packed, second unpacked.
     Emit BArray so [merge_array_writes] uses (size, elem_w) from
     the type rather than its (writes_count, 1) fallback (which is
     right for cell-Verilog `assign out[i] = X` but wrong for
     source `assign in_[i] = in_$00i` where each element is many
     bits).  Single-dim case stays BInt. *)
  let dims = extract_packed_dims ~pkgs ~params tok in
  (* A lone `[N]` SIZE-form dimension (grammar decl_variable_dimension2 /
     TUPLE4) is an UNPACKED array of 1-bit elements — `logic gnt_o [N]`.
     Without recognising it, such a port collapses to a scalar BInt[N] and
     element writes/reads `gnt_o[k]` go out of range → driverless wires.
     A lone `[m:l]` RANGE form is an ordinary packed vector → BInt. *)
  let dim_nodes =
    collect_by (has_tag (prefix_is "decl_variable_dimension")) tok in
  let lone_size_form =
    match dim_nodes with
    | [TUPLE4 (STRING tag, _, _, _)]
      when prefix_is "decl_variable_dimension2" tag -> true
    | _ -> false in
  (* A SIZE-form dim (`decl_variable_dimension2` = `[N]`) is the after-name
     UNPACKED dimension -> unpacked array; a port with only range dims
     (`[A][B] x`) is a genuinely PACKED 2-D (dm_csrs `[DataCount-1:0][31:0]`).
     Exclude STRUCT-element arrays (scalarised to a packed concat). *)
  let has_size_form =
    List.exists (function
      | TUPLE4 (STRING tag, _, _, _) when prefix_is "decl_variable_dimension2" tag -> true
      | _ -> false) dim_nodes in
  let is_struct_arr = !struct_w <> None in
  let stype, is_unpacked =
    match dims with
    | [(im, il); (om, ol)] ->
        (BArray {
          element = BInt { width = abs (im - il) + 1; signed = Unsigned };
          size = abs (om - ol) + 1;
        }, has_size_form && not is_struct_arr)
    | [(om, ol)] when lone_size_form ->
        (BArray {
          element = BInt { width = 1; signed = Unsigned };
          size = abs (om - ol) + 1;
        }, not is_struct_arr)
    | _ ->
        (BInt { width = w; signed = Unsigned }, false)
  in
  List.rev !names |> List.sort_uniq compare |> List.map (fun n ->
    {
      name = n;
      stype;
      direction = !dir;
      initial_value = None;
      attrs = if is_unpacked then [("unpacked", "1")] else [];
    })

(* ─── Continuous assigns ─────────────────────────────────────────── *)

(* Walk inside a token, return the first SymbolIdentifier found
 * inside an `unqualified_id` tag — the canonical way the lhs of
 * `cont_assign1` / `assignment_statement_no_expr1` /
 * `nonblocking_assignment1` carries the target name. *)
let lhs_name_of tok =
  let n = ref None in
  walk (function
    | TUPLE3 (STRING t, SymbolIdentifier id, _)
      when prefix_is "unqualified_id" t && !n = None -> n := Some id
    | _ -> ()
  ) tok;
  !n

(* Same as lhs_name_of but ALSO returns the optional index token if
 * the lhs is `name[idx]` (e.g. `regs[~waddr[4:0]] <= wdata`). The
 * index lives inside a `select_variable_dimension2` (TUPLE4 with
 * `LBRACK expr RBRACK`) sibling of the unqualified_id. *)
let lhs_indexed_of tok =
  let n = ref None and idx = ref None in
  walk (function
    | TUPLE3 (STRING t, SymbolIdentifier id, _)
      when prefix_is "unqualified_id" t && !n = None -> n := Some id
    | TUPLE4 (STRING dt, _, e, _)
      when prefix_is "select_variable_dimension" dt && !idx = None ->
        (* Single index `[N]`: select_variable_dimension2 = TUPLE4. *)
        idx := Some e
    | TUPLE6 (STRING dt, _, msb, _, _, _)
      when prefix_is "select_variable_dimension" dt && !idx = None ->
        (* Range `[M:N]`: select_variable_dimension1 = TUPLE6.  Use
           the msb subtree as the "index" — we don't actually need
           the value here, just to mark this LHS as indexed for the
           array_names auto-promotion. *)
        idx := Some msb
    | _ -> ()
  ) tok;
  match !n with
  | Some name -> Some (name, !idx)
  | None -> None

(* Detect concat-LHS `{a, b, c, d}` and split into per-name (name,
   width) parts.  Returns None if lhs isn't a concat or any part
   can't be resolved to a plain name.  Per-part widths come from
   the signal-width cache. *)
let concat_lhs_parts lhs =
  match lhs with
  | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
      let parts =
        match body with
        | TLIST xs ->
            List.rev (List.filter (fun e -> match e with
              | TLIST [] | EMPTY_TOKEN -> false
              | _ -> true) xs)
        | other -> [other] in
      let infos = List.map (fun p ->
        match lhs_indexed_of p with
        | Some (n, _) ->
            let w =
              match List.assoc_opt n !cur_signal_widths with
              | Some w -> w | None -> 1 in
            Some (n, w)
        | None -> None
      ) parts in
      if List.for_all (fun x -> x <> None) infos then
        Some (List.map (function Some x -> x | None -> assert false) infos)
      else None
  | _ -> None

(* Bit range [msb:lsb] of struct field [fname] in struct-typed signal
   [sig_name] (cur_signal_struct -> cur_struct_defs; MSB-first). *)
let struct_field_range_g sig_name fname =
  match List.assoc_opt sig_name !cur_signal_struct with
  | None -> None
  | Some sn ->
    (match List.assoc_opt sn !cur_struct_defs with
     | None -> None
     | Some fields ->
       let tw = List.fold_left (fun a (_, w) -> a + w) 0 fields in
       let rec find pos = function
         | [] -> None
         | (n, w) :: rest ->
           if n = fname then Some (tw - pos - 1, tw - pos - w)
           else find (pos + w) rest
       in find 0 fields)

(* Struct member-select LHS `sig.field` -> (base, field, msb, lsb) of the
   field's bit-range within the packed struct.  None if not a member-select
   or the field/struct is unknown.  Used to keep a single-field continuous
   assign (`assign dmi_req.op = ...`) from being lowered as a WHOLE-struct
   assign (which drops the member and destroys the other fields). *)
let member_lhs_of lhs =
  match lhs with
  | TUPLE3 (STRING t, ref_node, TUPLE3 (STRING ht, _, ext_id))
    when prefix_is "reference2" t && prefix_is "hierarchy_extension1" ht ->
      let base = lhs_name_of ref_node in
      let fld = ref None in
      walk (function SymbolIdentifier id when !fld = None -> fld := Some id | _ -> ()) ext_id;
      (match base, !fld with
       | Some b, Some f ->
         (match struct_field_range_g b f with
          | Some (msb, lsb) -> Some (b, f, msb, lsb) | None -> None)
       | _ -> None)
  | _ -> None

let extract_assign ~pkgs ~params ~arrays tok =
  let assigns = collect_by (has_tag (prefix_is "cont_assign")) tok in
  List.concat_map (fun a ->
    match a with
    | TUPLE4 (STRING tag, lhs, _eq, rhs) when prefix_is "cont_assign" tag ->
        (match member_lhs_of lhs with
         | Some (base, fld, msb, lsb) ->
             (* `assign sig.field = rhs` — write ONLY that field's bit-range via
                @part_sel_write_up (scalarize then maps it to the per-field
                signal).  Without this the member is dropped and the whole struct
                is assigned `= rhs`, corrupting every field (e.g. dmi_jtag's
                `dmi_req.op` became `((state==Write)?2:1)[33:32]` = 0/NOP). *)
             let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
             [BCombinational {
                name = "assign_" ^ base ^ "_" ^ fld;
                sensitivity = [BAny];
                body = [BCallStmt {
                  func = "@part_sel_write_up";
                  args = [ BVar base;
                           BConst { value = Z.of_int lsb; width = 32 };
                           BConst { value = Z.of_int (msb - lsb + 1); width = 32 };
                           rhs_e ] }];
              }]
         | None ->
        (match concat_lhs_parts lhs with
         | Some parts ->
             (* Split: each name gets a slice of the RHS, MSB-first. *)
             let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
             let total_w = List.fold_left (fun acc (_, w) -> acc + w) 0 parts in
             let cursor = ref total_w in
             List.map (fun (name, w) ->
               let hi = !cursor - 1 and lo = !cursor - w in
               cursor := !cursor - w;
               let part_rhs = BSlice { signal = rhs_e; msb = hi; lsb = lo } in
               BCombinational {
                 name = "assign_concat_" ^ name;
                 sensitivity = [BAny];
                 body = [BAssign { lhs = name; rhs = part_rhs }];
               }
             ) parts
         | None ->
             (match lhs_indexed_of lhs with
              | Some (name, Some idx_node) when List.mem name arrays ->
                  let idx = expr_to_bexpr ~pkgs ~params ~arrays idx_node in
                  let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
                  [BCombinational {
                    name = "assign_" ^ name;
                    sensitivity = [BAny];
                    body = [BCallStmt {
                      func = "@mem_write";
                      args = [BVar name; idx; rhs_e];
                    }];
                  }]
              | Some (name, _) ->
                  let rhs_e = expr_to_bexpr ~pkgs ~params ~arrays rhs in
                  (* Literal bit/range select on the LHS must be PRESERVED:
                     `assign v[m:l] = x` used to lower to a whole-bus BAssign
                     (range silently DISCARDED) — partial writes to the same
                     bus clobbered each other and misaligned (pcs_pma
                     `status_vector[13:9]/[7:0] = ^status_vector[...]` lost
                     the sync bit feeding the GT rxresetfsm data_valid).
                     Emit per-bit `v[k]` bracket-keyed assigns instead. *)
                  let sel = ref `Whole in
                  walk (function
                    | TUPLE4 (STRING dt, _, e, _)
                      when prefix_is "select_variable_dimension" dt && !sel = `Whole ->
                        sel := `Single e
                    | TUPLE6 (STRING dt, _, m, _, l, _)
                      when prefix_is "select_variable_dimension" dt && !sel = `Whole ->
                        sel := `Rng (m, l)
                    | _ -> ()) lhs;
                  let lit e = match expr_to_bexpr ~pkgs ~params ~arrays e with
                    | BConst { value; _ } -> Some (Z.to_int value)
                    | _ -> None in
                  let mk_bit k rhs =
                    BCombinational {
                      name = Printf.sprintf "assign_%s_bit%d" name k;
                      sensitivity = [BAny];
                      body = [BAssign { lhs = Printf.sprintf "%s[%d]" name k;
                                        rhs }];
                    } in
                  let whole () =
                    [BCombinational {
                      name = "assign_" ^ name;
                      sensitivity = [BAny];
                      body = [BAssign { lhs = name; rhs = rhs_e }];
                    }] in
                  (match !sel with
                   | `Single e ->
                       (match lit e with
                        | Some k -> [mk_bit k rhs_e]
                        | None ->
                            Printf.eprintf
                              "verible_to_behavioral: WARNING dynamic bit-select \
                               continuous-assign LHS %s — select dropped\n" name;
                            whole ())
                   | `Rng (m, l) ->
                       (match lit m, lit l with
                        | Some mv, Some lv ->
                            let lo = min mv lv and hi = max mv lv in
                            List.init (hi - lo + 1) (fun b ->
                              mk_bit (lo + b)
                                (BSlice { signal = rhs_e; msb = b; lsb = b }))
                        | _ ->
                            Printf.eprintf
                              "verible_to_behavioral: WARNING dynamic range-select \
                               continuous-assign LHS %s — select dropped\n" name;
                            whole ())
                   | `Whole -> whole ())
              | None -> [])))
    | _ -> []
  ) assigns

(* ─── Procedural statement → BIR statement ───────────────────────── *)

(* Recognised statement shapes inside an `always_*` body. Anything
 * else becomes BBlock []. *)
let rec stmt_to_bstmt ~pkgs ~params ~arrays tok =
  let recurse_e = expr_to_bexpr ~pkgs ~params ~arrays in
  let recurse_s = stmt_to_bstmt ~pkgs ~params ~arrays in
  (* Bit range [msb:lsb] of struct field [fname] in struct-typed signal
     [sig_name] (cur_signal_struct -> cur_struct_defs; MSB-first). *)
  let struct_field_range sig_name fname =
    match List.assoc_opt sig_name !cur_signal_struct with
    | None -> None
    | Some sn ->
      (match List.assoc_opt sn !cur_struct_defs with
       | None -> None
       | Some fields ->
         let tw = List.fold_left (fun a (_, w) -> a + w) 0 fields in
         let rec find pos = function
           | [] -> None
           | (n, w) :: rest ->
             if n = fname then Some (tw - pos - 1, tw - pos - w)
             else find (pos + w) rest
         in find 0 fields) in
  let assign_to lhs rhs =
    (* Struct FIELD-write LHS `sig.field = v` -> @part_sel_write_up at the field
       bit-range (NOT a full assign that destroys the register).  The scalarize
       pass then maps it to a per-field signal write. *)
    let member_lhs =
      match lhs with
      | TUPLE3 (STRING t, ref_node, TUPLE3 (STRING ht, _, ext_id))
        when prefix_is "reference2" t && prefix_is "hierarchy_extension1" ht ->
          let base = lhs_name_of ref_node in
          let fld = ref None in
          walk (function SymbolIdentifier id when !fld = None -> fld := Some id | _ -> ()) ext_id;
          (match base, !fld with
           | Some b, Some f ->
             (match struct_field_range b f with
              | Some (msb, lsb) -> Some (b, msb, lsb) | None -> None)
           | _ -> None)
      | _ -> None in
    match member_lhs with
    | Some (base, msb, lsb) ->
        BCallStmt { func = "@part_sel_write_up";
                    args = [ BVar base;
                             BConst { value = Z.of_int lsb; width = 32 };
                             BConst { value = Z.of_int (msb - lsb + 1); width = 32 };
                             recurse_e rhs ] }
    | None ->
    (* Concat-LHS detection: `{a, b, c, d} = expr` splits into
       per-name assigns where each name takes a slice of the RHS,
       MSB-first.  Names in the concat may themselves be sliced
       (rare but handled).  Per-name width comes from the signal
       width cache. *)
    let concat_parts =
      match lhs with
      | TUPLE4 (STRING tag, _, body, _) when prefix_is "range_list_in_braces" tag ->
          let parts =
            match body with
            | TLIST xs ->
                List.rev (List.filter (fun e -> match e with
                  | TLIST [] | EMPTY_TOKEN -> false
                  | _ -> true) xs)
            | other -> [other] in
          (* For each part, capture name + the OUTERMOST
             select_variable_dimension kind (1=range [m:l],
             3=indexed-up [base+:w], 4=indexed-down [base-:w], None=plain).
             Walking the part token directly (rather than the whole
             concat) keeps each part's range local. *)
          let get_part p =
            let name_opt = match lhs_indexed_of p with
              | Some (n, _) -> Some n | None -> None in
            let kind = ref `None in
            let stop = ref false in
            let rec walk_outer t =
              if !stop then ()
              else match t with
                | TUPLE6 (STRING dt, _, mn, _, ln, _)
                  when prefix_is "select_variable_dimension" dt ->
                    let suffix = String.sub dt
                        (String.length "select_variable_dimension")
                        (String.length dt
                         - String.length "select_variable_dimension") in
                    (match suffix with
                     | "1" -> kind := `Range (mn, ln); stop := true
                     | "3" -> kind := `IndexedUp (mn, ln); stop := true
                     | "4" -> kind := `IndexedDown (mn, ln); stop := true
                     | _   -> stop := true)
                | TUPLE4 (STRING dt, _, idx_n, _)
                  when prefix_is "select_variable_dimension" dt ->
                    (* Single-bit `[idx]` — write one bit at idx.
                       Encode as `Range (idx, idx)` so the per-part
                       width computation gives 1 and the emit reaches
                       the @slice_write branch. *)
                    kind := `Range (idx_n, idx_n); stop := true
                | TUPLE2 (a, b) -> walk_outer a; walk_outer b
                | TUPLE3 (a, b, c) -> List.iter walk_outer [a; b; c]
                | TUPLE4 (a, b, c, d) -> List.iter walk_outer [a; b; c; d]
                | TUPLE5 (a, b, c, d, e) ->
                    List.iter walk_outer [a; b; c; d; e]
                | TUPLE6 (a, b, c, d, e, f) ->
                    List.iter walk_outer [a; b; c; d; e; f]
                | TUPLE7 (a, b, c, d, e, f, g) ->
                    List.iter walk_outer [a; b; c; d; e; f; g]
                | TUPLE8 (a, b, c, d, e, f, g, h) ->
                    List.iter walk_outer [a; b; c; d; e; f; g; h]
                | TLIST xs -> List.iter walk_outer xs
                | _ -> ()
            in
            walk_outer p;
            (name_opt, !kind, p) in
          let infos = List.map get_part parts in
          if List.for_all (fun (n, _, _) -> n <> None) infos then Some infos
          else None
      | _ -> None
    in
    (match concat_parts with
     | Some infos ->
         (* Per-part width depends on the range kind:
              [m:l]      -> |m-l|+1
              [base+:w]  -> w  (the second operand is the width)
              [base-:w]  -> w
              plain      -> signal-width-cache lookup
            Each part then receives a slice of the outer RHS at the
            running cursor; sliced parts go through @slice_write /
            @part_sel_write_* so downstream RMW-lowers them properly,
            instead of overwriting the whole base signal with the
            slice value (which is what produced the picorv32 pcpi_mul
            comb loop on `{carry, next_rd[j+:4]} = ...`). *)
         let rhs_e = recurse_e rhs in
         let part_width (n_opt, kind, _) =
           let nm = match n_opt with Some s -> s | None -> "?" in
           match kind with
           | `Range (m_n, l_n) ->
               (match recurse_e m_n, recurse_e l_n with
                | BConst { value = m; _ }, BConst { value = l; _ } ->
                    abs (Z.to_int m - Z.to_int l) + 1
                | _ -> 1)
           | `IndexedUp (_, w_n) | `IndexedDown (_, w_n) ->
               (match recurse_e w_n with
                | BConst { value = w; _ } when Z.geq w Z.one -> Z.to_int w
                | _ -> 1)
           | `None ->
               (match List.assoc_opt nm !cur_signal_widths with
                | Some w -> w | None -> 1)
         in
         let part_widths = List.map part_width infos in
         let total_w = List.fold_left (+) 0 part_widths in
         let cursor = ref total_w in
         let stmts = List.map2 (fun (n_opt, kind, _) w ->
           let nm = match n_opt with Some s -> s | None -> "?" in
           let hi = !cursor - 1 and lo = !cursor - w in
           cursor := !cursor - w;
           let part_rhs = BSlice { signal = rhs_e; msb = hi; lsb = lo } in
           match kind with
           | `Range (m_n, l_n) ->
               BCallStmt { func = "@slice_write";
                           args = [BVar nm; recurse_e m_n; recurse_e l_n;
                                   part_rhs] }
           | `IndexedUp (base_n, w_n) ->
               BCallStmt { func = "@part_sel_write_up";
                           args = [BVar nm; recurse_e base_n;
                                   recurse_e w_n; part_rhs] }
           | `IndexedDown (base_n, w_n) ->
               BCallStmt { func = "@part_sel_write_down";
                           args = [BVar nm; recurse_e base_n;
                                   recurse_e w_n; part_rhs] }
           | `None -> BAssign { lhs = nm; rhs = part_rhs }
         ) infos part_widths in
         BBlock stmts
     | None ->
         (* Single LHS — fall through to the existing range / array /
            plain dispatch. *)
         (* Distinguish three select-variable-dimension shapes on the
            outermost LHS bracket.  All three are TUPLE6 with a
            "select_variable_dimensionN" tag — N tells us which:
              N=1: `[m:l]` (range, both literal indices)
              N=3: `[base +: w]` (indexed-up part-select)
              N=4: `[base -: w]` (indexed-down part-select)
            TUPLE4 is the single-index `[i]` shape (mem-write).
            We walk the LHS deliberately and stop at the first match
            so a nested inner range like `arr[i[hi:lo]]` doesn't
            get mistaken for the outer dimension. *)
         let range_kind =
           let kind = ref `None in
           let stop = ref false in
           let rec walk_outer t =
             if !stop then ()
             else match t with
               | TUPLE6 (STRING dt, _, m, _, l, _)
                 when prefix_is "select_variable_dimension" dt ->
                   let tag = String.sub dt
                       (String.length "select_variable_dimension")
                       (String.length dt
                        - String.length "select_variable_dimension") in
                   (match tag with
                    | "1" -> kind := `Range (m, l); stop := true
                    | "3" -> kind := `IndexedUp (m, l); stop := true
                    | "4" -> kind := `IndexedDown (m, l); stop := true
                    | _   -> stop := true)  (* unknown shape → leave alone *)
               | TUPLE4 (STRING dt, _, _, _)
                 when prefix_is "select_variable_dimension" dt ->
                   stop := true
               | TUPLE2 (a, b) -> walk_outer a; walk_outer b
               | TUPLE3 (a, b, c) -> List.iter walk_outer [a; b; c]
               | TUPLE4 (a, b, c, d) ->
                   List.iter walk_outer [a; b; c; d]
               | TUPLE5 (a, b, c, d, e) ->
                   List.iter walk_outer [a; b; c; d; e]
               | TUPLE6 (a, b, c, d, e, f) ->
                   List.iter walk_outer [a; b; c; d; e; f]
               | TUPLE7 (a, b, c, d, e, f, g) ->
                   List.iter walk_outer [a; b; c; d; e; f; g]
               | TUPLE8 (a, b, c, d, e, f, g, h) ->
                   List.iter walk_outer [a; b; c; d; e; f; g; h]
               | TLIST xs -> List.iter walk_outer xs
               | _ -> ()
           in
           walk_outer lhs; !kind
         in
         (match lhs_indexed_of lhs, range_kind with
          | Some (name, _), `Range (m_node, l_node) ->
              BCallStmt {
                func = "@slice_write";
                args = [BVar name;
                        recurse_e m_node;
                        recurse_e l_node;
                        recurse_e rhs];
              }
          | Some (name, _), `IndexedUp (base, width) ->
              (* `name[base +: width] <= rhs` — indexed part-select up.
                 base may be dynamic; width is constant.  Lowered by
                 [lower_part_sel_writes_in_seq] to a full-bus BAssign
                 that picks rhs into the appropriate slot when
                 base == k * width, else self-reads. *)
              BCallStmt {
                func = "@part_sel_write_up";
                args = [BVar name;
                        recurse_e base;
                        recurse_e width;
                        recurse_e rhs];
              }
          | Some (name, _), `IndexedDown (base, width) ->
              (* `name[base -: width] <= rhs` — indexed part-select down.
                 Equivalent to `[base-width+1 +: width]`. *)
              BCallStmt {
                func = "@part_sel_write_down";
                args = [BVar name;
                        recurse_e base;
                        recurse_e width;
                        recurse_e rhs];
              }
          | Some (name, Some idx_node), `None when List.mem name arrays ->
              (* `arr[i] = v` — but ALSO `arr[i][j] = v` (array element,
                 then BIT): the second dimension used to be silently
                 DROPPED, so rgmii_lfsr's diagonal-mask init
                 `lfsr_mask_state[i][i] = 1'b1` wrote the whole element
                 with 1.  Collect the select dims in source order; with
                 two, lower to a read-modify-write of the element. *)
              let dims = ref [] in
              (* Collect ONLY the dimensions directly on `name`.  Do NOT recurse
                 into a dimension's INDEX EXPRESSION: a bit-slice inside the index
                 (`mem[waddr[4:0]]`) is otherwise mis-read as a SECOND dimension of
                 `mem`, so the element gets a partial `[4:0]` slice-write instead of
                 a full write (picorv32_regs then only wrote 5 bits per register).
                 Sibling dimensions (`arr[i][j]`) are reached via the parent, so
                 stopping at each dimension boundary still finds them. *)
              let rec collect t = match t with
                | TUPLE4 (STRING dt, _, e, _)
                  when prefix_is "select_variable_dimension" dt ->
                    dims := `Single e :: !dims
                | TUPLE6 (STRING dt, _, m, _, l, _)
                  when prefix_is "select_variable_dimension" dt ->
                    (* Distinguish the TUPLE6 shapes by tag number (as the
                       single-signal path does): 1 = `[m:l]` range,
                       3 = `[base +: w]` indexed-up, 4 = `[base -: w]`
                       indexed-down.  Treating an indexed part-select as a range
                       (the old behaviour) mis-read `[i*8 +: 8]`'s base/width as
                       range bounds and, being dynamic, dropped the inner select
                       entirely — dm_mem's data_bits byte-write vanished, so the
                       abstract-command result (misa) never reached the DMI. *)
                    let tag = String.sub dt
                        (String.length "select_variable_dimension")
                        (String.length dt
                         - String.length "select_variable_dimension") in
                    let d = match tag with
                      | "3" -> `IdxUp (m, l)
                      | "4" -> `IdxDown (m, l)
                      | _   -> `Rng (m, l) in
                    dims := d :: !dims
                | TUPLE2 (a, b) -> collect a; collect b
                | TUPLE3 (a, b, c) -> collect a; collect b; collect c
                | TUPLE4 (a, b, c, d) -> List.iter collect [a; b; c; d]
                | TUPLE5 (a, b, c, d, e) -> List.iter collect [a; b; c; d; e]
                | TUPLE6 (a, b, c, d, e, f) -> List.iter collect [a; b; c; d; e; f]
                | TUPLE7 (a, b, c, d, e, f, g) -> List.iter collect [a; b; c; d; e; f; g]
                | TUPLE8 (a, b, c, d, e, f, g, h) ->
                    List.iter collect [a; b; c; d; e; f; g; h]
                | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
                    List.iter collect [a; b; c; d; e; f; g; h; i]
                | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
                    List.iter collect [a; b; c; d; e; f; g; h; i; j]
                | TLIST xs -> List.iter collect xs
                | _ -> () in
              collect lhs;
              let dims = List.rev !dims in
              let plain () =
                BCallStmt {
                  func = "@mem_write";
                  args = [BVar name; recurse_e idx_node; recurse_e rhs];
                } in
              (match dims with
               | [_] | [] -> plain ()
               | [(`Single _ | `Rng _); second] ->
                   let ei = recurse_e idx_node in
                   let old = BSelect { array = BVar name; index = ei } in
                   let u64 = BInt { width = 64; signed = Unsigned } in
                   let bop op lhs rhs = BBinOp { op; lhs; rhs; result_type = u64 } in
                   let rhs_e = recurse_e rhs in
                   let ins ~lo ~w_expr =
                     (* new = (old & ~(mask<<lo)) | ((rhs&mask)<<lo) *)
                     let mask = match w_expr with
                       | None -> BConst { value = Z.one; width = 64 }
                       | Some w ->
                           bop BSub (bop BShl (BConst { value = Z.one; width = 64 }) w)
                                    (BConst { value = Z.one; width = 64 }) in
                     let cleared =
                       bop BAnd old
                         (BUnOp { op = BNot; operand = bop BShl mask lo;
                                  result_type = u64 }) in
                     let inserted = bop BShl (bop BAnd rhs_e mask) lo in
                     BCallStmt {
                       func = "@mem_write";
                       args = [BVar name; ei; bop BOr cleared inserted];
                     } in
                   (match second with
                    | `Single b -> ins ~lo:(recurse_e b) ~w_expr:None
                    | `IdxUp (base, w) ->
                        (* `elem[base +: w]` — base may be DYNAMIC (dm_mem's
                           `data_bits[dc][i*8 +: 8]`); the RMW mask/shift handle
                           it directly. *)
                        ins ~lo:(recurse_e base) ~w_expr:(Some (recurse_e w))
                    | `IdxDown (base, w) ->
                        (* `elem[base -: w]` == `[base-w+1 +: w]`. *)
                        let base_e = recurse_e base and w_e = recurse_e w in
                        let lo = bop BAdd (bop BSub base_e w_e)
                                   (BConst { value = Z.one; width = 64 }) in
                        ins ~lo ~w_expr:(Some w_e)
                    | `Rng (mn, ln) ->
                        let m_e = recurse_e mn and l_e = recurse_e ln in
                        (match m_e, l_e with
                         | BConst { value = mv; _ }, BConst { value = lv; _ } ->
                             let lo = Z.min mv lv and hi = Z.max mv lv in
                             ins ~lo:(BConst { value = lo; width = 64 })
                                 ~w_expr:(Some (BConst {
                                   value = Z.add (Z.sub hi lo) Z.one; width = 64 }))
                         | _ ->
                             Printf.eprintf
                               "verible_to_behavioral: WARNING dynamic range on \
                                2-dim array write %s — inner select dropped\n" name;
                             plain ()))
               | _ ->
                   Printf.eprintf
                     "verible_to_behavioral: WARNING >2 select dims on array \
                      write %s — inner selects dropped\n" name;
                   plain ())
          | Some (name, _), `None -> BAssign { lhs = name; rhs = recurse_e rhs }
          | None, _ -> BBlock []))
  in
  match tok with
  (* Blocking assignment: lhs = rhs *)
  | TUPLE4 (STRING tag, lhs, _eq, rhs)
    when prefix_is "assignment_statement_no_expr" tag -> assign_to lhs rhs
  (* Non-blocking: lhs <= rhs (Verible: nonblocking_assignment1
   *   TUPLE6(tag, lhs, _, _, rhs, _)) *)
  | TUPLE6 (STRING tag, lhs, _, _, rhs, _)
    when prefix_is "nonblocking_assignment" tag -> assign_to lhs rhs
  (* Compound (modify) assignment: lhs OP= rhs.  Verible emits one
     tag per operator (`assign_modify_statement1`..`11`):
       1:+=  2:-=  3:*=  4:/=  5:%=  6:&=  7:|=  8:^=
       9:<<=  10:>>=  11:>>>=
     Expand to `lhs = lhs OP rhs` here; assign_to lowers the
     synthesised RHS the same way as any other assignment.        *)
  | TUPLE4 (STRING tag, lhs, _op_tok, rhs)
    when prefix_is "assign_modify_statement" tag ->
      let suffix_n =
        let plen = String.length "assign_modify_statement" in
        try int_of_string (String.sub tag plen (String.length tag - plen))
        with _ -> 0 in
      let op = match suffix_n with
        |  1 -> BAdd  |  2 -> BSub  |  3 -> BMul  |  4 -> BDiv
        |  5 -> BMod  |  6 -> BAnd  |  7 -> BOr   |  8 -> BXor
        |  9 -> BShl  | 10 -> BShr  | 11 -> BAshr
        | _ -> BAdd in
      let lhs_b = recurse_e lhs in
      let rhs_b = recurse_e rhs in
      let expanded = BBinOp { op;
                              lhs = lhs_b;
                              rhs = rhs_b;
                              result_type = result_type_for op lhs_b rhs_b } in
      (* Reuse assign_to's LHS lowering by handing it an expression
         that already evaluates to the expanded RHS.  We have an
         alternate code path for the bare-name common case;
         indexed/sliced LHS falls back to a BAssign on the base.   *)
      let bare_name = match lhs_indexed_of lhs with
        | Some (n, None) -> Some n
        | _ -> None in
      (match bare_name with
       | Some n -> BAssign { lhs = n; rhs = expanded }
       | None ->
           (* Fall back: emit a normal assignment using assign_to
              after synthesising a TLIST token that wraps expanded.
              Simpler: just BAssign with the base name if we can
              extract it. *)
           (match lhs_indexed_of lhs with
            | Some (n, _) -> BAssign { lhs = n; rhs = expanded }
            | None -> BBlock []))
  (* Conditional statement. Match by direct tuple structure to pick
   * the immediate then/else slots (don't recurse — collect_by would
   * pull in statements from nested conditionals too). Arities seen:
   *   TUPLE7 — full `if (cond) then else else`:
   *     (tag, _, if_kw, expr_in_parens, then, else_kw, else)
   *   TUPLE5 — `if (cond) then` (no else):
   *     (tag, _, if_kw, expr_in_parens, then) *)
  | TUPLE7 (STRING tag, _, _, exp_par, then_node, _, else_node)
    when prefix_is "conditional_statement" tag ->
      let cond = match exp_par with
        | TUPLE4 (_, _, e, _) -> recurse_e e
        | other -> recurse_e other
      in
      BIf { condition = cond;
            then_stmts = [recurse_s then_node];
            else_stmts = [recurse_s else_node] }
  | TUPLE5 (STRING tag, _, _, exp_par, then_node)
    when prefix_is "conditional_statement" tag ->
      let cond = match exp_par with
        | TUPLE4 (_, _, e, _) -> recurse_e e
        | other -> recurse_e other
      in
      BIf { condition = cond;
            then_stmts = [recurse_s then_node];
            else_stmts = [] }
  (* `case (selector) ... endcase` — case_statement1 (TUPLE8):
   *   tag, _, _, LPAREN, selector, RPAREN, case_items1, ENDCASE *)
  | TUPLE8 (STRING tag, _, _, _, selector, _, items, _)
    when prefix_is "case_statement" tag ->
      let sel = recurse_e selector in
      (* TOP-LEVEL case items ONLY: collect_by recursed into arm BODIES,
         so a nested `case (init_step)` leaked its arms into the parent
         `case (state)` arm list — a leaked narrow arm (2'd1) then matched
         state==1 BEFORE the real S_IDLE arm and hijacked the FSM
         (arp_ctrl stuck re-writing MAC-hi forever on silicon). *)
      let case_items =
        let acc = ref [] in
        let rec go t =
          match t with
          | TUPLE4 (STRING t', _, _, _) when prefix_is "case_item" t' ->
              acc := t :: !acc      (* don't descend into the arm body *)
          | TUPLE2 (a, b) -> go a; go b
          | TUPLE3 (a, b, c) -> go a; go b; go c
          | TUPLE4 (a, b, c, d) -> List.iter go [a; b; c; d]
          | TUPLE5 (a, b, c, d, e) -> List.iter go [a; b; c; d; e]
          | TUPLE6 (a, b, c, d, e, f) -> List.iter go [a; b; c; d; e; f]
          | TUPLE7 (a, b, c, d, e, f, g) -> List.iter go [a; b; c; d; e; f; g]
          | TUPLE8 (a, b, c, d, e, f, g, h) ->
              List.iter go [a; b; c; d; e; f; g; h]
          | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
              List.iter go [a; b; c; d; e; f; g; h; i]
          | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
              List.iter go [a; b; c; d; e; f; g; h; i; j]
          | TLIST xs -> List.iter go xs
          | _ -> ()
        in
        go items; List.rev !acc in
      (* casez/casex WILDCARD labels (15'b100_0001_0???_0000): the plain
         path lowered `?` as 0, turning the pattern into an exact compare
         that (almost) never matches — framing_top's whole register-read
         mux returned the default 0 on silicon.  Detect wildcard digits
         and lower the ENTIRE case to a priority if-chain of masked
         compares ((sel & mask) == value), preserving arm order. *)
      let wildcard_of key =
        let found = ref None in
        walk (function
          | TUPLE3 (STRING t', base, digits)
            when prefix_is "bin_based_number" t' && !found = None ->
              let btxt = (match base with TK_BinBase s -> s | _ -> "") in
              let dtxt = ref "" in
              walk (function
                | TK_BinDigits n -> dtxt := !dtxt ^ n
                | _ -> ()) digits;
              let ds = String.concat "" (String.split_on_char '_' !dtxt) in
              if ds <> ""
                 && String.exists (fun c ->
                      c = '?' || c = 'z' || c = 'Z' || c = 'x' || c = 'X') ds
              then begin
                let w = match String.index_opt btxt '\'' with
                  | Some i ->
                      (try int_of_string (String.sub btxt 0 i)
                       with _ -> String.length ds)
                  | None -> String.length ds in
                let v = ref Z.zero and m = ref Z.zero in
                String.iter (fun c ->
                  v := Z.shift_left !v 1; m := Z.shift_left !m 1;
                  match c with
                  | '0' -> m := Z.logor !m Z.one
                  | '1' -> v := Z.logor !v Z.one; m := Z.logor !m Z.one
                  | _ -> ()) ds;
                found := Some (!v, !m, w)
              end
          | _ -> ()) key;
        !found in
      let arms =    (* (cond-info, body) in source order; None key = default *)
        List.filter_map (fun ci ->
          match ci with
          | TUPLE4 (STRING t, key, _colon, body) when prefix_is "case_item" t ->
              if prefix_is "case_item2" t then Some (None, [recurse_s body])
              else Some (Some (key, wildcard_of key), [recurse_s body])
          | _ -> None) case_items in
      let key_has_range key =
        let found = ref false in
        walk (function
          | TUPLE5 (STRING t, _, _, _, _)
          | TUPLE6 (STRING t, _, _, _, _, _) when prefix_is "value_range" t ->
              found := true
          | _ -> ()) key;
        !found in
      (* A case item with MULTIPLE comma-separated labels
         (`3'b000, 3'b010, 3'b011: stmt`) carries an `expression_list_proper`
         key.  recurse_e on it would collapse the whole list into a single
         BConcat, so the arm matched `sel == {L1,L2,L3}` and NEVER fired
         (ibex_decoder marked every addi/branch illegal).  Split the comma
         spine into the individual label ASTs so each becomes its own
         equality arm.  A braced `{a,b}` label is a genuine concat node (not
         expression_list_proper) and is intentionally left intact. *)
      let case_key_labels key =
        let rec flatten acc t = match t with
          | TUPLE4 (STRING tag', x, _comma, y)
            when prefix_is "expression_list_proper" tag' -> flatten (y :: acc) x
          | other -> other :: acc in
        let found = ref None in
        let rec look t = match t with
          | TUPLE4 (STRING tag', _, _, _)
            when prefix_is "expression_list_proper" tag' ->
              if !found = None then found := Some t
          | TUPLE2 (a, b) -> look a; look b
          | TUPLE3 (a, b, c) -> look a; look b; look c
          | TUPLE4 (a, b, c, d) -> List.iter look [a; b; c; d]
          | TUPLE5 (a, b, c, d, e) -> List.iter look [a; b; c; d; e]
          | TLIST xs -> List.iter look xs
          | _ -> () in
        look key;
        (match !found with Some l -> flatten [] l | None -> [key]) in
      let has_wild =
        List.exists (function Some (_, Some _), _ -> true | _ -> false)
          (List.map (fun (k, b) -> (k, b)) arms) in
      (* `case (sel) inside` with range labels `[lo:hi]`: BCase only does
         equality, so lower the whole case to a priority if-chain whose arm
         condition is a set-membership test (lo<=sel<=hi, OR-combined across
         multiple ranges).  dm_csrs' DMI read/write case uses exactly this
         (`[(dm::Data0):DataEnd]`); without it the entire register case dropped
         and the debug module read all-zero / never latched dmactive. *)
      let has_range =
        List.exists (fun (k, _) ->
          match k with Some (key, _) -> key_has_range key | None -> false) arms in
      if has_wild || has_range then begin
        let bool_t = BBool in
        let membership_cond key =
          let ranges = ref [] in
          walk (function
            | TUPLE5 (STRING t, _, lo, _, hi)
            | TUPLE6 (STRING t, _, lo, _, hi, _) when prefix_is "value_range" t ->
                ranges := (lo, hi) :: !ranges
            | _ -> ()) key;
          match List.rev !ranges with
          | [] ->
              (* no ranges: OR an equality per comma-separated label so a
                 multi-label arm in a wild/range case still matches. *)
              let eq lbl = BBinOp { op = BEq; lhs = sel; rhs = recurse_e lbl;
                                    result_type = bool_t } in
              (match case_key_labels key with
               | [single] -> eq single
               | l :: rest ->
                   List.fold_left (fun a lbl ->
                     BBinOp { op = BOr; lhs = a; rhs = eq lbl;
                              result_type = bool_t }) (eq l) rest
               | [] -> eq key)
          | rs ->
              let one (lo, hi) =
                BBinOp { op = BAnd;
                         lhs = BBinOp { op = BGe; lhs = sel; rhs = recurse_e lo;
                                        result_type = bool_t };
                         rhs = BBinOp { op = BLe; lhs = sel; rhs = recurse_e hi;
                                        result_type = bool_t };
                         result_type = bool_t } in
              List.fold_left (fun a r ->
                BBinOp { op = BOr; lhs = a; rhs = one r; result_type = bool_t })
                (one (List.hd rs)) (List.tl rs) in
        let cond_of key = function
          | Some (v, m, w) ->
              BBinOp { op = BEq;
                       lhs = BBinOp { op = BAnd; lhs = sel;
                                      rhs = BConst { value = m; width = w };
                                      result_type = BInt { width = w; signed = Unsigned } };
                       rhs = BConst { value = v; width = w };
                       result_type = bool_t }
          | None -> membership_cond key in
        let default_body =
          match List.find_opt (fun (k, _) -> k = None) arms with
          | Some (_, b) -> b | None -> [] in
        let chain =
          List.fold_right (fun (k, b) acc ->
            match k with
            | None -> acc                    (* default handled below *)
            | Some (key, wc) ->
                [BIf { condition = cond_of key wc;
                       then_stmts = b; else_stmts = acc }])
            arms default_body in
        (match chain with [s] -> s | ss -> BBlock ss)
      end else begin
        let cases, default =
          List.fold_left (fun (cs, def) (k, b) ->
            match k with
            | None -> (cs, b)
            | Some (key, _) ->
                (* one equality arm per comma-separated label, all sharing b *)
                let arms_for_key =
                  List.map (fun lbl -> (recurse_e lbl, b)) (case_key_labels key) in
                (cs @ arms_for_key, def))
            ([], []) arms in
        BCase { selector = sel; cases; default }
      end
  (* `case (sel) inside … endcase` — case_statement3 (TUPLE9 with the `Inside`
     keyword).  Item labels are `open_range_list`s of plain values and/or
     `[lo:hi]` ranges (case_inside_item1); case_inside_item2/3 are the default.
     BCase does equality only, so lower to a priority if-chain whose arm
     condition is a set-membership test.  dm_csrs' DMI register read+write case
     is exactly this shape; without it the whole block was unhandled → the read
     mux and dmcontrol write dropped → DM read all-zero and dmactive never set. *)
  | TUPLE9 (STRING tag, _, _, _, selector, _, _, items, _)
    when prefix_is "case_statement3" tag ->
      let sel = recurse_e selector in
      let cmp_t = BBool in
      let items_acc = ref [] in
      let rec go t = match t with
        | TUPLE3 (STRING t', a, b) when prefix_is "case_inside_items" t' ->
            go a; go b
        | TUPLE4 (STRING t', _, _, _)
          when prefix_is "case_inside_item1" t' || prefix_is "case_inside_item2" t' ->
            items_acc := t :: !items_acc
        | TUPLE3 (STRING t', _, _) when prefix_is "case_inside_item3" t' ->
            items_acc := t :: !items_acc
        | TLIST xs -> List.iter go xs
        | _ -> () in
      go items;
      let case_inside_items = List.rev !items_acc in
      (* one open-range-list element → a membership sub-condition *)
      let arm_cond e = match e with
        | TUPLE5 (STRING t, _, lo, _, hi)
        | TUPLE6 (STRING t, _, lo, _, hi, _) when prefix_is "value_range" t ->
            BBinOp { op = BAnd;
                     lhs = BBinOp { op = BGe; lhs = sel; rhs = recurse_e lo;
                                    result_type = cmp_t };
                     rhs = BBinOp { op = BLe; lhs = sel; rhs = recurse_e hi;
                                    result_type = cmp_t };
                     result_type = cmp_t }
        | _ -> BBinOp { op = BEq; lhs = sel; rhs = recurse_e e;
                        result_type = cmp_t } in
      let membership rangelist =
        let elems = match rangelist with
          | TLIST xs ->
              List.filter_map (fun e -> match e with
                | TLIST [] | EMPTY_TOKEN | COMMA -> None
                | _ -> Some (arm_cond e)) xs
          | other -> [arm_cond other] in
        match elems with
        | [] -> BConst { value = Z.zero; width = 1 }
        | first :: rest ->
            List.fold_left (fun a e ->
              BBinOp { op = BOr; lhs = a; rhs = e; result_type = cmp_t })
              first rest in
      let arms = List.filter_map (fun ci -> match ci with
        | TUPLE4 (STRING t, _, _, body) when prefix_is "case_inside_item2" t ->
            Some (None, recurse_s body)
        | TUPLE3 (STRING t, _, body) when prefix_is "case_inside_item3" t ->
            Some (None, recurse_s body)
        | TUPLE4 (STRING t, rangelist, _, body) when prefix_is "case_inside_item1" t ->
            Some (Some (membership rangelist), recurse_s body)
        | _ -> None) case_inside_items in
      let default_body =
        match List.find_opt (fun (c, _) -> c = None) arms with
        | Some (_, b) -> [b] | None -> [] in
      let chain =
        List.fold_right (fun (c, b) acc ->
          match c with
          | None -> acc
          | Some cond -> [BIf { condition = cond; then_stmts = [b];
                                else_stmts = acc }])
          arms default_body in
      (match chain with [s] -> s | ss -> BBlock ss)
  (* `seq_block1(begin1, TLIST [stmts], end)`. Verible's parser builds
   * the statement list with `TLIST ($2 :: lst)`, prepending each new
   * statement — so the TLIST is in *reverse* source order. We must
   * reverse it back before generating BIR, or unconditional defaults
   * end up after their conditional refinements and overwrite them. *)
  | TUPLE4 (STRING tag, _begin, body, _end)
    when prefix_is "seq_block" tag ->
      let stmts = match body with
        | TLIST xs ->
            List.filter_map (fun s ->
              let mapped = recurse_s s in
              match mapped with
              | BBlock [] -> None
              | other -> Some other
            ) (List.rev xs)
        | _ -> []
      in
      BBlock stmts
  (* statement_item wrappers — descend. *)
  | TUPLE3 (STRING tag, inner, _) when prefix_is "statement_item" tag ->
      recurse_s inner
  | TUPLE3 (STRING tag, _, inner) when prefix_is "statement_item" tag ->
      recurse_s inner
  (* `for (init; cond; step) body` — procedural for-loop.
   *   loop_statement1: TUPLE10(tag, For, LPAREN, init_opt, SEMICOLON,
   *                            cond_opt, SEMICOLON, step_opt, RPAREN, body)
   * Emit BFor; Behavioral_unroll handles the actual unrolling
   * downstream when init/cond/step are constants. Required for
   * lzc.sv's `flip_vector: for (int unsigned i = 0; i < WIDTH;
   * i++) in_tmp[i] = …` — without this the always_comb body
   * extracts as empty, in_tmp is undriven, and the trailing-zero
   * counter computes wrong values (cnt_o = 0 instead of 1 for
   * in_i = 2'b10). *)
  | TUPLE10 (STRING tag, _For, _, init_opt, _, cond_opt, _, step_opt, _, body)
    when prefix_is "loop_statement" tag ->
      let var_name = ref None in
      let init_expr = ref None in
      (match init_opt with
       | TUPLE5 (STRING t, _, SymbolIdentifier id, _, expr)
         when prefix_is "for_init_decl_or_assign" t ->
           var_name := Some id; init_expr := Some expr
       | TUPLE6 (STRING t, _, _, SymbolIdentifier id, _, expr)
         when prefix_is "for_init_decl_or_assign" t ->
           var_name := Some id; init_expr := Some expr
       | TUPLE4 (STRING t, lhs, _, expr)
         when prefix_is "for_init_decl_or_assign" t ->
           walk (function
             | SymbolIdentifier id when !var_name = None -> var_name := Some id
             | _ -> ()) lhs;
           init_expr := Some expr
       | _ -> ());
      (* Step shapes:
           inc_or_dec_expression*  → `IncDec ±1
           assignment_statement_no_expr1(lpvalue, =, expr)
             — handles `i = i + 1` style steps used in SV-2005-ish
             code (categorical_sampler's for-loop) so the unroller
             can recognise them as `BAssign { lhs; rhs = i ± k }`.
           assign_modify_statement → `Modify  (currently unsupported) *)
      let step_kind = match step_opt with
        | TUPLE3 (STRING t, _, _) when prefix_is "inc_or_dec_expression" t ->
            `IncDec step_opt
        | TUPLE4 (STRING t, _, _, rhs_e)
          when prefix_is "assignment_statement_no_expr" t ->
            `Assign rhs_e
        | _ -> `None
      in
      (match !var_name, !init_expr, step_kind with
       | Some name, Some init_e, `IncDec step_e ->
           let delta = match step_e with
             | TUPLE3 (_, PLUS_PLUS, _) -> 1
             | TUPLE3 (_, _, PLUS_PLUS) -> 1
             | TUPLE3 (_, HYPHEN_HYPHEN, _) -> -1
             | TUPLE3 (_, _, HYPHEN_HYPHEN) -> -1
             | _ -> 1
           in
           let init_b = BAssign { lhs = name; rhs = recurse_e init_e } in
           let cond_b = recurse_e cond_opt in
           let upd_b = BAssign {
             lhs = name;
             rhs = BBinOp {
               op = if delta < 0 then BSub else BAdd;
               lhs = BVar name;
               rhs = BConst { value = Z.of_int (abs delta); width = 32 };
               result_type = BInt { width = 32; signed = Unsigned };
             }
           } in
           BFor { init = init_b; condition = cond_b;
                  update = upd_b; body = [recurse_s body] }
       | Some name, Some init_e, `Assign rhs_e ->
           let init_b = BAssign { lhs = name; rhs = recurse_e init_e } in
           let cond_b = recurse_e cond_opt in
           let upd_b = BAssign { lhs = name; rhs = recurse_e rhs_e } in
           BFor { init = init_b; condition = cond_b;
                  update = upd_b; body = [recurse_s body] }
       | _ -> BBlock [])
  | TUPLE4 (STRING t, _return_kw, ret_expr, _semi)
    when prefix_is "jump_statement" t ->
      (* `return <expr>;` inside a function body — encode as BReturn so
         Behavioral_inline (which treats a trailing `[BReturn (Some e)]` as the
         call's value) can substitute it.  dm_pkg's instruction encoders
         (nop/auipc/jal/csrr/…) are single-`return` functions; without this arm
         their bodies extracted to 0 statements and inline left every
         `dm::nop()` etc. as an opaque, unfoldable call. *)
      (match ret_expr with
       | EMPTY_TOKEN -> BReturn None
       | e -> BReturn (Some (recurse_e e)))
  | TLIST [single] -> recurse_s single
  | _ -> BBlock []

(* ─── always blocks ──────────────────────────────────────────────── *)

(* `always_construct1(<keyword>, body)`. Sequential iff the sensitivity
 * list contains a posedge/negedge edge operator. The strictly-correct
 * rule (state-holding iff some lhs isn't assigned on every code path)
 * would also catch level-sensitive latches like
 * `always @(en) if (en) a <= b;` — but a simple "any if without else"
 * proxy fires too often (e.g. an unconditional default at the top of
 * the block followed by a case-branch with conditional refinement is
 * still combinational). Keeping the simple edge-based rule until we
 * have per-lhs path-coverage analysis. *)
let extract_always ~pkgs ~params ~arrays tok =
  let always_nodes = collect_by
    (has_tag (prefix_is "always_construct")) tok in
  List.map (fun an ->
    let has_edge = ref false in
    walk (function
      | Posedge | Negedge -> has_edge := true
      | _ -> ()) an;
    match !has_edge with
    | true ->
        (* Sequential.  Walk the event_expression collecting (edge, id)
         * pairs in source order so we can pair edges to their idents
         * properly.  For `@(posedge clk or posedge rst)` we get
         * [(`Pos, "clk"); (`Pos, "rst")].  Order in the source must
         * NOT matter — clock vs reset is decided from body shape. *)
        let edge_idents =
          let evs = collect_by (has_tag (prefix_is "event_expression")) an in
          let acc = ref [] in
          let pending = ref None in
          (* When the clock is an interface-port member-select `posedge p.clk`,
           * the flat walk yields [p; clk]; combine to the scalarized member name
           * `p$clk` (the field the following id names) so the clock matches the
           * scalarized port, not the whole interface. *)
          let expect_field = ref false in
          let collect = function
            | Posedge -> pending := Some `Pos
            | Negedge -> pending := Some `Neg
            | SymbolIdentifier id ->
                (match !pending with
                 | Some e ->
                     acc := (e, id) :: !acc;
                     pending := None;
                     expect_field :=
                       List.exists (fun (pn, _, _) -> pn = id) !cur_iface_ports
                 | None ->
                     if !expect_field then begin
                       (match !acc with
                        | (e, base) :: tl -> acc := (e, base ^ "$" ^ id) :: tl
                        | [] -> ());
                       expect_field := false
                     end)
            | _ -> ()
          in
          List.iter (walk collect) evs;
          List.rev !acc
        in
        let body_nodes = collect_by (has_tag (fun t ->
          prefix_is "seq_block" t ||
          prefix_is "nonblocking_assignment" t ||
          prefix_is "assignment_statement_no_expr" t ||
          prefix_is "conditional_statement" t ||
          prefix_is "case_statement" t ||
          prefix_is "loop_statement" t)) an in
        let body = match body_nodes with
          | b :: _ -> [stmt_to_bstmt ~pkgs ~params ~arrays b]
          | [] -> []
        in
        (* Find the outer if in the body — skipping BBlock wrappers —
         * and decide whether its condition is an async-reset test.
         * Per the project's no-name-heuristics rule, classification
         * comes purely from structure: a sensitivity ID is the reset
         * iff the outer-if condition references it (directly or
         * negated).  Returns:
         *   `Reset (rst_name, polarity)   — async reset detected
         *   `NoReset                      — body has no outer reset-if *)
        let classify_reset () =
          let sens_names = List.map snd edge_idents in
          let mem n = List.mem n sens_names in
          let rec outer = function
            | [] -> None
            | BBlock xs :: _ -> outer xs
            | BIf { condition; _ } :: _ -> Some condition
            | _ :: tl -> outer tl
          in
          (* Strip wrappers Verible emits around 1-bit conditions:
             logical-not lowers via `or_reduce` for 1-bit operands so
             `!rst_n` arrives as `BUnOp(BNot, BCall(or_reduce, [rst_n]))`.
             Reduction-or of a 1-bit value is the value itself. *)
          let rec simplify = function
            | BCall { func = "or_reduce"; args = [x] } -> simplify x
            | BCall { func = "and_reduce"; args = [x] } -> simplify x
            (* For 1-bit operands, reduction-or/and is the identity;
               Verible inserts these around `!x` to widen to a bool
               truth-test, so strip them here. *)
            | BUnOp { op = BRedOr; operand; _ } -> simplify operand
            | BUnOp { op = BRedAnd; operand; _ } -> simplify operand
            | BUnOp { op = BNot; operand; result_type } ->
                BUnOp { op = BNot; operand = simplify operand; result_type }
            | other -> other
          in
          match outer body with
          | None -> `NoReset
          | Some cond ->
              (match simplify cond with
               | BVar n when mem n -> `Reset (n, `Pos)
               | BUnOp { op = BNot; operand = BVar n; _ } when mem n ->
                   `Reset (n, `Neg)
               | BBinOp { op = BEq; lhs = BVar n;
                          rhs = BConst { value = zv; _ }; _ } when mem n && Z.equal zv Z.zero ->
                   `Reset (n, `Neg)
               | BBinOp { op = BEq; lhs = BVar n;
                          rhs = BConst { value = zv; _ }; _ } when mem n && Z.equal zv Z.one ->
                   `Reset (n, `Pos)
               | BBinOp { op = BNe; lhs = BVar n;
                          rhs = BConst { value = zv; _ }; _ } when mem n && Z.equal zv Z.zero ->
                   `Reset (n, `Pos)
               | BBinOp { op = BNe; lhs = BVar n;
                          rhs = BConst { value = zv; _ }; _ } when mem n && Z.equal zv Z.one ->
                   `Reset (n, `Neg)
               | _ -> `NoReset)
        in
        let pick_clock () =
          (* Default = first edge ident (used when no reset detected). *)
          match edge_idents with
          | (e, n) :: _ -> (n, e)
          | [] -> ("clk", `Pos)
        in
        let clk_name, clk_edge, reset, reset_edge, reset_async =
          let nedges = List.length edge_idents in
          match nedges, classify_reset () with
          | 0, _ ->
              (* No edges parsed (shouldn't happen given has_edge) —
               * preserve old behaviour. *)
              ("clk", `Pos, None, None, false)
          | 1, _ ->
              (* Single edge: that's the clock; sync reset (if any)
               * stays in the body — caller derives sync-reset
               * semantics from `if (rst) ...` shape, no FF-spec
               * change needed. *)
              let (e, n) = List.hd edge_idents in
              (n, e, None, None, false)
          | _, `Reset (rst, rst_pol) ->
              (* Multi-edge: pick the non-reset edge as clock.  Reset
               * polarity comes from the body test, not from the
               * sensitivity edge — `@(... or posedge rst) if (!rst)`
               * is a real (if rare) idiom; the body wins. *)
              let clock = List.find (fun (_, n) -> n <> rst) edge_idents in
              let (cke, cn) = clock in
              let r_edge = try Some (fst (List.find (fun (_, n) -> n = rst) edge_idents))
                           with Not_found -> Some rst_pol in
              let _ = r_edge in
              (cn, cke, Some rst, Some rst_pol, true)
          | _, `NoReset ->
              (* Multi-edge but body has no reset-if — fall back to
               * first edge as clock, no reset.  Conservative. *)
              let (n, e) = pick_clock () in
              (n, e, None, None, false)
        in
        (* Collect names of LHS assigned with blocking `=` in this clocked
         * block.  In SV, `current_pc = reg_pc + 4` inside an always_ff
         * makes current_pc an in-cycle wire — subsequent reads must see
         * this value, not the registered Q (1-cycle stale) that a naive
         * `<=` lowering would expose.  Walk the parse subtree so we
         * preserve this distinction that the BIR's BAssign type itself
         * doesn't carry, and hand the set to [behavioral_to_hardcaml]
         * via BSequential.blocking_vars. *)
        let blocking_vars =
          let names = ref [] in
          walk (function
            | TUPLE4 (STRING t, lhs, _, _)
              when prefix_is "assignment_statement_no_expr" t ->
                let id = ref None in
                walk (function
                  | SymbolIdentifier n when !id = None -> id := Some n
                  | _ -> ()) lhs;
                (match !id with Some n -> names := n :: !names | None -> ())
            | _ -> ()) an;
          List.sort_uniq compare !names
        in
        BSequential {
          name = "always_ff";
          clock = clk_name;
          clock_edge = clk_edge;
          reset;
          reset_edge;
          reset_async;
          body;
          blocking_vars;
        }
    | false ->
        (* Level-sensitive always block. Treated as combinational here
         * to match the Verilator-side classifier. The strictly-correct
         * rule (must-assign analysis to detect true latches) was tried
         * but caused 12 cva6 leaves to regress — many `always_comb`
         * blocks in cva6 leave some lhs conditionally driven and rely
         * on the value being initialised earlier. Revisit once we can
         * thread a "no aggressive latch detect on cva6" toggle through
         * both sides consistently. *)
        let body_nodes = collect_by (has_tag (fun t ->
          prefix_is "seq_block" t ||
          prefix_is "assignment_statement_no_expr" t ||
          prefix_is "conditional_statement" t ||
          prefix_is "case_statement" t)) an in
        let body = match body_nodes with
          | b :: _ -> [stmt_to_bstmt ~pkgs ~params ~arrays b]
          | [] -> []
        in
        BCombinational {
          name = "always_comb";
          sensitivity = [BAny];
          body;
        }
  ) always_nodes

(* ─── Module instances ───────────────────────────────────────────── *)

(* `non_anonymous_gate_instance_or_register_variable2`:
 *   <inst_name>, _, LPAREN, TLIST [port_named1 ...], RPAREN
 * We treat each `port_named1(_, _, port_name, LPAREN, expr, RPAREN)`
 * as a port connection. Returns Behavioral_ir.binstance, with the
 * containing module name picked up from the surrounding
 * instantiation_base1. *)
(* Flatten an `any_port_list` (the `(...)` of an instantiation) into
 * its `any_port` items in SOURCE order.  The grammar is left-recursive
 * — `any_port_list_item_last1(rest, last)` and
 * `any_port_list_trailing_comma1(list, COMMA)` — so the textually-first
 * port is the deepest `rest`; recurse rest-first then append `last`. *)
let rec flatten_any_port_list = function
  | TLIST xs -> List.concat_map flatten_any_port_list xs
  | TUPLE3 (STRING t, rest, last) when prefix_is "any_port_list_item_last" t ->
      flatten_any_port_list rest @ [last]
  | TUPLE3 (STRING t, lst, _comma) when prefix_is "any_port_list_trailing_comma" t ->
      flatten_any_port_list lst
  | COMMA | EMPTY_TOKEN -> []
  | any_port -> [any_port]

(* Direct children of a CST tuple, as a list (for locating the
 * any_port_list element after the instance's LPAREN). *)
let children_of = function
  | TUPLE2 (a,b) -> [a;b]
  | TUPLE3 (a,b,c) -> [a;b;c]
  | TUPLE4 (a,b,c,d) -> [a;b;c;d]
  | TUPLE5 (a,b,c,d,e) -> [a;b;c;d;e]
  | TUPLE6 (a,b,c,d,e,f) -> [a;b;c;d;e;f]
  | TUPLE7 (a,b,c,d,e,f,g) -> [a;b;c;d;e;f;g]
  | TLIST xs -> xs
  | _ -> []

let extract_instances ~pkgs ~params ?(arrays = []) tok =
  (* `arrays` = the enclosing module's array-typed signal names, so a
     port-connection expression `arr[a][b]` on a packed-2D array (e.g. gpio's
     `.btn_i(gp_i_q[2][i])`) lowers the outer index as an element-SELECT, not
     a bit-select.  Without it the inner `arr[a]` became a 1-bit BSlice and the
     nested `[b]` flattened to `arr[a+b]` (gp_i_q[2][i] -> gp_i_q[2+i]) — an
     out-of-range flat index Vivado rejects. *)
  (* Use the labeled walker so each instance picks up the
   * `<begin_label>.` prefix of every enclosing labeled generate
   * block. This matches the inst_name format that
   * Verible_elaborate.specialise_design records into
   * inst_specialised, and the format yosys-slang and synthesis
   * tools use as the SV-2017 hierarchical-name segment
   * (e.g. `non_leaf_node.left_child`). Without the prefix,
   * convert_files's lookup `(s.s_name, i.inst_name)` against
   * inst_specialised misses (it has the labeled key but i.inst_name
   * was unlabeled) and the instance gets dropped. *)
  let bases_with_labels = ref [] in
  Verible_elaborate.walk_live_labeled
    (List.filter_map (fun (k, v) ->
       try Some (k, int_of_string v) with _ -> None) params)
    [] (fun label_stack node ->
    if has_tag (prefix_is "instantiation_base") node then
      bases_with_labels := (label_stack, node) :: !bases_with_labels
  ) tok;
  let bases_with_labels = List.rev !bases_with_labels in
  List.concat_map (fun (label_stack, base) ->
    let mod_name = ref None in
    walk (function
      | TUPLE3 (STRING t, SymbolIdentifier id, _)
        when prefix_is "unqualified_id" t && !mod_name = None ->
          mod_name := Some id
      | _ -> ()
    ) base;
    (* Extract the instantiation's `#(.NAME(value), …)` parameter overrides so
       EXTERNAL primitive instances (MMCME2_ADV CLKIN1_PERIOD=16.0, RAMB36E1
       INITs, GTXE2 line-rate config) keep their configuration -- otherwise
       every primitive comes out with zero params and Vivado DRCs on e.g.
       CLKIN1_PERIOD=0.  Integers -> param_values, everything else (reals,
       strings, enums) -> param_strs. *)
    let base_overrides = extract_overrides base in
    let base_pvals, base_pstrs =
      List.fold_left (fun (iv, sv) (name, s) ->
        match int_of_string_opt s with
        | Some i -> ((name, i) :: iv, sv)
        | None   -> (iv, (name, s) :: sv)) ([], []) base_overrides in
    let inst_nodes = collect_by (has_tag (fun t ->
      prefix_is "non_anonymous_gate_instance" t ||
      prefix_is "module_instance" t)) base in
    (* Parameter overrides `#(.INIT(64'h..), .IS_C_INVERTED(1'b0), ...)` sit on
       the shared `unqualified_id` type node, sibling to the gate instances in
       this declaration, so capture them once per base and attach to every
       binstance it produces.  Kept as source-style strings ("64'h..", "4'h6",
       "0") in param_strs because a 64-bit LUT INIT overflows a native int —
       Xil_prim_models.normalize_init re-parses base+digits.                 *)
    let param_value_str valexpr =
      (* parameter_expr wraps the value in an `expression` subtree, so search
         recursively for the first sized literal / number rather than matching
         at top level. *)
      let found = ref None in
      let take s = if !found = None then found := Some s in
      walk (function
        | TUPLE3 (STRING tag, base_token, digits)
          when prefix_is "bin_based_number" tag || prefix_is "hex_based_number" tag
            || prefix_is "dec_based_number" tag || prefix_is "oct_based_number" tag ->
            let basestr = match base_token with
              | TK_BinBase s | TK_HexBase s | TK_DecBase s | TK_OctBase s -> s
              | _ -> "" in
            let ds = ref "" in
            walk (function
              | TK_BinDigits n | TK_HexDigits n | TK_OctDigits n | TK_DecNumber n ->
                  ds := !ds ^ n
              | _ -> ()) digits;
            take (basestr ^ !ds)
        | TK_UnBasedNumber n -> take n
        | TK_StringLiteral s ->
            (* STRING parameter override (.LFSR_CONFIG("GALOIS")): keep it,
               quote-normalized, so specialization records the value and
               param_value_to_bexpr packs it as an ASCII constant.  It used
               to fall through to eval_int -> None -> silently DROPPED,
               leaving `LFSR_CONFIG == "GALOIS"` comparing an unbound var. *)
            let s =
              let n = String.length s in
              if n >= 2 && s.[0] = '"' && s.[n-1] = '"'
              then String.sub s 1 (n - 2) else s in
            take ("\"" ^ s ^ "\"")
        | _ -> ()) valexpr;
      match !found with
      | Some s -> Some s
      | None ->
          (match eval_int ~pkgs ~params valexpr with
           | Some i -> Some (string_of_int i)
           | None -> None)
    in
    let base_param_strs =
      collect_by (has_tag (prefix_is "parameter_value_byname")) base
      |> List.filter_map (function
           | TUPLE6 (_, _, mname, _, valexpr, _) ->
               let pname = ref None in
               walk (function
                 | SymbolIdentifier id when !pname = None -> pname := Some id
                 | _ -> ()) mname;
               (match !pname, param_value_str valexpr with
                | Some n, Some v -> Some (n, v)
                | _ -> None)
           | _ -> None)
    in
    (* Same int/string classification as base_overrides above: a plain
       unbased decimal override (e.g. GTXE2 RX_SIG_VALID_DLY=10) is an
       INTEGER parameter and must be emitted as EDIF (integer 10).  Left in
       param_strs it reaches bir_to_edif.edif_property_value, whose all-0/1
       heuristic re-reads "10" as the binary literal 2'b10 (=2) and silently
       corrupts the transceiver config.  Based literals ("'h3202", "'b0")
       keep their apostrophe here so int_of_string_opt rejects them -> they
       stay source-style strings for the INIT path. *)
    let base_param_ints, base_param_strs =
      List.fold_left (fun (iv, sv) (name, s) ->
        match int_of_string_opt s with
        | Some i -> ((name, i) :: iv, sv)
        | None   -> (iv, (name, s) :: sv)) ([], []) base_param_strs in
    List.filter_map (fun in_ ->
      let inst_name = ref None in
      let port_conns = ref [] in
      let pn_tags = collect_by (has_tag (prefix_is "port_named")) in_ in
      List.iter (fun pn ->
        match pn with
        | TUPLE6 (_, _, SymbolIdentifier port, _, expr, _) ->
            (* Use the full expression converter so port connections
             * carrying slices, concats, replications, and constant
             * expressions survive intact through to BIR — required
             * for Behavioral_flatten to substitute correctly. *)
            let be =
              try expr_to_bexpr ~pkgs ~params ~arrays expr
              (* Re-raise Silent_zero so default-strict actually fires;
                 only fall back to a BVar reference for genuine other
                 expr-shape failures. *)
              with Silent_zero_substitution _ as e -> raise e
                 | _ -> BVar (match expr with
                              | TUPLE3 (_, SymbolIdentifier id, _) -> id
                              | SymbolIdentifier id -> id
                              | _ -> "?")
            in
            port_conns := (port, be) :: !port_conns
        | _ -> ()
      ) pn_tags;
      (* `.*` implicit port connection: verible's grammar emits a bare
       * DOT_STAR token in the port list (no `port_named` STRING tag), so the
       * pn_tags scan above misses it and — for a pure `.*` instance — the
       * positional fallback below would feed the DOT_STAR to expr_to_bexpr
       * (→ Silent_zero "unhandled expression shape: DOT_STAR").  Record a
       * `$dotstar` sentinel instead; name_positional_ports expands it, once
       * every module's formal port order is known, to (formal, formal) for
       * each formal the instance did NOT connect explicitly.  This is the
       * SV-2017 `.*` rule (lowrisc's prim_* wrappers rely on it). *)
      let has_dotstar =
        collect_by (fun t -> match t with DOT_STAR -> true | _ -> false) in_
        <> [] in
      if has_dotstar then
        port_conns := ("$dotstar", BConst { value = Z.zero; width = 1 })
                      :: !port_conns;
      (* Positional connections (`sub u1 (a, b, c)`): no port_named
       * children, so capture the actuals in source order under
       * synthetic `$pos<N>` keys.  convert_files_inner resolves these
       * to the child's formal port names once every module's port
       * order is known.  Without this, positionally-connected
       * instances (e.g. apb_uart's slib_input_filter UART_IF_CTS) lose
       * all connectivity: the child's output never reaches the parent
       * net and Behavioral_hier promotes it as inst__port. *)
      if !port_conns = [] then begin
        let portlist =
          let rec after_lparen = function
            | LPAREN :: pl :: _ -> Some pl
            | _ :: rest -> after_lparen rest
            | [] -> None
          in after_lparen (children_of in_)
        in
        match portlist with
        | None -> ()
        | Some pl ->
            List.iteri (fun idx ap ->
              match ap with
              | TUPLE6 (STRING t, _, _, _, _, _) when prefix_is "port_named" t ->
                  ()  (* named — handled above *)
              | EMPTY_TOKEN | COMMA -> ()
              | expr ->
                  let be =
                    try expr_to_bexpr ~pkgs ~params ~arrays expr
                    with Silent_zero_substitution _ as e -> raise e
                       | _ -> BVar (match expr with
                                    | TUPLE3 (_, SymbolIdentifier id, _) -> id
                                    | SymbolIdentifier id -> id
                                    | _ -> "?")
                  in
                  port_conns := (Printf.sprintf "$pos%d" idx, be) :: !port_conns
            ) (flatten_any_port_list pl)
      end;
      (match in_ with
       | TUPLE6 (_, SymbolIdentifier id, _, _, _, _)
       | TUPLE5 (_, SymbolIdentifier id, _, _, _) ->
           inst_name := Some id
       | _ ->
           walk (function
             | SymbolIdentifier id when !inst_name = None -> inst_name := Some id
             | _ -> ()) in_);
      match !mod_name, !inst_name with
      | Some m, Some i ->
          (* Verible's `instantiation_base` non-terminal is shared
             between real module instantiations and certain unpacked-
             array data declarations (e.g.
                  reg [31:0] mem [0:WORDS-1];
             gets surfaced as if WORDS were the module type and `mem`
             the instance name).  Real instantiations always have a
             paren-bound port list (possibly empty); mis-tagged array
             decls have no port_named children at all and their
             "module name" is a parameter in scope giving the bound.
             Drop the candidate when both signs apply.

             SAME shape for STRUCT/ENUM-typed BODY declarations:
               `dm::sba_state_e state_d, state_q;`  -> "instance state_d of dm"
               `state_e         state_d;`           -> "instance state_d of state_e"
             The mis-parsed "module name" is a PACKAGE (the `pkg::` qualifier) or
             a TYPE name (a typedef/enum in type_widths).  These have no port
             list either — dropping them stops the real register from becoming a
             phantom submodule instance (which silently deletes the reg: the
             pulp-debug dm_csrs/dm_sba/dm_mem CSRs, killing JTAG DM activation). *)
          let param_names = List.map fst params in
          let is_pkg_name = List.exists (fun p -> p.Verible_elaborate.pkg_name = m) pkgs in
          let is_type_name = Hashtbl.mem type_widths m in
          (* DEFINITIVE discriminator: a real module instantiation ALWAYS has a
             paren port list `()` (empty allowed) or `#()` params.  A data
             declaration (`rz_fsm_e src_fsm_d, src_fsm_q;`, `logic [PtrW-1:0]
             fifo_wptr;`) has NO parens — so no LPAREN => it is a declaration
             mis-tagged as an instance, drop it.  This catches the cases the
             param/pkg/type heuristics miss: BODY localparams (PtrW) and
             module-LOCAL enums (rz_fsm_e) that aren't in header params /
             type_widths. *)
          let has_lparen = ref false in
          walk (function LPAREN -> has_lparen := true | _ -> ()) in_;
          (* A data declaration WITH an initializer (`logic unused = a & (|b);`,
             ibex_top's gen_noscramble unused-signal tie-off) has a net_decl_assign
             and its RHS parens set has_lparen, defeating the paren discriminator
             — it was mis-emitted as `<first-rhs-id> <label> ()`, an instance of a
             nonexistent module.  A real instantiation never carries a
             net_decl_assign, so treat its presence as decisive: not an instance. *)
          let has_decl_assign =
            collect_by (has_tag (fun t ->
              prefix_is "net_decl_assign" t
              || prefix_is "variable_decl_assignment" t
              (* `logic unused = a & (|b);` parses as a
                 non_anonymous_gate_instance_or_register_variable with a
                 trailing_decl_assignment initializer — the RHS parens fool the
                 has_lparen instance discriminator. *)
              || prefix_is "trailing_decl_assignment" t)) in_ <> [] in
          if !port_conns = []
             && (List.mem m param_names || is_pkg_name || is_type_name
                 || not !has_lparen || has_decl_assign) then None
          else
            let prefixed_inst = String.concat "." (label_stack @ [i]) in
            Some {
              inst_name = prefixed_inst;
              module_name = m;
              (* merged: HEAD's classified overrides (int param_values +
                 source-string param_strs -- the silicon-validated fix-18
                 extraction) take priority; origin's string-only extraction is
                 appended (List.assoc takes the first match, dups benign). *)
              param_values = base_pvals @ base_param_ints;
              param_strs = base_pstrs @ base_param_strs;
              port_connections = List.rev !port_conns;
            }
      | _ -> None
    ) inst_nodes
  ) bases_with_labels

(* ─── Module conversion ──────────────────────────────────────────── *)

(* `parameter X = 4;` declared either in the module header
 *   (`module foo #(parameter X = 4) (...)`) or the body
 *   (`parameter X = 4;`). The instance-override map (`params`) wins;
 * defaults declared here fill in for unbound names. Both shapes have a
 * `trailing_assign1` slot whose subtree is the value expression. *)
let extract_body_params ~pkgs ~params tok =
  (* Order matters: SystemVerilog localparams in the body resolve
   * left-to-right against earlier ones, so PaddedWidth = 1 <<
   * $clog2(INPUT_WIDTH) needs INPUT_WIDTH (a port-param default) to
   * already be in scope. We accumulate `params` as we process each
   * node, so the next eval_int call sees prior defaults/localparams. *)
  let port_nodes =
    collect_by (has_tag (prefix_is "module_parameter_port")) tok in
  let local_nodes =
    collect_by (has_tag (prefix_is "any_param_declaration")) tok in
  (* Instance overrides (RESOLVED incoming params) are AUTHORITATIVE: a module's
     own port-param DEFAULT must never clobber them.  dm_mem is instantiated with
     `.DmBaseAddress(1)` but its declared default is `'0`; re-evaluating that
     default rebound DmBaseAddress 1->0, which in turn collapsed the derived
     localparams `HasSndScratch = (DmBaseAddress != 0)` and `LoadBaseAddr`.
     Lock every resolved incoming name against rebind (unresolved incoming
     params — e.g. `SelectableHarts = {NrHarts{1'b1}}` passed through as a bare
     name — stay improvable). *)
  let is_int_str v = (try let _ = Z.of_string (String.trim v) in true with _ -> false) in
  let locked_names = List.filter_map (fun (n, v) ->
    if is_int_str v then Some n else None) params in
  let one acc n =
    (* Find the parameter NAME — but skip identifiers that live inside
     * the parameter's type/range subtree. For
     * `parameter logic [LfsrWidth-1:0] RstVal = '1`, a naive first-id
     * walk would grab `LfsrWidth` (inside the range) instead of
     * `RstVal`. The actual parameter name lives just before
     * `trailing_assign` — usually in a `param_assignment` or sibling. *)
    let nm = ref None in
    let rec walk_skip_type t =
      match t with
      | TUPLE6 (STRING t', _, _, _, _, _)
        when prefix_is "decl_variable_dimension" t'
          || prefix_is "select_variable_dimension" t' -> ()
      | TUPLE4 (STRING t', _, _, _)
        when prefix_is "decl_variable_dimension" t'
          || prefix_is "select_variable_dimension" t' -> ()
      | TUPLE3 (STRING t', _, _) when prefix_is "instantiation_type" t'
                                    || prefix_is "data_type" t' -> ()
      (* a `pkg::type` qualifier (`dm::dm_csr_e DataEnd`) is the localparam's
         TYPE, not its name — skip it wholesale, else the first bare id `dm`
         (or the type `dm_csr_e`) is grabbed as the param name and the real
         name (`DataEnd`) never binds. *)
      | TUPLE4 (STRING t', _, _, _) when prefix_is "qualified_id" t' -> ()
      (* `trailing_assign1`: TUPLE4(tag, EQ, value, ...).  The `value`
       * subtree is the parameter's default expression; identifiers
       * inside it (struct field names like `BHTEntries`, function
       * names like `$clog2`, etc.) must NOT be picked up as the
       * parameter's name.  Without this skip, a struct-typed default
       * like `parameter T cfg = '{F1: v1, F2: v2}` would grab `F1`
       * as the param name instead of `cfg` (task #141). *)
      | TUPLE4 (STRING t', _, _, _)
        when prefix_is "trailing_assign" t' -> ()
      | TUPLE3 (STRING t', SymbolIdentifier id, _)
        when prefix_is "param_assignment" t' || prefix_is "param_decl" t' ->
          if !nm = None then nm := Some id
      | SymbolIdentifier id
        when !nm = None
          && not (Hashtbl.mem type_widths id)
          && not (List.exists (fun p -> p.pkg_name = id) pkgs) ->
          nm := Some id
      | TUPLE2 (a, b) -> walk_skip_type a; walk_skip_type b
      | TUPLE3 (a, b, c) -> walk_skip_type a; walk_skip_type b; walk_skip_type c
      | TUPLE4 (a, b, c, d) -> List.iter walk_skip_type [a; b; c; d]
      | TUPLE5 (a, b, c, d, e) -> List.iter walk_skip_type [a; b; c; d; e]
      | TUPLE6 (a, b, c, d, e, f) -> List.iter walk_skip_type [a; b; c; d; e; f]
      | TUPLE7 (a, b, c, d, e, f, g) ->
          List.iter walk_skip_type [a; b; c; d; e; f; g]
      | TUPLE8 (a, b, c, d, e, f, g, h) ->
          List.iter walk_skip_type [a; b; c; d; e; f; g; h]
      | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
          List.iter walk_skip_type [a; b; c; d; e; f; g; h; i]
      | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
          List.iter walk_skip_type [a; b; c; d; e; f; g; h; i; j]
      | TLIST xs -> List.iter walk_skip_type xs
      | _ -> ()
    in
    walk_skip_type n;
    let value_node = ref None in
    walk (function
      | TUPLE4 (STRING t, _, v, _) when prefix_is "trailing_assign" t
                                         && !value_node = None ->
          value_node := Some v
      | _ -> ()) n;
    match !nm, !value_node with
    | Some id, Some v ->
        (* Before the int/array paths, look for a struct-typed default
         * `'{field: int_value, ...}` (assignment_pattern2 CST shape).
         * Extract (field, int) pairs into cur_struct_params so
         * eval_int's reference2+hierarchy_extension1 arm can later
         * resolve `id.field` to the concrete int (task #141).  We try
         * eval_int on each value expression; field values that don't
         * fold to ints are silently skipped (the param.field lookup
         * will fall back to Some 0 for those). *)
        let extract_struct_pairs t =
          (* Look for an `assignment_pattern2` only as the immediate or
           * single-step-wrapped value of this param.  A full-tree walk
           * was over-eager — cva6 code has `'{...}` patterns deep
           * inside cast/concat expressions that are not the param's
           * top-level default, and picking those up polluted the
           * cur_struct_params table. *)
          let rec find_ap2 ~depth t =
            if depth > 4 then None else match t with
            | TUPLE4 (STRING tag, _, body, _)
              when prefix_is "assignment_pattern2" tag -> Some body
            (* unwrap a few common single-child wrappers (parens, casts,
             * trailing-assign, type-tag). *)
            | TUPLE2 (_, x) -> find_ap2 ~depth:(depth+1) x
            | TUPLE3 (_, x, _) -> find_ap2 ~depth:(depth+1) x
            | TUPLE4 (STRING tag, _, x, _)
              when prefix_is "trailing_assign" tag
                || prefix_is "expr_primary_parens" tag
                || prefix_is "cast" tag ->
                find_ap2 ~depth:(depth+1) x
            | _ -> None
          in
          match find_ap2 ~depth:0 t with
          | Some body ->
              let pairs = match body with
                | TLIST xs ->
                    List.filter_map (fun e -> match e with
                      | TUPLE4 (STRING t, key, _, value)
                        when prefix_is "structure_or_array_pattern_expression" t ->
                          let kname = ref None in
                          walk (function
                            | SymbolIdentifier id when !kname = None ->
                                kname := Some id
                            | _ -> ()) key;
                          (match !kname,
                                 eval_int ~pkgs ~params:acc value with
                           | Some k, Some n -> Some (k, n)
                           | _ -> None)
                      | _ -> None) xs
                | _ -> []
              in
              if pairs = [] then None else Some pairs
          | None -> None
        in
        (match extract_struct_pairs v with
         | Some pairs ->
             if Sys.getenv_opt "SV_DECOMP_STRUCT_DEBUG" <> None then
               Printf.eprintf "[struct] param %s := %s\n" id
                 (String.concat ", "
                    (List.map (fun (f, n) ->
                      Printf.sprintf "%s=%d" f n) pairs));
             Hashtbl.replace cur_struct_params id pairs
         | None -> ());
        let cur = List.assoc_opt id acc in
        let str_default =
          (* string-typed default (`parameter LFSR_CONFIG = "GALOIS"`):
             preserve as a QUOTED value so param_value_to_bexpr packs it
             ASCII at full width — routing it through Z.to_string lost
             the width (re-parsed at 32 bits, truncating the compare). *)
          let s = ref None in
          walk (function
            | TK_StringLiteral x when !s = None ->
                let n = String.length x in
                let x = if n >= 2 && x.[0] = '"' && x.[n-1] = '"'
                        then String.sub x 1 (n - 2) else x in
                s := Some x
            | _ -> ()) v;
          !s in
        let new_val =
          match str_default with
          | Some s -> Some ("\"" ^ s ^ "\"")
          | None ->
          match eval_int ~pkgs ~params:acc v with
          | Some i -> Some (string_of_int i)
          | None ->
              (try
                 match expr_to_bexpr ~pkgs ~params:acc ~arrays:[] v with
                 | BConst { value; _ } -> Some (Z.to_string value)
                 | BConcat elems ->
                     (* Array-typed localparam (`'{e1, …, eN}` initialiser).
                        Register the elements so LUT[i] lookups in the
                        body can fold to the i-th element or build a
                        BCond mux tree.  Returns None so this param
                        doesn't enter the integer scope (it isn't an
                        int).  Task #139's ROM-promotion path.       *)
                     Hashtbl.replace cur_array_params id elems;
                     None
                 | _ -> None
               with _ -> None)
        in
        (if Sys.getenv_opt "PARAM_DEBUG" <> None
            && (let l = String.length id in
                (l >= 7 && String.sub id 0 7 = "DataEnd")
                || (l >= 10 && String.sub id 0 10 = "ProgBufEnd")
                || (l >= 14 && String.sub id 0 14 = "SelectableHart")) then
           Printf.eprintf "[param] id=%s new_val=%s\n%!" id
             (match new_val with Some v -> v | None -> "<UNRESOLVED>"));
        (match cur, new_val with
         | None, Some v -> (id, v) :: acc
         | Some old, Some v when v <> old && not (List.mem id locked_names) ->
             (* Re-bind: a later fixed-point pass found a better value
                (e.g. ICW=0 from expr_to_bexpr fallback got overridden
                to ICW=7 once H landed in acc and eval_int's $clog2 path
                could fire).  Without this, the partial first-pass
                result stuck and `[ICW-1:0]` widths stayed at 1 bit.
                Locked (resolved incoming override) names are exempt — their
                value is authoritative and must not revert to a default. *)
             (id, v) :: List.remove_assoc id acc
         | _ -> acc)
    | _ -> acc
  in
  (* Multi-decl support: a single `parameter` keyword followed by a
     comma list (`parameter A = 1, B = 2, C = 3;`) becomes an
     `any_param_declaration1` whose first identifier+value sit in the
     `param_type_followed_by_id_and_dimensions_opt`/`trailing_assign`
     subtrees (captured above), but the remaining declarators live as
     `parameter_assign1` / `localparam_assign1` nodes inside the
     `parameter_assign_list`.  Without this walk, only the FIRST
     parameter in such a declaration was bound — exactly what made
     Controller_for_traffic_signal collapse: its four FSM-state names
     were declared in one comma-separated group and only
     `high_green_lane_red` was resolved.                              *)
  let one acc n =
    let acc = one acc n in
    let extras = ref [] in
    let rec walk_extras = function
      | TUPLE5 (STRING t, SymbolIdentifier id, _, v, _)
        when prefix_is "parameter_assign1" t ->
          extras := (id, v) :: !extras
      | TUPLE4 (STRING t, SymbolIdentifier id, _, v)
        when prefix_is "localparam_assign1" t ->
          extras := (id, v) :: !extras
      | TUPLE2 (a, b) -> walk_extras a; walk_extras b
      | TUPLE3 (a, b, c) -> walk_extras a; walk_extras b; walk_extras c
      | TUPLE4 (a, b, c, d) -> List.iter walk_extras [a; b; c; d]
      | TUPLE5 (a, b, c, d, e) -> List.iter walk_extras [a; b; c; d; e]
      | TUPLE6 (a, b, c, d, e, f) -> List.iter walk_extras [a; b; c; d; e; f]
      | TUPLE7 (a, b, c, d, e, f, g) ->
          List.iter walk_extras [a; b; c; d; e; f; g]
      | TUPLE8 (a, b, c, d, e, f, g, h) ->
          List.iter walk_extras [a; b; c; d; e; f; g; h]
      | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
          List.iter walk_extras [a; b; c; d; e; f; g; h; i]
      | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
          List.iter walk_extras [a; b; c; d; e; f; g; h; i; j]
      | TLIST xs -> List.iter walk_extras xs
      | _ -> () in
    walk_extras n;
    List.fold_left (fun acc (id, v) ->
      let cur = List.assoc_opt id acc in
      let new_val =
        match eval_int ~pkgs ~params:acc v with
        | Some i -> Some (string_of_int i)
        | None ->
            (try
               match expr_to_bexpr ~pkgs ~params:acc ~arrays:[] v with
               | BConst { value; _ } -> Some (Z.to_string value)
               | BConcat elems ->
                   Hashtbl.replace cur_array_params id elems;
                   None
               | _ -> None
             with _ -> None) in
      match cur, new_val with
      | None, Some v -> (id, v) :: acc
      | Some old, Some v when v <> old && not (List.mem id locked_names) ->
          (id, v) :: List.remove_assoc id acc
      | _ -> acc
    ) acc (List.rev !extras)
  in
  (* Verible's parse tree presents parameter declarations in
     reverse source order, so a derived param like
       parameter mwidth = dwidth + cwidth;
     gets visited before its operands resolve.  Iterate to fixed
     point: after each pass, retry the unresolved ones; stop when
     no new bindings appear (cycle or all done).  Bound at
     [List.length nodes] passes per group to guarantee
     termination on any acyclic dependency graph. *)
  let resolve_until_fixed nodes acc =
    (* Stop only when nothing changes — both length AND values must
       be stable.  Length-only stop misses cases where a binding
       improves from a fallback-0 to a real value (e.g. ICW=0 → ICW=7
       once $clog2's inner H resolves on a later pass). *)
    let rec loop acc passes =
      let acc' = List.fold_left one acc nodes in
      if (List.length acc' = List.length acc
          && List.for_all2 (fun (k1, v1) (k2, v2) -> k1 = k2 && v1 = v2)
               (List.sort compare acc) (List.sort compare acc'))
         || passes <= 0
      then acc'
      else loop acc' (passes - 1)
    in
    loop acc (List.length nodes) in
  let acc = resolve_until_fixed port_nodes  params in
  let acc = resolve_until_fixed local_nodes acc in
  (* Port-param defaults may FORWARD-reference a body localparam — e.g.
     verilog-axi's crossbar has `parameter M_ID_WIDTH = S_ID_WIDTH +
     $clog2(S_COUNT)` in the #(...) port list with `localparam S_COUNT = 3`
     in the module BODY (declared textually later).  The two passes above
     resolve the port params before any body localparam exists, so
     $clog2(S_COUNT) folds to $clog2(0)=0 and M_ID_WIDTH sticks at 8 instead
     of 10 (verilator and synlig both give 10).  Now that BOTH groups are
     seeded in `acc`, run one more fixpoint over ALL nodes: `one` re-binds a
     param whenever its value improves, so this re-evaluates the port params
     with the body localparams in scope (M_ID_WIDTH 8 -> 10) while leaving the
     already-correct body localparams unchanged.  Instance overrides are
     protected by the caller's merge, which drops any body-derived value for a
     name it already holds. *)
  let acc = resolve_until_fixed (port_nodes @ local_nodes) acc in
  (* Drop the original `params` we seeded with — caller already has those.
   * EXCEPTION: keep a seeded param whose value we IMPROVED (e.g. an
   * unresolved incoming `SelectableHarts="SelectableHarts"` that we folded to
   * its `{NrHarts{1'b1}}` default = 1) so the caller's merge can pick the
   * resolved value over its own unresolved one. *)
  List.filter (fun (k, v) ->
    match List.assoc_opt k params with
    | None -> true
    | Some seed -> seed <> v) acc

(* Auto-discovered list of directories searched by `$readmemh`'s
 * resolver in addition to MEM_INIT_DIR / cwd / cwd/generated.  We
 * walk up from each input .sv file looking for sibling `generated/`
 * directories — the convention used by smollm and other RTL trees
 * that vendor their hex initialiser files alongside the source.
 * Populated by [convert_files_inner] before per-module conversion. *)
let mem_init_search_paths : string list ref = ref []

(* ─── Struct scalarization ───────────────────────────────────────────
   Split each INTERNAL struct-typed signal S into one signal per field
   (S$field), rewrite field reads/writes to the field signals, whole-struct
   reads to a MSB-first concat of the fields, and whole-struct writes to
   per-field slice assigns.  This makes a conditional field-write
   (`q <= d; if (c) q.f <= 0`) two sequential assigns to ONE variable q$f ->
   Always.compile builds a mux, instead of the part-select approach's two
   full-width assigns that collided ("assign wire multiple times").  Also the
   correct fix for `struct.field = v` being mis-lowered as a full-reg destroy.
   [signal_struct]: signal -> struct-type name; [struct_defs]: type -> MSB-first
   [(field,width)].  Gated by STRUCT_SCALARIZE. *)
(* base signal name -> ordered field list [(field,msb,lsb,width)] (MSB-first)
   for every scalarized struct register, so the cross-flow miter can tie SVS's
   `base$field` FF-states to a reference flow's whole `base` register as
   base == {field_msb, …, field_lsb}.  Keyed by bare base name (unique enough
   within the DM hierarchy). *)
(* scalarize_layouts now lives in Behavioral_ir (shared with the miter). *)

let scalarize_module ~signal_struct ~struct_defs (m : bmodule) : bmodule =
  let is_port = List.exists (fun (s : bsignal) ->
    s.direction <> `Internal) in
  ignore is_port;
  (* split map: S -> [(field, msb, lsb, width)] (within S), for internal
     struct signals only. *)
  let split : (string, (string * int * int * int) list) Hashtbl.t =
    Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    if s.direction = `Internal then
      match List.assoc_opt s.name signal_struct with
      | Some sn ->
        (match List.assoc_opt sn struct_defs with
         | Some fields when fields <> [] ->
           let tw = List.fold_left (fun a (_, w) -> a + w) 0 fields in
           let _, layout =
             List.fold_left (fun (pos, acc) (f, w) ->
               (pos + w, (f, tw - pos - 1, tw - pos - w, w) :: acc))
               (0, []) fields in
           let layout = List.rev layout in
           Hashtbl.replace split s.name layout;
           Hashtbl.replace scalarize_layouts s.name layout
         | _ -> ())
      | None -> ()) m.signals;
  if Hashtbl.length split = 0 then m else begin
    let fvar s f = s ^ "$" ^ f in
    (* Original-signal width oracle, so a @slice_write whose data spans a WHOLE
       struct can be recognised as a full-struct copy (see the handler below). *)
    let sig_widths : (string, int) Hashtbl.t = Hashtbl.create 64 in
    List.iter (fun (s : bsignal) ->
      Hashtbl.replace sig_widths s.name (Behavioral_ir.width_of_type s.stype))
      m.signals;
    (* rewrite reads *)
    let rec re e =
      match e with
      | BVar s when Hashtbl.mem split s ->
          BConcat (List.map (fun (f, _, _, _) -> BVar (fvar s f)) (Hashtbl.find split s))
      | BSlice { signal = BVar s; msb; lsb } when Hashtbl.mem split s ->
          (* A part-select on the WHOLE scalarized struct must be RELOCATED
             onto the member signals: for every field overlapping [msb:lsb],
             take its LOCAL sub-slice (abs index - fl) and concat MSB-first.
             The old fallback `BSlice{ re(BVar s); msb; lsb }` sliced the
             reconstituted BConcat, which a later const-fold rewrote into
             `field_const[abs:abs]` — an out-of-range extract (e.g. dm_csrs
             `resp_queue_inp$data` / dmstatus: `9'0[31:23]`) that crashed Z3. *)
          let layout = Hashtbl.find split s in
          let parts = List.filter_map (fun (f, fm, fl, _fw) ->
            let imsb = min fm msb and ilsb = max fl lsb in
            if imsb < ilsb then None                 (* field outside [msb:lsb] *)
            else if imsb = fm && ilsb = fl then Some (BVar (fvar s f))
            else Some (BSlice { signal = BVar (fvar s f);
                                msb = imsb - fl; lsb = ilsb - fl })) layout in
          (match parts with
           | []  -> BSlice { signal = re (BVar s); msb; lsb }  (* no overlap: keep *)
           | [x] -> x
           | xs  -> BConcat xs)
      | BSlice { signal; msb; lsb } -> BSlice { signal = re signal; msb; lsb }
      | BConcat es -> BConcat (List.map re es)
      | BSelect { array; index } -> BSelect { array = re array; index = re index }
      | BBinOp r -> BBinOp { r with lhs = re r.lhs; rhs = re r.rhs }
      | BUnOp r -> BUnOp { r with operand = re r.operand }
      | BCond r -> BCond { condition = re r.condition;
                           then_val = re r.then_val; else_val = re r.else_val }
      | BReplicate r -> BReplicate { r with value = re r.value }
      | BCall r -> BCall { r with args = List.map re r.args }
      | _ -> e in
    (* one field's next value for a write of [msb:lsb] <- data (data is the
       write-range value, bit (i-lsb) = write bit i).  RMW: keep field bits
       outside [msb:lsb], take data bits inside. *)
    let field_upd s (f, fm, fl, fw) msb lsb data =
      let imsb = min fm msb and ilsb = max fl lsb in
      if imsb < ilsb then None  (* no overlap: field unchanged *)
      else if imsb = fm && ilsb = fl then
        (* fully covered: field := data[fm-lsb : fl-lsb] *)
        Some (BAssign { lhs = fvar s f;
                        rhs = BSlice { signal = data; msb = fm - lsb; lsb = fl - lsb } })
      else begin
        (* partial: {field[fm:imsb+1], data[imsb-lsb:ilsb-lsb], field[ilsb-1:fl]} *)
        let parts = ref [] in
        if fm > imsb then
          parts := BSlice { signal = BVar (fvar s f); msb = fw - 1; lsb = imsb - fl + 1 } :: !parts;
        parts := BSlice { signal = data; msb = imsb - lsb; lsb = ilsb - lsb } :: !parts;
        if ilsb > fl then
          parts := BSlice { signal = BVar (fvar s f); msb = ilsb - fl - 1; lsb = 0 } :: !parts;
        let rhs = match List.rev !parts with [x] -> x | xs -> BConcat xs in
        Some (BAssign { lhs = fvar s f; rhs })
      end in
    (* rewrite statements *)
    let rec rs stmt =
      match stmt with
      | BAssign { lhs; rhs } when Hashtbl.mem split lhs ->
          let rhs' = re rhs in
          BBlock (List.map (fun (f, fm, fl, _) ->
            BAssign { lhs = fvar lhs f;
                      rhs = BSlice { signal = rhs'; msb = fm; lsb = fl } })
            (Hashtbl.find split lhs))
      | BAssign { lhs; rhs } -> BAssign { lhs; rhs = re rhs }
      | BCallStmt { func = ("@part_sel_write_up" | "@slice_write") as fn;
                    args = (BVar s) :: rest } when Hashtbl.mem split s ->
          let layout = Hashtbl.find split s in
          let tw = List.fold_left (fun a (_, _, _, w) -> a + w) 0 layout in
          (* CONST bit-bounds are authoritative: the write targets exactly that
             bit-range, so lower it field-by-field.  This MUST be tried before the
             full-struct heuristic below — a narrow field-write with a wide
             constant RHS (`acs.progbufsize = 5'(ProgBufSize)` reaches here as
             @part_sel_write_up(acs,24,5, 32'8), a 32-bit literal) would otherwise
             be mistaken for a whole-struct copy and land in the wrong field. *)
          let rng =
            match fn, rest with
            | "@part_sel_write_up", [BConst { value = base; _ }; BConst { value = w; _ }; d] ->
                let base = Z.to_int base and w = Z.to_int w in Some (base + w - 1, base, d)
            | "@slice_write", [BConst { value = hi; _ }; BConst { value = lo; _ }; d] ->
                Some (Z.to_int hi, Z.to_int lo, d)
            | _ -> None in
          (match rng with
           | Some (msb, lsb, data) ->
               let data' = re data in
               BBlock (List.filter_map (fun fld -> field_upd s fld msb lsb data')
                         (Hashtbl.find split s))
           | None ->
               (* NON-const bounds only: a packed struct-ARRAY element slice
                  `aligned[i +: 1] = src` (ibex dm's `hartinfo_aligned[NrHarts-1:0]
                  = hartinfo_i`) reaches here with ELEMENT-indexed bounds, often
                  left un-folded (`NrHarts-1` = `1-1`).  If the DATA spans the whole
                  struct it's a full-struct copy — assign each field from the
                  source's bit-range.  (Bounds were const above, so this can't
                  swallow a narrow wide-constant field-write.) *)
               let data_opt = match List.rev rest with d :: _ -> Some d | [] -> None in
               let full_copy = match data_opt with
                 | Some d -> Behavioral_const.width_of_expr_with sig_widths d = Some tw
                 | None -> false in
               if full_copy then
                 let data' = re (Option.get data_opt) in
                 BBlock (List.map (fun (f, fm, fl, _) ->
                   BAssign { lhs = fvar s f;
                             rhs = BSlice { signal = data'; msb = fm; lsb = fl } }) layout)
               else BCallStmt { func = fn; args = List.map re ((BVar s) :: rest) })
      | BCallStmt r -> BCallStmt { r with args = List.map re r.args }
      | BIf r -> BIf { condition = re r.condition;
                       then_stmts = List.map rs r.then_stmts;
                       else_stmts = List.map rs r.else_stmts }
      | BCase r -> BCase { selector = re r.selector;
                           cases = List.map (fun (k, b) -> (re k, List.map rs b)) r.cases;
                           default = List.map rs r.default }
      | BBlock b -> BBlock (List.map rs b)
      | BWhile r -> BWhile { condition = re r.condition; body = List.map rs r.body }
      | BFor r -> BFor { init = rs r.init; condition = re r.condition;
                         update = rs r.update; body = List.map rs r.body }
      | BReturn (Some e) -> BReturn (Some (re e))
      | s -> s in
    let rs_body = List.map rs in
    let processes' = List.map (function
      | BCombinational c -> BCombinational { c with body = rs_body c.body }
      | BSequential s ->
        BSequential { s with body = rs_body s.body;
          blocking_vars = List.concat_map (fun v ->
            match Hashtbl.find_opt split v with
            | Some layout -> List.map (fun (f, _, _, _) -> fvar v f) layout
            | None -> [v]) s.blocking_vars })
      m.processes in
    let instances' = List.map (fun (i : binstance) ->
      { i with port_connections = List.map (fun (p, e) -> (p, re e)) i.port_connections })
      m.instances in
    let funcs' = List.map (fun (f : bfunc) -> { f with body = rs_body f.body }) m.funcs in
    (* replace struct signals with their field signals *)
    let signals' = List.concat_map (fun (s : bsignal) ->
      match Hashtbl.find_opt split s.name with
      | Some layout ->
          List.map (fun (f, _, _, w) ->
            { s with name = fvar s.name f;
                     stype = BInt { width = w; signed = Unsigned } }) layout
      | None -> [s]) m.signals in
    { m with signals = signals'; processes = processes';
             instances = instances'; funcs = funcs' }
  end

(* ── Constant-index array flatten (packed-array analog of scalarize) ──────
   A packed array written only at CONSTANT element indices inside a
   COMBINATIONAL block (e.g. dm_mem's abstract_cmd ROM `logic [7:0][63:0]`)
   is not a real RAM.  Left as an array it becomes a memory whose
   read-modify-write element writes read the array back, and gate_map turns
   that into a combinational loop.  Flatten into per-element scalars arr$i:
   element writes become blocking scalar assignments (threaded by
   blocking_subst just like struct fields), a dynamic read becomes an index
   mux, and a whole-array read becomes a concat.  Reads may be dynamic; only
   WRITES must be constant-indexed, and the array must never be written in a
   sequential process (that would be a register file / block RAM — leave it).
   Prepend per-element zero defaults so partial RMW writes with no `= '0`
   default (abstract_cmd has none) don't structurally self-reference. *)
let scalarize_arrays_module (m : bmodule) : bmodule =
  let cand : (string, int * int) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    match s.direction, s.stype with
    | `Internal, BArray { element = BInt { width; _ }; size }
      when size > 0 && size <= 256 && width > 0 ->
        Hashtbl.replace cand s.name (size, width)
    | _ -> ()) m.signals;
  if Hashtbl.length cand = 0 then m else begin
    let const_i = function BConst { value; _ } -> Some (Z.to_int value) | _ -> None in
    let disq n = Hashtbl.remove cand n in
    (* Only arrays with a SELF-REFERENTIAL comb write (element write whose RHS
       reads the same array — the read-modify-write of abstract_cmd's partial
       writes) actually form the gate_map loop.  A plain const-index array
       (e.g. eth's `rs`) is a harmless table; flattening it would needlessly
       perturb the netlist (and break eth-arp byte-identity).  So flatten ONLY
       self-referential candidates. *)
    let self_ref : (string, unit) Hashtbl.t = Hashtbl.create 8 in
    let rec reads_arr a e =
      match e with
      | BSelect { array = BVar x; _ } when x = a -> true
      | BVar x when x = a -> true
      | BSelect { array; index } -> reads_arr a array || reads_arr a index
      | BSlice { signal; _ } -> reads_arr a signal
      | BConcat es -> List.exists (reads_arr a) es
      | BBinOp { lhs; rhs; _ } -> reads_arr a lhs || reads_arr a rhs
      | BUnOp { operand; _ } -> reads_arr a operand
      | BCond { condition; then_val; else_val } ->
          reads_arr a condition || reads_arr a then_val || reads_arr a else_val
      | BReplicate { value; _ } -> reads_arr a value
      | BCall { args; _ } -> List.exists (reads_arr a) args
      | _ -> false in
    (* Disqualify: non-constant write index; any write in a sequential
       process; non-constant slice-write element range. *)
    let rec scan_w ~seq st =
      (match st with
       | BCallStmt { func = "@mem_write"; args = (BVar a) :: idx :: rest }
         when Hashtbl.mem cand a ->
           if seq || const_i idx = None then disq a
           else if List.exists (reads_arr a) rest then Hashtbl.replace self_ref a ()
       | BCallStmt { func = ("@slice_write" | "@part_sel_write_up");
                     args = (BVar a) :: hi :: lo :: rest } when Hashtbl.mem cand a ->
           if seq || const_i hi = None || const_i lo = None then disq a
           else if List.exists (reads_arr a) rest then Hashtbl.replace self_ref a ()
       | BAssign { lhs; rhs } when Hashtbl.mem cand lhs ->
           if seq then disq lhs
           else if reads_arr lhs rhs then Hashtbl.replace self_ref lhs ()
       | _ -> ());
      match st with
      | BIf r -> List.iter (scan_w ~seq) r.then_stmts; List.iter (scan_w ~seq) r.else_stmts
      | BCase r -> List.iter (fun (_, b) -> List.iter (scan_w ~seq) b) r.cases;
                   List.iter (scan_w ~seq) r.default
      | BBlock b -> List.iter (scan_w ~seq) b
      | BWhile r -> List.iter (scan_w ~seq) r.body
      | BFor r -> scan_w ~seq r.init; scan_w ~seq r.update; List.iter (scan_w ~seq) r.body
      | _ -> () in
    List.iter (function
      | BCombinational c -> List.iter (scan_w ~seq:false) c.body
      | BSequential s -> List.iter (scan_w ~seq:true) s.body) m.processes;
    List.iter (fun (f : bfunc) -> List.iter (scan_w ~seq:false) f.body) m.funcs;
    (* keep only const-index-comb arrays that are ALSO self-referential *)
    Hashtbl.iter (fun a _ -> if not (Hashtbl.mem self_ref a) then disq a)
      (Hashtbl.copy cand);
    if Hashtbl.length cand = 0 then m else begin
      let evar a i = a ^ "$" ^ string_of_int i in
      let idx_bits size =
        let rec b n = if n <= 1 then 0 else 1 + b ((n + 1) / 2) in max 1 (b size) in
      let rec re e =
        match e with
        | BVar a when Hashtbl.mem cand a ->
            let (size, _) = Hashtbl.find cand a in
            BConcat (List.init size (fun k -> BVar (evar a (size - 1 - k))))
        | BSelect { array = BVar a; index } when Hashtbl.mem cand a ->
            let (size, ew) = Hashtbl.find cand a in
            (match const_i index with
             | Some i when i >= 0 && i < size -> BVar (evar a i)
             | Some _ -> BConst { value = Z.zero; width = ew }
             | None ->
                 let nb = idx_bits size in
                 let sel = BSlice { signal = re index; msb = nb - 1; lsb = 0 } in
                 let rec build k =
                   if k >= size - 1 then BVar (evar a (size - 1))
                   else BCond { condition = BBinOp { op = BEq; lhs = sel;
                                    rhs = BConst { value = Z.of_int k; width = nb };
                                    result_type = BBool };
                                then_val = BVar (evar a k);
                                else_val = build (k + 1) } in
                 build 0)
        | BSelect { array; index } -> BSelect { array = re array; index = re index }
        | BSlice r -> BSlice { r with signal = re r.signal }
        | BConcat es -> BConcat (List.map re es)
        | BBinOp r -> BBinOp { r with lhs = re r.lhs; rhs = re r.rhs }
        | BUnOp r -> BUnOp { r with operand = re r.operand }
        | BCond r -> BCond { condition = re r.condition;
                             then_val = re r.then_val; else_val = re r.else_val }
        | BReplicate r -> BReplicate { r with value = re r.value }
        | BCall r -> BCall { r with args = List.map re r.args }
        | _ -> e in
      let rec rs st =
        match st with
        | BCallStmt { func = "@mem_write"; args = [BVar a; idx; data] }
          when Hashtbl.mem cand a ->
            (match const_i idx with
             | Some i -> BAssign { lhs = evar a i; rhs = re data }
             | None -> BCallStmt { func = "@mem_write"; args = [BVar a; re idx; re data] })
        | BCallStmt { func = ("@slice_write" | "@part_sel_write_up") as fn;
                      args = [BVar a; hi; lo; data] } when Hashtbl.mem cand a ->
            (match const_i hi, const_i lo with
             | Some h, Some l when l <= h ->
                 let (_, ew) = Hashtbl.find cand a in
                 let d = re data in
                 BBlock (List.init (h - l + 1) (fun k ->
                   BAssign { lhs = evar a (l + k);
                             rhs = BSlice { signal = d; msb = (k + 1) * ew - 1; lsb = k * ew } }))
             | _ -> BCallStmt { func = fn; args = [BVar a; re hi; re lo; re data] })
        | BAssign { lhs; rhs } when Hashtbl.mem cand lhs ->
            let (size, ew) = Hashtbl.find cand lhs in
            let d = re rhs in
            BBlock (List.init size (fun i ->
              BAssign { lhs = evar lhs i;
                        rhs = BSlice { signal = d; msb = (i + 1) * ew - 1; lsb = i * ew } }))
        | BAssign { lhs; rhs } -> BAssign { lhs; rhs = re rhs }
        | BCallStmt r -> BCallStmt { r with args = List.map re r.args }
        | BIf r -> BIf { condition = re r.condition;
                         then_stmts = List.map rs r.then_stmts;
                         else_stmts = List.map rs r.else_stmts }
        | BCase r -> BCase { selector = re r.selector;
                             cases = List.map (fun (k, b) -> (re k, List.map rs b)) r.cases;
                             default = List.map rs r.default }
        | BBlock b -> BBlock (List.map rs b)
        | BWhile r -> BWhile { condition = re r.condition; body = List.map rs r.body }
        | BFor r -> BFor { init = rs r.init; condition = re r.condition;
                           update = rs r.update; body = List.map rs r.body }
        | BReturn (Some e) -> BReturn (Some (re e))
        | s -> s in
      (* which candidate arrays are written in a body → per-element 0 defaults *)
      let written_in body =
        let s = Hashtbl.create 8 in
        let rec w st =
          (match st with
           | BCallStmt { func = ("@mem_write" | "@slice_write" | "@part_sel_write_up");
                         args = (BVar a) :: _ } when Hashtbl.mem cand a -> Hashtbl.replace s a ()
           | BAssign { lhs; _ } when Hashtbl.mem cand lhs -> Hashtbl.replace s lhs ()
           | _ -> ());
          match st with
          | BIf r -> List.iter w r.then_stmts; List.iter w r.else_stmts
          | BCase r -> List.iter (fun (_, b) -> List.iter w b) r.cases; List.iter w r.default
          | BBlock b -> List.iter w b
          | BWhile r -> List.iter w r.body
          | BFor r -> w r.init; w r.update; List.iter w r.body
          | _ -> () in
        List.iter w body; Hashtbl.fold (fun k () acc -> k :: acc) s [] in
      let defaults body =
        List.concat_map (fun a ->
          let (size, ew) = Hashtbl.find cand a in
          List.init size (fun i ->
            BAssign { lhs = evar a i; rhs = BConst { value = Z.zero; width = ew } }))
          (written_in body) in
      let processes' = List.map (function
        | BCombinational c ->
            BCombinational { c with body = defaults c.body @ List.map rs c.body }
        | BSequential s -> BSequential { s with body = List.map rs s.body }) m.processes in
      let instances' = List.map (fun (i : binstance) ->
        { i with port_connections = List.map (fun (p, e) -> (p, re e)) i.port_connections })
        m.instances in
      let funcs' = List.map (fun (f : bfunc) -> { f with body = List.map rs f.body }) m.funcs in
      let signals' = List.concat_map (fun (s : bsignal) ->
        match Hashtbl.find_opt cand s.name with
        | Some (size, ew) ->
            List.init size (fun i ->
              { s with name = evar s.name i;
                       stype = BInt { width = ew; signed = Unsigned } })
        | None -> [s]) m.signals in
      if Sys.getenv_opt "SVS_DEBUG_ARRAYFLAT" <> None then
        Hashtbl.iter (fun a (sz, w) ->
          Printf.eprintf "[array-flatten] %s -> %d x uint<%d>\n%!" a sz w) cand;
      { m with signals = signals'; processes = processes'; instances = instances'; funcs = funcs' }
    end
  end

(* Extract (field, int) pairs from a struct assignment-pattern initializer node
   — the standalone form of extract_body_params inner extract_struct_pairs, used
   to fold PACKAGE struct localparams (ibex_pkg exc_cause_t ExcCause) the same
   way as module struct localparams. *)
let struct_field_pairs ~pkgs ~params node : (string * int) list =
  (* Find the (first / outermost) `'{field: v, …}` assignment_pattern2 anywhere
     in the node — robust whether called on the raw value expression or the whole
     param-declaration node. *)
  let ap2_body =
    match collect_by (has_tag (prefix_is "assignment_pattern2")) node with
    | TUPLE4 (_, _, body, _) :: _ -> Some body
    | _ -> None in
  match ap2_body with
  | Some (TLIST xs) ->
      List.filter_map (fun e -> match e with
        | TUPLE4 (STRING t, key, _, value)
          when prefix_is "structure_or_array_pattern_expression" t ->
            let kname = ref None in
            walk (function SymbolIdentifier id when !kname = None ->
                          kname := Some id | _ -> ()) key;
            (match !kname, eval_int ~pkgs ~params value with
             | Some k, Some n -> Some (k, n) | _ -> None)
        | _ -> None) xs
  | _ -> []

let convert_module ~pkgs (mdecl : module_decl)
                          (params : (string * string) list) : bmodule =
  (* Reset the per-module array-typed localparam ROM table BEFORE
     extract_body_params populates it.  Otherwise the Hashtbl.clear
     down in the signal-extraction block wipes the registrations,
     and the reference3 walker sees an empty table when processing
     `LUT[sel]` in the always_comb body.                              *)
  Hashtbl.clear cur_array_params;
  Hashtbl.clear cur_struct_params;
  cur_iface_ports := [];
  (* Merge instance-override params with body-declared defaults; the
   * override wins on conflict (List.assoc_opt finds it first). *)
  let body_params = extract_body_params ~pkgs ~params mdecl.m_body in
  let params =
    (* An incoming specialisation param may be UNRESOLVED (e.g. dm_csrs'
       `SelectableHarts = {NrHarts{1'b1}}` that specialise_design couldn't fold,
       passed through as the bare name).  A resolved body default then must win,
       or the body reference stays unbound (havereset_q <= SelectableHarts & …
       degenerates).  Treat a value that is neither an integer nor a quoted
       string as unresolved. *)
    let is_resolved v =
      (String.length v > 0 && v.[0] = '"')
      || (try let _ = Z.of_string v in true with _ -> false) in
    (if Sys.getenv_opt "PARAM_DEBUG" <> None then
       List.iter (fun (n, v) ->
         if (let l = String.length n in l >= 14 && String.sub n 0 14 = "SelectableHart") then
           Printf.eprintf "[merge] incoming %s=%S resolved=%b body=%s\n%!" n v (is_resolved v)
             (match List.assoc_opt n body_params with Some b -> b | None -> "<none>")) params);
    let params = List.map (fun (n, v) ->
      if is_resolved v then (n, v)
      else match List.assoc_opt n body_params with
        | Some bv when is_resolved bv -> (n, bv)
        | _ -> (n, v)) params in
    let known = List.map fst params in
    params @ List.filter (fun (n, _) -> not (List.mem n known)) body_params
  in
  (* Build int scope from the merged params and prune dead generate
   * branches from the body. Without this, popcount__W2's body would
   * carry assigns from the W==1 / W>=3 branches alongside the W==2
   * branch, with multiple writes to the same signal corrupting
   * downstream Behavioral_flatten. *)
  let int_scope =
    (* Package-scoped constants (enum members + localparams).  This int_scope is
     * built BEFORE the module `params` get the enum augmentation below, so a
     * generate condition referencing a bare imported enum — ibex_alu's
     * `if (RV32B != RV32BNone)` with a wildcard import of ibex_pkg — would
     * otherwise not resolve and the whole RV32B bitmanip block would survive,
     * its conditional shuffle_mode[k]=… slice-writes forming a comb loop.
     * Build these FIRST and use them as the scope when evaluating each param's
     * value too: a specialised param can itself be an enum name (RV32B =
     * RV32BNone), which `eval_string []` can't resolve. *)
    let pkg_consts =
      List.concat_map (fun (p : Verible_elaborate.package_decl) ->
        List.filter_map (fun (n, v) ->
          Option.map (fun i -> (n, i)) (Verible_elaborate.int_of_pvalue v))
          p.pkg_params) pkgs in
    let base = List.filter_map (fun (n, v) ->
      Option.map (fun i -> (n, i))
        (Verible_elaborate.Eval.eval_string pkg_consts v)
    ) params in
    base @ List.filter (fun (n, _) -> not (List.mem_assoc n base)) pkg_consts
  in
  Verible_elaborate.resolver_for_walk :=
    (fun t -> Verible_elaborate.resolve_value pkgs t);
  Verible_elaborate.evaluator_for_walk :=
    Verible_elaborate.Eval.eval_string;
  (* Default-on. The fuzzer (test/random/fuzz_const_fn.sh) shows
   * the prune turns cfg_recursive from 0/25 to 25/25, and the
   * 3 ct_vfdsu_* Z3 PASSes the prune-off path showed were false
   * positives (multi-driver pollution where the un-pruned bmodule
   * happened to be Z3-equivalent to Vivado's elaborated form by
   * coincidence, not by elaboration correctness). Opt-out via
   * DISABLE_GEN_PRUNE for triaging the residual width-mismatch
   * cases. *)
  let mdecl =
    if Sys.getenv_opt "DISABLE_GEN_PRUNE" <> None then mdecl
    else { mdecl with m_body =
      Verible_elaborate.prune_dead_generates int_scope mdecl.m_body }
  in
  (* Capture the pre-strip body so the function extractor below can
     walk function_declaration subtrees.  Strip only affects
     downstream signal/process/instance extraction. *)
  let mdecl_body = mdecl.m_body in
  let mdecl =
    { mdecl with m_body =
      Verible_elaborate.strip_function_decls mdecl.m_body }
  in
  (* Enum item names fold to their integer values via the same
   * params lookup that handles `parameter`. *)
  let enum_items = extract_enum_items ~pkgs ~params mdecl.m_body in
  (* Package-scoped constants (enum members + localparams) that a module made
   * bare-visible with `import pkg::*` — e.g. ibex_alu's `import ibex_pkg::*`
   * then bare `ALU_GREV`/`ALU_CLMUL`.  These live in the package, not the
   * module body, so extract_enum_items misses them and the bare reference
   * folds to an unbound BVar tied to 0 (collapsing case selectors → bad muxes).
   * eval_int's lookup already searches every package for a bare name; mirror
   * that here so the bare→BConst path resolves too.  Module-local params and
   * body enums keep priority. *)
  let pkg_consts =
    List.concat_map (fun (p : Verible_elaborate.package_decl) ->
      List.filter_map (fun (n, v) ->
        match Verible_elaborate.int_of_pvalue v with
        | Some i -> Some (n, string_of_int i)
        | None -> None) p.pkg_params) pkgs in
  let params =
    let known = List.map fst params in
    let extra = List.filter (fun (n, _) -> not (List.mem n known))
                  (enum_items @ pkg_consts) in
    (* dedup extra by name, first wins *)
    let seen = Hashtbl.create 64 in
    let extra = List.filter (fun (n, _) ->
      if Hashtbl.mem seen n then false
      else (Hashtbl.replace seen n (); true)) extra in
    params @ extra
  in
  let typedefs = extract_typedefs ~pkgs ~params mdecl.m_body in
  (* Register type widths for $bits(<type>): module-local typedefs (full
     params) take priority; package typedefs fill in cross-package types.
     Must run BEFORE extract_instances so `.Width($bits(dcsr_t))` folds. *)
  List.iter (fun (n, w) -> if w > 0 then Hashtbl.replace type_widths n w) typedefs;
  List.iter (fun p ->
    List.iter (fun (n, w) ->
      if w > 0 && not (Hashtbl.mem type_widths n) then Hashtbl.replace type_widths n w)
      (extract_typedefs ~pkgs ~params:[] p.pkg_body)) pkgs;
  (* Register SIGNAL widths for `$bits(<signal>)` (e.g. dmi_jtag's
     `dr_q[$bits(dr_q)-1:1]` where `dr_q` is `logic [$bits(dmi_t)-1:0]`): without
     this $bits(dr_q) folded to 0 -> slice `dr_q[-1:1]` (invalid extract, and a
     real malformed shift register).  Runs AFTER typedefs so the signal's own
     width ($bits(dmi_t)) resolves first.  Types take priority (gated). *)
  Hashtbl.clear signal_widths;
  let stype_w = function
    | BInt { width; _ } -> width | BBool -> 1
    | BArray { element = BInt { width; _ }; size } -> width * size
    | _ -> 0 in
  let decl_nodes = collect_by (has_tag (fun t ->
    prefix_is "data_declaration" t || prefix_is "net_declaration" t
    (* PORTS too: `$bits(be_i)` on an input port (dm_mem's byte-write loop
       bound `i < $bits(be_i)`) folded to 0 because ports were never
       registered here — the loop unrolled to zero iterations and the whole
       data-register write vanished, so the abstract-command result never
       reached the DMI.  extract_port_decl handles these tags too. *)
    || prefix_is "module_port_declaration" t
    || prefix_is "port_declaration_noattr" t)) mdecl.m_body in
  List.iter (fun n ->
    List.iter (fun (s : bsignal) ->
      let w = stype_w s.stype in
      if w > 0 && not (Hashtbl.mem signal_widths s.name) then
        Hashtbl.replace signal_widths s.name w)
      (try extract_port_decl ~pkgs ~params n with _ -> [])) decl_nodes;
  if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
    List.iter (fun (n, w) ->
      Printf.eprintf "[%s] typedef %s width=%d\n" mdecl.m_name n w
    ) typedefs;
  (* Populate the module-scoped struct typedef table for the
   * expression converter. Reset between modules so a typedef in one
   * doesn't leak into another. *)
  cur_struct_defs :=
    extract_struct_defs ~pkgs ~params mdecl.m_body
    @ List.concat_map (fun p -> extract_struct_defs ~pkgs ~params:[] p.pkg_body) pkgs
    (* Interfaces-as-structs: make each registered interface visible as a struct
     * typedef so interface ports (if.modport p) register in cur_signal_struct and
     * member access (p.field) slices the packed interface. *)
    @ Hashtbl.fold (fun name members acc -> (name, members) :: acc) iface_reg [];
  if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then begin
    Printf.eprintf "[%s] struct_defs:\n" mdecl.m_name;
    List.iter (fun (n, fs) ->
      Printf.eprintf "  %s: %s\n" n
        (String.concat ", "
           (List.map (fun (f, w) -> Printf.sprintf "%s(%d)" f w) fs))
    ) !cur_struct_defs
  end;
  (* Build signal_name → struct_typedef_name for every reg/net
   * declared with a struct-typed instantiation_type. *)
  let signal_struct_decls =
    let acc = ref [] in
    let bases = collect_by
      (has_tag (prefix_is "instantiation_base")) mdecl.m_body in
    List.iter (fun base ->
      (* The type name is the first SymbolIdentifier under the
       * `instantiation_type`; collect the var names that follow. *)
      (* Pick the identifier that IS a known struct type — robust to a
         `pkg::type_t` qualifier (the FIRST unqualified_id would be the PACKAGE
         `dm`, not `dmcontrol_t`, so `dm::dmcontrol_t dmcontrol_d` never
         registered as struct-typed).  Variable names aren't struct types so
         only the type matches. *)
      let type_name = ref None in
      walk (function
        | SymbolIdentifier id
          when !type_name = None && List.mem_assoc id !cur_struct_defs ->
            type_name := Some id
        | _ -> ()) base;
      if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
        Printf.eprintf "  base type_name = %s\n"
          (Option.value !type_name ~default:"<none>");
      match !type_name with
      | Some tn when List.mem_assoc tn !cur_struct_defs ->
          (* Pick out variable names — register_variable nodes. *)
          let vars = collect_by (has_tag (fun t ->
            (let l = String.length "register_variable" in
             String.length t >= l && String.sub t 0 l = "register_variable")
            || prefix_is "non_anonymous_gate_instance" t
            || prefix_is "gate_instance_or_register_variable" t)) base in
          if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
            Printf.eprintf "    %d var nodes\n" (List.length vars);
          List.iter (fun v ->
            let nm = ref None in
            walk (function
              | TUPLE3 (STRING t, SymbolIdentifier id, _)
                when prefix_is "unqualified_id" t && !nm = None ->
                  nm := Some id
              | SymbolIdentifier id when !nm = None ->
                  nm := Some id
              | _ -> ()) v;
            if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
              Printf.eprintf "    var=%s\n"
                (Option.value !nm ~default:"<none>");
            match !nm with
            | Some n when n <> tn -> acc := (n, tn) :: !acc
            | _ -> ()
          ) vars
      | _ -> ()
    ) bases;
    !acc
  in
  cur_signal_struct := signal_struct_decls;
  (* Struct-typed LOCALPARAMS (`localparam dm::hartinfo_t DebugHartInfo = '{…}`)
     are param declarations, not instantiation_base nodes, so signal_struct_decls
     above misses them.  Register name->struct-type for each so pack_struct_const
     can fold a whole/sliced reference.  Within each param decl, the NAME is the
     id already recorded in cur_struct_params (its `'{…}` fields), and the TYPE is
     the id that is a known struct typedef. *)
  (let param_struct_decls =
     let acc = ref [] in
     List.iter (fun pn ->
       let name = ref None and ty = ref None in
       walk (function
         | SymbolIdentifier id ->
             if !name = None && Hashtbl.mem cur_struct_params id then name := Some id;
             if !ty = None && List.mem_assoc id !cur_struct_defs then ty := Some id
         | _ -> ()) pn;
       match !name, !ty with
       | Some n, Some t -> acc := (n, t) :: !acc
       | _ -> ())
       (collect_by (has_tag (prefix_is "any_param_declaration")) mdecl.m_body);
     !acc in
   cur_signal_struct := param_struct_decls @ !cur_signal_struct);
  (* PACKAGE struct localparams (ibex_pkg exc_cause_t ExcCauseIllegalInsn, a
     struct assignment-pattern with irq_ext/irq_int/lower_cause fields) are the
     same shape as module struct localparams (DebugHartInfo) but live in package
     bodies.  Register their field VALUES (cur_struct_params) + name->type
     (cur_signal_struct) here too, so pack_struct_const folds a whole/sliced use
     everywhere downstream, not just inside the package that declares them.
     Guarded by (not mem cur_struct_params n) so a module-local decl wins. *)
  List.iter (fun (p : Verible_elaborate.package_decl) ->
    List.iter (fun pn ->
      (* The struct TYPE is the id that is a known struct typedef. *)
      let ty = ref None in
      walk (function
        | SymbolIdentifier id
          when !ty = None && List.mem_assoc id !cur_struct_defs -> ty := Some id
        | _ -> ()) pn;
      (* The param NAME is the first id that is neither the type nor one of its
         fields — for a user-typed param (`exc_cause_t ExcCauseIllegalInsn`) the
         last id of param_type_followed_by_id is the TYPE, not the name, and the
         initializer contributes the field ids. *)
      let name = match !ty with
        | None -> None
        | Some t ->
            let fields = match List.assoc_opt t !cur_struct_defs with
              | Some l -> List.map fst l | None -> [] in
            let nm = ref None in
            walk (function
              | SymbolIdentifier id
                when !nm = None && id <> t && not (List.mem id fields) ->
                  nm := Some id
              | _ -> ()) pn;
            !nm in
      match name, !ty with
      | Some n, Some t when not (Hashtbl.mem cur_struct_params n) ->
          let pairs = struct_field_pairs ~pkgs ~params:[] pn in
          if pairs <> [] then begin
            Hashtbl.replace cur_struct_params n pairs;
            cur_signal_struct := (n, t) :: !cur_signal_struct
          end
      | _ -> ())
      (collect_by (has_tag (prefix_is "any_param_declaration")) p.pkg_body)
  ) pkgs;
  (* Record the interface instances among these struct-typed locals for the
   * interface-elaboration pass (type is a registered interface), together with
   * each instance's parameter overrides so its bundle members can be sized
   * per-instance (`axi_if #(.ID_WIDTH(4)) lsu_if()`). *)
  (let iface_bases = collect_by
     (has_tag (prefix_is "instantiation_base")) mdecl.m_body in
   let ifis = List.concat_map (fun base ->
     let tn = ref None in
     walk (function
       | SymbolIdentifier id when !tn = None && Hashtbl.mem iface_reg id -> tn := Some id
       | _ -> ()) base;
     match !tn with
     | None -> []
     | Some tn ->
         let ov = Verible_elaborate.extract_overrides base in
         let vars = collect_by (has_tag (fun t ->
           (let l = String.length "register_variable" in
            String.length t >= l && String.sub t 0 l = "register_variable")
           || prefix_is "non_anonymous_gate_instance" t
           || prefix_is "gate_instance_or_register_variable" t)) base in
         List.filter_map (fun v ->
           let nm = ref None in
           walk (function
             | TUPLE3 (STRING t, SymbolIdentifier id, _)
               when prefix_is "unqualified_id" t && !nm = None -> nm := Some id
             | SymbolIdentifier id when !nm = None -> nm := Some id
             | _ -> ()) v;
           match !nm with
           | Some n when n <> tn -> Some (n, tn, ov)
           | _ -> None) vars) iface_bases in
   if ifis <> [] then Hashtbl.replace module_iface_insts mdecl.m_name ifis);
  (* Reset per-module width cache; populated below as soon as the
   * port + internal signals are extracted, before any expr_to_bexpr
   * call. Keeps result_type_for from falling back to dummy_bool. *)
  cur_signal_widths := [];
  cur_signal_lsb := [];
  if false && Sys.getenv_opt "STRUCT_DEBUG" <> None then
    List.iter (fun (n, t) ->
      Printf.eprintf "  signal %s : %s\n" n t
    ) !cur_signal_struct;
  (* Three port-list flavours we need to handle together:
   *   - module_port_declaration5  → K&R: `input clk;`
   *   - port_declaration_noattr1  → ANSI explicit: `input logic [W-1:0] x`
   *   - port1 / port_reference1   → ANSI inherited: in `input a, b, c`
   *     the names `b` and `c` only appear as port_reference1 entries
   *     with the direction inherited from the preceding decl.
   *)
  let port_nodes = collect_by (has_tag (fun t ->
    prefix_is "module_port_declaration" t ||
    prefix_is "port_declaration_noattr" t)) mdecl.m_body in
  if Sys.getenv_opt "IFACE_DEBUG" <> None then begin
    let rec dmp d t =
      let pad = String.make (2*d) ' ' in
      (match t with
       | STRING s -> Printf.eprintf "%sSTRING %s\n%!" pad s
       | SymbolIdentifier s -> Printf.eprintf "%sID %s\n%!" pad s
       | TUPLE2 (a,b) -> dmp d a; dmp (d+1) b
       | TUPLE3 (a,b,c) -> dmp d a; dmp (d+1) b; dmp (d+1) c
       | TUPLE4 (a,b,c,e) -> dmp d a; List.iter (dmp (d+1)) [b;c;e]
       | TUPLE5 (a,b,c,e,f) -> dmp d a; List.iter (dmp (d+1)) [b;c;e;f]
       | TUPLE6 (a,b,c,e,f,g) -> dmp d a; List.iter (dmp (d+1)) [b;c;e;f;g]
       | TUPLE7 (a,b,c,e,f,g,h) -> dmp d a; List.iter (dmp (d+1)) [b;c;e;f;g;h]
       | TUPLE8 (a,b,c,e,f,g,h,i) -> dmp d a; List.iter (dmp (d+1)) [b;c;e;f;g;h;i]
       | TLIST xs -> Printf.eprintf "%sTLIST\n%!" pad; List.iter (dmp (d+1)) xs
       | EMPTY_TOKEN -> Printf.eprintf "%s<empty>\n%!" pad
       | _ -> Printf.eprintf "%s<other>\n%!" pad) in
    List.iteri (fun i pn ->
      Printf.eprintf "[iface] %s port_node #%d:\n%!" mdecl.m_name i; dmp 1 pn) port_nodes
  end;
  let explicit_signals =
    List.concat_map (extract_port_decl ~pkgs ~params) port_nodes
  in
  (* Register struct-typed PORTS in cur_signal_struct too, so a field read
     like `dmi_req_i.addr` on a `dm::dmi_req_t` port resolves to a BSlice of
     the packed port.  signal_struct_decls above only scans instantiation_base
     (INTERNAL decls); a struct INPUT port therefore had no resolvable field
     reads, hence no fanout, and gate_map dropped it entirely — dm_csrs lost
     `dmi_req_i`, so its read mux saw addr=0 and every DMI read returned 0
     (constant fields incl. dmstatus.version too), and dmcontrol writes lost
     their data (dmactive never latched).  Output struct ports survive via
     their assignment; inputs need this.  scalarize_module skips non-Internal
     signals, so the packed port is preserved (sliced per field, not split). *)
  let port_struct_decls =
    List.concat_map (fun pn ->
      let type_name = ref None in
      walk (function
        | SymbolIdentifier id
          when !type_name = None && List.mem_assoc id !cur_struct_defs ->
            type_name := Some id
        | _ -> ()) pn;
      match !type_name with
      | Some tn ->
          List.filter_map (fun (s : bsignal) ->
            if s.name <> tn then Some (s.name, tn) else None)
            (try extract_port_decl ~pkgs ~params pn with _ -> [])
      | None -> []) port_nodes
  in
  cur_signal_struct := port_struct_decls @ !cur_signal_struct;
  if Sys.getenv_opt "PORT_DEBUG" <> None then begin
    Printf.eprintf "[port] %s: port_nodes=%d explicit=%d struct_ports=[%s]\n%!"
      mdecl.m_name (List.length port_nodes) (List.length explicit_signals)
      (String.concat "," (List.map (fun (n, t) -> n ^ ":" ^ t) port_struct_decls));
    List.iter (fun (s : bsignal) ->
      if (let l = String.length s.name in l >= 7 && String.sub s.name 0 7 = "dmi_req") then
        Printf.eprintf "[port]   explicit sig %s w=%s\n%!" s.name
          (match s.stype with BInt { width; _ } -> string_of_int width | BBool -> "1" | _ -> "?"))
      explicit_signals
  end;
  let explicit_names = List.map (fun (s : bsignal) -> s.name)
                         explicit_signals in
  (* Inherited ports (`output [3:0] sum1, sum2;` — sum2 is a bare
   * port_reference1 sibling that inherits BOTH the direction AND the
   * width from sum1's preceding port_declaration_noattr1).
   *
   * Strategy: walk the module_port_list_opt's children in source
   * order, maintaining a "current direction + width" state. When we
   * encounter an explicit port decl, update the state from it. When
   * we encounter a port_reference1 not covered by an explicit decl,
   * emit a signal using the current state. *)
  let port_list_node = collect_by
    (has_tag (prefix_is "module_port_list_opt")) mdecl.m_body in
  let inherited_signals =
    (* Verible visits port-related nodes in REVERSE of source order
     * (the explicit decl comes after its bare-reference siblings in
     * DFS). So: collect every port-related event in DFS order, then
     * iterate the list in REVERSE to apply direction+width
     * inheritance the way the source intended. *)
    let cur_dir = ref `Input in
    let cur_width = ref 1 in
    let acc = ref [] in
    let events = ref [] in (* (dir_opt, width_opt, name_opt) per port1 *)
    (* Iterate each `port1` in the module_port_list_opt in order. Each
     * `port1` is one comma-separated port — it either has its own
     * direction+width (the first port in `output [3:0] a, b`) OR is a
     * bare name that inherits from the previous port1 (b, in the same
     * example). *)
    (* Per-event collector — extract direction/width/name from a
     * port-related node. The name walker must skip identifier-bearing
     * dimension subtrees so `[$clog2(N)-1:0]` doesn't surface `$clog2`
     * or `N` as a port name. *)
    let process_port1 p1 =
      let dir = ref None in
      walk (function
        | Input -> dir := Some `Input
        | Output -> dir := Some `Output
        | Inout -> dir := Some `Input  (* bidirectional -> primary I/O linked var *)
        | _ -> ()) p1;
      let explicit_w = extract_range ~pkgs ~params p1 in
      let w_opt =
        match explicit_w with
        | Some _ -> Some (width_of ~pkgs ~params p1)
        | None ->
            (* No packed dimension — but if this port has an explicit
             * direction token, it's the start of a fresh comma group
             * (e.g. `output logic a, b` after `input [3:0] d`) and
             * the width defaults to 1, NOT inherited from the
             * previous group. *)
            (match !dir with
             | Some _ -> Some 1
             | None -> None)
      in
      let n = ref None in
      let rec walk_skip t =
        match t with
        | TUPLE6 (STRING t', _, _, _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE4 (STRING t', _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE3 (STRING t', SymbolIdentifier id, _)
          when prefix_is "unqualified_id" t' && !n = None ->
            n := Some id
        | TUPLE2 (a, b) -> walk_skip a; walk_skip b
        | TUPLE3 (a, b, c) -> walk_skip a; walk_skip b; walk_skip c
        | TUPLE4 (a, b, c, d) ->
            walk_skip a; walk_skip b; walk_skip c; walk_skip d
        | TUPLE5 (a, b, c, d, e) ->
            List.iter walk_skip [a; b; c; d; e]
        | TUPLE6 (a, b, c, d, e, f) ->
            List.iter walk_skip [a; b; c; d; e; f]
        | TUPLE7 (a, b, c, d, e, f, g) ->
            List.iter walk_skip [a; b; c; d; e; f; g]
        | TUPLE8 (a, b, c, d, e, f, g, h) ->
            List.iter walk_skip [a; b; c; d; e; f; g; h]
        | TLIST xs -> List.iter walk_skip xs
        | _ -> ()
      in
      walk_skip p1;
      events := (!dir, w_opt, !n) :: !events
    in
    (* Use the global walk (which handles all TUPLE arities up to
     * TUPLE15) to visit every node in depth-first pre-order. The
     * port1 / explicit-decl handler updates inheritance state and
     * emits names. *)
    let visit t =
      let is_port_tag tag =
        prefix_is "port1" tag
        || prefix_is "module_port_declaration" tag
        || prefix_is "port_declaration_noattr" tag
      in
      match t with
      | TUPLE2 (STRING tag, _) when is_port_tag tag -> process_port1 t
      | TUPLE3 (STRING tag, _, _) when is_port_tag tag -> process_port1 t
      | TUPLE4 (STRING tag, _, _, _) when is_port_tag tag -> process_port1 t
      | TUPLE5 (STRING tag, _, _, _, _) when is_port_tag tag -> process_port1 t
      | _ -> ()
    in
    (* Walk the whole module body — explicit port decls and the
     * module_port_list_opt's port1 entries are siblings in Verible's
     * AST, so we need a global in-order walk to keep direction+width
     * inheritance correct. *)
    walk visit mdecl.m_body;
    let _ = port_list_node in
    (* Verible's DFS order is the REVERSE of source order for sibling
     * port nodes (left-recursive grammar builds TLISTs back-to-front).
     * Walk the events list as-is (it was prepended) which gives
     * source order, applying inheritance as we go. *)
    let source_order = !events in
    List.iter (fun (dir_opt, w_opt, n_opt) ->
      (match dir_opt with Some d when d <> `Internal -> cur_dir := d | _ -> ());
      (match w_opt with Some w -> cur_width := w | None -> ());
      match n_opt with
      | Some name when not (List.mem name explicit_names) ->
          acc := {
            name;
            stype = BInt { width = !cur_width; signed = Unsigned };
            direction = !cur_dir;
            initial_value = None; attrs = []; 
          } :: !acc
      | _ -> ()
    ) source_order;
    (* Dedup by name, preserving first occurrence. *)
    let rev = List.rev !acc in
    List.fold_left (fun (seen, out) (s : bsignal) ->
      if List.mem s.name seen then (seen, out)
      else (s.name :: seen, s :: out)
    ) ([], []) rev
    |> snd |> List.rev
  in
  (* Drop interface type/modport names that the inherited-port walker wrongly
   * surfaced as bogus 1-bit ports: for `if2.wr p`, `if2` sits inside an
   * unqualified_id so it looked like a port name.  The real port `p` came from
   * explicit_signals (the interface-port path); exclude the interface type and
   * every modport name of the interfaces used by this module's ports. *)
  let iface_noise =
    List.concat_map (fun (_, iface, _) ->
      let mps = Hashtbl.fold (fun k _ acc ->
        match String.index_opt k '$' with
        | Some i when String.sub k 0 i = iface ->
            String.sub k (i+1) (String.length k - i - 1) :: acc
        | _ -> acc) iface_modports [] in
      iface :: mps) !cur_iface_ports in
  let inherited_signals =
    List.filter (fun (s : bsignal) -> not (List.mem s.name iface_noise))
      inherited_signals in
  let port_signals = explicit_signals @ inherited_signals in
  (* Record this module's interface ports (port_name, iface) so the interface
   * elaboration pass can rebuild source-order port slots from the FINAL
   * (scalarized) signal order — collapsing each port's members into one slot. *)
  (let ifps = List.map (fun (pn, iface, _) -> (pn, iface)) !cur_iface_ports in
   if ifps <> [] then Hashtbl.replace module_iface_ports mdecl.m_name ifps);
  (* Internal nets — `net_declaration1` (wires) plus
   * `non_anonymous_gate_instance_or_register_variable1` (the
   * register-variable shape Verible uses for `reg X;` / `logic X;`
   * declared in the module body). The ...register_variable*2* tag,
   * by contrast, is reserved for actual module instantiations (its
   * parent's instantiation_base1 has an unqualified_id rather than
   * a data_type_primitive). *)
  let net_nodes = collect_by
    (has_tag (prefix_is "net_declaration")) mdecl.m_body in
  (* `reg [W:0] foo, bar;` parses as `instantiation_base1` whose first
   * child is the `instantiation_type` (carrying the packed dimensions)
   * and whose second child is the register-variable list. Collect at
   * the instantiation_base level so the width and the variable names
   * stay paired; bases without a `register_variable1` child are module
   * instantiations and handled elsewhere. *)
  let instbase_nodes = collect_by
    (has_tag (prefix_is "instantiation_base")) mdecl.m_body in
  if Sys.getenv_opt "MITER_VERIBLE_DEBUG" <> None then
    Printf.eprintf "[verible_to_bir] %s: %d instantiation_base nodes\n%!"
      mdecl.m_name (List.length instbase_nodes);
  let reg_var_signals = List.concat_map (fun base ->
    (* The first variable in a comma-separated reg-var list is tagged
     * `non_anonymous_gate_instance_or_register_variable1`; subsequent
     * ones are tagged `gate_instance_or_register_variable1` (no
     * "non_anonymous_" prefix). Collect both so `state_type CState,
     * NState;` gives two signals, not one. *)
    let var_nodes =
      collect_by (has_tag (fun t ->
        prefix_is "non_anonymous_gate_instance_or_register_variable1" t
        || prefix_is "gate_instance_or_register_variable1" t)) base
    in
    if var_nodes = [] then []
    else
      let inst_type = match base with
        | TUPLE3 (_, t, _) -> t
        | _ -> base
      in
      let packed_dims = extract_packed_dims ~pkgs ~params inst_type in
      List.filter_map (fun n ->
        let nm = ref None in
        walk (function
          | SymbolIdentifier id when !nm = None -> nm := Some id
          | _ -> ()) n;
        (* Unpacked array: `reg [W-1:0] mem [0:N]` carries a
         * `decl_variable_dimension1` *inside the var node* (slot 2 of
         * `…_register_variable1`). When present, treat as BArray
         * with depth = |msb − lsb| + 1.
         *
         * 2-D packed array (`logic [A-1:0][B-1:0] x`): outer dim is
         * the array index, inner dim is the per-element width. lzc's
         * `index_lut[WIDTH-1:0][NumLevels-1:0]` and `index_nodes` are
         * exactly this shape — without recognising it as BArray, the
         * per-element writes `index_lut[k] = …` collapse into multi-
         * driver writes on a flat 1-D wire and the output is wrong. *)
        let unpacked = extract_range ~pkgs ~params n in
        (* is_unpacked_arr: outer dim is an UNPACKED range (after the name) —
           candidate for unpacked emission (a later pass forces PACKED any array
           that ever takes a whole-array write, so only per-slot-written arrays
           like device_rdata stay unpacked). *)
        let stype, is_unpacked_arr = match unpacked, packed_dims with
          | Some (m, l), _ ->
              let elem_w = match packed_dims with
                | (mi, li) :: _ -> abs (mi - li) + 1
                | [] -> 1
              in
              (BArray {
                element = BInt { width = elem_w; signed = Unsigned };
                size = abs (m - l) + 1;
              }, true)
          | None, [(om, ol); (im, il)] ->
              (BArray {
                element = BInt { width = abs (im - il) + 1; signed = Unsigned };
                size = abs (om - ol) + 1;
              }, false)
          | None, _ ->
              let elem_w = width_of ~typedefs ~pkgs ~params inst_type in
              (BInt { width = elem_w; signed = Unsigned }, false)
        in
        (* Capture `reg [W:0] X = expr;` declaration-time initialisers.
         * The SVS Verible parser tags these as
         * `trailing_decl_assignment2`, nested inside the register
         * variable node alongside the name and dimensions.  Pull the
         * value subtree (TUPLE4 slot 2 = third child) through the
         * existing expression evaluator; only constant initialisers
         * survive into BIR's `initial_value`.  Without this, FDRE
         * INIT is always 0, which breaks LFSRs / one-hot FSMs / any
         * design whose power-on state matters [[memory: 65]]. *)
        let init_v = ref None in
        let try_value v =
          if !init_v = None then
            try
              match expr_to_bexpr ~pkgs ~params ~arrays:[] v with
              | BConst { value; width } ->
                  init_v := Some (BConst { value; width })
              | _ -> ()
            with _ -> ()
        in
        walk (function
          | TUPLE3 (STRING t, _, v)
            when prefix_is "trailing_decl_assignment" t -> try_value v
          | TUPLE4 (STRING t, _, v, _)
            when prefix_is "trailing_decl_assignment" t -> try_value v
          | TUPLE5 (STRING t, _, v, _, _)
            when prefix_is "trailing_decl_assignment" t -> try_value v
          | _ -> ()) n;
        match !nm with
        | Some id ->
            (* Record a non-zero packed LSB (scalar `logic [31:1] X`) so
             * reference3's slices get rebased to the zero-based decl. *)
            (match stype, unpacked, packed_dims with
             | BInt _, None, [(_, l)] when l <> 0 ->
                 cur_signal_lsb := (id, l) :: !cur_signal_lsb
             | _ -> ());
            Some {
            name = id;
            stype;
            direction = `Internal;
            initial_value = !init_v;
            attrs = if is_unpacked_arr then [("unpacked", "1")] else [];
          }
        | None -> None
      ) var_nodes
  ) instbase_nodes in
  (* Names of memory-array signals — used by the assign/always
   * handlers below to switch indexed lhs to `@mem_write` and indexed
   * rhs to BSelect. *)
  let array_names =
    List.filter_map (fun (s : bsignal) ->
      match s.stype with
      | BArray _ -> Some s.name
      | _ -> None
    ) reg_var_signals
  in
  let internal_signals =
    (List.concat_map (extract_port_decl ~pkgs ~params) net_nodes
     |> List.map (fun s -> { s with direction = `Internal }))
    @ reg_var_signals
  in
  (* Merge by name. When the same signal appears twice (bare name from
   * the port-list AND an explicit K&R `output [W:0] X;` decl), the
   * second entry usually carries better information — explicit
   * direction (Output rather than the default Internal) and explicit
   * width. Old code did first-wins dedup which kept the bare-name
   * Internal/width=1 placeholder, so cva6's K&R-style ct_vfdsu_*
   * modules ended up with all ports classed as Input/1-bit. New rule:
   * later wins for direction (Output > Input > Internal) and for
   * width (>1 > 1).  *)
  let signals : bsignal list =
    let dir_rank = function
      | `Output -> 2 | `Input -> 1 | `Internal -> 0 in
    let bsignal_width (s : bsignal) =
      match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 0 in
    let merge a b : bsignal =
      let pick = if dir_rank b.direction > dir_rank a.direction then b
                 else if dir_rank b.direction < dir_rank a.direction then a
                 else if bsignal_width b > bsignal_width a then b
                 else a in
      pick
    in
    let by_name : (string, bsignal) Hashtbl.t = Hashtbl.create 16 in
    let order = ref [] in
    List.iter (fun (s : bsignal) ->
      match Hashtbl.find_opt by_name s.name with
      | None -> Hashtbl.replace by_name s.name s; order := s.name :: !order
      | Some prev -> Hashtbl.replace by_name s.name (merge prev s)
    ) (port_signals @ internal_signals);
    List.rev_map (Hashtbl.find by_name) !order
  in
  (* Populate per-module width cache before any expr_to_bexpr call —
   * lets `result_type_for` set real widths on BBinOp/BUnOp instead
   * of falling back to dummy_bool. *)
  cur_signal_widths := List.filter_map (fun (s : bsignal) ->
    let w = match s.stype with
      | BInt { width; _ } -> width
      | BArray { size; element = BInt { width; _ }; _ } -> size * width
      | _ -> 0 in
    if w > 0 then Some (s.name, w) else None
  ) signals;
  (* Pre-scan: any continuous assign with an indexed LHS gets its
     target promoted into array_names — even if the underlying
     signal is a plain BInt rather than a BArray.  This lets the
     extract_assign path emit @mem_write for cases like
     `assign out[i] = X` (cell-mapped Verilog from our own synth),
     so the downstream merge_array_writes pass can consolidate the
     N per-bit drives into a single full-bus concat assign — which
     is what both the Z3 miter (slice indices preserved) and
     OpenROAD's [read_verilog] (bit-blasted assigns visible in
     source) need to see. *)
  let cont_assigns =
    collect_by (has_tag (prefix_is "continuous_assign")) mdecl.m_body in
  let scan_indexed_lhs nodes tag_prefix lhs_pos =
    List.concat_map (fun n ->
      let found = collect_by (has_tag (prefix_is tag_prefix)) n in
      let _ = tag_prefix in
      List.filter_map (fun a ->
        let lhs = match a, lhs_pos with
          | TUPLE4 (_, l, _, _), 1 -> Some l
          | TUPLE6 (_, l, _, _, _, _), 1 -> Some l
          | _ -> None in
        match lhs with
        | Some lhs ->
            (match lhs_indexed_of lhs with
             | Some (name, Some _) -> Some name
             | _ -> None)
        | None -> None) found
    ) nodes in
  (* Pre-scan: collect every name that appears with an indexed LHS,
     across continuous assigns (`assign x[i] = ...`) AND procedural
     assigns (`x[i] <= ...` inside always_ff / always_comb).  Promote
     each into array_names so the assign/always handlers route them
     through the @mem_write path → merge_array_writes consolidates
     the per-slice drives into a single full-bus assign that both
     Verible's downstream and OpenROAD's read_verilog handle. *)
  let indexed_targets =
    let cont = scan_indexed_lhs cont_assigns "cont_assign" 1 in
    let blk = scan_indexed_lhs [mdecl.m_body] "assignment_statement_no_expr" 1 in
    let nb  = scan_indexed_lhs [mdecl.m_body] "nonblocking_assignment" 1 in
    List.sort_uniq compare (cont @ blk @ nb) in
  let array_names =
    (* The initial array_names came only from reg_var_signals (internal regs),
       so an array-typed PORT (`input [W-1:0] host_addr_i [NrHosts]`) was NOT
       treated as an array: a dynamic index `host_addr_i[sel]` then lowered to a
       1-bit select `(host_addr_i >> sel) & 1` instead of a W-bit element select,
       truncating the bus master-address mux (device_addr = 1 bit) and stalling
       the CPU fetch.  Include every BArray signal from the full port+internal
       list. *)
    let all_arrays = List.filter_map (fun (s : bsignal) ->
      match s.stype with BArray _ -> Some s.name | _ -> None) signals in
    List.sort_uniq compare (array_names @ indexed_targets @ all_arrays) in
  let assign_procs = List.concat_map (fun ca ->
    extract_assign ~pkgs ~params ~arrays:array_names ca
  ) cont_assigns in
  (* Wire-with-initialiser shape: `wire [W:0] foo = expr;` parses
     as a [net_declaration*] containing a [net_decl_assign1] subnode
     that carries the RHS expression.  Treat each one as an extra
     continuous assign — without this the wire is declared but never
     driven, and Hardcaml rejects the parent module with "circuit
     input signal must have a port name (unassigned wire?)".  Common
     in the SmolLM rtl (swiglu, matvec_int8_engine) and elsewhere. *)
  let net_decl_assigns =
    let nodes = collect_by (has_tag
      (fun t -> prefix_is "net_decl_assign1" t)) mdecl.m_body in
    List.filter_map (function
      | TUPLE4 (STRING tag, SymbolIdentifier name, _eq, rhs)
        when prefix_is "net_decl_assign" tag ->
          let rhs_e =
            try Some (expr_to_bexpr ~pkgs ~params
                        ~arrays:array_names rhs)
            (* Let Silent_zero_substitution propagate — default-strict
               should hit the top, not be silently dropped here. *)
            with Silent_zero_substitution _ as e -> raise e
               | _ -> None in
          (match rhs_e with
           | None -> None
           | Some rhs_e ->
               Some (BCombinational {
                 name = "net_assign_" ^ name;
                 sensitivity = [BAny];
                 body = [BAssign { lhs = name; rhs = rhs_e }];
               }))
      | _ -> None) nodes in
  let assign_procs = assign_procs @ net_decl_assigns in
  (* `initial $readmemh("file.hex", mem);` ROM initialiser (#132).
     Hardcaml has no concept of an initial block, so a memory whose
     content comes from $readmemh has no driver at the IR level and
     synthesis aborts ("circuit input signal must have a port name
     (unassigned wire?)").  Translate the call into a constant
     driver: read the hex file at convert time, build a BConcat of
     element-wide BConsts (MSB-first), and emit a BCombinational
     that drives the target memory with that BConcat.

     Path resolution: try the path verbatim, then relative to the
     directory of every input .sv (the converter doesn't see the
     file path here, so we use $MEM_INIT_DIR if set, plus PWD).
     Errors are non-fatal — we log to stderr and skip; the synth
     will then surface the unassigned-wire error and the user can
     point MEM_INIT_DIR at the correct directory. *)
  let mem_init_procs =
    let mem_shape name =
      List.find_map (fun (s : bsignal) ->
        if s.name = name then
          match s.stype with
          | BArray { size; element = BInt { width; _ } } -> Some (size, width)
          | _ -> None
        else None) reg_var_signals in
    let resolve path =
      let basename = Filename.basename path in
      let candidates =
        let env_dir = match Sys.getenv_opt "MEM_INIT_DIR" with
          | Some d -> [Filename.concat d basename]
          | None -> [] in
        let auto_dirs =
          List.map (fun d -> Filename.concat d basename)
            !mem_init_search_paths in
        path :: env_dir @ auto_dirs
        @ (List.map (fun d -> Filename.concat d basename)
             [Sys.getcwd ();
              Filename.concat (Sys.getcwd ()) "generated";
              Filename.concat (Sys.getcwd ()) "../generated"]) in
      List.find_opt Sys.file_exists candidates in
    let read_hex elem_w n_words path =
      try
        let ic = open_in path in
        let words = ref [] in
        let count = ref 0 in
        (try
          while !count < n_words do
            let line = input_line ic in
            (* Trim and strip Verilog `//` comments. *)
            let trim_at s c =
              try String.sub s 0 (String.index s c)
              with Not_found -> s in
            let line = trim_at line '/' in
            let line = String.trim line in
            if line <> "" then begin
              let v = int_of_string ("0x" ^ line) in
              let mask =
                if elem_w >= 63 then -1
                else (1 lsl elem_w) - 1 in
              words := (v land mask) :: !words;
              incr count
            end
          done
        with End_of_file -> ());
        close_in ic;
        (* Pad with zero if file shorter than memory. *)
        while !count < n_words do
          words := 0 :: !words; incr count
        done;
        Some (List.rev !words)
      with _ -> None in
    let initial_nodes =
      collect_by (has_tag (prefix_is "initial_construct")) mdecl.m_body in
    let strip_quotes p =
      let n = String.length p in
      if n >= 2 && p.[0] = '"' && p.[n - 1] = '"'
      then String.sub p 1 (n - 2) else p in
    List.filter_map (fun init ->
      (* Bind the $readmemh(FILE, MEM) call's TWO arguments specifically: the
         first token after $readmemh is FILE (a string literal, or a param
         identifier like MemInitFile), the second is the target MEM array.
         Scanning the whole initial block for "first string / first id" was
         wrong — it bound the $value$plusargs / $display strings and picked
         MemInitFile (the file) as the target. *)
      let saw_readmemh = ref false in
      let args = ref [] in  (* reversed: at most the first 2 arg tokens *)
      walk (function
        | SymbolIdentifier id when id = "$readmemh" -> saw_readmemh := true
        | TK_StringLiteral s when !saw_readmemh && List.length !args < 2 ->
            args := `Str s :: !args
        | SymbolIdentifier id
          when !saw_readmemh && id <> "$readmemh" && List.length !args < 2 ->
            args := `Id id :: !args
        | _ -> ()) init;
      match !saw_readmemh, List.rev !args with
      | true, [ file_arg; `Id tgt ] ->
          (* FILE: a literal path, or a parameter (MemInitFile) resolved from
             this module's specialised params.  Empty/unset -> no init, which
             matches the RTL guard `if (MemInitFile != "")`. *)
          let path =
            match file_arg with
            | `Str s -> strip_quotes s
            | `Id pid ->
                (match List.assoc_opt pid params with
                 | Some v -> strip_quotes (String.trim v)
                 | None -> "")
          in
          if path = "" then None
          else
          (match mem_shape tgt with
           | None ->
               Printf.eprintf
                 "[verible_to_bir] WARNING: $readmemh target %s is not a \
                  recognised BArray; skipping ROM init from %s\n%!" tgt path;
               None
           | Some (size, elem_w) ->
               (match resolve path with
                | None ->
                    Printf.eprintf
                      "[verible_to_bir] WARNING: $readmemh hex file not \
                       found: %s (set MEM_INIT_DIR or run from the \
                       directory containing %s)\n%!"
                      path (Filename.basename path);
                    None
                | Some p ->
                    (match read_hex elem_w size p with
                     | None ->
                         Printf.eprintf
                           "[verible_to_bir] WARNING: failed to read %s\n%!" p;
                         None
                     | Some words ->
                         (* MSB-first BConcat: words[size-1] is the high
                            slot, words[0] is the low.  Match the layout
                            used by the @mem_write lowering in
                            behavioral_to_hardcaml so reads via BSelect
                            land on the same slot the file declared. *)
                         let parts = List.rev_map (fun v ->
                           BConst { value = Z.of_int v; width = elem_w }) words in
                         Some (BCombinational {
                           name = "mem_init_" ^ tgt;
                           sensitivity = [BAny];
                           body = [BAssign {
                             lhs = tgt;
                             rhs = BConcat parts;
                           }];
                         })))
           )
      | _ -> None) initial_nodes
  in
  let assign_procs = assign_procs @ mem_init_procs in
  (* Procedural `initial` blocks (non-$readmemh): keep their statement
     bodies as reserved-name processes for COMPILE-TIME evaluation by
     behavioral_initeval (run inside svd.unroll).  rgmii_lfsr computes
     its CRC mask matrices this way — dropping the block left the masks
     zero and every CRC output constant-folded to 0 (FCS = ~0). *)
  let initial_procs =
    let initial_nodes =
      collect_by (has_tag (prefix_is "initial_construct")) mdecl.m_body in
    List.filter_map (fun init ->
      let saw_readmemh = ref false in
      walk (function
        | SymbolIdentifier id when id = "$readmemh" -> saw_readmemh := true
        | _ -> ()) init;
      if !saw_readmemh then None
      else
        let body_nodes = collect_by (has_tag (fun t ->
          prefix_is "seq_block" t ||
          prefix_is "assignment_statement_no_expr" t ||
          prefix_is "conditional_statement" t ||
          prefix_is "case_statement" t ||
          prefix_is "for_loop_statement" t ||
          prefix_is "loop_statement" t)) init in
        match body_nodes with
        | b :: _ ->
            Some (BCombinational {
              name = "__initial__";
              sensitivity = [BAny];
              body = [stmt_to_bstmt ~pkgs ~params ~arrays:array_names b];
            })
        | [] -> None)
      initial_nodes
  in
  let assign_procs = assign_procs @ initial_procs in
  let always_procs = extract_always ~pkgs ~params ~arrays:array_names mdecl.m_body in
  let instances = extract_instances ~pkgs ~params ~arrays:array_names mdecl.m_body in
  (* RULE: any array that ever takes a WHOLE-array write (`arr = <expr>`, a
     BAssign with the bare array as lhs — e.g. a combinational `arr = '0`
     default fill, or `imd_val_d_o = '0`) is forced PACKED, overriding the
     source-unpacked tag.  Whole-array writes are packed-friendly and, left
     unpacked, would either be illegal (`arr='0` fill) or lose the merge's
     0-fill of uncovered slots (→ X in sim).  Only STRICTLY per-slot-written
     arrays (device_rdata: `arr[k]=…` + instance drivers, no whole write)
     stay unpacked — exactly the multi-driver case that needs it. *)
  let signals =
    (* A whole-array COPY (`a = b`) must be PACKED on both sides: it survives to
       emit as a whole-array assign and the two sides must match.  A whole-array
       FILL / init concat (`arr = '0`, `mem = {…}`, non-BVar RHS) also forces
       packed — UNLESS the array is a genuine MEMORY (written per-element via
       indexed `@mem_write`, e.g. `mem[a] = …`).  A memory's whole assigns are
       the `$readmemh` boot-image loader (`mem = {word_{d-1},…,word_0}`, which
       Behavioral_meminfer.is_mem_init_driver lifts into the BRAM INIT and strips
       for any array with write ports) or a byte-slice read-modify-write
       (`mem = {…mem[a]…}`) — both re-lowered to a per-element `initial` block or
       indexed writes before emit.  So a RAM must stay UNPACKED (`reg [W] mem [D]`)
       for Vivado to infer BRAM.  A write-less constant ROM (`mem = {const,…}`,
       no @mem_write) keeps its concat as its value and stays packed, emitting a
       legal whole-array assign.  (A real `initial $readmemh(...)` is a BCallStmt,
       not a BAssign, so it never registers as a whole-write here anyway.) *)
    let whole_copied : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    let whole_filled : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    let mem_written : (string, unit) Hashtbl.t = Hashtbl.create 16 in
    let rec ss = function
      | BAssign { lhs; rhs = BVar y } ->
          Hashtbl.replace whole_copied lhs ();
          Hashtbl.replace whole_copied y ()
      | BAssign { lhs; _ } ->
          Hashtbl.replace whole_filled lhs ()
      | BCallStmt { func = "@mem_write"; args = BVar arr :: _ } ->
          Hashtbl.replace mem_written arr ()
      | BIf { then_stmts; else_stmts; _ } ->
          List.iter ss then_stmts; List.iter ss else_stmts
      | BCase { cases; default; _ } ->
          List.iter (fun (_, b) -> List.iter ss b) cases; List.iter ss default
      | BWhile { body; _ } -> List.iter ss body
      | BFor { init; update; body; _ } -> ss init; ss update; List.iter ss body
      | BBlock b -> List.iter ss b
      | BCallStmt _ | BReturn _ -> ()
    in
    List.iter (function
      | BCombinational { body; _ } -> List.iter ss body
      | BSequential { body; _ } -> List.iter ss body)
      (assign_procs @ always_procs);
    List.map (fun (s : bsignal) ->
      let force_pack =
        Hashtbl.mem whole_copied s.name
        || (Hashtbl.mem whole_filled s.name
            && not (Hashtbl.mem mem_written s.name)) in
      if List.mem_assoc "unpacked" s.attrs && force_pack
      then { s with attrs = List.remove_assoc "unpacked" s.attrs }
      else s) signals in
  (* Post-pass: merge combinational @mem_write groups targeting the
   * same array into a single full-array concat assignment. lzc's
   * `for (genvar j…) assign index_lut[j] = …` unrolls to N
   * BCombinational @mem_write nodes; downstream Z3 / meminfer
   * doesn't track combinational @mem_write side-effects (meminfer's
   * find_ram_writes only walks BSequential), so each @mem_write
   * is a no-op and the array stays free. Fold them here while we
   * still have ergonomic per-call info: collect all combinational
   * @mem_write to a given array, sort by literal index ascending
   * (LSB-first), build a BConcat in MSB-first order, and emit a
   * single BAssign. *)
  let merge_array_writes processes =
    let mem_writes : (string, (int * bexpr) list) Hashtbl.t =
      Hashtbl.create 8 in
    (* UNPACKED arrays keep per-slot `x[k]=data` assigns (one driver per slot);
       merging into a whole-array concat is illegal for an unpacked target and
       collides with instance drivers on other slots. *)
    let unpacked_arrays =
      List.filter_map (fun (s : bsignal) ->
        if List.mem_assoc "unpacked" s.attrs then Some s.name else None) signals in
    (* Constant-fold a small set of arithmetic shapes so addresses
     * like `((32'1 - 32'1) + 32'0)` (the lzc generate-for index
     * expression after genvar-substitution) reduce to BConst, which
     * lets the merge match on a literal index. *)
    let rec fold = function
      | BBinOp { op = BAdd;
                 lhs = BConst { value = a; width = w };
                 rhs = BConst { value = b; _ }; _ } ->
          BConst { value = Z.add a b; width = w }
      | BBinOp { op = BSub;
                 lhs = BConst { value = a; width = w };
                 rhs = BConst { value = b; _ }; _ } ->
          BConst { value = Z.sub a b; width = w }
      | BBinOp { op = BMul;
                 lhs = BConst { value = a; width = w };
                 rhs = BConst { value = b; _ }; _ } ->
          BConst { value = Z.mul a b; width = w }
      | BBinOp r ->
          let lhs' = fold r.lhs and rhs' = fold r.rhs in
          (match lhs', rhs' with
           | BConst { value = a; width = w }, BConst { value = b; _ } ->
               (match r.op with
                | BAdd -> BConst { value = Z.add a b; width = w }
                | BSub -> BConst { value = Z.sub a b; width = w }
                | BMul -> BConst { value = Z.mul a b; width = w }
                | _ -> BBinOp { r with lhs = lhs'; rhs = rhs' })
           | _ -> BBinOp { r with lhs = lhs'; rhs = rhs' })
      | e -> e
    in
    let other_procs = List.filter_map (fun p ->
      match p with
      | BCombinational { body = [BCallStmt {
          func = "@mem_write";
          args = [BVar arr; addr; data] }]; _ }
        when List.mem arr array_names
             && not (List.mem arr unpacked_arrays) ->
          (match fold addr with
           | BConst { value = idx; _ } ->
               let bucket =
                 try Hashtbl.find mem_writes arr with Not_found -> [] in
               Hashtbl.replace mem_writes arr ((Z.to_int idx, data) :: bucket);
               None
           | _ -> Some p)
      | _ -> Some p
    ) processes in
    let merged = Hashtbl.fold (fun arr writes acc ->
      let arr_size, elem_w, is_scalar =
        match List.find_opt (fun (s : bsignal) -> s.name = arr) signals with
        | Some { stype = BArray { size; element = BInt { width; _ } }; _ } ->
            (size, width, false)
        | Some { stype = BInt { width; _ }; _ } ->
            (* Scalar bit-vector — `assign foo[k] = X` writes a single
               bit, not an array element.  Each "index" corresponds to
               one bit; for multi-bit slice writes like `foo[3:2] = …`
               the slot occupies multiple bits and we recover its
               width from the data expression. *)
            (width, 1, true)
        | _ -> (List.length writes, 1, false)
      in
      (* Width of a single write's data — used to recover multi-bit
         slot widths for scalar bit-vector slice writes (`foo[3:2] = X`
         comes in as @mem_write(foo, 2, X) with X 2-bit). *)
      let data_width data =
        match width_of_bexpr_ctx
                (List.map (fun (s : bsignal) ->
                   s.name, width_of_type s.stype) signals) data with
        | Some w when w >= 1 -> w
        | _ -> elem_w in
      let truncate_to_elem ~w e =
        if w >= 32 then e
        else BSlice { signal = e; msb = w - 1; lsb = 0 }
      in
      (* Sort msb-first so the concat reads top-to-bottom. *)
      let sorted = List.sort (fun (a, _) (b, _) -> compare b a) writes in
      (* Filler for uncovered slots.  merge_array_writes only ever runs on
         COMBINATIONAL processes, where a slot not covered by an explicit
         write is simply undriven → its synthesizable default is constant
         zero.  A self-read (`arr[k] = arr[k]`) would instead close a
         combinational loop and leave the element wire driverless — which is
         exactly what happens now that arrayed ports/regs (`logic [W:0]
         arr [N]`) correctly carry BArray type: a partially-written comb
         array (e.g. only arr[0] assigned) used to be a scalar BInt and hit
         the zero branch, but as a true BArray it fell into the self-read
         branch and Hardcaml aborted.  Sequential array writes (inferred
         RAMs / register files) keep their read-modify-write self-read via
         lower_mem_writes_in_seq, which is unaffected by this. *)
      let filler_at _k =
        if true then BConst { value = Z.zero; width = elem_w }
        else BSelect {
          array = BVar arr;
          index = BConst { value = Z.of_int _k; width = 32 };
        }
      in
      (* A scalar VECTOR (`logic [W:0] v`, not an array) only PARTIALLY covered
         by these indexed writes has its other bits driven by ANOTHER process
         (ibex_alu: `assign amt[5]` here, `always_comb amt[4:0]` elsewhere).
         Reconstructing the whole vector with a zero filler adds a second,
         constant driver on those bits — Vivado multi-driven net, GND wins, the
         low bits read 0.  Emit the collected writes as PARTIAL slice-writes (one
         driver per covered range), leaving the rest to their own process.  A
         fully-covered vector (the cell-mapped `assign out[i]` bit-blast this
         merge was built for) falls through to the whole-vector concat. *)
      let covered =
        List.fold_left (fun a (_, data) ->
          a + (if is_scalar then data_width data else elem_w)) 0 writes in
      if is_scalar && covered < arr_size then
        List.map (fun (idx, data) ->
          let w = data_width data in
          BCombinational {
            name = Printf.sprintf "assign_%s_%d" arr idx;
            sensitivity = [BAny];
            body = [BCallStmt {
              func = "@slice_write";
              args = [BVar arr;
                      BConst { value = Z.of_int idx; width = 32 };
                      BConst { value = Z.of_int (idx - w + 1); width = 32 };
                      truncate_to_elem ~w data] }];
          }) writes @ acc
      else begin
      let parts = ref [] in
      let cursor = ref (arr_size - 1) in
      List.iter (fun (idx, data) ->
        (* For scalar bit-vector slice writes (`foo[hi:lo] = …`)
           verible's [lhs_indexed_of] stores `hi` (the msb) as the
           idx, and the data's width tells us how far down the slot
           extends.  For true unpacked arrays each idx is one
           element of elem_w bits.  In both cases the slot occupies
           [slot_top : slot_top - slot_w + 1]. *)
        let slot_w = if is_scalar then data_width data else elem_w in
        let slot_top = if is_scalar then idx else idx in
        if slot_top < !cursor then
          for k = !cursor downto slot_top + 1 do
            parts := filler_at k :: !parts
          done;
        parts := truncate_to_elem ~w:slot_w data :: !parts;
        cursor := slot_top - slot_w
      ) sorted;
      if !cursor >= 0 then
        for k = !cursor downto 0 do
          parts := filler_at k :: !parts
        done;
      let rhs = match List.rev !parts with
        | [single] -> single
        | many -> BConcat many
      in
      BCombinational {
        name = "merged_array_" ^ arr;
        sensitivity = [BAny];
        body = [BAssign { lhs = arr; rhs }];
      } :: acc
      end
    ) mem_writes [] in
    other_procs @ merged
  in
  (* Convert @mem_write inside BSequential bodies to a full-bus
     BAssign with read-modify-write semantics — same shape as the
     downstream behavioral_to_hardcaml lowering, lifted to BIR
     level so Behavioral_ffrip recognises BArray FFs as ordinary
     register writes.  Without this, source's `reg [W:0] arr[D:0]`
     with `always_ff @(posedge clk) arr[k] <= data` produces a
     BSequential with [BCallStmt @mem_write(arr, k, data)] — ffrip
     ignores BCallStmts, so arr never appears in the rip set, and
     the parent miter's interface check fails because the cell-
     mapped side (which uses BAssign throughout) does have arr. *)
  let lower_mem_writes_in_seq processes =
    List.map (fun p ->
      match p with
      | BSequential ({ body; _ } as s) ->
          (* Collect every @mem_write per target array, building an
             ordered list of (idx, data) pairs.  After merge_seq_processes,
             all writes from a clock domain live in one body, so we can
             build ONE full-bus BAssign per array — folding multiple
             writes via BCond chains so that non-blocking semantics
             survive ffrip's last-write-wins behaviour on BAssign. *)
          let writes : (string, (bexpr * bexpr) list) Hashtbl.t =
            Hashtbl.create 4 in
          List.iter (fun stmt ->
            match stmt with
            | BCallStmt { func = "@mem_write";
                          args = [BVar arr; idx; data] } ->
                let cur = try Hashtbl.find writes arr with Not_found -> [] in
                Hashtbl.replace writes arr (cur @ [(idx, data)])
            | _ -> ()) body;
          let emitted = Hashtbl.create 4 in
          let body' = List.filter_map (fun stmt ->
            match stmt with
            | BCallStmt { func = "@mem_write"; args = [BVar arr; _; _] } ->
                if Hashtbl.mem emitted arr then None
                else begin
                  Hashtbl.add emitted arr ();
                  match List.find_opt (fun (sg : bsignal) -> sg.name = arr)
                          signals with
                  | Some { stype = BArray { size; element = BInt { width=_; _ } }; _ } ->
                      let pairs = Hashtbl.find writes arr in
                      let parts = List.init size (fun k ->
                        let k_const = BConst { value = Z.of_int k; width = 32 } in
                        let init = BSelect { array = BVar arr;
                                             index = k_const } in
                        List.fold_left (fun acc (idx, data) ->
                          BCond {
                            condition = BBinOp {
                              op = BEq;
                              lhs = idx;
                              rhs = k_const;
                              result_type = BInt { width = 1; signed = Unsigned };
                            };
                            then_val = data;
                            else_val = acc;
                          }
                        ) init pairs) in
                      let rhs = BConcat (List.rev parts) in
                      Some (BAssign { lhs = arr; rhs })
                  | _ -> Some stmt
                end
            | other -> Some other
          ) body in
          BSequential { s with body = body' }
      | other -> other
    ) processes
  in
  (* Merge BSequentials with the same clock-domain key, so that 16
     separate `always @(posedge clk) text_out[hi:lo] <= byte` blocks
     coalesce into one BSequential whose body has 16 @slice_writes —
     then [lower_slice_writes_in_seq] can fuse them into a single
     [BAssign { lhs=text_out; rhs=BConcat […] }].  Without this, ffrip
     processes each slice's BSequential separately and the last one
     wins, leaving 15/16 slices undriven. *)
  let merge_seq_processes processes =
    let domain_key clock clock_edge reset reset_edge reset_async =
      Printf.sprintf "%s|%s|%s|%s|%b"
        clock
        (match clock_edge with `Pos -> "p" | `Neg -> "n")
        (Option.value reset ~default:"")
        (match reset_edge with
         | None -> "" | Some `Pos -> "rp" | Some `Neg -> "rn")
        reset_async
    in
    let groups = Hashtbl.create 4 in
    let order = ref [] in
    let other_procs = List.filter (fun p -> match p with
      | BSequential { name; clock; clock_edge; reset; reset_edge; blocking_vars;
                      reset_async; body } ->
          let key = domain_key clock clock_edge reset reset_edge reset_async in
          (match Hashtbl.find_opt groups key with
           | None ->
               Hashtbl.add groups key
                 (name, clock, clock_edge, reset, reset_edge,
                  reset_async, ref body, ref blocking_vars);
               order := key :: !order
           | Some (_, _, _, _, _, _, body_ref, bv_ref) ->
               body_ref := !body_ref @ body;
               bv_ref := List.sort_uniq compare (!bv_ref @ blocking_vars));
          false
      | _ -> true
    ) processes in
    let merged_seqs =
      List.rev_map (fun key ->
        let (name, clock, clock_edge, reset, reset_edge, reset_async,
             body_ref, bv_ref) = Hashtbl.find groups key in
        BSequential { name; clock; clock_edge; reset; reset_edge;
                      reset_async; body = !body_ref;
                      blocking_vars = !bv_ref }
      ) !order
    in
    other_procs @ merged_seqs
  in
  (* Lower @slice_write calls inside BSequential bodies to a single
     full-bus BAssign per target lvalue.  After [merge_seq_processes]
     a target's slice writes all live in the same body, so we can
     build a [BConcat] high-to-low covering the whole bus. *)
  let lower_slice_writes_in_seq processes =
    List.map (fun p ->
      match p with
      | BSequential ({ body; _ } as s) ->
          (* Collect @slice_writes per target. *)
          let slices : (string, (int * int * bexpr) list) Hashtbl.t =
            Hashtbl.create 4 in
          let const_of = function
            | BConst { value; _ } -> Some (Z.to_int value)
            | _ -> None in
          List.iter (fun stmt ->
            match stmt with
            | BCallStmt { func = "@slice_write";
                          args = [BVar lhs; m; l; data] } ->
                (match const_of m, const_of l with
                 | Some msb, Some lsb ->
                     let cur = try Hashtbl.find slices lhs with Not_found -> [] in
                     Hashtbl.replace slices lhs ((msb, lsb, data) :: cur)
                 | _ -> ())
            | _ -> ()
          ) body;
          let body' = List.filter_map (fun stmt ->
            match stmt with
            | BCallStmt { func = "@slice_write";
                          args = [BVar lhs; _; _; _] }
              when Hashtbl.mem slices lhs ->
                if Hashtbl.find slices lhs = [] then None
                else begin
                  let writes = Hashtbl.find slices lhs in
                  Hashtbl.replace slices lhs [];
                  let total_w =
                    match List.find_opt
                            (fun (sg : bsignal) -> sg.name = lhs) signals with
                    | Some s -> width_of_type s.stype
                    | None -> 1 in
                  let sorted =
                    List.sort (fun (a, _, _) (b, _, _) -> compare b a) writes in
                  let parts = ref [] in
                  let cursor = ref (total_w - 1) in
                  List.iter (fun (msb, lsb, data) ->
                    let hi = max msb lsb and lo = min msb lsb in
                    if hi < !cursor then begin
                      let gap_msb = !cursor in
                      let gap_lsb = hi + 1 in
                      parts := BSlice { signal = BVar lhs;
                                        msb = gap_msb; lsb = gap_lsb }
                               :: !parts
                    end;
                    parts := data :: !parts;
                    cursor := lo - 1
                  ) sorted;
                  if !cursor >= 0 then
                    parts := BSlice { signal = BVar lhs;
                                      msb = !cursor; lsb = 0 } :: !parts;
                  let rhs = match List.rev !parts with
                    | [single] -> single
                    | many -> BConcat many in
                  Some (BAssign { lhs; rhs })
                end
            | other -> Some other
          ) body in
          BSequential { s with body = body' }
      | other -> other
    ) processes
  in
  (* Lower @part_sel_write_up calls inside BSequential bodies to a
     full-bus BAssign per target.  `name[base +: width] <= rhs` (with
     constant width) becomes
        name := { … slot[N-1], …, slot[1], slot[0] }
     where slot[k] = (base == k * width) ? rhs : name[k*w +: w].
     N = total_width / width.  This mirrors the @mem_write lowering
     so [behavioral_to_hardcaml]'s downstream BCase / Always handling
     sees a single full-bus driver (not a partial slice) for the FF
     pre-pass and downstream encoders. *)
  let lower_part_sel_writes_in_seq processes =
    List.map (fun p ->
      match p with
      | BSequential ({ body; _ } as s) ->
          let body' = List.map (fun stmt ->
            match stmt with
            | BCallStmt { func = "@part_sel_write_up";
                          args = [BVar lhs; base; width; data] } ->
                let const_of = function
                  | BConst { value; _ } -> Some (Z.to_int value)
                  | _ -> None in
                (match const_of width with
                 | Some w when w > 0 ->
                     let total_w =
                       match List.find_opt
                               (fun (sg : bsignal) -> sg.name = lhs) signals with
                       | Some s -> width_of_type s.stype
                       | None -> 0 in
                     if total_w = 0 || total_w mod w <> 0 then stmt
                     else
                       let n = total_w / w in
                       let parts = List.init n (fun k ->
                         let k_const = BConst { value = Z.of_int (k * w); width = 32 } in
                         let cur_slot = BSlice {
                           signal = BVar lhs;
                           msb = (k + 1) * w - 1;
                           lsb = k * w;
                         } in
                         BCond {
                           condition = BBinOp {
                             op = BEq;
                             lhs = base;
                             rhs = k_const;
                             result_type = BInt { width = 1; signed = Unsigned };
                           };
                           then_val = data;
                           else_val = cur_slot;
                         }) in
                       BAssign { lhs;
                                 rhs = BConcat (List.rev parts) }
                 | _ -> stmt)
            | other -> other
          ) body in
          BSequential { s with body = body' }
      | other -> other
    ) processes in
  let processes =
    let merged = merge_array_writes (assign_procs @ always_procs) in
    if Sys.getenv_opt "DISABLE_MEM_WRITE_LOWER" <> None then merged
    else
      merged
      |> merge_seq_processes
      |> lower_mem_writes_in_seq
      |> lower_slice_writes_in_seq
      |> lower_part_sel_writes_in_seq
  in
  (* LHS-context width propagation (#128).
     SystemVerilog evaluates `r = a OP b` (and other arithmetic)
     at width max(width(r), width(a), width(b)).  result_type_for
     above only sees max(operand widths) and has no LHS context,
     so an arithmetic op assigned to a wider target ends up
     truncated mid-expression and the consumer (z3_miter,
     hardcaml lowering, lib_map) compensates per-op or doesn't.
     Walk each BAssign here, look up its LHS width, and propagate
     that down through arithmetic operators in the rhs.  Per the
     LRM, comparisons / reductions / concat / slice / select are
     "self-determined" — context width stops there. *)
  let processes =
    let lhs_width name =
      match List.find_opt (fun (s : bsignal) -> s.name = name) signals with
      | Some s -> width_of_type s.stype
      | None -> 0 in
    let rec propagate ctx_w e =
      match e with
      | BVar _ | BConst _ | BSelect _ | BSlice _
      | BConcat _ | BReplicate _ | BCall _ -> e
      | BBinOp { op; lhs; rhs; result_type } ->
          let comparison = match op with
            | BEq | BNe | BLt | BLe | BGt | BGe -> true
            | _ -> false in
          if comparison then
            (* Self-determined: don't propagate. *)
            BBinOp { op; lhs = propagate 0 lhs;
                            rhs = propagate 0 rhs;
                            result_type }
          else
            let cur = match result_type with
              | BInt { width; _ } -> width | _ -> 0 in
            let target = max ctx_w cur in
            let result_type' =
              if target > cur
              then BInt { width = target; signed = Unsigned }
              else result_type in
            BBinOp { op;
                     lhs = propagate target lhs;
                     rhs = propagate target rhs;
                     result_type = result_type' }
      | BUnOp { op; operand; result_type } ->
          let reduction = match op with
            | BRedAnd | BRedOr | BRedXor -> true
            | _ -> false in
          if reduction then
            BUnOp { op; operand = propagate 0 operand; result_type }
          else
            let cur = match result_type with
              | BInt { width; _ } -> width | _ -> 0 in
            let target = max ctx_w cur in
            let result_type' =
              if target > cur
              then BInt { width = target; signed = Unsigned }
              else result_type in
            BUnOp { op; operand = propagate target operand;
                    result_type = result_type' }
      | BCond { condition; then_val; else_val } ->
          BCond {
            condition = propagate 0 condition;
            then_val  = propagate ctx_w then_val;
            else_val  = propagate ctx_w else_val;
          }
    in
    let rec walk_stmt = function
      | BAssign { lhs; rhs } ->
          BAssign { lhs; rhs = propagate (lhs_width lhs) rhs }
      | BIf { condition; then_stmts; else_stmts } ->
          BIf { condition = propagate 0 condition;
                then_stmts = List.map walk_stmt then_stmts;
                else_stmts = List.map walk_stmt else_stmts }
      | BCase { selector; cases; default } ->
          BCase { selector = propagate 0 selector;
                  cases = List.map (fun (k, ss) ->
                    (k, List.map walk_stmt ss)) cases;
                  default = List.map walk_stmt default }
      | BBlock ss -> BBlock (List.map walk_stmt ss)
      | BWhile { condition; body } ->
          BWhile { condition = propagate 0 condition;
                   body = List.map walk_stmt body }
      | BFor { init; condition; update; body } ->
          BFor { init = walk_stmt init;
                 condition = propagate 0 condition;
                 update = walk_stmt update;
                 body = List.map walk_stmt body }
      | other -> other
    in
    List.map (fun p ->
      match p with
      | BCombinational c ->
          BCombinational { c with body = List.map walk_stmt c.body }
      | BSequential s ->
          BSequential { s with body = List.map walk_stmt s.body }
    ) processes
  in
  let funcs =
    (* Extract every function_declaration before the strip pass
       erased its body — using the ORIGINAL mdecl_body we captured.
       Each function becomes a [bfunc] consumed by behavioral_inline.

       Heuristics over the function_declaration subtree:
         - return-type packed dim → ftype width
         - first unqualified_id with `function` keyword nearby → fname
         - input declarations under the function → params
         - statements not under input/output decls → body *)
    (* Package functions (dm::nop, dm::auipc, … — the pulp-debug instruction
       encoders) live in package bodies, not the module body, so they were
       never extracted into m.funcs and Behavioral_inline could not substitute
       them: dm_mem's abstract_cmd stayed a tree of opaque calls.  Also collect
       every package's function_declarations so a `pkg::func()` call (now named
       by its member) resolves and folds.  inline only substitutes the ones a
       module actually calls, so surplus package functions are harmless. *)
    let function_decls =
      collect_by (has_tag (fun t -> prefix_is "function_declaration" t)) mdecl_body
      @ List.concat_map (fun (p : Verible_elaborate.package_decl) ->
          collect_by (has_tag (fun t -> prefix_is "function_declaration" t))
            p.pkg_body) pkgs in
    List.filter_map (fun fdecl ->
      (* Function name: header has `function ... NAME ;`.  The first
         unqualified_id under the function_declaration whose parent
         isn't a packed dim, port decl, or statement is the name. *)
      let names = ref [] in
      (* skip packed-dim subtrees: `function [ADDR_WIDTH:0] bin2gray(...)`
         must not pick up ADDR_WIDTH (inside the return-type range) as the
         function name — the call site then never resolves and the call
         silently evaluates to 0 (async_fifo gray pointers stuck). *)
      let rec wnm t =
        match t with
        | TUPLE4 (STRING t', _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE6 (STRING t', _, _, _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE3 (STRING t', SymbolIdentifier id, _)
          when prefix_is "unqualified_id" t' -> names := id :: !names
        | SymbolIdentifier id -> names := id :: !names
        | TUPLE2 (a, b) -> wnm a; wnm b
        | TUPLE3 (a, b, c) -> wnm a; wnm b; wnm c
        | TUPLE4 (a, b, c, d) -> List.iter wnm [a; b; c; d]
        | TUPLE5 (a, b, c, d, e) -> List.iter wnm [a; b; c; d; e]
        | TUPLE6 (a, b, c, d, e, f) -> List.iter wnm [a; b; c; d; e; f]
        | TUPLE7 (a, b, c, d, e, f, g) -> List.iter wnm [a; b; c; d; e; f; g]
        | TUPLE8 (a, b, c, d, e, f, g, h) ->
            List.iter wnm [a; b; c; d; e; f; g; h]
        | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
            List.iter wnm [a; b; c; d; e; f; g; h; i]
        | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
            List.iter wnm [a; b; c; d; e; f; g; h; i; j]
        | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
            List.iter wnm [a; b; c; d; e; f; g; h; i; j; k]
        | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
            List.iter wnm [a; b; c; d; e; f; g; h; i; j; k; l]
        | TLIST xs -> List.iter wnm xs
        | _ -> ()
      in
      wnm fdecl;
      let fname =
        match List.rev !names with
        | [] -> ""
        | first :: _ -> first
      in
      if Sys.getenv_opt "SVS_FUNC_DEBUG" <> None then
        Printf.eprintf "[funcname] %s cands=[%s]\n%!" mdecl.m_name
          (String.concat ";" (List.rev !names));
      if fname = "" then None
      else
        let return_w = width_of ~pkgs ~params fdecl in
        let return_w = if return_w <= 0 then 1 else return_w in
        (* Input declarations: walk for `port_declaration_noattr`,
           `tf_port_declaration`, `tf_port_item`, or `module_port_declaration`
           tags.  SystemVerilog `function f (input X, Y, Z);` ports
           parse as `tf_port_item1` — the extractor used to miss
           these and produced 0-param functions, blocking inline. *)
        let port_nodes = collect_by (has_tag (fun t ->
          prefix_is "tf_port_declaration" t
          || prefix_is "tf_port_item" t
          || prefix_is "port_declaration_noattr" t
          || prefix_is "module_port_declaration" t)) fdecl in
        (* In SV `input logic [15:0] a, p, b, q;` the `[15:0]` precedes
           a list of names — and Verible attaches the type info only to
           the FIRST tf_port_item.  Subsequent items (like p, b, q) get
           a `type_identifier_or_implicit_basic_followed_by_id_and_dimensions_opt4`
           wrapper without the packed dim, so extract_port_decl reports
           them as 1-bit.  Sweep through and propagate the most-recently
           seen explicit width forward. *)
        let last_w = ref 1 in
        (* collect_by walks tf_port_list (a left-recursive TLIST) in
           reverse source order, which both (a) puts func_params in
           reverse order so a downstream BCall arg-position lookup
           hits the wrong formal — apply_temperature_delta(delta_q12,
           temp_q8_8) ended up reading as (temp_q8_8, delta_q12) — and
           (b) propagates `last_w` backwards across `input [15:0] a, p,
           b, q;` style decls.  Reverse port_nodes before iterating so
           both the formal order AND the width propagation match the
           source. *)
        let port_nodes = List.rev port_nodes in
        let func_params =
          List.concat_map (fun pn ->
            let signals = extract_port_decl ~pkgs ~params pn in
            List.filter_map (fun (s : bsignal) ->
              if s.name = fname then None  (* skip the function name itself *)
              else
                let w =
                  match s.stype with
                  | BInt { width; _ } when width > 1 ->
                      last_w := width; width
                  | BInt { width; _ } -> width
                  | _ -> 1
                in
                let w = if w = 1 then !last_w else w in
                Some (s.name, BInt { width = w; signed = Unsigned }, `Input)
            ) signals
          ) port_nodes in
        (* Formal ORDER must match the source: verible's list nesting
           differs between `input`-keyword and bare-ANSI arg styles, so
           the blanket List.rev above is right for one and WRONG for the
           other (rx_addr(b,w) parsed as (w,b): every 2-arg call swapped
           its arguments — scrambled RX buffer addresses on silicon).
           Reorder by first appearance in the header walk (wnm collects
           identifiers in true source order). *)
        let src_order = List.rev !names in
        let pos n =
          let rec go i = function
            | [] -> max_int
            | x :: tl -> if x = n then i else go (i + 1) tl
          in go 0 src_order in
        let func_params =
          List.stable_sort
            (fun (a, _, _) (b, _, _) -> compare (pos a) (pos b))
            func_params in
        (* Body: walk the function looking for statement nodes, but
           STOP at the outermost matching node — don't descend into
           it.  Otherwise a function with `if(c) x = a; else x = b;`
           collects both the conditional_statement AND its inner
           assignment_statements as sibling top-level body stmts.
           Same hazard for a function with multiple top-level
           assigns followed by an if: collect_by returns depth-first,
           so the inner assigns of the if come BEFORE the standalone
           assigns, and `top :: _` previously took the wrong one. *)
        let is_stmt_tag t =
          prefix_is "case_statement" t
          || prefix_is "conditional_statement" t
          || prefix_is "assignment_statement_no_expr" t
          || prefix_is "nonblocking_assignment" t
          || prefix_is "jump_statement" t   (* `return <expr>;` — single-return
                                               package funcs (dm_pkg encoders) *)
          || prefix_is "seq_block" t in
        let stmt_nodes =
          let acc = ref [] in
          let rec walk_outer t =
            if has_tag is_stmt_tag t then acc := t :: !acc
            else match t with
              | TUPLE2 (a, b) -> List.iter walk_outer [a; b]
              | TUPLE3 (a, b, c) -> List.iter walk_outer [a; b; c]
              | TUPLE4 (a, b, c, d) -> List.iter walk_outer [a; b; c; d]
              | TUPLE5 (a, b, c, d, e) -> List.iter walk_outer [a; b; c; d; e]
              | TUPLE6 (a, b, c, d, e, f) ->
                  List.iter walk_outer [a; b; c; d; e; f]
              | TUPLE7 (a, b, c, d, e, f, g) ->
                  List.iter walk_outer [a; b; c; d; e; f; g]
              | TUPLE8 (a, b, c, d, e, f, g, h) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h]
              | TUPLE9 (a, b, c, d, e, f, g, h, i) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i]
              | TUPLE10 (a, b, c, d, e, f, g, h, i, j) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i; j]
              | TUPLE11 (a, b, c, d, e, f, g, h, i, j, k) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i; j; k]
              | TUPLE12 (a, b, c, d, e, f, g, h, i, j, k, l) ->
                  List.iter walk_outer [a; b; c; d; e; f; g; h; i; j; k; l]
              | TLIST xs -> List.iter walk_outer xs
              | _ -> ()
          in
          walk_outer fdecl;
          List.rev !acc
        in
        (* Verible's parse tree returns top-level function body
           statements in REVERSE source order (cons-list build-up).
           Source order matters because behavioral_inline's body_to_expr
           treats the LAST statement as the fname-yielding one and
           substitutes earlier locals into it. *)
        let stmt_nodes = List.rev stmt_nodes in
        let body =
          match stmt_nodes with
          | [] -> []
          | [top] ->
              [stmt_to_bstmt ~pkgs ~params ~arrays:array_names top]
          | many ->
              [BBlock (List.map
                (fun n -> stmt_to_bstmt ~pkgs ~params ~arrays:array_names n)
                many)]
        in
        Some {
          fname;
          is_task = false;
          ftype = BInt { width = return_w; signed = Unsigned };
          params = func_params;
          locals = [];
          body;
        }
    ) function_decls
  in
  (* ── Nonzero-lsb packed-range normalisation ──────────────────────────
     Vivado post-synth netlists declare sliced-bus remnants like
     `wire [3:3] \^b ;` — width 1, base offset 3 — drive them WHOLE
     (`.O(\^b )`) and read them indexed (`\^b [3]`).  BInt keeps only the
     width, so the read would select bit 3 of a 1-bit net → constant 0
     (found by the Vivado↔SVS cross-flow miter: rand_1's `b` cone).
     Record each declared name's nonzero base offset and rebase indexed
     reads onto bit 0.  Gated on the table being non-empty — ordinary
     [N:0] designs are untouched. *)
  let range_offsets : (string, int) Hashtbl.t = Hashtbl.create 8 in
  let decl_nodes = collect_by (has_tag (fun t ->
    prefix_is "net_declaration" t || prefix_is "data_declaration" t))
    mdecl.m_body in
  List.iter (fun dn ->
    match extract_range ~pkgs ~params dn with
    | Some (m, l) when min m l <> 0 ->
        let off = min m l in
        let names = ref [] in
        walk (function
          | TUPLE3 (STRING t, SymbolIdentifier nm, _)
            when prefix_is "net_variable" t
                 || prefix_is "register_variable" t -> names := nm :: !names
          | TUPLE4 (STRING t, SymbolIdentifier nm, _, _)
            when prefix_is "net_decl_assign" t -> names := nm :: !names
          | _ -> ()) dn;
        List.iter (fun nm -> Hashtbl.replace range_offsets nm off) !names
    | _ -> ()) decl_nodes;
  let signals, processes, instances =
    if Hashtbl.length range_offsets = 0 then signals, processes, instances
    else begin
      let off_of x = Hashtbl.find_opt range_offsets x in
      let rec rb e = match e with
        | BSlice { signal = BVar x; msb; lsb } when off_of x <> None ->
            let o = Option.get (off_of x) in
            BSlice { signal = BVar x; msb = msb - o; lsb = lsb - o }
        | BSelect { array = BVar x; index = BConst { value; width } }
          when off_of x <> None ->
            let o = Option.get (off_of x) in
            BSelect { array = BVar x;
                      index = BConst { value = Z.sub value (Z.of_int o); width } }
        | BVar _ | BConst _ -> e
        | BBinOp r -> BBinOp { r with lhs = rb r.lhs; rhs = rb r.rhs }
        | BUnOp r -> BUnOp { r with operand = rb r.operand }
        | BSelect r -> BSelect { array = rb r.array; index = rb r.index }
        | BSlice r -> BSlice { r with signal = rb r.signal }
        | BConcat es -> BConcat (List.map rb es)
        | BReplicate r -> BReplicate { r with value = rb r.value }
        | BCond r -> BCond { condition = rb r.condition;
                             then_val = rb r.then_val; else_val = rb r.else_val }
        | BCall r -> BCall { r with args = List.map rb r.args } in
      let rec rbs s = match s with
        | BAssign r -> BAssign { r with rhs = rb r.rhs }
        | BIf r -> BIf { condition = rb r.condition;
                         then_stmts = List.map rbs r.then_stmts;
                         else_stmts = List.map rbs r.else_stmts }
        | BCase r -> BCase { selector = rb r.selector;
                             cases = List.map (fun (g, b) -> (rb g, List.map rbs b)) r.cases;
                             default = List.map rbs r.default }
        | BWhile r -> BWhile { condition = rb r.condition; body = List.map rbs r.body }
        | BFor r -> BFor { init = rbs r.init; condition = rb r.condition;
                           update = rbs r.update; body = List.map rbs r.body }
        | BBlock b -> BBlock (List.map rbs b)
        | BCallStmt r -> BCallStmt { r with args = List.map rb r.args }
        | BReturn eo -> BReturn (Option.map rb eo) in
      (signals,
       List.map (function
         | BCombinational r -> BCombinational { r with body = List.map rbs r.body }
         | BSequential r -> BSequential { r with body = List.map rbs r.body })
         processes,
       List.map (fun (i : binstance) ->
         { i with port_connections =
                    List.map (fun (p, e) -> (p, rb e)) i.port_connections })
         instances)
    end in
  (* ── Declared-`signed` recovery + sign-extension normalisation ───────
     `input signed wire4; reg [1:0] r; … r <= wire4;` must SIGN-extend
     (r = {wire4,wire4}); every decl site above hardcodes Unsigned, so the
     qualifier was lost and downstream encoders zero-extended (found by the
     Vivado↔SVS cross-flow miter on yosys dff_init).  Recover `signed` from
     the decl CST (any port/net/data decl node containing a Signed token),
     mark the signals, and normalise assignments whose RHS is a signed
     expression narrower than the LHS by MATERIALISING the extension at BIR
     level ({{n{x[msb]}},x} via Behavioral_const.sign_extend) — encoder-
     agnostic, so the Z3 miter AND the gate_map netlist are both fixed.
     Signed div/mod and signed comparisons are NOT yet handled (audit P1g). *)
  let signed_names : (string, unit) Hashtbl.t = Hashtbl.create 8 in
  let sdecl_nodes = collect_by (has_tag (fun t ->
    prefix_is "module_port_declaration" t || prefix_is "port_declaration" t
    || prefix_is "net_declaration" t || prefix_is "data_declaration" t
    || prefix_is "tf_port_declaration" t)) mdecl.m_body in
  List.iter (fun dn ->
    let has_signed = ref false in
    walk (function Signed -> has_signed := true | _ -> ()) dn;
    if !has_signed then begin
      let rec collect t = match t with
        | TUPLE6 (STRING t', _, _, _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE4 (STRING t', _, _, _)
          when prefix_is "decl_variable_dimension" t'
            || prefix_is "select_variable_dimension" t' -> ()
        | TUPLE4 (STRING t', SymbolIdentifier id, _, _)
          when prefix_is "net_decl_assign" t' ->
            Hashtbl.replace signed_names id ()
        | SymbolIdentifier id -> Hashtbl.replace signed_names id ()
        | TUPLE2 (a,b) -> collect a; collect b
        | TUPLE3 (a,b,c) -> collect a; collect b; collect c
        | TUPLE4 (a,b,c,d) -> collect a; collect b; collect c; collect d
        | TUPLE5 (a,b,c,d,e) -> List.iter collect [a;b;c;d;e]
        | TUPLE6 (a,b,c,d,e,f) -> List.iter collect [a;b;c;d;e;f]
        | TUPLE7 (a,b,c,d,e,f,g) -> List.iter collect [a;b;c;d;e;f;g]
        | TUPLE8 (a,b,c,d,e,f,g,h) -> List.iter collect [a;b;c;d;e;f;g;h]
        | TLIST xs -> List.iter collect xs
        | _ -> ()
      in
      collect dn
    end) sdecl_nodes;
  let signals, processes =
    if Hashtbl.length signed_names = 0 then signals, processes
    else begin
      let signals = List.map (fun (s : bsignal) ->
        if Hashtbl.mem signed_names s.name then
          match s.stype with
          | BInt { width; _ } -> { s with stype = BInt { width; signed = Signed } }
          | _ -> s
        else s) signals in
      let widths : (string, int) Hashtbl.t = Hashtbl.create 32 in
      List.iter (fun (s : bsignal) ->
        match s.stype with
        | BInt { width; _ } -> Hashtbl.replace widths s.name width
        | _ -> ()) signals;
      let is_signed_sig n =
        Hashtbl.mem signed_names n && Hashtbl.mem widths n in
      (* LRM signedness of an expression: signed var; arith/bitwise op of
         all-signed operands; ?: of signed branches.  Part-selects, concats,
         replications and comparisons are UNSIGNED. *)
      let rec expr_signed = function
        | BVar n -> is_signed_sig n
        | BBinOp { op = (BAdd|BSub|BMul|BAnd|BOr|BXor); lhs; rhs; _ } ->
            expr_signed lhs && expr_signed rhs
        | BUnOp { op = (BNot|BNeg); operand; _ } -> expr_signed operand
        | BCond { then_val; else_val; _ } ->
            expr_signed then_val && expr_signed else_val
        | BCall { func = "@signed"; _ } -> true
        | _ -> false in
      (* Push the assignment-context width INTO the expression (extending
         the RESULT of a narrower binop is not the same as extending its
         operands first — carries), sign-extending at the leaves. *)
      let rec widen to_w e = match e with
        | BBinOp ({ op = (BAdd|BSub|BMul|BAnd|BOr|BXor); _ } as r) ->
            BBinOp { r with lhs = widen to_w r.lhs; rhs = widen to_w r.rhs;
                     result_type = BInt { width = to_w; signed = Signed } }
        | BUnOp ({ op = (BNot|BNeg); _ } as r) ->
            BUnOp { r with operand = widen to_w r.operand;
                    result_type = BInt { width = to_w; signed = Signed } }
        | BCond r -> BCond { r with then_val = widen to_w r.then_val;
                                    else_val = widen to_w r.else_val }
        | _ ->
            (match Behavioral_const.width_of_expr_with widths e with
             | Some w when w < to_w && w > 0 ->
                 Behavioral_const.sign_extend ~from_w:w ~to_w e
             | _ -> e) in
      let fix_assign lhs rhs =
        match Hashtbl.find_opt widths lhs with
        | Some lw when expr_signed rhs ->
            (match Behavioral_const.width_of_expr_with widths rhs with
             | Some rw when rw < lw && rw > 0 -> Some (widen lw rhs)
             | _ -> None)
        | _ -> None in
      let rec fs s = match s with
        | BAssign r ->
            (match fix_assign r.lhs r.rhs with
             | Some rhs' -> BAssign { r with rhs = rhs' }
             | None -> s)
        | BIf r -> BIf { r with then_stmts = List.map fs r.then_stmts;
                                else_stmts = List.map fs r.else_stmts }
        | BCase r -> BCase { r with cases = List.map (fun (g,b) ->
                                      (g, List.map fs b)) r.cases;
                                    default = List.map fs r.default }
        | BWhile r -> BWhile { r with body = List.map fs r.body }
        | BFor r -> BFor { r with body = List.map fs r.body }
        | BBlock b -> BBlock (List.map fs b)
        | _ -> s in
      let processes = List.map (function
        | BCombinational r -> BCombinational { r with body = List.map fs r.body }
        | BSequential r -> BSequential { r with body = List.map fs r.body })
        processes in
      signals, processes
    end in
  let m =
    {
      name = mdecl.m_name;
      params = [];
      signals;
      processes;
      instances;
      funcs;
      mems = []; attrs = [];
    } in
  (* Scalarize by default (splits internal struct registers into per-field FFs
     to fix conditional field-write wire-conflicts; whole-struct references are
     reconstituted as a MSB-first concat of the fields).  Opt out with
     STRUCT_SCALARIZE=0 or NO_STRUCT_SCALARIZE. *)
  let m =
    if Sys.getenv_opt "STRUCT_SCALARIZE" = Some "0"
       || Sys.getenv_opt "NO_STRUCT_SCALARIZE" <> None then m
    else
      scalarize_module ~signal_struct:!cur_signal_struct
        ~struct_defs:!cur_struct_defs m
  in
  (* Also flatten constant-index combinational packed arrays (abstract_cmd
     ROM class) into per-element scalars, else meminfer treats them as RAMs
     with self-referential RMW and gate_map forms a combinational loop.
     Opt out with NO_ARRAY_SCALARIZE. *)
  let m =
    if Sys.getenv_opt "NO_ARRAY_SCALARIZE" <> None then m
    else scalarize_arrays_module m
  in
  (* Promote interface-port members to formals with modport directions.
   * scalarize_module split each interface port `p` (Internal, packed) into
   * per-member scalars `p$clk`, `p$data`, … (still Internal).  For port `if2.wr p`
   * the modport `wr` says clk=input, data=output — so `p$clk` becomes an input
   * formal and `p$data` an output formal, matching Vivado's flattened interface
   * ports.  Members not named by the modport stay Internal (unused).  A bare
   * `if2 p` (no modport) exposes every member as inout→input. *)
  if !cur_iface_ports = [] then m
  else begin
    let dir_of port field =
      let (_, iface, mp) =
        List.find (fun (pn, _, _) -> pn = port) !cur_iface_ports in
      match Hashtbl.find_opt iface_modports (iface ^ "$" ^ mp) with
      | Some entries ->
          (match List.assoc_opt field entries with
           | Some d -> Some (d :> [ `Input | `Output | `Internal ])
           | None -> None)                       (* not in modport → stay internal *)
      | None -> Some `Input   (* no modport: whole interface, expose as input *)
    in
    let ports = List.map (fun (pn, _, _) -> pn) !cur_iface_ports in
    let signals = List.map (fun (s : bsignal) ->
      match String.index_opt s.name '$' with
      | Some i when s.direction = `Internal ->
          let base = String.sub s.name 0 i in
          let field = String.sub s.name (i+1) (String.length s.name - i - 1) in
          if List.mem base ports then
            (match dir_of base field with
             | Some d -> { s with direction = d }
             | None -> s)
          else s
      | _ -> s) m.signals in
    { m with signals }
  end

(* ─── Top-level entry ────────────────────────────────────────────── *)

let discover_init_dirs files =
  let acc = ref [] in
  List.iter (fun f ->
    let dir = ref (Filename.dirname f) in
    let last = ref "" in
    while !dir <> !last && !dir <> "/" do
      let candidate = Filename.concat !dir "generated" in
      if Sys.file_exists candidate
         && (try Sys.is_directory candidate with _ -> false)
         && not (List.mem candidate !acc) then
        acc := candidate :: !acc;
      last := !dir;
      dir := Filename.dirname !dir
    done
  ) files;
  List.rev !acc

(* Parse a list of SV files via Verible, find the top module, and
 * convert it (and its specialised children) to BIR. *)

(* Interface-INSTANCE elaboration (top-level miters).  An interface instance
 * `if2 u(clk)` is a bundle of nets, not a child module: scalarize it into per-
 * member nets (u$clk, u$cnt, u$data), wire its header ports (u$clk := clk), and
 * fan out every connection that binds an interface PORT to it — `drv d(u)` →
 * `.p$clk(u$clk), .p$data(u$data)` (only the child's modport members).  Reads of
 * the bundle (`u.cnt`, sliced to `u[15:8]` by the struct machinery) are relocated
 * to the member scalars.  Runs BEFORE name_positional_ports and fully resolves
 * any interface-touching instance's connections (so no `$posN` is left for it).
 * A module with no interface instances and no interface-port child is untouched. *)
let elaborate_interfaces ~pkgs (bmods : bmodule list) : bmodule list =
  let is_iface_mod n = Hashtbl.mem iface_reg n in
  let is_pos k = String.length k > 4 && String.sub k 0 4 = "$pos" in
  let pos_idx k = int_of_string_opt (String.sub k 4 (String.length k - 4)) in
  (* Strip a specialise_design suffix (`axi_master_passthru__MIW4_IIW4` ->
   * `axi_master_passthru`): module_iface_ports/_insts are keyed by the BASE name
   * (from mdecl.m_name at convert time) but bmodules carry the specialised name. *)
  let base_name n =
    let len = String.length n in
    let rec find i =
      if i + 1 >= len then n
      else if n.[i] = '_' && n.[i+1] = '_' then String.sub n 0 i
      else find (i + 1) in
    find 0
  in
  (* MSB-first bit layout of an explicit member list: [(member, msb, lsb)] *)
  let layout_of members =
    let tw = List.fold_left (fun a (_, w) -> a + w) 0 members in
    let _, layout = List.fold_left (fun (pos, acc) (m, w) ->
      (pos + w, (m, tw - pos - 1, tw - pos - w) :: acc)) (0, []) members in
    List.rev layout
  in
  (* Member widths for an interface instance under its parameter overrides,
   * memoised.  `axi_if #(.ID_WIDTH(4)) lsu_if()` gets 4-bit id members even though
   * axi_if's default ID_WIDTH is 6 — re-convert the interface decl with the
   * instance's params (safe: post main convert-loop, interface has no children). *)
  let spec_members iface params =
    let key = iface ^ "#" ^ String.concat ","
        (List.map (fun (k, v) -> k ^ "=" ^ v) (List.sort compare params)) in
    match Hashtbl.find_opt iface_spec_members key with
    | Some m -> m
    | None ->
        let dflt () = Option.value ~default:[] (Hashtbl.find_opt iface_reg iface) in
        let m =
          if params = [] then dflt ()
          else match Hashtbl.find_opt iface_decls iface with
            | None -> dflt ()
            | Some mdecl ->
                (try
                   let im = convert_module ~pkgs mdecl params in
                   let sw = function
                     | BInt { width; _ } -> width | BBool -> 1
                     | BArray { element = BInt { width; _ }; size } -> width * size
                     | _ -> 0 in
                   let ms = List.filter_map (fun (s : bsignal) ->
                     let w = sw s.stype in if w > 0 then Some (s.name, w) else None)
                     im.signals in
                   if ms = [] then dflt () else ms
                 with _ -> dflt ()) in
        Hashtbl.replace iface_spec_members key m; m
  in
  (* per-parent: inst_name -> (iface, member-widths under the instance's params) *)
  let members_tbl_of (m : bmodule) =
    let tbl = Hashtbl.create 8 in
    List.iter (fun (nm, iface, params) ->
      Hashtbl.replace tbl nm (iface, spec_members iface params))
      (Option.value ~default:[] (Hashtbl.find_opt module_iface_insts (base_name m.name)));
    (* interface instances that surfaced only as a binstance: default params *)
    List.iter (fun (i : binstance) ->
      if is_iface_mod i.module_name && not (Hashtbl.mem tbl i.inst_name) then
        Hashtbl.replace tbl i.inst_name
          (i.module_name, Option.value ~default:[]
             (Hashtbl.find_opt iface_reg i.module_name))) m.instances;
    tbl
  in
  (* Source-order port SLOTS for a module (interface port collapsed to one slot). *)
  let slots_of (mm : bmodule) =
    let ifps = Option.value ~default:[]
        (Hashtbl.find_opt module_iface_ports (base_name mm.name)) in
    let member_base name =
      match String.index_opt name '$' with
      | Some i ->
          let base = String.sub name 0 i in
          (match List.assoc_opt base ifps with
           | Some iface -> Some (base, iface) | None -> None)
      | None -> None in
    let seen = Hashtbl.create 8 in
    (* m.signals is REVERSE source order; reverse it back to forward source order. *)
    List.rev (List.filter_map (fun (s : bsignal) ->
      match s.direction with
      | `Input | `Output ->
          (match member_base s.name with
           | Some (base, iface) ->
               if Hashtbl.mem seen base then None
               else (Hashtbl.replace seen base (); Some (base, Some iface))
           | None -> Some (s.name, None))
      | _ -> None) mm.signals)
  in
  let mk_by_name mods =
    let h = Hashtbl.create 64 in
    List.iter (fun (mm : bmodule) -> Hashtbl.replace h mm.name mm) mods; h in
  let by_name = mk_by_name bmods in
  (* PASS 1 — the connected bundle's member widths are authoritative; record the
   * width each child interface-PORT member must take so its port net matches the
   * bundle net it's wired to (a passthru's `m$awid` must be 4b when wired to a
   * 4-bit-ID bundle, though axi_if's default ID makes it 6b). *)
  let key2 a b = a ^ "\000" ^ b in
  let child_port_w : (string, int) Hashtbl.t = Hashtbl.create 128 in
  List.iter (fun (m : bmodule) ->
    let im = members_tbl_of m in
    if Hashtbl.length im > 0 then
      List.iter (fun (i : binstance) ->
        if not (is_iface_mod i.module_name) then
          match Hashtbl.find_opt by_name i.module_name with
          | None -> ()
          | Some child ->
              let slots = slots_of child in
              let n = List.length slots in
              List.iter (fun (k, be) ->
                let slot =
                  if is_pos k then
                    (match pos_idx k with Some idx when idx < n -> Some (List.nth slots idx) | _ -> None)
                  else List.find_opt (fun (nm, _) -> nm = k) slots in
                match slot, be with
                | Some (name, Some _), BVar u when Hashtbl.mem im u ->
                    let (_, members) = Hashtbl.find im u in
                    List.iter (fun (mem, w) ->
                      Hashtbl.replace child_port_w (key2 i.module_name (name ^ "$" ^ mem)) w)
                      members
                | _ -> ()) i.port_connections) m.instances) bmods;
  (* PASS 2 — resize child interface-port member signals to the connected width. *)
  let bmods = List.map (fun (m : bmodule) ->
    { m with signals = List.map (fun (s : bsignal) ->
        match Hashtbl.find_opt child_port_w (key2 m.name s.name) with
        | Some w -> { s with stype = BInt { width = w; signed = Unsigned } }
        | None -> s) m.signals }) bmods in
  let by_name = mk_by_name bmods in
  (* PASS 3 — scalarize interface instances, wire header ports, fan out. *)
  List.map (fun (m : bmodule) ->
    let members_tbl = members_tbl_of m in
    let inst_iface = Hashtbl.create 8 in
    Hashtbl.iter (fun u (iface, _) -> Hashtbl.replace inst_iface u iface) members_tbl;
    let members_of u =
      match Hashtbl.find_opt members_tbl u with Some (_, ms) -> ms | None -> [] in
    let child_iface_slots (i : binstance) =
      match Hashtbl.find_opt by_name i.module_name with
      | Some mm -> List.exists (fun (_, io) -> io <> None) (slots_of mm)
      | None -> false in
    let touches =
      Hashtbl.length inst_iface > 0
      || List.exists child_iface_slots m.instances in
    if not touches then m
    else begin
      (* Relocate references to an interface-instance bundle net onto its members. *)
      let rec re e =
        match e with
        | BVar u when Hashtbl.mem inst_iface u ->
            BConcat (List.map (fun (mem, _) -> BVar (u ^ "$" ^ mem)) (members_of u))
        | BSlice { signal = BVar u; msb; lsb } when Hashtbl.mem inst_iface u ->
            let parts = List.filter_map (fun (mem, fm, fl) ->
              let imsb = min fm msb and ilsb = max fl lsb in
              if imsb < ilsb then None
              else if imsb = fm && ilsb = fl then Some (BVar (u ^ "$" ^ mem))
              else Some (BSlice { signal = BVar (u ^ "$" ^ mem);
                                  msb = imsb - fl; lsb = ilsb - fl }))
              (layout_of (members_of u)) in
            (match parts with [] -> e | [x] -> x | xs -> BConcat xs)
        | BSlice { signal; msb; lsb } -> BSlice { signal = re signal; msb; lsb }
        | BBinOp r -> BBinOp { r with lhs = re r.lhs; rhs = re r.rhs }
        | BUnOp r -> BUnOp { r with operand = re r.operand }
        | BSelect r -> BSelect { array = re r.array; index = re r.index }
        | BConcat es -> BConcat (List.map re es)
        | BReplicate r -> BReplicate { r with value = re r.value }
        | BCond r -> BCond { condition = re r.condition;
                             then_val = re r.then_val; else_val = re r.else_val }
        | BCall r -> BCall { r with args = List.map re r.args }
        | (BVar _ | BConst _) as x -> x
      in
      let rec re_stmt s =
        match s with
        | BAssign { lhs; rhs } -> BAssign { lhs; rhs = re rhs }
        | BIf r -> BIf { condition = re r.condition;
                         then_stmts = List.map re_stmt r.then_stmts;
                         else_stmts = List.map re_stmt r.else_stmts }
        | BCase r -> BCase { selector = re r.selector;
                             cases = List.map (fun (e, ss) -> (re e, List.map re_stmt ss)) r.cases;
                             default = List.map re_stmt r.default }
        | BWhile r -> BWhile { condition = re r.condition; body = List.map re_stmt r.body }
        | BFor r -> BFor { init = re_stmt r.init; condition = re r.condition;
                           update = re_stmt r.update; body = List.map re_stmt r.body }
        | BBlock ss -> BBlock (List.map re_stmt ss)
        | BCallStmt r -> BCallStmt { r with args = List.map re r.args }
        | BReturn eo -> BReturn (Option.map re eo)
      in
      let re_proc = function
        | BCombinational r -> BCombinational { r with body = List.map re_stmt r.body }
        | BSequential r -> BSequential { r with body = List.map re_stmt r.body }
      in
      (* Member nets for every interface instance (per-instance widths). *)
      let new_sigs = Hashtbl.fold (fun u (_, members) acc ->
        List.map (fun (mem, w) ->
          { name = u ^ "$" ^ mem; stype = BInt { width = w; signed = Unsigned };
            direction = `Internal; initial_value = None; attrs = [] })
          members @ acc) members_tbl [] in
      (* Header-port connections of each interface instance -> assigns. *)
      let hdr_procs = ref [] in
      List.iter (fun (i : binstance) ->
        if is_iface_mod i.module_name then begin
          let hdrs = Option.value ~default:[]
              (Hashtbl.find_opt iface_hdr_ports i.module_name) in
          let hdr_names = List.map fst hdrs in
          List.iter (fun (k, be) ->
            let port =
              if is_pos k then
                (match pos_idx k with
                 | Some idx when idx < List.length hdr_names -> Some (List.nth hdr_names idx)
                 | _ -> None)
              else Some k in
            match port with
            | Some p ->
                (match List.assoc_opt p hdrs with
                 | Some `Input ->
                     hdr_procs := BCombinational
                       { name = i.inst_name ^ "_" ^ p; sensitivity = [BAny];
                         body = [BAssign { lhs = i.inst_name ^ "$" ^ p; rhs = re be }] }
                       :: !hdr_procs
                 | Some `Output ->
                     (match be with
                      | BVar tgt ->
                          hdr_procs := BCombinational
                            { name = i.inst_name ^ "_" ^ p; sensitivity = [BAny];
                              body = [BAssign { lhs = tgt; rhs = BVar (i.inst_name ^ "$" ^ p) }] }
                            :: !hdr_procs
                      | _ -> ())
                 | None -> ())
            | None -> ()) i.port_connections
        end) m.instances;
      (* Resolve non-interface instances, fanning out interface-port connections. *)
      let resolve (i : binstance) =
        if is_iface_mod i.module_name then None
        else
          match Hashtbl.find_opt by_name i.module_name with
          | None -> Some { i with port_connections =
                             List.map (fun (k, be) -> (k, re be)) i.port_connections }
          | Some child_mod ->
              let slots = slots_of child_mod in
              let n = List.length slots in
              let has_iface_slot = List.exists (fun (_, io) -> io <> None) slots in
              let refs_iface = List.exists (fun (_, be) ->
                match be with BVar u -> Hashtbl.mem inst_iface u | _ -> false)
                i.port_connections in
              if not has_iface_slot && not refs_iface then
                Some { i with port_connections =
                         List.map (fun (k, be) -> (k, re be)) i.port_connections }
              else begin
                let formals_scalar =
                  List.filter_map (fun (s : bsignal) ->
                    match s.direction with `Input | `Output -> Some s.name | _ -> None)
                    child_mod.signals in
                let conns = List.concat_map (fun (k, be) ->
                  let slot =
                    if is_pos k then
                      (match pos_idx k with
                       | Some idx when idx < n -> Some (List.nth slots idx)
                       | _ -> None)
                    else List.find_opt (fun (nm, _) -> nm = k) slots in
                  match slot with
                  | Some (name, None) -> [(name, re be)]
                  | Some (name, Some _iface) ->
                      (match be with
                       | BVar u when Hashtbl.mem inst_iface u ->
                           List.filter_map (fun (mem, _) ->
                             let formal = name ^ "$" ^ mem in
                             if List.mem formal formals_scalar
                             then Some (formal, BVar (u ^ "$" ^ mem)) else None)
                             (members_of u)
                       | _ -> [(name, re be)])
                  | None -> [(k, re be)]) i.port_connections in
                Some { i with port_connections = conns }
              end
      in
      let insts' = List.filter_map resolve m.instances in
      { m with
        signals = m.signals @ new_sigs;
        processes = List.map re_proc m.processes @ List.rev !hdr_procs;
        instances = insts' }
    end
  ) bmods

(* Normalisation pass: rewrite positional instance port connections
 * (captured by extract_instances under synthetic `$pos<N>` keys) into
 * named form, using each module's formal port order.  This runs once,
 * up front, BEFORE any destructive manipulation (Behavioral_hier
 * flattening, Behavioral_ffrip, Behavioral_share in prep_for_z3): those
 * passes substitute and rename by formal/actual name, so every
 * connection must already be named — a leftover `$posN` would lose the
 * connection (the child output never reaches the parent net).  Verilog
 * itself forbids mixing named and positional in one instance, so a
 * given instance's keys are uniformly `$posN` or uniformly named.
 *
 * Port order is taken from each (specialised) module's port signals in
 * declaration order.  A `$posN` that can't be resolved (child module
 * absent, or index past the port count) is left as-is and reported —
 * it then drops harmlessly in the flattener, but the warning flags a
 * real arity mismatch worth investigating. *)
let name_positional_ports (bmods : bmodule list) : bmodule list =
  let is_pos k = String.length k > 4 && String.sub k 0 4 = "$pos" in
  let port_order = Hashtbl.create 32 in
  List.iter (fun (m : bmodule) ->
    let ports = List.filter_map (fun (s : bsignal) ->
      match s.direction with
      | `Input | `Output -> Some s.name
      | _ -> None) m.signals in
    Hashtbl.replace port_order m.name ports) bmods;
  let is_dotstar k = k = "$dotstar" in
  let resolve_inst (i : Behavioral_ir.binstance) =
    let has_pos = List.exists (fun (k, _) -> is_pos k) i.port_connections in
    let has_ds  = List.exists (fun (k, _) -> is_dotstar k) i.port_connections in
    if not has_pos && not has_ds then i
    else
      let formals = Option.value ~default:[]
        (Hashtbl.find_opt port_order i.module_name) in
      let n = List.length formals in
      (* First resolve positionals; drop the $dotstar sentinel (expanded next). *)
      let pc = List.filter_map (fun (k, be) ->
        if is_dotstar k then None
        else if not (is_pos k) then Some (k, be)
        else match int_of_string_opt (String.sub k 4 (String.length k - 4)) with
          | Some idx when idx < n -> Some (List.nth formals idx, be)
          | _ ->
              Printf.eprintf
                "[verible_to_bir] %s: positional port %s of %s unresolved \
                 (%d formals)\n" i.inst_name k i.module_name n;
              Some (k, be)) i.port_connections in
      (* `.*`: connect every formal the instance did not name explicitly to a
       * parent net of the same name (SV-2017 implicit port connection). *)
      let pc =
        if not has_ds then pc
        else if formals = [] then begin
          Printf.eprintf
            "[verible_to_bir] %s: `.*` on %s but no formal port order known\n"
            i.inst_name i.module_name;
          pc
        end else
          let connected = List.map fst pc in
          let extra = List.filter_map (fun f ->
            if List.mem f connected then None else Some (f, BVar f)) formals in
          pc @ extra
      in
      { i with port_connections = pc }
  in
  List.map (fun (m : bmodule) ->
    { m with instances = List.map resolve_inst m.instances }) bmods

let convert_files_inner ~top ?(top_params : (string * string) list = []) files : bprogram =
  mem_init_search_paths := discover_init_dirs files;
  if Sys.getenv_opt "MEM_INIT_DEBUG" <> None && !mem_init_search_paths <> [] then
    Printf.eprintf "[mem_init] auto-discovered: %s\n"
      (String.concat ", " !mem_init_search_paths);
  let mods, pkgs = parse_files_full files in
  (* Register every SystemVerilog interface as a packed struct: convert it like a
   * module (its nets/ports become signals), then record its members in iface_reg
   * and its total width in type_widths.  This lets interface ports (if.modport p)
   * be treated as struct-typed ports and member access (p.field) slice correctly. *)
  List.iter (fun (mdecl : module_decl) ->
    let is_iface = ref false in
    walk (function Interface -> is_iface := true | _ -> ()) mdecl.m_body;
    if !is_iface then
      try
        let im = convert_module ~pkgs mdecl [] in
        let stype_w = function
          | BInt { width; _ } -> width | BBool -> 1
          | BArray { element = BInt { width; _ }; size } -> width * size
          | _ -> 0 in
        let members = List.filter_map (fun (s : bsignal) ->
          let w = stype_w s.stype in
          if w > 0 then Some (s.name, w) else None) im.signals in
        if members <> [] then begin
          Hashtbl.replace iface_reg mdecl.m_name members;
          Hashtbl.replace iface_decls mdecl.m_name mdecl;
          let total = List.fold_left (fun a (_, w) -> a + w) 0 members in
          Hashtbl.replace type_widths mdecl.m_name total;
          (* Header ports of the interface itself (`interface if2(input clk)`)
           * — the Input/Output signals of the converted interface module. *)
          let hdr = List.filter_map (fun (s : bsignal) ->
            match s.direction with
            | `Input -> Some (s.name, `Input)
            | `Output -> Some (s.name, `Output)
            | _ -> None) im.signals in
          Hashtbl.replace iface_hdr_ports mdecl.m_name hdr;
          (* Parse modports: `modport wr(input clk, output data)`.  A single
           * IN-ORDER (pre-order DFS) walk of each modport_item yields the flat
           * leaf sequence [name; input; clk; output; data]: the first id is the
           * modport name, then each id inherits the most-recent direction leaf.
           * (Do NOT `collect_by` on generic prefixes — `prefix_is "modport_item"`
           * also matches `modport_item_list`, double-counting every member.) *)
          let modport_nodes =
            collect_by (has_tag (fun t ->
              prefix_is "modport_item" t
              && not (prefix_is "modport_item_list" t))) mdecl.m_body in
          List.iter (fun mi ->
            let mp_name = ref None in
            let cur_dir = ref `Input in
            let entries = ref [] in
            walk (function
              | Input -> cur_dir := `Input
              | Output -> cur_dir := `Output
              | Inout -> cur_dir := `Input
              | SymbolIdentifier id ->
                  if !mp_name = None then mp_name := Some id
                  else entries := (id, !cur_dir) :: !entries
              | _ -> ()) mi;
            let entries = List.rev !entries in
            match !mp_name with
            | Some mp when entries <> [] ->
                (if Sys.getenv_opt "IFACE_DEBUG" <> None then
                   Printf.eprintf "[iface] modport %s$%s: %s\n%!" mdecl.m_name mp
                     (String.concat "," (List.map (fun (n,d) ->
                        n ^ ":" ^ (match d with `Input -> "in" | `Output -> "out")) entries)));
                Hashtbl.replace iface_modports (mdecl.m_name ^ "$" ^ mp) entries
            | _ -> ()) modport_nodes
        end
      with _ -> ()) mods;
  (* Augment each package's params with its enum members so `pkg::Member`
   * references resolve via eval_int's package fallback (resolve_pkg_ref).
   * extract_enum_items previously ran only on MODULE bodies, so a package-
   * scoped state enum — `dm_pkg`'s `typedef enum {Idle,Read,…} sba_state_e`
   * used as `dm::Idle` in dm_sba — was unresolved, fell back to a bare
   * BVar "Idle" tied to zero, and collapsed the FSM into a combinational
   * loop. *)
  let pkgs =
    List.map (fun (p : Verible_elaborate.package_decl) ->
      let items = extract_enum_items ~pkgs ~params:[] p.pkg_body in
      let enum_params =
        List.filter_map (fun (nm, vstr) ->
          match int_of_string_opt vstr with
          | Some n -> Some (nm, Verible_elaborate.PInt n)
          | None -> None) items in
      if enum_params = [] then p
      else { p with pkg_params = p.pkg_params @ enum_params }) pkgs in
  let by_name = Hashtbl.create 32 in
  List.iter (fun m -> Hashtbl.replace by_name m.m_name m) mods;
  let specs = specialise_design ~pkgs ~top_params mods ~top_name:top in
  (* Override the TOP module's parameters (e.g. compare ibex_counter as the
     CounterWidth=64 instance Vivado synthesised, not the default 32).  A
     standalone top has no instantiation site, so specialise_design gives it its
     defaults; the caller can pin the real values here. *)
  let specs =
    if top_params = [] then specs
    else List.map (fun (s : specialised) ->
      if s.s_name = top || s.s_base = top then
        { s with s_params = top_params
                 @ List.filter (fun (n, _) -> not (List.mem_assoc n top_params)) s.s_params }
      else s) specs in
  let bmods = List.filter_map (fun (s : specialised) ->
    match Hashtbl.find_opt by_name s.s_base with
    | None ->
        Printf.eprintf "[verible_to_bir] no source module for base %s\n"
          s.s_base;
        None
    | Some mdecl ->
        let m = convert_module ~pkgs mdecl s.s_params in
        (* Rewrite each instance's module_name from the BASE name (which
         * extract_instances records) to the SPECIALISED sibling that
         * specialise_design picked for this exact instance site. Without
         * this, Behavioral_flatten would pick an arbitrary popcount__W*
         * for an internal `popcount` instance — usually the wrong one. *)
        let rewritten = List.filter_map (fun (i : Behavioral_ir.binstance) ->
          let key = (s.s_name, i.inst_name) in
          match Hashtbl.find_opt
                  Verible_elaborate.inst_specialised key with
          | Some specname -> Some { i with module_name = specname }
          | None ->
              (* No specialise_design entry → an external cell (Liberty cell or
               * vendor primitive with no SV body in `files`).  Always KEEP it:
               * silently dropping externals was the confusing `keep_external`
               * behaviour that made clock/GT/RAM primitives vanish from the
               * netlist.  A genuinely-unknown name (dead generate branch, a
               * mis-classified $clog2 local) surfaces LOUDLY downstream — the
               * gate_map port-direction check reports any cell it can't resolve
               * to a primitive interface — instead of being hidden here. *)
              Some i
        ) m.instances in
        Some { m with name = s.s_name; instances = rewritten }
  ) specs in
  (* Fallback specialisation: an instance can reference a PARSED user module
   * that specialise_design never specialised — e.g. the LIVE branch of a
   * conditional generate when walk_live's condition evaluation diverges from
   * the module body's.  dm_mem's `if (HasSndScratch) debug_rom else
   * debug_rom_one_scratch` keeps `debug_rom_one_scratch` in the body (default
   * DmBaseAddress='0 → HasSndScratch=0), but walk_live collected the other
   * branch, so debug_rom_one_scratch got no bmodule and gate_map's
   * port-direction check bombs on it.  Convert any still-missing referenced
   * module with DEFAULT params (parameterless leaves like ROMs are the common
   * case) so its interface exists downstream.  Iterated to a fixpoint so a
   * fallback module's own missing children are covered too. *)
  let bmods =
    let acc = ref bmods in
    let changed = ref true in
    while !changed do
      changed := false;
      let have = Hashtbl.create 128 in
      List.iter (fun (m : bmodule) -> Hashtbl.replace have m.name ()) !acc;
      let missing = Hashtbl.create 16 in
      List.iter (fun (m : bmodule) ->
        List.iter (fun (i : Behavioral_ir.binstance) ->
          if not (Hashtbl.mem have i.module_name)
             && Hashtbl.mem by_name i.module_name
             && not (Hashtbl.mem missing i.module_name)
          then Hashtbl.replace missing i.module_name ()) m.instances) !acc;
      Hashtbl.iter (fun name () ->
        match Hashtbl.find_opt by_name name with
        | Some mdecl ->
            (try
               let m = convert_module ~pkgs mdecl [] in
               acc := !acc @ [m]; changed := true;
               Printf.eprintf
                 "[verible_to_bir] fallback-specialised %s (default params) — \
                  referenced but not in specialise_design output\n" name
             with e ->
               Printf.eprintf
                 "[verible_to_bir] fallback specialise %s failed: %s\n"
                 name (Printexc.to_string e))
        | None -> ()) missing
    done;
    !acc
  in
  (* Elaborate interface instances (bundle nets + header wiring + interface-port
   * connection fan-out) before positional resolution. *)
  let bmods = elaborate_interfaces ~pkgs bmods in
  (* Positional → named port connections, before prep_for_z3's
   * destructive flatten/ffrip/share. *)
  let bmods = name_positional_ports bmods in
  (* Task #36: resolve any cell-type that has no user-supplied
   * bmodule against Vivado's per-primitive VHDL stubs via the
   * existing VHDL frontend (lookup_xil_primitive_ports).
   * Resolved ports land in bprogram.library_cells; unresolved
   * names are left silent here because keep_external already
   * warned on them upstream. *)
  let user_names =
    List.fold_left (fun acc (m : bmodule) -> m.name :: acc) [] bmods in
  let unresolved =
    List.fold_left (fun acc (m : bmodule) ->
      List.fold_left (fun acc (i : Behavioral_ir.binstance) ->
        if List.mem i.module_name user_names
           || List.mem i.module_name acc
        then acc
        else i.module_name :: acc) acc m.instances) [] bmods in
  let lib_cells =
    Vhdl_to_behavioral.lookup_xil_primitive_ports unresolved in
  let prog = { modules = bmods; library_cells = lib_cells } in
  (* CROSS-MODULE PACK PROPAGATION.  The per-module rule (a whole-array write
     or copy forces PACKED) is inconsistent across a port boundary: `imd_val`
     is copied in ibex_alu -> packed there, but the connecting wire in ibex_core
     was never whole-written -> stayed unpacked -> a packed<->unpacked port
     mismatch.  Union arrays linked by a WHOLE-array port connection
     (`.port(BVar wire)`), and if ANY member of a group is already PACKED
     (source-packed 2-D, or un-tagged by the whole-write rule), pack the whole
     group -> drop "unpacked" from every member.  Only fully-isolated per-slot
     arrays (device_rdata + its read-only port) stay unpacked. *)
  let prog =
    let mods = prog.modules in
    let modtbl : (string, bmodule) Hashtbl.t = Hashtbl.create 64 in
    List.iter (fun (m : bmodule) -> Hashtbl.replace modtbl m.name m) mods;
    let parent : (string * string, string * string) Hashtbl.t = Hashtbl.create 256 in
    let ensure x = if not (Hashtbl.mem parent x) then Hashtbl.replace parent x x in
    let rec find x =
      ensure x;
      match Hashtbl.find_opt parent x with
      | Some p when p <> x -> let r = find p in Hashtbl.replace parent x r; r
      | _ -> x in
    let union a b = let ra = find a and rb = find b in
      if ra <> rb then Hashtbl.replace parent ra rb in
    let is_arr mn sn = match Hashtbl.find_opt modtbl mn with
      | Some m -> List.exists (fun (s : bsignal) ->
          s.name = sn && (match s.stype with BArray _ -> true | _ -> false)) m.signals
      | None -> false in
    List.iter (fun (caller : bmodule) ->
      List.iter (fun (i : binstance) ->
        List.iter (fun (port, e) ->
          match e with
          | BVar wire when is_arr caller.name wire && is_arr i.module_name port ->
              union (i.module_name, port) (caller.name, wire)
          | _ -> ()) i.port_connections) caller.instances) mods;
    let must_pack : (string * string, unit) Hashtbl.t = Hashtbl.create 64 in
    List.iter (fun (m : bmodule) ->
      List.iter (fun (s : bsignal) ->
        match s.stype with
        | BArray _ when not (List.mem_assoc "unpacked" s.attrs) ->
            Hashtbl.replace must_pack (find (m.name, s.name)) ()
        | _ -> ()) m.signals) mods;
    let mods = List.map (fun (m : bmodule) ->
      { m with signals = List.map (fun (s : bsignal) ->
          match s.stype with
          | BArray _ when List.mem_assoc "unpacked" s.attrs
                          && Hashtbl.mem must_pack (find (m.name, s.name)) ->
              { s with attrs = List.remove_assoc "unpacked" s.attrs }
          | _ -> s) m.signals }) mods in
    { prog with modules = mods } in
  (* Stamp `(* sv_decomp_* *)` attributes from each source file's
   * pre-scan. Sv_attr_extract is a regex-based side pass that runs
   * on the raw SV text — Verible's parse-tree carries attributes in
   * tag layouts that vary per declaration kind, and threading them
   * through the converter cleanly would touch every extract_*
   * helper. The side pass is sufficient for module / signal /
   * port-level attributes, which is where sv_decomp_adder /
   * sv_decomp_mul attach in practice. *)
  let attr_tables = List.map Sv_attr_extract.extract_file files in
  List.fold_left (fun p tbl -> Sv_attr_extract.stamp_program tbl p)
    prog attr_tables

let convert_files ~top ?(top_params : (string * string) list = []) files : bprogram =
  convert_files_inner ~top ~top_params files

(* Retained as an alias: external (unmatched) instances are ALWAYS kept now, so
 * this behaves identically to convert_files.  Kept so existing callers (gate-
 * level netlist readers) don't break. *)
let convert_files_with_externals ~top files : bprogram =
  convert_files_inner ~top files

(* No-top "read everything" convenience: parse the input files, emit one
 * bmodule per declared module at its body-declared default parameters,
 * and skip the top-down specialise_design walk entirely.  Useful when
 * the caller (e.g. the GUI) wants to inspect a file before deciding
 * which module to elaborate as top.  Param-dependent specialisations
 * (popcount__W8 vs popcount__W16) are NOT generated here — they require
 * a known instantiation site and so live downstream of elaboration.   *)
let convert_files_all files : bprogram =
  mem_init_search_paths := discover_init_dirs files;
  let mods, pkgs = parse_files_full files in
  let bmods = List.map (fun (mdecl : module_decl) ->
    let m = convert_module ~pkgs mdecl [] in
    { m with name = mdecl.m_name }
  ) mods in
  (* Task #36 — same primitive-lookup as convert_files_inner. *)
  let user_names =
    List.fold_left (fun acc (m : bmodule) -> m.name :: acc) [] bmods in
  let unresolved =
    List.fold_left (fun acc (m : bmodule) ->
      List.fold_left (fun acc (i : Behavioral_ir.binstance) ->
        if List.mem i.module_name user_names
           || List.mem i.module_name acc
        then acc
        else i.module_name :: acc) acc m.instances) [] bmods in
  let lib_cells =
    Vhdl_to_behavioral.lookup_xil_primitive_ports unresolved in
  let prog = { modules = bmods; library_cells = lib_cells } in
  let attr_tables = List.map Sv_attr_extract.extract_file files in
  List.fold_left (fun p tbl -> Sv_attr_extract.stamp_program tbl p)
    prog attr_tables
