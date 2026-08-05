(* Hierarchical encode helper.
 *
 * Produces a transient flat bmodule by inlining every reachable
 * `binstance` in a top-level module. Used only at Z3-encode time —
 * the source bprogram is left intact, so module boundaries remain
 * available to downstream tools (timing, layout, hierarchical
 * verification certificates).
 *
 * Compared to Behavioral_flatten.flatten_program (which implements
 * specialisation-aware flattening for the cva6 sweep), this is a
 * straight recursive inliner with name prefixing — purpose-built for
 * the Z3 miter and intentionally simple. *)

open Behavioral_ir

let pname prefix name =
  if prefix = "" then name else prefix ^ "__" ^ name

(* Parse a Verilog literal ("W'bBITS" / "W'hHEX" / "W'dN" / plain int) into a
   CONSTANT bexpr.  A value wider than an OCaml int (e.g. a 64-bit LUT INIT) is
   split into <=31-bit BConst chunks under an MSB-first BConcat, so the encoder
   (which slices a BConcat as extract-of-concat) resolves any part-select of it.
   Used to substitute module PARAMETER values into an inlined child body — an
   unsubstituted wide param would otherwise encode as a free Z3 variable. *)
let const_bexpr_of_verilog (s : string) : bexpr option =
  let s = String.trim s in
  match String.index_opt s '\'' with
  | None -> (try Some (BConst { value = Z.of_string s; width = 32 }) with _ -> None)
  | Some ap ->
    let width = try int_of_string (String.trim (String.sub s 0 ap)) with _ -> 0 in
    if width <= 0 then None else begin
      let rest = String.sub s (ap + 1) (String.length s - ap - 1) in
      let base = if String.length rest > 0 then Char.lowercase_ascii rest.[0] else 'b' in
      let digits = if String.length rest > 1 then String.sub rest 1 (String.length rest - 1) else "" in
      let digits = String.concat "" (String.split_on_char '_' digits) in
      let bits =
        match base with
        | 'b' -> digits
        | 'h' ->
          let buf = Buffer.create (String.length digits * 4) in
          String.iter (fun c ->
            let v = match c with
              | '0'..'9' -> Char.code c - Char.code '0'
              | 'a'..'f' -> Char.code c - Char.code 'a' + 10
              | _ -> 0 in
            Buffer.add_string buf (Printf.sprintf "%d%d%d%d"
              ((v lsr 3) land 1) ((v lsr 2) land 1) ((v lsr 1) land 1) (v land 1))) digits;
          Buffer.contents buf
        | 'd' ->
          (try let v = int_of_string digits in
             String.init width (fun i -> if (v lsr (width - 1 - i)) land 1 = 1 then '1' else '0')
           with _ -> "")
        | _ -> "" in
      if bits = "" then None else begin
        (* normalise to exactly `width` bits, MSB-first *)
        let n = String.length bits in
        let bits = if n >= width then String.sub bits (n - width) width
                   else String.make (width - n) '0' ^ bits in
        let rec chunks i acc =
          if i >= width then List.rev acc
          else
            let len = min 31 (width - i) in
            let v = int_of_string ("0b" ^ String.sub bits i len) in
            chunks (i + len) (BConst { value = Z.of_int v; width = len } :: acc) in
        match chunks 0 [] with
        | [ c ] -> Some c
        | cs -> Some (BConcat cs)
      end
    end

let bit_width_of_int v = if v <= 1 then 1 else
  let rec f n = if v lsr n = 0 then n else f (n + 1) in f 0

(* Walk an expression. Substitution wins over prefixing: if a BVar's
 * name is in `subst`, replace with that bexpr verbatim (its sub-
 * expressions stay in *parent* scope, so we don't recurse into them).
 * Otherwise, prefix the BVar name with the current scope. *)
let rec sub_bexpr ~subst ~prefix = function
  | BVar n ->
      (match List.assoc_opt n subst with
       | Some e -> e
       | None -> BVar (pname prefix n))
  | BConst _ as e -> e
  | BBinOp { op; lhs; rhs; result_type } ->
      BBinOp { op;
               lhs = sub_bexpr ~subst ~prefix lhs;
               rhs = sub_bexpr ~subst ~prefix rhs;
               result_type }
  | BUnOp { op; operand; result_type } ->
      BUnOp { op;
              operand = sub_bexpr ~subst ~prefix operand;
              result_type }
  | BSelect { array; index } ->
      BSelect { array = sub_bexpr ~subst ~prefix array;
                index = sub_bexpr ~subst ~prefix index }
  | BSlice { signal; msb; lsb } ->
      BSlice { signal = sub_bexpr ~subst ~prefix signal; msb; lsb }
  | BConcat es -> BConcat (List.map (sub_bexpr ~subst ~prefix) es)
  | BReplicate { count; value } ->
      BReplicate { count; value = sub_bexpr ~subst ~prefix value }
  | BCond { condition; then_val; else_val } ->
      BCond { condition  = sub_bexpr ~subst ~prefix condition;
              then_val   = sub_bexpr ~subst ~prefix then_val;
              else_val   = sub_bexpr ~subst ~prefix else_val }
  | BCall { func; args } ->
      BCall { func; args = List.map (sub_bexpr ~subst ~prefix) args }

(* Walk a statement. lhs_subst maps `formal` → `parent_actual_name` for
 * the child's output ports — when the child writes to its formal-out,
 * the inlined assignment writes to the parent's actual wire instead,
 * skipping the prefix. *)
let rec sub_bstmt ~subst ~lhs_subst ~prefix = function
  | BAssign { lhs; rhs } ->
      let new_lhs =
        match List.assoc_opt lhs lhs_subst with
        | Some n -> n
        | None -> pname prefix lhs in
      BAssign { lhs = new_lhs; rhs = sub_bexpr ~subst ~prefix rhs }
  | BIf { condition; then_stmts; else_stmts } ->
      BIf { condition  = sub_bexpr ~subst ~prefix condition;
            then_stmts = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  then_stmts;
            else_stmts = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  else_stmts }
  | BCase { selector; cases; default } ->
      BCase { selector = sub_bexpr ~subst ~prefix selector;
              cases    = List.map (fun (v, body) ->
                (sub_bexpr ~subst ~prefix v,
                 List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body))
                cases;
              default  = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  default }
  | BWhile { condition; body } ->
      BWhile { condition = sub_bexpr ~subst ~prefix condition;
               body      = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                    body }
  | BFor { init; condition; update; body } ->
      BFor { init      = sub_bstmt ~subst ~lhs_subst ~prefix init;
             condition = sub_bexpr ~subst ~prefix condition;
             update    = sub_bstmt ~subst ~lhs_subst ~prefix update;
             body      = List.map (sub_bstmt ~subst ~lhs_subst ~prefix)
                                  body }
  | BBlock body ->
      BBlock (List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body)
  | BCallStmt { func; args } ->
      BCallStmt { func; args = List.map (sub_bexpr ~subst ~prefix) args }
  | BReturn None -> BReturn None
  | BReturn (Some e) -> BReturn (Some (sub_bexpr ~subst ~prefix e))

(* Resolve a bare signal NAME (clock / reset / sensitivity) through the
   port connections: an input port wired to a parent net [BVar pn] becomes
   [pn], an output port becomes its lhs_subst actual, otherwise it's an
   internal signal and gets the instance prefix.  Without this, an inlined
   module's `@posedge clk` becomes `@posedge inst__clk` with no driver. *)
let sub_name ~subst ~lhs_subst ~prefix n =
  match List.assoc_opt n subst with
  | Some (BVar pn) -> pn
  | Some _ -> pname prefix n
  | None ->
    (match List.assoc_opt n lhs_subst with
     | Some pn -> pn
     | None -> pname prefix n)

let sub_process ~subst ~lhs_subst ~prefix = function
  | BCombinational { name; sensitivity; body } ->
      BCombinational {
        name = pname prefix name;
        sensitivity = List.map (function
          | BPosEdge n -> BPosEdge (sub_name ~subst ~lhs_subst ~prefix n)
          | BNegEdge n -> BNegEdge (sub_name ~subst ~lhs_subst ~prefix n)
          | BLevel   n -> BLevel   (sub_name ~subst ~lhs_subst ~prefix n)
          | BAny       -> BAny
        ) sensitivity;
        body = List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body }
  | BSequential { name; clock; clock_edge; blocking_vars;
                  reset; reset_edge; reset_async; body } ->
      BSequential {
        name        = pname prefix name;
        clock       = sub_name ~subst ~lhs_subst ~prefix clock;
        clock_edge;
        reset       = (match reset with
                       | None -> None
                       | Some n -> Some (sub_name ~subst ~lhs_subst ~prefix n));
        reset_edge;
        reset_async;
        body        = List.map (sub_bstmt ~subst ~lhs_subst ~prefix) body;
        (* Rename blocking_vars in lockstep with the body's LHS rewrites so
         * `is_blocking` checks downstream match the flattened names
         * (e.g. picorv32's `current_pc` -> `cpu__current_pc`).  *)
        blocking_vars = List.map
          (sub_name ~subst ~lhs_subst ~prefix) blocking_vars }

(* Inline `child` (already flattened) into `parent` at instance `inst`.
 * Returns a new bmodule with child's signals/processes added under the
 * `<inst.inst_name>__` prefix, port pins routed via subst/lhs_subst. *)
let inline_instance ~debug (parent : bmodule) (inst : binstance)
                    (child : bmodule) : bmodule =
  let prefix = inst.inst_name in
  (* Classify each child port pin by direction. *)
  let port_dirs = List.filter_map (fun (s : bsignal) ->
    match s.direction with
    | `Input  -> Some (s.name, `Input)
    | `Output -> Some (s.name, `Output)
    | _ -> None
  ) child.signals in
  (* subst routes READS of a child port to its parent net.  Inputs map to
     the connected actual; OUTPUTS that are read internally (mem_addr,
     pcpi_*, …) must also map to their parent net — otherwise an internal
     read becomes inst__port (prefixed, undriven) while the driver uses the
     parent net via lhs_subst, splitting them. *)
  let port_subst = List.filter_map (fun (formal, actual) ->
    match List.assoc_opt formal port_dirs, actual with
    | Some `Input, _ -> Some (formal, actual)
    | Some `Output, BVar _ -> Some (formal, actual)
    | _ -> None
  ) inst.port_connections in
  (* PARAMETER substitution: a child body reference to a module parameter
     (e.g. a LUT's 64-bit INIT) must become its CONSTANT value, else the Z3
     encoder mints it as a free variable.  Wide values go through
     const_bexpr_of_verilog (BConcat of 31-bit chunks).  Ports win over params
     if a name somehow collides. *)
  let param_subst =
    let from_strs = List.filter_map (fun (name, s) ->
      match const_bexpr_of_verilog s with Some e -> Some (name, e) | None -> None)
      inst.param_strs in
    let from_vals = List.filter_map (fun (name, v) ->
      if List.mem_assoc name from_strs then None
      else Some (name, BConst { value = Z.of_int v; width = bit_width_of_int v }))
      inst.param_values in
    from_strs @ from_vals in
  let subst = port_subst @
    List.filter (fun (n, _) -> not (List.mem_assoc n port_subst)) param_subst in
  let lhs_subst = List.filter_map (fun (formal, actual) ->
    match List.assoc_opt formal port_dirs, actual with
    | Some `Output, BVar n -> Some (formal, n)
    | Some `Output, (BSlice { signal = BVar _; _ } | BSelect { array = BVar _; _ }
                    | BConcat _) ->
        (* handled below by slice_procs / bit_assigns / fanout_procs *)
        None
    | Some `Output, _ ->
        (* Anything else is a DROPPED DRIVER: the parent net is left read but
           never written, which Z3 mints as a free variable and a miter then
           reports DIFFER against an identical design.  Never silent. *)
        Printf.eprintf
          "[hier] WARNING: %s.%s drives an unsupported actual shape -- driver \
DROPPED, its net will be free\n" inst.inst_name formal;
        flush stderr;
        None
    | _ -> None
  ) inst.port_connections in
  (* Promote child's internal signals to parent-scope with prefix. Skip
   * port pins — their names already resolve via subst / lhs_subst. *)
  (* Promote every child signal (incl. port pins) under the prefix as an
     internal net.  Connected ports are remapped to parent nets via
     subst/lhs_subst (leaving the promoted copy dead), but UNconnected or
     conditionally-undriven pins (e.g. external pcpi/trace when those
     features are off) stay declared so create_circuit ties them to 0 at
     the right width instead of leaving a dangling reference. *)
  let new_signals =
    List.fold_left (fun acc (s : bsignal) ->
      { s with name = pname prefix s.name; direction = `Internal } :: acc)
      parent.signals child.signals
  in
  let new_processes =
    List.fold_left (fun acc proc ->
      sub_process ~subst ~lhs_subst ~prefix proc :: acc
    ) parent.processes child.processes in
  (* An OUTPUT port wired to a CONCAT of 1-bit nets — e.g. a CARRY4's
     `.O({_n_4,_n_3,_n_2,_n_1})` / `.CO(...)` — can't go through lhs_subst (which
     maps a formal to a single parent net).  The child writes the whole port
     `<inst>__<formal>`; fan it out bit-wise to the concat's nets (MSB-first) so
     they aren't left undriven.  Without this the CARRY4 sum/carry nets float. *)
  (* An OUTPUT port wired to a MULTI-BIT SLICE of a parent net.  Vivado's
     write_vhdl ALWAYS writes an explicit range -- `rptr_o(6 downto 0) =>
     \gen_normal_fifo.fifo_rptr\(6 downto 0)` -- so this is the normal shape,
     not an edge case.  lhs_subst takes only whole-net BVar actuals, the
     BConcat fanout below takes only concats, and bit_assigns only single bits
     (msb = lsb), so a multi-bit slice matched NOTHING and was dropped: the
     parent net was read but never written, Z3 minted it as a free variable,
     and the two sides of a miter got INDEPENDENT free values.  That is why
     every HIERARCHICAL module failed to prove equivalent to ITSELF while
     leaves passed (bisected on the ibex uart: prim_fifo_sync_cnt EQUIVALENT,
     prim_fifo_sync DIFFER on exactly \gen_normal_fifo.fifo_{r,w}ptr\).
     Full-width -> a plain whole-net write; partial -> @slice_write, which the
     SSA pass already understands. *)
  let parent_width n =
    let rec f = function
      | [] -> None
      | (s : bsignal) :: tl ->
          if s.name = n then
            (match s.stype with
             | BInt { width; _ } -> Some width
             | BBool -> Some 1
             | _ -> None)
          else f tl in
    f parent.signals in
  let slice_procs =
    List.filter_map (fun (formal, actual) ->
      match List.assoc_opt formal port_dirs, actual with
      | Some `Output, BSlice { signal = BVar n; msb; lsb } when msb <> lsb ->
          let hi = max msb lsb and lo = min msb lsb in
          let src = BVar (pname prefix formal) in
          let stmt =
            match parent_width n with
            | Some w when lo = 0 && hi = w - 1 -> BAssign { lhs = n; rhs = src }
            | _ ->
                BCallStmt { func = "@slice_write";
                            args = [BVar n;
                                    BConst { value = Z.of_int hi; width = 32 };
                                    BConst { value = Z.of_int lo; width = 32 };
                                    src] } in
          Some (BCombinational { name = pname prefix (formal ^ "__slicedrv");
                                 sensitivity = [BAny]; body = [stmt] })
      | _ -> None
    ) inst.port_connections in
  let fanout_procs =
    List.filter_map (fun (formal, actual) ->
      match List.assoc_opt formal port_dirs, actual with
      | Some `Output, BConcat parts
        when List.for_all (function BVar _ -> true | _ -> false) parts ->
          let w = List.length parts in
          let src = pname prefix formal in
          let body = List.mapi (fun j part ->
            let bit = w - 1 - j in
            match part with
            | BVar n -> BAssign { lhs = n;
                                  rhs = BSlice { signal = BVar src; msb = bit; lsb = bit } }
            | _ -> assert false) parts in
          Some (BCombinational { name = pname prefix (formal ^ "__fanout");
                                 sensitivity = [BAny]; body })
      | _ -> None
    ) inst.port_connections in
  (* An OUTPUT port wired to a single BUS BIT — `.O(y[3])` — is the common case
     in a synthesised netlist (every LUT/FF drives one bit of an output bus).
     lhs_subst only maps whole-net (BVar) actuals, so without this the bit floats
     (reads 0) and the whole bus collapses to 0.  Drive the per-bit net
     `obuf_<bus>_<i>__O` that resolve_input_bitbus assembles into the port. *)
  let bit_assigns =
    List.filter_map (fun (formal, actual) ->
      match List.assoc_opt formal port_dirs, actual with
      | Some `Output, BSlice { signal = BVar n; msb; lsb } when msb = lsb ->
          Some (Printf.sprintf "obuf_%s_%d__O" n msb, pname prefix formal)
      | Some `Output, BSelect { array = BVar n; index = BConst { value; _ } } ->
          Some (Printf.sprintf "obuf_%s_%d__O" n (Z.to_int value), pname prefix formal)
      | _ -> None
    ) inst.port_connections in
  let bit_proc =
    if bit_assigns = [] then []
    else [ BCombinational { name = pname prefix "__obuf_fanout"; sensitivity = [BAny];
             body = List.map (fun (dst, src) ->
               BAssign { lhs = dst; rhs = BVar src }) bit_assigns } ] in
  let obuf_sigs =
    List.map (fun (dst, _) ->
      { name = dst; stype = BInt { width = 1; signed = Unsigned };
        direction = `Internal; initial_value = None; attrs = [] }) bit_assigns in
  (* Carry the child's PRESERVED instances (unresolvable hard primitives --
     GTXE2/MMCM/RAMB -- kept by flatten_one rather than inlined away) up into
     the parent, renaming their connections exactly as reads are renamed.
     Without this they were dropped on the way up and never surfaced at top,
     so cut_blackboxes had nothing to cut and every cone fed by the block
     compared against an undriven net. *)
  let hoisted_instances =
    List.map (fun (ci : binstance) ->
      { ci with
        inst_name = pname prefix ci.inst_name;
        port_connections =
          List.map (fun (pin, e) -> (pin, sub_bexpr ~subst ~prefix e))
            ci.port_connections })
      child.instances in
  { parent with
    signals   = obuf_sigs @ new_signals;
    instances = hoisted_instances @ parent.instances;
    processes = slice_procs @ fanout_procs @ bit_proc @ new_processes }

(* Flatten a top-level module by recursively inlining every reachable
 * binstance. Memoised by module name so cells used multiple times only
 * pay the recursion cost once.
 *
 * Returns a transient flat bmodule. The input bprogram is unchanged. *)
(* Strip the parameter-specialisation suffix from a module name. The
 * Verible / yosys-slang pipeline names specialised siblings as
 * `<base>__<suffix>` (e.g. `popcount__IW16`). *)
let base_of_name n =
  try
    let i = Str.search_forward (Str.regexp "__") n 0 in
    String.sub n 0 i
  with Not_found -> n

(* Tracker for unresolved binstances encountered during the most
 * recent flatten_for_z3 run.  Cleared at the top of each call.
 * The miter (z3_miter) reads this after flatten to decide whether
 * it can declare EQUIVALENT or must report "INCONCLUSIVE — N
 * primitive bodies missing".  Per feedback-no-silent-lossage, a
 * missing instance must be auditable. *)
let unresolved : (string * string * string) list ref = ref []
let unresolved_seen : (string * string, unit) Hashtbl.t = Hashtbl.create 16

let unresolved_register ~parent_name (i : binstance) =
  let key = (parent_name, i.module_name) in
  if not (Hashtbl.mem unresolved_seen key) then begin
    Hashtbl.add unresolved_seen key ();
    unresolved := (parent_name, i.inst_name, i.module_name) :: !unresolved;
    Printf.eprintf
      "[flatten_for_z3] unresolved instance %s : %s in module %s\n"
      i.inst_name i.module_name parent_name
  end

let take_unresolved () : (string * string * string) list =
  let r = List.rev !unresolved in
  unresolved := [];
  Hashtbl.clear unresolved_seen;
  r

(* ORPHAN AUDIT.  An inlined child can leave a signal that is READ but never
 * WRITTEN -- an unconnected pin, a port whose actual did not substitute, a
 * fan-out that did not materialise.  Z3 mints such a signal as a FREE
 * variable, and the two sides of a miter get INDEPENDENT free variables, so
 * the verdict is DIFFER no matter what the logic does.  That is why a
 * hierarchical module could not be proven equivalent to ITSELF while a leaf
 * could: bisected on the ibex uart, prim_fifo_sync_cnt (leaf) EQUIVALENT,
 * prim_fifo_sync (one child) DIFFER, uart (two children) DIFFER.
 *
 * Per feedback-no-silent-lossage this must be auditable, so record and print
 * it the way `unresolved` does rather than letting a bad verdict stand. *)
let orphans : (string * string) list ref = ref []

let take_orphans () : (string * string) list =
  let r = List.rev !orphans in orphans := []; r

let rec expr_reads acc (e : bexpr) =
  match e with
  | BVar n -> n :: acc
  | BConst _ -> acc
  | BBinOp { lhs; rhs; _ } -> expr_reads (expr_reads acc lhs) rhs
  | BUnOp { operand; _ } -> expr_reads acc operand
  | BSelect { array; index } -> expr_reads (expr_reads acc array) index
  | BSlice { signal; _ } -> expr_reads acc signal
  | BConcat es -> List.fold_left expr_reads acc es
  | BReplicate { value; _ } -> expr_reads acc value
  | BCond { condition; then_val; else_val } ->
      expr_reads (expr_reads (expr_reads acc condition) then_val) else_val
  | BCall { args; _ } -> List.fold_left expr_reads acc args

let rec stmt_rw (wr, rd) (s : bstmt) =
  match s with
  | BAssign { lhs; rhs } -> (lhs :: wr, expr_reads rd rhs)
  | BIf { condition; then_stmts; else_stmts } ->
      let acc = (wr, expr_reads rd condition) in
      List.fold_left stmt_rw (List.fold_left stmt_rw acc then_stmts) else_stmts
  | BCase { selector; cases; default } ->
      let acc = (wr, expr_reads rd selector) in
      let acc = List.fold_left (fun a (v, ss) ->
        let (w, r) = a in
        List.fold_left stmt_rw (w, expr_reads r v) ss) acc cases in
      List.fold_left stmt_rw acc default
  | BWhile { condition; body } ->
      List.fold_left stmt_rw (wr, expr_reads rd condition) body
  | BFor { init; condition; update; body } ->
      let acc = stmt_rw (wr, expr_reads rd condition) init in
      let acc = stmt_rw acc update in
      List.fold_left stmt_rw acc body
  | BBlock ss -> List.fold_left stmt_rw (wr, rd) ss
  | BCallStmt { args; _ } -> (wr, List.fold_left expr_reads rd args)
  | BReturn (Some e) -> (wr, expr_reads rd e)
  | BReturn None -> (wr, rd)

let audit_orphans (m : bmodule) : unit =
  let body_of = function
    | BCombinational { body; _ } -> body
    | BSequential { body; _ } -> body
    | _ -> [] in
  let (wr, rd) =
    List.fold_left (fun acc p -> List.fold_left stmt_rw acc (body_of p))
      ([], []) m.processes in
  let written = Hashtbl.create 256 in
  List.iter (fun n -> Hashtbl.replace written n ()) wr;
  (* A top-level INPUT is legitimately free; so is anything a surviving
     black-box instance drives (cut_blackboxes compares those). *)
  let driven_ok = Hashtbl.create 64 in
  List.iter (fun (s : bsignal) ->
    match s.direction with
    | `Input -> Hashtbl.replace driven_ok s.name ()
    | _ -> ()) m.signals;
  List.iter (fun (i : binstance) ->
    List.iter (fun (_, e) ->
      List.iter (fun n -> Hashtbl.replace driven_ok n ()) (expr_reads [] e))
      i.port_connections) m.instances;
  let seen = Hashtbl.create 64 in
  List.iter (fun n ->
    if not (Hashtbl.mem written n) && not (Hashtbl.mem driven_ok n)
       && not (Hashtbl.mem seen n) then begin
      Hashtbl.replace seen n ();
      orphans := (m.name, n) :: !orphans
    end) rd;
  let n = List.length !orphans in
  if n > 0 then begin
    Printf.eprintf
      "[flatten_for_z3] WARNING: %d signal(s) READ but never WRITTEN in %s \
       after inlining -- Z3 will treat each as a FREE variable, so a miter \
       against an identical design can report DIFFER:\n" n m.name;
    List.iteri (fun i (_, s) ->
      if i < 20 then Printf.eprintf "    %s\n" s) (List.rev !orphans);
    if n > 20 then Printf.eprintf "    ... %d more\n" (n - 20);
    flush stderr
  end

let flatten_for_z3 ?(debug = false) (p : bprogram) ~top : bmodule =
  unresolved := [];
  orphans := [];
  Hashtbl.clear unresolved_seen;
  let by_name = Hashtbl.create 16 in
  List.iter (fun m -> Hashtbl.replace by_name m.name m) p.modules;
  let by_base = List.fold_left (fun acc (m : bmodule) ->
    let b = base_of_name m.name in
    let bucket = try List.assoc b acc with Not_found -> [] in
    (b, m :: bucket) :: List.remove_assoc b acc
  ) [] p.modules in
  let cache = Hashtbl.create 16 in
  (* For an instance whose `module_name` doesn't resolve directly,
   * try `<base>__*` siblings disambiguated by Behavioral_flatten's
   * port-shape match. Needs caller context for the width scoring. *)
  let lookup_resolving ~parent (i : binstance) =
    match Hashtbl.find_opt by_name i.module_name with
    | Some m -> Some m
    | None ->
        let b = base_of_name i.module_name in
        let candidates =
          try List.assoc b by_base with Not_found -> [] in
        if candidates = [] then None
        else begin
          let caller_widths = List.filter_map (fun (s : bsignal) ->
            let w = match s.stype with
              | BInt { width; _ } -> width
              | BArray { size; element = BInt { width; _ }; _ } ->
                  size * width
              | _ -> 0 in
            if w > 0 then Some (s.name, w) else None
          ) parent.signals in
          let chosen = Behavioral_flatten.pick_specialised
            ~caller_signal_widths:caller_widths ~candidates i in
          if debug then
            (match chosen with
             | Some m ->
                 Printf.eprintf "[hier] %s: %s → specialised %s\n"
                   parent.name i.module_name m.name
             | None ->
                 Printf.eprintf "[hier] %s: %s → no specialised match\n"
                   parent.name i.module_name);
          chosen
        end
  in
  let rec flatten ~parent (i : binstance) : bmodule =
    match lookup_resolving ~parent i with
    | None ->
        unresolved_register ~parent_name:parent.name i;
        { name = i.module_name; params = []; signals = []; processes = [];
          instances = []; funcs = []; mems = []; attrs = [] }
    | Some m ->
        match Hashtbl.find_opt cache m.name with
        | Some cached -> cached
        | None ->
            let m_flat = flatten_one m in
            Hashtbl.replace cache m.name m_flat;
            m_flat
  and flatten_one (m : bmodule) : bmodule =
    let acc = ref { m with instances = [] } in
    List.iter (fun (i : binstance) ->
      match lookup_resolving ~parent:m i with
      | None ->
          (* Unresolvable => a hard primitive (GTXE2_CHANNEL, MMCME2_ADV,
             RAMB36E1 ...).  PRESERVE the instance so it surfaces at top and
             cut_blackboxes can turn it into tied inputs / compared outputs.
             It used to be replaced by an EMPTY module and inlined away, which
             deleted the cell outright: on the eth-arp diagnostic the emission
             cut ONE black box (an MMCM at top level, the only one not buried
             in a submodule) where the Vivado netlist cut FIVE, so the GT and
             everything downstream of it was uncomparable. *)
          unresolved_register ~parent_name:m.name i;
          acc := { !acc with instances = i :: (!acc).instances }
      | Some _ ->
          let child_flat = flatten ~parent:m i in
          acc := inline_instance ~debug !acc i child_flat
    ) m.instances;
    !acc
  in
  match Hashtbl.find_opt by_name top with
  | Some m -> let f = flatten_one m in audit_orphans f; f
  | None ->
      failwith ("flatten_for_z3: no module '" ^ top ^ "' in program")
