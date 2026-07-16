(* Liberty-cell expansion pass.
 *
 * Input:  a Behavioral_ir.bprogram whose modules contain `binstance`s
 *         for cells declared in a Liberty library (e.g. simcells.lib's
 *         _AND_, _DFF_PP0_, …). The instances reach BIR via any
 *         existing structural-Verilog frontend (Verible's
 *         convert_files turns `_AND_ u1 (.A(a), .B(b), .Y(y));` into
 *         a binstance with module_name="_AND_").
 *
 * Output: a bprogram with those instances replaced by inline
 *         continuous-assigns (combinational cells) or BSequential
 *         processes (FF cells), built from each cell's `function:`
 *         string parsed via Sv_liberty.parse_function_to_bexpr.
 *
 * The result feeds straight into the existing Z3 miter — the gate-
 * level netlist becomes structurally identical to a behavioral BIR
 * once cells are expanded, so Z3_miter.check_miter_equivalence
 * compares them as it does any other pair. *)

open Behavioral_ir
open Sv_liberty

(* Bool/1-bit type used for cell I/O. *)
let bool1 = BInt { width = 1; signed = Unsigned }

(* Build the substitution map for a cell instance. The Liberty function
 * uses formal pin names (A, B, Y, IQ, …); the BIR instance carries
 * `port_connections : (formal, actual_bexpr) list`. We stitch them
 * here. *)
let port_map (inst : binstance) : (string * bexpr) list =
  inst.port_connections

(* The output pin's actual destination. Returns either a plain wire
 * name (full-bus driver) or a synthetic per-bit name (cell drives
 * one bit of a bus, e.g. `q[3:3]` ⇒ `q__b3`). The per-bit case is
 * tracked separately so expand_module can emit a final concat
 * reconstruction for the bus. *)
type dest =
  | DWhole of string                (* full wire `name` *)
  | DBit   of string * int          (* `name__b<idx>` for `name[idx:idx]` *)

let actual_dest port_map name =
  match List.assoc_opt name port_map with
  | Some (BVar n) -> Some (DWhole n)
  | Some (BSlice { signal = BVar n; msb; lsb }) when msb = lsb ->
      Some (DBit (n, msb))
  | Some (BSelect { array = BVar n; index = BConst { value; _ } }) ->
      Some (DBit (n, Z.to_int value))
  | Some _ | None -> None

let bit_wire_name bus idx = bus ^ "__b" ^ string_of_int idx

let dest_var_name = function
  | DWhole n -> n
  | DBit (n, i) -> bit_wire_name n i

(* Detect "negedge" form. Liberty `clocked_on: "C"` is posedge,
 * `clocked_on: "!C"` is negedge. Anything else (gated clock, multi-
 * clock) we conservatively treat as posedge of the trimmed signal —
 * the miter will catch the mismatch if it matters. *)
let parse_clock_expr s =
  let s = String.trim s in
  if String.length s > 1 && s.[0] = '!' then
    (`Neg, String.trim (String.sub s 1 (String.length s - 1)))
  else
    (`Pos, s)

(* Strip Liberty function-string formatting: simcells uses things like
 * "IQ" which we don't want to treat as the literal pin name when the
 * FF's IQ is the cell's internal state. For combinational cells the
 * function references input pins directly. *)
let parse_func env s =
  parse_function_to_bexpr env s

(* Build the BIR fragments for one cell instance.
 *
 * Returns (extra_processes, bit_taps, kept?).
 *   - extra_processes: BCombinational / BSequential bodies to add to
 *                      the parent bmodule.
 *   - bit_taps:        (bus, idx) entries recording that this cell
 *                      drove bus[idx] via the synthetic `<bus>__b<idx>`
 *                      net. expand_module aggregates them into a
 *                      final BConcat reconstruction.
 *   - kept?:           when true the binstance survives in the BIR.
 *                      Set when the cell is not in the Liberty (an
 *                      external module), or when expansion produced no
 *                      assignments.  *)
let expand_instance lib (inst : binstance)
  : bprocess list * (string * int) list * bool =
  match get_cell lib inst.module_name with
  | None -> ([], [], true)         (* not a Liberty cell — leave alone *)
  | Some cell ->
      let pm = port_map inst in
      let inputs =
        List.filter_map (fun (p : pin_info) ->
          if p.direction = Input then
            match List.assoc_opt p.name pm with
            | Some e -> Some (p.name, e)
            | None -> None
          else None
        ) cell.pins
      in
      let env = inputs in        (* formal-name → actual bexpr *)
      let outputs =
        List.filter (fun (p : pin_info) -> p.direction = Output) cell.pins
      in
      let bit_taps = ref [] in
      let drive_for op =
        match actual_dest pm op.name with
        | None -> None
        | Some d ->
            (match d with
             | DBit (bus, idx) -> bit_taps := (bus, idx) :: !bit_taps
             | DWhole _ -> ());
            Some (dest_var_name d)
      in
      (match cell.cell_type, cell.ff with
       | "ff", Some ff ->
           (* Sequential cell. Build one BSequential process whose body
            * is `Q <= (clear ? 0 : preset ? 1 : D)`. The function expr
            * on each Q pin is "IQ" / "IQN" — we only honour Q (IQ);
            * Qbar (IQN) is the inversion of Q. *)
           let (clk_edge, clk_name) = parse_clock_expr ff.clocked_on in
           let clk_actual =
             match List.assoc_opt clk_name pm with
             | Some (BVar n) -> n
             | _ -> clk_name
           in
           let next_e = parse_func env ff.next_state in
           let with_async cond const e =
             BCond { condition = cond;
                     then_val = BConst { value = const; width = 1 };
                     else_val = e } in
           let driven =
             let e0 = next_e in
             let e1 =
               match ff.clear with
               | None -> e0
               | Some c -> with_async (parse_func env c) Z.zero e0 in
             let e2 =
               match ff.preset with
               | None -> e1
               | Some p -> with_async (parse_func env p) Z.one e1 in
             e2
           in
           let stmts =
             List.filter_map (fun (op : pin_info) ->
               match drive_for op with
               | None -> None
               | Some dest_net ->
                   let rhs =
                     match op.function_expr with
                     | Some f when String.trim f = ff.iqn_name
                                && ff.iqn_name <> "" ->
                         BUnOp { op = BNot; operand = driven;
                                 result_type = bool1 }
                     | _ -> driven
                   in
                   Some (BAssign { lhs = dest_net; rhs })
             ) outputs
           in
           if stmts = [] then ([], [], true)
           else
             let proc =
               BSequential {
                 name = inst.inst_name;
                 clock = clk_actual;
                 clock_edge = clk_edge;
                 reset = None;
                 reset_edge = None;
                 reset_async = false;
                 body = stmts;
                 blocking_vars = [];
               } in
             ([proc], !bit_taps, false)
       | _ ->
           (* Combinational cell. *)
           let stmts =
             List.filter_map (fun (op : pin_info) ->
               match drive_for op, op.function_expr with
               | Some dest, Some f ->
                   let rhs = parse_func env f in
                   Some (BAssign { lhs = dest; rhs })
               | _ -> None
             ) outputs
           in
           if stmts = [] then ([], [], true)
           else
             let proc =
               BCombinational {
                 name = inst.inst_name;
                 sensitivity = [BAny];
                 body = stmts;
               } in
             ([proc], !bit_taps, false))

(* Width of a bus signal in the parent module, based on its declared
 * btype. Falls back to (max_bit_idx+1) if the signal isn't declared
 * (defensive — yosys-emitted gate-level always declares its outputs). *)
let bus_width_in (m : bmodule) bus fallback =
  match List.find_opt (fun (s : bsignal) -> s.name = bus) m.signals with
  | Some s ->
      (match s.stype with
       | BInt { width; _ } -> width
       | BArray { size; element = BInt { width; _ }; _ } -> size * width
       | _ -> fallback)
  | None -> fallback

let expand_module lib (m : bmodule) : bmodule =
  let extra = ref [] in
  let bit_taps = ref [] in
  let kept_insts =
    List.filter (fun inst ->
      let (procs, taps, kept) = expand_instance lib inst in
      extra := procs @ !extra;
      bit_taps := taps @ !bit_taps;
      kept
    ) m.instances
  in
  (* Group bit-taps by bus, deduplicate indices, and emit a final
   * `bus := { bit_w-1 ... 1, 0 }` continuous-assign per bus. The
   * synthetic per-bit nets are also declared as internal signals
   * so downstream passes (ffrip, share, encode_module) can refer to
   * them by name. *)
  let by_bus : (string, int list) Hashtbl.t = Hashtbl.create 8 in
  List.iter (fun (bus, idx) ->
    let prev = try Hashtbl.find by_bus bus with Not_found -> [] in
    if not (List.mem idx prev) then
      Hashtbl.replace by_bus bus (idx :: prev)
  ) !bit_taps;

  let new_signals = ref [] in
  let recon_processes = ref [] in
  Hashtbl.iter (fun bus indices ->
    let max_idx = List.fold_left max 0 indices in
    let bus_w = bus_width_in m bus (max_idx + 1) in
    (* Declare each per-bit synthetic net as a 1-bit Internal wire. *)
    for i = 0 to bus_w - 1 do
      if List.mem i indices then
        new_signals := {
          name = bit_wire_name bus i;
          stype = BInt { width = 1; signed = Unsigned };
          direction = `Internal;
          initial_value = None; attrs = []; 
        } :: !new_signals
    done;
    (* Reconstruct: bus := { bit[w-1], …, bit[1], bit[0] }. Bits for
     * which we don't have a tap fall back to BVar bit_wire_name —
     * they'll resolve to undriven if the gate-level was incomplete,
     * which the miter will surface as a counterexample. *)
    let parts =
      List.init bus_w (fun i ->
        let i = bus_w - 1 - i in   (* msb first for BConcat *)
        BVar (bit_wire_name bus i))
    in
    let rhs =
      if bus_w = 1 then List.hd parts else BConcat parts in
    let proc =
      BCombinational {
        name = bus ^ "_recon";
        sensitivity = [BAny];
        body = [BAssign { lhs = bus; rhs }];
      } in
    recon_processes := proc :: !recon_processes
  ) by_bus;

  { m with
    signals = !new_signals @ m.signals;
    processes = m.processes @ List.rev !extra @ !recon_processes;
    instances = kept_insts }

let expand_program (lib : library_info) (p : bprogram) : bprogram =
  let p' = { p with modules = List.map (expand_module lib) p.modules } in
  (* Re-pack bit-blasted bus FFs (#74). expand_module just emitted N
   * 1-bit BSequentials per cell instance plus the BConcat recon;
   * Behavioral_ffpack collapses those back into a single bus-level
   * BSequential so downstream FF-rip lines up with a behavioural
   * reference's bus FF. No-op when no bit-blasted patterns are
   * present. *)
  Behavioral_ffpack.pack_program p'

(* Convenience: load a Liberty file and expand a program in one shot. *)
let expand_program_with_liberty lib_file (p : bprogram) : bprogram =
  let lib = parse_liberty_file lib_file in
  expand_program lib p

(* ──────────────────────────────────────────────────────────────────
 * Gate-file preprocessing — yosys's `write_verilog` output uses two
 * grammar shapes Verible's BIR converter doesn't accept directly:
 *
 *   1. `\b_<digits>_\b` identifiers (e.g. `_0_`, `_3_`) for internal
 *      wires and unnamed cell instances.  Verible's lexer rejects
 *      these in some positions, so rewrite them to `n<digits>`.
 *      Cell types like `_AND_` are not pure-digit so they survive.
 *
 *   2. Old-style port lists (`module M(a, b, y); input [7:0] a; …`)
 *      where Verible's converter loses the widths.  Rewrite to ANSI
 *      form (`module M(input [7:0] a, …);`) and drop the redundant
 *      separate decls.
 *
 * `preprocess_gate_file` writes a clean copy to a temp file and
 * returns its path; callers feed that to `convert_files_with_externals`. *)

let rename_yosys_ids src =
  let buf = Buffer.create (String.length src + 64) in
  let len = String.length src in
  let is_word c =
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
    (c >= '0' && c <= '9') || c = '_'
  in
  let i = ref 0 in
  while !i < len do
    let c = src.[!i] in
    let prev_word = !i > 0 && is_word src.[!i - 1] in
    if c = '_' && not prev_word then begin
      let j = ref (!i + 1) in
      while !j < len && src.[!j] >= '0' && src.[!j] <= '9' do incr j done;
      let has_digits = !j > !i + 1 in
      let trailing_underscore =
        has_digits && !j < len && src.[!j] = '_'
        && (!j + 1 >= len || not (is_word src.[!j + 1])) in
      let no_trail_word_end =
        has_digits && (!j >= len || not (is_word src.[!j])) in
      if trailing_underscore then begin
        (* yosys-style `_<digits>_` → `n<digits>` *)
        Buffer.add_char buf 'n';
        Buffer.add_substring buf src (!i + 1) (!j - !i - 1);
        i := !j + 1
      end else if no_trail_word_end then begin
        (* Hardcaml-style `_<digits>` (no trailing) → `n<digits>` *)
        Buffer.add_char buf 'n';
        Buffer.add_substring buf src (!i + 1) (!j - !i - 1);
        i := !j
      end else begin
        Buffer.add_char buf c;
        incr i
      end
    end else begin
      Buffer.add_char buf c;
      incr i
    end
  done;
  Buffer.contents buf

let ansi_rewrite src =
  let module_re = Str.regexp
    "module[ \t\n]+\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t\n]*(\
     \\([^)]*\\))[ \t\n]*;" in
  (* Identifier class includes `$` because PYMTL-generated SV uses
     names like `in_$002`.  Without the `$`, the regex fails to
     match port-decl lines for those names and the ANSI rewrite
     drops their width — giving 1-bit ports in BIR. *)
  let port_decl_re = Str.regexp
    "[ \t]*\\(input\\|output\\|inout\\)[ \t]*\
     \\(\\[[^]]*\\][ \t]*\\)?\
     \\([A-Za-z_$][A-Za-z0-9_$]*\\)[ \t]*;" in
  let wire_decl_re = Str.regexp
    "[ \t]*wire[ \t]*\
     \\(\\[[^]]*\\][ \t]*\\)?\
     \\([A-Za-z_$][A-Za-z0-9_$]*\\)[ \t]*;" in
  let m =
    try Some (Str.search_forward module_re src 0) with Not_found -> None in
  if m = None then src
  else
    let mod_start = Str.match_beginning () in
    let mod_end   = Str.match_end () in
    let mod_name  = Str.matched_group 1 src in
    let port_list = Str.matched_group 2 src in
    let has_kw kw =
      try let _ = Str.search_forward
                    (Str.regexp_string kw) port_list 0 in true
      with Not_found -> false in
    if has_kw "input" || has_kw "output" || has_kw "inout" then src
    else begin
      let port_names =
        port_list
        |> String.split_on_char ','
        |> List.map String.trim
        |> List.filter (fun s -> s <> "") in
      let rest = String.sub src mod_end (String.length src - mod_end) in
      let endmod_idx =
        try Str.search_forward (Str.regexp "endmodule") rest 0
        with Not_found -> String.length rest in
      let header_region = String.sub rest 0 endmod_idx in
      let table : (string, string * string) Hashtbl.t = Hashtbl.create 16 in
      let scan re_ ~with_dir =
        let p = ref 0 in
        try
          while true do
            let _ = Str.search_forward re_ header_region !p in
            (if with_dir then
               let dir = Str.matched_group 1 header_region in
               let range =
                 try Str.matched_group 2 header_region with Not_found -> "" in
               let name = Str.matched_group 3 header_region in
               if List.mem name port_names then
                 Hashtbl.replace table name (dir, range)
             else
               let range =
                 try Str.matched_group 1 header_region with Not_found -> "" in
               let name = Str.matched_group 2 header_region in
               if List.mem name port_names && not (Hashtbl.mem table name) then
                 Hashtbl.replace table name ("", range));
            p := Str.match_end ()
          done; assert false
        with Not_found -> ()
      in
      scan port_decl_re ~with_dir:true;
      scan wire_decl_re ~with_dir:false;
      if Hashtbl.length table = 0 then src
      else begin
        let ansi_ports =
          List.map (fun n ->
            match Hashtbl.find_opt table n with
            | Some (dir, range) when dir <> "" ->
                Printf.sprintf "%s %s%s" dir range n
            | _ -> n
          ) port_names
          |> String.concat ", " in
        let new_header =
          Printf.sprintf "module %s (%s);" mod_name ansi_ports in
        let stripped =
          List.fold_left (fun s n ->
            let pd = Str.regexp
              (Printf.sprintf
                 "[ \t]*\\(input\\|output\\|inout\\)[ \t]*\
                  \\(\\[[^]]*\\][ \t]*\\)?%s[ \t]*;[ \t]*\n?"
                 (Str.quote n)) in
            let s = Str.global_replace pd "" s in
            let wd = Str.regexp
              (Printf.sprintf
                 "[ \t]*wire[ \t]*\
                  \\(\\[[^]]*\\][ \t]*\\)?%s[ \t]*;[ \t]*\n?"
                 (Str.quote n)) in
            Str.global_replace wd "" s
          ) header_region port_names in
        let prefix = String.sub src 0 mod_start in
        let suffix = String.sub rest endmod_idx
                       (String.length rest - endmod_idx) in
        prefix ^ new_header ^ "\n" ^ stripped ^ suffix
      end
    end

let preprocess_gate_file path =
  let ic = open_in path in
  let raw = really_input_string ic (in_channel_length ic) in
  close_in ic;
  let stage1 = rename_yosys_ids raw in
  let stage2 = ansi_rewrite stage1 in
  if stage2 = raw then path
  else begin
    let tmp = Filename.temp_file "gate_renamed_" ".v" in
    let oc = open_out tmp in
    output_string oc stage2;
    close_out oc;
    tmp
  end

(* Quick sanity counter — how many instances did we know about? *)
let instance_coverage lib (p : bprogram) =
  let known = ref 0 and unknown = ref 0 in
  let unknown_set = Hashtbl.create 16 in
  List.iter (fun (m : bmodule) ->
    List.iter (fun (inst : binstance) ->
      match get_cell lib inst.module_name with
      | Some _ -> incr known
      | None ->
          incr unknown;
          Hashtbl.replace unknown_set inst.module_name ()
    ) m.instances
  ) p.modules;
  let names = Hashtbl.fold (fun k () a -> k :: a) unknown_set [] in
  (!known, !unknown, names)
