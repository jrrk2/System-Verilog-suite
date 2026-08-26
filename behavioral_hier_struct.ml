(* Structural flattener for BIR (sibling of Behavioral_hier.flatten_for_z3).
 *
 * `flatten_for_z3` is designed for Z3 encoding: it inlines submodule
 * behavioural processes into the parent, but DROPS binstances whose
 * `module_name` doesn't resolve to a user bmodule — primitive cells
 * (LUT6, FDRE, CARRY4, BUFG, …) silently vanish.  That's fine when
 * Z3 only cares about behavioural logic.
 *
 * For netlist consumers (nextpnr-xilinx via Bir_to_nextpnr_json) we
 * need the opposite: KEEP every primitive binstance, and recursively
 * pull primitive binstances out of user-defined children so the
 * top-level bmodule ends up with one big flat list of LUT/FDRE/CARRY4
 * cells, name-prefixed by their original hierarchy path.
 *
 * Port connections from the parent's instantiation become net
 * rewrites: where a child's binstance refers to one of the child's
 * port names, we replace it with the parent's actual at that pin. *)

open Behavioral_ir

let pname prefix name =
  if prefix = "" then name else prefix ^ "__" ^ name

(* Pull base + bit from "name", "name[N]", or "name__N" (of_circuit's
   multi-bit-input memo naming).  The `__N` form must be recognised here too,
   else a flattened per-bit input reference `framing_rdata__40` fails to match
   the port `framing_rdata` and becomes a driverless orphan net.  port_actual
   disambiguates: if `base` is not a port the whole original name is kept. *)
let parse_bit (name : string) : string * int option =
  match
    (try
       let lb = String.rindex name '[' in
       let rb = String.rindex name ']' in
       if rb = String.length name - 1 then
         let base = String.sub name 0 lb in
         Some (base, int_of_string (String.sub name (lb + 1) (rb - lb - 1)))
       else None
     with _ -> None)
  with
  | Some (base, idx) -> (base, Some idx)
  | None ->
      let n = String.length name in
      let rec find i =
        if i < 1 then None
        else if name.[i] = '_' && name.[i - 1] = '_' then Some i else find (i - 1) in
      (match find (n - 1) with
       | Some j when j + 1 < n ->
           let suf = String.sub name (j + 1) (n - j - 1) in
           if suf <> "" && String.for_all (fun c -> c >= '0' && c <= '9') suf
           then (String.sub name 0 (j - 1), Some (int_of_string suf))
           else (name, None)
       | _ -> (name, None))

(* Hardcaml names a vector port's bits "<base>__<bit>" (double underscore +
   decimal index), while the port itself stays a vector signal "<base>".
   gate_map -> mapped_to_prog emits internal cell pins in this form, so a
   reference like "rx_data__3" must resolve to bit 3 of the port "rx_data".
   Split on the LAST "__" whose suffix is all digits.  Returns None for
   ordinary nets (e.g. Hardcaml temporaries "_n_69" have no "__"). *)
let split_dunder_bit (name : string) : (string * int) option =
  let len = String.length name in
  let rec find i =
    if i < 1 then None
    else if name.[i] = '_' && name.[i - 1] = '_' then Some (i - 1)
    else find (i - 1)
  in
  match find (len - 1) with
  | None -> None
  | Some us ->
      let base = String.sub name 0 us in
      let suff = String.sub name (us + 2) (len - us - 2) in
      if base <> "" && suff <> ""
         && String.for_all (fun c -> c >= '0' && c <= '9') suff
      then Some (base, int_of_string suff)
      else None

(* Rewrite a child's bexpr into parent scope.
   - if the BVar refers to a child PORT (possibly bit-selected), substitute
     the parent's actual for that pin; bit indices are propagated through.
   - otherwise prefix the name with the instance prefix. *)
(* Constant-sentinel net names that downstream emitters
 * (bir_to_nextpnr_json's const_of_name) map straight to "0"/"1"
 * string-bit tokens.  Must NOT be hierarchy-prefixed during flatten,
 * otherwise the emitter loses the constant and reports a driverless
 * net (caught by Vivado place_design's NDRV-1 DRC).               *)
let is_const_sentinel = function
  | "VCC" | "GND" | "<const0>" | "<const1>" -> true
  | _ -> false

(* Reduce a (possibly NESTED) constant bit-select chain over a BVar to
   (name, bit).  Vivado's EDIF wraps a scalar member in a redundant [0] once
   per hierarchy level it crosses, so `eth_rst_n_OBUF[0][0][0]` is simply
   bit 0 of that net.  A single-level match misses those and the enclosing
   continuous assign gets dropped. *)
let rec reduce_bitref (e : bexpr) : (string * int) option =
  match e with
  | BSlice { signal = BVar nm; msb; lsb } when msb = lsb -> Some (nm, lsb)
  | BSelect { array = BVar nm; index = BConst { value; _ } } ->
      Some (nm, Z.to_int value)
  | BSelect { array; index = BConst { value; _ } } when Z.equal value Z.zero ->
      reduce_bitref array
  | _ -> None

let rec rewrite_bexpr ~prefix ~(port_actual : string -> bexpr option)
                      ~(is_local : string -> bool) (e : bexpr) : bexpr =
  match e with
  | BVar nm when is_const_sentinel nm -> e
  | BVar nm ->
      (* FIRST try the FULL name as a port: gate_map may have split a vector
         port into PER-BIT ports each literally named "<base>__<bit>", so the
         child's reference and the port share that exact name.  Splitting into
         base+bit first (below) then looks up "<base>", which misses the per-bit
         port and leaves the reference an undriven orphan (gp_i__2, obuf inputs,
         CDC-FIFO cross-domain bits). *)
      (match port_actual nm with
       | Some actual -> actual
       (* A name this module DECLARES is a real net: never reinterpret it as
          "<port>__<bit>".  Vivado's EDIF mangles `dmcontrol_q_reg[haltreq]__0`
          to the VALIDID `dmcontrol_q_reg_haltreq___0`, which parse_bit happily
          splits into base `dmcontrol_q_reg_haltreq_` + bit 0 -- and dm_top
          really HAS a port of that name, so a private internal net was aliased
          onto the port's parent net.  That merged two distinct nets and left
          the netlist multiply driven (an FF .Q and a LUT .O on one net), which
          nextpnr rejects.  Only the EXACT port lookup above may claim nm. *)
       | None when is_local nm -> BVar (pname prefix nm)
       | None ->
      let base, idx_opt = parse_bit nm in
      (match port_actual base, idx_opt with
       | Some actual, None        -> actual
       | Some actual, Some bit    -> bit_select actual bit
       | None,        _           ->
           (* Try Hardcaml per-bit vector-port naming "<port>__<bit>". *)
           (match split_dunder_bit nm with
            | Some (b, bit) ->
                (match port_actual b with
                 | Some actual -> bit_select actual bit
                 | None        -> BVar (pname prefix nm))
            | None -> BVar (pname prefix nm))))
  | BSelect { array; index } ->
      let array' = rewrite_bexpr ~prefix ~port_actual ~is_local array in
      (match array', index with
       | BConcat es, BConst { value; _ } ->
           (* MSB-first concat: bit i is at position (width-1 - i).        *)
           let w = List.length es in
           let vi = Z.to_int value in
           if vi >= 0 && vi < w then List.nth es (w - 1 - vi)
           else BSelect { array = array'; index }
       | _ -> BSelect { array = array'; index })
  | BConst _ -> e
  | BConcat es ->
      BConcat (List.map (rewrite_bexpr ~prefix ~port_actual ~is_local) es)
  | BSlice { signal; msb; lsb } ->
      (* hardcaml_to_behavioral emits BSlice on output-port bits after
       * regrouping per-bit Circuit.t outputs into vector ports.        *)
      let signal' = rewrite_bexpr ~prefix ~port_actual ~is_local signal in
      BSlice { signal = signal'; msb; lsb }
  | _ -> e   (* other forms shouldn't appear in a structural net *)

(* Pull `bit` out of `expr`.  Used when a child's port-bit reference must
   route through to the parent's actual net for that bit. *)
and bit_select (expr : bexpr) (bit : int) : bexpr =
  match expr with
  | BVar _ ->
      (* Select one bit of a flat vector net.  Emit BSlice (msb=lsb=bit) — the
       * SAME representation hardcaml_to_behavioral uses for vector port bits
       * (e.g. `.O(dout[3:3])`), so the emitter resolves driver and reader to
       * the same net bit.  A bracket STRING "net[bit]" would instead become a
       * separate opaque scalar net and leave the bit undriven. *)
      BSlice { signal = expr; msb = bit; lsb = bit }
  | BConcat es ->
      let w = List.length es in
      if bit >= 0 && bit < w then List.nth es (w - 1 - bit)
      else expr
  | BConst { value; _ } ->
      (* Select one bit of a multi-bit constant (e.g. parent ties a 2-bit
       * port to 2'b11); without this, a single-bit reference would keep the
       * whole constant and land a 2-bit value on a 1-bit cell pin. *)
      BConst { value = (if Z.testbit value bit then Z.one else Z.zero); width = 1 }
  | BSelect _ -> expr  (* parent already bit-selected; assume scalar *)
  | _         -> expr

(* Width of a structural bexpr.  A bare BVar is a scalar net (1 bit) here —
   vector nets appear wrapped in BSlice/BConcat in these flattened netlists. *)
let rec bexpr_width = function
  | BConst { width; _ } -> width
  | BSlice { msb; lsb; _ } -> abs (msb - lsb) + 1
  | BConcat es -> List.fold_left (fun a e -> a + bexpr_width e) 0 es
  | BReplicate { count; value } -> count * bexpr_width value
  | _ -> 1

(* Extract bit `i` (LSB = 0) of a structural bexpr as a 1-bit expr, honouring
   each concat element's true width (bit_select above assumes 1-bit elements,
   which is wrong for a concat like {const0, ^sv[13:9], ^sv[7:0]}). *)
let rec bexpr_bit (e : bexpr) (i : int) : bexpr =
  match e with
  | BConst { value; _ } ->
      BConst { value = (if Z.testbit value i then Z.one else Z.zero); width = 1 }
  | BVar _ -> if i = 0 then e else BSlice { signal = e; msb = i; lsb = i }
  | BSlice { signal = BVar _ as v; msb; lsb } ->
      let lo = min msb lsb in BSlice { signal = v; msb = lo + i; lsb = lo + i }
  | BSlice { signal; msb; lsb } ->
      let lo = min msb lsb in bexpr_bit signal (lo + i)
  | BConcat es ->
      (* MSB-first list; walk from the LSB end subtracting element widths. *)
      let rec go i = function
        | [] -> BConst { value = Z.zero; width = 1 }
        | el :: rest ->
            let w = bexpr_width el in
            if i < w then bexpr_bit el i else go (i - w) rest
      in go i (List.rev es)
  | _ -> e

(* Flatten a bmodule: return the list of primitive binstances reachable
   from `m`, name-prefixed and port-rewritten so all references resolve
   in the top-level (caller's) scope. *)
(* SILENT-LOSSAGE GUARD: the structural flattener keeps only binstances and
   discards m.processes entirely.  That is correct for a FULLY gate-mapped
   module (all logic already lives in LUT/FF/CARRY cells), but a module that
   still carries continuous assigns — constant ties `assign x = <const0>`,
   net aliases `assign a = b`, or any un-lowered combinational logic — would
   have that logic VANISH silently, leaving driverless nets that P&R then
   trims (this dropped pcs_pma_flat.v's 1467 <const0>/<const1> ties in the PCS
   pass-through and killed downstream wrapper LUTs).  Fail loudly instead. *)
(* A process the structural flattener can RESOLVE into net connectivity: a
   combinational block of plain continuous assigns (`assign lhs = rhs`, incl.
   constant ties and bit-remaps).  Anything else (BIf/BCase comb logic, a
   clocked BSequential) is un-structural and must have been gate-mapped. *)
let process_is_resolvable = function
  | Behavioral_ir.BCombinational { body; _ } ->
      List.for_all (function Behavioral_ir.BAssign _ -> true | _ -> false) body
  | Behavioral_ir.BSequential { body; _ } -> body = []

(* Collect a module's continuous assigns as (lhs_net, rhs_expr) pairs, both
   rewritten into the caller's flat scope (so an output-port LHS resolves to
   the parent net it drives).  Fails loudly on any UN-resolvable process —
   silent-lossage guard: that logic would otherwise vanish. *)
let collect_resolvable_assigns ~prefix ~port_actual ~is_local (m : bmodule)
    : (string * bexpr) list =
  let bad = List.filter (fun p -> not (process_is_resolvable p)) m.processes in
  if bad <> [] then
    failwith (Printf.sprintf
      "flatten_structural: module %S has %d process(es) with un-structural logic \
       (BIf/BCase/clocked) that the structural flattener cannot resolve and would \
       SILENTLY DROP — gate_map this module first." m.name (List.length bad));
  List.concat_map (function
    | Behavioral_ir.BCombinational { body; _ } ->
        List.concat_map (function
          | Behavioral_ir.BAssign { lhs; rhs } ->
              let lhs' = rewrite_bexpr ~prefix ~port_actual ~is_local (BVar lhs) in
              let rhs' = rewrite_bexpr ~prefix ~port_actual ~is_local rhs in
              (match lhs' with
               | BVar nm -> [(nm, rhs')]
               | BSlice { signal = BVar nm; msb; lsb } when msb = lsb ->
                   (* bracket-keyed per-bit assign (`v[k] = x` from a ranged
                      continuous assign) whose base is a PORT: rewrite gave
                      bit k of the parent actual — key it per-bit *)
                   [(Printf.sprintf "%s[%d]" nm msb, rhs')]
               | BConcat elems ->
                   (* An output-port whose port_actual is a bit-split concat
                      {n16,...,n1} (MSB-first): emit one per-bit assign so every
                      parent net gets its driver.  Without this the whole assign
                      dropped (silent lossage) and each bit dangled — this is
                      what left pcspma_status[15:0] undriven. *)
                   let w = List.fold_left (fun a e -> a + bexpr_width e) 0 elems in
                   let hi = ref (w - 1) in
                   List.concat_map (fun el ->
                     let we = bexpr_width el in
                     let base_hi = !hi in
                     hi := !hi - we;
                     List.filter_map (fun b ->
                       let bitpos = base_hi - b in           (* net bit,   MSB-first *)
                       let el_bit = we - 1 - b in            (* elem bit,  LSB = 0   *)
                       match el with
                       | BVar nm -> Some (nm, bexpr_bit rhs' bitpos)
                       | BSlice { signal = BVar nm; msb; lsb } ->
                           let lo = min msb lsb in
                           Some (Printf.sprintf "%s[%d]" nm (lo + el_bit),
                                 bexpr_bit rhs' bitpos)
                       | _ -> None)
                       (List.init we (fun b -> b)))
                     elems
               | BSelect _ when reduce_bitref lhs' <> None ->
                   (* net_to_expr_aliased renders a port-aliased net as
                      BSelect{BVar port; bit}, and each hierarchy level the
                      assign crosses adds another redundant [0].  These used to
                      fall through to [] and the assign vanished silently —
                      which is how the EDIF sibling-port ties (uart E[0] clock
                      enable, eth_rst_n) were lost.  Key it per-bit, like the
                      BSlice case. *)
                   (match reduce_bitref lhs' with
                    | Some (nm, b) -> [(Printf.sprintf "%s[%d]" nm b, rhs')]
                    | None -> [])
               | other ->
                   (* Never drop an assign silently: a lost continuous assign
                      shows up much later as an undriven net and a whole-design
                      nextpnr failure. *)
                   Printf.eprintf
                     "[flatten_structural] WARN: dropped continuous assign in %s \
                      — unsupported LHS shape %s\n%!"
                     m.name (Behavioral_ir.string_of_bexpr other);
                   [])
          | _ -> []) body
    | _ -> []) m.processes

let rec flatten_module ~by_name ~prefix ~assigns ~sigw (m : bmodule)
                      ~(port_actual : string -> bexpr option) : binstance list =
  (* Record every signal this module declares under its FLAT name, so widths
     are carried through the flatten instead of being guessed afterwards from
     string shapes.  Guessing conflated same-named signals in different
     modules and silently widened primitive pins. *)
  (* Record EVERY signal, not just the wide ones: the whole point is that an
     exact hit is authoritative, and the nets that need protecting are the
     NARROW ones a heuristic would otherwise widen. *)
  List.iter (fun (s : bsignal) ->
    match s.stype with
    | BInt { width; _ } -> Hashtbl.replace sigw (pname prefix s.name) width
    | BBool -> Hashtbl.replace sigw (pname prefix s.name) 1
    | _ -> ()) m.signals;
  (* A name this module DECLARES is a real net and must never be reinterpreted
     as "<port>__<bit>" -- see rewrite_bexpr. *)
  let local_tbl = Hashtbl.create (List.length m.signals) in
  List.iter (fun (s : bsignal) -> Hashtbl.replace local_tbl s.name ()) m.signals;
  let is_local nm = Hashtbl.mem local_tbl nm in
  assigns := collect_resolvable_assigns ~prefix ~port_actual ~is_local m @ !assigns;
  List.concat_map (fun (i : binstance) ->
    let new_inst_name = pname prefix i.inst_name in
    match Hashtbl.find_opt by_name i.module_name with
    | None ->
        (* Primitive cell — rewrite its pin nets and emit. *)
        let pcs = List.map (fun (pin, expr) ->
          pin, rewrite_bexpr ~prefix ~port_actual ~is_local expr) i.port_connections in
        [{ i with inst_name = new_inst_name; port_connections = pcs }]
    | Some child ->
        (* User-defined cell — recurse with the inst's port_connections
           rewritten through the parent's port_actual to give the child's
           pins their concrete parent-level nets. *)
        let inst_port_rewritten =
          List.map (fun (pin, expr) ->
            pin, rewrite_bexpr ~prefix ~port_actual ~is_local expr)
            i.port_connections in
        let port_set =
          List.filter_map (fun (s : bsignal) ->
            match s.direction with
            | `Input | `Output -> Some s.name
            | _ -> None) child.signals in
        let new_port_actual pn =
          let r = if List.mem pn port_set then List.assoc_opt pn inst_port_rewritten
                  else None in
          (if Sys.getenv_opt "SVS_FLATTEN_DEBUG" <> None
              && r = None && List.mem pn port_set then
             Printf.eprintf "[orphan-port] inst=%s mod=%s pin=%s in_portset=true has_conn=%b\n"
               i.inst_name i.module_name pn (List.mem_assoc pn inst_port_rewritten));
          r
        in
        flatten_module ~by_name ~assigns ~sigw
          ~prefix:new_inst_name
          ~port_actual:new_port_actual
          child
  ) m.instances

(* Top-level entry: flatten the program around `top`, returning a single
   flat bmodule whose `instances` are all primitives and whose `signals`
   are the top's ports plus the unprefixed scalar nets referenced by the
   final primitive port_connections. *)
let flatten_structural (p : bprogram) ~top : bmodule =
  let by_name = Hashtbl.create 16 in
  List.iter (fun (m : bmodule) -> Hashtbl.replace by_name m.name m) p.modules;
  let top_mod =
    match Hashtbl.find_opt by_name top with
    | Some m -> m
    | None -> failwith ("flatten_structural: no module '" ^ top ^ "' in program")
  in
  let assigns = ref [] in
  let sigw : (string, int) Hashtbl.t = Hashtbl.create 4096 in
  let prims0 = flatten_module ~by_name ~assigns ~sigw ~prefix:"" ~port_actual:(fun _ -> None) top_mod in

  (* Resolve the collected continuous assigns (net aliases / constant ties /
     bit-remaps to output ports) into cell-pin connectivity: a read of an
     assigned net becomes the assign's RHS.  Without this the driver assign
     vanishes and the net dangles (P&R trims the reader). *)
  let amap : (string, bexpr) Hashtbl.t = Hashtbl.create 256 in
  List.iter (fun (nm, rhs) -> if not (Hashtbl.mem amap nm) then Hashtbl.add amap nm rhs) !assigns;
  (* A bit-keyed assign `nm[0]` on a SCALAR net must also answer a BARE `nm`
     read: the sibling-port tie for eth_rst_n arrives bit-keyed (the LHS came
     through a chain of redundant [0] selects) while the OBUF reads the net as
     a plain BVar, so the tie was recorded and never consulted.  Only safe when
     nm carries no OTHER assigned bit — a real bus keeps its per-bit keys. *)
  List.iter (fun (nm, rhs) ->
    let n = String.length nm in
    if n > 3 && nm.[n - 1] = ']' then
      match String.rindex_opt nm '[' with
      | Some lb when String.sub nm (lb + 1) (n - lb - 2) = "0" ->
          let base = String.sub nm 0 lb in
          let other_bit =
            List.exists (fun (k, _) ->
              String.length k > lb + 1 && String.sub k 0 (lb + 1) = base ^ "["
              && k <> nm) !assigns in
          if (not other_bit) && not (Hashtbl.mem amap base) then
            Hashtbl.add amap base rhs
      | _ -> ()) !assigns;
  (* TOP-port bitbus stitching: when the flatten TOP is itself a GATE-MAPPED
     module, its internal logic reads input-port bits by the Hardcaml
     convention `<port>__<i>` — with no parent to map them through
     port_actual, those names dangle (ERC: tx_rd_data__22 undriven).
     Resolve them directly to BSlice port[i:i]. *)
  let top_portw : (string, int) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    match s.direction, s.stype with
    | (`Input | `Output), BInt { width; _ } -> Hashtbl.replace top_portw s.name width
    | (`Input | `Output), BBool -> Hashtbl.replace top_portw s.name 1
    | _ -> ()) top_mod.signals;
  let rec apply ?(depth = 0) e =
    if depth > 64 then e else
    match e with
    | BVar nm ->
        (match Hashtbl.find_opt amap nm with
         | Some rhs -> apply ~depth:(depth + 1) rhs
         | None ->
             (match split_dunder_bit nm with
              | Some (b, bit) ->
                  (match Hashtbl.find_opt top_portw b with
                   | Some w when bit < w ->
                       BSlice { signal = BVar b; msb = bit; lsb = bit }
                   | _ -> e)
              | None -> e))
    | BSlice { signal = BVar nm; msb; lsb }
      when msb = lsb && Hashtbl.mem amap (Printf.sprintf "%s[%d]" nm msb) ->
        (* per-bit bracket-keyed assign resolves this bit read *)
        apply ~depth:(depth + 1) (Hashtbl.find amap (Printf.sprintf "%s[%d]" nm msb))
    | BSlice { signal = BVar nm; msb; lsb } when Hashtbl.mem amap nm ->
        let rhs = apply ~depth:(depth + 1) (Hashtbl.find amap nm) in
        (* width-aware bit extraction: rhs may be a concat of MULTI-BIT
           elements ({const0, ^sv[13:9], ^sv[7:0]}) — bit_select's 1-bit
           element assumption picked the wrong element and zero-tied the
           read (the GT rxresetfsm data_valid killer) *)
        if msb = lsb then bexpr_bit rhs msb
        else BConcat (List.init (msb - lsb + 1) (fun k -> bexpr_bit rhs (msb - k)))
    | BSlice { signal; msb; lsb } -> BSlice { signal = apply ~depth signal; msb; lsb }
    | BConcat es -> BConcat (List.map (apply ~depth) es)
    (* Reads arrive port-aliased as BSelect{BVar net; bit}; resolve them against
       the per-bit assign keys the BSelect LHS case above emits, else the tie is
       recorded but never consulted. *)
    | BSelect _
      when (match reduce_bitref e with
            | Some (nm, b) -> Hashtbl.mem amap (Printf.sprintf "%s[%d]" nm b)
            | None -> false) ->
        (match reduce_bitref e with
         | Some (nm, b) ->
             apply ~depth:(depth + 1) (Hashtbl.find amap (Printf.sprintf "%s[%d]" nm b))
         | None -> e)
    | BSelect _
      when (match reduce_bitref e with
            | Some (nm, _) -> Hashtbl.mem amap nm
            | None -> false) ->
        (match reduce_bitref e with
         | Some (nm, b) -> bexpr_bit (apply ~depth:(depth + 1) (Hashtbl.find amap nm)) b
         | None -> e)
    | BSelect { array; index } -> BSelect { array = apply ~depth array; index = apply ~depth index }
    | _ -> e
  in
  let prims =
    if Hashtbl.length amap = 0 then prims0
    else List.map (fun (i : binstance) ->
      { i with port_connections =
                 List.map (fun (pin, e) -> pin, apply e) i.port_connections }) prims0
  in

  (* Materialise assigns that drive TOP OUTPUT PORT bits: amap substitution
     rewrites READERS, but the port itself is not a reader pin — an output
     like `assign eth_clk_o = eth_clk` would emit with no driver (bit-level
     ERC: port:eth_clk_o[0] NO DRIVER).  Emit an identity LUT6 obuf per such
     bit; bir_to_edif's ident-obuf bypass collapses it into a pure net JOIN,
     so no actual LUT lands in the EDIF (safe for clock ports). *)
  let out_portw : (string, int) Hashtbl.t = Hashtbl.create 16 in
  List.iter (fun (s : bsignal) ->
    match s.direction, s.stype with
    | `Output, BInt { width; _ } -> Hashtbl.replace out_portw s.name width
    | `Output, BBool -> Hashtbl.replace out_portw s.name 1
    | _ -> ()) top_mod.signals;
  let port_bit_referenced =
    (* a port bit already touched by some instance pin is left alone —
       its driver (or an obuf) is already in the netlist *)
    let tbl : (string * int, unit) Hashtbl.t = Hashtbl.create 64 in
    let rec scan = function
      | BVar nm when Hashtbl.mem out_portw nm -> Hashtbl.replace tbl (nm, 0) ()
      | BSlice { signal = BVar nm; msb; lsb } when Hashtbl.mem out_portw nm ->
          for b = min msb lsb to max msb lsb do Hashtbl.replace tbl (nm, b) () done
      | BSlice { signal; _ } -> scan signal
      | BConcat es -> List.iter scan es
      | BSelect { array; _ } -> scan array
      | _ -> () in
    List.iter (fun (i : binstance) ->
      List.iter (fun (_, e) -> scan e) i.port_connections) prims;
    tbl in
  let mk_portdrv base w bit rhs_bit =
    Hashtbl.replace port_bit_referenced (base, bit) ();
    let rhs' = apply rhs_bit in
    let o = if w = 1 then BVar base
            else BSlice { signal = BVar base; msb = bit; lsb = bit } in
    { inst_name = Printf.sprintf "__portdrv_%s_%d" base bit;
      module_name = "LUT6";
      param_values = [];
      param_strs = [("INIT", "64'hFFFFFFFF00000000")];
      port_connections =
        [("I0", rhs'); ("I1", rhs'); ("I2", rhs');
         ("I3", rhs'); ("I4", rhs'); ("I5", rhs'); ("O", o)];
    } in
  let synth_obufs =
    List.concat_map (fun (nm, rhs) ->
      let base, bit = parse_bit nm in
      match Hashtbl.find_opt out_portw base with
      | None -> []
      | Some w ->
          (match bit with
           | Some b ->
               if b >= w || Hashtbl.mem port_bit_referenced (base, b) then []
               else [mk_portdrv base w b rhs]
           | None ->
               if w = 1 then
                 (if Hashtbl.mem port_bit_referenced (base, 0) then []
                  else [mk_portdrv base w 0 rhs])
               else
                 List.filter_map (fun b ->
                   if Hashtbl.mem port_bit_referenced (base, b) then None
                   else Some (mk_portdrv base w b (bexpr_bit rhs b)))
                   (List.init w (fun b -> b)))) !assigns in
  let prims = prims @ synth_obufs in

  (* Collect all signal names referenced by any primitive's port net.    *)
  let referenced = Hashtbl.create 4096 in
  let scan_bexpr =
    let rec go = function
      | BVar nm -> Hashtbl.replace referenced nm ()
      | BSelect { array; _ } -> go array
      | BConcat es -> List.iter go es
      | _ -> ()
    in go
  in
  List.iter (fun (i : binstance) ->
    List.iter (fun (_, e) -> scan_bexpr e) i.port_connections) prims;

  (* Keep top-level ports (with EDIF widths) and add an Internal signal
     for any referenced net not already a port. *)
  let port_names =
    List.filter_map (fun (s : bsignal) ->
      match s.direction with `Input | `Output -> Some s.name | _ -> None)
      top_mod.signals
  in
  let port_signals = List.filter (fun (s : bsignal) ->
    s.direction = `Input || s.direction = `Output) top_mod.signals in
  (* Width oracle for flattened internal nets.  A flattened net is
     `<inst_prefix>__<child_signal_name>`; forcing every one to width 1
     LOST declared vector widths (a `wire [5:0] .../rd_addr` fed a bare
     BVar to RAM64M.ADDRA, so the nextpnr-json emitter padded 5 orphan
     `__pad_` bits -> undriven read-address -> nextpnr's topo walk never
     retired the RAM read output -> FALSE "combinatorial loop").  Recover
     the width by matching the `__<signal>` suffix against every module's
     declared signals (child names carry Vivado's full escaped path, so
     the boundary match is precise; longest match wins). *)
  (* Widths for the flat nets, in TWO tables -- and the split is the point.
     A CHILD module's signal only ever appears in the flat netlist as
     "<instpath>__<name>"; it is never reachable under its bare name.  So its
     width may be consulted through the "__"-suffix rule ONLY.  Letting it
     match by BARE name lets a child's wide PORT claim a same-named narrow net
     in the TOP -- which is legal input, because Vivado sanitises different
     originals to a single EDIF identifier and EDIF keeps port, instance and
     net namespaces apart:

       cell BmbToWishbone   port minimalhyperramsoc_bus_errors_reg_31_  [32 bits]
       cell lowrisc_sonata  net  minimalhyperramsoc_bus_errors_reg_31_  [1 bit, FDRE Q]

     With one shared table the max() below handed the 1-bit net a width of 32,
     so the FDRE got a 32-bit Q; nextpnr then created Q0..Q31 on the packed
     SLICE_FFX and the router died with "No wire found for port Q31" -- 9000
     cells from the cause and looking exactly like an SRL cascade bug. *)
  let wide_top  : (string, int) Hashtbl.t = Hashtbl.create 1024 in
  let wide_hier : (string, int) Hashtbl.t = Hashtbl.create 4096 in
  List.iter (fun (m : bmodule) ->
    let tbl = if m.name = top then wide_top else wide_hier in
    List.iter (fun (s : bsignal) ->
      match s.stype with
      | BInt { width; _ } when width > 1 ->
          let prev = try Hashtbl.find tbl s.name with Not_found -> 0 in
          if width > prev then Hashtbl.replace tbl s.name width
      | _ -> ()) m.signals) p.modules;
  (* The width of a flat net is RECORDED during the flatten (see sigw above),
     never inferred.  There used to be a fallback here that guessed it by
     string-matching the name against every wide signal in the program -- exact
     match, or a "__"-delimited suffix -- and taking the MAXIMUM.  That guess
     can only ever be too WIDE, it fails silently, and it produced a corrupt
     netlist: a legal EDIF identifier reuse (Vivado sanitises different
     originals to one identifier, and EDIF keeps port/instance/net namespaces
     apart) let a child's 32-bit port claim a top-level 1-bit net, so an FDRE
     came out with a 32-bit Q.  nextpnr duly built Q0..Q31 on the packed
     SLICE_FFX and the router died 9000 cells away with "No wire found for port
     Q31", which reads exactly like an SRL cascade fault.

     So there is no fallback.  A miss means a net was referenced that no module
     declared, which is a real defect in the caller -- fail here, where the net
     can still be named, rather than emit a plausible width and corrupt the
     netlist. *)
  let width_of_net nm =
    match Hashtbl.find_opt sigw nm with
    | Some w -> w
    | None ->
      failwith (Printf.sprintf
        "flatten_structural: no declared width for flat net %S -- it is \
         referenced by a primitive pin but no module declares it.  Widths are \
         recorded during the flatten and are never guessed from the name." nm) in
  let extra_signals =
    Hashtbl.fold (fun nm () acc ->
      let base, _ = parse_bit nm in
      if List.mem base port_names || List.mem nm port_names then acc
      else { name = nm;
             stype = BInt { width = width_of_net nm; signed = Unsigned };
             direction = `Internal;
             initial_value = None;
             attrs = [];
           } :: acc) referenced []
  in
  (* ERC (post-flatten): every primitive INPUT pin must read a DRIVEN net — a
     primitive OUTPUT pin, a top-level INPUT port, or a constant.  A driverless
     reader is a dropped driver from gate_map/flatten: it reads X in simulation
     and its cell is pruned in P&R (this silently killed the ibex hybrid CPU —
     obuf_<port> inputs left undriven by the __<bit> name-resolution miss).
     Enabled by SVS_FLATTEN_ERC/SVS_FLATTEN_DEBUG; the former also fails loudly. *)
  (if Sys.getenv_opt "SVS_FLATTEN_ERC" <> None
      || Sys.getenv_opt "SVS_FLATTEN_DEBUG" <> None then begin
    let out_pin p =
      (String.length p >= 2 && String.sub p 0 2 = "DO")
      || List.mem p [ "O"; "CO"; "Q"; "MC31" ] in
    let driven : (string, unit) Hashtbl.t = Hashtbl.create 4096 in
    List.iter (fun (s : bsignal) ->
      if s.direction = `Input then Hashtbl.replace driven s.name ()) port_signals;
    List.iter (fun c -> Hashtbl.replace driven c ())
      [ "VCC"; "GND"; "<const0>"; "<const1>" ];
    let rec mark = function
      | BVar n -> Hashtbl.replace driven n ()
      | BSlice { signal; _ } -> mark signal
      | BSelect { array; _ } -> mark array
      | BConcat es -> List.iter mark es
      | _ -> () in
    List.iter (fun (i : binstance) ->
      List.iter (fun (pin, e) -> if out_pin pin then mark e) i.port_connections) prims;
    let reads : (string, string) Hashtbl.t = Hashtbl.create 4096 in
    let rec rd inst = function
      | BVar n -> if not (Hashtbl.mem driven n) then Hashtbl.replace reads n inst
      | BSlice { signal = BVar n; _ } ->
          if not (Hashtbl.mem driven n) then Hashtbl.replace reads n inst
      | BSlice { signal; _ } -> rd inst signal
      | BSelect { array; _ } -> rd inst array
      | BConcat es -> List.iter (rd inst) es
      | _ -> () in
    List.iter (fun (i : binstance) ->
      List.iter (fun (pin, e) -> if not (out_pin pin) then rd i.inst_name e)
        i.port_connections) prims;
    let n = Hashtbl.length reads in
    if n > 0 then begin
      Printf.eprintf
        "[flatten-ERC] %d driverless net(s) feed primitive inputs \
         (dropped driver in gate_map/flatten):\n" n;
      let c = ref 0 in
      Hashtbl.iter (fun net reader ->
        if !c < 20 then (Printf.eprintf "  %s  read by %s\n" net reader; incr c))
        reads;
      if Sys.getenv_opt "SVS_FLATTEN_ERC" <> None then
        failwith (Printf.sprintf
          "flatten_structural ERC: %d driverless net(s) — see [flatten-ERC] above" n)
    end
  end);
  { top_mod with
    signals = port_signals @ extra_signals;
    instances = prims;
    processes = [];
  }
