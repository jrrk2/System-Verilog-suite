(* Hierarchical synth driver (#103a).

   Synth each [bmodule] to gate level *separately*, in dependency
   order — leaves before parents.  Hardcaml never sees a parent's
   instances; instead, for each child instance encountered in a
   parent module, we add phantom IOs to the parent's hardcaml view:

     - child's output port  → phantom *input* on the parent
     - child's input  port  → phantom *output* on the parent

   At cell-Verilog emit time we then splice the child instance line
   back in alongside the parent's cells.  This preserves the dual
   hier/flat representation called for in
   project_hier_synth_architecture.md.

   Limitations of the first version:
     - port_connections must be [BVar name] (single signal per port).
       Concats / slices / constants on a child port → fail loudly.
     - param overrides on instances ignored (we synth the named
       child module directly; param specialisation is a follow-up).
     - bus widths on phantom signals come from the child module's
       declared port width, looked up by name; no implicit padding.

   When a leaf fails to lower (e.g. width-inference miss in
   Behavioral_to_hardcaml or unsupported BIR construct), we report
   which module + raise — the synth as a whole then fails loudly,
   per the no-silent-fallback rule.  *)

open Behavioral_ir

(* ── Output structure ──────────────────────────────────────────── *)

type child_inst_emit = {
  ci_module : string;
  ci_inst   : string;
  ci_conns  : (string * string) list;  (* port name, parent net *)
}

type module_netlist = {
  mn_name        : string;
  mn_real_inputs : (string * int) list;  (* declared input ports *)
  mn_real_outputs: (string * int) list;  (* declared output ports *)
  mn_netlist     : Lib_map.netlist;
  mn_child_insts : child_inst_emit list;
}

(* ── Helpers ───────────────────────────────────────────────────── *)

let signal_width (s : bsignal) =
  match s.stype with
  | BInt { width; _ } -> width
  | BBool -> 1
  | _ -> 1  (* fallback; complex types should be elaborated by now *)

let lookup_module name modules =
  List.find_opt (fun (m : bmodule) -> m.name = name) modules

(* Topological sort, children before parents. *)
let topo_sort (modules : bmodule list) : bmodule list =
  let visited = Hashtbl.create 16 in
  let temp = Hashtbl.create 16 in
  let order = ref [] in
  let rec visit (m : bmodule) =
    if Hashtbl.mem visited m.name then ()
    else if Hashtbl.mem temp m.name then
      failwith (Printf.sprintf "module dependency cycle at %s" m.name)
    else begin
      Hashtbl.add temp m.name ();
      List.iter (fun (i : binstance) ->
        match lookup_module i.module_name modules with
        | Some child -> visit child
        | None -> ()  (* unknown — treat as black box *)
      ) m.instances;
      Hashtbl.remove temp m.name;
      Hashtbl.add visited m.name ();
      order := m :: !order
    end in
  List.iter visit modules;
  List.rev !order

(* Render a child-pin connection expression to Verilog net text.
   Returns also the list of underlying signal names referenced
   (so the caller can promote them to phantom IO).  Sliced and
   concatenated connections come out as Verilog `n[hi:lo]` and
   `{a, b, …}` literally — the parent's hardcaml view sees the
   underlying signals as whole, the slice/concat lives only in the
   child instance line. *)
let rec render_conn_expr (parent_name : string) (port : string) (e : bexpr)
  : string * string list
  =
  match e with
  | BVar n -> (n, [n])
  | BSlice { signal; msb; lsb } ->
      let inner, deps = render_conn_expr parent_name port signal in
      let hi = max msb lsb and lo = min msb lsb in
      let s =
        if hi = lo then Printf.sprintf "%s[%d]" inner hi
        else Printf.sprintf "%s[%d:%d]" inner hi lo in
      (s, deps)
  | BConcat parts ->
      let rendered = List.map (render_conn_expr parent_name port) parts in
      let texts = List.map fst rendered in
      let deps = List.concat_map snd rendered in
      ("{" ^ String.concat ", " texts ^ "}", deps)
  | BConst { value; width } ->
      (Printf.sprintf "%d'b%s" width
         (let buf = Buffer.create width in
          for i = width - 1 downto 0 do
            Buffer.add_char buf (if (value lsr i) land 1 = 1 then '1' else '0')
          done; Buffer.contents buf), [])
  | BBinOp { op; lhs; rhs; _ } ->
      let l, ld = render_conn_expr parent_name port lhs in
      let r, rd = render_conn_expr parent_name port rhs in
      let opv = match op with
        | BAdd -> "+" | BSub -> "-" | BMul -> "*" | BDiv -> "/" | BMod -> "%"
        | BAnd -> "&" | BOr  -> "|" | BXor -> "^"
        | BShl -> "<<" | BShr -> ">>" | BAshr -> ">>>"
        | BEq -> "==" | BNe -> "!=" | BLt -> "<" | BLe -> "<="
        | BGt -> ">" | BGe -> ">="
      in
      (Printf.sprintf "(%s %s %s)" l opv r, ld @ rd)
  | BUnOp { op; operand; _ } ->
      let o, od = render_conn_expr parent_name port operand in
      let opv = match op with
        | BNot -> "~" | BNeg -> "-"
        | BRedAnd -> "&" | BRedOr -> "|" | BRedXor -> "^"
      in
      (Printf.sprintf "(%s%s)" opv o, od)
  | BCond { condition; then_val; else_val } ->
      let c, cd = render_conn_expr parent_name port condition in
      let t, td = render_conn_expr parent_name port then_val in
      let f, fd = render_conn_expr parent_name port else_val in
      (Printf.sprintf "(%s ? %s : %s)" c t f, cd @ td @ fd)
  | _ ->
      failwith (Printf.sprintf
        "hier_synth: parent %s's child connection on port %s is too \
         complex — only Var/Slice/Concat/Const/BinOp/UnOp/Cond supported \
         on instance pins"
        parent_name port)

let net_of_conn parent port e =
  let txt, _deps = render_conn_expr parent port e in txt

(* For each child instance, build the splice record + the list of
   (parent-side-signal, width, direction) phantom IO it adds.

   For non-trivial connection expressions (slices, concats), the
   parent's hardcaml view promotes the *underlying* signal names —
   not the slice/concat text.  The slice/concat lives only in the
   child instance line.  Width for the promotion comes from the
   parent's signal declaration, not the child's port — this matters
   when a parent signal is shared across multiple children (e.g.
   `subword` driven 8 bits at a time by 4 sboxes). *)
let child_inst_phantoms
    ~(parent : bmodule) ~(modules : bmodule list)
    (i : binstance)
  : child_inst_emit
    * (string * int * [ `In | `Out ]) list  (* phantom IO additions *)
  =
  let child = match lookup_module i.module_name modules with
    | Some c -> c
    | None ->
        failwith (Printf.sprintf
          "hier_synth: parent %s instantiates unknown module %s"
          parent.name i.module_name) in
  let ci_conns_with_deps =
    List.map (fun (port, e) ->
      let txt, deps = render_conn_expr parent.name port e in
      (port, txt, deps)) i.port_connections in
  let ci_conns =
    List.map (fun (port, txt, _) -> (port, txt)) ci_conns_with_deps in
  let phantoms =
    List.concat_map (fun (port, _txt, deps) ->
      let cs = List.find (fun (s : bsignal) -> s.name = port) child.signals in
      let dir =
        match cs.direction with
        | `Output -> `In
        | `Input  -> `Out
        | `Internal ->
            failwith (Printf.sprintf
              "hier_synth: child %s port %s has direction Internal — invalid"
              i.module_name port) in
      List.filter_map (fun dep_name ->
        match List.find_opt (fun (s : bsignal) -> s.name = dep_name)
                parent.signals with
        | Some s -> Some (dep_name, signal_width s, dir)
        | None ->
            (* Signal not declared at parent — uncommon but could
               happen if the parent uses a constant or an implicit
               wire.  Fall back to the child port's width. *)
            Some (dep_name, signal_width cs, dir)
      ) deps
    ) ci_conns_with_deps in
  { ci_module = i.module_name; ci_inst = i.inst_name; ci_conns }, phantoms

(* When a wire is the OUTPUT of one child instance and the INPUT of
   another within the same parent (an adder chain, a multiplier
   reduction, etc.), [child_inst_phantoms] computes both [`In] and
   [`Out] phantom directions for it and the downstream
   [find_opt] in [prepare_parent] picks only the first.  Hardcaml
   then either rejects the inconsistent declaration or silently
   miswires the second consumer.

   The fix is the same buffering convention that production hier
   flows use anyway: split the chain wire into producer-side [w]
   and consumer-side [w__hb] with an explicit pass-through assign
   in the parent, so each net has a single phantom direction.  We
   detect the bidirectional pattern here and rewrite the consuming
   instances' [port_connections] before phantom IO is computed.
   The buffer assign process is added to the parent's body so the
   parent's hardcaml view sees [w__hb] as a regular driven wire. *)
let auto_buffer_chain_wires
    (parent : bmodule) (modules : bmodule list) : bmodule =
  (* For each instance and each connection-dep, decide whether the
     dep is the producer (child output) or consumer (child input)
     side. *)
  let role_at_dep =
    List.concat_map (fun (i : binstance) ->
      match lookup_module i.module_name modules with
      | None -> []
      | Some child ->
          List.concat_map (fun (port, expr) ->
            let _txt, deps = render_conn_expr parent.name port expr in
            let cs =
              try Some (List.find
                          (fun (s : bsignal) -> s.name = port)
                          child.signals)
              with Not_found -> None in
            match cs with
            | None -> []
            | Some cs ->
                let role =
                  match cs.direction with
                  | `Output -> `Producer
                  | `Input  -> `Consumer
                  | `Internal -> `Other in
                List.map (fun d -> (d, role, i.inst_name, port)) deps
          ) i.port_connections) parent.instances in
  (* Bidirectional names: appear as both Producer and Consumer. *)
  let bidir =
    let producers = List.filter_map (fun (n, r, _, _) ->
      if r = `Producer then Some n else None) role_at_dep in
    let consumers = List.filter_map (fun (n, r, _, _) ->
      if r = `Consumer then Some n else None) role_at_dep in
    List.filter (fun n -> List.mem n consumers) producers
    |> List.sort_uniq compare in
  if bidir = [] then parent
  else begin
    let buf_name n = n ^ "__hb" in
    (* Rewrite consumer-side instance port connections: any occurrence
       of [BVar n] (or BSlice/BConcat referencing [n]) on a consumer
       port gets [n] replaced by [buf_name n].  Producer ports are
       untouched. *)
    let rec rewrite_expr e =
      match e with
      | BVar n when List.mem n bidir -> BVar (buf_name n)
      | BVar _ | BConst _ -> e
      | BSlice { signal; msb; lsb } ->
          BSlice { signal = rewrite_expr signal; msb; lsb }
      | BConcat es -> BConcat (List.map rewrite_expr es)
      | BSelect { array; index } ->
          BSelect { array = rewrite_expr array;
                    index = rewrite_expr index }
      | BBinOp r -> BBinOp { r with lhs = rewrite_expr r.lhs;
                                    rhs = rewrite_expr r.rhs }
      | BUnOp r -> BUnOp { r with operand = rewrite_expr r.operand }
      | BCond { condition; then_val; else_val } ->
          BCond { condition = rewrite_expr condition;
                  then_val = rewrite_expr then_val;
                  else_val = rewrite_expr else_val }
      | BReplicate r -> BReplicate { r with value = rewrite_expr r.value }
      | BCall r -> BCall { r with args = List.map rewrite_expr r.args }
    in
    let new_instances = List.map (fun (i : binstance) ->
      match lookup_module i.module_name modules with
      | None -> i
      | Some child ->
          let port_connections' = List.map (fun (port, expr) ->
            let cs =
              try Some (List.find
                          (fun (s : bsignal) -> s.name = port)
                          child.signals)
              with Not_found -> None in
            match cs with
            | Some cs when cs.direction = `Input ->
                (port, rewrite_expr expr)
            | _ -> (port, expr)
          ) i.port_connections in
          { i with port_connections = port_connections' }
    ) parent.instances in
    (* Add a buffer signal + pass-through process per bidir wire. *)
    let new_sigs =
      List.map (fun n ->
        let cur =
          try Some (List.find (fun (s : bsignal) -> s.name = n)
                      parent.signals)
          with Not_found -> None in
        match cur with
        | Some s -> { s with name = buf_name n; direction = `Internal }
        | None ->
            (* Fall back to a 1-bit wire if the source bmodule didn't
               declare the wire — caller's bug we shouldn't paper over,
               but a 1-bit fallback is the existing convention in
               [child_inst_phantoms]. *)
            { name = buf_name n; stype = BBool; direction = `Internal;
              initial_value = None; attrs = [] }
      ) bidir in
    let buffer_procs =
      List.map (fun n ->
        BCombinational {
          name = "auto_buffer_" ^ n;
          sensitivity = [BAny];
          body = [BAssign { lhs = buf_name n; rhs = BVar n }];
        }) bidir in
    { parent with
      signals  = parent.signals @ new_sigs;
      instances = new_instances;
      processes = parent.processes @ buffer_procs }
  end

(* Take a parent's bmodule, fold in phantom ports for child instances,
   and return: (synth-able bmodule, child instance splice list,
   set of phantom-IO names).  The synth-able view has m.instances=[]
   so [Behavioral_to_hardcaml.create_circuit] doesn't choke. *)
(* OpenSTA's read_verilog (and most netlist consumers) only accept
   simple expressions on instance pin connections — bare identifiers,
   slices, concats and constants.  When picosoc instantiates
     picosoc_mem #(.WORDS(MEM_WORDS)) memory (
       .wen( (mem_valid && !mem_ready &&
              mem_addr < 4*MEM_WORDS) ? mem_wstrb : 4'b0 ), ...);
   the expression on .wen has to come out of the parent already
   computed, with the instance pin reading a single wire.

   We hoist any non-simple pin expression into a fresh internal
   signal `__hoist_<inst>_<port>_<n>` and add an `assign` to the
   parent's body so the original instance line shrinks to
       .wen(__hoist_memory_wen_1)
   which OpenSTA happily reads.                                     *)
let is_simple_pin_expr = function
  | BVar _ | BSlice _ | BConcat _ | BConst _ -> true
  | _ -> false

let auto_hoist_complex_pins (parent : bmodule) (modules : bmodule list)
  : bmodule
  =
  let counter = ref 0 in
  let extra_signals = ref [] in
  let extra_assigns = ref [] in
  let lookup_port_w mod_name port =
    match lookup_module mod_name modules with
    | None -> 32
    | Some c ->
        (try
          let s = List.find (fun (s : bsignal) -> s.name = port) c.signals in
          signal_width s
         with Not_found -> 32)
  in
  let san s =
    String.map (fun c ->
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
         || (c >= '0' && c <= '9') || c = '_' then c
      else '_') s
  in
  let process_inst (i : binstance) =
    let conns' = List.map (fun (port, e) ->
      if is_simple_pin_expr e then (port, e)
      else begin
        incr counter;
        let w = lookup_port_w i.module_name port in
        let nm = Printf.sprintf "__hoist_%s_%s_%d"
          (san i.inst_name) (san port) !counter in
        let stype =
          if w = 1 then BBool else BInt { width = w; signed = Unsigned } in
        let s : bsignal =
          { name = nm; stype; direction = `Internal;
            initial_value = None; attrs = [] } in
        extra_signals := s :: !extra_signals;
        extra_assigns := BAssign { lhs = nm; rhs = e } :: !extra_assigns;
        (port, BVar nm)
      end
    ) i.port_connections in
    { i with port_connections = conns' }
  in
  let new_insts = List.map process_inst parent.instances in
  if !extra_signals = [] then parent
  else begin
    let proc =
      BCombinational {
        name = "__hoisted_pin_assigns";
        sensitivity = [ BAny ];
        body = List.rev !extra_assigns;
      } in
    { parent with
      signals = parent.signals @ List.rev !extra_signals;
      instances = new_insts;
      processes = parent.processes @ [proc];
    }
  end

let prepare_parent (parent : bmodule) (modules : bmodule list) =
  let parent = auto_hoist_complex_pins parent modules in
  let parent = auto_buffer_chain_wires parent modules in
  let child_results =
    List.map (child_inst_phantoms ~parent ~modules) parent.instances in
  let child_insts  = List.map fst child_results in
  let phantom_decl = List.concat_map snd child_results in
  (* Promote phantoms in the signal list, but never demote real ports.
     A phantom that names an existing real port is a no-op (the port
     already has the right direction). *)
  let signals' =
    List.map (fun (s : bsignal) ->
      match List.find_opt (fun (n, _, _) -> n = s.name) phantom_decl with
      | None -> s
      | Some (_, _, dir) ->
          (match s.direction, dir with
           | `Input,  _    -> s
           | `Output, _    -> s
           | `Internal, `In  -> { s with direction = `Input }
           | `Internal, `Out -> { s with direction = `Output })
    ) parent.signals in
  (* Add brand-new signals for phantom names not in the original list. *)
  let extra_signals =
    List.filter_map (fun (n, w, dir) ->
      if List.exists (fun (s : bsignal) -> s.name = n) signals' then None
      else Some {
        name = n;
        stype = (if w = 1 then BBool else BInt { width = w; signed = Unsigned });
        direction = (match dir with `In -> `Input | `Out -> `Output);
        initial_value = None;
        attrs = [];
      }) phantom_decl in
  let synth_bmod = {
    parent with
    signals = signals' @ extra_signals;
    instances = [];
  } in
  let phantom_names =
    List.fold_left (fun acc (n, _, _) -> n :: acc) [] phantom_decl
    |> List.sort_uniq compare in
  synth_bmod, child_insts, phantom_names

(* ── Per-module synth ─────────────────────────────────────────── *)

let synth_one ~(modules : bmodule list) (m : bmodule) : module_netlist =
  let real_inputs =
    List.filter_map (fun (s : bsignal) ->
      if s.direction = `Input then Some (s.name, signal_width s) else None
    ) m.signals in
  let real_outputs =
    List.filter_map (fun (s : bsignal) ->
      if s.direction = `Output then Some (s.name, signal_width s) else None
    ) m.signals in
  (* Modules tagged sv_decomp_blackbox=1 (e.g. memlower-emitted
     OpenRAM macro stubs) have no body to synthesise — return a
     no-cells netlist with the IO wired through, just enough for
     parent modules' phantom-IO promotion to find the port shape. *)
  let is_blackbox =
    try List.assoc "sv_decomp_blackbox" m.attrs = "1" with Not_found -> false in
  if is_blackbox then
    {
      mn_name = m.name;
      mn_real_inputs = real_inputs;
      mn_real_outputs = real_outputs;
      mn_netlist = Lib_map.{
        inputs = real_inputs;
        outputs = real_outputs;
        wires = [];
        insts = [];
        assigns = [];
      };
      mn_child_insts = [];
    }
  else
  let synth_bmod, child_insts, _phantom_names =
    if m.instances = []
    then m, [], []
    else prepare_parent m modules
  in
  let circuit =
    try Behavioral_to_hardcaml.create_circuit synth_bmod
    with e ->
      let bt = Printexc.get_backtrace () in
      if Sys.getenv_opt "HIER_SYNTH_DUMP_SIGS" <> None then begin
        Printf.eprintf "[hier_synth] %s signals at failure:\n" m.name;
        List.iter (fun (s : bsignal) ->
          Printf.eprintf "  %s : %s w=%d %s\n"
            (match s.direction with `Input -> "in"
             | `Output -> "out" | `Internal -> "int")
            s.name (signal_width s)
            (String.concat " "
               (List.map (fun (k, v) -> k ^ "=" ^ v) s.attrs))
        ) synth_bmod.signals
      end;
      (* Graceful-degradation: when HIER_SYNTH_STUB_ON_FAIL=1 is set,
         emit a stub module (declared ports, outputs tied to zero, no
         body) instead of failing the whole flow.  Lets downstream
         flows make progress on the rest of the design while specific
         modules are debugged.                                       *)
      if Sys.getenv_opt "HIER_SYNTH_STUB_ON_FAIL" = Some "1" then begin
        Printf.eprintf "[hier_synth] STUB: %s — %s\n%!"
          m.name (Printexc.to_string e);
        let stub : bmodule = {
          synth_bmod with
          processes = [];
          instances = [];
          mems = [];
          funcs = [];
          (* Keep declared ports, remove internals — outputs will be
             driven by Signal.zero via the create_circuit "Output that's
             never written becomes a zero" path. *)
          signals = List.filter (fun (s : bsignal) ->
            s.direction <> `Internal) synth_bmod.signals;
        } in
        try Behavioral_to_hardcaml.create_circuit stub
        with e2 ->
          failwith (Printf.sprintf
            "hier_synth: stub fallback ALSO failed for %s: %s"
            m.name (Printexc.to_string e2))
      end else
      failwith (Printf.sprintf "hier_synth: lowering failed for module %s: %s\n%s"
                  m.name (Printexc.to_string e) bt)
  in
  (* Set the Block_tag scope so every cell minted by Lib_map carries
     this module's hash in its inst name.  Hier_synth.synth_program
     calls synth_one once per module in topo order (children first);
     each module gets a clean modhash slot.  The Block_tag.reset call
     up in [synth_program] zeroes the per-program block-id counter so
     blocks get a stable global id across the whole synth run.       *)
  Block_tag.set_current_module m.name;
  let raw = Lib_map.map_circuit circuit in
  (* DCE on the per-module netlist, gated behind LIB_MAP_DCE=1.
     Backward-reachability sweep from outputs + child-instance pin
     nets; cells whose outputs aren't read are pruned.  Targets
     dead logic that the bit-blasted arith blocks introduce —
     gen_lt_brent_kung computes the full sum array but only the
     cout is consumed, so ~W XOR cells per LT block are dead by
     construction.  Across picosoc's eight wide LTs that's a
     meaningful cell-count drop with no timing change.

     ON by default.  Set SV_DECOMP_NO_DCE=1 to skip — useful for
     tiny designs (gcd) whose post-DCE area falls below the
     platform's PDN minimum core width (#111).  The legacy
     LIB_MAP_DCE=1 still flips it ON explicitly for back-compat
     with test rigs that set it deliberately.                       *)
  let netlist =
    let want_dce =
      Sys.getenv_opt "SV_DECOMP_NO_DCE" <> Some "1" in
    if want_dce then
      let child_pin_nets =
        List.concat_map (fun (c : child_inst_emit) ->
          List.concat_map (fun (_p, net) -> Lib_map.extract_idents net) c.ci_conns
        ) child_insts in
      let root_nets = List.map fst real_outputs @ child_pin_nets in
      Lib_map.dce ~root_nets raw
    else
      raw
  in
  {
    mn_name = m.name;
    mn_real_inputs = real_inputs;
    mn_real_outputs = real_outputs;
    mn_netlist = netlist;
    mn_child_insts = child_insts;
  }

(* ── Top-level driver ─────────────────────────────────────────── *)

let synth_program (prog : bprogram) : module_netlist list =
  Block_tag.reset ();
  let ordered = topo_sort prog.modules in
  List.map (synth_one ~modules:prog.modules) ordered
