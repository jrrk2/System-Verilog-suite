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

let rec rewrite_bexpr ~prefix ~(port_actual : string -> bexpr option) (e : bexpr) : bexpr =
  match e with
  | BVar nm when is_const_sentinel nm -> e
  | BVar nm ->
      let base, idx_opt = parse_bit nm in
      (match port_actual base, idx_opt with
       | Some actual, None        -> actual
       | Some actual, Some bit    -> bit_select actual bit
       | None,        _           -> BVar (pname prefix nm))
  | BSelect { array; index } ->
      let array' = rewrite_bexpr ~prefix ~port_actual array in
      (match array', index with
       | BConcat es, BConst { value; _ } ->
           (* MSB-first concat: bit i is at position (width-1 - i).        *)
           let w = List.length es in
           if value >= 0 && value < w then List.nth es (w - 1 - value)
           else BSelect { array = array'; index }
       | _ -> BSelect { array = array'; index })
  | BConst _ -> e
  | BConcat es ->
      BConcat (List.map (rewrite_bexpr ~prefix ~port_actual) es)
  | BSlice { signal; msb; lsb } ->
      (* hardcaml_to_behavioral emits BSlice on output-port bits after
       * regrouping per-bit Circuit.t outputs into vector ports.        *)
      let signal' = rewrite_bexpr ~prefix ~port_actual signal in
      BSlice { signal = signal'; msb; lsb }
  | _ -> e   (* other forms shouldn't appear in a structural net *)

(* Pull `bit` out of `expr`.  Used when a child's port-bit reference must
   route through to the parent's actual net for that bit. *)
and bit_select (expr : bexpr) (bit : int) : bexpr =
  match expr with
  | BVar nm ->
      (* Parent's actual is a flat scalar net name; append bracket. *)
      BVar (Printf.sprintf "%s[%d]" nm bit)
  | BConcat es ->
      let w = List.length es in
      if bit >= 0 && bit < w then List.nth es (w - 1 - bit)
      else expr
  | BSelect _ -> expr  (* parent already bit-selected; assume scalar *)
  | _         -> expr

(* Flatten a bmodule: return the list of primitive binstances reachable
   from `m`, name-prefixed and port-rewritten so all references resolve
   in the top-level (caller's) scope. *)
let rec flatten_module ~by_name ~prefix (m : bmodule)
                      ~(port_actual : string -> bexpr option) : binstance list =
  List.concat_map (fun (i : binstance) ->
    let new_inst_name = pname prefix i.inst_name in
    match Hashtbl.find_opt by_name i.module_name with
    | None ->
        (* Primitive cell — rewrite its pin nets and emit. *)
        let pcs = List.map (fun (pin, expr) ->
          pin, rewrite_bexpr ~prefix ~port_actual expr) i.port_connections in
        [{ i with inst_name = new_inst_name; port_connections = pcs }]
    | Some child ->
        (* User-defined cell — recurse with the inst's port_connections
           rewritten through the parent's port_actual to give the child's
           pins their concrete parent-level nets. *)
        let inst_port_rewritten =
          List.map (fun (pin, expr) ->
            pin, rewrite_bexpr ~prefix ~port_actual expr)
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
        flatten_module ~by_name
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
  let prims = flatten_module ~by_name ~prefix:"" ~port_actual:(fun _ -> None) top_mod in

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
  let extra_signals =
    Hashtbl.fold (fun nm () acc ->
      let base, _ = parse_bit nm in
      if List.mem base port_names || List.mem nm port_names then acc
      else { name = nm;
             stype = BInt { width = 1; signed = Unsigned };
             direction = `Internal;
             initial_value = None;
             attrs = [];
           } :: acc) referenced []
  in
  { top_mod with
    signals = port_signals @ extra_signals;
    instances = prims;
    processes = [];
  }
