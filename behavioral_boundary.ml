(* Replace every [binstance] with an uninterpreted-function
   boundary, used by [test_synth_equiv] at parent-module miter time.

   Compositional verification setup:

     1. Leaves (modules with no instances) are mitered first and
        proven equivalent by their full logic.
     2. Parents are mitered with their child instances replaced by
        [BCall { func = "<child_module>__<port>"; args }] on BOTH
        sides — the source (Verible-derived) bmodule and the cell-
        mapped (gate_netlist_to_behavioral-derived) bmodule.

   Because both sides have identical instance shapes (same module
   names, same inst names, same port connections — the cell-mapped
   side preserves the source hierarchy under hier_synth) the BCalls
   are textually equal on both sides and Z3's uninterpreted-function
   semantics gives them equal values.

   Slice-shaped output connections (e.g. four sboxes each driving
   one byte of `subword[31:0]`) are merged per-parent-var into a
   single [BAssign] of a [BConcat], so we don't need slice-write
   semantics in the encoder. *)

open Behavioral_ir

let bcall_name child_module port = child_module ^ "__" ^ port

(* Width-of helper that mirrors what z3_miter does — used to populate
   the [bcall_out_w] table with each output port's declared width. *)
let rec width_of_btype = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | BArray { element; size } -> size * width_of_btype element
  | _ -> 1

let signal_width (s : bsignal) = width_of_btype s.stype

(* For one instance, return the list of writes it does to parent
   wires.  Each write is (parent_var, msb_or_full, lsb_or_full,
   bcall) — we use msb/lsb = (-1, -1) to mean "writes the whole
   parent_var" (no slice). *)
type write_spec = {
  parent_var : string;
  msb : int;  (* -1 if full-bus *)
  lsb : int;  (* -1 if full-bus *)
  rhs : bexpr;
}

let debug = Sys.getenv_opt "BOUNDARY_DEBUG" <> None

let rec render_expr = function
  | BVar n -> n
  | BConst { value; width } -> Printf.sprintf "%d'd%d" width value
  | BSlice { signal; msb; lsb } ->
      Printf.sprintf "%s[%d:%d]" (render_expr signal) msb lsb
  | BConcat es -> "{" ^ String.concat "," (List.map render_expr es) ^ "}"
  | BBinOp _ | BUnOp _ | BCond _ | BSelect _ | BReplicate _ | BCall _ -> "?"

(* True iff the child has any [BSequential] process anywhere in its
   transitive instance closure — i.e. the boundary has internal
   state (FFs).  Treating stateful boundaries as pure uninterpreted
   functions creates spurious combinational loops at the parent
   level (a_reg→a_mux→a_reg via the BCall fixed-point), so we
   instead expose each sequential-child output as a fresh *primary
   input* on the parent and skip the driver.  The miter's input-
   matching constraint makes the outputs equal across designs,
   which is the depth-1 sequential check.

   We must walk the *transitive* closure so that source and cell-
   mapped sides agree on the classification.  GcdUnitDpathRTL on the
   source side has no own BSequentials (its FFs are inside RegEn
   children); on the cell side, gate_netlist_to_behavioral expands
   DFF_X1 cells to BSequentials inside the same bmodule.  Asymmetric
   classification breaks the miter.  *)
let is_sequential ~lookup_module (m : bmodule) =
  let visited = Hashtbl.create 8 in
  let rec walk (m : bmodule) =
    if Hashtbl.mem visited m.name then false
    else begin
      Hashtbl.add visited m.name ();
      List.exists (function BSequential _ -> true | _ -> false) m.processes
      || List.exists (fun (i : binstance) ->
          match lookup_module i.module_name with
          | Some child -> walk child
          | None -> false) m.instances
    end in
  walk m

let writes_for ~lookup_module (i : binstance)
    : write_spec list =
  match lookup_module i.module_name with
  | None -> []
  | Some (child : bmodule) when is_sequential ~lookup_module child ->
      (* Sequential child: don't generate any BCall driver.  The
         parent-side signals connected to the child's output ports
         will be promoted to primary inputs by [substitute_module]
         so they get equality-constrained across designs. *)
      let _ = child in []
  | Some (child : bmodule) ->
      let inputs =
        List.filter (fun (s : bsignal) -> s.direction = `Input) child.signals in
      let outputs =
        List.filter (fun (s : bsignal) -> s.direction = `Output) child.signals in
      (* Sort input ports alphabetically for canonical arg order on
         both sides of the miter — the source bmodule and the cell-
         mapped bmodule may declare ports in different orders. *)
      let inputs =
        List.sort (fun (a : bsignal) (b : bsignal) ->
          String.compare a.name b.name) inputs in
      let input_args =
        List.filter_map (fun (s : bsignal) ->
          List.assoc_opt s.name i.port_connections) inputs in
      if debug then begin
        Printf.eprintf "[boundary] inst %s : %s\n"
          i.inst_name child.name;
        List.iter2 (fun (s : bsignal) e ->
          Printf.eprintf "  in %s = %s\n" s.name (render_expr e)
        ) inputs input_args;
        List.iter (fun (s : bsignal) ->
          match List.assoc_opt s.name i.port_connections with
          | Some e -> Printf.eprintf "  out %s -> %s\n" s.name (render_expr e)
          | None -> ()
        ) outputs;
      end;
      List.filter_map (fun (out : bsignal) ->
        match List.assoc_opt out.name i.port_connections with
        | None -> None
        | Some conn ->
            let func = bcall_name child.name out.name in
            Hashtbl.replace Z3_miter.bcall_out_w func (signal_width out);
            let bcall = BCall { func; args = input_args } in
            (match conn with
             | BVar n -> Some { parent_var = n; msb = -1; lsb = -1; rhs = bcall }
             | BSlice { signal = BVar n; msb; lsb } ->
                 Some { parent_var = n;
                        msb = max msb lsb;
                        lsb = min msb lsb;
                        rhs = bcall }
             | _ -> None)
      ) outputs

(* Group writes by parent_var.  When a parent_var has only slice
   writes that together cover the entire signal, we emit a single
   [BAssign] with a [BConcat] (high-to-low) of the slice rhs's.  If
   coverage is partial or mixed full/slice, fall back to emitting one
   process per slice — the source-side bmodule would also emit
   partial coverage, so the miter still sees a structural match. *)
let merge_writes (m : bmodule) (specs : write_spec list) : bprocess list =
  let groups = Hashtbl.create 8 in
  List.iter (fun ws ->
    let cur = try Hashtbl.find groups ws.parent_var with Not_found -> [] in
    Hashtbl.replace groups ws.parent_var (ws :: cur)) specs;
  Hashtbl.fold (fun parent_var ws acc ->
    let total_w =
      match List.find_opt (fun (s : bsignal) -> s.name = parent_var) m.signals with
      | Some s -> signal_width s
      | None -> 0 in
    let any_full = List.exists (fun w -> w.msb = -1) ws in
    if any_full then
      (* Full-bus connection — only one of these per parent_var
         normally.  Emit each as its own assignment. *)
      List.fold_left (fun acc w ->
        BCombinational {
          name = "boundary_" ^ parent_var;
          sensitivity = [BAny];
          body = [BAssign { lhs = parent_var; rhs = w.rhs }];
        } :: acc) acc ws
    else begin
      (* All slice writes.  Sort high-to-low; if they cover [total_w]
         contiguously, build a single concat assignment. *)
      let sorted = List.sort (fun a b -> compare b.msb a.msb) ws in
      let covers_all =
        let rec check next_lsb = function
          | [] -> next_lsb = -1
          | w :: tl when w.msb = next_lsb -> check (w.lsb - 1) tl
          | _ -> false in
        match sorted with
        | first :: _ when first.msb = total_w - 1 ->
            check (first.msb) sorted
        | _ -> false in
      if covers_all then
        let concat = BConcat (List.map (fun w -> w.rhs) sorted) in
        BCombinational {
          name = "boundary_" ^ parent_var;
          sensitivity = [BAny];
          body = [BAssign { lhs = parent_var; rhs = concat }];
        } :: acc
      else
        (* Fall through: emit each slice as a partial write to the
           bare parent_var.  Both sides will do the same partial
           writes, so the miter still sees a match. *)
        List.fold_left (fun acc w ->
          BCombinational {
            name = "boundary_" ^ parent_var;
            sensitivity = [BAny];
            body = [BAssign { lhs = parent_var; rhs = w.rhs }];
          } :: acc) acc ws
    end
  ) groups []

(* Collect the parent-side names connected to the OUTPUT ports of
   every sequential child instance.  These will be promoted to
   primary inputs at the parent level. *)
let sequential_output_signals ~lookup_module (m : bmodule) : string list =
  List.concat_map (fun (i : binstance) ->
    match lookup_module i.module_name with
    | Some child when is_sequential ~lookup_module child ->
        let outputs =
          List.filter (fun (s : bsignal) -> s.direction = `Output)
            child.signals in
        List.filter_map (fun (out : bsignal) ->
          match List.assoc_opt out.name i.port_connections with
          | Some (BVar n) -> Some n
          | Some (BSlice { signal = BVar n; _ }) -> Some n
          | _ -> None) outputs
    | _ -> []) m.instances
  |> List.sort_uniq String.compare

let substitute_module ~lookup_module (m : bmodule) : bmodule =
  if m.instances = [] then m
  else
    let specs =
      List.concat_map (writes_for ~lookup_module) m.instances in
    let extra = merge_writes m specs in
    let promote = sequential_output_signals ~lookup_module m in
    let signals' =
      List.map (fun (s : bsignal) ->
        if List.mem s.name promote && s.direction = `Internal
        then { s with direction = `Input }
        else s
      ) m.signals in
    (* Drop any existing assignments to promoted signals — they're
       free primary inputs now and the miter's input-matching makes
       them equal across designs.  Without this, the leftover assign
       would re-constrain them and reintroduce the loop. *)
    let drop_assigns_to promoted body =
      List.filter (fun stmt ->
        match stmt with
        | BAssign { lhs; _ } when List.mem lhs promoted -> false
        | _ -> true) body in
    let processes' =
      List.map (fun p ->
        match p with
        | BCombinational c ->
            BCombinational { c with body = drop_assigns_to promote c.body }
        | BSequential s ->
            BSequential { s with body = drop_assigns_to promote s.body }
      ) m.processes in
    { m with
      signals = signals';
      processes = processes' @ extra;
      instances = [];
    }

let substitute_program (p : bprogram) : bprogram =
  let by_name = Hashtbl.create 16 in
  List.iter (fun (m : bmodule) -> Hashtbl.replace by_name m.name m) p.modules;
  let lookup_module name = Hashtbl.find_opt by_name name in
  { p with modules = List.map (substitute_module ~lookup_module) p.modules }
