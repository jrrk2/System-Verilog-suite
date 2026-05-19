(* Hierarchical architecture substitution (#81).
 *
 * For each `binstance` whose target module carries an
 * `(* sv_decomp_adder = "<arch>" *)` or `(* sv_decomp_mul = "<arch>" *)`
 * attribute AND has a corresponding leaf certificate from
 * `verify-arch`, replace the instance with a single BBinOp BAdd /
 * BMul driving the output port. The parent miter then encodes the
 * abstract operator instead of bit-blasting the gate-level prefix
 * tree / Wallace tree, so verification scales with the number of
 * abstracted blocks rather than their internal complexity.
 *
 * Soundness: the certificate is the proof obligation. Without it,
 * substitution refuses (the instance survives, the parent miter
 * either flattens it via Behavioral_hier or fails loudly on the
 * unbound module body). Stale certificates aren't auto-detected —
 * users re-run `verify-arch <kind> <arch> --width N` to regenerate.
 *
 * The pass is opt-out via env `SUBST_OFF=1` so users can compare
 * the abstracted-encoding miter result against the bit-blasted one. *)

open Behavioral_ir

let cache_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  home ^ "/.cache/sv_suite/arch"

let cert_exists ~kind ~arch_name ~width =
  let p = Printf.sprintf "%s/%s_%s_%d.proven"
            (cache_dir ()) kind arch_name width in
  Sys.file_exists p

(* Look up sv_decomp_{adder,mul} on a module. Returns
 * Some (kind, arch_name) or None. *)
let arch_of_module (m : bmodule) =
  let lookup k = List.assoc_opt k m.attrs in
  match lookup "sv_decomp_adder", lookup "sv_decomp_mul" with
  | Some a, _ -> Some ("adder", a)
  | _, Some a -> Some ("mul", a)
  | None, None -> None

(* Width inference for an instance — derived from its actual port
 * connections. The port the abstract op drives is the OUTPUT pin;
 * its width is the bit-width of the actual destination expression
 * (a BVar in parent scope). *)
let width_of_actual ?(default = 32) (parent : bmodule) actual =
  let rec lookup name =
    match List.find_opt (fun (s : bsignal) -> s.name = name) parent.signals with
    | Some s ->
        (match s.stype with
         | BInt { width; _ } -> Some width
         | BArray { size; element = BInt { width; _ }; _ } -> Some (size * width)
         | _ -> None)
    | None -> None
  and width = function
    | BVar n -> lookup n
    | BConst { width; _ } -> Some width
    | BSlice { msb; lsb; _ } -> Some (msb - lsb + 1)
    | BConcat es ->
        let ws = List.filter_map width es in
        if List.length ws = List.length es then
          Some (List.fold_left (+) 0 ws)
        else None
    | _ -> None
  in
  match width actual with
  | Some w -> w
  | None -> default

(* Identify the adder/mul shape inside a child module: two inputs and
 * one output, all the same width. The substitution drives the output
 * actual from `BBinOp <op> input1 input2` where input1/input2 are
 * the parent's actuals on the corresponding input ports.
 *
 * Returns Some (output_pin_name, in1_pin_name, in2_pin_name) or None
 * if the shape doesn't match. *)
let pin_layout (child : bmodule) =
  let inputs = List.filter_map (fun (s : bsignal) ->
    match s.direction with `Input  -> Some s.name | _ -> None) child.signals in
  let outputs = List.filter_map (fun (s : bsignal) ->
    match s.direction with `Output -> Some s.name | _ -> None) child.signals in
  match inputs, outputs with
  | [a; b], [y]    -> Some (y, a, b)
  | _              -> None

(* Build the BCombinational replacement process for an abstracted
 * arithmetic instance. *)
let make_subst_process ~op ~width ~inst_name
                       ~out_actual_name ~in1_actual ~in2_actual =
  let bool_w = BInt { width; signed = Unsigned } in
  BCombinational {
    name = inst_name ^ "_arch_subst";
    sensitivity = [BAny];
    body = [
      BAssign {
        lhs = out_actual_name;
        rhs = BBinOp { op; lhs = in1_actual; rhs = in2_actual;
                       result_type = bool_w };
      }
    ];
  }

(* For one parent module, walk its instances and substitute each one
 * whose target module is an attributed arch with a fresh certificate.
 * Returns the rewritten parent + a count of substitutions performed. *)
let substitute_in_module ~prog (parent : bmodule) : bmodule * int =
  if Sys.getenv_opt "SUBST_OFF" <> None then (parent, 0)
  else
    let by_name = Hashtbl.create 32 in
    List.iter (fun m -> Hashtbl.replace by_name m.name m) prog.modules;
    let extra_processes = ref [] in
    let count = ref 0 in
    let kept_insts = List.filter (fun (i : binstance) ->
      match Hashtbl.find_opt by_name i.module_name with
      | None -> true                           (* external — leave alone *)
      | Some child ->
          (match arch_of_module child with
           | None -> true                      (* not arch-attributed *)
           | Some (kind, arch_name) ->
               match pin_layout child with
               | None ->
                   Printf.eprintf
                     "[arch-subst] %s: %s shape isn't 2-in/1-out (skip)\n"
                     i.inst_name i.module_name;
                   true
               | Some (out_pin, in1_pin, in2_pin) ->
                   let out_actual =
                     List.assoc_opt out_pin i.port_connections in
                   let in1_actual =
                     List.assoc_opt in1_pin i.port_connections in
                   let in2_actual =
                     List.assoc_opt in2_pin i.port_connections in
                   match out_actual, in1_actual, in2_actual with
                   | Some (BVar out_name), Some e1, Some e2 ->
                       let width =
                         width_of_actual parent (BVar out_name) in
                       if not (cert_exists ~kind ~arch_name ~width) then begin
                         Printf.eprintf
                           "[arch-subst] %s: no cert for %s/%s/%d (keep)\n"
                           i.inst_name kind arch_name width;
                         true
                       end else begin
                         let op = match kind with
                           | "adder" -> BAdd
                           | "mul"   -> BMul
                           | _ -> assert false in
                         let p = make_subst_process ~op ~width
                                   ~inst_name:i.inst_name
                                   ~out_actual_name:out_name
                                   ~in1_actual:e1 ~in2_actual:e2 in
                         extra_processes := p :: !extra_processes;
                         incr count;
                         false
                       end
                   | _ -> true)
    ) parent.instances in
    let new_parent =
      { parent with
        instances = kept_insts;
        processes = parent.processes @ List.rev !extra_processes } in
    (new_parent, !count)

(* Whole-program pass: substitute in every module that hosts a
 * substitutable instance. *)
let substitute_program (p : bprogram) : bprogram * int =
  let total = ref 0 in
  let mods = List.map (fun m ->
    let m', n = substitute_in_module ~prog:p m in
    total := !total + n;
    m'
  ) p.modules in
  ({ p with modules = mods }, !total)
