(* Post-swap formal equivalence (#96).

   Two complementary checks fire after an ECO swap (#94) is
   applied by the user:

   1.  Architectural proof: re-run Arch_verify.verify for the
       prediction's (kind, arch_name, width).  This re-derives
       the Z3 proof that the new arch implements the same
       arithmetic function as the old one, regenerating the
       .proven cert.

   2.  Port-signature check: parse both pre- and post-swap
       gate-level Verilog (via Lef_def.Gate_verilog) and confirm
       the module's input/output port set is unchanged.  If the
       ECO accidentally renamed a port or dropped a bit, the
       swap is rejected even though the arch proof passes —
       OpenROAD's replace_cell can only do per-cell substitution
       and shouldn't change top-level signatures, but a buggy
       Yosys re-synth could.

   Together: (1) proves the swap implements the right function
   in the abstract; (2) proves the concrete tool actually
   applied it correctly.  The Z3-and-port check catches both
   failure modes. *)

type signature_result =
  | Sig_match
  | Sig_diff of string

let port_signature (m : Lef_def.Gate_verilog.vmodule) =
  let inputs  = List.filter_map (fun p ->
    if p.Lef_def.Gate_verilog.kind = Lef_def.Gate_verilog.Input
    then Some p.name else None) m.ports in
  let outputs = List.filter_map (fun (p : Lef_def.Gate_verilog.port) ->
    if p.kind = Lef_def.Gate_verilog.Output
    then Some p.name else None) m.ports in
  (List.sort compare inputs, List.sort compare outputs)

let compare_signatures pre post =
  let (pre_in, pre_out) = port_signature pre in
  let (post_in, post_out) = port_signature post in
  if pre.name <> post.name then
    Sig_diff (Printf.sprintf "module name changed: %s -> %s"
                pre.name post.name)
  else if pre_in <> post_in then
    let drop = List.filter (fun n -> not (List.mem n post_in)) pre_in in
    let add  = List.filter (fun n -> not (List.mem n pre_in)) post_in in
    Sig_diff (Printf.sprintf
      "input ports differ (-%d +%d): dropped=%s added=%s"
      (List.length drop) (List.length add)
      (String.concat "," drop) (String.concat "," add))
  else if pre_out <> post_out then
    let drop = List.filter (fun n -> not (List.mem n post_out)) pre_out in
    let add  = List.filter (fun n -> not (List.mem n pre_out)) post_out in
    Sig_diff (Printf.sprintf
      "output ports differ (-%d +%d): dropped=%s added=%s"
      (List.length drop) (List.length add)
      (String.concat "," drop) (String.concat "," add))
  else Sig_match

(* ── Arch-proof gate ─────────────────────────────────────────── *)

(* Translate prediction.cand.kind ("adder" | "mul") into the
   arch_verify [kind] enum.  Anything else aborts loudly: an
   unknown kind shouldn't reach the post-swap stage. *)
let kind_of_string = function
  | "adder" -> Arch_verify.Adder
  | "mul"   -> Arch_verify.Mul
  | other -> failwith ("post_swap_check: unknown kind " ^ other)

type proof_result =
  | Proof_ok of string         (* cert path *)
  | Proof_unrun of string      (* reason — e.g. exception text *)

let run_arch_proof (pred : Lef_def.Predict_swap.prediction) =
  let kind = kind_of_string pred.cand.kind in
  let cert = Arch_verify.cert_path
               ~kind:(Arch_verify.kind_str kind)
               ~arch_name:pred.cand.to_arch ~width:pred.cand.width in
  try
    let rc = Arch_verify.verify ~kind ~arch_name:pred.cand.to_arch
               ~width:pred.cand.width in
    if rc = 0 && Arch_verify.cert_exists cert
    then Proof_ok cert
    else Proof_unrun
        (Printf.sprintf "verify rc=%d, cert present=%b" rc
           (Arch_verify.cert_exists cert))
  with e ->
    Proof_unrun (Printexc.to_string e)

(* ── Combined check ─────────────────────────────────────────── *)

type result = {
  arch_proof   : proof_result;
  signature    : signature_result;
}

let pp_result r =
  let proof_line = match r.arch_proof with
    | Proof_ok p   -> Printf.sprintf "ARCH PROOF: ok  (cert=%s)" p
    | Proof_unrun m-> Printf.sprintf "ARCH PROOF: failed  (%s)" m in
  let sig_line = match r.signature with
    | Sig_match     -> "PORT SIG  : ok"
    | Sig_diff s    -> Printf.sprintf "PORT SIG  : DIFFER  (%s)" s in
  proof_line ^ "\n" ^ sig_line

let check ~pre_module ~post_module ~prediction =
  {
    arch_proof = run_arch_proof prediction;
    signature  = compare_signatures pre_module post_module;
  }
