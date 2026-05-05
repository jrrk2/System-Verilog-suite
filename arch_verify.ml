(* Per-architecture leaf verification (#80).
 *
 * Generates a SystemVerilog implementation of an arithmetic operator
 * for a chosen architecture (via Hardcaml_circuits), generates the
 * behavioral spec (`a + b` / `a * b`), runs the boundary-preserving
 * Z3 miter, and on success caches a tiny certificate file:
 *
 *   $HOME/.cache/sv_decompiler/arch/adder_brent_kung_32.proven
 *
 * The certificate is consumed by the parent miter (#81) when a design
 * carries `(* sv_decomp_adder = "brent_kung" *)` on a 32-bit signal:
 * if the cert exists, the instance is abstracted to BBinOp BAdd
 * before Z3 encoding. Missing/stale cert ⇒ fail loudly. *)

open Behavioral_ir

(* ──────────────────────────────────────────────────────────────────
 * Architecture enums *)

type adder_arch =
  | Ripple                                     (* SV `a + b` baseline *)
  | Sklansky
  | Brent_kung
  | Kogge_stone

type mul_arch =
  | Mul_ripple                                 (* SV `a * b` baseline *)
  | Wallace                                    (* via Hardcaml_circuits.Mul *)
  | Dadda

let adder_arch_of_string = function
  | "ripple"      -> Ripple
  | "sklansky"    -> Sklansky
  | "brent_kung"  -> Brent_kung
  | "kogge_stone" -> Kogge_stone
  | s -> failwith ("unknown adder arch: " ^ s)

let mul_arch_of_string = function
  | "ripple"  -> Mul_ripple
  | "wallace" -> Wallace
  | "dadda"   -> Dadda
  | s -> failwith ("unknown mul arch: " ^ s)

let string_of_adder_arch = function
  | Ripple      -> "ripple"
  | Sklansky    -> "sklansky"
  | Brent_kung  -> "brent_kung"
  | Kogge_stone -> "kogge_stone"

let string_of_mul_arch = function
  | Mul_ripple -> "ripple"
  | Wallace    -> "wallace"
  | Dadda      -> "dadda"

(* ──────────────────────────────────────────────────────────────────
 * Implementation: emit SV via Hardcaml *)

let emit_adder_verilog ~arch ~width =
  let open Hardcaml in
  let a    = Signal.input "a" width in
  let b    = Signal.input "b" width in
  let cin  = Signal.gnd in
  let s =
    match arch with
    | Ripple ->
        (* The reference: same width as inputs, no carry-out. *)
        Signal.( +: ) a b
    | Sklansky | Brent_kung | Kogge_stone ->
        let config = match arch with
          | Sklansky    -> Hardcaml_circuits.Prefix_sum.Config.Sklansky
          | Brent_kung  -> Hardcaml_circuits.Prefix_sum.Config.Brent_kung
          | Kogge_stone -> Hardcaml_circuits.Prefix_sum.Config.Kogge_stone
          | _ -> assert false in
        (* Prefix_sum returns N+1 bits (sum + carry-out). Drop the MSB
         * to align with `+:` semantics (truncated to N). *)
        let s_full = Hardcaml_circuits.Prefix_sum.create
          (module Signal) ~config
          ~input1:a ~input2:b ~carry_in:cin in
        Signal.select s_full (width - 1) 0
  in
  let circ = Circuit.create_exn ~name:"arch_op"
               [ Signal.output "s" s ] in
  let buf = Buffer.create 4096 in
  Rtl.output ~output_mode:(Rtl.Output_mode.To_buffer buf)
    Rtl.Language.Verilog circ;
  Buffer.contents buf

let emit_mul_verilog ~arch ~width =
  let open Hardcaml in
  let a = Signal.input "a" width in
  let b = Signal.input "b" width in
  let s =
    match arch with
    | Mul_ripple ->
        (* `*:` truncates to N bits — match this for the spec too. *)
        Signal.select Signal.(a *: b) (width - 1) 0
    | Wallace | Dadda ->
        let config = match arch with
          | Wallace -> Hardcaml_circuits.Mul.Config.Wallace
          | Dadda   -> Hardcaml_circuits.Mul.Config.Dadda
          | _ -> assert false in
        let s_full =
          Hardcaml_circuits.Mul.create ~config
            (module Signal) a b in
        Signal.select s_full (width - 1) 0
  in
  let circ = Circuit.create_exn ~name:"arch_op"
               [ Signal.output "s" s ] in
  let buf = Buffer.create 4096 in
  Rtl.output ~output_mode:(Rtl.Output_mode.To_buffer buf)
    Rtl.Language.Verilog circ;
  Buffer.contents buf

(* ──────────────────────────────────────────────────────────────────
 * Spec: trivial `assign s = a + b` / `assign s = a * b` *)

let emit_spec_verilog ~op ~width =
  Printf.sprintf
    "module arch_op (\n\
    \  input  [%d:0] a,\n\
    \  input  [%d:0] b,\n\
    \  output [%d:0] s\n\
     );\n\
    \  assign s = a %s b;\n\
     endmodule\n"
    (width - 1) (width - 1) (width - 1) op

(* ──────────────────────────────────────────────────────────────────
 * Cache *)

let cache_dir () =
  let home = try Sys.getenv "HOME" with Not_found -> "/tmp" in
  let d = home ^ "/.cache/sv_decompiler/arch" in
  let _ = Sys.command (Printf.sprintf "mkdir -p %s"
                         (Filename.quote d)) in
  d

let cert_path ~kind ~arch_name ~width =
  Printf.sprintf "%s/%s_%s_%d.proven" (cache_dir ()) kind arch_name width

let write_cert path =
  let oc = open_out path in
  Printf.fprintf oc "proven %s\n"
    (let t = Unix.localtime (Unix.time ()) in
     Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02dZ"
       (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
       t.tm_hour t.tm_min t.tm_sec);
  close_out oc

let cert_exists path = Sys.file_exists path

(* ──────────────────────────────────────────────────────────────────
 * Top-level verifier *)

type kind = Adder | Mul

(* Public: emit the architectural block as standalone SystemVerilog
 * with the matching `(* sv_decomp_<kind> *)` attribute on the
 * module declaration, so a parent design can instantiate it and
 * the substitution pass picks it up via the certificate. *)
let emit ~kind ~arch_name ~width ~out_path =
  let attr_key = match kind with Adder -> "sv_decomp_adder"
                                | Mul   -> "sv_decomp_mul" in
  let body =
    match kind with
    | Adder ->
        emit_adder_verilog ~arch:(adder_arch_of_string arch_name) ~width
    | Mul ->
        emit_mul_verilog ~arch:(mul_arch_of_string arch_name) ~width
  in
  (* Hardcaml emits `module arch_op (...);` — splice the attribute
   * onto the module header so it survives source-level parsing into
   * the bmodule's attrs (Sv_attr_extract picks it up there). *)
  let header = Printf.sprintf "(* %s = \"%s\" *)\nmodule arch_op"
                 attr_key arch_name in
  let body = Str.replace_first
    (Str.regexp "module arch_op") header body in
  let oc = open_out out_path in
  output_string oc body;
  close_out oc

let kind_str = function Adder -> "adder" | Mul -> "mul"

let verify ~kind ~arch_name ~width =
  let cert = cert_path ~kind:(kind_str kind) ~arch_name ~width in
  Printf.printf "═══════════════════════════════════════════════════════\n";
  Printf.printf "  verify-arch  %s/%s/width=%d\n"
    (kind_str kind) arch_name width;
  Printf.printf "  cert path:   %s\n" cert;
  Printf.printf "═══════════════════════════════════════════════════════\n\n";

  if cert_exists cert then begin
    Printf.printf "  certificate already present — re-running anyway\n\n"
  end;

  let impl_sv =
    match kind with
    | Adder ->
        emit_adder_verilog ~arch:(adder_arch_of_string arch_name) ~width
    | Mul ->
        emit_mul_verilog ~arch:(mul_arch_of_string arch_name) ~width
  in
  let spec_sv =
    let op = match kind with Adder -> "+" | Mul -> "*" in
    emit_spec_verilog ~op ~width
  in

  let tmpdir = Filename.temp_dir "sv_arch_" "" in
  let impl_path = Filename.concat tmpdir "impl.v" in
  let spec_path = Filename.concat tmpdir "spec.v" in
  if Sys.getenv_opt "ARCH_KEEP" <> None then
    Printf.printf "  (ARCH_KEEP=1)  impl: %s\n  (ARCH_KEEP=1)  spec: %s\n"
      impl_path spec_path;
  let oc = open_out impl_path in output_string oc impl_sv; close_out oc;
  let oc = open_out spec_path in output_string oc spec_sv; close_out oc;

  Printf.printf "[1/3] impl SV (Hardcaml-emitted) → BIR …\n%!";
  (* Hardcaml emits old-style port-decl Verilog with leading-underscore
   * synthetic wires (_4, _5, …). Verible's BIR converter loses widths
   * on the old port style, so route through the same ansi/identifier
   * rewriter that `gate-miter` uses for yosys output. *)
  let impl_clean =
    Gate_netlist_to_behavioral.preprocess_gate_file impl_path in
  let p_impl =
    Verible_to_behavioral.convert_files
      ~top:"arch_op" [impl_clean] in
  Printf.printf "  %d modules\n" (List.length p_impl.modules);

  Printf.printf "[2/3] spec SV (`a %s b`) → BIR …\n%!"
    (match kind with Adder -> "+" | Mul -> "*");
  let p_spec =
    Verible_to_behavioral.convert_files
      ~top:"arch_op" [spec_path] in
  Printf.printf "  %d modules\n" (List.length p_spec.modules);

  let pick label src =
    match List.find_opt (fun (m : bmodule) -> m.name = "arch_op")
            src with
    | Some m -> m
    | None ->
        Printf.eprintf "%s: no module 'arch_op'. Available: %s\n"
          label
          (String.concat ", "
             (List.map (fun (m : bmodule) -> m.name) src));
        exit 1
  in
  let prep p =
    let m = pick "arch" p.modules in
    if m.instances = [] then m
    else Behavioral_hier.flatten_for_z3 p ~top:"arch_op"
  in
  let mi = prep p_impl in
  let ms = prep p_spec in

  Printf.printf "[3/3] Z3 miter …\n";
  let ok = Z3_miter.check_miter_equivalence mi ms in
  if Sys.getenv_opt "ARCH_KEEP" = None then begin
    (try Sys.remove impl_path with _ -> ());
    (try Sys.remove spec_path with _ -> ());
    (try Unix.rmdir tmpdir with _ -> ())
  end;
  if ok then begin
    write_cert cert;
    Printf.printf "\n  ✅ PROVEN — certificate written to %s\n" cert;
    0
  end else begin
    Printf.printf "\n  ❌ NOT EQUIVALENT — no certificate written\n";
    1
  end
