(* Constrained-random SystemVerilog testcase generator.
 *
 * Emits small synthesizable modules that exercise the patterns the
 * Verilator↔Verible miter cares about. The basic mode covers ports +
 * combinational/sequential assigns; --features adds advanced
 * synthesisable constructs (functions, structs, generate, 2-D memory,
 * etc.) one mode at a time.
 *
 * Usage:
 *   random_sv_gen.exe [--seed N] [--n K] [--out DIR] [--keep-pass]
 *                     [--features simple,struct,func,gen,mem2d,mixed]
 *
 * For each generated case:
 *   1. Pick a mode (round-robin if --features=mixed; otherwise the
 *      single mode given)
 *   2. Write rand_<seed>.sv to <DIR>/work/
 *   3. Run test_verilator_vs_verible.exe
 *   4. If it fails, copy the .sv to <DIR>/found/rand_<seed>.sv and
 *      log the seed + the first error line to <DIR>/found/INDEX
 *
 * `--keep-pass` also preserves passing cases. *)

let seed = ref 1
let n_cases = ref 50
let out_dir = ref "/tmp/random_sv"
let keep_pass = ref false
let only_emit = ref false  (* skip the miter, just emit the .sv *)
let features_str = ref "simple"

let () =
  let speclist = [
    "--seed", Arg.Set_int seed, "<N> initial RNG seed (default 1)";
    "--n", Arg.Set_int n_cases, "<K> number of cases (default 50)";
    "--out", Arg.Set_string out_dir, "<DIR> output dir (default /tmp/random_sv)";
    "--keep-pass", Arg.Set keep_pass, "preserve passing .sv files too";
    "--emit-only", Arg.Set only_emit, "just emit the .sv to stdout (debug)";
    "--features", Arg.Set_string features_str,
      "<spec> simple|struct|func|gen|mem2d|mixed (comma-list ok)";
  ] in
  Arg.parse speclist (fun _ -> ()) "random_sv_gen [opts]"

let active_modes =
  String.split_on_char ',' !features_str |> List.map String.trim
  |> List.filter (fun s -> s <> "")

(* ─── RNG primitives ─────────────────────────────────────────────── *)

let rand_int n = Random.int n
let pick lst = List.nth lst (rand_int (List.length lst))
let coin () = rand_int 2 = 0
let weighted_pick (weighted : (int * 'a) list) : 'a =
  let total = List.fold_left (fun s (w, _) -> s + w) 0 weighted in
  let r = rand_int total in
  let rec go acc = function
    | [] -> failwith "weighted_pick empty"
    | (w, x) :: tl -> if r < acc + w then x else go (acc + w) tl
  in
  go 0 weighted

(* ─── Type / signal model ────────────────────────────────────────── *)

type direction = Input | Output
type port = { p_name : string; p_width : int; p_dir : direction }

let dir_str = function Input -> "input" | Output -> "output"

(* Sample a width — bias toward the small/common widths but allow up
 * to 64 occasionally for stress. *)
let gen_width () =
  weighted_pick [
    20, 1;
    20, 2;
    15, 4;
    15, 8;
    10, 16;
    10, 32;
     5, 5;       (* odd width *)
     5, 7;       (* odd width *)
  ]

(* Names: a, b, c, …  (avoid SV keywords). *)
let alpha = "abcdefghijklmnopqrstuvwxyz"
let gen_name n =
  let rec mk i =
    let c = alpha.[i mod 26] in
    if i < 26 then String.make 1 c
    else mk (i / 26 - 1) ^ String.make 1 c
  in
  mk n

(* ─── Port-list shape ────────────────────────────────────────────── *)

(* A "group" is a comma-separated set of ports that share a direction
 * and width: e.g. `input [3:0] a, b, c`. We generate a list of
 * groups, then flatten to per-port records. *)
type group = { g_dir : direction; g_width : int; g_names : string list }

let gen_groups ~n_inputs ~n_outputs =
  let names_left = ref [] in
  for i = n_outputs + n_inputs - 1 downto 0 do
    names_left := gen_name i :: !names_left
  done;
  let take_n n =
    let rec go acc = function
      | 0, rest -> (List.rev acc, rest)
      | k, x :: rest -> go (x :: acc) (k - 1, rest)
      | _, [] -> (List.rev acc, [])
    in
    let (taken, rest) = go [] (n, !names_left) in
    names_left := rest;
    taken
  in
  (* Outputs first (so inputs declared by groups can reference them on
   * RHS only after the input-group split — actually output names just
   * need to exist; they're written to in assigns). *)
  let mk_groups dir total =
    let rec build acc remaining =
      if remaining = 0 then List.rev acc
      else
        let group_size = min remaining (1 + rand_int 3) in
        let names = take_n group_size in
        let g = { g_dir = dir; g_width = gen_width (); g_names = names } in
        build (g :: acc) (remaining - group_size)
    in
    build [] total
  in
  let out_g = mk_groups Output n_outputs in
  let in_g = mk_groups Input n_inputs in
  in_g @ out_g

let ports_of_groups gs =
  List.concat_map (fun g ->
    List.map (fun n -> { p_name = n; p_width = g.g_width; p_dir = g.g_dir })
      g.g_names
  ) gs

(* ─── Expression generator ───────────────────────────────────────── *)

(* Bias toward simpler shapes; depth-bound recursion. The width is
 * threaded so generated subexpressions match the LHS width — Verilator
 * complains less and the comparison stays meaningful. *)
type ctx = { in_ports : port list; depth : int }

let port_of_width ctx w =
  match List.filter (fun p -> p.p_width = w) ctx.in_ports with
  | [] -> List.nth ctx.in_ports (rand_int (List.length ctx.in_ports))
  | matches -> List.nth matches (rand_int (List.length matches))

let bin_op_str = function
  | 0 -> "+"  | 1 -> "-"  | 2 -> "&"  | 3 -> "|"
  | 4 -> "^"  | 5 -> "<<" | 6 -> ">>"
  | _ -> "+"

let rec gen_expr ctx target_w =
  if ctx.depth <= 0 then leaf_expr ctx target_w
  else weighted_pick [
    35, lazy (leaf_expr ctx target_w);
    20, lazy (bin_expr ctx target_w);
    10, lazy (ternary ctx target_w);
    10, lazy (slice_expr ctx target_w);
     5, lazy (concat_expr ctx target_w);
     5, lazy (literal target_w);
     5, lazy (unary_expr ctx target_w);
  ] |> Lazy.force

and leaf_expr ctx target_w =
  let p = port_of_width ctx target_w in
  if p.p_width = target_w then p.p_name
  else if p.p_width > target_w && p.p_width > 1 then
    (* Only slice multi-bit ports — `scalar[0:0]` is illegal SV. *)
    Printf.sprintf "%s[%d:0]" p.p_name (target_w - 1)
  else if p.p_width < target_w then
    (* Zero-extend by concatenation. *)
    Printf.sprintf "{{%d{1'b0}}, %s}" (target_w - p.p_width) p.p_name
  else
    (* Width mismatch we can't slice (1-bit port asked to fill >1 bits
     * with a leaf): fall back to a literal of the right width. *)
    literal target_w

and bin_expr ctx target_w =
  let op = bin_op_str (rand_int 7) in
  let inner = { ctx with depth = ctx.depth - 1 } in
  Printf.sprintf "(%s %s %s)"
    (gen_expr inner target_w) op (gen_expr inner target_w)

and ternary ctx target_w =
  let inner = { ctx with depth = ctx.depth - 1 } in
  let cond = gen_expr inner 1 in
  let a = gen_expr inner target_w in
  let b = gen_expr inner target_w in
  Printf.sprintf "(%s ? %s : %s)" cond a b

and slice_expr ctx target_w =
  (* Pick a port that is BOTH at-least target_w AND multi-bit (a 1-bit
   * scalar can't be sliced — Verilator errors with "Illegal bit
   * select"). *)
  let candidates =
    List.filter (fun p -> p.p_width >= target_w && p.p_width > 1)
      ctx.in_ports
  in
  match candidates with
  | [] -> leaf_expr ctx target_w
  | _ ->
      let p = List.nth candidates (rand_int (List.length candidates)) in
      let lo = rand_int (p.p_width - target_w + 1) in
      if target_w = 1 then
        Printf.sprintf "%s[%d]" p.p_name lo
      else
        Printf.sprintf "%s[%d:%d]" p.p_name (lo + target_w - 1) lo

and concat_expr ctx target_w =
  if target_w < 2 then leaf_expr ctx target_w
  else begin
    let split = 1 + rand_int (target_w - 1) in
    let inner = { ctx with depth = ctx.depth - 1 } in
    Printf.sprintf "{%s, %s}"
      (gen_expr inner (target_w - split))
      (gen_expr inner split)
  end

and literal target_w =
  let v = rand_int (1 lsl (min target_w 16)) in
  Printf.sprintf "%d'd%d" target_w v

and unary_expr ctx target_w =
  let inner = { ctx with depth = ctx.depth - 1 } in
  let op = pick ["~"; "-"] in
  Printf.sprintf "(%s%s)" op (gen_expr inner target_w)

(* ─── Module emitter ─────────────────────────────────────────────── *)

let emit_port_groups gs =
  let groups_str =
    List.map (fun g ->
      let dim =
        if g.g_width = 1 then ""
        else Printf.sprintf " [%d:0]" (g.g_width - 1)
      in
      Printf.sprintf "  %s logic%s %s"
        (dir_str g.g_dir) dim (String.concat ", " g.g_names)
    ) gs
  in
  String.concat ",\n" groups_str

let emit_module ~name ~include_clock seed =
  Random.init seed;
  let n_inputs = 2 + rand_int 4 in
  let n_outputs = 1 + rand_int 3 in
  let groups = gen_groups ~n_inputs ~n_outputs in
  let ports = ports_of_groups groups in
  let inputs = List.filter (fun p -> p.p_dir = Input) ports in
  let outputs = List.filter (fun p -> p.p_dir = Output) ports in
  let buf = Buffer.create 1024 in
  Buffer.add_string buf (Printf.sprintf "// random_sv_gen seed=%d\n" seed);
  Buffer.add_string buf (Printf.sprintf "module %s (\n" name);
  let port_decls =
    if include_clock then
      "  input  logic clk,\n  input  logic rst_n,\n"
      ^ emit_port_groups groups
    else
      emit_port_groups groups
  in
  Buffer.add_string buf port_decls;
  Buffer.add_string buf "\n);\n";
  let in_ports =
    if include_clock then
      { p_name = "rst_n"; p_width = 1; p_dir = Input } :: inputs
    else inputs
  in
  let ctx = { in_ports; depth = 3 } in
  (* Decide per-output: combinational assign vs registered always_ff. *)
  List.iter (fun (p : port) ->
    if include_clock && coin () then begin
      Buffer.add_string buf (Printf.sprintf
        "  logic [%d:0] %s_q;\n" (p.p_width - 1) p.p_name);
      Buffer.add_string buf (Printf.sprintf
        "  always_ff @(posedge clk) begin\n\
         \    if (!rst_n) %s_q <= '0;\n\
         \    else        %s_q <= %s;\n\
         \  end\n\
         \  assign %s = %s_q;\n"
        p.p_name p.p_name (gen_expr ctx p.p_width)
        p.p_name p.p_name)
    end else
      Buffer.add_string buf (Printf.sprintf
        "  assign %s = %s;\n" p.p_name (gen_expr ctx p.p_width))
  ) outputs;
  Buffer.add_string buf "endmodule\n";
  Buffer.contents buf

(* ─── Advanced module shapes (gated by --features) ───────────────── *)

(* Each emitter returns a complete `module name(...); ... endmodule`
 * string, given the seed. They share the simple-mode helpers (RNG,
 * gen_expr, gen_width) but build their own port surfaces tailored to
 * the construct under test. *)

(* Packed struct: define a type with two fields, take an input of that
 * type via concatenation of the constituent ports, and emit outputs
 * that bit-select the high and low halves. Exercises Verilator and
 * Verible's struct typedef + member access. *)
let emit_struct ~name seed =
  Random.init seed;
  let hi_w = gen_width () in
  let lo_w = gen_width () in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=struct\n\
     module %s (\n\
     \  input  logic [%d:0] hi_in,\n\
     \  input  logic [%d:0] lo_in,\n\
     \  output logic [%d:0] hi_out,\n\
     \  output logic [%d:0] lo_out\n\
     );\n\
     \  typedef struct packed {\n\
     \    logic [%d:0] hi;\n\
     \    logic [%d:0] lo;\n\
     \  } pair_t;\n\
     \  pair_t p;\n\
     \  assign p = '{hi: hi_in, lo: lo_in};\n\
     \  assign hi_out = p.hi;\n\
     \  assign lo_out = p.lo;\n\
     endmodule\n"
    seed name
    (hi_w-1) (lo_w-1) (hi_w-1) (lo_w-1)
    (hi_w-1) (lo_w-1)

(* Function: define a small synthesisable automatic function and call
 * it from a continuous assign. Body uses the basic expression
 * generator over the function's own parameter list. *)
let emit_func ~name seed =
  Random.init seed;
  let w = gen_width () in
  let pa = { p_name = "pa"; p_width = w; p_dir = Input } in
  let pb = { p_name = "pb"; p_width = w; p_dir = Input } in
  let ctx = { in_ports = [pa; pb]; depth = 2 } in
  let body = gen_expr ctx w in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=func\n\
     module %s (\n\
     \  input  logic [%d:0] a,\n\
     \  input  logic [%d:0] b,\n\
     \  output logic [%d:0] y\n\
     );\n\
     \  function automatic logic [%d:0] f (input logic [%d:0] pa, input logic [%d:0] pb);\n\
     \    f = %s;\n\
     \  endfunction\n\
     \  assign y = f(a, b);\n\
     endmodule\n"
    seed name (w-1) (w-1) (w-1) (w-1) (w-1) (w-1) body

(* Generate-for: produce N parallel logic-cells via a generate loop,
 * each with the same body. The output is a packed vector formed by
 * the cells' results. Stresses both elaborators' generate-loop
 * unrolling. *)
let emit_gen ~name seed =
  Random.init seed;
  let n = 2 + rand_int 6 in (* 2..7 cells *)
  Printf.sprintf
    "// random_sv_gen seed=%d mode=gen N=%d\n\
     module %s #(parameter N = %d) (\n\
     \  input  logic [N-1:0] a,\n\
     \  input  logic [N-1:0] b,\n\
     \  output logic [N-1:0] y\n\
     );\n\
     \  genvar i;\n\
     \  generate\n\
     \    for (i = 0; i < N; i = i + 1) begin : g_cell\n\
     \      assign y[i] = a[i] ^ b[i];\n\
     \    end\n\
     \  endgenerate\n\
     endmodule\n"
    seed n name n

(* 2-D unpacked memory (multi-dim mem): registered write into a row,
 * combinational read of a column. This is the essence of distributed
 * register-file patterns. *)
let emit_mem2d ~name seed =
  Random.init seed;
  let depth_log2 = 2 + rand_int 2 in (* 4..8 deep *)
  let cols_log2 = 1 + rand_int 2 in   (* 2..4 columns *)
  let dw = gen_width () in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=mem2d %dx%dx%d\n\
     module %s (\n\
     \  input  logic clk,\n\
     \  input  logic we,\n\
     \  input  logic [%d:0] row,\n\
     \  input  logic [%d:0] col,\n\
     \  input  logic [%d:0] din,\n\
     \  output logic [%d:0] dout\n\
     );\n\
     \  logic [%d:0] mem [0:%d][0:%d];\n\
     \  always_ff @(posedge clk)\n\
     \    if (we) mem[row][col] <= din;\n\
     \  assign dout = mem[row][col];\n\
     endmodule\n"
    seed (1 lsl depth_log2) (1 lsl cols_log2) dw
    name
    (depth_log2-1) (cols_log2-1) (dw-1) (dw-1)
    (dw-1) ((1 lsl depth_log2)-1) ((1 lsl cols_log2)-1)

(* Const-fn struct config — exercises the symbolic-execution path in
 * Verible_elaborate.eval_function. A package defines a constant
 * function returning a packed struct; the top module uses that as a
 * parameter default and instantiates two child modules whose widths
 * come from the struct's fields. The two child specialisations
 * MUST get distinct concrete widths or the elaborator missed the
 * field access. *)
let emit_cfg_struct ~name seed =
  Random.init seed;
  let wa = gen_width () in
  let wb = gen_width () in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=cfg_struct\n\
     package %s_pkg;\n\
     \  typedef struct packed {\n\
     \    int unsigned A;\n\
     \    int unsigned B;\n\
     \  } cfg_t;\n\
     \  function automatic cfg_t mk_cfg();\n\
     \    cfg_t cfg;\n\
     \    cfg.A = %d;\n\
     \    cfg.B = %d;\n\
     \    return cfg;\n\
     \  endfunction\n\
     endpackage\n\
     \n\
     module %s_child #(parameter int unsigned WIDTH = 8) (\n\
     \    input  logic [WIDTH-1:0] din,\n\
     \    output logic [WIDTH-1:0] dout\n\
     );\n\
     \  assign dout = din;\n\
     endmodule\n\
     \n\
     module %s\n\
     \  #(parameter %s_pkg::cfg_t Cfg = %s_pkg::mk_cfg())\n\
     (\n\
     \    input  logic [Cfg.A-1:0] in_a,\n\
     \    input  logic [Cfg.B-1:0] in_b,\n\
     \    output logic [Cfg.A-1:0] out_a,\n\
     \    output logic [Cfg.B-1:0] out_b\n\
     );\n\
     \  %s_child #(.WIDTH(Cfg.A)) u_a (.din(in_a), .dout(out_a));\n\
     \  %s_child #(.WIDTH(Cfg.B)) u_b (.din(in_b), .dout(out_b));\n\
     endmodule\n"
    seed name wa wb name name name name name name

(* Cross-package localparam chain: pkg A defines a base constant,
 * pkg B's const-fn references it via A::CONST. Exercises layer-4
 * cross-package localparam lookup. *)
let emit_cfg_chain ~name seed =
  Random.init seed;
  let base_w = gen_width () in
  let mult = 1 + rand_int 3 in
  let a_value = base_w in
  let b_value = base_w * mult in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=cfg_chain\n\
     package %s_base_pkg;\n\
     \  localparam int unsigned BaseW = %d;\n\
     \  localparam int unsigned ScaleW = %d;\n\
     endpackage\n\
     \n\
     package %s_cfg_pkg;\n\
     \  typedef struct packed {\n\
     \    int unsigned A;\n\
     \    int unsigned B;\n\
     \  } cfg_t;\n\
     \  function automatic cfg_t mk_cfg();\n\
     \    cfg_t cfg;\n\
     \    cfg.A = %s_base_pkg::BaseW;\n\
     \    cfg.B = %s_base_pkg::ScaleW;\n\
     \    return cfg;\n\
     \  endfunction\n\
     endpackage\n\
     \n\
     module %s_child #(parameter int unsigned WIDTH = 8) (\n\
     \    input  logic [WIDTH-1:0] din,\n\
     \    output logic [WIDTH-1:0] dout\n\
     );\n\
     \  assign dout = din;\n\
     endmodule\n\
     \n\
     module %s\n\
     \  #(parameter %s_cfg_pkg::cfg_t Cfg = %s_cfg_pkg::mk_cfg())\n\
     (\n\
     \    input  logic [Cfg.A-1:0] in_a,\n\
     \    input  logic [Cfg.B-1:0] in_b,\n\
     \    output logic [Cfg.A-1:0] out_a,\n\
     \    output logic [Cfg.B-1:0] out_b\n\
     );\n\
     \  %s_child #(.WIDTH(Cfg.A)) u_a (.din(in_a), .dout(out_a));\n\
     \  %s_child #(.WIDTH(Cfg.B)) u_b (.din(in_b), .dout(out_b));\n\
     endmodule\n"
    seed name a_value b_value name name name name name name name name name

(* Const-fn body with ternary chains: cfg.X depends on a pair of
 * inputs via nested ternaries. Stresses the parse_ternary path
 * and ensures the >= / <= / && / || ops survive deep_string_of_token. *)
let emit_cfg_ternary ~name seed =
  Random.init seed;
  (* Two arg-driven outputs: small_w if inputs are small, else big_w *)
  let pivot   = pick [4; 8; 16; 32] in
  let small_w = pick [2; 4] in
  let big_w   = pick [16; 32; 64] in
  let in_w    = if coin () then small_w else big_w in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=cfg_ternary\n\
     package %s_pkg;\n\
     \  typedef struct packed {\n\
     \    int unsigned IN_W;\n\
     \  } in_cfg_t;\n\
     \  typedef struct packed {\n\
     \    int unsigned OUT_W;\n\
     \  } out_cfg_t;\n\
     \  function automatic out_cfg_t mk_cfg(in_cfg_t in_cfg);\n\
     \    out_cfg_t cfg;\n\
     \    cfg.OUT_W = (in_cfg.IN_W >= %d)\n\
     \              ? ((in_cfg.IN_W >= %d) ? %d : %d)\n\
     \              : %d;\n\
     \    return cfg;\n\
     \  endfunction\n\
     endpackage\n\
     \n\
     module %s_child #(parameter int unsigned W = 8) (\n\
     \    input  logic [W-1:0] din,\n\
     \    output logic [W-1:0] dout\n\
     );\n\
     \  assign dout = din;\n\
     endmodule\n\
     \n\
     module %s\n\
     \  #(parameter %s_pkg::out_cfg_t Cfg =\n\
     \      %s_pkg::mk_cfg('{IN_W: %d}))\n\
     (\n\
     \    input  logic [Cfg.OUT_W-1:0] in_d,\n\
     \    output logic [Cfg.OUT_W-1:0] out_d\n\
     );\n\
     \  %s_child #(.W(Cfg.OUT_W)) u (.din(in_d), .dout(out_d));\n\
     endmodule\n"
    seed name pivot (pivot * 2) big_w small_w small_w name name name name in_w name

(* Recursive instantiation with parameter-driven base case: a small
 * popcount-shape that exercises generate-block pruning + child-name
 * rewriting + flatten-via-instance-tree. Width chosen to be a power
 * of two so the recursion tower is unambiguous. *)
let emit_cfg_recursive ~name seed =
  Random.init seed;
  let log2_w = 2 + rand_int 4 in   (* 2..5 → W = 4, 8, 16, 32 *)
  let w = 1 lsl log2_w in
  Printf.sprintf
    "// random_sv_gen seed=%d mode=cfg_recursive\n\
     module %s_rec #(parameter int unsigned W = 8) (\n\
     \    input  logic [W-1:0] data_i,\n\
     \    output logic [W-1:0] sum_o\n\
     );\n\
     \  if (W == 1) begin : g_leaf\n\
     \    assign sum_o = data_i;\n\
     \  end else begin : g_split\n\
     \    logic [W/2-1:0] lo, hi, lo_sum, hi_sum;\n\
     \    assign lo = data_i[W/2-1:0];\n\
     \    assign hi = data_i[W-1:W/2];\n\
     \    %s_rec #(.W(W/2)) u_lo (.data_i(lo), .sum_o(lo_sum));\n\
     \    %s_rec #(.W(W/2)) u_hi (.data_i(hi), .sum_o(hi_sum));\n\
     \    assign sum_o = {hi_sum, lo_sum};\n\
     \  end\n\
     endmodule\n\
     \n\
     module %s (\n\
     \    input  logic [%d:0] din,\n\
     \    output logic [%d:0] dout\n\
     );\n\
     \  %s_rec #(.W(%d)) u (.data_i(din), .sum_o(dout));\n\
     endmodule\n"
    seed name name name name (w-1) (w-1) name w

(* ─── Mode dispatcher ────────────────────────────────────────────── *)

let emit_for_mode ~name ~seed mode =
  match mode with
  | "struct" -> emit_struct ~name seed
  | "func"   -> emit_func ~name seed
  | "gen"    -> emit_gen ~name seed
  | "mem2d"  -> emit_mem2d ~name seed
  | "cfg_struct"    -> emit_cfg_struct ~name seed
  | "cfg_chain"     -> emit_cfg_chain ~name seed
  | "cfg_ternary"   -> emit_cfg_ternary ~name seed
  | "cfg_recursive" -> emit_cfg_recursive ~name seed
  | "simple" | _ ->
      let include_clock = (let _ = Random.init seed in coin ()) in
      emit_module ~name ~include_clock seed

(* ─── Test runner ────────────────────────────────────────────────── *)

let repo_root =
  (* Same directory layout as test/regressions/ — assume cwd is repo
   * root or test_verilator_vs_verible.exe is on $PATH. *)
  try Sys.getenv "REPO_ROOT"
  with Not_found -> "/home/jonathan/System-Verilog-suite"

let miter_exe =
  Filename.concat repo_root "_build/default/test_verilator_vs_verible.exe"

let mkdir_p p =
  if not (Sys.file_exists p) then ignore (Sys.command (Printf.sprintf "mkdir -p %s" p))

let pick_mode s =
  match active_modes with
  | [] -> "simple"
  | ["mixed"] ->
      let modes = ["simple"; "struct"; "func"; "gen"; "mem2d";
                   "cfg_struct"; "cfg_chain"; "cfg_ternary";
                   "cfg_recursive"] in
      List.nth modes (s mod List.length modes)
  | _ ->
      List.nth active_modes (s mod List.length active_modes)

let run_one ~work_dir ~found_dir s =
  let mode = pick_mode s in
  let name = Printf.sprintf "rand_%s_%d" mode s in
  let sv_path = Filename.concat work_dir (name ^ ".sv") in
  let log_path = Filename.concat work_dir (name ^ ".log") in
  let txt = emit_for_mode ~name ~seed:s mode in
  let oc = open_out sv_path in
  output_string oc txt; close_out oc;
  let cmd = Printf.sprintf "%s %s %s > %s 2>&1"
    miter_exe name sv_path log_path in
  let rc = Sys.command cmd in
  let ok = rc = 0 in
  if not ok then begin
    let dst = Filename.concat found_dir (name ^ ".sv") in
    ignore (Sys.command (Printf.sprintf "cp %s %s" sv_path dst));
    let why =
      try
        let ic = open_in log_path in
        let rec scan () =
          let l = input_line ic in
          if String.length l > 0 &&
             (try ignore (Str.search_forward
                  (Str.regexp "differ\\|NOT EQUIV\\|int_of_string\\|Fatal\\|Cannot\\|Error\\|Sorts")
                  l 0); true with Not_found -> false)
          then begin close_in ic; l end
          else scan ()
        in
        try scan () with End_of_file -> close_in ic; "(no diagnostic)"
      with _ -> "(no log)"
    in
    let oc = open_out_gen [Open_append; Open_creat] 0o644
                          (Filename.concat found_dir "INDEX") in
    Printf.fprintf oc "seed=%d  %s\n" s
      (String.sub why 0 (min 100 (String.length why)));
    close_out oc
  end;
  if !keep_pass || not ok then ()
  else Sys.remove sv_path;
  ok

let () =
  if !only_emit then begin
    let mode = pick_mode !seed in
    let name = Printf.sprintf "rand_%s_%d" mode !seed in
    print_string (emit_for_mode ~name ~seed:!seed mode);
    exit 0
  end;
  let work = Filename.concat !out_dir "work" in
  let found = Filename.concat !out_dir "found" in
  mkdir_p work;
  mkdir_p found;
  Printf.printf "random SV generator: %d cases starting at seed %d\n"
    !n_cases !seed;
  Printf.printf "  output dir: %s\n" !out_dir;
  let pass = ref 0 in
  let fail = ref 0 in
  for i = 0 to !n_cases - 1 do
    let s = !seed + i in
    if run_one ~work_dir:work ~found_dir:found s then incr pass
    else begin
      incr fail;
      Printf.printf "  ❌ seed %d failed\n%!" s
    end
  done;
  Printf.printf "\n=== summary ===\n";
  Printf.printf "  passed: %d / %d\n" !pass !n_cases;
  Printf.printf "  failed: %d / %d  (saved to %s)\n" !fail !n_cases found
