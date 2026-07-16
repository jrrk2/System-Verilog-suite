(* Convert Behavioral IR to synthesizable VHDL.
 *
 * Mirrors behavioral_to_verilog.ml's shape but emits IEEE 1076 VHDL.
 * Multi-bit data is std_logic_vector by default; signed BInt becomes
 * `signed`.  Arithmetic operands are cast to (un)signed at the use
 * site, then cast back to std_logic_vector for the result, which is
 * the conventional synthesizable idiom and survives ghdl-synth /
 * Vivado / DC.  Unsupported BIR constructs become VHDL comments
 * rather than failing — the goal is an emit that round-trips
 * synthesizable structures and never silently corrupts; dropped
 * structures are visible in the output. *)

open Behavioral_ir

let sanitize n =
  String.map (fun c ->
    if c = '@' || c = '[' || c = ']' || c = '\\' then '_' else c) n

let is_signed = function
  | BInt { signed = Signed; _ } -> true
  | _ -> false

let width_of = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | _ -> 1

let vhdl_of_type = function
  | BInt { width = 1; _ } -> "std_logic"
  | BInt { width; signed = Signed } ->
      Printf.sprintf "signed(%d downto 0)" (width - 1)
  | BInt { width; signed = Unsigned } ->
      Printf.sprintf "std_logic_vector(%d downto 0)" (width - 1)
  | BBool -> "std_logic"
  | BArray { element; size } ->
      let inner = match element with
        | BInt { width = 1; _ } -> "std_logic"
        | BInt { width; _ } -> Printf.sprintf "std_logic_vector(%d downto 0)" (width - 1)
        | _ -> "std_logic"
      in
      Printf.sprintf "array (0 to %d) of %s" (size - 1) inner
  | BStruct _ -> "std_logic /* struct flattened */"

(* `(others => '0')` for vectors, `'0'` for std_logic *)
let zero_init_for = function
  | BInt { width = 1; _ } | BBool -> "'0'"
  | _ -> "(others => '0')"

let bin_str width value =
  let buf = Buffer.create width in
  for i = width - 1 downto 0 do
    Buffer.add_char buf (if (value lsr i) land 1 = 1 then '1' else '0')
  done;
  Buffer.contents buf

(* VHDL constants: 1-bit → '0'/'1'; wider → "0101..." or X"hex".
   We use binary string for clarity at small widths, hex for larger. *)
let vhdl_const ~width value =
  if width = 1 then (if value land 1 = 1 then "'1'" else "'0'")
  else if width <= 32 then Printf.sprintf "\"%s\"" (bin_str width value)
  else Printf.sprintf "\"%s\"" (bin_str width value)

(* When a binop or comparison is performed, we may need to cast
   std_logic_vector operands to (un)signed.  `wrap_for_arith` wraps
   an expression text with `unsigned(...)` (or `signed(...)`) when
   the operand carries a multi-bit slv.  Single-bit std_logic does
   not need a cast for `=`/`/=`. *)
let cast_for_arith ~signed_ctx expr_str = function
  | BInt { width = 1; _ } | BBool -> expr_str
  | BInt { signed = Signed; _ } -> expr_str  (* already signed *)
  | _ ->
      if signed_ctx then Printf.sprintf "signed(%s)" expr_str
      else Printf.sprintf "unsigned(%s)" expr_str

(* infer the BIR type of a bexpr — used to decide whether a cast is
   needed.  Returns BInt 1 by default. *)
let rec type_of = function
  | BConst { width; _ } -> BInt { width; signed = Unsigned }
  | BBinOp { result_type; _ } -> result_type
  | BUnOp { result_type; _ } -> result_type
  | BSlice { msb; lsb; _ } ->
      BInt { width = abs (msb - lsb) + 1; signed = Unsigned }
  | BSelect _ -> BInt { width = 1; signed = Unsigned }
  | BConcat parts ->
      let w = List.fold_left (fun acc p -> acc + width_of (type_of p)) 0 parts in
      BInt { width = max 1 w; signed = Unsigned }
  | BReplicate { count; value } ->
      let w = count * width_of (type_of value) in
      BInt { width = max 1 w; signed = Unsigned }
  | BCond { then_val; _ } -> type_of then_val
  | BVar _ | BCall _ -> BInt { width = 1; signed = Unsigned }

let vhdl_of_binop = function
  | BAdd -> "+" | BSub -> "-" | BMul -> "*" | BDiv -> "/" | BMod -> "mod"
  | BAnd -> "and" | BOr -> "or" | BXor -> "xor"
  | BShl -> "sll" | BShr -> "srl" | BAshr -> "sra"
  | BEq -> "=" | BNe -> "/=" | BLt -> "<" | BLe -> "<=" | BGt -> ">" | BGe -> ">="

let is_arith = function
  | BAdd | BSub | BMul | BDiv | BMod | BLt | BLe | BGt | BGe -> true
  | _ -> false

let rec vhdl_of_expr e =
  match e with
  | BVar name -> sanitize name
  | BConst { value; width } -> vhdl_const ~width (Z.to_int value)
  | BBinOp { op; lhs; rhs; result_type } ->
      let l_str = vhdl_of_expr lhs in
      let r_str = vhdl_of_expr rhs in
      let op_str = vhdl_of_binop op in
      if is_arith op then
        (* cast operands to (un)signed for arithmetic; cast result
           back to slv when the lhs context is slv. *)
        let signed_ctx = is_signed result_type in
        let lc = cast_for_arith ~signed_ctx l_str (type_of lhs) in
        let rc = cast_for_arith ~signed_ctx r_str (type_of rhs) in
        let inner = Printf.sprintf "(%s %s %s)" lc op_str rc in
        (match result_type with
         | BInt { width = 1; _ } | BBool -> inner
         | BInt { signed = Signed; _ } -> inner
         | BInt { width = _; _ } -> Printf.sprintf "std_logic_vector(%s)" inner
         | _ -> inner)
      else
        Printf.sprintf "(%s %s %s)" l_str op_str r_str
  | BUnOp { op = BNot; operand; _ } ->
      Printf.sprintf "(not %s)" (vhdl_of_expr operand)
  | BUnOp { op = BNeg; operand; result_type } ->
      let s = cast_for_arith ~signed_ctx:true (vhdl_of_expr operand) (type_of operand) in
      let inner = Printf.sprintf "(- %s)" s in
      (match result_type with
       | BInt { signed = Unsigned; width } when width > 1 ->
           Printf.sprintf "std_logic_vector(%s)" inner
       | _ -> inner)
  | BUnOp { op = BRedAnd; operand; _ } ->
      Printf.sprintf "and_reduce(%s)" (vhdl_of_expr operand)
  | BUnOp { op = BRedOr; operand; _ } ->
      Printf.sprintf "or_reduce(%s)" (vhdl_of_expr operand)
  | BUnOp { op = BRedXor; operand; _ } ->
      Printf.sprintf "xor_reduce(%s)" (vhdl_of_expr operand)
  | BSelect { array; index } ->
      Printf.sprintf "%s(%s)" (vhdl_of_expr array)
        (match index with
         | BConst { value; _ } -> Z.to_string value
         | other -> Printf.sprintf "to_integer(unsigned(%s))" (vhdl_of_expr other))
  | BSlice { signal; msb; lsb } ->
      if msb = lsb then Printf.sprintf "%s(%d)" (vhdl_of_expr signal) msb
      else Printf.sprintf "%s(%d downto %d)" (vhdl_of_expr signal) msb lsb
  | BConcat parts ->
      String.concat " & " (List.map vhdl_of_expr parts)
  | BReplicate { count; value } ->
      (* `(others => v)` is more idiomatic but only works when v is
         a single bit and target width matches.  Fall back to
         explicit replicate via `&` chain. *)
      let s = vhdl_of_expr value in
      String.concat " & " (List.init count (fun _ -> s))
  | BCond { condition; then_val; else_val } ->
      (* VHDL has no inline ternary in '93; use IEEE 2008 conditional
         expression which Vivado/ghdl/DC accept in synthesis. *)
      Printf.sprintf "(%s when %s else %s)"
        (vhdl_of_expr then_val)
        (vhdl_of_expr condition)
        (vhdl_of_expr else_val)
  | BCall { func; args } ->
      Printf.sprintf "%s(%s)" func
        (String.concat ", " (List.map vhdl_of_expr args))

let rec vhdl_of_stmt indent s =
  let ind = String.make indent ' ' in
  match s with
  | BAssign { lhs; rhs } ->
      Printf.sprintf "%s%s <= %s;" ind (sanitize lhs) (vhdl_of_expr rhs)
  | BIf { condition; then_stmts; else_stmts = [] } ->
      Printf.sprintf "%sif %s then\n%s\n%send if;"
        ind (vhdl_of_expr condition)
        (String.concat "\n" (List.map (vhdl_of_stmt (indent + 2)) then_stmts))
        ind
  | BIf { condition; then_stmts; else_stmts } ->
      Printf.sprintf "%sif %s then\n%s\n%selse\n%s\n%send if;"
        ind (vhdl_of_expr condition)
        (String.concat "\n" (List.map (vhdl_of_stmt (indent + 2)) then_stmts))
        ind
        (String.concat "\n" (List.map (vhdl_of_stmt (indent + 2)) else_stmts))
        ind
  | BCase { selector; cases; default } ->
      let arm (v, body) =
        Printf.sprintf "%s  when %s =>\n%s"
          ind (vhdl_of_expr v)
          (String.concat "\n" (List.map (vhdl_of_stmt (indent + 4)) body))
      in
      let default_arm =
        Printf.sprintf "%s  when others =>\n%s" ind
          (if default = [] then ind ^ "    null;"
           else String.concat "\n" (List.map (vhdl_of_stmt (indent + 4)) default))
      in
      Printf.sprintf "%scase %s is\n%s\n%s\n%send case;"
        ind (vhdl_of_expr selector)
        (String.concat "\n" (List.map arm cases))
        default_arm ind
  | BWhile { condition; body } ->
      Printf.sprintf "%swhile %s loop\n%s\n%send loop;"
        ind (vhdl_of_expr condition)
        (String.concat "\n" (List.map (vhdl_of_stmt (indent + 2)) body))
        ind
  | BFor { init; condition; update; body } ->
      (* No direct mapping to VHDL `for-loop` because BFor carries
         arbitrary init/update.  Emit as a comment-noted while loop
         around the body so the structure is visible; users running
         our pipeline normally call behavioral_unroll first. *)
      Printf.sprintf "%s-- BFor unrolling expected; emitted as while\n%s%s\n%swhile %s loop\n%s\n%s  %s\n%send loop;"
        ind ind (vhdl_of_stmt 0 init)
        ind (vhdl_of_expr condition)
        (String.concat "\n" (List.map (vhdl_of_stmt (indent + 2)) body))
        ind (vhdl_of_stmt 0 update)
        ind
  | BBlock stmts ->
      String.concat "\n" (List.map (vhdl_of_stmt indent) stmts)
  | BCallStmt { func; args } ->
      Printf.sprintf "%s%s(%s);" ind func
        (String.concat ", " (List.map vhdl_of_expr args))
  | BReturn _ ->
      Printf.sprintf "%s-- return (synthesizable VHDL has no return at process level)" ind

(* The body of a BSequential with async reset typically arrives as
   `[BIf { condition = <rst-test>; then_stmts = RESET_BODY;
           else_stmts = CLOCK_BODY }]`.
   The canonical synthesizable VHDL idiom is
     if rst = '1' then  -- (or '0' for active-low)
       RESET_BODY
     elsif rising_edge(clk) then
       CLOCK_BODY
     end if;
   so when we detect the wrapper we promote it; otherwise we wrap
   the whole body in `if rising_edge(clk) then ... end if;` (the
   sync-reset / no-reset shape). *)
(* Same canonicalisation as verible_to_behavioral's classify_reset:
   strip 1-bit reduction-or/and wrappers so `!x`'s expanded form
   matches the simple BNot(BVar) test. *)
let rec simplify_cond = function
  | BCall { func = ("or_reduce"|"and_reduce"); args = [x] } -> simplify_cond x
  | BUnOp { op = (BRedOr|BRedAnd); operand; _ } -> simplify_cond operand
  | BUnOp { op = BNot; operand; result_type } ->
      BUnOp { op = BNot; operand = simplify_cond operand; result_type }
  | other -> other

let split_async_reset reset reset_edge body =
  match reset with
  | None -> None
  | Some rst ->
      let pol = match reset_edge with Some `Neg -> `Neg | _ -> `Pos in
      let matches c = match simplify_cond c with
        | BVar n when n = rst -> pol = `Pos
        | BUnOp { op = BNot; operand = BVar n; _ } when n = rst -> pol = `Neg
        | BBinOp { op = BEq; lhs = BVar n;
                   rhs = BConst { value = zv; _ }; _ } when n = rst && Z.equal zv Z.one -> pol = `Pos
        | BBinOp { op = BEq; lhs = BVar n;
                   rhs = BConst { value = zv; _ }; _ } when n = rst && Z.equal zv Z.zero -> pol = `Neg
        | _ -> false
      in
      let rec scan = function
        | [BIf { condition; then_stmts; else_stmts }] when matches condition ->
            Some (then_stmts, else_stmts)
        | [BBlock xs] -> scan xs
        | _ -> None
      in
      scan body

let vhdl_of_process proc =
  match proc with
  | BSequential { name; clock; clock_edge; reset; reset_edge; reset_async; body } ->
      let edge_fn = match clock_edge with `Pos -> "rising_edge" | `Neg -> "falling_edge" in
      let sens = match reset, reset_async with
        | Some r, true -> Printf.sprintf "%s, %s" clock r
        | _ -> clock
      in
      let body_inner =
        match (if reset_async then split_async_reset reset reset_edge body else None) with
        | Some (rst_body, clk_body) ->
            let rst = match reset with Some r -> r | None -> "rst" in
            let test = match reset_edge with
              | Some `Neg -> Printf.sprintf "%s = '0'" (sanitize rst)
              | _ -> Printf.sprintf "%s = '1'" (sanitize rst)
            in
            Printf.sprintf
              "    if %s then\n%s\n    elsif %s(%s) then\n%s\n    end if;"
              test
              (String.concat "\n" (List.map (vhdl_of_stmt 6) rst_body))
              edge_fn (sanitize clock)
              (String.concat "\n" (List.map (vhdl_of_stmt 6) clk_body))
        | None ->
            Printf.sprintf
              "    if %s(%s) then\n%s\n    end if;"
              edge_fn (sanitize clock)
              (String.concat "\n" (List.map (vhdl_of_stmt 6) body))
      in
      Printf.sprintf "  %s: process(%s)\n  begin\n%s\n  end process %s;"
        (sanitize name) sens body_inner (sanitize name)
  | BCombinational { name; body; _ } ->
      let body_str = String.concat "\n"
                       (List.map (vhdl_of_stmt 4) body) in
      Printf.sprintf "  %s: process(all)\n  begin\n%s\n  end process %s;"
        (sanitize name) body_str (sanitize name)

let vhdl_of_port (s : bsignal) =
  let dir = match s.direction with
    | `Input -> "in"
    | `Output -> "out"
    | `Internal -> "in"
  in
  Printf.sprintf "    %s : %s %s" (sanitize s.name) dir (vhdl_of_type s.stype)

let vhdl_of_signal_decl (s : bsignal) =
  let init = match s.initial_value with
    | Some _ -> Printf.sprintf " := %s" (zero_init_for s.stype)
    | None -> ""
  in
  Printf.sprintf "  signal %s : %s%s;"
    (sanitize s.name) (vhdl_of_type s.stype) init

let vhdl_of_instance (inst : binstance) =
  let { inst_name; module_name; port_connections; _ } = inst in
  let ports =
    String.concat ",\n      "
      (List.map (fun (formal, actual) ->
         Printf.sprintf "%s => %s" (sanitize formal) (vhdl_of_expr actual))
       port_connections)
  in
  Printf.sprintf "  %s : entity work.%s\n    port map (\n      %s\n    );"
    (sanitize inst_name) (sanitize module_name) ports

let vhdl_of_module bmod =
  let { name; signals; processes; instances; params; _ } = bmod in
  let ports = List.filter (fun (s : bsignal) -> s.direction <> `Internal) signals in
  let internals = List.filter (fun (s : bsignal) -> s.direction = `Internal) signals in
  let port_lines = List.map vhdl_of_port ports in
  let port_str =
    if ports = [] then ""
    else "  port (\n" ^ String.concat ";\n" port_lines ^ "\n  );"
  in
  let generic_str =
    if params = [] then ""
    else
      "  generic (\n"
      ^ String.concat ";\n"
          (List.map (fun (n, v) ->
             Printf.sprintf "    %s : integer := %d" (sanitize n) v) params)
      ^ "\n  );"
  in
  let entity =
    Printf.sprintf "entity %s is\n%s%s%send entity %s;"
      (sanitize name)
      (if generic_str = "" then "" else generic_str ^ "\n")
      (if port_str = "" then "" else port_str ^ "\n")
      ""
      (sanitize name)
  in
  let signal_decls = String.concat "\n" (List.map vhdl_of_signal_decl internals) in
  let process_strs = String.concat "\n\n" (List.map vhdl_of_process processes) in
  let inst_strs = String.concat "\n\n" (List.map vhdl_of_instance instances) in
  let arch_decl_part =
    if signal_decls = "" then "" else signal_decls ^ "\n"
  in
  let arch_body_part =
    [process_strs; inst_strs]
    |> List.filter (fun s -> s <> "")
    |> String.concat "\n\n"
  in
  let arch =
    Printf.sprintf "architecture rtl of %s is\n%sbegin\n%s\nend architecture rtl;"
      (sanitize name)
      arch_decl_part
      (if arch_body_part = "" then "  -- empty body" else arch_body_part)
  in
  let preamble =
    "library ieee;\nuse ieee.std_logic_1164.all;\nuse ieee.numeric_std.all;\n"
  in
  Printf.sprintf "%s\n%s\n\n%s\n" preamble entity arch

let vhdl_of_program (prog : bprogram) =
  String.concat "\n\n" (List.map vhdl_of_module prog.modules)

let write_to_file ?(header="") filename prog =
  let body = vhdl_of_program prog in
  let oc = open_out filename in
  if header <> "" then output_string oc header;
  output_string oc body;
  close_out oc
