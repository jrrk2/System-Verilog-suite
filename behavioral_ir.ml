(* Behavioral IR - Language-neutral intermediate representation
 *
 * This IR sits between language frontends (VHDL, SystemVerilog) and the
 * hardware dataflow graph (opt_ir). It provides:
 * - Language-independent semantics
 * - High-level constructs (if/case/while)
 * - Easy SSA transformation
 * - Shared optimization passes
 *)

(* Basic types *)
type signedness = Signed | Unsigned [@@deriving yojson]

(* Type system *)
type btype =
  | BInt of { width: int; signed: signedness }
  | BBool
  | BArray of { element: btype; size: int }
  | BStruct of { fields: (string * btype) list }
  [@@deriving yojson]

(* Expressions *)
type bexpr =
  | BVar of string
  | BConst of { value: int; width: int }
  | BBinOp of { op: binop; lhs: bexpr; rhs: bexpr; result_type: btype }
  | BUnOp of { op: unop; operand: bexpr; result_type: btype }
  | BSelect of { array: bexpr; index: bexpr }  (* array[index] *)
  | BSlice of { signal: bexpr; msb: int; lsb: int }  (* signal[msb:lsb] *)
  | BConcat of bexpr list  (* {a, b, c} *)
  | BReplicate of { count: int; value: bexpr }  (* {count{value}} *)
  | BCond of { condition: bexpr; then_val: bexpr; else_val: bexpr }  (* cond ? t : e *)
  | BCall of { func: string; args: bexpr list }  (* function(args) *)
  [@@deriving yojson]

and binop =
  (* Arithmetic *)
  | BAdd | BSub | BMul | BDiv | BMod
  (* Bitwise *)
  | BAnd | BOr | BXor
  (* Shift *)
  | BShl | BShr | BAshr
  (* Comparison *)
  | BEq | BNe | BLt | BLe | BGt | BGe
  [@@deriving yojson]

and unop =
  (* Logical *)
  | BNot | BNeg
  (* Reduction *)
  | BRedAnd | BRedOr | BRedXor
  [@@deriving yojson]

(* Statements *)
type bstmt =
  | BAssign of { lhs: string; rhs: bexpr }
  | BIf of { condition: bexpr; then_stmts: bstmt list; else_stmts: bstmt list }
  | BCase of { selector: bexpr; cases: (bexpr * bstmt list) list; default: bstmt list }
  | BWhile of { condition: bexpr; body: bstmt list }
  | BFor of { init: bstmt; condition: bexpr; update: bstmt; body: bstmt list }
  | BBlock of bstmt list
  | BCallStmt of { func: string; args: bexpr list }
  | BReturn of bexpr option
  [@@deriving yojson]

(* Sensitivity list *)
type sensitivity =
  | BPosEdge of string  (* posedge clk *)
  | BNegEdge of string  (* negedge clk *)
  | BLevel of string    (* @(signal) *)
  | BAny                (* @* *)
  [@@deriving yojson]

(* Process types *)
type bprocess =
  | BCombinational of {
      name: string;
      sensitivity: sensitivity list;
      body: bstmt list;
    }
  | BSequential of {
      name: string;
      clock: string;
      clock_edge: [`Pos | `Neg];
      reset: string option;
      reset_edge: [`Pos | `Neg] option;
      reset_async: bool;  (* true = async reset, false = sync reset *)
      body: bstmt list;
    }
  [@@deriving yojson]

(* Signal declaration *)
type bsignal = {
  name: string;
  stype: btype;
  direction: [`Input | `Output | `Internal];
  initial_value: bexpr option;
  (* SystemVerilog `(* key = "value" *)` attributes attached to the
   * signal. sv_suite-specific keys: `sv_decomp_adder` /
   * `sv_decomp_mul` (architecture knobs for HardCaml emit and
   * hierarchical substitution). Defaults to []; vendor keys
   * (use_dsp, multstyle, etc.) survive but are ignored. *)
  attrs: (string * string) list [@default []];
} [@@deriving yojson]

(* Module instance *)
type binstance = {
  inst_name: string;
  module_name: string;
  param_values: (string * int) list;
  (* String / bit-vector parameters that don't fit an int: Verilog string
   * params (RAM_MODE="TDP", WRITE_MODE_A="WRITE_FIRST") and wide bit-vector
   * params (RAMB INIT_xx as 256-bit hex). behavioral_to_hardcaml maps an
   * all-0/1 value to a Std_logic_vector param and anything else to a
   * String param. Defaults to [] so non-instantiating frontends are
   * unaffected. *)
  param_strs: (string * string) list [@default []];
  port_connections: (string * bexpr) list;
} [@@deriving yojson]

(* Inferred memory. RAM: synchronous-write, async- or sync-read array,
 * with `init_values = []`. ROM: read-only with `init_values` carrying
 * the contents per address (small ROMs only — large ones would
 * benefit from a different representation). *)
type bmem_kind = BRam | BRom [@@deriving yojson]

(* Port-count + read-mode categorisation, used by downstream emitters
 * to pick a primitive (Xilinx BRAM, distributed LUTRAM, ASIC SRAM
 * macro, …). `read_is_sync` is false when the read appears in pure
 * combinational/assign code (distributed RAM territory) and true when
 * the read is registered (block-RAM territory). *)
type bmem = {
  mname: string;
  data_width: int;
  addr_width: int;
  depth: int;
  kind: bmem_kind;
  init_values: int list;   (* indexed by address; empty for RAM *)
  n_write_ports: int;      (* number of distinct sync write addresses *)
  n_read_ports: int;       (* number of distinct read addresses *)
  read_is_sync: bool;      (* true ⇒ block-RAM, false ⇒ async/distributed *)
} [@@deriving yojson]

(* Function / task definition. Functions return a value; tasks don't.
 * Both have parameter lists where each entry is (name, type, direction)
 * — direction matters for tasks (input/output/inout). For functions
 * every parameter is `Input. Local variables are listed separately
 * from parameters; their initial values default to zero. *)
type bfunc = {
  fname: string;
  is_task: bool;       (* false = function, true = task *)
  ftype: btype;        (* return type (functions); BBool for tasks *)
  params: (string * btype * [`Input | `Output | `Inout]) list;
  locals: (string * btype) list;
  body: bstmt list;
} [@@deriving yojson]

(* Module definition *)
type bmodule = {
  name: string;
  params: (string * int) list;  (* Parameters with default values *)
  signals: bsignal list;
  processes: bprocess list;
  instances: binstance list;
  funcs: bfunc list;            (* function/task declarations in this module *)
  mems: bmem list;              (* inferred memories — RAM or ROM *)
  (* Module-level `(* key = "value" *)` attributes. The miter consults
   * `sv_decomp_adder` / `sv_decomp_mul` here to decide whether an
   * instance of this module can be abstracted to BBinOp BAdd/BMul
   * (see Behavioral_arch_subst). Survives boundary-preserving
   * flatten and shows up in BIR dumps for downstream tools. *)
  attrs: (string * string) list [@default []];
} [@@deriving yojson]

(* Library cell port specification - simpler than full bsignal *)
type library_port = {
  port_name: string;
  port_direction: [`Input | `Output];
  port_width: int;
} [@@deriving yojson]

(* Top-level program *)
type bprogram = {
  modules: bmodule list;
  library_cells: (string * library_port list) list;  (* (cell_name, ports) *)
} [@@deriving yojson]

(* ========================================================================= *)
(* Pretty printing *)
(* ========================================================================= *)

let rec string_of_btype = function
  | BInt { width; signed = Signed } -> Printf.sprintf "int<%d>" width
  | BInt { width; signed = Unsigned } -> Printf.sprintf "uint<%d>" width
  | BBool -> "bool"
  | BArray { element; size } -> Printf.sprintf "%s[%d]" (string_of_btype element) size
  | BStruct { fields } ->
      let field_strs = List.map (fun (name, ty) ->
        Printf.sprintf "%s: %s" name (string_of_btype ty)
      ) fields in
      Printf.sprintf "struct { %s }" (String.concat ", " field_strs)

let string_of_binop = function
  | BAdd -> "+" | BSub -> "-" | BMul -> "*" | BDiv -> "/" | BMod -> "%"
  | BAnd -> "&" | BOr -> "|" | BXor -> "^"
  | BShl -> "<<" | BShr -> ">>" | BAshr -> ">>>"
  | BEq -> "==" | BNe -> "!=" | BLt -> "<" | BLe -> "<=" | BGt -> ">" | BGe -> ">="

let string_of_unop = function
  | BNot -> "~" | BNeg -> "-"
  | BRedAnd -> "&" | BRedOr -> "|" | BRedXor -> "^"

let rec string_of_bexpr = function
  | BVar name -> name
  | BConst { value; width } -> Printf.sprintf "%d'%d" width value
  | BBinOp { op; lhs; rhs; _ } ->
      Printf.sprintf "(%s %s %s)" (string_of_bexpr lhs) (string_of_binop op) (string_of_bexpr rhs)
  | BUnOp { op; operand; _ } ->
      Printf.sprintf "(%s%s)" (string_of_unop op) (string_of_bexpr operand)
  | BSelect { array; index } ->
      Printf.sprintf "%s[%s]" (string_of_bexpr array) (string_of_bexpr index)
  | BSlice { signal; msb; lsb } ->
      Printf.sprintf "%s[%d:%d]" (string_of_bexpr signal) msb lsb
  | BConcat exprs ->
      Printf.sprintf "{%s}" (String.concat ", " (List.map string_of_bexpr exprs))
  | BReplicate { count; value } ->
      Printf.sprintf "{%d{%s}}" count (string_of_bexpr value)
  | BCond { condition; then_val; else_val } ->
      Printf.sprintf "(%s ? %s : %s)" (string_of_bexpr condition) (string_of_bexpr then_val) (string_of_bexpr else_val)
  | BCall { func; args } ->
      Printf.sprintf "%s(%s)" func (String.concat ", " (List.map string_of_bexpr args))

let rec string_of_bstmt indent stmt =
  let ind = String.make indent ' ' in
  match stmt with
  | BAssign { lhs; rhs } ->
      Printf.sprintf "%s%s := %s;" ind lhs (string_of_bexpr rhs)
  | BIf { condition; then_stmts; else_stmts } ->
      let then_str = String.concat "\n" (List.map (string_of_bstmt (indent + 2)) then_stmts) in
      let else_str = if List.length else_stmts > 0 then
        let else_body = String.concat "\n" (List.map (string_of_bstmt (indent + 2)) else_stmts) in
        Printf.sprintf "\n%selse\n%s" ind else_body
      else "" in
      Printf.sprintf "%sif %s then\n%s%s" ind (string_of_bexpr condition) then_str else_str
  | BCase { selector; cases; default } ->
      let case_strs = List.map (fun (value, stmts) ->
        let stmt_str = String.concat "\n" (List.map (string_of_bstmt (indent + 4)) stmts) in
        Printf.sprintf "%s  %s:\n%s" ind (string_of_bexpr value) stmt_str
      ) cases in
      let default_str = if List.length default > 0 then
        let def_body = String.concat "\n" (List.map (string_of_bstmt (indent + 4)) default) in
        Printf.sprintf "%s  default:\n%s" ind def_body
      else "" in
      Printf.sprintf "%scase %s\n%s\n%s\n%send case" ind (string_of_bexpr selector)
        (String.concat "\n" case_strs) default_str ind
  | BWhile { condition; body } ->
      let body_str = String.concat "\n" (List.map (string_of_bstmt (indent + 2)) body) in
      Printf.sprintf "%swhile %s\n%s\n%send while" ind (string_of_bexpr condition) body_str ind
  | BFor { init; condition; update; body } ->
      let body_str = String.concat "\n" (List.map (string_of_bstmt (indent + 2)) body) in
      Printf.sprintf "%sfor (%s; %s; %s)\n%s\n%send for" ind
        (string_of_bstmt 0 init) (string_of_bexpr condition) (string_of_bstmt 0 update)
        body_str ind
  | BBlock stmts ->
      let body_str = String.concat "\n" (List.map (string_of_bstmt (indent + 2)) stmts) in
      Printf.sprintf "%sbegin\n%s\n%send" ind body_str ind
  | BCallStmt { func; args } ->
      Printf.sprintf "%s%s(%s);" ind func (String.concat ", " (List.map string_of_bexpr args))
  | BReturn None ->
      Printf.sprintf "%sreturn;" ind
  | BReturn (Some expr) ->
      Printf.sprintf "%sreturn %s;" ind (string_of_bexpr expr)

let string_of_bprocess = function
  | BCombinational { name; sensitivity; body } ->
      let sens_str = match sensitivity with
        | [] -> "@*"
        | senses -> String.concat " or " (List.map (function
            | BPosEdge s -> "posedge " ^ s
            | BNegEdge s -> "negedge " ^ s
            | BLevel s -> s
            | BAny -> "*"
          ) senses)
      in
      let body_str = String.concat "\n" (List.map (string_of_bstmt 2) body) in
      Printf.sprintf "process %s (%s)\n%s\nend process" name sens_str body_str
  | BSequential { name; clock; clock_edge; reset; reset_async; body; _ } ->
      let edge_str = match clock_edge with `Pos -> "posedge" | `Neg -> "negedge" in
      let reset_str = match reset with
        | Some rst -> Printf.sprintf ", %s %s" (if reset_async then "async" else "sync") rst
        | None -> ""
      in
      let body_str = String.concat "\n" (List.map (string_of_bstmt 2) body) in
      Printf.sprintf "process %s (@%s %s%s)\n%s\nend process" name edge_str clock reset_str body_str

let string_of_attrs attrs =
  if attrs = [] then ""
  else
    let pairs = List.map (fun (k, v) ->
      if v = "" then k else Printf.sprintf "%s = \"%s\"" k v
    ) attrs in
    "(* " ^ String.concat ", " pairs ^ " *) "

let string_of_bmodule bmod =
  let params_str = if List.length bmod.params > 0 then
    " #(" ^ String.concat ", " (List.map (fun (n, v) -> Printf.sprintf "%s=%d" n v) bmod.params) ^ ")"
  else "" in

  let signals_str = String.concat "\n" (List.map (fun s ->
    let dir = match s.direction with
      | `Input -> "input"
      | `Output -> "output"
      | `Internal -> "signal"
    in
    Printf.sprintf "  %s%s %s: %s"
      (string_of_attrs s.attrs) dir s.name (string_of_btype s.stype)
  ) bmod.signals) in

  let processes_str = String.concat "\n\n" (List.map string_of_bprocess bmod.processes) in

  let mems_str =
    if bmod.mems = [] then ""
    else
      "\nmems:\n" ^
      String.concat "\n" (List.map (fun m ->
        Printf.sprintf "  %s: %s [%dx%d, w_ports=%d, r_ports=%d, sync_read=%b]"
          m.mname
          (match m.kind with BRam -> "RAM" | BRom -> "ROM")
          m.depth m.data_width m.n_write_ports m.n_read_ports m.read_is_sync
      ) bmod.mems)
  in

  let instances_str =
    if bmod.instances = [] then ""
    else
      "\ninstances:\n" ^
      String.concat "\n" (List.map (fun i ->
        Printf.sprintf "  %s : %s (%s)"
          i.inst_name i.module_name
          (String.concat ", "
             (List.map (fun (p, e) ->
                Printf.sprintf ".%s(%s)" p (string_of_bexpr e))
                i.port_connections))
      ) bmod.instances)
  in

  let funcs_str =
    if bmod.funcs = [] then ""
    else
      "\nfuncs:\n" ^
      String.concat "\n" (List.map (fun (f : bfunc) ->
        let param_strs = List.map (fun (n, ty, _) ->
          Printf.sprintf "%s:%s" n (string_of_btype ty)) f.params in
        Printf.sprintf "  %s%s(%s) : %s, %d locals, %d body stmts"
          (if f.is_task then "task " else "function ")
          f.fname
          (String.concat ", " param_strs)
          (string_of_btype f.ftype)
          (List.length f.locals)
          (List.length f.body)
      ) bmod.funcs)
  in

  Printf.sprintf "%smodule %s%s\n%s%s%s%s\n\n%s\nend module"
    (string_of_attrs bmod.attrs) bmod.name params_str signals_str mems_str funcs_str instances_str processes_str

let string_of_bprogram prog =
  String.concat "\n\n" (List.map string_of_bmodule prog.modules)

(* ========================================================================= *)
(* Utilities *)
(* ========================================================================= *)

let width_of_type = function
  | BInt { width; _ } -> width
  | BBool -> 1
  | _ -> 0  (* Arrays/structs don't have simple width *)

let is_signed = function
  | BInt { signed = Signed; _ } -> true
  | _ -> false
