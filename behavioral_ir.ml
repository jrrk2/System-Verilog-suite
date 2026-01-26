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
} [@@deriving yojson]

(* Module instance *)
type binstance = {
  inst_name: string;
  module_name: string;
  param_values: (string * int) list;
  port_connections: (string * bexpr) list;
} [@@deriving yojson]

(* Module definition *)
type bmodule = {
  name: string;
  params: (string * int) list;  (* Parameters with default values *)
  signals: bsignal list;
  processes: bprocess list;
  instances: binstance list;
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
    Printf.sprintf "  %s %s: %s" dir s.name (string_of_btype s.stype)
  ) bmod.signals) in

  let processes_str = String.concat "\n\n" (List.map string_of_bprocess bmod.processes) in

  Printf.sprintf "module %s%s\n%s\n\n%s\nend module" bmod.name params_str signals_str processes_str

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
