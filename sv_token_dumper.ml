(* SystemVerilog Token Tree Dumper *)
(* Reconstructs source code from Verible's parsed token tree *)
(* This reveals the order in which Verible stores statements *)

open Source_text_verible

(* Indentation level for pretty printing *)
let indent_level = ref 0

let indent () = String.make (!indent_level * 2) ' '

let inc_indent () = indent_level := !indent_level + 1
let dec_indent () = indent_level := !indent_level - 1

(* Track statement numbers for debugging *)
let stmt_counter = ref 0

(* Extract string from identifier token *)
let rec get_identifier = function
  | SymbolIdentifier s -> s
  | _ -> "<id>"

(* Extract number from token *)
let get_number = function
  | TK_DecNumber s -> s
  | TK_UnBasedNumber s -> s
  | _ -> "<num>"

(* Dump expression to string *)
let rec dump_expression expr =
  match expr with
  | SymbolIdentifier s -> s
  | TK_DecNumber n -> n
  | TK_UnBasedNumber n -> n
  | TK_StringLiteral s -> Printf.sprintf "\"%s\"" s

  (* Logical AND *)
  | TUPLE4 (STRING "logand_expr2", left, _, right) ->
      Printf.sprintf "(%s && %s)"
        (dump_expression left)
        (dump_expression right)

  (* Logical OR *)
  | TUPLE4 (STRING "logor_expr2", left, _, right) ->
      Printf.sprintf "(%s || %s)"
        (dump_expression left)
        (dump_expression right)

  (* Equality *)
  | TUPLE4 (STRING "equality_expr2", left, _, right) ->
      Printf.sprintf "(%s == %s)"
        (dump_expression left)
        (dump_expression right)

  (* Inequality *)
  | TUPLE4 (STRING "equality_expr3", left, _, right) ->
      Printf.sprintf "(%s != %s)"
        (dump_expression left)
        (dump_expression right)

  (* Greater or equal *)
  | TUPLE4 (STRING "comp_expr4", left, _, right) ->
      Printf.sprintf "(%s >= %s)"
        (dump_expression left)
        (dump_expression right)

  (* Less than *)
  | TUPLE4 (STRING "comp_expr1", left, _, right) ->
      Printf.sprintf "(%s < %s)"
        (dump_expression left)
        (dump_expression right)

  (* Addition *)
  | TUPLE4 (STRING "add_expr2", left, _, right) ->
      Printf.sprintf "(%s + %s)"
        (dump_expression left)
        (dump_expression right)

  (* Subtraction *)
  | TUPLE4 (STRING "add_expr3", left, _, right) ->
      Printf.sprintf "(%s - %s)"
        (dump_expression left)
        (dump_expression right)

  (* Multiplication *)
  | TUPLE4 (STRING "mul_expr2", left, _, right) ->
      Printf.sprintf "(%s * %s)"
        (dump_expression left)
        (dump_expression right)

  (* Unary not *)
  | TUPLE3 (STRING "unary_prefix_expr2", _, expr) ->
      Printf.sprintf "!%s" (dump_expression expr)

  (* Parenthesized *)
  | TUPLE4 (STRING "expr_primary_parens1", _, expr, _) ->
      Printf.sprintf "(%s)" (dump_expression expr)

  (* Identifier *)
  | TUPLE3 (STRING "unqualified_id1", id, _) ->
      get_identifier id

  (* Reference *)
  | TUPLE3 (STRING "reference1", ref_base, _) ->
      dump_expression ref_base

  | TUPLE3 (STRING "reference_or_call_base1", id, _) ->
      dump_expression id

  (* Bit literal *)
  | TUPLE4 (STRING "number1", _, base, digits) ->
      Printf.sprintf "%s'%s" (get_number base) (get_number digits)

  | _ -> "<expr>"

and dump_lvalue lval =
  match lval with
  | TUPLE3 (STRING "unqualified_id1", id, _) ->
      get_identifier id
  | TUPLE3 (STRING "reference1", ref_base, _) ->
      dump_expression ref_base
  | _ -> dump_expression lval

(* Dump statement with optional numbering *)
let rec dump_statement ?(show_stmt_order=false) stmt =
  match stmt with
  (* Nonblocking assignment *)
  | TUPLE6 (STRING "nonblocking_assignment1", lhs, _, _, _, rhs) ->
      if show_stmt_order then begin
        stmt_counter := !stmt_counter + 1;
        Printf.sprintf "%s/* STMT#%d */ %s <= %s;"
          (indent ())
          !stmt_counter
          (dump_lvalue lhs)
          (dump_expression rhs)
      end else
        Printf.sprintf "%s%s <= %s;"
          (indent ())
          (dump_lvalue lhs)
          (dump_expression rhs)

  (* Blocking assignment *)
  | TUPLE6 (STRING "blocking_assignment1", lhs, _, _, _, rhs) ->
      if show_stmt_order then begin
        stmt_counter := !stmt_counter + 1;
        Printf.sprintf "%s/* STMT#%d */ %s = %s;"
          (indent ())
          !stmt_counter
          (dump_lvalue lhs)
          (dump_expression rhs)
      end else
        Printf.sprintf "%s%s = %s;"
          (indent ())
          (dump_lvalue lhs)
          (dump_expression rhs)

  (* Statement item wrapper *)
  | TUPLE3 (STRING "statement_item6", inner_stmt, _) ->
      dump_statement ~show_stmt_order inner_stmt

  (* Sequential block *)
  | TUPLE4 (STRING "seq_block1", Begin, stmts, End) ->
      inc_indent ();
      let stmt_strs = dump_statement_list ~show_stmt_order stmts in
      dec_indent ();
      Printf.sprintf "%sbegin\n%s\n%send"
        (indent ())
        (String.concat "\n" stmt_strs)
        (indent ())

  (* If statement (no else) *)
  | TUPLE4 (STRING "conditional_statement1", If, cond, then_stmt) ->
      inc_indent ();
      let then_str = dump_statement ~show_stmt_order then_stmt in
      dec_indent ();
      Printf.sprintf "%sif (%s)\n%s"
        (indent ())
        (dump_expression cond)
        then_str

  | TUPLE5 (STRING "conditional_statement1", _label, If, cond, then_stmt) ->
      inc_indent ();
      let then_str = dump_statement ~show_stmt_order then_stmt in
      dec_indent ();
      Printf.sprintf "%sif (%s)\n%s"
        (indent ())
        (dump_expression cond)
        then_str

  (* If-else statement *)
  | TUPLE6 (STRING "conditional_statement2", If, cond, then_stmt, Else, else_stmt) ->
      inc_indent ();
      let then_str = dump_statement ~show_stmt_order then_stmt in
      dec_indent ();
      inc_indent ();
      let else_str = dump_statement ~show_stmt_order else_stmt in
      dec_indent ();
      Printf.sprintf "%sif (%s)\n%s\n%selse\n%s"
        (indent ())
        (dump_expression cond)
        then_str
        (indent ())
        else_str

  | TUPLE7 (STRING "conditional_statement2", _label, If, cond, then_stmt, Else, else_stmt) ->
      inc_indent ();
      let then_str = dump_statement ~show_stmt_order then_stmt in
      dec_indent ();
      inc_indent ();
      let else_str = dump_statement ~show_stmt_order else_stmt in
      dec_indent ();
      Printf.sprintf "%sif (%s)\n%s\n%selse\n%s"
        (indent ())
        (dump_expression cond)
        then_str
        (indent ())
        else_str

  | _ -> Printf.sprintf "%s<unknown_stmt>" (indent ())

and dump_statement_list ?(show_stmt_order=false) stmts =
  match stmts with
  | TLIST lst ->
      if show_stmt_order then begin
        Printf.printf "%s[DEBUG] seq_block contains %d statements (in parse tree order):\n"
          (indent ()) (List.length lst);
        List.iteri (fun i stmt ->
          let kind = match stmt with
             | TUPLE3 (STRING "statement_item6", TUPLE6 (STRING "nonblocking_assignment1", _, _, _, _, _), _) -> "nonblocking_assign"
             | TUPLE3 (STRING "statement_item6", TUPLE6 (STRING "blocking_assignment1", _, _, _, _, _), _) -> "blocking_assign"
             | TUPLE3 (STRING "statement_item6", TUPLE4 (STRING "conditional_statement1", _, _, _), _) -> "if (no else)"
             | TUPLE3 (STRING "statement_item6", TUPLE6 (STRING "conditional_statement2", _, _, _, _, _), _) -> "if-else"
             | TUPLE3 (STRING "statement_item6", TUPLE7 (STRING "conditional_statement2", _, _, _, _, _, _), _) -> "if-else (labeled)"
             | _ -> "other"
          in
          Printf.printf "%s  [%d] %s\n" (indent ()) i kind
        ) lst
      end;
      List.map (dump_statement ~show_stmt_order) lst
  | single -> [dump_statement ~show_stmt_order single]

(* Dump always block *)
let dump_always ?(show_stmt_order=false) event_ctrl stmt =
  let event_str = match event_ctrl with
    | TUPLE3 (STRING "edge_event_expression2", Posedge, signal) ->
        Printf.sprintf "posedge %s" (dump_expression signal)
    | TUPLE3 (STRING "edge_event_expression2", Negedge, signal) ->
        Printf.sprintf "negedge %s" (dump_expression signal)
    | TUPLE5 (STRING "event_expression_list", e1, Or, e2, _) ->
        let e1_str = match e1 with
          | TUPLE3 (STRING "edge_event_expression2", Posedge, sig1) ->
              Printf.sprintf "posedge %s" (dump_expression sig1)
          | TUPLE3 (STRING "edge_event_expression2", Negedge, sig1) ->
              Printf.sprintf "negedge %s" (dump_expression sig1)
          | _ -> dump_expression e1
        in
        let e2_str = match e2 with
          | TUPLE3 (STRING "edge_event_expression2", Posedge, sig2) ->
              Printf.sprintf "posedge %s" (dump_expression sig2)
          | TUPLE3 (STRING "edge_event_expression2", Negedge, sig2) ->
              Printf.sprintf "negedge %s" (dump_expression sig2)
          | _ -> dump_expression e2
        in
        Printf.sprintf "%s or %s" e1_str e2_str
    | _ -> "<event>"
  in

  Printf.printf "\nalways @(%s)\n" event_str;
  if show_stmt_order then stmt_counter := 0;
  inc_indent ();
  let stmt_str = dump_statement ~show_stmt_order stmt in
  dec_indent ();
  Printf.printf "%s\n" stmt_str

(* Parse and dump a file *)
let dump_file ?(show_stmt_order=false) filename =
  Printf.printf "Parsing: %s\n" filename;

  (* Parse with Verible lexer *)
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let deflated_lexer = Source_text_verible_lex.deflate Source_text_verible_lex.token in
  let token_tree = Source_text_verible.ml_start deflated_lexer lexbuf in
  close_in ic;

  (* Walk the tree to find always blocks and dump them *)
  let rec find_always_blocks node =
    match node with
    | TUPLE5 (STRING "always_construct", Always, AT, event_ctrl, TUPLE2 (_, stmt)) ->
        dump_always ~show_stmt_order event_ctrl stmt

    | TUPLE4 (STRING "always_construct", Always, event_ctrl, stmt) ->
        dump_always ~show_stmt_order event_ctrl stmt

    | TUPLE5 (STRING "always_construct", Always_ff, AT, event_ctrl, TUPLE2 (_, stmt)) ->
        dump_always ~show_stmt_order event_ctrl stmt

    | TUPLE2 (a, b) ->
        find_always_blocks a;
        find_always_blocks b

    | TUPLE3 (a, b, c) ->
        find_always_blocks a;
        find_always_blocks b;
        find_always_blocks c

    | TUPLE4 (a, b, c, d) ->
        find_always_blocks a;
        find_always_blocks b;
        find_always_blocks c;
        find_always_blocks d

    | TUPLE5 (a, b, c, d, e) ->
        find_always_blocks a;
        find_always_blocks b;
        find_always_blocks c;
        find_always_blocks d;
        find_always_blocks e

    | TUPLE6 (a, b, c, d, e, f) ->
        find_always_blocks a;
        find_always_blocks b;
        find_always_blocks c;
        find_always_blocks d;
        find_always_blocks e;
        find_always_blocks f

    | TUPLE7 (a, b, c, d, e, f, g) ->
        find_always_blocks a;
        find_always_blocks b;
        find_always_blocks c;
        find_always_blocks d;
        find_always_blocks e;
        find_always_blocks f;
        find_always_blocks g

    | TLIST lst -> List.iter find_always_blocks lst

    | _ -> ()
  in

  find_always_blocks token_tree
