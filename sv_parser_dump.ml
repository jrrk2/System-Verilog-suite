(* sv_parser_dump — parse the text-tree CST that dalance's
 * sv-parser emits via `parse_sv -t`. Each line of the dump is
 *
 *     <indent>NodeName
 *  or
 *     <indent>Token: '<literal>' @ line:<n>
 *
 * Indentation is exactly one space per nesting level. Token lines
 * are leaves carrying the source-text lexeme; node lines are
 * interior nodes whose children are the lines immediately following
 * at one-greater indent.
 *
 * Usage:
 *   let tree = Sv_parser_dump.parse_text dump in
 *   Sv_parser_dump.walk tree ~f:(fun node -> ...)
 *
 * The format is stable enough to parse with a stack-based reader;
 * we don't need sv-parser's serde-JSON because the text-tree is
 * already deterministic and round-tripable for our needs. *)

type t =
  | Node of { name: string; children: t list }
  | Leaf of { value: string; line: int }

(* Compile the token-leaf pattern once at module load.  Token line
 * shape: "Token: '<lexeme>' @ line:<n>".  The lexeme is delimited
 * by single quotes and may itself contain anything (a double-quoted
 * string literal arrives as Token: '"hello"' @ line:N) — so we
 * match the OUTER delimiters via the trailing " @ line:" sentinel. *)
let token_re =
  Str.regexp "^Token: '\\(.*\\)' @ line:\\([0-9]+\\)$"

let count_indent s =
  let n = String.length s in
  let rec go i = if i < n && s.[i] = ' ' then go (i + 1) else i in
  go 0

(* Read a single line into a parsed kind. Returns
 *   Some (indent, `Node name | `Leaf (value, line))
 * or None for blank/non-tree lines.  Skips lines that don't look
 * like CST entries (sv-parser prepends a "parse succeeded: …"
 * line when quiet mode is off; we tolerate it by ignoring any
 * lines that don't start with at least one space or with a
 * recognised root tag). *)
let parse_line s =
  let s = if String.length s > 0 && s.[String.length s - 1] = '\r'
          then String.sub s 0 (String.length s - 1) else s in
  if s = "" then None
  else
    let indent = count_indent s in
    let body = String.sub s indent (String.length s - indent) in
    if body = "" then None
    else if Str.string_match token_re body 0 then
      let value = Str.matched_group 1 body in
      let line = int_of_string (Str.matched_group 2 body) in
      Some (indent, `Leaf (value, line))
    else
      Some (indent, `Node body)

(* Stack-based tree builder. The stack holds, at each depth, the
 * partial child-list (reversed) plus the node's name; on dedent we
 * pop frames and attach their reversed children as a Node into the
 * parent's child list. *)
let parse_text text =
  let lines = String.split_on_char '\n' text in
  let stack : (int * string * t list ref) list ref = ref [] in
  let attach_frame name kids =
    Node { name; children = List.rev kids }
  in
  let pop_to indent =
    while (match !stack with
           | (ind, _, _) :: _ when ind >= indent -> true
           | _ -> false) do
      match !stack with
      | (_, name, kids) :: ((_, _, parent_kids) :: _ as rest) ->
          parent_kids := attach_frame name !kids :: !parent_kids;
          stack := rest
      | [(_, name, kids)] ->
          (* Bottoming out — leave the single frame so the caller can
             pop it as the final result.  But we should only get here
             with one frame on the stack AT exactly this indent, which
             the loop guard already excluded.  Fall through silently. *)
          ignore (name, kids);
          stack := []
      | [] -> ()
    done
  in
  List.iter (fun line ->
    match parse_line line with
    | None -> ()
    | Some (indent, kind) ->
        pop_to indent;
        (match kind with
         | `Node name ->
             stack := (indent, name, ref []) :: !stack
         | `Leaf (value, line) ->
             (match !stack with
              | (_, _, kids) :: _ ->
                  kids := Leaf { value; line } :: !kids
              | [] -> ()))  (* top-level token, shouldn't happen *)
    ) lines;
  (* Drain stack — pop all remaining frames. *)
  let rec drain () =
    match !stack with
    | [] -> None
    | [(_, name, kids)] ->
        stack := [];
        Some (attach_frame name !kids)
    | (_, name, kids) :: ((_, _, parent_kids) :: _ as rest) ->
        parent_kids := attach_frame name !kids :: !parent_kids;
        stack := rest;
        drain ()
  in
  match drain () with
  | Some t -> t
  | None -> Node { name = "(empty)"; children = [] }

(* Run sv-parser's `parse_sv -t -q` on a source file and parse the
 * resulting tree.  sv_parser_bin defaults to
 * `~/sv-parser/target/release/examples/parse_sv` — overridable via
 * the SV_PARSER_BIN environment variable for users with a non-
 * standard checkout. *)
let default_sv_parser_bin =
  match Sys.getenv_opt "SV_PARSER_BIN" with
  | Some p -> p
  | None ->
      let home = try Sys.getenv "HOME" with Not_found -> "" in
      home ^ "/sv-parser/target/release/examples/parse_sv"

let parse_file ?(bin = default_sv_parser_bin)
               ?(incdirs = []) ?(defines = []) file =
  let inc_args =
    List.concat_map (fun d -> ["-i"; d]) incdirs in
  let def_args =
    List.concat_map (fun d -> ["-d"; d]) defines in
  let argv = [bin; "-t"; "-q"] @ inc_args @ def_args @ [file] in
  let cmd = String.concat " " (List.map Filename.quote argv) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 4096 in
  (try
    while true do
      Buffer.add_channel buf ic 4096
    done
  with End_of_file -> ());
  let status = Unix.close_process_in ic in
  let text = Buffer.contents buf in
  match status with
  | Unix.WEXITED 0 -> Ok (parse_text text)
  | Unix.WEXITED n -> Error (Printf.sprintf "sv-parser exited %d" n)
  | _ -> Error "sv-parser terminated abnormally"

(* Walk the tree in pre-order, calling [f] on every node and leaf.
 * [f] returns unit; the walker just visits.  Convenient for ad-hoc
 * inspection or for collecting matches via mutable references. *)
let rec walk t ~f =
  f t;
  match t with
  | Node { children; _ } -> List.iter (walk ~f) children
  | Leaf _ -> ()

(* Return all descendant nodes whose name matches the predicate, in
 * pre-order.  Useful for "find every PortIdentifier under this
 * AnsiPortDeclaration" style queries. *)
let find_all t ~name_is =
  let acc = ref [] in
  walk t ~f:(fun n ->
    match n with
    | Node { name; _ } when name_is name -> acc := n :: !acc
    | _ -> ());
  List.rev !acc

let find_first t ~name_is =
  let found = ref None in
  let rec go t =
    match !found, t with
    | Some _, _ -> ()
    | None, Node { name; children } ->
        if name_is name then found := Some t
        else List.iter go children
    | None, Leaf _ -> ()
  in
  go t;
  !found

(* Extract the first Leaf-string descendant (in pre-order). Used to
 * read the identifier under e.g. SimpleIdentifier → Token. *)
let rec first_leaf = function
  | Leaf { value; _ } -> Some value
  | Node { children; _ } ->
      let rec scan = function
        | [] -> None
        | c :: rest ->
            (match first_leaf c with
             | Some v -> Some v
             | None -> scan rest)
      in scan children

(* Render an abbreviated form for debugging: one line per node with
 * indent matching the tree depth, leaves shown as their value. *)
let rec to_string ?(depth = 0) t =
  let pad = String.make (depth * 2) ' ' in
  match t with
  | Leaf { value; line } ->
      Printf.sprintf "%s'%s' @ %d" pad value line
  | Node { name; children } ->
      let head = Printf.sprintf "%s%s" pad name in
      head :: List.map (to_string ~depth:(depth + 1)) children
      |> String.concat "\n"
