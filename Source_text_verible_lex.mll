(**************************************************************************)
(*                                                                        *)
(* OCaml template Copyright (C) 2004-2010                                 *)
(*  Sylvain Conchon, Jean-Christophe Filliatre and Julien Signoles        *)
(* Adapted to boolean logic by Jonathan Kimmitt                           *)
(*  Copyright 2016 University of Cambridge                                *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

{
  open Source_text_verible

  let deflate token = 
    let q = Queue.create () in
    fun lexbuf -> 
      if not (Queue.is_empty q) then Queue.pop q else   
        match token lexbuf with 
          | [   ] -> EOF_TOKEN
          | [tok] -> tok
          | hd::t -> List.iter (fun tok -> Queue.add tok q) t ; hd 

  let verbose = try int_of_string(Sys.getenv "LEXER_VERBOSE") > 0 with _ -> false
  let lincnt = ref 0

  (* System-task / elaboration-builtin identifier whitelist.
   * Names in this list lex as `SystemTFIdentifier`, which the grammar
   * routes through `system_tf_call` — and crucially that's one of the
   * `casting_type` alternatives, so `$clog2(X)'(Y)` size casts parse
   * cleanly.  Names NOT in the list fall through to SymbolIdentifier
   * and are handled as ordinary function calls (`reference_or_call_base`).
   *
   * Only elaboration-time / constant-foldable system functions belong
   * here.  Runtime simulation tasks ($display, $write, $readmemh, …)
   * should NOT be added — their parse-tree shape is consumed
   * specially by verible_to_behavioral.ml (e.g. $readmemh ROM init). *)
  let systask =
    let h = Hashtbl.create 32 in
    List.iter
      (fun s -> Hashtbl.add h s s)
      [
        "$test$plusargs";
        (* Width / log queries — constant-foldable at elaboration. *)
        "$clog2"; "$bits"; "$size";
        "$left"; "$right"; "$low"; "$high";
        "$increment"; "$dimensions"; "$unpacked_dimensions";
        (* Sign casts — value-preserving, take a single expr arg. *)
        "$signed"; "$unsigned";
        (* Math elaboration helpers. *)
        "$ceil"; "$floor"; "$pow"; "$ln"; "$log10"; "$exp"; "$sqrt";
        (* Bit-pattern queries. *)
        "$countbits"; "$countones"; "$onehot"; "$onehot0";
        "$isunknown";
        (* Real ↔ int casts. *)
        "$itor"; "$rtoi"; "$realtobits"; "$bitstoreal";
      ];
    fun s -> SystemTFIdentifier (Hashtbl.find h s)

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter 
      (fun (k,s) -> Hashtbl.add h s k)
      [
( Absdelay , "absdelay" );
( Abstol , "abstol" );
( Ac_stim , "ac_stim" );
( Accept_on , "accept_on" );
( Access , "access" );
( Alias , "alias" );
( Aliasparam , "aliasparam" );
( Always , "always" );
( Always_comb , "always_comb" );
( Always_ff , "always_ff" );
( Always_latch , "always_latch" );
( Analog , "analog" );
( Analysis , "analysis" );
( And , "and" );
( Assert , "assert" );
( Assign , "assign" );
( Assume , "assume" );
( Automatic , "automatic" );
( BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____ , "backquote_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_begin____" );
( BACKQUOTE_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____ , "backquote_UNDERSCORE_UNDERSCORE_UNDERSCORE_UNDERSCORE_verible_verilog_library_end____" );
( BACKQUOTE_begin_keywords , "backquote_begin_keywords" );
( BACKQUOTE_celldefine , "backquote_celldefine" );
( BACKQUOTE_default_decay_time , "backquote_default_decay_time" );
( BACKQUOTE_default_nettype , "backquote_default_nettype" );
( BACKQUOTE_default_trireg_strength , "backquote_default_trireg_strength" );
( BACKQUOTE_define , "backquote_define" );
( BACKQUOTE_delay_mode_distributed , "backquote_delay_mode_distributed" );
( BACKQUOTE_delay_mode_path , "backquote_delay_mode_path" );
( BACKQUOTE_delay_mode_unit , "backquote_delay_mode_unit" );
( BACKQUOTE_delay_mode_zero , "backquote_delay_mode_zero" );
( BACKQUOTE_disable_portfaults , "backquote_disable_portfaults" );
( BACKQUOTE_else , "backquote_else" );
( BACKQUOTE_elsif , "backquote_elsif" );
( BACKQUOTE_enable_portfaults , "backquote_enable_portfaults" );
( BACKQUOTE_end_keywords , "backquote_end_keywords" );
( BACKQUOTE_endcelldefine , "backquote_endcelldefine" );
( BACKQUOTE_endif , "backquote_endif" );
( BACKQUOTE_endprotect , "backquote_endprotect" );
( BACKQUOTE_ifdef , "backquote_ifdef" );
( BACKQUOTE_ifndef , "backquote_ifndef" );
( BACKQUOTE_include , "backquote_include" );
( BACKQUOTE_nosuppress_faults , "backquote_nosuppress_faults" );
( BACKQUOTE_nounconnected_drive , "backquote_nounconnected_drive" );
( BACKQUOTE_pragma , "backquote_pragma" );
( BACKQUOTE_protect , "backquote_protect" );
( BACKQUOTE_resetall , "backquote_resetall" );
( BACKQUOTE_suppress_faults , "backquote_suppress_faults" );
( BACKQUOTE_timescale , "backquote_timescale" );
( BACKQUOTE_unconnected_drive , "backquote_unconnected_drive" );
( BACKQUOTE_undef , "backquote_undef" );
( BACKQUOTE_uselib , "backquote_uselib" );
( Before , "before" );
( Begin , "begin" );
( Bind , "bind" );
( Bins , "bins" );
( Binsof , "binsof" );
( Bit , "bit" );
( Break , "break" );
( Buf , "buf" );
( Bufif0 , "bufif0" );
( Bufif1 , "bufif1" );
( Byte , "byte" );
( Case , "case" );
( Casex , "casex" );
( Casez , "casez" );
( Cell , "cell" );
( Chandle , "chandle" );
( Checker , "checker" );
( Class , "class" );
( Clocking , "clocking" );
( Cmos , "cmos" );
( Config , "config" );
( Connect , "connect" );
( Connectmodule , "connectmodule" );
( Connectrules , "connectrules" );
( Const , "const" );
( Constraint , "constraint" );
( Context , "context" );
( Continue , "continue" );
( Continuous , "continuous" );
( Cover , "cover" );
( Covergroup , "covergroup" );
( Coverpoint , "coverpoint" );
( Cross , "cross" );
( DLR_attribute , "dlr_attribute" );
( DLR_fullskew , "dlr_fullskew" );
( DLR_hold , "dlr_hold" );
( DLR_nochange , "dlr_nochange" );
( DLR_period , "dlr_period" );
( DLR_recovery , "dlr_recovery" );
( DLR_recrem , "dlr_recrem" );
( DLR_removal , "dlr_removal" );
( DLR_root , "dlr_root" );
( DLR_setup , "dlr_setup" );
( DLR_setuphold , "dlr_setuphold" );
( DLR_skew , "dlr_skew" );
( DLR_timeskew , "dlr_timeskew" );
( DLR_unit , "dlr_unit" );
( DLR_width , "dlr_width" );
( Ddt_nature , "ddt_nature" );
( Deassign , "deassign" );
( Default , "default" );
( Defparam , "defparam" );
( Design , "design" );
( Disable , "disable" );
( Discipline , "discipline" );
( Discrete , "discrete" );
( Dist , "dist" );
( Do , "do" );
( Domain , "domain" );
( Driver_update , "driver_update" );
( Edge , "edge" );
( Else , "else" );
( End , "end" );
( End_of_file , "end_of_file" );
( Endcase , "endcase" );
( Endchecker , "endchecker" );
( Endclass , "endclass" );
( Endclocking , "endclocking" );
( Endconfig , "endconfig" );
( Endconnectrules , "endconnectrules" );
( Enddiscipline , "enddiscipline" );
( Endfunction , "endfunction" );
( Endgenerate , "endgenerate" );
( Endgroup , "endgroup" );
( Endinterface , "endinterface" );
( Endmodule , "endmodule" );
( Endnature , "endnature" );
( Endpackage , "endpackage" );
( Endparamset , "endparamset" );
( Endprimitive , "endprimitive" );
( Endprogram , "endprogram" );
( Endproperty , "endproperty" );
( Endsequence , "endsequence" );
( Endspecify , "endspecify" );
( Endtable , "endtable" );
( Endtask , "endtask" );
( Enum , "enum" );
(*
( Error , "error" );
*)
( EscapedIdentifier , "escapedidentifier" );
( Event , "event" );
( Eventually , "eventually" );
( Exclude , "exclude" );
( Expect , "expect" );
( Export , "export" );
( Extends , "extends" );
( Extern , "extern" );
( Final , "final" );
( Find , "find" );
( Find_first , "find_first" );
( Find_first_index , "find_first_index" );
( Find_index , "find_index" );
( Find_last , "find_last" );
( Find_last_index , "find_last_index" );
( First_match , "first_match" );
( Flicker_noise , "flicker_noise" );
( Flow , "flow" );
( For , "for" );
( Force , "force" );
( Foreach , "foreach" );
( Forever , "forever" );
( Fork , "fork" );
( Forkjoin , "forkjoin" );
( From , "from" );
( Function , "function" );
( Generate , "generate" );
( Genvar , "genvar" );
( Global , "global" );
( Highz0 , "highz0" );
( Highz1 , "highz1" );
( Idt_nature , "idt_nature" );
( If , "if" );
( Iff , "iff" );
( Ifnone , "ifnone" );
( Ignore_bins , "ignore_bins" );
( Illegal_bins , "illegal_bins" );
( Implements , "implements" );
( Implies , "implies" );
( Import , "import" );
( Incdir , "incdir" );
( Include , "include" );
( Inf , "inf" );
( Infinite , "infinite" );
( Initial , "initial" );
( Inout , "inout" );
( Input , "input" );
( Inside , "inside" );
( Instance , "instance" );
( Int , "int" );
( Integer , "integer" );
( Interconnect , "interconnect" );
( Interface , "interface" );
( Intersect , "intersect" );
( Join , "join" );
( Join_any , "join_any" );
( Join_none , "join_none" );
( Laplace_nd , "laplace_nd" );
( Laplace_np , "laplace_np" );
( Laplace_zd , "laplace_zd" );
( Laplace_zp , "laplace_zp" );
( Large , "large" );
( Last_crossing , "last_crossing" );
( Let , "let" );
( Liblist , "liblist" );
( Library , "library" );
( Limexp , "limexp" );
( Local , "local" );
( Local_COLON_COLON , "local_COLON_COLON" );
( Localparam , "localparam" );
( Logic , "logic" );
( Longint , "longint" );
( MacroArg , "macroarg" );
( MacroCallCloseToEndLine , "macrocallclosetoendline" );
( MacroCallId , "macrocallid" );
( MacroIdItem , "macroiditem" );
( MacroIdentifier , "macroidentifier" );
( MacroNumericWidth , "macronumericwidth" );
( Macromodule , "macromodule" );
( Matches , "matches" );
( Max , "max" );
( Medium , "medium" );
( Min , "min" );
( Modport , "modport" );
( Module , "module" );
( NUMBER_step , "number_step" );
( Nand , "nand" );
( Nature , "nature" );
( Negedge , "negedge" );
( Net_resolution , "net_resolution" );
( Nettype , "nettype" );
( New , "new" );
( Nexttime , "nexttime" );
( Nmos , "nmos" );
( Noise_table , "noise_table" );
( Nor , "nor" );
( Noshowcancelled , "noshowcancelled" );
( Not , "not" );
( Notif0 , "notif0" );
( Notif1 , "notif1" );
( Null , "null" );
( Option , "option" );
( Or , "or" );
( Output , "output" );
( PP_Identifier , "pp_Identifier" );
( Package , "package" );
( Packed , "packed" );
( Parameter , "parameter" );
( Paramset , "paramset" );
( Pmos , "pmos" );
( Posedge , "posedge" );
( Potential , "potential" );
( Pow , "pow" );
( Primitive , "primitive" );
( Priority , "priority" );
(*
( Product , "product" );
*)
( Program , "program" );
( Property , "property" );
( Protected , "protected" );
( Pull0 , "pull0" );
( Pull1 , "pull1" );
( Pulldown , "pulldown" );
( Pullup , "pullup" );
( Pulsestyle_ondetect , "pulsestyle_ondetect" );
( Pulsestyle_onevent , "pulsestyle_onevent" );
( Pure , "pure" );
( Rand , "rand" );
( Randc , "randc" );
( Randcase , "randcase" );
( Randomize , "randomize" );
( Randsequence , "randsequence" );
( Rcmos , "rcmos" );
( Real , "real" );
( Realtime , "realtime" );
( Ref , "ref" );
( Reg , "reg" );
( Reject_on , "reject_on" );
( Release , "release" );
( Repeat , "repeat" );
( Resolveto , "resolveto" );
( Restrict , "restrict" );
( Return , "return" );
( Reverse , "reverse" );
( Rnmos , "rnmos" );
( Rpmos , "rpmos" );
( Rsort , "rsort" );
( Rtran , "rtran" );
( Rtranif0 , "rtranif0" );
( Rtranif1 , "rtranif1" );
( SEMICOLON_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN , "semicolon_LPAREN_after_HYPHEN_assertion_HYPHEN_variable_HYPHEN_decls_RPAREN" );
( SLASH_AMPERSAND_lowast_SEMICOLON_comment_AMPERSAND_lowast_SEMICOLON_SLASH , "slash_AMPERSAND_lowast_SEMICOLON_comment_AMPERSAND_lowast_SEMICOLON_SLASH" );
( SLASH_SLASH_end_of_line_comment , "slash_SLASH_end_of_line_comment" );
( S_always , "s_always" );
( S_eventually , "s_eventually" );
( S_nexttime , "s_nexttime" );
( S_until , "s_until" );
( S_until_with , "s_until_with" );
( Sample , "sample" );
( Scalared , "scalared" );
( Sequence , "sequence" );
( Shortint , "shortint" );
( Shortreal , "shortreal" );
( Showcancelled , "showcancelled" );
( Shuffle , "shuffle" );
( Signed , "signed" );
( Small , "small" );
( Soft , "soft" );
( Solve , "solve" );
( Sort , "sort" );
( Specify , "specify" );
( Specparam , "specparam" );
( Static , "static" );
( String , "string" );
( Strong , "strong" );
( Strong0 , "strong0" );
( Strong1 , "strong1" );
( Struct , "struct" );
(*
( Sum , "sum" );
*)
( Super , "super" );
( Supply0 , "supply0" );
( Supply1 , "supply1" );
( Sync_accept_on , "sync_accept_on" );
( Sync_reject_on , "sync_reject_on" );
( TK_AngleBracketInclude , "tk_AngleBracketInclude" );
( TK_EvalStringLiteral , "tk_EvalStringLiteral" );
( TK_TimeLiteral , "tk_TimeLiteral" );
( TK_XZDigits , "tk_XZDigits" );
( TK_edge_descriptor , "tk_edge_descriptor" );
( Table , "table" );
( Tagged , "tagged" );
( Task , "task" );
( This , "this" );
( Throughout , "throughout" );
( Time , "time" );
( Timeprecision , "timeprecision" );
( Timeprecision_check , "timeprecision_check" );
( Timeunit , "timeunit" );
( Timeunit_check , "timeunit_check" );
( Tran , "tran" );
( Tranif0 , "tranif0" );
( Tranif1 , "tranif1" );
( Transition , "transition" );
( Tri , "tri" );
( Tri0 , "tri0" );
( Tri1 , "tri1" );
( Triand , "triand" );
( Trior , "trior" );
( Trireg , "trireg" );
( Type , "type" );
( Type_option , "type_option" );
( Typedef , "typedef" );
( Union , "union" );
( Unique , "unique" );
( Unique0 , "unique0" );
( Unique_index , "unique_index" );
( Units , "units" );
( Unsigned , "unsigned" );
( Until , "until" );
( Until_with , "until_with" );
( Untyped , "untyped" );
( Use , "use" );
( Uwire , "uwire" );
( Var , "var" );
( Vectored , "vectored" );
( Virtual , "virtual" );
( Void , "void" );
( Wait , "wait" );
( Wait_order , "wait_order" );
( Wand , "wand" );
( Weak , "weak" );
( Weak0 , "weak0" );
( Weak1 , "weak1" );
( While , "while" );
( White_noise , "white_noise" );
( Wildcard , "wildcard" );
( Wire , "wire" );
( With , "with" );
( With_LPAREN_covergroup_RPAREN , "with_LPAREN_covergroup_RPAREN" );
( Within , "within" );
( Wone , "wone" );
( Wor , "wor" );
( Wreal , "wreal" );
( Xnor , "xnor" );
( Xor , "xor" );
( Zi_nd , "zi_nd" );
( Zi_np , "zi_np" );
( Zi_zd , "zi_zd" );
( Zi_zp , "zi_zp" );
      ];
    fun s -> Hashtbl.find h s

let tok' = Source_text_verible_tokens.getstr

let (typehash:(string,unit)Hashtbl.t) = Hashtbl.create 257
let (packhash:(string,unit)Hashtbl.t) = Hashtbl.create 257

let import_seen = ref false

let tok arg =
  import_seen := (match arg with Import -> true | Package -> true | _ -> !import_seen);
  if false then print_endline ("tok: "^tok' arg);
  [arg]
}

let ident = ['a'-'z' 'A'-'Z' '$' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9' '$']*
let escaped = '\\'[^' ']*' '
let digits_ = ['0'-'9'] ['0'-'9' '_']*
let exp_ = ['E' 'e'] ['+' '-']? digits_
(* SV real literal: D.D | D[.D]E[±]D — underscores allowed for grouping.
 * The plain `D.D` form has no exponent; the exponent form may skip the
 * fractional part (e.g. `23E10`). *)
let fltnum = digits_ '.' digits_ exp_?
           | digits_ exp_
(* Sized literal: `<W>'<s>?<base><digits>`.  SV allows the base char
 * in any case (`'H` and `'h` both legal; `'S` is sign-extension);
 * we accept both. *)
let sizednumber = ['0'-'9']*'\''['s' 'S']*['b' 'B' 'd' 'D' 'h' 'H' 'o' 'O' 'x' 'X'][' ']*[ '0'-'9' 'a'-'f' 'x' 'z' 'A'-'F' 'X' 'Z' '_' '?']+
let number = ['0'-'9' '_']+
let unbased = '''['0'-'9'  'a'-'f' 'x' 'z' 'A'-'F' 'X' 'Z' '_' '?']+
let space = [' ' '\t' '\r']+
let newline = ['\n']
(* String literal with C-style escapes; backslash-escape clause first
 * so an escaped quote is consumed as one unit. *)
let qstring = '"' ( '\\' _ | [^ '"' '\\'] )* '"'
let comment = '/''/'[^'\n']*
(* `line` matches ONLY the standard Verilog preprocessor directives,
 * which legitimately consume the rest of the source line. Anything
 * else with a backtick prefix (e.g. `assert(cond);` macro calls in
 * picorv32) is a real macro reference and must be tokenised so the
 * parser can see it. *)
let preproc_kw =
    "define" | "undef" | "ifdef" | "ifndef" | "else" | "elsif" | "endif"
  | "include" | "line" | "pragma" | "timescale" | "default_nettype"
  | "resetall" | "celldefine" | "endcelldefine"
  | "nounconnected_drive" | "unconnected_drive" | "begin_keywords"
  | "end_keywords"
let line = '`' preproc_kw [^'\n']*
let macro_call_id = '`' ident
let colon_begin = ':'[' ']*"begin"
   
rule token = parse
| "<<<=" { tok ( TK_LS_EQ ) }
| ">>>=" { tok ( TK_RSS_EQ ) }
| ">>>" { tok ( GT_GT_GT ) }
| "<<<" { tok ( LT_LT ) } (* placeholder *)
| "===" { tok ( EQ_EQ_EQ ) }
| "!==" { tok ( PLING_EQ_EQ ) }
| "<<=" { tok ( TK_LS_EQ ) }
| ">>=" { tok ( TK_RS_EQ ) }
| "<<" { tok ( LT_LT ) }
| ">>" { tok ( GT_GT ) }
| "<=" { tok ( LT_EQ ) }
| "==" { tok ( EQ_EQ ) }
| ">=" { tok ( GT_EQ ) }
| "!=" { tok ( PLING_EQ ) }
| "||" { tok ( VBAR_VBAR ) }
| "~|" { tok ( TILDE_VBAR ) }
| "~^" { tok ( TILDE_CARET ) }
| "~&" { tok ( TILDE_AMPERSAND ) }
| "+=" { tok ( PLUS_EQ ) }
| "-=" { tok ( HYPHEN_EQ ) }
| "*=" { tok ( STAR_EQ ) }
| "/=" { tok ( SLASH_EQ ) }
| "&=" { tok ( AMPERSAND_EQ ) }
| "|=>" { tok ( VBAR_EQ_GT ) }
| "|->" { tok ( VBAR_HYPHEN_GT ) }
| "|=" { tok ( VBAR_EQ ) }
| "^=" { tok ( CARET_EQ ) }
| ".*" { tok ( DOT_STAR ) }
| "+:" { tok ( PLUS_COLON ) }
| "-:" { tok ( HYPHEN_COLON ) }
| "::" { tok ( COLON_COLON ) }
| "++" { tok ( PLUS_PLUS ) }
| "--" { tok ( HYPHEN_HYPHEN ) }
| "**" { tok ( STAR_STAR ) }
| "&&" { tok ( AMPERSAND_AMPERSAND ) }
| "'{" { tok ( QUOTE_LBRACE ) }
| '-' { tok ( HYPHEN ) }
| '+' { tok ( PLUS ) }
| '!' { tok ( PLING ) }
| '"' { tok ( DOUBLEQUOTE ) }
| '#' { tok ( HASH ) }
| '$' { tok ( DOLLAR ) }
| '%' { tok ( PERCENT ) }
| '&' { tok ( AMPERSAND ) }
| ''' { tok ( QUOTE ) }
| '(' { tok ( LPAREN ) }
| '[' { tok ( LBRACK ) }
| '{' { tok ( LBRACE ) }
| '<' { tok ( LESS ) }
| ')' { tok ( RPAREN ) }
| ']' { tok ( RBRACK ) }
| '}' { tok ( RBRACE ) }
| '>' { tok ( GREATER ) }
| '*' { tok ( STAR ) }
| ',' { tok ( COMMA ) }
| '.' { tok ( DOT ) }
| '/' { tok ( SLASH ) }
| '\\' { tok ( BACKSLASH ) }
| ':' { tok ( COLON ) }
| ';' { tok ( SEMICOLON ) }
| '=' { tok ( EQUALS ) }
| '?' { tok ( QUERY ) }
| '@' { tok ( AT ) }
| '^' { tok ( CARET ) }
| '_' { tok ( UNDERSCORE ) }
| '|' { tok ( VBAR ) }
| '~' { tok ( TILDE ) }

| line { token lexbuf }
(* `<ident>( ... );?` is a macro CALL. Without a real preprocessor
 * we can't substitute the body, and the args may contain whatever
 * the macro syntactically allows — `$display(...)` calls, embedded
 * `;`s, etc. — which doesn't fit Verilog expression grammar. So
 * consume the entire balanced-parens body via the `macro_args`
 * sub-lexer below and silently drop it (plus any trailing `;`). *)
| macro_call_id '(' { macro_args 1 lexbuf; token lexbuf }
(* `<ident>` without an opening paren is a parameterless macro
 * reference (e.g. picorv32's `FORMAL_KEEP reg [63:0] x;`). Skip. *)
| macro_call_id { token lexbuf }
| "/*" { comment lexbuf }
| "(* " { comment lexbuf }

  | comment
      { token lexbuf }
  | space
      { token lexbuf }
  | newline
      (* Advance both our internal counter AND the lexbuf's position
       * tracking, so parse-error reports show the actual source line
       * instead of always reporting "line 1". *)
      { incr lincnt; Lexing.new_line lexbuf; token lexbuf }
  | sizednumber as n
      { match String.split_on_char '\'' n with
      lft::rght::[] -> (
      let ix = if rght.[0]=='s' || rght.[0]=='S' then 1 else 0 in
      let rght' = String.trim (String.sub rght (ix+1) (String.length rght - ix - 1)) in
      let lft' = lft^"'"^String.sub rght ix 1 in
      (* Accept both upper- and lower-case base chars (`'H` ≡ `'h`).
       * SV is case-insensitive on the base letter; the lexer is the
       * right place to normalise. *)
      match Char.lowercase_ascii rght.[ix] with
	| 'b' -> [TK_BinBase lft';TK_BinDigits rght']
	| 'o' -> [TK_OctBase lft';TK_OctDigits rght']
	| 'd' -> [TK_DecBase lft';TK_DecDigits rght']
	| 'h' -> [TK_HexBase lft';TK_HexDigits rght']
	| _ -> failwith rght)
      | _ -> failwith n
      }
  | number as n { tok (TK_DecNumber n) }
  | unbased as n { tok (TK_UnBasedNumber n) }
  | fltnum as f
      { tok ( TK_RealTime f) }
  | ident as s
      { tok ( try keyword s with Not_found -> try systask s with Not_found -> SymbolIdentifier s ) }
  | escaped as s
      (* Escaped identifier: `\<chars> ` — strip the leading backslash
       * and the trailing terminator space, keep the inner text as a
       * normal symbol identifier so downstream references resolve. *)
      { let s = String.sub s 1 (String.length s - 2) in
        tok ( SymbolIdentifier s ) }
  | qstring as s
      { tok ( TK_StringLiteral (String.sub s 1 (String.length s - 2)) ) }
  | eof
      { tok ( End_of_file ) }

| _ as oth
{ tok ( failwith ("Source_text_lex: "^String.make 1 oth) ) }

and comment = parse
| newline { incr lincnt; Lexing.new_line lexbuf; comment lexbuf }
| '*''/' as com { if false then print_endline ("/*"^com); token lexbuf }
| '*'')' as com { if false then print_endline ("/*"^com); token lexbuf }
| _ { comment lexbuf }

(* Skip the balanced-parens body of a macro call. `depth` is the
 * number of unclosed `(` already consumed (the caller passes 1 after
 * matching the macro's opening paren). String literals are tracked
 * so that `(`/`)` inside them don't affect the depth. The trailing
 * `;` (if any) is left for the parser to consume — it stands in for
 * the empty statement that the skipped macro produces in contexts
 * like `if (cond) `assert(...);`. *)
and macro_args depth = parse
| '(' { macro_args (depth + 1) lexbuf }
| ')' { if depth <= 1 then () else macro_args (depth - 1) lexbuf }
| '"' { macro_args_string depth lexbuf }
| newline { incr lincnt; macro_args depth lexbuf }
| _ { macro_args depth lexbuf }
| eof { () }

and macro_args_string depth = parse
| '\\' '"' { macro_args_string depth lexbuf }
| '"' { macro_args depth lexbuf }
| newline { incr lincnt; macro_args_string depth lexbuf }
| _ { macro_args_string depth lexbuf }
| eof { () }

(* pre-processing should have removed this stuff, but if not, skip over it *)
   
and ifdef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifdef lexbuf }
| _ { ifdef lexbuf }

and ifndef = parse
| "`endif" { token lexbuf }
| newline { incr lincnt; ifndef lexbuf }
| _ { ifndef lexbuf }

{
let parse_output_ast_from_file v =
  let ch = open_in v in
  let lb = Lexing.from_channel ch in
  let output = try
      ml_start (deflate token) lb
  with
    | Parsing.Parse_error ->
      let n = Lexing.lexeme_start lb in
      failwith (Printf.sprintf "Output.parse: parse error at character %d" n);
  in
  close_in ch;
  output
}
