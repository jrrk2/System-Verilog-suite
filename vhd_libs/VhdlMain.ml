(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*     File name:        VhdlMain.ml                                         *)
(*     Description:      Application main file: VHDL parser main entry point *)
(*                       and structural functions.                           *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(*   Copyright (C) 2008-2010 TIMA Laboratory - VDS Team                      *)
(*                                                                           *)
(*   This program is free software: you can redistribute it and/or modify    *)
(*   it under the terms of the GNU General Public License as published by    *)
(*   the Free Software Foundation, either version 3 of the License, or       *)
(*   (at your option) any later version.                                     *)
(*                                                                           *)
(*   This program is distributed in the hope that it will be useful,         *)
(*   but WITHOUT ANY WARRANTY; without even the implied warranty of          *)
(*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           *)
(*   GNU General Public License for more details.                            *)
(*                                                                           *)
(*   You should have received a copy of the GNU General Public License       *)
(*   along with this program.  If not, see <http://www.gnu.org/licenses/>.   *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(*     Author:      Florent Ouchet (Intern at TIMA-VDS)                      *)
(*     Version:     1.7 (April 2010)                                         *)
(*                                                                           *)
(*****************************************************************************)
(*                                                                           *)
(* $Rev:: 1431                                                             $ *)
(* $Author:: ouchet                                                        $ *)
(* $Date:: 2010-04-06 19:28:25 +0200 (Tue, 06 Apr 2010)                    $ *)
(*                                                                           *)
(*****************************************************************************)
(*****************************************************************************)

(* this project's namespaces *)
open VhdlTypes;;
open VhdlLexer;;
open VhdlParser;;
open VhdlSettings;;
(* open Vconvert;; *)
open VhdlTree;;

(* override stack size *)
Gc.set { (Gc.get()) with Gc.stack_limit= 100000000 };;

type design_source_object = {
  sourcepath: string; (* qualified name with prefix *)
  sourcename: string; (* original name as declared in VHDL source code *)
  sourcefile: string; (* declaration file *)
  sourcechar: int;    (* position in the file *)
  }

type design_assertion_severity =
  | AssertionNote
  | AssertionWarning
  | AssertionError
  | AssertionFailure
  | AssertionOther;;


exception FileNotFoundFailure of string(* exception raised when a file could not be found *)
exception ElaborationError2 of string* (* exception message *)
                               int     (* position that caused this failure *)
exception ElaborationError3 of string* (* exception message *)
                               string* (* file name *)
                               int     (* position that caused this failure *)
exception EvaluationFailure of string

exception AssertionMessage of (design_source_object*      (* source object of the assertion *)
                               string*                    (* assertion message *)
                               design_assertion_severity* (* evaluated severity *)
                               string*                    (* instance complete name *)
                               Big_int.big_int*           (* simulation time *)
                               int);;                     (* simulation delta *)
(* exception type for vhdl parsing *)
exception ParsingFailure1 of int;;              (* position of the failure *)
exception ParsingFailure4 of (int*int*string*string);; (* (line*col*filltext*linetext) position of the failure *)

let tlst = ref []

let dbglexer (buf:Lexing.lexbuf) : VhdlParser.token =
  let tok = VhdlLexer.lexer buf in tlst := tok :: !tlst; tok

(* convert a position in channel to a (line*col*fill) tupple *)
let chan_pos_to_line_col chan pos =
  let rec chan_pos_to_line_col_in i lin col fill text =
    let ch =
      input_char chan
    in
      let fill_str =
        match ch with
        | '\t'
            -> "\t";
        | _ -> " ";
      in
        try
          if (i >= pos) && (ch = '\n') then
            (lin,col,fill,text)
          else
            match ch with
            | '\n'
                -> chan_pos_to_line_col_in (i+1) (lin+1) 0 "" "";
            | _ -> if i >= pos then
                     chan_pos_to_line_col_in (i+1) lin col fill (text^(String.make 1 ch))
                   else
                     chan_pos_to_line_col_in (i+1) lin (col+1) (fill ^ fill_str) (text^(String.make 1 ch));
        with
        | End_of_file -> (lin,col,fill,text);
  in chan_pos_to_line_col_in 0 1 0 "" "";;

(* parse an input channel (either std_in or an opened file) and return a vhdl_design_file structure *)
let parse_vhdl_channel chan seekable =
  let lexbuf = Lexing.from_channel chan in
    try
      VhdlParser.top_level_file dbglexer lexbuf
    with
      Parsing.Parse_error -> let pos = (Lexing.lexeme_start lexbuf) in
                               if not seekable then
                                 raise (ParsingFailure1 pos)
                               else
                                 seek_in chan 0;
                                 raise (ParsingFailure4 (chan_pos_to_line_col chan pos));;

let print_status_open_file_failure outchan verbose name mess =
  if verbose then
    Printf.fprintf outchan "(failure) %s could not be opened, system error with message \"%s\"\n" name mess;;

let print_status_parsing_file outchan verbose filename =
  if verbose then
    Printf.fprintf outchan "parsing file %s: " filename;;

let print_status_parsing_stdin outchan verbose =
  if verbose then
    Printf.fprintf outchan "parsing standard input: ";;

let print_status_success_summary outchan n =
  Printf.fprintf outchan "\n%d item(s) were parsed with success.\n" n;;

let print_status_parse_success_1 outchan verbose =
  if verbose then
    Printf.fprintf outchan "(success) 1 unit was parsed\n";;

let print_status_parse_success_n outchan verbose n =
  if verbose then
    Printf.fprintf outchan "(success) %d units were parsed\n" n;;

let print_status_failure_summary outchan n =
  Printf.fprintf outchan "%d item(s) failed to be parsed.\nexiting with error.\n" n;;

let print_status_failure_1 outchan verbose error pos =
  if verbose then
    Printf.fprintf outchan "(failure) error \"%s\" at position %d\n" error pos;;

let print_status_failure_2 outchan verbose error line col =
  if verbose then
    Printf.fprintf outchan "(failure) error \"%s\" at line %d, column %d\n" error line col;;

let print_status_converting_file outchan verbose filename =
  if verbose then
    Printf.fprintf outchan "converting file %s: " filename;;

let print_status_assertion outchan message instance severity =
  Printf.fprintf outchan "assertion \"%s\" in instance \"%s\" with severity \"%s\"\n" message instance severity;;

let print_status_assertion_time outchan message instance severity time delta =
  Printf.fprintf outchan "assertion \"%s\" in instance \"%s\" with severity \"%s\" at %s fs (delta-cycle %s)\n" message instance severity time delta;;

let print_status_statistics outchan (memory:float) user_time system_time =
  Printf.fprintf outchan "total-memory %.0fMB, user-time %.3fs system-time %.3fs\n" (memory /. 1000000.0) user_time system_time;;

let parsing_error = "parsing error";;
let assertion_severity_note = "note";;
let assertion_severity_warning = "warning";;
let assertion_severity_error = "error";;
let assertion_severity_failure = "failure";;
let assertion_severity_other = "other";;
let descr_copyright     = "VHDL parser frontend (based on VSYML)";;
let descr_version       = "version 0.0 Jan 2012";;

let print_status_copyright outchan display =
  if display then
    Printf.fprintf outchan "%s %s ocaml %s %s (%s bits)\n" descr_copyright descr_version Sys.ocaml_version Sys.os_type (string_of_int Sys.word_size);;

(* this function is a callback to parse library packages on request by the vhdl to design convertor *)
(* this function searchs in all library path, keep order of this list *)
let parse_library_package settings library package =
  let find_file librarypath filename =
    let rec find_file_in path filename =
      match path with
        | [] -> raise (FileNotFoundFailure filename);
        | dir::next_path
            -> try
                 let dir_filename = dir ^ "/" ^ filename;
                 in dir_filename,open_in dir_filename;
               with
                 | Sys_error _ -> find_file_in next_path filename;
    in find_file_in librarypath filename;
  and filename = String.lowercase_ascii (library ^ "." ^ package ^ ".vhd");
  in
    let dir_filename,chan =
      find_file ["."] filename;
    in
      try
        let parsed_units =
          parse_vhdl_channel chan true;
        in close_in chan;
           {parsedfilename=dir_filename;
            parsedfilelibrary=library;
            parsedfileunits=parsed_units};
      with
      | ParsingFailure1 pos
          -> print_status_parsing_file !settings.statuschan !settings.statusverbose dir_filename;
             print_status_failure_1 !settings.statuschan !settings.statusverbose parsing_error pos;
             raise (ParsingFailure1 pos);
      | ParsingFailure4 (line,col,fill,text)
          -> print_status_parsing_file !settings.statuschan !settings.statusverbose dir_filename;
             print_status_failure_2 !settings.statuschan !settings.statusverbose parsing_error line col;
             Printf.fprintf !settings.statuschan "%s\n" text;
             Printf.fprintf !settings.statuschan "%s^\n" fill;
             raise (ParsingFailure4 (line,col,fill,text));;


(* this function is a callback that returns if an object is to be reported or not *)
let object_is_reported default_value filter_list aobject =
  match filter_list with
  (* no explicit filter, all objects are reported *)
  | []
      -> default_value;
  | filter_regexp_list
      -> let rec test_filter_regexp dec =
           match dec with
           | []
               -> false;
           | (_,_,filter_regexp)::next_dec
               -> true;
         in test_filter_regexp filter_regexp_list;;

(* this function is a callback that returns if an object is to be reported or not *)
let rec object_is_expanded filter_list aobject =
  match filter_list with
  (* no explicit filter, all objects are reported *)
  | []
      -> false;
  | (_,_,filter_regexp)::next_dec
      -> true

(* this function parses a channel and add the item to settings    *)
(* function with side effect, the settings parameter is modified: *)
(*  - on success, parsed file is added to parsedfiles field       *)
(*  - on failure, filename is added to failedfiles field          *)
let parse_add_vhdl_channel settings libraryname filename chan seekable =
  try
    let parsed_units = parse_vhdl_channel chan seekable in
      settings := {!settings with fileparsedlist = !settings.fileparsedlist @ [{parsedfilename=filename;
                                                                                parsedfilelibrary=libraryname;
                                                                                parsedfileunits=parsed_units}]};
      let unit_count = List.length parsed_units in
        if unit_count = 1 then
          print_status_parse_success_1 !settings.statuschan !settings.statusverbose
        else
          print_status_parse_success_n !settings.statuschan !settings.statusverbose unit_count;
  with
  | ParsingFailure1 pos
      -> settings := {!settings with filefailedlist = !settings.filefailedlist @ [filename]};
         print_status_failure_1 !settings.statuschan !settings.statusverbose parsing_error pos;
  | ParsingFailure4 (line,col,fill,text)
      -> settings := {!settings with filefailedlist = !settings.filefailedlist @ [filename]};
         print_status_failure_2 !settings.statuschan !settings.statusverbose parsing_error line col;
         Printf.fprintf !settings.statuschan "%s\n" text;
         Printf.fprintf !settings.statuschan "%s^\n" fill
  | err -> Printf.fprintf !settings.statuschan "Unexpected parsing error in %s\n" filename

(* this function is called when an unamed parameter is found in command line *)

let parse_add_vhdl_file settings (libraryname,filename) =
  try
    print_status_parsing_file !settings.statuschan !settings.statusverbose filename;
    let file_channel = open_in filename in
      parse_add_vhdl_channel settings libraryname filename file_channel true;
      close_in file_channel;
  with
    | Sys_error message 
        -> settings := {!settings with filefailedlist = !settings.filefailedlist @ [filename]};
          print_status_open_file_failure !settings.statuschan !settings.statusverbose filename message;;

let write_position_details settings message filename position =
  try
    let file_channel =
      open_in filename;
    in
      let line,column,fill,text =
        chan_pos_to_line_col file_channel position;
      in
        print_status_failure_2 settings.statuschan settings.statusverbose message line column;
        Printf.fprintf settings.statuschan "%s\n" text;
        Printf.fprintf settings.statuschan "%s^\n" fill;
  with
  | _ -> print_status_converting_file settings.statuschan settings.statusverbose filename;
         print_status_failure_1 settings.statuschan settings.statusverbose message position;;

let write_assertion_details settings message instance severity =
  match severity with
  | AssertionNote
      -> print_status_assertion settings.statuschan message instance assertion_severity_note;
  | AssertionWarning
      -> print_status_assertion settings.statuschan message instance assertion_severity_warning;
  | AssertionError
      -> print_status_assertion settings.statuschan message instance assertion_severity_error;
  | AssertionFailure
      -> print_status_assertion settings.statuschan message instance assertion_severity_failure;
  | AssertionOther
      -> print_status_assertion settings.statuschan message instance assertion_severity_other;;

let design_of_parsed_file_list settings =
  let readlib r = parse_library_package settings r.parsedfilelibrary r.parsedfilename in
     ignore(!settings.simulationinitialvalues,
      !settings.elaborationoperatoroverloading,
      !settings.elaborationoperatorprettyprinting,
      !settings.resultqualifiednames,
      !settings.elaborationblackboxinstance,
      !settings.elaborationblackboxsubprogram,
      !settings.elaborationtoplevel,
      !settings.resultdescriptionalldeclarations,
      !settings.elaborationarchitecturestatements,
      !settings.elaborationpreevaluation);
     List.iter (fun r -> List.iter (fun itm -> Hashtbl.add !Vabstraction.vhdlhash (dump_design_unit itm,"") VhdNone) r.parsedfileunits) !settings.fileparsedlist

(* create a design *)
let create_design settings =
  try
    design_of_parsed_file_list settings
  with
  | ElaborationError3 (message,filename,position)
      -> print_status_converting_file !settings.statuschan !settings.statusverbose filename;
         write_position_details !settings message filename position;
         raise (ElaborationError3 (message,filename,position));
  | AssertionMessage (source,message,severity,instance,time,delta)
      -> print_status_converting_file !settings.statuschan !settings.statusverbose source.sourcefile;
         write_assertion_details !settings message instance severity;
         write_position_details !settings message source.sourcefile source.sourcechar;
         raise (AssertionMessage (source,message,severity,instance,time,delta));;

(* main function of the program *)
let main psuccess args = ( if (!Globals.logfile == Setup.Closed) then
      let fd = open_out Globals.tmpnam in
          Globals.logfile := Setup.Open fd);
      settings := {!settings with rawresult = !settings.finalresult};
      (* finally open output channel from file_descr *)
      settings := {!settings with rawresultchan=Unix.out_channel_of_descr !settings.rawresult};
      settings := {!settings with statuschan=Unix.out_channel_of_descr !settings.status};
      List.iter (output_string !settings.statuschan) !settings.statusmessages;

      (* print copyright *)
      print_status_copyright !settings.statuschan !settings.statuscopyright;

      if not !settings.statusversion then
      begin
        (* initialization of keyword hashmap *)
        VhdlLexer.initVhdlLexer;

        (* parse source files *)
        List.iter (fun arg -> parse_add_vhdl_file settings ("work",arg)) args;

        (* display status *)
        print_status_success_summary !settings.statuschan (List.length (!settings.fileparsedlist));
        if (List.length !settings.filefailedlist) = 0 then
        begin
          (* convert vhdl to design description *)
          create_design settings;
        end
        else
        begin (* failure while parsing *)
          print_status_failure_summary !settings.statuschan (List.length (!settings.filefailedlist));
          settings := {!settings with systemreturncode=1};
	  psuccess := false
        end;
      end; (* version mode *)

      let allocated_bytes = Gc.allocated_bytes();
      and process_times = Unix.times();
      in print_status_statistics !settings.statuschan allocated_bytes process_times.Unix.tms_utime process_times.Unix.tms_stime;
      flush stderr;

      (* flush result output channel *)
      flush !settings.rawresultchan;
      (*close_out !settings.rawresultchan;*)
      (* flush status output channel *)
      flush !settings.statuschan;
      (* redirect result and status output channels to stdout and stderr *)
      settings := {!settings with rawresultchan=stdout;statuschan=stderr}

(*
let _ = main(List.tl (Array.to_list Sys.argv))
*)

