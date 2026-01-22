(*****************************************************************************)
(*****************************************************************************)
(*                                                                           *)
(*     File name:        VsymlSettings.ml                                    *)
(*     Description:      Settings for the application.                       *)
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

exception SettingsFailure of string;;

(* type for the result of the parse_arguments function *)
type vsyml_settings = {
  systemshell: string;
  systemreturncode: int;
  systemcompresscommand: string;
  systemcompresscommandpid: int;
  filelibrary: string;
  fileselectedlist: (string*string) list; (* library and file names *)
  fileparsedlist: VhdlTypes.vhdl_parsed_file_list;
  filefailedlist: string list;
  fileparsestdin: string; (* library name to parse stdin *)
  statusfilename: string;
  statusappend: bool;
  statuspid: int;
  status: Unix.file_descr;
  statuschan: out_channel;
  rawresult: Unix.file_descr;
  rawresultchan: out_channel;
  finalresultfilename: string;
  finalresultappend: bool;
  finalresultpid: int;
  finalresult: Unix.file_descr;
  statusverbose: bool;
  statusdebug: bool;
  statuscopyright: bool;
  statusversion: bool;
  statusmessages: string list;
  resultdescription: bool;
  resultdescriptionalldeclarations: bool;
  resulttemporalreport: bool;
  resultobjectreport: bool;
  resultfilterlist: (string*bool*Str.regexp) list; (* string representation,case sensitive,regular expression *)
  resultqualifiednames: bool;
  resultactivity: bool;
  resultxmlbyteordermask: bool;
  resultxmlindentation: int;
  resultxmlmaxline: int;
  resultxmlnamespace: bool;
  resultxmlprolog: bool;
  resultxmlstylesheet: string;
  resulttext: bool;
  resultmarshaleddata: bool;
  elaborationoperatoroverloading: bool;
  elaborationoperatorprettyprinting: bool;
  elaborationblackboxinstance: bool;
  elaborationblackboxsubprogram: bool;
  elaborationtoplevel: string;
  elaborationattributeserror: bool;
  elaborationarchitecturestatements: bool;
  elaborationpreevaluation: bool;
  simulationenable: bool;
  simulationmaxtime: string;
  simulationmaxdelta: string;
  simulationkeepdelta: bool;
  simulationobjectexpansionmasks: (string*bool*Str.regexp) list; (* string representation,case sensitive,regular expression *)
  simulationoperatorexpansion: bool;
  simulationoperandreduction: bool;
  simulationinitialvalues: bool;
  simulationwarnings: bool;
  };;

(*

(* this function extracts the path given a file name *)
let base_directory =
  let reg_exp = Str.regexp "\\(.*\\)bin[\\/][a-zA-Z0-9]*";
  and executable_file_name = Sys.executable_name;
  in
    if Str.string_match reg_exp executable_file_name 0 then
      Str.matched_group 1 executable_file_name
    else
      "."

*)

(* default settings *)
let settings = ref {
  systemshell="/bin/sh -c";
  systemreturncode=0;
  systemcompresscommand="";
  systemcompresscommandpid=0;
  filelibrary="work";
  fileselectedlist=[];
  fileparsedlist=[];
  filefailedlist=[];
  fileparsestdin="";
  statusfilename="";
  statusappend=false;
  statuspid=0;
  status=Unix.stderr;
  statuschan=stderr;
  rawresult=Unix.stdout;
  rawresultchan=stdout;
  finalresultfilename="";
  finalresultappend=false;
  finalresultpid=0;
  finalresult=Unix.stdout;
  statusverbose=true;
  statusdebug=false;
  statuscopyright=true;
  statusversion=false;
  statusmessages=[];
  resultdescription=false;
  resultdescriptionalldeclarations=false;
  resulttemporalreport=false;
  resultobjectreport=false;
  resultfilterlist=[];
  resultqualifiednames=false;
  resultactivity=false;
  resultxmlbyteordermask=false;
  resultxmlindentation=2;
  resultxmlmaxline=80;
  resultxmlnamespace=false;
  resultxmlprolog=false;
  resultxmlstylesheet="";
  resulttext=false;
  resultmarshaleddata=false;
  elaborationoperatoroverloading=false;
  elaborationoperatorprettyprinting=false;
  elaborationblackboxinstance=false;
  elaborationblackboxsubprogram=false;
  elaborationtoplevel="";
  elaborationattributeserror=false;
  elaborationarchitecturestatements=true;
  elaborationpreevaluation=true;
  simulationenable=false;
  simulationmaxtime="100 ns";
  simulationmaxdelta="100";
  simulationkeepdelta=false;
  simulationobjectexpansionmasks=[];
  simulationoperatorexpansion=true;
  simulationoperandreduction=true;
  simulationinitialvalues=false;
  simulationwarnings=false;
  };;

(* helper function to find an item in a list *)
let rec ocamlutils_find_item predicate alist =
  match alist with
  | []
      -> None;
  | item::next_alist
      -> if predicate item then
           Some item
         else
           ocamlutils_find_item predicate next_alist;;

let settings_add_file settings library_name file_name =
  let compare_file_names library_name file_name (comp_library_name,comp_file_name) =
    ((String.compare file_name comp_file_name) = 0);
  in
    match ocamlutils_find_item (compare_file_names library_name file_name) !settings.fileselectedlist with
    | Some (existing_library_name,existing_file_name)
        -> if !settings.statusverbose then
             settings := {!settings with statusmessages = !settings.statusmessages (* @ [text_existing_file existing_file_name existing_library_name] *) };
    | None
        -> settings := {!settings with fileselectedlist = !settings.fileselectedlist @ [library_name,file_name]};;
(*
let load_settings settings filename =
  let rec string_of_node node =
    match node with
    | Xml.Element (_,_,[Xml.PCData value])
        -> value;
    | Xml.Element (_,_,[])
        -> "";
    | _ -> raise (SettingsFailure "string_of_node");
  and boolean_of_node node =
    match node with
    | Xml.Element (_,_,[Xml.PCData "0"])
    | Xml.Element (_,_,[Xml.PCData "false"])
        -> false;
    | Xml.Element (_,_,[Xml.PCData "1"])
    | Xml.Element (_,_,[Xml.PCData "true"])
        -> true;
    | _ -> raise (SettingsFailure "boolean_of_node");
  and object_expansion_of_node node =
    match node with
    | Xml.Element (_,_,[Xml.PCData "none"])
        -> DesignEval.ExpandNone;
    | Xml.Element (_,_,[Xml.PCData "comb"])
        -> DesignEval.ExpandCombinational;
    | Xml.Element (_,_,[Xml.PCData "all"])
        -> DesignEval.ExpandAll;
    | _ -> raise (SettingsFailure "object_expansion_of_node");
  and parse_system_settings_elem node =
    match Xml.tag node with
    | "shell"
        -> settings := {!settings with systemshell = string_of_node node};
    | "compress"
        -> settings := {!settings with systemcompresscommand = string_of_node node};
    | _ -> raise (SettingsFailure "parse_system_settings_elem");
  and parse_file_settings_elem node =
    match Xml.tag node with
    | "library"
        -> settings := {!settings with filelibrary = string_of_node node};
    | "stdin"
        -> settings := {!settings with fileparsestdin = string_of_node node};
    | "file"
        -> settings_add_file settings !settings.filelibrary (string_of_node node);
    | "librarypath"
        -> settings := {!settings with filelibrarypath = !settings.filelibrarypath @ [string_of_node node]};
    | _ -> raise (SettingsFailure "parse_file_settings_elem");
  and parse_status_settings_elem node =
    match Xml.tag node with
    | "filename"
        -> settings := {!settings with statusfilename = string_of_node node};
    | "append"
        -> settings := {!settings with statusappend = boolean_of_node node};
    | "verbose"
        -> settings := {!settings with statusverbose = boolean_of_node node};
    | "debug"
        -> settings := {!settings with statusdebug = boolean_of_node node};
    | "copyright"
        -> settings := {!settings with statuscopyright = boolean_of_node node};
    | "version"
        -> settings := {!settings with statusversion = boolean_of_node node};
    | "message"
        -> settings := {!settings with statusmessages = !settings.statusmessages @ [string_of_node node]};
    | _ -> raise (SettingsFailure "parse_status_settings_elem");
  and parse_result_settings_elem node =
    match Xml.tag node with
    | "filename"
        -> settings := {!settings with finalresultfilename = string_of_node node};
    | "append"
        -> settings := {!settings with finalresultappend = boolean_of_node node};
    | "description"
        -> settings := {!settings with resultdescription = boolean_of_node node};
    | "alldeclarations"
        -> settings := {!settings with resultdescriptionalldeclarations = boolean_of_node node};
    | "temporalreport"
        -> settings := {!settings with resulttemporalreport = boolean_of_node node};
    | "objectreport"
        -> settings := {!settings with resultobjectreport = boolean_of_node node};
    | "filter"
        -> settings := {!settings with resultfilterlist = !settings.resultfilterlist @ [string_of_node node,true,Str.regexp (string_of_node node)]};
    | "ifilter"
        -> settings := {!settings with resultfilterlist = !settings.resultfilterlist @ [string_of_node node,false,Str.regexp_case_fold (string_of_node node)]};
    | "qualifiednames"
        -> settings := {!settings with resultqualifiednames = boolean_of_node node};
    | "activity"
        -> settings := {!settings with resultactivity = boolean_of_node node};
    | "text"
        -> settings := {!settings with resulttext = boolean_of_node node};
    | "marshaled"
        -> settings := {!settings with resultmarshaleddata = boolean_of_node node};
    | "xmlbom"
        -> settings := {!settings with resultxmlbyteordermask = boolean_of_node node};
    | "xmlindent"
        -> settings := {!settings with resultxmlindentation = int_of_string (string_of_node node)};
    | "xmlmaxline"
        -> settings := {!settings with resultxmlmaxline = int_of_string (string_of_node node)};
    | "xmlnamespace"
        -> settings := {!settings with resultxmlnamespace = boolean_of_node node};
    | "xmlprolog"
        -> settings := {!settings with resultxmlprolog = boolean_of_node node};
    | "xmlstylesheet"
        -> settings := {!settings with resultxmlstylesheet = string_of_node node};
    | _ -> raise (SettingsFailure "parse_result_settings_elem");
  and parse_elaboration_settings_elem node =
    match Xml.tag node with
    | "operatoroverloading"
        -> settings := {!settings with elaborationoperatoroverloading = boolean_of_node node};
    | "operatorprettyprinting"
        -> settings := {!settings with elaborationoperatorprettyprinting = boolean_of_node node};
    | "blackboxinstance"
        -> settings := {!settings with elaborationblackboxinstance = boolean_of_node node};
    | "blackboxsubprogram"
        -> settings := {!settings with elaborationblackboxsubprogram = boolean_of_node node};
    | "toplevel"
        -> settings := {!settings with elaborationtoplevel = string_of_node node};
    | "attributeserror"
        -> settings := {!settings with elaborationattributeserror = boolean_of_node node};
    | "architecturestatements"
        -> settings := {!settings with elaborationarchitecturestatements = boolean_of_node node};
    | "preevaluation"
        -> settings := {!settings with elaborationpreevaluation = boolean_of_node node};
    | _ -> raise (SettingsFailure "parse_elaboration_settings_elem");
  and parse_simulation_settings_elem node =
    match Xml.tag node with
    | "enable"
        -> settings := {!settings with simulationenable = boolean_of_node node};
    | "maxtime"
        -> settings := {!settings with simulationmaxtime = string_of_node node};
    | "maxdelta"
        -> settings := {!settings with simulationmaxdelta = string_of_node node};
    | "keepdelta"
        -> settings := {!settings with simulationkeepdelta = boolean_of_node node};
    | "objectexpansion"
        -> settings := {!settings with simulationobjectexpansion = object_expansion_of_node node};
    | "objectexpansionmask"
        -> settings := {!settings with simulationobjectexpansionmasks = !settings.simulationobjectexpansionmasks @ [string_of_node node,true,Str.regexp (string_of_node node)]};
    | "objectexpansionimask"
        -> settings := {!settings with simulationobjectexpansionmasks = !settings.simulationobjectexpansionmasks @ [string_of_node node,false,Str.regexp_case_fold (string_of_node node)]};
    | "operatorexpansion"
        -> settings := {!settings with simulationoperatorexpansion = boolean_of_node node};
    | "operandreduction"
        -> settings := {!settings with simulationoperandreduction = boolean_of_node node};
    | "initialvalues"
        -> settings := {!settings with simulationinitialvalues = boolean_of_node node};
    | "warnings"
        -> settings := {!settings with simulationwarnings = boolean_of_node node};
    | _ -> raise (SettingsFailure "parse_simulation_settings_elem");
  and parse_symbolic_simulator_settings_elem node =
    match Xml.tag node with
    | "systemsettings"
        -> Xml.iter parse_system_settings_elem node;
    | "filesettings"
        -> Xml.iter parse_file_settings_elem node;
    | "statussettings"
        -> Xml.iter parse_status_settings_elem node;
    | "resultsettings"
        -> Xml.iter parse_result_settings_elem node;
    | "elaborationsettings"
        -> Xml.iter parse_elaboration_settings_elem node;
    | "simulationsettings"
        -> Xml.iter parse_simulation_settings_elem node;
    | _ -> raise (SettingsFailure "parse_symbolic_simulator_settings_elem");
  in Xml.iter parse_symbolic_simulator_settings_elem (Xml.parse_file filename);;

let save_settings settings filename =
  let rec node_of_string name value =
    Xml.Element (name,[],[Xml.PCData value]);
  and node_of_boolean name value =
    match value with
    | true
        -> Xml.Element (name,[],[Xml.PCData "true"]);
    | false
        -> Xml.Element (name,[],[Xml.PCData "false"]);
  and node_of_object_expansion name value =
    match value with
    | DesignEval.ExpandNone
        -> Xml.Element (name,[],[Xml.PCData "none"]);
    | DesignEval.ExpandCombinational
        -> Xml.Element (name,[],[Xml.PCData "comb"]);
    | DesignEval.ExpandAll
        -> Xml.Element (name,[],[Xml.PCData "all"]);
  and create_system_settings_elem name =
    Xml.Element (name,[],[node_of_string "shell" settings.systemshell;
                          node_of_string "compress" settings.systemcompresscommand]);
  and create_file_settings_elem name =
    let rec create_file_elems prev_library_name acc dec =
      match dec with
      | []
          -> acc;
      | (library_name,file_name)::next_dec
          when String.compare library_name prev_library_name = 0
          -> create_file_elems library_name (acc @ [node_of_string "file" file_name]) next_dec;
      | (library_name,file_name)::next_dec
          -> create_file_elems library_name (acc @ [node_of_string "library" library_name;
                                                    node_of_string "file" file_name]) next_dec;
    in Xml.Element (name,[],(List.map (node_of_string "librarypath") settings.filelibrarypath) @
                            (create_file_elems "" [] settings.fileselectedlist) @
                            [node_of_string "library" settings.filelibrary;
                             node_of_string "stdin" settings.fileparsestdin]);
  and create_filter_elems case_sensitive_name case_insensitive_name acc dec =
      match dec with
      | []
          -> acc;
      | (reg_exp_str,true,_)::next_dec
          -> create_filter_elems case_sensitive_name case_insensitive_name ((node_of_string case_sensitive_name reg_exp_str)::acc) next_dec;
      | (reg_exp_str,false,_)::next_dec
          -> create_filter_elems case_sensitive_name case_insensitive_name ((node_of_string case_insensitive_name reg_exp_str)::acc) next_dec;
  and create_status_settings_elem name =
    Xml.Element (name,[],[node_of_string "filename" settings.statusfilename;
                          node_of_boolean "append" settings.statusappend;
                          node_of_boolean "verbose" settings.statusverbose;
                          node_of_boolean "debug" settings.statusdebug;
                          node_of_boolean "copyright" settings.statuscopyright;
                          node_of_boolean "version" settings.statusversion] @
                         (List.map (node_of_string "message") settings.statusmessages));
  and create_result_settings_elem name =
    Xml.Element (name,[],[node_of_string "filename" settings.finalresultfilename;
                          node_of_boolean "append" settings.finalresultappend;
                          node_of_boolean "description" settings.resultdescription;
                          node_of_boolean "alldeclarations" settings.resultdescriptionalldeclarations;
                          node_of_boolean "temporalreport" settings.resulttemporalreport;
                          node_of_boolean "objectreport" settings.resultobjectreport] @
                         (create_filter_elems "filter" "ifilter" [] settings.resultfilterlist) @
                         [node_of_boolean "qualifiednames" settings.resultqualifiednames;
                          node_of_boolean "activity" settings.resultactivity;
                          node_of_boolean "text" settings.resulttext;
                          node_of_boolean "marshaled" settings.resultmarshaleddata;
                          node_of_boolean "xmlbom" settings.resultxmlbyteordermask;
                          node_of_string "xmlindent" (string_of_int settings.resultxmlindentation);
                          node_of_string "xmlmaxline" (string_of_int settings.resultxmlmaxline);
                          node_of_boolean "xmlnamespace" settings.resultxmlnamespace;
                          node_of_boolean "xmlprolog" settings.resultxmlprolog;
                          node_of_string "xmlstylesheet" settings.resultxmlstylesheet]);
  and create_elaboration_settings_elem name =
    Xml.Element (name,[],[node_of_boolean "operatoroverloading" settings.elaborationoperatoroverloading;
                          node_of_boolean "operatorprettyprinting" settings.elaborationoperatorprettyprinting;
                          node_of_boolean "blackboxinstance" settings.elaborationblackboxinstance;
                          node_of_boolean "blackboxsubprogram" settings.elaborationblackboxsubprogram;
                          node_of_string "toplevel" settings.elaborationtoplevel;
                          node_of_boolean "attributeserror" settings.elaborationattributeserror;
                          node_of_boolean "architecturestatements" settings.elaborationarchitecturestatements;
                          node_of_boolean "preevaluation" settings.elaborationpreevaluation]);
  and create_simulation_settings_elem name =
    Xml.Element (name,[],[node_of_boolean "enable" settings.simulationenable;
                          node_of_string "maxtime" settings.simulationmaxtime;
                          node_of_string "maxdelta" settings.simulationmaxdelta;
                          node_of_boolean "keepdelta" settings.simulationkeepdelta;
                          node_of_object_expansion "objectexpansion" settings.simulationobjectexpansion] @
                         (create_filter_elems "objectexpansionmask" "objectexpansionimask" [] settings.resultfilterlist) @
                         [node_of_boolean "operatorexpansion" settings.simulationoperatorexpansion;
                          node_of_boolean "operandreduction" settings.simulationoperandreduction;
                          node_of_boolean "initialvalues" settings.simulationinitialvalues;
                          node_of_boolean "warnings" settings.simulationwarnings]);
  and create_symbolic_simulator_settings_elem name =
    Xml.Element (name,[],[create_system_settings_elem "systemsettings";
                          create_file_settings_elem "filesettings";
                          create_status_settings_elem "statussettings";
                          create_result_settings_elem "resultsettings";
                          create_elaboration_settings_elem "elaborationsettings";
                          create_simulation_settings_elem "simulationsettings"]);
  in
    let file_channel =
      open_out filename;
    and root_elem =
      create_symbolic_simulator_settings_elem "symbolicsimulatorsettings";
    in
      output_string file_channel (Xml.to_string_fmt root_elem);
      close_out file_channel;;
*)

