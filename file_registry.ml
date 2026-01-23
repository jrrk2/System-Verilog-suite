(* file_registry.ml - Type-safe file management for interactive mode *)

(* File types in the HDL toolchain *)
type file_type =
  | SystemVerilog       (* .sv, .v files *)
  | VHDL                (* .vhd, .vhdl files *)
  | VerilatorJSON       (* Verilator JSON AST *)
  | YosysRTLIL          (* Yosys RTLIL/ILANG *)
  | BehavioralIR        (* Behavioral IR (in-memory or serialized) *)
  | OptimizedIR         (* Optimized Behavioral IR *)
  | Unknown             (* Unknown/unregistered file *)

(* File handle with type information *)
type file_handle = {
  path: string;
  file_type: file_type;
  metadata: (string * string) list;
}

(* Available transformations *)
type transformation =
  | SVToVerilatorJSON of { input: string; output: string }
  | SVToYosysRTLIL of { input: string; output: string }
  | SVToBehavioralIR of { input: string }
  | VHDLToBehavioralIR of { input: string }
  | VerilatorJSONToBehavioralIR of { input: string }
  | YosysRTLILToOptIR of { input: string }
  | BehavioralIRToOptimizedIR of { input: string }

(* Registry state *)
type registry = {
  mutable files: (string, file_handle) Hashtbl.t;
  mutable temp_counter: int;
}

let create_registry () = {
  files = Hashtbl.create 100;
  temp_counter = 0;
}

(* Infer file type from extension *)
let infer_file_type path =
  if Filename.check_suffix path ".sv" || Filename.check_suffix path ".v" then
    SystemVerilog
  else if Filename.check_suffix path ".vhd" || Filename.check_suffix path ".vhdl" then
    VHDL
  else if Filename.check_suffix path ".json" then
    VerilatorJSON
  else if Filename.check_suffix path ".il" || Filename.check_suffix path ".rtlil" then
    YosysRTLIL
  else
    Unknown

(* String representation of file type *)
let string_of_file_type = function
  | SystemVerilog -> "SystemVerilog"
  | VHDL -> "VHDL"
  | VerilatorJSON -> "Verilator JSON"
  | YosysRTLIL -> "Yosys RTLIL"
  | BehavioralIR -> "Behavioral IR"
  | OptimizedIR -> "Optimized IR"
  | Unknown -> "Unknown"

(* Register a file in the registry *)
let register_file registry path =
  if not (Sys.file_exists path) then begin
    Printf.printf "✗ Error: File not found: %s\n" path;
    None
  end else begin
    let file_type = infer_file_type path in
    let handle = {
      path;
      file_type;
      metadata = [];
    } in
    Hashtbl.replace registry.files path handle;
    Printf.printf "✓ Registered: %s [%s]\n" path (string_of_file_type file_type);
    Some handle
  end

(* Look up file in registry *)
let lookup_file registry path =
  match Hashtbl.find_opt registry.files path with
  | Some handle -> Some handle
  | None ->
      (* Try to auto-register if file exists *)
      if Sys.file_exists path then
        register_file registry path
      else
        None

(* Generate temporary file path *)
let gen_temp_path registry ext =
  registry.temp_counter <- registry.temp_counter + 1;
  Printf.sprintf "/tmp/sv_interactive_%d%s" registry.temp_counter ext

(* Valid transformations for each file type *)
let valid_transformations = function
  | SystemVerilog -> ["verilator-json"; "synth-yosys"; "to-behavioral-ir"]
  | VHDL -> ["to-behavioral-ir"]
  | VerilatorJSON -> ["to-behavioral-ir"]
  | YosysRTLIL -> ["to-opt-ir"]
  | BehavioralIR -> ["optimize"; "to-z3"]
  | OptimizedIR -> ["to-z3"; "to-hardcaml"]
  | Unknown -> []

(* Check if transformation is valid *)
let can_transform file_type target_type =
  match file_type, target_type with
  | SystemVerilog, VerilatorJSON -> true
  | SystemVerilog, YosysRTLIL -> true
  | SystemVerilog, BehavioralIR -> true
  | VHDL, BehavioralIR -> true
  | VerilatorJSON, BehavioralIR -> true
  | YosysRTLIL, OptimizedIR -> true
  | BehavioralIR, OptimizedIR -> true
  | _, _ -> false

(* Execute transformation *)
let execute_transformation registry trans =
  match trans with
  | SVToVerilatorJSON { input; output } ->
      Printf.printf "→ Converting %s to Verilator JSON...\n" input;
      let cmd = Printf.sprintf "verilator --json-only --json-only-output %s -Wno-fatal %s 2>&1 > /dev/null"
        output input in
      let exit_code = Sys.command cmd in
      if exit_code = 0 then begin
        Printf.printf "✓ Generated: %s\n" output;
        let _ = register_file registry output in
        Some output
      end else begin
        Printf.printf "✗ Conversion failed\n";
        None
      end

  | SVToYosysRTLIL { input; output } ->
      Printf.printf "→ Synthesizing %s with Yosys...\n" input;
      let cmd = Printf.sprintf "yosys -q -q -p 'read_verilog -sv %s; synth; write_rtlil %s'"
        input output in
      let exit_code = Sys.command cmd in
      if exit_code = 0 then begin
        Printf.printf "✓ Generated: %s\n" output;
        let _ = register_file registry output in
        Some output
      end else begin
        Printf.printf "✗ Conversion failed\n";
        None
      end

  | _ ->
      Printf.printf "✗ Transformation not yet implemented\n";
      None

(* Auto-convert file to target type *)
let auto_convert registry path target_type =
  match lookup_file registry path with
  | None ->
      Printf.printf "✗ File not found or cannot be registered: %s\n" path;
      None
  | Some handle when handle.file_type = target_type ->
      (* Already correct type *)
      Some path
  | Some handle when can_transform handle.file_type target_type ->
      (* Need to convert *)
      Printf.printf "📋 Auto-converting %s from %s to %s\n"
        path
        (string_of_file_type handle.file_type)
        (string_of_file_type target_type);

      let output = match target_type with
        | VerilatorJSON -> gen_temp_path registry ".json"
        | YosysRTLIL -> gen_temp_path registry ".il"
        | BehavioralIR -> gen_temp_path registry ".bir"
        | OptimizedIR -> gen_temp_path registry ".oir"
        | _ -> gen_temp_path registry ".tmp"
      in

      let trans = match handle.file_type, target_type with
        | SystemVerilog, VerilatorJSON ->
            Some (SVToVerilatorJSON { input = path; output })
        | SystemVerilog, YosysRTLIL ->
            Some (SVToYosysRTLIL { input = path; output })
        | _ -> None
      in

      (match trans with
       | Some t -> execute_transformation registry t
       | None ->
           Printf.printf "✗ No automatic conversion path available\n";
           None)

  | Some handle ->
      Printf.printf "✗ Cannot convert %s to %s\n"
        (string_of_file_type handle.file_type)
        (string_of_file_type target_type);
      Printf.printf "  Valid transformations: %s\n"
        (String.concat ", " (valid_transformations handle.file_type));
      None

(* List all registered files *)
let list_files registry =
  if Hashtbl.length registry.files = 0 then
    Printf.printf "No files registered\n"
  else begin
    Printf.printf "\nRegistered Files:\n";
    Hashtbl.iter (fun path handle ->
      Printf.printf "  • %s [%s]\n" path (string_of_file_type handle.file_type)
    ) registry.files
  end

(* Show valid transformations for a file *)
let show_transformations registry path =
  match lookup_file registry path with
  | None ->
      Printf.printf "✗ File not found: %s\n" path
  | Some handle ->
      Printf.printf "File: %s [%s]\n" path (string_of_file_type handle.file_type);
      let transforms = valid_transformations handle.file_type in
      if List.length transforms = 0 then
        Printf.printf "  No transformations available\n"
      else begin
        Printf.printf "  Valid transformations:\n";
        List.iter (fun t -> Printf.printf "    • %s\n" t) transforms
      end
