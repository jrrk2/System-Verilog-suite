open Sv_ast
open Yojson.Safe.Util

let assoc_add lst kw data = lst := (kw,data) :: !lst
let assoc_find_opt lst kw = List.assoc_opt kw !lst
let assoc_find lst kw = List.assoc kw !lst
let assoc_create _ = ref []

(* Parse type table entries *)
let rec parse_type attr json =
  let node_type = json |> member "type" |> to_string in
  match node_type with
  | "BASICDTYPE" ->
      let keyword = json |> member "keyword" |> to_string_option |> Option.value ~default:"logic" in
      let range = json |> member "range" |> to_string_option in
      BasicType { keyword; range }
      
  | "ENUMDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      let items_json = json |> member "itemsp" |> to_list in
      let items = List.map (fun item ->
        let item_name = item |> member "name" |> to_string in
        let value = item |> member "valuep" |> to_list |> List.hd |> member "name" |> to_string in
        (item_name, value)
      ) items_json in
      EnumType { name; items }
      
  | "REFDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      let dtype = json |> member "dtypep" |> to_string in
      let refdtype = json |> member "refDTypep" |> to_string in
      RefType' { name; dtype; refdtype }
      
  | "VOIDDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      VoidType { name; resolved = None }
      
  | "UNPACKARRAYDTYPE" ->
      let base = json |> member "refDTypep" |> to_string_option |> Option.value ~default:"" in
      (* FIXED: Use declRange if available, otherwise parse rangep *)
      let range = match json |> member "declRange" |> to_string_option with
        | Some decl_range when decl_range <> "" -> 
            (* Remove brackets from "[0:3]" to get "0:3" *)
            if String.length decl_range > 2 then
              String.sub decl_range 1 (String.length decl_range - 2)
            else
              ""
        | _ ->
            (* Fall back to parsing rangep array *)
            let range_json = json |> member "rangep" |> to_list in
            (match range_json with
            | r :: _ -> 
                let left = r |> member "leftp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                let right = r |> member "rightp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                Printf.sprintf "%s:%s" left right
            | _ -> "")
      in
      ArrayType' { base; range }

  | "PACKARRAYDTYPE" ->
      let base = json |> member "refDTypep" |> to_string_option |> Option.value ~default:"" in
      (* FIXED: Use declRange if available, otherwise parse rangep *)
      let range = match json |> member "declRange" |> to_string_option with
        | Some decl_range when decl_range <> "" -> 
            (* Remove brackets from "[0:3]" to get "0:3" *)
            if String.length decl_range > 2 then
              String.sub decl_range 1 (String.length decl_range - 2)
            else
              ""
        | _ ->
            (* Fall back to parsing rangep array *)
            let range_json = json |> member "rangep" |> to_list in
            (match range_json with
            | r :: _ -> 
                let left = r |> member "leftp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                let right = r |> member "rightp" |> to_list |> List.hd |> member "name" |> to_string_option |> Option.value ~default:"0" in
                Printf.sprintf "%s:%s" left right
            | _ -> "")
      in
      PackArrayType' { base; range }

  | "IFACEREFDTYPE" ->
      let ifacename = json |> member "ifaceName" |> to_string_option |> Option.value ~default:"" in
      let modportname = json |> member "modportName" |> to_string_option |> Option.value ~default:"" in
      let ifacep = json |> member "ifacep" |> to_string_option |> Option.value ~default:"" in
      let modportp = json |> member "modportp" |> to_string_option |> Option.value ~default:"" in
      IntfRefType' { ifacename;
      modportname;
      ifacep;
      modportp }

  | "STRUCTDTYPE" ->
      let name = json |> member "name" |> to_string in
      let packed = json |> member "packed" |> to_bool in
      let members = json |> member "membersp" |> to_list |> List.map (parse_type attr) in
      StructType { name; packed; members }

  | "UNIONDTYPE" ->
      let name = json |> member "name" |> to_string in
      let packed = json |> member "packed" |> to_bool in
      let members = json |> member "membersp" |> to_list |> List.map (parse_type attr) in
      UnionType { name; packed; members }

  | "MEMBERDTYPE" ->
      let name = json |> member "name" |> to_string in
      let dtype = json |> member "dtypep" |> to_string in
      let child = json |> member "childDTypep" |> to_list |> List.map (parse_type attr) in
      let value = json |> member "valuep" |> to_list |> List.map (parse_type attr) in
      MemberType' { name; dtype; child; value }

  | "CONSTDTYPE" ->
      let name = json |> member "name" |> to_string in
      let dtype = json |> member "dtypep" |> to_string in
      let child = json |> member "childDTypep" |> to_list |> List.map (parse_type attr) in
      ConstType' { name; dtype; child }

  | "PARAMTYPEDTYPE" ->
      let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      ParamTypeType' { name; dtype }

  | oth -> print_endline oth; UnknownType (node_type, json )

let rec extract_const_value = function
  | `Assoc fields ->
      (try
        let name = List.assoc "name" fields |> to_string in
        name
      with _ -> "/* const value */")
  | _ -> "/* const value */"
  
(* Parse type table and populate global table *)
let rec parse_type_table attr json =
  let types_json = json |> member "typesp" |> to_list in
  List.iter (fun type_json ->
    let addr = type_json |> member "addr" |> to_string_option |> Option.value ~default:"" in
    let parsed_type = parse_type attr type_json in
    if addr <> "" then assoc_add attr.type_table addr parsed_type;
    let modportp = type_json |> member "modportp" |> to_string_option |> Option.value ~default:"" in
    if modportp <> "" then assoc_add attr.type_table modportp parsed_type;
    let varp = type_json |> member "varp" |> to_string_option |> Option.value ~default:"" in
    if varp <> "" then assoc_add attr.type_table varp parsed_type
  ) types_json

let netlist = ref []
let depth = ref []

(* Parse JSON to AST with type table support *)
let rec parse_json attr json =
  let node_type = json |> member "type" |> to_string in
  let name = json |> member "name" |> to_string_option |> Option.value ~default:"" in
  match node_type with
  | "NETLIST" ->
      (* Parse type table first *)
      let misc_list = json |> member "miscsp" |> to_list in
      List.iter (fun misc ->
        if (misc |> member "type" |> to_string) = "TYPETABLE" then
          parse_type_table attr misc
      ) misc_list;
      
      let modules = json |> member "modulesp" |> to_list |> List.map (parse' attr name) in
      (* let type_list = assoc_fold (fun k v acc -> (k, v) :: acc) attr.type_table [] in *)
      netlist := modules;
      Netlist modules
      
  | "MODULE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let m = Module { name; stmts } in
      if addr <> "" then assoc_add attr.module_table addr m;
      m
      
  | "PACKAGE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Package { name; stmts }

  | "IFACE" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let interface_node = Interface { name; params = []; stmts } in
      if addr <> "" then (
			  assoc_add attr.interface_table addr interface_node;
			  assoc_add attr.module_table addr interface_node
			 );
      assoc_add attr.interface_table name interface_node; (* Also store by name *)
      interface_node

  | "CELL" ->
      let pins = json |> member "pinsp" |> to_list |> List.map (parse' attr name) in
      let modp_addr = json |> member "modp" |> to_string_option |> Option.value ~default:"" in
      Cell' { name; modp_addr; pins }
      
  | "PIN" ->
      let expr = try Some (json |> member "exprp" |> to_list |> List.hd |> parse' attr name) with _ -> None in
      Pin { name; expr }
      
  | "MODPORT" ->
      let vars = json |> member "varsp" |> to_list |> List.map (parse' attr name) in
      let m = Modport { name; vars } in
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      if addr <> "" then assoc_add attr.interface_table addr m;
      m
      
  | "MODPORTVARREF" ->
      let direction = json |> member "direction" |> to_string_option |> Option.value ~default:"" in
      let var_ref = json |> member "varp" |> to_string_option |> Option.value ~default:"" in
      let m = ModportVarRef { name; direction; var_ref=attr.parent } in
      assoc_add attr.var_table var_ref m;
      m

  | "VAR" ->
      let addr = json |> member "addr" |> to_string_option |> Option.value ~default:"" in
      let dtype_name = json |> member "dtypeName" |> to_string_option |> Option.value ~default:"logic" in
      let var_type = json |> member "varType" |> to_string_option |> Option.value ~default:"VAR" in
      let direction = json |> member "direction" |> to_string_option |> Option.value ~default:"NONE" in
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let is_param = json |> member "isParam" |> to_bool_option |> Option.value ~default:false in
      let value = 
	try 
	  let valuep = json |> member "valuep" |> to_list in
	  match valuep with
	  | v :: _ -> 
	      (* Try to extract constant value directly *)
	      (try
		let const_name = v |> member "name" |> to_string in
		Some (Const { name = const_name; dtype_ref = None })
	      with _ ->
		Some (parse' attr name v))
	  | [] -> None
	with _ -> None
      in
      let v = Var' { name; dtype; var_type; direction; value; dtype_name; is_param } in
      if var_type = "IFACEREF" then List.iter (fun nam -> assoc_add attr.var_table nam v) [name;dtype;addr];
      v            
									    
  | "INITITEM" ->
      let value = json |> member "valuep" |> to_list |> List.map (parse' attr name) in
      InitItem { value }
									    
  | "CONST" ->
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Const' { name; dtype }

  | "ENUMITEMREF" ->
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      EnumItemRef' { name; dtype }

  | "TYPEDEF" ->
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      Typedef' { name; dtype }
      
  | "FUNC" ->
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let vars = json |> member "fvarp" |> to_list |> List.map (parse' attr name) in
      Func' { name; dtype; stmts; vars }
      
  | "TASK" ->
      let dtype = json |> member "dtypep" |> to_string_option |> Option.value ~default:"" in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let vars = json |> member "fvarp" |> to_list |> List.map (parse' attr name) in
      Task' { name; dtype; stmts; vars }
      
  | "ALWAYS" ->
      let always = json |> member "keyword" |> to_string_option |> Option.value ~default:"always" in
      let senses = json |> member "sensesp" |> to_list |> List.map (parse' attr name) in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Always { always; senses; stmts }
      
  | "BEGIN" ->
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let is_generate = json |> member "generate" |> to_bool_option |> Option.value ~default:false in
      Begin { name; stmts; is_generate }
      
  | "ASSIGN" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      Assign { lhs; rhs; is_blocking = true }
      
  | "INSIDERANGE" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      InsideRange { lhs; rhs }
      
  | "ASSIGNDLY" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      Assign { lhs; rhs; is_blocking = false }
      
  | "ASSIGNW" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      AssignW { lhs; rhs }
      
  | "IF" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let then_stmt = json |> member "thensp" |> to_list |> List.hd |> parse' attr name in
      let else_stmt = 
        try Some (json |> member "elsesp" |> to_list |> List.hd |> parse' attr name)
        with _ -> None
      in
      If { condition; then_stmt; else_stmt }
      
  | "CASE" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse' attr name in
      let items_json = json |> member "itemsp" |> to_list in
      let items = List.map (fun item ->
        let conditions = item |> member "condsp" |> to_list |> List.map (parse' attr name) in
        let statements = item |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
        { conditions; statements }
      ) items_json in
      Case { expr; items }
      
  | "CASEITEM" ->
      let conditions = json |> member "condsp" |> to_list |> List.map (parse' attr name) in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      CaseItem { conditions; stmts }

  | "WHILE" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      let incs = json |> member "incsp" |> to_list |> List.map (parse' attr name) in
      For' { condition; stmts; incs }

  | "VARREF" ->
      let dtype = json |> member "dtypep" |> to_string in
      let access = json |> member "access" |> to_string_option |> Option.value ~default:"RD" in
      VarRef' { name; access; dtype }
      
  | "VARXREF" ->
      let access = json |> member "access" |> to_string_option |> Option.value ~default:"RD" in
      let dotted = json |> member "dotted" |> to_string_option |> Option.value ~default:"." in
      VarXRef { name; access; dotted }
      
  | "SEL" ->
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let lsb = try Some (json |> member "lsbp" |> to_list |> List.hd |> parse' attr name) with _ -> None in
      let width = try Some (json |> member "widthp" |> to_list |> List.hd |> parse' attr name) with _ -> None in
      let width_const = try Some (json |> member "widthConst" |> to_int) with _ -> None in
      let range = json |> member "declRange" |> to_string_option |> Option.value ~default:"" in
      Sel { expr; lsb; width; width_const; range }
      
  | "ARRAYSEL" ->
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let index = json |> member "bitp" |> to_list |> List.hd |> parse' attr name in
      ArraySel { expr; index }

  | "SELBIT" ->
      (* Single bit selection - treat as Sel with lsb only *)
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let lsb = json |> member "bitp" |> to_list |> List.hd |> parse' attr name in
      Sel { expr; lsb = Some lsb; width = None; width_const = Some 1; range = "" }

  | "SELEXTRACT" ->
      (* Bit range extraction [msb:lsb] *)
      let expr = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let msb = json |> member "leftp" |> to_list |> List.hd |> parse' attr name in
      let lsb_expr = json |> member "rightp" |> to_list |> List.hd |> parse' attr name in
      (* Construct width from msb - lsb *)
      Sel { expr; lsb = Some lsb_expr; width = Some msb; width_const = None; range = "" }

  | "CASTPARSE" ->
      (* Type casting - just return the expression being casted *)
      let expr = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      expr

  | "FUNCREF" ->
      let args = json |> member "pinsp" |> to_list |> 
        List.filter_map (fun pin -> 
          try Some (pin |> member "exprp" |> to_list |> List.hd |> parse' attr name)
          with _ -> None
        ) in
      FuncRef { name; args }
      
  | "TASKREF" ->
      let args = json |> member "pinsp" |> to_list |> 
        List.filter_map (fun pin -> 
          try Some (pin |> member "exprp" |> to_list |> List.hd |> parse' attr name)
          with _ -> None
        ) in
      TaskRef { name; args }

  | "RAND" ->
      let name = json |> member "name" |> to_string in
      let args = json |> member "seedp" |> to_list |> List.map (parse' attr name) in
      FuncRef { name; args }
      
  | "COND" ->
      let condition = json |> member "condp" |> to_list |> List.hd |> parse' attr name in
      let then_val = json |> member "thenp" |> to_list |> List.hd |> parse' attr name in
      let else_val = json |> member "elsep" |> to_list |> List.hd |> parse' attr name in
      Cond { condition; then_val; else_val }
      
  | "CONCAT" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      Concat { parts = [lhs; rhs] }
      
  | "SENTREE" ->
      let senses = json |> member "sensesp" |> to_list |> List.map (parse' attr name) in
      SenTree senses
      
  | "SENITEM" ->
      let edge = json |> member "edgeType" |> to_string_option |> Option.value ~default:"" in
      let signal = json |> member "sensp" |> to_list |> List.hd |> parse' attr name in
      let edge_str = String.lowercase_ascii edge ^ "edge" in
      SenItem { edge_str; signal }
      
  (* Binary operators *)
  | "AND" | "OR" | "XOR"
  | "LOGAND" | "LOGOR"
  | "EQ" | "NEQ" | "LT" | "LTE" | "LTES" | "GT" | "GTE" | "GTES" | "LTS" | "GTS"
  | "EQWILD" | "NEQWILD" | "NEQCASE"
  | "ADD" | "SUB" | "MUL" | "MULS" | "DIV" | "DIVS" | "MOD" | "MODDIV" | "MODDIV" | "MODDIVS"
  | "POW" | "POWSS" | "POWSU" | "POWUS" | "SHIFTL" | "SHIFTR" | "SHIFTRS"
  | "STREAML" | "STREAMR" ->
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
      let dtype = json |> member "dtypep" |> to_string in
      BinaryOp' { op = node_type; lhs; rhs; dtype }
      
  (* Unary operators *)
  | "NOT" | "REDAND" | "REDOR" | "REDXOR" | "EXTEND" | "LOGNOT" | "ONEHOT" | "ONEHOT0" | "NEGATE"
  | "EXTENDS" | "ISUNKNOWN" | "CLOG2" ->
      let operand = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let dtype = json |> member "dtypep" |> to_string in
      UnaryOp' { op = node_type; operand; dtype }
  | "EVENTCONTROL" ->
      let sense = json |> member "sensesp" |> to_list |> List.map (parse' attr name) in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      EventCtrl {sense; stmts}
  | "INITARRAY" ->
      let inits = json |> member "initsp" |> to_list |> List.map (parse' attr name) in
      InitArray {inits}

  | "INITIAL" ->
      let suspend = json |> member "isSuspendable" |> to_bool in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Initial {suspend; stmts}

  | "INITIALSTATIC" ->
      let suspend = json |> member "isSuspendable" |> to_bool in
      let process = json |> member "needProcess" |> to_bool in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      InitialStatic {suspend; process; stmts}

  | "FINAL" ->
      let suspend = json |> member "isSuspendable" |> to_bool in
      let process = json |> member "needProcess" |> to_bool in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Final {suspend; process; stmts}

  | "FINISH" ->
      Finish

  | "DELAY" ->
      let cycle = json |> member "isCycleDelay" |> to_bool in
      let lhs = json |> member "lhsp" |> to_list |> List.hd |> parse' attr name in
      let stmts = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
      Delay {cycle; lhs; stmts}

  | "ITORD" ->
      let dtype = json |> member "dtypep" |> to_string in
      let lhs = json |> member "lhsp" |> to_list |> List.map (parse' attr name) in
      Itord' {dtype; lhs}

  | "CVTPACKSTRING" ->
      let dtype = json |> member "dtypep" |> to_string in
      let lhs = json |> member "lhsp" |> to_list |> List.map (parse' attr name) in
      CvtPackString' {dtype; lhs}

  | "FOPEN" ->
      let dtype = json |> member "dtypep" |> to_string in
      let filename = json |> member "filenamep" |> to_list |> List.map (parse' attr name) in
      let mode = json |> member "modep" |> to_list |> List.map (parse' attr name) in
      Fopen' {dtype; filename; mode}

  | "FCLOSE" ->
      let file = json |> member "filep" |> to_list |> List.map (parse' attr name) in
      Fclose {file}

  | "DISPLAY" ->
      let fmt = json |> member "fmtp" |> to_list |> List.map (parse' attr name) in
      let file = json |> member "filep" |> to_list |> List.map (parse' attr name) in
      Display {fmt; file}

  | "SFORMAT" ->
      let fmt = json |> member "fmtp" |> to_list |> List.map (parse' attr name) in
      let lhs = json |> member "lhsp" |> to_list |> List.map (parse' attr name) in
      Sformat {fmt; lhs}

  | "SFORMATF" ->
      let expr = json |> member "exprsp" |> to_list |> List.map (parse' attr name) in
      let scope = json |> member "scopeNamep" |> to_list |> List.map (parse' attr name) in
      Sformatp {expr; scope}

  | "SCOPENAME" ->
      let dtype = json |> member "dtypep" |> to_string in
      ScopeName {dtype}

  | "TIME" ->
      let dtype = json |> member "dtypep" |> to_string in
      Time {dtype}

  | "TEXT" ->
      let text = json |> member "shortText" |> to_string in
      Text {text}

  | "SAMPLED" ->
      let dtype = json |> member "dtypep" |> to_string in
      let expr = json |> member "exprp" |> to_list |> List.map (parse' attr name) in
      Sampled {dtype; expr}

  | "CEXPR" ->
      let dtype = json |> member "dtypep" |> to_string in
      let expr = json |> member "exprsp" |> to_list |> List.map (parse' attr name) in
      Cexpr {dtype; expr}

  | "STMTEXPR" ->
      let expr = json |> member "exprp" |> to_list |> List.hd |> parse' attr name in
     StmtExpr {expr}

  | "CONSPACKUORSTRUCT" ->
      let dtype = json |> member "dtypep" |> to_string in
      let members = json |> member "membersp" |> to_list |> List.map (parse' attr name) in
     ConsPack' {dtype; members}

  | "CONSPACKMEMBER" ->
      let dtype = json |> member "dtypep" |> to_string in
      let rhs = json |> member "rhsp" |> to_list |> List.hd |> parse' attr name in
     ConsPackMember' {dtype; rhs}

  | "VALUEPLUSARGS" ->
      let dtype = json |> member "dtypep" |> to_string in
      let search = json |> member "searchp" |> to_list |> List.map (parse' attr name) in
      let out = json |> member "outp" |> to_list |> List.map (parse' attr name) in
     ValuePlusArgs' {dtype; search; out}

  | "TESTPLUSARGS" ->
      let dtype = json |> member "dtypep" |> to_string in
      let search = json |> member "searchp" |> to_list |> List.map (parse' attr name) in
     TestPlusArgs' {dtype; search}

  | "REPLICATE" ->
      let dtype = json |> member "dtypep" |> to_string in
      let src = json |> member "srcp" |> to_list |> List.hd |> parse' attr name in
      let count = json |> member "countp" |> to_list |> List.hd |> parse' attr name in
     Replicate' {dtype; src; count}

  | "JUMPBLOCK" ->
      let stmt = json |> member "stmtsp" |> to_list |> List.map (parse' attr name) in
     JumpBlock { stmt }

  | "JUMPGO" ->
      let label = json |> member "labelp" |> to_string in
      JumpGo' { label }

  | "STOP" ->
      let fatal = json |> member "isFatal" |> to_bool in
      Stop { fatal }

  | "CMETHODHARD" ->
      let dtype = json |> member "dtypep" |> to_string in
      let from = json |> member "fromp" |> to_list |> List.hd |> parse' attr name in
      let pins = json |> member "pinsp" |> to_list |> List.map (parse' attr name) in
      CMethodHard' { dtype; from; pins }
 
  | oth -> Unknown (node_type, json)

  and parse' attr name json =
     depth := json :: !depth;
     let rslt = parse_json ({attr with parent=name::attr.parent}) json in
     depth := List.tl !depth;
     rslt

let othrw = ref None
let othrwtyp = ref None

let rec rw attr = function
| Netlist lst -> Netlist (rwlst attr lst)
| Module {name; stmts} -> Module {name; stmts=rwlst attr stmts}
| Var' {name; dtype; var_type; direction; value; dtype_name; is_param} ->
  let dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype) in
  Var {name; dtype_ref; var_type; direction; value; dtype_name; is_param}
| Cell' {name; modp_addr; pins } ->
  Cell {name; modp_addr=rwopt attr (assoc_find_opt attr.module_table modp_addr); pins=rwlst attr pins}
| Pin {name; expr} -> Pin {name; expr=rwopt attr expr}
| InsideRange {lhs; rhs} -> InsideRange {lhs=rw attr lhs; rhs=rw attr rhs}
| Assign {lhs; rhs; is_blocking} -> Assign {lhs=rw attr lhs; rhs=rw attr rhs; is_blocking}
| AssignW {lhs; rhs} -> AssignW {lhs=rw attr lhs; rhs=rw attr rhs}
| VarXRef {name; access; dotted} as v -> v
| Always {always; senses; stmts} -> Always {always; senses=rwlst attr senses; stmts=rwlst attr stmts};
| SenTree lst -> SenTree (rwlst attr lst)
| SenItem {edge_str; signal} -> SenItem {edge_str; signal=rw attr signal}
| BinaryOp' {op; lhs; rhs; dtype} ->
    BinaryOp {op;
    lhs=rw attr lhs;
    rhs=rw attr rhs;
    dtype_ref=rwtyp' attr (assoc_find_opt attr.type_table dtype) }
| Interface {name; params; stmts} ->
  Interface {name; params=rwlst attr params; stmts=rwlst attr stmts}
| Modport { name; vars } -> Modport { name; vars=rwlst attr vars }
| ModportVarRef { name; direction; var_ref } -> ModportVarRef { name; direction; var_ref }
| Const' { name; dtype } ->
  Const { name; dtype_ref=rwtyp' attr (assoc_find_opt attr.type_table dtype) }
| EnumItemRef' { name; dtype } ->
  EnumItemRef { name; dtype_ref=rwtyp' attr (assoc_find_opt attr.type_table dtype) }
| Begin { name; stmts; is_generate } -> Begin { name; stmts=rwlst attr stmts; is_generate }
| Sel { expr; lsb; width; width_const; range } -> Sel { expr=rw attr expr; lsb=rwopt attr lsb; width=rwopt attr width; width_const; range }
| Case {expr; items} -> Case {expr = rw attr expr; items = List.map (rwitm attr) items}
| EventCtrl { sense; stmts} -> EventCtrl {sense = rwlst attr sense; stmts = rwlst attr stmts}
| InitArray { inits} -> InitArray {inits = rwlst attr inits}
| Initial { suspend; stmts} -> Initial {suspend; stmts = rwlst attr stmts}
| InitialStatic { suspend; process; stmts} -> InitialStatic {suspend; process; stmts = rwlst attr stmts}
| Final { suspend; process; stmts} -> Final {suspend; process; stmts = rwlst attr stmts}
| Finish -> Finish
| Delay { cycle; lhs; stmts} -> Delay {cycle; lhs = rw attr lhs; stmts = rwlst attr stmts}
| For' { condition; stmts; incs} -> For' {condition = rw attr condition;
stmts = rwlst attr stmts;
incs = rwlst attr incs;}
| UnaryOp' {op; operand; dtype } -> UnaryOp { op; operand = rw attr operand;
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype)}
| Cond {condition; then_val; else_val } -> Cond {condition = rw attr condition;
    then_val = rw attr then_val;
    else_val = rw attr else_val} 
| If {condition; then_stmt; else_stmt } -> If {condition = rw attr condition;
    then_stmt = rw attr then_stmt;
    else_stmt = match else_stmt with Some stmt -> Some (rw attr stmt) | None -> None} 
| ArraySel { expr; index } -> ArraySel { expr = rw attr expr; index = rw attr index }
| FuncRef { name; args } -> FuncRef { name; args = rwlst attr args }
| TaskRef { name; args } -> TaskRef { name; args = rwlst attr args }
| Concat { parts } -> Concat { parts = rwlst attr parts }
| Display { fmt; file } -> Display { fmt=rwlst attr fmt; file = rwlst attr file }
| Sformat { fmt; lhs } -> Sformat { fmt=rwlst attr fmt; lhs = rwlst attr lhs }
| Sformatp { expr; scope } -> Sformatp { expr=rwlst attr expr; scope = rwlst attr scope }
| ScopeName { dtype } -> ScopeName { dtype }
| Time { dtype } -> Time { dtype }
| Text { text } -> Text { text }
| Sampled { dtype; expr } -> Sampled { dtype; expr = rwlst attr expr }
| Cexpr { dtype; expr } -> Cexpr { dtype; expr = rwlst attr expr }
| StmtExpr { expr } -> StmtExpr { expr = rw attr expr }
| Package { name; stmts } -> Package { name; stmts = rwlst attr stmts }
| Typedef' { name; dtype } -> Typedef { name;
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype) }
| Typedef { name; dtype_ref } -> Typedef { name; dtype_ref } 
| VarRef' { name; access; dtype } -> VarRef {
    name; access; dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype) }
| Itord' { dtype; lhs } -> Itord {
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
    lhs = rwlst attr lhs }
| Itord { dtype_ref; lhs } -> Itord { dtype_ref; lhs } 
| CvtPackString' { dtype; lhs } -> CvtPackString {
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
    lhs = rwlst attr lhs }
| CvtPackString { dtype_ref; lhs } -> CvtPackString { dtype_ref; lhs } 
| Fopen' { dtype; filename; mode } -> Fopen {
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
    filename = rwlst attr filename;
    mode = rwlst attr mode }
| Fopen { dtype_ref; filename; mode } -> Fopen { dtype_ref; filename; mode } 
| Fclose { file } -> Fclose {
    file = rwlst attr file }
| ValuePlusArgs' { dtype; search; out } -> ValuePlusArgs {
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
    search = rwlst attr search;
    out = rwlst attr out;
 }
| ValuePlusArgs _ as v -> v
| TestPlusArgs' { dtype; search } -> TestPlusArgs {
    dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
    search = rwlst attr search;
 }
| TestPlusArgs _ as v -> v
| Func' { name; dtype; stmts; vars } -> Func {
      name;
      dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
      stmts = rwlst attr stmts ;
      vars = rwlst attr vars }
| Func {
      name;
      dtype_ref;
      stmts;
      vars } -> Func {
      name;
      dtype_ref = rwtyp' attr dtype_ref;
      stmts = rwlst attr stmts;
      vars = rwlst attr vars }
| JumpBlock { stmt } -> JumpBlock {
      stmt = rwlst attr stmt }
| JumpGo' { label } -> JumpGo {
      label=rwtyp' attr (assoc_find_opt attr.type_table label) }
| JumpGo { label } -> JumpGo { label }
| Replicate' { dtype; src; count } -> Replicate {
      dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
      src = rw attr src ;
      count = rw attr count }
| Task' { name; dtype; stmts; vars } -> Task {
      name;
      dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype);
      stmts = rwlst attr stmts ;
      vars = rwlst attr vars }
| Task {
      name;
      dtype_ref;
      stmts;
      vars } -> Task {
      name;
      dtype_ref = rwtyp' attr dtype_ref;
      stmts = rwlst attr stmts;
      vars = rwlst attr vars }
| ConsPack' { dtype; members } -> ConsPack {
      dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype );
      members = rwlst attr members }
| ConsPack { dtype_ref; members } -> ConsPack { dtype_ref; members = rwlst attr members }
| ConsPackMember' { dtype; rhs } -> ConsPackMember {
      dtype_ref = rwtyp' attr (assoc_find_opt attr.type_table dtype );
      rhs = rw attr rhs }
| ConsPackMember { dtype_ref; rhs } -> ConsPackMember { dtype_ref; rhs = rw attr rhs }
| CaseItem { conditions; stmts } -> CaseItem {
      conditions = rwlst attr conditions;
      stmts = rwlst attr stmts }
| InitItem { value } -> InitItem {
      value = rwlst attr value }
| CMethodHard' { dtype; from; pins } -> CMethodHard {
      dtype_ref=rwtyp' attr (assoc_find_opt attr.type_table dtype);
      from=rw attr from;
      pins = rwlst attr pins }
| Const _
| EnumItemRef _
| Cell _
| Stop _
| Replicate _
| CMethodHard _
| VarRef _
| UnaryOp _
| BinaryOp _
| For _
| Var _ as skip -> skip
| Unknown (tag, json) as oth ->
    othrw := Some oth;
    Printf.eprintf "\n=== UNKNOWN AST NODE ===\n";
    Printf.eprintf "Tag: %s\n" tag;
    Printf.eprintf "JSON: %s\n" (Yojson.Safe.pretty_to_string json);
    Printf.eprintf "=====================\n%!";
    failwith "othrw"

and rwitm attr = function
| { conditions; statements } -> {
    conditions= rwlst attr conditions;
    statements = rwlst attr statements }

and rwopt attr = function
| Some x -> Some (rw attr x)
| None -> None

and rwtyp' attr = function
| Some x -> Some (rwtyp attr x)
| None -> None

and rwtyp attr = function
| ArrayType' { base=base_ref; range } ->
  let base_type = rwtyp attr (try assoc_find attr.type_table base_ref with Not_found -> UnknownType (base_ref, `Null)) in
  ArrayType { base = base_type; range }
| PackArrayType' { base=base_ref; range } ->
  let base_type = rwtyp attr (try assoc_find attr.type_table base_ref with Not_found -> UnknownType (base_ref, `Null)) in
  PackArrayType { base = base_type; range }
| IntfRefType' { ifacename; modportname; ifacep; modportp } ->
  IntfRefType { ifacename;
  modportname;
  ifacep=assoc_find_opt attr.interface_table ifacep;
  modportp=assoc_find_opt attr.interface_table modportp }
| BasicType _ as t -> t
| RefType' {name; dtype; refdtype } -> RefType { name;
    dtype_ref = (try Some (rwtyp attr (assoc_find attr.type_table dtype)) with Not_found -> None);
    refdtype_ref = (try Some (rwtyp attr (assoc_find attr.type_table refdtype)) with Not_found -> None) }
| EnumType {name; items} as t -> t
| StructType { name; packed; members } -> StructType { name; packed; members = List.map (rwtyp attr) members }
| UnionType { name; packed; members } -> UnionType { name; packed; members = List.map (rwtyp attr) members }
| MemberType' { name; dtype; child; value } ->
    MemberType { name;
    dtype_ref = (try Some (rwtyp attr (assoc_find attr.type_table dtype)) with Not_found -> None);
    child = List.map (rwtyp attr) child; value = List.map (rwtyp attr) value }
| MemberType { name; dtype_ref; child; value } ->
    MemberType { name; dtype_ref; child = List.map (rwtyp attr) child; value = List.map (rwtyp attr) value }
| ConstType' { name; dtype; child } ->
    ConstType { name;
    dtype_ref = assoc_find_opt attr.type_table dtype;
    child = List.map (rwtyp attr) child }
| ConstType { name; dtype_ref; child } ->
    ConstType { name; dtype_ref; child = List.map (rwtyp attr) child }
| ParamTypeType' { name; dtype } ->
    ParamTypeType { name; dtype_ref = assoc_find_opt attr.type_table dtype }
| ParamTypeType { name; dtype_ref } ->
    ParamTypeType { name; dtype_ref }
| VoidType { name; resolved } -> VoidType { name; resolved }
| oth -> othrwtyp := Some oth; failwith "othrwtyp"

and rwlst attr = function
| [] -> []
| Var {name; dtype_ref} :: Assign {lhs = VarRef {name=lname; access = "WR"} as lhs; rhs } :: For' {condition; stmts; incs } :: tl when name=lname ->
  For { name; dtype_ref; lhs; rhs; condition; stmts; incs } :: rwlst attr tl
| oth :: tl -> rw attr oth :: rwlst attr tl

let dbgpass1 = ref (Unknown ("",`Null))
let dbgpass2 = ref (Unknown ("",`Null))
let mods = ref []
let intf = ref []
let typ = ref []
let vars = ref []

let parse json =
  let attr = 
  {
  parent=[];
  type_table = assoc_create 100;
  interface_table = assoc_create 20;
  module_table = assoc_create 20;
  var_table = assoc_create 20;
  } in
  depth := json :: [];
  let ast' = parse_json attr json in
  dbgpass1 := ast';
  let ast = rw attr (rw attr ast') in
  dbgpass2 := ast;
  mods := !(attr.module_table);
  intf := !(attr.interface_table);
  typ  := !(attr.type_table);
  vars := !(attr.var_table);
  ast
