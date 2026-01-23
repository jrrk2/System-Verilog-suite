open VhdlTree
let mymap fn (x:vhdintf list) = List.map fn x
let maxidx idx = int_of_float(ceil(log(float_of_int idx)/.log(2.))) - 1

let debug = ref false

let rec abstraction = function
  | Octuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8) -> Octuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5,
     abstraction arg6,
     abstraction arg7,
     abstraction arg8)
  | Septuple(arg1,arg2,arg3,arg4,arg5,arg6,arg7) -> Septuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5,
     abstraction arg6,
     abstraction arg7)
  | Sextuple(arg1,arg2,arg3,arg4,arg5,arg6) -> Sextuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5,
     abstraction arg6)
  | Quintuple(arg1,arg2,arg3,arg4,arg5) -> Quintuple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4,
     abstraction arg5)
  | Quadruple(arg1,arg2,arg3,arg4) -> Quadruple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3,
     abstraction arg4)
  | Double (VhdSimpleName, Str vector) -> Str vector
  | Double (VhdBlockTypeDeclaration,
      Double (VhdFullType,
       Triple (Vhdfull_type_declaration, typ,
        Double (VhdEnumerationTypeDefinition,
         List enumlst)))) ->
     Triple (VhdEnumerationTypeDefinition,
	     abstraction typ,
	     List (List.map abstraction enumlst))
  | Triple (Vhdcase_statement_alternative,
            Double (VhdChoiceSimpleExpression,
             Double (VhdOperatorString, str)),
            Double (VhdSequentialReturn,
             Triple (Vhdreturn_statement, Str "",
              Triple (VhdNameParametersPrimary, nam,
               Triple (Vhdassociation_element, VhdFormalIndexed,
                Double (VhdActualDiscreteRange,
			Triple (VhdRange, hi, lo))))))) ->
     Quintuple (Vhdcase_statement_alternative,
	     abstraction str,
	     abstraction nam,
	     abstraction hi,
	     abstraction lo)
  | Triple(arg1,arg2,VhdNone) -> Double(abstraction arg1,abstraction arg2)
  | Triple(arg1,arg2,arg3) -> Triple(
     abstraction arg1,
     abstraction arg2,
     abstraction arg3)
(* *)
  | List [Triple (Vhdwaveform_element, Str s, VhdNone)] -> Str s
  | List (hd :: []) -> abstraction hd
  | List lst -> List (mymap abstraction lst)
  | Double (VhdTargetDotted, Str s) -> Str s
  | Double (VhdNamePrimary, Double (VhdSimpleName, Str "")) -> VhdNone
  | Double (VhdNamePrimary, Double (VhdSimpleName, Str s)) -> Str s
  | Double (VhdNamePrimary, s) -> abstraction s
(* *)
  | Double(arg1,arg2) -> Double(
     abstraction arg1,
     abstraction arg2)
  | VhdNone ->  VhdNone
  | Str s -> Str s
  | Num s -> Num s
  | Real s -> Real s
  | Char s -> Char s
  | others -> others

let unmatched = ref []
let blocklst = ref []
  
let mkblk = function
  | List lst -> List lst
  | oth -> List [oth]

let digit = function
  | '0'..'9' -> true
  | _ -> false

type match2_args = {
  mutable modref: string;
  bufhash: (string,Buffer.t)Hashtbl.t;
  mutable buf: Buffer.t;
  indent: string;
  liblst: (((vhdintf*string)*vhdintf) list) ref;
  subtype: ((string*(string*vhdintf)) list) ref;
  localp: ((string*vhdintf) list) ref;
  matched: vhdintf list ref;
  fns: ((string*(vhdintf*string)) list) ref;
  siglst: (string*vhdintf) list ref;
  components: (string*vhdintf list) list ref;
}

type const =
  | Unmatched
  | Symbolic of string
  | Int of int
  | Up of (const*const)
  | Down of (const*const)
  | Plus of (const*const)
  | Minus of (const*const)

let dummyrng hi lo = (Double(VhdRange,
		       (Triple (VhdDecreasingRange,
				Double (VhdIntPrimary, Num (string_of_int hi)),
				Double (VhdIntPrimary, Num (string_of_int lo))))))

let lookuprng args nam =
  if List.mem_assoc nam !(args.siglst) then
    List.assoc nam !(args.siglst)
  else
    dummyrng 0 0

let edgsense nam = function
| '0' -> "negedge "^nam
| '1' -> "posedge "^nam
| _ -> failwith "edge"

let rec const_expr args = function
  | Str nam ->
     if List.mem_assoc nam !(args.localp) then
       begin
	 let param = List.assoc nam !(args.localp) in
	 const_expr args param
       end
     else
       begin
	 Symbolic nam
       end
  | Double (VhdIntPrimary, Num num) -> Int (int_of_string num)
  | Double (VhdRange, expr) -> const_expr args expr
  | Triple (VhdDecreasingRange, lft, rght) ->
     Down (const_expr args lft, const_expr args rght)
  | Double (VhdParenthesedPrimary, expr) -> const_expr args expr
  | Triple (VhdAddSimpleExpression, lft, rght) ->
     (match (const_expr args lft, const_expr args rght) with
      | Int lft, Int rght -> Int(lft + rght)
      | oth -> Plus oth)
  | Triple (VhdSubSimpleExpression, lft, rght) ->
     (match (const_expr args lft, const_expr args rght) with
      | Int lft, Int rght -> Int(lft - rght)
      | oth -> Minus oth)
  | Double (VhdActualDiscreteRange, expr) -> const_expr args expr
  | oth -> blocklst := ("",oth,[]) :: !blocklst; Unmatched

let noportlst = ref []

let rec match2' (args:match2_args) = function
  | List lst12 ->
    Buffer.add_string args.buf (args.indent^"begin\n");
    List.iter (Buffer.add_string args.buf args.indent; match2 {args with indent=args.indent^"  "}) lst12;
    Buffer.add_string args.buf (args.indent^"end\n"^args.indent);
  | Str str -> Buffer.add_string args.buf str
  | Triple (VhdEqualRelation, lft, rght) ->
     (match2 args) lft; Buffer.add_string args.buf " == "; (match2 args) rght
  | Triple (VhdNotEqualRelation, lft, rght) ->
     (match2 args) lft; Buffer.add_string args.buf " != "; (match2 args) rght
  | Triple (VhdLessRelation, lft, rght) ->
     (match2 args) lft; Buffer.add_string args.buf " < "; (match2 args) rght
  | Triple (VhdGreaterRelation, lft, rght) ->
     (match2 args) lft; Buffer.add_string args.buf " > "; (match2 args) rght
  | Triple (VhdGreaterOrEqualRelation, lft, rght) ->
     (match2 args) lft; Buffer.add_string args.buf " >= "; (match2 args) rght
  | Triple (VhdAddSimpleExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " + "; (match2 args) rght
  | Triple (VhdSubSimpleExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " - "; (match2 args) rght
  | Triple (VhdOrLogicalExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " | "; (match2 args) rght
  | Triple (VhdXorLogicalExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " ^ "; (match2 args) rght
  | Triple (VhdShiftLeftLogicalExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " << "; (match2 args) rght
  | Triple (VhdShiftRightLogicalExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " >> "; (match2 args) rght
  | Double (VhdParenthesedPrimary, x) ->
     Buffer.add_string args.buf "(";
     match2 args x;
     Buffer.add_string args.buf ")"
  | Triple (VhdAndLogicalExpression, lft, rght) ->
      (match2 args) lft; Buffer.add_string args.buf " && "; (match2 args) rght
  | Triple (VhdExpFactor, Double (VhdIntPrimary, Num "2"), expr) ->
      Buffer.add_string args.buf "1 << "; (match2 args) expr
  | Double (VhdCondition, VhdNone) -> Buffer.add_string args.buf ("/* cond none, 197 */");
  | Double (VhdCondition,
            Triple (VhdNameParametersPrimary, Str rising_edge,
		    Triple (Vhdassociation_element, VhdFormalIndexed,
			    Double (VhdActualExpression, Str clk)))) ->
     Buffer.add_string args.buf ("@(posedge "^clk)
  | Triple (VhdNameParametersPrimary,
            Str src,
            Triple (Vhdassociation_element,
                    VhdFormalIndexed,
                    Double (VhdActualExpression, idx))) ->
	    Buffer.add_string args.buf (src^"[");
	    match2 args idx;
	    Buffer.add_string args.buf ("]");
  | Triple (VhdNameParametersPrimary, Str src,
	    Triple (Vhdassociation_element, VhdFormalIndexed,
		    Double (VhdActualDiscreteRange, range))) ->
	    Buffer.add_string args.buf (src);
	    match2 args range
  | Double (VhdCondition, x) -> Buffer.add_string args.buf "("; (match2 args) x; Buffer.add_string args.buf ")"
  | Double (VhdOperatorString, Str v) when digit v.[0] ->
     Buffer.add_string args.buf (string_of_int (String.length v)^"'b"^v);
  | Double (VhdCharPrimary, Char ch) -> Buffer.add_string args.buf (" 1'b"^String.make 1 ch)
  | Triple (VhdNameParametersPrimary,
	    Double (VhdAttributeName,
		    Triple (Vhdattribute_name,
			    Double (VhdSuffixSimpleName, Str typ), Str "succ")),
	    Triple (Vhdassociation_element, VhdFormalIndexed,
					    Double (VhdActualExpression, Str state))) ->
					    Buffer.add_string args.buf (state^"+1");
  | Triple (VhdNameParametersPrimary, fn, List params) ->
     (match2 args) fn;
     let delim = ref "(" in List.iter (fun itm -> Buffer.add_string args.buf !delim; match2 args itm; delim := ", ") params;
     Buffer.add_string args.buf ")";
  | Triple (Vhdassociation_element,
                    VhdFormalIndexed,
                    Double (VhdActualExpression,
			    Str src)) -> Buffer.add_string args.buf src

  | Double (VhdIntPrimary, Num n) -> Buffer.add_string args.buf (n);
    
  | Triple (Vhdassociation_element,
          VhdFormalIndexed,
          Double (VhdActualExpression,
		  Double (VhdIntPrimary, Num n))) -> Buffer.add_string args.buf n
  | Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str result,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualExpression, Str i)))) -> Buffer.add_string args.buf (result^"."^i);

  | Double (VhdSequentialVariableAssignment,
                                   Double (VhdSimpleVariableAssignment,
                                    Quadruple (Vhdsimple_variable_assignment,
					       Str "", dst, expr))) ->
   (match2 args) dst;
   Buffer.add_string args.buf (" = ");
   (match2 args) expr;
   Buffer.add_string args.buf (";\n"^args.indent)

  | Double (VhdSequentialSignalAssignment,
          Double (VhdSimpleSignalAssignment,
                  Quintuple
                    (Vhdsimple_signal_assignment_statement,
                     Str "", Str nam, VhdDelayNone,
                     Double (Vhdwaveform_element,
			     Double (VhdAggregatePrimary, (Triple _ as oth)))))) ->
   Buffer.add_string args.buf ("/* block const 263 */\n");
     block_const args nam (lookuprng args nam) [oth]
  | Double (VhdSequentialSignalAssignment,
 Double (VhdSimpleSignalAssignment,
  Quintuple (Vhdsimple_signal_assignment_statement, Str "",
   Double (VhdTargetDotted, dst), VhdDelayNone, Double (Vhdwaveform_element, rhs)))) ->
    (match2 args) dst;
   Buffer.add_string args.buf (" <= ");
    (match2 args) rhs;
   Buffer.add_string args.buf (";\n");

  | Double (VhdSequentialIf,
                 Quintuple (Vhdif_statement, Str "",
                  cond,
                   thenstmts,
			    VhdElseNone)) ->
     Buffer.add_string args.buf ("if ");
    (match2 args) cond;
    Buffer.add_string args.buf ("\n"^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts);
    Buffer.add_string args.buf "\n";
  | Double (VhdSequentialNull, Double (Vhdnull_statement, Str "")) -> Buffer.add_string args.buf "begin end"
  | Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "",
                     cond,
                     thenstmts,
                     Double (VhdElse,
                             elsestmts))) ->
  Buffer.add_string args.buf ("if ");
  (match2 args) cond;
    Buffer.add_string args.buf ("\n"^args.indent);
  (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts);
  Buffer.add_string args.buf " else\n";
  (match2  {args with indent=args.indent^"  "}) (mkblk elsestmts);

| Double (VhdSequentialIf,
          Quintuple (Vhdif_statement, Str "",
                     cond,
                     thenstmts,
                     Double (VhdElsif,
                             Quintuple (Vhdif_statement, Str "",
					cond',
					thenstmts',
					VhdElseNone)))) ->		
  Buffer.add_string args.buf ("if ");
  (match2 args) cond;
    Buffer.add_string args.buf ("\n"^args.indent);
  (match2 {args with indent=args.indent^"  "}) (mkblk thenstmts);
  Buffer.add_string args.buf (args.indent^"else if "^args.indent);
  (match2 args) cond';
  Buffer.add_string args.buf ("\n"^args.indent);
  (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts');
| Double (VhdSequentialCase,
    Quintuple (Vhdcase_statement, Str "",
     Double (VhdSelector, Str sel), VhdOrdinarySelection,
     List
      [Triple (Vhdcase_statement_alternative,
        Double (VhdChoiceDiscreteRange,
         Double (VhdRange,
          Triple (VhdIncreasingRange, Double (VhdIntPrimary, Num lo),
           Double (VhdIntPrimary, Num hi)))),
        matching_stmt);
       Triple (Vhdcase_statement_alternative, VhdChoiceOthers,
        non_matching_stmt)])) ->
  Buffer.add_string args.buf ("if ("^sel^" >= "^lo^" && "^sel^" <= "^hi^")");
    Buffer.add_string args.buf ("\n  "^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk matching_stmt);
    Buffer.add_string args.buf (args.indent^"else\n"^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk non_matching_stmt);

| Double (VhdSequentialCase,
          Quintuple (Vhdcase_statement, Str "",
                     Double (VhdSelector, sel), VhdOrdinarySelection,
                     List cases)) ->
  Buffer.add_string args.buf ("case (");
  (match2 args) sel;
  Buffer.add_string args.buf ")";
  List.iter (match2 {args with indent=args.indent^"  "}) cases;
  Buffer.add_string args.buf ("\n"^args.indent^"endcase\n");

| Double (VhdChoiceSimpleExpression,
                               case') ->
   (match2 args) case';


| Triple (Vhdcase_statement_alternative, cse, stmts) ->
  Buffer.add_string args.buf ("\n"^args.indent);
   (match cse with
   | VhdChoiceOthers -> Buffer.add_string args.buf "default"
   | List lst -> let delim = ref "" in List.iter (fun itm -> Buffer.add_string args.buf !delim; match2 args itm; delim := ", ") lst
   | oth -> (match2 args) cse);
  Buffer.add_string args.buf (":\n"^args.indent);
  match2 args (match stmts with List _ -> stmts | _ -> List [stmts])
     
  |              Double (VhdSequentialIf,
               Quintuple (Vhdif_statement, Str "",
			  cond,
			  then',else')) ->
     Buffer.add_string args.buf ("if ");
    (match2 args) cond;
    Buffer.add_string args.buf ("\n  "^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk then');
    Buffer.add_string args.buf (args.indent^"else\n"^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk else');

  | Double (VhdElse,
                 List
                   lst51) ->
                   List.iter (match2 args) lst51;
  | Double (VhdElse,
 Double (VhdSequentialSignalAssignment,
  Double (VhdSimpleSignalAssignment,
   Quintuple (Vhdsimple_signal_assignment_statement, Str "", Str dst,
    VhdDelayNone,
    Double (Vhdwaveform_element, Double (VhdOperatorString, Str rhs)))))) -> 
    Buffer.add_string args.buf "/* else begin end */"
| VhdElseNone -> Buffer.add_string args.buf "/* else begin end */"
| Double (VhdProcessVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            VhdNoConstraint),
		     VhdNone)) -> (match kind with
		     | "boolean" -> Buffer.add_string args.buf ("reg "^nam^"; // 352\n")
		     | oth ->  Buffer.add_string args.buf ("reg /*"^oth^" */ "^nam^"; // 353\n"))

| Double (VhdProcessVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            Double (VhdArrayConstraint,
             Triple (Vhdassociation_element, VhdFormalIndexed,
              range))),
		     VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> Buffer.add_string args.buf ("reg "); match2 args range;  Buffer.add_string args.buf (nam^"; // 363\n")
		     | oth ->  Buffer.add_string args.buf ("reg /*"^oth^" */ "^nam^"; // 364\n"))
	   
| Triple (Vhdconditional_waveform,
            Double (Vhdwaveform_element, ch), Double (VhdCondition, VhdNone)) -> (match2 args) ch;

| Triple (Vhdconditional_waveform,
          Double (Vhdwaveform_element, level),
          Double (VhdCondition, cond)) -> 
                match2 args cond;
                Buffer.add_string args.buf (" ? ");
                match2 args level; 
                Buffer.add_string args.buf (" : ");
(* *)
|   Double (VhdConcurrentSignalAssignmentStatement,
    Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
     Str "false",
     Double (VhdConcurrentSimpleSignalAssignment,
      Quintuple (Vhdconcurrent_simple_signal_assignment, Str nam,
       Str "false", VhdDelayNone,
       Double (Vhdwaveform_element,
        Double (VhdAggregatePrimary,
         (Triple (Vhdelement_association, VhdChoiceOthers, _) as oth))))))) ->
   Buffer.add_string args.buf ("assign ");
     block_const args nam (lookuprng args nam) [oth]
| Double (VhdConcurrentSignalAssignmentStatement,
      Quadruple (Vhdconcurrent_signal_assignment_statement, Str "",
       Str _false,
       Double (VhdConcurrentSimpleSignalAssignment,
        Quintuple (Vhdconcurrent_simple_signal_assignment,
         Str dst, Str _false', VhdDelayNone,
         Double (Vhdwaveform_element, expr))))) -> Buffer.add_string args.buf ("assign /*432*/ "^dst^" = ");
  (match2 args) expr;
    Buffer.add_string args.buf ("; // 434\n"^args.indent);
| Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str cond,
  Double (VhdConcurrentConditionalSignalAssignment,
   Quintuple (Vhdconcurrent_conditional_signal_assignment,
    Str dst, Str cond', VhdDelayNone,
    List lst)))) ->
  let delim = ref ("assign /*903*/ "^dst^" = ") in
  List.iter (fun itm -> Buffer.add_string args.buf !delim; match2 args itm; delim := " ") lst;
  Buffer.add_string args.buf ("; // 905\n"^args.indent);
| Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str "false",
  Double (VhdConcurrentSimpleSignalAssignment,
   Quintuple (Vhdconcurrent_simple_signal_assignment,
    Double (VhdTargetDotted,
     Triple (VhdNameParametersPrimary, Str dst,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualDiscreteRange,
        range)))),
    Str "false", VhdDelayNone,
    Double (Vhdwaveform_element,
     Double (VhdAggregatePrimary,
      Triple (Vhdelement_association, VhdChoiceOthers,
                                      Double (VhdCharPrimary, Char '0')))))))) ->
Buffer.add_string args.buf ("assign /*459*/ "^dst^" ");
match2 args range;
Buffer.add_string args.buf (" = 0;\n");
| Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str "false",
  Double (VhdConcurrentSimpleSignalAssignment,
   Quintuple (Vhdconcurrent_simple_signal_assignment,
    Double (VhdTargetDotted,
     Triple (VhdNameParametersPrimary, Str dst,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualExpression, Double (VhdIntPrimary, Num n))))),
    Str "false", VhdDelayNone, Double (Vhdwaveform_element, Str src))))) ->
Buffer.add_string args.buf ("assign "^dst^"["^n^"] = "^src^";\n");
| Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str "false",
  Double (VhdConcurrentConditionalSignalAssignment,
   Quintuple (Vhdconcurrent_conditional_signal_assignment,
    Double (VhdTargetDotted,
     Triple (VhdNameParametersPrimary, Str dst,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualExpression, Double (VhdIntPrimary, Num n))))),
    Str "false", VhdDelayNone,
    List lst)))) ->
  let delim = ref ("assign "^dst^"["^n^"] = ") in
  List.iter (fun itm -> Buffer.add_string args.buf !delim; match2 args itm; delim := " ") lst;
  Buffer.add_string args.buf ("; // 483\n"^args.indent);
| Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str "false",
  Double (VhdConcurrentSimpleSignalAssignment,
   Quintuple (Vhdconcurrent_simple_signal_assignment,
    Double (VhdTargetDotted,
     Triple (VhdNameParametersPrimary, Str dst,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualExpression, Double (VhdIntPrimary, Num n))))),
    Str "false", VhdDelayNone,
    Double (Vhdwaveform_element, expr))))) ->
Buffer.add_string args.buf ("assign "^dst^"["^n^"] = \n");
(match2 args) expr; Buffer.add_string args.buf ("; // 504\n"^args.indent)
| Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str "false",
  Double (VhdConcurrentSimpleSignalAssignment,
   Quintuple (Vhdconcurrent_simple_signal_assignment,
    Double (VhdTargetDotted,
     Triple (VhdNameParametersPrimary, Str dst,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualDiscreteRange,
        Double (VhdRange,
         Triple (VhdDecreasingRange, Double (VhdIntPrimary, Num hi),
          Double (VhdIntPrimary, Num lo))))))),
    Str "false", VhdDelayNone,
    Double (Vhdwaveform_element, Str rhs))))) ->
Buffer.add_string args.buf ("assign /*499*/ "^dst^" = "^rhs^";\n");
(*
 | Double (VhdConcurrentSignalAssignmentStatement,
 Quadruple (Vhdconcurrent_signal_assignment_statement, Str "", Str "false",
  Double (VhdConcurrentConditionalSignalAssignment,
   Quintuple (Vhdconcurrent_conditional_signal_assignment,
    Double (VhdTargetDotted,
     Triple (VhdNameParametersPrimary, Str dst,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualExpression, Double (VhdIntPrimary, Num n))))),
    Str "false", VhdDelayNone,
    List rhslst)))) ->
Buffer.add_string args.buf ("assign "^dst^"["^n^"] = ");
List.iter (match2 args) rhslst;
 Buffer.add_string args.buf ("; // 531\n"^args.indent)
*)
| Double (VhdSequentialSignalAssignment,
    Double (VhdSimpleSignalAssignment,
     Quintuple (Vhdsimple_signal_assignment_statement, Str "", Str dst,
      VhdDelayNone,
      Double (Vhdwaveform_element,
       Triple (VhdNameParametersPrimary, Str "std_logic_vector",
        Triple (Vhdassociation_element, VhdFormalIndexed,
         Double (VhdActualExpression,
          Triple (VhdNameParametersPrimary, Str "to_unsigned",
           List
            [expr;
             Triple (Vhdassociation_element, VhdFormalIndexed,
                                             Double (VhdActualExpression, Double (VhdIntPrimary, Num "8")))])))))))) ->
                                             Buffer.add_string args.buf (dst^" <= ");
                                             match2 args expr;
                                             Buffer.add_string args.buf ("; // 427\n")

| Double (VhdSequentialSignalAssignment,
          Double (VhdSimpleSignalAssignment,
                  Quintuple
                    (Vhdsimple_signal_assignment_statement,
                     Str "", Str dst, VhdDelayNone,
                     Double (Vhdwaveform_element,
			     expr)))) ->
   Buffer.add_string args.buf (dst^" <= "); (match2 args) expr; Buffer.add_string args.buf ("; // 413\n"^args.indent)
| Double (VhdConcurrentProcessStatement, Sextuple (Vhdprocess_statement, Str process, Str cond,
  Double (VhdSensitivityExpressionList, List dep_lst),
  decls,
  Double (VhdSequentialIf,
 Quintuple (Vhdif_statement, Str "",
  Double (VhdCondition,
   Double (VhdParenthesedPrimary,
    (Triple (VhdEqualRelation, Str reset, Double (VhdCharPrimary, Char rsense)) as rcond))),
  reset_clause,
  Double (VhdElsif,
   Quintuple (Vhdif_statement, Str "",
    Double (VhdCondition,
     Double (VhdParenthesedPrimary,
      Triple (VhdAndLogicalExpression,
       Double (VhdAttributeName,
        Triple (Vhdattribute_name, Double (VhdSuffixSimpleName, Str clk), Str "event")),
       Triple (VhdEqualRelation, Str clk',
        Double (VhdCharPrimary, Char csense))))),
    main_clause,
    VhdElseNone))))))
| Double (VhdConcurrentProcessStatement,
    Sextuple (Vhdprocess_statement, Str process,
     Str cond,
     Double (VhdSensitivityExpressionList, List dep_lst),
     decls,
     Double (VhdSequentialIf,
      Quintuple (Vhdif_statement, Str "",
       Double (VhdCondition,
        (Triple (VhdEqualRelation, Str reset,
         Double (VhdCharPrimary, Char rsense)) as rcond)),
       reset_clause,
       Double (VhdElsif,
        Quintuple (Vhdif_statement, Str "",
         Double (VhdCondition,
          Triple (VhdAndLogicalExpression,
           Double (VhdAttributeName,
            Triple (Vhdattribute_name,
             Double (VhdSuffixSimpleName, Str clk), Str "event")),
           Triple (VhdEqualRelation, Str clk',
            Double (VhdCharPrimary, Char csense)))),
         main_clause,
         VhdElseNone))))))
     when List.mem (Str clk) dep_lst && List.mem (Str reset) dep_lst && clk=clk' ->
  
   Buffer.add_string args.buf ("\n"^args.indent^"always @("^edgsense clk csense^" or "^edgsense reset rsense^")\n");
   Buffer.add_string args.buf (args.indent^"  if (");
   match2 args rcond;
   Buffer.add_string args.buf ")\n";
   match2 {args with indent=args.indent^"  "} reset_clause;
   Buffer.add_string args.buf (args.indent^"  else ");
   match2 {args with indent=args.indent^"  "} (mkblk main_clause);
   Buffer.add_string args.buf "\n"
| Double (VhdConcurrentProcessStatement,
	   Sextuple (Vhdprocess_statement, Str nam, Str _false,
		     Double (VhdSensitivityExpressionList, Str clk),
		     List decls,
		     Double (VhdSequentialIf,
			     Quintuple (Vhdif_statement, Str "", cond,
					Double (VhdSequentialIf,
						Quintuple (Vhdif_statement, Str "", cond',
							   rststmts,
							   Double (VhdElse,
								   stmts))),
					VhdElseNone)))) ->
   Buffer.add_string args.buf ("\n"^args.indent^"always @ ( posedge "^clk^")\n");
   Buffer.add_string args.buf (args.indent^"  if ");
   match2 {args with indent=args.indent^"  "} cond';
   Buffer.add_string args.buf ("\n");
   match2 {args with indent=args.indent^"  "} (mkblk rststmts);
   Buffer.add_string args.buf (args.indent^"  else\n");
   match2 args stmts;
| VhdInterfaceModeIn -> Buffer.add_string args.buf "input wire"
| VhdInterfaceModeOut -> Buffer.add_string args.buf "output logic"
| Num n -> Buffer.add_string args.buf ("/* Num */'d"^n)
| Triple (Vhddesign_unit,
    List liblst,
 Double (VhdPrimaryUnit,
   Double (VhdEntityDeclaration,
   Quintuple (Vhdentity_declaration, Str design,
      Triple (Vhdentity_header, obj,
       List lst),
   List [], List [])))) ->
        let buf' = Buffer.create 4096 in
        if Hashtbl.mem args.bufhash design then
            begin
            args.buf <- Hashtbl.find args.bufhash design;
            Buffer.add_buffer buf' args.buf;
            Buffer.clear args.buf;
            end
        else
            begin
            let buf'' = Buffer.create 4096 in
            Hashtbl.add args.bufhash design buf'';
            args.buf <- buf'';
            end;
   	let delim = ref "(\n" in
	Buffer.add_string args.buf ("module "^design);
	List.iter (fun itm ->
		   Buffer.add_string args.buf (!delim^"\t");
		   match2 args itm;
		   delim := ",\n") lst;
	Buffer.add_string args.buf "); // 507\n";
        Buffer.add_buffer args.buf buf';
        args.modref <- design
| Triple (Vhddesign_unit, List liblst,
			  Double (VhdSecondaryUnit,
				  Double (VhdArchitectureBody,
					  Quintuple (Vhdarchitecture_body, Str arch, Str design, lst1, lst2)))) ->
        if Hashtbl.mem args.bufhash design then
            begin
            args.buf <- Hashtbl.find args.bufhash design;
            end
        else
            begin
            let buf' = Buffer.create 4096 in
            Hashtbl.add args.bufhash design buf';
            args.buf <- buf';
            end;
        
   Buffer.add_string args.buf ("/* design "^design^" */\n");
   Buffer.add_string args.buf ("/* architecture "^arch^" */\n");
   Buffer.add_string args.buf ("typedef enum {FALSE,TRUE} bool_t; // 527\n");
List.iter (match2 args) (match lst1 with List lst -> lst | _ -> [lst1]);
List.iter (match2 args) (match lst2 with List lst -> lst | _ -> [lst2]);
| Triple (Vhddesign_unit,
    List liblst,
      (Double ((VhdPrimaryUnit|VhdSecondaryUnit),
         Double ((VhdEntityDeclaration|VhdPackageDeclaration|VhdArchitectureBody|VhdPackageBody|VhdConfigurationDeclaration) as decl,
            (Quintuple ((Vhdentity_declaration|Vhdpackage_declaration|Vhdconfiguration_declaration), Str nam, _, _, _) |
                Quintuple (Vhdarchitecture_body, _, Str nam, _, _) |
                   Triple (Vhdpackage_body, Str nam, _)))) as u)) ->
   let _ = List.map (function
     | Double (VhdContextLibraryClause, List [Str first]) -> first
     | Double (VhdContextUseClause,
	       List
		 [List
		     [VhdSuffixAll;
		      Double (VhdSuffixSimpleName,
			      Double (VhdSimpleName, Str second));
		      Double (VhdSuffixSimpleName, Double (VhdSimpleName, Str third))]]) -> String.concat "." [second;third]
     | _ -> "unknown") liblst in args.liblst := ((decl,nam), u) :: !(args.liblst)
| Double (VhdBlockSubProgramDeclaration,
      Double (VhdFunctionSpecification,
       Septuple (Vhdfunction_specification,
        Double (VhdDesignatorIdentifier, Str fn),
        arg', List [], List [], Str kind, VhdUnknown))) ->
   Buffer.add_string args.buf ("/* FunctionSpecification "^fn^":"^kind^" */\n");
   args.fns := (fn, (arg', kind)) :: !(args.fns);
| Double (VhdBlockSubProgramBody,
 Quadruple (Vhdsubprogram_body,
  Double (VhdFunctionSpecification,
   Septuple (Vhdfunction_specification,
    Double (VhdDesignatorIdentifier, Str fn),
    arglst, List [], List [], Str typ, VhdUnknown)),
  decls, stmts)) ->
   if List.mem_assoc fn !(args.fns) then
     begin
       match List.assoc fn !(args.fns) with
       | (fnargs,fnkind) ->
	  if List.mem_assoc fnkind !(args.subtype) then
	    begin
	      match List.assoc typ !(args.subtype) with
	      | "std_ulogic_vector", range ->
		 Buffer.add_string args.buf ("      function ");
		match2 args range;
		Buffer.add_string args.buf (" "^fn^"; // 571\n");
	      | _ -> Buffer.add_string args.buf ("    function [???] "^fn^"; // 572\n");
	    end
	  else
	    begin
	      Buffer.add_string args.buf ("    function "^fnkind^" "^fn^"; // 576\n");
	    end;
	 List.iter (function
	 | Double (VhdInterfaceObjectDeclaration,
		   Double (VhdInterfaceDefaultDeclaration,
			   Sextuple (Vhdinterface_default_declaration, Str itm,
				     dir,
				     Quadruple (Vhdsubtype_indication, Str "", Str kind,
						VhdNoConstraint),
				     VhdSignalKindDefault, VhdNone))) ->
	    if List.mem_assoc kind !(args.subtype) then
	      begin
		match List.assoc typ !(args.subtype) with
		| "std_ulogic_vector", range ->
		   Buffer.add_string args.buf ("       input ");
		  match2 args range;
		  Buffer.add_string args.buf (itm^"; // 592\n");
		  args.siglst := (itm,range) :: !(args.siglst);
		| _ -> Buffer.add_string args.buf ("       input [???] "^itm^"; // 594\n");
		  args.siglst := (itm,dummyrng 0 0) :: !(args.siglst);
	      end
	 | Double (VhdInterfaceObjectDeclaration,
		   Double (VhdInterfaceDefaultDeclaration,
			   Sextuple (Vhdinterface_default_declaration, Str itm,
				     dir,
				     Quadruple (Vhdsubtype_indication, Str "", Str kind,
						Double (VhdArrayConstraint,
							Triple (Vhdassociation_element, VhdFormalIndexed, range))),
				     VhdSignalKindDefault, VhdNone))) ->
            Buffer.add_string args.buf ("       input ");
	   match2 args range;
	   Buffer.add_string args.buf (itm^"; // 607\n");
	   args.siglst := (itm,range) :: !(args.siglst);
	 | oth -> failwith "fnargs") (match fnargs with List lst -> lst | oth -> [oth]);
	 match2 args decls;
	 Buffer.add_string args.buf ("       begin\n");
	 match2 args stmts;
	 Buffer.add_string args.buf ("       end\n");
	 Buffer.add_string args.buf ("    endfunction \n");
     end
   else
       Buffer.add_string args.buf ("/* VhdBlockSubProgramBody "^fn^": interface not found */\n");
| Double (VhdBlockSignalDeclaration,
          Quintuple (Vhdsignal_declaration,
           List lst,
           Quadruple (Vhdsubtype_indication, Str "", Str typ,
            VhdNoConstraint), VhdSignalKindDefault, Str reset)) ->
   let delim = ref typ in
   List.iter (fun itm -> Buffer.add_string args.buf (!delim^" "); match2 args itm; delim := ",") lst;
   Buffer.add_string args.buf ("; // 604\n");
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str signal,
  Quadruple (Vhdsubtype_indication, Str "", Str typ,
   Double (VhdArrayConstraint,
    Triple (Vhdassociation_element, VhdFormalIndexed, range))),
  VhdSignalKindDefault, VhdNone)) ->
   Buffer.add_string args.buf ("reg ");
   match2 args range;
   Buffer.add_string args.buf (signal^"; // 605\n");
   args.siglst := (signal,range) :: !(args.siglst);
| Double (VhdBlockSignalDeclaration,
	    Quintuple (Vhdsignal_declaration, Str signal,
		       Quadruple (Vhdsubtype_indication, Str "", typ,
				  VhdNoConstraint),
		       VhdSignalKindDefault, VhdNone)) ->
   Buffer.add_string args.buf ("reg "^signal^"; // 612\n");
   args.siglst := (signal,dummyrng 0 0) :: !(args.siglst);
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str signal,
  Quadruple (Vhdsubtype_indication, Str "", Str ("natural"|"integer"),
   Double (VhdRangeConstraint,
    Triple (VhdIncreasingRange, Double (VhdIntPrimary, Num num),
     expr))),
  VhdSignalKindDefault, VhdNone)) ->
   let hi = match const_expr args expr with
     | Int hi -> string_of_int hi
     | Symbolic nam -> nam
     | Up(const1, const2) -> "Up of (const1, const2)"
     | Down(const1, const2) -> "Down of (const1, const2)"
     | Plus(const1, const2) -> "Plus of (const1, const2)"
     | Minus(const1, const2) -> "Minus of (const1, const2)"
     | Unmatched -> failwith "Invalid natural range" in
   Buffer.add_string args.buf ("reg [$clog2("^hi^")-1:"^num^"] "^signal^"; // 625\n");
   args.siglst := (signal,dummyrng 0 0) :: !(args.siglst);
| Double (VhdBlockSubTypeDeclaration,
	  Triple (Vhdsubtype_declaration, Str typ,
		  Quadruple (Vhdsubtype_indication, Str "", kind,
			     Double (VhdRangeConstraint,
				     Triple (VhdIncreasingRange, lo, hi))))) ->
   Buffer.add_string args.buf ("/* SubTypeDeclaration_range "^typ^" */\n");
| Triple (VhdEnumerationTypeDefinition, Str typ, List enumlst) ->
   let hi = maxidx (List.length enumlst) and lo = 0 in
   Buffer.add_string args.buf ("typedef enum");
   let delim = ref " {" in
   List.iteri (fun ix itm ->
     Buffer.add_string args.buf !delim;
     match2 args itm;
     delim := ",\n"^args.indent;
     match itm with
     | Str id -> 
	args.siglst := (id,dummyrng hi lo) :: !(args.siglst);
     | _ -> ()) enumlst;
   Buffer.add_string args.buf ("} "^typ^"; // 674\n");
| Double (VhdIdentifierEnumeration, Str enum) -> Buffer.add_string args.buf enum
| Double (VhdRange, Triple (VhdDecreasingRange, hi, lo)) ->
   Buffer.add_string args.buf ("[");
   match2 args hi;
   Buffer.add_string args.buf (":");
   match2 args lo;
   Buffer.add_string args.buf ("] ");
| Triple (VhdDecreasingRange, hi, lo) ->
   match2 args hi;
   Buffer.add_string args.buf (":");
   match2 args lo;
| Double (VhdBlockConstantDeclaration,
	  Quadruple (Vhdconstant_declaration, Str nam,
		     Quadruple (Vhdsubtype_indication, Str "", kind,
				Double (VhdArrayConstraint,
					Triple (Vhdassociation_element, VhdFormalIndexed,
						range))), Double (VhdAggregatePrimary, List lst))) ->
   Buffer.add_string args.buf ("/* block const 692 */\n");
   block_const args nam range lst
| Triple (Str nam, Str kind, mode) ->  Buffer.add_string args.buf ("Nam "^kind^":"); match2 args mode
| Double (VhdInterfaceObjectDeclaration,
	  Double (VhdInterfaceDefaultDeclaration,
		  Sextuple (Vhdinterface_default_declaration, Str port,
			    mode,
			    Quadruple (Vhdsubtype_indication, Str "", Str kind, subtype_constraint),
			    VhdSignalKindDefault, VhdNone))) when String.uppercase_ascii(kind)="INTEGER" ->
  (match mode with
    | VhdInterfaceModeIn -> Buffer.add_string args.buf "input integer"
    | VhdInterfaceModeOut -> Buffer.add_string args.buf "output integer"
    | oth -> match2 args oth);
  Buffer.add_string args.buf ("\t\t"^port);
  args.siglst := (port,dummyrng 0 0) :: !(args.siglst);
| Double (VhdInterfaceObjectDeclaration,
	  Double (VhdInterfaceDefaultDeclaration,
		  Sextuple (Vhdinterface_default_declaration, Str port,
			    mode,
			    Quadruple (Vhdsubtype_indication, Str "", Str kind, subtype_constraint),
			    VhdSignalKindDefault, VhdNone))) when String.uppercase_ascii(kind)="STD_LOGIC" ->
   match2 args mode;
  Buffer.add_string args.buf ("\t\t"^port);
  args.siglst := (port,dummyrng 0 0) :: !(args.siglst);
| Double (VhdInterfaceObjectDeclaration,
	  Double (VhdInterfaceDefaultDeclaration,
		  Sextuple (Vhdinterface_default_declaration, Str port,
			    mode,
			    Quadruple (Vhdsubtype_indication, Str "", Str kind,
				       Double (VhdArrayConstraint,
					       Triple (Vhdassociation_element, VhdFormalIndexed,
						       range))),
			    VhdSignalKindDefault, VhdNone))) ->
   match2 args mode;
   Buffer.add_string args.buf ("\t");
   match2 args range;
   Buffer.add_string args.buf ("\t"^port);
  args.siglst := (port,range) :: !(args.siglst);
(*
| Double (VhdActualExpression, Double (VhdIntPrimary, Num n)) -> Buffer.add_string args.buf ("/* actual */'d"^n)
 *)
| Double (VhdActualExpression,
    (Triple (VhdNameParametersPrimary, Str "to_unsigned",
     List params) as func)) -> match2 args func
| Double (VhdActualDiscreteRange, range) -> match2 args range
| Double (VhdBlockSubTypeDeclaration,
      Triple (Vhdsubtype_declaration, Str typ,
       Quadruple (Vhdsubtype_indication, Str "", Str kind,
        Double (VhdArrayConstraint,
		Triple (Vhdassociation_element, VhdFormalIndexed, rng))))) ->
   args.subtype := (typ, (kind,rng)) :: !(args.subtype);
   Buffer.add_string args.buf ("/* SubTypeDeclaration "^typ^":"^kind^":");
   match2 args rng;
   Buffer.add_string args.buf " */\n"
| Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, Str nam,
       Quadruple (Vhdsubtype_indication, Str "", kind,
        Double (VhdArrayConstraint,
         Triple (Vhdassociation_element, VhdFormalIndexed,
          range))), cexpr)) ->
   Buffer.add_string args.buf ("localparam ");
   match2 args range;
   Buffer.add_string args.buf (" "^nam^" = ");
   match2 args cexpr;
   Buffer.add_string args.buf "; // 755\n";
   args.localp := (nam,cexpr) :: !(args.localp)
| Double (VhdBlockConstantDeclaration,
      Quadruple (Vhdconstant_declaration, Str nam,
       Quadruple (Vhdsubtype_indication, Str "", typ,
		  VhdNoConstraint), expr)) ->
     Buffer.add_string args.buf ("localparam "^nam^" = ");
     match2 args expr;
     Buffer.add_string args.buf ("; // 763\n");
   args.localp := (nam,expr) :: !(args.localp)
| Triple (VhdMultTerm, lft, rght) ->
   match2 args lft;
   Buffer.add_string args.buf (" * ");
   match2 args rght;
| Triple (VhdDivTerm, lft, rght) ->
   match2 args lft;
   Buffer.add_string args.buf (" / ");
   match2 args rght;
| Double (VhdNotFactor, arg) ->
   Buffer.add_string args.buf (" ~ ");
   match2 args arg;
   
| Double (VhdAttributeName,
     Triple (Vhdattribute_name, Double (VhdSuffixSimpleName, Str typ),
	     Str attr)) ->
   (match attr with
    | "length" ->
       if List.mem_assoc typ !(args.subtype) then
	 begin
	   match List.assoc typ !(args.subtype) with
	   | "std_ulogic_vector",
	     Double (VhdActualDiscreteRange,
		     Double (VhdRange,
			     Triple (VhdDecreasingRange, hi, lo))) ->
	      Buffer.add_string args.buf ("((");
	      match2 args hi;
	      Buffer.add_string args.buf (")-(");
	      match2 args lo;
	      Buffer.add_string args.buf (")+1)");
	   | _ -> Buffer.add_string args.buf ("vhdattr_"^attr^"("^typ^")");
	 end
       else
	 Buffer.add_string args.buf ("vhdattr_"^attr^"("^typ^")");
    | _ -> Buffer.add_string args.buf ("vhdattr_"^attr^"("^typ^")"))

|
  Double (VhdSequentialReturn,
   Triple (Vhdreturn_statement, Str "", arg))
   -> Buffer.add_string args.buf "return ";
match2 args arg;

| Double (VhdSequentialLoop,
   Quadruple (Vhdloop_statement, Str "",
    Double (VhdForLoop,
     Triple (Vhdparameter_specification, Str i, range)), stmt))
-> Buffer.add_string args.buf ("for ("^i^"= ...)");
match2 args stmt

| Double (VhdSequentialAssertion,
   Triple (Vhdassertion_statement, Str _,
    Quadruple (Vhdassertion, cond, arange, VhdNone)))
   -> Buffer.add_string args.buf ("/* assert ");
      match2 args cond;
Buffer.add_string args.buf (" */");
| Double (VhdSubProgramVariableDeclaration,
   Quintuple (Vhdvariable_declaration, Str _false_, Str nam,
    Quadruple (Vhdsubtype_indication, Str _, Str kind,
     Double (VhdArrayConstraint,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualExpression,
        Double (VhdAttributeName,
         Triple (Vhdattribute_name,
          Double (VhdSuffixSimpleName, Str _old_crc_), Str _range_)))))),
    VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> Buffer.add_string args.buf ("reg "^_range_^nam^"; // 830\n")
		     | oth ->  Buffer.add_string args.buf ("reg /*"^oth^" */ "^nam^"; // 831\n"))

 | Double (VhdSubProgramAliasDeclaration,
    Quintuple (Vhdalias_declaration,
     Double (VhdDesignatorIdentifier, Str vec1),
     Quadruple (Vhdsubtype_indication, Str "", Str "std_ulogic_vector",
      Double (VhdArrayConstraint,
       Triple (Vhdassociation_element, VhdFormalIndexed,
        Double (VhdActualExpression,
         Double (VhdAttributeName,
          Triple (Vhdattribute_name, Double (VhdSuffixSimpleName, Str vec2),
           Str range)))))),
     Str vec3, VhdNone)) -> Buffer.add_string args.buf ("/* alias "^vec1^" "^vec2^" "^vec3^" "^range^" */\n");

| Double (VhdSubProgramVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            VhdNoConstraint),
		     VhdNone)) -> (match kind with
		     | "boolean" -> Buffer.add_string args.buf ("reg "^nam^"; // 850\n")
		     | oth ->  Buffer.add_string args.buf ("reg /*"^oth^" */ "^nam^"; // 851\n"))

 |									 
  Double (VhdSubProgramVariableDeclaration,
          Quintuple (Vhdvariable_declaration, Str _false, Str nam,
           Quadruple (Vhdsubtype_indication, Str "", Str kind,
            Double (VhdArrayConstraint,
             Triple (Vhdassociation_element, VhdFormalIndexed,
              range))),
		     VhdNone)) ->
   (match kind with
   | "std_ulogic_vector" -> Buffer.add_string args.buf ("reg "); match2 args range;  Buffer.add_string args.buf (nam^"; // 862\n")
		     | oth ->  Buffer.add_string args.buf ("reg /*"^oth^" */ "^nam^"; // 863\n"))
| Triple (Vhdassociation_element, VhdFormalIndexed,
 Double (VhdActualExpression,
  Triple (VhdNameParametersPrimary, Str input,
   Triple (Vhdassociation_element, VhdFormalIndexed,
                                   Double (VhdActualExpression, Str i))))) -> Buffer.add_string args.buf (input^"("^i^")")

| Double (VhdConcurrentComponentInstantiationStatement,
 Quintuple (Vhdcomponent_instantiation_statement,
  Str inst,
  Double (VhdInstantiatedComponent, Str kind),
  assoc',
  List lst)) ->
 Buffer.add_string args.buf (kind^" ");
 let assoc = match assoc' with List assoc -> assoc | _ -> [assoc'] in
 if assoc <> [] then
    begin
    let delim = ref " #(" in
    List.iter (fun arg -> Buffer.add_string args.buf !delim; match2 args arg; delim := ", ") assoc;
    Buffer.add_string args.buf (") ");
    end;
 Buffer.add_string args.buf (inst^" ");
 let delim = ref "(" in
 let portlst = List.assoc kind !(args.components) in
 List.iteri (fun ix itm -> Buffer.add_string args.buf !delim;
                 (match itm, List.nth portlst ix with
                        | Double (VhdTargetDotted, _ ),_ -> match2 args itm
                        | _, Double (VhdInterfaceObjectDeclaration,
                                      Double (VhdInterfaceDefaultDeclaration,
                                       Sextuple (Vhdinterface_default_declaration, Str s, _, _, _, _))) ->
                                       Buffer.add_string args.buf ("."^s^"(");
                                       match2 args itm; 
                                       Buffer.add_string args.buf (")");
                        | oth -> noportlst := oth :: !noportlst;
                                    Buffer.add_string args.buf (!delim^"/*"^string_of_int ix^"*/"));
                 delim := ",\n") lst;
 Buffer.add_string args.buf "); // 879\n";
| Triple (Vhdassociation_element,
              Double (VhdFormalExpression, Str formal),
              Double (VhdActualExpression, actual)) ->
 Buffer.add_string args.buf ("\n\t."^formal^"(");
 match2 args actual;
 Buffer.add_string args.buf (")");
| Double (VhdConcurrentProcessStatement, Sextuple (Vhdprocess_statement, Str process, Str cond,
  Double (VhdSensitivityExpressionList, List dep_lst),
  decls,
  contents)) ->
  let delim = ref "\nalways @(" in
  List.iter (fun itm -> Buffer.add_string args.buf !delim; match2 args itm; delim := " or ") dep_lst;
  Buffer.add_string args.buf ")\nbegin\n";
  List.iter (match2 args) (match contents with List lst -> lst | oth -> [oth]);
  Buffer.add_string args.buf "\nend\n\n"

| Double (VhdConcatSimpleExpression, List lst) ->
  let delim = ref "{" in
  List.iter (fun itm -> Buffer.add_string args.buf !delim; match2 args itm; delim := ", ") lst;
  Buffer.add_string args.buf "}\n";

| Double (VhdConcurrentProcessStatement,
 Sextuple (Vhdprocess_statement, Str nam, Str _false,
  Double (VhdSensitivityExpressionList, Str clk), List decls,
  Double (VhdSequentialIf,
   Quintuple (Vhdif_statement, Str "",
    Double (VhdCondition,
     Triple (VhdAndLogicalExpression,
      Double (VhdAttributeName,
       Triple (Vhdattribute_name,
        Double (VhdSuffixSimpleName, Str clk'), Str "event")),
      Triple (VhdEqualRelation, Str clk'',
       Double (VhdCharPrimary, Char '1')))), stmt, VhdElseNone)))) when clk=clk' && clk'=clk'' ->
   Buffer.add_string args.buf ("\n"^args.indent^"always @ ( posedge "^clk^")\n");
   match2 args stmt
| Double (VhdElsif, Quintuple (Vhdif_statement, Str "", cond, thenstmts, elsestmts)) ->
   Buffer.add_string args.buf ("else if ");
    (match2 args) cond;
    Buffer.add_string args.buf ("\n"^args.indent);
    (match2  {args with indent=args.indent^"  "}) (mkblk thenstmts);
    Buffer.add_string args.buf "\n";
    match2 args elsestmts
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration,
  List decls,
  Quadruple (Vhdsubtype_indication, Str "", Str kind,
   VhdNoConstraint),
  VhdSignalKindDefault, VhdNone)) ->
   let delim = ref kind in List.iter (fun itm -> Buffer.add_string args.buf (!delim^" "); match2 args itm; delim := ",") decls;
   Buffer.add_string args.buf ("; // 908\n")
| Double (VhdBlockComponentDeclaration, Quadruple (Vhdcomponent_declaration, Str kind,  _,  List fields)) ->
   args.components := (kind,fields) :: !(args.components);
(*
| Double (VhdBlockComponentDeclaration,
 Quadruple (Vhdcomponent_declaration, Str kind,
  Double (VhdInterfaceObjectDeclaration,
   Double (VhdInterfaceDefaultDeclaration,
    Sextuple (Vhdinterface_default_declaration, Str width,
     VhdInterfaceModeIn,
     Quadruple (Vhdsubtype_indication, Str "", Str typ,
      Double (VhdRangeConstraint,
       Triple (VhdIncreasingRange, Double (VhdIntPrimary, Num num),
        Double (VhdIntPrimary, Num num')))),
     VhdSignalKindDefault, Double (VhdIntPrimary, Num num'')))),
  List lst)) ->
   Buffer.add_string args.buf ("// Declare component "^kind^"; // 1152\n") (* Verilog doesn't need this *)
| Double (VhdBlockComponentDeclaration,
 Quadruple (Vhdcomponent_declaration, Str kind,
  Double (VhdInterfaceObjectDeclaration,
   Double (VhdInterfaceDefaultDeclaration,
    Sextuple (Vhdinterface_default_declaration, Str width,
     VhdInterfaceModeIn,
     Quadruple (Vhdsubtype_indication, Str "", Str typ,
      VhdNoConstraint),
     VhdSignalKindDefault, Double (VhdIntPrimary, Num num'')))),
  List fields)) ->
   args.components := (kind,fields) :: !(args.components);
| Double (VhdBlockComponentDeclaration,
 Quadruple (Vhdcomponent_declaration, Str kind, List lst, List fields)) ->
   args.components := (kind,fields) :: !(args.components);
*)
| Double (VhdAggregatePrimary,
    Triple (Vhdelement_association, VhdChoiceOthers,
                                    Double (VhdCharPrimary, Char '-'))) -> Buffer.add_string args.buf ("// Aggregate 966;\n")
| Double (VhdChoiceDiscreteRange,
    Double (VhdRange,
     Triple (VhdIncreasingRange, Double (VhdIntPrimary, Num lo),
      Double (VhdIntPrimary, Num hi)))) -> Buffer.add_string args.buf ("// Discrete range "^lo^":"^hi^" // 970;\n")
| Double (VhdAggregatePrimary,
    Triple (Vhdelement_association, VhdChoiceOthers,
     Double (VhdCharPrimary, Char '0'))) -> Buffer.add_string args.buf ("'b0")
| Triple (Vhdassociation_element, VhdFormalIndexed,
    Double (VhdActualExpression, actual)) -> match2 args actual
| Double (VhdOperatorString, Str "--") -> Buffer.add_string args.buf ("'bxx")
| Double (VhdOperatorString, Str "---") -> Buffer.add_string args.buf ("'bxxx")
| Double (VhdBlockTypeDeclaration,
  Double (VhdFullType,
  Triple (Vhdfull_type_declaration, Str typ,
   Double (VhdArrayTypeDefinition,
    Double (VhdConstrainedArray,
     Triple (Vhdconstrained_array_definition,
      Triple (Vhdassociation_element, VhdFormalIndexed,
       Double (VhdActualDiscreteRange, rng)),
      Quadruple (Vhdsubtype_indication, Str "", Str "std_logic_vector",
       Double (VhdArrayConstraint,
        Triple (Vhdassociation_element, VhdFormalIndexed,
         Double (VhdActualDiscreteRange, rng')))))))))) -> Buffer.add_string args.buf "full_type\n"

(*
| Double (VhdBlockSignalDeclaration,
 Quintuple (Vhdsignal_declaration, Str nam,
  Quadruple (Vhdsubtype_indication, Str "", Str "integer",
   Double (VhdRangeConstraint,
    Triple (VhdIncreasingRange, Double (VhdIntPrimary, Num lo),
     Double (VhdIntPrimary, Num hi)))),
  VhdSignalKindDefault, VhdNone)) ->  Buffer.add_string args.buf ("integer "^nam^"; // 900\n")
| Double (VhdSequentialSignalAssignment,
 Double (VhdSimpleSignalAssignment,
  Quintuple (Vhdsimple_signal_assignment_statement, Str "",
   Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str id,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualExpression, Double (VhdIntPrimary, Num n))))),
   VhdDelayNone, Double (Vhdwaveform_element, Str "D")))) -> Buffer.add_string args.buf ("/* 442 */")
| Double (VhdSequentialSignalAssignment,
 Double (VhdSimpleSignalAssignment,
  Quintuple (Vhdsimple_signal_assignment_statement, Str "",
   Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str id,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualExpression, Double (VhdIntPrimary, Num n))))),
   VhdDelayNone,
   Double (Vhdwaveform_element,
    Triple (VhdNameParametersPrimary, Str id',
     Triple (Vhdassociation_element, VhdFormalIndexed,
                                     Double (VhdActualExpression, Double (VhdIntPrimary, Num n')))))))) -> Buffer.add_string args.buf ("/* 454 */")
| Double (VhdSequentialSignalAssignment,
 Double (VhdSimpleSignalAssignment,
  Quintuple (Vhdsimple_signal_assignment_statement, Str "",
   Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str dst,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualDiscreteRange,
       Double (VhdRange,
        Triple (VhdDecreasingRange, Double (VhdIntPrimary, Num hi),
         Double (VhdIntPrimary, Num lo))))))),
   VhdDelayNone, Double (Vhdwaveform_element, Str rhs)))) ->
Buffer.add_string args.buf (dst^" <= "^rhs);
| Double (VhdSequentialSignalAssignment,
 Double (VhdSimpleSignalAssignment,
  Quintuple (Vhdsimple_signal_assignment_statement, Str "",
   Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str dst,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualDiscreteRange,
       Double (VhdRange,
        Triple (VhdDecreasingRange, Double (VhdIntPrimary, Num hi),
         Double (VhdIntPrimary, Num lo))))))),
   VhdDelayNone,
   Double (Vhdwaveform_element,
    Triple (VhdNameParametersPrimary, Str rhs,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualDiscreteRange,
       Double (VhdRange,
        Triple (VhdDecreasingRange, Double (VhdIntPrimary, Num hi'),
         Double (VhdIntPrimary, Num lo')))))))))) ->
Buffer.add_string args.buf (dst^" <= "^rhs);
| Double (VhdSequentialSignalAssignment,
 Double (VhdSimpleSignalAssignment,
  Quintuple (Vhdsimple_signal_assignment_statement, Str "",
   Double (VhdTargetDotted,
    Triple (VhdNameParametersPrimary, Str dst,
     Triple (Vhdassociation_element, VhdFormalIndexed,
      Double (VhdActualDiscreteRange,
       Double (VhdRange,
        Triple (VhdDecreasingRange, Double (VhdIntPrimary, Num hi),
         Double (VhdIntPrimary, Num lo))))))),
   VhdDelayNone,
   Double (Vhdwaveform_element,
    Double (VhdAggregatePrimary,
     Triple (Vhdelement_association, VhdChoiceOthers,
      Double (VhdCharPrimary, Char '0'))))))) ->
Buffer.add_string args.buf (dst^" <= 0;");
*)
| oth -> unmatched := oth :: !unmatched; Buffer.add_string args.buf "unmatched\n"

and match2 args pat =
  args.matched := pat :: !(args.matched);
  match2' args pat

and block_const args nam range = function
| Triple (Vhdelement_association, VhdChoiceOthers,
      Double (VhdCharPrimary, Char '0')) :: [] ->
  Buffer.add_string args.buf (nam^" <= '0;\n")
| lst ->
  blocklst := (nam,range,lst) :: !blocklst;
  match const_expr args range with
    | Down(Int hi, Int lo) ->
       let delim = ref "" in
       Buffer.add_string args.buf (nam^" <= ");
       for i = hi downto lo do
	 let others = ref None and choice = ref None in
	 List.iter (function
		     | Triple (Vhdelement_association, VhdChoiceOthers, Double (VhdCharPrimary, Char ch)) ->
			others := Some (int_of_char ch - int_of_char '0');
		     | Triple (Vhdelement_association,
			       List lst',
			       Double (VhdCharPrimary, Char ch)) ->
			List.iter (function
				    | Double (VhdChoiceSimpleExpression, Double (VhdIntPrimary, Num num)) ->
				       if (i == int_of_string num) then
					 begin
					   if (!debug) then Printf.printf "Choice %d,%s,%c\n" i num ch;
					   choice := Some (int_of_char ch - int_of_char '0');
					 end
				    | oth -> unmatched := oth :: !unmatched) lst';
		     | oth -> blocklst := (nam,oth,[]) :: !blocklst) lst;
	 let chosen = match (!choice,!others) with
	   | Some n, _ -> n
	   | None, Some n -> n
	   | None, None -> failwith "No others choice found in constant init" in
	 Buffer.add_string args.buf (!delim^"("^string_of_int chosen^"<<"^string_of_int i^")");
	 delim := "|";
       done;
       Buffer.add_string args.buf (";\n");
       (*
        | Down _ as oth -> downopt := Some oth; Buffer.add_string args.buf (nam^" <= downrange;\n");
       *)
   | oth  -> failwith "const_expr_range"

let cnv lst =
  let args = {modref="TBD";
              bufhash=Hashtbl.create 256;
              buf=Buffer.create 4096;
	      indent="";
	      liblst=ref [];
	      subtype=ref [];
	      localp=ref [];
	      matched = ref [];
	      fns = ref [];
	      siglst = ref [];
              components = ref []} in
  Printexc.record_backtrace true;
  List.iter (match2 args) lst;
  Buffer.add_string args.buf ("\nendmodule\n");
  args

let dump lst =
  let args = cnv lst in
  Hashtbl.iter (fun design buf ->
    let chan = open_out (design^".sv") in
    Buffer.output_buffer chan buf;
    Buffer.clear buf;
    close_out chan) args.bufhash;
  Printexc.print_backtrace stderr;
  args
