open String
let esymbols = Hashtbl.create 256
(* Case-insensitive EDIF keyword matching: Vivado emits `edifversion` /
   `edifLevel` / `keywordmap` / `Library` / `Cell` etc. in mixed case and
   the prior `lowercase_first` only matched the conventional "Title-Case"
   form of each keyword.  Lowercasing the full string at both registration
   and lookup time makes the lexer accept every case variant uniformly,
   matching Cadence/Synopsys/Xilinx EDIF practice. *)
let _ = List.iter (fun (str,key) -> if str <> "" then Hashtbl.add esymbols (String.lowercase_ascii str) key) [
("Ycoord", Edif2.Ycoord);
("Xor", Edif2.Xor);
("Xnor", Edif2.Xnor);
("Xcoord", Edif2.Xcoord);
("Written", Edif2.Written);
("Wire", Edif2.Wire);
("While", Edif2.While);
("When", Edif2.When);
("Weakjoined", Edif2.Weakjoined);
("Weak", Edif2.Weak);
("Wavevalue", Edif2.Wavevalue);
("Voltagemap", Edif2.Voltagemap);
("Visible", Edif2.Visible);
("Viewtype", Edif2.Viewtype);
("Viewref", Edif2.Viewref);
("Viewmap", Edif2.Viewmap);
("Viewlist", Edif2.Viewlist);
("View", Edif2.View);
("Variable", Edif2.Variable);
("Valuenameref", Edif2.Valuenameref);
("VBAR", Edif2.VBAR);
("Userdata", Edif2.Userdata);
("Unused", Edif2.Unused);
("Unit", Edif2.Unit);
("Union", Edif2.Union);
("Undefined", Edif2.Undefined);
("Unconstrained", Edif2.Unconstrained);
("Typedvalue", Edif2.Typedvalue);
("True", Edif2.True);
("Tri", Edif2.Tri);
("Transition", Edif2.Transition);
("Transform", Edif2.Transform);
("Timing", Edif2.Timing);
("Timeinterval", Edif2.Timeinterval);
("TimeStamp", Edif2.TimeStamp);
("Then", Edif2.Then);
("Textheight", Edif2.Textheight);
("Technology", Edif2.Technology);
("Tabledefault", Edif2.Tabledefault);
("Table", Edif2.Table);
("TILDE", Edif2.TILDE);
("Symmetry", Edif2.Symmetry);
("Symbol", Edif2.Symbol);
("Sum", Edif2.Sum);
("Subtract", Edif2.Subtract);
("Strong", Edif2.Strong);
("Stringdisplay", Edif2.Stringdisplay);
("String", Edif2.String);
("Strictlyincreasing", Edif2.Strictlyincreasing);
("Steady", Edif2.Steady);
("Status", Edif2.Status);
("Statement", Edif2.Statement);
("Socketset", Edif2.Socketset);
("Socket", Edif2.Socket);
("Site", Edif2.Site);
("Singlevalueset", Edif2.Singlevalueset);
("Simulationinfo", Edif2.Simulationinfo);
("Simulate", Edif2.Simulate);
("Shape", Edif2.Shape);
("Section", Edif2.Section);
("Scaley", Edif2.Scaley);
("Scalex", Edif2.Scalex);
("Scale", Edif2.Scale);
("SLASH", Edif2.SLASH);
("SEMI", Edif2.SEMI);
("Resolves", Edif2.Resolves);
("Rename", Edif2.Rename);
("Reg", Edif2.Reg);
("Rectanglesize", Edif2.Rectanglesize);
("Rectangle", Edif2.Rectangle);
("Rangevector", Edif2.Rangevector);
("RPAREN", Edif2.RPAREN);
("RCURLY", Edif2.RCURLY);
("RBRACK", Edif2.RBRACK);
("QUOTE", Edif2.QUOTE);
("QUERY", Edif2.QUERY);
("Pt", Edif2.Pt);
("Protectionframe", Edif2.Protectionframe);
("Propertydisplay", Edif2.Propertydisplay);
("Property", Edif2.Property);
("Program", Edif2.Program);
("Product", Edif2.Product);
("Posedge", Edif2.Posedge);
("Portref", Edif2.Portref);
("Portmap", Edif2.Portmap);
("Portlistalias", Edif2.Portlistalias);
("Portlist", Edif2.Portlist);
("Portinstance", Edif2.Portinstance);
("Portimplementation", Edif2.Portimplementation);
("Portgroup", Edif2.Portgroup);
("Portdelay", Edif2.Portdelay);
("Portbundle", Edif2.Portbundle);
("Portbackannotate", Edif2.Portbackannotate);
("Port", Edif2.Port);
("Polygon", Edif2.Polygon);
("Pointsum", Edif2.Pointsum);
("Pointsubtract", Edif2.Pointsubtract);
("Pointlist", Edif2.Pointlist);
("Pointdisplay", Edif2.Pointdisplay);
("Point", Edif2.Point);
("Plug", Edif2.Plug);
("Physicaldesignrule", Edif2.Physicaldesignrule);
("Permutable", Edif2.Permutable);
("Pathwidth", Edif2.Pathwidth);
("Pathdelay", Edif2.Pathdelay);
("Path", Edif2.Path);
("Parameterdisplay", Edif2.Parameterdisplay);
("Parameterassign", Edif2.Parameterassign);
("Parameter", Edif2.Parameter);
("Pagesize", Edif2.Pagesize);
("Page", Edif2.Page);
("Owner", Edif2.Owner);
("Oversize", Edif2.Oversize);
("Overlapdistance", Edif2.Overlapdistance);
("Overhangdistance", Edif2.Overhangdistance);
("Output", Edif2.Output);
("Origin", Edif2.Origin);
("Orientation", Edif2.Orientation);
("Or", Edif2.Or);
("Openshape", Edif2.Openshape);
("Offsetevent", Edif2.Offsetevent);
("Offpageconnector", Edif2.Offpageconnector);
("Numberdisplay", Edif2.Numberdisplay);
("NumberDefinition", Edif2.NumberDefinition);
("Number", Edif2.Number);
("Notchspacing", Edif2.Notchspacing);
("Notallowed", Edif2.Notallowed);
("Not", Edif2.Not);
("Nor", Edif2.Nor);
("Nonpermutable", Edif2.Nonpermutable);
("Nochange", Edif2.Nochange);
("Netref", Edif2.Netref);
("Netmap", Edif2.Netmap);
("Netgroup", Edif2.Netgroup);
("Netdelay", Edif2.Netdelay);
("Netbundle", Edif2.Netbundle);
("Netbackannotate", Edif2.Netbackannotate);
("Net", Edif2.Net);
("Negate", Edif2.Negate);
("Nand", Edif2.Nand);
("Name", Edif2.Name);
("Mustjoin", Edif2.Mustjoin);
("Multiplevalueset", Edif2.Multiplevalueset);
("Module", Edif2.Module);
("Mod", Edif2.Mod);
("Mnm", Edif2.Mnm);
("Minomaxdisplay", Edif2.Minomaxdisplay);
("Minomax", Edif2.Minomax);
("Min", Edif2.Min);
("Member", Edif2.Member);
("Max", Edif2.Max);
("Match", Edif2.Match);
("Maintain", Edif2.Maintain);
("Logicwaveform", Edif2.Logicwaveform);
("Logicvalue", Edif2.Logicvalue);
("Logicref", Edif2.Logicref);
("Logicport", Edif2.Logicport);
("Logicoutput", Edif2.Logicoutput);
("Logiconeof", Edif2.Logiconeof);
("Logicmapoutput", Edif2.Logicmapoutput);
("Logicmapinput", Edif2.Logicmapinput);
("Logiclist", Edif2.Logiclist);
("Logicinput", Edif2.Logicinput);
("Logicassign", Edif2.Logicassign);
("Loaddelay", Edif2.Loaddelay);
("Listofports", Edif2.Listofports);
("Listofnets", Edif2.Listofnets);
("Libraryref", Edif2.Libraryref);
("Library", Edif2.Library);
("Lessthan", Edif2.Lessthan);
("LPAREN", Edif2.LPAREN);
("LCURLY", Edif2.LCURLY);
("LBRACK", Edif2.LBRACK);
("Keywordmap", Edif2.Keywordmap);
("Keywordlevel", Edif2.Keywordlevel);
("Keyworddisplay", Edif2.Keyworddisplay);
("Justify", Edif2.Justify);
("Joined", Edif2.Joined);
("Iterate", Edif2.Iterate);
("Isolated", Edif2.Isolated);
("Inverse", Edif2.Inverse);
("Intrafiguregroupspacing", Edif2.Intrafiguregroupspacing);
("Intersection", Edif2.Intersection);
("Interfiguregroupspacing", Edif2.Interfiguregroupspacing);
("Interface", Edif2.Interface);
("Integerdisplay", Edif2.Integerdisplay);
("Integer", Edif2.Integer);
("Instanceref", Edif2.Instanceref);
("Instancenamedef", Edif2.Instancenamedef);
("Instancemap", Edif2.Instancemap);
("Instancegroup", Edif2.Instancegroup);
("Instancebackannotate", Edif2.Instancebackannotate);
("Instance", Edif2.Instance);
("Input", Edif2.Input);
("Inout", Edif2.Inout);
("Initial", Edif2.Initial);
("Increasing", Edif2.Increasing);
("Includefiguregroup", Edif2.Includefiguregroup);
("Ignore", Edif2.Ignore);
("If", Edif2.If);
("HASH_", Edif2.HASH_);
("Gridmap", Edif2.Gridmap);
("Greaterthan", Edif2.Greaterthan);
("Globalportref", Edif2.Globalportref);
("Form", Edif2.Form);
("Forbiddenevent", Edif2.Forbiddenevent);
("Follow", Edif2.Follow);
("Floor", Edif2.Floor);
("Fix", Edif2.Fix);
("Fillpattern", Edif2.Fillpattern);
("Figurewidth", Edif2.Figurewidth);
("Figureperimeter", Edif2.Figureperimeter);
("Figuregroupref", Edif2.Figuregroupref);
("Figuregroupoverride", Edif2.Figuregroupoverride);
("Figuregroupobject", Edif2.Figuregroupobject);
("Figuregroup", Edif2.Figuregroup);
("Figurearea", Edif2.Figurearea);
("Figure", Edif2.Figure);
("False", Edif2.False);
("Fabricate", Edif2.Fabricate);
("External", Edif2.External);
("Exactly", Edif2.Exactly);
("Event", Edif2.Event);
("Escape", Edif2.Escape);
("Equal", Edif2.Equal);
("Entry", Edif2.Entry);
("Endtype", Edif2.Endtype);
("Endmodule", Edif2.Endmodule);
("Endcase", Edif2.Endcase);
("End", Edif2.End);
("Enclosuredistance", Edif2.Enclosuredistance);
("Else", Edif2.Else);
("Edifversion", Edif2.Edifversion);
("EdifLevel", Edif2.EdifLevel);
("Edif", Edif2.Edif);
("EQUALS", Edif2.EQUALS);
("EOL", Edif2.EOL);
("ENDPRAGMA", Edif2.ENDPRAGMA);
("ENDOFFILE", Edif2.ENDOFFILE);
("EMPTYEDIF", Edif2.EMPTYEDIF);
(* "E" scaled-number keyword removed: collides with Vivado signal names like E[7:0]; (e mant exp) is unused. *)
("Duration", Edif2.Duration);
("Dot", Edif2.Dot);
("Dominates", Edif2.Dominates);
("Divide", Edif2.Divide);
("Display", Edif2.Display);
("Direction", Edif2.Direction);
("Difference", Edif2.Difference);
("Designator", Edif2.Designator);
("Design", Edif2.Design);
("Derivation", Edif2.Derivation);
("Delta", Edif2.Delta);
("Delay", Edif2.Delay);
("Default", Edif2.Default);
("Dcmaxfanout", Edif2.Dcmaxfanout);
("Dcmaxfanin", Edif2.Dcmaxfanin);
("Dcfanoutload", Edif2.Dcfanoutload);
("Dcfaninload", Edif2.Dcfaninload);
("Dataorigin", Edif2.Dataorigin);
("DOT", Edif2.DOT);
("DEFAULT", Edif2.DEFAULT);
("Cycle", Edif2.Cycle);
("Curve", Edif2.Curve);
("Currentmap", Edif2.Currentmap);
("Criticality", Edif2.Criticality);
("Cornertype", Edif2.Cornertype);
("Contents", Edif2.Contents);
("Constraint", Edif2.Constraint);
("Constant", Edif2.Constant);
("Connectlocation", Edif2.Connectlocation);
("Concat", Edif2.Concat);
("Compound", Edif2.Compound);
("Commentgraphics", Edif2.Commentgraphics);
("Comment", Edif2.Comment);
("Color", Edif2.Color);
("Circle", Edif2.Circle);
("Change", Edif2.Change);
("Celltype", Edif2.Celltype);
("Cellref", Edif2.Cellref);
("Cell", Edif2.Cell);
("Ceiling", Edif2.Ceiling);
("Case", Edif2.Case);
("COMMA", Edif2.COMMA);
("COLON", Edif2.COLON);
("Boundingbox", Edif2.Boundingbox);
("Borderwidth", Edif2.Borderwidth);
("Borderpattern", Edif2.Borderpattern);
("Booleanvalue", Edif2.Booleanvalue);
("Booleanmap", Edif2.Booleanmap);
("Booleandisplay", Edif2.Booleandisplay);
("Boolean", Edif2.Boolean);
("Block", Edif2.Block);
("Between", Edif2.Between);
("Begin", Edif2.Begin);
("Becomes", Edif2.Becomes);
("Basearray", Edif2.Basearray);
("BEGINPRAGMA", Edif2.BEGINPRAGMA);
("BECOMES", Edif2.BECOMES);
("Author", Edif2.Author);
("Atmost", Edif2.Atmost);
("Atleast", Edif2.Atleast);
("Assign", Edif2.Assign);
("Arraysite", Edif2.Arraysite);
("Arrayrelatedinfo", Edif2.Arrayrelatedinfo);
("Arraymacro", Edif2.Arraymacro);
("Array", Edif2.Array);
("Arc", Edif2.Arc);
("Apply", Edif2.Apply);
("Annotate", Edif2.Annotate);
("And", Edif2.And);
("Always", Edif2.Always);
("After", Edif2.After);
("Acload", Edif2.Acload);
("Abs", Edif2.Abs);
("AT", Edif2.AT);
("AMPERSAND", Edif2.AMPERSAND);
("", Edif2.EMPTYEDIF)]
let getstr tok = match tok with
  | Edif2.Ycoord -> ("Ycoord")
  | Edif2.Xor -> ("Xor")
  | Edif2.Xnor -> ("Xnor")
  | Edif2.Xcoord -> ("Xcoord")
  | Edif2.Written -> ("Written")
  | Edif2.Wire -> ("Wire")
  | Edif2.While -> ("While")
  | Edif2.When -> ("When")
  | Edif2.Weakjoined -> ("Weakjoined")
  | Edif2.Weak -> ("Weak")
  | Edif2.Wavevalue -> ("Wavevalue")
  | Edif2.Voltagemap -> ("Voltagemap")
  | Edif2.Visible -> ("Visible")
  | Edif2.Viewtype -> ("Viewtype")
  | Edif2.Viewref -> ("Viewref")
  | Edif2.Viewmap -> ("Viewmap")
  | Edif2.Viewlist -> ("Viewlist")
  | Edif2.View -> ("View")
  | Edif2.Variable -> ("Variable")
  | Edif2.Valuenameref -> ("Valuenameref")
  | Edif2.VBAR -> ("VBAR")
  | Edif2.Userdata -> ("Userdata")
  | Edif2.Unused -> ("Unused")
  | Edif2.Unit -> ("Unit")
  | Edif2.Union -> ("Union")
  | Edif2.Undefined -> ("Undefined")
  | Edif2.Unconstrained -> ("Unconstrained")
  | Edif2.Typedvalue -> ("Typedvalue")
  | Edif2.True -> ("True")
  | Edif2.Tri -> ("Tri")
  | Edif2.Transition -> ("Transition")
  | Edif2.Transform -> ("Transform")
  | Edif2.Timing -> ("Timing")
  | Edif2.Timeinterval -> ("Timeinterval")
  | Edif2.TimeStamp -> ("TimeStamp")
  | Edif2.Then -> ("Then")
  | Edif2.Textheight -> ("Textheight")
  | Edif2.Technology -> ("Technology")
  | Edif2.Tabledefault -> ("Tabledefault")
  | Edif2.Table -> ("Table")
  | Edif2.TRIPLE arg  -> ("TRIPLE")
  | Edif2.TLIST2 arg  -> ("TLIST2")
  | Edif2.TLIST arg  -> ("TLIST")
  | Edif2.TILDE -> ("TILDE")
  | Edif2.Symmetry -> ("Symmetry")
  | Edif2.Symbol -> ("Symbol")
  | Edif2.Sum -> ("Sum")
  | Edif2.Subtract -> ("Subtract")
  | Edif2.Strong -> ("Strong")
  | Edif2.Stringdisplay -> ("Stringdisplay")
  | Edif2.String -> ("String")
  | Edif2.Strictlyincreasing -> ("Strictlyincreasing")
  | Edif2.Steady -> ("Steady")
  | Edif2.Status -> ("Status")
  | Edif2.Statement -> ("Statement")
  | Edif2.Socketset -> ("Socketset")
  | Edif2.Socket -> ("Socket")
  | Edif2.Site -> ("Site")
  | Edif2.Singlevalueset -> ("Singlevalueset")
  | Edif2.Simulationinfo -> ("Simulationinfo")
  | Edif2.Simulate -> ("Simulate")
  | Edif2.Shape -> ("Shape")
  | Edif2.Section -> ("Section")
  | Edif2.Scaley -> ("Scaley")
  | Edif2.Scalex -> ("Scalex")
  | Edif2.Scale -> ("Scale")
  | Edif2.STRING arg  -> ("STRING")
  | Edif2.SLASH -> ("SLASH")
  | Edif2.SEXTUPLE arg  -> ("SEXTUPLE")
  | Edif2.SEMI -> ("SEMI")
  | Edif2.Resolves -> ("Resolves")
  | Edif2.Rename -> ("Rename")
  | Edif2.Reg -> ("Reg")
  | Edif2.Rectanglesize -> ("Rectanglesize")
  | Edif2.Rectangle -> ("Rectangle")
  | Edif2.Rangevector -> ("Rangevector")
  | Edif2.RPAREN -> ("RPAREN")
  | Edif2.RCURLY -> ("RCURLY")
  | Edif2.RBRACK -> ("RBRACK")
  | Edif2.QUOTE -> ("QUOTE")
  | Edif2.QUERY -> ("QUERY")
  | Edif2.QUADRUPLE arg  -> ("QUADRUPLE")
  | Edif2.Pt -> ("Pt")
  | Edif2.Protectionframe -> ("Protectionframe")
  | Edif2.Propertydisplay -> ("Propertydisplay")
  | Edif2.Property -> ("Property")
  | Edif2.Program -> ("Program")
  | Edif2.Product -> ("Product")
  | Edif2.Posedge -> ("Posedge")
  | Edif2.Portref -> ("Portref")
  | Edif2.Portmap -> ("Portmap")
  | Edif2.Portlistalias -> ("Portlistalias")
  | Edif2.Portlist -> ("Portlist")
  | Edif2.Portinstance -> ("Portinstance")
  | Edif2.Portimplementation -> ("Portimplementation")
  | Edif2.Portgroup -> ("Portgroup")
  | Edif2.Portdelay -> ("Portdelay")
  | Edif2.Portbundle -> ("Portbundle")
  | Edif2.Portbackannotate -> ("Portbackannotate")
  | Edif2.Port -> ("Port")
  | Edif2.Polygon -> ("Polygon")
  | Edif2.Pointsum -> ("Pointsum")
  | Edif2.Pointsubtract -> ("Pointsubtract")
  | Edif2.Pointlist -> ("Pointlist")
  | Edif2.Pointdisplay -> ("Pointdisplay")
  | Edif2.Point -> ("Point")
  | Edif2.Plug -> ("Plug")
  | Edif2.Physicaldesignrule -> ("Physicaldesignrule")
  | Edif2.Permutable -> ("Permutable")
  | Edif2.Pathwidth -> ("Pathwidth")
  | Edif2.Pathdelay -> ("Pathdelay")
  | Edif2.Path -> ("Path")
  | Edif2.Parameterdisplay -> ("Parameterdisplay")
  | Edif2.Parameterassign -> ("Parameterassign")
  | Edif2.Parameter -> ("Parameter")
  | Edif2.Pagesize -> ("Pagesize")
  | Edif2.Page -> ("Page")
  | Edif2.Owner -> ("Owner")
  | Edif2.Oversize -> ("Oversize")
  | Edif2.Overlapdistance -> ("Overlapdistance")
  | Edif2.Overhangdistance -> ("Overhangdistance")
  | Edif2.Output -> ("Output")
  | Edif2.Origin -> ("Origin")
  | Edif2.Orientation -> ("Orientation")
  | Edif2.Or -> ("Or")
  | Edif2.Openshape -> ("Openshape")
  | Edif2.Offsetevent -> ("Offsetevent")
  | Edif2.Offpageconnector -> ("Offpageconnector")
  | Edif2.Numberdisplay -> ("Numberdisplay")
  | Edif2.NumberDefinition -> ("NumberDefinition")
  | Edif2.Number -> ("Number")
  | Edif2.Notchspacing -> ("Notchspacing")
  | Edif2.Notallowed -> ("Notallowed")
  | Edif2.Not -> ("Not")
  | Edif2.Nor -> ("Nor")
  | Edif2.Nonpermutable -> ("Nonpermutable")
  | Edif2.Nochange -> ("Nochange")
  | Edif2.Netref -> ("Netref")
  | Edif2.Netmap -> ("Netmap")
  | Edif2.Netgroup -> ("Netgroup")
  | Edif2.Netdelay -> ("Netdelay")
  | Edif2.Netbundle -> ("Netbundle")
  | Edif2.Netbackannotate -> ("Netbackannotate")
  | Edif2.Net -> ("Net")
  | Edif2.Negate -> ("Negate")
  | Edif2.Nand -> ("Nand")
  | Edif2.Name -> ("Name")
  | Edif2.Mustjoin -> ("Mustjoin")
  | Edif2.Multiplevalueset -> ("Multiplevalueset")
  | Edif2.Module -> ("Module")
  | Edif2.Mod -> ("Mod")
  | Edif2.Mnm -> ("Mnm")
  | Edif2.Minomaxdisplay -> ("Minomaxdisplay")
  | Edif2.Minomax -> ("Minomax")
  | Edif2.Min -> ("Min")
  | Edif2.Member -> ("Member")
  | Edif2.Max -> ("Max")
  | Edif2.Match -> ("Match")
  | Edif2.Maintain -> ("Maintain")
  | Edif2.MACRO arg  -> ("MACRO")
  | Edif2.Logicwaveform -> ("Logicwaveform")
  | Edif2.Logicvalue -> ("Logicvalue")
  | Edif2.Logicref -> ("Logicref")
  | Edif2.Logicport -> ("Logicport")
  | Edif2.Logicoutput -> ("Logicoutput")
  | Edif2.Logiconeof -> ("Logiconeof")
  | Edif2.Logicmapoutput -> ("Logicmapoutput")
  | Edif2.Logicmapinput -> ("Logicmapinput")
  | Edif2.Logiclist -> ("Logiclist")
  | Edif2.Logicinput -> ("Logicinput")
  | Edif2.Logicassign -> ("Logicassign")
  | Edif2.Loaddelay -> ("Loaddelay")
  | Edif2.Listofports -> ("Listofports")
  | Edif2.Listofnets -> ("Listofnets")
  | Edif2.Libraryref -> ("Libraryref")
  | Edif2.Library -> ("Library")
  | Edif2.Lessthan -> ("Lessthan")
  | Edif2.LPAREN -> ("LPAREN")
  | Edif2.LCURLY -> ("LCURLY")
  | Edif2.LBRACK -> ("LBRACK")
  | Edif2.Keywordmap -> ("Keywordmap")
  | Edif2.Keywordlevel -> ("Keywordlevel")
  | Edif2.Keyworddisplay -> ("Keyworddisplay")
  | Edif2.Justify -> ("Justify")
  | Edif2.Joined -> ("Joined")
  | Edif2.Iterate -> ("Iterate")
  | Edif2.Isolated -> ("Isolated")
  | Edif2.Inverse -> ("Inverse")
  | Edif2.Intrafiguregroupspacing -> ("Intrafiguregroupspacing")
  | Edif2.Intersection -> ("Intersection")
  | Edif2.Interfiguregroupspacing -> ("Interfiguregroupspacing")
  | Edif2.Interface -> ("Interface")
  | Edif2.Integerdisplay -> ("Integerdisplay")
  | Edif2.Integer -> ("Integer")
  | Edif2.Instanceref -> ("Instanceref")
  | Edif2.Instancenamedef -> ("Instancenamedef")
  | Edif2.Instancemap -> ("Instancemap")
  | Edif2.Instancegroup -> ("Instancegroup")
  | Edif2.Instancebackannotate -> ("Instancebackannotate")
  | Edif2.Instance -> ("Instance")
  | Edif2.Input -> ("Input")
  | Edif2.Inout -> ("Inout")
  | Edif2.Initial -> ("Initial")
  | Edif2.Increasing -> ("Increasing")
  | Edif2.Includefiguregroup -> ("Includefiguregroup")
  | Edif2.Ignore -> ("Ignore")
  | Edif2.If -> ("If")
  | Edif2.ITEM2 arg  -> ("ITEM2")
  | Edif2.ITEM arg  -> ("ITEM")
  | Edif2.INT arg  -> ("INT")
  | Edif2.ILLEGAL arg  -> ("ILLEGAL")
  | Edif2.IDSTR arg  -> ("IDSTR")
  | Edif2.ID arg  -> ("ID")
  | Edif2.HASH_ -> ("HASH_")
  | Edif2.Gridmap -> ("Gridmap")
  | Edif2.Greaterthan -> ("Greaterthan")
  | Edif2.Globalportref -> ("Globalportref")
  | Edif2.Form -> ("Form")
  | Edif2.Forbiddenevent -> ("Forbiddenevent")
  | Edif2.Follow -> ("Follow")
  | Edif2.Floor -> ("Floor")
  | Edif2.Fix -> ("Fix")
  | Edif2.Fillpattern -> ("Fillpattern")
  | Edif2.Figurewidth -> ("Figurewidth")
  | Edif2.Figureperimeter -> ("Figureperimeter")
  | Edif2.Figuregroupref -> ("Figuregroupref")
  | Edif2.Figuregroupoverride -> ("Figuregroupoverride")
  | Edif2.Figuregroupobject -> ("Figuregroupobject")
  | Edif2.Figuregroup -> ("Figuregroup")
  | Edif2.Figurearea -> ("Figurearea")
  | Edif2.Figure -> ("Figure")
  | Edif2.False -> ("False")
  | Edif2.Fabricate -> ("Fabricate")
  | Edif2.FLT arg  -> ("FLT")
  | Edif2.External -> ("External")
  | Edif2.Exactly -> ("Exactly")
  | Edif2.Event -> ("Event")
  | Edif2.Escape -> ("Escape")
  | Edif2.Equal -> ("Equal")
  | Edif2.Entry -> ("Entry")
  | Edif2.Endtype -> ("Endtype")
  | Edif2.Endmodule -> ("Endmodule")
  | Edif2.Endcase -> ("Endcase")
  | Edif2.End -> ("End")
  | Edif2.Enclosuredistance -> ("Enclosuredistance")
  | Edif2.Else -> ("Else")
  | Edif2.Edifversion -> ("Edifversion")
  | Edif2.EdifLevel -> ("EdifLevel")
  | Edif2.Edif -> ("Edif")
  | Edif2.EQUALS -> ("EQUALS")
  | Edif2.EOL -> ("EOL")
  | Edif2.ENDPRAGMA -> ("ENDPRAGMA")
  | Edif2.ENDOFFILE -> ("ENDOFFILE")
  | Edif2.EMPTYEDIF -> ("EMPTYEDIF")
  | Edif2.E -> ("E")
  | Edif2.Duration -> ("Duration")
  | Edif2.Dot -> ("Dot")
  | Edif2.Dominates -> ("Dominates")
  | Edif2.Divide -> ("Divide")
  | Edif2.Display -> ("Display")
  | Edif2.Direction -> ("Direction")
  | Edif2.Difference -> ("Difference")
  | Edif2.Designator -> ("Designator")
  | Edif2.Design -> ("Design")
  | Edif2.Derivation -> ("Derivation")
  | Edif2.Delta -> ("Delta")
  | Edif2.Delay -> ("Delay")
  | Edif2.Default -> ("Default")
  | Edif2.Dcmaxfanout -> ("Dcmaxfanout")
  | Edif2.Dcmaxfanin -> ("Dcmaxfanin")
  | Edif2.Dcfanoutload -> ("Dcfanoutload")
  | Edif2.Dcfaninload -> ("Dcfaninload")
  | Edif2.Dataorigin -> ("Dataorigin")
  | Edif2.DOUBLE arg  -> ("DOUBLE")
  | Edif2.DOT -> ("DOT")
  | Edif2.DEFAULT -> ("DEFAULT")
  | Edif2.Cycle -> ("Cycle")
  | Edif2.Curve -> ("Curve")
  | Edif2.Currentmap -> ("Currentmap")
  | Edif2.Criticality -> ("Criticality")
  | Edif2.Cornertype -> ("Cornertype")
  | Edif2.Contents -> ("Contents")
  | Edif2.Constraint -> ("Constraint")
  | Edif2.Constant -> ("Constant")
  | Edif2.Connectlocation -> ("Connectlocation")
  | Edif2.Concat -> ("Concat")
  | Edif2.Compound -> ("Compound")
  | Edif2.Commentgraphics -> ("Commentgraphics")
  | Edif2.Comment -> ("Comment")
  | Edif2.Color -> ("Color")
  | Edif2.Circle -> ("Circle")
  | Edif2.Change -> ("Change")
  | Edif2.Celltype -> ("Celltype")
  | Edif2.Cellref -> ("Cellref")
  | Edif2.Cell -> ("Cell")
  | Edif2.Ceiling -> ("Ceiling")
  | Edif2.Case -> ("Case")
  | Edif2.COMMENT arg  -> ("COMMENT")
  | Edif2.COMMA -> ("COMMA")
  | Edif2.COLON -> ("COLON")
  | Edif2.Boundingbox -> ("Boundingbox")
  | Edif2.Borderwidth -> ("Borderwidth")
  | Edif2.Borderpattern -> ("Borderpattern")
  | Edif2.Booleanvalue -> ("Booleanvalue")
  | Edif2.Booleanmap -> ("Booleanmap")
  | Edif2.Booleandisplay -> ("Booleandisplay")
  | Edif2.Boolean -> ("Boolean")
  | Edif2.Block -> ("Block")
  | Edif2.Between -> ("Between")
  | Edif2.Begin -> ("Begin")
  | Edif2.Becomes -> ("Becomes")
  | Edif2.Basearray -> ("Basearray")
  | Edif2.BEGINPRAGMA -> ("BEGINPRAGMA")
  | Edif2.BECOMES -> ("BECOMES")
  | Edif2.Author -> ("Author")
  | Edif2.Atmost -> ("Atmost")
  | Edif2.Atleast -> ("Atleast")
  | Edif2.Assign -> ("Assign")
  | Edif2.Arraysite -> ("Arraysite")
  | Edif2.Arrayrelatedinfo -> ("Arrayrelatedinfo")
  | Edif2.Arraymacro -> ("Arraymacro")
  | Edif2.Array -> ("Array")
  | Edif2.Arc -> ("Arc")
  | Edif2.Apply -> ("Apply")
  | Edif2.Annotate -> ("Annotate")
  | Edif2.And -> ("And")
  | Edif2.Always -> ("Always")
  | Edif2.After -> ("After")
  | Edif2.Acload -> ("Acload")
  | Edif2.Abs -> ("Abs")
  | Edif2.AT -> ("AT")
  | Edif2.ASCNUM arg  -> ("ASCNUM")
  | Edif2.AMPERSAND -> ("AMPERSAND")
