# Alternative Makefile for manual building
OCAMLC = ocamlc
OCAMLOPT = ocamlopt -g
OCAMLMKTOP = ocamlmktop -g
MENHIR = menhir
OCAMLLEX = ocamllex

SOURCES = sv_ast.mli sv_parse.ml sv_gen.ml sv_main.ml sv_args.ml
SOURCES_YOSYS = sv_ast.mli sv_parse.ml sv_gen_yosys.ml sv_main_yosys.ml
SOURCES_STRUCT = sv_ast.mli sv_parse.ml sv_transform.ml sv_tran_struct.ml sv_gen.ml sv_main_struct.ml
SOURCES_OPT = sv_ast.mli sv_parse.ml sv_transform.ml sv_opt_ir.ml behavioural_to_opt_ir.ml opt_ir_to_sv.ml sv_gen.ml sv_main_opt.ml
SOURCES_SAT = sv_ast.mli sv_parse.ml sv_transform.ml sv_opt_ir.ml behavioural_to_opt_ir.ml opt_ir_to_sv.ml sv_to_z3.ml sv_main_sat.ml

TARGET = json_verilog
TARGET_TOP = json_verilog_top
TARGET_YOSYS_TOP = json_yosys_top
TARGET_STRUCT_TOP = json_struct_top
TARGET_OPT_TOP = json_opt_top
TARGET_SAT_TOP = json_sat_top

.PHONY: all clean debug

all: $(TARGET) $(TARGET_TOP) $(TARGET_YOSYS_TOP) $(TARGET_STRUCT_TOP) $(TARGET_OPT_TOP) $(TARGET_SAT_TOP)

json_verilog.ml json_verilog.mli: json_verilog.mly json_types.cmi
	$(MENHIR) --explain --dump --infer $<

json_verilog_lexer.ml: json_verilog_lexer.mll json_verilog.mli
	$(OCAMLLEX) $<

json_types.cmi: json_types.mli
	$(OCAMLC) -c $<

$(TARGET_TOP): $(SOURCES)
	ocamlfind $(OCAMLMKTOP) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

$(TARGET): $(SOURCES)
	ocamlfind $(OCAMLOPT) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

$(TARGET_YOSYS_TOP): $(SOURCES_YOSYS)
	ocamlfind $(OCAMLMKTOP) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

$(TARGET_STRUCT_TOP): $(SOURCES_STRUCT)
	ocamlfind $(OCAMLMKTOP) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

$(TARGET_OPT_TOP): $(SOURCES_OPT)
	ocamlfind $(OCAMLMKTOP) -package yojson,str,unix -linkpkg -I +unix -o $@ $^

$(TARGET_SAT_TOP): $(SOURCES_SAT)
	ocamlfind $(OCAMLMKTOP) -package yojson,str,unix,z3 -linkpkg -I +unix -o $@ $^

clean:
	rm -f *.cmi *.cmx *.cmo *.o $(TARGET)
	rm -f json_verilog.ml json_verilog.mli json_verilog_lexer.ml
	rm -f json_verilog.automaton json_verilog.conflicts

install: $(TARGET)
	cp $(TARGET) /usr/local/bin/

test: $(TARGET)
	rm -rf results
	./$(TARGET) -da test.json test.v

.INTERMEDIATE: json_verilog.ml json_verilog.mli json_verilog_lexer.ml
