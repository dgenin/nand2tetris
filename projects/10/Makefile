TARGET=jc
CMX=lexer.cmx parser.cmx jc.cmx

jc: $(CMX)
	ocamlopt -g $^ -o $@

gendot: lexer.cmx statemachine.cmx
	ocamlopt -g $^ -o $@

%.cmx:%.ml
	ocamlopt -c $^

test: $(TARGET)
	./$< < test.jack

class: $(TARGET)
	./$< < class.jack

clean:
	rm -f $(TARGET) $(CMX) $(CMX:.cmx=.cmi) $(CMX:.cmx=.o)
