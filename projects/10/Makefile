TARGET=jc gendot
JCCMX=lexer.cmx parser.cmx jc.cmx
GENDOTCMX=lexer.cmx statemachine.cmx
CMX=${JCCMX} ${GENDOTCMX}

jc: $(JCCMX)
	ocamlopt -g $^ -o $@

gendot: $(GENDOTCMX)
	ocamlopt -g $^ -o $@

sm.ps:sm.dot
	dot -Tps2 $^ -o $@

%.dot: gendot
	./gendot > $@

%.cmx:%.ml
	ocamlopt -c $^

test: $(TARGET)
	./$< < test.jack

class: $(TARGET)
	./$< < class.jack

clean:
	rm -f $(TARGET) $(CMX) $(CMX:.cmx=.cmi) $(CMX:.cmx=.o) sm.ps sm.dot
