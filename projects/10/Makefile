TARGET=jc gendot
JCCMX=src/lexer.cmx src/parser.cmx src/jc.cmx
GENDOTCMX=src/lexer.cmx src/statemachine.cmx
CMX=${JCCMX} ${GENDOTCMX}
INC=-I src

jc: $(JCCMX)
	ocamlopt ${INC} -g $^ -o $@

gendot: $(GENDOTCMX)
	ocamlopt ${INC} -g $^ -o $@

sm.ps:sm.dot
	dot -Tps2 $^ -o $@

%.dot: gendot
	./gendot > $@

%.cmx:%.ml
	ocamlopt ${INC} -c $^

test: $(TARGET)
	./$< test.jack

class: $(TARGET)
	./$< class.jack

clean:
	rm -f $(TARGET) $(CMX) $(CMX:.cmx=.cmi) $(CMX:.cmx=.o) sm.ps sm.dot
