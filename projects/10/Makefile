TARGET=jc
JCCMX=lexer.cmx parser.cmx jc.cmx
GENDOTCMX=lexer.cmx statemachine.cmx
CMX=${JCCMX} ${GENDOTCMX}

jc: $(JCCMX)
	ocamlopt -g $^ -o $@

gendot: GENDOTCMX
	ocamlopt -g $^ -o $@

sm2.pdf: sm2.dot
	dot -Tps2 sm2.dot -o sm2.ps | ps2pdf sm2.ps

%.cmx:%.ml
	ocamlopt -c $^

test: $(TARGET)
	./$< < test.jack

class: $(TARGET)
	./$< < class.jack

clean:
	rm -f $(TARGET) $(CMX) $(CMX:.cmx=.cmi) $(CMX:.cmx=.o) sm2.ps
