TARGET=jc
CMX=lexer.cmx parser.cmx jc.cmx

jc: $(CMX)
	ocamlopt -g $^ -o $@

gendot: lexer.cmx statemachine.cmx
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
	rm -f $(TARGET) $(CMX) $(CMX:.cmx=.cmi) $(CMX:.cmx=.o)
