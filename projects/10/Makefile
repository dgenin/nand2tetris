TARGET=jc
JCCMX=src/lexer.cmx src/parser.cmx src/jc.cmx
CMX=${JCCMX}
INC=-I src

jc: $(JCCMX)
	ocamlopt ${INC} -g $^ -o $@

%.cmx:%.ml
	ocamlopt ${INC} -c $^

test: $(TARGET)
	./$< test.jack

class: $(TARGET)
	./$< class.jack

clean:
	rm -f $(TARGET) $(CMX) $(CMX:.cmx=.cmi) $(CMX:.cmx=.o) sm.ps sm.dot
