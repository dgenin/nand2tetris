TARGET=jc
OBJ=lexer.cmx parser.cmx jc.cmx

jc: $(OBJ)
	ocamlopt -g $^ -o $@

%.cmx:%.ml
	ocamlopt -c $^

test: $(TARGET)
	./$< < test.jack

class: $(TARGET)
	./$< < class.jack

clean:
	rm -f $(TARGET) $(OBJ)
