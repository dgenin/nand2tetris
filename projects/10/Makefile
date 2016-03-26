TARGET=JackTokenizer
OBJ=lexer.cmx JackTokenizer.cmx

JackTokenizer: $(OBJ)
	ocamlopt -g $^ -o $@

%.cmx:%.ml
	ocamlopt -c $^

test: $(TARGET)
	./$< < test.jack

class: $(TARGET)
	./$< < class.jack

clean:
	rm -f $(TARGET) $(OBJ)
