JackTokenizer: JackTokenizer.ml
	ocamlopt -g JackTokenizer.ml -o $@

test: JackTokenizer
	./$< < test.jack

class: JackTokenizer
	./$< < class.jack
