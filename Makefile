lexer: lexer.ml
	ocamlc lexer.ml -o lexer

lexer.ml:
	ocamllex lexer.mll

clean:
	rm lexer.ml
	rm lexer.cmo
	rm lexer.cmi
	rm lexer
