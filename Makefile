lexer: lexer.ml
	ocamlc lexer.ml -o lexer

lexer.ml: lexer.mll
	ocamllex lexer.mll

clean:
	rm lexer.ml
	rm lexer.cmo
	rm lexer.cmi
	rm lexer
