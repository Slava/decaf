lexer: lexer.ml
	ocamlc lexer.ml -o lexer

lexer.ml: lexer.mll
	ocamllex lexer.mll

tests: lexer
	./lexer < tests/test.decaf

clean:
	rm lexer.ml
	rm lexer.cmo
	rm lexer.cmi
	rm lexer
