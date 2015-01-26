all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native

lexer: lexer.ml
	ocamlc lexer.ml -o lexer

lexer.ml: lexer.mll
	ocamllex lexer.mll

tests: lexer tests_files
	./lexer < tests/test.decaf

tests_files: $(shell find tests -type f)

clean:
	rm lexer.ml
	rm lexer.cmo
	rm lexer.cmi
	rm lexer
