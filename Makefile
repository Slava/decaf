all: parser_program lexer_program

parser_program: parser_program.native
parser_program.native: sources
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core parser_program.native

lexer_program: lexer_program.native
lexer_program.native: sources
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core lexer_program.native

sources: $(shell find . -maxdepth 1 -name "*.ml" -o -name "*.mll" -o -name "*.mly")

test: tests
tests: parser_program
	./parser_program.native test.decaf

lexer_tests: lexer_program
	./lexer_program.native test.decaf

clean:
	rm *.native
	rm -rf _build
