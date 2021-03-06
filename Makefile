FLAGS=-use-menhir -tag thread -use-ocamlfind -quiet -pkg core -menhir 'menhir --explain'

all: parser_program lexer_program analysis_program

analysis_program: analysis_program.native
analysis_program.native: sources
	ocamlbuild $(FLAGS) analysis_program.native

parser_program: parser_program.native
parser_program.byte: sources
	ocamlbuild $(FLAGS) parser_program.byte
parser_program.native: sources
	ocamlbuild $(FLAGS) parser_program.native

lexer_program: lexer_program.native
lexer_program.native: sources
	ocamlbuild $(FLAGS) lexer_program.native

sources: $(shell find . -maxdepth 1 -name "*.ml" -o -name "*.mll" -o -name "*.mly")

test: tests
tests: all
	./run-tests.sh

clean:
	rm *.native
	rm -rf _build
