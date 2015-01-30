all: test.native

test.native: sources
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native

sources: $(shell find . -maxdepth 1 -name "*.ml" -o -name "*.mll" -o -name "*.mly")

test: test.native
	./test.native test.decaf

clean:
	rm *.native
	rm -rf _build
