all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test.native

test:
	./test.native test.decaf

clean:
	rm *.native
	rm -rf _build
