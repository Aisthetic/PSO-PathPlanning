# Makefile
build:
	ocamlbuild -use-ocamlfind gui.d.byte
clean:
	ocamlbuild -clean
.PHONY:	build clean