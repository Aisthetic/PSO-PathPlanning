# Makefile
build:
	ocamlbuild -r -Is Modules_communs -use-ocamlfind -pkg graphics PSO_dynamique_particule_valide/initialisation.d.byte
clean:
	ocamlbuild -clean
.PHONY:	build clean
