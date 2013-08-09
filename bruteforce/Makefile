

all: main

main: main.native
	ln -sf $^ $@

main.native: main.ml brute.ml type.ml print.ml eval.ml feedback.ml guess.ml guess.mli brute.mli 
	ocamlbuild $@ -cflag -annot 
