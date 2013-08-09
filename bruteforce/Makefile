

all: main

main: main.byte
	ln -sf $^ $@

main.byte:
	ocamlbuild $@ -cflag -annot 
