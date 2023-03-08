default: native

clean:
	ocamlbuild -clean

native:
	ocamlbuild -use-menhir prime.native

byte:
	ocamlbuild -use-menhir prime.byte

byte-debug:
	ocamlbuild -use-menhir prime.byte -tag 'debug'
