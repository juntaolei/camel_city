build:
	dune build --profile dev

build-release:
	dune build

install:
	opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx js_of_ocaml-lwt ounit yojson

clean:
	dune clean

test:
	dune runtest

zip:
	zip -r camel_city.zip bin lib test .ocamlformat dune-project Makefile README.md