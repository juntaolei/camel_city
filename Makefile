build:
	dune build --profile dev

build-release:
	dune build

clean:
	dune clean

install:
	opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx js_of_ocaml-lwt ounit yojson

linecount:
	cloc --by-file --include-lang=OCaml .

runtest:
	make build && dune runtest

zip:
	zip -r camel_city.zip bin lib test .ocamlformat dune-project Makefile README.md
