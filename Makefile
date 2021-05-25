all:
	make build-release

build:
	dune build --profile dev

build-release:
	dune build

clean:
	dune clean

docs:
	dune build @doc

install:
	opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx odoc ounit yojson

linecount:
	cloc --by-file --include-lang=OCaml .

test:
	make build-release && dune runtest

zip:
	zip -r camel_city.zip bin lib tests .gitignore .ocamlformat dune-project Makefile README.md
