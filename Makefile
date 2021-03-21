build:
	dune build --profile dev

build-release:
	dune build

clean:
	dune clean

test:
	dune runtest