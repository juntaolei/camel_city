# Camel City

## Instruction

This assumes that the user is using the same OCaml switch as cs3110-2021sp.

Install the required dependencies:

```bash
make install
```

*in case make does not work, use the following to install all required dependencies:

```bash
opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx odoc ounit yojson
```

Compiling to JavaScript:

For production:

```bash
make
```

or

```bash
make build-release
```

For development:

```bash
make build
```

Run tests:

```bash
make test
```

Create a zip of the source code:

```bash
make zip
```

Create documentation

```bash
make docs
```

## Running the Game in the Browser

Build the project as instructed above.

Open ./_build/default/bin/index.html in a browser.

Note: The GUI is primarily tested with Firefox.

## Reading the Documentation

Open ./_build/default/_doc/_html/index.html in a browser.

## Acknowledgement and Credit

The textures in this project embed Font Awesome icons. [Link to Font Awesome License](https://fontawesome.com/license)
