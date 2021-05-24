# Camel City

## TODO: Features

* [x] Beautify GUI
* [ ] Finalizing GUI inputs and displays
* [x] Random Events (e.g. Random Attacks, Nature Disasters, etc...)
* [x] Add Happiness
* [ ] Refine Building Attributes
* [ ] Update Documentation
* [ ] Add More Test Cases
* [x] Add Game Pausing
* [ ] Add Endgame Building
* [x] Add Gameover States

## Instruction

This assumes that the user is using the same OCaml switch as cs3110-2021sp.

Install the required dependencies:

```bash
make install
```

*in case make does not work, use the following to install all required dependencies:

```bash
opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx js_of_ocaml-lwt odoc ounit yojson
```

Compiling to JavaScript:

For production:

```bash
make
```

For development:

```bash
make build
```

Run tests:

```bash
make runtest
```

Create a zip of the source code:

```bash
make zip
```

Create documentation

```bash
make doc
```

## Running the Game in the Browser

Build the project as instructed above.

Open ./_build/default/bin/index.html in a browser.

Note: The GUI is primarily tested with Firefox.

## Reading the Documentation

Open ./_build/default/_doc/_html/index.html in a browser.
