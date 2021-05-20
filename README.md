# Camel City

## TODO: Features

* [x] Beautify GUI
* [ ] Finalizing GUI inputs and displays
* [x] Random Events (e.g. Random Attacks, Nature Disasters, etc...)
* [ ] Add Happiness
* [ ] Refine Building Attributes
* [ ] Update Documentation
* [ ] Add More Test Cases
* [ ] Add Game Pausing
* [ ] Add Endgame Building
* [ ] Add Gameover States

## Instruction

This assumes that the user is using the same OCaml switch as cs3110-2021sp.

Install the required dependencies:

```bash
opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx
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

## Running the Game in the Browser

Build the project as instructed above.

Open _build/default/bin/index.html in a browser.

Note: The GUI is primarily tested with Firefox.
