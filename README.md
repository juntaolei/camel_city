# Camel City

## Instruction

This assumes that the user is using the same OCaml switch as cs3110-2021sp.

Install the required dependencies:

```bash
opam install -y dune js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx
```
  
Compiling to JavaScript:

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

## TODO (MS2)

- [ ] Implement placing buildings.

- [ ] Add a start page to pick game setting or upload JSON saves.

- [ ] Import buildings from JSON.

- [ ] Implement save to JSON.

  - [ ] Add ability to read from JSON file.

  - [ ] Create state from JSON.

  - [ ] Add ability to convert state to JSON.

## Running the Game in the Browser

Build the project as instructed above.

Open _build/default/bin/index.html in a browser.

Note: The GUI is primarily tested with Firefox.
