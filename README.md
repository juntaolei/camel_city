# Camel City

## TODO (MS2)

Target Line Count: 833 / 1000

- [x] Implement placing buildings.

- [ ] Add a start page to pick game setting or upload JSON saves.

  - [x] Add ability to pick game setting.

  - [ ] Add ability to upload JSON saves.

- [x] Import buildings from JSON.

- [?] Implement save to JSON.

  - [x] Add ability to read from JSON file.

  - [x] Create state from JSON.

  - [x] Add ability to convert state to JSON.

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

## Running the Game in the Browser

Build the project as instructed above.

Open _build/default/bin/index.html in a browser.

Note: The GUI is primarily tested with Firefox.
