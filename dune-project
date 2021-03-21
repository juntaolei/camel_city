(lang dune 2.8)

(name camel_city)

(version 0.1)

(implicit_transitive_deps true)

(wrapped_executables true)

(package
 (name camel_city)
 (synopsis "A city building simulator.")
 (description "A city building simulator for camels.")
 (depends js_of_ocaml js_of_ocaml-compiler js_of_ocaml-ppx yojson ounit))
