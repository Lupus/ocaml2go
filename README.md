# OCaml2Go

OCaml2Go is a transpiler from OCaml bytecode to Golang. Although it can compile
considerably complex OCaml code, project is in early stages of development, I
would say proof of concept so far.

## Known limitations

* resulting code is very slow (OCaml works with dynamic value representation, a
  lot of `interface{}` to get it working in Go)
* large monolithic output takes long time to compile
* no OCaml or Go build system integration

## How it works

OCaml2Go is build on top of [Rehp](https://github.com/jordwalke/rehp), a fork
of js_of_ocaml that supports pluggable backends.

Rehp produces intermediate representation, not specific to JavaScript backend,
OCaml2Go takes it from there and produces Golang code.

## Building and running tests

OCaml2Go depends on some pending PRs to Rehp, so to build it you need to pin
the following branch for now:

```
opam pin js_of_ocaml-compiler git+https://github.com/Lupus/rehp.git#ocaml2go
opam pin js_of_ocaml git+https://github.com/Lupus/rehp.git#ocaml2go
```

After that tests should be runnable with this script (from opam switch with all
dependencies installed):

```
./testGo.sh
```

