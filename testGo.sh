#!/bin/bash
set -e

export OCAMLRUNPARAM=b
export DUNE_CACHE=disabled

dune build bin/main.exe
export OCAML2GO="$PWD/_build/default/bin/main.exe"

for test in hello_world loop_labels ocaml_lib_tests btree huffman nbody strings polymorphism mutual_references hindley_milner_lang
do
  echo
  echo "************  $test  ************"
  echo
  echo "building ./test/$test/$test.bc"
  dune build --profile=release test/$test/$test.bc
  echo "building ./test/output/$test.go"
  js_of_ocaml \
	--enable excwrap \
	--enable wrapped-exceptions  \
	--noinline \
	--disable shortvar \
	--disable simplify_ifdecl \
	--pretty \
        --debuginfo \
	--backend exec \
	--backend-flags "$OCAML2GO" \
	_build/default/test/$test/$test.bc \
	-o ./test/output/$test.go
   echo "go fmt ./test/output/$test.go"
   go fmt ./test/output/$test.go
   echo "go build -gcflags="-e" ./test/output/$test.go"
   go build -gcflags="-e" ./test/output/$test.go
   dune exec test/$test/$test.exe > .native.output
   ./$test > .go.output
   echo "checking outputs..."
   diff -u .native.output .go.output
   echo OK
done

