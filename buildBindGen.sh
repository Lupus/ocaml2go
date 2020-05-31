#!/bin/bash
set -e

export OCAMLRUNPARAM=b
export DUNE_CACHE=disabled

dune build bin/main.exe
export OCAML2GO="$PWD/_build/default/bin/main.exe"

mkdir -p ./gotest/output

echo "building bindgen/bindgen.bc"
dune build bindgen/bindgen.bc
echo "building ./gotest/output/bindgen.go"
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
	_build/default/bindgen/bindgen.bc \
	-o ./gotest/output/bindgen.go
echo "go build -gcflags="-e" -o bindgen.exe ./gotest/output/bindgen.go"
go build -gcflags="-e" -o bindgen.exe ./gotest/output/bindgen.go

