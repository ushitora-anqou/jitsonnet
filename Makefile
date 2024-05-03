.PHONY: build
build:
	dune build
	dune exec bin/main.exe -- compile --target=stdjsonnet test/std.jsonnet > bundle/stdjsonnet.ml
	cd bundle && ocamlc -w -a -c common.ml
	cd bundle && ocamlc -w -a -c stdjsonnet.ml

.PHONY: test
test: build
	dune runtest -f
