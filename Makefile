.PHONY: build
build:
	dune build
	dune exec bin/main.exe -- compile --target=stdjsonnet thirdparty/jsonnet/stdlib/std.jsonnet > bundle/stdjsonnet.ml
	cd bundle && ocamlc -w -a -c common.ml
	cd bundle && ocamlc -w -a -c stdjsonnet.ml
	cp thirdparty/uutf/src/uutf.ml bundle/
	cd bundle && ocamlc -w -a -c uutf.ml
	cp _build/default/thirdparty/ocaml-dtoa/src/.dtoa.objs/byte/dtoa* bundle/
	cp _build/default/thirdparty/ocaml-dtoa/src/dlldtoa_stubs.so bundle/

.PHONY: test
test: build
	dune runtest -f
