.PHONY: build
build:
	dune build bin lib test thirdparty/ocaml-dtoa/src thirdparty/ocaml-rope/src
	cp thirdparty/uutf/src/uutf.ml bundle/
	cd bundle && ocamlc -w -a -c uutf.ml
	cd bundle && ocamlopt -w -a -c uutf.ml
	cp _build/default/thirdparty/ocaml-dtoa/src/.dtoa.objs/byte/dtoa* bundle/
	cp _build/default/thirdparty/ocaml-dtoa/src/dtoa.cmxa bundle/
	cp _build/default/thirdparty/ocaml-dtoa/src/dtoa.a bundle/
	cp _build/default/thirdparty/ocaml-dtoa/src/dlldtoa_stubs.so bundle/
	cp _build/default/thirdparty/ocaml-rope/src/rope.a bundle/
	cp _build/default/thirdparty/ocaml-rope/src/rope.cma bundle/
	cp _build/default/thirdparty/ocaml-rope/src/rope.cmxa bundle/
	cp _build/default/thirdparty/ocaml-rope/src/.rope.objs/byte/rope* bundle/
	dune exec bin/main.exe -- compile --target=stdjsonnet thirdparty/jsonnet/stdlib/std.jsonnet > bundle/stdjsonnet.ml
	cd bundle && ocamlc -w -a -c common.ml
	cd bundle && ocamlopt -w -a -c common.ml
	cd bundle && ocamlc -w -a -c stdjsonnet.ml
	cd bundle && ocamlopt -w -a -c stdjsonnet.ml

.PHONY: test
test: build
	dune runtest -f test
