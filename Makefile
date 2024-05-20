.PHONY: build
build:
	dune build

.PHONY: update-stdjsonnet
update-stdjsonnet:
	dune exec bin/main.exe -- compile --target=stdjsonnet thirdparty/jsonnet/stdlib/std.jsonnet > lib_runtime/stdjsonnet.ml

.PHONY: test
test: build
	dune runtest -f test

.PHONY: env
env:
	@echo "JITSONNET_OPAM_LIB=../_opam/lib JITSONNET_LIB_RUNTIME=_build/default/lib_runtime"
