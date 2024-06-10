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

.PHONY: update-stdjsonnet-hs
update-stdjsonnet-hs:
	dune exec bin/main.exe -- compile --haskell --target=stdjsonnet thirdparty/jsonnet/stdlib/std.jsonnet > runtime_hs/Stdjsonnet.hs
	fourmolu -i --column-limit=120 --indentation=1 runtime_hs/Stdjsonnet.hs
