.PHONY: build
build: runtime_hs/Common.o
	dune build

.PHONY: update-stdjsonnet
update-stdjsonnet:
	dune exec bin/main.exe -- compile --target=stdjsonnet thirdparty/jsonnet/stdlib/std.jsonnet > lib_runtime/stdjsonnet.ml

.PHONY: test
test: build
	OUNIT_CI=true dune runtest -f

.PHONY: env
env:
	@echo "JITSONNET_RUNTIME_HS=$(PWD)/runtime_hs JITSONNET_OPAM_LIB=../_opam/lib JITSONNET_LIB_RUNTIME=_build/default/lib_runtime"

.PHONY: update-stdjsonnet-hs
update-stdjsonnet-hs:
	dune exec bin/main.exe -- compile --haskell --target=stdjsonnet thirdparty/jsonnet/stdlib/std.jsonnet > runtime_hs/Stdjsonnet.hs
	fourmolu -o '-XGHC2021' -i --column-limit=120 --indentation=1 runtime_hs/Stdjsonnet.hs
	ghc -iruntime_hs -O2 -c runtime_hs/Common.hs
	ghc -iruntime_hs -O2 -c runtime_hs/Stdjsonnet.hs

runtime_hs/Common.o: runtime_hs/Common.hs
	ghc -iruntime_hs -O2 -c runtime_hs/Common.hs

.PHONY: fmt-hs
fmt-hs:
	fourmolu -o '-XGHC2021' --column-limit=90 --indentation=2 -i runtime_hs/Common.hs
