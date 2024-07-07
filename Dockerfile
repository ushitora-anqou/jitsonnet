FROM ocaml/opam:debian-10-ocaml-5.2

ENV DEBIAN_FRONTEND=noninteractive

#USER root
#RUN apt-get update && apt-get install -y \
#    && rm -rf /var/lib/apt/lists/*

USER opam
WORKDIR /home/opam/jitsonnet
RUN opam-2.1 update && opam-2.1 install alcotest
COPY --chown=opam jitsonnet.opam .
RUN opam-2.1 install . --deps-only
COPY --chown=opam . .
RUN eval $(opam-2.1 env) && opam-2.1 install . --deps-only && dune build

FROM haskell:9-buster

#RUN apt-get update && apt-get install -y \
#    && rm -rf /var/lib/apt/lists/*

RUN cabal update && cabal install --lib vector text unordered-containers bytestring double-conversion deque filepath directory && rm -rf /root/.cache/cabal

WORKDIR /jitsonnet
COPY --from=0 /home/opam/jitsonnet/_build/default/bin/main.exe ./jitsonnet
COPY --from=0 /home/opam/jitsonnet/runtime_hs ./runtime_hs
RUN ghc -iruntime_hs -O2 -c runtime_hs/Common.hs
RUN ghc -iruntime_hs -O2 -c runtime_hs/Stdjsonnet.hs

ENV JITSONNET_RUNTIME_HS=/jitsonnet/runtime_hs
ENTRYPOINT ["/jitsonnet/jitsonnet"]
