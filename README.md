# JITsonnet

A JIT compiler for Jsonnet programming language.

## Usage (via Docker)

```
git clone https://github.com/ushitora-anqou/jitsonnet.git
cd jitsonnet
docker build . -t jitsonnet:dev
docker run -v $PWD:/pwd jitsonnet:dev run --haskell /pwd/path/to/file.jsonnet
```

## License

Each submodule in `thirdparty/` are distributed under its own licese.

The rest of the files are licensed under MIT License. See the LICENSE file for details.
