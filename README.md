# JITsonnet

A JIT compiler for Jsonnet programming language.

## How to install necessary Haskell's libraries

```
cabal install --lib vector text unordered-containers bytestring double-conversion deque filepath directory
```

## How to run native mode with mold linker

```
time dune exec bin/main.exe -- run --profile --native --mold ~/workspace/mold-2.31.0-x86_64-linux/bin/mold thirdparty/go-jsonnet/testdata/builtin_stripChars.jsonnet bundle
```

## How to run with profiling and a custom working directory

```
dune exec bin/main.exe -- run --profile --work-dir /tmp misc/hoge.jsonnet bundle
```

## How to list `test_compiler_with_go_jsonnet_testdata` test cases

```
grep RUNTIME go-jsonnet/testdata/*.golden | awk -F: '{print $1}' | awk -F/ '{print $3}' >> bad
grep Unknown go-jsonnet/testdata/*.golden | awk -F: '{print $1}' | awk -F/ '{print $3}' >> bad
cat bad | sed -r 's/.golden$/.jsonnet/' | sponge bad
ls go-jsonnet/testdata | grep -v golden | grep -v linter | grep -v bad | grep -v error | while read line; do grep --quiet "$line" bad || echo $line; done | grep -E '.jsonnet$' | sed -r 's/^(.*).jsonnet$/assert_compile "\1" `Success;/'
```


## How to generate `bundle/stdjsonnet.cma`

```
dune exec bin/main.exe -- compile --target=stdjsonnet test/std.jsonnet > bundle/stdjsonnet.ml
ocamlc -a -o bundle/stdjsonnet.cma bundle/stdjsonnet.ml
```

## How to update `test/std_ast.ml`

```
let oc = open_out "test/std_ast.ml" in (Jitsonnet.Parser.parse_file "test/std.jsonnet" |> Result.get_ok).expr |> Jitsonnet.Syntax.show_expr |> Printf.fprintf oc "open Jitsonnet\n\nlet expected =\n%s\n"; close_out oc;;

ocamlformat -i test/std_ast.ml
```

## License

Each submodule in `thirdparty/` are distributed under its own licese.

The rest of the files are licensed under MIT License. See the LICENSE file for details.
