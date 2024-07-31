## How to setup cpp jsonnet tests

```
cd thirdparty/jsonnet/test_suite
ls *.jsonnet | grep -v '^error' | while read line; do if [ -f "${line}.golden" ]; then echo $line; fi; done | sed -r 's/^(.*)\.jsonnet$/testcase0 "\1" `Success;/'
ls *.jsonnet | grep -v '^error' | while read line; do if [ ! -f "${line}.golden" ]; then echo $line; fi; done | sed -r 's/^(.*)\.jsonnet$/testcase0 "\1" `SuccessSimple;/'
ls *.jsonnet | grep '^error' | while read line; do if [ -f "${line}.golden" ]; then echo $line; fi; done | sed -r 's/^(.*)\.jsonnet$/testcase0 "\1" `Error;/'
ls *.jsonnet | grep '^error' | while read line; do if [ ! -f "${line}.golden" ]; then echo $line; fi; done | sed -r 's/^(.*)\.jsonnet$/testcase0 "\1" `ErrorSimple;/'
```

## How to install necessary Haskell's libraries

```
cabal install --lib vector text unordered-containers bytestring double-conversion deque filepath directory utf8-string aeson scientific yaml crypton
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
let oc = open_out "test/std_ast.ml" in (Jitsonnet.Parser.parse_file "thirdparty/jsonnet/stdlib/std.jsonnet" |> Result.get_ok).expr |> Jitsonnet.Syntax.show_expr |> Printf.fprintf oc "open Jitsonnet\n\nlet expected =\n%s\n"; close_out oc;;

ocamlformat -i test/std_ast.ml
```
