# JITsonnet

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

### `test/std.jsonnet`

from https://github.com/google/jsonnet

> Copyright 2015 Google Inc. All rights reserved.
> 
> Licensed under the Apache License, Version 2.0 (the "License");
> you may not use this file except in compliance with the License.
> You may obtain a copy of the License at
> 
>     http://www.apache.org/licenses/LICENSE-2.0
> 
> Unless required by applicable law or agreed to in writing, software
> distributed under the License is distributed on an "AS IS" BASIS,
> WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
> See the License for the specific language governing permissions and
> limitations under the License.
