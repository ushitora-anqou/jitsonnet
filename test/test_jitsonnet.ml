open Jitsonnet

let read_all file_path =
  let ic = open_in_bin file_path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> In_channel.input_all ic)

let assert_token expected got_src =
  let got = L.main (Lexing.from_string got_src) in
  Logs.info (fun m ->
      m "got %s, expected %s"
        (Parser.string_of_token got)
        (Parser.string_of_token expected));
  assert (got = expected);
  ()

let test_lexer_keyword () =
  assert_token ASSERT "assert";
  assert_token ELSE "else";
  assert_token ERROR "error";
  assert_token FALSE "false";
  assert_token FOR "for";
  assert_token FUNCTION "function";
  assert_token IF "if";
  assert_token IMPORT "import";
  assert_token IMPORTSTR "importstr";
  assert_token IMPORTBIN "importbin";
  assert_token IN "in";
  assert_token LOCAL "local";
  assert_token NULL "null";
  assert_token TAILSTRICT "tailstrict";
  assert_token THEN "then";
  assert_token SELF "self";
  assert_token SUPER "super";
  assert_token TRUE "true";
  ()

let test_lexer_number () =
  assert_token (NUMBER 0.0) "0";
  assert_token (NUMBER 1.0) "1";
  assert_token (NUMBER 0.0) "0.0";
  assert_token (NUMBER 1.0) "1.0";
  assert_token (NUMBER 1.0) "1e0";
  assert_token (NUMBER 10.0) "1e1";
  assert_token (NUMBER 10.0) "1e+1";
  assert_token (NUMBER 0.1) "1e-1";
  ()

let test_lexer_string () =
  assert_token (STRING "") {|""|};
  assert_token (STRING "abc") {|"abc"|};
  assert_token (STRING "ab\nc") {|"ab
c"|};
  assert_token (STRING "\"") {|"\""|};
  assert_token (STRING "\'") {|"\'"|};
  assert_token (STRING "\\") {|"\\"|};
  assert_token (STRING "/") {|"\/"|};
  assert_token (STRING "\b") {|"\b"|};
  assert_token (STRING "\x0c") {|"\f"|};
  assert_token (STRING "\n") {|"\n"|};
  assert_token (STRING "\r") {|"\r"|};
  assert_token (STRING "\t") {|"\t"|};
  assert_token (STRING "\u{30F9}") {|"\u30F9"|};

  assert_token (STRING "") {|''|};
  assert_token (STRING "abc") {|'abc'|};
  assert_token (STRING "ab\nc") {|'ab
c'|};
  assert_token (STRING "\"") {|'\"'|};
  assert_token (STRING "\'") {|'\''|};
  assert_token (STRING "\\") {|'\\'|};
  assert_token (STRING "/") {|'\/'|};
  assert_token (STRING "\b") {|'\b'|};
  assert_token (STRING "\x0c") {|'\f'|};
  assert_token (STRING "\n") {|'\n'|};
  assert_token (STRING "\r") {|'\r'|};
  assert_token (STRING "\t") {|'\t'|};
  assert_token (STRING "\u{30F9}") {|'\u30F9'|};

  assert_token (STRING "") {|@""|};
  assert_token (STRING "abc") {|@"abc"|};
  assert_token (STRING "ab\nc") {|@"ab
c"|};
  assert_token (STRING "\\") {|@"\"|};
  assert_token (STRING "\"") {|@""""|};
  assert_token (STRING "\'") {|"\'"|};
  assert_token (STRING "\\\\") {|@"\\"|};
  assert_token (STRING "\\/") {|@"\/"|};
  assert_token (STRING "\\b") {|@"\b"|};
  assert_token (STRING "\\f") {|@"\f"|};
  assert_token (STRING "\\n") {|@"\n"|};
  assert_token (STRING "\\r") {|@"\r"|};
  assert_token (STRING "\\t") {|@"\t"|};
  assert_token (STRING "\\u30F9") {|@"\u30F9"|};

  assert_token (STRING "") {|@''|};
  assert_token (STRING "abc") {|@'abc'|};
  assert_token (STRING "ab\nc") {|@'ab
c'|};
  assert_token (STRING "\\\"") {|@'\"'|};
  assert_token (STRING "\\") {|@'\'|};
  assert_token (STRING "'") {|@''''|};
  assert_token (STRING "\\\\") {|@'\\'|};
  assert_token (STRING "\\/") {|@'\/'|};
  assert_token (STRING "\\b") {|@'\b'|};
  assert_token (STRING "\\f") {|@'\f'|};
  assert_token (STRING "\\n") {|@'\n'|};
  assert_token (STRING "\\r") {|@'\r'|};
  assert_token (STRING "\\t") {|@'\t'|};
  assert_token (STRING "\\u30F9") {|@'\u30F9'|};

  assert_token (STRING "\nabc\n\ndef\n  gh\n\n|||\n |||\n")
    {|

 |||

  abc

  def
    gh

  |||
   |||
 |||

|};

  ()

let assert_expr expected got =
  match Parser.parse_string got with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      Logs.info (fun m -> m "expected %s" (Syntax.show_expr expected));
      assert false
  | Ok { expr = got } ->
      Logs.info (fun m ->
          m "got %s, expected %s" (Syntax.show_expr got)
            (Syntax.show_expr expected));
      assert (got = expected);
      ()

let test_parse_atoms () =
  assert_expr Null "null";
  assert_expr True "true";
  assert_expr False "false";
  assert_expr Self "self";
  assert_expr Dollar "$";
  assert_expr (String "abc") "\"abc\"";
  assert_expr (Number 1.0) "1";
  ()

let test_parse_object () =
  assert_expr (Object (ObjectMemberList [])) "{}";
  assert_expr
    (Object
       (ObjectMemberList
          [ MemberField (Field (FieldnameID "x", false, H 1, Number 1.0)) ]))
    "{x: 1}";
  assert_expr
    (Object
       (ObjectMemberList
          [
            MemberObjlocal (Bind ("a", String "b"));
            MemberField
              (Field (FieldnameExpr (String "foo"), false, H 1, String "bar"));
          ]))
    {|
{
  local a = "b",
  ["foo"]: "bar",
}|};
  assert_expr
    (Object
       (ObjectMemberList
          [
            MemberObjlocal (Bind ("a", String "b"));
            MemberField
              (Field (FieldnameExpr (String "foo"), true, H 1, String "bar"));
          ]))
    {|
{
  local a = "b",
  ["foo"]+: "bar",
}|};
  assert_expr
    (Object
       (ObjectMemberList
          [
            MemberObjlocal (Bind ("a", String "b"));
            MemberField
              (Field (FieldnameExpr (String "foo"), false, H 1, String "bar"));
            MemberField (Field (FieldnameID "x", false, H 1, Number 1.0));
          ]))
    {|
{
  local a = "b",
  ["foo"]: "bar",
  x: 1.0,
}|};
  assert_expr
    (Object
       (ObjectMemberList
          [
            MemberField
              (Field (FieldnameExpr (String "foo"), true, H 1, String "bar"));
            MemberField (Field (FieldnameID "x", true, H 1, Number 1.0));
            MemberObjlocal (Bind ("a", String "b"));
            MemberField (FieldFunc (FieldnameID "y", [], H 2, String "a"));
            MemberAssert (True, None);
            MemberField
              (FieldFunc
                 ( FieldnameID "z",
                   [ ("v1", None); ("v2", Some (String "s")) ],
                   H 2,
                   String "s" ));
            MemberAssert (True, Some (String "s"));
            MemberField (Field (FieldnameString "w", false, H 3, Number 1.0));
          ]))
    {|
{
  ["foo"]+: "bar",
  x+: 1,
  local a = "b",
  y():: "a",
  assert true,
  z(v1, v2="s")::"s",
  assert true : "s",
  "w":::1.0,
}|};

  assert_expr
    (Object (ObjectFor ([], Var "x", String "y", [], ("x", Var "a"), [])))
    {|{[x]: "y" for x in a}|};
  assert_expr
    (Object
       (ObjectFor
          ( [ Bind ("v1", String "s") ],
            Var "x",
            String "y",
            [ Bind ("v2", String "s") ],
            ("x", Var "a"),
            [ Forspec ("y", Var "a"); Ifspec True ] )))
    {|
{
  local v1 = "s",
  [x]: "y",
  local v2 = "s",
  for x in a
  for y in a
  if true
}|};

  ()

let test_parse_array () =
  assert_expr (Array []) "[]";
  assert_expr (Array [ Number 1.0 ]) "[1]";
  assert_expr (Array [ Number 1.0; String "s" ]) {|[1, "s"]|};

  assert_expr (ArrayFor (Var "x", ("x", Var "a"), [])) {|[x for x in a]|};
  assert_expr
    (ArrayFor
       ( Array [ Var "x"; Var "y" ],
         ("x", Var "a"),
         [ Forspec ("y", Var "a"); Ifspec True ] ))
    {|
[
  [x, y],
  for x in a
  for y in a
  if true
]|};

  ()

let test_parse_select () =
  assert_expr (Select (Var "x", "y")) {|x.y|};
  assert_expr (Select (Select (Var "x", "y"), "z")) {|x.y.z|};
  assert_expr
    (Select
       ( Select
           ( Object
               (ObjectMemberList
                  [
                    MemberField
                      (Field
                         ( FieldnameID "y",
                           false,
                           H 1,
                           Object
                             (ObjectMemberList
                                [
                                  MemberField
                                    (Field
                                       (FieldnameID "z", false, H 1, String "a"));
                                ]) ));
                  ]),
             "y" ),
         "z" ))
    {|{y: {z: "a"}}.y.z|};
  assert_expr (SuperIndex (String "x")) {|super.x|};
  assert_expr (SuperIndex (String "x")) {|super["x"]|};
  ()

let test_parse_array_index () =
  assert_expr (ArrayIndex (Var "a", Number 0.0)) {|a[0]|};
  assert_expr (ArrayIndex (Array [ String "a" ], Number 0.0)) {|["a"][0]|};
  ()

let test_parse_array_slice () =
  assert_expr (ArraySlice (Var "x", None, None, None)) {|x[::]|};
  assert_expr (ArraySlice (Var "x", None, None, Some (Number 0.0))) {|x[::0]|};
  assert_expr (ArraySlice (Var "x", None, Some (Number 0.0), None)) {|x[:0:]|};
  assert_expr
    (ArraySlice (Var "x", None, Some (Number 0.0), Some (Number 0.0)))
    {|x[:0:0]|};
  assert_expr (ArraySlice (Var "x", Some (Number 0.0), None, None)) {|x[0::]|};
  assert_expr
    (ArraySlice (Var "x", Some (Number 0.0), None, Some (Number 0.0)))
    {|x[0::0]|};
  assert_expr
    (ArraySlice (Var "x", Some (Number 0.0), Some (Number 0.0), None))
    {|x[0:0:]|};
  assert_expr
    (ArraySlice
       (Var "x", Some (Number 0.0), Some (Number 0.0), Some (Number 0.0)))
    {|x[0:0:0]|};
  ()

let test_parse_call () =
  assert_expr (Call (Var "x", ([], []), false)) {|x()|};
  assert_expr (Call (Var "x", ([], []), true)) {|x() tailstrict|};
  assert_expr (Call (Var "x", ([ Var "a" ], []), false)) {|x(a)|};
  assert_expr (Call (Var "x", ([ Var "a" ], []), false)) {|x(a,)|};
  assert_expr (Call (Var "x", ([ Var "a"; Var "b" ], []), false)) {|x(a, b)|};
  assert_expr (Call (Var "x", ([ Var "a"; Var "b" ], []), false)) {|x(a, b,)|};
  assert_expr (Call (Var "x", ([], [ ("a", Var "b") ]), false)) {|x(a=b)|};
  assert_expr (Call (Var "x", ([], [ ("a", Var "b") ]), false)) {|x(a=b,)|};
  assert_expr
    (Call (Var "x", ([], [ ("a", Var "b"); ("c", Var "d") ]), false))
    {|x(a=b,c=d)|};
  assert_expr
    (Call (Var "x", ([], [ ("a", Var "b"); ("c", Var "d") ]), false))
    {|x(a=b,c=d,)|};
  assert_expr
    (Call (Var "x", ([ Var "a" ], [ ("b", Var "c") ]), false))
    {|x(a, b=c)|};
  assert_expr
    (Call (Select (Var "std", "x"), ([ Var "a" ], [ ("b", Var "c") ]), false))
    {|std.x(a, b=c)|};
  ()

let test_parse_local () =
  assert_expr (Local ([ Bind ("a", Number 0.0) ], Var "a")) {|local a = 0; a|};
  assert_expr
    (Local ([ BindFunc ("a", [], Number 0.0) ], Call (Var "a", ([], []), false)))
    {|local a() = 0; a()|};
  assert_expr
    (Local
       ( [ BindFunc ("a", [ ("v", None); ("w", Some (Number 1.0)) ], Var "v") ],
         Call (Var "a", ([ Number 0.0 ], []), false) ))
    {|local a(v, w=1) = v; a(0)|};
  ()

let test_parse_if () =
  assert_expr (If (True, Number 1.0, None)) {|if true then 1.0|};
  assert_expr
    (If (True, Number 1.0, Some (Number 2.0)))
    {|if true then 1.0 else 2|};
  assert_expr
    (If
       ( If (True, True, Some False),
         If (True, Number 1.0, Some (Number 1.0)),
         Some (If (True, Number 1.0, Some (Number 1.0))) ))
    {|
if
  if true then true
  else false
then
  if true then 1.0
  else 1.0
else
  if true then 1.0
  else 1.0|};
  ()

let test_parse_binary () =
  assert_expr
    (Binary (Number 1.0, `Add, Binary (Number 1.0, `Mult, Number 1.0)))
    {|1+1*1|};
  assert_expr
    (Binary (Binary (Number 1.0, `Add, Number 1.0), `Mult, Number 1.0))
    {|(1+1)*1|};
  assert_expr
    (Binary
       ( Binary
           ( Binary
               ( Binary
                   ( Binary
                       ( Binary
                           ( Binary
                               ( Binary (Number 1., `Add, Unary (Pos, Number 1.)),
                                 `Sub,
                                 Unary
                                   ( Neg,
                                     Binary
                                       ( Binary
                                           ( Binary (Number 1., `Mult, Number 1.),
                                             `Div,
                                             Unary (Lnot, Number 1.) ),
                                         `Mod,
                                         Number 1. ) ) ),
                             `Lsl,
                             Number 1. ),
                         `Lsr,
                         Number 1. ),
                     `Lt,
                     Number 1. ),
                 `Lor,
                 Binary
                   ( True,
                     `Xor,
                     Binary
                       (Binary (True, `Equal, Unary (Not, True)), `Land, True)
                   ) ),
             `And,
             True ),
         `Or,
         False ))
    {|1++1--1*1/~1%1<<1>>1<1|true^true==!true&true&&true||false|};
  assert_expr
    (ObjectSeq
       ( Object
           (ObjectMemberList
              [ MemberField (Field (FieldnameID "x", false, H 1, String "a")) ]),
         ObjectMemberList
           [
             MemberField
               (Field (FieldnameID "y", false, H 1, InSuper (String "x")));
           ] ))
    {|{x: "a"} {y: "x" in super}|};
  ()

let test_parse_function () =
  assert_expr (Function ([], Number 1.0)) {|function() 1.0|};
  assert_expr
    (Function
       ( [ ("x", None); ("y", Some (Number 1.0)) ],
         Binary (Var "x", `Add, Var "y") ))
    {|function(x, y=1.0) x+y|};
  ()

let test_parse_unary () =
  assert_expr (Unary (Pos, Number 1.0)) {|+1|};
  assert_expr (Unary (Neg, Number 1.0)) {|-1|};
  assert_expr (Unary (Not, True)) {|!true|};
  assert_expr (Unary (Lnot, True)) {|~true|};
  ()

let test_parse_objectseq () =
  assert_expr
    (ObjectSeq
       ( Var "x",
         ObjectMemberList
           [ MemberField (Field (FieldnameID "x", false, H 1, Number 1.0)) ] ))
    {|x { x: 1 }|};
  ()

let test_parse_assert () =
  assert_expr
    (Assert ((Binary (Var "x", `Equal, Number 3.0), None), Number 0.0))
    {|assert x == 3.0; 0.0|};
  assert_expr
    (Assert
       ((Binary (Var "x", `Equal, Number 3.0), Some (String "s")), Number 0.0))
    {|assert x == 3.0 : "s"; 0.0|};
  assert_expr
    (Assert
       ( ( Assert
             ( (Binary (Var "x", `Equal, Number 3.0), Some (String "s1")),
               Binary (Number 0.0, `Equal, Number 0.0) ),
           Some (String "s2") ),
         Number 0.0 ))
    {|assert assert x == 3.0 : "s1"; 0.0 == 0.0 : "s2"; 0.0|};
  ()

let test_parse_import () =
  assert_expr (Import "foo") {|import "foo"|};
  assert_expr (Importstr "foo") {|importstr "foo"|};
  assert_expr (Importbin "foo") {|importbin "foo"|};
  ()

let test_parse_error () =
  assert_expr
    (Error (Binary (String "%d", `Mod, Number 3.0)))
    {|error "%d" % 3|};
  ()

let test_parse_std () =
  assert_expr Std_ast.expected
    (let ic = open_in_bin "../../../test/std.jsonnet" in
     Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
     In_channel.input_all ic);
  ()

let assert_core_expr expected got =
  match Parser.parse_string got with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      Logs.info (fun m -> m "expected %s" (Syntax.Core.show_expr expected));
      assert false
  | Ok { expr = got } ->
      let got = Syntax.desugar_expr false got in
      Logs.info (fun m ->
          m "got %s, expected %s"
            (Syntax.Core.show_expr got)
            (Syntax.Core.show_expr expected));
      assert (got = expected);
      ()

let test_desugar_object () =
  Syntax.reset_gensym_i ();
  assert_core_expr
    (Object { binds = [ ("$", Self) ]; assrts = []; fields = [] })
    "{}";
  assert_core_expr
    (Object
       {
         binds = [ ("$", Self) ];
         assrts = [];
         fields = [ (String "x", false, H 1, Number 1.) ];
       })
    "{x: 1}";
  assert_core_expr
    (Object
       {
         binds = [ ("$", Self); ("a", String "b") ];
         assrts = [];
         fields =
           [
             (String "foo", false, H 1, String "bar");
             (String "hoge", false, H 1, String "piyo");
           ];
       })
    {|
{
  local a = "b",
  ["foo"]: "bar",
  ["hoge"]: "piyo",
}|};
  assert_core_expr
    (Object
       {
         binds = [ ("$", Self); ("a", String "b") ];
         assrts = [];
         fields = [ (String "foo", true, H 1, String "bar") ];
       })
    {|
{
  local a = "b",
  ["foo"]+: "bar",
}|};
  assert_core_expr
    (Object
       {
         binds = [ ("$", Self) ];
         assrts =
           [
             If
               ( Call
                   ( ArrayIndex (Var "std", String "equals"),
                     [ Var "x"; Number 1. ],
                     [] ),
                 Null,
                 Error (String "Assertion failed") );
           ];
         fields = [ (String "x", false, H 1, Number 1.) ];
       })
    "{x: 1, assert x == 1}";
  assert_core_expr
    (Object
       {
         binds = [ ("$", Self) ];
         assrts = [];
         fields =
           [
             ( String "x",
               false,
               Syntax.H 1,
               Object
                 {
                   binds = [];
                   assrts = [];
                   fields = [ (String "y", false, Syntax.H 1, Number 1.) ];
                 } );
           ];
       })
    "{x: {y: 1}}";
  assert_core_expr
    (ObjectFor
       ( Local
           ( [ ("x", ArrayIndex (Var "$v1", Number 0.)) ],
             Binary (Var "x", `Add, String "foo") ),
         Local ([ ("x", ArrayIndex (Var "$v1", Number 0.)) ], String "y"),
         "$v1",
         Local
           ( [ ("$v2", Var "a") ],
             Call
               ( ArrayIndex (Var "std", String "join"),
                 [
                   Array [];
                   Call
                     ( ArrayIndex (Var "std", String "makeArray"),
                       [
                         Call
                           ( ArrayIndex (Var "std", String "length"),
                             [ Var "$v2" ],
                             [] );
                         Function
                           ( [ ("$v3", None) ],
                             Local
                               ( [ ("x", ArrayIndex (Var "$v2", Var "$v3")) ],
                                 Array [ Array [ Var "x" ] ] ) );
                       ],
                       [] );
                 ],
                 [] ) ) ))
    {|{[x+"foo"]: "y" for x in a}|};
  assert_core_expr
    (ObjectFor (Var "x", String "y", "x", Var "a"))
    {|{[x]: "y" for x in a}|};
  ()

let test_desugar_array () =
  Syntax.reset_gensym_i ();
  assert_core_expr
    (Local
       ( [ ("$v1", Var "xs") ],
         Call
           ( ArrayIndex (Var "std", String "join"),
             [
               Array [];
               Call
                 ( ArrayIndex (Var "std", String "makeArray"),
                   [
                     Call
                       ( ArrayIndex (Var "std", String "length"),
                         [ Var "$v1" ],
                         [] );
                     Function
                       ( [ ("$v2", None) ],
                         Local
                           ( [ ("x", ArrayIndex (Var "$v1", Var "$v2")) ],
                             Array [ Var "x" ] ) );
                   ],
                   [] );
             ],
             [] ) ))
    {|[x for x in xs]|};
  ()

let assert_static_check good src =
  match Parser.parse_string src with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      assert false
  | Ok { expr = got } -> (
      let desugared = Syntax.desugar_expr false got in
      match Static_check.f desugared with
      | Ok () when good -> ()
      | Error _ when not good -> ()
      | Ok () ->
          Logs.err (fun m ->
              m "failed to static check: expect error but got ok: %s" src);
          assert false
      | Error msg ->
          Logs.err (fun m ->
              m "failed to static check: expect ok but got error: %s: %s" src
                msg);
          assert false)

let test_static_check_basics () =
  assert_static_check true "null";
  assert_static_check false "x";
  assert_static_check false "self";
  assert_static_check true "{ x: self }";
  assert_static_check false "{ x: 1, x: 2 }";
  assert_static_check false "local x = 1, x = 2; x";
  ()

let assert_compile ?(remove_tmp_dir = true) ?(bundle_dir = "../../../bundle")
    ?(test_cases_dir = "../../../test/cases")
    ?(expected_file_suffix = ".expected") src_file_path result_pat =
  let input_file_path =
    Filename.concat test_cases_dir (src_file_path ^ ".jsonnet")
  in
  let expected_file_path =
    Filename.concat test_cases_dir (src_file_path ^ expected_file_suffix)
  in
  match Loader.load_root input_file_path with
  | Error msg ->
      Logs.err (fun m ->
          m "assert_compile_expr: failed to load: %s: %s" input_file_path msg);
      assert false
  | Ok t -> (
      try
        let got =
          t |> Loader.compile |> Executor.execute ~bundle_dir ~remove_tmp_dir
        in
        match result_pat with
        | `Success ->
            let expected = read_all expected_file_path in
            Alcotest.(check string) "" expected got;
            ()
        | `Error -> assert false
      with
      | Executor.Compilation_failed (_, stderr_msg) ->
          Logs.err (fun m -> m "failed to compile: '%s'" stderr_msg);
          assert false
      | Executor.Compiled_executable_failed (_, stderr_msg) -> (
          match result_pat with
          | `Success ->
              Logs.err (fun m -> m "failed to execute: '%s'" stderr_msg);
              assert false
          | `Error -> (
              let expected = read_all expected_file_path |> String.trim in
              match Str.(search_forward (regexp expected) stderr_msg 0) with
              | exception Not_found ->
                  Logs.err (fun m ->
                      m
                        "failed to find expected substring: expect '%s', got \
                         '%s'"
                        expected stderr_msg);
                  assert false
              | _ -> ())))

let test_compiler_error () =
  assert_compile "error00" `Error;
  assert_compile "error01" `Error;
  assert_compile "error02" `Error;
  ()

let test_compiler () =
  assert_compile "success00" `Success;
  ()

let test_compiler_with_go_jsonnet_testdata () =
  let assert_compile =
    assert_compile ~test_cases_dir:"../../../go-jsonnet/testdata"
      ~expected_file_suffix:".golden"
  in

  assert_compile "argcapture_builtin_call" `Success;
  assert_compile "array_index1" `Success;
  assert_compile "array_index2" `Success;
  assert_compile "array_index3" `Success;
  assert_compile "array_index4" `Success;
  assert_compile "arrcomp" `Success;
  assert_compile "arrcomp2" `Success;
  assert_compile "arrcomp3" `Success;
  assert_compile "arrcomp4" `Success;
  assert_compile "arrcomp6" `Success;
  assert_compile "arrcomp7" `Success;

  (*
  assert_compile "arrcomp_if" `Success;
  assert_compile "arrcomp_if2" `Success;
  assert_compile "arrcomp_if3" `Success;
  assert_compile "arrcomp_if5" `Success;
  assert_compile "assert2" `Success;
  assert_compile "assert_equal" `Success;
  assert_compile "assert_equal2" `Success;
  assert_compile "assert_equal3" `Success;
  assert_compile "binaryNot" `Success;
  assert_compile "bitwise_and" `Success;
  assert_compile "bitwise_and2" `Success;
  assert_compile "bitwise_and5" `Success;
  assert_compile "bitwise_and6" `Success;
  assert_compile "bitwise_or" `Success;
  assert_compile "bitwise_or2" `Success;
  assert_compile "bitwise_or3" `Success;
  assert_compile "bitwise_or4" `Success;
  assert_compile "bitwise_or5" `Success;
  assert_compile "bitwise_or6" `Success;
  assert_compile "bitwise_or7" `Success;
  assert_compile "bitwise_or8" `Success;
  assert_compile "bitwise_or9" `Success;
  assert_compile "bitwise_shift" `Success;
  assert_compile "bitwise_shift2" `Success;
  assert_compile "bitwise_shift3" `Success;
  assert_compile "bitwise_shift5" `Success;
  assert_compile "bitwise_xor" `Success;
  assert_compile "bitwise_xor2" `Success;
  assert_compile "bitwise_xor3" `Success;
  assert_compile "bitwise_xor4" `Success;
  assert_compile "bitwise_xor5" `Success;
  assert_compile "bitwise_xor6" `Success;
  assert_compile "bitwise_xor8" `Success;
  assert_compile "bitwise_xor9" `Success;
  assert_compile "block_escaping" `Success;
  assert_compile "block_string" `Success;
  assert_compile "boolean_literal" `Success;
  assert_compile "builtinAvg" `Success;
  assert_compile "builtinBase64" `Success;
  assert_compile "builtinBase64Decode" `Success;
  assert_compile "builtinBase64DecodeBytes" `Success;
  assert_compile "builtinBase64_byte_array" `Success;
  assert_compile "builtinChar" `Success;
  assert_compile "builtinChar2" `Success;
  assert_compile "builtinChar4" `Success;
  assert_compile "builtinChar6" `Success;
  assert_compile "builtinContains" `Success;
  assert_compile "builtinContains2" `Success;
  assert_compile "builtinEqualsIgnoreCase" `Success;
  assert_compile "builtinEqualsIgnoreCase2" `Success;
  assert_compile "builtinIsDecimal" `Success;
  assert_compile "builtinIsDecimal2" `Success;
  assert_compile "builtinIsEmpty" `Success;
  assert_compile "builtinIsEmpty1" `Success;
  assert_compile "builtinIsEven" `Success;
  assert_compile "builtinIsEven2" `Success;
  assert_compile "builtinIsInteger" `Success;
  assert_compile "builtinIsInteger2" `Success;
  assert_compile "builtinIsOdd" `Success;
  assert_compile "builtinIsOdd2" `Success;
  assert_compile "builtinManifestJsonEx" `Success;
  assert_compile "builtinMaxArray" `Success;
  assert_compile "builtinMinArray" `Success;
  assert_compile "builtinObjectFieldsEx" `Success;
  assert_compile "builtinObjectFieldsExWithHidden" `Success;
  assert_compile "builtinObjectHasEx" `Success;
  assert_compile "builtinObjectRemoveKey" `Success;
  assert_compile "builtinRemove" `Success;
  assert_compile "builtinRemoveAt" `Success;
  assert_compile "builtinReverse" `Success;
  assert_compile "builtinReverse_empty" `Success;
  assert_compile "builtinReverse_many" `Success;
  assert_compile "builtinReverse_single" `Success;
  assert_compile "builtinRound" `Success;
  assert_compile "builtinSha1" `Success;
  assert_compile "builtinSha256" `Success;
  assert_compile "builtinSha3" `Success;
  assert_compile "builtinSha512" `Success;
  assert_compile "builtinSubStr_length_larger" `Success;
  assert_compile "builtinSubStr_start_larger_then_size" `Success;
  assert_compile "builtinSubstr" `Success;
  assert_compile "builtinSum" `Success;
  assert_compile "builtinTrim" `Success;
  assert_compile "builtinTrim1" `Success;
  assert_compile "builtinTrim2" `Success;
  assert_compile "builtinTrim3" `Success;
  assert_compile "builtinXnor" `Success;
  assert_compile "builtinXnor1" `Success;
  assert_compile "builtinXor" `Success;
  assert_compile "builtinXor1" `Success;
  assert_compile "builtin_acos" `Success;
  assert_compile "builtin_asin" `Success;
  assert_compile "builtin_atan" `Success;
  assert_compile "builtin_ceil" `Success;
  assert_compile "builtin_cos" `Success;
  assert_compile "builtin_exp" `Success;
  assert_compile "builtin_exp2" `Success;
  assert_compile "builtin_exp4" `Success;
  assert_compile "builtin_exp6" `Success;
  assert_compile "builtin_exp7" `Success;
  assert_compile "builtin_exp8" `Success;
  assert_compile "builtin_floor" `Success;
  assert_compile "builtin_log" `Success;
  assert_compile "builtin_log2" `Success;
  assert_compile "builtin_log3" `Success;
  assert_compile "builtin_log4" `Success;
  assert_compile "builtin_log6" `Success;
  assert_compile "builtin_lstripChars" `Success;
  assert_compile "builtin_manifestTomlEx" `Success;
  assert_compile "builtin_member_array" `Success;
  assert_compile "builtin_member_string" `Success;
  assert_compile "builtin_parseInt" `Success;
  assert_compile "builtin_parseInt2" `Success;
  assert_compile "builtin_rstripChars" `Success;
  assert_compile "builtin_sin" `Success;
  assert_compile "builtin_sqrt" `Success;
  assert_compile "builtin_stripChars" `Success;
  assert_compile "builtin_substr_multibyte" `Success;
  assert_compile "builtin_tan" `Success;
  assert_compile "comparisons" `Success;
  assert_compile "decodeUTF8" `Success;
  assert_compile "div1" `Success;
  assert_compile "div2" `Success;
  assert_compile "div3" `Success;
  assert_compile "dollar_end" `Success;
  assert_compile "dollar_end2" `Success;
  assert_compile "empty_array" `Success;
  assert_compile "empty_object" `Success;
  assert_compile "empty_object_comp" `Success;
  assert_compile "encodeUTF8" `Success;
  assert_compile "equals" `Success;
  assert_compile "equals2" `Success;
  assert_compile "equals3" `Success;
  assert_compile "equals4" `Success;
  assert_compile "equals5" `Success;
  assert_compile "equals6" `Success;
  assert_compile "escaped_fields" `Success;
  assert_compile "escaped_single_quote" `Success;
  assert_compile "extvar_code" `Success;
  assert_compile "extvar_mutually_recursive" `Success;
  assert_compile "extvar_self_recursive" `Success;
  assert_compile "extvar_string" `Success;
  assert_compile "false" `Success;
  assert_compile "filled_thunk" `Success;
  assert_compile "foldl_empty" `Success;
  assert_compile "foldl_single_element" `Success;
  assert_compile "foldl_string" `Success;
  assert_compile "foldl_various" `Success;
  assert_compile "foldr_empty" `Success;
  assert_compile "foldr_single_element" `Success;
  assert_compile "foldr_string" `Success;
  assert_compile "foldr_various" `Success;
  assert_compile "function_capturing" `Success;
  assert_compile "function_in_object" `Success;
  assert_compile "function_no_params" `Success;
  assert_compile "function_with_argument" `Success;
  assert_compile "greater" `Success;
  assert_compile "greaterEq" `Success;
  assert_compile "greaterEq2" `Success;
  assert_compile "ifthen_false" `Success;
  assert_compile "ifthenelse_false" `Success;
  assert_compile "ifthenelse_true" `Success;
  assert_compile "import2" `Success;
  assert_compile "import3" `Success;
  assert_compile "import4" `Success;
  assert_compile "import_block_literal" `Success;
  assert_compile "import_computed" `Success;
  assert_compile "import_twice" `Success;
  assert_compile "import_various_literals" `Success;
  assert_compile "importbin_block_literal" `Success;
  assert_compile "importbin_computed" `Success;
  assert_compile "importbin_nonutf8" `Success;
  assert_compile "importstr_block_literal" `Success;
  assert_compile "importstr_computed" `Success;
  assert_compile "in" `Success;
  assert_compile "in2" `Success;
  assert_compile "in3" `Success;
  assert_compile "in4" `Success;
  assert_compile "insuper" `Success;
  assert_compile "insuper2" `Success;
  assert_compile "insuper3" `Success;
  assert_compile "insuper4" `Success;
  assert_compile "insuper5" `Success;
  assert_compile "insuper7" `Success;
  assert_compile "lazy" `Success;
  assert_compile "lazy_operator1" `Success;
  assert_compile "less" `Success;
  assert_compile "lessEq" `Success;
  assert_compile "lessEq2" `Success;
  assert_compile "local_in_object_assertion" `Success;
  assert_compile "local_within_nested_object" `Success;
  assert_compile "local_within_object" `Success;
  assert_compile "method_call" `Success;
  assert_compile "modulo" `Success;
  assert_compile "modulo4" `Success;
  assert_compile "modulo5" `Success;
  assert_compile "modulo6" `Success;
  assert_compile "modulo7" `Success;
  assert_compile "mult" `Success;
  assert_compile "mult2" `Success;
  assert_compile "mult3" `Success;
  assert_compile "multi" `Success;
  assert_compile "multi_string_output" `Success;
  assert_compile "native1" `Success;
  assert_compile "native2" `Success;
  assert_compile "native3" `Success;
  assert_compile "native6" `Success;
  assert_compile "native_nonexistent" `Success;
  assert_compile "number_leading_zero" `Success;
  assert_compile "numeric_literal" `Success;
  assert_compile "obj_local_right_level" `Success;
  assert_compile "obj_local_right_level2" `Success;
  assert_compile "obj_local_right_level3" `Success;
  assert_compile "object_comp" `Success;
  assert_compile "object_comp2" `Success;
  assert_compile "object_comp3" `Success;
  assert_compile "object_comp4" `Success;
  assert_compile "object_comp_assert" `Success;
  assert_compile "object_comp_dollar" `Success;
  assert_compile "object_comp_dollar2" `Success;
  assert_compile "object_comp_dollar3" `Success;
  assert_compile "object_comp_if" `Success;
  assert_compile "object_comp_illegal" `Success;
  assert_compile "object_comp_local" `Success;
  assert_compile "object_comp_local2" `Success;
  assert_compile "object_comp_local3" `Success;
  assert_compile "object_comp_super" `Success;
  assert_compile "object_hidden" `Success;
  assert_compile "object_invariant" `Success;
  assert_compile "object_invariant12" `Success;
  assert_compile "object_invariant3" `Success;
  assert_compile "object_invariant4" `Success;
  assert_compile "object_invariant5" `Success;
  assert_compile "object_invariant6" `Success;
  assert_compile "object_invariant_perf" `Success;
  assert_compile "object_invariant_plus3" `Success;
  assert_compile "object_invariant_plus4" `Success;
  assert_compile "object_invariant_plus5" `Success;
  assert_compile "object_invariant_plus7" `Success;
  assert_compile "object_literal_in_array_comp" `Success;
  assert_compile "object_literal_in_object_comp" `Success;
  assert_compile "object_local_from_parent" `Success;
  assert_compile "object_local_from_parent_through_local" `Success;
  assert_compile "object_local_recursive" `Success;
  assert_compile "object_local_self_super" `Success;
  assert_compile "object_local_uses_local_from_outside" `Success;
  assert_compile "object_sum" `Success;
  assert_compile "object_sum2" `Success;
  assert_compile "object_sum3" `Success;
  assert_compile "object_super" `Success;
  assert_compile "object_super_deep" `Success;
  assert_compile "object_super_within" `Success;
  assert_compile "object_various_field_types" `Success;
  assert_compile "object_within_object" `Success;
  assert_compile "optional_args" `Success;
  assert_compile "optional_args10" `Success;
  assert_compile "optional_args12" `Success;
  assert_compile "optional_args14" `Success;
  assert_compile "optional_args15" `Success;
  assert_compile "optional_args16" `Success;
  assert_compile "optional_args17" `Success;
  assert_compile "optional_args18" `Success;
  assert_compile "optional_args19" `Success;
  assert_compile "optional_args2" `Success;
  assert_compile "optional_args20" `Success;
  assert_compile "optional_args21" `Success;
  assert_compile "optional_args22" `Success;
  assert_compile "optional_args3" `Success;
  assert_compile "optional_args4" `Success;
  assert_compile "optional_args5" `Success;
  assert_compile "optional_args6" `Success;
  assert_compile "optional_args7" `Success;
  assert_compile "or3" `Success;
  assert_compile "overriding_stdlib_desugared" `Success;
  assert_compile "parseJson" `Success;
  assert_compile "parseYaml" `Success;
  assert_compile "percent_format_float" `Success;
  assert_compile "percent_format_str" `Success;
  assert_compile "percent_format_str2" `Success;
  assert_compile "percent_format_str3" `Success;
  assert_compile "percent_format_str8" `Success;
  assert_compile "percent_mod_int" `Success;
  assert_compile "percent_mod_int2" `Success;
  assert_compile "percent_mod_int3" `Success;
  assert_compile "percent_mod_int4" `Success;
  assert_compile "percent_mod_int6" `Success;
  assert_compile "plus3" `Success;
  assert_compile "plus4" `Success;
  assert_compile "plus7" `Success;
  assert_compile "plus8" `Success;
  assert_compile "plus9" `Success;
  assert_compile "positional_after_optional" `Success;
  assert_compile "pow" `Success;
  assert_compile "pow2" `Success;
  assert_compile "pow3" `Success;
  assert_compile "pow5" `Success;
  assert_compile "pow6" `Success;
  assert_compile "proto_object_comp" `Success;
  assert_compile "recursive_local" `Success;
  assert_compile "self" `Success;
  assert_compile "simple_arith1" `Success;
  assert_compile "simple_arith2" `Success;
  assert_compile "simple_arith3" `Success;
  assert_compile "simple_arith_string" `Success;
  assert_compile "simple_arith_string2" `Success;
  assert_compile "simple_arith_string3" `Success;
  assert_compile "simple_arith_string_empty" `Success;
  assert_compile "slice" `Success;
  assert_compile "slice2" `Success;
  assert_compile "slice3" `Success;
  assert_compile "slice4" `Success;
  assert_compile "slice5" `Success;
  assert_compile "slice6" `Success;
  assert_compile "slice7" `Success;
  assert_compile "stackbug-regression-test" `Success;
  assert_compile "std.codepoint" `Success;
  assert_compile "std.codepoint2" `Success;
  assert_compile "std.codepoint4" `Success;
  assert_compile "std.codepoint5" `Success;
  assert_compile "std.exponent" `Success;
  assert_compile "std.exponent2" `Success;
  assert_compile "std.exponent3" `Success;
  assert_compile "std.exponent4" `Success;
  assert_compile "std.exponent5" `Success;
  assert_compile "std.exponent6" `Success;
  assert_compile "std.exponent7" `Success;
  assert_compile "std.filter" `Success;
  assert_compile "std.filter3" `Success;
  assert_compile "std.filter7" `Success;
  assert_compile "std.flatmap" `Success;
  assert_compile "std.flatmap2" `Success;
  assert_compile "std.flatmap3" `Success;
  assert_compile "std.flatmap4" `Success;
  assert_compile "std.flatmap6" `Success;
  assert_compile "std.join" `Success;
  assert_compile "std.join2" `Success;
  assert_compile "std.join3" `Success;
  assert_compile "std.join4" `Success;
  assert_compile "std.join5" `Success;
  assert_compile "std.join6" `Success;
  assert_compile "std" `Success;
  assert_compile "std.length" `Success;
  assert_compile "std.length_array" `Success;
  assert_compile "std.length_function" `Success;
  assert_compile "std.length_object" `Success;
  assert_compile "std.length_object_sum" `Success;
  assert_compile "std.length_object_with_hidden" `Success;
  assert_compile "std.length_string" `Success;
  assert_compile "std.lstripChars.multibyte" `Success;
  assert_compile "std.makeArray" `Success;
  assert_compile "std.makeArrayNamed" `Success;
  assert_compile "std.makeArrayNamed2" `Success;
  assert_compile "std.makeArrayNamed4" `Success;
  assert_compile "std.makeArray_recursive" `Success;
  assert_compile "std.mantissa" `Success;
  assert_compile "std.mantissa2" `Success;
  assert_compile "std.mantissa3" `Success;
  assert_compile "std.mantissa4" `Success;
  assert_compile "std.mantissa5" `Success;
  assert_compile "std.mantissa6" `Success;
  assert_compile "std.mantissa7" `Success;
  assert_compile "std.md5" `Success;
  assert_compile "std.md5_2" `Success;
  assert_compile "std.md5_3" `Success;
  assert_compile "std.md5_4" `Success;
  assert_compile "std.md5_5" `Success;
  assert_compile "std.mod_int" `Success;
  assert_compile "std.mod_string" `Success;
  assert_compile "std.modulo" `Success;
  assert_compile "std.objectFields" `Success;
  assert_compile "std.objectHasEx" `Success;
  assert_compile "std.objectHasEx2" `Success;
  assert_compile "std.objectHasEx3" `Success;
  assert_compile "std.objectHasEx4" `Success;
  assert_compile "std.primitiveEquals" `Success;
  assert_compile "std.primitiveEquals11" `Success;
  assert_compile "std.primitiveEquals12" `Success;
  assert_compile "std.primitiveEquals14" `Success;
  assert_compile "std.primitiveEquals15" `Success;
  assert_compile "std.primitiveEquals16" `Success;
  assert_compile "std.primitiveEquals17" `Success;
  assert_compile "std.primitiveEquals18" `Success;
  assert_compile "std.primitiveEquals19" `Success;
  assert_compile "std.primitiveEquals2" `Success;
  assert_compile "std.primitiveEquals20" `Success;
  assert_compile "std.primitiveEquals21" `Success;
  assert_compile "std.primitiveEquals3" `Success;
  assert_compile "std.primitiveEquals4" `Success;
  assert_compile "std.primitiveEquals5" `Success;
  assert_compile "std.primitiveEquals8" `Success;
  assert_compile "std.rstripChars.multibyte" `Success;
  assert_compile "std.slice" `Success;
  assert_compile "std.sort" `Success;
  assert_compile "std.sort2" `Success;
  assert_compile "std.thisFile" `Success;
  assert_compile "std.thisFile2" `Success;
  assert_compile "std.toString" `Success;
  assert_compile "std.toString2" `Success;
  assert_compile "std.toString3" `Success;
  assert_compile "std.toString4" `Success;
  assert_compile "std.toString6" `Success;
  assert_compile "std.toString7" `Success;
  assert_compile "std.toString8" `Success;
  assert_compile "std_in_local" `Success;
  assert_compile "std_substr" `Success;
  assert_compile "stdlib_smoke_test" `Success;
  assert_compile "strReplace" `Success;
  assert_compile "strReplace2" `Success;
  assert_compile "string2" `Success;
  assert_compile "string_comparison1" `Success;
  assert_compile "string_comparison2" `Success;
  assert_compile "string_comparison3" `Success;
  assert_compile "string_comparison4" `Success;
  assert_compile "string_comparison5" `Success;
  assert_compile "string_comparison6" `Success;
  assert_compile "string_comparison7" `Success;
  assert_compile "string_index" `Success;
  assert_compile "string_index2" `Success;
  assert_compile "string_to_bool" `Success;
  assert_compile "supersugar" `Success;
  assert_compile "supersugar2" `Success;
  assert_compile "supersugar3" `Success;
  assert_compile "supersugar4" `Success;
  assert_compile "supersugar5" `Success;
  assert_compile "supersugar6" `Success;
  assert_compile "supersugar7" `Success;
  assert_compile "supersugar9" `Success;
  assert_compile "tailstrict" `Success;
  assert_compile "tailstrict4" `Success;
  assert_compile "tailstrict5" `Success;
  assert_compile "tailstrict_operator1" `Success;
  assert_compile "tailstrict_operator2" `Success;
  assert_compile "tailstrict_operator3" `Success;
  assert_compile "true" `Success;
  assert_compile "type_array" `Success;
  assert_compile "type_builtin_function" `Success;
  assert_compile "type_function" `Success;
  assert_compile "type_number" `Success;
  assert_compile "type_object" `Success;
  assert_compile "type_string" `Success;
  assert_compile "unary_minus" `Success;
  assert_compile "unary_minus2" `Success;
  assert_compile "unary_minus3" `Success;
  assert_compile "unfinished_args" `Success;
  assert_compile "unicode" `Success;
  assert_compile "unicode2" `Success;
  assert_compile "use_object" `Success;
  assert_compile "use_object_in_object" `Success;
  assert_compile "variable" `Success;
  assert_compile "verbatim_string" `Success;
  *)
  ()

let () =
  let open Alcotest in
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  run "jitsonnet"
    [
      ( "lexer",
        [
          test_case "keyword" `Quick test_lexer_keyword;
          test_case "number" `Quick test_lexer_number;
          test_case "string" `Quick test_lexer_string;
        ] );
      ( "parse",
        [
          test_case "atoms" `Quick test_parse_atoms;
          test_case "object" `Quick test_parse_object;
          test_case "select" `Quick test_parse_select;
          test_case "array" `Quick test_parse_array;
          test_case "array index" `Quick test_parse_array_index;
          test_case "array slice" `Quick test_parse_array_slice;
          test_case "call" `Quick test_parse_call;
          test_case "local" `Quick test_parse_local;
          test_case "if" `Quick test_parse_if;
          test_case "binary" `Quick test_parse_binary;
          test_case "unary" `Quick test_parse_unary;
          test_case "object seq" `Quick test_parse_objectseq;
          test_case "function" `Quick test_parse_function;
          test_case "assert" `Quick test_parse_assert;
          test_case "import" `Quick test_parse_import;
          test_case "error" `Quick test_parse_error;
          test_case "std" `Quick test_parse_std;
        ] );
      ( "desugar",
        [
          test_case "object" `Quick test_desugar_object;
          test_case "array" `Quick test_desugar_array;
        ] );
      ("static check", [ test_case "basics" `Quick test_static_check_basics ]);
      ( "compiler",
        [
          test_case "ok" `Quick test_compiler;
          test_case "error" `Quick test_compiler_error;
          test_case "go-jsonnet ok" `Quick
            test_compiler_with_go_jsonnet_testdata;
        ] );
    ]
