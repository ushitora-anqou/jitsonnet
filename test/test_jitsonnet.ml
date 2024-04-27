open Jitsonnet

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
  assert_expr (Select (Super, "x")) {|super.x|};
  assert_expr (ArrayIndex (Super, String "x")) {|super["x"]|};
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
               (Field
                  (FieldnameID "y", false, H 1, Binary (String "x", `In, Super)));
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
  assert_core_expr (Object ([], [])) "{}";
  assert_core_expr
    (Object ([], [ (String "x", H 1, Local ([ ("$", Self) ], Number 1.)) ]))
    "{x: 1}";
  assert_core_expr
    (Object
       ( [],
         [
           ( String "foo",
             H 1,
             Local ([ ("$", Self); ("a", String "b") ], String "bar") );
         ] ))
    {|
{
  local a = "b",
  ["foo"]: "bar",
}|};
  assert_core_expr
    (Object
       ( [],
         [
           ( String "foo",
             H 1,
             Local
               ( [ ("$", Self); ("a", String "b") ],
                 If
                   ( Call
                       ( ArrayIndex (Var "std", String "objectHasEx"),
                         [ Super; String "foo"; True ],
                         [] ),
                     Binary
                       (ArrayIndex (Super, String "foo"), `Add, String "bar"),
                     String "bar" ) ) );
         ] ))
    {|
{
  local a = "b",
  ["foo"]+: "bar",
}|};
  assert_core_expr
    (Object
       ( [
           Local
             ( [ ("$", Self) ],
               If
                 ( Call
                     ( ArrayIndex (Var "std", String "equals"),
                       [ Var "x"; Number 1. ],
                       [] ),
                   Null,
                   Error (String "Assertion failed") ) );
         ],
         [ (String "x", H 1, Local ([ ("$", Self) ], Number 1.)) ] ))
    "{x: 1, assert x == 1}";
  assert_core_expr
    (Object
       ( [],
         [
           ( String "x",
             Syntax.H 1,
             Local
               ( [ ("$", Self) ],
                 Local
                   ( [ ("$outerself", Self); ("$outersuper", Super) ],
                     Object
                       ([], [ (String "y", Syntax.H 1, Local ([], Number 1.)) ])
                   ) ) );
         ] ))
    "{x: {y: 1}}";
  ()

let test_desugar_array () =
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
                       ( [ ("$v2", Error (String "Parameter not bound")) ],
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

let test_executor_basics () =
  let open Ppxlib in
  let loc = !Ast_helper.default_loc in
  assert (
    Executor.execute
      [%str
        module M = struct
          let () = print_string "hello"
        end]
    = "hello");
  ()

let assert_compile_expr ?(remove_tmp_dir = true) expected got =
  match Parser.parse_string got with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      Logs.info (fun m -> m "expected %s" expected);
      assert false
  | Ok { expr } ->
      let got =
        expr |> Syntax.desugar_expr false |> Compiler.compile
        |> Executor.execute ~remove_tmp_dir
      in
      Alcotest.(check string) "" expected got;
      ()

let test_compiler () =
  let code =
    {|
[
  true,
  false,
  null,
  'foo',
  0.0,
  [],
  [1,2,3],
  [1,2,[3,[4,5,6][0],7][1]][2],
  2*(1+(8>>1)-(1<<(1|1&1^0))),
  !false && false || true,
  [
    1 < 2,  "a" < "b",  [] < ["a"],  [] < [],  ["a"] < [],
    1 > 2,  "a" > "b",  [] > ["a"],  [] > [],  ["a"] > [],
    1 <= 2, "a" <= "b", [] <= ["a"], [] <= [], ["a"] <= [],
    1 >= 2, "a" >= "b", [] >= ["a"], [] >= [], ["a"] >= [],
  ],
  [[1]+[2],"a"+"b"],
  [!false, ~1, -1, +1],
  if 1 < 2 then 10 else 20,
  if 1 > 2 then 10,
  (function() 10)(),
  true || error "unreachable",
  false && error "unreachable",
  [0, error "unreachable"][0],
  local x = 3; 10,
  local x = y * 2, y = z + 3, z = 5; x,
]
|}
  in
  let expected =
    String.trim
      {|
[
   true,
   false,
   null,
   "foo",
   0,
   [ ],
   [
      1,
      2,
      3
   ],
   4,
   6,
   true,
   [
      true,
      true,
      true,
      false,
      false,
      false,
      false,
      false,
      false,
      true,
      true,
      true,
      true,
      true,
      false,
      false,
      false,
      false,
      true,
      true
   ],
   [
      [
         1,
         2
      ],
      "ab"
   ],
   [
      true,
      -2,
      -1,
      1
   ],
   10,
   null,
   10,
   true,
   false,
   0,
   10,
   16
]
  |}
  in
  assert_compile_expr ~remove_tmp_dir:false expected code;
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
      ("executor", [ test_case "basics" `Quick test_executor_basics ]);
      ("compiler", [ test_case "all" `Quick test_compiler ]);
    ]
