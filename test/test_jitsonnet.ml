open Jitsonnet

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
          [
            MemberField (Field (FieldnameID "x", H 1, Number 1.0));
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
            MemberField (Field (FieldnameString "w", H 3, Number 1.0));
          ]))
    {|
{
  x: 1,
  local a = "b",
  y():: "a",
  assert true,
  z(v1, v2="s")::"s",
  assert true : "s",
  "w":::1.0,
}|};
  ()

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

let () =
  let open Alcotest in
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  run "jitsonnet"
    [
      ( "parse",
        [
          test_case "atoms" `Quick test_parse_atoms;
          test_case "object" `Quick test_parse_object;
        ] );
      ( "lexer",
        [
          test_case "keyword" `Quick test_lexer_keyword;
          test_case "number" `Quick test_lexer_number;
          test_case "string" `Quick test_lexer_string;
        ] );
    ]
