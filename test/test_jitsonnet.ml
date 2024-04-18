open Jitsonnet

(*
let assert_expr expected got =
  assert (Parser.parse_string got = Ok Syntax.{ expr = expected });
  ()

let test_parse_comment () =
  assert_expr (Number 0) "0 # comment";
  assert_expr (Number 0) "0 // comment";
  assert_expr (Number 0) "/* comment */ 0 /* comment */";
  ()
*)

let assert_token expected got_src =
  let got = L.main (Lexing.from_string got_src) in
  Logs.info (fun m ->
      m "got %s, expected %s"
        (Parser.string_of_token got)
        (Parser.string_of_token expected));
  assert (got = expected);
  ()

let test_lexer_keyword () =
  assert_token Assert "assert";
  assert_token Else "else";
  assert_token Error "error";
  assert_token False "false";
  assert_token For "for";
  assert_token Function "function";
  assert_token If "if";
  assert_token Import "import";
  assert_token Importstr "importstr";
  assert_token Importbin "importbin";
  assert_token In "in";
  assert_token Local "local";
  assert_token Null "null";
  assert_token Tailstrict "tailstrict";
  assert_token Then "then";
  assert_token Self "self";
  assert_token Super "super";
  assert_token True "true";
  ()

let test_lexer_number () =
  assert_token (Number 0.0) "0";
  assert_token (Number 1.0) "1";
  assert_token (Number 0.0) "0.0";
  assert_token (Number 1.0) "1.0";
  assert_token (Number 1.0) "1e0";
  assert_token (Number 10.0) "1e1";
  assert_token (Number 10.0) "1e+1";
  assert_token (Number 0.1) "1e-1";
  ()

let test_lexer_string () =
  assert_token (String "") {|""|};
  assert_token (String "abc") {|"abc"|};
  assert_token (String "ab\nc") {|"ab
c"|};
  assert_token (String "\"") {|"\""|};
  assert_token (String "\'") {|"\'"|};
  assert_token (String "\\") {|"\\"|};
  assert_token (String "/") {|"\/"|};
  assert_token (String "\b") {|"\b"|};
  assert_token (String "\x0c") {|"\f"|};
  assert_token (String "\n") {|"\n"|};
  assert_token (String "\r") {|"\r"|};
  assert_token (String "\t") {|"\t"|};
  assert_token (String "\u{30F9}") {|"\u30F9"|};

  assert_token (String "") {|''|};
  assert_token (String "abc") {|'abc'|};
  assert_token (String "ab\nc") {|'ab
c'|};
  assert_token (String "\"") {|'\"'|};
  assert_token (String "\'") {|'\''|};
  assert_token (String "\\") {|'\\'|};
  assert_token (String "/") {|'\/'|};
  assert_token (String "\b") {|'\b'|};
  assert_token (String "\x0c") {|'\f'|};
  assert_token (String "\n") {|'\n'|};
  assert_token (String "\r") {|'\r'|};
  assert_token (String "\t") {|'\t'|};
  assert_token (String "\u{30F9}") {|'\u30F9'|};

  assert_token (String "") {|@""|};
  assert_token (String "abc") {|@"abc"|};
  assert_token (String "ab\nc") {|@"ab
c"|};
  assert_token (String "\\") {|@"\"|};
  assert_token (String "\"") {|@""""|};
  assert_token (String "\'") {|"\'"|};
  assert_token (String "\\\\") {|@"\\"|};
  assert_token (String "\\/") {|@"\/"|};
  assert_token (String "\\b") {|@"\b"|};
  assert_token (String "\\f") {|@"\f"|};
  assert_token (String "\\n") {|@"\n"|};
  assert_token (String "\\r") {|@"\r"|};
  assert_token (String "\\t") {|@"\t"|};
  assert_token (String "\\u30F9") {|@"\u30F9"|};

  assert_token (String "") {|@''|};
  assert_token (String "abc") {|@'abc'|};
  assert_token (String "ab\nc") {|@'ab
c'|};
  assert_token (String "\\\"") {|@'\"'|};
  assert_token (String "\\") {|@'\'|};
  assert_token (String "'") {|@''''|};
  assert_token (String "\\\\") {|@'\\'|};
  assert_token (String "\\/") {|@'\/'|};
  assert_token (String "\\b") {|@'\b'|};
  assert_token (String "\\f") {|@'\f'|};
  assert_token (String "\\n") {|@'\n'|};
  assert_token (String "\\r") {|@'\r'|};
  assert_token (String "\\t") {|@'\t'|};
  assert_token (String "\\u30F9") {|@'\u30F9'|};

  assert_token (String "\nabc\n\ndef\n  gh\n\n|||\n |||\n")
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
      (*("parse", [ test_case "comment" `Quick test_parse_comment ]);*)
      ( "lexer",
        [
          test_case "keyword" `Quick test_lexer_keyword;
          test_case "number" `Quick test_lexer_number;
          test_case "string" `Quick test_lexer_string;
        ] );
    ]
