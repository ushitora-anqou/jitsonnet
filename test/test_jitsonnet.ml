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
        ] );
    ]
