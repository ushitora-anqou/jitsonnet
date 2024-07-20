open Jitsonnet
open Common [@@warning "-33"]

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

let assert_tokens expected got_src =
  let lex = Lexing.from_string got_src in
  let got =
    let rec loop acc =
      match L.main lex with
      | P.EOF -> List.rev acc
      | token -> loop (token :: acc)
    in
    loop []
  in
  Logs.info (fun m ->
      m "got [%s], expected [%s]"
        (String.concat ";" (List.map Parser.string_of_token got))
        (String.concat ";" (List.map Parser.string_of_token expected)));
  assert (got = expected);
  ()

let test_lexer_slice () =
  assert_tokens
    P.[ LBRACKET; RBRACKET; LBRACKET; NUMBER 1.000000; COLON; COLON; RBRACKET ]
    {|[][/*foo*/1/*bar*/:/*baz*/:]|};
  ()

let wloc (startpos, endpos) v =
  Syntax.
    {
      v;
      loc =
        {
          fname = "";
          ran =
            Some
              {
                startpos = { line = fst startpos; column = snd startpos };
                endpos = { line = fst endpos; column = snd endpos };
              };
        };
    }
[@@warning "-32"]

(*
let assert_core_expr expected got =
  match Parser.parse_string got with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      Logs.info (fun m -> m "expected %s" (Syntax.Core.show_expr expected));
      assert false
  | Ok p ->
      let got = Syntax.desugar p in
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
                   ( ArrayIndex (Var "$std", String "equals"),
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
         Local
           ( [ ("$", Self) ],
             Local ([ ("x", ArrayIndex (Var "$v1", Number 0.)) ], String "y") ),
         "$v1",
         Local
           ( [ ("$v2", Var "a") ],
             Call
               ( ArrayIndex (Var "$std", String "join"),
                 [
                   Array [];
                   Call
                     ( ArrayIndex (Var "$std", String "makeArray"),
                       [
                         Call
                           ( ArrayIndex (Var "$std", String "length"),
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
    (ObjectFor (Var "x", Local ([ ("$", Self) ], String "y"), "x", Var "a"))
    {|{[x]: "y" for x in a}|};
  ()

let test_desugar_array () =
  Syntax.reset_gensym_i ();
  assert_core_expr
    (Local
       ( [ ("$v1", Var "xs") ],
         Call
           ( ArrayIndex (Var "$std", String "join"),
             [
               Array [];
               Call
                 ( ArrayIndex (Var "$std", String "makeArray"),
                   [
                     Call
                       ( ArrayIndex (Var "$std", String "length"),
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
*)

let assert_freevars expected got =
  match Parser.parse_string got with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      assert false
  | Ok p ->
      let got = Syntax.desugar p |> Syntax.Core.freevars in
      Logs.info (fun m ->
          m "got [%s], expected [%s] %s"
            (got |> Syntax.StringSet.to_list |> String.concat ", ")
            (expected |> String.concat ", ")
            (Syntax.Core.show_expr (Syntax.desugar p)));
      assert (Syntax.StringSet.equal got (Syntax.StringSet.of_list expected));
      ()

let test_freevars_local () =
  assert_freevars [ "y"; "a" ] "local x = y, z = w, w = 4; local x = z; w + a";
  assert_freevars [ "x" ] "local y = x; local x = 3; y";
  ()

let test_freevars_function () =
  assert_freevars [ "a"; "w"; "z" ]
    "local f(x) = local y = x + 3 + z; a + x + y; f(1, w)";
  ()

let test_freevars_object () =
  assert_freevars [ "x"; "z" ] "{ local x = 1, y: z, [ x ]: x }";
  assert_freevars [ "$std"; "x"; "w"; "z" ]
    "{ [ x + z ]: x + w for x in [ x ] for y in [ x ] }";
  assert_freevars [ "self"; "y" ] "self.x + y";
  assert_freevars [ "super"; "y" ] "super.x + y";
  assert_freevars [ "self" ] "{ [ self.x ]: 10 }";
  assert_freevars [ "super" ] "{ [ super.x ]: 10 }";
  ()

let assert_static_check good src =
  match Parser.parse_string src with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      assert false
  | Ok p -> (
      let desugared = Syntax.desugar p in
      match Static_check.f false desugared with
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

let test_alpha_conversion_basics () =
  let got =
    Parser.parse_string {|std.foo()|}
    |> Result.get_ok |> Syntax.desugar |> Syntax.Core.alpha_conv
  in
  let expected =
    (* FIXME *)
    wloc ((1, 0), (1, 9))
    @@ Syntax.Core.(
         Call
           ( wloc ((1, 0), (1, 7))
             @@ ArrayIndex
                  ( wloc ((1, 0), (1, 3)) @@ Var "$std",
                    wloc ((1, 0), (1, 7)) @@ String "foo" ),
             [],
             [] ))
  in
  Logs.info (fun m -> m "%s" (Syntax.Core.show_expr got));
  assert (got = expected);
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
          test_case "slice" `Quick test_lexer_slice;
        ] );
      (*
      ( "desugar",
        [
          test_case "object" `Quick test_desugar_object;
          test_case "array" `Quick test_desugar_array;
        ] );
        *)
      ( "freevars",
        [
          test_case "local" `Quick test_freevars_local;
          test_case "function" `Quick test_freevars_function;
          test_case "object" `Quick test_freevars_object;
        ] );
      ("static check", [ test_case "basics" `Quick test_static_check_basics ]);
      ( "alpha conversion",
        [ test_case "basics" `Quick test_alpha_conversion_basics ] );
    ]
