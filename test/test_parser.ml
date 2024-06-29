open Jitsonnet
open Common [@@warning "-33"]

let testcase0 file_name =
  let open OUnit2 in
  let in_file_path = "test/cases/parse/" ^ file_name ^ ".in" in
  let expected_file_path =
    "../../../test/cases/parse/" ^ file_name ^ ".expected"
  in
  in_file_path >:: fun _test_ctxt ->
  match
    let cwd = Unix.getcwd () in
    Unix.chdir "../../../";
    Fun.protect
      ~finally:(fun () -> Unix.chdir cwd)
      (fun () -> Parser.parse_file in_file_path)
  with
  | Error msg ->
      Logs.err (fun m -> m "failed to parse: %s" msg);
      assert false
  | Ok { expr = got } ->
      let got_printed = Syntax.show_expr got in
      let expected_printed = read_all expected_file_path in
      if got_printed = expected_printed then ()
      else (
        Logs.err (fun m ->
            m "test failure:\ngot:      %s\nexpected: %s" got_printed
              expected_printed);
        assert false)

let suite =
  [
    testcase0 "null";
    testcase0 "true";
    testcase0 "false";
    testcase0 "self";
    testcase0 "dollar";
    testcase0 "string";
    testcase0 "number";
    testcase0 "block_string";
    testcase0 "object_empty";
    testcase0 "object_single";
    testcase0 "object_complex1";
    testcase0 "object_complex2";
    testcase0 "object_complex3";
    testcase0 "object_complex4";
    testcase0 "object_for";
    testcase0 "array_empty";
    testcase0 "array_single";
    testcase0 "array_double";
    testcase0 "array_for2";
    testcase0 "select_single";
    testcase0 "select_double";
    testcase0 "select_immediate";
    testcase0 "select_super1";
    testcase0 "select_super2";
    testcase0 "array_index_simple";
    testcase0 "array_index_immediate";
    testcase0 "array_slice1";
    testcase0 "array_slice2";
    testcase0 "array_slice3";
    testcase0 "array_slice4";
    testcase0 "array_slice5";
    testcase0 "array_slice6";
    testcase0 "array_slice7";
    testcase0 "array_slice8";
    testcase0 "call_empty_args";
    testcase0 "call_empty_args_tailstrict";
    testcase0 "call_one_arg1";
    testcase0 "call_one_arg2";
    testcase0 "call_two_arg1";
    testcase0 "call_two_arg2";
    testcase0 "call_named_arg1";
    testcase0 "call_named_arg2";
    testcase0 "call_named_arg3";
    testcase0 "call_named_arg4";
    testcase0 "call_named_arg5";
    testcase0 "call_named_arg6";
    testcase0 "local_simple";
    testcase0 "local_func1";
    testcase0 "local_func2";
    testcase0 "if_then";
    testcase0 "if_then_else1";
    testcase0 "if_then_else2";
    testcase0 "binary_add_implicit";
    testcase0 "binary_complex1";
    testcase0 "binary_simple1";
    testcase0 "binary_simple2";
    testcase0 "function_complex";
    testcase0 "function_simple";
    testcase0 "unary_lnot";
    testcase0 "unary_minus";
    testcase0 "unary_not";
    testcase0 "unary_plus";
    testcase0 "objectseq";
    testcase0 "assert_no_msg";
    testcase0 "assert_w_msg";
    testcase0 "assert_complex";
    testcase0 "import_unmarked";
    testcase0 "import_str";
    testcase0 "import_bin";
    testcase0 "std";
  ]

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  let open OUnit2 in
  run_test_tt_main ("parser" >::: suite)
