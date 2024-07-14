open Jitsonnet
open Common

let assert_compile ?remove_work_dir ?(runtime_dir = "../../../runtime_hs")
    ?test_cases_dir ?expected_suffix ?multi ?string ?ext_codes ?ext_strs
    src_file_path result_pat =
  assert_compile' ?test_cases_dir ?expected_suffix ?multi ?ext_codes ?ext_strs
    ?string ~loader_optimize:false src_file_path result_pat
    ~compiler:(fun ~multi_output_dir ~t ~string ->
      let compiled = Loader.compile_haskell ?multi:multi_output_dir ~string t in
      Executor_hs.(
        execute
          (make_config ?remove_work_dir ~interactive_compile:true
             ~interactive_execute:false ~runtime_dir ())
          compiled))

let assert_compile ?multi ?string ?ext_codes ?ext_strs src_file_path result_pat
    =
  let saved_wd = Unix.getcwd () in
  Unix.chdir "../../../thirdparty/jsonnet";
  Fun.protect ~finally:(fun () -> Unix.chdir saved_wd) @@ fun () ->
  assert_compile ~test_cases_dir:"test_suite" ~expected_suffix:".jsonnet.golden"
    ?multi ?string ?ext_codes ?ext_strs ~runtime_dir:"../../runtime_hs"
    src_file_path result_pat

let testcase0 src_file_path result_path =
  let open OUnit2 in
  src_file_path >:: fun _test_ctxt -> assert_compile src_file_path result_path

let suite =
  [
    testcase0 "arith_bool" `SuccessSimple;
    (*
    testcase0 "arith_float" `SuccessSimple;
    *)
    testcase0 "arith_string" `SuccessSimple;
    testcase0 "array" `SuccessSimple;
    testcase0 "array_comparison" `Success;
    testcase0 "array_comparison2" `Success;
    testcase0 "assert" `SuccessSimple;
    testcase0 "binary" `SuccessSimple;
    testcase0 "comments" `SuccessSimple;
    testcase0 "condition" `SuccessSimple;
    testcase0 "dos_line_endings" `Success;
    testcase0 "fmt_idempotence_issue" `SuccessSimple;
    testcase0 "fmt_no_trailing_newline" `SuccessSimple;
    testcase0 "fmt_trailing_c_style" `SuccessSimple;
    testcase0 "fmt_trailing_multiple_comments" `SuccessSimple;
    testcase0 "fmt_trailing_newlines" `SuccessSimple;
    testcase0 "fmt_trailing_newlines2" `SuccessSimple;
    testcase0 "fmt_trailing_same_line_comment" `SuccessSimple;
    testcase0 "format" `SuccessSimple;
    testcase0 "formatter" `Success;
    testcase0 "formatting_braces" `Success;
    testcase0 "formatting_braces2" `Success;
    testcase0 "formatting_braces3" `SuccessSimple;
    testcase0 "functions" `SuccessSimple;
    testcase0 "import" `SuccessSimple;
    (*
    testcase0 "import_sorting" `SuccessSimple;
    testcase0 "import_sorting_by_filename" `SuccessSimple;
    testcase0 "import_sorting_crazy" `SuccessSimple;
    testcase0 "import_sorting_function_sugar" `SuccessSimple;
    testcase0 "import_sorting_group_ends" `SuccessSimple;
    testcase0 "import_sorting_groups" `SuccessSimple;
    testcase0 "import_sorting_multiple_binds_and_comments" `SuccessSimple;
    testcase0 "import_sorting_multiple_in_local" `SuccessSimple;
    testcase0 "import_sorting_unicode" `SuccessSimple;
    testcase0 "import_sorting_with_license" `SuccessSimple;
    testcase0 "invariant" `SuccessSimple;
    *)
    testcase0 "invariant_manifest" `Success;
    testcase0 "local" `SuccessSimple;
    testcase0 "merge" `SuccessSimple;
    testcase0 "null" `SuccessSimple;
    testcase0 "object" `SuccessSimple;
    testcase0 "oop" `SuccessSimple;
    (*
    testcase0 "oop_extra" `SuccessSimple;
    testcase0 "parseJson_long_array_gc_test" `Success;
    *)
    testcase0 "parsing_edge_cases" `SuccessSimple;
    testcase0 "precedence" `SuccessSimple;
    testcase0 "recursive_function" `SuccessSimple;
    testcase0 "recursive_import_ok" `SuccessSimple;
    testcase0 "recursive_object" `SuccessSimple;
    testcase0 "sanity" `Success;
    testcase0 "sanity2" `Success;
    testcase0 "shebang" `SuccessSimple;
    testcase0 "slice.sugar" `SuccessSimple;
    testcase0 "std_all_hidden" `SuccessSimple;
    testcase0 "text_block" `SuccessSimple;
    (*
    testcase0 "tla.simple" `SuccessSimple;
    testcase0 "trace" `Success;
    *)
    testcase0 "unicode" `SuccessSimple;
    (*testcase0 "unicode_bmp" `Success;*)
    testcase0 "unix_line_endings" `Success;
    (*testcase0 "unparse" `Success;*)
    testcase0 "verbatim_strings" `SuccessSimple;
    (*testcase0 "stdlib" `Success;*)
  ]

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let open OUnit2 in
  run_test_tt_main ("haskell backend w cpp jsonnet testcases" >::: suite)
