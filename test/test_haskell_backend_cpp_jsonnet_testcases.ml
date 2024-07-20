open Common

let assert_compile ?multi ?string ?ext_codes ?ext_strs src_file_path result_pat
    =
  let saved_wd = Unix.getcwd () in
  Unix.chdir "../../../thirdparty/jsonnet/test_suite";
  Fun.protect ~finally:(fun () -> Unix.chdir saved_wd) @@ fun () ->
  assert_compile_hs ~expected_suffix:".jsonnet.golden" ?multi ?string ?ext_codes
    ?ext_strs ~runtime_dir:"../../../runtime_hs" src_file_path result_pat

let testcase0 ?ext_codes ?ext_strs src_file_path result_path =
  let open OUnit2 in
  src_file_path >:: fun _test_ctxt ->
  assert_compile ?ext_codes ?ext_strs src_file_path result_path

let suite =
  [
    testcase0 "array_comparison" `Success;
    testcase0 "array_comparison2" `Success;
    testcase0 "dos_line_endings" `Success;
    testcase0 "fmt_idempotence_issue" `Success;
    testcase0 "fmt_no_trailing_newline" `Success;
    testcase0 "fmt_trailing_c_style" `Success;
    testcase0 "fmt_trailing_multiple_comments" `Success;
    testcase0 "fmt_trailing_newlines" `Success;
    testcase0 "fmt_trailing_newlines2" `Success;
    testcase0 "fmt_trailing_same_line_comment" `Success;
    testcase0 "formatter" `Success;
    testcase0 "formatting_braces" `Success;
    testcase0 "formatting_braces2" `Success;
    testcase0 "invariant_manifest" `Success;
    (*
    testcase0 "native_not_found" `Success;
    *)
    testcase0 "parseJson_long_array_gc_test" `Success;
    testcase0 "sanity" `Success;
    testcase0 "sanity2" `Success;
    testcase0 ~ext_strs:[ "var1=test" ] ~ext_codes:[ "var2={x:1,y:2}" ] "stdlib"
      `Success;
    testcase0 "trace" `Success;
    (*
    testcase0 "unicode_bmp" `Success;
    *)
    testcase0 "unix_line_endings" `Success;
    (*
    testcase0 "unparse" `Success;
    *)
    testcase0 "arith_bool" `SuccessSimple;
    testcase0 "arith_float" `SuccessSimple;
    testcase0 "arith_string" `SuccessSimple;
    testcase0 "array" `SuccessSimple;
    testcase0 "assert" `SuccessSimple;
    testcase0 "binary" `SuccessSimple;
    testcase0 "comments" `SuccessSimple;
    testcase0 "condition" `SuccessSimple;
    testcase0 "format" `SuccessSimple;
    testcase0 "formatting_braces3" `SuccessSimple;
    testcase0 "functions" `SuccessSimple;
    testcase0 "import" `SuccessSimple;
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
    testcase0 "local" `SuccessSimple;
    testcase0 "merge" `SuccessSimple;
    testcase0 "null" `SuccessSimple;
    testcase0 "object" `SuccessSimple;
    testcase0 "oop" `SuccessSimple;
    testcase0 "oop_extra" `SuccessSimple;
    testcase0 "parsing_edge_cases" `SuccessSimple;
    testcase0 "parsing_error" `SuccessSimple;
    testcase0 "precedence" `SuccessSimple;
    testcase0 "recursive_function" `SuccessSimple;
    testcase0 "recursive_import_ok" `SuccessSimple;
    testcase0 "recursive_object" `SuccessSimple;
    testcase0 "shebang" `SuccessSimple;
    testcase0 "slice.sugar" `SuccessSimple;
    testcase0 "std_all_hidden" `SuccessSimple;
    testcase0 "text_block" `SuccessSimple;
    (*
    testcase0 "tla.simple" `SuccessSimple;
    testcase0 "unicode" `SuccessSimple;
    *)
    testcase0 "verbatim_strings" `SuccessSimple;
    testcase0 "error.01" `ErrorPrecise;
    testcase0 "error.02" `ErrorPrecise;
    testcase0 "error.03" `ErrorPrecise;
    testcase0 "error.04" `ErrorPrecise;
    testcase0 "error.05" `ErrorPrecise;
    (*testcase0 "error.06" `ErrorSimple;*)
    testcase0 "error.07" `ErrorSimple;
    testcase0 "error.08" `ErrorSimple;
    testcase0 "error.args_commafodder" `ErrorSimple;
    (*testcase0 "error.array_fractional_index" `ErrorSimple;*)
    testcase0 "error.array_index_string" `ErrorSimple;
    testcase0 "error.array_large_index" `ErrorSimple;
    (*
    testcase0 "error.array_recursive_manifest" `ErrorSimple;
    *)
    testcase0 "error.assert.fail1" `ErrorSimple;
    testcase0 "error.assert.fail2" `ErrorSimple;
    testcase0 "error.assert_equal_obj" `ErrorSimple;
    testcase0 "error.assert_equal_str" `ErrorSimple;
    testcase0 "error.comprehension_spec_object" `ErrorSimple;
    testcase0 "error.comprehension_spec_object2" `ErrorSimple;
    testcase0 "error.computed_field_scope" `ErrorSimple;
    testcase0 "error.decodeUTF8_float" `ErrorSimple;
    testcase0 "error.decodeUTF8_nan" `ErrorSimple;
    (*
    testcase0 "error.divide_zero" `ErrorSimple;
    testcase0 "error.equality_function" `ErrorSimple;
    *)
    testcase0 "error.field_not_exist" `ErrorSimple;
    testcase0 "error.format.too_few_values" `ErrorSimple;
    (*
    testcase0 "error.function_duplicate_arg" `ErrorSimple;
    *)
    testcase0 "error.function_duplicate_param" `ErrorSimple;
    (*
    testcase0 "error.function_infinite_default" `ErrorSimple;
    *)
    testcase0 "error.function_no_default_arg" `ErrorSimple;
    (*
    testcase0 "error.function_too_many_args" `ErrorSimple;
    *)
    testcase0 "error.import_empty" `ErrorSimple;
    testcase0 "error.import_static-check-failure" `ErrorSimple;
    (*
    testcase0 "error.import_syntax-error" `ErrorSimple;
    *)
    testcase0 "error.inside_equals_array" `ErrorSimple;
    testcase0 "error.inside_equals_object" `ErrorSimple;
    testcase0 "error.inside_tostring_array" `ErrorSimple;
    testcase0 "error.inside_tostring_object" `ErrorSimple;
    testcase0 "error.invariant.avoid_output_change" `ErrorSimple;
    testcase0 "error.invariant.equality" `ErrorSimple;
    testcase0 "error.invariant.option" `ErrorSimple;
    testcase0 "error.invariant.simple" `ErrorSimple;
    testcase0 "error.invariant.simple2" `ErrorSimple;
    testcase0 "error.invariant.simple3" `ErrorSimple;
    testcase0 "error.manifest_toml_null_value" `ErrorSimple;
    testcase0 "error.manifest_toml_wrong_type" `ErrorSimple;
    (*
    testcase0 "error.negative_shfit" `ErrorSimple;
    *)
    testcase0 "error.obj_assert.fail1" `ErrorSimple;
    testcase0 "error.obj_assert.fail2" `ErrorSimple;
    (*
    testcase0 "error.obj_recursive" `ErrorSimple;
    testcase0 "error.obj_recursive_manifest" `ErrorSimple;
    *)
    (*
    testcase0 "error.overflow" `ErrorSimple;
    testcase0 "error.overflow2" `ErrorSimple;
    testcase0 "error.overflow3" `ErrorSimple;
    *)
    testcase0 "error.parse.array_comma" `ErrorSimple;
    testcase0 "error.parse.function_arg_positional_after_named" `ErrorSimple;
    testcase0 "error.parse.import_not_literal" `ErrorSimple;
    testcase0 "error.parse.import_text_block" `ErrorSimple;
    testcase0 "error.parse.index_unterminated" `ErrorSimple;
    testcase0 "error.parse.method_plus" `ErrorSimple;
    testcase0 "error.parse.object_comma" `ErrorSimple;
    testcase0 "error.parse.object_comprehension_local_clash" `ErrorSimple;
    (*
    testcase0 "error.parse.object_local_clash" `ErrorSimple;
    *)
    testcase0 "error.parse.self_in_computed_field" `ErrorSimple;
    testcase0 "error.parse.static_error_bad_number" `ErrorSimple;
    (*
    testcase0 "error.parse.string.invalid_escape" `ErrorSimple;
    testcase0 "error.parse.string.invalid_escape_unicode_non_hex" `ErrorSimple;
    *)
    testcase0 "error.parse.string.invalid_escape_unicode_short" `ErrorSimple;
    (*
    testcase0 "error.parse.string.invalid_escape_unicode_short2" `ErrorSimple;
    *)
    testcase0 "error.parse.string.invalid_escape_unicode_short3" `ErrorSimple;
    testcase0 "error.parse.string.unfinished" `ErrorSimple;
    testcase0 "error.parse.string.unfinished2" `ErrorSimple;
    testcase0 "error.parse.string_multi_no_newline" `ErrorSimple;
    (*
    testcase0 "error.parse.text_block_bad_whitespace" `ErrorSimple;
    *)
    testcase0 "error.parse.text_block_eof" `ErrorSimple;
    (*
    testcase0 "error.parse.text_block_not_terminated" `ErrorSimple;
    *)
    testcase0 "error.parse_json" `ErrorSimple;
    (*
    testcase0 "error.recursive_function_nonterm" `ErrorSimple;
    testcase0 "error.recursive_import" `ErrorSimple;
    testcase0 "error.recursive_object_non_term" `ErrorSimple;
    *)
    testcase0 "error.sanity" `ErrorSimple;
    testcase0 "error.static_error_self" `ErrorSimple;
    testcase0 "error.static_error_super" `ErrorSimple;
    testcase0 "error.static_error_var_not_exist" `ErrorSimple;
    testcase0 "error.std_join_types1" `ErrorSimple;
    testcase0 "error.std_join_types2" `ErrorSimple;
    (*
    testcase0 "error.std_makeArray_negative" `ErrorSimple;
    *)
    testcase0 "error.top_level_func" `ErrorSimple;
    testcase0 "error.trace_one_param" `ErrorSimple;
    (*
    testcase0 "error.trace_three_param" `ErrorSimple;
    *)
    testcase0 "error.trace_two_param" `ErrorSimple;
    testcase0 "error.trace_zero_param" `ErrorSimple;
    testcase0 "error.verbatim_import" `ErrorSimple;
    testcase0 "error.wrong_type" `ErrorSimple;
  ]

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let open OUnit2 in
  run_test_tt_main ("haskell backend w cpp jsonnet testcases" >::: suite)
