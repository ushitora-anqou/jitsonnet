open Jitsonnet
open Common

let assert_compile ?remove_work_dir ?(runtime_dir = "../../../runtime_hs")
    ?test_cases_dir ?expected_suffix ?multi ?string ?ext_codes ?ext_strs
    src_file_path result_pat =
  assert_compile' ?test_cases_dir ?expected_suffix ?multi ?ext_codes ?ext_strs
    ?string src_file_path result_pat
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
  Unix.chdir "../../../thirdparty/go-jsonnet";
  Fun.protect ~finally:(fun () -> Unix.chdir saved_wd) @@ fun () ->
  assert_compile ~test_cases_dir:"testdata" ~expected_suffix:".golden" ?multi
    ?string ?ext_codes ?ext_strs ~runtime_dir:"../../runtime_hs" src_file_path
    result_pat

let testcase0 src_file_path result_path =
  let open OUnit2 in
  src_file_path >:: fun _test_ctxt -> assert_compile src_file_path result_path

let suite =
  [
    testcase0 "argcapture_builtin_call" `Success;
    testcase0 "array_index1" `Success;
    testcase0 "array_index2" `Success;
    testcase0 "array_index3" `Success;
    testcase0 "array_index4" `Success;
    testcase0 "arrcomp" `Success;
    testcase0 "arrcomp2" `Success;
    testcase0 "arrcomp3" `Success;
    testcase0 "arrcomp4" `Success;
    testcase0 "arrcomp6" `Success;
    testcase0 "arrcomp7" `Success;
    testcase0 "arrcomp_if" `Success;
    testcase0 "arrcomp_if2" `Success;
    testcase0 "arrcomp_if3" `Success;
    testcase0 "arrcomp_if5" `Success;
    testcase0 "assert2" `Success;
    testcase0 "assert_equal" `Success;
    testcase0 "assert_equal2" `Success;
    testcase0 "assert_equal3" `Success;
    testcase0 "binaryNot" `Success;
    testcase0 "bitwise_and" `Success;
    testcase0 "bitwise_and2" `Success;
    testcase0 "bitwise_and5" `Success;
    testcase0 "bitwise_and6" `Success;
    testcase0 "bitwise_or" `Success;
    testcase0 "bitwise_or2" `Success;
    testcase0 "bitwise_or3" `Success;
    testcase0 "bitwise_or4" `Success;
    testcase0 "bitwise_or5" `Success;
    testcase0 "bitwise_or6" `Success;
    testcase0 "bitwise_or7" `Success;
    testcase0 "bitwise_or8" `Success;
    (*testcase0 "bitwise_or9" `Success;*)
    testcase0 "bitwise_shift" `Success;
    testcase0 "bitwise_shift2" `Success;
    testcase0 "bitwise_shift3" `Success;
    (*testcase0 "bitwise_shift5" `Success;*)
    testcase0 "bitwise_xor" `Success;
    testcase0 "bitwise_xor2" `Success;
    testcase0 "bitwise_xor3" `Success;
    testcase0 "bitwise_xor4" `Success;
    testcase0 "bitwise_xor5" `Success;
    testcase0 "bitwise_xor6" `Success;
    testcase0 "bitwise_xor8" `Success;
    testcase0 "bitwise_xor9" `Success;
    testcase0 "block_escaping" `Success;
    testcase0 "block_string" `Success;
    testcase0 "boolean_literal" `Success;
    testcase0 "builtinAvg" `Success;
    testcase0 "builtinBase64" `Success;
    testcase0 "builtinBase64Decode" `Success;
    testcase0 "builtinBase64DecodeBytes" `Success;
    testcase0 "builtinBase64_byte_array" `Success;
    testcase0 "builtinChar" `Success;
    testcase0 "builtinChar2" `Success;
    testcase0 "builtinChar4" `Success;
    testcase0 "builtinChar6" `Success;
    testcase0 "builtinContains" `Success;
    testcase0 "builtinContains2" `Success;
    testcase0 "builtinEqualsIgnoreCase" `Success;
    testcase0 "builtinEqualsIgnoreCase2" `Success;
    testcase0 "builtinIsDecimal" `Success;
    testcase0 "builtinIsDecimal2" `Success;
    testcase0 "builtinIsEmpty" `Success;
    testcase0 "builtinIsEmpty1" `Success;
    testcase0 "builtinIsEven" `Success;
    testcase0 "builtinIsEven2" `Success;
    testcase0 "builtinIsInteger" `Success;
    testcase0 "builtinIsInteger2" `Success;
    testcase0 "builtinIsOdd" `Success;
    testcase0 "builtinIsOdd2" `Success;
    testcase0 "builtinManifestJsonEx" `Success;
    testcase0 "builtinMaxArray" `Success;
    testcase0 "builtinMinArray" `Success;
    testcase0 "builtinObjectFieldsEx" `Success;
    testcase0 "builtinObjectFieldsExWithHidden" `Success;
    testcase0 "builtinObjectHasEx" `Success;
    testcase0 "builtinObjectRemoveKey" `Success;
    testcase0 "builtinRemove" `Success;
    testcase0 "builtinRemoveAt" `Success;
    testcase0 "builtinReverse" `Success;
    testcase0 "builtinReverse_empty" `Success;
    testcase0 "builtinReverse_many" `Success;
    testcase0 "builtinReverse_single" `Success;
    testcase0 "builtinRound" `Success;
    (*
    testcase0 "builtinSha1" `Success;
    testcase0 "builtinSha256" `Success;
    testcase0 "builtinSha3" `Success;
    testcase0 "builtinSha512" `Success;
    *)
    testcase0 "builtinSubStr_length_larger" `Success;
    testcase0 "builtinSubStr_start_larger_then_size" `Success;
    testcase0 "builtinSubstr" `Success;
    testcase0 "builtinSum" `Success;
    testcase0 "builtinTrim" `Success;
    testcase0 "builtinTrim1" `Success;
    testcase0 "builtinTrim2" `Success;
    testcase0 "builtinTrim3" `Success;
    testcase0 "builtinXnor" `Success;
    testcase0 "builtinXnor1" `Success;
    testcase0 "builtinXor" `Success;
    testcase0 "builtinXor1" `Success;
    (*
    testcase0 "builtin_acos" `Success;
    testcase0 "builtin_asin" `Success;
    testcase0 "builtin_atan" `Success;
    testcase0 "builtin_ceil" `Success;
    (*testcase0 "builtin_cos" `Success;*)
    testcase0 "builtin_exp" `Success;
    testcase0 "builtin_exp2" `Success;
    testcase0 "builtin_exp4" `Success;
    testcase0 "builtin_exp6" `Success;
    testcase0 "builtin_exp7" `Success;
    testcase0 "builtin_exp8" `Success;
    testcase0 "builtin_floor" `Success;
    testcase0 "builtin_log" `Success;
    testcase0 "builtin_log2" `Success;
    (*testcase0 "builtin_log3" `Success;*)
    testcase0 "builtin_log4" `Success;
    testcase0 "builtin_log6" `Success;
    *)
    testcase0 "builtin_lstripChars" `Success;
    (*testcase0 "builtin_manifestTomlEx" `Success;*)
    testcase0 "builtin_member_array" `Success;
    testcase0 "builtin_member_string" `Success;
    testcase0 "builtin_parseInt" `Success;
    testcase0 "builtin_parseInt2" `Success;
    testcase0 "builtin_rstripChars" `Success;
    (*
    testcase0 "builtin_sin" `Success;
    testcase0 "builtin_sqrt" `Success;
    *)
    testcase0 "builtin_stripChars" `Success;
    testcase0 "builtin_substr_multibyte" `Success;
    (*
    testcase0 "builtin_tan" `Success;
    *)
    testcase0 "comparisons" `Success;
    (*testcase0 "decodeUTF8" `Success;*)
    testcase0 "div1" `Success;
    testcase0 "div2" `Success;
    (*testcase0 "div3" `Success;*)
    testcase0 "dollar_end" `Success;
    testcase0 "dollar_end2" `Success;
    testcase0 "empty_array" `Success;
    testcase0 "empty_object" `Success;
    testcase0 "empty_object_comp" `Success;
    (*testcase0 "encodeUTF8" `Success;*)
    testcase0 "equals" `Success;
    testcase0 "equals2" `Success;
    testcase0 "equals3" `Success;
    testcase0 "equals4" `Success;
    testcase0 "equals5" `Success;
    testcase0 "equals6" `Success;
    testcase0 "escaped_fields" `Success;
    testcase0 "escaped_single_quote" `Success;
    (*
    testcase0 ~ext_codes:[ "codeVar=3+3" ] "extvar_code" `Success;
    testcase0
      ~ext_codes:
        [
          {|mutuallyRecursiveVar1=[42, std.extVar("mutuallyRecursiveVar2")[0] + 1]|};
          {|mutuallyRecursiveVar2=[42, std.extVar("mutuallyRecursiveVar1")[0] + 1]|};
        ]
      "extvar_mutually_recursive" `Success;
    testcase0
      ~ext_codes:
        [ {|selfRecursiveVar=[42, std.extVar("selfRecursiveVar")[0] + 1]|} ]
      "extvar_self_recursive" `Success;
    testcase0 ~ext_strs:[ {|stringVar=2 + 2|} ] "extvar_string" `Success;
    *)
    testcase0 "false" `Success;
    testcase0 "filled_thunk" `Success;
    testcase0 "foldl_empty" `Success;
    testcase0 "foldl_single_element" `Success;
    testcase0 "foldl_string" `Success;
    testcase0 "foldl_various" `Success;
    testcase0 "foldr_empty" `Success;
    testcase0 "foldr_single_element" `Success;
    testcase0 "foldr_string" `Success;
    testcase0 "foldr_various" `Success;
    testcase0 "function_capturing" `Success;
    testcase0 "function_in_object" `Success;
    (*testcase0 "function_no_params" `Success;*)
    testcase0 "function_with_argument" `Success;
    testcase0 "greater" `Success;
    testcase0 "greaterEq" `Success;
    testcase0 "greaterEq2" `Success;
    testcase0 "ifthen_false" `Success;
    testcase0 "ifthenelse_false" `Success;
    testcase0 "ifthenelse_true" `Success;
    testcase0 "import2" `Success;
    testcase0 "import3" `Success;
    testcase0 "import4" `Success;
    testcase0 "import_twice" `Success;
    testcase0 "import_various_literals" `Success;
    testcase0 "importbin_nonutf8" `Success;
    testcase0 "in" `Success;
    testcase0 "in2" `Success;
    testcase0 "in3" `Success;
    testcase0 "in4" `Success;
    testcase0 "insuper" `Success;
    testcase0 "insuper2" `Success;
    testcase0 "insuper3" `Success;
    testcase0 "insuper5" `Success;
    testcase0 "insuper7" `Success;
    testcase0 "lazy" `Success;
    testcase0 "lazy_operator1" `Success;
    testcase0 "less" `Success;
    testcase0 "lessEq" `Success;
    testcase0 "lessEq2" `Success;
    testcase0 "local_in_object_assertion" `Success;
    testcase0 "local_within_nested_object" `Success;
    testcase0 "local_within_object" `Success;
    testcase0 "method_call" `Success;
    testcase0 "modulo" `Success;
    (*testcase0 "modulo4" `Success;*)
    testcase0 "modulo5" `Success;
    testcase0 "modulo6" `Success;
    testcase0 "modulo7" `Success;
    testcase0 "mult" `Success;
    testcase0 "mult2" `Success;
    testcase0 "mult3" `Success;
    testcase0 "numeric_literal" `Success;
    testcase0 "obj_local_right_level" `Success;
    testcase0 "obj_local_right_level2" `Success;
    testcase0 "obj_local_right_level3" `Success;
    testcase0 "object_comp" `Success;
    testcase0 "object_comp2" `Success;
    testcase0 "object_comp3" `Success;
    testcase0 "object_comp4" `Success;
    testcase0 "object_comp_dollar" `Success;
    testcase0 "object_comp_dollar2" `Success;
    testcase0 "object_comp_dollar3" `Success;
    testcase0 "object_comp_if" `Success;
    testcase0 "object_comp_local" `Success;
    testcase0 "object_comp_local2" `Success;
    testcase0 "object_comp_local3" `Success;
    testcase0 "object_comp_super" `Success;
    testcase0 "object_hidden" `Success;
    testcase0 "object_invariant" `Success;
    testcase0 "object_invariant12" `Success;
    testcase0 "object_invariant3" `Success;
    testcase0 "object_invariant4" `Success;
    testcase0 "object_invariant5" `Success;
    testcase0 "object_invariant6" `Success;
    testcase0 "object_invariant_perf" `Success;
    testcase0 "object_invariant_plus3" `Success;
    testcase0 "object_invariant_plus4" `Success;
    testcase0 "object_invariant_plus5" `Success;
    testcase0 "object_invariant_plus7" `Success;
    testcase0 "object_literal_in_array_comp" `Success;
    testcase0 "object_literal_in_object_comp" `Success;
    testcase0 "object_local_from_parent" `Success;
    testcase0 "object_local_from_parent_through_local" `Success;
    testcase0 "object_local_recursive" `Success;
    (*testcase0 "object_local_self_super" `Success;*)
    testcase0 "object_local_uses_local_from_outside" `Success;
    testcase0 "object_sum" `Success;
    testcase0 "object_sum2" `Success;
    testcase0 "object_sum3" `Success;
    testcase0 "object_super" `Success;
    testcase0 "object_super_deep" `Success;
    testcase0 "object_super_within" `Success;
    testcase0 "object_various_field_types" `Success;
    testcase0 "object_within_object" `Success;
    testcase0 "optional_args" `Success;
    testcase0 "optional_args10" `Success;
    testcase0 "optional_args12" `Success;
    testcase0 "optional_args14" `Success;
    testcase0 "optional_args15" `Success;
    testcase0 "optional_args16" `Success;
    testcase0 "optional_args17" `Success;
    testcase0 "optional_args18" `Success;
    testcase0 "optional_args19" `Success;
    testcase0 "optional_args2" `Success;
    testcase0 "optional_args20" `Success;
    testcase0 "optional_args21" `Success;
    testcase0 "optional_args22" `Success;
    testcase0 "optional_args3" `Success;
    testcase0 "optional_args4" `Success;
    testcase0 "optional_args5" `Success;
    testcase0 "optional_args6" `Success;
    testcase0 "optional_args7" `Success;
    testcase0 "or3" `Success;
    testcase0 "overriding_stdlib_desugared" `Success;
    (*
    testcase0 "parseJson" `Success;
    testcase0 "parseYaml" `Success;
    *)
    testcase0 "percent_format_float" `Success;
    testcase0 "percent_format_str" `Success;
    testcase0 "percent_format_str2" `Success;
    testcase0 "percent_format_str3" `Success;
    testcase0 "percent_format_str8" `Success;
    testcase0 "percent_mod_int" `Success;
    (*testcase0 "percent_mod_int2" `Success;*)
    testcase0 "percent_mod_int3" `Success;
    (*testcase0 "percent_mod_int4" `Success;*)
    testcase0 "percent_mod_int6" `Success;
    testcase0 "plus3" `Success;
    testcase0 "plus4" `Success;
    testcase0 "plus7" `Success;
    testcase0 "plus8" `Success;
    testcase0 "plus9" `Success;
    testcase0 "positional_after_optional" `Success;
    testcase0 "pow" `Success;
    testcase0 "pow2" `Success;
    testcase0 "pow3" `Success;
    testcase0 "pow5" `Success;
    (*testcase0 "pow6" `Success;*)
    testcase0 "proto_object_comp" `Success;
    testcase0 "recursive_local" `Success;
    testcase0 "self" `Success;
    testcase0 "simple_arith1" `Success;
    testcase0 "simple_arith2" `Success;
    testcase0 "simple_arith3" `Success;
    testcase0 "simple_arith_string" `Success;
    testcase0 "simple_arith_string2" `Success;
    testcase0 "simple_arith_string3" `Success;
    testcase0 "simple_arith_string_empty" `Success;
    testcase0 "slice" `Success;
    testcase0 "slice2" `Success;
    testcase0 "slice3" `Success;
    testcase0 "slice4" `Success;
    testcase0 "slice5" `Success;
    testcase0 "slice6" `Success;
    testcase0 "slice7" `Success;
    testcase0 "stackbug-regression-test" `Success;
    testcase0 "std.codepoint" `Success;
    testcase0 "std.codepoint2" `Success;
    testcase0 "std.codepoint4" `Success;
    testcase0 "std.codepoint5" `Success;
    (*
    testcase0 "std.exponent" `Success;
    testcase0 "std.exponent2" `Success;
    testcase0 "std.exponent3" `Success;
    testcase0 "std.exponent4" `Success;
    testcase0 "std.exponent5" `Success;
    testcase0 "std.exponent6" `Success;
    testcase0 "std.exponent7" `Success;
    *)
    testcase0 "std.filter" `Success;
    testcase0 "std.filter3" `Success;
    testcase0 "std.filter7" `Success;
    testcase0 "std.flatmap" `Success;
    testcase0 "std.flatmap2" `Success;
    testcase0 "std.flatmap3" `Success;
    testcase0 "std.flatmap4" `Success;
    testcase0 "std.flatmap6" `Success;
    testcase0 "std.join" `Success;
    testcase0 "std.join2" `Success;
    testcase0 "std.join3" `Success;
    testcase0 "std.join4" `Success;
    testcase0 "std.join5" `Success;
    testcase0 "std.join6" `Success;
    testcase0 "std" `Success;
    testcase0 "std.length" `Success;
    testcase0 "std.length_array" `Success;
    testcase0 "std.length_function" `Success;
    testcase0 "std.length_object" `Success;
    testcase0 "std.length_object_sum" `Success;
    testcase0 "std.length_object_with_hidden" `Success;
    testcase0 "std.length_string" `Success;
    testcase0 "std.lstripChars.multibyte" `Success;
    testcase0 "std.makeArray" `Success;
    testcase0 "std.makeArrayNamed" `Success;
    testcase0 "std.makeArrayNamed2" `Success;
    testcase0 "std.makeArrayNamed4" `Success;
    testcase0 "std.makeArray_recursive" `Success;
    (*
    testcase0 "std.mantissa" `Success;
    testcase0 "std.mantissa2" `Success;
    (*testcase0 "std.mantissa3" `Success;*)
    testcase0 "std.mantissa4" `Success;
    testcase0 "std.mantissa5" `Success;
    testcase0 "std.mantissa6" `Success;
    testcase0 "std.mantissa7" `Success;
    testcase0 "std.md5" `Success;
    testcase0 "std.md5_2" `Success;
    testcase0 "std.md5_3" `Success;
    testcase0 "std.md5_4" `Success;
    testcase0 "std.md5_5" `Success;
    *)
    testcase0 "std.mod_int" `Success;
    testcase0 "std.mod_string" `Success;
    testcase0 "std.modulo" `Success;
    testcase0 "std.objectFields" `Success;
    testcase0 "std.objectHasEx" `Success;
    testcase0 "std.objectHasEx2" `Success;
    testcase0 "std.objectHasEx3" `Success;
    testcase0 "std.objectHasEx4" `Success;
    testcase0 "std.primitiveEquals" `Success;
    testcase0 "std.primitiveEquals11" `Success;
    testcase0 "std.primitiveEquals12" `Success;
    testcase0 "std.primitiveEquals14" `Success;
    testcase0 "std.primitiveEquals15" `Success;
    testcase0 "std.primitiveEquals16" `Success;
    testcase0 "std.primitiveEquals17" `Success;
    testcase0 "std.primitiveEquals18" `Success;
    testcase0 "std.primitiveEquals19" `Success;
    testcase0 "std.primitiveEquals2" `Success;
    testcase0 "std.primitiveEquals20" `Success;
    testcase0 "std.primitiveEquals21" `Success;
    testcase0 "std.primitiveEquals3" `Success;
    testcase0 "std.primitiveEquals4" `Success;
    testcase0 "std.primitiveEquals5" `Success;
    testcase0 "std.primitiveEquals8" `Success;
    testcase0 "std.rstripChars.multibyte" `Success;
    testcase0 "std.slice" `Success;
    testcase0 "std.sort" `Success;
    testcase0 "std.sort2" `Success;
    (*
    testcase0 "std.thisFile" `Success;
    testcase0 "std.thisFile2" `Success;
    *)
    testcase0 "std.toString" `Success;
    testcase0 "std.toString2" `Success;
    testcase0 "std.toString3" `Success;
    testcase0 "std.toString4" `Success;
    testcase0 "std.toString6" `Success;
    testcase0 "std.toString7" `Success;
    testcase0 "std.toString8" `Success;
    testcase0 "std_in_local" `Success;
    testcase0 "std_substr" `Success;
    (*testcase0 "stdlib_smoke_test" `Success;*)
    testcase0 "strReplace" `Success;
    testcase0 "strReplace2" `Success;
    testcase0 "string2" `Success;
    testcase0 "string_comparison1" `Success;
    testcase0 "string_comparison2" `Success;
    testcase0 "string_comparison3" `Success;
    testcase0 "string_comparison4" `Success;
    testcase0 "string_comparison5" `Success;
    testcase0 "string_comparison6" `Success;
    testcase0 "string_comparison7" `Success;
    testcase0 "string_index" `Success;
    testcase0 "string_index2" `Success;
    testcase0 "string_to_bool" `Success;
    testcase0 "supersugar" `Success;
    testcase0 "supersugar2" `Success;
    testcase0 "supersugar3" `Success;
    testcase0 "supersugar4" `Success;
    testcase0 "supersugar5" `Success;
    testcase0 "supersugar6" `Success;
    testcase0 "supersugar7" `Success;
    testcase0 "supersugar9" `Success;
    testcase0 "tailstrict" `Success;
    testcase0 "tailstrict4" `Success;
    testcase0 "tailstrict5" `Success;
    testcase0 "tailstrict_operator1" `Success;
    testcase0 "tailstrict_operator2" `Success;
    testcase0 "tailstrict_operator3" `Success;
    testcase0 "true" `Success;
    testcase0 "type_array" `Success;
    testcase0 "type_builtin_function" `Success;
    testcase0 "type_function" `Success;
    testcase0 "type_number" `Success;
    testcase0 "type_object" `Success;
    testcase0 "type_string" `Success;
    testcase0 "unary_minus" `Success;
    testcase0 "unary_minus2" `Success;
    testcase0 "unary_minus3" `Success;
    testcase0 "unicode" `Success;
    testcase0 "unicode2" `Success;
    testcase0 "use_object" `Success;
    testcase0 "use_object_in_object" `Success;
    testcase0 "variable" `Success;
    testcase0 "verbatim_string" `Success;
  ]

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  let open OUnit2 in
  run_test_tt_main ("haskell backend w go jsonnet testcases" >::: suite)
