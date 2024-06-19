open Jitsonnet
open Common

let assert_compile ?(mode = `Bytecode) ?remove_work_dir
    ?(opam_lib = "../../../../_opam/lib")
    ?(lib_runtime = "../../../_build/default/lib_runtime") ?test_cases_dir
    ?expected_suffix ?multi ?string ?ext_codes ?ext_strs src_file_path
    result_pat =
  assert_compile' ?test_cases_dir ?expected_suffix ?multi ?ext_codes ?ext_strs
    ?string src_file_path result_pat
    ~compiler:(fun ~multi_output_dir ~t ~string ->
      let compiled = Loader.compile ?multi:multi_output_dir ~string t in
      Executor.(
        execute
          (make_config ~mode ?remove_work_dir ~interactive_compile:true
             ~interactive_execute:false ~opam_lib ~lib_runtime ())
          compiled))

let test_custom_error () =
  assert_compile "error00" `Error;
  assert_compile "error01" `Error;
  assert_compile "error02" `Error;
  ()

let test_custom_success () =
  assert_compile "success00" `Success;
  assert_compile ~mode:`Native "success00" `Success;
  assert_compile ~multi:true ~string:true "success01_multi_string" `Success;
  assert_compile ~string:true "success02_string" `Success;
  ()

let test_go_jsonnet_testcases () =
  let assert_compile ?multi ?string ?ext_codes ?ext_strs src_file_path
      result_pat =
    let saved_wd = Unix.getcwd () in
    Unix.chdir "../../../thirdparty/go-jsonnet";
    Fun.protect ~finally:(fun () -> Unix.chdir saved_wd) @@ fun () ->
    assert_compile ~test_cases_dir:"testdata" ~expected_suffix:".golden" ?multi
      ?string ?ext_codes ?ext_strs ~opam_lib:"../../../_opam/lib"
      ~lib_runtime:"../../_build/default/lib_runtime" src_file_path result_pat
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
  (*assert_compile "builtin_cos" `Success;*)
  assert_compile "builtin_exp" `Success;
  assert_compile "builtin_exp2" `Success;
  assert_compile "builtin_exp4" `Success;
  assert_compile "builtin_exp6" `Success;
  assert_compile "builtin_exp7" `Success;
  assert_compile "builtin_exp8" `Success;
  assert_compile "builtin_floor" `Success;
  assert_compile "builtin_log" `Success;
  assert_compile "builtin_log2" `Success;
  (*assert_compile "builtin_log3" `Success;*)
  assert_compile "builtin_log4" `Success;
  assert_compile "builtin_log6" `Success;
  assert_compile "builtin_lstripChars" `Success;
  (*assert_compile "builtin_manifestTomlEx" `Success;*)
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
  (*assert_compile "div3" `Success;*)
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
  assert_compile ~ext_codes:[ "codeVar=3+3" ] "extvar_code" `Success;
  assert_compile
    ~ext_codes:
      [
        {|mutuallyRecursiveVar1=[42, std.extVar("mutuallyRecursiveVar2")[0] + 1]|};
        {|mutuallyRecursiveVar2=[42, std.extVar("mutuallyRecursiveVar1")[0] + 1]|};
      ]
    "extvar_mutually_recursive" `Success;
  assert_compile
    ~ext_codes:
      [ {|selfRecursiveVar=[42, std.extVar("selfRecursiveVar")[0] + 1]|} ]
    "extvar_self_recursive" `Success;
  assert_compile ~ext_strs:[ {|stringVar=2 + 2|} ] "extvar_string" `Success;
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
  assert_compile "import_twice" `Success;
  assert_compile "import_various_literals" `Success;
  assert_compile "importbin_nonutf8" `Success;
  assert_compile "in" `Success;
  assert_compile "in2" `Success;
  assert_compile "in3" `Success;
  assert_compile "in4" `Success;
  assert_compile "insuper" `Success;
  assert_compile "insuper2" `Success;
  assert_compile "insuper3" `Success;
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
  assert_compile "numeric_literal" `Success;
  assert_compile "obj_local_right_level" `Success;
  assert_compile "obj_local_right_level2" `Success;
  assert_compile "obj_local_right_level3" `Success;
  assert_compile "object_comp" `Success;
  assert_compile "object_comp2" `Success;
  assert_compile "object_comp3" `Success;
  assert_compile "object_comp4" `Success;
  assert_compile "object_comp_dollar" `Success;
  assert_compile "object_comp_dollar2" `Success;
  assert_compile "object_comp_dollar3" `Success;
  assert_compile "object_comp_if" `Success;
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
  (*assert_compile "pow6" `Success;*)
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
  (*assert_compile "std.mantissa3" `Success;*)
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
  (*assert_compile "std.thisFile" `Success;*)
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
  (*assert_compile "stdlib_smoke_test" `Success;*)
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
  assert_compile "unicode" `Success;
  assert_compile "unicode2" `Success;
  assert_compile "use_object" `Success;
  assert_compile "use_object_in_object" `Success;
  assert_compile "variable" `Success;
  assert_compile "verbatim_string" `Success;

  assert_compile ~multi:true ~string:false "multi" `Success;
  ()

let test_jsonnet_testsuite () =
  let assert_compile ?multi ?string ?ext_codes ?ext_strs src_file_path
      result_pat =
    let saved_wd = Unix.getcwd () in
    Unix.chdir "../../../thirdparty/jsonnet";
    Fun.protect ~finally:(fun () -> Unix.chdir saved_wd) @@ fun () ->
    assert_compile ~test_cases_dir:"test_suite" ?multi ?string ?ext_codes
      ?ext_strs ~opam_lib:"../../../_opam/lib"
      ~lib_runtime:"../../_build/default/lib_runtime" src_file_path result_pat
      ~expected_suffix:".jsonnet.golden"
  in

  assert_compile "arith_bool" `SuccessSimple;
  assert_compile "arith_float" `SuccessSimple;
  assert_compile "arith_string" `SuccessSimple;
  assert_compile "array" `SuccessSimple;
  assert_compile "array_comparison" `Success;
  assert_compile "array_comparison2" `Success;
  assert_compile "assert" `SuccessSimple;
  assert_compile "binary" `SuccessSimple;
  assert_compile "comments" `SuccessSimple;
  assert_compile "condition" `SuccessSimple;
  assert_compile "dos_line_endings" `Success;
  assert_compile "fmt_idempotence_issue" `SuccessSimple;
  assert_compile "fmt_no_trailing_newline" `SuccessSimple;
  assert_compile "fmt_trailing_c_style" `SuccessSimple;
  assert_compile "fmt_trailing_multiple_comments" `SuccessSimple;
  assert_compile "fmt_trailing_newlines" `SuccessSimple;
  assert_compile "fmt_trailing_newlines2" `SuccessSimple;
  assert_compile "fmt_trailing_same_line_comment" `SuccessSimple;
  assert_compile "format" `SuccessSimple;
  assert_compile "formatter" `Success;
  assert_compile "formatting_braces" `Success;
  assert_compile "formatting_braces2" `Success;
  assert_compile "formatting_braces3" `SuccessSimple;
  assert_compile "functions" `SuccessSimple;
  assert_compile "import" `SuccessSimple;
  (*
  assert_compile "import_sorting" `SuccessSimple;
  assert_compile "import_sorting_by_filename" `SuccessSimple;
  assert_compile "import_sorting_crazy" `SuccessSimple;
  assert_compile "import_sorting_function_sugar" `SuccessSimple;
  assert_compile "import_sorting_group_ends" `SuccessSimple;
  assert_compile "import_sorting_groups" `SuccessSimple;
  assert_compile "import_sorting_multiple_binds_and_comments" `SuccessSimple;
  assert_compile "import_sorting_multiple_in_local" `SuccessSimple;
  assert_compile "import_sorting_unicode" `SuccessSimple;
  assert_compile "import_sorting_with_license" `SuccessSimple;
  *)
  (*assert_compile "invariant" `SuccessSimple;*)
  assert_compile "invariant_manifest" `Success;
  assert_compile "local" `SuccessSimple;
  assert_compile "merge" `SuccessSimple;
  assert_compile "null" `SuccessSimple;
  (*
  assert_compile "object" `SuccessSimple;
  assert_compile "oop" `SuccessSimple;
  assert_compile "oop_extra" `SuccessSimple;
  *)
  assert_compile "parseJson_long_array_gc_test" `Success;
  assert_compile "parsing_edge_cases" `SuccessSimple;
  assert_compile "precedence" `SuccessSimple;
  assert_compile "recursive_function" `SuccessSimple;
  assert_compile "recursive_import_ok" `SuccessSimple;
  assert_compile "recursive_object" `SuccessSimple;
  assert_compile "sanity" `Success;
  assert_compile "sanity2" `Success;
  assert_compile "shebang" `SuccessSimple;
  assert_compile "slice.sugar" `SuccessSimple;
  assert_compile "std_all_hidden" `SuccessSimple;
  assert_compile "text_block" `SuccessSimple;
  (*
  assert_compile "tla.simple" `SuccessSimple;
  assert_compile "trace" `Success;
  *)
  assert_compile "unicode" `SuccessSimple;
  (*assert_compile "unicode_bmp" `Success;*)
  assert_compile "unix_line_endings" `Success;
  (*assert_compile "unparse" `Success;*)
  assert_compile "verbatim_strings" `SuccessSimple;
  (*assert_compile "stdlib" `Success;*)
  ()

let () =
  let open Alcotest in
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  run "ocaml backend"
    [
      ( "custom",
        [
          test_case "success" `Quick test_custom_success;
          test_case "error" `Quick test_custom_error;
        ] );
      ( "go-jsonnet testcases",
        [ test_case "ok" `Quick test_go_jsonnet_testcases ] );
      ("jsonnet testsuite", [ test_case "ok" `Quick test_jsonnet_testsuite ]);
    ]
