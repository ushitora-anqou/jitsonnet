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

let test_custom_ok _test_ctxt =
  assert_compile "success00" `Success;
  assert_compile ~multi:true ~string:true "success01_multi_string" `Success;
  assert_compile ~string:true "success02_string" `Success;
  ()

let test_custom_error _test_ctxt =
  assert_compile "error00" `Error;
  assert_compile "error01" `Error;
  assert_compile "error02" `Error;
  ()

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  let open OUnit2 in
  run_test_tt_main
    ("haskell backend"
    >::: [
           "custom ok" >:: test_custom_ok; "custom error" >:: test_custom_error;
         ])
