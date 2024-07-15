open Common

let assert_compile = assert_compile_hs ~test_cases_dir:"../../../test/cases"

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
  Logs.set_level (Some Logs.Info);
  let open OUnit2 in
  run_test_tt_main
    ("haskell backend"
    >::: [
           "custom ok" >:: test_custom_ok; "custom error" >:: test_custom_error;
         ])
