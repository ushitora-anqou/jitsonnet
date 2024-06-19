open Jitsonnet
open Common

let assert_compile_hs ?remove_work_dir ?(runtime_dir = "../../../runtime_hs")
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

let test_ok () =
  assert_compile_hs "success00" `Success;
  ()

let () =
  let open Alcotest in
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  run "haskell backend" [ ("custom", [ test_case "ok" `Quick test_ok ]) ]
