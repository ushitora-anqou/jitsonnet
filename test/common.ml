open Jitsonnet

let read_all file_path =
  let ic = open_in_bin file_path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> In_channel.input_all ic)

let assert_compile' ~loader_optimize ~compiler ?test_cases_dir
    ?(expected_suffix = ".expected") ?(multi = false) ?(string = false)
    ?(tla_codes = []) ?(tla_strs = []) ?(ext_codes = []) ?(ext_strs = [])
    src_file_path result_pat =
  let input_file_path =
    (test_cases_dir |> Option.fold ~none:Fun.id ~some:Filename.concat)
      (src_file_path ^ ".jsonnet")
  in
  let expected_path =
    (test_cases_dir |> Option.fold ~none:Fun.id ~some:Filename.concat)
      (src_file_path ^ expected_suffix)
  in
  match
    Loader.load_root ~optimize:loader_optimize ~tla_codes ~tla_strs ~ext_codes
      ~ext_strs (`File input_file_path)
  with
  | Error _ when result_pat = `ErrorSimple -> ()
  | Error msg ->
      Logs.err (fun m ->
          m "assert_compile_expr: failed to load: %s: %s" input_file_path msg);
      assert false
  | Ok t -> (
      let multi_output_dir =
        if multi then Some (Filename.temp_dir "jitsonnet_" "") else None
      in
      match compiler ~multi_output_dir ~t ~string with
      | Unix.WEXITED 0, got_stdout, got_stderr -> (
          let got = got_stderr ^ got_stdout in
          match result_pat with
          | `Success when multi ->
              let exit_code =
                Sys.command
                  (Filename.quote_command "diff"
                     [ "-r"; Option.get multi_output_dir; expected_path ])
              in
              Alcotest.(check int) "" 0 exit_code;
              ()
          | `SuccessSimple -> ()
          | `Success ->
              let expected = read_all expected_path in
              Alcotest.(check string) "" expected got;
              ()
          | `ErrorSimple | `Error | `ErrorPrecise -> assert false)
      | Unix.WEXITED _, _, got -> (
          match result_pat with
          | `Success | `SuccessSimple ->
              Logs.err (fun m -> m "failed to execute: '%s'" got);
              assert false
          | `ErrorSimple -> ()
          | `Error -> (
              let expected = read_all expected_path |> String.trim in
              match Str.(search_forward (regexp expected) got 0) with
              | exception Not_found ->
                  Logs.err (fun m ->
                      m
                        "failed to find expected substring: expect '%s', got \
                         '%s'"
                        expected got);
                  assert false
              | _ -> ())
          | `ErrorPrecise ->
              let expected = read_all expected_path in
              (match
                 Str.(
                   search_forward
                     (regexp
                        "^RUNTIME ERROR:.*\n\
                         \\(\t[^\t]*\t\\(function <.*>\\)?\n\
                         ?\\)*")
                     expected 0)
               with
              | exception Not_found -> assert false
              | _ -> ());
              let expected = Str.matched_string expected in
              if String.trim got = String.trim expected then ()
              else (
                Logs.err (fun m ->
                    m "not equal to the expected string: expect '%s', got '%s'"
                      expected got);
                assert false))
      | status, stdout_content, stderr_content ->
          Logs.err (fun m ->
              m "unexpected error: %s:\n%s\n%s"
                (Executor.string_of_process_status status)
                stdout_content stderr_content);
          assert false
      | exception _ when result_pat = `ErrorSimple -> ()
      | exception Executor.Compilation_failed msg ->
          Logs.err (fun m -> m "failed to compile: '%s'" msg);
          assert false
      | exception Executor.Execution_failed msg ->
          Logs.err (fun m -> m "failed to execute: '%s'" msg);
          assert false)

let assert_compile_hs ?remove_work_dir ?(runtime_dir = "../../../runtime_hs")
    ?test_cases_dir ?expected_suffix ?multi ?string ?ext_codes ?ext_strs
    ?tla_codes ?tla_strs src_file_path result_pat =
  assert_compile' ?test_cases_dir ?expected_suffix ?multi ?ext_codes ?ext_strs
    ?tla_codes ?tla_strs ?string ~loader_optimize:false src_file_path result_pat
    ~compiler:(fun ~multi_output_dir ~t ~string ->
      let compiled = Loader.compile_haskell ?multi:multi_output_dir ~string t in
      Executor_hs.(
        execute
          (make_config ?remove_work_dir ~interactive_compile:true
             ~interactive_execute:false ~runtime_dir ())
          compiled))
