open Jitsonnet

let read_all file_path =
  let ic = open_in_bin file_path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () -> In_channel.input_all ic)

let assert_compile' ~compiler ?(test_cases_dir = "../../../test/cases")
    ?(expected_suffix = ".expected") ?(multi = false) ?(string = false)
    ?(ext_codes = []) ?(ext_strs = []) src_file_path result_pat =
  let input_file_path =
    Filename.concat test_cases_dir (src_file_path ^ ".jsonnet")
  in
  let expected_path =
    Filename.concat test_cases_dir (src_file_path ^ expected_suffix)
  in
  match Loader.load_root ~ext_codes ~ext_strs input_file_path with
  | Error msg ->
      Logs.err (fun m ->
          m "assert_compile_expr: failed to load: %s: %s" input_file_path msg);
      assert false
  | Ok t -> (
      let multi_output_dir =
        if multi then Some (Filename.temp_dir "jitsonnet_" "") else None
      in
      match compiler ~multi_output_dir ~t ~string with
      | Unix.WEXITED 0, got, _ -> (
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
          | `Error -> assert false)
      | Unix.WEXITED _, _, got -> (
          match result_pat with
          | `Success | `SuccessSimple ->
              Logs.err (fun m -> m "failed to execute: '%s'" got);
              assert false
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
              | _ -> ()))
      | status, stdout_content, stderr_content ->
          Logs.err (fun m ->
              m "unexpected error: %s:\n%s\n%s"
                (Executor.string_of_process_status status)
                stdout_content stderr_content);
          assert false
      | exception Executor.Compilation_failed msg ->
          Logs.err (fun m -> m "failed to compile: '%s'" msg);
          assert false
      | exception Executor.Execution_failed msg ->
          Logs.err (fun m -> m "failed to execute: '%s'" msg);
          assert false)
