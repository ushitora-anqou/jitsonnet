open Ppxlib

let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run file_path show_profile work_dir_prefix native mold
    (multi : string option) (string : bool) ext_codes ext_strs opam_lib
    lib_runtime haskell runtime_hs =
  match Loader.load_root ~ext_codes ~ext_strs file_path with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  | Ok t when haskell -> (
      let compiled = Loader.compile_haskell ?multi ~string t in
      match
        Executor_hs.(
          execute
            (make_config ~interactive_compile:false ~interactive_execute:true
               ~show_profile ?work_dir_prefix
               ~remove_work_dir:(Option.is_none work_dir_prefix)
               ~runtime_dir:runtime_hs ()))
          compiled
      with
      | WEXITED code, _, _ -> exit code
      | status, _, _ ->
          Logs.err (fun m ->
              m "unexpected error: %s"
                (Executor.string_of_process_status status));
          exit 1
      | exception Executor.Compilation_failed msg ->
          Logs.err (fun m -> m "BUG: compilation failed: %s" msg);
          exit 1
      | exception Executor.Execution_failed msg ->
          Logs.err (fun m -> m "BUG: execution failed: %s" msg);
          exit 1)
  | Ok t -> (
      let compiled = Loader.compile ?multi ~string t in
      match
        Executor.(
          execute
            (make_config
               ~mode:(if native then `Native else `Bytecode)
               ~interactive_compile:false ~interactive_execute:true
               ~show_profile ?work_dir_prefix
               ~remove_work_dir:(Option.is_none work_dir_prefix)
               ~opam_lib ~lib_runtime ?mold ()))
          compiled
      with
      | WEXITED code, _, _ -> exit code
      | status, _, _ ->
          Logs.err (fun m ->
              m "unexpected error: %s"
                (Executor.string_of_process_status status));
          exit 1
      | exception Executor.Compilation_failed msg ->
          Logs.err (fun m -> m "BUG: compilation failed: %s" msg);
          exit 1
      | exception Executor.Execution_failed msg ->
          Logs.err (fun m -> m "BUG: execution failed: %s" msg);
          exit 1)

let compile file_path target haskell =
  let target =
    match target with Some "stdjsonnet" -> `Stdjsonnet | _ -> `Main
  in
  match Loader.load_root ~is_stdjsonnet:(target = `Stdjsonnet) file_path with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  | Ok t when haskell -> t |> Loader.compile_haskell ~target |> print_string
  | Ok t ->
      t |> Loader.compile ~target |> Pprintast.structure Format.std_formatter
