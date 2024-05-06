open Ppxlib

let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run file_path bundle_path show_profile work_dir_prefix =
  match Loader.load_root file_path with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  | Ok t -> (
      let compiled = Loader.compile t in
      match
        Executor.(
          execute
            (make_config ~bundle_path ~mode:`Bytecode ~interactive_compile:false
               ~interactive_execute:true ~show_profile ?work_dir_prefix
               ~remove_work_dir:(Option.is_none work_dir_prefix)
               ()))
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

let compile file_path target =
  let target =
    match target with Some "stdjsonnet" -> `Stdjsonnet | _ -> `Main
  in
  match Loader.load_root file_path with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  | Ok t ->
      t |> Loader.compile ~target |> Pprintast.structure Format.std_formatter
