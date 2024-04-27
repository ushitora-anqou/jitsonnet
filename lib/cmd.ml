let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run file_path =
  match Parser.parse_file file_path with
  | Error msg -> Logs.err (fun m -> m "%s" msg)
  | Ok { expr } -> (
      let desugared = Syntax.desugar_expr false expr in
      match Static_check.f desugared with
      | Error msg -> Logs.err (fun m -> m "%s" msg)
      | Ok () -> desugared |> Compiler.compile |> Executor.execute_from_cli)
