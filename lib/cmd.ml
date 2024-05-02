open Ppxlib

let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run file_path =
  match Loader.load_root file_path with
  | Error msg ->
      Logs.err (fun m -> m "%s" msg);
      exit 1
  | Ok t -> t |> Loader.compile |> Executor.execute_from_cli

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
