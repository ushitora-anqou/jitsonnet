let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run file_path =
  match Parser.parse_file file_path with
  | Ok _ -> ()
  | Error msg -> Logs.err (fun m -> m "%s" msg)
