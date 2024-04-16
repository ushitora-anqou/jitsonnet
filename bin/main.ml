let run file_path =
  match Jitsonnet.Cmd.run file_path with
  | Ok () -> ()
  | Error msg -> Logs.err (fun m -> m "%s" msg)

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Cmdliner.(
    Cmd.(
      v (info "jitsonnet")
        Term.(
          const run
          $ Arg.(required & pos 0 (some string) None & info ~docv:"FILE" []))
      |> eval))
  |> exit
