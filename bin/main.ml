let run = Jitsonnet.Cmd.run
let compile = Jitsonnet.Cmd.compile

let () =
  Fmt.set_style_renderer Fmt.stderr `Ansi_tty;
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Debug);
  Cmdliner.(
    Cmd.(
      group (info "jitsonnet")
        [
          v (info "run")
            Term.(
              const run
              $ Arg.(required & pos 0 (some string) None & info ~docv:"FILE" [])
              $ Arg.(value & flag & info [ "profile" ])
              $ Arg.(value & opt (some string) None & info [ "work-dir" ])
              $ Arg.(value & flag & info [ "native" ])
              $ Arg.(value & opt (some string) None & info [ "mold" ])
              $ Arg.(value & opt (some string) None & info [ "m"; "multi" ])
              $ Arg.(value & flag & info [ "S"; "string" ])
              $ Arg.(value & opt_all string [] & info [ "ext-code" ])
              $ Arg.(value & opt_all string [] & info [ "V"; "ext-str" ])
              $ Arg.(
                  value & opt string ""
                  & info ~env:(Env.info "JITSONNET_OPAM_LIB") [ "opam-lib" ])
              $ Arg.(
                  value & opt string ""
                  & info
                      ~env:(Env.info "JITSONNET_LIB_RUNTIME")
                      [ "lib-runtime" ]));
          v (info "compile")
            Term.(
              const compile
              $ Arg.(required & pos 0 (some string) None & info ~docv:"FILE" [])
              $ Arg.(
                  value
                  & opt (some string) None
                  & info ~docv:"TARGET" [ "target" ]));
        ]
      |> eval))
  |> exit
