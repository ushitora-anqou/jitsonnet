let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

let run file_path =
  let ic = open_in_bin file_path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let lex = Lexing.from_channel ic in
  Lexing.set_filename lex file_path;

  let format_error fmt =
    let pos = lex.Lexing.lex_start_p in
    errf ("%s:%d:%d: " ^^ fmt) pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in

  match Parser.toplevel Lexer.main lex with
  | exception Lexer.Unexpected_char c ->
      format_error "lexer: unexpected char: %c" c
  | exception Parser.Error -> format_error "parser: syntax error"
  | None -> Ok ()
  | Some _ -> Ok ()
