let string_of_token = function
  | P.Assert -> "Assert"
  | Else -> "Else"
  | Error -> "Error"
  | False -> "False"
  | For -> "For"
  | Function -> "Function"
  | If -> "If"
  | Import -> "Import"
  | Importstr -> "Importstr"
  | Importbin -> "Importbin"
  | In -> "In"
  | Local -> "Local"
  | Null -> "Null"
  | Tailstrict -> "Tailstrict"
  | Then -> "Then"
  | Self -> "Self"
  | Super -> "Super"
  | True -> "True"
  | EOF -> "EOF"
  | Number f -> Printf.sprintf "Number(%f)" f
  | ID s -> Printf.sprintf "ID(\"%s\")" s
  | String s -> Printf.sprintf "String(\"%s\")" s

let parse_lex lex =
  let format_error fmt =
    let pos = lex.Lexing.lex_start_p in
    Printf.ksprintf
      (fun s -> Error s)
      ("%s:%d:%d: " ^^ fmt) pos.pos_fname pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol + 1)
  in

  match P.toplevel L.main lex with
  | exception L.Unexpected_char c -> format_error "lexer: unexpected char: %c" c
  | exception P.Error -> format_error "parser: syntax error"
  | None -> Error "failed to parse"
  | Some x -> Ok x

let parse_string str =
  let lex = Lexing.from_string ~with_positions:true str in
  parse_lex lex

let parse_file file_path =
  let ic = open_in_bin file_path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let lex = Lexing.from_channel ic in
  Lexing.set_filename lex file_path;
  parse_lex lex
