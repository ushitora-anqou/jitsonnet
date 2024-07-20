let string_of_token = function
  | P.AND -> "&"
  | ANDAND -> "ANDAND"
  | ASSERT -> "ASSERT"
  | BANG -> "BANG"
  | BANGEQ -> "BANGEQ"
  | BAR -> "BAR"
  | BARBAR -> "BARBAR"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | DOLLAR -> "DOLLAR"
  | DOT -> "DOT"
  | ELSE -> "ELSE"
  | EOF -> "EOF"
  | EQ -> "EQ"
  | EQEQ -> "EQEQ"
  | ERROR -> "ERROR"
  | FALSE -> "FALSE"
  | FOR -> "FOR"
  | FUNCTION -> "FUNCTION"
  | GE -> "GE"
  | GT -> "GT"
  | GTGT -> "GTGT"
  | HAT -> "HAT"
  | ID s -> Printf.sprintf "ID(\"%s\")" s
  | IF -> "IF"
  | IMPORT -> "IMPORT"
  | IMPORTBIN -> "IMPORTBIN"
  | IMPORTSTR -> "IMPORTSTR"
  | IN -> "IN"
  | LBRACE -> "LBRACE"
  | LBRACKET -> "LBRACKET"
  | LE -> "LE"
  | LOCAL -> "LOCAL"
  | LPAREN -> "LPAREN"
  | LT -> "LT"
  | LTLT -> "LTLT"
  | MINUS -> "MINUS"
  | NULL -> "NULL"
  | NUMBER f -> Printf.sprintf "NUMBER(%f)" f
  | PERCENT -> "PERCENT"
  | PLUS -> "PLUS"
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | RPAREN -> "RPAREN"
  | SELF -> "SELF"
  | SEMICOLON -> "SEMICOLON"
  | SLASH -> "SLASH"
  | STAR -> "STAR"
  | STRING s -> Printf.sprintf "STRING(\"%s\")" s
  | SUPER -> "SUPER"
  | TAILSTRICT -> "TAILSTRICT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TRUE -> "TRUE"

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
  | exception Syntax.General_parse_error msg ->
      format_error "parser: syntax error: %s" msg
  | x -> Ok x

let parse_string ?(filename = "") str =
  let lex = Lexing.from_string ~with_positions:true str in
  Lexing.set_filename lex filename;
  parse_lex lex

let parse_file file_path =
  let ic = open_in_bin file_path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let lex = Lexing.from_channel ic in
  Lexing.set_filename lex file_path;
  parse_lex lex
