let string_of_token = function
  | P.AND -> "&"
  | ANDAND -> "&&"
  | ASSERT -> "assert"
  | BANG -> "!"
  | BANGEQ -> "!="
  | BAR -> "|"
  | BARBAR -> "||"
  | COLON -> ":"
  | COMMA -> ","
  | DOLLAR -> "$"
  | DOT -> "."
  | DOUBLECOLONS -> "::"
  | ELSE -> "else"
  | EOF -> "EOF"
  | EQ -> "="
  | EQEQ -> "=="
  | ERROR -> "error"
  | FALSE -> "false"
  | FOR -> "for"
  | FUNCTION -> "function"
  | GE -> ">="
  | GT -> ">"
  | GTGT -> ">>"
  | HAT -> "^"
  | ID s -> Printf.sprintf "%s" s
  | IF -> "if"
  | IMPORT -> "import"
  | IMPORTBIN -> "importbin"
  | IMPORTSTR -> "importstr"
  | IN -> "in"
  | LBRACE -> "{"
  | LBRACKET -> "["
  | LE -> "<="
  | LOCAL -> "local"
  | LPAREN -> "("
  | LT -> "<"
  | LTLT -> "<<"
  | MINUS -> "-"
  | NULL -> "null"
  | NUMBER f -> Printf.sprintf "%f" f
  | PERCENT -> "%"
  | PLUS -> "+"
  | RBRACE -> "}"
  | RBRACKET -> "]"
  | RPAREN -> ")"
  | SELF -> "self"
  | SEMICOLON -> ":"
  | SLASH -> "/"
  | STAR -> "*"
  | STRING s -> Printf.sprintf "\"%s\"" s
  | SUPER -> "super"
  | TAILSTRICT -> "tailstrict"
  | THEN -> "then"
  | TILDE -> "~"
  | TRIPLECOLONS -> ":::"
  | TRUE -> "true"

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

let parse_string str =
  let lex = Lexing.from_string ~with_positions:true str in
  parse_lex lex

let parse_file file_path =
  let ic = open_in_bin file_path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let lex = Lexing.from_channel ic in
  Lexing.set_filename lex file_path;
  parse_lex lex
