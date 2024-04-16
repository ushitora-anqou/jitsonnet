{
  module P = Parser

  let keywords = [
  ]

  exception Unexpected_char of char
}

rule main = parse
|  [' ' '\009' '\012']+ {
  main lexbuf
}
| '\n' {
  Lexing.new_line lexbuf;
  main lexbuf
}
| "-"? ['0'-'9']+ {
  P.IntLiteral (int_of_string (Lexing.lexeme lexbuf))
}
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']* {
  let id = Lexing.lexeme lexbuf in
  match List.assoc_opt id keywords with
  | Some x -> x
  | None -> P.ID id
}
| eof {
  P.EOF
}
| _ as c {
  raise (Unexpected_char c)
}
