{
  module P = Parser

  let keywords = [
  ]

  exception Unexpected_char of char
}

let newline = '\n' | '\r' | "\r\n"

rule main = parse
| [' ' '\t']+ {
  main lexbuf
}
| newline {
  Lexing.new_line lexbuf;
  main lexbuf
}
| "//" | '#' {
  line_comment lexbuf;
  main lexbuf
}
| "/*" {
  block_comment lexbuf;
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

and line_comment = parse
| newline {
  ()
}
| _ {
  line_comment lexbuf
}

and block_comment = parse
| "*/" {
  ()
}
| _ {
  block_comment lexbuf
}
