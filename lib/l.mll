{
  let keywords = [
    ("assert", P.Assert);
    ("else", P.Else);
    ("error", P.Error);
    ("false", P.False);
    ("for", P.For);
    ("function", P.Function);
    ("if", P.If);
    ("import", P.Import);
    ("importstr", P.Importstr);
    ("importbin", P.Importbin);
    ("in", P.In);
    ("local", P.Local);
    ("null", P.Null);
    ("tailstrict", P.Tailstrict);
    ("then", P.Then);
    ("self", P.Self);
    ("super", P.Super);
    ("true", P.True);
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
| ('0' | ['1'-'9'] ['0'-'9']*) ('.' ['0'-'9']+)? (['e' 'E'] ['-' '+']? ['0'-'9']+)? {
  P.Number (Lexing.lexeme lexbuf |> float_of_string)
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
