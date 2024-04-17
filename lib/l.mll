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

  let is_string_literal_verbatim = ref false
  let string_literal_buffer = Buffer.create 0

  let add_unicode_char_to_buffer s =
    let v = "0x" ^ (String.sub s 2 (String.length s - 2)) |> int_of_string in
    let e = Uutf.encoder `UTF_8 (`Buffer string_literal_buffer) in
    Uutf.encode e (`Uchar (Uchar.of_int v)) |> ignore;
    Uutf.encode e `End |> ignore;
    ()

  let char_for_backslash = function
    | 'b' -> '\008'
    | 'f' -> '\012'
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 't' -> '\009'
    | c -> c
}

let newline = '\n' | '\r' | "\r\n"
let hexadecimal_unicode_escape =
  "\\u"
  ['0'-'9' 'a'-'f' 'A'-'F']
  ['0'-'9' 'a'-'f' 'A'-'F']
  ['0'-'9' 'a'-'f' 'A'-'F']
  ['0'-'9' 'a'-'f' 'A'-'F']

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
| ('@'? as c1) (('\'' | '"') as c2) {
  Buffer.clear string_literal_buffer;
  let () =
    match c1, c2 with
    | "", '"' -> double_quoted_string lexbuf
    | "", '\'' -> single_quoted_string lexbuf
    | _, '"' -> double_quoted_verbatim_string lexbuf
    | _, '\'' -> single_quoted_verbatim_string lexbuf
    | _ -> assert false (* unreachable *)
  in
  P.String (Buffer.contents string_literal_buffer)
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

and double_quoted_string = parse
| '"' {
  ()
}
| hexadecimal_unicode_escape {
  add_unicode_char_to_buffer (Lexing.lexeme lexbuf);
  double_quoted_string lexbuf
}
| '\\' ([^ 'u'] as c) {
  Buffer.add_char string_literal_buffer (char_for_backslash c);
  double_quoted_string lexbuf
}
| _ as c {
  Buffer.add_char string_literal_buffer c;
  double_quoted_string lexbuf
}

and single_quoted_string = parse
| '\'' {
  ()
}
| hexadecimal_unicode_escape {
  add_unicode_char_to_buffer (Lexing.lexeme lexbuf);
  single_quoted_string lexbuf
}
| '\\' ([^ 'u'] as c) {
  Buffer.add_char string_literal_buffer (char_for_backslash c);
  single_quoted_string lexbuf
}
| _ as c {
  Buffer.add_char string_literal_buffer c;
  single_quoted_string lexbuf
}

and double_quoted_verbatim_string = parse
| '"' {
  ()
}
| '"' '"' {
  Buffer.add_char string_literal_buffer '"';
  double_quoted_verbatim_string lexbuf
}
| _ as c {
  Buffer.add_char string_literal_buffer c;
  double_quoted_verbatim_string lexbuf
}

and single_quoted_verbatim_string = parse
| '\'' {
  ()
}
| '\'' '\'' {
  Buffer.add_char string_literal_buffer '\'';
  single_quoted_verbatim_string lexbuf
}
| _ as c {
  Buffer.add_char string_literal_buffer c;
  single_quoted_verbatim_string lexbuf
}
