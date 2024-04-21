%{
%}

%token ASSERT COLON COMMA DOLLAR DOT DOUBLECOLONS ELSE EOF EQUAL ERROR FALSE FOR FUNCTION IF IMPORT IMPORTBIN IMPORTSTR IN LBRACE LBRACKET LOCAL LPAREN NULL PLUS RBRACE RBRACKET RPAREN SELF SEMICOLON SUPER TAILSTRICT THEN TRIPLECOLONS TRUE

%token <float> NUMBER
%token <string> ID
%token <string> STRING

(*
%nonassoc EOF
%nonassoc ASSERT COLON COMMA DOLLAR DOUBLECOLONS ELSE EQUAL ERROR FALSE FOR FUNCTION IF IMPORT IMPORTBIN IMPORTSTR IN LBRACE LBRACKET LOCAL LPAREN NULL PLUS RBRACE RBRACKET RPAREN SELF SEMICOLON SUPER TAILSTRICT THEN TRIPLECOLONS TRUE NUMBER ID STRING
%left DOT
*)

%start toplevel
%type <Syntax.program> toplevel
%%

(*
  Rule for the following:
  - /* nothing */
  - X
  - X sep X ... sep
  - X sep X ... sep X
*)
separated_list1(separator, X):
  | /* nothing */ {
    []
  }
  | x=X {
    [x]
  }
  | x=X; separator; xs=separated_list1(separator, X) {
    x :: xs
  }

(*
  Rule for the following:
  - /* nothing */
  - sep
  - sep X sep ... sep
  - sep X sep ... sep X
*)
separated_list2(separator, X):
  | /* nothing */ {
    []
  }
  | separator {
    []
  }
  | separator; x=X xs=separated_list2(separator, X) {
    x :: xs
  }

toplevel :
  | e=Expr EOF { Syntax.{ expr = e } }

Expr :
  | NULL {
    Syntax.Null
  }
  | TRUE {
    Syntax.True
  }
  | FALSE {
    Syntax.False
  }
  | SELF {
    Syntax.Self
  }
  | DOLLAR {
    Syntax.Dollar
  }
  | s=STRING {
    Syntax.String s
  }
  | i=NUMBER {
    Syntax.Number i
  }
  | LBRACE x=Objinside RBRACE {
    Syntax.Object x
  }
  | LBRACKET
      xs=separated_list1(COMMA, Expr)
      spec=option(forspec=Forspec compspec=Compspec { (forspec, compspec) })
    RBRACKET {
    match xs, spec with
    | [ x ], Some (forspec, compspec) ->
      Syntax.ArrayFor (x, forspec, compspec)
    | _, None ->
      Syntax.Array xs
    | _ ->
      raise (Syntax.General_parse_error "invalid array")
  }
  | e=Expr DOT id=ID {
    Syntax.Select (e, id)
  }

  (*
  | Expr LBRACKET option(Expr) option(COLON option(Expr) option(COLON option(Expr))) RBRACKET {
    Syntax.Index
  }
  | SUPER DOT ID {
    Syntax.SuperMember
  }
  | SUPER LBRACKET Expr RBRACKET {
    Syntax.SuperIndex
  }
  | Expr LPAREN option(Args) RPAREN {
    Syntax.Call
  }
  *)
  | id=ID {
    Syntax.Var id
  }
  (*
  | LOCAL separated_nonempty_list(COMMA, Bind) SEMICOLON Expr {
    Syntax.Local
  }
  | IF Expr THEN Expr option(ELSE Expr) {
    Syntax.If
  }
  | Expr Binaryop Expr {
    Syntax.Binary
  }
  | Unaryop Expr {
    Syntax.Unary
  }
  | Expr LBRACE Objinside RBRACE {
    Syntax.ObjSeq
  }
  | FUNCTION LPAREN Params RPAREN Expr {
    Syntax.Function
  }
  | Assert SEMICOLON Expr {
    Syntax.Assert
  }
  | IMPORT String {
    Syntax.Import
  }
  | IMPORTSTR String {
    Syntax.Importstr
  }
  | IMPORTBIN String {
    Syntax.Importbin
  }
  | ERROR Expr {
    Syntax.Error
  }
  | Expr IN SUPER {
    Syntax.InSuper
  }
  *)

Objinside :
  | x=Objinside0 {
    x
  }
  | x=Objinside1 {
    x
  }

Objinside0 :
  | /* nothing */ {
    Syntax.ObjectMemberList []
  }
  | x=Objlocal {
    Syntax.ObjectMemberList [MemberObjlocal x]
  }
  | x=Objlocal COMMA y=Objinside {
    match y with
    | Syntax.ObjectMemberList xs ->
      Syntax.ObjectMemberList (MemberObjlocal x :: xs)
    | Syntax.ObjectFor (xs, e1, e2, robjlocals, forspec, compspec) ->
      Syntax.ObjectFor (x :: xs, e1, e2, robjlocals, forspec, compspec)
  }

Objinside1 :
  | x=Assert xs=separated_list2(COMMA, Member) {
    Syntax.ObjectMemberList (MemberAssert x :: xs)
  }
  | x=FieldExceptBracket xs=separated_list2(COMMA, Member) {
    Syntax.ObjectMemberList (MemberField x :: xs)
  }
  | LBRACKET e1=Expr RBRACKET plus=option(PLUS) h=H e2=Expr ys=separated_list2(COMMA, Objlocal) post=Objinside2 {
    match plus, h, post with
    | None, Syntax.H 1, `For (forspec, compspec) ->
      Syntax.ObjectFor ([], e1, e2, ys, forspec, compspec)
    | _, _, `MemberList members ->
      let a = Syntax.MemberField (Field (FieldnameExpr e1, Option.is_some plus, h, e2)) in
      let bs = List.map (fun y -> Syntax.MemberObjlocal y) ys in
      Syntax.ObjectMemberList (a :: (bs @ members))
    | _ -> raise (Syntax.General_parse_error "invalid brackets")
  }

Objinside2 :
  | /* nothing */ {
    `MemberList []
  }
  | x=Forspec y=Compspec {
    `For (x, y)
  }
  | x=Assert xs=separated_list2(COMMA, Member) {
    `MemberList (Syntax.MemberAssert x :: xs)
  }
  | x=Field xs=separated_list2(COMMA, Member) {
    `MemberList (Syntax.MemberField x :: xs)
  }

Member :
  | x=Objlocal {
    Syntax.MemberObjlocal x
  }
  | x=Assert {
    Syntax.MemberAssert x
  }
  | x=Field {
    Syntax.MemberField x
  }

FieldExceptBracket :
  | name=FieldnameExceptBracket plus=option(PLUS) h=H e=Expr {
    Syntax.Field (name, Option.is_some plus, h, e)
  }
  | name=FieldnameExceptBracket LPAREN params=Params RPAREN h=H e=Expr {
    Syntax.FieldFunc (name, params, h, e)
  }

Field :
  | name=Fieldname plus=option(PLUS) h=H e=Expr {
    Syntax.Field (name, Option.is_some plus, h, e)
  }
  | name=Fieldname LPAREN params=Params RPAREN h=H e=Expr {
    Syntax.FieldFunc (name, params, h, e)
  }

H :
  | COLON {
    Syntax.H 1
  }
  | DOUBLECOLONS {
    Syntax.H 2
  }
  | TRIPLECOLONS {
    Syntax.H 3
  }

Objlocal :
  | LOCAL b=Bind {
    b
  }

Compspec :
  | xs=list(x=Forspec { Syntax.Forspec x } | y=Ifspec { Syntax.Ifspec y }) {
    xs
  }

Forspec :
  | FOR id=ID IN e=Expr {
    (id, e)
  }

Ifspec :
  | IF e=Expr {
    e
  }

FieldnameExceptBracket :
  | id=ID {
    Syntax.FieldnameID id
  }
  | s=STRING {
    Syntax.FieldnameString s
  }

Fieldname :
  | x=FieldnameExceptBracket {
    x
  }
  | LBRACKET e=Expr RBRACKET {
    Syntax.FieldnameExpr e
  }

Assert :
  | ASSERT e1=Expr e2=option(COLON e=Expr { e }) {
    (e1, e2)
  }

Bind :
  | id=ID EQUAL e=Expr {
    Syntax.Bind (id, e)
  }
  | id=ID LPAREN params=Params RPAREN EQUAL e=Expr {
    Syntax.BindFunc (id, params, e)
  }

  (*
Args :
  | pos=separated_nonempty_list(COMMA, Expr) named=list(COMMA id=ID EQUAL e=Expr { (id, e) }) option(COMMA) {
    Syntax.Args (pos, named)
  }
  | named=separated_nonempty_list(COMMA, id=ID EQUAL e=Expr { (id, e) }) option(COMMA) {
    Syntax.Args ([], named)
  }
*)

Params :
  | xs=separated_list1(COMMA, Param) {
    xs
  }

Param :
  | id=ID v=option(EQUAL e=Expr { e }) {
    (id, v)
  }
