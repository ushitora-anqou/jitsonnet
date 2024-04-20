%{
%}

%token ASSERT
%token COLON
%token COMMA
%token DOLLAR
%token DOT
%token DOUBLECOLONS
%token ELSE
%token EOF
%token EQUAL
%token ERROR
%token FALSE
%token FOR
%token FUNCTION
%token IF
%token IMPORT
%token IMPORTBIN
%token IMPORTSTR
%token IN
%token LBRACE
%token LBRACKET
%token LOCAL
%token LPAREN
%token NULL
%token PLUS
%token RBRACE
%token RBRACKET
%token RPAREN
%token SELF
%token SEMICOLON
%token SUPER
%token TAILSTRICT
%token THEN
%token TRIPLECOLONS
%token TRUE

%token <float> NUMBER
%token <string> ID
%token <string> STRING

%start toplevel
%type <Syntax.program option> toplevel
%%

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
  | EOF { None }
  | e=Expr { Some Syntax.{ expr = e } }

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

  (*
  | LBRACKET option(separated_nonempty_list(COMMA, Expr) option(COMMA)) RBRACKET {
    Syntax.Array
  }
  | LBRACKET Expr option(COMMA) Forspec Compspec {
    Syntax.ArrayFor
  }
  | Expr DOT ID {
    Syntax.Member
  }
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
  | id=ID {
    Syntax.Var id
  }
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
    Syntax.ObjectMemberList x
  }
  | x=Objinside2 {
    Syntax.ObjectFor x
  }

Objinside0 :
  | /* nothing */ {
    Syntax.ObjectMemberList []
  }
  | x=Objlocal {
    Syntax.ObjectMemberList [Syntax.MemberObjlocal x]
  }
  | x=Objlocal COMMA y=Objinside0 {
    match y with
    | Syntax.ObjectMemberList xs ->
      ObjectMemberList (MemberObjlocal x :: xs)
    | Syntax.ObjectFor (xs, e1, e2, robjlocals, forspec, compspec) ->
      ObjectFor (x :: xs, e1, e2, robjlocals, forspec, compspec)
  }
  | x=Objlocal COMMA y=Assert xs=separated_list2(COMMA, Member) {
    Syntax.ObjectMemberList (MemberObjlocal x :: MemberAssert y :: xs)
  }
  | x=Objlocal COMMA y=Field xs=separated_list2(COMMA, Member) {
    Syntax.ObjectMemberList (MemberObjlocal x :: MemberField y :: xs)
  }
  | x=Objlocal COMMA y=Objinside2 {
    let (xs, e1, e2, robjlocals, forspec, compspec) = y in
    Syntax.ObjectFor (x :: xs, e1, e2, robjlocals, forspec, compspec)
  }

Objinside1 :
  | x=Assert xs=separated_list2(COMMA, Member) {
    (Syntax.MemberAssert x) :: xs
  }
  | x=Field xs=separated_list2(COMMA, Member) {
    (Syntax.MemberField x) :: xs
  }

Objinside2 :
  | LBRACKET e1=Expr RBRACKET COLON e2=Expr robjlocals=separated_list2(COMMA, Objlocal) forspec=Forspec compspec=Compspec {
    ([], e1, e2, robjlocals, forspec, compspec)
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

Field :
  | name=Fieldname option(PLUS) h=H e=Expr {
    Syntax.Field (name, h, e)
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

Fieldname :
  | id=ID {
    Syntax.FieldnameID id
  }
  | s=STRING {
    Syntax.FieldnameString s
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
