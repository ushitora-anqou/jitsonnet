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
  | FUNCTION LPAREN option(Params) RPAREN Expr {
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
  | xs=loption(xs=separated_nonempty_list(COMMA, Member) option(COMMA) { xs }) {
    Syntax.ObjectMemberList xs
  }
  | lobjlocals=list(x=Objlocal COMMA { x }) LBRACKET e1=Expr RBRACKET COLON e2=Expr robjlocals=list(COMMA x=Objlocal { x }) option(COMMA) forspec=Forspec compspec=Compspec {
    Syntax.ObjectFor (lobjlocals, e1, e2, robjlocals, forspec, compspec)
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
  | name=Fieldname LPAREN params=option(Params) RPAREN h=H e=Expr {
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
    Syntax.Assert (e1, e2)
  }

Bind :
  | id=ID EQUAL e=Expr {
    Syntax.Bind (id, e)
  }
  | id=ID LPAREN params=option(Params) RPAREN EQUAL e=Expr {
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
  | params=separated_nonempty_list(COMMA, Param) option(COMMA) {
    params
  }

Param :
  | id=ID v=option(EQUAL e=Expr { e }) {
    (id, v)
  }
