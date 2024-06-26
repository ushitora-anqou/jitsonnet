%{
%}

%token AND ANDAND ASSERT BANG BANGEQ BAR COLON COMMA DOLLAR DOT ELSE EOF EQ EQEQ ERROR FALSE FOR FUNCTION GE GT GTGT HAT IF IMPORT IMPORTBIN IMPORTSTR IN LBRACE LBRACKET LE LOCAL LPAREN LT LTLT MINUS NULL BARBAR PERCENT PLUS RBRACE RBRACKET RPAREN SELF SEMICOLON SLASH STAR SUPER TAILSTRICT THEN TILDE TRUE

%token <float> NUMBER
%token <string> ID
%token <string> STRING

%nonassoc THEN FUNCTION ERROR
%nonassoc ELSE
%nonassoc SEMICOLON
%left BARBAR
%left ANDAND
%left BAR
%left HAT
%left AND
%left EQEQ BANGEQ
%left LT GT LE GE IN
%left LTLT GTGT
%left PLUS MINUS
%left STAR SLASH PERCENT
%left BANG TILDE
%left LPAREN LBRACKET DOT SUPER (* Probably correct until here*) LBRACE (* FIXME: correct? *)

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
  | x=Expr
    LBRACKET
      a=option(Expr)
      bc=option(
        COLON
        x=option(
          x=Expr
          y=option(
            COLON
            x=option(Expr) { x }
          ) { (x, y) }
        ) { x })
    RBRACKET { (* x[a], x[a:], x[a:b], x[a:b:], x[a:b:c] *)
    match a, bc with
    | Some a, None -> (* x[a] *)
      Syntax.ArrayIndex (x, a)
    | None, None ->
      raise (Syntax.General_parse_error "ast.Index requires an expression")
    | _, Some None -> (* x[:], x[a:] *)
      Syntax.ArraySlice (x, a, None, None)
    | _, Some (Some (b, (None | Some None))) -> (* x[:b], x[:b], x[a:b], x[a:b:] *)
      Syntax.ArraySlice (x, a, Some b, None)
    | _, Some (Some (b, Some (Some c))) -> (* x[:b:c], x[a:b:c] *)
      Syntax.ArraySlice (x, a, Some b, Some c)
  }
  | x=Expr LBRACKET a=option(Expr) COLON COLON c=option(Expr) RBRACKET { (* x[::], x[::c], x[a::], x[a::c] *)
    Syntax.ArraySlice(x, a, None, c)
  }
  | SUPER DOT id=ID {
    Syntax.SuperIndex (String id)
  }
  | SUPER LBRACKET e=Expr RBRACKET {
    Syntax.SuperIndex e
  }
  | e=Expr LPAREN args=Args RPAREN tailstrict=option(TAILSTRICT) {
    Syntax.Call (e, args, Option.is_some tailstrict)
  }
  | id=ID {
    Syntax.Var id
  }
  | LOCAL binds=separated_nonempty_list(COMMA, Bind) SEMICOLON e=Expr {
    Syntax.Local (binds, e)
  }
  | IF e1=Expr THEN e2=Expr e3=ioption(ELSE x=Expr { x }) {
    Syntax.If (e1, e2, e3)
  }
  | e1=Expr op=Binaryop e2=Expr {
    Syntax.Binary (e1, op, e2)
  }
  | op=Unaryop e=Expr {
    Syntax.Unary (op, e)
  }
  | e=Expr LBRACE o=Objinside RBRACE {
    Syntax.ObjectSeq (e, o)
  }
  | FUNCTION LPAREN params=Params RPAREN e=Expr %prec FUNCTION {
    Syntax.Function (params, e)
  }
  | a=Assert SEMICOLON e=Expr {
    Syntax.Assert (a, e)
  }
  | IMPORT s=STRING {
    Syntax.Import s
  }
  | IMPORTSTR s=STRING {
    Syntax.Importstr s
  }
  | IMPORTBIN s=STRING {
    Syntax.Importbin s
  }
  | ERROR e=Expr {
    Syntax.Error e
  }
  | e=Expr IN SUPER {
    Syntax.InSuper e
  }
  | LPAREN e=Expr RPAREN {
    e
  }

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
  | LBRACKET e1=Expr RBRACKET plus=option(PLUS) h=H e2=Expr post=Objinside2 {
    match plus, h, post with
    | None, Syntax.H 1, (ys, `For (forspec, compspec)) ->
      Syntax.ObjectFor ([], e1, e2, ys, forspec, compspec)
    | _, _, (ys, `MemberList members) ->
      let a = Syntax.MemberField (Field (FieldnameExpr e1, Option.is_some plus, h, e2)) in
      let bs = List.map (fun y -> Syntax.MemberObjlocal y) ys in
      Syntax.ObjectMemberList (a :: (bs @ members))
    | _ -> raise (Syntax.General_parse_error "invalid brackets")
  }

Objinside2 :
  | /* nothing */ {
    ([], `MemberList [])
  }
  | COMMA {
    ([], `MemberList [])
  }
  | COMMA x=Objlocal y=Objinside2 {
    let (xs, post) = y in
    (x :: xs, post)
  }
  | COMMA x=Assert xs=separated_list2(COMMA, Member) {
    ([], `MemberList (Syntax.MemberAssert x :: xs))
  }
  | COMMA x=Field xs=separated_list2(COMMA, Member) {
    ([], `MemberList (Syntax.MemberField x :: xs))
  }
  | COMMA x=Forspec y=Compspec {
    ([], `For (x, y))
  }
  | x=Forspec y=Compspec {
    ([], `For (x, y))
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
  | COLON COLON {
    Syntax.H 2
  }
  | COLON COLON COLON {
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
  | id=ID EQ e=Expr {
    Syntax.Bind (id, e)
  }
  | id=ID LPAREN params=Params RPAREN EQ e=Expr {
    Syntax.BindFunc (id, params, e)
  }

Args :
  | /* nothing */ {
    ([], [])
  }
  | e=Expr args=Args1 {
    (e :: (fst args), snd args)
  }
  | id=ID EQ e=Expr args=Args2 {
    (fst args, (id, e) :: (snd args))
  }

Args1 :
  | option(COMMA) {
    ([], [])
  }
  | COMMA e=Expr args=Args1 {
    (e :: (fst args), snd args)
  }
  | COMMA id=ID EQ e=Expr args=Args2 {
    (fst args, (id, e) :: (snd args))
  }

Args2 :
  | option(COMMA) {
    ([], [])
  }
  | COMMA id=ID EQ e=Expr args=Args2 {
    (fst args, (id, e) :: (snd args))
  }

Params :
  | xs=separated_list1(COMMA, Param) {
    xs
  }

Param :
  | id=ID v=option(EQ e=Expr { e }) {
    (id, v)
  }

%inline Binaryop :
  | AND { `Land }
  | ANDAND { `And }
  | BANGEQ { `NotEqual }
  | BAR { `Lor }
  | EQEQ { `Equal }
  | GE { `Ge }
  | GT { `Gt }
  | GTGT { `Lsr }
  | HAT { `Xor }
  | IN { `In }
  | LE { `Le }
  | LT { `Lt }
  | LTLT { `Lsl }
  | MINUS { `Sub }
  | BARBAR { `Or }
  | PERCENT { `Mod }
  | PLUS { `Add }
  | SLASH { `Div }
  | STAR { `Mult }

%inline Unaryop :
  | BANG { Syntax.Not }
  | MINUS { Syntax.Neg }
  | PLUS { Syntax.Pos }
  | TILDE { Syntax.Lnot }
