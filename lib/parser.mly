%{
%}

%token EOF
%token <int> IntLiteral
%token <string> ID

%start toplevel
%type <Syntax.program option> toplevel
%%

toplevel :
  | EOF { None }
  | e=Expr { Some Syntax.{ expr = e } }

Expr :
  | i=IntLiteral { Syntax.Number i }
  | id=ID { Syntax.ID id }
