%{
%}

%token Assert Else Error False For Function If Import Importstr Importbin In Local Null Tailstrict Then Self Super True
%token EOF
%token <float> Number
%token <string> ID

%start toplevel
%type <Syntax.program option> toplevel
%%

toplevel :
  | EOF { None }
  | e=Expr { Some Syntax.{ expr = e } }

Expr :
  | id=ID { Syntax.ID id }
