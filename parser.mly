%{ open Ast %}

%token PLUS MINUS TIMES DIVIDE EOF SEMI ASSIGN
%token <int> LITERAL
%token <string> NAME

%left SEMI
%right ASSIGN
%left PLUS MINUS
%left TIMES DIVIDE

%start expr
%type <Ast.expr> expr

%%

expr:
  expr PLUS   expr { Binop($1, Add, $3) }
| expr MINUS  expr { Binop($1, Sub, $3) }
| expr TIMES  expr { Binop($1, Mul, $3) }
| expr DIVIDE expr { Binop($1, Div, $3) }
| LITERAL          { Lit($1) }
| NAME ASSIGN expr {Assignmentop($1, Eq, $3)}
| expr SEMI expr   { Binop($1, Semi, $3)}
| NAME             {Getval($1)}


