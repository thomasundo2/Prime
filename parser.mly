%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE RBRACK LBRACK COMMA PLUS MINUS TIMES DIVIDE MOD POWER ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR
%token ACCESS
%token RETURN IF ELSE FOR WHILE INT LINT POLY POINT RING CHAR STRING //(*add float/void here if wanted*)
%token <int> LITERAL
%token <char> CHARLIT
%token <string> STRLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program //(* Add in later when we define the AST *)

// (*precedence*)
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LEQ GEQ
// %left MOD (* mod takes precedence below all arithmetic operators *)
%left PLUS MINUS
%left TIMES DIVIDE MOD //(* Change this order later if necessary *)
%right NOT
%right POWER
%left ACCESS    // Built in access methods 

%%

//(* All the semantic action braces will be empty for this submission *)

program:
  decls EOF { }

decls:
   /* nothing */ {}
  | decls declare_init {}  //(* No external variables ? or keep as is*)
  | decls fdecl {}

fdecl:
   typ ID LPAREN params_opt RPAREN LBRACE seq_stmts RBRACE
     { }

params_opt:
    /* nothing */ {  }
  | params_list   {  }

params_list:
    typ ID                   {  }
  | params_list COMMA typ ID {  }

typ:
    INT   {  }
  | LINT  {  }
  | POLY  {  }
  | POINT {  }
  | RING  {  }
  | CHAR  {  }
  | STRING { }

declare_init:
  typ declarator SEMI {}

declarator:
    ID {}
  | ID ASSIGN expr {}
  | ID LPAREN params_opt RPAREN {}
  | ID LBRACK expr RBRACK {}

seq_stmts:
  decls stmt_list {}

stmt_list:
    /* nothing */  {}
  | stmt_list stmt {}

stmt:
    expr_opt SEMI                           {  } //(* Expr-stmt *)
  | RETURN expr_opt SEMI                    {  } //(* Return stmt *)
  | LBRACE seq_stmts RBRACE                 {  } //(* Seq stmts *)
  | IF LPAREN expr RPAREN stmt %prec NOELSE {  } //(* If dangling *)
  | IF LPAREN expr RPAREN stmt ELSE stmt    {  } //(* If no dangle *)
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
                                            {  } //(* Loops no infinite FOR *)
  | WHILE LPAREN expr RPAREN stmt           {  }

expr_opt:
    /* nothing */ {  }
  | expr          {  }

expr:
    ID               {  }
  | LITERAL          {  }
  | CHARLIT          {  }
  | STRLIT           {  }
  | expr ACCESS expr {  } // will be used for accessor methods
  | expr MOD    expr {  } 
  | expr POWER  expr {  } 
  | expr PLUS   expr {  }
  | expr MINUS  expr {  }
  | expr TIMES  expr {  }
  | expr DIVIDE expr {  }
  | expr EQ     expr {  }
  | expr NEQ    expr {  }
  | expr LT     expr {  }
  | expr LEQ    expr {  }
  | expr GT     expr {  }
  | expr GEQ    expr {  }
  | expr AND    expr {  }
  | expr OR     expr {  }
  | MINUS expr %prec NOT { }
  | NOT expr         {  }
  | ID ASSIGN expr   {   }
  | ID LPAREN args_opt RPAREN {  }
  | LBRACK args_list RBRACK    {   }    // Point initialisation 
  | LPAREN expr RPAREN {    }

args_opt:
    /* nothing */ {  }
  | args_list  {  }

args_list:
    expr                    { }
  | args_list COMMA expr { }
