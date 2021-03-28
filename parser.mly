%{ open Ast %}
// Thank you again to Professor Edwards for the MicroC template.
// We have made alterations and additions for our language's functionality

%token SEMI LPAREN RPAREN LBRACE RBRACE RBRACK LBRACK COMMA PLUS MINUS TIMES DIVIDE MOD POWER ASSIGN
%token EQ NEQ LT LEQ GT GEQ AND OR NOT
%token ACCESS
%token RETURN IF ELSE FOR WHILE INT LINT POLY POINT RING CHAR STRING //(*add float/void here if wanted*)
%token <int> LITERAL
%token <string> CHARLIT // Is there a way to change this to char from scanner?
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
%left MOD //(* mod takes precedence below all arithmetic operators - l.guru approves*)
%left PLUS MINUS
%left TIMES DIVIDE //(* Change this order later if necessary \r moved mod up -l.guru*)
%right NOT
%right POWER
%left ACCESS    // Built in access methods

%%

//(* All the semantic action braces will be empty for this submission *)

program:
  decls EOF { $1 }

// fst gets the var decs, snd gets the function decs
decls:
   /* nothing */ { ([], []) } // Building up list of variable decs and list of function decs
  | decls declare_init { (($2 :: fst $1), snd $1) }  //(* No external variables ? or keep as is*)
  | decls fdecl { (fst $1, ($2 :: snd $1)) }

// create the record denoted by AST
// the body will then contain declarations (allowed by expr)
fdecl:
   typ ID LPAREN params_opt RPAREN LBRACE seq_stmts RBRACE
     { { typ = $1;
         name = $2;
         params = List.rev $4;
         locals = List.rev (fst $7);
         body = List.rev (snd $7) (* Might have to split this for hello world *)
        } }

params_opt:
    /* nothing */ { [] }
  | params_list   { $1 }

// Have the lists on the left
params_list:
    typ ID                   { [($1,$2)] }
  | params_list COMMA typ ID { ($3,$4) :: $1 }

// Fill in the following types when we need them after Hello world
typ:
    INT   { Int }
  //| LINT  {  }
  //| POLY  {  }
    | POINT { Point }
  //| RING  {  }
  //| CHAR  {  }
  //| STRING { String } delete? strings implemented without typ STRING

declare_init:
  typ declarator SEMI { ($1, $2) }

declarator:
    ID { $1 }
  //| ID ASSIGN expr {} // Allow assignment
  //| ID LPAREN params_opt RPAREN {} // Points
  //| ID LBRACK expr RBRACK {} // Rings

seq_stmts:
    declare_init stmt_list { ([$1], $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr_opt SEMI                           { Expr $1 } //(* Expr-stmt *)
  | RETURN expr_opt SEMI                    { Return $2 } //(* Return stmt *)
  //| LBRACE seq_stmts RBRACE                 {  } //(* Seq stmts (nested?) *)
  //| IF LPAREN expr RPAREN stmt %prec NOELSE {  } //(* If dangling *)
  //| IF LPAREN expr RPAREN stmt ELSE stmt    {  } //(* If no dangle *)
  //| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
  //                                          {  } //(* Loops no infinite FOR *)
  //| WHILE LPAREN expr RPAREN stmt           {  }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ID               { Id($1) }
  | LITERAL          { Lit($1) }
  //| CHARLIT          {  }
  | STRLIT           { Strlit($1) }
  //  | PTLIT   	     { Ptlit($1, $2, $3) }
  | LBRACK expr COMMA expr COMMA expr RBRACK { Ptlit ($2, $4, $6) }
  //| expr ACCESS expr {  } // will be used for accessor methods
  | expr MOD    expr { Binop($1, Mod, $3) }
  | expr POWER  expr { Binop($1, Pow, $3) }
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mul, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  //| expr EQ     expr {  }
  //| expr NEQ    expr {  }
  //| expr LT     expr {  }
  //| expr LEQ    expr {  }
  //| expr GT     expr {  }
  //| expr GEQ    expr {  }
  //| expr AND    expr {  }
  //| expr OR     expr {  }
  | MINUS expr %prec NOT { Unop(Neg, $2)  }
  | NOT expr         { Unop(Not, $2)      }
  //| ID ASSIGN expr   {   }
  //| ID LBRACK expr RBRACK ASSIGN expr {}
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  // | LBRACK args_list RBRACK    { Ptlit($1, $2, $3)  }    // Point initialisation
  | LPAREN expr RPAREN {  $2  }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
