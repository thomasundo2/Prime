%{ open Ast %}
// Thank you again to Professor Edwards for the MicroC template.
// We have made alterations and additions for our language's functionality
%token SEMI COLON AMP LPAREN RPAREN LBRACE RBRACE RBRACK LBRACK COMMA PLUS MINUS TIMES DIVIDE MOD POWER ASSIGN INVERT
%token PMOD LPOWER
%token BEQ BNEQ LTH GTH GEQ LEQ AND OR NOT
%token ACCESS
%token RETURN IF ELSE WHILE FOR INT LINT POLY POINT RING CHAR STRING //(*add float/void here if wanted*)
%token <int> LITERAL
%token <string> STRLIT LINTLIT ID
%token EOF

%start program
%type <Ast.program> program //(* Add in later when we define the AST *)

// (*precedence*)
%nonassoc NOELSE
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left BEQ BNEQ
%left LTH GTH LEQ GEQ
%left MOD //(* mod takes precedence below all arithmetic operators - l.guru approves*)
%left PLUS MINUS
%left TIMES DIVIDE //(* Change this order later if necessary \r moved mod up -l.guru*)
%right INVERT
%right NOT
%right POWER
%nonassoc AMP
%right PMOD
%left LPOWER
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


typ:
    INT   { Int }
  | LINT  { Lint }
  | POINT { Point }
  | POLY { Poly }
  | STRING { String }

vars:
  /* nothing */ { [] }
  | vars declare_init  { $2 :: $1 }

declare_init:
  typ declarator SEMI { ($1, $2) }

declarator:
    ID { $1 }
  //| ID ASSIGN expr {$1} // Allow assignment

seq_stmts:
    vars stmt_list { ($1, $2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr_opt SEMI                           { Expr $1 } //(* Expr-stmt *)
  | RETURN expr_opt SEMI                    { Return $2 } //(* Return stmt *)
  | LBRACE stmt_list RBRACE                 { Block(List.rev $2)    }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7)        }
  | FOR LPAREN expr SEMI expr SEMI expr_opt RPAREN stmt
                                            { For($3, $5, $7, $9)   }
  | WHILE LPAREN expr RPAREN stmt           { While($3, $5)         }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    ID               { Id($1) }
  | LITERAL          { Lit($1) }
  | STRLIT           { Strlit($1) }
  | LINTLIT          { Lintlit($1) }
  | LBRACK expr COMMA expr RBRACK AMP expr { Ptlit ($2, $4, $7) }
  | LBRACK LPAREN expr COMMA expr RPAREN COLON expr RBRACK {Polylit($3, $5, $8)}
  | expr MOD    expr { Binop($1, Mod, $3) }
  | expr POWER  expr { Binop($1, Pow, $3) }
  | expr PLUS   expr { Binop($1, Add, $3) }
  | expr MINUS  expr { Binop($1, Sub, $3) }
  | expr TIMES  expr { Binop($1, Mul, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr INVERT expr { Binop($1, Inv, $3) }
  | expr BEQ    expr { Relop($1, Beq, $3) }
  | expr BNEQ   expr { Relop($1, Bneq, $3)}
  | expr LTH    expr { Relop($1, Lth, $3) }
  | expr LEQ    expr { Relop($1, Leq, $3) }
  | expr GTH    expr { Relop($1, Gth, $3) }
  | expr GEQ    expr { Relop($1, Geq, $3) }
  | expr AND    expr { Relop($1, And, $3) }
  | expr OR     expr { Relop($1, Or, $3)  }
  | expr LPOWER   expr PMOD expr { Trnop($1, Lpw, $3, Pmd, $5) }
  | MINUS expr %prec NOT { Unop(Neg, $2)  }
  | NOT expr         { Unop(Not, $2)      }
  | ID ASSIGN expr   { Assign($1, $3)     }
  | ID ACCESS ID     { Access($1, $3) } // will be used for accessor methods
  | ID LPAREN args_opt RPAREN { Call($1, $3) }
  // | typ LPAREN expr RPAREN   { Cast($3) } (* Can later be generalised to other types *)
  | LPAREN expr RPAREN {  $2  }

args_opt:
    /* nothing */ { [] }
  | args_list  { List.rev $1 }

args_list:
    expr                    { [$1] }
  | args_list COMMA expr { $3 :: $1 }
