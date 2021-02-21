(* Ocamllex scanner for PRIME
   Many thanks to the MicroC compiler example created by
   Professor Edwards
   Many of the symbols here are directly from or follow that.
*)

{ open Parser }

let digit = ['0' - '9']
let digits = digit+

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| "/*"                { comment lexbuf }  (* add comments *)
| '('      { LPAREN } (* Grouping operators *)
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ','      { COMMA }
| '='      { EQUALS } (* Binary Operators (semi perhaps not) *)
| ';'      { SEMI }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { POWER }
| '%'      { MOD }
| "=="     { EQ }   (* Relational Ops (which ones of these do we want?)*)
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }   (* Keywords and types *)
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
(*| "bool"   { BOOL }*)
(*| "float"  { FLOAT } *)
| "lint"   { LINT }  (* OUR CUSTOM TYPES *)
| "poly"   { POLY } (*More needs to be done here*)
| "pt"     { POINT }
| "ring"   { RING }
(* | "void"   { VOID } *) (* Want or not? *)
| digits as lxm { LITERAL(int_of_string lxm) }
| digits '.'  digit* ( ['e' 'E'] ['+' '-']? digits )? as lxm { FLIT(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*     as lxm { ID(lxm) }
| eof { EOF }

(* part of rule for ending comments *)
and comment = parse
  "*/" { token lexbuf } (*back to normal scanning *)
| _    { comment lexbuf } (* keep reading comments *)
