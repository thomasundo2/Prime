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
| '['      { RBRACK }
| ']'      { LBRACK }
| ','      { COMMA }
| '='      { EQUALS } (* Binary Operators (semi perhaps not) *)
| ';'      { SEMI }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { POWER }
| '%'      { MOD }
| '.'      { ACCESS }
| "=="     { EQ }   (* Relational Ops (which ones of these do we want?)*)
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }   (* Keywords and types *)
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
(* | "float"  { FLOAT } *)
| "lint"   { LINT }  (* OUR CUSTOM TYPES *)
(* | "poly"   { POLY } (*More needs to be done here*)*)
| "pt"     { POINT }
| "ring"   { RING }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { NAME(name) } (*ids can be alpha followed by alphanum and _*)
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof      { EOF }
| _  as char      { raise (Failure("Undefined character " ^ Char.escaped char)) } (* any other character is not allowed *)

(* part of rule for ending comments *)
and comment = parse
  "*/" { token lexbuf } (*back to normal scanning *)
| _    { comment lexbuf } (* keep reading comments *)