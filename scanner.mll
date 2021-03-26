(* Ocamllex scanner for PRIME
   Many thanks to the MicroC compiler example created by
   Professor Edwards
   Many of the symbols here are directly from or follow that.
*)

{
  open Parser
}

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                { comment lexbuf }  (* add comments *)
| '('      { LPAREN } (* Grouping operators *)
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| '['      { RBRACK }
| ']'      { LBRACK }
| ','      { COMMA }
| '='      { ASSIGN } (* Binary Operators (semi perhaps not) *)
| ';'      { SEMI }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| '^'      { POWER }
| '%'      { MOD }
| '.'      { ACCESS }
(* | ':'      { OVERLOAD } Not included in this part*)
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
| "char"   { CHAR }
| "string" { STRING }
| "lint"   { LINT }  (* OUR CUSTOM TYPES *)
| "poly"   { POLY } (*More needs to be done here*)
| "pt"     { POINT }
| "ring"   { RING }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { ID(name) } (*ids can be alpha followed by alphanum and _*)
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| '"'(_* as lit)'"' { STRLIT(lit) }  (* Make a separate rule for looking through string literals and comment literals *)
| eof      { EOF }
| _  as char      { raise (Failure("Undefined character " ^ Char.escaped char)) } (* any other character is not allowed *)

(* part of rule for ending comments *)
and comment = parse
  "*/" { token lexbuf } (*back to normal scanning *)
| _    { comment lexbuf } (* keep reading comments *)

{ 

}