(* This is a file with our scanner which just prints the tokens *)
{ 
    (* No Parser here *)
    open Printf
}

let digit = ['0' - '9']
let digits = digit+

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "/*"                { comment lexbuf }  (* add comments *)
| '('      { printf "symbol: %c\n" '(' } (* Grouping operators *)
| ')'      { printf "symbol: %c\n" ')' }
| '{'      { printf "symbol: %c\n" '{' }
| '}'      { printf "symbol: %c\n" '}' }
| '['      { printf "symbol: %c\n" '[' }
| ']'      { printf "symbol: %c\n" ']' }
| ','      { printf "symbol: %c\n" ',' }
| '='      { printf "symbol: %c\n" '=' } (* Binary Operators (semi perhaps not) *)
| ';'      { printf "symbol: %c\n" ';' }
| '+'      { printf "symbol: %c\n" '+' }
| '-'      { printf "symbol: %c\n" '-' }
| '*'      { printf "symbol: %c\n" '*' }
| '/'      { printf "symbol: %c\n" '/' }
| '^'      { printf "symbol: %c\n" '^' }
| '%'      { printf "symbol: %c\n" '%' }
| '.'      { printf "symbol: %c\n" '.' }
(* | ':'      { OVERLOAD } Not included in this part*)
| "=="     { printf "Relational op\n" }   (* Relational Ops (which ones of these do we want?)*)
| "!="     { printf "Relational op\n" }
| '<'      { printf "Relational op\n" }
| "<="     { printf "Relational op\n" }
| ">"      { printf "Relational op\n" }
| ">="     { printf "Relational op\n" }
| "&&"     { printf "Relational op\n" }
| "||"     { printf "Relational op\n" }
| "!"      { printf "Relational op\n" }
| "if"     { printf "keyword\n" }   (* Keywords and types *)
| "else"   { printf "keyword\n" }
| "for"    { printf "keyword\n" }
| "while"  { printf "keyword\n" }
| "return" { printf "return\n" }
| "int"    { printf "type: int\n" }
| "char"   { printf "type: char\n" }
| "string" { printf "type: string\n" }
| "lint"   { printf "type: lint\n" }  (* OUR CUSTOM TYPES *)
| "poly"   { printf "type: poly\n" } (*More needs to be done here*)
| "pt"     { printf "type: pt\n" }
| "ring"   { printf "type: ring\n" }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as name { print_string ("ID: " ^ name ^ "\n") } (*ids can be alpha followed by alphanum and _*)
| ['0'-'9']+ as lit { print_string ("Literal: " ^ lit ^ "\n") }
| '"'_*'"'  as lit { print_string ("Strlit: " ^ lit ^ "\n") }  (* Make a separate rule for looking through string literals and comment literals *)
| "'"_"'"  as lit { print_string ("Charlit: " ^ lit ^ "\n") } 
| eof      { raise End_of_file }
| _  as char      { raise (Failure("Undefined character " ^ Char.escaped char)) } (* any other character is not allowed *)

(* part of rule for ending comments *)
and comment = parse
  "*/" { token lexbuf } (*back to normal scanning *)
| _    { comment lexbuf } (* keep reading comments *)

{ 
  (* Source: http://www.iro.umontreal.ca/~monnier/3065/ocamllex-tutorial.pdf *)
  (* Take from file if given else stdin *)
  let rec parse lexbuf = 
    let ret = token lexbuf in
        parse lexbuf

  let main () =
    let cin =
      if Array.length Sys.argv > 1
      then open_in Sys.argv.(1)
      else stdin
    in
    let lexbuf = Lexing.from_channel cin in
    try parse lexbuf
    with End_of_file -> ()

  let _ = Printexc.print main ()
}