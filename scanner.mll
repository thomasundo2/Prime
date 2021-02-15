(*identify semicolon*)
{ open Parser }

rule tokenize = parse
  [' ' '\t' '\r' '\n'] { tokenize lexbuf }
| '=' { EQUALS }
| ';' { SEMI }
| '+' { PLUS }
| '-' { MINUS }
| '*' { TIMES }
| '/' { DIVIDE }
| ['a'-'z']+ as name { NAME(name) }
| ['0'-'9']+ as lit { LITERAL(int_of_string lit) }
| eof { EOF }
