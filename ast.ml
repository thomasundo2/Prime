(* Create a new operator for assignment and create a new expression*)
(* sequences of expressions *)

type operator = Add | Sub | Mul | Div | Mod | Pow | Beq | Bneq | Leq | Geq | Lth | Gth | And | Or | Inv
type eqsign = Eq
type uoperator = Neg | Not
type accessor = Access
type toperator = Lpw | Pmd

type typ = Int | Lint | Chr | Ring | String | Point | Poly | Void
type bind = typ * string

type expr =
    Strlit of string
  | Lit of int
  | Lintlit of string
  | Ptlit of expr * expr * expr
  | Polylit of expr * expr * expr
  | Id of string
  | Binop of expr * operator * expr
  | Relop of expr * operator * expr
  | Trnop of expr * toperator * expr * toperator * expr
  | Unop of uoperator * expr
  | Assign of string * expr
  | Access of string * string (* we will use the second string to convert to gep*)
  | Call of string * expr list
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt (*need for pretty print, temp*)
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
  typ : typ;
  name : string;
  params : bind list;
  locals : bind list;
  body : stmt list;
}


(* Essentially means variable declarations followed by function defs *)
type program = bind list * func_decl list


let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "/\\"
  | Inv -> "`"
  | Beq -> "=="
  | Bneq -> "!="
  | Leq -> "<=" 
  | Geq -> ">="
  | Lth -> "<"
  | Gth -> ">"
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_top = function
    Lpw -> "^"
  | Pmd -> "@"

let rec string_of_expr = function
    Strlit(l) -> "\"" ^ l ^ "\""
  | Id(s)   -> s
  | Lit(l) -> string_of_int l
  | Lintlit(l) -> l
  | Ptlit(i, j, p) -> "[" ^ string_of_expr i ^ "," ^ string_of_expr j ^ "] & "^string_of_expr p
  | Polylit(i,j, m) -> "[(" ^ string_of_expr i ^ "," ^ string_of_expr j^ ") : " ^string_of_expr m ^ "]"
  | Binop(e1, o, e2) ->
          string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Relop(e1, o, e2) ->
          string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Trnop(e1, o1, e2, o2, e3) ->
    string_of_expr e1 ^ " " ^ string_of_top o1 ^ " " ^ string_of_expr e2 ^ " " ^
    string_of_top o2 ^ " " ^ string_of_top e3
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Access(v, s) -> v ^ "." ^ s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int" 
  | String -> "string"
  | Lint -> "lint"
  | Point -> "Point"
  | Poly -> "poly"
  | Void -> "void"
  | _ -> "typ PP not implemented"


let string_of_vdecl (t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.name ^ "(" ^ String.concat ", " (List.map snd fdecl.params) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
