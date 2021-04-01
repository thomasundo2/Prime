(* Create a new operator for assignment and create a new expression*)
(* sequences of expressions *)

type operator = Add | Sub | Mul | Div | Mod | Pow
type eqsign = Eq
type uoperator = Neg | Not

type typ = Int | Lint | Chr | Ring | String | Point | Poly | Void
type bind = typ * string

type expr =
    Strlit of string
  | Lit of int
  | Ptlit of expr * expr
  | Id of string
  | Binop of expr * operator * expr
  | Unop of uoperator * expr
  | Assign of string * expr
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

(* This is probably wrong but needs to be added for parser *)
(* Essentially means variable declarations followed by function defs *)
type program = bind list * func_decl list


let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Pow -> "^"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let rec string_of_expr = function
  Strlit(l) -> "\"" ^ l ^ "\""
  | Id(s)   -> s
  | Lit(l) -> string_of_int l
  | Ptlit(i, j) -> "[" ^ string_of_expr i ^ "," ^ string_of_expr j ^ "]"
  | Binop(e1, o, e2) ->
          string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e 
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
