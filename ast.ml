(* Create a new operator for assignment and create a new expression*)
(* sequences of expressions *)
type operator = Add | Sub | Mul | Div | Semi

type eqsign = Eq

(* type expr =
    Binop of expr * operator * expr
  | Assignmentop of string  * eqsign * expr
  | Getval of string
  | Lit of int *)

type typ = Int | Lint | Chr | Ring | String | Point | Poly | Void

type bind = typ * string

type expr = 
    Strlit of string
  | Lit of int
  |  Id of string
  | Call of string * expr list
  | Noexpr

type stmt = 
    Block of stmt list
  | Expr of expr
  | Return of expr

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