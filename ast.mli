(* Create a new operator for assignment and create a new expression*)
(* sequences of expressions *)
type operator = Add | Sub | Mul | Div | Semi
type eqsign = Eq
type expr =
    Binop of expr * operator * expr
  | Assignmentop of string  * eqsign * expr
  | Getval of string
  | Lit of int

type typ = Int | Lint | Chr | Ring | String | Point | Poly
type program = string (* This is probably wrong but needs to be added for parser *)