open Ast

(* The key thing for the semantic checked ast *)
type sexpr = typ * sx
and sx = 
    SLit of int
  | SStrlit of string
  | SId of string
  | SBinop of sexpr * operator * sexpr
  | SUnop of uoperator * sexpr
  | SCall of string * sexpr list
  | SNoexpr

type sstmt =
    SBlock of sstmt list
  | SExpr of sexpr
  | SReturn of sexpr

type sfunc_decl = {
    styp : typ;
    sname : string;
    sparams: bind list;
    slocals : bind list;
    sbody : sstmt list;
}

type sprogram = bind list * sfunc_decl list
