open Ast
module StringHash = Hashtbl.Make(struct
    type t = string             (* type of keys *)
    let equal x y = x = y       (* use structural comparison *)
    let hash = Hashtbl.hash     (* generic hash function *)
   end)

let myHash : int StringHash.t = StringHash.create 10

let rec eval = function
    Lit(x)            ->  x
  | Getval (str) -> StringHash.find myHash str
  | Assignmentop (str, op, e2) ->  let v = eval e2 in StringHash.add myHash str v; v
  | Binop(e1, op, e2) ->
      let v1  = eval e1 in
      let v2 = eval e2 in
      (match op with
        Add -> v1 + v2
          | Sub -> v1 - v2
          | Mul -> v1 * v2
          | Div -> v1 / v2
          | Semi -> v2
      )

(* Our pretty print function to display the abstract syntax tree *)
let rec pretty_print = function
    Lit(x)            ->  "lit: " ^ string_of_int x
  (* | Getval (str) -> print_string ( "(id: " ^ str ^ 
                                    ", val: " ^ string_of_int (StringHash.find myHash str) ^ ")")
  | Assignmentop (str, op, e2) ->  print_endline "="; print_string ("left: " ^ str ^ "\t right: "); pretty_print e2 *)
  | Binop(e1, op, e2) -> 
    let v1 = pretty_print e1 in
    let v2 = pretty_print e2 in
    (match op with
        | Add ->  "+(" ^ v1 ^ "   " ^ v2 ^ ")"
        | Sub -> "-(" ^ v1 ^ "   " ^ v2 ^ ")"
        | Mul -> "*(" ^ v1 ^ "   " ^ v2 ^ ")"
        | Div -> "/(" ^ v1 ^ "   " ^ v2 ^ ")"
        | Semi -> "")
  | _ -> "Not yet implemented"

(*Similar to main in C*)
let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.expr Scanner.tokenize lexbuf in
  (*let result = eval expr in
  print_endline (string_of_int result) *)
  print_endline (pretty_print expr)
