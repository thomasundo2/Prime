(* Semantic checking file *)

open Ast
open Sast

(* Make a map to keep track of globals *)
module StringMap = Map.Make(String)

(* Begin Semantic checking sast if good else error *)

let check (globals, functions) = 
  (* Check binds have types and ids are unique *)
  let check_binds (kind : string) (binds : bind list) =
    List.iter (function 
    (Void, b) -> raise (Failure ("missing/wrong type in declaration " ^ kind ^ " " ^ b))
      | _ -> ()) binds;
    let rec dups = function 
        [] -> () (* No name found here *)
      | ((_, n1) :: (_, n2) :: _) when n1 = n2 -> (* check if same in order because sorted *)
        raise (Failure ("duplicate " ^ " " ^ n1))
      | _ :: t -> dups t (* check the tail of the binds *)
  in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
in 

(* Now actually perform the checks first variables then functions *)
check_binds "global" globals;

