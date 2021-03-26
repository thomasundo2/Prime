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

(* Start with function declarations for built-ins (just print for now)*)
(* TODO: Add for scan/compute for rings *)
(* Just call the formal parameter ID of our inbuit functions x*)
let built_in_decls =
  let add_bind map (name, ty) = StringMap.add name {
    typ = Void; (* Our built in print functions will return string*)
    name = name;
    params = [(ty, "x")];
    locals = []; body = [] (* In-built don't have body. Determine semantics here *)
  } map
  in List.fold_left add_bind StringMap.empty [ ("print", Int); ("prints", String) ]  (* "Only print string for now" *)
in

(* Now keep track of these named built-in funcs in the top-level symbol table *)
let add_func map fd =
  (* Define what errors we might have  *)
  let built_in_err = "function " ^ fd.name ^ " not defined"
  and dup_err = "duplicate function found: " ^ fd.name
  and make_err er = raise (Failure er) (* Helper to throw error with msg = er *)
  and n = fd.name
  in match fd with
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
in

(* Make the symbol table starting with the built-in functions *)
let function_decls = List.fold_left add_func built_in_decls functions
in

(* Returning the added function *)
let find_func s =
  try StringMap.find s function_decls
  with Not_found -> raise (Failure ("function not found: " ^ s))
in

let _ = find_func "main" (* main must exist as entrypoint *)
in

(* check function bodies *)
let check_function func =
  (* All TODO: *)
  (* check type and identifiers in formal parameters and local vars *)
  (* check all assignments are valid types. Should we co-erce? *)
  let check_assign lvaltype rvaltype err =  (* used for now in function params *)
    if lvaltype = rvaltype then lvaltype else raise (Failure err)
  in
  (* make local symbol table and functions to use it*)

  (* Build local symbol table of variables for this function *)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m) StringMap.empty (globals @ func.params @ func.params )
  in

  (* Return a variable from our local symbol table *)
  let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* semantic expression checking *)
  let rec expr = function
      Lit l -> (Int, SLit l)
    | Id s -> (type_of_identifier s, SId s)
    | Strlit l -> (String, SStrlit l) (* String literals *)
    | Noexpr   -> (Void, SNoexpr)
    | Unop(op, e) as ex ->
            let (t, e') = expr e in
            let ty = match op with
              Neg when t = Int -> Int
            | Not when t = Int -> Int
            | _ -> raise (Failure ("illegal unary operator " ^
                                   string_of_uop op ^ string_of_typ t ^
                                   " in " ^ string_of_expr ex))
            in (ty, SUnop(op, (t, e')))
    | Binop(e1, op, e2) as e ->
            let (t1, e1') = expr e1
            and (t2, e2') = expr e2 in
            (* All binary operators require operands of the same type *)
            let same = t1 = t2 in
            (* Determine expression type based on operator and operand types *)
            let ty = match op with
              Add | Sub | Mul | Div | Mod | Pow when same && t1 = Int -> Int
            | _ -> raise (
                Failure ("illegal binary operator " ^
                        string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                        string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Call(name, args) (* as call *) ->
        let fd = find_func name in
        let param_length = List.length fd.params in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ name))
        else let check_call (param_typ, _) e = (* validate call *)
          let (et, e') = expr e in (* recursively semantic check expr *)
          let err = "illegal argument"
          in (check_assign param_typ et err, e')
        in
        let args' = List.map2 check_call fd.params args
        in (fd.typ, SCall(name, args'))
  in

  (* Here is where we check statements (only expr and Block for now)*)
  let rec check_stmt = function
      Expr e -> SExpr (expr e) (* recursive check *)
    | Return e -> let (t, e') = expr e in
        if t = func.typ then SReturn (t, e') (* Correct return type for function *)
        else raise (Failure "wrong return type")
    | Block sl ->
        let rec check_stmt_list = function (* Maybe add other return checks here *)
          | s :: ss -> check_stmt s :: check_stmt_list ss (* one statement at a time *)
          | []      -> [] (* done *)
        in SBlock(check_stmt_list sl)
    | _   -> raise (Failure "stmt type not implemented")
  in
  { styp = func.typ;
    sname = func.name;
    sparams = func.params;
    slocals = func.locals;
    sbody = match check_stmt (Block func.body) with
        SBlock(sl) -> sl
      | _ -> raise (Failure ("blocking failed"))
  }
in (globals, List.map check_function functions)
