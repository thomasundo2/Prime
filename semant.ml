(* Semantic checking file *)

open Ast
open Sast

(* Make a map to keep track of globals *)
module StringMap = Map.Make(String)

(* String hashmap for lint string conversion *)
(* e.g. RSA *)

module StringHash = Hashtbl.Make(struct
  type t = string
  let equal x y = x = y
  let hash = Hashtbl.hash
  end);;

let vals : int StringHash.t = StringHash.create 10;;


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
(* Just call the formal parameter ID of our inbuit functions x*)
let built_in_decls =
  let add_bind map (name, ty) = StringMap.add name {
    typ = Void; (* Our built in print functions will return string*)
    name = name;
    params = [(ty, "x")];
    locals = []; body = [] (* In-built don't have body. Determine semantics here *)
  } map
  in let void_decls = List.fold_left add_bind StringMap.empty [ ("print", Int); 
                                               ("prints", String);
                                               ("printl", Lint);
                                               ("printpt", Point); 
                                               ("printpoly", Poly);] 
     and add_cast map (name, ty) = StringMap.add name {
       typ = Lint;
       name = name;
       params = [(ty, "x")];
       locals = []; body = []
     } map
  in let void_decls = List.fold_left add_cast void_decls [ ("tolint", Int) ]
     and add_rand map (name, ty) = StringMap.add name {
        typ = Lint;
        name = name;
        params = [(ty, ("x")); (ty, ("y"))];
        locals = []; body = []
      } map
  in let void_decls = List.fold_left add_rand void_decls [ ("random", Lint) ]
     and add_decode map (name, ty) = StringMap.add name {
       typ = String;
       name = name;
       params = [(ty, "x")];
       locals = []; body = []
     } map
     and add_encode map (name, _) = StringMap.add name {
       typ = Void;
       name = name;
       params = [(Lint, "x"); (String, "y")]; (* Don't necessarily have to hard-code this but time is short *)
       locals = []; body = [];
     } map
  in let built_decls = List.fold_left add_decode void_decls [ ("decode", Lint)]
  in List.fold_left add_encode built_decls [ ("encode", (Lint, String)) ]
  (* We likely don't need the GMP functions here because they are not called directly (in fact should not be) *)
in

(* Now keep track of these named built-in funcs in the top-level symbol table *)
let add_func map fd =
  (* Define what errors we might have  *)
  let built_in_err = "function " ^ fd.name ^ " not defined"
  and dup_err = "duplicate function found: " ^ fd.name
  and make_err er = raise (Failure er) (* Helper to throw error with msg = er *)
  and n = fd.name
  in match fd with
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
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
  (* All #TODO: *)
  (* check type and identifiers in formal parameters and local vars *)
  (* check all assignments are valid types. Should we co-erce? *)
  let check_assign lvaltype rvaltype err =
    (* print_string ("param: " ^ (string_of_typ lvaltype) ^ " actual: " ^ (string_of_typ rvaltype) ^ "\n"); *)
    match lvaltype with
      (* Lint -> if rvaltype = String || rvaltype = Lint then lvaltype else raise (Failure err) *)
    | _    -> if lvaltype = rvaltype then lvaltype else raise (Failure err)
    (* if lvaltype is lint and rvaltype is string then lvaltype else raise failure*)
  in
  (* make local symbol table and functions to use it*)

  (* Build local symbol table of variables for this function *)
  let symbols = List.fold_left (fun m (ty, name) -> StringMap.add name ty m)
                       StringMap.empty (globals @ func.params @ func.locals )
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
  | Lintlit l -> (Lint, SLintlit l)
  | Noexpr   -> (Void, SNoexpr)
  | Assign(var, e) as ex ->
          let lt = type_of_identifier var
          and (rt, e') = expr e in
          let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
            string_of_typ rt ^ " in " ^ string_of_expr ex
          in (check_assign lt rt err, SAssign(var, (rt, e')))
  | Ptlit(e1, e2, e3) ->
	    let (t1, e1') = expr e1
 	    and (t2, e2') = expr e2
 	    and (t3, e3') = expr e3 in
	    let ty = match t1, t2, t3 with
	    Lint, Lint, Poly -> Point
	    | _ -> raise (Failure ("points must have Lint coordinates and be defined under a Poly"))
            in (ty, SPtlit((t1, e1'), (t2, e2'), (t3, e3')))
  | Access(var, e2) as ex -> (* Will give us the right index for gep from string *)
      let lt = type_of_identifier var in
      (match lt with
        Point -> (match e2 with
                    "x" -> (Lint, SAccess(var, 0))
                  | "y" -> (Lint, SAccess(var, 1))
                  | _   -> raise (Failure ("invalid access element " ^ e2 ^ " in "
                                           ^ string_of_expr ex)))
      | _     -> raise (Failure ("cannot access type: " ^ string_of_typ lt
                                 ^ " in " ^ string_of_expr ex)))
  | Polylit(e1, e2, e3) ->
	    let (t1, e1') = expr e1
 	    and (t2, e2') = expr e2
 	    and (t3, e3') = expr e3 in
	    let ty = match t1, t2, t3 with
	    Lint, Lint, Lint -> Poly
	    | _ -> raise (Failure ("Polynomials must have Lint coefficients and a Lint modulus"))
            in (ty, SPolylit((t1, e1'), (t2, e2'), (t3, e3')))
    | Unop(op, e) as ex ->
            let (t, e') = expr e in
            let ty = match op with
              Neg when t = Int -> Int
            | Not when t = Int -> Int
            | Neg when t = Lint -> Lint
            | Not when t = Lint -> Lint
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
            | Add | Sub | Mul | Div | Mod | Inv when same && t1 = Lint -> Lint
            | Add                               when same && t1 = Point -> Point
	    | Pow                               when t1 = Lint && t2 = Int -> Lint
            | Mul                               when t1 = Lint && t2 = Point -> Point
	    | Beq | Bneq | Leq | Geq | Lth | Gth | And | Or when same && t1 = Int -> Int
            | Beq | Bneq | Leq | Geq | Lth | Gth            when same && t1 = Lint -> Int
            | _ -> raise (
                Failure ("illegal binary operator " ^
                        string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                        string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SBinop((t1, e1'), op, (t2, e2')))
    | Relop(e1, op, e2) as e ->
            let (t1, e1') = expr e1
            and (t2, e2') = expr e2 in
            let same = t1 = t2 in
            let ty = match op with
            | Beq | Bneq | Leq | Geq | Lth | Gth | And | Or when same && t1 = Int -> Int
            | Beq | Bneq | Leq | Geq | Lth | Gth | And | Or when same && t1 = Lint -> Int 
            | _ -> raise (
                Failure ("illegal relational operator " ^
                        string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
                        string_of_typ t2 ^ " in " ^ string_of_expr e))
            in (ty, SRelop((t1, e1'), op, (t2, e2')))
    | Trnop(e1, o1, e2, o2, e3) as e ->
            let (t1, e1') = expr e1
            and (t2, e2') = expr e2
            and (t3, e3') = expr e3 in
            let ty = match o1, o2 with
            Lpw, Pmd when t1 = Lint && t2 = Lint && t3 = Lint -> Lint
          | _ -> raise (
              Failure ("illegal ternary operator " ^ string_of_typ t1 ^ " " ^
              string_of_top o1 ^ " " ^ string_of_typ t2 ^ " " ^ string_of_top o2 ^ " " ^
              string_of_typ t3 ^ " in " ^ string_of_expr e))
            in (ty, STrnop((t1, e1'), o1, (t2, e2'), o2, (t3, e3')))
    | Call(name, args) (* as call *) ->
        let fd = find_func name in
        let param_length = List.length fd.params in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ name))
        else let check_call (param_typ, _) e = (* validate call *)
          let (et, e') = expr e in (* recursively semantic check expr *)
          let err = "illegal argument " ^ string_of_typ et ^
          " expected " ^ string_of_typ param_typ ^ " in " ^ string_of_expr e
          in (check_assign param_typ et err, e')
        in
        let args' = List.map2 check_call fd.params args
        in (fd.typ, SCall(name, args'))
  in

  let check_int_expr e =
      let (t', e') = expr e
      and err = "expected integer expression in " ^ string_of_expr e
      in if t' != Int then raise (Failure err) else (t', e')
    in

  (* Here is where we check statements (only expr and Block for now)*)
  let rec check_stmt = function
    Expr e -> SExpr (expr e) (* recursive check *)
  | Return e -> let (t, e') = expr e in
      if t = func.typ then SReturn (t, e') (* Correct return type for function *)
      else raise (Failure "wrong return type")
  | Block sl ->
      let rec check_stmt_list = function (* Maybe add other return checks here *)
        [Return _ as s] -> [check_stmt s]
      | Return _ :: _   -> raise (Failure "nothing may follow a return")
      | Block sl :: ss  -> check_stmt_list (sl @ ss) (* Flatten blocks *)| s :: ss -> check_stmt s :: check_stmt_list ss (* one statement at a time *)
      | []      -> [] (* done *)
      in SBlock(check_stmt_list sl)
  | If(p, b1, b2) -> SIf(check_int_expr p, check_stmt b1, check_stmt b2)
  | For(e1, e2, e3, st) ->
	SFor(expr e1, check_int_expr e2, expr e3, check_stmt st)
  | While(p, s) -> SWhile(check_int_expr p, check_stmt s)
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
