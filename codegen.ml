(* This file will be used to get LLVM to work for our compiler as an IR *)

(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(*http://www.cs.columbia.edu/~sedwards/classes/2018/4115-fall/reports/FIRE.pdf*)
(*Simple Shape Oriented Language*)
module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Prime" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and void_t     = L.void_type   context in
  let point_t    = L.struct_type context [| i32_t ; i32_t |]
  and string_t   = L.pointer_type (i8_t)
  in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
      A.String   -> string_t
    | A.Lint     -> string_t
    | A.Point -> point_t
    | A.Int      -> i32_t
    | A.Void     -> void_t
    | _ -> void_t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n) =
      let init = match t with
        _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  let printf_t : L.lltype =
      L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue =
      L.declare_function "printf" printf_t the_module in

  (* Declare our external functions here*)

  (*points and printing points*)
  let init_point_t : L.lltype =
     L.function_type point_t [| i32_t; i32_t |] in
  let init_point_func : L.llvalue =
     L.declare_function "Point" init_point_t the_module in
  let printpt_t : L.lltype =
     L.function_type string_t [| point_t |] in
  let printpt_func : L.llvalue =
     L.declare_function "printpt" printpt_t the_module in

  let ptadd_t : L.lltype =
      L.function_type point_t [| point_t; point_t |] in
  let ptadd_func : L.llvalue =
     L.declare_function "ptadd" ptadd_t the_module in

  (*lint operators*)
  let ladd_t : L.lltype =
      L.function_type string_t [| string_t; string_t |] in
  let ladd_func : L.llvalue =
      L.declare_function "add" ladd_t the_module in

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sname(*sfname*)
      and formal_types =
	Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sparams)
      in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and string_format_str = L.build_global_stringptr "%s\n" "fmt" builder
    in
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p =
        L.set_value_name n p;
	let local = L.build_alloca (ltype_of_typ t) n builder in
        ignore (L.build_store p local builder);
	StringMap.add n local m

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map *)
      and add_local m (t, n) =
	let local_var = L.build_alloca (ltype_of_typ t) n builder
	in StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sparams
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* Return the value for a variable or formal argument.
       Check local names first, then global names *)
    let lookup n = try StringMap.find n local_vars
                   with Not_found -> StringMap.find n global_vars
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SStrlit i  ->  L.build_global_stringptr i "string" builder
      | SLit i  -> L.const_int i32_t i
      | SPtlit (i, j) ->
              let e1' = expr builder i
              and e2' = expr builder j in
              L.build_call init_point_func [| e1' ; e2' |] "Point" builder
      | SNoexpr    -> L.const_int i32_t 0
      | SId s       -> L.build_load (lookup s) s builder
      | SAssign (s, e) -> let e' = expr builder e in
                           ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Lint, _) as e1, operator, e2) ->
              let e1' = expr builder e1
              and e2' = expr builder e2 in
              (match operator with
              | A.Add     -> L.build_call ladd_func [| e1'; e2' |] "add" builder
              | _         -> raise (Failure "Operator not implemented for Lint")
              )
      | SBinop ((A.Point, _) as e1, operator, e2) ->
              let e1' = expr builder e1
              and e2' = expr builder e2 in
              (match operator with
              | A.Add     -> L.build_call ptadd_func [| e1'; e2' |] "ptadd" builder
              | _         -> raise (Failure "Operator not implemented for Point")
              )
      | SBinop (e1, operator, e2) ->
              let e1' = expr builder e1
              and e2' = expr builder e2 in
              (match operator with
                A.Add     -> L.build_add
              | A.Sub     -> L.build_sub
              | A.Mul     -> L.build_mul
              | A.Div     -> L.build_sdiv
              | A.Mod     -> L.build_srem
              | A.Pow     -> L.build_mul
	      | A.And     -> L.build_and
	      | A.Or      -> L.build_or
	      | A.Beq     -> L.build_icmp L.Icmp.Eq
	      | A.Bneq    -> L.build_icmp L.Icmp.Ne
	      | A.Lth     -> L.build_icmp L.Icmp.Slt
	      | A.Leq     -> L.build_icmp L.Icmp.Sle
	      | A.Gth     -> L.build_icmp L.Icmp.Sgt
	      | A.Geq     -> L.build_icmp L.Icmp.Sge
              ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
              let e' = expr builder e in
              (match op with
                A.Neg     -> L.build_neg e' "tmp" builder
              | A.Not     -> L.const_int i32_t (if e' = (L.const_int i32_t 0) then 1 else 0))
      | SCall ("print", [e]) -> (*keep print delete printb printf*)
	        L.build_call printf_func [| int_format_str ; (expr builder e) |]
	        "printf" builder
      | SCall ("prints", [e]) -> (*print string*)
          L.build_call printf_func [| string_format_str ; (expr builder e) |]
          "prints" builder
      | SCall ("printl", [e]) ->
          L.build_call printf_func [| string_format_str ; (expr builder e) |]
          "printl" builder
	  | SCall ("printpt", [e]) ->
          let ptStr = L.build_call printpt_func [|expr builder e|] "printpt" builder in
          L.build_call printf_func [| string_format_str ; ptStr |] "prints" builder
      | SCall (f, args) ->
          let (fdef, fdecl) = StringMap.find f function_decls in
	 let llargs = List.rev (List.map (expr builder) (List.rev args)) in
	 let result = (match fdecl.styp with
                        A.Void -> ""
                      | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list llargs) result builder
      | _ -> L.const_int i32_t 0
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)

    let rec stmt builder = function
        SBlock sl -> List.fold_left stmt builder sl
      | SExpr e -> ignore(expr builder e); builder
      | SReturn e -> ignore(match fdecl.styp with
                              (* Special "return nothing" instr *)
                              A.Void -> L.build_ret_void builder
                              (* Build return statement *)
  (*  | SIf (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder predicate in
	 let merge_bb = L.append_block context "merge" the_function in
         let build_br_merge = L.build_br merge_bb in (* partial function *)

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   build_br_merge;

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   build_br_merge;

	 ignore(L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | SWhile (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore(L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] ) *)
      | _ -> L.build_ret (expr builder e) builder );
      builder
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
