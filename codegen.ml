(* This file will be used to get LLVM to work for our compiler as an IR *)

(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
LLVM tutorial: Make sure to read the OCaml version of the tutorial
http://llvm.org/docs/tutorial/index.html
Detailed documentation on the OCaml LLVM library:
http://llvm.moe/
http://llvm.moe/ocaml/
*)

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
  and i1_t       = L.i1_type     context
  and i8_t       = L.i8_type     context
  and string_t   = L.pointer_type (L.i8_type context)
  and point_t    = L.pointer_type (L.i8_type context)
  and void_t     = L.void_type   context in
  let point_t    = L.struct_type context [| i32_t ; i32_t |]
  and string_t   = L.pointer_type (i8_t)
  (* and mpz_t      = L.struct_type context [| (L.i32_type context); (L.i32_type context); (L.pointer_type (L.i64_type context)) |] *)
(* in *)
  and mpz_t      = L.named_struct_type context "mpz_t"
    in let mpz_t = L.struct_set_body mpz_t [| (L.i32_type context); (L.i32_type context); L.pointer_type (L.i64_type context) |] false; mpz_t
  in

  (* Return the LLVM type for a MicroC type *)
  let ltype_of_typ = function
    A.String   -> string_t
  | A.Lint     -> mpz_t
  | A.Point    -> point_t
  | A.Int      -> i32_t
  | A.Void     -> void_t
  | _          -> void_t
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

  (* LINTS *)
  let linit_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; string_t; i32_t |] in
  let linit_func : L.llvalue =
      L.declare_function "__gmpz_init_set_str" linit_t the_module in
  let linitdup_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t |] in
  let linitdup_func : L.llvalue =
      L.declare_function "__gmpz_init_set" linitdup_t the_module in
  let lclear_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t |] in
  let lclear_func : L.llvalue =
      L.declare_function "__gmpz_clear" lclear_t the_module in
  (* We don't use the mpz_out_str because FILE* is a pain *)
  let lprint_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t |] in
  let lprint_func : L.llvalue =
      L.declare_function "printl" lprint_t the_module in
  let ladd_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t; L.pointer_type mpz_t |] in
  let ladd_func : L.llvalue =
      L.declare_function "__gmpz_add" ladd_t the_module in
  let lsub_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t; L.pointer_type mpz_t |] in
  let lsub_func : L.llvalue =
      L.declare_function "__gmpz_sub" lsub_t the_module in
  let lmul_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t; L.pointer_type mpz_t |] in
  let lmul_func : L.llvalue =
      L.declare_function "__gmpz_mul" lmul_t the_module in
  let ldiv_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t; L.pointer_type mpz_t |] in
  let ldiv_func : L.llvalue =
      L.declare_function "__gmpz_tdiv_q" ldiv_t the_module in
  let lmod_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t; L.pointer_type mpz_t |] in
  let lmod_func : L.llvalue =
      L.declare_function "__gmpz_tdiv_r" lmod_t the_module in
  (* This power function will be used to raise to an unsigned int power *)
  let lpow_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; L.pointer_type mpz_t; i32_t |] in
  let lpow_func : L.llvalue =
      L.declare_function "__gmpz_pow_ui" lpow_t the_module in


  (*points and printing points*)
  let init_point_t : L.lltype =
     L.function_type point_t [| i32_t; i32_t |] in
  let init_point_func : L.llvalue =
     L.declare_function "Point" init_point_t the_module in
  let printpt_t : L.lltype =
     L.function_type string_t [| point_t |] in
  let printpt_func : L.llvalue =
     L.declare_function "printpt" printpt_t the_module in
  (*point operators*)
  let ptadd_t : L.lltype =
      L.function_type point_t [| point_t; point_t |] in
  let ptadd_func : L.llvalue =
     L.declare_function "ptadd" ptadd_t the_module in

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
    and point_format_str = L.build_global_stringptr "%s\n" "fmt" builder
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

    (* Helper function to deal with unassigned lint lits
       Returns: mpz_t pointer to be used for function args *)
    let llit_helper i =
      let lstr = L.build_global_stringptr i "string" builder
      and space = L.build_alloca (ltype_of_typ A.Lint) "" builder in
      let calls = ignore(L.build_call linit_func
        [| L.build_in_bounds_gep space [| L.const_int i32_t 0 |] "" builder; lstr; L.const_int i32_t 10 |]
        "__gmpz_init_set_str" builder);
        L.build_in_bounds_gep space [| L.const_int i32_t 0 |] "" builder
      in calls
      (* how to free after done using *)
    in

    (* Helpful when writing geps *)
    let zero = L.const_int i32_t 0
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((stype, e) : sexpr) = match e with
        SStrlit i     -> L.build_global_stringptr i "string" builder
      | SLintlit i    -> llit_helper i (* Pointer to new mpz*)
      | SLit i        -> L.const_int i32_t i
      | SPtlit (i, j) ->
              let e1' = expr builder i
              and e2' = expr builder j in
              L.build_call init_point_func [| e1' ; e2' |] "Point" builder
      | SNoexpr       -> L.const_int i32_t 0
      | SId s         -> (match stype with
                          A.Lint -> L.build_in_bounds_gep (lookup s) [| zero |] s builder
                        | _      -> L.build_load (lookup s) s builder)
      | SAssign (s, ((A.Lint, _) as e1)) -> let e1' = expr builder e1 in
                (* Here we have a pointer to mpz val *)
                ignore(L.build_call linitdup_func
                [| L.build_in_bounds_gep (lookup s) [| zero |] s builder; e1' |] "" builder); e1'
      | SAssign (s, e) -> let e' = expr builder e in
                           ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Lint, _) as e1, operator, e2) ->
      (* for e1, e2 take second argument of the tuple (A.Lint, _) and do what printl does.
       * See if its an id or lintlit. If id get inbounds elt pointer to struct.
       * If its lintlit use helper function to make new mpz_t and get pointer to it
       * Helper function will return a 1d array. Concat 2 1elt array. call OCaml array.append
       * Pass this to Add *)
              let e1' = expr builder e1
              and e2' = expr builder e2
              and tmp = llit_helper "0" in
              ignore((match operator with
                       A.Add -> L.build_call ladd_func [| tmp; e1'; e2' |] "__gmpz_add" builder
                     | A.Sub -> L.build_call lsub_func [| tmp; e1'; e2' |] "__gmpz_sub" builder
                     | A.Mul -> L.build_call lmul_func [| tmp; e1'; e2' |] "__gmpz_mul" builder
                     | A.Div -> L.build_call ldiv_func [| tmp; e1'; e2' |] "__gmpz_tdiv_q" builder
                     | A.Mod -> L.build_call lmod_func [| tmp; e1'; e2' |] "__gmpz_tdiv_r" builder
                     | A.Pow -> L.build_call lpow_func [| tmp; e1'; e2' |] "__gmpz_pow_ui" builder
                     )); tmp
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
                A.Add     -> L.build_add e1' e2' "tmp" builder
              | A.Sub     -> L.build_sub e1' e2' "tmp" builder
              | A.Mul     -> L.build_mul e1' e2' "tmp" builder
              | A.Div     -> L.build_sdiv e1' e2' "tmp" builder
              | A.Mod     -> L.build_srem e1' e2' "tmp" builder
              | A.Pow     -> L.build_mul e1' e2' "tmp" builder
	      | A.And     -> L.build_zext
                             (L.build_and
                             (L.build_icmp L.Icmp.Ne e1' (L.const_int i32_t 0) "tmp" builder)
                             (L.build_icmp L.Icmp.Ne e2' (L.const_int i32_t 0) "tmp" builder)
                             "tmp" builder) i32_t "tmp" builder
	      | A.Or      -> L.build_zext
                             (L.build_or 
                             (L.build_icmp L.Icmp.Ne e1' (L.const_int i32_t 0) "tmp" builder) 
                             (L.build_icmp L.Icmp.Ne e2' (L.const_int i32_t 0) "tmp" builder)
                             "tmp" builder) i32_t "tmp" builder
	      | A.Beq     -> L.build_zext (L.build_icmp L.Icmp.Eq e1' e2' "tmp" builder) 
                             i32_t "tmp" builder
	      | A.Bneq    -> L.build_zext (L.build_icmp L.Icmp.Ne e1' e2' "tmp" builder) i32_t
                             "tmp" builder
	      | A.Lth     -> L.build_zext (L.build_icmp L.Icmp.Slt e1' e2' "tmp" builder) i32_t
                             "tmp" builder
	      | A.Leq     -> L.build_zext (L.build_icmp L.Icmp.Sle e1' e2' "tmp" builder) i32_t
                             "tmp" builder
	      | A.Gth     -> L.build_zext (L.build_icmp L.Icmp.Sgt e1' e2' "tmp" builder) i32_t
                             "tmp" builder
	      | A.Geq     -> L.build_zext (L.build_icmp L.Icmp.Sge e1' e2' "tmp" builder) i32_t
                             "tmp" builder
              ) (*e1' e2' "tmp" builder*)
      | SUnop(op, ((t, _) as e)) ->
              let e' = expr builder e in
              (match op with
                A.Neg     -> L.build_neg  e' "tmp" builder
              | A.Not     -> (L.build_zext
                             (L.build_icmp L.Icmp.Eq e' (L.const_int i32_t 0) "tmp" builder)
                             i32_t "tmp" builder))
      | SCall ("print", [e]) -> (*keep print delete printb printf*)
	        L.build_call printf_func [| int_format_str ; (expr builder e) |]
	        "printf" builder
      | SCall ("prints", [e]) -> (*print string*)
          L.build_call printf_func [| string_format_str ; (expr builder e) |]
          "printf" builder
      | SCall ("printl", [(_, e) as ptr]) ->
          L.build_call lprint_func
          (match e with
            SId s -> [| (L.build_in_bounds_gep (lookup s)
                        [| L.const_int i32_t 0 |] "" builder) |]
          | SLintlit i -> [| llit_helper i |]
          | _     -> [| expr builder ptr |]) "printl" builder
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
      (* | _ -> L.const_int i32_t 0 unused *)
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
                              | _ -> L.build_ret (expr builder e) builder );
                     builder
      | SIf (predicate, then_stmt, else_stmt) ->
         let int_val = expr builder predicate in
         let bool_val = L.build_icmp L.Icmp.Ne int_val (L.const_int i32_t 0) "tmp" builder in
         (*L.const_int i1_t (* (if int_val = (L.const_int i32_t 0) then 0 else 1)*)
         ignore(match int_val with
           (L.const_int i32_t 0) -> 0
         | (L.const_int i32_t _) -> 1
         | _ -> raise(Failure "case")
         )*)
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
          let int_val = expr pred_builder predicate in
	  let bool_val = (L.build_icmp L.Icmp.Ne int_val (L.const_int i32_t 0)) "tmp" pred_builder in
          
	  let merge_bb = L.append_block context "merge" the_function in
	  ignore(L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) -> stmt builder
	    ( SBlock [SExpr e1 ; SWhile (e2, SBlock [body ; SExpr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (*#TODO:  We need some code to clear our lints so free all at the end of this function
       We will iterate through our locals map and add clears at the end of each of
       them
       have map that contains all the lints (does local vars work for this?) if not
       we need to make new map.
       Function called for each lint, check elt match type with A.Lint getelementptr inbounds
       and pass to lclear_t same way we pass stuff to printl.
       *)

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in

  List.iter build_function_body functions;
  the_module
