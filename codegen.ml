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


  (* LINTS *)
  let linit_t : L.lltype =
      L.function_type i32_t [| L.pointer_type mpz_t; string_t; i32_t |] in
  let linit_func : L.llvalue =
      L.declare_function "__gmpz_init_set_str" linit_t the_module in
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
      L.function_type string_t [| string_t; string_t |] in
  let ladd_func : L.llvalue =
      L.declare_function "add" ladd_t the_module in
  let lpow_t : L.lltype =
      L.function_type string_t [| string_t; i32_t |] in
  let lpow_func : L.llvalue =
      L.declare_function "power" lpow_t the_module in

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
       Returns: Array with mpz_t pointer to be used for function args *)
    let llit_helper i =
      let lstr = L.build_global_stringptr i "string" builder
      and space = L.build_alloca (ltype_of_typ A.Lint) "" builder in
      let calls = ignore(L.build_call linit_func
        [| L.build_in_bounds_gep space [| L.const_int i32_t 0 |] "" builder; lstr; L.const_int i32_t 10 |]
        "__gmpz_init_set_str" builder);
        [| L.build_in_bounds_gep space [| L.const_int i32_t 0 |] "" builder |]
      in calls
    in

    (* Construct code for an expression; return its value *)
    let rec expr builder ((_, e) : sexpr) = match e with
        SStrlit i     -> L.build_global_stringptr i "string" builder
      | SLintlit i    -> L.build_global_stringptr i "string" builder
      | SLit i        -> L.const_int i32_t i
      | SPtlit (i, j) ->
              let e1' = expr builder i
              and e2' = expr builder j in
              L.build_call init_point_func [| e1' ; e2' |] "Point" builder
      | SNoexpr       -> L.const_int i32_t 0
      | SId s         -> L.build_load (lookup s) s builder
      | SAssign (s, ((A.Lint, _) as e1)) -> let string_val = expr builder e1 and zero = L.const_int i32_t 0 in
        L.build_call linit_func
        [| L.build_in_bounds_gep (lookup s) [| zero |] "" builder;
        string_val; L.const_int i32_t 10 |] "__gmpz_init_set_str" builder
      | SAssign (s, e) -> let e' = expr builder e in
                           ignore(L.build_store e' (lookup s) builder); e'
      | SBinop ((A.Lint, _) as e1, operator, e2) ->
              let e1' = expr builder e1
              and e2' = expr builder e2 in
              (match operator with
              | A.Add     -> L.build_call ladd_func [| e1'; e2' |] "add" builder
              | A.Pow     -> L.build_call lpow_func [| e1'; e2' |] "power" builder
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
              ) e1' e2' "tmp" builder
      | SUnop(op, ((t, _) as e)) ->
              let e' = expr builder e in
              (match op with
                A.Neg     -> L.build_neg
              | A.Not     -> L.build_not) e' "tmp" builder
      | SCall ("print", [e]) -> (*keep print delete printb printf*)
	        L.build_call printf_func [| int_format_str ; (expr builder e) |]
	        "printf" builder
      | SCall ("prints", [e]) -> (*print string*)
          L.build_call printf_func [| string_format_str ; (expr builder e) |]
          "printf" builder
      | SCall ("printl", [(_, e)]) ->
          (* print_string "Printing lint"; *)
          (* L.build_call printf_func [| string_format_str ; (expr builder e) |]
          "printf" builder *)
          L.build_call lprint_func
          (match e with
            SId s -> [| (L.build_in_bounds_gep (lookup s) [| L.const_int i32_t 0 |] "" builder) |]
          | SLintlit i -> llit_helper i
          | _     -> raise (Failure("printl param not yet allowed"))) "printl" builder
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
                            | _ -> L.build_ret (expr builder e) builder );
                     builder
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (SBlock fdecl.sbody) in

    (*#TODO:  We need some code to clear our lints so free all at the end of this function
       We will iterate through our locals map and add clears at the end of each of
       them *)

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.styp with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))

  in

  List.iter build_function_body functions;
  the_module
