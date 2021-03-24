(* Compiler command centre: tell sequence of actions here *)

type action = Ast | Sast | LLVM_IR | Compile

let () = (* don't care about return type *)
  let action = ref Compile in (* set default? *)
  let set_action a () = action := a in
  let options = [
    ("-a", Arg.Unit (set_action Ast), "Print the AST");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in (* Only one mode for now *)
  let usage_msg = "usage: ./prime.native [-a|-c] <filename>" in
  let channel = ref stdin in
  (* take the options and a function that takes filename and opens it for reading *)
  Arg.parse options (fun filename -> channel := open_in filename) usage_msg;

  (* Start reading input *)
  let lexbuf = Lexing.from_channel !channel in (* ! operator dereferences *)
  (* Construct AST *)
  let ast = Parser.program Scanner.token lexbuf in
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | _ -> let sast = Semant.check ast in
    match !action with (* add other options to stop at later *)
      Ast     -> ()
    | Compile -> let modu =
        Codegen.translate sast in
          Llvm_analysis.assert_valid_module modu;
          print_string (Llvm.string_of_llmodule modu)
    | _       -> raise (Failure "invalid option provided")
