(* Compiler command centre: tell sequence of actions here *)

type action = Ast | Sast | LLVM_IR | Compile

let () = (* don't care about return type *)
  let action = ref Compile in (* set default? *)
  let set_action a () = action := a in
  let options = [] in (* Only one mode for now *)
  let usage_msg = "usage: ./prime.native <filename>" in
  let channel = ref stdin in
  (* take the options and a function that takes filename and opens it for reading *)
  Arg.parse options (fun filename -> channel := open_in filename) usage_msg;

  (* Start reading input *)
  let lexbuf = Lexing.from_channel !channel in (* ! operator dereferences *)
  (* Construct AST *)
  let ast = Parser.program Scanner.token lexbuf in
    match !action with (* add other options to stop at later *)
    | Compile -> 
        let sast = Semant.check ast in
        let mod = Codegen.translate sast in
          Llvm_analysis.assert_valid_module mod;
          print_string "Done"