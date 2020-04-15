open Lexer
open Parser

(* Generates the assembly code as a string given the ast in Parser.program_t type. *)
let generate_assembly ast =
  let buf = Buffer.create 32 in
  let generate_statement st =
    match st with
    | ReturnStatement exp ->
        (match exp with
        | ConstantIntExp n -> Buffer.add_string buf ("movl    $" ^ (string_of_int n) ^ ", %eax\nret\n" ))
  in
  let generate_function f =
    match f with
    | IntFunction st ->
        Buffer.add_string buf ".globl _main\n_main:\n";
        generate_statement st
  in
  match ast with
  | Program f ->
      generate_function f;
  Buffer.contents buf


let _ =
  let ast = get_ast (Lexer.parse_tokens (Lexer.read_file_content "test.cc"))
  in
  Printf.printf "\nParsed AST: \n%s" (print_ast ast);
  let code = generate_assembly ast
  in
  Printf.printf "\nGenerated Code:\n%s\n" code;
  let oc = open_out "assembly.s" in
  Printf.fprintf oc "%s\n" code





