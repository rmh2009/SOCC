open Lexer
open Parser
open Codegen

let _ =
  let ast = get_ast (parse_tokens (read_file_content "test.cc")) in
  Printf.printf "\nParsed AST: \n%s" (print_ast ast);
  let code = generate_assembly ast in
  Printf.printf "\nGenerated Code:\n%s\n" code;
  let oc = open_out "assembly.s" in
  Printf.fprintf oc "%s\n" code
