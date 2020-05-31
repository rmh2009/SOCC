open Codegen_util

module CGS = Codegen.MakeCodeGen(Codegen_util.MakeCodeGenUtil (System32Bit))

let _ =
  let print = false in
  let ast = Parser.get_ast (Lexer.parse_tokens (Lexer.read_file_content "test.cc")) in
  if print then Printf.printf "\nParsed AST: \n%s" (Debug.print_ast ast);

  let code = CGS.generate_assembly ast in

  if print then Printf.printf "\nGenerated Code:\n%s\n" code;
  let oc = open_out "assembly.s" in
  Printf.fprintf oc "%s\n" code
