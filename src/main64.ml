open Codegen_util

module CGS = Codegen.MakeCodeGen(Codegen_util.MakeCodeGenUtil (System64Bit))

let _ =
  let ast = Parser.get_ast (Lexer.parse_tokens (Lexer.read_file_content "test.cc")) in
  Printf.printf "\nParsed AST: \n%s" (Debug.print_ast ast);

  let code = CGS.generate_assembly ast in

  Printf.printf "\nGenerated Code:\n%s\n" code;
  let oc = open_out "assembly.s" in
  Printf.fprintf oc "%s\n" code
