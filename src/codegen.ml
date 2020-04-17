open Lexer
open Parser

(* Generates the assembly code as a string given the ast in Parser.program_t type. *)
let generate_assembly ast =
  let buf = Buffer.create 32 in
  let rec generate_expression exp =
    match exp with
        | ConstantIntExp n -> Buffer.add_string buf ("movl    $" ^ (string_of_int n) ^ ", %eax\n" )
        | NegateOp exp ->
            generate_expression exp;
            Buffer.add_string buf "neg    %eax\n"
        | LogicalNegateOp exp ->
            generate_expression exp;
            Buffer.add_string buf "cmpl    $0, %eax\nmovl     $0, %eax\nsete    %al\n"
        | ComplementOp exp ->
            generate_expression exp;
            Buffer.add_string buf "not    %eax\n"
        | GroupedExpression exp ->
            generate_expression exp
        | AdditionExp (exp1, exp2) ->
            generate_expression exp1;
            Buffer.add_string buf "push    %eax\n";
            generate_expression exp2;
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "addl    %ecx, %eax\n"
        | MinusExp (exp1, exp2) ->
            generate_expression exp2;
            Buffer.add_string buf "push    %eax\n";
            generate_expression exp1;
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "subl    %ecx, %eax\n"
        | MultiExp (exp1, exp2) ->
            generate_expression exp1;
            Buffer.add_string buf "push    %eax\n";
            generate_expression exp2;
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "imul    %ecx, %eax\n"
        | DivideExp (exp1, exp2) ->
            generate_expression exp2;
            Buffer.add_string buf "push    %eax\n";
            generate_expression exp1;
            Buffer.add_string buf "cdq\n";
            Buffer.add_string buf "pop    %ecx\n";
            Buffer.add_string buf "idvl    %ecx\n"
  in
  let generate_statement st =
    match st with
    | ReturnStatement exp ->
        generate_expression exp;
        Buffer.add_string buf "ret\n"
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
  let ast = get_ast (parse_tokens (read_file_content "test.cc"))
  in
  Printf.printf "\nParsed AST: \n%s" (print_ast ast);
  let code = generate_assembly ast
  in
  Printf.printf "\nGenerated Code:\n%s\n" code;
  let oc = open_out "assembly.s" in
  Printf.fprintf oc "%s\n" code





