open Codegen_util
module CG = MakeCodeGenUtil (System32Bit)

exception TestError of string

let expect_str_eq a b =
  if a = b then ()
  else
    raise
      (TestError
         (Printf.sprintf "Test failed, expecting >%s<, actual >%s<." b a))

let expect_data_type_eq a b =
  if a = b then ()
  else
    raise
      (TestError
         (Printf.sprintf "Test failed, expecting type: %s, actual type: %s."
            (Debug.print_data_type b) (Debug.print_data_type a)))

let codegen_util_test () =
  expect_str_eq
    (CG.gen_command (Mov (Reg BP, Reg SP)) (PointerType UnknownType))
    "    movl    %ebp,%esp\n";
  expect_str_eq
    (CG.gen_command (Mov (RegV BP, Reg SP)) (PointerType UnknownType))
    "    movl    (%ebp),%esp\n";
  expect_str_eq
    (CG.gen_command (Mov (RegV BP, Reg AX)) IntType)
    "    movl    (%ebp),%eax\n";
  expect_str_eq
    (CG.gen_command (Mov (Disp (8, BP), Reg BX)) IntType)
    "    movl    8(%ebp),%ebx\n";
  expect_str_eq
    (CG.gen_command (Mov (Disp (8, BP), RegV BX)) (PointerType UnknownType))
    "    movl    8(%ebp),(%ebx)\n";
  expect_str_eq
    (CG.gen_command (Mov (Reg AX, Disp (8, BP))) IntType)
    "    movl    %eax,8(%ebp)\n";
  ()

let typeutil_test () =
  let name, dtype, _ =
    Typeutil.parse_data_type (Lexer.parse_tokens "int a[2][3]")
  in
  expect_data_type_eq dtype (ArrayType (ArrayType (IntType, 3), 2));
  expect_str_eq name "a";

  let name, dtype, _ =
    Typeutil.parse_data_type (Lexer.parse_tokens "int *a[2][3]")
  in
  expect_data_type_eq dtype (ArrayType (ArrayType (PointerType IntType, 3), 2));
  expect_str_eq name "a";

  let name, dtype, _ =
    Typeutil.parse_data_type (Lexer.parse_tokens "char** p[5]")
  in
  expect_data_type_eq dtype (ArrayType (PointerType (PointerType CharType), 5));
  expect_str_eq name "p";
  ()

let _ =
  codegen_util_test ();
  typeutil_test ();

  print_endline "Passed!"
