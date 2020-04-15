open Lexer

type expression_t =
  ConstantIntExp of int

type statement_t =
  ReturnStatement of expression_t

type function_t =
  IntFunction of statement_t

type program_t =
  Program of function_t

let print_ast ast =
  let print_expression spaces exp =
    match exp with
    | ConstantIntExp n -> (String.make spaces ' ') ^ "IntegerExpression: " ^ (string_of_int n) ^ "\n"
  in
  let print_statement spaces st =
    match st with
    | ReturnStatement exp -> (String.make spaces ' ') ^ "Return Statement: \n" ^ (print_expression (spaces+1) exp)
  in
  let print_function spaces f =
    match f with
    | IntFunction st -> (String.make spaces ' ') ^ "Function: \n" ^ (print_statement (spaces+1) st)
  in
  match ast with
  | Program f -> "Program: \n" ^ (print_function 1 f)

exception TokenError of string

let fail message =
  raise (TokenError message)

(* Parses tokens to get an expression, returns the expression and the remaining tokens. *)
let parse_expression tokens =
  match tokens with
  | [] -> fail "Empty expression."
  | IntegerLiteral a :: r -> ConstantIntExp (int_of_string a), r
  | a :: r -> fail ("Invalid token in parse_expression: " ^ (print_token a))

(* Parses tokens to get a statement, returns the statement and the remaining tokens. *)
let parse_statement tokens =
  match tokens with
  | [] -> fail "Empty statement."
  | ReturnKeyword :: r ->
      let expression, left_tokens = parse_expression r in
      ( match left_tokens with
      | Semicolon :: r -> ReturnStatement expression, r
      | _ -> fail "Expecting semicolon." )
  | a :: r -> fail ("Unrecognized token in parse_statement: " ^ (print_token a))

(* Parses tokens to get a function, returns the function and the remaining tokens. *)
let parse_function tokens =
  match tokens with
  | [] -> fail "Empty function."
  | IntKeyword :: MainKeyword :: LeftParentheses :: RightParentheses:: LeftBrace :: r ->
      let statement, left_tokens = parse_statement r in
      ( match left_tokens with
      | RightBrace :: r -> IntFunction statement, r
      | _ -> fail "Expecting right brace in parse_function." )
  | a :: r -> fail ("Unrecognized token in parse_function: " ^ (print_token a))

(* Parses tokens to get a program, returns the program and possibly remaining tokens. *)
let parse_program tokens =
  match tokens with
  | [] -> fail "Empty program."
  | a -> let func, r = parse_function a in
  Program func, r

(* Parses tokens to get the AST in program_t. *)
let get_ast tokens =
  let program, left_tokens = parse_program tokens in
  program

let _ =
  let ast = get_ast (parse_tokens (read_file_content "test.cc"))
  in
  Printf.printf "\nParsed AST: \n%s" (print_ast ast)
