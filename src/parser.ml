open Lexer

(* Expression is further decomposed into factors and terms. Factor is the smallest group
 * consisting of constants, unary operator or grouped expressions.
 * Term is defined as multiple factors connected by / or *. 
 * Expression is multiple terms connected by + or - signs. *)
type expression_t =
  ConstantIntExp of int
  | GroupedExpression of expression_t
  | NegateOp of expression_t
  | LogicalNegateOp of expression_t
  | ComplementOp of expression_t
  | MultiExp of expression_t * expression_t 
  | DivideExp of expression_t * expression_t 
  | AdditionExp of expression_t * expression_t 
  | MinusExp of expression_t * expression_t 

type statement_t =
  ReturnStatement of expression_t

type function_t =
  IntFunction of statement_t

type program_t =
  Program of function_t

let print_ast ast =
  let rec print_expression spaces exp =
    match exp with
    | ConstantIntExp n -> (String.make spaces ' ') ^ "IntegerExpression: " ^ (string_of_int n) ^ "\n"
    | NegateOp exp -> (String.make spaces ' ') ^ "NegateOp:\n" ^ (print_expression (spaces + 1) exp)
    | LogicalNegateOp exp -> (String.make spaces ' ') ^ "LogicalNegateOp:\n" ^ (print_expression (spaces + 1) exp)
    | ComplementOp exp -> (String.make spaces ' ') ^ "ComplementOp:\n" ^ (print_expression (spaces + 1) exp)
    | GroupedExpression exp -> (String.make spaces ' ') ^ "Grouped:\n" ^ (print_expression (spaces + 1) exp)
    | MultiExp (exp1, exp2) -> (String.make spaces ' ') ^ "Multiplication of \n" ^ (print_expression (spaces+1) exp1) ^ (print_expression (spaces+1) exp2)
    | DivideExp (exp1, exp2) -> (String.make spaces ' ') ^ "Division of \n" ^ (print_expression (spaces+1) exp1) ^ (print_expression (spaces+1) exp2)
    | AdditionExp (exp1, exp2) ->  (String.make spaces ' ') ^ "Addition of \n" ^ (print_expression (spaces+1) exp1) ^ (print_expression (spaces+1) exp2)
    | MinusExp (exp1, exp2) ->  (String.make spaces ' ') ^ "Minus of \n" ^ (print_expression (spaces+1) exp1) ^ (print_expression (spaces+1) exp2)
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

let rec parse_factor tokens =
  (* Parses a factor. *)
  match tokens with
  | [] -> fail "Empty expression."
  | IntegerLiteral a :: r -> ConstantIntExp (int_of_string a), r
  | Negation :: r ->
      (let exp, left = parse_factor r in
      NegateOp exp, left)
  | LogicalNegation :: r ->
      (let exp, left = parse_factor r in
      LogicalNegateOp exp, left)
  | BitComplement :: r ->
      (let exp, left = parse_factor r in
      ComplementOp exp, left)
  | LeftParentheses :: r ->
      (let exp, left = parse_expression r in
      match left with
      | RightParentheses :: r -> GroupedExpression exp, r
      | _ -> fail "Expecting RightParentheses.")
  | a :: r -> fail ("Unexpected token: " ^ (print_token a))
and parse_term tokens =
  (* Parses a term. *)
  let rec parse_term_rec previous_factor tokens =
    match tokens with
    | Multiplication :: r -> 
        let factor, left = parse_factor r in
        parse_term_rec (MultiExp(previous_factor, factor)) left
    | Division :: r ->
        let factor, left = parse_factor r in
        parse_term_rec (DivideExp(previous_factor, factor)) left
    | _ -> 
        previous_factor, tokens
  in
  let factor, left = parse_factor tokens in
  parse_term_rec factor left
and parse_expression tokens =
  (* Parses an expression. *)
  let rec parse_expression_rec previous_term tokens =
    match tokens with
    | Addition :: r -> 
        let term, left = parse_term r in
        parse_expression_rec (AdditionExp(previous_term, term)) left
    | Negation :: r ->
        let term, left = parse_term r in
        parse_expression_rec (MinusExp(previous_term, term)) left
    | _ -> 
        previous_term, tokens
  in
  let term, left = parse_term tokens in
  parse_expression_rec term left

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
