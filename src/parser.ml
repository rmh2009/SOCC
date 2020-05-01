open Lexer

(* Expression is further decomposed into factors and terms. Factor is the smallest group
 * consisting of constants, unary operator or grouped expressions.
 * Term is defined as multiple factors connected by / or *. 
 * Expression is multiple terms connected by + or - signs. *)
type expression_t =
  ConstantIntExp of int
  | VarExp of string
  | GroupedExpression of expression_t
  | NegateOp of expression_t
  | LogicalNegateOp of expression_t
  | ComplementOp of expression_t
  | MultiExp of expression_t * expression_t 
  | DivideExp of expression_t * expression_t 
  | AdditionExp of expression_t * expression_t 
  | MinusExp of expression_t * expression_t 
  | LessExp of expression_t * expression_t 
  | LessOrEqualExp of expression_t * expression_t 
  | GreaterExp of expression_t * expression_t 
  | GreaterOrEqualExp of expression_t * expression_t 
  | EqualExp of expression_t * expression_t 
  | NotEqualExp of expression_t * expression_t 
  | OrExp of expression_t * expression_t 
  | AndExp of expression_t * expression_t 
  | AssignExp of string * expression_t
  | ConditionExp of expression_t * expression_t * expression_t
  | FunctionCallExp of string * expression_t list

type statement_t =
  ReturnStatement of expression_t
  | ExpressionStatement of expression_t option
  | ConditionalStatement of expression_t * statement_t * statement_t option
  | CompoundStatement of block_item_t list
  | ForStatement of expression_t option * expression_t * expression_t option * statement_t
  | ForDeclStatement of declare_t * expression_t * expression_t option * statement_t
  | WhileStatement of expression_t * statement_t
  | DoStatement of statement_t * expression_t
  | BreakStatement
  | ContinueStatement

and declare_t =
  DeclareStatement of string * expression_t option

and block_item_t =
  StatementItem of statement_t
  | DeclareItem of declare_t

type function_t =
  IntFunction of string * string list * block_item_t list option

type program_t =
  Program of function_t list

exception TokenError of string

let fail message =
  raise (TokenError message)

let peek tokens =
  match tokens with
  | [] -> fail "No tokens left in peek tokens."
  | a :: _ -> a

let consume_token token tokens =
  match tokens with
  | hd :: tl -> if hd = token then tl
  else fail ("Failed to consume token: " ^ (print_token token) ^ ", saw token: " ^ (print_token hd))
  | [] -> fail ("No tokens left in consuming tokens, expecting token: " ^ (print_token token))

let print_ast ast =
  let rec print_expression spaces exp =
    let rec print_binary spaces op exp1 exp2 =
      (String.make spaces ' ') ^ op ^ " of \n" ^ (print_expression (spaces+1) exp1) ^ (print_expression (spaces+1) exp2)
    in
    match exp with
    | VarExp (a) -> (String.make spaces ' ') ^ "Ref of " ^ a ^ "\n"
    | ConstantIntExp n -> (String.make spaces ' ') ^ "IntegerExpression: " ^ (string_of_int n) ^ "\n"
    | NegateOp exp -> (String.make spaces ' ') ^ "NegateOp:\n" ^ (print_expression (spaces + 1) exp)
    | LogicalNegateOp exp -> (String.make spaces ' ') ^ "LogicalNegateOp:\n" ^ (print_expression (spaces + 1) exp)
    | ComplementOp exp -> (String.make spaces ' ') ^ "ComplementOp:\n" ^ (print_expression (spaces + 1) exp)
    | GroupedExpression exp -> (String.make spaces ' ') ^ "Grouped:\n" ^ (print_expression (spaces + 1) exp)
    | MultiExp (exp1, exp2) -> (String.make spaces ' ') ^ "Multiplication of \n" ^ (print_expression (spaces+1) exp1) ^ (print_expression (spaces+1) exp2)
    | DivideExp (exp1, exp2) -> print_binary spaces "Division" exp1 exp2 
    | AdditionExp (exp1, exp2) ->  print_binary spaces "Addition" exp1 exp2 
    | MinusExp (exp1, exp2) -> print_binary spaces "Minus" exp1 exp2
    | LessExp (exp1, exp2) -> print_binary spaces "LessThan" exp1 exp2
    | LessOrEqualExp (exp1, exp2) -> print_binary spaces "LessOrEqualThan" exp1 exp2
    | GreaterExp (exp1, exp2)  -> print_binary spaces "GreaterThan" exp1 exp2
    | GreaterOrEqualExp (exp1, exp2)  ->print_binary spaces "GreaterOrEqualThan" exp1 exp2
    | EqualExp (exp1, exp2)  -> print_binary spaces "Equal" exp1 exp2
    | NotEqualExp (exp1, exp2)  -> print_binary spaces "NotEqual" exp1 exp2
    | OrExp (exp1, exp2)  -> print_binary spaces "LogicalOr" exp1 exp2
    | AndExp (exp1, exp2)  -> print_binary spaces "LogicalAnd" exp1 exp2
    | AssignExp (a, st) -> (String.make spaces ' ') ^ "Assignment of " ^ a ^ ":\n" ^ (print_expression (spaces+1) st)
    | ConditionExp(exp1, exp2, exp3) -> ((String.make spaces ' ') ^ "ConditionExp:\n" ^
    (print_expression (spaces+1) exp1) ^
    (print_expression (spaces+1) exp2) ^
    (print_expression (spaces+1) exp3))
    | FunctionCallExp (fname, exps) -> ((String.make spaces ' ') ^ "Function call:\n" ^
    (List.fold_left (fun acc exp -> acc ^ (print_expression (spaces+1) exp)) "" exps))
  in
  let print_expression_option spaces exp_opt =
    match exp_opt with
    | None -> ""
    | Some exp -> print_expression spaces exp
  in
  let rec print_statement spaces st =
    match st with
    | ReturnStatement exp -> (String.make spaces ' ') ^ "Return Statement: \n" ^ (print_expression (spaces+1) exp)
    | ExpressionStatement (Some exp)-> (String.make spaces ' ') ^ "Expression Statement: \n" ^ (print_expression (spaces+1) exp)
    | ExpressionStatement (None)-> (String.make spaces ' ') ^ "Empty expression statement"
    | ConditionalStatement (exp, st1, Some st2) -> ((String.make spaces ' ') ^ "ConditionStatement:\n" ^
    (print_expression (spaces+1) exp) ^
    (print_statement (spaces+1) st1) ^
    (print_statement (spaces+1) st2))
    | CompoundStatement (items) -> (String.make spaces ' ') ^ "CompountStatement:\n" ^
    ( List.fold_left (fun acc st -> acc ^ (print_block_item (spaces+1) st)) "" items)
    | ConditionalStatement (exp, st1, None) -> ((String.make spaces ' ') ^ "ConditionStatement:\n" ^
    (print_expression (spaces+1) exp) ^
    (print_statement (spaces+1) st1))
    | ForStatement (exp1_opt, exp2, exp3_opt, st) -> (String.make spaces ' ') ^ "ForStatement:\n" ^
    (print_expression_option (spaces+1) exp1_opt) ^
    (print_expression (spaces+1) exp2) ^
    (print_expression_option (spaces+1) exp3_opt) ^
    (print_statement (spaces+1) st)
    | ForDeclStatement(decl, exp2, exp3_opt, st) -> (String.make spaces ' ') ^ "ForDeclareStatement:\n" ^
    (print_declare (spaces+1) decl) ^
    (print_expression (spaces+1) exp2) ^
    (print_expression_option (spaces+1) exp3_opt) ^
    (print_statement (spaces+1) st)
    | DoStatement (st, exp) -> (String.make spaces ' ') ^ "DoStatementt:\n" ^
    (print_expression (spaces+1) exp) ^
    (print_statement (spaces+1) st)
    | WhileStatement (exp, st) -> (String.make spaces ' ') ^ "WhileStatementt:\n" ^
    (print_expression (spaces+1) exp) ^
    (print_statement (spaces+1) st)
    | ContinueStatement -> (String.make spaces ' ') ^ "Continue\n"
    | BreakStatement -> (String.make spaces ' ') ^ "Break\n"

  and print_declare spaces st =
    match st with
    | DeclareStatement (a, None) -> (String.make spaces ' ') ^ "Declare Statement of " ^ a ^ "\n" ;
    | DeclareStatement (a, Some exp) -> (String.make spaces ' ') ^ "Declare and Init Statement of " ^ a ^ ":\n" ^ (print_expression (spaces+1) exp)
  and print_block_item spaces item =
    match item with
    | StatementItem st -> (String.make spaces ' ') ^ "StatementItem:\n" ^ (print_statement (spaces+1) st)
    | DeclareItem decl -> (String.make spaces ' ') ^ "DeclareItem:\n" ^ (print_declare (spaces+1) decl)
  in
  let print_params params =
    List.fold_left (fun acc param -> acc ^ param ^ ",") "" params
  in
  let print_function spaces f =
    let print_statements spaces sts_opt =
      match sts_opt with
      | None -> "Empty Statements (Function declaration only.)"
      | Some statements ->
          List.fold_left (fun acc st -> acc ^ (print_block_item spaces st)) "" statements
    in
    match f with
    | IntFunction (a, params, sts) -> ((String.make spaces ' ') ^ "Function " ^ a ^
    ": , Params: " ^ (print_params params) ^ "\n" ^ (print_statements (spaces+1) sts))
  in
  match ast with
  | Program fns -> "Program: \n" ^ (List.fold_left (fun acc f -> acc ^ (print_function 1 f) ^ "\n") "" fns)

let rec parse_factor tokens =
  (* Parses a factor. *)
  let rec parse_call_params acc tokens =
    match tokens with
    | [] -> fail "saw empty tokens in parse_call_params."
    | RightParentheses :: r -> (List.rev acc), r
    | Comma :: r -> let exp, r2 = parse_expression r in
    parse_call_params (exp :: acc) r2
    | a -> let exp, r = parse_expression a in
    parse_call_params (exp :: acc) r
  in
  match tokens with
  | [] -> fail "Empty expression."
  | IntegerLiteral a :: r -> ConstantIntExp (int_of_string a), r
  | Identifier(a) :: r ->
      (match r with
      | LeftParentheses :: r2 ->
          let params, r3 = parse_call_params [] r2 in
          FunctionCallExp(a, params), r3
      | r2 -> VarExp (a), r2)
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
  | a :: r -> fail ("Unexpected token in parse_factor: " ^ (print_token a))

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

and parse_additive_expression tokens =
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

and parse_relational_expression tokens =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | Less :: r -> 
        let exp, left = parse_additive_expression r in
        parse_expression_rec (LessExp(previous_exp, exp)) left
    | LessOrEqual :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (LessOrEqualExp(previous_exp, exp)) left
    | Greater :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (GreaterExp(previous_exp, exp)) left
    | GreaterOrEqual :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (GreaterOrEqualExp(previous_exp, exp)) left
    | _ -> 
        previous_exp, tokens
  in
  let exp, left = parse_additive_expression tokens in
  parse_expression_rec exp left

and parse_equality_expression tokens =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | Equal :: r -> 
        let exp, left = parse_relational_expression r in
        parse_expression_rec (EqualExp(previous_exp, exp)) left
    | NotEqual :: r ->
        let exp, left = parse_relational_expression r in
        parse_expression_rec (NotEqualExp(previous_exp, exp)) left
    | _ -> 
        previous_exp, tokens
  in
  let exp, left = parse_relational_expression tokens in
  parse_expression_rec exp left

and parse_logical_and_expression tokens =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | And :: r -> 
        let exp, left = parse_equality_expression r in
        parse_expression_rec (AndExp(previous_exp, exp)) left
    | _ -> 
        previous_exp, tokens
  in
  let exp, left = parse_equality_expression tokens in
  parse_expression_rec exp left

and parse_logical_or_expression tokens =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | Or :: r -> 
        let exp, left = parse_logical_and_expression r in
        parse_expression_rec (OrExp(previous_exp, exp)) left
    | _ -> 
        previous_exp, tokens
  in
  let exp, left = parse_logical_and_expression tokens in
  parse_expression_rec exp left

and parse_conditional_expression tokens =
  let first_exp, left = parse_logical_or_expression tokens in
  match left with
  | QuestionMark :: r ->
      let second_exp, r2 = parse_expression r in
      ( match r2 with
      | Colon :: r3 ->
          let third_exp, r4 = parse_expression r3 in
          ConditionExp(first_exp, second_exp, third_exp), r4
      | a :: _ -> fail ("Expecting Colon, but see " ^ (print_token a))
      | [] -> fail ("Expecting Colon, but see end of file."))
  | _ -> first_exp, left

and parse_expression tokens =
  (* Parses an expression. *)
  match tokens with
  | Identifier (a) :: Assignment :: r ->
      let exp, left = parse_expression r in
      AssignExp (a, exp), left
  | l -> parse_conditional_expression l

(* Returns expression Option. If the next token is Semi-colon will return None. *)
let parse_expression_opt tokens = 
  match tokens with
  | Semicolon :: _ -> None, tokens
  | RightParentheses :: _ -> None, tokens
  | _ -> let exp, r = parse_expression tokens in
  (Some exp), r

(* Does not consume the trailing Semicolon (if there is one). *)
let rec parse_statement tokens =
  let parse_expression_opt_or_declare tokens = 
    if (peek tokens) = Semicolon then None, None, tokens
    else if peek tokens = IntKeyword then
      let declare, r = parse_declare tokens in
      None, Some declare, r
    else
      let exp, r = parse_expression tokens in
      Some exp, None, r
  in
          match tokens with
  | [] -> fail "Empty statement."
  | ReturnKeyword :: r ->
      let expression, left = parse_expression r in
      ReturnStatement expression, left
  | LeftBrace :: r ->
      let block_items, left = parse_block_items [] r in
      CompoundStatement(block_items), left
  | BreakKeyword :: r -> BreakStatement, r
  | ContinueKeyword :: r -> ContinueStatement, r
  | WhileKeyword :: LeftParentheses :: r -> 
      let exp, r = parse_expression r in
      let r = consume_token RightParentheses r in
      let st, r = parse_statement r in
      WhileStatement(exp, st), r
  | DoKeyword :: r ->
      let st, r = parse_statement r in
      let r = consume_token WhileKeyword r in
      let r = consume_token LeftParentheses r in
      let exp, r = parse_expression r in
      let r = consume_token RightParentheses r in
      DoStatement(st, exp), r
  | ForKeyword :: LeftParentheses :: left ->
      let exp1, declare, left = parse_expression_opt_or_declare left in
      let left = consume_token Semicolon left in
      let exp2, left = parse_expression_opt left in
      let left = consume_token Semicolon left in
      let exp3, left = parse_expression_opt left in
      let exp2 = (match exp2 with
      | None -> ConstantIntExp(1)
      | Some exp -> exp)
      in
      let left = consume_token RightParentheses left in
      let st, left = parse_statement left in
      (match declare with
      | None -> ForStatement (exp1, exp2, exp3, st), left
      | Some d -> ForDeclStatement(d, exp2, exp3, st), left)
  | IfKeyword :: LeftParentheses :: r ->
      let cond_exp, r1 = parse_expression r in
      ( match r1 with
      | RightParentheses :: r2 ->
          let first_st, r3 = parse_statement r2 in
          ( match r3 with
          | ElseKeyword :: r4 ->
              let second_st, r5 = parse_statement r4 in
              ConditionalStatement(cond_exp, first_st, Some second_st), r5
          | _ -> ConditionalStatement(cond_exp, first_st, None), r3)
      | _ -> fail "Expecting right parentheses in parse_statement")
  | l -> 
      let exp, left = parse_expression l in
      ExpressionStatement (Some exp), left

(* Does not consume the trailing Semicolon. *)
and parse_declare tokens =
  match tokens with
  | [] -> fail "Empty declare statement."
  | IntKeyword :: Identifier (a) :: Assignment :: r ->
      let exp, left = parse_expression r in
      DeclareStatement (a, Some exp), left
  | IntKeyword :: Identifier (a) :: r->
      DeclareStatement(a, None), r
  | a :: r -> fail ("Unexpected token in parse_declare " ^ (print_token a))

(* Parses tokens to get a statement, returns the statement and the remaining tokens. *)
(* This function consumes the trailing ; if it exists. *)
and parse_block_item tokens =
  let result, r =
          match tokens with
  | [] -> fail "Empty statement or declare statement."
  | IntKeyword :: Identifier (a) :: r ->
      let declare, r = parse_declare tokens in
      DeclareItem declare, r
  | _ -> 
      let st, r = parse_statement tokens in
      StatementItem st, r
  in
  (* CompoundStatement does not have a trailing semi-colon. *)
  match result with
  | StatementItem(CompoundStatement (_)) -> result, r
  | StatementItem(ConditionalStatement(_, _, Some CompoundStatement(_))) -> result, r
  | StatementItem(ConditionalStatement(_, CompoundStatement(_), None)) -> result, r
  | StatementItem(ForStatement(_, _, _, CompoundStatement(_))) -> result, r
  | StatementItem(ForDeclStatement(_, _, _, CompoundStatement(_))) -> result, r
  | StatementItem(WhileStatement(_, CompoundStatement(_))) -> result, r
  | StatementItem(DoStatement(_)) -> result, r
  | _ -> result, (consume_token Semicolon r)

and parse_block_items acc tokens =
    match tokens with 
      | RightBrace :: r -> (List.rev acc), r
      | l -> let statement, left = parse_block_item tokens 
      in
      parse_block_items (statement :: acc) left

(* Parses tokens to get a function, returns the function and the remaining tokens. *)
and parse_function tokens =
  let rec parse_parameters acc tokens =
    match tokens with
    | IntKeyword :: Identifier(a) :: r -> parse_parameters (a :: acc) r
    | Comma :: IntKeyword :: Identifier(a) :: r -> parse_parameters (a :: acc) r
    | RightParentheses :: r -> (List.rev acc), tokens
    | a :: _ -> fail ("Unexpected token in parse_parameters: " ^ (print_token a))
  in
  match tokens with
  | [] -> fail "Empty function."
  | IntKeyword :: MainKeyword :: LeftParentheses ::  r ->
      let params, r = parse_parameters [] r in
      let r = consume_token RightParentheses r in
      let r = consume_token LeftBrace r in
      let statements, r = parse_block_items [] r in
      IntFunction ("main", params, Some statements), r
  | IntKeyword :: Identifier (fname) :: LeftParentheses :: r ->
      let params, r = parse_parameters [] r in
      let r = consume_token RightParentheses r in
      let r = consume_token LeftBrace r in
      let statements, r = parse_block_items [] r in
      IntFunction (fname, params, Some statements), r
  | a :: r -> fail ("Unexpected token in parse_function: " ^ (print_token a))

let rec parse_functions acc tokens =
  match tokens with
  | [] -> (List.rev acc), tokens
  | a -> let f, r = parse_function a in parse_functions (f :: acc) r

(* Parses tokens to get a program, returns the program and possibly remaining tokens. *)
let parse_program tokens =
  match tokens with
  | [] -> fail "Empty program."
  | a -> let functions, r = parse_functions [] a in
  Program functions, r

  (* Parses tokens to get the AST in program_t. *)
let get_ast tokens =
  let program, left_tokens = parse_program tokens in
  program

let _ =
  let ast = get_ast (parse_tokens (read_file_content "test.cc"))
  in
  print_endline "Parse ast complete.";
  Printf.printf "\nParsed AST: \n%s" (print_ast ast)
