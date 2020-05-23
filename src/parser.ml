open Lexer
open Type
open Typeutil

let is_assignable (exp : expression_t) : bool =
  match exp with
  | VarExp _ -> true
  | ArrayIndexExp (_, _) -> true
  | DereferenceExp exp -> true
  | _ -> false

let fail message = raise (TokenError message)

let peek (tokens : token_t list) : token_t =
  match tokens with [] -> fail "No tokens left in peek tokens." | a :: _ -> a

let consume_token (token : token_t) (tokens : token_t list) : token_t list =
  match tokens with
  | hd :: tl ->
      if hd = token then tl
      else
        fail
          ( "Failed to consume token: " ^ print_token token ^ ", saw token: "
          ^ print_token hd )
  | [] ->
      fail
        ( "No tokens left in consuming tokens, expecting token: "
        ^ print_token token )

let rec print_expression spaces exp =
  let rec print_binary spaces op exp1 exp2 =
    String.make spaces ' ' ^ op ^ " of \n"
    ^ print_expression (spaces + 1) exp1
    ^ print_expression (spaces + 1) exp2
  in
  let print_space str = String.make spaces ' ' ^ str in
  let print_exp e = print_expression (spaces + 1) e in
  match exp with
  | VarExp a -> String.make spaces ' ' ^ "Ref of " ^ a ^ "\n"
  | ArrayIndexExp (arr, index) ->
      print_space "ArrayIndex:\n" ^ print_exp arr ^ print_exp index
  | DereferenceExp exp -> print_space "PointerDereference:\n" ^ print_exp exp
  | AddressOfExp exp ->
      String.make spaces ' ' ^ "AddressOf:\n"
      ^ print_expression (spaces + 1) exp
  | ConstantIntExp n ->
      String.make spaces ' ' ^ "IntegerExpression: " ^ string_of_int n ^ "\n"
  | ConstantCharExp n ->
      String.make spaces ' ' ^ "CharExpression: " ^ String.make 1 n ^ "\n"
  | ConstantFloatExp n ->
      String.make spaces ' ' ^ "FloatExpression: " ^ string_of_float n ^ "\n"
  | ConstantStringExp n ->
      String.make spaces ' ' ^ "StringExpression: " ^ n ^ "\n"
  | PreIncExp exp ->
      String.make spaces ' ' ^ "PreInc : " ^ print_expression (spaces + 1) exp
  | PostIncExp exp ->
      String.make spaces ' ' ^ "PostInc : " ^ print_expression (spaces + 1) exp
  | PreDecExp exp ->
      String.make spaces ' ' ^ "PreDec : " ^ print_expression (spaces + 1) exp
  | PostDecExp exp ->
      String.make spaces ' ' ^ "PostDec : " ^ print_expression (spaces + 1) exp
  | NegateOp exp ->
      String.make spaces ' ' ^ "NegateOp:\n" ^ print_expression (spaces + 1) exp
  | LogicalNegateOp exp ->
      String.make spaces ' ' ^ "LogicalNegateOp:\n"
      ^ print_expression (spaces + 1) exp
  | ComplementOp exp ->
      String.make spaces ' ' ^ "ComplementOp:\n"
      ^ print_expression (spaces + 1) exp
  | GroupedExpression exp ->
      String.make spaces ' ' ^ "Grouped:\n" ^ print_expression (spaces + 1) exp
  | MultiExp (exp1, exp2) ->
      String.make spaces ' ' ^ "Multiplication of \n"
      ^ print_expression (spaces + 1) exp1
      ^ print_expression (spaces + 1) exp2
  | DivideExp (exp1, exp2) -> print_binary spaces "Division" exp1 exp2
  | AdditionExp (exp1, exp2) -> print_binary spaces "Addition" exp1 exp2
  | MinusExp (exp1, exp2) -> print_binary spaces "Minus" exp1 exp2
  | LessExp (exp1, exp2) -> print_binary spaces "LessThan" exp1 exp2
  | LessOrEqualExp (exp1, exp2) ->
      print_binary spaces "LessOrEqualThan" exp1 exp2
  | GreaterExp (exp1, exp2) -> print_binary spaces "GreaterThan" exp1 exp2
  | GreaterOrEqualExp (exp1, exp2) ->
      print_binary spaces "GreaterOrEqualThan" exp1 exp2
  | EqualExp (exp1, exp2) -> print_binary spaces "Equal" exp1 exp2
  | NotEqualExp (exp1, exp2) -> print_binary spaces "NotEqual" exp1 exp2
  | OrExp (exp1, exp2) -> print_binary spaces "LogicalOr" exp1 exp2
  | AndExp (exp1, exp2) -> print_binary spaces "LogicalAnd" exp1 exp2
  | AssignExp (a, st) ->
      String.make spaces ' ' ^ "Assignment of :\n"
      ^ print_expression (spaces + 1) a
      ^ print_expression (spaces + 1) st
  | ConditionExp (exp1, exp2, exp3) ->
      String.make spaces ' ' ^ "ConditionExp:\n"
      ^ print_expression (spaces + 1) exp1
      ^ print_expression (spaces + 1) exp2
      ^ print_expression (spaces + 1) exp3
  | FunctionCallExp (fname, exps) ->
      String.make spaces ' ' ^ "Function call -> " ^ fname ^ "\n"
      ^ List.fold_left
          (fun acc exp -> acc ^ print_expression (spaces + 1) exp)
          "" exps

(* Pretty prints an AST. *)
and print_ast (ast : program_t) : string =
  let print_expression_option spaces exp_opt =
    match exp_opt with None -> "" | Some exp -> print_expression spaces exp
  in
  let rec print_statement spaces st =
    match st with
    | ReturnStatement exp ->
        String.make spaces ' ' ^ "Return Statement: \n"
        ^ print_expression (spaces + 1) exp
    | ExpressionStatement (Some exp) ->
        String.make spaces ' ' ^ "Expression Statement: \n"
        ^ print_expression (spaces + 1) exp
    | ExpressionStatement None ->
        String.make spaces ' ' ^ "Empty expression statement"
    | ConditionalStatement (exp, st1, Some st2) ->
        String.make spaces ' ' ^ "ConditionStatement:\n"
        ^ print_expression (spaces + 1) exp
        ^ print_statement (spaces + 1) st1
        ^ print_statement (spaces + 1) st2
    | CompoundStatement items ->
        String.make spaces ' ' ^ "CompountStatement:\n"
        ^ List.fold_left
            (fun acc st -> acc ^ print_block_item (spaces + 1) st)
            "" items
    | ConditionalStatement (exp, st1, None) ->
        String.make spaces ' ' ^ "ConditionStatement:\n"
        ^ print_expression (spaces + 1) exp
        ^ print_statement (spaces + 1) st1
    | ForStatement (exp1_opt, exp2, exp3_opt, st) ->
        String.make spaces ' ' ^ "ForStatement:\n"
        ^ print_expression_option (spaces + 1) exp1_opt
        ^ print_expression (spaces + 1) exp2
        ^ print_expression_option (spaces + 1) exp3_opt
        ^ print_statement (spaces + 1) st
    | ForDeclStatement (decl, exp2, exp3_opt, st) ->
        String.make spaces ' ' ^ "ForDeclareStatement:\n"
        ^ print_declare (spaces + 1) decl
        ^ print_expression (spaces + 1) exp2
        ^ print_expression_option (spaces + 1) exp3_opt
        ^ print_statement (spaces + 1) st
    | DoStatement (st, exp) ->
        String.make spaces ' ' ^ "DoStatementt:\n"
        ^ print_expression (spaces + 1) exp
        ^ print_statement (spaces + 1) st
    | WhileStatement (exp, st) ->
        String.make spaces ' ' ^ "WhileStatementt:\n"
        ^ print_expression (spaces + 1) exp
        ^ print_statement (spaces + 1) st
    | ContinueStatement -> String.make spaces ' ' ^ "Continue\n"
    | BreakStatement -> String.make spaces ' ' ^ "Break\n"
  and print_declare spaces st =
    match st with
    | DeclareStatement (t, a, None) ->
        String.make spaces ' ' ^ "Declare Statement of " ^ a ^ " type: "
        ^ print_data_type t ^ "\n"
    | DeclareStatement (t, a, Some exp) ->
        String.make spaces ' ' ^ "Declare and Init Statement of " ^ a
        ^ " type: " ^ print_data_type t ^ "\n"
        ^ print_expression (spaces + 1) exp
  and print_block_item spaces item =
    match item with
    | StatementItem st ->
        String.make spaces ' ' ^ "StatementItem:\n"
        ^ print_statement (spaces + 1) st
    | DeclareItem decl ->
        String.make spaces ' ' ^ "DeclareItem:\n"
        ^ print_declare (spaces + 1) decl
  in
  let print_params params =
    List.fold_left (fun acc (name, dtype) -> acc ^ name ^ ",") "" params
  in
  let print_function spaces f =
    let print_statements spaces sts_opt =
      match sts_opt with
      | None ->
          String.make spaces ' '
          ^ "Empty Statements (Function declaration only.)"
      | Some statements ->
          List.fold_left
            (fun acc st -> acc ^ print_block_item spaces st)
            "" statements
    in
    match f with
    | IntFunction (a, params, sts) ->
        String.make spaces ' ' ^ "Function " ^ a ^ ": , Params: "
        ^ print_params params ^ "\n"
        ^ print_statements (spaces + 1) sts
  in
  match ast with
  | Program fns ->
      "Program: \n"
      ^ List.fold_left (fun acc f -> acc ^ print_function 1 f ^ "\n") "" fns

(* Parses a factor expression. *)
let rec parse_factor (tokens : token_t list) : expression_t * token_t list =
  (* Parses a factor. *)
  let rec parse_call_params acc tokens =
    match tokens with
    | [] -> fail "saw empty tokens in parse_call_params."
    | RightParentheses :: r -> (List.rev acc, r)
    | Comma :: r ->
        let exp, r2 = parse_expression r in
        parse_call_params (exp :: acc) r2
    | a ->
        let exp, r = parse_expression a in
        parse_call_params (exp :: acc) r
  in
  (* Parses array index expressions, such as 'arr[0]' or 'arr[0][i+3]',
   * assuming we already proccessed LeftBracket. *)
  let rec parse_array_index_exp exp tokens =
    let index, r = parse_expression tokens in
    let r = consume_token RightBracket r in
    if peek r = LeftBracket then
      let r = consume_token LeftBracket r in
      parse_array_index_exp (ArrayIndexExp (exp, index)) r
    else (ArrayIndexExp (exp, index), r)
  in

  match tokens with
  | [] -> fail "Empty expression."
  | Literal l :: r -> (
      match l with
      | IntLiteral i -> (ConstantIntExp i, r)
      | CharLiteral c -> (ConstantCharExp c, r)
      | StringLiteral s -> (ConstantStringExp s, r)
      | FloatLiteral f -> (ConstantFloatExp f, r) )
  | Identifier a :: r -> (
      match r with
      | LeftParentheses :: r2 ->
          let params, r3 = parse_call_params [] r2 in
          (FunctionCallExp (a, params), r3)
      | LeftBracket :: r2 -> parse_array_index_exp (VarExp a) r2
      | Addition :: Addition :: r2 -> (PostIncExp (VarExp a), r2)
      | Negation :: Negation :: r2 -> (PostDecExp (VarExp a), r2)
      | r2 -> (VarExp a, r2) )
  | Multiplication :: r ->
      let factor, r = parse_factor r in
      (DereferenceExp factor, r)
  | Address :: r ->
      let factor, r = parse_factor r in
      (AddressOfExp factor, r)
  | Addition :: Addition :: r ->
      let factor, r = parse_factor r in
      (PreIncExp factor, r)
  | Negation :: Negation:: r ->
      let factor, r = parse_factor r in
      (PreDecExp factor, r)
  | Negation :: r ->
      let exp, left = parse_factor r in
      (NegateOp exp, left)
  | LogicalNegation :: r ->
      let exp, left = parse_factor r in
      (LogicalNegateOp exp, left)
  | BitComplement :: r ->
      let exp, left = parse_factor r in
      (ComplementOp exp, left)
  | LeftParentheses :: r -> (
      let exp, left = parse_expression r in
      match left with
      | RightParentheses :: r -> (GroupedExpression exp, r)
      | _ -> fail "Expecting RightParentheses." )
  | a :: r -> fail ("Unexpected token in parse_factor: " ^ print_token a)

(* Parses a term expression. *)
and parse_term (tokens : token_t list) : expression_t * token_t list =
  (* Parses a term. *)
  let rec parse_term_rec previous_factor tokens =
    match tokens with
    | Multiplication :: r ->
        let factor, left = parse_factor r in
        parse_term_rec (MultiExp (previous_factor, factor)) left
    | Division :: r ->
        let factor, left = parse_factor r in
        parse_term_rec (DivideExp (previous_factor, factor)) left
    | _ -> (previous_factor, tokens)
  in
  let factor, left = parse_factor tokens in
  parse_term_rec factor left

(* Parses an additive expression. *)
and parse_additive_expression (tokens : token_t list) :
    expression_t * token_t list =
  let rec parse_expression_rec previous_term tokens =
    match tokens with
    | Addition :: r ->
        let term, left = parse_term r in
        parse_expression_rec (AdditionExp (previous_term, term)) left
    | Negation :: r ->
        let term, left = parse_term r in
        parse_expression_rec (MinusExp (previous_term, term)) left
    | _ -> (previous_term, tokens)
  in
  let term, left = parse_term tokens in
  parse_expression_rec term left

(* Parses a relational expression. *)
and parse_relational_expression (tokens : token_t list) :
    expression_t * token_t list =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | Less :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (LessExp (previous_exp, exp)) left
    | LessOrEqual :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (LessOrEqualExp (previous_exp, exp)) left
    | Greater :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (GreaterExp (previous_exp, exp)) left
    | GreaterOrEqual :: r ->
        let exp, left = parse_additive_expression r in
        parse_expression_rec (GreaterOrEqualExp (previous_exp, exp)) left
    | _ -> (previous_exp, tokens)
  in
  let exp, left = parse_additive_expression tokens in
  parse_expression_rec exp left

(* Parses equality expression. *)
and parse_equality_expression (tokens : token_t list) :
    expression_t * token_t list =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | Equal :: r ->
        let exp, left = parse_relational_expression r in
        parse_expression_rec (EqualExp (previous_exp, exp)) left
    | NotEqual :: r ->
        let exp, left = parse_relational_expression r in
        parse_expression_rec (NotEqualExp (previous_exp, exp)) left
    | _ -> (previous_exp, tokens)
  in
  let exp, left = parse_relational_expression tokens in
  parse_expression_rec exp left

and parse_logical_and_expression (tokens : token_t list) :
    expression_t * token_t list =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | And :: r ->
        let exp, left = parse_equality_expression r in
        parse_expression_rec (AndExp (previous_exp, exp)) left
    | _ -> (previous_exp, tokens)
  in
  let exp, left = parse_equality_expression tokens in
  parse_expression_rec exp left

and parse_logical_or_expression (tokens : token_t list) :
    expression_t * token_t list =
  let rec parse_expression_rec previous_exp tokens =
    match tokens with
    | Or :: r ->
        let exp, left = parse_logical_and_expression r in
        parse_expression_rec (OrExp (previous_exp, exp)) left
    | _ -> (previous_exp, tokens)
  in
  let exp, left = parse_logical_and_expression tokens in
  parse_expression_rec exp left

and parse_conditional_expression (tokens : token_t list) :
    expression_t * token_t list =
  let first_exp, left = parse_logical_or_expression tokens in
  match left with
  | QuestionMark :: r -> (
      let second_exp, r2 = parse_expression r in
      match r2 with
      | Colon :: r3 ->
          let third_exp, r4 = parse_expression r3 in
          (ConditionExp (first_exp, second_exp, third_exp), r4)
      | a :: _ -> fail ("Expecting Colon, but see " ^ print_token a)
      | [] -> fail "Expecting Colon, but see end of file." )
  | _ -> (first_exp, left)

(* Parses any one expression. *)
and parse_expression (tokens : token_t list) : expression_t * token_t list =
  (* Parses an expression. *)
  match tokens with
  | Identifier a :: Assignment :: r ->
      let exp, left = parse_expression r in
      (AssignExp (VarExp a, exp), left)
  (* We don't know directly if this is assigning to a array variable or not. so we need to check the left operand is assignable. *)
  | l ->
      let exp, r = parse_conditional_expression l in
      if peek r = Assignment then
        if is_assignable exp then
          let r = consume_token Assignment r in
          let exp2, r2 = parse_expression r in
          (AssignExp (exp, exp2), r2)
        else
          "Left operand is not assignable in parse_expression: "
          ^ print_expression 0 exp
          |> fail
      else (exp, r)

(* Returns expression Option. If the next token is Semi-colon will return None. *)
let parse_expression_opt (tokens : token_t list) :
    expression_t option * token_t list =
  match tokens with
  | Semicolon :: _ -> (None, tokens)
  | RightParentheses :: _ -> (None, tokens)
  | _ ->
      let exp, r = parse_expression tokens in
      (Some exp, r)

(* Does not consume the trailing Semicolon (if there is one). *)
let rec parse_statement (tokens : token_t list) : statement_t * token_t list =
  let parse_expression_opt_or_declare tokens =
    if peek tokens = Semicolon then (None, None, tokens)
    else if peek tokens = IntKeyword then
      let declare, r = parse_declare tokens in
      (None, Some declare, r)
    else
      let exp, r = parse_expression tokens in
      (Some exp, None, r)
  in
  match tokens with
  | [] -> fail "Empty statement."
  | ReturnKeyword :: r ->
      let expression, left = parse_expression r in
      (ReturnStatement expression, left)
  | LeftBrace :: r ->
      let block_items, left = parse_block_items [] r in
      (CompoundStatement block_items, left)
  | BreakKeyword :: r -> (BreakStatement, r)
  | ContinueKeyword :: r -> (ContinueStatement, r)
  | WhileKeyword :: LeftParentheses :: r ->
      let exp, r = parse_expression r in
      let r = consume_token RightParentheses r in
      let st, r = parse_statement r in
      (WhileStatement (exp, st), r)
  | DoKeyword :: r ->
      let st, r = parse_statement r in
      let r = consume_token WhileKeyword r in
      let r = consume_token LeftParentheses r in
      let exp, r = parse_expression r in
      let r = consume_token RightParentheses r in
      (DoStatement (st, exp), r)
  | ForKeyword :: LeftParentheses :: left -> (
      let exp1, declare, left = parse_expression_opt_or_declare left in
      let left = consume_token Semicolon left in
      let exp2, left = parse_expression_opt left in
      let left = consume_token Semicolon left in
      let exp3, left = parse_expression_opt left in
      let exp2 = match exp2 with None -> ConstantIntExp 1 | Some exp -> exp in
      let left = consume_token RightParentheses left in
      let st, left = parse_statement left in
      match declare with
      | None -> (ForStatement (exp1, exp2, exp3, st), left)
      | Some d -> (ForDeclStatement (d, exp2, exp3, st), left) )
  | IfKeyword :: LeftParentheses :: r -> (
      let cond_exp, r1 = parse_expression r in
      match r1 with
      | RightParentheses :: r2 -> (
          let first_st, r3 = parse_statement r2 in
          match r3 with
          | ElseKeyword :: r4 ->
              let second_st, r5 = parse_statement r4 in
              (ConditionalStatement (cond_exp, first_st, Some second_st), r5)
          | _ -> (ConditionalStatement (cond_exp, first_st, None), r3) )
      | _ -> fail "Expecting right parentheses in parse_statement" )
  | l ->
      let exp, left = parse_expression l in
      (ExpressionStatement (Some exp), left)

(* Parses a delcare statement. Does not consume the trailing Semicolon. *)
and parse_declare (tokens : token_t list) : declare_t * token_t list =
  (* Parses '[1][2]' into an int list *)
  let identifier, data_type, r = parse_data_type tokens in
  match r with
  | Assignment :: r ->
      let exp, left = parse_expression r in
      (DeclareStatement (data_type, identifier, Some exp), left)
  | r -> (DeclareStatement (data_type, identifier, None), r)

(* Parses tokens to get a statement, returns the statement and the remaining tokens. *)
(* This function consumes the trailing ; if it exists. *)
and parse_block_item (tokens : token_t list) : block_item_t * token_t list =
  let result, r =
    match tokens with
    | [] -> fail "Empty statement or declare statement."
    | a :: r ->
        if is_type_keyword a then
          let declare, r2 = parse_declare tokens in
          (DeclareItem declare, r2)
        else
          let st, r2 = parse_statement tokens in
          (StatementItem st, r2)
  in
  (* CompoundStatement does not have a trailing semi-colon. *)
  match result with
  | StatementItem (CompoundStatement _) -> (result, r)
  | StatementItem (ConditionalStatement (_, _, Some (CompoundStatement _))) ->
      (result, r)
  | StatementItem (ConditionalStatement (_, CompoundStatement _, None)) ->
      (result, r)
  | StatementItem (ForStatement (_, _, _, CompoundStatement _)) -> (result, r)
  | StatementItem (ForDeclStatement (_, _, _, CompoundStatement _)) ->
      (result, r)
  | StatementItem (WhileStatement (_, CompoundStatement _)) -> (result, r)
  | StatementItem (DoStatement _) -> (result, r)
  | _ -> (result, consume_token Semicolon r)

and parse_block_items (acc : block_item_t list) (tokens : token_t list) :
    block_item_t list * token_t list =
  match tokens with
  | RightBrace :: r -> (List.rev acc, r)
  | l ->
      let statement, left = parse_block_item tokens in
      parse_block_items (statement :: acc) left

(* Parses tokens to get a function, returns the function and the remaining tokens. *)
and parse_function (tokens : token_t list) : function_t * token_t list =
  let rec parse_parameters (acc : (string * data_type_t) list) tokens :
      (string * data_type_t) list * token_t list =
    match tokens with
    | RightParentheses :: r -> (List.rev acc, tokens)
    | a :: r ->
        let identifier, dtype, left = parse_data_type tokens in
        let left =
          if peek left = Comma then consume_token Comma left else left
        in
        parse_parameters ((identifier, dtype) :: acc) left
    | [] -> fail "Expecting token or RightParentheses in parse_parameters."
  in
  match tokens with
  | [] -> fail "Empty function."
  | IntKeyword :: Identifier fname :: LeftParentheses :: r ->
      let params, r = parse_parameters [] r in
      let r = consume_token RightParentheses r in
      if peek r = Semicolon then
        let r = consume_token Semicolon r in
        (IntFunction (fname, params, None), r)
      else
        let r = consume_token LeftBrace r in
        let statements, r = parse_block_items [] r in
        (IntFunction (fname, params, Some statements), r)
  | a :: r -> fail ("Unexpected token in parse_function: " ^ print_token a)

(* Parses all functions. *)
let rec parse_functions (acc : function_t list) (tokens : token_t list) :
    function_t list * token_t list =
  match tokens with
  | [] -> (List.rev acc, tokens)
  | a ->
      let f, r = parse_function a in
      parse_functions (f :: acc) r

(* Parses tokens to get a program, returns the program and possibly remaining tokens. *)
let parse_program (tokens : token_t list) : program_t * token_t list =
  match tokens with
  | [] -> fail "Empty program."
  | a ->
      let functions, r = parse_functions [] a in
      (Program functions, r)

(* Parses tokens to get the AST in program_t. *)
let get_ast (tokens : token_t list) : program_t =
  let program, left_tokens = parse_program tokens in
  program
