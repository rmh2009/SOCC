open Tokens
open Type
open Typeutil

let is_assignable (exp : expression_t) : bool =
  match exp with
  | VarExp _
  | ArrayIndexExp (_, _)
  | DereferenceExp _
  | StructMemberExp (_, _)
  | ArrowStructMemberExp (_, _) ->
      true
  | _ -> false

exception ParserError of string

let fail message = raise (ParserError message)

let peek (tokens : token_t list) : token_t =
  match tokens with [] -> fail "No tokens left in peek tokens." | a :: _ -> a

let consume_token (token : token_t) (tokens : token_t list) : token_t list =
  match tokens with
  | hd :: tl ->
      if hd = token then tl
      else
        fail
          ( "Failed to consume token: " ^ Debug.print_token token
          ^ ", saw token: " ^ Debug.print_token hd )
  | [] ->
      fail
        ( "No tokens left in consuming tokens, expecting token: "
        ^ Debug.print_token token )

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
  (* Parses chained variable access such as a.b[10]->c.e *)
  let rec parse_variable_access_exp parent tokens =
    match tokens with
    | LeftBracket :: r ->
        let index, r = parse_expression r in
        let r = consume_token RightBracket r in
        parse_variable_access_exp (ArrayIndexExp (parent, index)) r
    | Dot :: Identifier child :: r ->
        parse_variable_access_exp (StructMemberExp (parent, child)) r
    | Arrow :: Identifier child :: r ->
        parse_variable_access_exp (ArrowStructMemberExp (parent, child)) r
    | _ -> (parent, tokens)
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
      | Dot :: _ | Arrow :: _ | LeftBracket :: _ ->
          parse_variable_access_exp (VarExp a) r
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
  | Negation :: Negation :: r ->
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
  | a :: r -> fail ("Unexpected token in parse_factor: " ^ Debug.print_token a)

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
      | a :: _ -> fail ("Expecting Colon, but see " ^ Debug.print_token a)
      | [] -> fail "Expecting Colon, but see end of file." )
  | _ -> (first_exp, left)

(* Parses any one expression. *)
and parse_expression (tokens : token_t list) : expression_t * token_t list =
  (* Parses an expression. *)
  let exp, r = parse_conditional_expression tokens in
  if peek r = Assignment then
    if is_assignable exp then
      let r = consume_token Assignment r in
      let exp2, r2 = parse_expression r in
      (AssignExp (exp, exp2), r2)
    else
      "Left operand is not assignable in parse_expression: "
      ^ Debug.print_expression 0 exp
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
        if Lexer.is_type_keyword a then
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
  | tokens -> (
      let fname, return_type, remain = parse_data_type tokens in
      match remain with
      | LeftParentheses :: r ->
          let params, r = parse_parameters [] r in
          let r = consume_token RightParentheses r in
          if peek r = Semicolon then
            let r = consume_token Semicolon r in
            (Function (fname, return_type, params, None), r)
          else
            let r = consume_token LeftBrace r in
            let statements, r = parse_block_items [] r in
            (Function (fname, return_type, params, Some statements), r)
      | a :: _ ->
          "Expecing LeftParentheses in parse function, saw "
          ^ Debug.print_token a
          |> fail
      | [] ->
          fail "Expecing LeftParentheses in parse function, saw no token left."
      )

let parse_struct_definition (tokens : token_t list) : data_type_t * token_t list
    =
  let rec parse_declares_helper acc tokens : declare_t list * token_t list =
    if peek tokens != RightBrace then
      let declare, r = parse_declare tokens in
      let r = consume_token Semicolon r in
      parse_declares_helper (declare :: acc) r
    else (List.rev acc, tokens)
  in
  match tokens with
  | StructKeyword :: Identifier id :: LeftBrace :: r ->
      let declares, left = parse_declares_helper [] r in
      let res =
        StructType
          ( id,
            List.map
              (fun (DeclareStatement (t, name, _) : declare_t) -> (name, t))
              declares )
      in
      let left = consume_token RightBrace left in
      let left = consume_token Semicolon left in
      (res, left)
  | _ -> fail "Illegal struct definition."

(* Parses all functions. *)
let rec parse_globals (acc : global_item_t list) (tokens : token_t list) :
    global_item_t list * token_t list =
  match tokens with
  | [] -> (List.rev acc, tokens)
  | StructKeyword :: Identifier (_) :: LeftBrace :: _ ->
      let struct_type, r = parse_struct_definition tokens in
      parse_globals (GlobalDef struct_type :: acc) r
  | _ ->
      let f, r = parse_function tokens in
      parse_globals (GlobalFunction f :: acc) r

(* Parses tokens to get a program, returns the program and possibly remaining tokens. *)
let parse_program (tokens : token_t list) : program_t * token_t list =
  match tokens with
  | [] -> fail "Empty program."
  | a ->
      let global_items, r = parse_globals [] a in
      (Program global_items, r)

(* Parses tokens to get the AST in program_t. *)
let get_ast (tokens : token_t list) : program_t =
  let program, left_tokens = parse_program tokens in
  program
