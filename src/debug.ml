open Tokens
open Type

let print_token (token : token_t) : string =
  let print_literal l =
    match l with
    | StringLiteral s -> "String Literal: " ^ s
    | IntLiteral i -> "Int Literal: " ^ string_of_int i
    | CharLiteral c -> "Char Literal: " ^ String.make 1 c
    | FloatLiteral f -> "Float Literal: " ^ string_of_float f
  in
  match token with
  | IntKeyword -> "IntKeyword"
  | LeftParentheses -> "LeftParentheses"
  | RightParentheses -> "RightParentheses"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | LeftBracket -> "LeftBracket"
  | RightBracket -> "RightBracket"
  | ReturnKeyword -> "ReturnKeyword"
  | Identifier a -> "Identifier: " ^ a
  | Literal l -> print_literal l
  | Semicolon -> "Semicolon"
  | Negation -> "Negation -"
  | LogicalNegation -> "LogicalNegation !"
  | BitComplement -> "BitComplement ~"
  | Addition -> "Addition"
  | Multiplication -> "Multiplication"
  | Division -> "Division"
  | EndOfFile -> "EndOfFile"
  | And -> "And"
  | Or -> "Or"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | Less -> "Less"
  | LessOrEqual -> "LessOrEqual"
  | Greater -> "Greater"
  | GreaterOrEqual -> "GreaterOrEqual"
  | Assignment -> "Assignment"
  | IfKeyword -> "If"
  | ElseKeyword -> "Else"
  | Colon -> "Colon"
  | QuestionMark -> "QuestionMark"
  | ForKeyword -> "For"
  | WhileKeyword -> "While"
  | DoKeyword -> "Do"
  | BreakKeyword -> "Break"
  | ContinueKeyword -> "Continue"
  | Comma -> "Comma"
  | Address -> "Address"
  | CharKeyword -> "Char"
  | FloatKeyword -> "Float"
  | DoubleKeyword -> "Double"

let rec print_data_type (t : data_type_t) : string =
  match t with
  | IntType -> "IntType"
  | FloatType -> "FloatType"
  | DoubleType -> "DoubleType"
  | CharType -> "CharType"
  | ArrayType (t, size) ->
      "Array of (" ^ print_data_type t ^ ")[" ^ string_of_int size ^ "]"
  | PointerType t -> "Pointer of " ^ print_data_type t
  | UnknownType -> "UnknownType"
  | VoidType -> "VoidType"

let rec print_expression spaces exp =
  let print_space str = String.make spaces ' ' ^ str in
  let print_exp e = print_expression (spaces + 1) e in
  let rec print_binary spaces op exp1 exp2 =
    String.make spaces ' ' ^ op ^ " of \n"
    ^ print_expression (spaces + 1) exp1
    ^ print_expression (spaces + 1) exp2
  in
  match exp with
  | VarExp a -> String.make spaces ' ' ^ "Ref of " ^ a ^ "\n"
  | ArrayIndexExp (arr, index) ->
      print_space "ArrayIndex:\n" ^ print_exp arr ^ print_exp index
  | DereferenceExp exp -> print_space "PointerDereference:\n" ^ print_exp exp
  | AddressOfExp exp -> print_space "AddressOf:\n" ^ print_exp exp
  | ConstantIntExp n ->
      print_space "IntegerExpression: " ^ string_of_int n ^ "\n"
  | ConstantCharExp n -> print_space "CharExpression: " ^ String.make 1 n ^ "\n"
  | ConstantFloatExp n ->
      print_space "FloatExpression: " ^ string_of_float n ^ "\n"
  | ConstantStringExp n -> print_space "StringExpression: " ^ n ^ "\n"
  | PreIncExp exp -> print_space "PreInc : " ^ print_exp exp
  | PostIncExp exp -> print_space "PostInc : " ^ print_exp exp
  | PreDecExp exp -> print_space "PreDec : " ^ print_exp exp
  | PostDecExp exp -> print_space "PostDec : " ^ print_exp exp
  | NegateOp exp -> print_space "NegateOp:\n" ^ print_exp exp
  | LogicalNegateOp exp -> print_space "LogicalNegateOp:\n" ^ print_exp exp
  | ComplementOp exp -> print_space "ComplementOp:\n" ^ print_exp exp
  | GroupedExpression exp -> print_space "Grouped:\n" ^ print_exp exp
  | MultiExp (exp1, exp2) ->
      print_space "Multiplication of \n" ^ print_exp exp1 ^ print_exp exp2
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
      print_space "Assignment of :\n"
      ^ print_expression (spaces + 1) a
      ^ print_expression (spaces + 1) st
  | ConditionExp (exp1, exp2, exp3) ->
      print_space "ConditionExp:\n"
      ^ print_exp exp1 ^ print_exp exp2 ^ print_exp exp3
  | FunctionCallExp (fname, exps) ->
      print_space "Function call -> "
      ^ fname ^ "\n"
      ^ List.fold_left (fun acc exp -> acc ^ print_exp exp) "" exps

(* Pretty prints an AST. *)
and print_ast (ast : program_t) : string =
  let print_expression_option spaces exp_opt =
    match exp_opt with None -> "" | Some exp -> print_expression spaces exp
  in
  let rec print_statement spaces st =
    let print_space str = String.make spaces ' ' ^ str in
    let print_st st = print_statement (spaces + 1) st in
    let print_exp e = print_expression (spaces + 1) e in
    match st with
    | ReturnStatement exp -> print_space "Return Statement: \n" ^ print_exp exp
    | ExpressionStatement (Some exp) ->
        print_space "Expression Statement: \n" ^ print_exp exp
    | ExpressionStatement None -> print_space "Empty expression statement"
    | ConditionalStatement (exp, st1, Some st2) ->
        print_space "ConditionStatement:\n"
        ^ print_exp exp ^ print_st st1 ^ print_st st2
    | CompoundStatement items ->
        print_space "CompountStatement:\n"
        ^ List.fold_left
            (fun acc st -> acc ^ print_block_item (spaces + 1) st)
            "" items
    | ConditionalStatement (exp, st1, None) ->
        print_space "ConditionStatement:\n" ^ print_exp exp ^ print_st st1
    | ForStatement (exp1_opt, exp2, exp3_opt, st) ->
        print_space "ForStatement:\n"
        ^ print_expression_option (spaces + 1) exp1_opt
        ^ print_exp exp2
        ^ print_expression_option (spaces + 1) exp3_opt
        ^ print_st st
    | ForDeclStatement (decl, exp2, exp3_opt, st) ->
        print_space "ForDeclareStatement:\n"
        ^ print_declare (spaces + 1) decl
        ^ print_exp exp2
        ^ print_expression_option (spaces + 1) exp3_opt
        ^ print_st st
    | DoStatement (st, exp) ->
        print_space "DoStatementt:\n" ^ print_exp exp ^ print_st st
    | WhileStatement (exp, st) ->
        print_space "WhileStatementt:\n" ^ print_exp exp ^ print_st st
    | ContinueStatement -> print_space "Continue\n"
    | BreakStatement -> print_space "Break\n"
  and print_declare spaces st =
    let print_space str = String.make spaces ' ' ^ str in
    let print_exp e = print_expression (spaces + 1) e in
    match st with
    | DeclareStatement (t, a, None) ->
        print_space "Declare Statement of "
        ^ a ^ " type: " ^ print_data_type t ^ "\n"
    | DeclareStatement (t, a, Some exp) ->
        print_space "Declare and Init Statement of "
        ^ a ^ " type: " ^ print_data_type t ^ "\n" ^ print_exp exp
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
