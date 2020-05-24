type literal_t =
  | IntLiteral of int
  | StringLiteral of string
  | CharLiteral of char
  | FloatLiteral of float

type token_t =
  | IntKeyword
  | CharKeyword
  | FloatKeyword
  | DoubleKeyword
  | LeftParentheses
  | RightParentheses
  | LeftBrace
  | RightBrace
  | LeftBracket
  | RightBracket
  | ReturnKeyword
  | Identifier of string
  | Literal of literal_t
  | Semicolon
  | Negation (* this is also Minus *)
  | LogicalNegation (* this is the ! sign *)
  | BitComplement
  | Addition
  | Multiplication (* Also pointer declaration and dereference *)
  | Division
  | EndOfFile
  | And
  | Or
  | Equal
  | NotEqual
  | Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | Assignment
  | IfKeyword
  | ElseKeyword
  | Colon
  | QuestionMark
  | ForKeyword
  | WhileKeyword
  | DoKeyword
  | BreakKeyword
  | ContinueKeyword
  | Comma
  | Address


