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

let is_type_keyword token : bool =
  if token = IntKeyword then true
  else if token = DoubleKeyword then true
  else if token = CharKeyword then true
  else if token = FloatKeyword then true
  else false

let read_file_content (file_name : string) : string =
  let file = open_in file_name in
  let buf = Buffer.create (in_channel_length file) in
  try
    while true do
      let line = input_line file in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n'
    done;
    assert false
  with End_of_file -> Buffer.contents buf

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

exception LexerError of string

let is_number (c : char) : bool = if c >= '0' && c <= '9' then true else false

let is_alpha (c : char) : bool =
  if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then true else false

let is_alphanumeric (c : char) : bool =
  if is_alpha c || is_number c then true else false

(* Parses a string content and get a list of tokens. *)
let parse_tokens (content : string) : token_t list =
  let parse_keyword_identifier_literal content i =
    let rec parse_word acc content i =
      if i >= String.length content then (acc, i)
      else
        let a = content.[i] in
        if is_alphanumeric a then
          parse_word (acc ^ String.make 1 a) content (i + 1)
        else (acc, i)
    in
    let word, loc = parse_word "" content i in
    if word = "int" then (IntKeyword, loc)
    else if word = "char" then (CharKeyword, loc)
    else if word = "return" then (ReturnKeyword, loc)
    else if word = "float" then (FloatKeyword, loc)
    else if word = "double" then (DoubleKeyword, loc)
    else if word = "if" then (IfKeyword, loc)
    else if word = "else" then (ElseKeyword, loc)
    else if word = "for" then (ForKeyword, loc)
    else if word = "while" then (WhileKeyword, loc)
    else if word = "do" then (DoKeyword, loc)
    else if word = "break" then (BreakKeyword, loc)
    else if word = "continue" then (ContinueKeyword, loc)
    else if is_alpha word.[0] then (Identifier word, loc)
    else (Literal (IntLiteral (int_of_string word)), loc)
  in
  let parse_char content i =
    let c = content.[i] in
    if content.[i + 1] = '\'' then (Literal (CharLiteral c), i + 2)
    else raise (LexerError "Expecting Right Single Quote in parse_char.")
  in
  (* Parses a string literal. *)
  let rec parse_string_literal acc content i =
    if i >= String.length content then
      LexerError "Unexpected end of file in parse_string_literal." |> raise
    else if content.[i] = '"' then (Literal (StringLiteral acc), i + 1)
    else parse_string_literal (acc ^ String.make 1 content.[i]) content (i + 1)
  in

  let rec parse_one_token content i =
    if i >= String.length content then (EndOfFile, i)
    else
      match content.[i] with
      | '{' -> (LeftBrace, i + 1)
      | '}' -> (RightBrace, i + 1)
      | '(' -> (LeftParentheses, i + 1)
      | ')' -> (RightParentheses, i + 1)
      | '[' -> (LeftBracket, i + 1)
      | ']' -> (RightBracket, i + 1)
      | ';' -> (Semicolon, i + 1)
      | '-' -> (Negation, i + 1)
      | ',' -> (Comma, i + 1)
      | '!' ->
          if content.[i + 1] = '=' then (NotEqual, i + 2)
          else (LogicalNegation, i + 1)
      | '~' -> (BitComplement, i + 1)
      | '+' -> (Addition, i + 1)
      | '*' -> (Multiplication, i + 1)
      | '/' -> (Division, i + 1)
      | ':' -> (Colon, i + 1)
      | '?' -> (QuestionMark, i + 1)
      | '&' ->
          if content.[i + 1] = '&' then (And, i + 2) else (Address, i + 1)
      | '|' ->
          if content.[i + 1] = '|' then (Or, i + 2)
          else raise (LexerError "Expeting another | sign.")
      | '=' ->
          if content.[i + 1] = '=' then (Equal, i + 2) else (Assignment, i + 1)
      | '>' ->
          if content.[i + 1] = '=' then (GreaterOrEqual, i + 2)
          else (Greater, i + 1)
      | '<' ->
          if content.[i + 1] = '=' then (LessOrEqual, i + 2) else (Less, i + 1)
      | ' ' -> parse_one_token content (i + 1)
      | '\n' -> parse_one_token content (i + 1)
      | '\'' -> parse_char content (i + 1)
      | '"' -> parse_string_literal "" content (i + 1)
      | a ->
          if is_alphanumeric a then parse_keyword_identifier_literal content i
          else raise (LexerError ("Illegal character: " ^ String.make 1 a))
  in
  let rec parse_tokens_acc tokens content i =
    let token, new_i = parse_one_token content i in
    if new_i < String.length content then
      parse_tokens_acc (token :: tokens) content new_i
    else tokens
  in
  List.rev (parse_tokens_acc [] content 0)
