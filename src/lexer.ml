type token_t =
  | IntKeyword
  | LeftParentheses
  | RightParentheses
  | LeftBrace
  | RightBrace
  | ReturnKeyword
  | Identifier of string
  | IntegerLiteral of string
  | Semicolon
  | Negation (* this is also Minus *)
  | LogicalNegation (* this is the ! sign *)
  | BitComplement
  | Addition
  | Multiplication
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
  match token with
  | IntKeyword -> "IntKeyword"
  | LeftParentheses -> "LeftParentheses"
  | RightParentheses -> "RightParentheses"
  | LeftBrace -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | ReturnKeyword -> "ReturnKeyword"
  | Identifier a -> "Identifier: " ^ a
  | IntegerLiteral a -> "IntegerLiteral: " ^ a
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
    else if word = "return" then (ReturnKeyword, loc)
    else if word = "if" then (IfKeyword, loc)
    else if word = "else" then (ElseKeyword, loc)
    else if word = "for" then (ForKeyword, loc)
    else if word = "while" then (WhileKeyword, loc)
    else if word = "do" then (DoKeyword, loc)
    else if word = "break" then (BreakKeyword, loc)
    else if word = "continue" then (ContinueKeyword, loc)
    else if is_alpha word.[0] then (Identifier word, loc)
    else (IntegerLiteral word, loc)
  in
  let rec parse_one_token content i =
    if i >= String.length content then (EndOfFile, i)
    else
      match content.[i] with
      | '{' -> (LeftBrace, i + 1)
      | '}' -> (RightBrace, i + 1)
      | '(' -> (LeftParentheses, i + 1)
      | ')' -> (RightParentheses, i + 1)
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
          if content.[i + 1] = '&' then (And, i + 2)
          else raise (LexerError "Expeting another & sign.")
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
