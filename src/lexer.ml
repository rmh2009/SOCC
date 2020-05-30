open Tokens

let is_type_keyword token : bool =
  match token with
  | IntKeyword | DoubleKeyword | CharKeyword | FloatKeyword | StructKeyword ->
      true
  | _ -> false

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

exception LexerError of string

let fail msg = raise (LexerError msg)

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
    else if word = "struct" then (StructKeyword, loc)
    else if is_alpha word.[0] then (Identifier word, loc)
    else (Literal (IntLiteral (int_of_string word)), loc)
  in
  let parse_char content i =
    let c = content.[i] in
    if content.[i + 1] = '\'' then (Literal (CharLiteral c), i + 2)
    else fail "Expecting Right Single Quote in parse_char."
  in
  (* Parses a string literal. *)
  let rec parse_string_literal acc content i =
    if i >= String.length content then
      fail "Unexpected end of file in parse_string_literal."
    else if content.[i] = '"' then (Literal (StringLiteral acc), i + 1)
    else parse_string_literal (acc ^ String.make 1 content.[i]) content (i + 1)
  in

  let rec consume_after_new_line content i : token_t * int =
    if i >= String.length content then (EndOfFile, i)
    else
      match content.[i] with
      | '\n' -> parse_one_token content (i + 1)
      | a -> consume_after_new_line content (i + 1)
  and parse_one_token content i : token_t * int =
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
      | '.' -> (Dot, i + 1)
      | '-' -> 
          if content.[i+1] = '>' then (Arrow, i + 2)
          else (Negation, i + 1)
      | ',' -> (Comma, i + 1)
      | '!' ->
          if content.[i + 1] = '=' then (NotEqual, i + 2)
          else (LogicalNegation, i + 1)
      | '~' -> (BitComplement, i + 1)
      | '+' -> (Addition, i + 1)
      | '*' -> (Multiplication, i + 1)
      | '/' ->
          if content.[i + 1] = '/' then consume_after_new_line content (i + 1)
          else (Division, i + 1)
      | ':' -> (Colon, i + 1)
      | '?' -> (QuestionMark, i + 1)
      | '&' -> if content.[i + 1] = '&' then (And, i + 2) else (Address, i + 1)
      | '|' ->
          if content.[i + 1] = '|' then (Or, i + 2)
          else fail "Expeting another | sign."
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
          else fail ("Illegal character: " ^ String.make 1 a)
  in
  let rec parse_tokens_acc tokens content i : token_t list =
    let token, new_i = parse_one_token content i in
    match token with
    | EndOfFile -> tokens
    | _ ->
        if new_i < String.length content then
          parse_tokens_acc (token :: tokens) content new_i
        else token :: tokens
  in
  List.rev (parse_tokens_acc [] content 0)
