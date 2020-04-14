
let read_file_content file_name =
  let file = open_in file_name in
  let buf = Buffer.create (in_channel_length file) in
  try
    while true do
      let line = input_line file in
      Buffer.add_string buf line;
      Buffer.add_char buf '\n';
    done; assert false
  with End_of_file ->
    Buffer.contents buf

type token_t = 
  MainKeyword 
  | IntKeyword 
  | LeftParentheses 
  | RightParentheses 
  | LeftBrace 
  | RightBrace 
  | ReturnKeyword 
  | Identifier of string 
  | IntegerLiteral of string 
  | Semicolon 
  | EndOfFile

let print_token token =
  match token with
  | MainKeyword -> "MainKeyword"
  | IntKeyword -> "IntKeyword"
  | LeftParentheses -> "LeftParentheses"
  | RightParentheses -> "RightParentheses"
  | LeftBrace  -> "LeftBrace"
  | RightBrace -> "RightBrace"
  | ReturnKeyword -> "ReturnKeyword"
  | Identifier a -> "Identifier: " ^ a
  | IntegerLiteral a -> "IntegerLiteral: " ^ a
  | Semicolon -> "Semicolon"
  | EndOfFile -> "EndOfFile"

exception LexerError of string

let is_number c =
  if (c >= '0' && c <= '9') then true
  else false

let is_alpha c =
  if (c >= 'a' && c <= 'z') || ( c >= 'A' && c <= 'Z') then true
  else false

let is_alphanumeric c =
  if (is_alpha c) || (is_number c) then true
  else false

  (* Parses a string content and get a list of tokens *)
let parse_tokens content =
  let parse_keyword_identifier_literal content i =
    let rec parse_word acc content i =
      if i >= (String.length content) then (acc, i)
      else
        let a = String.get content i in
        if (is_alphanumeric a) then parse_word (acc ^ String.make 1 a) content (i+1)
        else (acc, i)
        in
    let word, loc = parse_word "" content i in
    if word = "main" then MainKeyword, loc
    else if word = "int" then IntKeyword, loc
    else if word = "return" then ReturnKeyword, loc
    else if (is_alpha (String.get word 0)) then (Identifier word), loc
    else (IntegerLiteral word), loc
    in
  let rec parse_one_token content i =
    if i >= String.length content then (EndOfFile, i)
    else
      match String.get content i with
    | '{' -> LeftParentheses, i+1
    | '}' -> RightParentheses, i+1
    | '(' -> LeftBrace, i+1
    | ')' -> RightBrace, i+1
    | ';' -> Semicolon, i+1
    | ' ' -> parse_one_token content (i+1)
    | '\n' -> parse_one_token content (i+1)
    | a -> 
        if (is_alphanumeric a) then parse_keyword_identifier_literal content i
        else raise (LexerError "Illegal character.")
  in
  let rec parse_tokens_acc tokens content i =
    let token, new_i = parse_one_token content i in
    if (new_i < String.length content) then parse_tokens_acc (token :: tokens) content new_i
    else tokens
    in
  parse_tokens_acc [] content 0

let _ = 
  Printf.printf "%s" (read_file_content "test_file");
  let tokens = parse_tokens (read_file_content "test_file") in
  List.iter (fun a -> Printf.printf "%s\n" (print_token a)) tokens


