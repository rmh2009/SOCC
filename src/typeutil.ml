open Type
open Lexer

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

let rec get_data_size (t : data_type_t) : int =
  match t with
  | IntType -> 4
  | ArrayType (t2, size) -> size * get_data_size t2
  | PointerType _ -> 4 (* TODO 32 bit assembly limit *)
  | _ -> TokenError "Unsupported data type." |> raise

let parse_data_type (tokens : token_t list) :
    string * data_type_t * token_t list =
  let rec helper name_opt cur_type tokens =
    match tokens with
    | [] -> (name_opt, cur_type, tokens)
    | Multiplication :: r -> helper name_opt (PointerType cur_type) r
    | Identifier name :: r ->
        if name_opt = None then helper (Some name) cur_type r
        else
          TokenError
            ("Already parsed an identifier, found another identifer: " ^ name)
          |> raise
    | LeftBracket :: Literal (IntLiteral a) :: RightBracket :: r ->
        helper name_opt (ArrayType (cur_type, a)) r
    | Semicolon :: r -> (name_opt, cur_type, tokens)
    | Assignment :: r -> (name_opt, cur_type, tokens)
    | x :: r ->
        TokenError ("Illegal token encountered in parse type: " ^ print_token x)
        |> raise
  in
  let name_opt, cur_type, r =
    match tokens with
    | IntKeyword :: r -> helper None IntType r
    | CharKeyword :: r -> helper None CharType r
    | DoubleKeyword :: r -> helper None DoubleType r
    | FloatKeyword :: r -> helper None FloatType r
    | x :: r ->
        TokenError ("Illegal token in parse type: " ^ print_token x) |> raise
  in
  match name_opt with
  | None -> TokenError "Failed to get declared name in parse_type." |> raise
  | Some name -> (name, cur_type, r)

let is_type_array (data_type : data_type_t) =
  match data_type with ArrayType (_, _) -> true | _ -> false

let is_type_pointer (data_type : data_type_t) =
  match data_type with PointerType _ -> true | _ -> false
