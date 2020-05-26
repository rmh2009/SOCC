open Type
open Tokens

exception ParseTypeError of string

let parse_data_type (tokens : token_t list) :
    string * data_type_t * token_t list =
  let rec helper name_opt cur_type tokens =
    match tokens with
    | Multiplication :: r -> helper name_opt (PointerType cur_type) r
    | Identifier name :: r ->
        if name_opt = None then helper (Some name) cur_type r
        else
          ParseTypeError
            ("Already parsed an identifier, found another identifer: " ^ name)
          |> raise
    | LeftBracket :: Literal (IntLiteral a) :: RightBracket :: r ->
        let name_res, child_type, rest = helper name_opt cur_type r in
        name_res, ArrayType(child_type, a), rest
    | _ -> (name_opt, cur_type, tokens)
  in
  let name_opt, cur_type, r =
    match tokens with
    | IntKeyword :: r -> helper None IntType r
    | CharKeyword :: r -> helper None CharType r
    | DoubleKeyword :: r -> helper None DoubleType r
    | FloatKeyword :: r -> helper None FloatType r
    | x :: r ->
        ParseTypeError ("Illegal token in parse type: " ^ Debug.print_token x) |> raise
    | [] -> 
        ParseTypeError ("Empty tokens in parse_data_type.") |> raise
  in
  match name_opt with
  | None -> ParseTypeError "Failed to get declared name in parse_type." |> raise
  | Some name -> (name, cur_type, r)

let is_type_array (data_type : data_type_t) =
  match data_type with ArrayType (_, _) -> true | _ -> false

let is_type_pointer (data_type : data_type_t) =
  match data_type with PointerType _ -> true | _ -> false

(* Decays an array to pointer to array. This is used for interpreting
 * array types in function parameter. *)
let decay (dtype: data_type_t) : data_type_t =
  match dtype with
  | ArrayType(child, size) -> PointerType(dtype)
  | a -> a

